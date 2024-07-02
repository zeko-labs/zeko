open Async
open Core_kernel
open Mina_base
open Mina_ledger

module Config = struct
  type t =
    { da_websocket : Bounded_types.String.Stable.V1.t
    ; da_contract_address : Bounded_types.String.Stable.V1.t
    ; da_private_key : Bounded_types.String.Stable.V1.t
    }
end

let deploy ~da_websocket ~da_private_key ~quorum ~validators =
  Da_utils.deploy ~da_websocket ~da_private_key ~quorum ~validators

module Genesis_state = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Account.Stable.V2.t list [@@deriving yojson]

      let to_latest = Fn.id
    end
  end]

  let get (config : Config.t) =
    let%bind.Deferred.Result data =
      Da_utils.get_genesis_state ~da_websocket:config.da_websocket
        ~da_contract_address:config.da_contract_address
    in
    let%bind.Deferred.Result data =
      match Base64.decode data with
      | Ok data ->
          return (Ok data)
      | Error (`Msg e) ->
          return (Error (Error.of_string e))
    in
    let accounts = Binable.of_string (module Stable.Latest) data in
    return (Ok accounts)

  let init (config : Config.t) ~(genesis_accounts : Account.t list) =
    let data =
      Base64.encode_string
      @@ Binable.to_string (module Stable.Latest) genesis_accounts
    in
    Da_utils.init_genesis_state ~da_websocket:config.da_websocket
      ~da_contract_address:config.da_contract_address
      ~da_private_key:config.da_private_key ~data
end

module Account_map = struct
  include Core.Map.Make (Account_id)

  let to_yojson a_to_yojson (m : 'a t) =
    to_alist m |> [%to_yojson: (Account_id.t * 'a) list] a_to_yojson

  let of_yojson a_of_yojson json =
    let%map.Result alist =
      [%of_yojson: (Account_id.t * 'a) list] a_of_yojson json
    in
    of_alist_exn alist
end

module Batch = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t =
        { previous_location : Bounded_types.String.Stable.V1.t
        ; receipt_chain_hashes :
            (Account_id.Stable.V2.t * Receipt.Chain_hash.Stable.V1.t) list
        ; commands : User_command.Stable.V2.t list
        ; target_ledger : Sparse_ledger.Stable.V2.t
        }
      [@@deriving yojson]

      let to_latest = Fn.id
    end
  end]

  let post (config : Config.t) ~previous_location ~commands
      ~receipt_chain_hashes ~target_ledger =
    let t =
      { previous_location; receipt_chain_hashes; commands; target_ledger }
    in
    let sig_data =
      [ Ledger_hash.to_decimal_string @@ Sparse_ledger.merkle_root target_ledger
      ]
    in
    let batch_data =
      Base64.encode_string @@ Binable.to_string (module Stable.Latest) t
    in
    Da_utils.post_batch ~da_websocket:config.da_websocket
      ~da_contract_address:config.da_contract_address
      ~da_private_key:config.da_private_key ~batch_data ~sig_data

  let get (config : Config.t) ~location =
    let%bind.Deferred.Result data =
      Da_utils.get_batch_data ~da_websocket:config.da_websocket
        ~da_contract_address:config.da_contract_address ~location
    in
    let%bind.Deferred.Result data =
      match Base64.decode data with
      | Ok data ->
          return (Ok data)
      | Error (`Msg e) ->
          return (Error (Error.of_string e))
    in
    let t = Binable.of_string (module Stable.Latest) data in
    return (Ok t)

  (* Get all commands from genesis state to the `location` *)
  let rec get_commands (config : Config.t) ~location =
    if Int.of_string location = 0 then return (Ok [])
    else
      let%bind.Deferred.Result { commands; previous_location } =
        get config ~location
      in
      let%bind.Deferred.Result previous =
        get_commands config ~location:previous_location
      in
      return (Ok (previous @ commands))

  let is_valid { commands; receipt_chain_hashes; target_ledger; _ } =
    let%bind.Result map =
      match Account_map.of_alist receipt_chain_hashes with
      | `Ok map ->
          Ok map
      | `Duplicate_key account_id ->
          Error (sprintf !"Duplicate key: %{sexp: Account_id.t}" account_id)
    in
    let map_err =
      sprintf !"Receipt chain hash not found for account: %{sexp:Account_id.t}"
    in
    let%bind.Result applied_hashes =
      List.fold_result commands ~init:map ~f:(fun acc command ->
          match command with
          | Signed_command command ->
              let account_id = Signed_command.fee_payer command in
              let%bind.Result old_receipt_chain_hash =
                Account_map.find acc account_id
                |> Result.of_option ~error:(map_err account_id)
              in
              let new_receipt_chain_hash =
                Receipt.Chain_hash.cons_signed_command_payload
                  (Signed_command_payload (Signed_command.payload command))
                  old_receipt_chain_hash
              in
              Ok
                (Account_map.set acc ~key:account_id
                   ~data:new_receipt_chain_hash )
          | Zkapp_command command ->
              let _, commitment =
                Zkapp_command.get_transaction_commitments command
              in
              let%bind.Result _, acc =
                List.fold_result
                  (Zkapp_command.all_account_updates_list command)
                  ~init:(Unsigned.UInt32.zero, acc)
                  ~f:(fun (index, acc) account_update ->
                    (* Receipt chain hash is updated only for authorised account updates *)
                    match Account_update.authorization account_update with
                    | None_given ->
                        Ok (Unsigned.UInt32.succ index, acc)
                    | _ ->
                        let account_id =
                          Account_update.account_id account_update
                        in
                        let%bind.Result old_receipt_chain_hash =
                          Account_map.find acc account_id
                          |> Result.of_option ~error:(map_err account_id)
                        in
                        let new_receipt_chain_hash =
                          Receipt.Chain_hash.cons_zkapp_command_commitment index
                            (Zkapp_command_commitment commitment)
                            old_receipt_chain_hash
                        in
                        Ok
                          ( Unsigned.UInt32.succ index
                          , Account_map.set acc ~key:account_id
                              ~data:new_receipt_chain_hash ) )
              in
              Ok acc )
    in
    let%bind.Result () =
      List.fold_result (Account_map.to_alist applied_hashes) ~init:()
        ~f:(fun _ (account_id, expected_receipt_chain_hash) ->
          let%bind.Result account =
            try
              let location =
                Sparse_ledger.find_index_exn target_ledger account_id
              in
              Ok (Sparse_ledger.get_exn target_ledger location)
            with _ -> Error "Account not found in target ledger"
          in
          if
            Receipt.Chain_hash.equal expected_receipt_chain_hash
              account.receipt_chain_hash
          then Ok ()
          else
            Error
              (sprintf
                 !"Receipt chain hash mismatch for account: \
                   %{sexp:Account_id.t}"
                 account_id ) )
    in
    Ok `Valid

  let sign t keypair = ()
end
