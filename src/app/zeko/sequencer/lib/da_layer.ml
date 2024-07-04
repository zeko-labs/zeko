(** 
  This module provides an interface to interact with the data availability layer.
  It provides functions to deploy the data availability contract, initialize the genesis state,
  post batches of user commands, and get the commands from the genesis state to a given location.

  All the types used in this module are versioned and serialized using bin_prot.
  This is needed for backwards compatibility in case we need to change the types in the future.
*)

open Core_kernel
open Mina_base
open Mina_ledger
open Signature_lib
open Snark_params
open Async

module Config = struct
  type t =
    { da_websocket : Bounded_types.String.Stable.V1.t
    ; da_contract_address : Bounded_types.String.Stable.V1.t
    ; da_private_key : Bounded_types.String.Stable.V1.t
    }
end

let deploy ~da_websocket ~da_private_key ~quorum ~validators =
  Da_utils.deploy ~da_websocket ~da_private_key ~quorum ~validators

(* List of accounts in the genesis state *)
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

(**
  Mapping of Account_id.t -> 'a
  Used for storing the receipt chain hashes of accounts in the batch
*)
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

(**
  A batch is a collection of user commands that have been applied to the ledger state.
  The batch also contains the receipt chain hashes of the accounts that are updated by the commands.
  
  The receipt chain hashes are used to verify the validity of the batch by the DA validators.
  The validator applies all the receipts to the chain hashes and compares with the target ledger.
  If all of them match, they sign the target ledger hash together with the location and previous location.
*)
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
    (* 0th location is reserved for the genesis state *)
    if Int.of_string location = 0 then return (Ok [])
    else
      let%bind.Deferred.Result { commands; previous_location } =
        get config ~location
      in
      let%bind.Deferred.Result previous =
        get_commands config ~location:previous_location
      in
      return (Ok (previous @ commands))

  (**
    Returns `Ok of `Valid` if all the receipts match the target ledger.
    Otherwise, returns an `Error of string` with the reason.
  *)
  let is_valid { commands; receipt_chain_hashes; target_ledger; _ } =
    (* Create mapping from the list of receipt chain hashes *)
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

    (* Apply all the commands receipts to the receipt chain hashes *)
    let%bind.Result applied_hashes =
      List.fold_result commands ~init:map ~f:(fun acc command ->
          match command with
          | Signed_command command ->
              (* For command only the fee payer gets the receipt *)
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
              let _commitment, full_transaction_commitment =
                Zkapp_command.get_transaction_commitments command
              in
              let%bind.Result _, acc =
                List.fold_result
                  (Zkapp_command.all_account_updates_list command)
                  ~init:(Unsigned.UInt32.zero, acc)
                  ~f:(fun (index, acc) account_update ->
                    (* Receipt chain hash is updated only for account updates authorised with Proof of Signature *)
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
                            (Zkapp_command_commitment
                               full_transaction_commitment )
                            old_receipt_chain_hash
                        in
                        Ok
                          ( Unsigned.UInt32.succ index
                          , Account_map.set acc ~key:account_id
                              ~data:new_receipt_chain_hash ) )
              in
              Ok acc )
    in

    (* Verify the receipt chain hashes with the target ledger *)
    let%bind.Result () =
      List.fold_result (Account_map.to_alist applied_hashes) ~init:()
        ~f:(fun _ (account_id, expected_receipt_chain_hash) ->
          (* Find the account in target ledger *)
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

  let to_message t ~location =
    let { previous_location; target_ledger } = t in
    let ledger_hash = Sparse_ledger.merkle_root target_ledger in
    Random_oracle.Input.Chunked.field_elements
      [| ledger_hash
       ; Tick.Field.of_string previous_location
       ; Tick.Field.of_string location
      |]

  let sign t ~location ~(keypair : Keypair.t) =
    let message = to_message t ~location in
    Schnorr.Chunked.sign keypair.private_key message
end

module Batch_signature = struct
  let post (config : Config.t) ~location ~(signature : Signature.t) ~pk =
    let sig_rx, sig_s = signature in
    let open Snark_params in
    Da_utils.post_batch_signature ~da_websocket:config.da_websocket
      ~da_private_key:config.da_private_key
      ~da_contract_address:config.da_contract_address ~location
      ~mina_pk:(Public_key.Compressed.to_base58_check pk)
      ~sig_rx:(Tick.Field.to_string sig_rx)
      ~sig_s:(Tock.Field.to_string sig_s)

  module Sol_types = struct
    type sol_public_key = { x : string; y : string } [@@deriving yojson]

    type sol_schnorr_signature =
      { public_key : sol_public_key; rx : string; s : string }
    [@@deriving yojson]

    let of_yojson json : ((Public_key.t * Signature.t) list, Error.t) result =
      let map_result = function
        | Ok data ->
            Ok data
        | Error e ->
            Error (Error.of_string e)
      in
      let%bind.Result raw_data =
        [%of_yojson: sol_schnorr_signature list] json |> map_result
      in
      Ok
        (List.map raw_data ~f:(fun { public_key; rx; s } ->
             let pkx = Tick.Field.of_string public_key.x in
             let pky = Tick.Field.of_string public_key.y in
             let pk = (pkx, pky) in

             let rx = Tick.Field.of_string rx in
             let s = Tock.Field.of_string s in
             let signature = (rx, s) in

             (pk, signature) ) )
  end

  let get (config : Config.t) ~location =
    let%bind.Deferred.Result data =
      Da_utils.get_batch_signatures ~da_websocket:config.da_websocket
        ~da_contract_address:config.da_contract_address ~location
    in
    return @@ Sol_types.of_yojson @@ Yojson.Safe.from_string data
end
