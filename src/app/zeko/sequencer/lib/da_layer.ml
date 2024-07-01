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
end

let get_genesis_accounts (config : Config.t) =
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
  let accounts = Binable.of_string (module Genesis_state.Stable.Latest) data in
  return (Ok accounts)

let init_genesis_accounts (config : Config.t)
    ~(genesis_accounts : Account.t list) =
  let data =
    Base64.encode_string
    @@ Binable.to_string (module Genesis_state.Stable.Latest) genesis_accounts
  in
  Da_utils.init_genesis_state ~da_websocket:config.da_websocket
    ~da_contract_address:config.da_contract_address
    ~da_private_key:config.da_private_key ~data

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
end

let post_batch (config : Config.t) ~previous_location ~commands
    ~receipt_chain_hashes ~target_ledger =
  let t =
    Batch.Stable.V1.
      { previous_location; receipt_chain_hashes; commands; target_ledger }
  in
  let sig_data =
    [ Ledger_hash.to_decimal_string @@ Sparse_ledger.merkle_root target_ledger ]
  in
  let batch_data =
    Base64.encode_string @@ Binable.to_string (module Batch.Stable.Latest) t
  in
  Da_utils.post_batch ~da_websocket:config.da_websocket
    ~da_contract_address:config.da_contract_address
    ~da_private_key:config.da_private_key ~batch_data ~sig_data

let rec get_batches (config : Config.t) ~location =
  if Int.of_string location = 0 then return (Ok [])
  else
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
    let Batch.{ commands; previous_location } =
      Binable.of_string (module Batch.Stable.Latest) data
    in
    (* FIXME: make this tail recursion somehow *)
    let%bind.Deferred.Result previous =
      get_batches config ~location:previous_location
    in
    return (Ok (previous @ commands))
