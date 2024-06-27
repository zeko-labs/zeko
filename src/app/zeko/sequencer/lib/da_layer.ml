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

module Genesis_state = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Account.Stable.V2.t list [@@deriving to_yojson]

      let to_latest = Fn.id
    end
  end]
end

let get_genesis_accounts (config : Config.t) =
  let%bind.Deferred.Result data =
    Da_utils.get_genesis_state ~da_websocket:config.da_websocket
      ~da_contract_address:config.da_contract_address
  in
  let accounts = Binable.of_string (module Genesis_state.Stable.Latest) data in
  return (Ok accounts)

let init_genesis_accounts (config : Config.t)
    ~(genesis_accounts : Account.t list) =
  let data =
    Binable.to_string (module Genesis_state.Stable.Latest) genesis_accounts
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
      [@@deriving to_yojson]

      let to_latest = Fn.id
    end
  end]
end

let post_batch (config : Config.t) ~previous_location ~commands ~source_ledger
    ~target_ledger =
  let fee_payers = List.map commands ~f:User_command.fee_payer in
  let receipt_chain_hashes =
    List.map fee_payers ~f:(fun account_id ->
        let index = Sparse_ledger.find_index_exn source_ledger account_id in
        let account = Sparse_ledger.get_exn source_ledger index in
        (account_id, Account.Poly.receipt_chain_hash account) )
  in
  let t =
    Batch.Stable.V1.
      { previous_location; receipt_chain_hashes; commands; target_ledger }
  in
  let sig_data =
    [ Ledger_hash.to_decimal_string @@ Sparse_ledger.merkle_root target_ledger ]
  in
  let batch_data = Binable.to_string (module Batch.Stable.Latest) t in
  Da_utils.post_batch ~da_websocket:config.da_websocket
    ~da_contract_address:config.da_contract_address
    ~da_private_key:config.da_private_key ~batch_data ~sig_data

let rec get_batches (config : Config.t) ~location =
  let%bind.Deferred.Result data =
    Da_utils.get_batch_data ~da_websocket:config.da_websocket
      ~da_contract_address:config.da_contract_address ~location
  in

  let Batch.{ commands; previous_location } =
    Binable.of_string (module Batch.Stable.Latest) data
  in
  (* FIXME: make this tail recursion somehow *)
  let%bind.Deferred.Result previous =
    get_batches config ~location:previous_location
  in
  return (Ok (previous @ commands))

(*
   module Config = struct
     type t = { da_contract_address : string option }
   end

   let command_to_yojson command =
     match command with
     | User_command.Signed_command signed_command ->
         `Assoc
           [ ("commandType", `Int 0)
           ; ("data", `String (Signed_command.to_base64 signed_command))
           ]
     | User_command.Zkapp_command zkapp_command ->
         `Assoc
           [ ("commandType", `Int 1)
           ; ("data", `String (Zkapp_command.to_base64 zkapp_command))
           ]

   let post_batch (config : Config.t) ~commands ~batch_id ~previous_batch_id =
     Utils.retry
       ~f:(fun () ->
         match config.da_contract_address with
         | None ->
             print_endline
               "No da contract address provided. Skipping batch posting" ;
             return (Ok ())
         | Some da_contract_address ->
             let payload =
               Yojson.to_string
                 (`Assoc
                   [ ("address", `String da_contract_address)
                   ; ("id", `String batch_id)
                   ; ("previousId", `String previous_batch_id)
                   ; ("commands", `List (List.map commands ~f:command_to_yojson))
                   ] )
             in
             let postBatchCmd =
               match Sys.getenv "DA_POST_BATCH" with
               | Some cmd ->
                   cmd
               | None ->
                   "exec $cd ../da-layer && npx hardhat run scripts/postBatch.ts"
             in
             let stdin = Core.Unix.open_process_out postBatchCmd in
             let pid = UnixLabels.process_out_pid stdin in
             Out_channel.output_string stdin payload ;
             Out_channel.close stdin ;
             Unix.waitpid (Pid.of_int pid) )
       ()

   let get_batches (config : Config.t) ~to_ =
     match config.da_contract_address with
     | None ->
         print_endline "No da contract address provided. Skipping bootstrapping" ;
         return []
     | Some da_contract_address ->
         let payload =
           Yojson.to_string
             (`Assoc
               [ ("address", `String da_contract_address); ("to", `String to_) ] )
         in
         let getBatchesCmd =
           match Sys.getenv "DA_GET_BATCHES" with
           | Some cmd ->
               cmd
           | None ->
               "exec $cd ../da-layer && npx hardhat run scripts/getBatches.ts"
         in
         let stdout, stdin = Core.Unix.open_process getBatchesCmd in
         let pid = UnixLabels.process_pid (stdout, stdin) in
         Out_channel.output_string stdin payload ;
         Out_channel.close stdin ;
         let result = In_channel.input_all stdout in
         In_channel.close stdout ;
         let%bind () = Unix.waitpid_exn (Pid.of_int pid) in
         let commands =
           Yojson.Safe.Util.(
             to_list @@ Yojson.Safe.from_string result
             |> List.map ~f:(fun json ->
                    match member "commandType" json |> to_int with
                    | 0 ->
                        let data = member "data" json |> to_string in
                        let signed_command =
                          Signed_command.of_base64 data |> ok_exn
                        in
                        User_command.Signed_command signed_command
                    | 1 ->
                        let data = member "data" json |> to_string in
                        let zkapp_command =
                          Zkapp_command.of_base64 data |> ok_exn
                        in
                        User_command.Zkapp_command zkapp_command
                    | _ ->
                        failwith "Unknown command type" ))
         in
         return commands *)
