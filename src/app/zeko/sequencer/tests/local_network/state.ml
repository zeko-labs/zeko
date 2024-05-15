open Core
open Async
open Async_kernel
open Mina_base
open Mina_transaction
open Network_pool
module Ledger = Mina_ledger.Ledger

let logger = Logger.create ()

module Constants = struct
  let constraint_constants = Genesis_constants.Constraint_constants.compiled

  let genesis_constants = Genesis_constants.compiled

  let consensus_constants =
    Consensus.Constants.create ~constraint_constants
      ~protocol_constants:genesis_constants.protocol

  let state_body =
    let compile_time_genesis =
      Mina_state.Genesis_protocol_state.t
        ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
        ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
        ~constraint_constants ~consensus_constants
        ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference
    in
    Mina_state.Protocol_state.body compile_time_genesis.data
end

type t =
  { block_period : Time_ns.Span.t option
  ; mutable block_height : int
  ; db : Ledger.Db.t
  ; commands : (string, User_command.t * Transaction_status.t) Hashtbl.t
  ; mutable pool : Indexed_pool.t
  }

let db t = t.db

let commands t = t.commands

let pooled_commands t = Indexed_pool.transactions ~logger t.pool

let apply_command t ~command =
  let l = Ledger.of_database t.db in
  let%bind.Result partialy_applied_txn =
    Ledger.apply_transaction_first_pass
      ~constraint_constants:Constants.constraint_constants
      ~global_slot:Mina_numbers.Global_slot_since_genesis.zero
      ~txn_state_view:(Mina_state.Protocol_state.Body.view Constants.state_body)
      l (Command command)
  in
  let%bind.Result txn_applied =
    Ledger.apply_transaction_second_pass l partialy_applied_txn
  in

  Ledger.Mask.Attached.commit l ;

  let txn_hash =
    Transaction_hash.to_base58_check @@ Transaction_hash.hash_command command
  in
  Hashtbl.add_exn t.commands ~key:txn_hash
    ~data:(command, Ledger.Transaction_applied.transaction_status txn_applied) ;

  print_endline @@ "applied zkapp command: " ^ txn_hash ^ " "
  ^ Yojson.Safe.pretty_to_string @@ Transaction_status.to_yojson
  @@ Ledger.Transaction_applied.transaction_status txn_applied ;

  Ok ()

let get_account t account_id =
  let%bind.Option location = Ledger.Db.location_of_account t.db account_id in
  Ledger.Db.get t.db location

let add_command_to_pool t ~(command : User_command.Valid.t) =
  match t.block_period with
  | None -> (
      match apply_command t ~command:(User_command.forget_check command) with
      | Ok () ->
          `Applied
      | Error err ->
          `Failed err )
  | Some _ -> (
      match
        get_account t
          (User_command.fee_payer @@ User_command.forget_check command)
      with
      | None ->
          `Failed (Error.of_string "fee payer account not found")
      | Some account -> (
          let nonce = account.nonce in
          let balance = account.balance in
          match
            Indexed_pool.add_from_gossip_exn t.pool
              (Transaction_hash.User_command_with_valid_signature.create command)
              nonce
              (Currency.Balance.to_amount balance)
          with
          | Error err ->
              `Failed
                ( Error.of_string @@ Yojson.Safe.to_string
                @@ Command_error.to_yojson err )
          | Ok (_, pool, _) ->
              t.pool <- pool ;
              print_endline "added command to pool" ;
              `Enqueued ) )

let create_pool () =
  Indexed_pool.empty ~constraint_constants:Constants.constraint_constants
    ~consensus_constants:Constants.consensus_constants
    ~time_controller:(Block_time.Controller.basic ~logger)
    ~slot_tx_end:None

let create_new_block t =
  t.block_height <- t.block_height + 1 ;
  printf "Creating a new block %d\n%!" t.block_height ;
  let transactions = Indexed_pool.transactions ~logger t.pool in
  Sequence.iter transactions ~f:(fun txn ->
      let command =
        Transaction_hash.User_command_with_valid_signature.command txn
      in
      match apply_command t ~command with
      | Ok () ->
          ()
      | Error err ->
          printf "Failed to apply command: %s\n%!" err ) ;
  t.pool <- create_pool ()

let create ~block_period ~db_dir () =
  let t =
    { block_period
    ; block_height = 0
    ; db =
        Ledger.Db.create ~directory_name:db_dir
          ~depth:Constants.constraint_constants.ledger_depth ()
    ; commands = Hashtbl.create (module String)
    ; pool = create_pool ()
    }
  in
  match block_period with
  | None ->
      t
  | Some block_period ->
      every ~start:(after block_period) block_period (fun () ->
          create_new_block t ) ;
      t
