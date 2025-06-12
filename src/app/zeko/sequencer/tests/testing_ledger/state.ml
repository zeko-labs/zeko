open Core
open Async_kernel
open Mina_base
open Mina_transaction
open Network_pool
open Sequencer_lib
module Ledger = Mina_ledger.Ledger

module Constants = struct
  let constraint_constants = Zeko_constants.constraint_constants

  let consensus_constants =
    let protocol_constants : Genesis_constants.Protocol.t =
      { k = 1
      ; slots_per_epoch = 1000
      ; slots_per_sub_window = 1
      ; grace_period_slots = 1
      ; delta = 1
      ; genesis_state_timestamp = Int64.one
      }
    in
    Consensus.Constants.create ~constraint_constants ~protocol_constants

  let compile_time_genesis =
    Mina_state.Genesis_protocol_state.t
      ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
      ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
      ~constraint_constants ~consensus_constants
      ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference
end

type t =
  { block_period : Time_ns.Span.t option
  ; mutable block_height : int
  ; db : Ledger.Db.t
  ; commands : (string, User_command.t * Transaction_status.t) Hashtbl.t
  ; mutable pool : Indexed_pool.t
  ; archive : Archive.t
  ; signature_kind : Mina_signature_kind.t
  ; disable_proofs : bool
  ; logger : Logger.t
  }

let db t = t.db

let commands t = t.commands

let pooled_commands t = Indexed_pool.transactions ~logger:t.logger t.pool

let apply_command t ~command =
  let logger = t.logger in
  let l = Ledger.of_database t.db in
  let%bind.Result partialy_applied_txn =
    Ledger.apply_transaction_first_pass
      ~constraint_constants:Constants.constraint_constants
      ~global_slot:Mina_numbers.Global_slot_since_genesis.zero
      ~txn_state_view:
        (Mina_state.Protocol_state.Body.view
           Constants.compile_time_genesis.data.body )
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
    ~data:
      ( command
      , Mina_transaction_logic.Transaction_applied.transaction_status
          txn_applied ) ;

  let status =
    Mina_transaction_logic.Transaction_applied.transaction_status txn_applied
  in
  [%log info] !"Applied command: %{sexp: Transaction_status.t\n}" status ;

  let () =
    match (status, command) with
    | Applied, Zkapp_command zkapp_command ->
        Zkapp_command.(
          Call_forest.iteri (account_updates zkapp_command) ~f:(fun _ update ->
              let account =
                let account_id =
                  Account_id.create
                    (Account_update.public_key update)
                    (Account_update.token_id update)
                in
                Option.(
                  map
                    (Ledger.location_of_account l account_id)
                    ~f:(Ledger.get l)
                  |> join |> value_exn)
              in
              Archive.add_account_update t.archive ~height:t.block_height update
                account
                (Some
                   Archive.Transaction_info.
                     { status = Applied
                     ; hash =
                         Mina_transaction.Transaction_hash.hash_command
                           (Zkapp_command zkapp_command)
                     ; memo = Zkapp_command.memo zkapp_command
                     ; authorization_kind =
                         Account_update.Body.authorization_kind
                         @@ Account_update.body update
                     } ) ))
    | _ ->
        ()
  in

  Ok ()

let get_account t account_id =
  let%bind.Option location = Ledger.Db.location_of_account t.db account_id in
  Ledger.Db.get t.db location

let add_command_to_pool t ~(command : User_command.Valid.t) =
  let logger = t.logger in
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
          let command =
            Transaction_hash.User_command_with_valid_signature.create command
          in
          match
            Indexed_pool.add_from_gossip_exn t.pool command nonce
              (Currency.Balance.to_amount balance)
          with
          | Error err ->
              `Failed
                ( Error.of_string @@ Yojson.Safe.to_string
                @@ Command_error.to_yojson err )
          | Ok (_, pool, _) ->
              t.pool <- pool ;
              [%log info] "added command to pool: %s"
                Transaction_hash.(
                  to_base58_check
                  @@ User_command_with_valid_signature.hash command) ;
              `Enqueued ) )

let create_pool ~logger () =
  Indexed_pool.empty ~constraint_constants:Constants.constraint_constants
    ~consensus_constants:Constants.consensus_constants
    ~time_controller:(Block_time.Controller.basic ~logger)
    ~slot_tx_end:None

let clear_pool t = t.pool <- create_pool ~logger:t.logger ()

let create_new_block t =
  let logger = t.logger in
  t.block_height <- t.block_height + 1 ;
  [%log info] "Creating a new block %d" t.block_height ;
  let transactions = Indexed_pool.transactions ~logger t.pool in
  Sequence.iter transactions ~f:(fun txn ->
      let command =
        Transaction_hash.User_command_with_valid_signature.command txn
      in
      match apply_command t ~command with
      | Ok () ->
          ()
      | Error err ->
          [%log error] "Failed to apply command %s: %s"
            ( Transaction_hash.to_base58_check
            @@ Transaction_hash.hash_command command )
            (Error.to_string_hum err) ) ;
  clear_pool t

let create ~logger ~disable_proofs ~block_period ~db_dir ~signature_kind () =
  let db =
    Ledger.Db.create ~directory_name:db_dir
      ~depth:Constants.constraint_constants.ledger_depth ()
  in
  let t =
    { block_period
    ; block_height = 0
    ; db
    ; commands = Hashtbl.create (module String)
    ; pool = create_pool ~logger ()
    ; archive = Sequencer_lib.Archive.create ~kvdb:(Ledger.Db.zeko_kvdb db)
    ; signature_kind
    ; disable_proofs
    ; logger
    }
  in
  match block_period with
  | None ->
      t
  | Some block_period ->
      every ~start:(after block_period) block_period (fun () ->
          create_new_block t ) ;
      t
