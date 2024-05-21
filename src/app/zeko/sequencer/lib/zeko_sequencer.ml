open Base
open Core_kernel
open Async
open Async_kernel
open Mina_base
module L = Mina_ledger.Ledger

module Test_accounts = struct
  type t = { pk : string; balance : int64 } [@@deriving yojson]

  let parse_accounts_exn ~test_accounts_path : (Account_id.t * Account.t) list =
    let accounts =
      Yojson.Safe.(
        from_file test_accounts_path
        |> Util.to_list
        |> List.map ~f:(fun t ->
               match of_yojson t with
               | Ppx_deriving_yojson_runtime.Result.Ok t ->
                   t
               | Ppx_deriving_yojson_runtime.Result.Error e ->
                   failwith e ))
    in
    List.map accounts ~f:(fun { pk; balance } ->
        let account_id =
          Account_id.create
            (Signature_lib.Public_key.Compressed.of_base58_check_exn pk)
            Token_id.default
        in
        let account =
          Account.create account_id
            (Currency.Balance.of_uint64 (Unsigned.UInt64.of_int64 balance))
        in
        (account_id, account) )
end

module Sequencer = struct
  module Config = struct
    type t =
      { max_pool_size : int
      ; commitment_period_sec : float
      ; db_dir : string option
      ; zkapp_pk : Signature_lib.Public_key.Compressed.t
      ; signer : Signature_lib.Keypair.t
      ; l1_uri : Uri.t Cli_lib.Flag.Types.with_name
      }
  end

  module Transfer = struct
    type direction = Deposit | Withdraw

    type t =
      { address : Account.key
      ; amount : Unsigned.UInt64.t
      ; direction : direction
      }
  end

  let constraint_constants = Genesis_constants.Constraint_constants.compiled

  let genesis_constants = Genesis_constants.compiled

  let compile_time_genesis_state =
    let consensus_constants =
      Consensus.Constants.create ~constraint_constants
        ~protocol_constants:genesis_constants.protocol
    in
    let compile_time_genesis =
      Mina_state.Genesis_protocol_state.t
        ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
        ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
        ~constraint_constants ~consensus_constants
        ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference
    in
    compile_time_genesis.data

  module T = Transaction_snark.Make (struct
    let constraint_constants = constraint_constants

    let proof_level = Genesis_constants.Proof_level.Full
  end)

  module M = Zkapps_rollup.Make (T)

  let keypair = Signature_lib.Keypair.create ()

  let sok_digest =
    Sok_message.digest
    @@ Sok_message.create ~fee:Currency.Fee.zero
         ~prover:(Signature_lib.Public_key.compress keypair.public_key)

  module Snark_queue = struct
    module Command_witness = struct
      type t =
        | Signed_command of
            Mina_ledger.Sparse_ledger.t
            * Signed_command.With_valid_signature.t Transaction_protocol_state.t
            * Transaction_snark.Statement.With_sok.t
        | Zkapp_command of
            ( Transaction_witness.Zkapp_command_segment_witness.t
            * Transaction_snark.Zkapp_command_segment.Basic.t
            * Mina_state.Snarked_ledger_state.With_sok.t )
            list
            * Zkapp_command.t
      [@@deriving yojson]
    end

    module State = struct
      type t =
        { last : Zkapps_rollup.t option
        ; staged_commands : User_command.t list
        ; queued_commands : Command_witness.t list
        ; previous_committed_ledger_hash : Frozen_ledger_hash.t option
        ; previous_committed_ledger : Mina_ledger.Sparse_ledger.t option
        }
      [@@deriving yojson, fields]

      let create () =
        { last = None
        ; staged_commands = []
        ; queued_commands = []
        ; previous_committed_ledger_hash = None
        ; previous_committed_ledger = None
        }

      let set_last t last = { t with last = Some last }

      let add_staged_command t command =
        { t with staged_commands = t.staged_commands @ [ command ] }

      let add_queued_command t command =
        { t with queued_commands = t.queued_commands @ [ command ] }

      let pop_queued_command t =
        match t.queued_commands with
        | [] ->
            t
        | _ :: rest ->
            { t with queued_commands = rest }

      let clear_staged_commands t = { t with staged_commands = [] }

      let clear_queued_commands t = { t with queued_commands = [] }

      let reset_for_new_batch t previous_ledger =
        { t with
          last = None
        ; staged_commands = []
        ; previous_committed_ledger_hash =
            Some (Mina_ledger.Sparse_ledger.merkle_root previous_ledger)
        ; previous_committed_ledger = Some previous_ledger
        }
    end

    type t =
      { q : unit Throttle.t
      ; da_config : Da_layer.Config.t
      ; config : Config.t
      ; transfers_memory : Transfers_memory.t
      ; executor : Executor.t
      ; kvdb : Kvdb.t
      ; mutable state : State.t
      }

    let create ~da_config ~config ~signer ~kvdb =
      { q = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1
      ; da_config
      ; config
      ; transfers_memory = Transfers_memory.create ~lifetime:Float.(60. * 10.)
      ; executor = Executor.create ~l1_uri:config.l1_uri ~signer ~kvdb ()
      ; kvdb
      ; state = State.create ()
      }

    let queue_size t = Throttle.num_jobs_waiting_to_start t.q

    let persist_state t () =
      Kvdb.set t.kvdb ~key:SNARK_QUEUE_STATE
        ~data:
          ( Bigstring.of_string @@ Yojson.Safe.to_string
          @@ State.to_yojson t.state )

    let get_state ~kvdb =
      let%bind.Option data = Kvdb.get kvdb ~key:SNARK_QUEUE_STATE in
      match
        State.of_yojson @@ Yojson.Safe.from_string @@ Bigstring.to_string data
      with
      | Ok state ->
          Some state
      | Error e ->
          failwith e

    let wrap_and_merge t txn_snark command =
      let%bind wrapped = M.Wrapper.wrap txn_snark in
      let%bind final_snark =
        match t.state.last with
        | Some last' ->
            M.Wrapper.merge last' wrapped
        | None ->
            return wrapped
      in
      t.state <- State.set_last t.state final_snark ;
      t.state <- State.add_staged_command t.state command ;
      t.state <- State.pop_queued_command t.state ;
      return ()

    let prove_signed_command t ~sparse_ledger ~user_command_in_block ~statement
        =
      let handler =
        unstage @@ Mina_ledger.Sparse_ledger.handler sparse_ledger
      in
      let%bind txn_snark =
        Utils.time "Transaction_snark.of_signed_command"
          (T.of_user_command ~init_stack:Mina_base.Pending_coinbase.Stack.empty
             ~statement user_command_in_block handler )
      in
      wrap_and_merge t txn_snark
        (User_command.Signed_command
           (Signed_command.forget_check user_command_in_block.transaction) )

    let prove_zkapp_command t ~witnesses ~zkapp_command =
      let%bind txn_snark =
        match witnesses with
        | [] ->
            failwith "No witnesses"
        | (witness, spec, statement) :: rest ->
            let%bind p1 =
              Utils.time "Transaction_snark.of_zkapp_command_segment"
                (T.of_zkapp_command_segment_exn ~statement ~witness ~spec)
            in
            Deferred.List.fold ~init:p1 rest
              ~f:(fun acc (witness, spec, statement) ->
                let%bind prev = return acc in
                let%bind curr =
                  Utils.time "Transaction_snark.of_zkapp_command_segment"
                    (T.of_zkapp_command_segment_exn ~statement ~witness ~spec)
                in
                let%bind merged =
                  Utils.time "Transaction_snark.merge"
                    (T.merge curr prev ~sok_digest)
                in
                return (Or_error.ok_exn merged) )
      in
      wrap_and_merge t txn_snark (User_command.Zkapp_command zkapp_command)

    let enqueue t f =
      Throttle.enqueue t.q (fun () ->
          let%map result = f () in
          let () = persist_state t () in
          result )

    let enqueue_prove_command t command_witness =
      t.state <- State.add_queued_command t.state command_witness ;
      persist_state t () ;
      enqueue t (fun () ->
          match command_witness with
          | Command_witness.Signed_command
              (sparse_ledger, user_command_in_block, statement) ->
              prove_signed_command t ~sparse_ledger ~user_command_in_block
                ~statement
          | Command_witness.Zkapp_command (witnesses, zkapp_command) ->
              prove_zkapp_command t ~witnesses ~zkapp_command )

    let enqueue_prove_transfer t ~key ~(transfer : Transfer.t) =
      enqueue t (fun () ->
          let%bind tree =
            match transfer.direction with
            | Deposit ->
                M.Outer.submit_deposit ~outer_public_key:t.config.zkapp_pk
                  ~deposit:
                    { amount = Currency.Amount.of_uint64 transfer.amount
                    ; recipient = transfer.address
                    }
            | Withdraw ->
                M.Inner.submit_withdrawal
                  ~withdrawal:
                    { amount = Currency.Amount.of_uint64 transfer.amount
                    ; recipient = transfer.address
                    }
          in
          Transfers_memory.add t.transfers_memory key
            (Zkapp_command.Call_forest.cons_tree tree []) ;
          return () )

    let enqueue_prove_commit t ~target_ledger =
      enqueue t (fun () ->
          match List.is_empty t.state.staged_commands with
          | true ->
              print_endline "Nothing to commit" ;
              return ()
          | false -> (
              print_endline "Committing..." ;

              let ledger_hash =
                Zkapps_rollup.target_ledger @@ Option.value_exn t.state.last
              in
              let batch_id =
                Frozen_ledger_hash0.to_decimal_string ledger_hash
              in
              let previous_batch_id =
                Frozen_ledger_hash0.to_decimal_string
                  (Option.value
                     ~default:(Frozen_ledger_hash0.of_decimal_string "0")
                     t.state.previous_committed_ledger_hash )
              in

              match%bind
                try_with (fun () ->
                    let%bind () =
                      match%bind
                        Da_layer.post_batch t.da_config
                          ~commands:t.state.staged_commands ~batch_id
                          ~previous_batch_id
                      with
                      | Ok _ ->
                          return ()
                      | Error e ->
                          failwith (Unix.Exit_or_signal.to_string_hum (Error e))
                    in
                    print_endline ("Posted batch " ^ batch_id) ;

                    (* FIXME: disable only for internal MVP *)
                    (* let%bind account_update, _, _ =
                         time "Outer.step"
                           (M.Outer.step (Option.value_exn t.last) ~public_key:t.zkapp_pk
                              ~old_action_state:Zkapp_account.Actions.empty_hash
                              ~new_actions:[]
                              ~withdrawals_processed:Zkapp_account.Actions.empty_hash
                              ~remaining_withdrawals:[]
                              ~source_ledger:
                                (Option.value_exn t.previous_committed_ledger)
                              ~target_ledger )
                       in *)
                    let old_inner_ledger =
                      Option.value_exn t.state.previous_committed_ledger
                    in
                    let new_inner_ledger = target_ledger in
                    let%bind account_update =
                      M.Outer.step
                        (Option.value_exn t.state.last)
                        ~outer_public_key:t.config.zkapp_pk ~new_deposits:[]
                        ~unprocessed_deposits:[] ~old_inner_ledger
                        ~new_inner_ledger
                    in
                    let command : Zkapp_command.t =
                      { fee_payer =
                          { Account_update.Fee_payer.body =
                              { public_key =
                                  Signature_lib.Public_key.compress
                                    t.executor.signer.public_key
                              ; fee = Currency.Fee.of_mina_int_exn 1
                              ; valid_until = None
                              ; nonce = Unsigned.UInt32.zero
                              }
                          ; authorization = Signature.dummy
                          }
                      ; account_updates =
                          Zkapp_command.Call_forest.cons_tree account_update []
                      ; memo = Signed_command_memo.empty
                      }
                    in
                    return @@ don't_wait_for
                    @@ Executor.send_commit t.executor command
                         ~source:
                           (Mina_ledger.Sparse_ledger.merkle_root
                              old_inner_ledger )
                         ~target:
                           (Mina_ledger.Sparse_ledger.merkle_root
                              new_inner_ledger ) )
              with
              | Ok _ ->
                  t.state <- State.reset_for_new_batch t.state target_ledger ;
                  return ()
              | Error e ->
                  (* Continue expanding batch if commit failed *)
                  print_endline ("Commit failed: " ^ Exn.to_string e) ;
                  return () ) )

    let wait_to_finish t = Throttle.capacity_available t.q
  end

  module Subscriptions = struct
    type t =
      { mutable transactions : string Pipe.Writer.t list
      ; mutable staged_archive_diffs :
          Archive_lib.Diff.Transition_frontier.t list
      }

    let create () = { transactions = []; staged_archive_diffs = [] }

    let add_staged_diff t diff =
      t.staged_archive_diffs <- t.staged_archive_diffs @ [ diff ]

    let clear_staged_diffs t = t.staged_archive_diffs <- []
  end

  type t =
    { db : L.Db.t
    ; archive : Archive.t
    ; config : Config.t
    ; da_config : Da_layer.Config.t
    ; snark_q : Snark_queue.t
    ; stop : unit Ivar.t
    ; genesis_accounts : (Account_id.t * Account.t) list
    ; mutable protocol_state : Mina_state.Protocol_state.value
    ; mutable subscriptions : Subscriptions.t
    }

  let persist_protocol_state t =
    let data =
      Bigstring.of_string @@ Yojson.Safe.to_string
      @@ Mina_state.Protocol_state.value_to_yojson t.protocol_state
    in
    Kvdb.(set (of_db t.db) ~key:PROTOCOL_STATE ~data)

  let load_protocol_state_exn t =
    let data =
      Kvdb.(get (of_db t.db) ~key:PROTOCOL_STATE) |> Option.value_exn
    in
    match
      Mina_state.Protocol_state.value_of_yojson @@ Yojson.Safe.from_string
      @@ Bigstring.to_string data
    with
    | Ok protocol_state ->
        t.protocol_state <- protocol_state
    | Error e ->
        failwith e

  let close t =
    L.Db.close t.db ;
    Ivar.fill_if_empty t.stop () ;
    Throttle.kill t.snark_q.q

  let add_account t account_id account =
    ( L.Db.get_or_create_account t.db account_id account |> Or_error.ok_exn
      : [ `Added | `Existed ] * L.Db.Location.t )
    |> ignore

  let get_account t public_key token_id =
    let account_id = Account_id.create public_key token_id in
    let%bind.Option location = L.Db.location_of_account t.db account_id in
    L.Db.get t.db location

  let get_global_slot t =
    Mina_state.Protocol_state.consensus_state t.protocol_state
    |> Consensus.Data.Consensus_state.global_slot_since_genesis

  let infer_nonce t public_key =
    match get_account t public_key Token_id.default with
    | Some account ->
        account.nonce
    | None ->
        Unsigned.UInt32.zero

  let get_root t = L.Db.merkle_root t.db

  let is_empty t = L.Db.num_accounts t.db = 0

  let dispatch_transaction t ~ledger ~accounts_created ~new_state_hash ~txn =
    let new_protocol_state, diff =
      Archive_lib.Diff.Builder.transaction_added ~constraint_constants
        ~accounts_created ~new_state_hash ~protocol_state:t.protocol_state
        ~ledger ~txn
    in
    t.protocol_state <- new_protocol_state ;
    persist_protocol_state t ;
    Subscriptions.add_staged_diff t.subscriptions diff ;
    match
      Base64.encode
      @@ Binable.to_string (module Archive_lib.Diff.Transition_frontier) diff
    with
    | Ok data ->
        List.iter t.subscriptions.transactions ~f:(fun w ->
            Pipe.write_without_pushback_if_open w data )
    | Error (`Msg e) ->
        print_endline e

  let apply_signed_command t (signed_command : Signed_command.t) =
    let%bind.Result () =
      if Snark_queue.queue_size t.snark_q >= t.config.max_pool_size then
        Error (Error.of_string "Maximum pool size reached, try later")
      else Ok ()
    in

    let%bind.Result with_valid_signature =
      match Signed_command.check_only_for_signature signed_command with
      | Some x ->
          Ok x
      | None ->
          Error (Error.of_string "Signature check failed")
    in
    let txn =
      Mina_transaction.Transaction.Command
        (User_command.Signed_command signed_command)
    in
    (* the protocol state from sequencer has dummy values which wouldn't pass the txn snark *)
    let global_slot = Mina_numbers.Global_slot_since_genesis.zero in
    let state_body =
      Mina_state.Protocol_state.body compile_time_genesis_state
    in
    let l = L.of_database t.db in
    let source_ledger_hash = L.merkle_root l in
    let sparse_ledger =
      Mina_ledger.Sparse_ledger.of_ledger_subset_exn l
        (Signed_command.accounts_referenced signed_command)
    in
    let%bind.Result txn_applied =
      Result.( >>= )
        (L.apply_transaction_first_pass ~constraint_constants ~global_slot
           ~txn_state_view:(Mina_state.Protocol_state.Body.view state_body)
           l (Command (Signed_command signed_command)) )
        (L.apply_transaction_second_pass l)
    in
    let%bind.Result txn_applied =
      match L.Transaction_applied.transaction_status txn_applied with
      | Failed failure ->
          Error
            ( Error.of_string @@ Yojson.Safe.to_string
            @@ Transaction_status.Failure.Collection.to_yojson failure )
      | Applied ->
          Ok txn_applied
    in

    L.Mask.Attached.commit l ;

    let target_ledger_hash = L.merkle_root l in
    let pc : Transaction_snark.Pending_coinbase_stack_state.t =
      (* No coinbase to add to the stack. *)
      let stack_with_state global_slot =
        Pending_coinbase.Stack.push_state
          (Mina_state.Protocol_state.Body.hash state_body)
          global_slot Pending_coinbase.Stack.empty
      in
      { source = stack_with_state global_slot
      ; target = stack_with_state global_slot
      }
    in
    let user_command_in_block =
      { Transaction_protocol_state.Poly.transaction = with_valid_signature
      ; block_data = state_body
      ; global_slot
      }
    in
    let (statement : Transaction_snark.Statement.With_sok.t) =
      Transaction_snark.Statement.Poly.with_empty_local_state
        ~source_first_pass_ledger:source_ledger_hash
        ~target_first_pass_ledger:target_ledger_hash
        ~source_second_pass_ledger:target_ledger_hash
        ~target_second_pass_ledger:target_ledger_hash
        ~connecting_ledger_left:target_ledger_hash
        ~connecting_ledger_right:target_ledger_hash ~sok_digest
        ~fee_excess:
          (Mina_transaction.Transaction.fee_excess txn |> Or_error.ok_exn)
        ~supply_increase:
          (L.Transaction_applied.supply_increase txn_applied |> Or_error.ok_exn)
        ~pending_coinbase_stack_state:pc
    in

    dispatch_transaction t ~ledger:l
      ~accounts_created:(L.Transaction_applied.new_accounts txn_applied)
      ~new_state_hash:target_ledger_hash
      ~txn:(L.Transaction_applied.transaction txn_applied) ;

    Result.return
      ( txn_applied
      , Snark_queue.Command_witness.Signed_command
          (sparse_ledger, user_command_in_block, statement) )

  let apply_zkapp_command t (zkapp_command : Zkapp_command.t) =
    let%bind.Result () =
      if Snark_queue.queue_size t.snark_q >= t.config.max_pool_size then
        Error (Error.of_string "Maximum pool size reached, try later")
      else Ok ()
    in
    (* the protocol state from sequencer has dummy values which wouldn't pass the txn snark *)
    let global_slot = Mina_numbers.Global_slot_since_genesis.zero in
    let state_body =
      Mina_state.Protocol_state.body compile_time_genesis_state
    in
    let l = L.of_database t.db in
    let%bind.Result first_pass_ledger, second_pass_ledger, txn_applied =
      let accounts_referenced =
        Zkapp_command.accounts_referenced zkapp_command
      in

      let first_pass_ledger =
        Mina_ledger.Sparse_ledger.of_ledger_subset_exn l accounts_referenced
      in
      let%bind.Result partialy_applied_txn =
        L.apply_transaction_first_pass ~constraint_constants ~global_slot
          ~txn_state_view:(Mina_state.Protocol_state.Body.view state_body)
          l (Command (Zkapp_command zkapp_command))
      in

      let second_pass_ledger =
        Mina_ledger.Sparse_ledger.of_ledger_subset_exn l accounts_referenced
      in
      let%map.Result txn_applied =
        let%bind.Result txn_applied =
          L.apply_transaction_second_pass l partialy_applied_txn
        in
        match L.Transaction_applied.transaction_status txn_applied with
        | Failed failure ->
            Error
              ( Error.of_string @@ Yojson.Safe.to_string
              @@ Transaction_status.Failure.Collection.to_yojson failure )
        | Applied ->
            Ok txn_applied
      in

      L.Mask.Attached.commit l ;

      Zkapp_command.(
        Call_forest.iteri (account_updates zkapp_command) ~f:(fun _ update ->
            let account =
              Option.value_exn
              @@ get_account t
                   (Account_update.public_key update)
                   (Account_update.token_id update)
            in
            Archive.add_account_update t.archive update account
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
                   } ) )) ;

      (first_pass_ledger, second_pass_ledger, txn_applied)
    in

    let pc : Transaction_snark.Pending_coinbase_stack_state.t =
      (* No coinbase to add to the stack. *)
      let stack_with_state global_slot =
        Pending_coinbase.Stack.push_state
          (Mina_state.Protocol_state.Body.hash state_body)
          global_slot Pending_coinbase.Stack.empty
      in
      { source = stack_with_state global_slot
      ; target = stack_with_state global_slot
      }
    in
    let witnesses =
      Transaction_snark.zkapp_command_witnesses_exn ~constraint_constants
        ~global_slot ~state_body
        ~fee_excess:
          ( Currency.Amount.Signed.of_unsigned
          @@ Currency.Amount.of_fee (Zkapp_command.fee zkapp_command) )
        [ ( `Pending_coinbase_init_stack Pending_coinbase.Stack.empty
          , `Pending_coinbase_of_statement pc
          , `Sparse_ledger first_pass_ledger
          , `Sparse_ledger second_pass_ledger
          , `Connecting_ledger_hash
              (Mina_ledger.Sparse_ledger.merkle_root second_pass_ledger)
          , zkapp_command )
        ]
    in

    dispatch_transaction t ~ledger:l
      ~accounts_created:(L.Transaction_applied.new_accounts txn_applied)
      ~new_state_hash:(L.merkle_root l)
      ~txn:(L.Transaction_applied.transaction txn_applied) ;

    Result.return
      ( txn_applied
      , Snark_queue.Command_witness.Zkapp_command (witnesses, zkapp_command) )

  let apply_deposits t =
    let%bind inner_account_update =
      M.Inner.step ~all_deposits:Zkapp_account.Actions.empty_state_element
    in
    let fee = Currency.Fee.of_mina_int_exn 0 in
    let command : Zkapp_command.t =
      { fee_payer =
          (* Setting public_key to empty results in a dummy fee payer with public key near 123456789 (dumb). *)
          (* FIXME: Do this a better way without hard-coding values. *)
          { Account_update.Fee_payer.body =
              { public_key = Signature_lib.Public_key.Compressed.empty
              ; fee
              ; valid_until = None
              ; nonce = Account.Nonce.zero
              }
          ; authorization = Signature.dummy
          }
      ; account_updates =
          Zkapp_command.Call_forest.cons_tree inner_account_update []
      ; memo = Signed_command_memo.empty
      }
    in
    let _, command_witness =
      Result.ok_exn
      @@ Result.map_error ~f:Error.to_exn
      @@ apply_zkapp_command t command
    in
    Snark_queue.enqueue_prove_command t.snark_q command_witness

  let commit t =
    (* FIXME: disable only for internal MVP *)
    (* let%bind () = apply_deposits t in *)
    let target_ledger =
      Mina_ledger.Sparse_ledger.of_ledger_subset_exn
        L.(of_database t.db)
        [ M.Inner.account_id ]
    in
    Subscriptions.clear_staged_diffs t.subscriptions ;
    Snark_queue.enqueue_prove_commit t.snark_q ~target_ledger

  let run_committer t =
    if Float.(t.config.commitment_period_sec <= 0.) then ()
    else
      let period = Time_ns.Span.of_sec t.config.commitment_period_sec in
      every ~start:(after period) ~stop:(Ivar.read t.stop) period (fun () ->
          don't_wait_for @@ commit t )

  let add_transactions_subscriber t =
    let r, w = Pipe.create () in
    List.iter t.subscriptions.staged_archive_diffs ~f:(fun diff ->
        match
          Base64.encode
          @@ Binable.to_string
               (module Archive_lib.Diff.Transition_frontier)
               diff
        with
        | Ok data ->
            Pipe.write_without_pushback_if_open w data
        | Error (`Msg e) ->
            print_endline e ) ;
    t.subscriptions.transactions <- w :: t.subscriptions.transactions ;
    r

  let bootstrap ({ config; da_config; genesis_accounts; snark_q; _ } as t) =
    let%bind committed_ledger_hash =
      Gql_client.infer_committed_state config.l1_uri ~zkapp_pk:config.zkapp_pk
        ~signer_pk:(Signature_lib.Public_key.compress config.signer.public_key)
    in
    printf "Fetched root: %s\n%!"
      Frozen_ledger_hash.(to_decimal_string committed_ledger_hash) ;

    (* add initial accounts *)
    List.iter genesis_accounts ~f:(fun (account_id, account) ->
        add_account t account_id account ) ;

    printf "Init root: %s\n%!"
      Frozen_ledger_hash.(to_decimal_string (get_root t)) ;

    (* apply commands from DA layer *)
    let%bind commands =
      Da_layer.get_batches da_config
        ~to_:(Frozen_ledger_hash.to_decimal_string committed_ledger_hash)
    in
    printf "Applying %d commands\n%!" (List.length commands) ;
    List.iter commands ~f:(fun command ->
        match command with
        | User_command.Signed_command signed_command ->
            let _res =
              apply_signed_command t signed_command |> Or_error.ok_exn
            in
            ()
        | User_command.Zkapp_command zkapp_command ->
            let _res = apply_zkapp_command t zkapp_command |> Or_error.ok_exn in
            () ) ;

    let current_root = get_root t in
    printf "Current root: %s\n%!"
      Frozen_ledger_hash.(to_decimal_string current_root) ;

    if not @@ Frozen_ledger_hash.equal current_root committed_ledger_hash then
      print_endline "Ledger mismatch" ;

    let sparse_ledger =
      Mina_ledger.Sparse_ledger.of_ledger_subset_exn
        L.(of_database t.db)
        [ M.Inner.account_id ]
    in
    t.snark_q.state <-
      Snark_queue.State.reset_for_new_batch t.snark_q.state sparse_ledger ;
    return ()

  let create ~zkapp_pk ~max_pool_size ~commitment_period_sec
      ~da_contract_address ~db_dir ~l1_uri ~signer ~test_accounts_path =
    let db =
      L.Db.create ?directory_name:db_dir
        ~depth:constraint_constants.ledger_depth ()
    in
    let da_config : Da_layer.Config.t = { da_contract_address } in
    let genesis_accounts =
      [ (M.Inner.account_id, M.Inner.initial_account) ]
      @
      match test_accounts_path with
      | Some test_accounts_path ->
          print_endline "Adding test accounts" ;
          Test_accounts.parse_accounts_exn ~test_accounts_path
      | None ->
          print_endline "No test accounts" ;
          []
    in
    let config =
      Config.
        { max_pool_size
        ; commitment_period_sec
        ; db_dir
        ; l1_uri
        ; zkapp_pk
        ; signer
        }
    in
    let t =
      { db
      ; archive = Archive.create ~kvdb:(L.Db.kvdb db)
      ; config
      ; da_config
      ; snark_q =
          Snark_queue.create ~da_config ~config ~signer ~kvdb:(L.Db.kvdb db)
      ; stop = Ivar.create ()
      ; genesis_accounts
      ; protocol_state = compile_time_genesis_state
      ; subscriptions = Subscriptions.create ()
      }
    in
    let%bind () =
      if is_empty t then bootstrap t
      else (
        load_protocol_state_exn t ;

        Snark_queue.get_state ~kvdb:(L.Db.kvdb db)
        |> Option.iter ~f:(fun state -> t.snark_q.state <- state) ;

        printf "Staged %d commands \n%!"
          (List.length t.snark_q.state.staged_commands) ;
        printf "Requeueing %d commands\n%!"
          (List.length t.snark_q.state.queued_commands) ;

        let queued_commands = t.snark_q.state.queued_commands in
        (* enqueue will requeue also state *)
        t.snark_q.state <-
          Snark_queue.State.clear_queued_commands t.snark_q.state ;
        List.iter queued_commands ~f:(fun command_witness ->
            don't_wait_for
            @@ Snark_queue.enqueue_prove_command t.snark_q command_witness ) ;

        return () )
    in
    let%bind () =
      Executor.recommit_all t.snark_q.executor ~zkapp_pk:config.zkapp_pk
    in
    return t
end

include Sequencer

let%test_unit "apply commands and commit" =
  Base.Backtrace.elide := false ;
  let number_of_transactions = 5 in
  let zkapp_keypair = Signature_lib.Keypair.create () in
  let gql_uri =
    { Cli_lib.Flag.Types.value = Uri.of_string "http://localhost:8080/graphql"
    ; name = "gql-uri"
    }
  in

  (* Create signer *)
  let signer = Signature_lib.Keypair.create () in
  Thread_safe.block_on_async_exn (fun () ->
      let%bind _res =
        Gql_client.For_tests.create_account gql_uri
          (Signature_lib.Public_key.compress signer.public_key)
      in
      return () ) ;

  let open Mina_transaction_logic.For_tests in
  Quickcheck.test ~trials:1
    (Test_spec.mk_gen ~num_transactions:number_of_transactions ())
    ~f:(fun { init_ledger; specs } ->
      let add_test_accounts ~f =
        Array.iter init_ledger ~f:(fun (keypair, balance) ->
            let pk = Signature_lib.Public_key.compress keypair.public_key in
            let account_id = Account_id.create pk Token_id.default in
            let balance = Unsigned.UInt64.of_int64 balance in
            let account =
              Account.create account_id (Currency.Balance.of_uint64 balance)
            in
            f account_id account )
      in
      let batch1, batch2 = List.split_n specs 3 in

      L.with_ledger ~depth:constraint_constants.ledger_depth
        ~f:(fun expected_ledger ->
          (* Init expected ledger *)
          L.create_new_account_exn expected_ledger M.Inner.account_id
            M.Inner.initial_account ;
          add_test_accounts ~f:(L.create_new_account_exn expected_ledger) ;

          (* Deploy *)
          Thread_safe.block_on_async_exn (fun () ->
              ( print_endline
              @@ Signature_lib.Public_key.(
                   Compressed.to_base58_check
                   @@ compress zkapp_keypair.public_key) ) ;
              let%bind nonce =
                Gql_client.infer_nonce gql_uri
                  (Signature_lib.Public_key.compress signer.public_key)
              in
              let command =
                Deploy.deploy_command_exn ~signer ~zkapp:zkapp_keypair
                  ~fee:(Currency.Fee.of_mina_int_exn 1)
                  ~nonce ~initial_ledger:expected_ledger ~constraint_constants
                  (module M)
              in
              let%bind _ = Gql_client.send_zkapp gql_uri command in
              let%bind _created =
                Gql_client.For_tests.create_new_block gql_uri
              in
              return () ) ;

          (* Init sequencer *)
          let sequencer =
            Thread_safe.block_on_async_exn (fun () ->
                Sequencer.create
                  ~zkapp_pk:
                    Signature_lib.Public_key.(compress zkapp_keypair.public_key)
                  ~max_pool_size:10 ~commitment_period_sec:0.
                  ~da_contract_address:None ~db_dir:None ~l1_uri:gql_uri ~signer
                  ~test_accounts_path:None )
          in
          add_test_accounts ~f:(add_account sequencer) ;
          sequencer.snark_q.state <-
            { sequencer.snark_q.state with
              previous_committed_ledger =
                Some
                  (Mina_ledger.Sparse_ledger.of_ledger_subset_exn
                     expected_ledger [ M.Inner.account_id ] )
            } ;

          (* Apply first batch *)
          let () =
            Thread_safe.block_on_async_exn (fun () ->
                let source_ledger_hash = get_root sequencer in

                [%test_eq: Frozen_ledger_hash.t] source_ledger_hash
                  (L.merkle_root expected_ledger) ;

                List.iteri batch1 ~f:(fun i spec ->
                    let result =
                      match i % 2 = 0 with
                      | true ->
                          let command = account_update_send spec in
                          ( match
                              L.apply_zkapp_command_unchecked expected_ledger
                                command ~constraint_constants
                                ~global_slot:
                                  Mina_numbers.Global_slot_since_genesis.zero
                                ~state_view:
                                  Mina_state.Protocol_state.(
                                    Body.view @@ body sequencer.protocol_state)
                            with
                          | Ok _ ->
                              ()
                          | _ ->
                              () ) ;

                          apply_zkapp_command sequencer command
                      | false ->
                          let command = command_send spec in
                          ( match
                              L.apply_user_command_unchecked expected_ledger
                                command ~constraint_constants
                                ~txn_global_slot:
                                  Mina_numbers.Global_slot_since_genesis.zero
                            with
                          | Ok _ ->
                              ()
                          | _ ->
                              () ) ;

                          apply_signed_command sequencer command
                    in

                    [%test_eq: Bool.t] true (Or_error.is_ok result) ;
                    let txn_applied, command_witness = Or_error.ok_exn result in

                    don't_wait_for
                    @@ Snark_queue.enqueue_prove_command sequencer.snark_q
                         command_witness ;

                    let status =
                      L.Transaction_applied.transaction_status txn_applied
                    in
                    [%test_eq: Transaction_status.t] status Applied ) ;

                let target_ledger_hash = get_root sequencer in

                [%test_eq: Frozen_ledger_hash.t] target_ledger_hash
                  (L.merkle_root expected_ledger) ;

                let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in

                [%test_eq: Bool.t] true
                  (Option.is_some sequencer.snark_q.state.last) ;
                let snark = Option.value_exn sequencer.snark_q.state.last in
                (* let stmt = Zkapps_rollup.Wrapper_rules.statement snark in *)
                [%test_eq: Frozen_ledger_hash.t]
                  (Zkapps_rollup.source_ledger snark)
                  source_ledger_hash ;
                [%test_eq: Frozen_ledger_hash.t]
                  (Zkapps_rollup.target_ledger snark)
                  target_ledger_hash ;

                return () )
          in

          (* First commit *)
          Thread_safe.block_on_async_exn (fun () ->
              let%bind () = commit sequencer in
              let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in
              let%bind () =
                Executor.wait_to_finish sequencer.snark_q.executor
              in
              let%bind committed_ledger_hash =
                Gql_client.infer_committed_state gql_uri
                  ~signer_pk:
                    (Signature_lib.Public_key.compress signer.public_key)
                  ~zkapp_pk:
                    (Signature_lib.Public_key.compress zkapp_keypair.public_key)
              in
              let target_ledger_hash = get_root sequencer in
              [%test_eq: Frozen_ledger_hash.t] committed_ledger_hash
                target_ledger_hash ;

              Deferred.unit ) ;

          (* To test nonce inferring from pool *)
          (* The first commit is still in the pool *)
          Executor.refresh_nonce sequencer.snark_q.executor ;

          (* Apply second batch *)
          Thread_safe.block_on_async_exn (fun () ->
              let source_ledger_hash = get_root sequencer in

              [%test_eq: Frozen_ledger_hash.t] source_ledger_hash
                (L.merkle_root expected_ledger) ;

              List.iteri batch2 ~f:(fun i spec ->
                  let result =
                    match i % 2 = 0 with
                    | true ->
                        let command = account_update_send spec in
                        ( match
                            L.apply_zkapp_command_unchecked expected_ledger
                              command ~constraint_constants
                              ~global_slot:
                                Mina_numbers.Global_slot_since_genesis.zero
                              ~state_view:
                                Mina_state.Protocol_state.(
                                  Body.view @@ body sequencer.protocol_state)
                          with
                        | Ok _ ->
                            ()
                        | _ ->
                            () ) ;

                        apply_zkapp_command sequencer command
                    | false ->
                        let command = command_send spec in
                        ( match
                            L.apply_user_command_unchecked expected_ledger
                              command ~constraint_constants
                              ~txn_global_slot:
                                Mina_numbers.Global_slot_since_genesis.zero
                          with
                        | Ok _ ->
                            ()
                        | _ ->
                            () ) ;

                        apply_signed_command sequencer command
                  in

                  [%test_eq: Bool.t] true (Or_error.is_ok result) ;
                  let txn_applied, command_witness = Or_error.ok_exn result in

                  don't_wait_for
                  @@ Snark_queue.enqueue_prove_command sequencer.snark_q
                       command_witness ;

                  let status =
                    L.Transaction_applied.transaction_status txn_applied
                  in
                  [%test_eq: Transaction_status.t] status Applied ) ;

              let target_ledger_hash = get_root sequencer in

              [%test_eq: Frozen_ledger_hash.t] target_ledger_hash
                (L.merkle_root expected_ledger) ;

              let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in

              [%test_eq: Bool.t] true
                (Option.is_some sequencer.snark_q.state.last) ;
              let snark = Option.value_exn sequencer.snark_q.state.last in
              (* let stmt = Zkapps_rollup.Wrapper_rules.statement snark in *)
              [%test_eq: Frozen_ledger_hash.t]
                (Zkapps_rollup.source_ledger snark)
                source_ledger_hash ;
              [%test_eq: Frozen_ledger_hash.t]
                (Zkapps_rollup.target_ledger snark)
                target_ledger_hash ;

              return () ) ;

          (* Second commit *)
          Thread_safe.block_on_async_exn (fun () ->
              let%bind () = commit sequencer in
              let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in
              let%bind () =
                Executor.wait_to_finish sequencer.snark_q.executor
              in
              let%bind _created =
                Gql_client.For_tests.create_new_block gql_uri
              in
              let%bind committed_ledger_hash =
                Gql_client.fetch_committed_state gql_uri
                  Signature_lib.Public_key.(compress zkapp_keypair.public_key)
              in
              let target_ledger_hash = get_root sequencer in
              [%test_eq: Frozen_ledger_hash.t] committed_ledger_hash
                target_ledger_hash ;

              Deferred.unit ) ) )
