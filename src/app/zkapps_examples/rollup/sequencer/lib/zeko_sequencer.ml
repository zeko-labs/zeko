open Base
open Core_kernel
open Async
open Async_kernel
open Mina_base
module L = Mina_ledger.Ledger

let time label (d : 'a Deferred.t) =
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  printf "%s: %s\n%!" label (Time.Span.to_string_hum @@ Time.diff stop start) ;
  return x

let l1_global_slot ~(genesis_timestamp : Time.t) =
  let d = Time.diff (Time.now ()) genesis_timestamp in
  let slot = Time.Span.to_min d /. 3. in
  Mina_numbers.Global_slot_since_genesis.of_int @@ Float.to_int slot

module Sequencer = struct
  module Config = struct
    type t =
      { max_pool_size : int
      ; commitment_period_sec : float
      ; db_dir : string option
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

  let consensus_constants =
    Consensus.Constants.create ~constraint_constants
      ~protocol_constants:genesis_constants.protocol

  let state_body =
    (* FIXME: Use the correct values *)
    let compile_time_genesis =
      Mina_state.Genesis_protocol_state.t
        ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
        ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
        ~constraint_constants ~consensus_constants
        ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference
    in
    Mina_state.Protocol_state.body compile_time_genesis.data

  module T = Transaction_snark.Make (struct
    let constraint_constants = constraint_constants

    let proof_level = Genesis_constants.Proof_level.Full
  end)

  module M = Zkapps_rollup.Make (struct
    let tag = T.tag
  end)

  let keypair = Signature_lib.Keypair.create ()

  let sok_digest =
    Sok_message.digest
    @@ Sok_message.create ~fee:Currency.Fee.zero
         ~prover:(Signature_lib.Public_key.compress keypair.public_key)

  module Snark_queue = struct
    type t =
      { q : unit Throttle.t
      ; da_config : Da_layer.Config.t
      ; mutable last : Zkapps_rollup.t option
      ; mutable staged_commands :
          ( Signed_command.With_valid_signature.t
          , Zkapp_command.t )
          User_command.t_
          list
      ; mutable previous_committed_ledger_hash : Frozen_ledger_hash0.t option
      ; mutable previous_committed_ledger : Mina_ledger.Sparse_ledger.t option
      ; mutable previous_committed_global_slot :
          Mina_numbers.Global_slot_since_genesis.t
      ; zkapp_pk : Signature_lib.Public_key.Compressed.t
      ; signer : Signature_lib.Keypair.t
      ; l1_uri : Uri.t Cli_lib.Flag.Types.with_name
      ; transfers_memory : Transfers_memory.t
      }

    let create ~da_config ~zkapp_pk ~l1_uri ~signer
        ~previous_committed_global_slot =
      { q = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1
      ; da_config
      ; last = None
      ; staged_commands = []
      ; previous_committed_ledger_hash = None
      ; previous_committed_ledger = None
      ; previous_committed_global_slot
      ; zkapp_pk
      ; signer
      ; l1_uri
      ; transfers_memory = Transfers_memory.create ~lifetime:Float.(60. * 10.)
      }

    let queue_size t = Throttle.num_jobs_waiting_to_start t.q

    let wrap_and_merge t txn_snark command global_slot =
      let%bind wrapped =
        time "Wrapper.wrap" (M.Wrapper.wrap txn_snark global_slot)
      in
      let%bind final_snark =
        match t.last with
        | Some last' ->
            time "Wrapper.merge" (M.Wrapper.merge last' wrapped)
        | None ->
            return wrapped
      in
      t.last <- Some final_snark ;
      t.staged_commands <- t.staged_commands @ [ command ] ;
      return ()

    module Command_witness = struct
      type t =
        | Signed_command of
            Mina_ledger.Sparse_ledger.t
            * Signed_command.With_valid_signature.t Transaction_protocol_state.t
            * Transaction_snark.Statement.With_sok.t
            * Mina_numbers.Global_slot_since_genesis.t
        | Zkapp_command of
            ( Transaction_witness.Zkapp_command_segment_witness.t
            * Transaction_snark.Zkapp_command_segment.Basic.t
            * Mina_state.Snarked_ledger_state.With_sok.t )
            list
            * Zkapp_command.t
            * Mina_numbers.Global_slot_since_genesis.t
    end

    let prove_signed_command t ~sparse_ledger ~user_command_in_block ~statement
        ~global_slot =
      let handler =
        unstage @@ Mina_ledger.Sparse_ledger.handler sparse_ledger
      in
      let%bind txn_snark =
        time "Transaction_snark.of_signed_command"
          (T.of_user_command ~init_stack:Mina_base.Pending_coinbase.Stack.empty
             ~statement user_command_in_block handler )
      in
      wrap_and_merge t txn_snark
        (User_command.Signed_command user_command_in_block.transaction)
        global_slot

    let prove_zkapp_command t ~witnesses ~zkapp_command ~global_slot =
      let%bind txn_snark =
        match witnesses with
        | [] ->
            failwith "No witnesses"
        | (witness, spec, statement) :: rest ->
            let%bind p1 =
              time "Transaction_snark.of_zkapp_command_segment"
                (T.of_zkapp_command_segment_exn ~statement ~witness ~spec)
            in
            Deferred.List.fold ~init:p1 rest
              ~f:(fun acc (witness, spec, statement) ->
                let%bind prev = return acc in
                let%bind curr =
                  time "Transaction_snark.of_zkapp_command_segment"
                    (T.of_zkapp_command_segment_exn ~statement ~witness ~spec)
                in
                let%bind merged =
                  time "Transaction_snark.merge" (T.merge curr prev ~sok_digest)
                in
                return (Or_error.ok_exn merged) )
      in
      wrap_and_merge t txn_snark (User_command.Zkapp_command zkapp_command)
        global_slot

    let enqueue_prove_command t command_witness =
      Throttle.enqueue t.q (fun () ->
          match command_witness with
          | Command_witness.Signed_command
              (sparse_ledger, user_command_in_block, statement, global_slot) ->
              prove_signed_command t ~sparse_ledger ~user_command_in_block
                ~statement ~global_slot
          | Command_witness.Zkapp_command (witnesses, zkapp_command, global_slot)
            ->
              prove_zkapp_command t ~witnesses ~zkapp_command ~global_slot )

    let enqueue_prove_transfer t ~key ~(transfer : Transfer.t) =
      Throttle.enqueue t.q (fun () ->
          let%bind call_forest =
            match transfer.direction with
            | Deposit ->
                time "Outer.deposit"
                  (M.Outer.deposit ~public_key:t.zkapp_pk
                     ~amount:(Currency.Amount.of_uint64 transfer.amount)
                     ~recipient:transfer.address )
            | Withdraw ->
                time "Inner.withdraw"
                  (M.Inner.withdraw ~public_key:t.zkapp_pk
                     ~amount:(Currency.Amount.of_uint64 transfer.amount)
                     ~recipient:transfer.address )
          in
          Transfers_memory.add t.transfers_memory key call_forest ;
          return () )

    let enqueue_prove_commit t ~target_ledger ~l1_genesis_timestamp =
      Throttle.enqueue t.q (fun () ->
          match List.is_empty t.staged_commands with
          | true ->
              print_endline "Nothing to commit" ;
              return ()
          | false -> (
              print_endline "Committing..." ;

              let ledger_hash =
                Zkapps_rollup.target_ledger @@ Option.value_exn t.last
              in
              let batch_id =
                Frozen_ledger_hash0.to_decimal_string ledger_hash
              in
              let previous_batch_id =
                Frozen_ledger_hash0.to_decimal_string
                  (Option.value
                     ~default:(Frozen_ledger_hash0.of_decimal_string "0")
                     t.previous_committed_ledger_hash )
              in

              match%bind
                try_with (fun () ->
                    let%bind () =
                      Da_layer.post_batch t.da_config
                        ~commands:t.staged_commands ~batch_id ~previous_batch_id
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
                    let%bind account_update =
                      time "Outer.step_without_transfers"
                        (M.Outer.step_without_transfers
                           (Option.value_exn t.last) ~public_key:t.zkapp_pk
                           ~previous_global_slot_update:
                             t.previous_committed_global_slot )
                    in
                    let%bind nonce =
                      Gql_client.fetch_nonce t.l1_uri
                        (Signature_lib.Public_key.compress t.signer.public_key)
                    in
                    let command : Zkapp_command.t =
                      { fee_payer =
                          { Account_update.Fee_payer.body =
                              { public_key =
                                  Signature_lib.Public_key.compress
                                    t.signer.public_key
                              ; fee = Currency.Fee.of_mina_int_exn 1
                              ; valid_until =
                                  Some
                                    (Mina_numbers.Global_slot_since_genesis.add
                                       (l1_global_slot
                                          ~genesis_timestamp:
                                            l1_genesis_timestamp )
                                       M.Outer.acceptable_global_slot_difference )
                              ; nonce = Unsigned.UInt32.of_int nonce
                              }
                          ; authorization = Signature.dummy
                          }
                      ; account_updates = account_update
                      ; memo = Signed_command_memo.empty
                      }
                    in
                    let full_commitment =
                      Zkapp_command.Transaction_commitment.create_complete
                        (Zkapp_command.commitment command)
                        ~memo_hash:(Signed_command_memo.hash command.memo)
                        ~fee_payer_hash:
                          (Zkapp_command.Digest.Account_update.create
                             (Account_update.of_fee_payer command.fee_payer) )
                    in
                    let signature =
                      Signature_lib.Schnorr.Chunked.sign
                        ~signature_kind:Mina_signature_kind.Testnet
                        t.signer.private_key
                        (Random_oracle.Input.Chunked.field full_commitment)
                    in
                    let command =
                      { command with
                        fee_payer =
                          { command.fee_payer with authorization = signature }
                      }
                    in
                    Gql_client.send_zkapp t.l1_uri command )
              with
              | Ok _ ->
                  t.staged_commands <- [] ;
                  t.last <- None ;
                  t.previous_committed_ledger_hash <- Some ledger_hash ;
                  t.previous_committed_ledger <- Some target_ledger ;
                  return ()
              | Error e ->
                  (* Continue expanding batch if commit failed *)
                  print_endline ("Commit failed: " ^ Exn.to_string e) ;
                  return () ) )

    let wait_to_finish t = Throttle.capacity_available t.q
  end

  type t =
    { db : L.Db.t
    ; archive : Archive.t
    ; config : Config.t
    ; da_config : Da_layer.Config.t
    ; snark_q : Snark_queue.t
    ; stop : unit Ivar.t
    ; l1_genesis_timestamp : Time.t
    }

  let create ~zkapp_pk ~max_pool_size ~commitment_period_sec
      ~da_contract_address ~db_dir ~l1_uri ~signer ~l1_genesis_timestamp
      ~previous_committed_global_slot =
    let db =
      L.Db.create ?directory_name:db_dir
        ~depth:constraint_constants.ledger_depth ()
    in
    let da_config : Da_layer.Config.t = { da_contract_address } in
    { db
    ; archive = Archive.create ~kvdb:(L.Db.kvdb db)
    ; config = { max_pool_size; commitment_period_sec; db_dir }
    ; da_config
    ; snark_q =
        Snark_queue.create ~da_config ~zkapp_pk ~l1_uri ~signer
          ~previous_committed_global_slot
    ; stop = Ivar.create ()
    ; l1_genesis_timestamp
    }

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

  let inferr_nonce t public_key =
    match get_account t public_key Token_id.default with
    | Some account ->
        account.nonce
    | None ->
        Unsigned.UInt32.zero

  let get_root t = L.Db.merkle_root t.db

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
    let global_slot =
      l1_global_slot ~genesis_timestamp:t.l1_genesis_timestamp
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

    Result.return
      ( txn_applied
      , Snark_queue.Command_witness.Signed_command
          (sparse_ledger, user_command_in_block, statement, global_slot) )

  let apply_zkapp_command t (zkapp_command : Zkapp_command.t) =
    let%bind.Result () =
      if Snark_queue.queue_size t.snark_q >= t.config.max_pool_size then
        Error (Error.of_string "Maximum pool size reached, try later")
      else Ok ()
    in
    let global_slot =
      l1_global_slot ~genesis_timestamp:t.l1_genesis_timestamp
    in
    let%bind.Result first_pass_ledger, second_pass_ledger, txn_applied =
      let l = L.of_database t.db in
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

    Result.return
      ( txn_applied
      , Snark_queue.Command_witness.Zkapp_command
          (witnesses, zkapp_command, global_slot) )

  let apply_deposits t =
    let%bind inner_account_update, _, _ =
      time "Inner.step"
        (M.Inner.step ~deposits_processed:Zkapp_account.Actions.empty_hash
           ~remaining_deposits:[] )
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
      ; account_updates = inner_account_update
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
    let () =
      match t.config.db_dir with
      | None ->
          ()
      | Some db_dir -> (
          let ledger_hash =
            Frozen_ledger_hash.to_decimal_string L.Db.(merkle_root t.db)
          in
          let commit_dir = Filename.concat db_dir ("commit-" ^ ledger_hash) in
          try
            FileUtil.rm ~force:Force ~recurse:true [ commit_dir ] ;
            L.Db.make_checkpoint t.db ~directory_name:commit_dir
          with err -> print_endline (Exn.to_string err) )
    in
    let target_ledger =
      Mina_ledger.Sparse_ledger.of_ledger_subset_exn
        L.(of_database t.db)
        [ M.Inner.account_id ]
    in
    Snark_queue.enqueue_prove_commit t.snark_q ~target_ledger
      ~l1_genesis_timestamp:t.l1_genesis_timestamp

  let run_committer t =
    if Float.(t.config.commitment_period_sec <= 0.) then ()
    else
      every ~stop:(Ivar.read t.stop)
        (Time_ns.Span.of_sec t.config.commitment_period_sec) (fun () ->
          don't_wait_for @@ commit t )

  module Test_accounts = struct
    type t = { pk : string; balance : int64 } [@@deriving yojson]

    let parse_accounts_exn ~test_accounts_path : (Account_id.t * Account.t) list
        =
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

    let add_accounts_exn sequencer ~test_accounts_path =
      let accounts = parse_accounts_exn ~test_accounts_path in
      List.iter accounts ~f:(fun (account_id, account) ->
          add_account sequencer account_id account )
  end

  let bootstrap ~zkapp_pk ~max_pool_size ~commitment_period_sec
      ~da_contract_address ~db_dir ~l1_uri ~signer ~test_accounts_path =
    let%bind commited_state = Gql_client.fetch_commited_state l1_uri zkapp_pk in
    let commited_ledger_hash =
      Frozen_ledger_hash.of_decimal_string @@ List.nth_exn commited_state 0
    in
    let previous_committed_global_slot =
      Mina_numbers.Global_slot_since_genesis.of_string
        (List.nth_exn commited_state 3)
    in
    let commit_dir =
      Filename.concat db_dir
        ("commit-" ^ Frozen_ledger_hash.to_decimal_string commited_ledger_hash)
    in
    let found_checkpoint = FileUtil.test Exists commit_dir in

    (* prepare db *)
    let () =
      match found_checkpoint with
      | true -> (
          print_endline "Found checkpoint, skipping bootstrap" ;
          let commit_files = FileUtil.ls commit_dir in
          let old_files =
            List.filter (FileUtil.ls db_dir) ~f:(fun file ->
                not @@ String.is_substring ~substring:"commit-" file )
          in
          (* restore checkpoint *)
          try
            FileUtil.rm ~force:Force old_files ;
            FileUtil.cp ~force:FileUtil.Force ~recurse:true commit_files db_dir
          with err -> print_endline (Exn.to_string err) )
      | false -> (
          print_endline "No checkpoint found, bootstrapping from genesis" ;
          (* remove present db *)
          try FileUtil.rm ~force:Force ~recurse:true [ db_dir ]
          with err -> print_endline (Exn.to_string err) )
    in

    let%bind l1_genesis_timestamp = Gql_client.fetch_genesis_timestamp l1_uri in

    (* create sequencer state *)
    let t =
      create ~zkapp_pk ~max_pool_size ~commitment_period_sec
        ~da_contract_address ~db_dir:(Some db_dir) ~l1_uri ~signer
        ~l1_genesis_timestamp ~previous_committed_global_slot
    in

    (* add initial accounts *)
    if not found_checkpoint then (
      add_account t M.Inner.account_id M.Inner.initial_account ;

      (* Only for testing *)
      ( match test_accounts_path with
      | Some test_accounts_path ->
          print_endline "Adding test accounts" ;
          Test_accounts.add_accounts_exn t ~test_accounts_path
      | None ->
          print_endline "No test accounts" ) ;

      print_endline
        ("Init root: " ^ Frozen_ledger_hash.(to_decimal_string (get_root t))) ) ;

    (* apply command from DA layer *)
    let%bind () =
      match found_checkpoint with
      | false ->
          let%bind commands =
            Da_layer.get_batches t.da_config
              ~to_:(Frozen_ledger_hash.to_decimal_string commited_ledger_hash)
          in
          return
          @@ List.iter commands ~f:(fun command ->
                 match command with
                 | User_command.Signed_command signed_command ->
                     ( apply_signed_command t signed_command |> Or_error.ok_exn
                       : L.Transaction_applied.t * _ )
                     |> ignore
                 | User_command.Zkapp_command zkapp_command ->
                     ( apply_zkapp_command t zkapp_command |> Or_error.ok_exn
                       : L.Transaction_applied.t * _ )
                     |> ignore )
      | true ->
          return ()
    in
    t.snark_q.previous_committed_ledger_hash <- Some (get_root t) ;
    t.snark_q.previous_committed_ledger <-
      Some
        (Mina_ledger.Sparse_ledger.of_ledger_subset_exn
           L.(of_database t.db)
           [ M.Inner.account_id ] ) ;
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

  (* Fetch L1 genesis timestamp *)
  let l1_genesis_timestamp =
    Thread_safe.block_on_async_exn (fun () ->
        Gql_client.fetch_genesis_timestamp gql_uri )
  in

  let open Mina_transaction_logic.For_tests in
  Quickcheck.test ~trials:1
    (Test_spec.mk_gen ~num_transactions:number_of_transactions ())
    ~f:(fun { init_ledger; specs } ->
      let sequencer =
        Sequencer.create
          ~zkapp_pk:Signature_lib.Public_key.(compress zkapp_keypair.public_key)
          ~max_pool_size:10 ~commitment_period_sec:0. ~da_contract_address:None
          ~db_dir:None ~l1_uri:gql_uri ~signer ~l1_genesis_timestamp
          ~previous_committed_global_slot:
            Mina_numbers.Global_slot_since_genesis.zero
      in
      L.with_ledger ~depth:constraint_constants.ledger_depth
        ~f:(fun expected_ledger ->
          (* Init ledgers *)
          L.create_new_account_exn expected_ledger M.Inner.account_id
            M.Inner.initial_account ;
          add_account sequencer M.Inner.account_id M.Inner.initial_account ;

          Array.iter init_ledger ~f:(fun (keypair, balance) ->
              let pk = Signature_lib.Public_key.compress keypair.public_key in
              let account_id = Account_id.create pk Token_id.default in
              let balance = Unsigned.UInt64.of_int64 balance in
              let account =
                Account.create account_id (Currency.Balance.of_uint64 balance)
              in
              L.create_new_account_exn expected_ledger account_id account ;
              add_account sequencer account_id account ) ;

          let source_ledger_hash = get_root sequencer in

          [%test_eq: Frozen_ledger_hash.t] source_ledger_hash
            (L.merkle_root expected_ledger) ;

          sequencer.snark_q.previous_committed_ledger <-
            Some
              (Mina_ledger.Sparse_ledger.of_ledger_subset_exn expected_ledger
                 [ M.Inner.account_id ] ) ;

          (* Deploy *)
          Thread_safe.block_on_async_exn (fun () ->
              ( print_endline
              @@ Signature_lib.Public_key.(
                   Compressed.to_base58_check
                   @@ compress zkapp_keypair.public_key) ) ;
              let%bind nonce =
                Gql_client.fetch_nonce gql_uri
                  (Signature_lib.Public_key.compress signer.public_key)
              in
              let command =
                M.Outer.deploy_command_exn ~signer ~zkapp:zkapp_keypair
                  ~fee:(Currency.Fee.of_mina_int_exn 1)
                  ~nonce:(Account.Nonce.of_int nonce)
                  ~initial_ledger:expected_ledger
              in
              let%bind _ = Gql_client.send_zkapp gql_uri command in
              return () ) ;

          (* Apply commands *)
          let () =
            Thread_safe.block_on_async_exn (fun () ->
                List.iteri specs ~f:(fun i spec ->
                    let result =
                      match i % 2 = 0 with
                      | true ->
                          let command = account_update_send spec in
                          ( match
                              L.apply_zkapp_command_unchecked expected_ledger
                                command ~constraint_constants
                                ~global_slot:
                                  (l1_global_slot
                                     ~genesis_timestamp:
                                       sequencer.l1_genesis_timestamp )
                                ~state_view:
                                  (Mina_state.Protocol_state.Body.view
                                     state_body )
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
                                  (l1_global_slot
                                     ~genesis_timestamp:
                                       sequencer.l1_genesis_timestamp )
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

                [%test_eq: Bool.t] true (Option.is_some sequencer.snark_q.last) ;
                let snark = Option.value_exn sequencer.snark_q.last in
                (* let stmt = Zkapps_rollup.Wrapper_rules.statement snark in *)
                [%test_eq: Frozen_ledger_hash.t]
                  (Zkapps_rollup.source_ledger snark)
                  source_ledger_hash ;
                [%test_eq: Frozen_ledger_hash.t]
                  (Zkapps_rollup.target_ledger snark)
                  target_ledger_hash ;

                let%bind res = M.Wrapper.verify snark in
                [%test_eq: Bool.t] true (Or_error.is_ok res) ;

                return () )
          in

          (* Commit *)
          Thread_safe.block_on_async_exn (fun () ->
              let%bind () = commit sequencer in
              let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in
              let%bind commited_state =
                Gql_client.fetch_commited_state gql_uri
                  Signature_lib.Public_key.(compress zkapp_keypair.public_key)
              in
              let commited_ledger_hash =
                Frozen_ledger_hash.of_decimal_string
                @@ List.nth_exn commited_state 0
              in
              let target_ledger_hash = get_root sequencer in
              [%test_eq: Frozen_ledger_hash.t] commited_ledger_hash
                target_ledger_hash ;

              Deferred.unit ) ) )
