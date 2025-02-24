open Base
open Core_kernel
open Async
open Async_kernel
open Mina_base
open Mina_ledger
open Signature_lib
module L = Ledger
module Field = Snark_params.Tick.Field

let constraint_constants = Genesis_constants.Compiled.constraint_constants

module Sequencer = struct
  let constraint_constants = constraint_constants

  module Config = struct
    type t =
      { max_pool_size : int
      ; commitment_period_sec : float
      ; db_dir : string option
      ; zkapp_pk : Public_key.Compressed.t
      ; signer : Keypair.t
      ; l1_uri : Uri.t Cli_lib.Flag.Types.with_name
      ; archive_uri : Uri.t Cli_lib.Flag.Types.with_name
      ; network_id : string
      ; deposit_delay_blocks : int
      }
  end

  module Transfer = struct
    type direction = Deposit | Withdraw

    type t = { transfer : Zkapps_rollup.TR.t; direction : direction }

    type claim =
      { is_new : bool
      ; pointer : Field.t
      ; before : Zkapps_rollup.TR.t list
      ; after : Zkapps_rollup.TR.t list
      ; transfer : t
      }
  end

  let genesis_constants = Genesis_constants.Compiled.genesis_constants

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

  let keypair = Keypair.create ()

  let sok_digest =
    Sok_message.digest
    @@ Sok_message.create ~fee:Currency.Fee.zero
         ~prover:(Public_key.compress keypair.public_key)

  module Snark_queue = struct
    type t =
      { q : unit Throttle.t
      ; config : Config.t
      ; transfers_memory : Transfers_memory.t
      ; provers : Zeko_prover.Client.t
      }

    let create ~config ~provers =
      { q = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1
      ; config
      ; transfers_memory = Transfers_memory.create ~lifetime:Float.(60. * 10.)
      ; provers
      }

    let queue_size t = Throttle.num_jobs_waiting_to_start t.q

    let enqueue t f =
      Throttle.enqueue t.q (fun () ->
          let%map result = f () in
          result )

    let enqueue_prove_transfer_request t ~key ~(transfer : Transfer.t) =
      Throttle.enqueue t.q (fun () ->
          let%bind result =
            try_with (fun () ->
                match transfer with
                | { direction = Deposit; transfer } ->
                    Zeko_prover.Client.submit_deposit t.provers
                      ~outer_pk:t.config.zkapp_pk ~deposit:transfer
                | { direction = Withdraw; transfer } ->
                    Zeko_prover.Client.submit_withdrawal t.provers
                      ~withdrawal:transfer )
          in
          let () =
            match result with
            | Ok tree ->
                Transfers_memory.add t.transfers_memory key
                  (Ok (Zkapp_command.Call_forest.cons_tree tree []))
            | Error e ->
                printf "Warning: prove_transfer_request failed %s\n%!"
                  (Exn.to_string e) ;
                Transfers_memory.add t.transfers_memory key
                  (Error (Exn.to_string e))
          in
          return () )

    let enqueue_prove_transfer_claim t ~key ~(claim : Transfer.claim) =
      Throttle.enqueue t.q (fun () ->
          let%bind result =
            try_with (fun () ->
                match claim with
                | { transfer = { direction = Deposit; transfer }
                  ; is_new
                  ; pointer
                  ; before
                  ; after
                  } ->
                    Zeko_prover.Client.process_deposit t.provers ~is_new
                      ~pointer ~before ~after ~deposit:transfer
                | { transfer = { direction = Withdraw; transfer }
                  ; is_new
                  ; pointer
                  ; before
                  ; after
                  } ->
                    Zeko_prover.Client.process_withdrawal t.provers
                      ~outer_pk:t.config.zkapp_pk ~is_new ~pointer ~before
                      ~after ~withdrawal:transfer )
          in
          let () =
            match result with
            | Ok forest ->
                Transfers_memory.add t.transfers_memory key (Ok forest)
            | Error e ->
                printf "Warning: prove_transfer_claim failed %s\n%!"
                  (Exn.to_string e) ;
                Transfers_memory.add t.transfers_memory key
                  (Error (Exn.to_string e))
          in
          return () )

    let wait_to_finish t = Throttle.capacity_available t.q
  end

  module Merger = struct
    module Command_witness = struct
      type t =
        | Signed_command of
            Sparse_ledger.t
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

    let wrap provers txn_snark =
      Zeko_prover.Client.wrapper_wrap provers ~txn_snark

    let merge provers a b = Zeko_prover.Client.wrapper_merge provers a b

    let prove_signed_command provers ~sparse_ledger ~user_command_in_block
        ~statement =
      let%bind txn_snark =
        Zeko_prover.Client.transaction_snark_of_signed_command provers
          ~statement ~user_command_in_block ~sparse_ledger
      in
      wrap provers txn_snark

    let prove_zkapp_command provers ~witnesses ~zkapp_command =
      let%bind txn_snark =
        match witnesses with
        | [] ->
            failwith "No witnesses"
        | (witness, spec, statement) :: rest ->
            let%bind p1 =
              Zeko_prover.Client.transaction_snark_of_zkapp_command_segment
                provers ~statement ~witness ~spec
            in
            Deferred.List.fold ~init:p1 rest
              ~f:(fun acc (witness, spec, statement) ->
                let%bind prev = return acc in
                let%bind curr =
                  Zeko_prover.Client.transaction_snark_of_zkapp_command_segment
                    provers ~statement ~witness ~spec
                in
                let%bind merged =
                  Zeko_prover.Client.transaction_snark_merge provers curr prev
                in
                return merged )
      in
      wrap provers txn_snark

    module Context = struct
      module State = struct
        type t =
          { mutable previous_committed_ledger : Sparse_ledger.t option
          ; mutable previous_committed_ledger_hash : Ledger_hash.t option
          ; mutable commands : Command_witness.t array ref list
          }
        [@@deriving yojson]

        let create () =
          { previous_committed_ledger = None
          ; previous_committed_ledger_hash = None
          ; commands = []
          }
      end

      module Db = Kvdb_base.Make_singleton (struct
        type t = State.t [@@deriving yojson]

        let key = "context_state"
      end)

      type t =
        { provers : Zeko_prover.Client.t
        ; da_client : Da_layer.Client.Sequencer.t
        ; executor : Executor.t
        ; config : Config.t
        ; kvdb : Committer.Store.Kvdb.t
        ; state : State.t
        }

      let save_state t = Db.set t.kvdb ~data:t.state

      let load_state kvdb =
        match Db.get kvdb with Some state -> state | None -> State.create ()

      let committed t ledger =
        t.state.commands <- List.tl_exn t.state.commands ;
        t.state.previous_committed_ledger <- Some ledger ;
        t.state.previous_committed_ledger_hash <-
          Some (Sparse_ledger.merkle_root ledger) ;
        save_state t

      let set_last_committed_ledger t ledger =
        t.state.previous_committed_ledger <- Some ledger ;
        t.state.previous_committed_ledger_hash <-
          Some (Sparse_ledger.merkle_root ledger) ;
        save_state t

      let add_command t command =
        let arr = List.last_exn t.state.commands in
        arr := Array.append !arr [| command |] ;
        save_state t

      let created_new_tree t =
        t.state.commands <- t.state.commands @ [ ref [||] ] ;
        save_state t
    end

    module Merge = struct
      type t = Zkapps_rollup.t [@@deriving yojson]

      let process ({ provers; _ } : Context.t) a b = merge provers a b
    end

    module Base = struct
      type t = Command_witness.t [@@deriving yojson]

      let process (ctx : Context.t) command_witness =
        Context.add_command ctx command_witness ;
        match command_witness with
        | Command_witness.Signed_command
            (sparse_ledger, user_command_in_block, statement) ->
            prove_signed_command ctx.provers ~sparse_ledger
              ~user_command_in_block ~statement
        | Command_witness.Zkapp_command (witnesses, zkapp_command) ->
            prove_zkapp_command ctx.provers ~witnesses ~zkapp_command
    end

    module Commit = struct
      (* Only for yojson serialization of Field *)
      module Field = Data_hash.Make_full_size (struct
        let description = "Field"

        let version_byte = '\x00'
      end)

      type t =
        { new_inner_ledger : Sparse_ledger.t
        ; old_deposits_pointer : Field.t
        ; processed_deposits_pointer : Field.t
        }
      [@@deriving yojson]

      let process
          ({ da_client; provers; executor; config; kvdb; state } as ctx :
            Context.t )
          { new_inner_ledger; old_deposits_pointer; processed_deposits_pointer }
          last_snark =
        let%bind signatures =
          Da_layer.Client.Sequencer.get_signatures da_client
            ~ledger_hash:(Sparse_ledger.merkle_root new_inner_ledger)
          |> Deferred.map ~f:(fun x ->
                 Option.value_exn x ~message:"No signatures" )
        in
        printf "Received %d signatures from da layer\n%!"
          (List.length signatures) ;

        let old_inner_ledger =
          Option.value_exn state.previous_committed_ledger
            ~message:"No previous committed ledger"
        in
        let commit_witness : Committer.Commit_witness.t =
          { old_inner_ledger
          ; new_inner_ledger
          ; old_deposits_pointer
          ; processed_deposits_pointer
          ; signatures
          ; last_snark
          }
        in
        Committer.Store.store_commit kvdb commit_witness
          ~source:(Sparse_ledger.merkle_root old_inner_ledger)
          ~target:(Sparse_ledger.merkle_root new_inner_ledger) ;

        let%bind command =
          Committer.prove_commit ~provers ~executor ~zkapp_pk:config.zkapp_pk
            ~archive_uri:config.archive_uri commit_witness
        in
        let%bind () = Executor.send_zkapp_command executor command in
        Context.committed ctx new_inner_ledger ;
        return ()
    end

    module P = Parallel_merger.Make (Context) (Merge) (Base) (Commit)

    let requeue_after_restart t (ctx : Context.t) =
      let commands_to_requeue =
        ctx.state.commands
        |> List.map ~f:(fun arr -> Array.to_list !arr)
        |> List.join
      in
      (* Adding jobs will repopulate the list *)
      assert (phys_equal (P.current_tree t) None) ;
      ctx.state.commands <- [] ;
      printf "Requeueing %d commands\n%!" (List.length commands_to_requeue) ;
      List.iter commands_to_requeue ~f:(fun command ->
          don't_wait_for @@ P.add_job t ctx ~data:command )
  end

  module State_hashes = struct
    type t =
      { proved_ledger_hash : Ledger_hash.t
      ; unproved_ledger_hash : Ledger_hash.t
      ; committed_ledger_hash : Ledger_hash.t
      }
  end

  module Subscriptions = struct
    type t =
      { mutable state_hashes_changed : State_hashes.t Pipe.Writer.t list }

    let create () = { state_hashes_changed = [] }

    let add_state_hashes_subscriber t =
      let r, w = Pipe.create () in
      t.state_hashes_changed <- w :: t.state_hashes_changed ;
      (r, w)
  end

  type t =
    { db : L.Db.t
    ; logger : Logger.t
    ; archive : Archive.t
    ; config : Config.t
    ; snark_q : Snark_queue.t
    ; merger : Merger.P.t
    ; merger_ctx : Merger.Context.t
    ; stop : unit Ivar.t
    ; da_client : Da_layer.Client.Sequencer.t
    ; apply_q : unit Sequencer.t
          (* Applying of the user command is async operation, but we need to keep the application synchronous *)
    ; mutable subscriptions : Subscriptions.t
    ; mutable analytics_state : Analytics.State.t
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

  let infer_nonce t public_key =
    match get_account t public_key Token_id.default with
    | Some account ->
        account.nonce
    | None ->
        Unsigned.UInt32.zero

  let get_root t = L.Db.merkle_root t.db

  let is_empty t = L.Db.num_accounts t.db = 0

  let get_latest_state t =
    (* TODO: proved hashes *)
    State_hashes.
      { proved_ledger_hash = Field.zero
      ; unproved_ledger_hash = get_root t
      ; committed_ledger_hash = Field.zero
      }

  let trigger_state_hashes_changed t =
    let state_hashes = get_latest_state t in
    List.iter t.subscriptions.state_hashes_changed ~f:(fun w ->
        Pipe.write_without_pushback_if_open w state_hashes )

  (** Apply user command to the ledger without checking the validity of the command *)
  let apply_user_command_without_check l archive command ~global_slot
      ~state_body ~analytics_state =
    let accounts_referenced = User_command.accounts_referenced command in

    let first_pass_ledger =
      Sparse_ledger.of_ledger_subset_exn l accounts_referenced
    in
    let%bind.Result partialy_applied_txn =
      L.apply_transaction_first_pass ~constraint_constants ~global_slot
        ~txn_state_view:(Mina_state.Protocol_state.Body.view state_body)
        l (Command command)
    in

    let second_pass_ledger =
      Sparse_ledger.of_ledger_subset_exn l accounts_referenced
    in
    let%map.Result txn_applied =
      let%bind.Result txn_applied =
        L.apply_transaction_second_pass l partialy_applied_txn
      in
      match
        Mina_transaction_logic.Transaction_applied.transaction_status
          txn_applied
      with
      | Failed failure ->
          Error
            ( Error.of_string @@ Yojson.Safe.to_string
            @@ Transaction_status.Failure.Collection.to_yojson failure )
      | Applied ->
          Ok txn_applied
    in

    let target_ledger_hash = L.merkle_root l in

    L.Mask.Attached.commit l ;

    (* Add events and actions to the memory *)
    let () =
      match command with
      | Signed_command _ ->
          ()
      | Zkapp_command zkapp_command ->
          Zkapp_command.(
            Call_forest.iteri (account_updates zkapp_command)
              ~f:(fun _ update ->
                let account =
                  let account_id =
                    Account_id.create
                      (Account_update.public_key update)
                      (Account_update.token_id update)
                  in
                  let location =
                    L.location_of_account l account_id
                    |> Option.value_exn ~message:"No location"
                  in
                  L.get l location |> Option.value_exn ~message:"No account"
                in
                Archive.add_account_update archive update account
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
    in
    ( first_pass_ledger
    , second_pass_ledger
    , txn_applied
    , target_ledger_hash
    , Analytics.State.update_with_command analytics_state command l )

  (** Apply user command to the sequencer's state, including the check of command validity *)
  let apply_user_command t ?(skip_validity_check = false)
      (command : User_command.t) =
    if Throttle.num_jobs_waiting_to_start t.apply_q >= t.config.max_pool_size
    then
      return
        (Error (Error.of_string "Sequencer is under the load, try again later"))
    else
      Throttle.enqueue t.apply_q (fun () ->
          let%bind.Deferred.Result () =
            let weight = User_command.weight command in
            return
            @@
            if
              Zeko_prover.Client.queue_size t.merger_ctx.provers + weight
              > t.config.max_pool_size
            then
              Error
                (Error.of_string "Maximum proof queue size reached, try later")
            else Ok ()
          in

          (* the protocol state from sequencer has dummy values which wouldn't pass the txn snark *)
          let global_slot = Mina_numbers.Global_slot_since_genesis.zero in
          let state_body =
            Mina_state.Protocol_state.body compile_time_genesis_state
          in
          let l = L.of_database t.db in

          let%bind.Deferred.Result () =
            if skip_validity_check then return (Ok ())
            else
              let%bind.Deferred.Result verifiable =
                return
                @@ User_command.to_verifiable ~failed:false
                     ~find_vk:
                       (Zkapp_command.Verifiable.load_vk_from_ledger
                          ~get:(L.get l)
                          ~location_of_account:(L.location_of_account l) )
                     command
              in
              match%bind
                try_with (fun () ->
                    Verifier.verify_command
                      { data = verifiable; status = Applied } )
                >>| Result.map_error ~f:Error.of_exn
                >>| Result.join
              with
              | Ok (`Valid _) ->
                  return (Ok ())
              | Ok (`Valid_assuming _) ->
                  return (Error (Error.of_string "Invalid proof"))
              | Ok (#Verifier.invalid as invalid) ->
                  return (Error (Verifier.invalid_to_error invalid))
              | Error e ->
                  return (Error e)
          in

          let%bind.Deferred.Result ( first_pass_ledger
                                   , second_pass_ledger
                                   , txn_applied
                                   , target_ledger_hash
                                   , new_analytics_state ) =
            return
              (apply_user_command_without_check l t.archive command ~global_slot
                 ~state_body ~analytics_state:t.analytics_state )
          in
          t.analytics_state <- new_analytics_state ;

          (* Post transaction to the DA layer *)
          let changed_accounts =
            let account_ids =
              User_command.accounts_referenced command
              |> List.map ~f:(fun id ->
                     if Public_key.Compressed.(Account_id.public_key id = empty)
                     then Zkapps_rollup.inner_account_id
                     else id )
              |> List.stable_dedup
            in
            List.map account_ids ~f:(fun id ->
                let index = L.index_of_account_exn l id in
                (index, L.get_at_index_exn l index) )
          in
          let diff =
            Da_layer.Diff.create
              ~source_ledger_hash:(Sparse_ledger.merkle_root first_pass_ledger)
              ~changed_accounts
              ~command_with_action_step_flags:
                (Some
                   ( command
                   , match command with
                     | Signed_command _ ->
                         []
                     | Zkapp_command command ->
                         Zkapp_command.all_account_updates_list command
                         |> List.map ~f:(fun _ -> true) ) )
          in
          Da_layer.Client.Sequencer.enqueue_distribute_diff t.da_client
            ~ledger_openings:first_pass_ledger ~diff ~target_ledger_hash ;

          trigger_state_hashes_changed t ;

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

          return
          @@
          match command with
          | Signed_command signed_command ->
              let user_command_in_block =
                { Transaction_protocol_state.Poly.transaction =
                    Signed_command.check_only_for_signature signed_command
                    |> Option.value_exn
                         ~message:"check_only_for_signature failed"
                ; block_data = state_body
                ; global_slot
                }
              in
              let source_ledger_hash =
                Sparse_ledger.merkle_root first_pass_ledger
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
                    ( Mina_transaction.Transaction.fee_excess (Command command)
                    |> Or_error.ok_exn )
                  ~supply_increase:
                    ( Mina_transaction_logic.Transaction_applied.supply_increase
                        ~constraint_constants txn_applied
                    |> Or_error.ok_exn )
                  ~pending_coinbase_stack_state:pc
              in
              Result.return
                ( txn_applied
                , Merger.Command_witness.Signed_command
                    (first_pass_ledger, user_command_in_block, statement) )
          | Zkapp_command zkapp_command ->
              let witnesses =
                Transaction_snark.zkapp_command_witnesses_exn
                  ~constraint_constants ~global_slot ~state_body
                  ~fee_excess:
                    ( Currency.Amount.Signed.of_unsigned
                    @@ Currency.Amount.of_fee (Zkapp_command.fee zkapp_command)
                    )
                  [ ( `Pending_coinbase_init_stack Pending_coinbase.Stack.empty
                    , `Pending_coinbase_of_statement pc
                    , `Sparse_ledger first_pass_ledger
                    , `Sparse_ledger second_pass_ledger
                    , `Connecting_ledger_hash
                        (Sparse_ledger.merkle_root second_pass_ledger)
                    , zkapp_command )
                  ]
              in
              Result.return
                ( txn_applied
                , Merger.Command_witness.Zkapp_command (witnesses, zkapp_command)
                ) )

  let update_inner_account t =
    let old_deposits_state =
      Utils.get_inner_deposits_state_exn (L.of_database t.db)
    in
    let%bind new_deposits =
      Gql_client.fetch_transfers t.config.archive_uri
        ~from_action_state:old_deposits_state t.config.zkapp_pk
    in
    let%bind current_height = Gql_client.fetch_block_height t.config.l1_uri in
    (* Find pointer for deposits to be processed *)
    let processed_pointer =
      List.fold new_deposits ~init:old_deposits_state
        ~f:(fun curr_state (transfer, block_height) ->
          if block_height + t.config.deposit_delay_blocks <= current_height then
            Zkapp_account.Actions.push_events curr_state
              (Zkapps_rollup.TR.to_actions transfer)
          else curr_state )
    in
    if Field.equal old_deposits_state processed_pointer then
      (* In case no new deposits are to process, we don't need to update inner account *)
      return (old_deposits_state, old_deposits_state)
    else
      let%bind inner_account_update =
        Zeko_prover.Client.inner_step t.snark_q.provers
          ~all_deposits:processed_pointer
      in
      let fee = Currency.Fee.of_mina_int_exn 0 in
      let command : Zkapp_command.t =
        { fee_payer =
            (* Setting public_key to empty results in a dummy fee payer with public key near 123456789 (dumb). *)
            (* FIXME: Do this a better way without hard-coding values. *)
            { Account_update.Fee_payer.body =
                { public_key = Public_key.Compressed.empty
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
      let%bind command_witness =
        match%bind
          (* Skip validity check because dummy fee payer triggers invalid public key error *)
          apply_user_command t ~skip_validity_check:true (Zkapp_command command)
        with
        | Ok (status, witness) -> (
            match
              Mina_transaction_logic.Transaction_applied.transaction_status
                status
            with
            | Applied ->
                return witness
            | Failed failure ->
                failwithf
                  !"Failed to apply inner account update \
                    %{sexp:Transaction_status.Failure.Collection.t}"
                  failure () )
        | Error e ->
            Error.raise e
      in
      let () =
        don't_wait_for
        @@ Merger.P.add_job t.merger t.merger_ctx ~data:command_witness
      in
      return (old_deposits_state, processed_pointer)

  let commit t =
    let%bind old_deposits_pointer, processed_pointer = update_inner_account t in
    let target_ledger =
      Sparse_ledger.of_ledger_subset_exn
        L.(of_database t.db)
        [ Zkapps_rollup.inner_account_id ]
    in
    if
      Merger.P.current_tree t.merger
      |> Option.map ~f:Merger.P.Tree.is_empty
      |> Option.value ~default:true
    then return (print_endline "Nothing to commit")
    else
      Merger.P.commit_exn t.merger t.merger_ctx
        ~commit_witness:
          { new_inner_ledger = target_ledger
          ; old_deposits_pointer
          ; processed_deposits_pointer = processed_pointer
          }
      |> Deferred.ignore_m

  let run_committer t =
    if Float.(t.config.commitment_period_sec <= 0.) then ()
    else
      let period = Time_ns.Span.of_sec t.config.commitment_period_sec in
      every ~start:(after period) ~stop:(Ivar.read t.stop) period (fun () ->
          don't_wait_for @@ Deferred.ignore_m @@ commit t )

  let bootstrap ~logger ({ config; _ } as t) da_config =
    print_endline "Bootstrapping" ;
    let%bind committed_ledger_hash =
      Gql_client.infer_committed_state config.l1_uri ~zkapp_pk:config.zkapp_pk
        ~signer_pk:(Public_key.compress config.signer.public_key)
    in
    printf "Fetched root: %s\n%!"
      Ledger_hash.(to_decimal_string committed_ledger_hash) ;

    printf "Init root: %s\n%!" Ledger_hash.(to_decimal_string (get_root t)) ;

    (* apply diffs from DA layer *)
    let%bind () =
      Da_layer.Client.map_diffs ~logger ~config:da_config
        ~depth:constraint_constants.ledger_depth ~source_ledger_hash:`Genesis
        ~target_ledger_hash:committed_ledger_hash
        ~f:(fun ~current_chunk ~chunks_length diff ->
          assert (
            Ledger_hash.equal
              (Da_layer.Diff.Stable.Latest.source_ledger_hash diff)
              (get_root t) ) ;
          [%log info] "Applying diff with hash %s, progress: %.0f%%"
            (Ledger_hash.to_decimal_string
               (Da_layer.Diff.Stable.Latest.source_ledger_hash diff) )
            (Float.of_int current_chunk /. Float.of_int chunks_length *. 100.0) ;
          match
            Da_layer.Diff.Stable.Latest.command_with_action_step_flags diff
          with
          | None ->
              (* Apply accounts diff *)
              let mask = L.of_database t.db in
              let changed_accounts =
                Da_layer.Diff.Stable.Latest.changed_accounts diff
              in
              printf "Setting %d accounts\n%!" (List.length changed_accounts) ;
              List.iter changed_accounts ~f:(fun (index, account) ->
                  L.set_at_index_exn mask index account ) ;
              L.Mask.Attached.commit mask ;
              return ()
          | Some (command, _) ->
              (* Apply command *)
              let mask = L.of_database t.db in
              let global_slot = Mina_numbers.Global_slot_since_genesis.zero in
              let state_body =
                Mina_state.Protocol_state.body compile_time_genesis_state
              in
              let _, _, _, _, analytics_state =
                apply_user_command_without_check mask t.archive command
                  ~global_slot ~state_body ~analytics_state:t.analytics_state
                |> Or_error.ok_exn
              in
              t.analytics_state <- analytics_state ;
              L.Mask.Attached.commit mask ;
              return () )
      >>| Or_error.ok_exn >>| ignore
    in

    let current_root = get_root t in
    printf "Current root: %s\n%!" Ledger_hash.(to_decimal_string current_root) ;

    if not @@ Ledger_hash.equal current_root committed_ledger_hash then
      print_endline "Ledger mismatch" ;

    let sparse_ledger =
      Sparse_ledger.of_ledger_subset_exn
        L.(of_database t.db)
        [ Zkapps_rollup.inner_account_id ]
    in
    Merger.Context.set_last_committed_ledger t.merger_ctx sparse_ledger ;
    return ()

  let create ~logger ~zkapp_pk ~max_pool_size ~commitment_period_sec ~da_config
      ~da_quorum ~db_dir ~l1_uri ~archive_uri ~signer ~network_id
      ~deposit_delay_blocks ~provers =
    print_endline "Precomputing srs" ;
    Pickles.Side_loaded.srs_precomputation () ;
    let db =
      L.Db.create ?directory_name:db_dir
        ~depth:constraint_constants.ledger_depth ()
    in
    let config =
      Config.
        { max_pool_size
        ; commitment_period_sec
        ; db_dir
        ; l1_uri
        ; archive_uri
        ; zkapp_pk
        ; signer
        ; network_id
        ; deposit_delay_blocks
        }
    in
    let da_client =
      Da_layer.Client.Sequencer.create ~logger ~config:da_config
        ~quorum:da_quorum
    in
    let kvdb = L.Db.zeko_kvdb db in
    let provers =
      Zeko_prover.Client.create
        (List.map provers ~f:Tcp.Where_to_connect.of_host_and_port)
    in
    let executor = Executor.create ~l1_uri:config.l1_uri ~signer ~kvdb () in
    let t =
      { db
      ; logger
      ; archive = Archive.create ~kvdb:(L.Db.zeko_kvdb db)
      ; config
      ; da_client
      ; snark_q = Snark_queue.create ~config ~provers
      ; merger = Merger.P.create ()
      ; merger_ctx =
          { provers
          ; da_client
          ; executor
          ; config
          ; kvdb
          ; state = Merger.Context.load_state kvdb
          }
      ; stop = Ivar.create ()
      ; apply_q = Sequencer.create ()
      ; subscriptions = Subscriptions.create ()
      ; analytics_state = Analytics.State.empty
      }
    in
    let%bind () =
      if is_empty t then bootstrap ~logger t da_config
      else return @@ Merger.requeue_after_restart t.merger t.merger_ctx
    in
    let%bind () =
      Committer.recommit_all ~provers:t.snark_q.provers
        ~executor:t.merger_ctx.executor ~db ~zkapp_pk:config.zkapp_pk
        ~archive_uri:config.archive_uri
    in
    let%bind () =
      Da_layer.Client.check_synced_nodes ~logger ~config:da_config
        ~target_ledger_hash:(get_root t)
    in
    return t
end

let%test_module "Sequencer tests" =
  ( module struct
    let start_time = Time.now ()

    let () = Base.Backtrace.elide := false

    let logger = Logger.create ()

    module T = Transaction_snark.Make (struct
      let constraint_constants = constraint_constants

      let proof_level = Genesis_constants.Proof_level.Full
    end)

    module M = Zkapps_rollup.Make (T)
    open Sequencer

    let number_of_transactions = 5

    let gql_uri =
      { Cli_lib.Flag.Types.value = Uri.of_string "http://localhost:8080/graphql"
      ; name = "gql-uri"
      }

    let da_config = Da_layer.Client.Config.of_string_list [ "127.0.0.1:8555" ]

    let provers =
      [ Host_and_port.create ~host:"localhost" ~port:9990
      ; Host_and_port.create ~host:"localhost" ~port:9991
      ]

    module Sequencer_test_spec = struct
      type t =
        { zkapp_keypair : Keypair.t
        ; signer : Keypair.t
        ; ephemeral_ledger : L.t (* The ledger to test the expected outcome *)
        ; specs : Mina_transaction_logic.For_tests.Transaction_spec.t list
              (* Transaction specs *)
        ; sequencer : Sequencer.t
        }

      let gen ?(delay_deposit = 0) () =
        let zkapp_keypair = Keypair.create () in

        (* Create signer *)
        let signer = Keypair.create () in
        Thread_safe.block_on_async_exn (fun () ->
            let%bind _res =
              Gql_client.For_tests.create_account gql_uri
                (Public_key.compress signer.public_key)
            in
            return () ) ;

        let%bind.Quickcheck.Generator { init_ledger; specs } =
          Mina_transaction_logic.For_tests.Test_spec.mk_gen
            ~num_transactions:number_of_transactions ()
        in

        let genesis_accounts =
          (M.Inner.account_id, M.Inner.initial_account)
          :: ( Array.map init_ledger ~f:(fun (keypair, balance) ->
                   let pk =
                     Signature_lib.Public_key.compress keypair.public_key
                   in
                   let account_id = Account_id.create pk Token_id.default in
                   let balance = Unsigned.UInt64.of_int64 balance in
                   let account =
                     Account.create account_id
                       (Currency.Balance.of_uint64 balance)
                   in
                   (account_id, account) )
             |> Array.to_list )
        in

        (* Init ephemeral ledger *)
        let ephemeral_ledger =
          L.create_ephemeral ~depth:constraint_constants.ledger_depth ()
        in
        List.iter genesis_accounts ~f:(fun (aid, acc) ->
            L.create_new_account_exn ephemeral_ledger aid acc ) ;

        (* Post genesis batch *)
        Thread_safe.block_on_async_exn (fun () ->
            match%bind
              Da_layer.Client.distribute_genesis_diff ~logger ~config:da_config
                ~ledger:ephemeral_ledger
            with
            | Ok _ ->
                return ()
            | Error e ->
                Error.raise e ) ;

        (* Deploy *)
        Thread_safe.block_on_async_exn (fun () ->
            ( print_endline
            @@ Public_key.(
                 Compressed.to_base58_check @@ compress zkapp_keypair.public_key)
            ) ;
            let%bind nonce =
              Gql_client.infer_nonce gql_uri
                (Public_key.compress signer.public_key)
            in
            let command =
              Deploy.deploy_command_exn ~signer ~zkapp:zkapp_keypair
                ~fee:(Currency.Fee.of_mina_int_exn 1)
                ~nonce ~initial_ledger:ephemeral_ledger ~constraint_constants
                (module M)
            in
            let%bind _ = Gql_client.send_zkapp gql_uri command in
            let%bind _created = Gql_client.For_tests.create_new_block gql_uri in
            return () ) ;

        (* Init sequencer *)
        let sequencer =
          Thread_safe.block_on_async_exn (fun () ->
              Sequencer.create ~logger
                ~zkapp_pk:
                  Signature_lib.Public_key.(compress zkapp_keypair.public_key)
                ~max_pool_size:10 ~commitment_period_sec:0. ~da_config
                ~da_quorum:1 ~db_dir:None ~l1_uri:gql_uri ~archive_uri:gql_uri
                ~signer ~network_id:"testnet"
                ~deposit_delay_blocks:delay_deposit ~provers )
        in

        Quickcheck.Generator.return
          { zkapp_keypair; signer; ephemeral_ledger; specs; sequencer }
    end

    let sign_cmd (cmd : Zkapp_command.t) (keys : Keypair.t list) :
        Zkapp_command.t =
      let full_commitment =
        Zkapp_command.Transaction_commitment.create_complete
          (Zkapp_command.commitment cmd)
          ~memo_hash:(Signed_command_memo.hash cmd.memo)
          ~fee_payer_hash:
            (Zkapp_command.Digest.Account_update.create
               (Account_update.of_fee_payer cmd.fee_payer) )
      in
      let sign_raw (pk : Public_key.Compressed.t) msg =
        printf "Signing for %s\n" (Public_key.Compressed.to_base58_check pk) ;
        let rec go (keys : Keypair.t list) msg =
          match keys with
          | (kp : Keypair.t) :: keys ->
              if
                Public_key.Compressed.equal
                  (Public_key.compress kp.public_key)
                  pk
              then (
                printf "key found\n" ;
                Signature_lib.Schnorr.Chunked.sign
                  ~signature_kind:Mina_signature_kind.Testnet kp.private_key
                  (Random_oracle.Input.Chunked.field msg) )
              else (
                printf "not equal to %s\n"
                  Public_key.(
                    kp.public_key |> compress |> Compressed.to_base58_check) ;
                go keys msg )
          | [] ->
              failwithf "key not found: %s\n"
                (Public_key.Compressed.to_base58_check pk)
                ()
        in
        go keys msg
      in
      let rec sign_tree (tree : Zeko_util.call_forest_tree) :
          Zeko_util.call_forest_tree =
        { tree with
          account_update =
            { tree.account_update with
              authorization =
                ( match tree.account_update.body.authorization_kind with
                | Signature ->
                    assert tree.account_update.body.use_full_commitment ;
                    Signature
                      (sign_raw tree.account_update.body.public_key
                         full_commitment )
                | _ ->
                    tree.account_update.authorization )
            }
        ; calls = sign_forest tree.calls
        }
      and sign_forest (forest : Zeko_util.call_forest) : Zeko_util.call_forest =
        List.map ~f:(fun tree -> { tree with elt = sign_tree tree.elt }) forest
      in
      { cmd with
        fee_payer =
          { cmd.fee_payer with
            authorization =
              ( if
                Public_key.Compressed.(
                  equal empty cmd.fee_payer.body.public_key)
              then cmd.fee_payer.authorization
              else sign_raw cmd.fee_payer.body.public_key full_commitment )
          }
      ; account_updates = sign_forest cmd.account_updates
      }

    let%test_unit "apply commands and commit" =
      print_endline "Started test 'apply commands and commit'" ;
      Quickcheck.test ~trials:1 (Sequencer_test_spec.gen ())
        ~f:(fun { zkapp_keypair; signer; ephemeral_ledger; specs; sequencer } ->
          let batch1, batch2 = List.split_n specs 3 in

          (* Apply first batch *)
          let () =
            Thread_safe.block_on_async_exn (fun () ->
                let source_ledger_hash = get_root sequencer in

                [%test_eq: Ledger_hash.t] source_ledger_hash
                  (L.merkle_root ephemeral_ledger) ;

                let%bind () =
                  Deferred.List.iteri batch1 ~f:(fun i spec ->
                      [%test_eq: Ledger_hash.t] (get_root sequencer)
                        (L.merkle_root ephemeral_ledger) ;
                      let%map result =
                        match i % 2 = 0 with
                        | true ->
                            let command =
                              Mina_transaction_logic.For_tests
                              .account_update_send spec
                            in
                            ( match
                                L.apply_zkapp_command_unchecked ephemeral_ledger
                                  command ~constraint_constants
                                  ~global_slot:
                                    Mina_numbers.Global_slot_since_genesis.zero
                                  ~state_view:
                                    Mina_state.Protocol_state.(
                                      Body.view
                                      @@ body compile_time_genesis_state)
                              with
                            | Ok (applied, _) ->
                                [%test_eq: Transaction_status.t]
                                  applied.command.status Applied
                            | Error e ->
                                Error.raise
                                  (Error.create "Expected ledger apply failed" e
                                     Error.sexp_of_t ) ) ;

                            apply_user_command sequencer (Zkapp_command command)
                        | false ->
                            let command =
                              Mina_transaction_logic.For_tests.command_send spec
                            in
                            ( match
                                L.apply_user_command_unchecked ephemeral_ledger
                                  command ~constraint_constants
                                  ~txn_global_slot:
                                    Mina_numbers.Global_slot_since_genesis.zero
                              with
                            | Ok applied ->
                                [%test_eq: Transaction_status.t]
                                  applied.common.user_command.status Applied
                            | Error e ->
                                Error.raise
                                  (Error.create "Expected ledger apply failed" e
                                     Error.sexp_of_t ) ) ;

                            apply_user_command sequencer (Signed_command command)
                      in

                      let txn_applied, command_witness =
                        match result with
                        | Ok result ->
                            result
                        | Error e ->
                            Error.raise e
                      in
                      don't_wait_for
                      @@ Merger.P.add_job sequencer.merger sequencer.merger_ctx
                           ~data:command_witness ;

                      let status =
                        Mina_transaction_logic.Transaction_applied
                        .transaction_status txn_applied
                      in
                      [%test_eq: Transaction_status.t] status Applied )
                in

                let target_ledger_hash = get_root sequencer in

                [%test_eq: Ledger_hash.t] target_ledger_hash
                  (L.merkle_root ephemeral_ledger) ;

                return () )
          in

          (* First commit *)
          Thread_safe.block_on_async_exn (fun () ->
              let%bind _ = commit sequencer in
              let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in
              let%bind () =
                Executor.wait_to_finish sequencer.merger_ctx.executor
              in
              let%bind committed_ledger_hash =
                Gql_client.infer_committed_state gql_uri
                  ~signer_pk:(Public_key.compress signer.public_key)
                  ~zkapp_pk:(Public_key.compress zkapp_keypair.public_key)
              in
              let target_ledger_hash = get_root sequencer in
              [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;

              Deferred.unit ) ;

          (* To test nonce inferring from pool *)
          (* The first commit is still in the pool *)
          Executor.refresh_nonce sequencer.merger_ctx.executor ;

          (* Apply second batch *)
          Thread_safe.block_on_async_exn (fun () ->
              let source_ledger_hash = get_root sequencer in

              [%test_eq: Ledger_hash.t] source_ledger_hash
                (L.merkle_root ephemeral_ledger) ;

              let%bind () =
                Deferred.List.iteri batch2 ~f:(fun i spec ->
                    let%map result =
                      match i % 2 = 0 with
                      | true ->
                          let command =
                            Mina_transaction_logic.For_tests.account_update_send
                              spec
                          in
                          ( match
                              L.apply_zkapp_command_unchecked ephemeral_ledger
                                command ~constraint_constants
                                ~global_slot:
                                  Mina_numbers.Global_slot_since_genesis.zero
                                ~state_view:
                                  Mina_state.Protocol_state.(
                                    Body.view @@ body compile_time_genesis_state)
                            with
                          | Ok _ ->
                              ()
                          | Error e ->
                              Error.raise
                                (Error.create "Expected ledger apply failed" e
                                   Error.sexp_of_t ) ) ;

                          apply_user_command sequencer (Zkapp_command command)
                      | false ->
                          let command =
                            Mina_transaction_logic.For_tests.command_send spec
                          in
                          ( match
                              L.apply_user_command_unchecked ephemeral_ledger
                                command ~constraint_constants
                                ~txn_global_slot:
                                  Mina_numbers.Global_slot_since_genesis.zero
                            with
                          | Ok _ ->
                              ()
                          | Error e ->
                              Error.raise
                                (Error.create "Expected ledger apply failed" e
                                   Error.sexp_of_t ) ) ;

                          apply_user_command sequencer (Signed_command command)
                    in

                    let txn_applied, command_witness =
                      match result with
                      | Ok result ->
                          result
                      | Error e ->
                          Error.raise e
                    in
                    don't_wait_for
                    @@ Merger.P.add_job sequencer.merger sequencer.merger_ctx
                         ~data:command_witness ;

                    let status =
                      Mina_transaction_logic.Transaction_applied
                      .transaction_status txn_applied
                    in
                    [%test_eq: Transaction_status.t] status Applied )
              in

              let target_ledger_hash = get_root sequencer in

              [%test_eq: Ledger_hash.t] target_ledger_hash
                (L.merkle_root ephemeral_ledger) ;

              return () ) ;

          (* Second commit *)
          let final_ledger_hash =
            Thread_safe.block_on_async_exn (fun () ->
                let%bind _ = commit sequencer in
                let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in
                let%bind () =
                  Executor.wait_to_finish sequencer.merger_ctx.executor
                in
                let%bind _created =
                  Gql_client.For_tests.create_new_block gql_uri
                in
                let%bind committed_ledger_hash =
                  Gql_client.fetch_committed_state gql_uri
                    Signature_lib.Public_key.(compress zkapp_keypair.public_key)
                in
                let target_ledger_hash = get_root sequencer in
                [%test_eq: Ledger_hash.t] committed_ledger_hash
                  target_ledger_hash ;

                return target_ledger_hash )
          in

          (* Try to bootstrap again *)
          Thread_safe.block_on_async_exn (fun () ->
              let%bind new_sequencer =
                Sequencer.create ~logger
                  ~zkapp_pk:
                    Signature_lib.Public_key.(compress zkapp_keypair.public_key)
                  ~max_pool_size:10 ~commitment_period_sec:0. ~da_config
                  ~da_quorum:1 ~db_dir:None ~l1_uri:gql_uri ~archive_uri:gql_uri
                  ~signer ~network_id:"testnet" ~deposit_delay_blocks:0 ~provers
              in
              return
              @@ [%test_eq: Frozen_ledger_hash.t] (get_root new_sequencer)
                   final_ledger_hash ) )

    let%test_unit "dummy signature should fail" =
      print_endline "Started test 'dummy signature should fail'" ;
      Quickcheck.test ~trials:1 (Sequencer_test_spec.gen ())
        ~f:(fun { zkapp_keypair; signer; ephemeral_ledger; specs; sequencer } ->
          let dummy_signature_command : Zkapp_command.t =
            let command =
              Mina_transaction_logic.For_tests.account_update_send
                (List.hd_exn specs)
            in
            { command with
              account_updates =
                Zkapp_command.Call_forest.map command.account_updates
                  ~f:(fun account_update ->
                    match Account_update.authorization account_update with
                    | Signature _ ->
                        { account_update with
                          authorization = Signature Signature.dummy
                        }
                    | _ ->
                        account_update )
            }
          in
          let result =
            Thread_safe.block_on_async_exn (fun () ->
                apply_user_command sequencer
                  (Zkapp_command dummy_signature_command) )
          in
          match result with
          | Error e
            when String.is_substring ~substring:"Invalid_signature"
                   (Error.to_string_hum e) ->
              ()
          | Ok _ ->
              failwith "Transaction should have failed"
          | Error unexpected_error ->
              Error.raise unexpected_error )

    let%test_unit "deposits" =
      print_endline "Started test 'deposits'" ;
      Quickcheck.test ~trials:1 (Sequencer_test_spec.gen ~delay_deposit:2 ())
        ~f:(fun { zkapp_keypair; signer; ephemeral_ledger; specs; sequencer } ->
          (* Create l1 accounts *)
          let l1_accounts =
            Array.create ~len:5 ()
            |> Array.map ~f:Signature_lib.Keypair.create
            |> Array.to_list
          in
          Thread_safe.block_on_async_exn (fun () ->
              Deferred.List.iter l1_accounts ~f:(fun keypair ->
                  let%bind _res =
                    Gql_client.For_tests.create_account gql_uri
                      (Signature_lib.Public_key.compress keypair.public_key)
                  in
                  return () ) ) ;

          (* Send deposits *)
          let deposits =
            Thread_safe.block_on_async_exn (fun () ->
                let submit_deposit ~fee (signer : Keypair.t) deposit =
                  let%bind nonce =
                    Gql_client.fetch_nonce gql_uri
                      (Signature_lib.Public_key.compress signer.public_key)
                  in
                  let fee_payer =
                    Account_update.Fee_payer.
                      { body =
                          { public_key = Public_key.compress signer.public_key
                          ; fee = Currency.Fee.of_mina_int_exn fee
                          ; valid_until = None
                          ; nonce = Account.Nonce.of_uint32 nonce
                          }
                      ; authorization = Signature.dummy
                      }
                  in
                  let%bind transfer_update =
                    M.Outer.submit_deposit
                      ~outer_public_key:
                        (Public_key.compress zkapp_keypair.public_key)
                      ~deposit
                  in
                  let transferrer_update : Account_update.t =
                    { body =
                        { Account_update.Body.dummy with
                          public_key = Public_key.compress signer.public_key
                        ; balance_change =
                            Currency.Amount.Signed.(
                              negate @@ of_unsigned deposit.amount)
                        ; use_full_commitment = true
                        ; authorization_kind = Signature
                        }
                    ; authorization = Signature Signature.dummy
                    }
                  in
                  let transfer_cmd : Zkapp_command.t =
                    { fee_payer
                    ; account_updates =
                        Zkapp_command.Call_forest.(
                          cons_tree transfer_update @@ accumulate_hashes'
                          @@ of_account_updates
                               ~account_update_depth:(fun _ -> 0)
                               [ transferrer_update ])
                    ; memo = Signed_command_memo.empty
                    }
                  in
                  return @@ sign_cmd transfer_cmd [ signer ]
                in
                let account1 = List.nth_exn l1_accounts 0 in
                let account2 = List.nth_exn l1_accounts 1 in
                let account3 = List.nth_exn l1_accounts 2 in
                let account4 = List.nth_exn l1_accounts 3 in
                let account5 = List.nth_exn l1_accounts 4 in

                let deposit1 : Zkapps_rollup.TR.t =
                  { recipient = Public_key.compress account1.public_key
                  ; amount = Currency.Amount.of_mina_int_exn 10
                  }
                in
                let deposit2 : Zkapps_rollup.TR.t =
                  { recipient = Public_key.compress account2.public_key
                  ; amount = Currency.Amount.of_mina_int_exn 20
                  }
                in
                let deposit3 : Zkapps_rollup.TR.t =
                  { recipient = Public_key.compress account3.public_key
                  ; amount = Currency.Amount.of_mina_int_exn 30
                  }
                in
                let deposit4 : Zkapps_rollup.TR.t =
                  { recipient = Public_key.compress account4.public_key
                  ; amount = Currency.Amount.of_mina_int_exn 40
                  }
                in
                let deposit5 : Zkapps_rollup.TR.t =
                  { recipient = Public_key.compress account5.public_key
                  ; amount = Currency.Amount.of_mina_int_exn 50
                  }
                in

                (* Send deposits for accounts 1 and 2 *)
                let%bind _ =
                  submit_deposit ~fee:5 account1 deposit1
                  >>= Gql_client.send_zkapp gql_uri
                in
                let%bind _ =
                  submit_deposit ~fee:4 account2 deposit2
                  >>= Gql_client.send_zkapp gql_uri
                in

                (* Create 2 new blocks for delay *)
                let%bind _created =
                  Gql_client.For_tests.create_new_block gql_uri
                in
                let%bind _created =
                  Gql_client.For_tests.create_new_block gql_uri
                in

                (* Send deposits for accounts 3, 4 and 5 which won't be processed *)
                let%bind _ =
                  submit_deposit ~fee:3 account3 deposit3
                  >>= Gql_client.send_zkapp gql_uri
                in
                let%bind _ =
                  submit_deposit ~fee:2 account4 deposit4
                  >>= Gql_client.send_zkapp gql_uri
                in
                let%bind _ =
                  submit_deposit ~fee:1 account5 deposit5
                  >>= Gql_client.send_zkapp gql_uri
                in
                let%bind _created =
                  Gql_client.For_tests.create_new_block gql_uri
                in
                return [ deposit1; deposit2; deposit3; deposit4; deposit5 ] )
          in

          (* Commit should process first 2 deposits *)
          Thread_safe.block_on_async_exn (fun () ->
              let%bind _ = commit sequencer in
              let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in
              let%bind () =
                Executor.wait_to_finish sequencer.merger_ctx.executor
              in
              let%bind _created =
                Gql_client.For_tests.create_new_block gql_uri
              in
              let%bind committed_ledger_hash =
                Gql_client.fetch_committed_state gql_uri
                  Signature_lib.Public_key.(compress zkapp_keypair.public_key)
              in
              let target_ledger_hash = get_root sequencer in
              [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;

              return () ) ;

          let deposits_state =
            Utils.get_inner_deposits_state_exn (L.of_database sequencer.db)
          in
          let expected_deposits_state =
            (* Expected should be only first 2 deposits *)
            List.take deposits 2
            |> List.fold ~init:Zkapp_account.Actions.empty_state_element
                 ~f:(fun acc transfer ->
                   Zkapp_account.Actions.push_events acc
                     (Zkapps_rollup.TR.to_actions transfer) )
          in
          [%test_eq: Field.t] deposits_state expected_deposits_state ;

          print_endline "Processing remaining deposits" ;

          (* Create new blocks to process remaining deposits *)
          Thread_safe.block_on_async_exn (fun () ->
              let%bind _created =
                Gql_client.For_tests.create_new_block gql_uri
              in
              let%bind _created =
                Gql_client.For_tests.create_new_block gql_uri
              in
              return () ) ;

          (* Commit should process remaining deposits *)
          Thread_safe.block_on_async_exn (fun () ->
              let%bind _ = commit sequencer in
              let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in
              let%bind () =
                Executor.wait_to_finish sequencer.merger_ctx.executor
              in
              let%bind _created =
                Gql_client.For_tests.create_new_block gql_uri
              in
              let%bind committed_ledger_hash =
                Gql_client.fetch_committed_state gql_uri
                  Signature_lib.Public_key.(compress zkapp_keypair.public_key)
              in
              let target_ledger_hash = get_root sequencer in
              [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;

              return () ) ;

          let deposits_state =
            Utils.get_inner_deposits_state_exn (L.of_database sequencer.db)
          in
          let expected_deposits_state =
            List.fold deposits ~init:Zkapp_account.Actions.empty_state_element
              ~f:(fun acc transfer ->
                Zkapp_account.Actions.push_events acc
                  (Zkapps_rollup.TR.to_actions transfer) )
          in
          [%test_eq: Field.t] deposits_state expected_deposits_state )

    let () =
      printf "Sequencer tests took %s\n"
        (Time.Span.to_string (Time.diff (Time.now ()) start_time))
  end )
