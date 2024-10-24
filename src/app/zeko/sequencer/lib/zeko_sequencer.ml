open Base
open Core_kernel
open Async
open Async_kernel
open Mina_base
open Mina_ledger
open Signature_lib
module L = Ledger
module Field = Snark_params.Tick.Field

let constraint_constants = Genesis_constants.Constraint_constants.compiled

module Make (T : Transaction_snark.S) (M : Zkapps_rollup.S) = struct
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

  let keypair = Keypair.create ()

  let sok_digest =
    Sok_message.digest
    @@ Sok_message.create ~fee:Currency.Fee.zero
         ~prover:(Public_key.compress keypair.public_key)

  module Snark_queue = struct
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

    module State = struct
      type t =
        { last : Zkapps_rollup.t option
        ; staged_commands : User_command.t list
        ; queued_commands : Command_witness.t list
        ; previous_committed_ledger_hash : Ledger_hash.t option
        ; previous_committed_ledger : Sparse_ledger.t option
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
            Some (Sparse_ledger.merkle_root previous_ledger)
        ; previous_committed_ledger = Some previous_ledger
        }
    end

    module Kvdb = struct
      let ok_exn x =
        let open Ppx_deriving_yojson_runtime.Result in
        match x with Ok x -> x | Error e -> failwith e

      module Key_value = struct
        type _ t = Snark_queue_state : (unit * State.t) t

        let serialize_key : type k v. (k * v) t -> k -> Bigstring.t =
         fun pair_type key ->
          match pair_type with
          | Snark_queue_state ->
              Bigstring.of_string "snark_queue_state"

        let serialize_value : type k v. (k * v) t -> v -> Bigstring.t =
         fun pair_type value ->
          match pair_type with
          | Snark_queue_state ->
              Bigstring.of_string @@ Yojson.Safe.to_string
              @@ State.to_yojson value

        let deserialize_value : type k v. (k * v) t -> Bigstring.t -> v =
         fun pair_type data ->
          match pair_type with
          | Snark_queue_state ->
              ok_exn @@ State.of_yojson @@ Yojson.Safe.from_string
              @@ Bigstring.to_string data
      end

      include Kvdb_base.Make (Key_value)
    end

    type t =
      { q : unit Throttle.t
      ; da_client : Da_layer.Client.Sequencer.t
      ; config : Config.t
      ; transfers_memory : Transfers_memory.t
      ; executor : Executor.t
      ; kvdb : Kvdb.t
      ; mutable state : State.t
      }

    let create ~da_client ~config ~signer ~kvdb =
      { q = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1
      ; da_client
      ; config
      ; transfers_memory = Transfers_memory.create ~lifetime:Float.(60. * 10.)
      ; executor = Executor.create ~l1_uri:config.l1_uri ~signer ~kvdb ()
      ; kvdb
      ; state = State.create ()
      }

    let queue_size t = Throttle.num_jobs_waiting_to_start t.q

    let persist_state ~kvdb t () =
      Kvdb.set kvdb Snark_queue_state ~key:() ~data:t

    let get_state ~kvdb = Kvdb.get kvdb Snark_queue_state ~key:()

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
      let handler = unstage @@ Sparse_ledger.handler sparse_ledger in
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
          let () = persist_state ~kvdb:t.kvdb t.state () in
          result )

    let enqueue_prove_command t command_witness =
      t.state <- State.add_queued_command t.state command_witness ;
      persist_state ~kvdb:t.kvdb t.state () ;
      enqueue t (fun () ->
          match command_witness with
          | Command_witness.Signed_command
              (sparse_ledger, user_command_in_block, statement) ->
              prove_signed_command t ~sparse_ledger ~user_command_in_block
                ~statement
          | Command_witness.Zkapp_command (witnesses, zkapp_command) ->
              prove_zkapp_command t ~witnesses ~zkapp_command )

    let enqueue_prove_transfer_request t ~key ~(transfer : Transfer.t) =
      Throttle.enqueue t.q (fun () ->
          let%bind result =
            try_with (fun () ->
                match transfer with
                | { direction = Deposit; transfer } ->
                    M.Outer.submit_deposit ~outer_public_key:t.config.zkapp_pk
                      ~deposit:transfer
                | { direction = Withdraw; transfer } ->
                    M.Inner.submit_withdrawal ~withdrawal:transfer )
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
                    M.Inner.process_deposit ~is_new ~pointer ~before ~after
                      ~deposit:transfer
                | { transfer = { direction = Withdraw; transfer }
                  ; is_new
                  ; pointer
                  ; before
                  ; after
                  } ->
                    M.Outer.process_withdrawal
                      ~outer_public_key:t.config.zkapp_pk ~is_new ~pointer
                      ~before ~after ~withdrawal:transfer )
          in
          let () =
            match result with
            | Ok (_, forest) ->
                Transfers_memory.add t.transfers_memory key (Ok forest)
            | Error e ->
                printf "Warning: prove_transfer_claim failed %s\n%!"
                  (Exn.to_string e) ;
                Transfers_memory.add t.transfers_memory key
                  (Error (Exn.to_string e))
          in
          return () )

    let enqueue_prove_commit t ~target_ledger ~old_deposits_pointer
        ~processed_deposits_pointer =
      enqueue t (fun () ->
          match List.is_empty t.state.staged_commands with
          | true ->
              print_endline "Nothing to commit" ;
              return ()
          | false -> (
              print_endline "Committing..." ;

              match%bind
                try_with (fun () ->
                    let%bind signatures =
                      Da_layer.Client.Sequencer.get_signatures t.da_client
                        ~ledger_hash:(Sparse_ledger.merkle_root target_ledger)
                      |> Deferred.map ~f:(fun x -> Option.value_exn x)
                    in
                    printf "Received %d signatures from da layer\n%!"
                      (List.length signatures) ;

                    let old_inner_ledger =
                      Option.value_exn t.state.previous_committed_ledger
                    in
                    let new_inner_ledger = target_ledger in
                    let commit_witness : Committer.Commit_witness.t =
                      { old_inner_ledger
                      ; new_inner_ledger
                      ; old_deposits_pointer
                      ; processed_deposits_pointer
                      ; signatures
                      ; last_snark = Option.value_exn t.state.last
                      }
                    in
                    Committer.Store.store_commit t.kvdb commit_witness
                      ~source:(Sparse_ledger.merkle_root old_inner_ledger)
                      ~target:(Sparse_ledger.merkle_root new_inner_ledger) ;

                    let%bind command =
                      Committer.prove_commit
                        (module M)
                        ~executor:t.executor ~zkapp_pk:t.config.zkapp_pk
                        ~archive_uri:t.config.archive_uri commit_witness
                    in
                    return @@ don't_wait_for
                    @@ Executor.send_zkapp_command t.executor command )
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
    ; stop : unit Ivar.t
    ; da_client : Da_layer.Client.Sequencer.t
    ; apply_q : unit Sequencer.t
          (* Applying of the user command is async operation, but we need to keep the application synchronous *)
    ; mutable subscriptions : Subscriptions.t
    ; mutable number_of_transactions : int
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
    let committed_ledger_hash =
      Option.value ~default:Field.zero
        t.snark_q.state.previous_committed_ledger_hash
    in
    State_hashes.
      { proved_ledger_hash =
          Option.map t.snark_q.state.last ~f:Zkapps_rollup.target_ledger
          |> Option.value ~default:committed_ledger_hash
      ; unproved_ledger_hash = get_root t
      ; committed_ledger_hash
      }

  let trigger_state_hashes_changed t =
    let state_hashes = get_latest_state t in
    List.iter t.subscriptions.state_hashes_changed ~f:(fun w ->
        Pipe.write_without_pushback_if_open w state_hashes )

  (** Apply user command to the ledger without checking the validity of the command *)
  let apply_user_command_without_check l archive command ~global_slot
      ~state_body =
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
      match L.Transaction_applied.transaction_status txn_applied with
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
                    L.location_of_account l account_id |> Option.value_exn
                  in
                  L.get l location |> Option.value_exn
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
    (first_pass_ledger, second_pass_ledger, txn_applied, target_ledger_hash)

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
            return
            @@
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
                Verifier.verify_command { data = verifiable; status = Applied }
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
                                   , target_ledger_hash ) =
            return
              (apply_user_command_without_check l t.archive command ~global_slot
                 ~state_body )
          in

          (* Post transaction to the DA layer *)
          let changed_accounts =
            let account_ids =
              User_command.accounts_referenced command
              |> List.map ~f:(fun id ->
                     if Public_key.Compressed.(Account_id.public_key id = empty)
                     then M.Inner.account_id
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

          t.number_of_transactions <- t.number_of_transactions + 1 ;

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
                    ( L.Transaction_applied.supply_increase txn_applied
                    |> Or_error.ok_exn )
                  ~pending_coinbase_stack_state:pc
              in
              Result.return
                ( txn_applied
                , Snark_queue.Command_witness.Signed_command
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
                , Snark_queue.Command_witness.Zkapp_command
                    (witnesses, zkapp_command) ) )

  let update_inner_account t =
    let old_deposits_state =
      Utils.get_inner_deposits_state_exn (module M) (L.of_database t.db)
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
        M.Inner.step ~all_deposits:processed_pointer
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
            match L.Transaction_applied.transaction_status status with
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
        @@ Snark_queue.enqueue_prove_command t.snark_q command_witness
      in
      return (old_deposits_state, processed_pointer)

  let commit t =
    let%bind old_deposits_pointer, processed_pointer = update_inner_account t in
    let target_ledger =
      Sparse_ledger.of_ledger_subset_exn
        L.(of_database t.db)
        [ M.Inner.account_id ]
    in
    Snark_queue.enqueue_prove_commit t.snark_q ~target_ledger
      ~old_deposits_pointer ~processed_deposits_pointer:processed_pointer

  let run_committer t =
    if Float.(t.config.commitment_period_sec <= 0.) then ()
    else
      let period = Time_ns.Span.of_sec t.config.commitment_period_sec in
      every ~start:(after period) ~stop:(Ivar.read t.stop) period (fun () ->
          don't_wait_for @@ commit t )

  let bootstrap ~logger ({ config; snark_q; _ } as t) da_config =
    let%bind committed_ledger_hash =
      Gql_client.infer_committed_state config.l1_uri ~zkapp_pk:config.zkapp_pk
        ~signer_pk:(Public_key.compress config.signer.public_key)
    in
    printf "Fetched root: %s\n%!"
      Ledger_hash.(to_decimal_string committed_ledger_hash) ;

    printf "Init root: %s\n%!" Ledger_hash.(to_decimal_string (get_root t)) ;

    (* apply diffs from DA layer *)
    let%bind ledger_hashes_chain =
      Da_layer.Client.get_ledger_hashes_chain ~logger ~config:da_config
        ~depth:constraint_constants.ledger_depth
        ~target_ledger_hash:committed_ledger_hash
      |> Deferred.map ~f:Or_error.ok_exn
    in
    let%bind () =
      Deferred.List.iter ~how:`Sequential ledger_hashes_chain
        ~f:(fun ledger_hash ->
          let%bind diff : Da_layer.Diff.t Deferred.t =
            Da_layer.Client.get_diff ~logger ~config:da_config ~ledger_hash
            |> Deferred.map ~f:(fun r -> Or_error.ok_exn r)
          in
          assert (
            Ledger_hash.equal
              (Da_layer.Diff.source_ledger_hash diff)
              (get_root t) ) ;
          match Da_layer.Diff.command_with_action_step_flags diff with
          | None ->
              (* Apply accounts diff *)
              let mask = L.of_database t.db in
              let changed_accounts = Da_layer.Diff.changed_accounts diff in
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
              let _res =
                apply_user_command_without_check mask t.archive command
                  ~global_slot ~state_body
                |> Or_error.ok_exn
              in
              L.Mask.Attached.commit mask ;
              return () )
    in

    let current_root = get_root t in
    printf "Current root: %s\n%!" Ledger_hash.(to_decimal_string current_root) ;

    if not @@ Ledger_hash.equal current_root committed_ledger_hash then
      print_endline "Ledger mismatch" ;

    let sparse_ledger =
      Sparse_ledger.of_ledger_subset_exn
        L.(of_database t.db)
        [ M.Inner.account_id ]
    in
    t.snark_q.state <-
      Snark_queue.State.reset_for_new_batch t.snark_q.state sparse_ledger ;
    return ()

  let create ~logger ~zkapp_pk ~max_pool_size ~commitment_period_sec ~da_config
      ~da_quorum ~db_dir ~l1_uri ~archive_uri ~signer ~network_id
      ~deposit_delay_blocks =
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
    let t =
      { db
      ; logger
      ; archive = Archive.create ~kvdb:(L.Db.zeko_kvdb db)
      ; config
      ; da_client
      ; snark_q =
          Snark_queue.create ~da_client ~config ~signer
            ~kvdb:(L.Db.zeko_kvdb db)
      ; stop = Ivar.create ()
      ; apply_q = Sequencer.create ()
      ; subscriptions = Subscriptions.create ()
      ; number_of_transactions = 0
      }
    in
    let%bind () =
      if is_empty t then bootstrap ~logger t da_config
      else (
        Snark_queue.get_state ~kvdb:(L.Db.zeko_kvdb db)
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
      Committer.recommit_all
        (module M)
        ~executor:t.snark_q.executor ~db ~zkapp_pk:config.zkapp_pk
        ~archive_uri:config.archive_uri
    in
    let%bind () =
      match%bind
        Da_layer.Client.sync_nodes ~logger ~config:da_config
          ~depth:constraint_constants.ledger_depth
          ~target_ledger_hash:(get_root t)
      with
      | Ok _ ->
          return ()
      | Error e ->
          Error.raise e
    in
    return t

  module Statistics = struct
    type t =
      { transactions : int
      ; deposits : int
      ; lumina_swaps : int
      ; lumina_lps : int
      }

    let get ~sequencer ~lumina_factory =
      let%bind deposits =
        Gql_client.fetch_transfers sequencer.config.archive_uri
          sequencer.config.zkapp_pk
        >>| List.length
      in
      let%bind lps =
        Gql_client.fetch_events sequencer.config.archive_uri lumina_factory
        >>| List.join
        >>| List.filter_map ~f:(function
              | _sender_x :: _sender_oddity :: pool_x :: pool_oddity :: _ ->
                  (* only one event type, so no index *)
                  (* https://github.com/Lumina-DEX/lumina-mvp/blob/284341acc924bf21ac6947514e23cce7088e68ed/contracts/src/PoolFactory.ts#L39 *)
                  Some
                    ( { x = pool_x
                      ; is_odd =
                          ( if Field.equal pool_oddity Field.zero then false
                          else true )
                      }
                      : Public_key.Compressed.t )
              | _ ->
                  None )
      in
      let%bind swaps =
        Deferred.List.map ~how:`Parallel lps ~f:(fun lp ->
            Gql_client.fetch_events sequencer.config.archive_uri lp
            >>| List.join
            >>| List.filter ~f:(function
                  | event_type :: _ ->
                      (* [1] is event type [addLiquidity] *)
                      (* https://github.com/Lumina-DEX/lumina-mvp/blob/284341acc924bf21ac6947514e23cce7088e68ed/contracts/src/PoolMina.ts#L61 *)
                      if Field.equal event_type Field.one then true else false
                  | _ ->
                      false ) )
        >>| List.length
      in
      return
        { transactions = sequencer.number_of_transactions
        ; deposits
        ; lumina_swaps = swaps
        ; lumina_lps = List.length lps
        }
  end
end

let prover_modules :
    ((module Transaction_snark.S) * (module Zkapps_rollup.S)) lazy_t =
  lazy
    (let module T = Transaction_snark.Make (struct
       let constraint_constants = constraint_constants

       let proof_level = Genesis_constants.Proof_level.Full
     end) in
    let module M = Zkapps_rollup.Make (T) in
    ((module T), (module M)) )

let%test_module "Sequencer tests" =
  ( module struct
    let () = Base.Backtrace.elide := false

    let logger = Logger.create ()

    module T = Transaction_snark.Make (struct
      let constraint_constants = constraint_constants

      let proof_level = Genesis_constants.Proof_level.Full
    end)

    module M = Zkapps_rollup.Make (T)
    module Sequencer = Make (T) (M)
    open Sequencer

    let number_of_transactions = 5

    let gql_uri =
      { Cli_lib.Flag.Types.value = Uri.of_string "http://localhost:8080/graphql"
      ; name = "gql-uri"
      }

    let da_config = Da_layer.Client.Config.of_string_list [ "127.0.0.1:8555" ]

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
                ~deposit_delay_blocks:delay_deposit )
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
                      @@ Snark_queue.enqueue_prove_command sequencer.snark_q
                           command_witness ;

                      let status =
                        L.Transaction_applied.transaction_status txn_applied
                      in
                      [%test_eq: Transaction_status.t] status Applied )
                in

                let target_ledger_hash = get_root sequencer in

                [%test_eq: Ledger_hash.t] target_ledger_hash
                  (L.merkle_root ephemeral_ledger) ;

                let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in

                [%test_eq: Bool.t] true
                  (Option.is_some sequencer.snark_q.state.last) ;
                let snark = Option.value_exn sequencer.snark_q.state.last in
                (* let stmt = Zkapps_rollup.Wrapper_rules.statement snark in *)
                [%test_eq: Ledger_hash.t]
                  (Zkapps_rollup.source_ledger snark)
                  source_ledger_hash ;
                [%test_eq: Ledger_hash.t]
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
                  ~signer_pk:(Public_key.compress signer.public_key)
                  ~zkapp_pk:(Public_key.compress zkapp_keypair.public_key)
              in
              let target_ledger_hash = get_root sequencer in
              [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;

              Deferred.unit ) ;

          (* To test nonce inferring from pool *)
          (* The first commit is still in the pool *)
          Executor.refresh_nonce sequencer.snark_q.executor ;

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
                    @@ Snark_queue.enqueue_prove_command sequencer.snark_q
                         command_witness ;

                    let status =
                      L.Transaction_applied.transaction_status txn_applied
                    in
                    [%test_eq: Transaction_status.t] status Applied )
              in

              let target_ledger_hash = get_root sequencer in

              [%test_eq: Ledger_hash.t] target_ledger_hash
                (L.merkle_root ephemeral_ledger) ;

              let%bind () = Snark_queue.wait_to_finish sequencer.snark_q in

              [%test_eq: Bool.t] true
                (Option.is_some sequencer.snark_q.state.last) ;
              let snark = Option.value_exn sequencer.snark_q.state.last in
              (* let stmt = Zkapps_rollup.Wrapper_rules.statement snark in *)
              [%test_eq: Ledger_hash.t]
                (Zkapps_rollup.source_ledger snark)
                source_ledger_hash ;
              [%test_eq: Ledger_hash.t]
                (Zkapps_rollup.target_ledger snark)
                target_ledger_hash ;

              return () ) ;

          (* Second commit *)
          let final_ledger_hash =
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
                  ~signer ~network_id:"testnet" ~deposit_delay_blocks:0
              in
              return
              @@ [%test_eq: Frozen_ledger_hash.t] (get_root new_sequencer)
                   final_ledger_hash ) )

    let%test_unit "dummy signature should fail" =
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
              [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;

              return () ) ;

          let deposits_state =
            Utils.get_inner_deposits_state_exn
              (module M)
              (L.of_database sequencer.db)
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
              [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;

              return () ) ;

          let deposits_state =
            Utils.get_inner_deposits_state_exn
              (module M)
              (L.of_database sequencer.db)
          in
          let expected_deposits_state =
            List.fold deposits ~init:Zkapp_account.Actions.empty_state_element
              ~f:(fun acc transfer ->
                Zkapp_account.Actions.push_events acc
                  (Zkapps_rollup.TR.to_actions transfer) )
          in
          [%test_eq: Field.t] deposits_state expected_deposits_state )
  end )
