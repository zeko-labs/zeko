open Core_kernel
open Async_kernel
open Mina_base
open Mina_ledger
open Mina_numbers
open Signature_lib
open Zeko_types
module C = Zeko_circuits
module L = Ledger
module Field = Snark_params.Tick.Field

module Sequencer = struct
  let constraint_constants = Zeko_constants.constraint_constants

  module Config = struct
    type t =
      { max_pool_size : int
      ; commitment_period_sec : float
      ; db_dir : string
      ; checkpoints_dir : string option
      ; signer : Signer_service.Signer.t
      ; l1_uri : Uri.t
      ; archive_uri : Uri.t
      ; deposit_delay_blocks : int
      ; fee_modifier : float
      ; minimum_fee : float
      ; l1_config : Utils.Slot.l1_config
      ; slot_acceptance : Time.Span.t
      ; commit_validity_period : Global_slot_span.t
      }
  end

  module State = struct
    type t = Kvdb_base.t

    module Fee_excess = struct
      include Kvdb_base.Make_singleton (struct
        type t = Currency.Fee.t [@@deriving yojson]

        let key = "fee_excess"
      end)

      let get t = get t |> Option.value ~default:Currency.Fee.zero

      let add t fee =
        let old_fee_excess = get t in
        set t
          ~data:
            ( Currency.Fee.add old_fee_excess fee
            |> Option.value_exn ~message:"Fee excess overflow" )

      let reset t = set t ~data:Currency.Fee.zero
    end

    module Last_committed_ledger = Kvdb_base.Make_singleton (struct
      type t = Sparse_ledger.t [@@deriving yojson]

      let key = "last_committed_ledger"
    end)
  end

  let keypair = Keypair.create ()

  let sok_digest =
    Sok_message.digest
    @@ Sok_message.create ~fee:Currency.Fee.zero
         ~prover:(Public_key.compress keypair.public_key)

  module Merger = struct
    module Context = struct
      type t =
        { provers : Zeko_prover.Client.t
        ; da_client : Da_layer.Client.t
        ; executor : Executor.t
        ; config : Config.t
        ; sequencer_state : State.t
        ; db_pool : Relational_db.Db.pool
        ; archive : Archive.t
        ; logger : Logger.t
        ; proof_cache_db : Proof_cache_tag.cache_db
        }
    end

    module Merge = struct
      type t = Txn_snark.serializable

      let process ({ provers; _ } : Context.t) ((left, left_proof) : t)
          ((right, right_proof) : t) =
        match%map
          Utils.retry
            ~f:(fun () ->
              Zeko_prover.Client.transaction_snark provers
                (Merge { left; left_proof; right; right_proof }) )
            ()
        with
        | Ok snark ->
            snark
        | Error err ->
            Monitor.send_exn Monitor.main (Error.to_exn err) ;
            Error.raise err
    end

    module Base = struct
      type t = Txn_snark_witness.t [@@deriving yojson]

      let process (ctx : Context.t) witness =
        match%map
          Utils.retry
            ~f:(fun () ->
              match witness with
              | Txn_snark_witness.Zkapp_command segment ->
                  Zeko_prover.Client.transaction_snark ctx.provers
                    (Zkapp_command segment)
              | Signed_command w ->
                  Zeko_prover.Client.transaction_snark ctx.provers
                    (Signed_command w) )
            ()
        with
        | Ok snark ->
            snark
        | Error err ->
            Monitor.send_exn Monitor.main (Error.to_exn err) ;
            Error.raise err
    end

    module Commit = struct
      type t =
        { new_inner_ledger : Sparse_ledger.t
        ; processed_actions_pointer : Field.t
        }
      [@@deriving yojson]

      type out = unit -> (unit, Caqti_error.t) Result.t Deferred.t

      let process
          ({ da_client
           ; provers
           ; executor
           ; config
           ; sequencer_state
           ; archive
           ; logger
           ; db_pool
           ; proof_cache_db
           ; _
           } :
            Context.t ) { new_inner_ledger; processed_actions_pointer }
          txn_snark =
        match%map
          Utils.retry
            ~f:(fun () ->
              let open Deferred.Result.Let_syntax in
              let%bind da_multisig =
                Da_layer.Client.get_multisig da_client
                  ~ledger_hash:(Sparse_ledger.merkle_root new_inner_ledger)
                |> Deferred.map ~f:(fun (quorum, multisig) ->
                       Multisig.Witness.make ~signatures:multisig ~quorum )
                |> Deferred.map ~f:Result.return
              in

              let old_inner_ledger =
                State.Last_committed_ledger.get sequencer_state
                |> Option.value_exn ~message:"No previous committed ledger"
              in
              let commit_witness : Committer.Commit_witness.t =
                { old_inner_ledger
                ; new_inner_ledger
                ; processed_actions_pointer
                ; da_multisig
                ; txn_snark
                }
              in
              let%bind command =
                Committer.prove_commit ~logger ~proof_cache_db ~provers
                  ~executor ~archive
                  ~zkapp_pk:Zeko_circuits_config.Inputs.zeko_l1
                  ~archive_uri:config.archive_uri ~l1_config:config.l1_config
                  ~commit_validity_period:config.commit_validity_period
                  commit_witness
              in
              let%bind () =
                Executor.send_zkapp_command ~logger executor command
              in
              State.Last_committed_ledger.set sequencer_state
                ~data:new_inner_ledger ;
              return (fun () ->
                  let open Relational_db in
                  Pool.use
                    (fun conn ->
                      Committer.Commit_table.insert conn
                        { source_ledger_hash =
                            Sparse_ledger.merkle_root old_inner_ledger
                        ; target_ledger_hash =
                            Sparse_ledger.merkle_root new_inner_ledger
                        ; witness = commit_witness
                        } )
                    db_pool ) )
            ()
        with
        | Ok result ->
            result
        | Error err ->
            Monitor.send_exn Monitor.main (Error.to_exn err) ;
            Error.raise err
    end

    module M = struct
      include Parallel_merger.Make (Context) (Merge) (Base) (Commit)
      module Context = Context
      module Merge = Merge
      module Base = Base
      module Commit = Commit
    end

    module P = Parallel_merger.Persisted.Make (M)
  end

  module State_hashes = struct
    type t =
      { proved_ledger_hash : Ledger_hash.t
      ; unproved_ledger_hash : Ledger_hash.t
      ; committed_ledger_hash : Ledger_hash.t
      }
  end

  type t =
    { ledger : L.Db.t
    ; imt : Indexed_merkle_tree.Db.t
    ; db_pool : Relational_db.Db.pool
    ; state : State.t
    ; logger : Logger.t
    ; archive : Archive.t
    ; config : Config.t
    ; bridge_prover : Bridge_prover.t
    ; merger : Merger.M.t
    ; merger_ctx : Merger.Context.t
    ; da_client : Da_layer.Client.t
    ; closed : unit Ivar.t
    ; apply_q : unit Sequencer.t
          (* Applying of the user command is async operation, but we need to keep the application synchronous *)
    }

  let shutdown t =
    let logger = t.logger in
    [%log info] "Shutting down sequencer" ;
    Ivar.fill t.closed () ;
    Da_layer.Client.stop t.da_client ;
    L.Db.close t.ledger ;
    Indexed_merkle_tree.Db.close t.imt ;
    Relational_db.Pool.drain t.db_pool

  let add_account t account_id account =
    ( L.Db.get_or_create_account t.ledger account_id account |> Or_error.ok_exn
      : [ `Added | `Existed ] * L.Db.Location.t )
    |> ignore

  let get_account t public_key token_id =
    let account_id = Account_id.create public_key token_id in
    let%bind.Option location = L.Db.location_of_account t.ledger account_id in
    L.Db.get t.ledger location

  let infer_nonce t public_key =
    match get_account t public_key Token_id.default with
    | Some account ->
        account.nonce
    | None ->
        Unsigned.UInt32.zero

  let get_root t = L.Db.merkle_root t.ledger

  let is_empty t = L.Db.num_accounts t.ledger = 0

  let get_latest_state t =
    (* TODO: proved hashes *)
    State_hashes.
      { proved_ledger_hash = Field.zero
      ; unproved_ledger_hash = get_root t
      ; committed_ledger_hash = Field.zero
      }

  let apply_events_and_actions t command =
    let ledger = L.of_database t.ledger in
    Zkapp_command.(Call_forest.to_list (Poly.account_updates command))
    |> List.mapi ~f:(fun i update ->
           let%bind.Result account =
             match
               let account_id =
                 Account_id.create
                   (Account_update.public_key update)
                   (Account_update.token_id update)
               in
               let%bind.Option location =
                 Ledger.location_of_account ledger account_id
               in
               Ledger.get ledger location
             with
             | Some acc ->
                 Ok acc
             | None ->
                 Error (Error.of_string "Account not present in the db")
           in
           Ok
             (Archive.add_account_update t.archive i update account
                (Some
                   Archive.Transaction_info.
                     { status = Applied
                     ; hash =
                         Mina_transaction.Transaction_hash.hash_command
                           (Zkapp_command
                              (Zkapp_command.read_all_proofs_from_disk command)
                           )
                     ; memo = Zkapp_command.Poly.memo command
                     ; authorization_kind =
                         Account_update.Body.authorization_kind update.body
                     ; sequence_no = 0
                     ; zkapp_account_update_ids =
                         Zkapp_command.Poly.account_updates command
                         |> List.mapi ~f:(fun i _ -> i)
                     } ) ) )
    |> Or_error.combine_errors |> Result.map ~f:ignore

  let calculate_required_fee t weight =
    let jobs_in_queue = Zeko_prover.Client.queue_size t.merger_ctx.provers in
    let Config.{ minimum_fee; fee_modifier; _ } = t.config in
    List.init weight ~f:Fn.id
    |> List.fold ~init:0. ~f:(fun acc i ->
           acc
           +. Utils.fee_per_weight_unit ~minimum_fee ~fee_modifier
                ~jobs_in_queue:(Float.of_int (jobs_in_queue + i)) )

  (** Apply user command to the sequencer's state, including the check of command validity *)
  let apply_user_command t ?(skip_validity_check = false)
      (command : User_command.t) =
    if
      Throttle.num_jobs_waiting_to_start t.apply_q >= t.config.max_pool_size
      && not skip_validity_check
    then
      return
        (Error (Error.of_string "Sequencer is under the load, try again later"))
    else
      Throttle.enqueue t.apply_q (fun () ->
          (* TODO: instead apply directly from prover *)
          let is_deposit_finalization = Utils.is_deposit_finalization command in
          let%bind.Deferred.Result command =
            if is_deposit_finalization then
              match command with
              | Zkapp_command
                  ( { fee_payer = { body = { public_key = fee_payer_pk; _ }; _ }
                    ; _
                    } as zkapp_command )
                when Public_key.Compressed.equal fee_payer_pk
                       (Signer_service.Signer.public_key t.config.signer) ->
                  let signature_kind = Zeko_circuits_config.Inputs.chain_l2 in
                  Signer_service.Signer.sign_fee_payer ~signature_kind
                    t.config.signer
                    (Zkapp_command.read_all_proofs_from_disk zkapp_command)
                  >>| Result.map
                        ~f:
                          (Zkapp_command.write_all_proofs_to_disk
                             ~signature_kind
                             ~proof_cache_db:
                               (Proof_cache_tag.create_identity_db ()) )
                  >>| Result.map ~f:(fun zkapp_command ->
                          User_command.Zkapp_command zkapp_command )
              | _ ->
                  return (Ok command)
            else return (Ok command)
          in

          let%bind.Deferred.Result () =
            if skip_validity_check || is_deposit_finalization then return (Ok ())
            else
              let weight = User_command.weight command in
              let required_fee = calculate_required_fee t weight in
              let command_fee =
                User_command.fee command |> Currency.Fee.to_nanomina_int
                |> Float.of_int
              in
              if Float.(command_fee < required_fee) then
                return
                  (Error
                     ( Error.of_info
                     @@ Info.create
                          (Format.asprintf "Fee is too low, expected %f"
                             (required_fee /. 10e8) )
                          (required_fee /. 10e8) Float.sexp_of_t ) )
              else return (Ok ())
          in

          let l1_global_slot =
            Utils.Slot.global_slot ~l1_config:t.config.l1_config
          in
          let acceptable_future_slot =
            Global_slot_since_genesis.add l1_global_slot
              ( (Float.to_int @@ Time.Span.to_min t.config.slot_acceptance) / 3
              |> Global_slot_span.of_int )
          in
          let%bind.Deferred.Result () =
            if skip_validity_check then return (Ok ())
            else
              match Utils.command_slot_range command with
              | None ->
                  return (Error (Error.of_string "Conflicting slot ranges"))
              | Some { lower; upper } ->
                  if Global_slot_since_genesis.(lower > l1_global_slot) then
                    return
                      (Error (Error.of_string "Lower slot is in the future"))
                  else if
                    Global_slot_since_genesis.(upper < acceptable_future_slot)
                  then
                    return
                      (Error
                         (Error.of_string
                            "Upper slot has too small margin to be committed" )
                      )
                  else return (Ok ())
          in

          let l = L.of_database t.ledger in

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
                      ~allowed_empty_fee_payer:is_deposit_finalization
                      ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
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

          let%bind.Deferred.Result source_ledger, witnesses =
            let sequencer_pk =
              Even_PC.create_exn
              @@ Signer_service.Signer.public_key t.config.signer
            in
            return
              (Zeko_transaction_logic.apply_user_command_unchecked
                 ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
                 ~sequencer_pk ~zeko_env:Zeko_transaction_logic.zeko_dummy_env
                 ~constraint_constants ~global_slot:l1_global_slot l t.imt
                 command )
          in

          let%bind.Deferred.Result () =
            match command with
            | Signed_command _ ->
                return (Ok ( (* Signed command has no events nor actions *) ))
            | Zkapp_command command ->
                return (apply_events_and_actions t command)
          in

          (* Accumulate fee *)
          State.Fee_excess.add t.state (User_command.fee command) ;

          (* Post transaction to the DA layer *)
          let changed_accounts =
            let account_ids =
              User_command.accounts_referenced command
              |> List.map ~f:(fun id ->
                     if Public_key.Compressed.(Account_id.public_key id = empty)
                     then Zeko_constants.inner_account_id
                     else id )
              |> List.stable_dedup
            in
            List.map account_ids ~f:(fun id ->
                let index = L.index_of_account_exn l id in
                (index, L.get_at_index_exn l index) )
          in
          let diff =
            Da_layer.Diff.create
              ~source_ledger_hash:(Sparse_ledger.merkle_root source_ledger)
              ~changed_accounts
              ~command_with_action_step_flags:
                (Some
                   ( User_command.read_all_proofs_from_disk command
                   , match command with
                     | Signed_command _ ->
                         []
                     | Zkapp_command command ->
                         Zkapp_command.all_account_updates_list command
                         |> List.map ~f:(fun _ -> true) ) )
          in
          let new_accounts_keys =
            List.filter changed_accounts ~f:(fun (index, _) ->
                Account.equal
                  (Sparse_ledger.get_exn source_ledger index)
                  Account.empty )
            |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
            |> List.map ~f:(fun (_, account) ->
                   Account_id.derive_token_id
                     ~owner:(Account.identifier account) )
          in
          let%bind () =
            Da_layer.Client.enqueue_diff t.da_client ~genesis:false
              ~ledger_openings:source_ledger
              ~acc_set_openings:
                (Indexed_merkle_tree.Sparse.of_db_subset ~logger:t.logger
                   ~db:t.imt ~keys:new_accounts_keys )
              ~diff
              ~target_ledger_hash:(L.Db.merkle_root t.ledger)
          in

          (* Add witnesses to the merger *)
          let%bind.Deferred.Result () =
            Deferred.List.iter ~how:`Sequential witnesses ~f:(fun witness ->
                Merger.P.add_job t.db_pool t.merger t.merger_ctx ~data:witness
                >>| Relational_db.caqti_ok_exn
                      ~msg:"Fatal error: failed to add witness: %s" )
            >>| Result.return
          in

          return (Ok ()) )

  let apply_fee_transfer t =
    let fee = State.Fee_excess.get t.state in
    if Currency.Fee.(equal fee zero) then return `No_fee
    else
      let receiver_pk =
        Even_PC.create_exn @@ Signer_service.Signer.public_key t.config.signer
      in
      let l1_global_slot =
        Utils.Slot.global_slot ~l1_config:t.config.l1_config
      in
      let ledger = L.of_database t.ledger in

      let receiver_location =
        L.location_of_account ledger
          (Account_id.of_public_key
             (Signer_service.Signer.public_key_decompressed t.config.signer) )
      in
      if
        Option.is_none receiver_location
        && Currency.Fee.(fee < constraint_constants.account_creation_fee)
      then return `Skip
      else
        match
          Zeko_transaction_logic.apply_fee_transfer_unchecked ~receiver_pk ~fee
            ~constraint_constants ~global_slot:l1_global_slot ledger t.imt
        with
        | Error e ->
            return (`Error e)
        | Ok (source_ledger, witness) -> (
            State.Fee_excess.reset t.state ;

            (* Post transaction to the DA layer *)
            let changed_accounts =
              let account_ids =
                [ Account_id.of_public_key
                    (Signer_service.Signer.public_key_decompressed
                       t.config.signer )
                ]
              in
              List.map account_ids ~f:(fun id ->
                  let index = L.index_of_account_exn ledger id in
                  (index, L.get_at_index_exn ledger index) )
            in
            let diff =
              (* FIXME: add fee transfer command to DA *)
              Da_layer.Diff.create
                ~source_ledger_hash:(Sparse_ledger.merkle_root source_ledger)
                ~changed_accounts ~command_with_action_step_flags:None
            in
            let new_accounts_keys =
              List.filter changed_accounts ~f:(fun (index, _) ->
                  Account.equal
                    (Sparse_ledger.get_exn source_ledger index)
                    Account.empty )
              |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
              |> List.map ~f:(fun (_, account) ->
                     Account_id.derive_token_id
                       ~owner:(Account.identifier account) )
            in
            let%bind () =
              Da_layer.Client.enqueue_diff t.da_client ~genesis:false
                ~ledger_openings:source_ledger
                ~acc_set_openings:
                  (Indexed_merkle_tree.Sparse.of_db_subset ~logger:t.logger
                     ~db:t.imt ~keys:new_accounts_keys )
                ~diff
                ~target_ledger_hash:(L.Db.merkle_root t.ledger)
            in
            match%map
              Merger.P.add_job t.db_pool t.merger t.merger_ctx ~data:witness
            with
            | Ok () ->
                `Ok
            | Error e ->
                `Error (Error.of_string (Caqti_error.show e)) )

  let current_synced_outer_action_state t =
    Utils.get_synced_outer_action_state_exn (L.of_database t.ledger)

  let update_inner_account t =
    let open Deferred.Result.Let_syntax in
    let logger = t.logger in
    let old_synced_outer_action_state, old_deposits_length =
      let s = current_synced_outer_action_state t in
      C.Rollup_state.Outer_action_state.With_length.(raw s, length s)
    in
    let%bind all_new_actions =
      Gql_client.fetch_actions ~logger t.config.archive_uri
        ~from_action_state:old_synced_outer_action_state
        Zeko_circuits_config.Inputs.zeko_l1
    in
    [%log info] "All new actions: %d" (List.length all_new_actions) ;
    let%bind current_height =
      Gql_client.fetch_block_height ~logger t.config.l1_uri
    in
    (* Find pointer for actions to be processed *)
    let processed_pointer, processed_new_actions =
      List.fold all_new_actions ~init:(old_synced_outer_action_state, [])
        ~f:(fun
             (curr_state, curr_actions)
             (action, `Block_height block_height, _, _, _)
           ->
          if block_height + t.config.deposit_delay_blocks <= current_height then
            ( Zkapp_account.Actions_impl.(push_hash curr_state (hash action))
            , action :: curr_actions )
          else (curr_state, curr_actions) )
      |> Tuple2.map_snd ~f:List.rev
    in
    let proof_cache_db = t.merger_ctx.proof_cache_db in
    if Field.equal old_synced_outer_action_state processed_pointer then (
      (* In case no new actions are to process, we don't need to update inner account *)
      [%log info] "No new actions to process" ;
      return (0, old_synced_outer_action_state) )
    else (
      [%log info] "Processing %d new actions from %s to %s"
        (List.length processed_new_actions)
        (Field.to_string old_synced_outer_action_state)
        (Field.to_string processed_pointer) ;
      let%bind forest =
        let%map (body, _, calls), proof =
          Zeko_prover.Client.inner_sync t.bridge_prover.provers
            ~public_key:Zeko_constants.inner_public_key
            ~ase_elms:
              (List.map processed_new_actions ~f:Zkapp_account.Actions_impl.hash)
            ~ase_source:
              ( { action_state = old_synced_outer_action_state
                ; length = old_deposits_length
                }
                : C.Ase.With_length.Stmt.t )
        in
        (* see #286 *)
        Utils.attach_proof_to_forest
          ~signature_kind:Zeko_circuits_config.Inputs.chain_l2 ~proof_cache_db
          ~body ~calls ~proof
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
            Zkapp_command.Call_forest.map forest
              ~f:(Account_update.write_all_proofs_to_disk ~proof_cache_db)
        ; memo = Signed_command_memo.empty
        }
      in
      let%map () =
        (* Skip validity check because dummy fee payer triggers invalid public key error *)
        apply_user_command t ~skip_validity_check:true (Zkapp_command command)
      in
      let processed_witnesses =
        List.filter_map processed_new_actions ~f:(function
          | [ action ] ->
              Some (Utils.actions_to_outer_action action)
          | _ ->
              None )
        |> List.filter ~f:(function Commit _ -> false | Witness _ -> true)
      in
      (List.length processed_witnesses, processed_pointer) )

  (** Double Deferred.t is deliberate, the first is filled after update of ledger, the second is filled after commit *)
  let commit t :
      Txn_snark.serializable option Deferred.Or_error.t Deferred.Or_error.t =
    let logger = t.logger in
    let open Deferred.Result.Let_syntax in
    let%map processed_witnesses, processed_actions_pointer =
      update_inner_account t
    in
    Throttle.enqueue t.apply_q (fun () ->
        match%bind.Deferred apply_fee_transfer t with
        | `Skip ->
            [%log info]
              "Skipping commit because there's not enough accumulated fee to \
               create recipient account" ;
            return None
        | `Error e ->
            Deferred.return (Error e)
        | `No_fee | `Ok ->
            let tree_leaves =
              Merger.M.current_tree t.merger
              |> Option.map ~f:(fun tree ->
                     Merger.M.Tree.base_jobs_count tree.value )
              |> Option.value ~default:0
            in
            (* If the only txn was update of inner account, we don't need to commit *)
            if tree_leaves <= 1 && processed_witnesses = 0 then (
              [%log info] "Nothing to commit" ;
              return None )
            else
              let target_ledger =
                Sparse_ledger.of_ledger_subset_exn
                  L.(of_database t.ledger)
                  [ Zeko_constants.inner_account_id ]
              in
              let () =
                match t.config.checkpoints_dir with
                | Some dir ->
                    let open Core in
                    let dir =
                      dir
                      ^/ ( Sparse_ledger.merkle_root target_ledger
                         |> Ledger_hash.to_decimal_string )
                    in
                    if not (FileUtil.test Exists dir) then (
                      Core.Unix.mkdir_p dir ;
                      [%log info] "Making checkpoint for ledger and imt in %s"
                        dir ;
                      Ledger.Db.make_checkpoint t.ledger
                        ~directory_name:(dir ^/ "ledger") ;
                      Indexed_merkle_tree.Db.make_checkpoint t.imt
                        ~directory_name:(dir ^/ "imt") )
                | None ->
                    ()
              in
              let%bind.Deferred result =
                Merger.P.commit_exn t.db_pool t.merger t.merger_ctx
                  ~commit_witness:
                    { new_inner_ledger = target_ledger
                    ; processed_actions_pointer
                    }
              in
              return (Some result) )

  let run_committer t =
    if Float.(t.config.commitment_period_sec <= 0.) then ()
    else
      let logger = t.logger in
      let period = Time_ns.Span.of_sec t.config.commitment_period_sec in
      let rec go () =
        let after = after period in
        let%bind ledger_applied = commit t >>| Or_error.ok_exn in
        let%bind () =
          match%map ledger_applied >>| Or_error.ok_exn with
          | Some (stmt, _) ->
              [%log info] "Committed: %s -> %s"
                (Ledger_hash.to_decimal_string stmt.source_ledger)
                (Ledger_hash.to_decimal_string stmt.target_ledger)
          | None ->
              [%log info] "Skipped commit"
        in
        let%bind () = Deferred.any [ after; Ivar.read t.closed ] in
        if Ivar.is_full t.closed then return () else go ()
      in
      don't_wait_for (within' ~monitor:Monitor.main (fun () -> go ()))

  let sync ~logger ({ config; _ } as t) da_config source =
    [%log info] "Syncing" ;
    let%bind commited_ledger_hash =
      Gql_client.infer_state ~logger config.l1_uri
        ~zkapp_pk:Zeko_circuits_config.Inputs.zeko_l1
        ~signer_pk:(Signer_service.Signer.public_key config.signer)
      >>| Or_error.ok_exn
      >>| Utils.value_of_zkapp_state Zeko_circuits.Rollup_state.Outer_state.typ
      >>| fun { ledger_hash; _ } -> ledger_hash
    in
    [%log info] "Fetched commited root: %s"
      Ledger_hash.(to_decimal_string commited_ledger_hash) ;

    [%log info] "Init root: %s" Ledger_hash.(to_decimal_string (get_root t)) ;

    let%bind () =
      match source with
      | `Genesis ->
          [%log info] "Syncing from genesis" ;
          return ()
      | `Specific ledger_hash ->
          [%log info] "Syncing from specific ledger hash: %s"
            (Ledger_hash.to_decimal_string ledger_hash) ;

          [%log info] "Creating genesis diff" ;
          let ledger = L.of_database t.ledger in
          let%bind diffs =
            Da_layer.Client.create_genesis_diffs ~logger ledger
          in
          let%bind () =
            Deferred.List.iteri ~how:`Sequential diffs
              ~f:(fun
                   i
                   ( diff
                   , ledger_openings
                   , acc_set_openings
                   , `Target target_ledger_hash )
                 ->
                Da_layer.Client.enqueue_diff t.da_client ~diff ~ledger_openings
                  ~acc_set_openings ~target_ledger_hash ~genesis:(i = 0) )
          in
          [%log info] "Enqueued genesis diff" ;
          return ()
    in

    (* apply diffs from DA layer *)
    let%bind () =
      Da_layer.Client.map_diffs
        ?interval_size:
          (Sys.getenv_opt "ZEKO_INTERVAL_SIZE" |> Option.map ~f:Int.of_string)
        ~logger ~config:da_config ~depth:constraint_constants.ledger_depth
        ~source_ledger_hash:source ~target_ledger_hash:commited_ledger_hash ()
        ~f:(fun ~current_chunk ~current_diff ~chunks_length diff ->
          assert (
            Ledger_hash.equal
              (Da_layer.Diff.Stable.Latest.source_ledger_hash diff)
              (get_root t) ) ;
          [%log info]
            "Applying diff with source ledger hash %s, progress: %.0f%%"
            (Ledger_hash.to_decimal_string
               (Da_layer.Diff.Stable.Latest.source_ledger_hash diff) )
            (Float.of_int current_chunk /. Float.of_int chunks_length *. 100.0) ;

          (* Apply accounts diff *)
          let mask = L.of_database t.ledger in
          let ledger_openings =
            Da_layer.Client.get_ledger_openings
              ~diff:(Da_layer.Diff.drop_time diff)
              ~ledger:mask
          in
          let changed_accounts =
            Da_layer.Diff.Stable.Latest.changed_accounts diff
            |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
          in
          List.iter changed_accounts ~f:(fun (index, account) ->
              L.set_at_index_exn mask index account ) ;
          L.Mask.Attached.commit mask ;

          (* Add to Indexed Merkle Tree *)
          List.iter changed_accounts ~f:(fun (_, account) ->
              let aid = Account.identifier account in
              let _w =
                Indexed_merkle_tree.Db.get_or_create_entry_exn t.imt
                  (Account_id.derive_token_id ~owner:aid)
              in
              () ) ;

          let acc_set_openings =
            Da_layer.Client.get_acc_set_openings ~logger
              ~diff:(Da_layer.Diff.drop_time diff)
              ~ledger_openings ~imt:t.imt
          in

          (* Store diff to DA client *)
          let%bind () =
            Da_layer.Client.enqueue_diff t.da_client
              ~diff:(Da_layer.Diff.drop_time diff)
              ~ledger_openings ~acc_set_openings
              ~target_ledger_hash:(L.Db.merkle_root t.ledger)
              ~genesis:(current_chunk = 0 && current_diff = 0)
          in

          (* Add events and actions *)
          let result =
            match
              Da_layer.Diff.Stable.Latest.command_with_action_step_flags diff
            with
            | Some (Zkapp_command command, _) ->
                apply_events_and_actions t
                  (Zkapp_command.write_all_proofs_to_disk
                     ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
                     ~proof_cache_db:t.merger_ctx.proof_cache_db command )
            | _ ->
                Ok ( (* No events nor actions to add *) )
          in
          return
            ( match result with
            | Ok () ->
                ()
            | Error e ->
                [%log warn] "Warning: Failed to add events and actions: %s"
                  (Error.to_string_hum e) ) )
      >>| Or_error.ok_exn >>| ignore
    in

    let current_root = get_root t in
    [%log info] "Current root: %s" Ledger_hash.(to_decimal_string current_root) ;
    [%log info] "IMT root: %s"
      (Ledger_hash.to_decimal_string @@ Indexed_merkle_tree.Db.merkle_root t.imt) ;

    if not @@ Ledger_hash.equal current_root commited_ledger_hash then
      [%log error] "Ledger mismatch" ;

    let sparse_ledger =
      Sparse_ledger.of_ledger_subset_exn
        L.(of_database t.ledger)
        [ Zeko_constants.inner_account_id ]
    in
    State.Last_committed_ledger.set t.state ~data:sparse_ledger ;
    return ()

  let create_ledger ~logger ~db_dir ~(checkpoints_dir : string option) ~zkapp_pk
      ~l1_uri ~signer_pk ~archive_uri =
    let ledger_dir = Filename.concat db_dir "ledger" in
    let imt_dir = Filename.concat db_dir "imt" in
    match (FileUtil.test Is_dir ledger_dir, FileUtil.test Is_dir imt_dir) with
    | true, true ->
        [%log info] "Ledger and IMT directories exist %s and %s" ledger_dir
          imt_dir ;
        return
          ( `Synced
          , ( L.Db.create ~directory_name:ledger_dir
                ~depth:constraint_constants.ledger_depth ()
            , Indexed_merkle_tree.Db.create ~directory_name:imt_dir
                ~depth:constraint_constants.ledger_depth () ) )
    | false, false -> (
        [%log info] "No ledger and IMT directories exist, fetching commits" ;
        let%bind commits =
          Gql_client.fetch_actions ~logger archive_uri zkapp_pk
          >>| Or_error.ok_exn
          >>| List.filter_map ~f:(fun (fields, _, _, _, _) ->
                  match fields with
                  | [ action ] ->
                      Some (Utils.actions_to_outer_action action)
                  | _ ->
                      None )
          >>| List.filter_map ~f:(function
                | C.Rollup_state.Outer_action.Commit commit ->
                    Some commit.ledger
                | Witness _ ->
                    None )
        in
        let%map commited_ledger_hash =
          Gql_client.infer_state ~logger l1_uri
            ~zkapp_pk:Zeko_circuits_config.Inputs.zeko_l1
            ~signer_pk:(Public_key.compress signer_pk)
          >>| Or_error.ok_exn
          >>| Utils.value_of_zkapp_state
                Zeko_circuits.Rollup_state.Outer_state.typ
          >>| fun { ledger_hash; _ } -> ledger_hash
        in
        let commits_from_newest =
          commits @ [ commited_ledger_hash ] |> List.rev
        in
        match
          List.find commits_from_newest ~f:(fun ledger_hash ->
              match checkpoints_dir with
              | Some checkpoints_dir ->
                  FileUtil.test Is_dir
                    (Filename.concat checkpoints_dir
                       (Ledger_hash.to_decimal_string ledger_hash) )
              | None ->
                  false )
        with
        | None ->
            [%log info]
              "No checkpoint found, creating new ledger and IMT %s and %s"
              ledger_dir imt_dir ;
            ( `Syncing_from `Genesis
            , ( L.Db.create ~directory_name:ledger_dir
                  ~depth:constraint_constants.ledger_depth ()
              , Indexed_merkle_tree.Db.create ~directory_name:imt_dir
                  ~depth:constraint_constants.ledger_depth () ) )
        | Some latest_checkpoint ->
            let checkpoint_dir =
              Filename.concat
                (Option.value_exn checkpoints_dir
                   ~message:"checkpoints_dir should not be None, unreachable" )
                (Ledger_hash.to_decimal_string latest_checkpoint)
            in
            let ledger_checkpoint_dir =
              Filename.concat checkpoint_dir "ledger"
            in
            let imt_checkpoint_dir = Filename.concat checkpoint_dir "imt" in
            [%log info]
              "Checkpoint found for ledger hash %s, creating ledger and IMT \
               from checkpoint %s and %s"
              (Ledger_hash.to_decimal_string latest_checkpoint)
              ledger_checkpoint_dir imt_checkpoint_dir ;
            if
              FileUtil.test Is_dir ledger_checkpoint_dir
              && FileUtil.test Is_dir imt_checkpoint_dir
            then
              let () = Core.Unix.mkdir_p db_dir in
              ( `Syncing_from (`Specific latest_checkpoint)
              , ( Utils.create_db_from_checkpoint
                    (module L.Db)
                    ~depth:constraint_constants.ledger_depth ~db_dir:ledger_dir
                    ~checkpoint_dir:ledger_checkpoint_dir
                , Utils.create_db_from_checkpoint
                    (module Indexed_merkle_tree.Db)
                    ~depth:constraint_constants.ledger_depth ~db_dir:imt_dir
                    ~checkpoint_dir:imt_checkpoint_dir ) )
            else failwithf "Corrupted checkpoint %s" checkpoint_dir () )
    | true, false | false, true ->
        failwithf "Corrupted db %s and %s" ledger_dir imt_dir ()

  let create ~logger ~max_pool_size ~commitment_period_sec ~da_config ~da_keys
      ~da_quorum ~db_dir ~checkpoints_dir ~postgres_uri ~l1_uri ~archive_uri
      ~(signer : Signer_service.Signer.t) ~deposit_delay_blocks ~mq_host
      ~fee_modifier ~minimum_fee ~slot_acceptance ~proof_cache_db ~l1_config
      ~commit_validity_period =
    [%log info] "Precomputing srs" ;
    Pickles.Side_loaded.srs_precomputation () ;
    let db_dir =
      match db_dir with
      | Some db_dir ->
          db_dir
      | None ->
          let uuid = Uuid_unix.create () in
          Filename.concat Cache_dir.autogen_path (Uuid.to_string uuid)
    in
    let%bind sync_check, (ledger, imt) =
      create_ledger ~logger ~db_dir ~checkpoints_dir
        ~zkapp_pk:Zeko_circuits_config.Inputs.zeko_l1 ~l1_uri
        ~signer_pk:(Signer_service.Signer.public_key_decompressed signer)
        ~archive_uri
    in
    let config =
      Config.
        { max_pool_size
        ; commitment_period_sec
        ; db_dir
        ; checkpoints_dir
        ; l1_uri
        ; archive_uri
        ; signer
        ; deposit_delay_blocks
        ; fee_modifier
        ; minimum_fee
        ; l1_config
        ; slot_acceptance
        ; commit_validity_period
        }
    in
    let%bind db_pool = Db.create_and_migrate ~postgres_uri ~logger in
    let%bind da_client =
      Da_layer.Client.create ~logger ~config:da_config ~quorum:da_quorum
        ~da_keys ~db_pool
    in
    let kvdb = L.Db.zeko_kvdb ledger in
    let%bind provers = Zeko_prover.Client.create ~logger ~db_pool ~mq_host in
    let executor =
      Executor.create ~l1_uri:config.l1_uri
        ~signature_kind:Zeko_circuits_config.Inputs.chain_l1 ~signer ~kvdb ()
    in
    let archive = Archive.create ~kvdb in
    let merger_ctx =
      Merger.Context.
        { provers
        ; da_client
        ; executor
        ; config
        ; sequencer_state = kvdb
        ; db_pool
        ; archive
        ; logger
        ; proof_cache_db
        }
    in
    let%bind merger = Merger.P.create_and_requeue ~logger merger_ctx db_pool in
    let t =
      { ledger
      ; imt
      ; db_pool
      ; state = kvdb
      ; logger
      ; archive
      ; config
      ; da_client
      ; bridge_prover =
          Bridge_prover.create ~provers ~proof_cache_db
            ~fee_recipient_l1:(Signer_service.Signer.public_key signer)
            ~fee_recipient_l2:(Signer_service.Signer.public_key signer)
      ; merger
      ; merger_ctx
      ; closed = Ivar.create ()
      ; apply_q = Sequencer.create ()
      }
    in
    let%bind () =
      match sync_check with
      | `Syncing_from source ->
          sync ~logger t da_config source
      | `Synced ->
          return ()
    in
    let%bind () =
      Committer.recommit_all ~logger ~proof_cache_db
        ~provers:t.bridge_prover.provers ~executor:t.merger_ctx.executor
        ~archive ~db_pool ~zkapp_pk:Zeko_circuits_config.Inputs.zeko_l1
        ~archive_uri:config.archive_uri ~l1_config ~commit_validity_period
      >>| Or_error.ok_exn
    in
    let () =
      Da_layer.Client.start_client da_client ~target_ledger_hash:(get_root t)
    in
    return t
end
