open Core_kernel
open Async
open Async_kernel
open Mina_base
open Mina_ledger
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
      ; db_dir : string option
      ; zkapp_pk : Public_key.Compressed.t
      ; signer : Keypair.t
      ; l1_uri : Uri.t Cli_lib.Flag.Types.with_name
      ; archive_uri : Uri.t Cli_lib.Flag.Types.with_name
      ; network_id : Mina_signature_kind.t
      ; deposit_delay_blocks : int
      ; da_key : Even_PC.t
      ; fee_modifier : float
      ; minimum_fee : float
      }
  end

  let keypair = Keypair.create ()

  let sok_digest =
    Sok_message.digest
    @@ Sok_message.create ~fee:Currency.Fee.zero
         ~prover:(Public_key.compress keypair.public_key)

  module Merger = struct
    module Context = struct
      module State = struct
        type t =
          { mutable previous_committed_ledger : Sparse_ledger.t option
          ; mutable previous_committed_ledger_hash : Ledger_hash.t option
          ; mutable fee_excess : Currency.Fee.t
          }
        [@@deriving yojson]

        let create () =
          { previous_committed_ledger = None
          ; previous_committed_ledger_hash = None
          ; fee_excess = Currency.Fee.zero
          }
      end

      module Db = Kvdb_base.Make_singleton (struct
        type t = State.t [@@deriving yojson]

        let key = "context_state"
      end)

      type t =
        { provers : Zeko_prover.Client.t
        ; da_client : Da_layer.Client.t
        ; executor : Executor.t
        ; config : Config.t
        ; kvdb : Committer.Store.Kvdb.t
        ; state : State.t
        ; archive : Archive.t
        ; logger : Logger.t
        ; proof_cache_db : Proof_cache_tag.cache_db
        }

      let save_state t = Db.set t.kvdb ~data:t.state

      let load_state kvdb =
        match Db.get kvdb with Some state -> state | None -> State.create ()

      let committed t ledger =
        t.state.previous_committed_ledger <- Some ledger ;
        t.state.previous_committed_ledger_hash <-
          Some (Sparse_ledger.merkle_root ledger) ;
        t.state.fee_excess <- Currency.Fee.zero ;
        save_state t

      let set_last_committed_ledger t ledger =
        t.state.previous_committed_ledger <- Some ledger ;
        t.state.previous_committed_ledger_hash <-
          Some (Sparse_ledger.merkle_root ledger) ;
        save_state t

      let add_fee_excess t fee_excess =
        t.state.fee_excess <-
          Currency.Fee.add t.state.fee_excess fee_excess
          |> Option.value_exn ~message:"Fee excess overflow" ;
        save_state t

      let reset_fee_excess t =
        t.state.fee_excess <- Currency.Fee.zero ;
        save_state t
    end

    module Merge = struct
      type t = Txn_snark.serializable

      let process ({ provers; _ } : Context.t) ((left, left_proof) : t)
          ((right, right_proof) : t) =
        Zeko_prover.Client.transaction_snark provers
          (Merge { left; left_proof; right; right_proof })
    end

    module Base = struct
      type t = Txn_snark_witness.t [@@deriving yojson]

      let process (ctx : Context.t) witness =
        match witness with
        | Txn_snark_witness.Zkapp_command segment ->
            Zeko_prover.Client.transaction_snark ctx.provers
              (Zkapp_command segment)
        | Signed_command w ->
            Zeko_prover.Client.transaction_snark ctx.provers (Signed_command w)
    end

    module Commit = struct
      type t =
        { new_inner_ledger : Sparse_ledger.t
        ; processed_actions_pointer : Field.t
        }
      [@@deriving yojson]

      let process
          ({ da_client
           ; provers
           ; executor
           ; config
           ; kvdb
           ; state
           ; archive
           ; logger
           ; proof_cache_db
           } as ctx :
            Context.t ) { new_inner_ledger; processed_actions_pointer }
          txn_snark =
        let%bind count, signature =
          Da_layer.Client.get_signature da_client
            ~da_key:(Even_PC.to_pc config.da_key)
            ~ledger_hash:(Sparse_ledger.merkle_root new_inner_ledger)
        in
        [%log info] "Received %d signatures from da layer" count ;
        assert (count > 0) ;
        let old_inner_ledger =
          Option.value_exn state.previous_committed_ledger
            ~message:"No previous committed ledger"
        in
        let commit_witness : Committer.Commit_witness.t =
          { old_inner_ledger
          ; new_inner_ledger
          ; processed_actions_pointer
          ; signature
          ; txn_snark
          }
        in
        Committer.Store.store_commit kvdb commit_witness
          ~source:(Sparse_ledger.merkle_root old_inner_ledger)
          ~target:(Sparse_ledger.merkle_root new_inner_ledger) ;

        let%bind command =
          Committer.prove_commit ~proof_cache_db ~provers ~executor ~archive
            ~zkapp_pk:config.zkapp_pk ~archive_uri:config.archive_uri
            commit_witness
        in
        let%bind () = Executor.send_zkapp_command ~logger executor command in
        Context.committed ctx new_inner_ledger ;
        return ()
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
    ; logger : Logger.t
    ; archive : Archive.t
    ; config : Config.t
    ; snark_q : Snark_queue.t
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
    |> List.map ~f:(fun update ->
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
             (Archive.add_account_update t.archive update account
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
                     } ) ) )
    |> Or_error.combine_errors |> Result.map ~f:ignore

  (** minimum_fee * e^(q * 0.1 * modifier) *)
  let current_fee_per_weight_unit t =
    let jobs_in_queue =
      Zeko_prover.Client.queue_size t.merger_ctx.provers |> Float.of_int
    in
    t.config.minimum_fee
    *. exp (jobs_in_queue *. 0.1 *. t.config.fee_modifier)
    (* convert to nanomina *)
    *. 10e8

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
            if skip_validity_check then return (Ok ())
            else
              let weight = User_command.weight command |> Float.of_int in
              let required_fee = weight *. current_fee_per_weight_unit t in
              let command_fee =
                User_command.fee command |> Currency.Fee.to_nanomina_int
                |> Float.of_int
              in
              if Float.(command_fee < required_fee) then
                return
                  (Error
                     (Error.of_string
                        (Format.asprintf "Fee is too low, expected %f, got %f"
                           (required_fee /. 10e8) (command_fee /. 10e8) ) ) )
              else return (Ok ())
          in

          (* the protocol state from sequencer has dummy values which wouldn't pass the txn snark *)
          let global_slot = Mina_numbers.Global_slot_since_genesis.zero in
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
                    Verifier.verify_command ~signature_kind:t.config.network_id
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
              @@ Public_key.compress t.config.signer.public_key
            in
            return
              (Zeko_transaction_logic.apply_user_command_unchecked
                 ~signature_kind:t.config.network_id ~sequencer_pk
                 ~zeko_env:Zeko_transaction_logic.zeko_dummy_env
                 ~constraint_constants ~global_slot l t.imt command )
          in

          let%bind.Deferred.Result () =
            match command with
            | Signed_command _ ->
                return (Ok ( (* Signed command has no events nor actions *) ))
            | Zkapp_command command ->
                return (apply_events_and_actions t command)
          in

          (* Accumulate fee *)
          Merger.Context.add_fee_excess t.merger_ctx (User_command.fee command) ;

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
          let%bind () =
            Da_layer.Client.enqueue_diff t.da_client ~genesis:false
              ~ledger_openings:source_ledger ~diff
              ~target_ledger_hash:(L.Db.merkle_root t.ledger)
          in

          return (Ok witnesses) )

  let apply_fee_transfer t =
    let%bind.Deferred.Result witness, (source_ledger, diff) =
      return
      @@
      let fee = t.merger_ctx.state.fee_excess in
      let receiver_pk =
        Even_PC.create_exn @@ Public_key.compress t.config.signer.public_key
      in
      let global_slot = Mina_numbers.Global_slot_since_genesis.zero in
      let ledger = L.of_database t.ledger in
      let%map.Result source_ledger, witness =
        Zeko_transaction_logic.apply_fee_transfer_unchecked ~receiver_pk ~fee
          ~constraint_constants ~global_slot ledger t.imt
      in
      Merger.Context.reset_fee_excess t.merger_ctx ;

      (* Post transaction to the DA layer *)
      let changed_accounts =
        let account_ids =
          [ Account_id.of_public_key t.config.signer.public_key ]
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
      (witness, (source_ledger, diff))
    in
    let%bind () =
      Da_layer.Client.enqueue_diff t.da_client ~genesis:false
        ~ledger_openings:source_ledger ~diff
        ~target_ledger_hash:(L.Db.merkle_root t.ledger)
    in
    Merger.P.add_job t.db_pool t.merger t.merger_ctx ~data:witness
    >>| Result.map_error ~f:(fun e -> Error.of_string (Caqti_error.show e))

  let update_inner_account t =
    let old_synced_outer_action_state, old_deposits_length =
      let s =
        Utils.get_synced_outer_action_state_exn (L.of_database t.ledger)
      in
      C.Rollup_state.Outer_action_state.With_length.(raw s, length s)
    in
    let%bind all_new_actions =
      Gql_client.fetch_actions t.config.archive_uri
        ~from_action_state:old_synced_outer_action_state t.config.zkapp_pk
    in
    let%bind current_height = Gql_client.fetch_block_height t.config.l1_uri in
    (* Find pointer for actions to be processed *)
    let processed_pointer, processed_new_actions =
      List.fold all_new_actions ~init:(old_synced_outer_action_state, [])
        ~f:(fun (curr_state, curr_actions) (action, block_height) ->
          if block_height + t.config.deposit_delay_blocks <= current_height then
            ( Zkapp_account.Actions_impl.(push_hash curr_state (hash action))
            , action :: curr_actions )
          else (curr_state, curr_actions) )
    in
    if Field.equal old_synced_outer_action_state processed_pointer then
      (* In case no new actions are to process, we don't need to update inner account *)
      return (0, old_synced_outer_action_state)
    else
      let%bind tree =
        let%map (body, account_update_digest, calls), proof =
          Zeko_prover.Client.inner_sync t.snark_q.provers
            ~public_key:Zeko_constants.inner_public_key
            ~ase_elms:
              (List.map processed_new_actions ~f:Zkapp_account.Actions_impl.hash)
            ~ase_source:
              ( { action_state = old_synced_outer_action_state
                ; length = old_deposits_length
                }
                : C.Ase.With_length.Stmt.t )
        in
        let proof_cache_db = t.merger_ctx.proof_cache_db in
        (* see #286 *)
        match Is_compile_simple_real.is_compile_simple_real with
        | Some eq ->
            let proof_eq, _ = Type_equal.detuple2 eq in
            let account_update : Account_update.t =
              Account_update.with_aux ~body
                ~authorization:
                  (Control.Poly.Proof
                     (Proof_cache_tag.write_proof_to_disk proof_cache_db
                        (Type_equal.conv proof_eq proof) ) )
            in
            Zkapp_command.Call_forest.Tree.
              { account_update
              ; account_update_digest
              ; calls =
                  Zkapp_command.Call_forest.With_hashes.write_all_proofs_to_disk
                    ~proof_cache_db calls
              }
        | None ->
            let account_update : Account_update.t =
              Account_update.with_aux
                ~body:{ body with authorization_kind = None_given }
                ~authorization:Control.Poly.None_given
            in
            Zkapp_command.Call_forest.Tree.
              { account_update
              ; account_update_digest =
                  Zkapp_command.Digest.Account_update.create
                    ~signature_kind:t.config.network_id account_update
              ; calls =
                  Zkapp_command.Call_forest.With_hashes.write_all_proofs_to_disk
                    ~proof_cache_db calls
              }
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
        ; account_updates = Zkapp_command.Call_forest.cons_tree tree []
        ; memo = Signed_command_memo.empty
        }
      in
      let%bind witnesses =
        match%bind
          (* Skip validity check because dummy fee payer triggers invalid public key error *)
          apply_user_command t ~skip_validity_check:true (Zkapp_command command)
        with
        | Ok witness ->
            return witness
        | Error e ->
            Error.raise e
      in
      let%bind () =
        Deferred.List.iter ~how:`Sequential witnesses ~f:(fun witness ->
            Merger.P.add_job t.db_pool t.merger t.merger_ctx ~data:witness
            >>| Relational_db.caqti_ok_exn
                  ~msg:"Failed to add witness for inner account update: %s" )
      in
      return (List.length processed_new_actions, processed_pointer)

  let commit t =
    let logger = t.logger in
    let%bind () = apply_fee_transfer t >>| Or_error.ok_exn in
    let%bind processed_actions, processed_actions_pointer =
      update_inner_account t
    in
    let target_ledger =
      Sparse_ledger.of_ledger_subset_exn
        L.(of_database t.ledger)
        [ Zeko_constants.inner_account_id ]
    in
    let tree_leaves =
      Merger.M.current_tree t.merger
      |> Option.map ~f:(fun tree -> Merger.M.Tree.base_jobs_count tree.value)
      |> Option.value ~default:0
    in
    (* If the only txn was update of inner account, we don't need to commit *)
    if tree_leaves = 0 || (tree_leaves = 1 && processed_actions = 1) then
      return ([%log info] "Nothing to commit")
    else
      Merger.P.commit_exn t.db_pool t.merger t.merger_ctx
        ~commit_witness:
          { new_inner_ledger = target_ledger; processed_actions_pointer }
      |> Deferred.ignore_m

  let run_committer t =
    if Float.(t.config.commitment_period_sec <= 0.) then ()
    else
      let period = Time_ns.Span.of_sec t.config.commitment_period_sec in
      every ~start:(after period) ~stop:(Ivar.read t.closed) period (fun () ->
          don't_wait_for @@ Deferred.ignore_m @@ commit t )

  let bootstrap ~logger ({ config; _ } as t) da_config =
    [%log info] "Bootstrapping" ;
    let%bind commited_ledger_hash =
      match Sys.getenv "ZEKO_OVERRIDE_BOOTSTRAP_HASH" with
      | None ->
          Gql_client.infer_state config.l1_uri ~zkapp_pk:config.zkapp_pk
            ~signer_pk:(Public_key.compress config.signer.public_key)
          >>| Utils.value_of_zkapp_state
                Zeko_circuits.Rollup_state.Outer_state.typ
          >>| fun { ledger_hash; _ } -> ledger_hash
      | Some hash ->
          [%log info] "Using override hash: %s" hash ;
          return (Ledger_hash.of_decimal_string hash)
    in
    [%log info] "Fetched commited root: %s"
      Ledger_hash.(to_decimal_string commited_ledger_hash) ;

    [%log info] "Init root: %s" Ledger_hash.(to_decimal_string (get_root t)) ;

    (* apply diffs from DA layer *)
    let%bind () =
      Da_layer.Client.map_diffs ~logger ~config:da_config
        ~depth:constraint_constants.ledger_depth ~source_ledger_hash:`Genesis
        ~target_ledger_hash:commited_ledger_hash
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
            Da_layer.Client.get_openings
              ~diff:(Da_layer.Diff.drop_time diff)
              ~ledger:mask
          in
          let changed_accounts =
            Da_layer.Diff.Stable.Latest.changed_accounts diff
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

          (* Store diff to DA client *)
          let%bind () =
            Da_layer.Client.enqueue_diff t.da_client
              ~diff:(Da_layer.Diff.drop_time diff)
              ~ledger_openings
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
                     ~signature_kind:t.config.network_id
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
    Merger.Context.set_last_committed_ledger t.merger_ctx sparse_ledger ;
    return ()

  let create ~logger ~zkapp_pk ~max_pool_size ~commitment_period_sec ~da_config
      ~da_quorum ~db_dir ~postgres_uri ~l1_uri ~archive_uri ~signer
      ~l1_network_id ~l2_network_id ~deposit_delay_blocks ~provers ~da_key
      ~fee_modifier ~minimum_fee =
    [%log info] "Precomputing srs" ;
    Pickles.Side_loaded.srs_precomputation () ;
    let ledger =
      L.Db.create
        ?directory_name:
          (Option.map db_dir ~f:(fun db_dir -> Filename.concat db_dir "ledger"))
        ~depth:constraint_constants.ledger_depth ()
    in
    let imt =
      Indexed_merkle_tree.Db.create
        ?directory_name:
          (Option.map db_dir ~f:(fun db_dir -> Filename.concat db_dir "imt"))
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
        ; network_id = Utils.signature_kind l2_network_id
        ; deposit_delay_blocks
        ; da_key
        ; fee_modifier
        ; minimum_fee
        }
    in
    let%bind db_pool = Db.create_and_migrate ~postgres_uri ~logger in
    let da_client =
      Da_layer.Client.create ~logger ~config:da_config ~quorum:da_quorum
        ~db_pool
    in
    let kvdb = L.Db.zeko_kvdb ledger in
    let provers =
      Zeko_prover.Client.create ~logger
        (List.map provers ~f:Tcp.Where_to_connect.of_host_and_port)
    in
    let executor =
      Executor.create ~l1_uri:config.l1_uri
        ~signature_kind:(Utils.signature_kind l1_network_id)
        ~signer ~kvdb ()
    in
    let archive = Archive.create ~kvdb in
    let proof_cache_db = Proof_cache_tag.create_identity_db () in
    let merger_ctx =
      Merger.Context.
        { provers
        ; da_client
        ; executor
        ; config
        ; kvdb
        ; state = Merger.Context.load_state kvdb
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
      ; logger
      ; archive
      ; config
      ; da_client
      ; snark_q = Snark_queue.create ~provers
      ; merger
      ; merger_ctx
      ; closed = Ivar.create ()
      ; apply_q = Sequencer.create ()
      }
    in
    let%bind () =
      if is_empty t then bootstrap ~logger t da_config else return ()
    in
    let%bind () =
      Committer.recommit_all ~logger ~proof_cache_db ~provers:t.snark_q.provers
        ~executor:t.merger_ctx.executor ~archive ~kvdb ~zkapp_pk:config.zkapp_pk
        ~archive_uri:config.archive_uri
    in
    let%bind () =
      Da_layer.Client.start_client da_client ~target_ledger_hash:(get_root t)
    in
    return t
end
