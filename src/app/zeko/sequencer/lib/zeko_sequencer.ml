open Core_kernel
open Async
open Async_kernel
open Mina_base
open Mina_ledger
open Signature_lib
open Zeko_prover.Zeko_types
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
      ; imt_dir : string option
      ; zkapp_pk : Public_key.Compressed.t
      ; signer : Keypair.t
      ; l1_uri : Uri.t Cli_lib.Flag.Types.with_name
      ; archive_uri : Uri.t Cli_lib.Flag.Types.with_name
      ; network_id : string
      ; deposit_delay_blocks : int
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
          ; mutable witnesses : Txn_snark_witness.t list
          ; mutable fee_excess : Currency.Fee.t
          }
        [@@deriving yojson]

        let create () =
          { previous_committed_ledger = None
          ; previous_committed_ledger_hash = None
          ; witnesses = []
          ; fee_excess = Currency.Fee.zero
          }
      end

      module Db = struct
        module Key_value = struct
          type _ t = Context_state : (unit * State.t) t

          let serialize_key : type k v. (k * v) t -> k -> Bigstring.t =
           fun pair_type key ->
            match pair_type with
            | Context_state ->
                Bigstring.of_string "context_state"

          let serialize_value : type k v. (k * v) t -> v -> Bigstring.t =
           fun pair_type value ->
            match pair_type with
            | Context_state ->
                Bigstring.of_string @@ Yojson.Safe.to_string
                @@ State.to_yojson value

          let deserialize_value : type k v. (k * v) t -> Bigstring.t -> v =
            let ok_exn x =
              let open Ppx_deriving_yojson_runtime.Result in
              match x with Ok x -> x | Error e -> failwith e
            in
            fun pair_type data ->
              match pair_type with
              | Context_state ->
                  ok_exn @@ State.of_yojson @@ Yojson.Safe.from_string
                  @@ Bigstring.to_string data
        end

        include Kvdb_base.Make (Key_value)
      end

      type t =
        { provers : Zeko_prover.Client.State.t
        ; da_client : Da_layer.Client.Sequencer.t
        ; executor : Executor.t
        ; config : Config.t
        ; kvdb : Committer.Store.Kvdb.t
        ; state : State.t
        }

      let save_state t =
        Db.set t.kvdb Db.Key_value.Context_state ~key:() ~data:t.state

      let load_state kvdb =
        match Db.get kvdb Db.Key_value.Context_state ~key:() with
        | Some state ->
            state
        | None ->
            State.create ()

      let reset_state_after_commit t ledger =
        t.state.witnesses <- [] ;
        t.state.previous_committed_ledger <- Some ledger ;
        t.state.previous_committed_ledger_hash <-
          Some (Sparse_ledger.merkle_root ledger) ;
        t.state.fee_excess <- Currency.Fee.zero ;
        save_state t

      let add_witness t w =
        t.state.witnesses <- t.state.witnesses @ [ w ] ;
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
        Context.add_witness ctx witness ;
        match witness with
        | Zkapp_command segment ->
            Zeko_prover.Client.transaction_snark ctx.provers
              (Zkapp_command segment)
        | Signed_command w ->
            Zeko_prover.Client.transaction_snark ctx.provers (Signed_command w)
    end

    module Commit = struct
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
          txn_snark =
        let%bind signatures =
          Da_layer.Client.Sequencer.get_signatures da_client
            ~ledger_hash:(Sparse_ledger.merkle_root new_inner_ledger)
          |> Deferred.map ~f:(fun x -> Option.value_exn x)
        in
        printf "Received %d signatures from da layer\n%!"
          (List.length signatures) ;

        let old_inner_ledger =
          Option.value_exn state.previous_committed_ledger
        in
        let commit_witness : Committer.Commit_witness.t =
          { old_inner_ledger
          ; new_inner_ledger
          ; old_deposits_pointer
          ; processed_deposits_pointer
          ; signatures
          ; txn_snark
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
        Context.reset_state_after_commit ctx new_inner_ledger ;
        return ()
    end

    module P = Parallel_merger.Make (Context) (Merge) (Base) (Commit)

    let requeue_after_restart t (ctx : Context.t) =
      let witnesses_to_requeue = ctx.state.witnesses in
      (* Adding jobs will repopulate the list *)
      ctx.state.witnesses <- [] ;
      printf "Requeueing %d proofs\n%!" (List.length witnesses_to_requeue) ;
      List.iter witnesses_to_requeue ~f:(fun witness ->
          don't_wait_for @@ P.add_job t ctx ~data:witness )
  end

  module State_hashes = struct
    type t =
      { proved_ledger_hash : Ledger_hash.t
      ; unproved_ledger_hash : Ledger_hash.t
      ; committed_ledger_hash : Ledger_hash.t
      }
  end

  type t =
    { db : L.Db.t
    ; imt : Indexed_merkle_tree.Db.t
    ; logger : Logger.t
    ; archive : Archive.t
    ; config : Config.t
    ; snark_q : Snark_queue.t
    ; merger : Merger.P.t
    ; merger_ctx : Merger.Context.t
    ; da_client : Da_layer.Client.Sequencer.t
    ; apply_q : unit Sequencer.t
          (* Applying of the user command is async operation, but we need to keep the application synchronous *)
    }

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

  let apply_events_and_actions t command =
    let ledger = L.of_database t.db in
    Zkapp_command.(Call_forest.to_list (account_updates command))
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
                           (Zkapp_command command)
                     ; memo = Zkapp_command.memo command
                     ; authorization_kind =
                         Account_update.Body.authorization_kind update.body
                     } ) ) )
    |> Or_error.combine_errors |> Result.map ~f:ignore

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
            if Merger.P.number_of_wip_jobs t.merger >= t.config.max_pool_size
            then Error (Error.of_string "Maximum pool size reached, try later")
            else Ok ()
          in

          (* TODO: Check if fee is sufficient *)

          (* the protocol state from sequencer has dummy values which wouldn't pass the txn snark *)
          let global_slot = Mina_numbers.Global_slot_since_genesis.zero in
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

          let%bind.Deferred.Result source_ledger, witnesses =
            let sequencer_pk =
              Even_PC.create_exn
              @@ Public_key.compress t.config.signer.public_key
            in
            return
              (Zeko_transaction_logic.apply_user_command_unchecked ~sequencer_pk
                 ~zeko_env:Zeko_transaction_logic.zeko_dummy_env
                 ~constraint_constants ~global_slot l t.imt t.archive command )
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
                   ( command
                   , match command with
                     | Signed_command _ ->
                         []
                     | Zkapp_command command ->
                         Zkapp_command.all_account_updates_list command
                         |> List.map ~f:(fun _ -> true) ) )
          in
          Da_layer.Client.Sequencer.enqueue_distribute_diff t.da_client
            ~ledger_openings:source_ledger ~diff
            ~target_ledger_hash:(Ledger.Db.merkle_root t.db) ;

          return (Ok witnesses) )

  let apply_fee_transfer t =
    let fee = t.merger_ctx.state.fee_excess in
    let receiver_pk =
      Even_PC.create_exn @@ Public_key.compress t.config.signer.public_key
    in
    let global_slot = Mina_numbers.Global_slot_since_genesis.zero in
    let ledger = L.of_database t.db in
    let%bind.Result source_ledger, witness =
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
    Da_layer.Client.Sequencer.enqueue_distribute_diff t.da_client
      ~ledger_openings:source_ledger ~diff
      ~target_ledger_hash:(Ledger.Db.merkle_root t.db) ;

    don't_wait_for @@ Merger.P.add_job t.merger t.merger_ctx ~data:witness ;

    Ok ()

  let update_inner_account t =
    let old_deposits_state, old_deposits_length =
      let s = Utils.get_inner_deposits_state_exn (L.of_database t.db) in
      C.Rollup_state.Outer_action_state.With_length.(raw s, length s)
    in
    let%bind all_new_actions =
      Gql_client.fetch_actions t.config.archive_uri
        ~from_action_state:old_deposits_state t.config.zkapp_pk
    in
    let%bind current_height = Gql_client.fetch_block_height t.config.l1_uri in
    (* Find pointer for deposits to be processed *)
    let processed_pointer, processed_new_actions =
      List.fold all_new_actions ~init:(old_deposits_state, [])
        ~f:(fun (curr_state, curr_actions) (action, block_height) ->
          if block_height + t.config.deposit_delay_blocks <= current_height then
            ( Zkapp_account.Actions.push_events curr_state action
            , action :: curr_actions )
          else (curr_state, curr_actions) )
    in
    if Field.equal old_deposits_state processed_pointer then
      (* In case no new deposits are to process, we don't need to update inner account *)
      return (old_deposits_state, old_deposits_state)
    else
      let%bind inner_account_update =
        Zeko_prover.Client.inner_sync t.snark_q.provers
          ~public_key:Zeko_constants.inner_public_key
          ~ase_elms:
            (List.map processed_new_actions ~f:Account_update.Actions.hash)
          ~ase_source:
            ( C.Rollup_state.Outer_action_state.With_length.
                { action_state = old_deposits_state
                ; length = old_deposits_length
                }
              : C.Ase.With_length.Stmt.t )
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
      let () =
        List.iter witnesses ~f:(fun witness ->
            don't_wait_for
            @@ Merger.P.add_job t.merger t.merger_ctx ~data:witness )
      in
      return (old_deposits_state, processed_pointer)

  let commit t =
    apply_fee_transfer t |> Or_error.ok_exn ;
    let%bind old_deposits_pointer, processed_pointer = update_inner_account t in
    let target_ledger =
      Sparse_ledger.of_ledger_subset_exn
        L.(of_database t.db)
        [ Zeko_constants.inner_account_id ]
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
      every ~start:(after period) period (fun () ->
          don't_wait_for @@ Deferred.ignore_m @@ commit t )

  let bootstrap ~logger ({ config; _ } as t) da_config =
    let%bind committed_ledger_hash =
      Gql_client.infer_committed_state config.l1_uri ~zkapp_pk:config.zkapp_pk
        ~signer_pk:(Public_key.compress config.signer.public_key)
    in
    printf "Fetched root: %s\n%!"
      Ledger_hash.(to_decimal_string committed_ledger_hash) ;

    printf "Init root: %s\n%!" Ledger_hash.(to_decimal_string (get_root t)) ;

    (* apply diffs from DA layer *)
    let%bind diffs =
      Da_layer.Client.get_diffs_chain ~logger ~config:da_config
        ~source_ledger_hash:`Genesis ~target_ledger_hash:committed_ledger_hash
      |> Deferred.map ~f:Or_error.ok_exn
    in
    let%bind () =
      Deferred.List.iter ~how:`Sequential diffs ~f:(fun diff ->
          assert (
            Ledger_hash.equal
              (Da_layer.Diff.Stable.Latest.source_ledger_hash diff)
              (get_root t) ) ;

          (* Apply accounts diff *)
          let mask = L.of_database t.db in
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

          (* Add events and actions *)
          let result =
            match
              Da_layer.Diff.Stable.Latest.command_with_action_step_flags diff
            with
            | Some (Zkapp_command command, _) ->
                apply_events_and_actions t command
            | _ ->
                Ok ( (* No events nor actions to add *) )
          in
          return
            ( match result with
            | Ok () ->
                ()
            | Error e ->
                printf "Warning: Failed to add events and actions: %s\n%!"
                  (Error.to_string_hum e) ) )
    in

    let current_root = get_root t in
    printf "Current root: %s\n%!" Ledger_hash.(to_decimal_string current_root) ;
    printf "IMT root: %s\n%!"
      (Ledger_hash.to_decimal_string @@ Indexed_merkle_tree.Db.merkle_root t.imt) ;

    if not @@ Ledger_hash.equal current_root committed_ledger_hash then
      print_endline "Ledger mismatch" ;

    let sparse_ledger =
      Sparse_ledger.of_ledger_subset_exn
        L.(of_database t.db)
        [ Zeko_constants.inner_account_id ]
    in
    Merger.Context.reset_state_after_commit t.merger_ctx sparse_ledger ;
    return ()

  let create ~logger ~zkapp_pk ~max_pool_size ~commitment_period_sec ~da_config
      ~da_quorum ~db_dir ~imt_dir ~l1_uri ~archive_uri ~signer ~network_id
      ~deposit_delay_blocks ~provers =
    let db =
      L.Db.create ?directory_name:db_dir
        ~depth:constraint_constants.ledger_depth ()
    in
    let imt =
      Indexed_merkle_tree.Db.create ?directory_name:imt_dir
        ~depth:constraint_constants.ledger_depth ()
    in
    let config =
      Config.
        { max_pool_size
        ; commitment_period_sec
        ; db_dir
        ; imt_dir
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
      Zeko_prover.Client.State.create
        (List.map provers ~f:Tcp.Where_to_connect.of_host_and_port)
    in
    let executor = Executor.create ~l1_uri:config.l1_uri ~signer ~kvdb () in
    let t =
      { db
      ; imt
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
      ; apply_q = Sequencer.create ()
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
end
