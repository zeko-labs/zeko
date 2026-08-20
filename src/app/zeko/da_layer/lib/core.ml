open Core_kernel
open Mina_base
open Mina_ledger
open Signature_lib
module Field = Snark_params.Tick.Field

(** 1. Check that [root ledger_openings = diff.source_ledger_hash].
    2. Check that [diff.source_ledger_hash] is either in the databse or an empty ledger.
    3. Check that the indices in [diff.diff] are unique. 
    4. Set each account in [diff.diff] to the [ledger_openings] and call the resulting ledger hash [target_ledger_hash].
    5. Apply all the actions in [diff.diff] to the [ledger_openings] and check it matches the target ledger..
    6. Check that after applying all the receipts of the command, the receipt chain hashes match the target ledger.
    7. Check that new accounts in ledger openings are in same order as in acc set openings.
    8. Attach timestamp and acc set root.
    9. Store the diff under the [target_ledger_hash]. 
    10. Sign [target_ledger_hash]. *)
let post_diff ~logger ~proof_cache_db ~kvdb ~network_id
    ~(signer : Signer_service.Signer.t) ~(source_state : Da_state.t)
    ~ledger_openings ~acc_set_openings ~(diff : Diff.Pending.t) =
  let get_account ledger account_id =
    try
      let index = Sparse_ledger.find_index_exn ledger account_id in
      Ok (Sparse_ledger.get_exn ledger index)
    with e -> Error (Error.of_exn e)
  in

  let%bind.Result () =
    if Ledger_hash.equal source_state.ledger_hash diff.source_ledger_hash then
      Ok ()
    else
      Error
        (Error.create "Source DA state does not match diff source ledger hash"
           (source_state, diff.source_ledger_hash)
           [%sexp_of: Da_state.t * Ledger_hash.t] )
  in

  (* 1 *)
  let%bind.Result () =
    try
      match
        Ledger_hash.equal
          (Sparse_ledger.merkle_root_without_cache_exn ledger_openings)
          diff.source_ledger_hash
      with
      | true ->
          Ok ()
      | false ->
          Error
            (Error.create "Source ledger hash mismatch" diff.source_ledger_hash
               Ledger_hash.sexp_of_t )
    with e -> Error (Error.of_exn e)
  in

  (* 2 *)
  let%bind.Result () =
    match Db.get_diff kvdb ~state:source_state with
    | Some _ ->
        Ok ()
    | None ->
        if
          Da_state.equal source_state
            (Da_state.empty ~depth:(Sparse_ledger.depth ledger_openings))
        then Ok ()
        else
          Error
            (Error.create "Source ledger not found in the database" source_state
               Da_state.sexp_of_t )
  in

  (* 3 *)
  let indices = List.map diff.changed_accounts ~f:fst in
  let%bind.Result () =
    match List.contains_dup ~compare:Int.compare indices with
    | false ->
        Ok ()
    | true ->
        Error (Error.create "Duplicate indices" diff [%sexp_of: Diff.Pending.t])
  in

  (* 4 *)
  let%bind.Result target_ledger =
    List.fold_result diff.changed_accounts ~init:ledger_openings
      ~f:(fun ledger (diff_index, account) ->
        try
          (* Check that the index of the account matches the index in the diff *)
          let%bind.Result () =
            let opening_index =
              Sparse_ledger.find_index_exn ledger_openings
              @@ Account.identifier account
            in
            if opening_index = diff_index then Ok ()
            else
              Error
                (Error.of_string
                   (sprintf "Index mismatch %d <> %d" opening_index diff_index) )
          in
          Ok (Sparse_ledger.set_exn ledger diff_index account)
        with e -> Error (Error.of_exn e) )
  in
  let target_ledger_hash = Sparse_ledger.merkle_root target_ledger in

  (* 5 *)
  let get_action_state account =
    Option.value_map account.Account.zkapp
      ~default:Zkapp_account.Actions.empty_state_element ~f:(fun zkapp ->
        Pickles_types.Vector.to_list zkapp.action_state |> List.hd_exn )
  in
  let apply_actions action_state (actions : Field.t list list) =
    Zkapp_account.Actions_impl.(
      push_hash action_state (hash (List.map actions ~f:Array.of_list)))
  in
  let actions_in_diff : (Account_id.t * Field.t list list list) list =
    match diff.actions with
    | `Actions actions ->
        actions
    | `Command_with_action_step_flags (Signed_command _, _) ->
        []
    | `Command_with_action_step_flags (Zkapp_command command, _) ->
        let command =
          Zkapp_command.write_all_proofs_to_disk ~signature_kind:network_id
            ~proof_cache_db command
        in
        Zkapp_command.all_account_updates_list command
        |> List.fold ~init:Account_id.Map.empty ~f:(fun acc account_update ->
               match account_update.body.actions with
               | [] ->
                   acc
               | actions ->
                   let account_id =
                     let aid = Account_update.account_id account_update in
                     if
                       Public_key.Compressed.(Account_id.public_key aid = empty)
                     then Zeko_constants.inner_account_id
                     else aid
                   in
                   let actions = List.map actions ~f:Array.to_list in
                   Map.update acc account_id ~f:(function
                     | None ->
                         [ actions ]
                     | Some prev ->
                         prev @ [ actions ] ) )
        |> Map.to_alist
  in
  let%bind.Result applied_action_states =
    let%bind.Result initial_action_states =
      List.fold_result diff.changed_accounts ~init:Account_id.Map.empty
        ~f:(fun acc (_, changed_account) ->
          let account_id = Account.identifier changed_account in
          let%map.Result source_account =
            get_account ledger_openings account_id
          in
          Map.set acc ~key:account_id ~data:(get_action_state source_account) )
    in
    List.fold_result actions_in_diff ~init:initial_action_states
      ~f:(fun acc (account_id, actions_list) ->
        let%bind.Result start_action_state =
          match Map.find acc account_id with
          | Some action_state ->
              Ok action_state
          | None ->
              Error
                (Error.create
                   "Actions reference account not in changed_accounts"
                   account_id Account_id.sexp_of_t )
        in
        let action_state =
          List.fold actions_list ~init:start_action_state ~f:apply_actions
        in
        Ok (Map.set acc ~key:account_id ~data:action_state) )
  in
  let%bind.Result () =
    match network_id with
    | Testnet ->
        Ok ()
    | Mainnet | Other_network _ ->
        List.fold_result diff.changed_accounts ~init:()
          ~f:(fun _ (_, account) ->
            let account_id = Account.identifier account in
            let%bind.Result target_account =
              get_account target_ledger account_id
            in
            let target_action_state = get_action_state target_account in
            let applied_action_state =
              Map.find_exn applied_action_states account_id
            in
            if Field.equal target_action_state applied_action_state then Ok ()
            else
              Error
                (Error.create "Action state mismatch"
                   (account_id, target_action_state, applied_action_state)
                   [%sexp_of: Account_id.t * Field.t * Field.t] ) )
  in

  (* 6 *)
  (* First try to find in [map] and fallback to the [ledger_openings] *)
  let get_account's_receipt_chain_hash map account_id =
    match Account_id.Map.find map account_id with
    | Some account ->
        Ok account
    | None ->
        let%bind.Result account = get_account ledger_openings account_id in
        Ok account.receipt_chain_hash
  in
  let%bind.Result applied_hashes =
    match diff.actions with
    | `Actions _ ->
        Ok Account_id.Map.empty
    | `Command_with_action_step_flags (Signed_command command, _) ->
        (* For signed command only the fee payer gets the receipt *)
        let account_id = Signed_command.fee_payer command in
        let%bind.Result old_receipt_chain_hash =
          get_account's_receipt_chain_hash Account_id.Map.empty account_id
        in
        let new_receipt_chain_hash =
          Receipt.Chain_hash.cons_signed_command_payload
            (Signed_command_payload (Signed_command.payload command))
            old_receipt_chain_hash
        in
        Ok
          (Account_id.Map.set Account_id.Map.empty ~key:account_id
             ~data:new_receipt_chain_hash )
    | `Command_with_action_step_flags (Zkapp_command command, _) ->
        let command =
          Zkapp_command.write_all_proofs_to_disk ~signature_kind:network_id
            ~proof_cache_db command
        in
        let _commitment, full_transaction_commitment =
          Zkapp_command.get_transaction_commitments ~signature_kind:network_id
            command
        in
        let%bind.Result _, acc =
          List.fold_result (Zkapp_command.all_account_updates_list command)
            ~init:(Unsigned.UInt32.zero, Account_id.Map.empty)
            ~f:(fun (index, acc) account_update ->
              (* Receipt chain hash is updated only for account updates authorised with Proof or Signature *)
              match Account_update.Poly.authorization account_update with
              | None_given ->
                  Ok (Unsigned.UInt32.succ index, acc)
              | Proof _ | Signature _ ->
                  let account_id =
                    let aid = Account_update.account_id account_update in
                    if Public_key.Compressed.(Account_id.public_key aid = empty)
                    then Zeko_constants.inner_account_id
                    else aid
                  in
                  let%bind.Result old_receipt_chain_hash =
                    get_account's_receipt_chain_hash acc account_id
                  in
                  let new_receipt_chain_hash =
                    Receipt.Chain_hash.cons_zkapp_command_commitment index
                      (Zkapp_command_commitment full_transaction_commitment)
                      old_receipt_chain_hash
                  in
                  Ok
                    ( Unsigned.UInt32.succ index
                    , Account_id.Map.set acc ~key:account_id
                        ~data:new_receipt_chain_hash ) )
        in
        Ok acc
  in
  let%bind.Result () =
    List.fold_result diff.changed_accounts ~init:() ~f:(fun _ (_, account) ->
        (* For command-backed diffs, [applied_hashes] contains every receipt
           update authorized by the command. For actions-only diffs it is
           empty, so this requires each receipt chain hash to stay unchanged. *)
        let account_id = Account.identifier account in
        let%bind.Result target_account = get_account target_ledger account_id in
        let target_receipt_chain_hash = target_account.receipt_chain_hash in
        let%bind.Result applied_receipt_chain_hash =
          get_account's_receipt_chain_hash applied_hashes account_id
        in
        if
          Receipt.Chain_hash.equal target_receipt_chain_hash
            applied_receipt_chain_hash
        then Ok ()
        else
          Error
            (Error.create "Receipt chain hash mismatch"
               ( account_id
               , target_receipt_chain_hash
               , applied_receipt_chain_hash )
               [%sexp_of:
                 Account_id.t * Receipt.Chain_hash.t * Receipt.Chain_hash.t] ) )
  in

  (* 7 *)
  let%bind.Result () =
    try
      let new_accounts =
        List.filter diff.changed_accounts ~f:(fun (index, _) ->
            Account.equal
              (Sparse_ledger.get_exn ledger_openings index)
              Account.empty )
        |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
      in
      let acc_set_entries =
        List.map new_accounts ~f:(fun (_, account) ->
            let key =
              Account_id.derive_token_id ~owner:(Account.identifier account)
            in
            let acc_set_index =
              Indexed_merkle_tree.Sparse.find_index_exn acc_set_openings
                (Indexed_merkle_tree.Account_id.with_empty_key key)
            in
            let entry =
              Indexed_merkle_tree.Sparse.get_exn acc_set_openings acc_set_index
            in
            (acc_set_index, entry.value) )
        |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
        |> List.map ~f:(fun (_, value) -> value)
      in
      let ledger_entries =
        List.map new_accounts ~f:(fun (_, account) ->
            Account_id.derive_token_id ~owner:(Account.identifier account) )
      in
      if List.equal Token_id.equal acc_set_entries ledger_entries then Ok ()
      else
        Error
          (Error.create
             "New accounts in ledger openings are not in same order as in acc \
              set openings"
             (ledger_entries, acc_set_entries)
             [%sexp_of: Token_id.t list * Token_id.t list] )
    with e -> Error (Error.of_exn e)
  in

  (* 8 *)
  (* V2 was added time *)
  let target_acc_set =
    Indexed_merkle_tree.Sparse.merkle_root_without_cache_exn acc_set_openings
  in
  let diff : Diff.Stable.V4.t =
    Diff.add_time_and_acc_set ~logger diff ~acc_set:target_acc_set
  in
  let target_state =
    Da_state.create ~ledger_hash:target_ledger_hash ~acc_set:target_acc_set
  in
  let stored_diff : Stored_diff.t = { source_state; target_state; diff } in

  (* 9 *)
  let%bind.Result () =
    match Db.add_diff kvdb ~diff:stored_diff with
    | `Already_existed ->
        [%log info] "DA diff for state %s already exists"
          (Da_state.to_string target_state) ;
        Ok ()
    | `Added ->
        [%log info] "DA diff for state %s added to the database"
          (Da_state.to_string target_state) ;
        Ok ()
    | `Conflicting_diff existing ->
        Error
          (Error.create
             "Refusing to sign a DA state whose stored diff has different \
              contents"
             (target_state, existing.diff, diff)
             [%sexp_of: Da_state.t * Diff.Stable.V4.t * Diff.Stable.V4.t] )
  in

  (* 10 *)
  let%bind.Result message =
    try Da_state.signing_message target_state |> Result.return
    with e -> Error (Error.of_exn e)
  in
  let signature =
    Signer_service.Signer.sign_field ~signature_kind:network_id signer message
    (* Schnorr.Chunked.sign ~signature_kind:network_id signer.private_key message *)
  in
  Ok signature

let%test_unit "actions-only diffs cannot change receipt-chain hashes" =
  let depth = 8 in
  let logger = Logger.create () in
  let proof_cache_db = Proof_cache_tag.create_identity_db () in
  let signer =
    Signer_service.Signer.of_keypair (Signature_lib.Keypair.create ())
  in
  let db_dir =
    Filename.concat Filename.temp_dir_name
      ("zeko-da-core-test-" ^ (Uuid_unix.create () |> Uuid.to_string))
  in
  let kvdb = Db.create db_dir in
  Exn.protect
    ~finally:(fun () -> Db.close kvdb)
    ~f:(fun () ->
      Ledger.with_ledger ~depth ~f:(fun ledger ->
          let keypair = Signature_lib.Keypair.create () in
          let account_id =
            Account_id.create
              (Public_key.compress keypair.public_key)
              Token_id.default
          in
          let source_account =
            Account.create account_id Currency.Balance.zero
          in
          Ledger.create_new_account_exn ledger account_id source_account ;
          let source_ledger_hash = Ledger.merkle_root ledger in
          let ledger_openings =
            Sparse_ledger.of_ledger_subset_exn ledger [ account_id ]
          in
          let account_set = Indexed_merkle_tree.In_memory.create ~depth () in
          Indexed_merkle_tree.In_memory.insert_exn account_set
            (Account_id.derive_token_id ~owner:account_id) ;
          let acc_set_openings =
            Indexed_merkle_tree.Sparse.of_in_memory_subset ~logger
              ~db:account_set ~keys:[]
          in
          let acc_set =
            Indexed_merkle_tree.Sparse.merkle_root acc_set_openings
          in
          let source_diff : Diff.Stable.V4.t =
            { source_ledger_hash
            ; changed_accounts = []
            ; actions = `Actions []
            ; timestamp = Block_time.zero
            ; acc_set
            }
          in
          ignore
            ( Db.add_diff kvdb
                ~diff:
                  { Stored_diff.source_state =
                      Da_state.create
                        ~ledger_hash:source_diff.source_ledger_hash ~acc_set
                  ; target_state =
                      Da_state.create ~ledger_hash:source_ledger_hash ~acc_set
                  ; diff = source_diff
                  }
              : [ `Added
                | `Already_existed
                | `Conflicting_diff of Stored_diff.t ] ) ;
          let source_state =
            Da_state.create ~ledger_hash:source_ledger_hash ~acc_set
          in
          let changed_receipt_chain_hash =
            Receipt.Chain_hash.cons_zkapp_command_commitment
              Unsigned.UInt32.zero
              (Receipt.Zkapp_command_elt.Zkapp_command_commitment Field.one)
              source_account.receipt_chain_hash
          in
          let target_account =
            { source_account with
              receipt_chain_hash = changed_receipt_chain_hash
            }
          in
          let diff =
            Diff.create_pending ~source_ledger_hash
              ~changed_accounts:
                [ (Ledger.index_of_account_exn ledger account_id, target_account)
                ]
              ~actions:(`Actions [])
          in
          let result =
            match
              post_diff ~logger ~proof_cache_db ~kvdb ~network_id:Mainnet
                ~signer ~source_state ~ledger_openings ~acc_set_openings ~diff
            with
            | Error error ->
                Error error
            | Ok signature ->
                Async.Thread_safe.block_on_async_exn (fun () -> signature)
          in
          assert (Result.is_error result) ;
          let receipt_preserving_account =
            { source_account with nonce = Unsigned.UInt32.one }
          in
          let receipt_preserving_diff =
            Diff.create_pending ~source_ledger_hash
              ~changed_accounts:
                [ ( Ledger.index_of_account_exn ledger account_id
                  , receipt_preserving_account )
                ]
              ~actions:(`Actions [])
          in
          let receipt_preserving_result =
            match
              post_diff ~logger ~proof_cache_db ~kvdb ~network_id:Mainnet
                ~signer ~source_state ~ledger_openings ~acc_set_openings
                ~diff:receipt_preserving_diff
            with
            | Error error ->
                Error error
            | Ok signature ->
                Async.Thread_safe.block_on_async_exn (fun () -> signature)
          in
          assert (Result.is_ok receipt_preserving_result) ;
          let conflicting_diff =
            Diff.create_pending ~source_ledger_hash
              ~changed_accounts:
                [ ( Ledger.index_of_account_exn ledger account_id
                  , receipt_preserving_account )
                ]
              ~actions:(`Actions [ (account_id, []) ])
          in
          let conflicting_result =
            match
              post_diff ~logger ~proof_cache_db ~kvdb ~network_id:Mainnet
                ~signer ~source_state ~ledger_openings ~acc_set_openings
                ~diff:conflicting_diff
            with
            | Error error ->
                Error error
            | Ok signature ->
                Async.Thread_safe.block_on_async_exn (fun () -> signature)
          in
          assert (Result.is_error conflicting_result) ) )
