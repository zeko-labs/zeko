open Core_kernel
open Mina_base
open Mina_ledger
open Signature_lib
module Rpc_def = Rpc
open Async

let constraint_constants = Zeko_constants.constraint_constants

type t = { db : Db.t; signer : Keypair.t; logger : Logger.t }

(** 1. Check that [root ledger_openings = diff.source_ledger_hash].
    2. Check that [diff.source_ledger_hash] is either in the databse or an empty ledger.
    3. Check that the indices in [diff.diff] are unique. 
    4. Set each account in [diff.diff] to the [ledger_openings] and call the resulting ledger hash [target_ledger_hash].
    5. Sign [target_ledger_hash].
    6. Check that after applying all the receipts of the command, the receipt chain hashes match the target ledger.
    7. Attach timestamp.
    8. Store the diff under the [target_ledger_hash]. *)
let post_diff t ~ledger_openings ~diff =
  let logger = t.logger in
  (* 1 *)
  let%bind.Result () =
    match
      Ledger_hash.equal
        (Sparse_ledger.merkle_root ledger_openings)
        (Diff.source_ledger_hash diff)
    with
    | true ->
        Ok ()
    | false ->
        Error
          (Error.create "Source ledger hash mismatch"
             (Diff.source_ledger_hash diff)
             Ledger_hash.sexp_of_t )
  in

  (* 2 *)
  let%bind.Result () =
    match Db.get_diff t.db ~ledger_hash:(Diff.source_ledger_hash diff) with
    | Some _ ->
        Ok ()
    | None ->
        if
          Ledger_hash.equal
            (Diff.source_ledger_hash diff)
            (Diff.empty_ledger_hash
               ~depth:(Sparse_ledger.depth ledger_openings) )
        then Ok ()
        else
          Error
            (Error.create "Source ledger not found in the database"
               (Diff.source_ledger_hash diff)
               Ledger_hash.sexp_of_t )
  in

  (* 3 *)
  let indices = List.map (Diff.changed_accounts diff) ~f:fst in
  let%bind.Result () =
    match List.contains_dup ~compare:Int.compare indices with
    | false ->
        Ok ()
    | true ->
        Error
          (Error.create "Duplicate indices" diff [%sexp_of: Diff.Stable.V1.t])
  in

  (* 4 *)
  let%bind.Result target_ledger =
    List.fold_result (Diff.changed_accounts diff) ~init:ledger_openings
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
  let message =
    Random_oracle.Input.Chunked.field
    @@ Random_oracle.hash
         ~init:(Hash_prefix_create.salt Zeko_constants.da_layer_check_salt)
         [| target_ledger_hash |]
  in
  let signature = Schnorr.Chunked.sign t.signer.private_key message in

  (* 6 *)
  let get_account ledger account_id =
    try
      let index = Sparse_ledger.find_index_exn ledger account_id in
      Ok (Sparse_ledger.get_exn ledger index)
    with e -> Error (Error.of_exn e)
  in
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
    match Diff.command_with_action_step_flags diff with
    | None ->
        Ok Account_id.Map.empty
    | Some (Signed_command command, _) ->
        (* For command only the fee payer gets the receipt *)
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
    | Some (Zkapp_command command, _) ->
        let _commitment, full_transaction_commitment =
          Zkapp_command.get_transaction_commitments command
        in
        let%bind.Result _, acc =
          List.fold_result (Zkapp_command.all_account_updates_list command)
            ~init:(Unsigned.UInt32.zero, Account_id.Map.empty)
            ~f:(fun (index, acc) account_update ->
              (* Receipt chain hash is updated only for account updates authorised with Proof or Signature *)
              match Account_update.authorization account_update with
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
    if Option.is_none (Diff.command_with_action_step_flags diff) then Ok ()
    else
      List.fold_result (Diff.changed_accounts diff) ~init:()
        ~f:(fun _ (_, account) ->
          (* account's target_receipt_chain_hash needs to be either unchanged or the same as in [applied_hashes] *)
          let account_id = Account.identifier account in
          let%bind.Result target_account =
            get_account target_ledger account_id
          in
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
                 (target_receipt_chain_hash, applied_receipt_chain_hash)
                 [%sexp_of: Receipt.Chain_hash.t * Receipt.Chain_hash.t] ) )
  in

  (* 7 *)
  (* V2 was added time *)
  let diff : Diff.Stable.V2.t = Diff.add_time ~logger diff in

  (* 8 *)
  (* We don't care if the diff already existed *)
  let () =
    match Db.add_diff t.db ~ledger_hash:target_ledger_hash ~diff with
    | `Already_existed ->
        [%log warn] "Diff with target ledger hash $hash already exists"
          ~metadata:
            [ ( "hash"
              , `String (Ledger_hash.to_decimal_string target_ledger_hash) )
            ]
    | `Added ->
        [%log info] "Diff with target ledger hash $hash added to the database"
          ~metadata:
            [ ( "hash"
              , `String (Ledger_hash.to_decimal_string target_ledger_hash) )
            ]
  in
  Ok signature

let sync t ~node_location ~ledger_hash =
  let logger = t.logger in
  [%log info] "Syncing" ;
  [%log info] "Fetching intervals" ;
  let ledger =
    Ledger.create_ephemeral ~depth:constraint_constants.ledger_depth ()
  in
  Client.map_diffs ~logger ~depth:constraint_constants.ledger_depth
    ~config:(Client.Config.of_node_locations [ node_location ])
    ~source_ledger_hash:`Genesis ~target_ledger_hash:ledger_hash
    ~f:(fun ~current_chunk ~chunks_length diff ->
      let progress = Float.of_int current_chunk /. Float.of_int chunks_length in
      printf "Progress: %.2f%%\n%!" (progress *. 100.0) ;
      let diff = Diff.drop_time diff in
      let ledger_openings = Client.get_openings ~diff ~ledger in
      match post_diff t ~diff ~ledger_openings with
      | Ok _signature ->
          return (Ok ())
      | Error e ->
          let logger = t.logger in
          [%log warn] "Error posting diff: $error"
            ~metadata:[ ("error", `String (Error.to_string_hum e)) ] ;
          return (Error e) )
  >>| Result.map ~f:(fun asd -> Result.all_unit asd)
  >>| Result.join

let get_signature t ~ledger_hash =
  let%bind.Option _diff = Db.get_diff t.db ~ledger_hash in
  let message =
    Random_oracle.Input.Chunked.field
    @@ Random_oracle.hash
         ~init:(Hash_prefix_create.salt Zeko_constants.da_layer_check_salt)
         [| ledger_hash |]
  in
  Some (Schnorr.Chunked.sign t.signer.private_key message)

let get_ledger_hashes_chain t
    ({ source = source_opt; target; max_length = max_length_opt } :
      Rpc_def.Get_ledger_hashes_chain.V1.Query.t ) =
  let max_length =
    match max_length_opt with Some n -> n | None -> Int.max_value
  in
  let source =
    match source_opt with
    | `Genesis ->
        Diff.empty_ledger_hash ~depth:constraint_constants.ledger_depth
    | `Specific source ->
        source
  in
  let rec go n current =
    if Ledger_hash.equal current source || n <= 0 then return []
    else
      let%bind source =
        Db.Async.get_diff ~ledger_hash:current t.db
        >>| fun diff ->
        Option.value_exn ~here:[%here]
          ~message:"Get_ledger_hashes_chain: diff not found" diff
        |> Diff.Stable.V2.source_ledger_hash
      in
      let%map next = go (n - 1) source in
      current :: next
  in
  go max_length target >>| List.rev

let implementations t =
  Rpc.Implementations.create_exn ~on_unknown_rpc:`Raise
    ~implementations:
      [ (* Post_diff *)
        Rpc.Rpc.implement Rpc_def.Post_diff.V1.t
          (fun () { ledger_openings; diff } ->
            match post_diff t ~ledger_openings ~diff with
            | Ok signature ->
                let pk = Public_key.compress t.signer.public_key in
                return (pk, signature)
            | Error e ->
                let logger = t.logger in
                [%log warn] "Error posting diff: $error"
                  ~metadata:[ ("error", `String (Error.to_string_hum e)) ] ;
                failwith (Error.to_string_hum e) )
      ; (* Get_diff *)
        Rpc.Rpc.implement Rpc_def.Get_diff.V1.t (fun () query ->
            let%map v2_diff = Db.Async.get_diff t.db ~ledger_hash:query in
            let v1_diff = Option.map v2_diff ~f:Diff.drop_time in
            v1_diff )
      ; Rpc.Rpc.implement Rpc_def.Get_diff.V2.t (fun () query ->
            Db.Async.get_diff t.db ~ledger_hash:query )
      ; (* Get_all_keys *)
        Rpc.Rpc.implement Rpc_def.Get_all_keys.V1.t (fun () () ->
            Db.Async.get_index t.db )
      ; (* Get_diff_source *)
        Rpc.Rpc.implement Rpc_def.Get_diff_source.V1.t (fun () query ->
            Db.Async.get_diff t.db ~ledger_hash:query
            >>| fun diff ->
            Option.value_exn
              ~error:
                ( Error.of_string
                @@ sprintf
                     "Get_diff_source exception: Diff not found for ledger \
                      hash %s"
                     (Ledger_hash.to_decimal_string query) )
              diff
            |> Diff.Stable.Latest.source_ledger_hash )
      ; (* Get_signed_public_key *)
        Rpc.Rpc.implement Rpc_def.Get_signer_public_key.V1.t (fun () () ->
            return @@ Public_key.compress @@ t.signer.public_key )
      ; (* Get_signature *)
        Async.Rpc.Rpc.implement Rpc_def.Get_signature.V1.t (fun () query ->
            let pk = Public_key.compress t.signer.public_key in
            let signature = get_signature t ~ledger_hash:query in
            return (Option.map signature ~f:(fun s -> (pk, s))) )
      ; (* Get_ledger_hashes_chain *)
        Rpc.Rpc.implement Rpc_def.Get_ledger_hashes_chain.V1.t (fun () query ->
            get_ledger_hashes_chain t query )
      ; (* Get_diffs_chain *)
        Rpc.Rpc.implement Rpc_def.Get_diffs_chain.V1.t
          (fun () { source; target; max_length } ->
            let%bind chain =
              get_ledger_hashes_chain t { source; target; max_length }
            in
            Deferred.List.map ~how:`Parallel chain ~f:(fun ledger_hash ->
                Db.Async.get_diff ~ledger_hash t.db
                >>| fun diff ->
                Option.value_exn ~here:[%here] ~message:"Diff not found" diff ) )
      ]

let create_server ~sync_arg ~port ~logger ~db_dir ~signer_sk ~no_migrations () =
  let where_to_listen =
    Tcp.Where_to_listen.bind_to All_addresses (On_port port)
  in
  let%bind db_existed = Sys.file_exists_exn db_dir in
  let t =
    { db = Db.create db_dir
    ; signer =
        Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn signer_sk
    ; logger
    }
  in

  (* Set the migration to the latest migration if the database didn't exist *)
  if not db_existed then
    Db.set_migration t.db ~migration:Migrations.latest_migration ;

  if not no_migrations then Migrations.run_migrations ~logger t.db ;

  let%bind () =
    match sync_arg with
    | None ->
        return ()
    | Some (node_location, ledger_hash) -> (
        match%bind sync t ~node_location ~ledger_hash with
        | Ok () ->
            return ()
        | Error e ->
            failwith (Error.to_string_hum e) )
  in

  let implementations = implementations t in
  Tcp.Server.create
    ~on_handler_error:
      (`Call
        (fun _net exn ->
          [%log error] "Exception while handling TCP server request: $error"
            ~metadata:
              [ ("error", `String (Core.Exn.to_string_mach exn))
              ; ("context", `String "rpc_tcp_server")
              ] ) )
    where_to_listen
    (fun address reader writer ->
      let address = Socket.Address.Inet.addr address in
      Rpc.Connection.server_with_close reader writer ~implementations
        ~connection_state:(fun _ -> ())
        ~on_handshake_error:
          (`Call
            (fun exn ->
              return
              @@ [%log error]
                   "Exception while handling RPC server request from $address: \
                    $error"
                   ~metadata:
                     [ ("error", `String (Core.Exn.to_string_mach exn))
                     ; ("context", `String "rpc_server")
                     ; ("address", `String (Unix.Inet_addr.to_string address))
                     ] ) ) )
