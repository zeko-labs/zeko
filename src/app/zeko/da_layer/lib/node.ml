open Core_kernel
open Mina_base
open Mina_ledger
open Signature_lib

module Db = struct
  (** Holds keys to all the batches *)
  module Index = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t = Ledger_hash.Stable.V1.t list

        let to_latest = Fn.id
      end
    end]

    let to_bigstring = Binable.to_bigstring (module Stable.Latest)

    let of_bigstring = Binable.of_bigstring (module Stable.Latest)
  end

  module Key_value = struct
    type _ t =
      | Batch : (Ledger_hash.t * Batch.t) t
      | Batch_index : (unit * Index.t) t

    let serialize_key : type k v. (k * v) t -> k -> Bigstring.t =
     fun pair_type key ->
      match pair_type with
      | Batch ->
          Bigstring.concat
            [ Bigstring.of_string "batch"
            ; Bigstring.of_string @@ Ledger_hash.to_decimal_string key
            ]
      | Batch_index ->
          Bigstring.of_string "batch_index"

    let serialize_value : type k v. (k * v) t -> v -> Bigstring.t =
     fun pair_type value ->
      match pair_type with
      | Batch ->
          Batch.to_bigstring value
      | Batch_index ->
          Index.to_bigstring value

    let deserialize_value : type k v. (k * v) t -> Bigstring.t -> v =
     fun pair_type data ->
      match pair_type with
      | Batch ->
          Batch.of_bigstring data
      | Batch_index ->
          Index.of_bigstring data
  end

  include Kvdb_base.Make (Key_value)

  let set_index t ~index = set t Batch_index ~key:() ~data:index

  let get_index t = get t Batch_index ~key:() |> Option.value ~default:[]

  let add_batch t ~ledger_hash ~batch =
    let index = get_index t in
    if List.mem index ledger_hash ~equal:Ledger_hash.equal then `Already_existed
    else (
      set t Batch ~key:ledger_hash ~data:batch ;
      set_index t ~index:(ledger_hash :: index) ;
      `Added )

  let get_batch t ~ledger_hash = get t Batch ~key:ledger_hash
end

type t = { db : Db.t; signer : Keypair.t; logger : Logger.t }

(** 1. Check that [root ledger_openings = batch.source_ledger_hash].
    2. Check that [batch.source_ledger_hash] is either in the databse or an empty ledger.
    3. Check that the indices in [batch.diff] are unique. 
    4. Set each account in [batch.diff] to the [ledger_openings] and call the resulting ledger hash [target_ledger_hash].
    5. Sign [target_ledger_hash].
    6. Check that after applying all the receipts of the command, the receipt chain hashes match the target ledger.
    7. Store the batch under the [target_ledger_hash]. *)
let post_batch t ~ledger_openings ~batch =
  let logger = t.logger in
  (* 1 *)
  let%bind.Result () =
    match
      Ledger_hash.equal
        (Sparse_ledger.merkle_root ledger_openings)
        (Batch.source_ledger_hash batch)
    with
    | true ->
        Ok ()
    | false ->
        Error
          (Error.create "Source ledger hash mismatch"
             (Batch.source_ledger_hash batch)
             Ledger_hash.sexp_of_t )
  in

  (* 2 *)
  let%bind.Result () =
    match Db.get_batch t.db ~ledger_hash:(Batch.source_ledger_hash batch) with
    | Some _ ->
        Ok ()
    | None ->
        if
          Ledger_hash.equal
            (Batch.source_ledger_hash batch)
            (Batch.empty_ledger_hash
               ~depth:(Sparse_ledger.depth ledger_openings) )
        then Ok ()
        else
          Error
            (Error.create "Source ledger not found in the database"
               (Batch.source_ledger_hash batch)
               Ledger_hash.sexp_of_t )
  in

  (* 3 *)
  let indices = List.map (Batch.diff batch) ~f:(fun (i, _) -> i) in
  let%bind.Result () =
    match List.contains_dup ~compare:Int.compare indices with
    | false ->
        Ok ()
    | true ->
        Error
          (Error.create "Duplicate indices" indices
             [%sexp_of: Account.Index.t list] )
  in

  (* 4 *)
  let%bind.Result target_ledger =
    List.fold_result (Batch.diff batch) ~init:ledger_openings
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
    Random_oracle.Input.Chunked.field_elements [| target_ledger_hash |]
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
    match Batch.command_with_action_step_flags batch with
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
              (* Receipt chain hash is updated only for account updates authorised with Proof of Signature *)
              match Account_update.authorization account_update with
              | None_given ->
                  Ok (Unsigned.UInt32.succ index, acc)
              | Proof _ | Signature _ ->
                  let account_id = Account_update.account_id account_update in
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
    List.fold_result (Batch.diff batch) ~init:() ~f:(fun _ (_, account) ->
        (* account's target_receipt_chain_hash needs to be either unchanged or the same as in [applied_hashes] *)
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
               (target_receipt_chain_hash, applied_receipt_chain_hash)
               [%sexp_of: Receipt.Chain_hash.t * Receipt.Chain_hash.t] ) )
  in

  (* 7 *)
  (* We don't care if the batch already existed *)
  let () =
    match Db.add_batch t.db ~ledger_hash:target_ledger_hash ~batch with
    | `Already_existed ->
        [%log warn] "Batch with target ledger hash $hash already exists"
          ~metadata:
            [ ( "hash"
              , `String (Ledger_hash.to_decimal_string target_ledger_hash) )
            ]
    | `Added ->
        [%log info] "Batch with target ledger hash $hash added to the database"
          ~metadata:
            [ ( "hash"
              , `String (Ledger_hash.to_decimal_string target_ledger_hash) )
            ]
  in
  Ok signature

(** Find missing keys and fetch corresponding batches *)
let sync ~logger ~node_location t =
  let open Async in
  let%bind.Deferred.Result remote_keys =
    Client.Rpc.get_all_keys ~logger ~node_location ()
    |> Deferred.map
         ~f:(Result.map ~f:(List.dedup_and_sort ~compare:Ledger_hash.compare))
  in
  let my_keys = Db.get_index t.db in
  let missing_keys =
    let set1 = Set.of_list (module Ledger_hash) remote_keys in
    let set2 = Set.of_list (module Ledger_hash) my_keys in
    Set.diff set1 set2 |> Set.to_list
  in
  let%bind () =
    Deferred.List.iter ~how:`Parallel missing_keys ~f:(fun ledger_hash ->
        let%bind batch =
          match%bind
            Client.Rpc.get_batch ~logger ~node_location ~ledger_hash
          with
          | Ok (Some batch) ->
              return batch
          | Ok None ->
              failwithf "Syncing node claimed to have batch %s but it doesn't"
                (Ledger_hash.to_decimal_string ledger_hash)
                ()
          | Error err ->
              failwithf "Failed syncing the batch %s, error: %s"
                (Ledger_hash.to_decimal_string ledger_hash)
                (Error.to_string_hum err) ()
        in
        match Db.add_batch t.db ~ledger_hash ~batch with
        | `Added ->
            return
            @@ [%log info] "Batch with target ledger hash $hash added"
                 ~metadata:
                   [ ( "hash"
                     , `String (Ledger_hash.to_decimal_string ledger_hash) )
                   ]
        | `Already_existed ->
            failwithf
              "Batch with target ledger hash %s already existed during syncing"
              (Ledger_hash.to_decimal_string ledger_hash)
              () )
  in
  return (Ok ())

let implementations t =
  Async.Rpc.Implementations.create_exn ~on_unknown_rpc:`Raise
    ~implementations:
      [ Async.Rpc.Rpc.implement Rpc.Post_batch.v1
          (fun () { ledger_openings; batch } ->
            match post_batch t ~ledger_openings ~batch with
            | Ok signature ->
                Async.return signature
            | Error e ->
                let logger = t.logger in
                [%log warn] "Error posting batch: $error"
                  ~metadata:[ ("error", `String (Error.to_string_hum e)) ] ;
                failwith (Error.to_string_hum e) )
      ; Async.Rpc.Rpc.implement Rpc.Get_batch.v1 (fun () query ->
            Async.return @@ Db.get_batch t.db ~ledger_hash:query )
      ; Async.Rpc.Rpc.implement Rpc.Get_all_keys.v1 (fun () () ->
            Async.return @@ Db.get_index t.db )
      ; Async.Rpc.Rpc.implement Rpc.Get_batch_source.v1 (fun () query ->
            Async.return @@ Batch.source_ledger_hash
            @@ Option.value_exn
                 ~error:
                   ( Error.of_string
                   @@ sprintf
                        "Get_batch_source exception: Batch not found for \
                         ledger hash %s"
                        (Ledger_hash.to_decimal_string query) )
            @@ Db.get_batch t.db ~ledger_hash:query )
      ; Async.Rpc.Rpc.implement Rpc.Get_signer_public_key.v1 (fun () () ->
            Async.return @@ Public_key.compress @@ t.signer.public_key )
      ]

let create_server ?node_to_sync ~port ~logger ~db_dir ~signer_sk () =
  let open Async in
  let where_to_listen =
    Tcp.Where_to_listen.bind_to All_addresses (On_port port)
  in
  let t =
    { db = Db.create db_dir
    ; signer =
        Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn signer_sk
    ; logger
    }
  in

  (* Syncing of the node is optional, the first one can't sync *)
  let%bind () =
    match node_to_sync with
    | None ->
        return ()
    | Some n -> (
        match%bind sync ~logger ~node_location:n t with
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
