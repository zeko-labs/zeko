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

  module Key = struct
    type t = Batch of Ledger_hash.t | Batch_index

    let serialize = function
      | Batch ledger_hash ->
          Bigstring.concat
            [ Bigstring.of_string "batch"
            ; Bigstring.of_string @@ Ledger_hash.to_decimal_string ledger_hash
            ]
      | Batch_index ->
          Bigstring.of_string "batch_index"
  end

  include Kvdb_base.Make (Key)

  let set_index t ~index =
    set t ~key:Batch_index ~data:(Index.to_bigstring index)

  let get_index t =
    get t ~key:Batch_index
    |> Option.map ~f:Index.of_bigstring
    |> Option.value ~default:[]

  let add_batch t ~ledger_hash ~batch =
    let index = get_index t in
    if List.mem index ledger_hash ~equal:Ledger_hash.equal then `Already_exists
    else (
      set_index t ~index:(ledger_hash :: index) ;
      set t ~key:(Batch ledger_hash) ~data:(Batch.to_bigstring batch) ;
      `Added )

  let get_batch t ~ledger_hash =
    get t ~key:(Batch ledger_hash) |> Option.map ~f:Batch.of_bigstring
end

type t = { db : Db.t; signer : Keypair.t; logger : Logger.t }

(** 1. Check that [root ledger_openings = batch.source_ledger_hash].
    2. Check that the indices in [batch.diff] are unique. 
    3. Set each account in [batch.diff] to the [ledger_openings] and sign the resulting ledger hash.
    4. Store the batch under the resulting ledger hash key *)
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
          (Error.create "Ledger hash mismatch"
             (Batch.source_ledger_hash batch)
             Ledger_hash.sexp_of_t )
  in

  (* 2 *)
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

  (* 3 *)
  let%bind.Result target_ledger =
    List.fold_result (Batch.diff batch) ~init:ledger_openings
      ~f:(fun ledger (index, account) ->
        try
          (* Check that the index of the account matches the index in the diff *)
          let%bind.Result () =
            if
              Sparse_ledger.find_index_exn ledger_openings
              @@ Account.identifier account
              = index
            then Ok ()
            else
              Error
                (Error.create "Index mismatch" index Account.Index.sexp_of_t)
          in
          Ok (Sparse_ledger.set_exn ledger index account)
        with e -> Error (Error.of_exn e) )
  in
  let target_ledger_hash = Sparse_ledger.merkle_root target_ledger in
  let message =
    Random_oracle.Input.Chunked.field_elements [| target_ledger_hash |]
  in
  let signature = Schnorr.Chunked.sign t.signer.private_key message in

  (* 4 *)
  (* We ignore the result of [add_batch] because we don't want to overwrite existing batches *)
  let () =
    match Db.add_batch t.db ~ledger_hash:target_ledger_hash ~batch with
    | `Already_exists ->
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

let get_batch t ~(ledger_hash : Ledger_hash.t) = Db.get_batch t.db ~ledger_hash

let implementations t =
  Async.Rpc.Implementations.create_exn ~on_unknown_rpc:`Raise
    ~implementations:
      [ Async.Rpc.Rpc.implement Rpc.Post_batch.v1 (fun () query ->
            match
              post_batch t ~ledger_openings:query.ledger_openings
                ~batch:query.batch
            with
            | Ok signature ->
                Async.return signature
            | Error e ->
                failwith (Error.to_string_hum e) )
      ; Async.Rpc.Rpc.implement Rpc.Get_batch.v1 (fun () query ->
            Async.return @@ get_batch t ~ledger_hash:query )
      ]

let create_server ~port ~logger ~db_dir ~signer_sk =
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
