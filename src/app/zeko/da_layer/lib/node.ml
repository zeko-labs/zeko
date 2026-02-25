open Core_kernel
open Mina_base
open Signature_lib
module Rpc_def = Rpc
open Async

let constraint_constants = Zeko_constants.constraint_constants

type t =
  { db : Db.t
  ; signer : Keypair.t
  ; logger : Logger.t
  ; chain : Mina_signature_kind.t
  ; proof_cache_db : Proof_cache_tag.cache_db
  }

let get_signature t ~ledger_hash =
  let%bind.Option diff = Db.get_diff t.db ~ledger_hash in
  let message =
    Random_oracle.Input.Chunked.field
    @@ Random_oracle.hash
         ~init:(Hash_prefix_create.salt Zeko_constants.da_layer_check_salt)
         [| ledger_hash; diff.acc_set |]
  in
  Some
    (Schnorr.Chunked.sign ~signature_kind:t.chain t.signer.private_key message)

let get_ledger_hashes_chain t
    ({ source = source_opt; target; max_length = max_length_opt } :
      Rpc_def.Get_ledger_hashes_chain.V1.Query.t ) =
  let logger = t.logger in
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
  [%log debug] "Getting ledger hashes chain from $source to $target"
    ~metadata:
      [ ("source", `String (Ledger_hash.to_decimal_string source))
      ; ("target", `String (Ledger_hash.to_decimal_string target))
      ] ;
  let rec go n current =
    if Ledger_hash.equal current source || n <= 0 then return []
    else
      let%bind source =
        Db.Async.get_diff ~ledger_hash:current t.db
        >>| fun diff ->
        Option.value_exn ~here:[%here]
          ~message:"Get_ledger_hashes_chain: diff not found" diff
        |> Diff.Stable.V3.source_ledger_hash
      in
      let%map next = go (n - 1) source in
      current :: next
  in
  go max_length target >>| List.rev

let implementations t =
  Rpc.Implementations.create_exn ~on_unknown_rpc:`Close_connection
    ~implementations:
      [ (* Post_diff *)
        Rpc.Rpc.implement Rpc_def.Post_diff.V1.t
          (fun () { ledger_openings; acc_set_openings; diff } ->
            match
              Core.post_diff ~logger:t.logger ~proof_cache_db:t.proof_cache_db
                ~kvdb:t.db ~network_id:t.chain ~signer:t.signer ~ledger_openings
                ~acc_set_openings ~diff
            with
            | Ok signature ->
                let pk = Public_key.compress t.signer.public_key in
                return (pk, signature)
            | Error e ->
                let logger = t.logger in
                [%log warn] "Error posting diff: %s" (Error.to_string_hum e) ;
                failwith (Error.to_string_hum e) )
      ; (* Get_diff *)
        Rpc.Rpc.implement Rpc_def.Get_diff.V1.t (fun () query ->
            let%map v2_diff = Db.Async.get_diff t.db ~ledger_hash:query in
            let v1_diff = Option.map v2_diff ~f:Diff.drop_time in
            v1_diff )
      ; Rpc.Rpc.implement Rpc_def.Get_diff.V3.t (fun () query ->
            Db.Async.get_diff t.db ~ledger_hash:query )
      ; (* Has_diff *)
        Rpc.Rpc.implement Rpc_def.Has_diff.V1.t (fun () query ->
            Db.Async.has_diff t.db ~ledger_hash:query )
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
            let logger = t.logger in
            let%bind chain =
              get_ledger_hashes_chain t { source; target; max_length }
            in
            [%log debug]
              "Got ledger hashes chain from $source to $target with length \
               $length"
              ~metadata:
                [ ( "source"
                  , `String
                      ( match source with
                      | `Genesis ->
                          "genesis"
                      | `Specific source ->
                          Ledger_hash.to_decimal_string source ) )
                ; ("target", `String (Ledger_hash.to_decimal_string target))
                ; ("length", `Int (List.length chain))
                ] ;
            Deferred.List.map ~how:`Parallel chain ~f:(fun ledger_hash ->
                [%log debug] "Getting diff for ledger hash: $ledger_hash"
                  ~metadata:
                    [ ( "ledger_hash"
                      , `String (Ledger_hash.to_decimal_string ledger_hash) )
                    ] ;
                Db.Async.get_diff ~ledger_hash t.db
                >>| fun diff ->
                Option.value_exn ~here:[%here] ~message:"Diff not found" diff ) )
      ; (* Diffs_stream *)
        Rpc.Pipe_rpc.implement Rpc_def.Diffs_stream.V2.t
          (fun () { source; target } ->
            let logger = t.logger in
            let r, w = Pipe.create () in
            let%bind chain =
              get_ledger_hashes_chain t { source; target; max_length = None }
            in
            don't_wait_for
              ( Deferred.List.iter ~how:`Sequential chain ~f:(fun ledger_hash ->
                    [%log debug] "Getting diff for ledger hash: $ledger_hash"
                      ~metadata:
                        [ ( "ledger_hash"
                          , `String (Ledger_hash.to_decimal_string ledger_hash)
                          )
                        ] ;
                    let%bind diff =
                      Db.Async.get_diff ~ledger_hash t.db
                      >>| fun o ->
                      Option.value_exn o ~here:[%here]
                        ~message:
                          (sprintf "Diff stream didn't find diff %s"
                             (Ledger_hash.to_decimal_string ledger_hash) )
                    in
                    let%map () = Pipe.write w diff in
                    [%log debug]
                      "Wrote diff to pipe for ledger hash: $ledger_hash"
                      ~metadata:
                        [ ( "ledger_hash"
                          , `String (Ledger_hash.to_decimal_string ledger_hash)
                          )
                        ] )
              >>| fun () ->
              [%log debug] "Closing pipe" ;
              Pipe.close w ) ;

            return (Ok r) )
      ]

let create_server ~chain ~port ~logger ~db_dir ~signer_sk ~no_migrations () =
  let where_to_listen =
    Tcp.Where_to_listen.bind_to All_addresses (On_port port)
  in
  let%bind db_existed = Sys.file_exists_exn db_dir in
  let t =
    { db = Db.create db_dir
    ; signer =
        Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn signer_sk
    ; logger
    ; chain
    ; proof_cache_db = Proof_cache_tag.create_identity_db ()
    }
  in

  (* Set the migration to the latest migration if the database didn't exist *)
  if not db_existed then
    Db.set_migration t.db ~migration:Migrations.latest_migration ;

  if not no_migrations then Migrations.run_migrations ~logger t.db ;

  let implementations = implementations t in
  Tcp.Server.create
    ~on_handler_error:
      (`Call
        (fun _net exn ->
          [%log error] "Exception while handling TCP server request: $error"
            ~metadata:
              [ ("error", `String (Exn.to_string_mach exn))
              ; ("context", `String "rpc_tcp_server")
              ] ) )
    where_to_listen
    (fun _ reader writer ->
      Rpc.Connection.server_with_close reader writer ~implementations
        ~connection_state:(fun _ -> ())
        ~on_handshake_error:`Ignore )
