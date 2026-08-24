open Core_kernel
module Rpc_def = Rpc
open Async

let constraint_constants = Zeko_constants.constraint_constants

type t =
  { db : Db.t
  ; signer : Signer_service.Signer.t
  ; logger : Logger.t
  ; chain : Mina_signature_kind.t
  ; proof_cache_db : Proof_cache_tag.cache_db
  }

let get_signature t ~state =
  let%bind stored_diff = Db.Async.get_diff t.db ~state in
  match stored_diff with
  | None ->
      return None
  | Some _ -> (
      let message = Da_state.signing_message state in
      Signer_service.Signer.sign_field ~signature_kind:t.chain t.signer message
      >>| function
      | Ok signature ->
          Some signature
      | Error err ->
          let logger = t.logger in
          [%log error] "Failed to sign DA receipt: %s" (Error.to_string_hum err) ;
          None )

let get_states_chain t
    ({ source = source_opt; target; max_length = max_length_opt } :
      Rpc_def.Get_ledger_hashes_chain.V2.Query.t ) =
  let logger = t.logger in
  let max_length =
    match max_length_opt with Some n -> n | None -> Int.max_value
  in
  let source =
    match source_opt with
    | `Genesis ->
        Da_state.empty ~depth:constraint_constants.ledger_depth
    | `Specific source ->
        source
  in
  [%log debug] "Getting DA states chain from $source to $target"
    ~metadata:
      [ ("source", `String (Da_state.to_string source))
      ; ("target", `String (Da_state.to_string target))
      ] ;
  let rec go n current =
    if Da_state.equal current source || n <= 0 then return []
    else
      let%bind source =
        Db.Async.get_diff ~state:current t.db
        >>| fun stored_diff ->
        Option.value_exn ~here:[%here]
          ~message:"Get_states_chain: diff not found" stored_diff
        |> fun (stored : Stored_diff.t) -> stored.source_state
      in
      let%map next = go (n - 1) source in
      current :: next
  in
  go max_length target >>| List.rev

let implementations t =
  Rpc.Implementations.create_exn ~on_unknown_rpc:`Close_connection
    ~implementations:
      [ (* Healthcheck *)
        Rpc.Rpc.implement Rpc_def.Healthcheck.V1.t (fun () () -> return ())
      ; (* Post_diff *)
        Rpc.Rpc.implement Rpc_def.Post_diff.V2.t
          (fun () { source_state; ledger_openings; acc_set_openings; diff } ->
            match%bind
              Core.post_diff ~logger:t.logger ~proof_cache_db:t.proof_cache_db
                ~kvdb:t.db ~network_id:t.chain ~signer:t.signer ~ledger_openings
                ~source_state ~acc_set_openings ~diff
              |> function Error e -> Deferred.return (Error e) | Ok d -> d
            with
            | Ok (state, signature) ->
                return
                  Rpc_def.Post_diff.V2.Response.
                    { state_id = state
                    ; signer = Signer_service.Signer.public_key t.signer
                    ; signature
                    }
            | Error e ->
                let logger = t.logger in
                [%log warn] "Error posting diff: %s" (Error.to_string_hum e) ;
                failwith (Error.to_string_hum e) )
      ; (* Get_diff *)
        Rpc.Rpc.implement Rpc_def.Get_diff.V5.t (fun () query ->
            Db.Async.get_diff t.db ~state:query )
      ; (* Has_diff *)
        Rpc.Rpc.implement Rpc_def.Has_diff.V2.t (fun () query ->
            Db.Async.has_diff t.db ~state:query )
      ; (* Get_diff_source *)
        Rpc.Rpc.implement Rpc_def.Get_diff_source.V2.t (fun () query ->
            Db.Async.get_diff t.db ~state:query
            >>| fun stored_diff ->
            Option.value_exn
              ~error:
                ( Error.of_string
                @@ sprintf
                     "Get_diff_source exception: Diff not found for DA state %s"
                     (Da_state.to_string query) )
              stored_diff
            |> fun (stored : Stored_diff.t) -> stored.source_state )
      ; (* Get_signed_public_key *)
        Rpc.Rpc.implement Rpc_def.Get_signer_public_key.V1.t (fun () () ->
            return @@ Signer_service.Signer.public_key t.signer )
      ; (* Get_signature *)
        Async.Rpc.Rpc.implement Rpc_def.Get_signature.V2.t (fun () query ->
            let pk = Signer_service.Signer.public_key t.signer in
            let%map signature = get_signature t ~state:query in
            Option.map signature ~f:(fun s -> (pk, s)) )
      ; (* Get_ledger_hashes_chain *)
        Rpc.Rpc.implement Rpc_def.Get_ledger_hashes_chain.V2.t (fun () query ->
            get_states_chain t query )
      ; (* Get_diffs_chain *)
        Rpc.Rpc.implement Rpc_def.Get_diffs_chain.V3.t
          (fun () { source; target; max_length } ->
            let logger = t.logger in
            let%bind chain =
              get_states_chain t { source; target; max_length }
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
                          Da_state.to_string source ) )
                ; ("target", `String (Da_state.to_string target))
                ; ("length", `Int (List.length chain))
                ] ;
            Deferred.List.map ~how:`Parallel chain ~f:(fun state ->
                [%log debug] "Getting diff for DA state: $state"
                  ~metadata:[ ("state", `String (Da_state.to_string state)) ] ;
                Db.Async.get_diff ~state t.db
                >>| fun stored_diff ->
                Option.value_exn ~here:[%here] ~message:"Diff not found"
                  stored_diff ) )
      ; (* Diffs_stream *)
        Rpc.Pipe_rpc.implement Rpc_def.Diffs_stream.V4.t
          (fun () { source; target } ->
            let logger = t.logger in
            let r, w = Pipe.create () in
            let%bind chain =
              get_states_chain t { source; target; max_length = None }
            in
            don't_wait_for
              ( Monitor.try_with (fun () ->
                    Deferred.List.iter ~how:`Sequential chain ~f:(fun state ->
                        [%log debug] "Getting diff for DA state: $state"
                          ~metadata:
                            [ ("state", `String (Da_state.to_string state)) ] ;
                        let%bind stored_diff =
                          Db.Async.get_diff ~state t.db
                          >>| fun o ->
                          Option.value_exn o ~here:[%here]
                            ~message:
                              (sprintf "Diff stream didn't find DA state %s"
                                 (Da_state.to_string state) )
                        in
                        let%map () = Pipe.write w stored_diff in
                        [%log debug] "Wrote diff to pipe for DA state: $state"
                          ~metadata:
                            [ ("state", `String (Da_state.to_string state)) ] ) )
              >>| Result.iter_error ~f:(fun exn ->
                      [%log error] "Diff stream worker crashed: $error"
                        ~metadata:
                          [ ("error", `String (Exn.to_string_mach exn)) ] )
              >>| fun () ->
              [%log debug] "Closing pipe" ;
              Pipe.close w ) ;

            return (Ok r) )
      ]

let start_healthcheck_server ~logger ~port =
  let%map _server =
    Cohttp_async.Server.create_expert
      ~on_handler_error:
        (`Call
          (fun _ exn ->
            [%log error] "Unhandled exception: %s" (Exn.to_string exn) ) )
      (Async.Tcp.Where_to_listen.of_port port)
      (fun ~body:_ _sock req ->
        let uri = Cohttp_async.Request.uri req in
        let status, body =
          match Uri.path uri with
          | "" | "/" | "/health" | "/ping" ->
              (`OK, "pong\n")
          | _ ->
              (`Not_found, "not found\n")
        in
        Cohttp_async.Server.respond_string ~status body
        >>| fun response -> `Response response )
  in
  [%log info] "Healthcheck server started on port %d" port

let create_server ?healthcheck_port ~chain ~port ~logger ~db_dir ~signer
    ~no_migrations () =
  let where_to_listen =
    Tcp.Where_to_listen.bind_to All_addresses (On_port port)
  in
  let%bind db_existed = Sys.file_exists_exn db_dir in
  let t =
    { db = Db.create db_dir
    ; signer
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
  let%bind () =
    match healthcheck_port with
    | None ->
        return ()
    | Some healthcheck_port ->
        start_healthcheck_server ~logger ~port:healthcheck_port
  in
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
