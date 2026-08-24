open Core_kernel
open Async_kernel
open Mina_base
open Mina_ledger
open Signature_lib
open Relational_db
module Field = Snark_params.Tick.Field
module Rpc_def = Rpc

let rec keep_retrying ~logger ?(delay = Time_ns.Span.of_sec 5.) ~f () =
  match%bind
    Monitor.try_with ~here:[%here] f
    >>| Result.map_error ~f:Error.of_exn
    >>| Or_error.join
  with
  | Ok x ->
      return x
  | Error err ->
      [%log error] "Failed to execute function, retrying... %s"
        (Error.to_string_hum err) ;
      let%bind () = after delay in
      keep_retrying ~logger ~delay ~f ()

module Diff_table = struct
  type t =
    { diff : Diff.Pending.Stable.V1.t
    ; ledger_openings : Sparse_ledger.t
    ; acc_set_openings : Indexed_merkle_tree.Sparse.t
    ; genesis : bool
    ; source_state : Da_state.t
    ; target_state : Da_state.t
    }
  [@@deriving hlist, fields, sexp]

  let make ~diff ~ledger_openings ~acc_set_openings ~source_state ~target_state
      ~genesis =
    { diff
    ; ledger_openings
    ; acc_set_openings
    ; source_state
    ; target_state
    ; genesis
    }

  let typ =
    Mina_caqti.Type_spec.custom_type
      ~to_hlist:(fun { diff
                     ; ledger_openings
                     ; acc_set_openings
                     ; source_state
                     ; target_state
                     ; genesis
                     } ->
        H_list.
          [ Ledger_hash.to_decimal_string target_state.ledger_hash
          ; Field.to_string target_state.acc_set
          ; ( if genesis then None
            else Some (Ledger_hash.to_decimal_string source_state.ledger_hash)
            )
          ; Field.to_string source_state.acc_set
          ; Binable.to_bigstring
              (module Diff.Pending.Stable.V1.With_top_version_tag)
              diff
            |> Bigstring.to_string
          ; Sparse_ledger.to_yojson ledger_openings |> Yojson.Safe.to_string
          ; Indexed_merkle_tree.Sparse.to_yojson acc_set_openings
            |> Yojson.Safe.to_string
          ] )
      ~of_hlist:(fun H_list.
                       [ target_ledger_hash
                       ; target_acc_set
                       ; source_ledger_hash
                       ; source_acc_set
                       ; diff
                       ; ledger_openings
                       ; acc_set_openings
                       ] ->
        let ok_exn = function
          | Ppx_deriving_yojson_runtime.Result.Ok x ->
              x
          | Ppx_deriving_yojson_runtime.Result.Error e ->
              failwithf "Error parsing ledger openings: %s" e ()
        in
        let diff =
          Binable.of_bigstring
            (module Diff.Pending.Stable.V1.With_top_version_tag)
            (Bigstring.of_string diff)
        in
        let source_state =
          Da_state.create
            ~ledger_hash:
              (Option.value_map source_ledger_hash
                 ~default:diff.source_ledger_hash
                 ~f:Ledger_hash.of_decimal_string )
            ~acc_set:(Field.of_string source_acc_set)
        in
        { diff
        ; ledger_openings =
            Sparse_ledger.of_yojson (Yojson.Safe.from_string ledger_openings)
            |> ok_exn
        ; acc_set_openings =
            Indexed_merkle_tree.Sparse.of_yojson
              (Yojson.Safe.from_string acc_set_openings)
            |> ok_exn
        ; genesis = Option.is_none source_ledger_hash
        ; source_state
        ; target_state =
            Da_state.create
              ~ledger_hash:(Ledger_hash.of_decimal_string target_ledger_hash)
              ~acc_set:(Field.of_string target_acc_set)
        } )
      Caqti_type.
        [ string; string; option string; string; octets; octets; octets ]

  let insert (module Conn : CONNECTION) t =
    Conn.exec
      (Caqti_request.exec typ
         {sql| INSERT INTO da_diff (target_ledger_hash, target_acc_set, source_ledger_hash, source_acc_set, diff, ledger_openings, acc_set_openings)
                VALUES (?, ?, ?, ?, ?, ?, ?) |sql} )
      t

  let get_diff_by_source (module Conn : CONNECTION) state =
    match state with
    | Some (state : Da_state.t) ->
        Conn.find_opt
          (Caqti_request.find_opt
             Caqti_type.(tup2 string string)
             typ
             {sql| SELECT target_ledger_hash, target_acc_set, source_ledger_hash, source_acc_set, diff, ledger_openings, acc_set_openings FROM da_diff WHERE source_ledger_hash = ? AND source_acc_set = ? |sql} )
          ( Ledger_hash.to_decimal_string state.ledger_hash
          , Field.to_string state.acc_set )
    | None ->
        Conn.find_opt
          (Caqti_request.find_opt Caqti_type.unit typ
             {sql| SELECT target_ledger_hash, target_acc_set, source_ledger_hash, source_acc_set, diff, ledger_openings, acc_set_openings FROM da_diff WHERE source_ledger_hash IS NULL |sql} )
          ()

  let get_id_by_target (module Conn : CONNECTION) (state : Da_state.t) =
    Conn.find_opt
      (Caqti_request.find_opt
         Caqti_type.(tup2 string string)
         Caqti_type.int
         {sql| SELECT id FROM da_diff WHERE target_ledger_hash = ? AND target_acc_set = ? |sql} )
      ( Ledger_hash.to_decimal_string state.ledger_hash
      , Field.to_string state.acc_set )

  let get_target_by_id (module Conn : CONNECTION) id =
    let%map.Deferred.Result result =
      Conn.find_opt
        (Caqti_request.find_opt Caqti_type.int
           Caqti_type.(tup2 string string)
           {sql| SELECT target_ledger_hash, target_acc_set FROM da_diff WHERE id = ? |sql} )
        id
    in
    Option.map result ~f:(fun (ledger_hash, acc_set) ->
        Da_state.create
          ~ledger_hash:(Ledger_hash.of_decimal_string ledger_hash)
          ~acc_set:(Field.of_string acc_set) )
end

module Signature_table = struct
  type t =
    { target_state : Da_state.t
    ; public_key : Public_key.Compressed.t
    ; signature : Signature.t
    }
  [@@deriving hlist, fields]

  let typ =
    Mina_caqti.Type_spec.custom_type
      ~to_hlist:(fun { target_state; public_key; signature } ->
        H_list.
          [ Ledger_hash.to_decimal_string target_state.ledger_hash
          ; Field.to_string target_state.acc_set
          ; Public_key.Compressed.to_base58_check public_key
          ; Signature.to_base58_check signature
          ] )
      ~of_hlist:(fun H_list.
                       [ target_ledger_hash
                       ; target_acc_set
                       ; public_key
                       ; signature
                       ] ->
        { target_state =
            Da_state.create
              ~ledger_hash:(Ledger_hash.of_decimal_string target_ledger_hash)
              ~acc_set:(Field.of_string target_acc_set)
        ; public_key = Public_key.Compressed.of_base58_check_exn public_key
        ; signature = Signature.of_base58_check_exn signature
        } )
      Caqti_type.[ string; string; string; octets ]

  let insert (module Conn : CONNECTION) t =
    Conn.exec
      (Caqti_request.exec typ
         {sql| INSERT INTO da_signature (target_ledger_hash, target_acc_set, public_key, signature) VALUES (?, ?, ?, ?) |sql} )
      t

  let get_signature_opt (module Conn : CONNECTION) (state : Da_state.t)
      public_key =
    Conn.find_opt
      (Caqti_request.find_opt
         Caqti_type.(tup3 string string string)
         typ
         {sql| SELECT target_ledger_hash, target_acc_set, public_key, signature FROM da_signature WHERE target_ledger_hash = ? AND target_acc_set = ? AND public_key = ? |sql} )
      ( Ledger_hash.to_decimal_string state.ledger_hash
      , Field.to_string state.acc_set
      , Public_key.Compressed.to_base58_check public_key )

  let get_signatures (module Conn : CONNECTION) (state : Da_state.t) =
    Conn.collect_list
      (Caqti_request.collect
         Caqti_type.(tup2 string string)
         typ
         {sql| SELECT target_ledger_hash, target_acc_set, public_key, signature FROM da_signature WHERE target_ledger_hash = ? AND target_acc_set = ? |sql} )
      ( Ledger_hash.to_decimal_string state.ledger_hash
      , Field.to_string state.acc_set )
end

let verify_da_signature ~signature_kind ~state ~public_key ~signature =
  match Public_key.decompress public_key with
  | None ->
      Or_error.error_string "DA response contains an invalid signer public key"
  | Some public_key ->
      let verifies =
        Schnorr.Chunked.verify ~signature_kind signature
          (Snark_params.Tick.Inner_curve.of_affine public_key)
          (Random_oracle.Input.Chunked.field (Da_state.signing_message state))
      in
      if verifies then Ok ()
      else Or_error.error_string "DA signature does not verify for target state"

let validate_post_diff_response ~signature_kind ~expected_state
    (response : Rpc_def.Post_diff.V2.Response.t) =
  if not (Da_state.equal response.state_id expected_state) then
    Or_error.error_s
      [%message
        "DA node returned a signature for an unexpected state"
          (expected_state : Da_state.t)
          (response.state_id : Da_state.t)]
  else
    let%map.Or_error () =
      verify_da_signature ~signature_kind ~state:response.state_id
        ~public_key:response.signer ~signature:response.signature
    in
    (response.signer, response.signature)

let%test_unit "post-diff responses are bound to the expected state" =
  let signature_kind = Mina_signature_kind.Other_network "da-response-test" in
  let keypair = Keypair.create () in
  let expected_state =
    Da_state.create ~ledger_hash:Ledger_hash.empty_hash ~acc_set:Field.zero
  in
  let signature =
    Schnorr.Chunked.sign ~signature_kind keypair.private_key
      (Random_oracle.Input.Chunked.field
         (Da_state.signing_message expected_state) )
  in
  let response =
    Rpc_def.Post_diff.V2.Response.
      { state_id = expected_state
      ; signer = Public_key.compress keypair.public_key
      ; signature
      }
  in
  assert (
    Result.is_ok
      (validate_post_diff_response ~signature_kind ~expected_state response) ) ;
  let unexpected_state = { expected_state with acc_set = Field.one } in
  assert (
    Result.is_error
      (validate_post_diff_response ~signature_kind
         ~expected_state:unexpected_state response ) ) ;
  let bad_signature =
    Schnorr.Chunked.sign ~signature_kind (Keypair.create ()).private_key
      (Random_oracle.Input.Chunked.field
         (Da_state.signing_message expected_state) )
  in
  assert (
    Result.is_error
      (validate_post_diff_response ~signature_kind ~expected_state
         { response with signature = bad_signature } ) )

module Rpc = struct
  let dispatch ?(max_tries = 5) ?(timeout = 5.) ~logger
      (node_location : Host_and_port.t Cli_lib.Flag.Types.with_name) rpc data =
    let rec go tries_left errs =
      if Int.( <= ) tries_left 0 then
        let e = Error.of_list (List.rev errs) in
        return
          (Error
             (Error.tag_arg e
                (sprintf
                   "Could not send query to da node after %d tries. The \
                    process may not be running, please check the \
                    daemon-argument"
                   max_tries )
                ( ("host_and_port", node_location.value)
                , ("daemon-argument", node_location.name) )
                [%sexp_of: (string * Host_and_port.t) * (string * string)] ) )
      else
        match%bind Daemon_rpcs.Client.dispatch rpc data node_location.value with
        | Ok result ->
            return (Ok result)
        | Error e ->
            if tries_left > 1 then
              [%log error] "Error sending data to the da node %s. Retrying..."
                (Error.to_string_hum e) ;
            let%bind () = after (Time_ns.Span.of_sec timeout) in
            go (tries_left - 1) (e :: errs)
    in
    go max_tries []

  let healthcheck ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name) () =
    dispatch ~max_tries:1 ~logger node_location Rpc.Healthcheck.V1.t ()

  module Versioned_rpc_same_query = struct
    type ('q, 'latest) t =
      | V : ('q, 'r) Async.Rpc.Rpc.t * ('r -> 'latest) -> ('q, 'latest) t
  end

  let rec dispatch_with_fallback_same_query ?(max_tries = 5) ?(timeout = 5.)
      ~logger (node_location : Host_and_port.t Cli_lib.Flag.Types.with_name)
      data ~(versions : ('q, 'latest) Versioned_rpc_same_query.t list) :
      ('latest, Error.t) Result.t Deferred.t =
    match versions with
    | [] ->
        return (Error (Error.of_string "No versions to try"))
    | Versioned_rpc_same_query.V (rpc, to_latest) :: versions -> (
        match%bind
          dispatch ~max_tries ~timeout ~logger node_location rpc data
        with
        | Ok result ->
            return (Ok (to_latest result))
        | Error e ->
            let version_unimplemented =
              Error.to_string_mach e
              |> String.is_substring
                   ~substring:
                     (sprintf "Unimplemented_rpc %s (Version %d)"
                        (Async.Rpc.Rpc.name rpc)
                        (Async.Rpc.Rpc.version rpc) )
            in
            if version_unimplemented then
              dispatch_with_fallback_same_query ~max_tries ~timeout ~logger
                node_location data ~versions
            else return (Error e) )

  module Versioned_rpc_same_response = struct
    type ('r, 'latest) t =
      | V : ('q, 'r) Async.Rpc.Rpc.t * ('latest -> 'q) -> ('r, 'latest) t
  end

  let rec dispatch_with_fallback_same_response ?(max_tries = 5) ?(timeout = 5.)
      ~logger (node_location : Host_and_port.t Cli_lib.Flag.Types.with_name)
      data ~(versions : ('r, 'latest) Versioned_rpc_same_response.t list) :
      ('r, Error.t) Result.t Deferred.t =
    match versions with
    | [] ->
        return (Error (Error.of_string "No versions to try"))
    | V (rpc, from_latest) :: versions -> (
        match%bind
          dispatch ~max_tries ~timeout ~logger node_location rpc
            (from_latest data)
        with
        | Ok result ->
            return (Ok result)
        | Error e ->
            let version_unimplemented =
              Error.to_string_mach e
              |> String.is_substring
                   ~substring:
                     (sprintf "Unimplemented_rpc %s (Version %d)"
                        (Async.Rpc.Rpc.name rpc)
                        (Async.Rpc.Rpc.version rpc) )
            in
            if version_unimplemented then
              dispatch_with_fallback_same_response ~max_tries ~timeout ~logger
                node_location data ~versions
            else return (Error e) )

  let post_diff ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name)
      ~signature_kind ~source_state ~target_state ~ledger_openings
      ~acc_set_openings ~diff =
    [%log debug] "Posting diff to da node %s"
      (Host_and_port.to_string node_location.value) ;
    let%map result =
      dispatch ~max_tries:5 ~logger node_location Rpc_def.Post_diff.V2.t
        { source_state; ledger_openings; diff; acc_set_openings }
    in
    Result.bind result ~f:(fun response ->
        validate_post_diff_response ~signature_kind ~expected_state:target_state
          response )

  let get_diff ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name) ~state =
    [%log debug] "Getting diff from da node %s"
      (Host_and_port.to_string node_location.value) ;
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_diff.V5.t state

  let get_diff_source ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name) ~state =
    [%log debug] "Getting diff source from da node %s"
      (Host_and_port.to_string node_location.value) ;
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_diff_source.V2.t state

  let get_node_public_key ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name) () =
    [%log debug] "Getting node public key from da node %s"
      (Host_and_port.to_string node_location.value) ;
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_signer_public_key.V1.t
      ()

  let get_signature ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name) ~state =
    [%log debug] "Getting signature from da node %s"
      (Host_and_port.to_string node_location.value) ;
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_signature.V2.t state

  let get_states_chain ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name)
      ?max_length ~source ~target () =
    [%log debug] "Getting ledger hashes chain from da node %s"
      (Host_and_port.to_string node_location.value) ;
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_ledger_hashes_chain.V2.t
      { source; target; max_length }

  let get_diffs_chain ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name)
      ?max_length ~source ~target () =
    [%log debug] "Getting diffs chain from da node %s"
      (Host_and_port.to_string node_location.value) ;
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_diffs_chain.V3.t
      { source; target; max_length }

  let has_diff ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name) ~state =
    [%log debug] "Checking if diff exists in da node %s"
      (Host_and_port.to_string node_location.value) ;
    dispatch ~max_tries:1 ~logger node_location Rpc.Has_diff.V2.t state

  let pipe_dispatch rpc query (host_and_port : Host_and_port.t) =
    let open Async in
    Deferred.Or_error.try_with_join ~here:[%here] (fun () ->
        let%bind _socket, r, w =
          Tcp.connect
            (Tcp.Where_to_connect.of_host_and_port host_and_port)
            ~timeout:(Time.Span.of_sec 5.)
        in
        let open Deferred.Let_syntax in
        match%bind
          Rpc.Connection.create
            ~handshake_timeout:
              (Time.Span.of_sec
                 Node_config_unconfigurable_constants.rpc_handshake_timeout_sec )
            ~heartbeat_config:
              (Rpc.Connection.Heartbeat_config.create
                 ~timeout:
                   (Time_ns.Span.of_sec
                      Node_config_unconfigurable_constants
                      .rpc_heartbeat_timeout_sec )
                 ~send_every:
                   (Time_ns.Span.of_sec
                      Node_config_unconfigurable_constants
                      .rpc_heartbeat_send_every_sec )
                 () )
            r w
            ~connection_state:(fun _ -> ())
        with
        | Error exn ->
            return
              (Or_error.errorf
                 !"Error connecting to the daemon on %{sexp:Host_and_port.t} \
                   using the RPC call, %s,: %s"
                 host_and_port (Rpc.Pipe_rpc.name rpc) (Exn.to_string exn) )
        | Ok conn -> (
            match%map Rpc.Pipe_rpc.dispatch rpc conn query with
            | Ok (Ok (pipe, metadata)) ->
                upon (Pipe.closed pipe) (fun () ->
                    don't_wait_for (Rpc.Connection.close conn) ) ;
                Ok (Ok (pipe, metadata))
            | Ok (Error _ as rpc_err) ->
                don't_wait_for (Rpc.Connection.close conn) ;
                Ok rpc_err
            | Error _ as transport_err ->
                don't_wait_for (Rpc.Connection.close conn) ;
                transport_err ) )

  let diffs_stream ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name) ~source
      ~target () =
    [%log debug] "Getting diffs stream from da node %s"
      (Host_and_port.to_string node_location.value) ;
    pipe_dispatch Rpc.Diffs_stream.V4.t { source; target } node_location.value
end

module Config = struct
  type t = { nodes : Host_and_port.t Cli_lib.Flag.Types.with_name list }
  [@@deriving fields]

  let of_string_list uris =
    { nodes =
        List.mapi uris ~f:(fun i s ->
            Cli_lib.Flag.Types.
              { value = Host_and_port.of_string s
              ; name = sprintf "da-node-%d" i
              } )
    }

  let of_node_locations nodes = { nodes }

  let fetch_public_keys ~logger t =
    let%map.Deferred fetched_da_keys =
      Deferred.List.map ~how:`Parallel t.nodes ~f:(fun node_location ->
          Rpc.get_node_public_key ~logger ~node_location ()
          >>| function
          | Ok result ->
              Some result
          | Error err ->
              [%log warn] "Failed to get node public key from da node %s: %s"
                (Host_and_port.to_string node_location.value)
                (Error.to_string_hum err) ;
              None )
      >>| List.filter_opt
    in
    List.sort fetched_da_keys ~compare:Public_key.Compressed.compare
end

type t =
  { logger : Logger.t
  ; config : Config.t
  ; signature_kind : Mina_signature_kind.t
  ; quorum : int  (** The amount of signatures needed when distributing diff *)
  ; db_pool : Db.pool
  ; pushed_diff : unit Condition.t
  ; pushed_signature : unit Condition.t
  ; stop : unit Ivar.t
  ; da_keys : Public_key.Compressed.t list
  }

let create ~logger ~(config : Config.t) ~signature_kind ~quorum ~da_keys
    ~db_pool =
  let%map.Deferred fetched_da_keys = Config.fetch_public_keys ~logger config in
  let sorted_da_keys =
    List.sort da_keys ~compare:Public_key.Compressed.compare
  in
  if not (List.equal Public_key.Compressed.equal fetched_da_keys sorted_da_keys)
  then
    [%log warn]
      !"DA keys do not match:\n\
        fetched: %{sexp: Public_key.Compressed.t list}\n\
        expected: %{sexp: Public_key.Compressed.t list}"
      fetched_da_keys sorted_da_keys ;
  { logger
  ; config
  ; signature_kind
  ; quorum
  ; db_pool
  ; pushed_diff = Condition.create ()
  ; pushed_signature = Condition.create ()
  ; stop = Ivar.create ()
  ; da_keys = sorted_da_keys
  }

let stop t = Ivar.fill t.stop ()

let enqueue_diff t ~source_state ~target_state ~ledger_openings
    ~acc_set_openings ~diff ~genesis =
  let%map () =
    Pool.use
      (fun conn ->
        Diff_table.insert conn
          { diff
          ; ledger_openings
          ; acc_set_openings
          ; source_state
          ; target_state
          ; genesis
          } )
      t.db_pool
    >>| caqti_ok_exn ~msg:"Failed to insert diff into db: %s"
  in
  Condition.broadcast t.pushed_diff ()

let rec start_posting_diffs_from ?timeout_on_failure ?pushed_diff t
    ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name)
    ~source_state () =
  if Ivar.is_full t.stop then return ()
  else
    let logger = t.logger in
    let pushed_diff =
      Option.value pushed_diff ~default:(Condition.wait t.pushed_diff)
    in
    match%bind
      Pool.use (fun c -> Diff_table.get_diff_by_source c source_state) t.db_pool
      >>| caqti_ok_exn ~msg:"Failed to get diff from db: %s"
    with
    | None ->
        [%log info]
          !"No diff found for source DA state: %{sexp: Da_state.t option} for \
            node %s, waiting"
          source_state
          (Host_and_port.to_string node_location.value) ;
        let%bind () = Deferred.any [ pushed_diff; Ivar.read t.stop ] in
        start_posting_diffs_from ?timeout_on_failure
          ~pushed_diff:(Condition.wait t.pushed_diff)
          t ~node_location ~source_state ()
    | Some
        { diff
        ; ledger_openings
        ; acc_set_openings
        ; source_state = diff_source_state
        ; target_state
        ; _
        } -> (
        match%bind
          Rpc.post_diff ~logger:t.logger ~node_location
            ~signature_kind:t.signature_kind ~source_state:diff_source_state
            ~target_state ~ledger_openings ~acc_set_openings ~diff
        with
        | Error err ->
            [%log error] "Failed to post diff to da node: %s"
              (Error.to_string_hum err) ;
            let%bind () =
              match timeout_on_failure with
              | None ->
                  return ()
              | Some timeout ->
                  [%log warn]
                    "Failed to post diff to da node $host_and_port, retrying \
                     in %s"
                    (Time_ns.Span.to_string_hum timeout)
                    ~metadata:
                      [ ( "host_and_port"
                        , `String (Host_and_port.to_string node_location.value)
                        )
                      ] ;
                  after timeout
            in
            start_posting_diffs_from ?timeout_on_failure t ~node_location
              ~source_state ()
        | Ok (public_key, signature) ->
            [%log info]
              !"Posted diff to da node %s for state: %{sexp: Da_state.t}"
              (Host_and_port.to_string node_location.value)
              target_state ;
            let%bind () =
              Pool.use
                (fun c ->
                  Signature_table.insert c
                    { target_state; public_key; signature } )
                t.db_pool
              >>| fun r ->
              if Ivar.is_full t.stop then ()
              else caqti_ok_exn ~msg:"Failed to insert signatures into db: %s" r
            in
            Condition.broadcast t.pushed_signature () ;
            start_posting_diffs_from ?timeout_on_failure t ~node_location
              ~source_state:(Some target_state) () )

let wait_for_successful_healthcheck ~timeout ~logger ~node_location () =
  keep_retrying ~delay:timeout ~logger
    ~f:(fun () ->
      match%map Rpc.healthcheck ~logger ~node_location () with
      | Ok () ->
          Ok ()
      | Error err ->
          [%log warn] "Healthcheck failed for node $host_and_port: $error"
            ~metadata:
              [ ( "host_and_port"
                , `String (Host_and_port.to_string node_location.value) )
              ; ("error", `String (Error.to_string_hum err))
              ] ;
          Error err )
    ()

let binary_search_last_state t ~node_location ~target_state =
  let return = Deferred.Result.return in
  if%bind.Deferred.Result
    Rpc.has_diff ~logger:t.logger ~node_location ~state:target_state
  then return (Some target_state)
  else
    let%bind.Deferred.Result target_id =
      Pool.use (fun c -> Diff_table.get_id_by_target c target_state) t.db_pool
      >>| caqti_ok_exn ~msg:"Failed to get id from target ledger hash: %s"
      >>| (fun opt -> Option.value_exn ~message:"No diff found" opt)
      >>| Result.return
    in
    let rec go ~left ~right =
      if left > right then return None
      else
        let mid = (left + right) / 2 in
        let%bind.Deferred.Result mid_state =
          Pool.use (fun c -> Diff_table.get_target_by_id c mid) t.db_pool
          >>| caqti_ok_exn ~msg:"Failed to find mid ledger hash: %s"
          >>| (fun opt ->
                Option.value_exn ~message:"Mid target ledger hash not found" opt
                )
          >>| Result.return
        in
        let%bind.Deferred.Result mid_found =
          Rpc.has_diff ~logger:t.logger ~node_location ~state:mid_state
        in
        let%bind.Deferred.Result next_state =
          Pool.use (fun c -> Diff_table.get_target_by_id c (mid + 1)) t.db_pool
          >>| caqti_ok_exn ~msg:"Failed to find next ledger hash: %s"
          >>| Result.return
        in
        let%bind.Deferred.Result next_found =
          match next_state with
          | Some next_state ->
              Rpc.has_diff ~logger:t.logger ~node_location ~state:next_state
          | None ->
              return false
        in
        if mid_found && not next_found then return (Some mid_state)
        else if mid_found && mid = target_id then return (Some mid_state)
        else if next_found && mid + 1 = target_id then return next_state
        else if (not mid_found) && mid = 1 then return None
        else if mid_found && next_found then go ~left:mid ~right
        else go ~left ~right:mid
    in
    go ~left:1 ~right:target_id

let catch_up t ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name)
    ~target_state =
  let logger = t.logger in
  let%bind.Deferred () =
    wait_for_successful_healthcheck ~timeout:(Time_ns.Span.of_sec 30.) ~logger
      ~node_location ()
  in
  let%bind last_state =
    keep_retrying ~logger
      ~f:(fun () -> binary_search_last_state t ~node_location ~target_state)
      ()
  in
  [%log info]
    !"Found last DA state: %{sexp: Da_state.t option} for node %s"
    last_state
    (Host_and_port.to_string node_location.value) ;
  let%bind () =
    match (last_state, Ivar.is_full t.stop) with
    | None, _ | _, true ->
        return ()
    | Some last_state, false ->
        (* In case we've started from checkpoint, we need to refetch the signature *)
        let%bind public_key =
          keep_retrying ~logger:t.logger
            ~f:(fun () ->
              Rpc.get_node_public_key ~logger:t.logger ~node_location () )
            ()
        in
        Pool.use
          (with_transaction ~f:(fun c ->
               let%bind.Deferred.Result signature_opt =
                 Signature_table.get_signature_opt c last_state public_key
               in
               if Option.is_some signature_opt then return (Ok ())
               else (
                 [%log info]
                   "Signature from node %s not present, fetching and inserting"
                   (Host_and_port.to_string node_location.value) ;
                 let%bind public_key, signature =
                   Rpc.get_signature ~logger:t.logger ~node_location
                     ~state:last_state
                   >>| Or_error.ok_exn
                   >>| fun x ->
                   Option.value_exn ~message:"Signature not found" x
                 in
                 Signature_table.insert c
                   { target_state = last_state; public_key; signature } ) ) )
          t.db_pool
        >>| fun res ->
        if Ivar.is_full t.stop then ()
        else caqti_ok_exn ~msg:"Failed to refetch signature: %s" res
  in
  if Ivar.is_full t.stop then return ()
  else
    start_posting_diffs_from t ~timeout_on_failure:(Time_ns.Span.of_sec 10.)
      ~node_location ~source_state:last_state ()

let start_client t ~target_state =
  List.iter t.config.nodes ~f:(fun node_location ->
      don't_wait_for
        (within' ~monitor:Monitor.main (fun () ->
             catch_up t ~node_location ~target_state ) ) )

let rec get_multisig ?pushed_signature t ~state =
  let pushed_signature =
    Option.value pushed_signature ~default:(Condition.wait t.pushed_signature)
  in
  let%bind signatures =
    Pool.use (fun c -> Signature_table.get_signatures c state) t.db_pool
    >>| caqti_ok_exn ~msg:"Failed to get signatures from db: %s"
  in
  if List.length signatures >= t.quorum then
    return
      ( t.quorum
      , List.map t.da_keys ~f:(fun da_key ->
            ( da_key
            , List.find_map signatures ~f:(fun { public_key; signature; _ } ->
                  if Public_key.Compressed.equal public_key da_key then
                    Some signature
                  else None ) ) ) )
  else
    let logger = t.logger in
    [%log info] "Not enough signatures, waiting for more" ;
    let%bind () = Deferred.any [ pushed_signature; Ivar.read t.stop ] in
    if Ivar.is_full t.stop then failwith "Da layer client stopped"
    else
      get_multisig
        ~pushed_signature:(Condition.wait t.pushed_signature)
        t ~state

(** Useful for querying data, will fallback to the next node in list in case the first one fails *)
let try_all_nodes ~config ~f =
  let rec try_first ~accum_errors = function
    | [] ->
        return (Error (Error.of_list accum_errors))
    | first_node :: rest -> (
        match%bind f ~node_location:first_node () with
        | Ok result ->
            return (Ok result)
        | Error err ->
            try_first ~accum_errors:(err :: accum_errors) rest )
  in
  try_first ~accum_errors:[] (Config.nodes config)

(** Get the chain of exact DA states from [source_state] to [target_state]. *)
let get_states_chain ~logger ~config ?max_length ~source_state ~target_state ()
    =
  try_all_nodes ~config ~f:(fun ~node_location () ->
      Rpc.get_states_chain ~logger ~node_location ?max_length
        ~source:source_state ~target:target_state () )

(** Get the chain of stored diffs from [source_state] to [target_state]. *)
let get_diffs_chain ~logger ~config ?max_length ~source_state ~target_state () =
  try_all_nodes ~config ~f:(fun ~node_location () ->
      Rpc.get_diffs_chain ~logger ~node_location ?max_length
        ~source:source_state ~target:target_state () )

let diff_exists ~logger ~config ~state () =
  try_all_nodes ~config ~f:(fun ~node_location () ->
      Rpc.has_diff ~logger ~node_location ~state )

let stream_diffs ~logger ~config ~source_state ~target_state () =
  try_all_nodes ~config ~f:(fun ~node_location () ->
      match%bind
        Rpc.diffs_stream ~logger ~node_location ~source:source_state
          ~target:target_state ()
      with
      | Ok (Ok stream) ->
          return (Ok stream)
      | Ok (Error err) | Error err ->
          return (Error err) )

(** Lazily fetch chunks of diffs, used to minimize memory usage *)
let get_lazy_diffs_chunks ~logger ~config ?(n = 1000) ~source_state
    ~target_state
    (rpc :
         logger:Logger.t
      -> config:Config.t
      -> source_state:[ `Genesis | `Specific of Da_state.t ]
      -> target_state:Da_state.t
      -> unit
      -> ('a, Error.t) result Deferred.t ) () =
  let specific_source_state =
    match source_state with `Genesis -> None | `Specific state -> Some state
  in
  let counter = ref 0 in
  (* Get ledger hashes intervals of size [n] *)
  let rec get_intervals ~target_state =
    let%bind.Deferred.Result chain =
      get_states_chain ~logger ~config ~max_length:n ~source_state ~target_state
        ()
    in
    counter := !counter + List.length chain ;
    [%log info] "Fetched %s ledger hashes" (Int.to_string_hum !counter) ;
    match chain with
    | [] ->
        return (Ok [])
    | [ last ] ->
        let interval_source =
          match specific_source_state with
          | Some state ->
              `Specific state
          | None ->
              `Genesis
        in
        let interval = (interval_source, last) in
        return (Ok [ interval ])
    | chain ->
        let hd = List.hd_exn chain in
        let tl = List.last_exn chain in
        let interval = (`Specific hd, tl) in
        let%bind.Deferred.Result next_intervals =
          get_intervals ~target_state:hd
        in
        return (Ok (interval :: next_intervals))
  in
  let%bind.Deferred.Result intervals =
    [%log info] "Fetching intervals from da layer of size %s"
      (Int.to_string_hum n) ;
    get_intervals ~target_state >>| Result.map ~f:List.rev
  in
  [%log debug] "Fetched %s intervals"
    (Int.to_string_hum (List.length intervals)) ;
  return
  @@ Ok
       (List.map intervals ~f:(fun (source, target) ->
            [%log debug] "Creating lazy chunk from %s to %s"
              ( match source with
              | `Genesis ->
                  "genesis"
              | `Specific state ->
                  Da_state.to_string state )
              (Da_state.to_string target) ;
            lazy
              ( [%log debug] "Forcing diffs chunk from %s to %s"
                  ( match source with
                  | `Genesis ->
                      "genesis"
                  | `Specific state ->
                      Da_state.to_string state )
                  (Da_state.to_string target) ;
                rpc ~logger ~config ~source_state:source ~target_state:target ()
              ) ) )

let map_diffs :
       ?interval_size:int
    -> logger:Logger.t
    -> config:Config.t
    -> source_state:[< `Genesis | `Specific of Da_state.t ]
    -> target_state:Da_state.t
    -> f:
         (   current_chunk:int
          -> current_diff:int
          -> chunks_length:int
          -> Stored_diff.t
          -> 'a Deferred.t )
    -> unit
    -> ('a list, Error.t) Deferred.Result.t =
 fun ?interval_size ~logger ~config ~source_state ~target_state ~f () ->
  let%bind.Deferred.Result lazy_chunks =
    get_lazy_diffs_chunks ?n:interval_size ~logger ~config ~source_state
      ~target_state
      (get_diffs_chain ?max_length:None)
      ()
  in
  [%log debug] "Fetched %s lazy chunks"
    (Int.to_string_hum (List.length lazy_chunks)) ;
  let l = List.length lazy_chunks in
  let%map.Deferred.Result result =
    Deferred.List.foldi ~init:(Ok []) lazy_chunks ~f:(fun i acc lazy_chunk ->
        match acc with
        | Error err ->
            return (Error err)
        | Ok acc -> (
            match%bind Lazy.force lazy_chunk with
            | Ok diffs ->
                let%bind result =
                  Deferred.List.mapi ~how:`Sequential diffs ~f:(fun j diff ->
                      f ~current_chunk:i ~current_diff:j ~chunks_length:l diff )
                in
                return (Ok (result :: acc))
            | Error err ->
                return (Error err) ) )
  in
  List.rev result |> List.join

let iter_diffs :
       ?interval_size:int
    -> logger:Logger.t
    -> config:Config.t
    -> source_state:[< `Genesis | `Specific of Da_state.t ]
    -> target_state:Da_state.t
    -> f:
         (   current_chunk:int
          -> current_diff:int
          -> chunks_length:int
          -> Stored_diff.t
          -> unit Deferred.t )
    -> unit
    -> (unit, Error.t) Deferred.Result.t =
 fun ?interval_size ~logger ~config ~source_state ~target_state ~f () ->
  let%bind.Deferred.Result lazy_chunks =
    get_lazy_diffs_chunks ?n:interval_size ~logger ~config ~source_state
      ~target_state stream_diffs ()
  in
  [%log debug] "Fetched %s lazy chunks"
    (Int.to_string_hum (List.length lazy_chunks)) ;
  let l = List.length lazy_chunks in
  Deferred.List.foldi ~init:(Ok ()) lazy_chunks ~f:(fun i acc lazy_chunk ->
      match acc with
      | Error err ->
          return (Error err)
      | Ok () -> (
          match%bind Lazy.force lazy_chunk with
          | Ok (diffs, _) ->
              let%bind () =
                Pipe.fold diffs ~init:0 ~f:(fun j diff ->
                    let%map () =
                      f ~current_chunk:i ~current_diff:j ~chunks_length:l diff
                    in
                    j + 1 )
                >>| ignore
              in
              return (Ok ())
          | Error err ->
              return (Error err) ) )

(** Try to get the diff from the first node in the list, if it fails, try the next one *)
let get_diff ~logger ~config ~state =
  try_all_nodes ~config ~f:(fun ~node_location () ->
      match%bind Rpc.get_diff ~logger ~node_location ~state with
      | Ok (Some diff) ->
          return (Ok diff)
      | Ok None ->
          return (Error (Error.of_string "Diff not found"))
      | Error e ->
          return (Error e) )

let distribute_diff ~logger ~config ~signature_kind ~source_state ~target_state
    ~ledger_openings ~acc_set_openings ~diff =
  Deferred.List.iter ~how:`Parallel
    Config.(config.nodes)
    ~f:(fun n ->
      match%map
        Rpc.post_diff ~logger ~node_location:n ~signature_kind ~source_state
          ~target_state ~ledger_openings ~acc_set_openings ~diff
      with
      | Ok _ ->
          ()
      | Error e ->
          [%log error] "Failed to post diff to da node: %s"
            (Error.to_string_hum e) )

let acc_set_opening_keys ~new_keys ~find_lower =
  Acc_set_transition.opening_keys ~find_lower new_keys |> Or_error.ok_exn

let get_in_memory_acc_set_openings ~logger ~changed_accounts ~ledger_openings
    ~imt =
  let new_keys =
    Acc_set_transition.new_account_keys ~changed_accounts ~ledger_openings
  in
  let keys =
    acc_set_opening_keys ~new_keys
      ~find_lower:(Indexed_merkle_tree.In_memory.find_lower_entry_tid imt)
  in
  Indexed_merkle_tree.Sparse.of_in_memory_subset ~logger ~db:imt ~keys

(** One diff can be too big, split it into multiple smaller ones
    To have only one diff set [max_size] to [Int.max_value]
*)
let create_genesis_diffs ?(max_size = 50) ~logger ledger ~get_actions_for_aid =
  let%bind account_ids =
    Ledger.to_list ledger >>| List.map ~f:Account.identifier
  in
  let changed_accounts =
    List.map account_ids ~f:(fun aid ->
        let index = Ledger.index_of_account_exn ledger aid in
        let account = Ledger.get_at_index_exn ledger index in
        (index, account) )
  in
  let acc_set =
    Indexed_merkle_tree.In_memory.create ~depth:(Ledger.depth ledger) ()
  in
  let ephemeral = Ledger.create_ephemeral ~depth:(Ledger.depth ledger) () in
  let account_chunks = List.chunks_of changed_accounts ~length:max_size in
  [%log debug] "Created %s account chunks"
    (Int.to_string_hum (List.length account_chunks)) ;
  let%map result =
    Deferred.List.map ~how:`Sequential account_chunks ~f:(fun chunk ->
        let source_state =
          Da_state.create
            ~ledger_hash:(Ledger.merkle_root ephemeral)
            ~acc_set:(Indexed_merkle_tree.In_memory.merkle_root acc_set)
        in
        let ledger_openings =
          Sparse_ledger.of_ledger_subset_exn ephemeral
            (List.map chunk ~f:snd |> List.map ~f:Account.identifier)
        in
        List.iter chunk ~f:(fun (index, account) ->
            Ledger.set_at_index_exn ephemeral index account ) ;
        let actions =
          List.map chunk ~f:(fun (_, account) ->
              let aid = Account.identifier account in
              (aid, get_actions_for_aid (Account.identifier account)) )
        in
        let diff =
          Diff.create_pending
            ~source_ledger_hash:(Sparse_ledger.merkle_root ledger_openings)
            ~changed_accounts:chunk ~actions:(`Actions actions)
        in
        [%log debug] "Adding accounts to acc set db" ;
        let new_account_keys =
          List.map chunk ~f:(fun (_, account) ->
              Account_id.derive_token_id ~owner:(Account.identifier account) )
        in
        Indexed_merkle_tree.In_memory.insert_batch_exn acc_set new_account_keys ;
        [%log debug] "Creating acc set openings" ;
        let acc_set_openings =
          get_in_memory_acc_set_openings ~logger
            ~changed_accounts:diff.changed_accounts ~ledger_openings
            ~imt:acc_set
        in
        let target_state =
          Da_state.create
            ~ledger_hash:(Ledger.merkle_root ephemeral)
            ~acc_set:(Indexed_merkle_tree.In_memory.merkle_root acc_set)
        in
        return
          ( diff
          , ledger_openings
          , acc_set_openings
          , `Source source_state
          , `Target target_state ) )
  in
  let (_ : Ledger.unattached_mask) =
    Ledger.Maskable.unregister_mask_exn ~loc:__LOC__ ~grandchildren:`Recursive
      ephemeral
  in
  result

(** Distribute diff of initial accounts *)
let distribute_genesis_diff ~logger ~config ~signature_kind ~ledger
    ~get_actions_for_aid =
  let%bind diffs = create_genesis_diffs ~logger ledger ~get_actions_for_aid in
  Deferred.List.iter ~how:`Sequential diffs
    ~f:(fun
         ( diff
         , ledger_openings
         , acc_set_openings
         , `Source source_state
         , `Target target_state )
       ->
      distribute_diff ~logger ~config ~signature_kind ~source_state
        ~target_state ~ledger_openings ~acc_set_openings ~diff )

let get_ledger_openings ~diff ~ledger =
  let changed_accounts =
    Diff.changed_accounts diff
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  in
  let account_ids =
    List.map changed_accounts ~f:snd |> List.map ~f:Account.identifier
  in
  let ledger_openings = Sparse_ledger.of_ledger_subset_exn ledger account_ids in
  ledger_openings

let attach_ledger_openings ~diffs ~ledger =
  List.map diffs ~f:(fun diff -> (diff, get_ledger_openings ~diff ~ledger))

let get_acc_set_openings ~logger ~changed_accounts ~ledger_openings ~imt =
  let new_keys =
    Acc_set_transition.new_account_keys ~changed_accounts ~ledger_openings
  in
  let keys =
    acc_set_opening_keys ~new_keys
      ~find_lower:(Indexed_merkle_tree.Db.find_lower_entry_tid imt)
  in
  Indexed_merkle_tree.Sparse.of_db_subset ~logger ~db:imt ~keys
