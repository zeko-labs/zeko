open Core_kernel
open Async_kernel
open Mina_base
open Mina_ledger
open Signature_lib
open Relational_db
module Field = Snark_params.Tick.Field

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
    { diff : Diff.Stable.V1.t
    ; ledger_openings : Sparse_ledger.t
    ; acc_set_openings : Indexed_merkle_tree.Sparse.t
    ; genesis : bool
    ; target_ledger_hash : Ledger_hash.t
    }
  [@@deriving hlist, fields, sexp]

  let make ~diff ~ledger_openings ~acc_set_openings ~target_ledger_hash ~genesis
      =
    { diff; ledger_openings; acc_set_openings; target_ledger_hash; genesis }

  let typ =
    Mina_caqti.Type_spec.custom_type
      ~to_hlist:(fun { diff
                     ; ledger_openings
                     ; acc_set_openings
                     ; target_ledger_hash
                     ; genesis
                     } ->
        H_list.
          [ Ledger_hash.to_decimal_string target_ledger_hash
          ; ( if genesis then None
            else Some (Ledger_hash.to_decimal_string diff.source_ledger_hash) )
          ; Binable.to_bigstring
              (module Diff.Stable.V1.With_top_version_tag)
              diff
            |> Bigstring.to_string
          ; Sparse_ledger.to_yojson ledger_openings |> Yojson.Safe.to_string
          ; Indexed_merkle_tree.Sparse.to_yojson acc_set_openings
            |> Yojson.Safe.to_string
          ] )
      ~of_hlist:(fun H_list.
                       [ target_ledger_hash
                       ; source_ledger_hash
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
        { diff =
            Binable.of_bigstring
              (module Diff.Stable.V1.With_top_version_tag)
              (Bigstring.of_string diff)
        ; ledger_openings =
            Sparse_ledger.of_yojson (Yojson.Safe.from_string ledger_openings)
            |> ok_exn
        ; acc_set_openings =
            Indexed_merkle_tree.Sparse.of_yojson
              (Yojson.Safe.from_string acc_set_openings)
            |> ok_exn
        ; genesis = Option.is_none source_ledger_hash
        ; target_ledger_hash = Ledger_hash.of_decimal_string target_ledger_hash
        } )
      Caqti_type.[ string; option string; octets; octets; octets ]

  let insert (module Conn : CONNECTION) t =
    Conn.exec
      (Caqti_request.exec typ
         {sql| INSERT INTO da_diff (target_ledger_hash, source_ledger_hash, diff, ledger_openings, acc_set_openings)
                VALUES (?, ?, ?, ?, ?) |sql} )
      t

  let get_diff_by_source (module Conn : CONNECTION) ledger_hash =
    match ledger_hash with
    | Some ledger_hash ->
        Conn.find_opt
          (Caqti_request.find_opt Caqti_type.string typ
             {sql| SELECT target_ledger_hash, source_ledger_hash, diff, ledger_openings, acc_set_openings FROM da_diff WHERE source_ledger_hash = ? |sql} )
          (Ledger_hash.to_decimal_string ledger_hash)
    | None ->
        Conn.find_opt
          (Caqti_request.find_opt Caqti_type.unit typ
             {sql| SELECT target_ledger_hash, source_ledger_hash, diff, ledger_openings, acc_set_openings FROM da_diff WHERE source_ledger_hash IS NULL |sql} )
          ()

  let get_id_by_target (module Conn : CONNECTION) ledger_hash =
    Conn.find_opt
      (Caqti_request.find_opt Caqti_type.string Caqti_type.int
         {sql| SELECT id FROM da_diff WHERE target_ledger_hash = ? |sql} )
      (Ledger_hash.to_decimal_string ledger_hash)

  let get_target_by_id (module Conn : CONNECTION) id =
    let%map.Deferred.Result result =
      Conn.find_opt
        (Caqti_request.find_opt Caqti_type.int Caqti_type.string
           {sql| SELECT target_ledger_hash FROM da_diff WHERE id = ? |sql} )
        id
    in
    Option.map ~f:Ledger_hash.of_decimal_string result
end

module Signature_table = struct
  type t =
    { target_ledger_hash : Ledger_hash.t
    ; public_key : Public_key.Compressed.t
    ; signature : Signature.t
    }
  [@@deriving hlist, fields]

  let typ =
    Mina_caqti.Type_spec.custom_type
      ~to_hlist:(fun { target_ledger_hash; public_key; signature } ->
        H_list.
          [ Ledger_hash.to_decimal_string target_ledger_hash
          ; Public_key.Compressed.to_base58_check public_key
          ; Signature.to_base58_check signature
          ] )
      ~of_hlist:(fun H_list.[ target_ledger_hash; public_key; signature ] ->
        { target_ledger_hash = Ledger_hash.of_decimal_string target_ledger_hash
        ; public_key = Public_key.Compressed.of_base58_check_exn public_key
        ; signature = Signature.of_base58_check_exn signature
        } )
      Caqti_type.[ string; string; octets ]

  let insert (module Conn : CONNECTION) t =
    Conn.exec
      (Caqti_request.exec typ
         {sql| INSERT INTO da_signature (target_ledger_hash, public_key, signature) VALUES (?, ?, ?) |sql} )
      t

  let get_signature_opt (module Conn : CONNECTION) ledger_hash public_key =
    Conn.find_opt
      (Caqti_request.find_opt
         Caqti_type.(tup2 string string)
         typ
         {sql| SELECT target_ledger_hash, public_key, signature FROM da_signature WHERE target_ledger_hash = ? AND public_key = ? |sql} )
      ( Ledger_hash.to_decimal_string ledger_hash
      , Public_key.Compressed.to_base58_check public_key )

  let get_signatures (module Conn : CONNECTION) ledger_hash =
    Conn.collect_list
      (Caqti_request.collect Caqti_type.string typ
         {sql| SELECT target_ledger_hash, public_key, signature FROM da_signature WHERE target_ledger_hash = ? |sql} )
      (Ledger_hash.to_decimal_string ledger_hash)
end

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
      ~ledger_openings ~acc_set_openings ~diff =
    [%log debug] "Posting diff to da node %s"
      (Host_and_port.to_string node_location.value) ;
    dispatch ~max_tries:5 ~logger node_location Rpc.Post_diff.V1.t
      { ledger_openings; diff; acc_set_openings }

  let get_diff ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name)
      ~ledger_hash =
    [%log debug] "Getting diff from da node %s"
      (Host_and_port.to_string node_location.value) ;
    dispatch_with_fallback_same_query ~max_tries:1 ~logger node_location
      ledger_hash
      ~versions:
        [ Versioned_rpc_same_query.V (Rpc.Get_diff.V3.t, Fn.id)
        ; Versioned_rpc_same_query.V
            (Rpc.Get_diff.V2.t, Option.map ~f:Diff.Stable.V2.to_latest)
        ; Versioned_rpc_same_query.V
            (Rpc.Get_diff.V1.t, Option.map ~f:Diff.Stable.V1.to_latest)
        ]

  let get_diff_source ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name)
      ~ledger_hash =
    [%log debug] "Getting diff source from da node %s"
      (Host_and_port.to_string node_location.value) ;
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_diff_source.V1.t
      ledger_hash

  let get_node_public_key ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name) () =
    [%log debug] "Getting node public key from da node %s"
      (Host_and_port.to_string node_location.value) ;
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_signer_public_key.V1.t
      ()

  let get_signature ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name)
      ~ledger_hash =
    [%log debug] "Getting signature from da node %s"
      (Host_and_port.to_string node_location.value) ;
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_signature.V1.t
      ledger_hash

  let get_ledger_hashes_chain ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name)
      ?max_length ~source ~target () =
    [%log debug] "Getting ledger hashes chain from da node %s"
      (Host_and_port.to_string node_location.value) ;
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_ledger_hashes_chain.V1.t
      { source; target; max_length }

  let get_diffs_chain ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name)
      ?max_length ~source ~target () =
    [%log debug] "Getting diffs chain from da node %s"
      (Host_and_port.to_string node_location.value) ;
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_diffs_chain.V1.t
      { source; target; max_length }

  let has_diff ~logger
      ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name)
      ~ledger_hash =
    [%log debug] "Checking if diff exists in da node %s"
      (Host_and_port.to_string node_location.value) ;
    dispatch ~max_tries:1 ~logger node_location Rpc.Has_diff.V1.t ledger_hash

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
    pipe_dispatch Rpc.Diffs_stream.V2.t { source; target } node_location.value
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
    let%map.Deferred.Result fetched_da_keys =
      Deferred.List.map ~how:`Parallel t.nodes ~f:(fun node_location ->
          Rpc.get_node_public_key ~logger ~node_location () )
      >>| Result.all
    in
    List.sort fetched_da_keys ~compare:Public_key.Compressed.compare
end

type t =
  { logger : Logger.t
  ; config : Config.t
  ; quorum : int  (** The amount of signatures needed when distributing diff *)
  ; db_pool : Db.pool
  ; pushed_diff : unit Condition.t
  ; pushed_signature : unit Condition.t
  ; stop : unit Ivar.t
  ; da_keys : Public_key.Compressed.t list
  }

let create ~logger ~(config : Config.t) ~quorum ~da_keys ~db_pool =
  let%map.Deferred.Result fetched_da_keys =
    Config.fetch_public_keys ~logger config
  in
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
  ; quorum
  ; db_pool
  ; pushed_diff = Condition.create ()
  ; pushed_signature = Condition.create ()
  ; stop = Ivar.create ()
  ; da_keys = sorted_da_keys
  }

let stop t = Ivar.fill t.stop ()

let enqueue_diff t ~target_ledger_hash ~ledger_openings ~acc_set_openings ~diff
    ~genesis =
  let%map () =
    Pool.use
      (fun conn ->
        Diff_table.insert conn
          { diff
          ; ledger_openings
          ; acc_set_openings
          ; target_ledger_hash
          ; genesis
          } )
      t.db_pool
    >>| caqti_ok_exn ~msg:"Failed to insert diff into db: %s"
  in
  Condition.broadcast t.pushed_diff ()

let rec start_posting_diffs_from ?pushed_diff t
    ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name)
    ~source_ledger_hash () =
  if Ivar.is_full t.stop then return ()
  else
    let logger = t.logger in
    let pushed_diff =
      Option.value pushed_diff ~default:(Condition.wait t.pushed_diff)
    in
    match%bind
      Pool.use
        (fun c -> Diff_table.get_diff_by_source c source_ledger_hash)
        t.db_pool
      >>| caqti_ok_exn ~msg:"Failed to get diff from db: %s"
    with
    | None ->
        [%log info]
          !"No diff found for source ledger hash: %{sexp: Ledger_hash.t \
            option} for node %s, waiting"
          source_ledger_hash
          (Host_and_port.to_string node_location.value) ;
        let%bind () = Deferred.any [ pushed_diff; Ivar.read t.stop ] in
        start_posting_diffs_from
          ~pushed_diff:(Condition.wait t.pushed_diff)
          t ~node_location ~source_ledger_hash ()
    | Some { diff; ledger_openings; acc_set_openings; target_ledger_hash; _ }
      -> (
        match%bind
          Rpc.post_diff ~logger:t.logger ~node_location ~ledger_openings
            ~acc_set_openings ~diff
        with
        | Error err ->
            [%log error] "Failed to post diff to da node: %s"
              (Error.to_string_hum err) ;
            start_posting_diffs_from t ~node_location ~source_ledger_hash ()
        | Ok (public_key, signature) ->
            [%log info]
              !"Posted diff to da node %s with hash: %{sexp: Ledger_hash.t}"
              (Host_and_port.to_string node_location.value)
              target_ledger_hash ;
            let%bind () =
              Pool.use
                (fun c ->
                  Signature_table.insert c
                    { target_ledger_hash; public_key; signature } )
                t.db_pool
              >>| fun r ->
              if Ivar.is_full t.stop then ()
              else caqti_ok_exn ~msg:"Failed to insert signatures into db: %s" r
            in
            Condition.broadcast t.pushed_signature () ;
            start_posting_diffs_from t ~node_location
              ~source_ledger_hash:(Some target_ledger_hash) () )

let binary_search_last_ledger_hash t ~node_location ~target_ledger_hash =
  let return = Deferred.Result.return in
  if%bind.Deferred.Result
    Rpc.has_diff ~logger:t.logger ~node_location ~ledger_hash:target_ledger_hash
  then return (Some target_ledger_hash)
  else
    let%bind.Deferred.Result target_id =
      Pool.use
        (fun c -> Diff_table.get_id_by_target c target_ledger_hash)
        t.db_pool
      >>| caqti_ok_exn ~msg:"Failed to get id from target ledger hash: %s"
      >>| (fun opt -> Option.value_exn ~message:"No diff found" opt)
      >>| Result.return
    in
    let rec go ~left ~right =
      if left > right then return None
      else
        let mid = (left + right) / 2 in
        let%bind.Deferred.Result mid_ledger_hash =
          Pool.use (fun c -> Diff_table.get_target_by_id c mid) t.db_pool
          >>| caqti_ok_exn ~msg:"Failed to find mid ledger hash: %s"
          >>| (fun opt ->
                Option.value_exn ~message:"Mid target ledger hash not found" opt
                )
          >>| Result.return
        in
        let%bind.Deferred.Result mid_found =
          Rpc.has_diff ~logger:t.logger ~node_location
            ~ledger_hash:mid_ledger_hash
        in
        let%bind.Deferred.Result next_ledger_hash =
          Pool.use (fun c -> Diff_table.get_target_by_id c (mid + 1)) t.db_pool
          >>| caqti_ok_exn ~msg:"Failed to find next ledger hash: %s"
          >>| Result.return
        in
        let%bind.Deferred.Result next_found =
          match next_ledger_hash with
          | Some next_ledger_hash ->
              Rpc.has_diff ~logger:t.logger ~node_location
                ~ledger_hash:next_ledger_hash
          | None ->
              return false
        in
        if mid_found && not next_found then return (Some mid_ledger_hash)
        else if mid_found && mid = target_id then return (Some mid_ledger_hash)
        else if next_found && mid + 1 = target_id then return next_ledger_hash
        else if (not mid_found) && mid = 1 then return None
        else if mid_found && next_found then go ~left:mid ~right
        else go ~left ~right:mid
    in
    go ~left:1 ~right:target_id

let catch_up t ~(node_location : Host_and_port.t Cli_lib.Flag.Types.with_name)
    ~target_ledger_hash =
  let logger = t.logger in
  let%bind last_ledger_hash =
    keep_retrying ~logger
      ~f:(fun () ->
        binary_search_last_ledger_hash t ~node_location ~target_ledger_hash )
      ()
  in
  [%log info]
    !"Found last ledger hash: %{sexp: Ledger_hash.t option} for node %s"
    last_ledger_hash
    (Host_and_port.to_string node_location.value) ;
  let%map () =
    match last_ledger_hash with
    | None ->
        return ()
    | Some last_ledger_hash ->
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
                 Signature_table.get_signature_opt c last_ledger_hash public_key
               in
               if Option.is_some signature_opt then return (Ok ())
               else (
                 [%log info]
                   "Signature from node %s not present, fetching and inserting"
                   (Host_and_port.to_string node_location.value) ;
                 let%bind public_key, signature =
                   Rpc.get_signature ~logger:t.logger ~node_location
                     ~ledger_hash:last_ledger_hash
                   >>| Or_error.ok_exn
                   >>| fun x ->
                   Option.value_exn ~message:"Signature not found" x
                 in
                 Signature_table.insert c
                   { target_ledger_hash; public_key; signature } ) ) )
          t.db_pool
        >>| caqti_ok_exn ~msg:"Failed to refetch signature: %s"
  in
  don't_wait_for
    (within' ~monitor:Monitor.main (fun () ->
         start_posting_diffs_from t ~node_location
           ~source_ledger_hash:last_ledger_hash () ) )

let start_client t ~target_ledger_hash =
  Deferred.List.iter ~how:`Parallel t.config.nodes ~f:(fun node_location ->
      catch_up t ~node_location ~target_ledger_hash )

let rec get_multisig ?pushed_signature t ~ledger_hash =
  let pushed_signature =
    Option.value pushed_signature ~default:(Condition.wait t.pushed_signature)
  in
  let%bind signatures =
    Pool.use (fun c -> Signature_table.get_signatures c ledger_hash) t.db_pool
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
        t ~ledger_hash

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

(** Get the chain of ledger hashes from [source_ledger_hash] hash to [target_ledger_hash] *)
let get_ledger_hashes_chain ~logger ~config ?max_length ~source_ledger_hash
    ~target_ledger_hash () =
  try_all_nodes ~config ~f:(fun ~node_location () ->
      Rpc.get_ledger_hashes_chain ~logger ~node_location ?max_length
        ~source:source_ledger_hash ~target:target_ledger_hash () )

(** Get the chain of diffs from [source_ledger_hash] hash to [target_ledger_hash] *)
let get_diffs_chain ~logger ~config ?max_length ~source_ledger_hash
    ~target_ledger_hash () =
  try_all_nodes ~config ~f:(fun ~node_location () ->
      Rpc.get_diffs_chain ~logger ~node_location ?max_length
        ~source:source_ledger_hash ~target:target_ledger_hash () )

let diff_exists ~logger ~config ~ledger_hash () =
  try_all_nodes ~config ~f:(fun ~node_location () ->
      Rpc.has_diff ~logger ~node_location ~ledger_hash )

let stream_diffs ~logger ~config ~source_ledger_hash ~target_ledger_hash () =
  try_all_nodes ~config ~f:(fun ~node_location () ->
      match%bind
        Rpc.diffs_stream ~logger ~node_location ~source:source_ledger_hash
          ~target:target_ledger_hash ()
      with
      | Ok (Ok stream) ->
          return (Ok stream)
      | Ok (Error err) | Error err ->
          return (Error err) )

(** Lazily fetch chunks of diffs, used to minimize memory usage *)
let get_lazy_diffs_chunks ~logger ~depth ~config ?(n = 1000) ~source_ledger_hash
    ~target_ledger_hash
    (rpc :
         logger:Logger.t
      -> config:Config.t
      -> source_ledger_hash:[ `Genesis | `Specific of Field.t ]
      -> target_ledger_hash:Field.t
      -> unit
      -> ('a, Error.t) result Deferred.t ) () =
  let source_ledger_hash =
    match source_ledger_hash with
    | `Genesis ->
        Diff.empty_ledger_hash ~depth
    | `Specific h ->
        h
  in
  let counter = ref 0 in
  (* Get ledger hashes intervals of size [n] *)
  let rec get_intervals ~target_ledger_hash =
    let%bind.Deferred.Result chain =
      get_ledger_hashes_chain ~logger ~config ~max_length:n
        ~source_ledger_hash:(`Specific source_ledger_hash) ~target_ledger_hash
        ()
    in
    counter := !counter + List.length chain ;
    [%log info] "Fetched %s ledger hashes" (Int.to_string_hum !counter) ;
    match chain with
    | [] ->
        return (Ok [])
    | [ last ] ->
        let interval = (source_ledger_hash, last) in
        return (Ok [ interval ])
    | chain ->
        let hd = List.hd_exn chain in
        let tl = List.last_exn chain in
        let interval = (hd, tl) in
        let%bind.Deferred.Result next_intervals =
          get_intervals ~target_ledger_hash:hd
        in
        return (Ok (interval :: next_intervals))
  in
  let%bind.Deferred.Result intervals =
    [%log info] "Fetching intervals from da layer of size %s"
      (Int.to_string_hum n) ;
    get_intervals ~target_ledger_hash >>| Result.map ~f:List.rev
  in
  [%log debug] "Fetched %s intervals"
    (Int.to_string_hum (List.length intervals)) ;
  return
  @@ Ok
       (List.map intervals ~f:(fun (source, target) ->
            [%log debug] "Creating lazy chunk from %s to %s"
              (Ledger_hash.to_decimal_string source)
              (Ledger_hash.to_decimal_string target) ;
            lazy
              ( [%log debug] "Forcing diffs chunk from %s to %s"
                  (Ledger_hash.to_decimal_string source)
                  (Ledger_hash.to_decimal_string target) ;
                rpc ~logger ~config ~source_ledger_hash:(`Specific source)
                  ~target_ledger_hash:target () ) ) )

let map_diffs :
       ?interval_size:int
    -> logger:Logger.t
    -> depth:int
    -> config:Config.t
    -> source_ledger_hash:[< `Genesis | `Specific of Field.t ]
    -> target_ledger_hash:Field.t
    -> f:
         (   current_chunk:int
          -> current_diff:int
          -> chunks_length:int
          -> Diff.Stable.V3.t
          -> 'a Deferred.t )
    -> unit
    -> ('a list, Error.t) Deferred.Result.t =
 fun ?interval_size ~logger ~depth ~config ~source_ledger_hash
     ~target_ledger_hash ~f () ->
  let%bind.Deferred.Result lazy_chunks =
    get_lazy_diffs_chunks ?n:interval_size ~logger ~depth ~config
      ~source_ledger_hash ~target_ledger_hash
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
    -> depth:int
    -> config:Config.t
    -> source_ledger_hash:[< `Genesis | `Specific of Field.t ]
    -> target_ledger_hash:Field.t
    -> f:
         (   current_chunk:int
          -> current_diff:int
          -> chunks_length:int
          -> Diff.Stable.V3.t
          -> unit Deferred.t )
    -> unit
    -> (unit, Error.t) Deferred.Result.t =
 fun ?interval_size ~logger ~depth ~config ~source_ledger_hash
     ~target_ledger_hash ~f () ->
  let%bind.Deferred.Result lazy_chunks =
    get_lazy_diffs_chunks ?n:interval_size ~logger ~depth ~config
      ~source_ledger_hash ~target_ledger_hash stream_diffs ()
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
let get_diff ~logger ~config ~ledger_hash =
  try_all_nodes ~config ~f:(fun ~node_location () ->
      match%bind Rpc.get_diff ~logger ~node_location ~ledger_hash with
      | Ok (Some diff) ->
          return (Ok diff)
      | Ok None ->
          return (Error (Error.of_string "Diff not found"))
      | Error e ->
          return (Error e) )

let distribute_diff ~logger ~config ~ledger_openings ~acc_set_openings ~diff =
  Deferred.List.iter ~how:`Parallel
    Config.(config.nodes)
    ~f:(fun n ->
      match%map
        Rpc.post_diff ~logger ~node_location:n ~ledger_openings
          ~acc_set_openings ~diff
      with
      | Ok _ ->
          ()
      | Error e ->
          [%log error] "Failed to post diff to da node: %s"
            (Error.to_string_hum e) )

(** One diff can be too big, split it into multiple smaller ones
    To have only one diff set [max_size] to [Int.max_value]
*)
let create_genesis_diffs ?(max_size = 50) ~logger ledger =
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
  Deferred.List.map ~how:`Sequential account_chunks ~f:(fun chunk ->
      let ledger_openings =
        Sparse_ledger.of_ledger_subset_exn ephemeral
          (List.map chunk ~f:snd |> List.map ~f:Account.identifier)
      in
      List.iter chunk ~f:(fun (index, account) ->
          Ledger.set_at_index_exn ephemeral index account ) ;
      let diff =
        Diff.create
          ~source_ledger_hash:(Sparse_ledger.merkle_root ledger_openings)
          ~changed_accounts:chunk ~command_with_action_step_flags:None
      in
      [%log debug] "Adding accounts to acc set db" ;
      Indexed_merkle_tree.In_memory.insert_batch_exn acc_set
        (List.map chunk ~f:(fun (_, account) ->
             Account_id.derive_token_id ~owner:(Account.identifier account) ) ) ;
      [%log debug] "Creating acc set openings" ;
      let acc_set_openings =
        Indexed_merkle_tree.Sparse.of_in_memory_subset ~logger ~db:acc_set
          ~keys:
            ( [%log debug] "Getting accounts from chunk" ;
              List.map chunk ~f:snd
              |> List.map ~f:(fun acc ->
                     Account_id.derive_token_id ~owner:(Account.identifier acc) )
            )
      in
      return
        ( diff
        , ledger_openings
        , acc_set_openings
        , `Target (Ledger.merkle_root ephemeral) ) )

(** Distribute diff of initial accounts *)
let distribute_genesis_diff ~logger ~config ~ledger =
  let%bind diffs = create_genesis_diffs ~logger ledger in
  Deferred.List.iter ~how:`Sequential diffs
    ~f:(fun (diff, ledger_openings, acc_set_openings, `Target _) ->
      distribute_diff ~logger ~config ~ledger_openings ~acc_set_openings ~diff )

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

let get_acc_set_openings ~diff ~ledger_openings ~imt =
  let new_accounts_keys =
    List.filter (Diff.changed_accounts diff) ~f:(fun (index, _) ->
        Account.equal
          (Sparse_ledger.get_exn ledger_openings index)
          Account.empty )
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
    |> List.map ~f:(fun (_, account) ->
           Account_id.derive_token_id ~owner:(Account.identifier account) )
  in
  let acc_set_openings =
    Indexed_merkle_tree.Sparse.of_db_subset ~db:imt ~keys:new_accounts_keys
  in
  acc_set_openings
