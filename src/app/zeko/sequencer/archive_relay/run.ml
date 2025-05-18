open Async
open Core_kernel
open Mina_base
open Mina_lib
open Mina_ledger
open Cli_lib

let constraint_constants = Zeko_constants.constraint_constants

(* FIXME: Don't use Mina_compile_config.For_tests.t *)
let compile_config = Mina_compile_config.For_unit_tests.t

let compile_time_genesis =
  let consensus_constants =
    let protocol_constants : Genesis_constants.Protocol.t =
      { k = 1
      ; slots_per_epoch = 1000
      ; slots_per_sub_window = 1
      ; grace_period_slots = 1
      ; delta = 1
      ; genesis_state_timestamp = Int64.one
      }
    in
    Consensus.Constants.create ~constraint_constants ~protocol_constants
  in
  Mina_state.Genesis_protocol_state.t
    ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
    ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
    ~constraint_constants ~consensus_constants
    ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference

let rec rmrf path =
  match Sys.is_directory path with
  | true ->
      Sys.readdir path
      |> Array.iter ~f:(fun name -> rmrf (Filename.concat path name)) ;
      Sys.rmdir path
  | false ->
      Sys.remove path

let time ~logger label (d : 'a Deferred.t) =
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  [%log info] "%s: %s" label (Time.Span.to_string_hum @@ Time.diff stop start) ;
  return x

module Protocol_state = struct
  include Kvdb_base.Make_singleton (struct
    type t = Mina_state.Protocol_state.value [@@deriving yojson]

    let key = "archive_relay_state"
  end)

  let get kvdb =
    match get kvdb with
    | Some state ->
        state
    | None ->
        compile_time_genesis.data
end

type t =
  { logger : Logger.t
  ; archive_uri : Host_and_port.t Cli_lib.Flag.Types.with_name
  ; zeko_uri : Uri.t
  ; da_config : Da_layer.Client.Config.t
  ; mutable db : Ledger.Db.t
  }

let create ~logger ~archive_uri ~zeko_uri ~da_nodes ~ledger_cache =
  let db =
    Ledger.Db.create ~directory_name:ledger_cache
      ~depth:constraint_constants.ledger_depth ()
  in
  { logger
  ; archive_uri
  ; zeko_uri
  ; da_config = Da_layer.Client.Config.of_string_list da_nodes
  ; db
  }

let reset_ledger_cache t () =
  let directory_name =
    Option.value_exn ~message:"No ledger_cache directory"
    @@ Ledger.Db.get_directory t.db
  in
  Ledger.Db.close t.db ;

  Sys.readdir directory_name
  |> Array.iter ~f:(fun file_name ->
         rmrf (Filename.concat directory_name file_name) ) ;
  t.db <-
    Ledger.Db.create ~directory_name ~depth:constraint_constants.ledger_depth ()

let sync_archive (t : t) ~hash =
  let logger = t.logger in
  Da_layer.Client.map_diffs ~logger ~config:t.da_config
    ~depth:constraint_constants.ledger_depth
    ~source_ledger_hash:(`Specific (Ledger.Db.merkle_root t.db))
    ~target_ledger_hash:hash
    ~f:(fun ~current_chunk ~chunks_length diff ->
      let ledger = Ledger.of_database t.db in
      let changed_accounts =
        Da_layer.Diff.Stable.Latest.changed_accounts diff
      in
      let accounts_created =
        let aids =
          List.map changed_accounts ~f:snd |> List.map ~f:Account.identifier
        in
        Ledger.location_of_account_batch ledger aids
        |> List.filter_map ~f:(fun (aid, opt) ->
               if Option.is_some opt then Some aid else None )
      in
      List.iter changed_accounts ~f:(fun (index, account) ->
          Ledger.set_at_index_exn ledger index account ) ;
      Ledger.commit ledger ;
      match Da_layer.Diff.Stable.Latest.command_with_action_step_flags diff with
      | None ->
          return ()
      | Some (command, _) -> (
          let kvdb = Ledger.Db.zeko_kvdb t.db in
          let new_protocol_state, diff =
            Archive_lib.Diff.Builder.zeko_transaction_added
              ~constraint_constants ~accounts_created
              ~new_state_hash:(Ledger.merkle_root ledger)
              ~protocol_state:(Protocol_state.get kvdb) ~ledger
              ~txn:
                With_status.
                  { data = Mina_transaction.Transaction.Command command
                  ; status = Transaction_status.Applied
                  }
              ~dummy_fee_payer:Zeko_constants.inner_public_key
              ~timestamp:(Da_layer.Diff.Stable.Latest.timestamp diff)
          in
          Protocol_state.set kvdb ~data:new_protocol_state ;
          match%bind
            Archive_client.dispatch ~logger ~compile_config t.archive_uri
              (Archive_lib.Diff.Transition_frontier diff)
          with
          | Ok () ->
              [%log info]
                "Synced diff to archive with hash: %s, progress %.0f%%"
                (Ledger_hash.to_decimal_string @@ Ledger.merkle_root ledger)
                ( Float.of_int current_chunk /. Float.of_int chunks_length
                *. 100.0 ) ;
              return ()
          | Error e ->
              raise (Error.to_exn e) ) )
  >>| Result.map ~f:ignore

let fetch_current_ledger_hash ~zeko_uri () =
  let query =
    {|
      query {
        stateHashes {
          unprovedLedgerHash
        }
      }
    |}
  in
  let body =
    Yojson.Safe.to_string
    @@ `Assoc [ ("query", `String query); ("variables", `Assoc []) ]
  in
  let headers =
    List.fold ~init:(Cohttp.Header.init ())
      ~f:(fun acc (k, v) -> Cohttp.Header.add acc k v)
      [ ("Accept", "application/json"); ("Content-Type", "application/json") ]
  in
  let%bind.Deferred.Result response, body =
    Deferred.Or_error.try_with ~here:[%here] ~extract_exn:true (fun () ->
        Cohttp_async.Client.post ~headers
          ~body:(Cohttp_async.Body.of_string body)
          zeko_uri )
    |> Deferred.Result.map_error ~f:(fun e -> Error.to_string_hum e)
  in
  let%bind body_str = Cohttp_async.Body.to_string body in
  let%bind.Deferred.Result body_json =
    match
      Cohttp.Code.code_of_status (Cohttp_async.Response.status response)
    with
    | 200 ->
        Deferred.return (Ok (Yojson.Safe.from_string body_str))
    | code ->
        Deferred.return
          (Error (Printf.sprintf "Status code %d -- %s" code body_str))
  in
  let open Yojson.Safe.Util in
  match (member "errors" body_json, member "data" body_json) with
  | `Null, `Null ->
      return (Error "Empty response from graphql query")
  | error, `Null ->
      return (Error (Yojson.Safe.to_string error))
  | _, raw_json ->
      let unproved_ledger_hash =
        member "stateHashes" raw_json
        |> member "unprovedLedgerHash"
        |> to_string |> Ledger_hash.of_decimal_string
      in
      return (Ok unproved_ledger_hash)

let sync (t : t) () =
  let logger = t.logger in
  Thread_safe.block_on_async_exn (fun () ->
      let%bind ledger_hash =
        match%bind fetch_current_ledger_hash ~zeko_uri:t.zeko_uri () with
        | Ok hash ->
            [%log info] "Fetched ledger hash: %s"
              (Ledger_hash.to_decimal_string hash) ;
            return hash
        | Error e ->
            failwith e
      in
      time ~logger "Synced" (sync_archive t ~hash:ledger_hash) )

let rec run (t : t) ~sync_period () =
  let logger = t.logger in
  let () =
    match sync t () with
    | Ok () ->
        (* wait *)
        Thread_safe.block_on_async_exn (fun () ->
            after (Time.Span.of_sec sync_period) )
    | Error e ->
        (* ledger_hash_invalidated *)
        [%log error] "Error syncing: %s" (Error.to_string_hum e) ;
        [%log warn] "Invalidating ledger cache" ;
        reset_ledger_cache t ()
  in
  (* go again *)
  run t ~sync_period ()

let () =
  Command_unix.run
  @@ Command.basic ~summary:"Run archive adapter for zeko"
       (let%map_open.Command log_json = Flag.Log.json
        and log_level = Flag.Log.level
        and zeko_uri =
          flag "--zeko-uri" (required string) ~doc:"Zeko sequencer graphql uri"
        and da_nodes = flag "--da-node" (listed string) ~doc:"DA node uri"
        and archive_host =
          flag "--archive-host" (required string) ~doc:"Archive node host"
        and archive_port =
          flag "--archive-port" (required int) ~doc:"Archive node port"
        and sync_period =
          flag "--sync-period"
            (optional_with_default 30. float)
            ~doc:"Sync period"
        and ledger_cache =
          flag "--ledger-cache"
            (optional_with_default "ledger_cache" string)
            ~doc:"Ledger cache"
        in
        let logger = Logger.create () in
        Stdout_log.setup log_json log_level ;
        let zeko_uri = Uri.of_string zeko_uri in
        let archive_uri =
          Cli_lib.Flag.Types.
            { value = Host_and_port.create ~host:archive_host ~port:archive_port
            ; name = "archive-uri"
            }
        in

        let t = create ~logger ~archive_uri ~zeko_uri ~da_nodes ~ledger_cache in
        run t ~sync_period )
