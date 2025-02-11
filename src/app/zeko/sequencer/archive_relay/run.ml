open Async
open Core_kernel
open Mina_base
open Mina_lib
open Mina_ledger
open Cli_lib

let constraint_constants = Genesis_constants.Compiled.constraint_constants

let rec rmrf path =
  match Sys.is_directory path with
  | true ->
      Sys.readdir path
      |> Array.iter ~f:(fun name -> rmrf (Filename.concat path name)) ;
      Sys.rmdir path
  | false ->
      Sys.remove path

let compile_time_genesis_state =
  let genesis_constants = Genesis_constants.Compiled.genesis_constants in
  let consensus_constants =
    Consensus.Constants.create ~constraint_constants
      ~protocol_constants:genesis_constants.protocol
  in
  let compile_time_genesis =
    Mina_state.Genesis_protocol_state.t
      ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
      ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
      ~constraint_constants ~consensus_constants
      ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference
  in
  compile_time_genesis.data

let time ~logger label (d : 'a Deferred.t) =
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  [%log info] "%s: %s\n%!" label
    (Time.Span.to_string_hum @@ Time.diff stop start) ;
  return x

module State = struct
  type _t = { mutable protocol_state : Mina_state.Protocol_state.value }
  [@@deriving yojson]

  type t = _t

  module Db = Kvdb_base.Make_singleton (struct
    type t = _t [@@deriving yojson]

    let key = "archive_relay_state"
  end)

  let save kvdb t = Db.set ~data:t kvdb

  let load kvdb =
    match Db.get kvdb with
    | Some state ->
        state
    | None ->
        { protocol_state = compile_time_genesis_state }

  let set_protocol_state t kvdb protocol_state =
    t.protocol_state <- protocol_state ;
    save kvdb t
end

type t =
  { logger : Logger.t
  ; archive_uri : Host_and_port.t Cli_lib.Flag.Types.with_name
  ; zeko_uri : Uri.t
  ; da_config : Da_layer.Client.Config.t
  ; state : State.t
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
  ; state = State.load (Ledger.Db.zeko_kvdb db)
  ; db
  }

let reset_ledger_cache t () =
  let directory_name =
    Option.value_exn ~message:"No ledger_cache directory"
    @@ Ledger.Db.get_directory t.db
  in
  Ledger.Db.close t.db ;
  rmrf directory_name ;
  t.db <-
    Ledger.Db.create ~directory_name ~depth:constraint_constants.ledger_depth ()

let sync_archive (t : t) ~hash =
  let logger = t.logger in
  Da_layer.Client.map_diffs ~logger ~config:t.da_config
    ~depth:constraint_constants.ledger_depth
    ~source_ledger_hash:(`Specific (Ledger.Db.merkle_root t.db))
    ~target_ledger_hash:hash ~print_progress:true
    ~f:(fun diff ->
      let ledger = Ledger.of_database t.db in
      match Da_layer.Diff.Stable.Latest.command_with_action_step_flags diff with
      | None ->
          (* Apply accounts diff *)
          let changed_accounts =
            Da_layer.Diff.Stable.Latest.changed_accounts diff
          in
          List.iter changed_accounts ~f:(fun (index, account) ->
              Ledger.set_at_index_exn ledger index account ) ;
          Ledger.commit ledger ;
          return ()
      | Some (command, _) -> (
          let txn_applied =
            Or_error.ok_exn
            @@ Result.( >>= )
                 (Ledger.apply_transaction_first_pass ~constraint_constants
                    ~global_slot:Mina_numbers.Global_slot_since_genesis.zero
                    ~txn_state_view:
                      Mina_state.Protocol_state.(
                        Body.view @@ body compile_time_genesis_state)
                    ledger (Command command) )
                 (Ledger.apply_transaction_second_pass ledger)
          in
          Ledger.commit ledger ;
          let new_protocol_state, diff =
            Archive_lib.Diff.Builder.zeko_transaction_added
              ~constraint_constants
              ~accounts_created:
                (Ledger.Transaction_applied.new_accounts txn_applied)
              ~new_state_hash:(Ledger.merkle_root ledger)
              ~protocol_state:t.state.protocol_state ~ledger
              ~txn:(Ledger.Transaction_applied.transaction txn_applied)
              ~dummy_fee_payer:Zkapps_rollup.inner_public_key
              ~timestamp:(Da_layer.Diff.Stable.Latest.timestamp diff)
          in
          State.set_protocol_state t.state (Ledger.Db.zeko_kvdb t.db)
            new_protocol_state ;
          match%bind
            Archive_client.dispatch ~logger t.archive_uri
              (Archive_lib.Diff.Transition_frontier diff)
          with
          | Ok () ->
              return
              @@ [%log info] "Synced diff to archive with hash: %s\n%!"
                   (Ledger_hash.to_decimal_string @@ Ledger.merkle_root ledger)
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
            [%log info] "Fetched ledger hash: %s\n%!"
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
        [%log error] "Error syncing: %s\n%!" (Error.to_string_hum e) ;
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
            (optional_with_default 60. float)
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
