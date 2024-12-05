open Async
open Core_kernel
open Mina_base
open Mina_lib
open Mina_ledger

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
  type t =
    { logger : Logger.t
    ; archive_uri : Host_and_port.t Cli_lib.Flag.Types.with_name
    ; zeko_uri : Uri.t
    ; da_config : Da_layer.Client.Config.t
    ; mutable ledger_cache : Ledger.Db.t
    ; mutable already_relayed_hashes : Ledger_hash.Set.t
    }

  let create ~logger ~archive_uri ~zeko_uri ~da_nodes ~ledger_cache =
    { logger
    ; archive_uri
    ; zeko_uri
    ; da_config = Da_layer.Client.Config.of_string_list da_nodes
    ; ledger_cache =
        Ledger.Db.create ~directory_name:ledger_cache
          ~depth:constraint_constants.ledger_depth ()
    ; already_relayed_hashes = Ledger_hash.Set.empty
    }

  let add_hash t hash =
    t.already_relayed_hashes <- Set.add t.already_relayed_hashes hash

  let has_been_relayed t hash = Set.mem t.already_relayed_hashes hash

  let reset_ledger_cache t () =
    let directory_name =
      Option.value_exn ~message:"No ledger_cache directory"
      @@ Ledger.Db.get_directory t.ledger_cache
    in
    Ledger.Db.close t.ledger_cache ;
    rmrf directory_name ;
    t.ledger_cache <-
      Ledger.Db.create ~directory_name ~depth:constraint_constants.ledger_depth
        ()
end

let sync_archive ~(state : State.t) ~hash =
  let logger = state.logger in
  let%bind.Deferred.Result lazy_chunks =
    Da_layer.Client.get_lazy_diffs_chunks ~logger ~config:state.da_config
      ~depth:constraint_constants.ledger_depth
      ~source_ledger_hash:(`Specific (Ledger.Db.merkle_root state.ledger_cache))
      ~target_ledger_hash:hash ()
  in
  let ledger = Ledger.of_database state.ledger_cache in
  let protocol_state = ref compile_time_genesis_state in
  Deferred.List.mapi ~how:`Sequential lazy_chunks ~f:(fun i lazy_chunk ->
      let%bind.Deferred.Result diffs = Lazy.force lazy_chunk in
      Deferred.List.iter ~how:`Sequential diffs ~f:(fun diff ->
          match
            Da_layer.Diff.Stable.Latest.command_with_action_step_flags diff
          with
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
                    (Mina_transaction_logic.Transaction_applied.new_accounts
                       txn_applied )
                  ~new_state_hash:(Ledger.merkle_root ledger)
                  ~protocol_state:!protocol_state ~ledger
                  ~txn:
                    (Mina_transaction_logic.Transaction_applied
                     .transaction_with_status txn_applied )
                  ~dummy_fee_payer:Zkapps_rollup.inner_public_key
                  ~timestamp:(Da_layer.Diff.Stable.Latest.timestamp diff)
              in
              protocol_state := new_protocol_state ;
              if State.has_been_relayed state (Ledger.merkle_root ledger) then
                return ()
              else
                (* FIXME: Don't use Mina_compile_config.For_tests.t *)
                let compile_config = Mina_compile_config.For_unit_tests.t in
                match%bind
                  Archive_client.dispatch ~compile_config ~logger
                    state.archive_uri (Archive_lib.Diff.Transition_frontier diff)
                with
                | Ok () ->
                    State.add_hash state (Ledger.merkle_root ledger) ;
                    return
                    @@ [%log info] "Synced diff to archive with hash: %s\n%!"
                         ( Ledger_hash.to_decimal_string
                         @@ Ledger.merkle_root ledger )
                | Error e ->
                    raise (Error.to_exn e) ) )
      >>| Result.return )
  >>| Result.all_unit

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

let sync ~(state : State.t) () =
  let logger = state.logger in
  Thread_safe.block_on_async_exn (fun () ->
      let%bind ledger_hash =
        match%bind fetch_current_ledger_hash ~zeko_uri:state.zeko_uri () with
        | Ok hash ->
            [%log info] "Fetched ledger hash: %s\n%!"
              (Ledger_hash.to_decimal_string hash) ;
            return hash
        | Error e ->
            failwith e
      in
      time ~logger "Synced" (sync_archive ~state ~hash:ledger_hash) )

let rec run ~(state : State.t) ~sync_period () =
  let logger = state.logger in
  let () =
    match sync ~state () with
    | Ok () ->
        (* wait *)
        Thread_safe.block_on_async_exn (fun () ->
            after (Time.Span.of_sec sync_period) )
    | Error e ->
        (* ledger_hash_invalidated *)
        [%log error] "Error syncing: %s\n%!" (Error.to_string_hum e) ;
        [%log warn] "Invalidating ledger cache" ;
        State.reset_ledger_cache state ()
  in
  (* go again *)
  run ~state ~sync_period ()

let () =
  Command_unix.run
  @@ Command.basic ~summary:"Run archive adapter for zeko"
       (let%map_open.Command zeko_uri =
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
        let zeko_uri = Uri.of_string zeko_uri in
        let archive_uri =
          Cli_lib.Flag.Types.
            { value = Host_and_port.create ~host:archive_host ~port:archive_port
            ; name = "archive-uri"
            }
        in

        let state =
          State.create ~logger ~archive_uri ~zeko_uri ~da_nodes ~ledger_cache
        in
        run ~state ~sync_period )
