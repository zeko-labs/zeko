open Async
open Core_kernel
open Mina_base
open Mina_lib
open Mina_ledger
module Ws = Websocket.Make (Cohttp_async.Io)

let constraint_constants = Genesis_constants.Constraint_constants.compiled

let compile_time_genesis_state =
  let genesis_constants = Genesis_constants.compiled in
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

let time label (d : 'a Deferred.t) =
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  printf "%s: %s\n%!" label (Time.Span.to_string_hum @@ Time.diff stop start) ;
  return x

module Cache = struct
  type t = Da_layer.Diff.With_timestamp.t Ledger_hash.Map.t

  let empty = Ledger_hash.Map.empty

  let get (t : t) hash = Ledger_hash.Map.find t hash

  let add (t : t) hash diff = Ledger_hash.Map.set t ~key:hash ~data:diff
end

module State = struct
  type t =
    { logger : Logger.t
    ; archive_uri : Host_and_port.t Cli_lib.Flag.Types.with_name
    ; zeko_uri : Uri.t
    ; da_config : Da_layer.Client.Config.t
    ; mutable cache : Cache.t
    ; mutable already_relayed_hashes : Ledger_hash.Set.t
    }

  let create ~logger ~archive_uri ~zeko_uri ~da_nodes =
    { logger
    ; archive_uri
    ; zeko_uri
    ; da_config = Da_layer.Client.Config.of_string_list da_nodes
    ; cache = Cache.empty
    ; already_relayed_hashes = Ledger_hash.Set.empty
    }

  let add_hash t hash =
    t.already_relayed_hashes <- Set.add t.already_relayed_hashes hash

  let has_been_relayed t hash = Set.mem t.already_relayed_hashes hash
end

let fetch_diff ~(state : State.t) hash =
  match Cache.get state.cache hash with
  | Some diff ->
      return diff
  | None -> (
      match%bind
        Da_layer.Client.get_diff ~logger:state.logger ~config:state.da_config
          ~ledger_hash:hash
      with
      | Ok diff ->
          state.cache <- Cache.add state.cache hash diff ;
          return diff
      | Error e ->
          raise (Error.to_exn e) )

module Graphql_ws = struct
  type client_message =
    | Gql_connection_init
    | Gql_start of
        { id : int
        ; query : string
        ; variables : Yojson.Safe.t
        ; operation_name : string option
        }

  type server_message =
    | Gql_connection_ack
    | Gql_data of { data : Yojson.Safe.t }
    | Gql_unknown of string

  let client_message_to_string =
    Fn.compose Yojson.Safe.to_string (function
      | Gql_connection_init ->
          `Assoc [ ("type", `String "connection_init"); ("payload", `Assoc []) ]
      | Gql_start { id; query; variables; operation_name } ->
          `Assoc
            [ ("type", `String "start")
            ; ("id", `String (Int.to_string id))
            ; ( "payload"
              , `Assoc
                  [ ("query", `String query)
                  ; ("variables", variables)
                  ; ( "operationName"
                    , Option.value ~default:`Null
                      @@ Option.map ~f:(fun s -> `String s) operation_name )
                  ] )
            ] )

  let server_message_of_string s =
    let open Yojson.Safe.Util in
    let json = Yojson.Safe.from_string s in
    let type_ = json |> member "type" |> to_string in
    match type_ with
    | "connection_ack" ->
        Gql_connection_ack
    | "data" ->
        Gql_data { data = json |> member "payload" |> member "data" }
    | _ ->
        Gql_unknown s
end

let sync_archive ~(state : State.t) ~hash =
  let logger = state.logger in
  let%bind.Deferred.Result chain =
    Da_layer.Client.get_ledger_hashes_chain ~logger ~config:state.da_config
      ~depth:constraint_constants.ledger_depth ~target_ledger_hash:hash
  in
  let%bind diffs = Deferred.List.map chain ~f:(fetch_diff ~state) in
  Ledger.with_ledger ~depth:constraint_constants.ledger_depth ~f:(fun ledger ->
      let protocol_state = ref compile_time_genesis_state in
      Deferred.List.iter diffs ~f:(fun (diff, diff_timestamp) ->
          match Da_layer.Diff.command_with_action_step_flags diff with
          | None ->
              (* Apply accounts diff *)
              let changed_accounts = Da_layer.Diff.changed_accounts diff in
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
                  ~protocol_state:!protocol_state ~ledger
                  ~txn:(Ledger.Transaction_applied.transaction txn_applied)
                  ~dummy_fee_payer:Zkapps_rollup.inner_public_key
                  ~timestamp:diff_timestamp
              in
              protocol_state := new_protocol_state ;
              if State.has_been_relayed state (Ledger.merkle_root ledger) then
                return ()
              else
                match%bind
                  Archive_client.dispatch ~logger state.archive_uri
                    (Archive_lib.Diff.Transition_frontier diff)
                with
                | Ok () ->
                    State.add_hash state (Ledger.merkle_root ledger) ;
                    return
                    @@ printf "Synced diff to archive with hash: %s\n%!"
                         ( Ledger_hash.to_decimal_string
                         @@ Ledger.merkle_root ledger )
                | Error e ->
                    raise (Error.to_exn e) ) ) )
  |> Deferred.map ~f:Result.return

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
  Thread_safe.block_on_async_exn (fun () ->
      let%bind ledger_hash =
        match%bind fetch_current_ledger_hash ~zeko_uri:state.zeko_uri () with
        | Ok hash ->
            printf "Fetched ledger hash: %s\n%!"
              (Ledger_hash.to_decimal_string hash) ;
            return hash
        | Error e ->
            failwith e
      in
      time "Synced" (sync_archive ~state ~hash:ledger_hash) )

let rec run ~(state : State.t) ~sync_period () =
  let () =
    match sync ~state () with
    | Ok () ->
        ()
    | Error e ->
        print_endline (Error.to_string_hum e)
  in
  Thread_safe.block_on_async_exn (fun () ->
      after (Time.Span.of_sec sync_period) ) ;
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
        in
        let logger = Logger.create () in
        let zeko_uri = Uri.of_string zeko_uri in
        let archive_uri =
          Cli_lib.Flag.Types.
            { value = Host_and_port.create ~host:archive_host ~port:archive_port
            ; name = "archive-uri"
            }
        in

        let state = State.create ~logger ~archive_uri ~zeko_uri ~da_nodes in
        run ~state ~sync_period )
