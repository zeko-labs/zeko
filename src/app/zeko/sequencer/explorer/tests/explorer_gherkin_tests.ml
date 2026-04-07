(* Executes minimal Gherkin acceptance tests for the explorer-facing behavior
   introduced in this PR: event payloads, backfill GraphQL/SSE behavior, and
   the sequencer replay classification hook. *)

open Core
open Async
open Sequencer_lib
open Mina_base

let find_assoc_exn key json =
  match json with
  | `Assoc fields ->
      List.Assoc.find_exn fields key ~equal:String.equal
  | _ ->
      failwith "Expected JSON object"

let find_header_exn headers key =
  List.Assoc.find_exn headers key ~equal:String.equal

let test_service ?(instance_id = "instance-1") () : Explorer_backfill_service.t =
  { logger = Logger.create ()
  ; da_config = Da_layer.Client.Config.of_string_list []
  ; nats_client = None
  ; jobs = String.Table.create ()
  ; instance_id
  ; started_at = Time.epoch
  }

let graphql_response service query =
  Thread_safe.block_on_async_exn (fun () ->
      match Graphql_parser.parse query with
      | Error err ->
          failwith err
      | Ok doc -> (
          Graphql_async.Schema.execute Explorer_backfill_graphql.schema service
            doc
          >>| function
          | Ok (`Response response) ->
              response
          | Ok (`Stream _) ->
              failwith "Expected a GraphQL response"
          | Error err ->
              failwith (Yojson.Basic.to_string err) ) )

let pipe_read_exn reader =
  Thread_safe.block_on_async_exn (fun () ->
      Pipe.read reader
      >>| function
      | `Ok value ->
          value
      | `Eof ->
          failwith "Unexpected EOF" )

let add_job service ?(status = Explorer_backfill_service.Queued)
    ?(diffs_published = 0) ?error ?started_at ?finished_at ?(id = "job-1")
    () =
  let job : Explorer_backfill_service.job =
    { id
    ; from_hash = Explorer_backfill_service.genesis_hash
    ; to_hash = Ledger_hash.empty_hash
    ; status
    ; diffs_published
    ; error
    ; created_at = Time.epoch
    ; started_at
    ; finished_at
    ; subscribers = ref []
    }
  in
  Hashtbl.set service.jobs ~key:job.id ~data:job ;
  job

let sample_diff ?(source_ledger_hash = Ledger_hash.empty_hash) () =
  Explorer_events.build_live_diff ~logger:(Logger.create ())
    ~diff:
      (Da_layer.Diff.create ~source_ledger_hash ~changed_accounts:[]
         ~command_with_action_step_flags:None )
    ~acc_set_root:Snark_params.Tick.Field.zero

let%test_unit "A genesis backfill marks only the first replayed diff as genesis" =
  Feature_parser.assert_scenario "backfill-api.feature"
    "A genesis backfill marks only the first replayed diff as genesis" ;
  [%test_eq: Explorer_events.Transaction_kind.t]
    (Explorer_backfill_service.backfill_kind
       ~from_hash:Explorer_backfill_service.genesis_hash ~index:0 )
    Explorer_events.Transaction_kind.Genesis_replay ;
  [%test_eq: Explorer_events.Transaction_kind.t]
    (Explorer_backfill_service.backfill_kind
       ~from_hash:Explorer_backfill_service.genesis_hash ~index:1 )
    Explorer_events.Transaction_kind.Sync_replay

let%test_unit "The backfill mutation returns a failed job snapshot for invalid hashes" =
  Feature_parser.assert_scenario "backfill-api.feature"
    "The backfill mutation returns a failed job snapshot for invalid hashes" ;
  let service = test_service () in
  let response =
    graphql_response service
      {|mutation { backfill(fromHash: "nonexistent", toHash: "D") { status error } }|}
  in
  let backfill_job =
    find_assoc_exn "backfill" (find_assoc_exn "data" response)
  in
  [%test_eq: Yojson.Basic.t]
    (find_assoc_exn "status" backfill_job)
    (`String "failed") ;
  [%test_eq: bool]
    (match find_assoc_exn "error" backfill_job with
    | `String error ->
        String.is_substring error ~substring:"Invalid ledger hash"
    | _ ->
        false )
    true

let%test_unit "Backfill progress subscriptions stream job updates" =
  Feature_parser.assert_scenario "backfill-api.feature"
    "Backfill progress subscriptions stream job updates" ;
  let service = test_service () in
  let job = add_job service () in
  let reader =
    match Explorer_backfill_service.subscribe_progress service ~id:job.id with
    | Ok reader ->
        reader
    | Error err ->
        failwith (Error.to_string_hum err)
  in
  let initial = pipe_read_exn reader in
  [%test_eq: int] initial.diffs_published 0 ;
  Explorer_backfill_service.update_job job ~status:Running ~diffs_published:1
    ~started_at:Time.epoch () ;
  let first = pipe_read_exn reader in
  [%test_eq: int] first.diffs_published 1 ;
  Explorer_backfill_service.update_job job ~status:Completed
    ~diffs_published:2 ~finished_at:Time.epoch () ;
  let final_progress = pipe_read_exn reader in
  [%test_eq: int] final_progress.diffs_published 2 ;
  [%test_eq: string] final_progress.status "completed"

let%test_unit "The backfill health query exposes the service instance" =
  Feature_parser.assert_scenario "backfill-api.feature"
    "The backfill health query exposes the service instance" ;
  let response =
    graphql_response (test_service ())
      {|query { health { instanceId startedAt } }|}
  in
  let health = find_assoc_exn "health" (find_assoc_exn "data" response) in
  [%test_eq: Yojson.Basic.t]
    (find_assoc_exn "instanceId" health)
    (`String "instance-1") ;
  [%test_eq: bool]
    (match find_assoc_exn "startedAt" health with
    | `String _ ->
        true
    | _ ->
        false )
    true

let%test_unit "Transaction events include the replay kind, diff payload, and NATS dedup header" =
  Feature_parser.assert_scenario "event-contract.feature"
    "Transaction events include the replay kind, diff payload, and NATS dedup header" ;
  let target_ledger_hash = Ledger_hash.empty_hash in
  let message =
    Explorer_events.build_transaction_message
      ~kind:Explorer_events.Transaction_kind.Sync_replay
      ~target_ledger_hash ~genesis:false ~diff:(sample_diff ())
  in
  [%test_eq: string] message.subject Explorer_events.Subject.transactions ;
  ignore (find_assoc_exn "kind" message.payload : Yojson.Safe.t) ;
  ignore (find_assoc_exn "target_ledger_hash" message.payload : Yojson.Safe.t) ;
  ignore (find_assoc_exn "diff" message.payload : Yojson.Safe.t) ;
  [%test_eq: string]
    (find_header_exn message.headers "Nats-Msg-Id")
    (Ledger_hash.to_decimal_string target_ledger_hash)

let%test_unit "Finality events include status and ledger hashes" =
  Feature_parser.assert_scenario "event-contract.feature"
    "Finality events include status and ledger hashes" ;
  let message =
    Explorer_events.build_finality_message ~logger:(Logger.create ())
      ~status:Explorer_events.Finality_status.Proved
      ~source_ledger_hash:Ledger_hash.empty_hash
      ~target_ledger_hash:Ledger_hash.empty_hash
  in
  [%test_eq: string] message.subject Explorer_events.Subject.finality ;
  ignore (find_assoc_exn "status" message.payload : Yojson.Safe.t) ;
  ignore
    (find_assoc_exn "source_ledger_hash" message.payload : Yojson.Safe.t) ;
  ignore
    (find_assoc_exn "target_ledger_hash" message.payload : Yojson.Safe.t)

let%test_unit "Health events include the service identity and publishing state" =
  Feature_parser.assert_scenario "event-contract.feature"
    "Health events include the service identity and publishing state" ;
  let message =
    Explorer_events.build_health_message ~logger:(Logger.create ())
      ~service:"sequencer-nats-publisher" ~instance_id:"instance-1"
      ~status:"ok" ~last_published_hash:Ledger_hash.empty_hash
      ~unproved_hash:Ledger_hash.empty_hash ()
  in
  [%test_eq: string] message.subject Explorer_events.Subject.health ;
  ignore (find_assoc_exn "service" message.payload : Yojson.Safe.t) ;
  ignore (find_assoc_exn "instance_id" message.payload : Yojson.Safe.t) ;
  ignore
    (find_assoc_exn "last_published_hash" message.payload : Yojson.Safe.t) ;
  ignore (find_assoc_exn "unproved_hash" message.payload : Yojson.Safe.t)

let%test_unit "Sync replay from genesis marks the very first diff as genesis" =
  Feature_parser.assert_scenario "sequencer-hooks.feature"
    "Sync replay from genesis marks the very first diff as genesis" ;
  [%test_eq: bool]
    (Zeko_sequencer.Sequencer.replay_genesis_flag ~source:`Genesis
       ~current_chunk:0 ~current_diff:0 )
    true

let%test_unit "Sync replay from a checkpoint never re-labels diffs as genesis" =
  Feature_parser.assert_scenario "sequencer-hooks.feature"
    "Sync replay from a checkpoint never re-labels diffs as genesis" ;
  [%test_eq: bool]
    (Zeko_sequencer.Sequencer.replay_genesis_flag
       ~source:(`Specific Ledger_hash.empty_hash)
       ~current_chunk:0 ~current_diff:0 )
    false
