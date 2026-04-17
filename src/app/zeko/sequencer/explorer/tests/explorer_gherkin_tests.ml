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

let graphql_data_exn service query field =
  find_assoc_exn field
    (find_assoc_exn "data" (graphql_response service query))

let pipe_read_exn reader =
  Thread_safe.block_on_async_exn (fun () ->
      Pipe.read reader
      >>| function
      | `Ok value ->
          value
      | `Eof ->
          failwith "Unexpected EOF" )

let pipe_read_string_exn reader =
  Thread_safe.block_on_async_exn (fun () ->
      Pipe.read reader
      >>| function
      | `Ok value ->
          value
      | `Eof ->
          failwith "Unexpected EOF" )

let add_job (service : Explorer_backfill_service.t)
    ?(status = Explorer_backfill_service.Queued)
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

let sse_request body =
  let headers = Cohttp.Header.of_list [ ("Content-Type", "application/json") ] in
  Cohttp.Request.make ~meth:`POST ~headers
    (Uri.of_string "http://localhost/graphql/stream"),
  body

let sse_reader_exn service body =
  let req, body = sse_request body in
  Thread_safe.block_on_async_exn (fun () ->
      match%bind Explorer_backfill_sse.execute_subscription service req body with
      | Error err ->
          failwith (Error.to_string_hum err)
      | Ok stream ->
          let reader, writer = Pipe.create () in
          don't_wait_for (Explorer_backfill_sse.write_stream writer stream) ;
          return reader )

let parse_sse_next_event_exn event =
  let prefix = "event: next\ndata: " in
  if not (String.is_prefix event ~prefix)
  then failwithf "Unexpected SSE event: %s" event ()
  else
    String.drop_prefix event (String.length prefix)
    |> String.chop_suffix_exn ~suffix:"\n\n"
    |> Yojson.Basic.from_string

let backfill_progress_event_exn event =
  find_assoc_exn "backfillProgress"
    (find_assoc_exn "data" (parse_sse_next_event_exn event))

let assert_basic_json_equal actual expected =
  if not (Yojson.Basic.equal actual expected)
  then
    failwithf "Expected %s but got %s" (Yojson.Basic.to_string expected)
      (Yojson.Basic.to_string actual) ()

let assert_safe_json_equal actual expected =
  if not (Yojson.Safe.equal actual expected)
  then
    failwithf "Expected %s but got %s" (Yojson.Safe.to_string expected)
      (Yojson.Safe.to_string actual) ()

let%test_unit "A genesis backfill marks only the first replayed diff as genesis" =
  Feature_parser.assert_scenario "backfill-api.feature"
    "A genesis backfill marks only the first replayed diff as genesis" ;
  if
    not
      (Poly.equal
         (Explorer_backfill_service.backfill_kind
            ~from_hash:Explorer_backfill_service.genesis_hash ~index:0 )
         Explorer_events.Transaction_kind.Genesis_replay )
  then failwith "expected the first genesis replay diff to be marked as genesis" ;
  if
    not
      (Poly.equal
         (Explorer_backfill_service.backfill_kind
            ~from_hash:Explorer_backfill_service.genesis_hash ~index:1 )
         Explorer_events.Transaction_kind.Sync_replay )
  then failwith "expected later genesis replay diffs to be marked as sync"

let%test_unit "The backfill mutation returns a failed job snapshot for invalid hashes" =
  Feature_parser.assert_scenario "backfill-api.feature"
    "The backfill mutation returns a failed job snapshot for invalid hashes" ;
  let service = test_service () in
  let backfill_job =
    graphql_data_exn service
      {|mutation { backfill(fromHash: "nonexistent", toHash: "D") { status error } }|}
      "backfill"
  in
  assert_basic_json_equal (find_assoc_exn "status" backfill_job)
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
    sse_reader_exn service
      (Yojson.Basic.to_string
         (`Assoc
           [ ( "query"
             , `String
                 (sprintf
                    {|subscription { backfillProgress(id: "%s") { id status diffsPublished error } }|}
                    job.id ) )
           ]) )
  in
  let initial =
    pipe_read_string_exn reader |> backfill_progress_event_exn
  in
  assert_basic_json_equal (find_assoc_exn "diffsPublished" initial) (`Int 0) ;
  Explorer_backfill_service.update_job job ~status:Running ~diffs_published:1
    ~started_at:Time.epoch () ;
  let first =
    pipe_read_string_exn reader |> backfill_progress_event_exn
  in
  assert_basic_json_equal (find_assoc_exn "diffsPublished" first) (`Int 1) ;
  Explorer_backfill_service.update_job job ~status:Completed
    ~diffs_published:2 ~finished_at:Time.epoch () ;
  let final_progress =
    pipe_read_string_exn reader |> backfill_progress_event_exn
  in
  assert_basic_json_equal (find_assoc_exn "diffsPublished" final_progress)
    (`Int 2) ;
  assert_basic_json_equal (find_assoc_exn "status" final_progress)
    (`String "completed")

let%test_unit "The backfill health query exposes the service instance" =
  Feature_parser.assert_scenario "backfill-api.feature"
    "The backfill health query exposes the service instance" ;
  let health =
    graphql_data_exn (test_service ())
      {|query { health { ok instanceId startedAt } }|}
      "health"
  in
  assert_basic_json_equal (find_assoc_exn "ok" health) (`Bool true) ;
  assert_basic_json_equal (find_assoc_exn "instanceId" health)
    (`String "instance-1") ;
  [%test_eq: bool]
    (match find_assoc_exn "startedAt" health with
    | `String _ ->
        true
    | _ ->
        false )
    true

let%test_unit "The backfill job query returns the current job snapshot" =
  Feature_parser.assert_scenario "backfill-api.feature"
    "The backfill job query returns the current job snapshot" ;
  let service = test_service () in
  let job = add_job service ~status:Explorer_backfill_service.Running () in
  let backfill_job =
    graphql_data_exn service
      (sprintf
         {|query { backfillJob(id: "%s") { id status diffsPublished } }|}
         job.id )
      "backfillJob"
  in
  assert_basic_json_equal (find_assoc_exn "id" backfill_job) (`String job.id) ;
  assert_basic_json_equal (find_assoc_exn "status" backfill_job)
    (`String "running") ;
  assert_basic_json_equal (find_assoc_exn "diffsPublished" backfill_job)
    (`Int 0)

let%test_unit "Backfill progress subscriptions stream GraphQL-SSE events" =
  Feature_parser.assert_scenario "backfill-api.feature"
    "Backfill progress subscriptions stream GraphQL-SSE events" ;
  let service = test_service () in
  let job = add_job service () in
  let reader =
    sse_reader_exn service
      (Yojson.Basic.to_string
         (`Assoc
           [ ( "query"
             , `String
                 (sprintf
                    {|subscription { backfillProgress(id: "%s") { id status diffsPublished error } }|}
                    job.id ) )
           ]) )
  in
  let first =
    pipe_read_string_exn reader |> backfill_progress_event_exn
  in
  assert_basic_json_equal (find_assoc_exn "diffsPublished" first) (`Int 0) ;
  Explorer_backfill_service.update_job job ~status:Running ~diffs_published:1
    ~started_at:Time.epoch () ;
  let running =
    pipe_read_string_exn reader |> backfill_progress_event_exn
  in
  assert_basic_json_equal (find_assoc_exn "diffsPublished" running) (`Int 1) ;
  Explorer_backfill_service.update_job job ~status:Completed
    ~diffs_published:2 ~finished_at:Time.epoch () ;
  let completed =
    pipe_read_string_exn reader |> backfill_progress_event_exn
  in
  assert_basic_json_equal (find_assoc_exn "status" completed)
    (`String "completed") ;
  [%test_eq: string]
    (pipe_read_string_exn reader)
    Explorer_backfill_sse.complete_event

let%test_unit "Transaction events include the replay kind, diff payload, and NATS dedup header" =
  Feature_parser.assert_scenario "event-contract.feature"
    "Transaction events include the replay kind, diff payload, and NATS dedup header" ;
  let target_ledger_hash = Ledger_hash.empty_hash in
  let diff = sample_diff () in
  let message =
    Explorer_events.build_transaction_message
      ~kind:Explorer_events.Transaction_kind.Sync_replay
      ~target_ledger_hash ~genesis:false ~diff
  in
  [%test_eq: string] message.subject Explorer_events.Subject.transactions ;
  assert_safe_json_equal (find_assoc_exn "kind" message.payload)
    (`String "sync_replay") ;
  assert_safe_json_equal (find_assoc_exn "target_ledger_hash" message.payload)
    (Ledger_hash.to_yojson target_ledger_hash) ;
  assert_safe_json_equal (find_assoc_exn "genesis" message.payload)
    (`Bool false) ;
  assert_safe_json_equal (find_assoc_exn "diff" message.payload)
    (Da_layer.Diff.Stable.V3.to_yojson diff) ;
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
  assert_safe_json_equal (find_assoc_exn "status" message.payload)
    (`String "proved") ;
  assert_safe_json_equal
    (find_assoc_exn "source_ledger_hash" message.payload)
    (Ledger_hash.to_yojson Ledger_hash.empty_hash) ;
  assert_safe_json_equal
    (find_assoc_exn "target_ledger_hash" message.payload)
    (Ledger_hash.to_yojson Ledger_hash.empty_hash)

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
  assert_safe_json_equal (find_assoc_exn "service" message.payload)
    (`String "sequencer-nats-publisher") ;
  assert_safe_json_equal (find_assoc_exn "instance_id" message.payload)
    (`String "instance-1") ;
  assert_safe_json_equal (find_assoc_exn "status" message.payload)
    (`String "ok") ;
  assert_safe_json_equal
    (find_assoc_exn "last_published_hash" message.payload)
    (Ledger_hash.to_yojson Ledger_hash.empty_hash) ;
  assert_safe_json_equal (find_assoc_exn "unproved_hash" message.payload)
    (Ledger_hash.to_yojson Ledger_hash.empty_hash)

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
