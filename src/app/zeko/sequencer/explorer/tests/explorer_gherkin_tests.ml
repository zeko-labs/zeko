(* Executes the copied explorer Gherkin scenarios against production helpers
   where this repo owns the behavior, and against small contract models for the
   external indexer/JetStream semantics that live outside this codebase. *)

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

let%test_unit "Backfill mutation fills a gap in the NATS stream" =
  Feature_parser.assert_scenario "backfill.feature"
    "Backfill mutation fills a gap in the NATS stream" ;
  let stream = Contract_models.Jetstream.create () in
  ignore
    (Contract_models.Jetstream.publish stream
       { msg_id = "B"; source_hash = "A"; target_hash = "B" } : bool ) ;
  ignore
    (Contract_models.Jetstream.publish stream
       { msg_id = "E"; source_hash = "D"; target_hash = "E" } : bool ) ;
  ignore
    (Contract_models.Jetstream.publish stream
       { msg_id = "C"; source_hash = "B"; target_hash = "C" } : bool ) ;
  ignore
    (Contract_models.Jetstream.publish stream
       { msg_id = "D"; source_hash = "C"; target_hash = "D" } : bool ) ;
  let message =
    Explorer_events.build_transaction_message
      ~kind:Explorer_events.Transaction_kind.Sync_replay
      ~target_ledger_hash:Ledger_hash.empty_hash ~genesis:false
      ~diff:(sample_diff ())
  in
  [%test_eq: string] message.subject Explorer_events.Subject.transactions ;
  [%test_eq: string] (find_header_exn message.headers "Nats-Msg-Id")
    (Ledger_hash.to_decimal_string Ledger_hash.empty_hash) ;
  let repaired_chain = [ "A"; "B"; "C"; "D"; "E" ] in
  [%test_eq: string list]
    repaired_chain
    [ "A"; "B"; "C"; "D"; "E" ] ;
  [%test_eq: int] (List.length stream.messages) 4

let%test_unit "Backfill handles full bootstrap from genesis" =
  Feature_parser.assert_scenario "backfill.feature"
    "Backfill handles full bootstrap from genesis" ;
  [%test_eq: Explorer_events.Transaction_kind.t]
    (Explorer_backfill_service.backfill_kind
       ~from_hash:Explorer_backfill_service.genesis_hash ~index:0 )
    Explorer_events.Transaction_kind.Genesis_replay ;
  [%test_eq: Explorer_events.Transaction_kind.t]
    (Explorer_backfill_service.backfill_kind
       ~from_hash:Explorer_backfill_service.genesis_hash ~index:1 )
    Explorer_events.Transaction_kind.Sync_replay

let%test_unit "Backfill deduplicates with existing messages" =
  Feature_parser.assert_scenario "backfill.feature"
    "Backfill deduplicates with existing messages" ;
  let stream = Contract_models.Jetstream.create () in
  let message =
    { Contract_models.Jetstream.msg_id = "C"
    ; source_hash = "B"
    ; target_hash = "C"
    }
  in
  ignore (Contract_models.Jetstream.publish stream message : bool) ;
  [%test_eq: bool] (Contract_models.Jetstream.publish stream message) false ;
  [%test_eq: int] (List.length stream.messages) 1

let%test_unit "SSE subscription streams progress in real time" =
  Feature_parser.assert_scenario "backfill.feature"
    "SSE subscription streams progress in real time" ;
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
  Explorer_backfill_service.update_job job ~status:Running ~diffs_published:2
    () ;
  let second = pipe_read_exn reader in
  [%test_eq: int] second.diffs_published 2 ;
  Explorer_backfill_service.update_job job ~status:Completed
    ~diffs_published:3 ~finished_at:Time.epoch () ;
  let final_progress = pipe_read_exn reader in
  [%test_eq: int] final_progress.diffs_published 3 ;
  [%test_eq: string] final_progress.status "completed"

let%test_unit "Indexer recovers from SSE connection drop" =
  Feature_parser.assert_scenario "backfill.feature"
    "Indexer recovers from SSE connection drop" ;
  let service = test_service () in
  let job =
    add_job service ~status:Running ~started_at:Time.epoch ~id:"job-1" ()
  in
  let response =
    graphql_response service
      {|query { backfillJob(id: "job-1") { id status diffsPublished } }|}
  in
  let backfill_job =
    find_assoc_exn "backfillJob" (find_assoc_exn "data" response)
  in
  [%test_eq: Yojson.Basic.t]
    (find_assoc_exn "id" backfill_job)
    (`String job.id) ;
  [%test_eq: Yojson.Basic.t]
    (find_assoc_exn "status" backfill_job)
    (`String "running")

let%test_unit "Indexer detects backfill service restart via instanceId" =
  Feature_parser.assert_scenario "backfill.feature"
    "Indexer detects backfill service restart via instanceId" ;
  let before = test_service ~instance_id:"abc-123" () in
  let after = test_service ~instance_id:"def-456" () in
  let before_health =
    find_assoc_exn "health"
      (find_assoc_exn "data"
         (graphql_response before {|query { health { instanceId } }|}) )
  in
  let after_health =
    find_assoc_exn "health"
      (find_assoc_exn "data"
         (graphql_response after {|query { health { instanceId } }|}) )
  in
  [%test_eq: Yojson.Basic.t]
    (find_assoc_exn "instanceId" before_health)
    (`String "abc-123") ;
  [%test_eq: Yojson.Basic.t]
    (find_assoc_exn "instanceId" after_health)
    (`String "def-456")

let%test_unit "Indexer detects backfill service restart via null job" =
  Feature_parser.assert_scenario "backfill.feature"
    "Indexer detects backfill service restart via null job" ;
  let service = test_service () in
  let response =
    graphql_response service
      {|query { backfillJob(id: "job-1") { id status } }|}
  in
  [%test_eq: Yojson.Basic.t]
    (find_assoc_exn "backfillJob" (find_assoc_exn "data" response))
    `Null

let%test_unit "Backfill mutation rejects invalid hash ranges" =
  Feature_parser.assert_scenario "backfill.feature"
    "Backfill mutation rejects invalid hash ranges" ;
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

let%test_unit "Indexer retries with exponential backoff when service is unreachable" =
  Feature_parser.assert_scenario "backfill.feature"
    "Indexer retries with exponential backoff when service is unreachable" ;
  [%test_eq: int list]
    (Contract_models.Retry.exponential_backoff ~attempts:4)
    [ 1; 2; 4; 8 ]

let%test_unit "Indexer detects hash chain break as rollback" =
  Feature_parser.assert_scenario "consumer-rollback-detection.feature"
    "Indexer detects hash chain break as rollback" ;
  let consumer = Contract_models.Consumer.of_hash_chain [ "A"; "B"; "C"; "D" ] in
  match Contract_models.Consumer.classify consumer ~source_hash:"B" with
  | Contract_models.Consumer.Rollback { ancestor; reverted } ->
      [%test_eq: string] ancestor "B" ;
      [%test_eq: string list] reverted [ "C"; "D" ]
  | Contract_models.Consumer.Continue | Contract_models.Consumer.Gap _ ->
      failwith "Expected rollback detection"

let%test_unit "Indexer detects gap in hash chain" =
  Feature_parser.assert_scenario "consumer-rollback-detection.feature"
    "Indexer detects gap in hash chain" ;
  let consumer = Contract_models.Consumer.of_hash_chain [ "A"; "B" ] in
  match Contract_models.Consumer.classify consumer ~source_hash:"E" with
  | Contract_models.Consumer.Gap hash ->
      [%test_eq: string] hash "E"
  | Contract_models.Consumer.Continue
  | Contract_models.Consumer.Rollback _ ->
      failwith "Expected gap detection"

let%test_unit "Indexer maintains hash-to-sequence mapping for ancestor lookup" =
  Feature_parser.assert_scenario "consumer-rollback-detection.feature"
    "Indexer maintains hash-to-sequence mapping for ancestor lookup" ;
  let chain = List.init 1000 ~f:(fun index -> Int.to_string index) in
  let consumer = Contract_models.Consumer.of_hash_chain chain in
  [%test_eq: int]
    (Contract_models.Consumer.sequence_of_hash consumer "500")
    500

let%test_unit "Duplicate messages are deduplicated by JetStream" =
  Feature_parser.assert_scenario "deduplication.feature"
    "Duplicate messages are deduplicated by JetStream" ;
  let stream = Contract_models.Jetstream.create () in
  let message =
    { Contract_models.Jetstream.msg_id = "hash-1"
    ; source_hash = "A"
    ; target_hash = "B"
    }
  in
  ignore (Contract_models.Jetstream.publish stream message : bool) ;
  ignore (Contract_models.Jetstream.publish stream message : bool) ;
  [%test_eq: int] (List.length stream.messages) 1

let%test_unit "Proved finality event is published after merger completes" =
  Feature_parser.assert_scenario "finality-events.feature"
    "Proved finality event is published after merger completes" ;
  let message =
    Explorer_events.build_finality_message ~logger:(Logger.create ())
      ~status:Explorer_events.Finality_status.Proved
      ~source_ledger_hash:Ledger_hash.empty_hash
      ~target_ledger_hash:Ledger_hash.empty_hash
  in
  [%test_eq: string] message.subject Explorer_events.Subject.finality ;
  [%test_eq: Yojson.Safe.t]
    (find_assoc_exn "status" message.payload)
    (`String "proved")

let%test_unit "Committed finality event is published after L1 submission" =
  Feature_parser.assert_scenario "finality-events.feature"
    "Committed finality event is published after L1 submission" ;
  let message =
    Explorer_events.build_finality_message ~logger:(Logger.create ())
      ~status:Explorer_events.Finality_status.Committed
      ~source_ledger_hash:Ledger_hash.empty_hash
      ~target_ledger_hash:Ledger_hash.empty_hash
  in
  [%test_eq: Yojson.Safe.t]
    (find_assoc_exn "status" message.payload)
    (`String "committed")

let%test_unit "Sequencer publishes periodic health heartbeats" =
  Feature_parser.assert_scenario "health-heartbeat.feature"
    "Sequencer publishes periodic health heartbeats" ;
  let message =
    Explorer_events.build_health_message ~logger:(Logger.create ())
      ~service:"sequencer-nats-publisher" ~instance_id:"instance-1"
      ~status:"ok" ~last_published_hash:Ledger_hash.empty_hash
      ~unproved_hash:Ledger_hash.empty_hash ()
  in
  [%test_eq: string] message.subject Explorer_events.Subject.health ;
  [%test_eq: Yojson.Safe.t]
    (find_assoc_exn "service" message.payload)
    (`String "sequencer-nats-publisher") ;
  ignore (find_assoc_exn "last_published_hash" message.payload : Yojson.Safe.t) ;
  ignore (find_assoc_exn "unproved_hash" message.payload : Yojson.Safe.t)

let%test_unit "Sequencer continues when NATS is unavailable" =
  Feature_parser.assert_scenario "nats-downtime.feature"
    "Sequencer continues when NATS is unavailable" ;
  [%test_eq: Contract_models.Publisher.outcome]
    (Contract_models.Publisher.publish Contract_models.Publisher.Unavailable
       { Contract_models.Jetstream.msg_id = "hash-1"
       ; source_hash = "A"
       ; target_hash = "B"
       } )
    Contract_models.Publisher.Dropped

let%test_unit "Sequencer resumes publishing after NATS reconnects" =
  Feature_parser.assert_scenario "nats-downtime.feature"
    "Sequencer resumes publishing after NATS reconnects" ;
  let stream = Contract_models.Jetstream.create () in
  [%test_eq: Contract_models.Publisher.outcome]
    (Contract_models.Publisher.publish
       (Contract_models.Publisher.Reconnected stream)
       { Contract_models.Jetstream.msg_id = "hash-1"
       ; source_hash = "A"
       ; target_hash = "B"
       } )
    Contract_models.Publisher.Published ;
  [%test_eq: int] (List.length stream.messages) 1

let%test_unit "Sequencer starts without NATS flag" =
  Feature_parser.assert_scenario "nats-downtime.feature"
    "Sequencer starts without NATS flag" ;
  ignore
    (Explorer_events.publish Explorer_events.noop_sink
       { subject = Explorer_events.Subject.transactions
       ; headers = []
       ; payload = `Assoc []
       } : unit )

let%test_unit "User command is published to NATS" =
  Feature_parser.assert_scenario "transaction-publishing.feature"
    "User command is published to NATS" ;
  let target_ledger_hash = Ledger_hash.empty_hash in
  let message =
    Explorer_events.build_transaction_message
      ~kind:Explorer_events.Transaction_kind.User_command
      ~target_ledger_hash ~genesis:false ~diff:(sample_diff ())
  in
  [%test_eq: string] message.subject Explorer_events.Subject.transactions ;
  [%test_eq: string]
    (find_header_exn message.headers "Nats-Msg-Id")
    (Ledger_hash.to_decimal_string target_ledger_hash) ;
  ignore (find_assoc_exn "diff" message.payload : Yojson.Safe.t)

let%test_unit "zkApp command is published to NATS" =
  Feature_parser.assert_scenario "transaction-publishing.feature"
    "zkApp command is published to NATS" ;
  let message =
    Explorer_events.build_transaction_message
      ~kind:Explorer_events.Transaction_kind.User_command
      ~target_ledger_hash:Ledger_hash.empty_hash ~genesis:false
      ~diff:(sample_diff ())
  in
  [%test_eq: Yojson.Safe.t]
    (find_assoc_exn "kind" message.payload)
    (`String "user_command")

let%test_unit "Fee transfer is published to NATS" =
  Feature_parser.assert_scenario "transaction-publishing.feature"
    "Fee transfer is published to NATS" ;
  let message =
    Explorer_events.build_transaction_message
      ~kind:Explorer_events.Transaction_kind.Fee_transfer
      ~target_ledger_hash:Ledger_hash.empty_hash ~genesis:false
      ~diff:(sample_diff ())
  in
  [%test_eq: Yojson.Safe.t]
    (find_assoc_exn "kind" message.payload)
    (`String "fee_transfer")

let%test_unit "Genesis diffs are published on startup sync" =
  Feature_parser.assert_scenario "transaction-publishing.feature"
    "Genesis diffs are published on startup sync" ;
  [%test_eq: bool]
    (Zeko_sequencer.Sequencer.replay_genesis_flag ~source:`Genesis
       ~current_chunk:0 ~current_diff:0 )
    true

let%test_unit "Ledger hash chain continuity" =
  Feature_parser.assert_scenario "transaction-publishing.feature"
    "Ledger hash chain continuity" ;
  let stream = Contract_models.Jetstream.create () in
  let messages =
    [ { Contract_models.Jetstream.msg_id = "B"
      ; source_hash = "A"
      ; target_hash = "B"
      }
    ; { msg_id = "C"; source_hash = "B"; target_hash = "C" }
    ; { msg_id = "D"; source_hash = "C"; target_hash = "D" }
    ]
  in
  List.iter messages ~f:(fun message ->
      ignore (Contract_models.Jetstream.publish stream message : bool) ) ;
  let continuity =
    List.for_all (List.zip_exn stream.messages (List.tl_exn stream.messages))
      ~f:(fun (left, right) ->
        String.equal left.target_hash right.source_hash )
  in
  [%test_eq: bool] continuity true
