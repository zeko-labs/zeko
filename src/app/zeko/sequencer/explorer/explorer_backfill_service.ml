(* Runs the standalone explorer backfill service: tracks in-memory backfill
   jobs, republishes DA diffs to NATS, and exposes GraphQL query/mutation/
   subscription endpoints for job control and progress streaming. *)

open Core
open Async
open Sequencer_lib
open Mina_base

type job_status =
  | Queued
  | Running
  | Completed
  | Failed

type job_snapshot =
  { id : string
  ; from_hash : string
  ; to_hash : string
  ; status : string
  ; diffs_published : int
  ; error : string option
  ; created_at : string
  ; started_at : string option
  ; finished_at : string option
  }

type progress_snapshot =
  { id : string
  ; status : string
  ; diffs_published : int
  ; error : string option
  }

type subscriber =
  { writer : progress_snapshot Pipe.Writer.t
  ; mutable pending : progress_snapshot option
  ; mutable draining : bool
  ; mutable close_after_flush : bool
  }

type health_snapshot =
  { ok : bool
  ; instance_id : string
  ; started_at : string
  }

type job =
  { id : string
  ; from_hash : Ledger_hash.t
  ; to_hash : Ledger_hash.t
  ; mutable status : job_status
  ; mutable diffs_published : int
  ; mutable error : string option
  ; created_at : Time.t
  ; mutable started_at : Time.t option
  ; mutable finished_at : Time.t option
  ; subscribers : subscriber list ref
  }

type t =
  { logger : Logger.t
  ; da_config : Da_layer.Client.Config.t
  ; nats_client : Nats_client_async.client option
  ; jobs : job String.Table.t
  ; instance_id : string
  ; started_at : Time.t
  }

let constraint_constants = Zeko_constants.constraint_constants

let timestamp_string time =
  Time.to_string_iso8601_basic time ~zone:Time.Zone.utc

let string_of_job_status = function
  | Queued ->
      "queued"
  | Running ->
      "running"
  | Completed ->
      "completed"
  | Failed ->
      "failed"

let is_terminal = function
  | Completed | Failed ->
      true
  | Queued | Running ->
      false

let genesis_hash =
  Da_layer.Diff.empty_ledger_hash ~depth:constraint_constants.ledger_depth

let is_genesis_hash ledger_hash = Ledger_hash.equal ledger_hash genesis_hash

let snapshot job =
  { id = job.id
  ; from_hash = Ledger_hash.to_decimal_string job.from_hash
  ; to_hash = Ledger_hash.to_decimal_string job.to_hash
  ; status = string_of_job_status job.status
  ; diffs_published = job.diffs_published
  ; error = job.error
  ; created_at = timestamp_string job.created_at
  ; started_at = Option.map job.started_at ~f:timestamp_string
  ; finished_at = Option.map job.finished_at ~f:timestamp_string
  }

let progress_of_job job =
  { id = job.id
  ; status = string_of_job_status job.status
  ; diffs_published = job.diffs_published
  ; error = job.error
  }

let health t =
  { ok = true
  ; instance_id = t.instance_id
  ; started_at = timestamp_string t.started_at
  }

let rec flush_subscriber subscriber =
  let open Deferred.Let_syntax in
  match subscriber.pending with
  | None ->
      subscriber.draining <- false ;
      if subscriber.close_after_flush then Pipe.close subscriber.writer ;
      Deferred.unit
  | Some progress ->
      subscriber.pending <- None ;
      let%bind () = Pipe.write_if_open subscriber.writer progress in
      flush_subscriber subscriber

let create_subscriber writer =
  { writer; pending = None; draining = false; close_after_flush = false }

let enqueue_progress subscriber progress ~terminal =
  if Pipe.is_closed subscriber.writer then false
  else (
    subscriber.pending <- Some progress ;
    if terminal then subscriber.close_after_flush <- true ;
    if not subscriber.draining then (
      subscriber.draining <- true ;
      don't_wait_for (flush_subscriber subscriber) ) ;
    true )

let notify_subscribers job =
  let progress = progress_of_job job in
  let terminal = is_terminal job.status in
  let subscribers =
    List.filter !(job.subscribers) ~f:(fun subscriber ->
        enqueue_progress subscriber progress ~terminal )
  in
  job.subscribers := if terminal then [] else subscribers

let update_job job ~status ?error ?started_at ?finished_at ?diffs_published () =
  job.status <- status ;
  Option.iter error ~f:(fun value -> job.error <- Some value) ;
  Option.iter started_at ~f:(fun value -> job.started_at <- Some value) ;
  Option.iter finished_at ~f:(fun value -> job.finished_at <- Some value) ;
  Option.iter diffs_published ~f:(fun value -> job.diffs_published <- value) ;
  notify_subscribers job

let backfill_kind ~from_hash ~index =
  if is_genesis_hash from_hash && Int.equal index 0
  then Explorer_events.Transaction_kind.Genesis_replay
  else Explorer_events.Transaction_kind.Sync_replay

let source_query from_hash =
  if is_genesis_hash from_hash then `Genesis
  else `Specific from_hash

let create ~logger ~da_config ~nats_url =
  let%map nats_client = Nats_client_async.connect (Some nats_url) in
  { logger
  ; da_config
  ; nats_client = Some nats_client
  ; jobs = String.Table.create ()
  ; instance_id = Uuid.to_string (Uuid_unix.create ())
  ; started_at = Time.now ()
  }

let shutdown t =
  match t.nats_client with
  | None ->
      Deferred.unit
  | Some client ->
      Nats_client_async.close client

let find_job t id = Hashtbl.find t.jobs id

let subscribe_progress t ~id =
  match find_job t id with
  | None ->
      Or_error.errorf "Unknown backfill job %s" id
  | Some job ->
      let reader, writer = Pipe.create () in
      let subscriber = create_subscriber writer in
      if not (is_terminal job.status) then
        job.subscribers := subscriber :: !(job.subscribers) ;
      ignore
        (enqueue_progress subscriber (progress_of_job job)
           ~terminal:(is_terminal job.status) : bool ) ;
      Ok reader

let publish_message_result client ({ subject; headers; payload } :
    Explorer_events.message ) =
  let headers =
    match headers with
    | [] ->
        None
    | headers ->
        Some (Nats_client.Headers.of_list headers)
  in
  Nats_client_async.publish_result client ~subject ?headers
    (Yojson.Safe.to_string payload)

let publish_backfill_diff t ~from_hash ~index ~target_ledger_hash diff =
  let genesis = is_genesis_hash from_hash && Int.equal index 0 in
  let message =
    Explorer_events.build_transaction_message
      ~kind:(backfill_kind ~from_hash ~index)
      ~target_ledger_hash ~genesis ~diff
  in
  match t.nats_client with
  | None ->
      `Dropped
  | Some client ->
      publish_message_result client message

let run_job t job =
  let now = Time.now () in
  update_job job ~status:Running ~started_at:now () ;
  let source = source_query job.from_hash in
  don't_wait_for
    (Monitor.try_with_or_error (fun () ->
         let source_ledger_hash =
           match source with
           | `Genesis ->
               genesis_hash
           | `Specific ledger_hash ->
               ledger_hash
         in
         let%bind.Deferred.Result target_ledger_hashes =
           Da_layer.Client.get_ledger_hashes_chain ~logger:t.logger
             ~config:t.da_config ~source_ledger_hash:source
             ~target_ledger_hash:job.to_hash ()
         in
         let rec publish_targets current_source index = function
           | [] ->
               if Ledger_hash.equal current_source job.to_hash
               then Deferred.return (Ok ())
               else
                 Deferred.return
                   (Error
                      (Error.of_string
                         "Ledger hash chain ended before reaching backfill \
                          target") )
           | target_ledger_hash :: rest ->
               let%bind.Deferred.Result diff =
                 Da_layer.Client.get_diff ~logger:t.logger ~config:t.da_config
                   ~ledger_hash:target_ledger_hash
               in
               if
                 not
                   (Ledger_hash.equal
                      (Da_layer.Diff.Stable.V3.source_ledger_hash diff)
                      current_source )
               then
                 Deferred.return
                   (Error
                      (Error.of_string
                         "Backfill diff chain does not match requested ledger \
                          hash progression") )
               else (
               match
                 publish_backfill_diff t ~from_hash:job.from_hash ~index
                   ~target_ledger_hash diff
               with
               | `Queued ->
                   update_job job ~status:Running
                     ~diffs_published:(job.diffs_published + 1)
                     () ;
                   publish_targets target_ledger_hash (index + 1) rest
               | `Dropped ->
                   Deferred.return
                     (Error
                        (Error.of_string
                           "NATS publish dropped while backfilling diffs") ) )
         in
         let%bind.Deferred.Result () =
           publish_targets source_ledger_hash 0 target_ledger_hashes
         in
         return () )
     >>= function
     | Ok () ->
         update_job job ~status:Completed ~finished_at:(Time.now ()) ()
     | Error error ->
         update_job job ~status:Failed ~finished_at:(Time.now ())
           ~error:(Error.to_string_hum error) () )

let parse_ledger_hash hash =
  Or_error.try_with (fun () -> Ledger_hash.of_decimal_string hash)
  |> Or_error.map_error ~f:(fun err ->
         Error.tag_arg err "Invalid ledger hash" hash String.sexp_of_t )

let start_backfill t ~from_hash ~to_hash =
  let job =
    { id = Uuid.to_string (Uuid_unix.create ())
    ; from_hash
    ; to_hash
    ; status = Queued
    ; diffs_published = 0
    ; error = None
    ; created_at = Time.now ()
    ; started_at = None
    ; finished_at = None
    ; subscribers = ref []
    }
  in
  Hashtbl.set t.jobs ~key:job.id ~data:job ;
  notify_subscribers job ;
  run_job t job ;
  job

let start_backfill_from_strings t ~from_hash ~to_hash =
  let open Or_error.Let_syntax in
  let%bind from_hash = parse_ledger_hash from_hash in
  let%map to_hash = parse_ledger_hash to_hash in
  start_backfill t ~from_hash ~to_hash

module Gql = struct
  open Graphql_async.Schema

  let backfill_job_typ : (t, job_snapshot) typ =
    obj "BackfillJob"
      ~fields:(fun _ ->
        [ field "id" ~typ:(non_null string) ~args:[]
            ~resolve:(fun _ job -> job.id)
        ; field "fromHash" ~typ:(non_null string) ~args:[]
            ~resolve:(fun _ job -> job.from_hash)
        ; field "toHash" ~typ:(non_null string) ~args:[]
            ~resolve:(fun _ job -> job.to_hash)
        ; field "status" ~typ:(non_null string) ~args:[]
            ~resolve:(fun _ job -> job.status)
        ; field "diffsPublished" ~typ:(non_null int) ~args:[]
            ~resolve:(fun _ job -> job.diffs_published)
        ; field "error" ~typ:string ~args:[]
            ~resolve:(fun _ job -> job.error)
        ; field "createdAt" ~typ:(non_null string) ~args:[]
            ~resolve:(fun _ job -> job.created_at)
        ; field "startedAt" ~typ:string ~args:[]
            ~resolve:(fun _ job -> job.started_at)
        ; field "finishedAt" ~typ:string ~args:[]
            ~resolve:(fun _ job -> job.finished_at)
        ] )

  let progress_typ : (t, progress_snapshot) typ =
    obj "BackfillProgress"
      ~fields:(fun _ ->
        [ field "id" ~typ:(non_null string) ~args:[]
            ~resolve:(fun _ progress -> progress.id)
        ; field "status" ~typ:(non_null string) ~args:[]
            ~resolve:(fun _ progress -> progress.status)
        ; field "diffsPublished" ~typ:(non_null int) ~args:[]
            ~resolve:(fun _ progress -> progress.diffs_published)
        ; field "error" ~typ:string ~args:[]
            ~resolve:(fun _ progress -> progress.error)
        ] )

  let health_typ : (t, health_snapshot) typ =
    obj "Health"
      ~fields:(fun _ ->
        [ field "ok" ~typ:(non_null bool) ~args:[]
            ~resolve:(fun _ value -> value.ok)
        ; field "instanceId" ~typ:(non_null string) ~args:[]
            ~resolve:(fun _ value -> value.instance_id)
        ; field "startedAt" ~typ:(non_null string) ~args:[]
            ~resolve:(fun _ value -> value.started_at)
        ] )

  let query_fields =
    [ io_field "backfillJob" ~typ:backfill_job_typ
        ~args:Arg.[ arg "id" ~typ:(non_null string) ]
        ~resolve:(fun { ctx; _ } () id ->
          return (Option.map (find_job ctx id) ~f:snapshot))
    ; io_field "health" ~typ:(non_null health_typ) ~args:[]
        ~resolve:(fun { ctx; _ } () () -> return (health ctx))
    ]

  let mutation_fields =
    [ io_field "backfill" ~typ:(non_null backfill_job_typ)
        ~args:
          Arg.
            [ arg "fromHash" ~typ:(non_null string)
            ; arg "toHash" ~typ:(non_null string)
            ]
        ~resolve:(fun { ctx; _ } () from_hash to_hash ->
          match start_backfill_from_strings ctx ~from_hash ~to_hash with
          | Ok job ->
              return (Ok (snapshot job))
          | Error err ->
              return (Error (Error.to_string_hum err)))
    ]

  let subscription_fields =
    [ subscription_field "backfillProgress" ~typ:(non_null progress_typ)
        ~args:Arg.[ arg "id" ~typ:(non_null string) ]
        ~resolve:(fun { ctx; _ } id ->
          match subscribe_progress ctx ~id with
          | Ok progress ->
              Deferred.Result.return progress
          | Error err ->
              Deferred.return (Error (Error.to_string_hum err)))
    ]

  let schema =
    Graphql_async.Schema.(
      schema query_fields ~mutations:mutation_fields
        ~subscriptions:subscription_fields)
end

module Sse = struct
  let headers =
    Cohttp.Header.of_list
      [ ("Content-Type", "text/event-stream")
      ; ("Cache-Control", "no-cache")
      ; ("Connection", "keep-alive")
      ]

  let next_event payload =
    "event: next\ndata: " ^ Yojson.Basic.to_string payload ^ "\n\n"

  let complete_event = "event: complete\ndata: {}\n\n"

  let parse_request req body =
    Init.Graphql_internal.Params.extract req body
    |> Result.map_error ~f:Error.of_string

  let execute_subscription t req body =
    let open Deferred.Let_syntax in
    match parse_request req body with
    | Error err ->
        Deferred.return (Error err)
    | Ok (query, variables, operation_name) -> (
        match Graphql_parser.parse query with
        | Error err ->
            Deferred.return (Error (Error.of_string err))
        | Ok doc ->
            let%map result =
              Graphql_async.Schema.execute Gql.schema t ?variables
                ?operation_name doc
            in
            match result with
            | Ok (`Stream stream) ->
                Ok stream
            | Ok (`Response _) ->
                Error
                  (Error.of_string
                     "Expected a GraphQL subscription for /graphql/stream")
            | Error err ->
                Error
                  (Error.of_string
                     ("Invalid GraphQL subscription: "
                     ^ Yojson.Basic.to_string err ) ) )

  let rec write_stream body_writer stream =
    let open Deferred.Let_syntax in
    match stream () with
    | Seq.Nil ->
        let%map () = Pipe.write body_writer complete_event in
        Pipe.close body_writer
    | Seq.Cons (payload, next) ->
        let payload =
          match payload with
          | Ok payload ->
              payload
          | Error err ->
              err
        in
        let%bind () = Pipe.write body_writer (next_event payload) in
        write_stream body_writer next

let callback t _conn req body =
    let open Deferred.Let_syntax in
    let%bind body = Cohttp_async.Body.to_string body in
    match%bind execute_subscription t req body with
    | Error err ->
        Cohttp_async.Server.respond_string ~status:`Bad_request
          (Error.to_string_hum err)
        >>| fun response -> `Response response
    | Ok stream ->
        let body_reader, body_writer = Pipe.create () in
        don't_wait_for (write_stream body_writer stream) ;
        Cohttp_async.Server.respond ~headers
          ~body:(Cohttp_async.Body.of_pipe body_reader)
          ()
        >>| fun response -> `Response response
end
