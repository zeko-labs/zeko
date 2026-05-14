(* Owns explorer backfill job state and execution: tracks in-memory jobs,
   republishes DA diffs to NATS, and exposes the snapshots used by the GraphQL
   and SSE transport layers. *)

open Core
open Async
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

let terminal_job_retention = Time.Span.of_hr 1.

let backfill_interval_size = 1000

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

let remove_subscriber job subscriber =
  job.subscribers :=
    List.filter !(job.subscribers) ~f:(fun subscriber' ->
        not (phys_equal subscriber subscriber') )

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
  let%map nats_client =
    Explorer_events.connect_and_ensure_jetstream_strict ~logger nats_url
  in
  let nats_client =
    match nats_client with
    | Ok client ->
        [%log debug] "Explorer backfill service connected to NATS: url=%s"
          (Uri.to_string nats_url) ;
        client
    | Error error ->
        failwithf "Explorer backfill service requires JetStream: %s"
          (Error.to_string_hum error) ()
  in
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
      let logger = t.logger in
      [%log debug] "Shutting down explorer backfill NATS client: instance_id=%s"
        t.instance_id ;
      Nats_client_async.close client

let find_job t id = Hashtbl.find t.jobs id

let prune_terminal_jobs t =
  let cutoff = Time.sub (Time.now ()) terminal_job_retention in
  Hashtbl.filter_inplace t.jobs ~f:(fun job ->
      (not (is_terminal job.status))
      ||
      match job.finished_at with
      | None ->
          true
      | Some finished_at ->
          Time.( > ) finished_at cutoff )

let find_active_job_by_range t ~from_hash ~to_hash =
  Hashtbl.data t.jobs
  |> List.find ~f:(fun job ->
         (not (is_terminal job.status))
         && Ledger_hash.equal job.from_hash from_hash
         && Ledger_hash.equal job.to_hash to_hash )

let subscribe_progress t ~id =
  match find_job t id with
  | None ->
      Or_error.errorf "Unknown backfill job %s" id
  | Some job ->
      let reader, writer = Pipe.create () in
      let subscriber = create_subscriber writer in
      if not (is_terminal job.status) then (
        job.subscribers := subscriber :: !(job.subscribers) ;
        don't_wait_for
          ( Pipe.closed reader
          >>| fun () -> remove_subscriber job subscriber ) ) ;
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
  let logger = t.logger in
  let genesis = is_genesis_hash from_hash && Int.equal index 0 in
  let kind = backfill_kind ~from_hash ~index in
  [%log debug]
    "Publishing explorer backfill transaction event: subject=%s kind=%s \
     from_hash=%s target_ledger_hash=%s index=%d genesis=%b"
    Explorer_events.Subject.transactions
    (Explorer_events.Transaction_kind.to_string kind)
    (Ledger_hash.to_decimal_string from_hash)
    (Ledger_hash.to_decimal_string target_ledger_hash)
    index genesis ;
  let message =
    Explorer_events.build_transaction_message
      ~kind ~target_ledger_hash ~genesis ~diff
  in
  match t.nats_client with
  | None ->
      [%log debug]
        "Dropped explorer backfill transaction event: no NATS client \
         target_ledger_hash=%s"
        (Ledger_hash.to_decimal_string target_ledger_hash) ;
      `Dropped
  | Some client ->
      let result = publish_message_result client message in
      [%log debug]
        "Explorer backfill transaction event publish result: \
         target_ledger_hash=%s result=%s"
        (Ledger_hash.to_decimal_string target_ledger_hash)
        (Format.asprintf "%a" Nats_client_async.pp_publish_result result) ;
      result

let handle_backfill_publish_result t job ~target_ledger_hash result =
  let logger = t.logger in
  match result with
  | `Queued ->
      update_job job ~status:Running
        ~diffs_published:(job.diffs_published + 1)
        () ;
      [%log debug]
        "Backfill queued explorer transaction event: job_id=%s \
         target_ledger_hash=%s diffs_published=%d"
        job.id
        (Ledger_hash.to_decimal_string target_ledger_hash)
        job.diffs_published ;
      Ok target_ledger_hash
  | `Dropped ->
      [%log debug]
        "Backfill failed to queue explorer transaction event: job_id=%s \
         target_ledger_hash=%s"
        job.id
        (Ledger_hash.to_decimal_string target_ledger_hash) ;
      Error (Error.of_string "NATS publish dropped while backfilling diffs")

let source_hash = function
  | `Genesis ->
      genesis_hash
  | `Specific ledger_hash ->
      ledger_hash

let backfill_intervals t ~source_ledger_hash ~target_ledger_hash =
  let rec get_intervals ~target_ledger_hash =
    let%bind.Deferred.Result chain =
      Da_layer.Client.get_ledger_hashes_chain ~logger:t.logger
        ~config:t.da_config ~max_length:backfill_interval_size
        ~source_ledger_hash:(`Specific source_ledger_hash) ~target_ledger_hash
        ()
    in
    match chain with
    | [] ->
        return (Ok [])
    | [ last ] ->
        return (Ok [ (source_ledger_hash, last) ])
    | chain ->
        let interval_source = List.hd_exn chain in
        let interval_target = List.last_exn chain in
        let%bind.Deferred.Result intervals =
          get_intervals ~target_ledger_hash:interval_source
        in
        return (Ok ((interval_source, interval_target) :: intervals))
  in
  get_intervals ~target_ledger_hash >>| Result.map ~f:List.rev

let publish_target t job ~current_source ~index ~target_ledger_hash =
  let logger = t.logger in
  [%log debug]
    "Backfill fetching diff before explorer publish: job_id=%s \
     current_source=%s target_ledger_hash=%s index=%d"
    job.id
    (Ledger_hash.to_decimal_string current_source)
    (Ledger_hash.to_decimal_string target_ledger_hash)
    index ;
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
            "Backfill diff chain does not match requested ledger hash \
             progression") )
  else
    publish_backfill_diff t ~from_hash:job.from_hash ~index ~target_ledger_hash
      diff
    |> handle_backfill_publish_result t job ~target_ledger_hash
    |> Deferred.return

let run_job t job =
  let logger = t.logger in
  let now = Time.now () in
  [%log debug]
    "Starting explorer backfill job: job_id=%s from_hash=%s to_hash=%s"
    job.id
    (Ledger_hash.to_decimal_string job.from_hash)
    (Ledger_hash.to_decimal_string job.to_hash) ;
  update_job job ~status:Running ~started_at:now () ;
  let source = source_query job.from_hash in
  don't_wait_for
    (Monitor.try_with_or_error (fun () ->
         let source_ledger_hash = source_hash source in
         let%bind.Deferred.Result intervals =
           backfill_intervals t ~source_ledger_hash
             ~target_ledger_hash:job.to_hash
         in
         let index = ref 0 in
         let%bind.Deferred.Result final_source =
           Deferred.List.fold intervals ~init:(Ok source_ledger_hash)
             ~f:(fun acc (interval_source, interval_target) ->
               match acc with
               | Error _ as error ->
                   return error
               | Ok current_source ->
                   if not (Ledger_hash.equal current_source interval_source)
                   then
                     return
                       (Error
                          (Error.of_string
                             "Backfill intervals do not match requested ledger \
                              hash progression") )
                   else
                     let%bind.Deferred.Result target_ledger_hashes =
                       Da_layer.Client.get_ledger_hashes_chain ~logger:t.logger
                         ~config:t.da_config ~max_length:backfill_interval_size
                         ~source_ledger_hash:(`Specific interval_source)
                         ~target_ledger_hash:interval_target ()
                     in
                     Deferred.List.fold target_ledger_hashes
                       ~init:(Ok interval_source)
                       ~f:(fun acc target_ledger_hash ->
                         match acc with
                         | Error _ as error ->
                             return error
                         | Ok current_source ->
                             let%map result =
                               publish_target t job ~current_source ~index:!index
                                 ~target_ledger_hash
                             in
                             (match result with Ok _ -> incr index | Error _ -> ()) ;
                             result ) )
         in
         if Ledger_hash.equal final_source job.to_hash
         then Deferred.Result.return ()
         else
           Deferred.return
             (Error
                (Error.of_string
                   "Ledger hash chain ended before reaching backfill target") ) )
     >>= function
     | Ok (Ok ()) ->
         [%log debug]
           "Completed explorer backfill job: job_id=%s diffs_published=%d"
           job.id job.diffs_published ;
         update_job job ~status:Completed ~finished_at:(Time.now ()) () ;
         Deferred.unit
     | Ok (Error error) | Error error ->
         [%log debug]
           "Failed explorer backfill job: job_id=%s error=%s"
           job.id (Error.to_string_hum error) ;
         update_job job ~status:Failed ~finished_at:(Time.now ())
           ~error:(Error.to_string_hum error) () ;
         Deferred.unit )

let parse_ledger_hash hash =
  Or_error.try_with (fun () -> Ledger_hash.of_decimal_string hash)
  |> Result.map_error ~f:(fun err ->
         Error.tag_arg err "Invalid ledger hash" hash String.sexp_of_t )

let create_job ?error ?started_at ?finished_at ~status ~from_hash ~to_hash () =
  { id = Uuid.to_string (Uuid_unix.create ())
  ; from_hash
  ; to_hash
  ; status
  ; diffs_published = 0
  ; error
  ; created_at = Time.now ()
  ; started_at
  ; finished_at
  ; subscribers = ref []
  }

let register_job t job =
  let logger = t.logger in
  Hashtbl.set t.jobs ~key:job.id ~data:job ;
  [%log debug]
    "Registered explorer backfill job: job_id=%s from_hash=%s to_hash=%s"
    job.id
    (Ledger_hash.to_decimal_string job.from_hash)
    (Ledger_hash.to_decimal_string job.to_hash) ;
  notify_subscribers job ;
  job

let start_backfill t ~from_hash ~to_hash =
  let logger = t.logger in
  prune_terminal_jobs t ;
  match find_active_job_by_range t ~from_hash ~to_hash with
  | Some job ->
      [%log debug]
        "Reusing active explorer backfill job: job_id=%s from_hash=%s to_hash=%s"
        job.id
        (Ledger_hash.to_decimal_string from_hash)
        (Ledger_hash.to_decimal_string to_hash) ;
      job
  | None ->
      let job =
        create_job ~status:Queued ~from_hash ~to_hash ()
      in
      let job = register_job t job in
      run_job t job ;
      job

let failed_job_snapshot_from_strings ~from_hash ~to_hash error =
  let now = Time.now () in
  { id = Uuid.to_string (Uuid_unix.create ())
  ; from_hash
  ; to_hash
  ; status = string_of_job_status Failed
  ; diffs_published = 0
  ; error = Some error
  ; created_at = timestamp_string now
  ; started_at = None
  ; finished_at = Some (timestamp_string now)
  }

let start_backfill_from_strings t ~from_hash ~to_hash =
  let open Or_error.Let_syntax in
  let%bind from_hash = parse_ledger_hash from_hash in
  let%map to_hash = parse_ledger_hash to_hash in
  start_backfill t ~from_hash ~to_hash
