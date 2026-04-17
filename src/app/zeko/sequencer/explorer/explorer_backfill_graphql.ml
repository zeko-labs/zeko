(* Defines the GraphQL schema for the standalone backfill server on top of the
   Explorer_backfill_service job and health snapshots. *)

open Core
open Async

open Graphql_async.Schema

let backfill_job_typ :
    (Explorer_backfill_service.t, Explorer_backfill_service.job_snapshot option) typ
    =
  obj "BackfillJob"
    ~fields:
      [ field "id" ~typ:(non_null string) ~args:[]
          ~resolve:(fun _ (job : Explorer_backfill_service.job_snapshot) -> job.id)
      ; field "fromHash" ~typ:(non_null string) ~args:[]
          ~resolve:(fun _ (job : Explorer_backfill_service.job_snapshot) ->
            job.from_hash)
      ; field "toHash" ~typ:(non_null string) ~args:[]
          ~resolve:(fun _ (job : Explorer_backfill_service.job_snapshot) ->
            job.to_hash)
      ; field "status" ~typ:(non_null string) ~args:[]
          ~resolve:(fun _ (job : Explorer_backfill_service.job_snapshot) ->
            job.status)
      ; field "diffsPublished" ~typ:(non_null int) ~args:[]
          ~resolve:(fun _ (job : Explorer_backfill_service.job_snapshot) ->
            job.diffs_published)
      ; field "error" ~typ:string ~args:[]
          ~resolve:(fun _ (job : Explorer_backfill_service.job_snapshot) ->
            job.error)
      ; field "createdAt" ~typ:(non_null string) ~args:[]
          ~resolve:(fun _ (job : Explorer_backfill_service.job_snapshot) ->
            job.created_at)
      ; field "startedAt" ~typ:string ~args:[]
          ~resolve:(fun _ (job : Explorer_backfill_service.job_snapshot) ->
            job.started_at)
      ; field "finishedAt" ~typ:string ~args:[]
          ~resolve:(fun _ (job : Explorer_backfill_service.job_snapshot) ->
            job.finished_at)
      ]

let progress_typ :
    (Explorer_backfill_service.t, Explorer_backfill_service.progress_snapshot option) typ
    =
  obj "BackfillProgress"
    ~fields:
      [ field "id" ~typ:(non_null string) ~args:[]
          ~resolve:
            (fun _ (progress : Explorer_backfill_service.progress_snapshot) ->
              progress.id)
      ; field "status" ~typ:(non_null string) ~args:[]
          ~resolve:
            (fun _ (progress : Explorer_backfill_service.progress_snapshot) ->
              progress.status)
      ; field "diffsPublished" ~typ:(non_null int) ~args:[]
          ~resolve:
            (fun _ (progress : Explorer_backfill_service.progress_snapshot) ->
              progress.diffs_published)
      ; field "error" ~typ:string ~args:[]
          ~resolve:
            (fun _ (progress : Explorer_backfill_service.progress_snapshot) ->
              progress.error)
      ]

let health_typ :
    (Explorer_backfill_service.t, Explorer_backfill_service.health_snapshot option) typ
    =
  obj "Health"
    ~fields:
      [ field "ok" ~typ:(non_null bool) ~args:[]
          ~resolve:(fun _ (value : Explorer_backfill_service.health_snapshot) ->
            value.ok)
      ; field "instanceId" ~typ:(non_null string) ~args:[]
          ~resolve:(fun _ (value : Explorer_backfill_service.health_snapshot) ->
            value.instance_id)
      ; field "startedAt" ~typ:(non_null string) ~args:[]
          ~resolve:(fun _ (value : Explorer_backfill_service.health_snapshot) ->
            value.started_at)
      ]

let query_fields =
  [ io_field "backfillJob" ~typ:backfill_job_typ
      ~args:Arg.[ arg "id" ~typ:(non_null string) ]
      ~resolve:(fun { ctx; _ } () id ->
        return
          (Ok
          (Option.map (Explorer_backfill_service.find_job ctx id)
             ~f:Explorer_backfill_service.snapshot ) ) )
  ; io_field "health" ~typ:(non_null health_typ) ~args:[]
      ~resolve:(fun { ctx; _ } () ->
        return (Ok (Explorer_backfill_service.health ctx)))
  ]

let mutation_fields =
  [ io_field "backfill" ~typ:(non_null backfill_job_typ)
      ~args:
        Arg.
          [ arg "fromHash" ~typ:(non_null string)
          ; arg "toHash" ~typ:(non_null string)
          ]
      ~resolve:(fun { ctx; _ } () from_hash to_hash ->
        let snapshot =
          match
            Explorer_backfill_service.start_backfill_from_strings ctx ~from_hash
              ~to_hash
          with
          | Ok job ->
              Explorer_backfill_service.snapshot job
          | Error err ->
              Explorer_backfill_service.failed_job_snapshot_from_strings
                ~from_hash ~to_hash (Error.to_string_hum err)
        in
        return (Ok snapshot))
  ]

let subscription_fields =
  [ subscription_field "backfillProgress" ~typ:(non_null progress_typ)
      ~args:Arg.[ arg "id" ~typ:(non_null string) ]
      ~resolve:(fun { ctx; _ } id ->
        match Explorer_backfill_service.subscribe_progress ctx ~id with
        | Ok progress ->
            Deferred.Result.return progress
        | Error err ->
            Deferred.return (Error (Error.to_string_hum err)))
  ]

let schema =
  Graphql_async.Schema.(
    schema query_fields ~mutations:mutation_fields
      ~subscriptions:subscription_fields)
