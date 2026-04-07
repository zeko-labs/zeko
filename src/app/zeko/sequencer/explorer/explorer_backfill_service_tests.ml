(* Keeps the backfill service tests in a dedicated module so the implementation
   file stays focused on job execution and API wiring. *)

open Core
open Async
open Sequencer_lib
open Mina_base

let test_service () : Explorer_backfill_service.t =
  { logger = Logger.create ()
  ; da_config = Da_layer.Client.Config.of_string_list []
  ; nats_client = None
  ; jobs = String.Table.create ()
  ; instance_id = "instance-1"
  ; started_at = Time.epoch
  }

let%test_unit "status strings are stable" =
  [%test_eq: string]
    (Explorer_backfill_service.string_of_job_status Explorer_backfill_service.Queued)
    "queued" ;
  [%test_eq: string]
    (Explorer_backfill_service.string_of_job_status Explorer_backfill_service.Completed)
    "completed"

let%test_unit "backfill kind uses genesis replay only for the first genesis diff" =
  [%test_eq: Explorer_events.Transaction_kind.t]
    (Explorer_backfill_service.backfill_kind
       ~from_hash:Explorer_backfill_service.genesis_hash ~index:0 )
    Explorer_events.Transaction_kind.Genesis_replay ;
  [%test_eq: Explorer_events.Transaction_kind.t]
    (Explorer_backfill_service.backfill_kind ~from_hash:Ledger_hash.empty_hash
       ~index:1 )
    Explorer_events.Transaction_kind.Sync_replay

let%test_unit "health snapshot includes the instance id" =
  [%test_eq: string]
    (Explorer_backfill_service.health (test_service ())).instance_id
    "instance-1"

let%test_unit "invalid backfill hash returns an error instead of raising" =
  [%test_eq: bool]
    (Result.is_error
       (Explorer_backfill_service.start_backfill_from_strings (test_service ())
          ~from_hash:"bad-hash" ~to_hash:"also-bad" ) )
    true

let%test_unit "backfill publish reports dropped when no NATS client is present" =
  let diff =
    Explorer_events.build_live_diff ~logger:(Logger.create ())
      ~diff:
        (Da_layer.Diff.create ~source_ledger_hash:Ledger_hash.empty_hash
           ~changed_accounts:[] ~command_with_action_step_flags:None )
      ~acc_set_root:Snark_params.Tick.Field.zero
  in
  [%test_eq: Nats_client_async.publish_result]
    (Explorer_backfill_service.publish_backfill_diff (test_service ())
       ~from_hash:Ledger_hash.empty_hash ~index:0
       ~target_ledger_hash:Ledger_hash.empty_hash diff )
    `Dropped

let%test_unit "sse next event carries GraphQL JSON" =
  let event =
    Explorer_backfill_service.Sse.next_event
      (`Assoc
        [ ( "data"
          , `Assoc
              [ ( "backfillProgress"
                , `Assoc [ ("id", `String "job-1") ] )
              ] )
        ] )
  in
  [%test_eq: bool]
    (String.is_substring event ~substring:"backfillProgress")
    true
