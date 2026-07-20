open Core_kernel
open Sequencer_lib
module Schedule = Zeko_sequencer.Sequencer.Commit_schedule

let () =
  let disabled = Schedule.(create 0. |> snapshot) in
  assert (match disabled.phase with Schedule.Disabled -> true | _ -> false) ;
  assert (Option.is_none disabled.next_attempt_at) ;
  let schedule = Schedule.create 900. in
  let started_at = Time_ns.now () in
  let next_attempt_at = Time_ns.add started_at (Time_ns.Span.of_sec 900.) in
  Schedule.start_attempt schedule ~started_at ~next_attempt_at ;
  let active = Schedule.snapshot schedule in
  assert (match active.phase with Schedule.Committing -> true | _ -> false) ;
  assert (
    Option.equal Time_ns.equal active.next_attempt_at (Some next_attempt_at) ) ;
  Schedule.wait schedule ;
  assert (
    match (Schedule.snapshot schedule).phase with
    | Schedule.Waiting ->
        true
    | _ ->
        false ) ;
  Schedule.disable schedule ;
  let disabled = Schedule.snapshot schedule in
  assert (match disabled.phase with Schedule.Disabled -> true | _ -> false) ;
  assert (Option.is_none disabled.next_attempt_at)
