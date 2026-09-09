open Async
open Core_kernel
module Finality = Sequencer_lib.Settlement_finality

let test_wait_until_idle () =
  let observations = Queue.of_list [ true; false ] in
  let fetch_count = ref 0 in
  let wait_count = ref 0 in
  Thread_safe.block_on_async_exn (fun () ->
      Finality.wait_until_idle
        ~sleep:(fun _ -> Deferred.unit)
        ~has_pending:(fun () ->
          Int.incr fetch_count ;
          Deferred.Or_error.return (Queue.dequeue_exn observations) )
        ~on_wait:(fun () -> Int.incr wait_count)
        ()
      >>| Or_error.ok_exn ) ;
  assert (!fetch_count = 2) ;
  assert (!wait_count = 1)

let test_preparation_follows_finality_without_blocking_admission () =
  Thread_safe.block_on_async_exn (fun () ->
      let apply_q =
        Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1
      in
      let wait_started = Ivar.create () in
      let release_wait = Ivar.create () in
      let preparation_started = Ivar.create () in
      let admission_started = Ivar.create () in
      let outer_state = ref `Before_finality in
      let commit =
        Finality.prepare_and_enqueue_after_wait
          ~wait:(fun () ->
            Ivar.fill wait_started () ;
            Ivar.read release_wait >>| Or_error.return )
          ~prepare:(fun () ->
            Ivar.fill preparation_started () ;
            Deferred.Or_error.return !outer_state )
          ~enqueue:(fun job -> Throttle.enqueue apply_q job)
          (fun prepared_state () ->
            assert (Poly.equal prepared_state `After_finality) ;
            Deferred.Or_error.return () )
      in
      let%bind () = Ivar.read wait_started in
      let admission =
        Throttle.enqueue apply_q (fun () ->
            Ivar.fill admission_started () ;
            Deferred.unit )
      in
      let%bind () = Scheduler.yield_until_no_jobs_remain () in
      assert (Ivar.is_full admission_started) ;
      assert (Ivar.is_empty preparation_started) ;
      outer_state := `After_finality ;
      Ivar.fill release_wait () ;
      let%bind commit_result, () = Deferred.both commit admission in
      let%map commit_result = Or_error.ok_exn commit_result in
      Or_error.ok_exn commit_result ;
      assert (Ivar.is_full preparation_started) )

let () =
  test_wait_until_idle () ;
  test_preparation_follows_finality_without_blocking_admission ()
