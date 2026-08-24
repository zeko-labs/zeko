open Async
open Core_kernel
module Finality = Sequencer_lib.Settlement_finality

let () =
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
