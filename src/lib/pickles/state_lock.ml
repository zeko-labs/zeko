open Core_kernel

open struct
  type waiter = unit -> unit

  type t = { mutable locked : bool; waiters : waiter Queue.t }

  let t = { locked = false; waiters = Queue.create () }

  let lock () =
    if not t.locked then (
      t.locked <- true ;
      Promise.return () )
    else
      let promise =
        Promise.create (fun resolve -> Queue.enqueue t.waiters resolve)
      in
      promise

  let unlock () =
    match Queue.dequeue t.waiters with
    | None ->
        t.locked <- false
    | Some resolve ->
        resolve ()
end

let with_lock ~f =
  let%bind.Promise () = lock () in
  let%map.Promise result = f () in
  unlock () ; result
