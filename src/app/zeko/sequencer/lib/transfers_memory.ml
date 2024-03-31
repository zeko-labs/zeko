(**
  Hash table that holds the item only for the specified lifetime.
  Used to store proved transfer requested by users.
*)

open Base
open Mina_base

module Transfers_memory = struct
  type t =
    { table :
        ( string
        , float
          * ( Account_update.t
            , Zkapp_command.Digest.Account_update.t
            , Zkapp_command.Digest.Forest.t )
            Zkapp_command.Call_forest.t )
        Hashtbl.t
    ; queue : (string * float) Queue.t
    ; lifetime : float
    }

  let create ~lifetime =
    { table = Hashtbl.create (module String)
    ; queue = Queue.create ()
    ; lifetime
    }

  let cleanup t =
    let now = Unix.gettimeofday () in
    let rec loop () =
      match Queue.peek t.queue with
      | Some (key, timestamp) when Float.(now -. timestamp > t.lifetime) ->
          Hashtbl.remove t.table key ;
          (Queue.dequeue_exn t.queue : string * float) |> ignore ;
          loop ()
      | _ ->
          ()
    in
    loop ()

  let add t key account_update =
    let now = Unix.time () in
    Hashtbl.set t.table ~key ~data:(now, account_update) ;
    Queue.enqueue t.queue (key, now) ;
    cleanup t

  let get t key = Hashtbl.find t.table key
end

include Transfers_memory
