(* Small in-memory models used by the Gherkin tests for external systems that
   are outside this repo, such as JetStream deduplication and explorer-side
   rollback handling. *)

open Core

module Jetstream = struct
  type message =
    { msg_id : string
    ; source_hash : string
    ; target_hash : string
    }

  type t =
    { seen : String.Hash_set.t
    ; mutable messages : message list
    }

  let create () = { seen = String.Hash_set.create (); messages = [] }

  let publish t message =
    if Hash_set.mem t.seen message.msg_id
    then false
    else (
      Hash_set.add t.seen message.msg_id ;
      t.messages <- t.messages @ [ message ] ;
      true )

  let targets t = List.map t.messages ~f:(fun message -> message.target_hash)
end

module Consumer = struct
  type classification =
    | Continue
    | Gap of string
    | Rollback of
        { ancestor : string
        ; reverted : string list
        }

  type t =
    { chain : string list
    ; positions : int String.Table.t
    }

  let of_hash_chain chain =
    let positions = String.Table.create () in
    List.iteri chain ~f:(fun index hash ->
        Hashtbl.set positions ~key:hash ~data:index ) ;
    { chain; positions }

  let classify t ~source_hash =
    let tip = List.last_exn t.chain in
    if String.equal source_hash tip then Continue
    else
      match Hashtbl.find t.positions source_hash with
      | Some index ->
          Rollback
            { ancestor = source_hash
            ; reverted = List.drop t.chain (index + 1)
            }
      | None ->
          Gap source_hash

  let sequence_of_hash t hash = Hashtbl.find_exn t.positions hash
end

module Retry = struct
  let exponential_backoff ~attempts =
    List.init attempts ~f:(fun attempt -> Int.pow 2 attempt)
end

module Publisher = struct
  type outcome =
    | Published
    | Dropped

  type mode =
    | Disabled
    | Unavailable
    | Reconnected of Jetstream.t

  let publish mode message =
    match mode with
    | Disabled | Unavailable ->
        Dropped
    | Reconnected stream ->
        ignore (Jetstream.publish stream message : bool) ;
        Published
end
