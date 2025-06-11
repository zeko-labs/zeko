open Core_kernel
open Mina_base

(** Holds keys to all the diffes *)
module Index = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Ledger_hash.Stable.V1.t list

      let to_latest = Fn.id
    end
  end]

  let to_bigstring = Binable.to_bigstring (module Stable.Latest)

  let of_bigstring = Binable.of_bigstring (module Stable.Latest)
end

module Key_value = struct
  type _ t =
    | Diff : (Ledger_hash.t * Diff.Stable.V2.t) t
    | Diff_index : (unit * Index.t) t
    | Migration : (unit * int) t

  let serialize_key : type k v. (k * v) t -> k -> Bigstring.t =
   fun pair_type key ->
    match pair_type with
    | Diff ->
        Bigstring.concat
          [ Bigstring.of_string "diff"
          ; Bigstring.of_string @@ Ledger_hash.to_decimal_string key
          ]
    | Diff_index ->
        Bigstring.of_string "diff_index"
    | Migration ->
        Bigstring.of_string "migration"

  let serialize_value : type k v. (k * v) t -> v -> Bigstring.t =
   fun pair_type value ->
    match pair_type with
    | Diff ->
        Diff.to_bigstring value
    | Diff_index ->
        Index.to_bigstring value
    | Migration ->
        Bigstring.of_string @@ Int.to_string value

  let deserialize_value : type k v. (k * v) t -> Bigstring.t -> v =
   fun pair_type data ->
    match pair_type with
    | Diff ->
        Diff.of_bigstring data |> Or_error.ok_exn
    | Diff_index ->
        Index.of_bigstring data
    | Migration ->
        Int.of_string @@ Bigstring.to_string data
end

include Kvdb_base.Make (Key_value)

let set_index t ~index = set t Diff_index ~key:() ~data:index

let get_index t = get t Diff_index ~key:() |> Option.value ~default:[]

let add_diff t ~ledger_hash ~diff =
  let index = get_index t in
  if List.mem index ledger_hash ~equal:Ledger_hash.equal then `Already_existed
  else
    let new_index = ledger_hash :: index in
    set_batch t
      [ Pack (Diff, ledger_hash, diff); Pack (Diff_index, (), new_index) ] ;
    `Added

let get_diff t ~ledger_hash = get t Diff ~key:ledger_hash

let get_migration t = get t Migration ~key:() |> Option.value ~default:0

let set_migration t ~migration = set t Migration ~key:() ~data:migration

let has_diff t ~ledger_hash = get t Diff ~key:ledger_hash |> Option.is_some

module Async = struct
  let set_index t ~index = Async.return (set_index t ~index)

  let get_index t = Async.return (get_index t)

  let add_diff t ~ledger_hash ~diff =
    Async.return (add_diff t ~ledger_hash ~diff)

  let get_diff t ~ledger_hash = Async.return (get_diff t ~ledger_hash)

  let get_migration t = Async.return (get_migration t)

  let set_migration t ~migration = Async.return (set_migration t ~migration)

  let has_diff t ~ledger_hash = Async.return (has_diff t ~ledger_hash)
end
