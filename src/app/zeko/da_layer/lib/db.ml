open Core_kernel
open Mina_base

module Key_value = struct
  type _ t =
    | Diff : (Ledger_hash.t * Diff.Stable.V2.t) t
    | Migration : (unit * int) t

  let serialize_key : type k v. (k * v) t -> k -> Bigstring.t =
   fun pair_type key ->
    match pair_type with
    | Diff ->
        Bigstring.concat
          [ Bigstring.of_string "diff"
          ; Bigstring.of_string @@ Ledger_hash.to_decimal_string key
          ]
    | Migration ->
        Bigstring.of_string "migration"

  let serialize_value : type k v. (k * v) t -> v -> Bigstring.t =
   fun pair_type value ->
    match pair_type with
    | Diff ->
        Diff.to_bigstring value
    | Migration ->
        Bigstring.of_string @@ Int.to_string value

  let deserialize_value : type k v. (k * v) t -> Bigstring.t -> v =
   fun pair_type data ->
    match pair_type with
    | Diff ->
        Diff.of_bigstring data |> Or_error.ok_exn
    | Migration ->
        Int.of_string @@ Bigstring.to_string data
end

include Kvdb_base.Make (Key_value)

let has_diff t ~ledger_hash = get t Diff ~key:ledger_hash |> Option.is_some

let add_diff t ~ledger_hash ~diff =
  if has_diff t ~ledger_hash then `Already_existed
  else (
    set t Diff ~key:ledger_hash ~data:diff ;
    `Added )

let get_diff t ~ledger_hash = get t Diff ~key:ledger_hash

let get_migration t = get t Migration ~key:() |> Option.value ~default:0

let set_migration t ~migration = set t Migration ~key:() ~data:migration

module Async = struct
  let add_diff t ~ledger_hash ~diff =
    Async.return (add_diff t ~ledger_hash ~diff)

  let get_diff t ~ledger_hash = Async.return (get_diff t ~ledger_hash)

  let get_migration t = Async.return (get_migration t)

  let set_migration t ~migration = Async.return (set_migration t ~migration)

  let has_diff t ~ledger_hash = Async.return (has_diff t ~ledger_hash)
end
