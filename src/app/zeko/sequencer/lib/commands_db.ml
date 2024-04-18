open Core_kernel
open Mina_base
open Mina_transaction
open Sexplib.Std
open Snark_params
open Key_value_database.Monad.Ident.Let_syntax
module Db = Mina_ledger.Ledger.Kvdb

type t = Db.t

let serialize_key command_hash =
  Bigstring.of_string
    ("command_hash-" ^ Transaction_hash.to_base58_check command_hash)

let store_command_hashes t ~hashes =
  List.iter hashes ~f:(fun hash ->
      Db.set t ~key:(serialize_key hash) ~data:(Bigstring.of_string "1") )

let is_hash_stored t ~hash =
  Option.is_some @@ Db.get t ~key:(serialize_key hash)
