open Core_kernel
open Mina_base
module Db = Mina_ledger.Ledger.Kvdb

type t = Db.t

let of_db db = Mina_ledger.Ledger.Db.kvdb db

let of_dir dir = Mina_ledger.Ledger.Kvdb.create dir

type key =
  | EVENTS of Account_id.t
  | ACTIONS of Account_id.t
  | COMMIT of (Frozen_ledger_hash.t * Frozen_ledger_hash.t)
  | COMMIT_INDEX
  | SNARK_QUEUE_STATE
  | PROTOCOL_STATE

let serialize_key = function
  | EVENTS account_id ->
      Bigstring.(
        concat
          [ of_string "events"
          ; of_string
              ( Signature_lib.Public_key.Compressed.to_base58_check
                  (Account_id.public_key account_id)
              ^ "-"
              ^ Token_id.to_string (Account_id.token_id account_id) )
          ])
  | ACTIONS account_id ->
      Bigstring.(
        concat
          [ of_string "actions"
          ; of_string
              ( Signature_lib.Public_key.Compressed.to_base58_check
                  (Account_id.public_key account_id)
              ^ "-"
              ^ Token_id.to_string (Account_id.token_id account_id) )
          ])
  | COMMIT (hash1, hash2) ->
      Bigstring.(
        concat
          [ of_string "commit"
          ; of_string
              ( Frozen_ledger_hash.to_base58_check hash1
              ^ "-"
              ^ Frozen_ledger_hash.to_base58_check hash2 )
          ])
  | COMMIT_INDEX ->
      Bigstring.of_string "commit_index"
  | SNARK_QUEUE_STATE ->
      Bigstring.of_string "snark_queue_state"
  | PROTOCOL_STATE ->
      Bigstring.of_string "protocol_state"

let set t ~key ~data = Db.set t ~key:(serialize_key key) ~data

let get t ~key = Db.get t ~key:(serialize_key key)
