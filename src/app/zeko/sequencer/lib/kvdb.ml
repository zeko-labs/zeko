open Core_kernel
open Mina_base

module Key = struct
  type t =
    | Events of Account_id.t
    | Actions of Account_id.t
    | Commit of (Frozen_ledger_hash.t * Frozen_ledger_hash.t)
    | Commit_index
    | Snark_queue_state
    | Protocol_state

  let serialize = function
    | Events account_id ->
        Bigstring.(
          concat
            [ of_string "events"
            ; of_string
                ( Signature_lib.Public_key.Compressed.to_base58_check
                    (Account_id.public_key account_id)
                ^ "-"
                ^ Token_id.to_string (Account_id.token_id account_id) )
            ])
    | Actions account_id ->
        Bigstring.(
          concat
            [ of_string "actions"
            ; of_string
                ( Signature_lib.Public_key.Compressed.to_base58_check
                    (Account_id.public_key account_id)
                ^ "-"
                ^ Token_id.to_string (Account_id.token_id account_id) )
            ])
    | Commit (hash1, hash2) ->
        Bigstring.(
          concat
            [ of_string "commit"
            ; of_string
                ( Frozen_ledger_hash.to_base58_check hash1
                ^ "-"
                ^ Frozen_ledger_hash.to_base58_check hash2 )
            ])
    | Commit_index ->
        Bigstring.of_string "commit_index"
    | Snark_queue_state ->
        Bigstring.of_string "snark_queue_state"
    | Protocol_state ->
        Bigstring.of_string "protocol_state"
end

include Kvdb_base.Make (Key)
