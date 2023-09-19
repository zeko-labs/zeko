module Js = Js_of_ocaml.Js
open Async_kernel
open Mina_base
module Step = Pickles.Impls.Step

type t

type user_command =
  < signature : Js.js_string Js.t Js.readonly_prop
  ; fromBase58 : Js.js_string Js.t Js.readonly_prop
  ; toBase58 : Js.js_string Js.t Js.readonly_prop
  ; amount : Js.js_string Js.t Js.readonly_prop
  ; fee : Js.js_string Js.t Js.readonly_prop
  ; validUntil : Js.js_string Js.t Js.readonly_prop
  ; nonce : Js.js_string Js.t Js.readonly_prop
  ; memo : Js.js_string Js.t Js.readonly_prop >
  Js.t

type txn_snark_input =
  { sparse_ledger : Mina_ledger.Sparse_ledger.t
  ; statement : Transaction_snark.Statement.With_sok.t
  ; command : Signed_command.With_valid_signature.t
  ; global_slot : Mina_numbers.Global_slot_since_genesis.t
  ; init_stack : Pending_coinbase.Stack_versioned.t
  ; sok_digest : Sok_message.Digest.t
  }

val rollup :
  < compile :
      < applyUserCommand :
             t
          -> user_command
          -> < txHash : Js.js_string Js.t Js.readonly_prop
             ; txId : Js.js_string Js.t Js.readonly_prop
             ; txnSnarkInputJson : txn_snark_input Js.readonly_prop >
             Js.t
             Js.meth
      ; proveUserCommand :
             txn_snark_input
          -> Js.js_string Js.t
          -> (Js.js_string Js.t -> unit)
          -> unit Deferred.t Js.meth
      ; commit :
             t
          -> Transaction_snark.t option
          -> (Js.js_string Js.t -> unit)
          -> unit Deferred.t Js.meth
      ; createZkapp :
             Js.js_string Js.t
          -> Account.key
          -> < publicKey : Account.key Js.prop
             ; balance : Js.js_string Js.t Js.prop >
             Js.t
             Js.js_array
             Js.t
          -> < accountUpdate : Js.js_string Js.t Js.readonly_prop
             ; rollup : t Js.readonly_prop >
             Js.t
             Js.meth
      ; getAccount :
          t -> Account.key -> Step.field -> Js.Unsafe.any Js.optdef Js.meth
      ; vk : Side_loaded_verification_key.t Js.readonly_prop >
      Js.t
      Js.meth >
  Js.t
