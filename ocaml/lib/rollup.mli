module Js = Js_of_ocaml.Js
open Async_kernel
open Mina_base
module Step = Pickles.Impls.Step

type t

(* type user_command =
   < signature : Js.js_string Js.t Js.readonly_prop
   ; fromBase58 : Js.js_string Js.t Js.readonly_prop
   ; toBase58 : Js.js_string Js.t Js.readonly_prop
   ; amount : Js.js_string Js.t Js.readonly_prop
   ; fee : Js.js_string Js.t Js.readonly_prop
   ; validUntil : Js.js_string Js.t Js.readonly_prop
   ; nonce : Js.js_string Js.t Js.readonly_prop
   ; memo : Js.js_string Js.t Js.readonly_prop
   ; accountCreationFee : Js.js_string Js.t Js.readonly_prop >
   Js.t *)

module Js_user_command : sig
  type t =
    < signature : Js.js_string Js.t Js.readonly_prop
    ; fromBase58 : Js.js_string Js.t Js.readonly_prop
    ; toBase58 : Js.js_string Js.t Js.readonly_prop
    ; amount : Js.js_string Js.t Js.readonly_prop
    ; fee : Js.js_string Js.t Js.readonly_prop
    ; validUntil : Js.js_string Js.t Js.readonly_prop
    ; nonce : Js.js_string Js.t Js.readonly_prop
    ; memo : Js.js_string Js.t Js.readonly_prop
    ; accountCreationFee : Js.js_string Js.t Js.readonly_prop >
    Js.t
end

val rollup :
  < compile :
         Account.key
      -> < applyUserCommand :
                t
             -> Js_user_command.t
             -> < txHash : Js.js_string Js.t Js.readonly_prop
                ; txId : Js.js_string Js.t Js.readonly_prop
                ; txnSnarkInputJson : Js.js_string Js.t Js.readonly_prop >
                Js.t
                Js.meth
         ; proveUserCommand :
                Js.js_string Js.t
             -> Js.js_string Js.t Js.optdef
             -> (Js.js_string Js.t -> unit)
             -> unit Deferred.t Js.meth
         ; commit :
                Js.js_string Js.t
             -> (Js.js_string Js.t -> unit)
             -> unit Deferred.t Js.meth
         ; createZkapp :
                Js.js_string Js.t
             -> < publicKey : Account.key Js.prop
                ; balance : Js.js_string Js.t Js.prop >
                Js.t
                Js.js_array
                Js.t
             -> < accountUpdate : Js.js_string Js.t Js.readonly_prop
                ; rollup : t Js.readonly_prop
                ; genesisLedgerHash : Js.js_string Js.t Js.readonly_prop >
                Js.t
                Js.meth
         ; getAccount :
             t -> Account.key -> Step.field -> Js.Unsafe.any Js.optdef Js.meth
         ; getRoot : t -> Js.js_string Js.t Js.meth
         ; getLedgerHashFromSnark :
             Js.js_string Js.t -> Js.js_string Js.t Js.meth
         ; vk : Side_loaded_verification_key.t Js.readonly_prop >
         Js.t
         Js.meth >
  Js.t
