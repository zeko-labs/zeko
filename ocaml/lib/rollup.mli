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
  ; memo : Js.js_string Js.t Js.readonly_prop
  ; accountCreationFee : Js.js_string Js.t Js.readonly_prop >
  Js.t

val rollup :
  < compile :
      < applyUserCommand : t -> user_command -> unit Js.meth
      ; commit : t -> (Js.js_string Js.t -> unit) -> unit Deferred.t Js.meth
      ; createZkapp :
             Js.js_string Js.t
          -> < publicKey : Js.js_string Js.t Js.prop
             ; balance : Js.js_string Js.t Js.prop >
             Js.t
             Js.js_array
             Js.t
          -> < accountUpdate : Js.js_string Js.t Js.readonly_prop
             ; rollup : t Js.readonly_prop >
             Js.t
             Js.meth
      ; getAccount : t -> Account.key -> Step.field -> 'a option Js.meth
      ; vk : Side_loaded_verification_key.t Js.readonly_prop >
      Js.t
      Js.meth >
  Js.t
