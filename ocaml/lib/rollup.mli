module Js = Js_of_ocaml.Js
open Async_kernel
open Mina_base
module Step = Pickles.Impls.Step

type t

type user_command =
  < signature : Js.js_string Js.t Js.readonly_prop
  ; from_base58 : Js.js_string Js.t Js.readonly_prop
  ; to_base58 : Js.js_string Js.t Js.readonly_prop
  ; amount : Js.js_string Js.t Js.readonly_prop
  ; fee : Js.js_string Js.t Js.readonly_prop
  ; valid_until : Js.js_string Js.t Js.readonly_prop
  ; nonce : Js.js_string Js.t Js.readonly_prop
  ; memo : Js.js_string Js.t Js.readonly_prop
  ; account_creation_fee : Js.js_string Js.t Js.readonly_prop >
  Js.t

val rollup :
  < compile :
      < apply_user_command : t -> user_command -> unit Js.meth
      ; commit : t -> (Js.js_string Js.t -> unit) -> unit Deferred.t Js.meth
      ; create_zkapp :
             Js.js_string Js.t
          -> < account_update : Account_update.t Js.readonly_prop
             ; rollup : t Js.readonly_prop >
             Js.t
             Js.meth
      ; get_account : t -> Account.key -> Step.field -> 'a option Js.meth
      ; vk : Side_loaded_verification_key.t Js.readonly_prop >
      Js.t
      Js.meth >
  Js.t
