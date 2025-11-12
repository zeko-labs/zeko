open Core_kernel
open Mina_base
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Bridge_state
open Zeko_util

module Make (Inputs : sig
  val token_owner_l2 : Account_id.t option

  val chain_l2 : Mina_signature_kind.t
end) =
struct
  open Inputs

  let token_id_l2 = token_owner_id token_owner_l2

  module Witness = struct
    type t = { public_key : PC.t; vk_hash : F.t; amount : Currency.Amount.t }
    [@@deriving snarky]
  end

  (** Allow receiving any amount, even though access = Proof. *)
  let main (w : Witness.t V.t) =
    with_label ("main " ^ __LOC__) (fun () ->
        let* Witness.{ public_key; vk_hash; amount } =
          exists Witness.typ ~compute:(V.get w)
        in
        let account_update =
          { default_account_update with
            public_key
          ; token_id = constant Token_id.typ token_id_l2
          ; may_use_token =
              constant Account_update.May_use_token.typ Parents_own_token
          ; authorization_kind = authorization_vk_hash vk_hash
          ; balance_change = Currency.Amount.Signed.Checked.(of_unsigned amount)
          }
        in
        let*| out = make_outputs ~chain:chain_l2 account_update [] in
        Compile_simple.{ prevs = No_prevs; out } )

  let rule : _ Compile_simple.branch lazy_t =
    lazy { branch_name = "inner receive"; tags = No_tags; main }
end
