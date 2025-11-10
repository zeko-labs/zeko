open Zeko_util
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Rollup_state

module Witness = struct
  type t = { public_key : PC.t; vk_hash : F.t; witness : Inner_action.t }
  [@@deriving snarky]
end

module Make (Inputs : sig
  val chain_l2 : Mina_signature_kind.t
end) =
struct
  open Inputs

  let%snarkydef_ main (w : Witness.t V.t) =
    let* Witness.{ public_key; vk_hash; witness } =
      exists ~compute:(V.get w) Witness.typ
    in
    let* actions = Inner_action.to_actions_var witness in
    let account_update =
      { default_account_update with
        public_key
      ; authorization_kind = authorization_vk_hash vk_hash
      ; actions
      }
    in
    let*| out =
      make_outputs ~chain:chain_l2 account_update (Raw witness.children)
    in
    Compile_simple.{ prevs = No_prevs; out }

  let rule : _ Compile_simple.branch lazy_t =
    lazy { branch_name = "zeko action witness"; tags = No_tags; main }
end
