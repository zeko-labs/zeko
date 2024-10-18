open Zeko_util
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Rollup_state

module Witness = struct
  type t = { public_key : PC.t; vk_hash : F.t; witness : Outer.Action.Witness.t }
  [@@deriving snarky]
end

include MkHandler (Witness)

let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
  let* Witness.{ public_key; vk_hash; witness } = exists_witness in
  let* actions = Outer.Action.witness_to_actions_var witness in
  let valid_while = Slot_range.Checked.to_valid_while witness.slot_range in
  let account_update =
    { default_account_update with
      public_key
    ; authorization_kind = authorization_vk_hash vk_hash
    ; actions
    ; preconditions = { default_account_update.preconditions with valid_while }
    }
  in
  let* public_output, auxiliary_output =
    make_outputs account_update (Raw witness.children)
  in
  let*| auxiliary_output = V.create auxiliary_output in
  Pickles.Inductive_rule.
    { previous_proof_statements = []; public_output; auxiliary_output }

let rule : _ Pickles.Inductive_rule.t =
  { identifier = "zeko action witness"
  ; prevs = []
  ; main = (fun x -> main x |> Run.run_checked)
  ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
  }
