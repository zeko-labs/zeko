open Core_kernel
open Zeko_util
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Mina_base

module Make (Inputs : sig
  val macroslot_size : int

  val zeko_token_owner : PC.t

  val zeko_token_owner_vk_hash : field
end)
(Macroslot : module type of Macroslot.Make (Inputs))
(Outer_inst : module type of Outer.Make (Inputs) (Macroslot)) =
struct
  open Inputs
  open Outer_inst

  let zeko_token_id =
    constant Token_id.typ
      (Account_id.derive_token_id
         ~owner:(Account_id.create zeko_token_owner Token_id.default) )

  module Witness = struct
    type t = { public_key : PC.t; vk_hash : F.t; bid : Action.Bid.t }
    [@@deriving snarky]
  end

  include MkHandler (Witness)

  let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
    let* Witness.{ public_key; vk_hash; bid } = exists_witness in
    let* actions = Action.bid_to_actions_var bid in
    let valid_while = Slot_range.Checked.to_valid_while bid.slot_range in
    let account_update =
      { default_account_update with
        public_key
      ; authorization_kind = authorization_vk_hash vk_hash
      ; actions
      ; preconditions =
          { default_account_update.preconditions with valid_while }
      }
    in
    let zeko_recipient =
      { default_account_update with
        token_id = zeko_token_id
      ; public_key
      ; balance_change =
          Currency.Amount.Signed.Checked.of_unsigned bid.bid_amount
      ; may_use_token =
          Account_update.May_use_token.Checked.constant Parents_own_token
      }
    in
    let zeko_sender =
      { default_account_update with
        token_id = zeko_token_id
      ; public_key = bid.sequencer
      ; balance_change =
          Currency.Amount.Signed.Checked.(of_unsigned bid.bid_amount |> negate)
      ; may_use_token =
          Account_update.May_use_token.Checked.constant Parents_own_token
      }
    in
    let zeko_token_owner =
      { default_account_update with
        public_key = constant PC.typ zeko_token_owner
      ; authorization_kind =
          zeko_token_owner_vk_hash |> Run.Field.constant
          |> authorization_vk_hash
          (* FIXME add call_data *)
      }
    in
    let* public_output, auxiliary_output =
      make_outputs account_update
        [ (zeko_token_owner, [ (zeko_sender, []); (zeko_recipient, []) ]) ]
    in
    let*| auxiliary_output = create_prover_value auxiliary_output in
    Pickles.Inductive_rule.
      { previous_proof_statements = []; public_output; auxiliary_output }

  let rule : _ Pickles.Inductive_rule.t =
    { identifier = "zeko submit transfer"
    ; prevs = []
    ; main = (fun x -> main x |> Run.run_checked)
    ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
    }
end
