open Mina_base
open Snark_params.Tick
open Zeko_util
open Checked.Let_syntax
module PC = Signature_lib.Public_key.Compressed

module Make (Inputs : sig
  module Deposit_params : Bridge_state.DEPOSIT_PARAMS

  val holder_accounts_l1 : Signature_lib.Public_key.Compressed.t list

  val ethereum_holder_account_l1 : Signature_lib.Public_key.Compressed.t option

  val ethereum_asset_id :
    (Snark_params.Tick.Field.t * Snark_params.Tick.Field.t) option

  val token_owner_l1 : Account_id.t option

  val chain_l1 : Mina_signature_kind.t

  val bridge_fee_recipient_l1 : Signature_lib.Public_key.Compressed.t

  val bridge_proof_fee : Currency.Amount.t
end)
() =
struct
  open Inputs

  module Definition = struct
    module Stmt = struct
      type t =
        { params : Deposit_params.t
        ; action_state : Rollup_state.Outer_action_state.t
        ; deposit_index : Checked32.t
        ; n_steps : Checked32.t
        ; is_rejected : Boolean.t
        ; is_accepted : Boolean.t
        }
      [@@deriving snarky]
    end

    module Elem = Rollup_state.Outer_action

    let dummy_elem =
      Rollup_state.Outer_action.Witness
        { aux = Field.zero
        ; children_digest = Rollup_state.Zkapp_call_forest.Digest.empty
        ; slot_range = Slot_range.infinite
        }

    module Init = struct
      type t =
        { params : Deposit_params.t
        ; original_action_state : Rollup_state.Outer_action_state.t
        ; deposit_index : Checked32.t
        }
      [@@deriving snarky]
    end

    let init ~check:_
        ({ params; original_action_state; deposit_index } : Init.var) :
        Stmt.var Checked.t =
      let* witness =
        Bridge_state.deposit_action ~chain_l1
          ~bridge_fee_recipient_l1:(constant PC.typ bridge_fee_recipient_l1)
          ~bridge_proof_fee:(constant Currency.Amount.typ bridge_proof_fee)
          ~holder_accounts_l1 ~token_owner_l1 ~ethereum_holder_account_l1
          ~ethereum_asset_id
          (module Deposit_params)
          params
      in
      let* action_state =
        Rollup_state.Outer_action.push_witness_var witness original_action_state
      in
      Checked.return
        ( { params
          ; action_state
          ; deposit_index
          ; n_steps = Checked32.Checked.zero
          ; is_rejected = Boolean.false_
          ; is_accepted = Boolean.false_
          }
          : Stmt.var )

    let step (action : Rollup_state.Outer_action.var)
        ({ params
         ; action_state
         ; n_steps
         ; is_rejected
         ; is_accepted
         ; deposit_index
         } :
          Stmt.var ) =
      let* n_steps = Checked32.Checked.succ n_steps in
      let* action_state =
        Rollup_state.Outer_action.push_var action action_state
      in
      let* valid_while =
        if_ ~typ:Slot_range.typ action.is_witness
          ~then_:action.case_witness.slot_range
          ~else_:action.case_commit.slot_range
      in
      let base_params = Deposit_params.base params in
      let open Boolean in
      let* is_rejected =
        Slot.Checked.(valid_while.lower > base_params.timeout)
        >>= ( &&& ) (not is_accepted) >>= ( ||| ) is_rejected
      in
      let* deposit_index_before_commit =
        Checked32.Checked.(
          Rollup_state.Outer_action_state.With_length.length_var
            action.case_commit.synchronized_outer_action_state
          > deposit_index)
      in
      let*| is_accepted =
        Slot.Checked.(valid_while.upper <= base_params.timeout)
        >>= ( &&& ) deposit_index_before_commit
        >>= ( &&& ) action.is_commit >>= ( &&& ) (not is_rejected)
        >>= ( ||| ) is_accepted
      in
      Stmt.
        { params
        ; action_state
        ; n_steps
        ; is_rejected
        ; is_accepted
        ; deposit_index
        }

    let name = "check_accepted"

    let leaf_iterations =
      Zeko_constants.Folder_iterations.Check_accepted.leaf_iterations

    let leaf_option_iterations =
      Zeko_constants.Folder_iterations.Check_accepted.leaf_option_iterations

    let extend_iterations =
      Zeko_constants.Folder_iterations.Check_accepted.extend_iterations

    let extend_option_iterations =
      Zeko_constants.Folder_iterations.Check_accepted.extend_option_iterations

    let wrap_domain = Some `N14
  end

  include Folder.Make (Definition) ()
end
