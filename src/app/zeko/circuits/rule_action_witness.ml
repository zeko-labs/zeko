open Zeko_util
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Rollup_state

module Witness = struct
  type t =
    { public_key : PC.t; vk_hash : F.t; witness : Outer_action.Witness.t }
  [@@deriving snarky]
end

module Make (Inputs : sig
  val chain_l1 : Mina_signature_kind.t
end) =
struct
  open Inputs

  let%snarkydef_ main (w : Witness.t V.t) =
    let* Witness.{ public_key; vk_hash; witness } =
      exists ~compute:(V.get w) Witness.typ
    in
    let* actions = Outer_action.witness_to_actions_var witness in
    let valid_while = Slot_range.Checked.to_valid_while witness.slot_range in
    let* status_flags_precondition =
      Outer_state.Status_flags.of_bools_var ~paused:Boolean.false_
        ~emergency:Boolean.false_
    in
    let account_update =
      { default_account_update with
        public_key
      ; authorization_kind = authorization_vk_hash vk_hash
      ; actions
      ; preconditions =
          { default_account_update.preconditions with
            valid_while
          ; account =
              { default_account_update.preconditions.account with
                state =
                  Outer_state.fine
                    { pause_key = None
                    ; status_flags =
                        Some status_flags_precondition
                        (* We don't allow adding actions if the rollup is paused since it signals something is wrong. *)
                    ; ledger_hash = None
                    ; inner_action_state = { length = None; state = None }
                    ; sequencer = None
                    ; da_key = None
                    ; acc_set = None
                    }
                  |> var_to_precondition_fine
              }
          }
      }
    in
    let*| out =
      make_outputs ~chain:chain_l1 account_update (Raw witness.children)
    in
    Compile_simple.{ prevs = No_prevs; out }

  let rule : _ Compile_simple.branch lazy_t =
    lazy { branch_name = "zeko action witness"; tags = No_tags; main }
end
