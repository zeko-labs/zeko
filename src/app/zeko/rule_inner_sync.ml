open Core_kernel
open Inner
open Zeko_util
open Snark_params.Tick

module ASE = Action_state_extension.Make (struct
  let get_iterations = Int.pow 2 14
end)

module Witness = struct
  type t = { vk_hash : F.t; ext : ASE.t } [@@deriving snarky]
end

include MkHandler (Witness)

let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
  let* Witness.{ vk_hash; ext } = exists_witness in
  let* outer_action_state_ext, verify_ext = ASE.get ext in
  let update =
    { default_account_update.update with
      app_state =
        State.(
          var_to_app_state typ
            { outer_action_state = outer_action_state_ext.target.action_state })
        (* This is equal to outer state action state and is checked in outer account rule *)
    }
  in
  let preconditions =
    { default_account_update.preconditions with
      account =
        { default_account_update.preconditions.account with
          state =
            State.(
              var_to_precondition typ
                { outer_action_state =
                    outer_action_state_ext.source.action_state
                })
        }
    }
  in
  let account_update =
    { default_account_update with
      public_key = constant Signature_lib.Public_key.Compressed.typ public_key
    ; authorization_kind = authorization_vk_hash vk_hash
    ; update
    ; preconditions
    }
  in
  let* public_output, auxiliary_output = make_outputs account_update [] in
  let*| auxiliary_output = create_prover_value auxiliary_output in
  Pickles.Inductive_rule.
    { previous_proof_statements = [ verify_ext ]
    ; public_output
    ; auxiliary_output
    }

let rule : _ Pickles.Inductive_rule.t =
  { identifier = "Rollup inner account step"
  ; prevs = [ force Action_state_extension.tag ]
  ; main = (fun x -> main x |> Run.run_checked)
  ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
  }
