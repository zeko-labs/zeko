open Core_kernel
open Rollup_state
open Zeko_util
open Snark_params.Tick

module Ase_inst = Ase.Make_with_length (struct
  module Action_state = Outer_action_state
  module Action = Outer.Action

  let get_iterations = Int.pow 2 14
end)

module Witness = struct
  type t = { vk_hash : F.t; ase : Ase_inst.t } [@@deriving snarky]
end

include MkHandler (Witness)

let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
  let* Witness.{ vk_hash; ase } = exists_witness in
  let* ase, verify_ase = Ase_inst.get ase in
  let update =
    { default_account_update.update with
      app_state =
        Inner.State.(var_to_app_state typ { outer_action_state = ase.target })
        (* This is equal to outer action state and is checked in outer account rule *)
    }
  in
  let preconditions =
    { default_account_update.preconditions with
      account =
        { default_account_update.preconditions.account with
          state =
            Inner.State.fine
              { outer_action_state =
                  { state =
                      Some (Outer_action_state.With_length.state_var ase.source)
                  ; length = None
                  }
              }
            |> var_to_precondition_fine
        }
    }
  in
  let account_update =
    { default_account_update with
      public_key =
        constant Signature_lib.Public_key.Compressed.typ Inner.public_key
    ; authorization_kind = authorization_vk_hash vk_hash
    ; update
    ; preconditions
    }
  in
  let* public_output, auxiliary_output = make_outputs account_update [] in
  let*| auxiliary_output = V.create auxiliary_output in
  Pickles.Inductive_rule.
    { previous_proof_statements = [ verify_ase ]
    ; public_output
    ; auxiliary_output
    }

let rule () : _ Pickles.Inductive_rule.t =
  { identifier = "Rollup inner account step"
  ; prevs = [ force Ase.tag_with_length ]
  ; main = (fun x -> main x |> Run.run_checked)
  ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
  }
