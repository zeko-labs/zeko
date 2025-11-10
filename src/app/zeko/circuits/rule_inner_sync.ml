open Rollup_state
open Zeko_util
open Snark_params.Tick

module Ase_inst = Ase.With_length.Make (struct
  module Action_state = Outer_action_state

  let get_iterations = Zeko_constants.Max_excess_actions.Inner_sync.outer
end)

module Witness = struct
  type t =
    { public_key : Signature_lib.Public_key.Compressed.t
    ; vk_hash : F.t
    ; ase : Ase_inst.t
    }
  [@@deriving snarky]
end

module Make (Inputs : sig
  val chain_l2 : Mina_signature_kind.t
end) =
struct
  open Inputs

  let%snarkydef_ main (w : Witness.t V.t) =
    let* Witness.{ public_key; vk_hash; ase } =
      exists ~compute:(V.get w) Witness.typ
    in
    let* ase, verify_ase = Ase_inst.get ase in
    let update =
      { default_account_update.update with
        app_state =
          Inner_state.fine
            { outer_action_state =
                { length =
                    Some (Outer_action_state.With_length.length_var ase.target)
                ; state =
                    Some (Outer_action_state.With_length.state_var ase.target)
                }
                (* This is equal to outer action state and is checked in outer account rule *)
            }
          |> var_to_app_state_fine
      }
    in
    let preconditions =
      { default_account_update.preconditions with
        account =
          { default_account_update.preconditions.account with
            state =
              Inner_state.fine
                { outer_action_state =
                    { state =
                        Some
                          (Outer_action_state.With_length.state_var ase.source)
                    ; length = None
                    }
                }
              |> var_to_precondition_fine
          }
      }
    in
    let account_update =
      { default_account_update with
        public_key
      ; authorization_kind = authorization_vk_hash vk_hash
      ; update
      ; preconditions
      }
    in
    let*| out = make_outputs ~chain:chain_l2 account_update [] in
    Compile_simple.{ prevs = One_prev verify_ase; out }

  let rule : _ Compile_simple.branch lazy_t =
    lazy
      { branch_name = "Rollup inner account step"
      ; tags = One_tag (Lazy.force Ase.With_length.tag)
      ; main
      }
end
