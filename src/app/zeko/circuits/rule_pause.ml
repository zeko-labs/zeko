open Zeko_util
open Snark_params.Tick
open Rollup_state

module Witness = struct
  type t = { public_key : PC.t; vk_hash : F.t; pause_key : Even_PC.t }
  [@@deriving snarky]
end

module Make (Inputs : sig
  val chain_l1 : Mina_signature_kind.t
end) =
struct
  open Inputs

  let%snarkydef_ main (w : Witness.t V.t) =
    let* Witness.{ public_key; vk_hash; pause_key } =
      exists ~compute:(V.get w) Witness.typ
    in
    let signature_witness =
      { default_account_update with
        public_key = Even_PC.to_pc_var pause_key
      ; authorization_kind = authorization_signed ()
      ; use_full_commitment =
          Boolean.true_ (* added here too to be extra sure *)
      }
    in
    let* status_flags_update =
      Outer_state.Status_flags.of_bools_var ~paused:Boolean.true_
        ~emergency:Boolean.false_
    in
    let account_update =
      { default_account_update with
        public_key
      ; authorization_kind = authorization_vk_hash vk_hash
      ; update =
          { default_account_update.update with
            app_state =
              Outer_state.fine
                { pause_key = None
                ; status_flags = Some status_flags_update
                ; ledger_hash = None
                ; inner_action_state = { length = None; state = None }
                ; sequencer = None
                ; da_key = None
                ; acc_set = None
                }
              |> var_to_app_state_fine
          }
      ; preconditions =
          { default_account_update.preconditions with
            account =
              { default_account_update.preconditions.account with
                state =
                  Outer_state.fine
                    { pause_key = Some pause_key
                    ; status_flags = None
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
      make_outputs ~chain:chain_l1 account_update [ (signature_witness, []) ]
    in
    Compile_simple.{ prevs = No_prevs; out }

  let rule : _ Compile_simple.branch lazy_t =
    lazy { branch_name = "zeko pause"; tags = No_tags; main }
end
