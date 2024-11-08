open Zeko_util
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Rollup_state

module Witness = struct
  type t = { public_key : PC.t; vk_hash : F.t; pause_key : PC.t }
  [@@deriving snarky]
end

let%snarkydef_ main (w : Witness.t V.t) =
  let* Witness.{ public_key; vk_hash; pause_key } =
    exists ~compute:(V.get w) Witness.typ
  in
  let signature_witness =
    { default_account_update with
      public_key = pause_key
    ; authorization_kind = authorization_signed ()
    ; use_full_commitment = Boolean.true_ (* added here too to be extra sure *)
    }
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
              ; paused = Some Boolean.true_
              ; ledger_hash = None
              ; inner_action_state = { length = None; state = None }
              ; sequencer = None
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
                  ; paused = None
                  ; ledger_hash = None
                  ; inner_action_state = { length = None; state = None }
                  ; sequencer = None
                  }
                |> var_to_precondition_fine
            }
        }
    }
  in
  let*| out = make_outputs account_update [ (signature_witness, []) ] in
  Compile_simple.{ prevs = No_prevs; out }

let rule : _ Compile_simple.branch =
  { branch_name = "zeko action witness"; tags = No_tags; main }
