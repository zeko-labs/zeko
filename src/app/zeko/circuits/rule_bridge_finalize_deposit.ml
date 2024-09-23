open Mina_base
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Bridge_inner
open Zeko_util

module Token_id = struct
  include Token_id

  type var = Checked.t
end

module May_use_token = struct
  include Account_update.May_use_token

  type var = Checked.t
end

module Witness = struct
  type t =
    { token_id : Token_id.t
    ; public_key : PC.t
    ; vk_hash : F.t
    ; params : Deposit_params.t
    ; may_use_token : May_use_token.t
    ; inner_authorization_kind : A.t
    ; outer_action_state : Outer.Action.State.t
    }
  [@@deriving snarky]
end

include MkHandler (Witness)
module Len = Ase_lengthed.Len

module Check_accepted = struct
  module Stmt = struct
    type t =
      { deposit : Deposit.t
      ; idx : Len.t
      ; action_state : Outer.Action.State.t
      ; len : Len.t
      ; is_rejected : Boolean.t
      ; is_accepted : Boolean.t
      }
    [@@deriving snarky]
  end

  module Elem = Outer.Action.t
  module ElemOption = Outer.Action.t

  let elem_to_option x = x

  let elem_option_none = failwith "FIXME"

  module Init = Stmt

  let init ~check:_ x = failwith "FIXME"

  let step ~check:_ actions ({ action_state; len } : Stmt.var) =
    let*| len = Len.Checked.succ len in
    Stmt.
      { action_state =
          Zkapp_account.Actions.push_events_checked action_state actions
      ; len
      }

  let step_option ~check:_ actions ({ action_state; len } : Stmt.var) =
    let* dummy_ref = As_prover.Ref.create (As_prover.return []) in
    (* Bad unsafe use, with mismatching data and hash, but it works *)
    let actions = Data_as_hash.make_unsafe actions dummy_ref in
    let* is_dummy =
      Field.Checked.equal
        (Data_as_hash.hash actions)
        (constant Field.typ Outside_hash_image.t)
    in
    let* action_state =
      Field.Checked.if_ is_dummy ~then_:action_state
        ~else_:(Zkapp_account.Actions.push_events_checked action_state actions)
    in
    let*| len = Len.Checked.succ_if len (Boolean.not is_dummy) in
    Stmt.{ action_state; len }

  let name = "deposit acceptance/rejection check"

  let leaf_iterations = Int.pow 2 16

  let leaf_option_iterations = Int.pow 2 15

  let extend_iterations = Int.pow 2 15

  let extend_option_iterations = Int.pow 2 14

  let override_wrap_domain = None
end

(* FIXME: Doesn't check action state *)
let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
  let* Witness.
         { token_id
         ; public_key
         ; vk_hash
         ; params
         ; may_use_token
         ; inner_authorization_kind
         ; outer_action_state
         } =
    exists_witness
  in
  let account_update =
    { default_account_update with
      public_key
    ; token_id
    ; may_use_token
    ; authorization_kind = authorization_vk_hash vk_hash
    ; balance_change =
        Currency.Amount.Signed.Checked.(
          of_unsigned params.deposit.amount |> negate)
    }
  in
  let account_id = Account_id.Checked.create public_key token_id in
  let our_token_id = Account_id.Checked.derive_token_id ~owner:account_id in
  let* next_deposit = exists Next_deposit.typ in
  let* prev_next_deposit = exists Next_deposit.typ in
  let helper_account =
    { default_account_update with
      public_key = params.deposit.recipient
    ; token_id = our_token_id
    ; authorization_kind = authorization_signed ()
    ; use_full_commitment = Boolean.true_
    ; may_use_token = constant May_use_token.typ Parents_own_token
    ; update =
        { default_account_update.update with
          app_state = Inner_user_state.(var_to_app_state typ { next_deposit })
        }
    ; preconditions =
        { default_account_update.preconditions with
          account =
            { default_account_update.preconditions.account with
              state =
                Inner_user_state.to_precondition
                  { next_deposit = Some prev_next_deposit }
            }
        }
    }
  in
  let witness_inner =
    { default_account_update with
      public_key = constant PC.typ Inner.public_key
    ; authorization_kind = inner_authorization_kind
    ; preconditions =
        { default_account_update.preconditions with
          account =
            { default_account_update.preconditions.account with
              state =
                Inner.State.to_precondition
                  { outer_action_state =
                      Outer.Action.State.to_field_var outer_action_state |> Some
                  }
            }
        }
    }
  in
  let* public_output, auxiliary_output =
    make_outputs account_update [ (helper_account, []); (witness_inner, []) ]
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
