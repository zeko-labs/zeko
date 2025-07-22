open Core_kernel
open Mina_base
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Bridge_state
open Zeko_util

module A = struct
  include Account_update.Authorization_kind

  type var = Checked.t
end

module Make (Inputs : sig
  val token_owner_l1 : Account_id.t option

  val token_owner_l2 : Account_id.t option

  val helper_token_owner_l1 : PC.t

  val zeko_l1 : PC.t

  module Withdrawal_params : Bridge_state.WITHDRAWAL_PARAMS

  val holder_account_l2 : PC.t

  val withdrawal_delay : Mina_numbers.Global_slot_span.t

  val chain_l1 : Mina_signature_kind.t

  val chain_l2 : Mina_signature_kind.t
end) =
struct
  open Inputs

  let token_id_l1 = token_owner_id token_owner_l1

  module Token_id = struct
    include Token_id

    type var = Checked.t
  end

  module May_use_token = struct
    include Account_update.May_use_token

    type var = Checked.t
  end

  module Ase_outer_inst = Ase.Without_length.Make (struct
    module Action_state = Rollup_state.Outer_action_state

    let get_iterations =
      Zeko_constants.Max_excess_actions.Finalize_withdrawal.outer
  end)

  module Ase_inner_inst = Ase.With_length.Make (struct
    module Action_state = Rollup_state.Inner_action_state

    let get_iterations =
      Zeko_constants.Max_excess_actions.Finalize_withdrawal.inner
  end)

  module Witness = struct
    type t =
      { public_key : PC.t
      ; vk_hash : F.t
      ; may_use_token : May_use_token.t
      ; outer_authorization_kind : A.t
      ; commit : Rollup_state.Outer_action.Commit.t
      ; before_commit : Rollup_state.Outer_action_state.t
      ; commit_ase : Ase_outer_inst.t
      ; before_withdrawal : Rollup_state.Inner_action_state.t
      ; withdrawal_ase : Ase_inner_inst.t
      ; prev_next_withdrawal : Checked32.t
      ; withdrawal_params : Withdrawal_params.t
      ; helper_token_owner_l1_vk_hash : F.t
      ; inner_vk_hash : F.t
      }
    [@@deriving snarky]
  end

  let main (w : Witness.t V.t) =
    with_label ("main " ^ __LOC__) (fun () ->
        let* Witness.
               { public_key
               ; vk_hash
               ; may_use_token
               ; outer_authorization_kind
               ; commit
               ; before_commit
               ; commit_ase
               ; before_withdrawal
               ; withdrawal_ase
               ; prev_next_withdrawal
               ; withdrawal_params
               ; helper_token_owner_l1_vk_hash
               ; inner_vk_hash
               } =
          exists Witness.typ ~compute:(V.get w)
        in
        let@ () = with_label __LOC__ in
        let* commit_ase, verify_commit_ase = Ase_outer_inst.get commit_ase in
        let@ () = with_label __LOC__ in
        let* withdrawal_ase, verify_withdrawal_ase =
          Ase_inner_inst.get withdrawal_ase
        in
        let@ () = with_label __LOC__ in
        let helper_token_id =
          let account_id =
            Account_id.create helper_token_owner_l1 Token_id.default
          in
          Account_id.derive_token_id ~owner:account_id |> constant Token_id.typ
        in
        let@ () = with_label __LOC__ in
        (* make sure that withdrawal ase is connected to withdrawal *)
        let* () =
          let* action =
            withdrawal_action ~chain_l2 ~holder_account_l2 ~token_owner_l2
              ~inner_vk_hash
              (module Withdrawal_params)
              withdrawal_params
          in
          let* withdrawal_ase_source' =
            Rollup_state.Inner_action.push_var action before_withdrawal
          in
          assert_equal ~label:__LOC__ Rollup_state.Inner_action_state.typ
            (Rollup_state.Inner_action_state.With_length.state_var
               withdrawal_ase.source )
            withdrawal_ase_source'
        in
        let@ () = with_label __LOC__ in
        let next_withdrawal =
          Rollup_state.Inner_action_state.With_length.length_var
            withdrawal_ase.source
        in
        let* () =
          assert_var __LOC__
            Checked32.Checked.(fun () -> prev_next_withdrawal < next_withdrawal)
        in
        let@ () = with_label __LOC__ in
        (* make sure that commit ase is connected to commit *)
        let* () =
          let* commit_ase_source' =
            Rollup_state.Outer_action.push_commit_var commit before_commit
          in
          assert_equal ~label:__LOC__ Rollup_state.Outer_action_state.typ
            commit_ase.source commit_ase_source'
        in
        let* () =
          assert_equal ~label:__LOC__
            Rollup_state.Inner_action_state.With_length.typ
            commit.inner_action_state withdrawal_ase.target
        in
        let@ () = with_label __LOC__ in
        let base_params = Withdrawal_params.base withdrawal_params in
        let@ () = with_label __LOC__ in
        let helper_token_owner =
          { default_account_update with
            public_key = constant PC.typ helper_token_owner_l1
          ; authorization_kind =
              authorization_vk_hash helper_token_owner_l1_vk_hash
          }
        in
        let@ () = with_label __LOC__ in
        let helper_account =
          { default_account_update with
            public_key = base_params.recipient
          ; token_id = helper_token_id
          ; authorization_kind = authorization_signed ()
          ; use_full_commitment = Boolean.true_
          ; may_use_token = constant May_use_token.typ Parents_own_token
          ; update =
              { default_account_update.update with
                app_state =
                  Outer_user_state.fine
                    { next_cancelled_deposit = None
                    ; next_withdrawal = Some next_withdrawal
                    }
                  |> var_to_app_state_fine
              }
          ; preconditions =
              { default_account_update.preconditions with
                account =
                  { default_account_update.preconditions.account with
                    state =
                      Outer_user_state.fine
                        { next_cancelled_deposit = None
                        ; next_withdrawal = Some prev_next_withdrawal
                        }
                      |> var_to_precondition_fine
                  }
              }
          }
        in
        let@ () = with_label __LOC__ in
        let* lower =
          Slot.Checked.add commit.slot_range.upper
            (constant Mina_numbers.Global_slot_span.typ withdrawal_delay)
        in
        let@ () = with_label __LOC__ in
        let witness_outer =
          { default_account_update with
            public_key = constant PC.typ zeko_l1
          ; authorization_kind = outer_authorization_kind
          ; preconditions =
              { default_account_update.preconditions with
                account =
                  { default_account_update.preconditions.account with
                    state =
                      Rollup_state.Outer_state.fine
                        { pause_key = None
                        ; paused = Some Boolean.false_ (* must not be paused *)
                        ; ledger_hash = None
                        ; inner_action_state = { state = None; length = None }
                        ; sequencer = None
                        ; da_key = None
                        ; acc_set = None
                        }
                      |> var_to_precondition_fine
                  ; action_state =
                      Zkapp_basic.Or_ignore.Checked.make_unsafe Boolean.true_
                        (Rollup_state.Outer_action_state.raw_var
                           commit_ase.target )
                  }
              ; valid_while =
                  Slot_range.Checked.to_valid_while
                    { lower; upper = constant Slot.typ Slot.max_value }
              }
          }
        in
        let@ () = with_label __LOC__ in
        let account_update =
          { default_account_update with
            public_key
          ; token_id = constant Token_id.typ token_id_l1
          ; may_use_token
          ; authorization_kind = authorization_vk_hash vk_hash
          ; balance_change =
              Currency.Amount.Signed.Checked.(
                of_unsigned base_params.amount |> negate)
          }
        in
        let@ () = with_label __LOC__ in
        let*| out =
          make_outputs ~chain:chain_l1 account_update
            [ (helper_token_owner, [ (helper_account, []) ])
            ; (witness_outer, [])
            ]
        in
        Compile_simple.
          { prevs = Two_prevs (verify_commit_ase, verify_withdrawal_ase); out } )

  let rule : _ Compile_simple.branch =
    { branch_name = "finalize withdrawal"
    ; tags = Two_tags (Ase.Without_length.tag, Ase.With_length.tag)
    ; main
    }
end
