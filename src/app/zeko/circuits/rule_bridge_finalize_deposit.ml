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

  val holder_accounts_l1 : PC.t list

  module Deposit_params : DEPOSIT_PARAMS

  val zeko_l2 : PC.t

  val bridge_proof_fee : Currency.Amount.t

  val bridge_fee_recipient_l2 : PC.t

  val chain_l1 : Mina_signature_kind.t

  val chain_l2 : Mina_signature_kind.t

  module Check_accepted :
      module type of
        Check_accepted_make.Make
          (struct
            let holder_accounts_l1 = holder_accounts_l1

            let token_owner_l1 = token_owner_l1

            module Deposit_params = Deposit_params

            let chain_l1 = chain_l1
          end)
          ()
end) =
struct
  open Inputs

  let token_id_l2 = token_owner_id token_owner_l2

  module Token_id = struct
    include Token_id

    type var = Checked.t
  end

  module May_use_token = struct
    include Account_update.May_use_token

    type var = Checked.t
  end

  module Check_accepted_params = struct
    let get_iterations =
      Zeko_constants.Max_excess_actions.Finalize_deposit.check_accepted
  end

  module Check_accepted_inst = Check_accepted.Make (Check_accepted_params)

  module Ase_inst = Ase.With_length.Make (struct
    module Action_state = Rollup_state.Outer_action_state

    let get_iterations =
      Zeko_constants.Max_excess_actions.Finalize_deposit.outer
  end)

  module Witness = struct
    type t =
      { public_key : PC.t
      ; vk_hash : F.t
      ; may_use_token : May_use_token.t
      ; inner_authorization_kind : A.t
      ; ase : Ase_inst.t
      ; check_accepted : Check_accepted_inst.t
      ; prev_next_deposit : Checked32.t
      }
    [@@deriving snarky]
  end

  (** Prove that we have submitted a deposit, and that it's been accepted. *)
  let main (w : Witness.t V.t) =
    let@ () = with_label ("main " ^ __LOC__) in
    let* Witness.
           { public_key
           ; vk_hash
           ; may_use_token
           ; inner_authorization_kind
           ; ase
           ; check_accepted
           ; prev_next_deposit
           } =
      exists Witness.typ ~compute:(V.get w)
    in
    let* ( { params
           ; action_state = mid_outer_action_state'
           ; deposit_index
           ; n_steps
           ; is_rejected
           ; is_accepted
           }
         , verify_check_accepted ) =
      Check_accepted_inst.get check_accepted
    in
    let@ () = with_label __LOC__ in
    let* helper_token_id =
      make_checked
      @@ fun () ->
      let account_id =
        Account_id.Checked.create public_key (constant Token_id.typ token_id_l2)
      in
      Account_id.Checked.derive_token_id ~owner:account_id
    in
    let@ () = with_label __LOC__ in
    let* () = Boolean.Assert.is_true is_accepted in
    let@ () = with_label __LOC__ in
    let* () = Boolean.(Assert.is_true (not is_rejected)) in
    let@ () = with_label __LOC__ in
    let* ( { source = mid_outer_action_state; target = outer_action_state }
         , verify_ase ) =
      Ase_inst.get ase
    in
    let* () =
      assert_equal ~label:__LOC__ Rollup_state.Outer_action_state.typ
        (Rollup_state.Outer_action_state.With_length.state_var
           mid_outer_action_state )
        mid_outer_action_state'
    in
    let@ () = with_label __LOC__ in
    let* next_deposit =
      Checked32.Checked.(
        sub
          (Rollup_state.Outer_action_state.With_length.length_var
             mid_outer_action_state )
          n_steps)
    in
    let@ () = with_label __LOC__ in
    let* next_deposit' = Checked32.Checked.succ deposit_index in
    let* () =
      assert_equal ~label:__LOC__ Checked32.typ next_deposit next_deposit'
    in
    let* () =
      assert_var __LOC__
        Checked32.Checked.(fun () -> prev_next_deposit < next_deposit)
    in
    let@ () = with_label __LOC__ in
    let base_params = Deposit_params.base params in
    let@ () = with_label __LOC__ in
    let helper_account =
      { default_account_update with
        public_key = base_params.recipient
      ; token_id = helper_token_id
      ; authorization_kind = authorization_signed ()
      ; use_full_commitment = Boolean.false_
      ; may_use_token = constant May_use_token.typ Parents_own_token
      ; implicit_account_creation_fee = constant Boolean.typ false
      ; update =
          { default_account_update.update with
            app_state =
              Inner_user_state.fine { next_deposit = Some next_deposit }
              |> var_to_app_state_fine
          }
      ; preconditions =
          { default_account_update.preconditions with
            account =
              { default_account_update.preconditions.account with
                state =
                  Inner_user_state.fine
                    { next_deposit = Some prev_next_deposit }
                  |> var_to_precondition_fine
              }
          }
      }
    in
    let@ () = with_label __LOC__ in
    let witness_inner =
      { default_account_update with
        public_key = constant PC.typ zeko_l2
      ; authorization_kind = inner_authorization_kind
      ; preconditions =
          { default_account_update.preconditions with
            account =
              { default_account_update.preconditions.account with
                state =
                  Rollup_state.Inner_state.fine
                    { outer_action_state =
                        { state =
                            Some
                              (Rollup_state.Outer_action_state.With_length
                               .state_var outer_action_state )
                        ; length =
                            Some
                              (Rollup_state.Outer_action_state.With_length
                               .length_var outer_action_state )
                        }
                    }
                  |> var_to_precondition_fine
              }
          }
      }
    in
    let@ () = with_label __LOC__ in
    let* events =
      var_to_events
        Typ.(Checked32.typ * Deposit_params.typ)
        (deposit_index, params)
    in
    let account_update =
      { default_account_update with
        public_key
      ; token_id = constant Token_id.typ token_id_l2
      ; may_use_token
      ; authorization_kind = authorization_vk_hash vk_hash
      ; balance_change =
          Currency.Amount.Signed.Checked.(
            of_unsigned base_params.amount |> negate)
      ; events
      }
    in
    let fee_balance_change =
      Currency.Amount.Signed.Checked.of_unsigned
        (constant Currency.Amount.typ bridge_proof_fee)
    in
    let* recipient_balance_change =
      Currency.Amount.Signed.Checked.add
        (Currency.Amount.Signed.Checked.of_unsigned base_params.amount)
        (Currency.Amount.Signed.Checked.negate fee_balance_change)
    in
    let recipient_payout =
      { default_account_update with
        public_key = base_params.recipient
      ; token_id = constant Token_id.typ token_id_l2
      ; may_use_token = constant May_use_token.typ Parents_own_token
      ; authorization_kind = constant A.typ None_given
      ; balance_change = recipient_balance_change
      ; implicit_account_creation_fee = constant Boolean.typ false
      }
    in
    let sequencer_fee_payout =
      { default_account_update with
        public_key = constant PC.typ bridge_fee_recipient_l2
      ; token_id = constant Token_id.typ token_id_l2
      ; may_use_token = constant May_use_token.typ Parents_own_token
      ; authorization_kind = constant A.typ None_given
      ; balance_change = fee_balance_change
      ; implicit_account_creation_fee = constant Boolean.typ false
      }
    in
    let@ () = with_label __LOC__ in
    let*| out =
      make_outputs ~chain:chain_l2 account_update
        [ (helper_account, [])
        ; (witness_inner, [])
        ; (recipient_payout, [])
        ; (sequencer_fee_payout, [])
        ]
    in
    Compile_simple.
      { prevs = Two_prevs (verify_check_accepted, verify_ase); out }

  let rule : _ Compile_simple.branch lazy_t =
    lazy
      { branch_name = "finalize deposit"
      ; tags =
          Two_tags
            (Lazy.force Check_accepted.tag, Lazy.force Ase.With_length.tag)
      ; main
      }
end
