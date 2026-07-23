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

  val ethereum_holder_account_l1 : PC.t option

  module Deposit_params : DEPOSIT_PARAMS

  val zeko_l2 : PC.t

  val bridge_proof_fee : Currency.Amount.t

  val bridge_fee_recipient_l1 : PC.t

  val bridge_fee_recipient_l2 : PC.t

  val chain_l1 : Mina_signature_kind.t

  val chain_l2 : Mina_signature_kind.t
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
      ; params : Deposit_params.t
      ; original_action_state : Rollup_state.Outer_action_state.t
      ; deposit_index : Checked32.t
      ; prev_next_deposit : Checked32.t
      ; prev_nonce : Checked32.t
      ; helper_account_new : Boolean.t
      }
    [@@deriving snarky]
  end

  (** Prove that an Ethereum deposit is part of the outer-action prefix already
      synchronized into the inner account. Canonical Ethereum deposits have no
      cancellation path, so neither a later commit nor the deposit timeout is
      relevant to their L2 finalization. *)
  let main (w : Witness.t V.t) =
    let@ () = with_label ("main " ^ __LOC__) in
    let* Witness.
           { public_key
           ; vk_hash
           ; may_use_token
           ; inner_authorization_kind
           ; ase
           ; params
           ; original_action_state
           ; deposit_index
           ; prev_next_deposit
           ; prev_nonce
           ; helper_account_new
           } =
      exists Witness.typ ~compute:(V.get w)
    in
    let* ( { source = deposit_action_state; target = outer_action_state }
         , verify_ase ) =
      Ase_inst.get ase
    in
    let ethereum_holder_account_l1 =
      match ethereum_holder_account_l1 with
      | Some holder ->
          Some holder
      | None ->
          failwith
            "the Ethereum deposit-finalization circuit requires an Ethereum \
             holder account"
    in
    let* deposit =
      Bridge_state.deposit_action ~chain_l1
        ~bridge_fee_recipient_l1:(constant PC.typ bridge_fee_recipient_l1)
        ~bridge_proof_fee:(constant Currency.Amount.typ bridge_proof_fee)
        ~holder_accounts_l1 ~token_owner_l1 ~ethereum_holder_account_l1
        (module Deposit_params)
        params
    in
    let* expected_deposit_action_state =
      Rollup_state.Outer_action.push_witness_var deposit original_action_state
    in
    let* () =
      assert_equal ~label:__LOC__ Rollup_state.Outer_action_state.typ
        (Rollup_state.Outer_action_state.With_length.state_var
           deposit_action_state )
        expected_deposit_action_state
    in
    let* next_deposit = Checked32.Checked.succ deposit_index in
    let* () =
      assert_equal ~label:__LOC__ Checked32.typ
        (Rollup_state.Outer_action_state.With_length.length_var
           deposit_action_state )
        next_deposit
    in
    let* () =
      assert_var __LOC__
        Checked32.Checked.(fun () -> prev_next_deposit < next_deposit)
    in
    let* helper_token_id =
      make_checked
      @@ fun () ->
      let account_id =
        Account_id.Checked.create public_key (constant Token_id.typ token_id_l2)
      in
      Account_id.Checked.derive_token_id ~owner:account_id
    in
    let base_params = Deposit_params.base params in
    let prev_nonce =
      Mina_numbers.Account_nonce.Checked.Unsafe.of_field
        (Checked32.Checked.to_field prev_nonce)
    in
    let helper_account =
      { default_account_update with
        public_key = base_params.recipient
      ; token_id = helper_token_id
      ; authorization_kind = authorization_signed ()
      ; use_full_commitment = Boolean.false_
      ; increment_nonce = Boolean.true_
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
                is_new =
                  Zkapp_basic.Or_ignore.Checked.make_unsafe Boolean.true_
                    helper_account_new
              ; state =
                  Inner_user_state.fine
                    { next_deposit = Some prev_next_deposit }
                  |> var_to_precondition_fine
              ; nonce =
                  Zkapp_basic.Or_ignore.Checked.make_unsafe Boolean.true_
                    { Zkapp_precondition.Closed_interval.lower = prev_nonce
                    ; upper = prev_nonce
                    }
              }
          }
      }
    in
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
    let bridge_proof_fee = constant Currency.Amount.typ bridge_proof_fee in
    let* recipient_payout =
      let* recipient_payout, `Underflow underflow =
        Currency.Amount.Checked.sub_flagged base_params.amount bridge_proof_fee
      in
      let* () = Boolean.Assert.is_true (Boolean.not underflow) in
      let account_creation_fee =
        constant Currency.Amount.typ
          (Currency.Amount.of_fee
             Zeko_constants.constraint_constants.account_creation_fee )
      in
      let* paid_for_helper_account_creation, `Underflow underflow =
        Currency.Amount.Checked.sub_flagged recipient_payout
          account_creation_fee
      in
      let* () = Boolean.Assert.is_true (Boolean.not underflow) in
      if_ ~typ:Currency.Amount.typ helper_account_new
        ~then_:paid_for_helper_account_creation ~else_:recipient_payout
    in
    let recipient_payout =
      { default_account_update with
        public_key = base_params.recipient
      ; token_id = constant Token_id.typ token_id_l2
      ; may_use_token = constant May_use_token.typ Parents_own_token
      ; authorization_kind = constant A.typ None_given
      ; balance_change =
          Currency.Amount.Signed.Checked.of_unsigned recipient_payout
      ; implicit_account_creation_fee = constant Boolean.typ true
      }
    in
    let sequencer_fee_payout =
      { default_account_update with
        public_key = constant PC.typ bridge_fee_recipient_l2
      ; token_id = constant Token_id.typ token_id_l2
      ; may_use_token = constant May_use_token.typ Parents_own_token
      ; authorization_kind = constant A.typ None_given
      ; balance_change =
          Currency.Amount.Signed.Checked.of_unsigned bridge_proof_fee
      ; implicit_account_creation_fee = constant Boolean.typ true
      }
    in
    let*| out =
      make_outputs ~chain:chain_l2 account_update
        [ (helper_account, [])
        ; (witness_inner, [])
        ; (recipient_payout, [])
        ; (sequencer_fee_payout, [])
        ]
    in
    Compile_simple.{ prevs = One_prev verify_ase; out }

  let rule : _ Compile_simple.branch lazy_t =
    lazy
      { branch_name = "finalize ethereum deposit"
      ; tags = One_tag (Lazy.force Ase.With_length.tag)
      ; main
      }
end
