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

  val helper_token_owner_l1 : PC.t

  val holder_accounts_l1 : PC.t list

  val zeko_l1 : PC.t

  val chain_l1 : Mina_signature_kind.t

  val bridge_fee_recipient_l1 : PC.t

  val bridge_proof_fee : Currency.Amount.t

  val outer_account_creation_fee : Currency.Fee.t

  module Deposit_params : Bridge_state.DEPOSIT_PARAMS

  module Check_accepted :
      module type of
        Check_accepted_make.Make
          (struct
            let holder_accounts_l1 = holder_accounts_l1

            let token_owner_l1 = token_owner_l1

            module Deposit_params = Deposit_params

            let chain_l1 = chain_l1

            let bridge_fee_recipient_l1 = bridge_fee_recipient_l1

            let bridge_proof_fee = bridge_proof_fee
          end)
          ()
end)
() =
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

  (** Used to prove that there is a commit with some synchronized outer action state. *)
  module Ase_outer_inst = Ase.Without_length.Make (struct
    module Action_state = Rollup_state.Outer_action_state

    let get_iterations =
      Zeko_constants.Max_excess_actions.Finalize_cancelled_deposit.outer
  end)

  (** Used to prove that the synchronized outer action state is a predecessor of the current one. *)
  module Ase_outer_with_length_inst = Ase.With_length.Make (struct
    module Action_state = Rollup_state.Outer_action_state

    let get_iterations =
      Zeko_constants.Max_excess_actions.Finalize_cancelled_deposit
      .outer_with_length
  end)

  (** Exists to circumvent limit. *)
  module Verify_two_outer_ases = struct
    let main (w : (Ase_outer_inst.t * Ase_outer_with_length_inst.t) V.t) =
      let* outer, outer_with_length =
        exists ~compute:(V.get w)
          Typ.(Ase_outer_inst.typ * Ase_outer_with_length_inst.typ)
      in
      let* outer, verify_outer = Ase_outer_inst.get outer in
      let*| outer_with_length, verify_outer_with_length =
        Ase_outer_with_length_inst.get outer_with_length
      in
      Compile_simple.
        { prevs = Two_prevs (verify_outer, verify_outer_with_length)
        ; out = (outer, outer_with_length)
        }

    let rule : _ Compile_simple.branch lazy_t =
      lazy
        { branch_name = "Verify_two_outer_ases"
        ; tags =
            Two_tags
              (Lazy.force Ase.Without_length.tag, Lazy.force Ase.With_length.tag)
        ; main
        }

    include
      ( val Compile_simple.compile ~name:"Verify_both_ases" ~branches:[ rule ]
              ~out_typ:
                Typ.(
                  Ase_outer_inst.Stmt.typ * Ase_outer_with_length_inst.Stmt.typ)
              () )
  end

  module Check_accepted_params = struct
    let get_iterations =
      Zeko_constants.Max_excess_actions.Finalize_cancelled_deposit
      .check_accepted
  end

  module Check_accepted_inst = Check_accepted.Make (Check_accepted_params)

  (** We could do a Check_accepted alone, but the issue is that proving time
      for a user might be slower than the rate at which new actions get added,
      meaning the user would never catch up, preventing them from finalizing their
      cancelled deposit.
     *)
  module Verify_check_accepted_and_ase = struct
    let main (w : (Check_accepted_inst.t * Ase_outer_with_length_inst.t) V.t) =
      let* check_accepted, ase =
        exists ~compute:(V.get w)
          Typ.(Check_accepted_inst.typ * Ase_outer_with_length_inst.typ)
      in
      let* check_accepted, verify_check_accepted =
        Check_accepted_inst.get check_accepted
      in
      let*| ase, verify_ase = Ase_outer_with_length_inst.get ase in
      Compile_simple.
        { prevs = Two_prevs (verify_check_accepted, verify_ase)
        ; out = (check_accepted, ase)
        }

    let rule : _ Compile_simple.branch lazy_t =
      lazy
        { branch_name = "Verify_two_outer_ases"
        ; tags =
            Two_tags
              (Lazy.force Check_accepted.tag, Lazy.force Ase.With_length.tag)
        ; main
        }

    include
      ( val Compile_simple.compile ~name:"Verify_check_accepted_and_ase"
              ~wrap_domain:`N14 ~branches:[ rule ]
              ~out_typ:
                Typ.(
                  Check_accepted.Definition.Stmt.typ
                  * Ase_outer_with_length_inst.Stmt.typ)
              () )
  end

  module Witness = struct
    type t =
      { public_key : PC.t
      ; vk_hash : F.t
      ; may_use_token : May_use_token.t
      ; outer_authorization_kind : A.t
      ; commit : Rollup_state.Outer_action.Commit.t
      ; before_commit_ase : Rollup_state.Outer_action_state.t
      ; verify_two_outer_ases : Verify_two_outer_ases.t
      ; verify_check_accepted_and_ase : Verify_check_accepted_and_ase.t
      ; prev_next_cancelled_deposit : Checked32.t
      ; helper_token_owner_l1_vk_hash : F.t
      ; prev_nonce : Checked32.t
      ; helper_account_new : Boolean.t
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
               ; before_commit_ase
               ; verify_two_outer_ases
               ; verify_check_accepted_and_ase
               ; prev_next_cancelled_deposit
               ; helper_token_owner_l1_vk_hash
               ; prev_nonce
               ; helper_account_new
               } =
          exists Witness.typ ~compute:(V.get w)
        in
        let@ () = with_label __LOC__ in
        let* (commit_ase, sync_ase), verify_two_outer_ases =
          Verify_two_outer_ases.get verify_two_outer_ases
        in
        let@ () = with_label __LOC__ in
        let* ( ( ({ params
                  ; action_state = mid_outer_action_state'
                  ; deposit_index
                  ; n_steps
                  ; is_rejected
                  ; is_accepted
                  } :
                   Check_accepted.Definition.Stmt.var )
               , ({ source = mid_outer_action_state
                  ; target = outer_action_state
                  } :
                   Ase_outer_with_length_inst.Stmt.var ) )
             , verify_check_accepted_and_ase ) =
          Verify_check_accepted_and_ase.get verify_check_accepted_and_ase
        in
        let@ () = with_label __LOC__ in
        let helper_token_id =
          let account_id =
            Account_id.create helper_token_owner_l1 Token_id.default
          in
          Account_id.derive_token_id ~owner:account_id |> constant Token_id.typ
        in
        let@ () = with_label __LOC__ in
        let* () = Boolean.(Assert.is_true @@ not is_accepted) in
        let@ () = with_label __LOC__ in
        let* () = Boolean.Assert.is_true is_rejected in
        let@ () = with_label __LOC__ in
        let* () =
          assert_equal ~label:__LOC__ Rollup_state.Outer_action_state.typ
            (Rollup_state.Outer_action_state.With_length.state_var
               mid_outer_action_state )
            mid_outer_action_state'
        in
        let@ () = with_label __LOC__ in
        let* next_cancelled_deposit =
          Checked32.Checked.(
            sub
              (Rollup_state.Outer_action_state.With_length.length_var
                 mid_outer_action_state )
              n_steps)
        in
        let@ () = with_label __LOC__ in
        let* next_cancelled_deposit' = Checked32.Checked.succ deposit_index in
        let* () =
          assert_equal ~label:__LOC__ Checked32.typ next_cancelled_deposit
            next_cancelled_deposit'
        in
        let* () =
          assert_var __LOC__
            Checked32.Checked.(
              fun () -> prev_next_cancelled_deposit < next_cancelled_deposit)
        in
        let* () =
          let@ () = with_label __LOC__ in
          let* commit_ase_source' =
            Rollup_state.Outer_action.push_commit_var commit before_commit_ase
          in
          assert_equal ~label:__LOC__ Rollup_state.Outer_action_state.typ
            commit_ase.source commit_ase_source'
        in
        let* () =
          assert_equal ~label:__LOC__ Rollup_state.Outer_action_state.typ
            commit_ase.target
            (Rollup_state.Outer_action_state.With_length.state_var
               outer_action_state )
        in
        let* () =
          assert_equal ~label:__LOC__
            Rollup_state.Outer_action_state.With_length.typ
            commit.synchronized_outer_action_state sync_ase.source
        in
        let* () =
          assert_equal ~label:__LOC__
            Rollup_state.Outer_action_state.With_length.typ sync_ase.target
            outer_action_state
        in
        let@ () = with_label __LOC__ in
        let base_params = Deposit_params.base params in
        let helper_token_owner =
          { default_account_update with
            public_key = constant PC.typ helper_token_owner_l1
          ; authorization_kind =
              authorization_vk_hash helper_token_owner_l1_vk_hash
          }
        in
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
                  Outer_user_state.fine
                    { next_cancelled_deposit = Some next_cancelled_deposit
                    ; next_withdrawal = None
                    }
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
                      Outer_user_state.fine
                        { next_cancelled_deposit =
                            Some prev_next_cancelled_deposit
                        ; next_withdrawal = None
                        }
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
        let* status_flags_precondition =
          Rollup_state.Outer_state.Status_flags.of_bools_var
            ~paused:Boolean.false_ ~emergency:Boolean.false_
        in
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
                        ; status_flags = Some status_flags_precondition
                        ; ledger_hash = None
                        ; inner_action_state = { state = None; length = None }
                        ; sequencer = None
                        ; da_key = None
                        ; acc_set = None
                        }
                      |> var_to_precondition_fine
                  ; action_state =
                      Zkapp_basic.Or_ignore.Checked.make_unsafe Boolean.true_
                        (Rollup_state.Outer_action_state.With_length.raw_var
                           outer_action_state )
                  }
              }
          }
        in
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
        let bridge_proof_fee = constant Currency.Amount.typ bridge_proof_fee in
        let* recipient_payout =
          let* recipient_payout, `Underflow underflow =
            Currency.Amount.Checked.sub_flagged base_params.amount
              bridge_proof_fee
          in
          let* () = Boolean.Assert.is_true (Boolean.not underflow) in
          let account_creation_fee =
            constant Currency.Amount.typ
              (Currency.Amount.of_fee outer_account_creation_fee)
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
          ; token_id = constant Token_id.typ token_id_l1
          ; may_use_token = constant May_use_token.typ Parents_own_token
          ; authorization_kind = constant A.typ None_given
          ; balance_change =
              Currency.Amount.Signed.Checked.of_unsigned recipient_payout
          ; implicit_account_creation_fee = constant Boolean.typ true
          }
        in
        let sequencer_fee_payout =
          { default_account_update with
            public_key = constant PC.typ bridge_fee_recipient_l1
          ; token_id = constant Token_id.typ token_id_l1
          ; may_use_token = constant May_use_token.typ Parents_own_token
          ; authorization_kind = constant A.typ None_given
          ; balance_change =
              Currency.Amount.Signed.Checked.of_unsigned bridge_proof_fee
          ; implicit_account_creation_fee = constant Boolean.typ true
          }
        in
        let@ () = with_label __LOC__ in
        let*| out =
          make_outputs ~chain:chain_l1 account_update
            [ (helper_token_owner, [ (helper_account, []) ])
            ; (witness_outer, [])
            ; (recipient_payout, [])
            ; (sequencer_fee_payout, [])
            ]
        in
        Compile_simple.
          { prevs =
              Two_prevs (verify_two_outer_ases, verify_check_accepted_and_ase)
          ; out
          } )

  let rule : _ Compile_simple.branch lazy_t =
    lazy
      { branch_name = "finalize cancelled deposit"
      ; tags =
          Two_tags
            ( Lazy.force Verify_two_outer_ases.tag
            , Lazy.force Verify_check_accepted_and_ase.tag )
      ; main
      }
end
