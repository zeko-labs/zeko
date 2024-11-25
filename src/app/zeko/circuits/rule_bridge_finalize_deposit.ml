open Core_kernel
open Mina_base
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Bridge_state
open Zeko_util
open Checked.Let_syntax

module A = struct
  include Account_update.Authorization_kind

  type var = Checked.t
end

module Make (Inputs : sig
  val token_owner_l1 : Account_id.t option

  val token_owner_l2 : Account_id.t option

  val holder_accounts_l1 : PC.t list

  val zeko_l1 : PC.t

  module Deposit_params : sig
    include SnarkType

    val base : var -> Deposit_params_base.var

    val custom : var -> Deposit_params_custom.var option
  end

  module Withdrawal_params : sig
    include SnarkType

    val base : var -> Withdrawal_params_base.var

    val custom : var -> Withdrawal_params_custom.var option
  end

  val zeko_l2 : PC.t

  val holder_account_l2 : PC.t

  val withdrawal_delay : Mina_numbers.Global_slot_span.t

  val holder_account_l1_permissions_enabled : Mina_base.Permissions.t

  val holder_account_l1_permissions_disabled : Mina_base.Permissions.t
end)
() =
struct
  open Inputs

  let token_id_l1 =
    match token_owner_l1 with
    | Some owner ->
        Account_id.derive_token_id ~owner
    | None ->
        Token_id.default

  let token_id_l2 =
    match token_owner_l2 with
    | Some owner ->
        Account_id.derive_token_id ~owner
    | None ->
        Token_id.default

  module Token_id = struct
    include Token_id

    type var = Checked.t
  end

  module May_use_token = struct
    include Account_update.May_use_token

    type var = Checked.t
  end

  let deposit_action (params : Deposit_params.var) :
      Rollup_state.Outer_action.Witness.var Checked.t =
    (* The chosen account must be one of the valid holder accounts.
       NB: If we invalidate an account later on,
       a yet unfinalized deposit will be made unfinalizable.
       Adding an account is however not a problem.
    *)
    let base_params = Deposit_params.base params in
    let* () =
      Checked.List.map
        ~f:(fun holder_account_l1' ->
          constant PC.typ holder_account_l1'
          |> PC.Checked.equal base_params.holder_account_l1 )
        holder_accounts_l1
      >>= Boolean.Assert.any
    in
    let a =
      { default_account_update with
        public_key = base_params.holder_account_l1
      ; token_id = constant Token_id.typ token_id_l1
      ; balance_change =
          Currency.Amount.Signed.Checked.of_unsigned base_params.amount
      ; may_use_token = constant May_use_token.typ Parents_own_token
      ; authorization_kind =
          constant Account_update.Authorization_kind.typ None_given
      }
    in
    let a', (children : Calls.t) =
      match token_owner_l1 with
      | None ->
          (a, [])
      | Some token_owner_l1 ->
          let custom_params =
            Deposit_params.custom params
            |> Option.value_exn
                 ~message:
                   "If token_id isn't default, then Deposit_params_custom must \
                    be used."
          in
          ( { default_account_update with
              public_key =
                Account_id.public_key token_owner_l1 |> constant PC.typ
            ; token_id =
                Account_id.token_id token_owner_l1 |> constant Token_id.typ
            ; authorization_kind = custom_params.authorization_kind
            ; call_data = custom_params.call_data
            }
          , (a, []) :: Raw custom_params.nested_children )
    in
    let* children' = Calls.hash ((a', children) :: Raw base_params.children) in
    let hash_prefix = Hash_prefix_create.salt "Deposit_params - qFB3jXP*)" in
    let* aux = var_to_hash ~init:hash_prefix Deposit_params.typ params in
    Checked.return
      ( { aux
        ; children = children'
        ; slot_range = constant Slot_range.typ Slot_range.infinite
        }
        : Rollup_state.Outer_action.Witness.var )

  module Check_accepted_definition = struct
    module Stmt = struct
      type t =
        { params : Deposit_params.t
        ; action_state : Rollup_state.Outer_action_state.t
        ; deposit_index : Checked32.t
        ; n_steps : Checked32.t
        ; is_rejected : Boolean.t
        ; is_accepted : Boolean.t
        }
      [@@deriving snarky]
    end

    module Elem = Rollup_state.Outer_action

    let dummy_elem =
      Rollup_state.Outer_action.Witness
        { aux = Field.zero; children = []; slot_range = Slot_range.infinite }

    module Init = struct
      type t =
        { params : Deposit_params.t
        ; original_action_state : Rollup_state.Outer_action_state.t
        ; deposit_index : Checked32.t
        }
      [@@deriving snarky]
    end

    let init ~check:_
        ({ params; original_action_state; deposit_index } : Init.var) :
        Stmt.var Checked.t =
      let* witness = deposit_action params in
      let* action_state =
        Rollup_state.Outer_action.push_witness_var witness original_action_state
      in
      Checked.return
        ( { params
          ; action_state
          ; deposit_index
          ; n_steps = Checked32.Checked.zero
          ; is_rejected = Boolean.false_
          ; is_accepted = Boolean.false_
          }
          : Stmt.var )

    let step (action : Rollup_state.Outer_action.var)
        ({ params
         ; action_state
         ; n_steps
         ; is_rejected
         ; is_accepted
         ; deposit_index
         } :
          Stmt.var ) =
      let* n_steps = Checked32.Checked.succ n_steps in
      let* action_state =
        Rollup_state.Outer_action.push_var action action_state
      in
      let* valid_while =
        if_ ~typ:Slot_range.typ action.is_witness
          ~then_:action.case_witness.slot_range
          ~else_:action.case_commit.slot_range
      in
      let base_params = Deposit_params.base params in
      let open Boolean in
      let* is_rejected =
        Slot.Checked.(valid_while.lower > base_params.timeout)
        >>= ( &&& ) (not is_accepted) >>= ( ||| ) is_rejected
      in
      let* deposit_index_before_commit =
        Checked32.Checked.(
          Rollup_state.Outer_action_state.With_length.length_var
            action.case_commit.synchronized_outer_action_state
          > deposit_index)
      in
      let*| is_accepted =
        Slot.Checked.(valid_while.upper <= base_params.timeout)
        >>= ( &&& ) deposit_index_before_commit
        >>= ( &&& ) action.is_commit >>= ( &&& ) (not is_rejected)
        >>= ( ||| ) is_accepted
      in
      Stmt.
        { params
        ; action_state
        ; n_steps
        ; is_rejected
        ; is_accepted
        ; deposit_index
        }

    let name = "deposit acceptance/rejection check"

    let leaf_iterations = Int.pow 2 8

    let leaf_option_iterations = Int.pow 2 7

    let extend_iterations = Int.pow 2 8

    let extend_option_iterations = Int.pow 2 7

    let override_wrap_domain = None
  end

  module Check_accepted = Folder.Make (Check_accepted_definition) ()

  module Rule_finalize_deposit = struct
    module Check_accepted_params = struct
      let get_iterations = Int.pow 2 8
    end

    module Check_accepted_inst = Check_accepted.Make (Check_accepted_params)

    module Ase_inst = Ase.With_length.Make (struct
      module Action_state = Rollup_state.Outer_action_state

      let get_iterations = Int.pow 2 8
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
      with_label ("main " ^ __LOC__) (fun () ->
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
          let helper_token_id =
            let account_id =
              Account_id.Checked.create public_key
                (constant Token_id.typ token_id_l2)
            in
            Account_id.Checked.derive_token_id ~owner:account_id
          in
          let* () = Boolean.Assert.is_true is_accepted in
          let* () = Boolean.(Assert.is_true (not is_rejected)) in
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
          let* next_deposit =
            Checked32.Checked.(
              sub
                (Rollup_state.Outer_action_state.With_length.length_var
                   mid_outer_action_state )
                n_steps)
          in
          let* next_deposit' = Checked32.Checked.succ deposit_index in
          let* () =
            assert_equal ~label:__LOC__ Checked32.typ next_deposit next_deposit'
          in
          let* () =
            assert_var __LOC__
              Checked32.Checked.(fun () -> prev_next_deposit < next_deposit)
          in
          let base_params = Deposit_params.base params in
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
          let account_update =
            { default_account_update with
              public_key
            ; token_id = constant Token_id.typ token_id_l2
            ; may_use_token
            ; authorization_kind = authorization_vk_hash vk_hash
            ; balance_change =
                Currency.Amount.Signed.Checked.(
                  of_unsigned base_params.amount |> negate)
            }
          in
          let*| out =
            make_outputs account_update
              [ (helper_account, []); (witness_inner, []) ]
          in
          Compile_simple.
            { prevs = Two_prevs (verify_check_accepted, verify_ase); out } )

    let rule : _ Compile_simple.branch =
      { branch_name = "finalize deposit"
      ; tags = Two_tags (Check_accepted.tag, Ase.With_length.tag)
      ; main
      }
  end

  module Rule_finalize_cancelled_deposit = struct
    (** Used to prove that there is a commit with some synchronized outer action state. *)
    module Ase_outer_inst = Ase.Without_length.Make (struct
      module Action_state = Rollup_state.Outer_action_state

      let get_iterations = Int.pow 2 14
    end)

    (** Used to prove that the synchronized outer action state is a predecessor of the current one. *)
    module Ase_outer_with_length_inst = Ase.With_length.Make (struct
      module Action_state = Rollup_state.Outer_action_state

      let get_iterations = Int.pow 2 14
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

      let rule : _ Compile_simple.branch =
        { branch_name = "Verify_two_outer_ases"
        ; tags = Two_tags (Ase.Without_length.tag, Ase.With_length.tag)
        ; main
        }

      include
        ( val Compile_simple.compile ~name:"Verify_both_ases" ~branches:[ rule ]
                ~out_typ:
                  Typ.(
                    Ase_outer_inst.Stmt.typ
                    * Ase_outer_with_length_inst.Stmt.typ)
                () )
    end

    module Check_accepted_params = struct
      let get_iterations = Int.pow 2 8
    end

    module Check_accepted_inst = Check_accepted.Make (Check_accepted_params)

    (** We could do a Check_accepted alone, but the issue is that proving time
      for a user might be slower than the rate at which new actions get added,
      meaning the user would never catch up, preventing them from finalizing their
      cancelled deposit.
     *)
    module Verify_check_accepted_and_ase = struct
      let main (w : (Check_accepted_inst.t * Ase_outer_with_length_inst.t) V.t)
          =
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

      let rule : _ Compile_simple.branch =
        { branch_name = "Verify_two_outer_ases"
        ; tags = Two_tags (Check_accepted.tag, Ase.With_length.tag)
        ; main
        }

      include
        ( val Compile_simple.compile ~name:"Verify_both_ases" ~branches:[ rule ]
                ~out_typ:
                  Typ.(
                    Check_accepted_definition.Stmt.typ
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
                 } =
            exists Witness.typ ~compute:(V.get w)
          in
          let* (commit_ase, sync_ase), verify_two_outer_ases =
            Verify_two_outer_ases.get verify_two_outer_ases
          in
          let* ( ( ({ params
                    ; action_state = mid_outer_action_state'
                    ; deposit_index
                    ; n_steps
                    ; is_rejected
                    ; is_accepted
                    } :
                     Check_accepted_definition.Stmt.var )
                 , ({ source = mid_outer_action_state
                    ; target = outer_action_state
                    } :
                     Ase_outer_with_length_inst.Stmt.var ) )
               , verify_check_accepted_and_ase ) =
            Verify_check_accepted_and_ase.get verify_check_accepted_and_ase
          in
          let helper_token_id =
            let account_id =
              Account_id.Checked.create public_key
                (constant Token_id.typ token_id_l1)
            in
            Account_id.Checked.derive_token_id ~owner:account_id
          in
          let* () = Boolean.(Assert.is_true @@ not is_accepted) in
          let* () = Boolean.Assert.is_true is_rejected in
          let* () =
            assert_equal ~label:__LOC__ Rollup_state.Outer_action_state.typ
              (Rollup_state.Outer_action_state.With_length.state_var
                 mid_outer_action_state )
              mid_outer_action_state'
          in
          let* next_cancelled_deposit =
            Checked32.Checked.(
              sub
                (Rollup_state.Outer_action_state.With_length.length_var
                   mid_outer_action_state )
                n_steps)
          in
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
          let base_params = Deposit_params.base params in
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
                      { next_cancelled_deposit = Some next_cancelled_deposit
                      ; next_withdrawal = None
                      }
                    |> var_to_app_state_fine
                }
            ; preconditions =
                { default_account_update.preconditions with
                  account =
                    { default_account_update.preconditions.account with
                      state =
                        Outer_user_state.fine
                          { next_cancelled_deposit =
                              Some prev_next_cancelled_deposit
                          ; next_withdrawal = None
                          }
                        |> var_to_precondition_fine
                    }
                }
            }
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
                          ; paused =
                              Some Boolean.false_ (* must not be paused *)
                          ; ledger_hash = None
                          ; inner_action_state = { state = None; length = None }
                          ; sequencer = None
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
          let*| out =
            make_outputs account_update
              [ (helper_account, []); (witness_outer, []) ]
          in
          Compile_simple.
            { prevs =
                Two_prevs (verify_two_outer_ases, verify_check_accepted_and_ase)
            ; out
            } )

    let rule : _ Compile_simple.branch =
      { branch_name = "finalize cancelled deposit"
      ; tags =
          Two_tags (Verify_two_outer_ases.tag, Verify_check_accepted_and_ase.tag)
      ; main
      }
  end

  module Rule_finalize_withdrawal = struct
    module Ase_outer_inst = Ase.Without_length.Make (struct
      module Action_state = Rollup_state.Outer_action_state

      let get_iterations = Int.pow 2 14
    end)

    module Ase_inner_inst = Ase.With_length.Make (struct
      module Action_state = Rollup_state.Inner_action_state

      let get_iterations = Int.pow 2 14
    end)

    let withdrawal_action (params : Withdrawal_params.var) :
        Rollup_state.Inner_action.var Checked.t =
      (* The chosen account must be one of the valid holder accounts.
         NB: If we invalidate an account later on,
         a yet unfinalized withdrawal will be made unfinalizable.
         Adding an account is however not a problem.
      *)
      let base_params = Withdrawal_params.base params in
      let a =
        { default_account_update with
          public_key = constant PC.typ holder_account_l2
        ; token_id = constant Token_id.typ token_id_l2
        ; balance_change =
            Currency.Amount.Signed.Checked.of_unsigned base_params.amount
        ; may_use_token = constant May_use_token.typ Parents_own_token
        ; authorization_kind =
            constant Account_update.Authorization_kind.typ None_given
        }
      in
      let a', (children : Calls.t) =
        match token_owner_l2 with
        | None ->
            (a, [])
        | Some token_owner_l2 ->
            let custom_params =
              Withdrawal_params.custom params
              |> Option.value_exn
                   ~message:
                     "If token_id isn't default, then Withdrawal_params_custom \
                      must be used."
            in
            ( { default_account_update with
                public_key =
                  Account_id.public_key token_owner_l2 |> constant PC.typ
              ; token_id =
                  Account_id.token_id token_owner_l2 |> constant Token_id.typ
              ; authorization_kind = custom_params.authorization_kind
              ; call_data = custom_params.call_data
              }
            , (a, []) :: Raw custom_params.nested_children )
      in
      let* children' =
        Calls.hash ((a', children) :: Raw base_params.children)
      in
      let hash_prefix =
        Hash_prefix_create.salt "Withdrawal_params - qFB3jXP*)"
      in
      let* aux = var_to_hash ~init:hash_prefix Withdrawal_params.typ params in
      Checked.return
        ({ aux; children = children' } : Rollup_state.Inner_action.var)

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
                 } =
            exists Witness.typ ~compute:(V.get w)
          in
          let* commit_ase, verify_commit_ase = Ase_outer_inst.get commit_ase in
          let* withdrawal_ase, verify_withdrawal_ase =
            Ase_inner_inst.get withdrawal_ase
          in
          let helper_token_id =
            let account_id =
              Account_id.Checked.create public_key
                (constant Token_id.typ token_id_l1)
            in
            Account_id.Checked.derive_token_id ~owner:account_id
          in
          (* make sure that withdrawal ase is connected to withdrawal *)
          let* () =
            let* action = withdrawal_action withdrawal_params in
            let* withdrawal_ase_source' =
              Rollup_state.Inner_action.push_var action before_withdrawal
            in
            assert_equal ~label:__LOC__ Rollup_state.Inner_action_state.typ
              (Rollup_state.Inner_action_state.With_length.state_var
                 withdrawal_ase.source )
              withdrawal_ase_source'
          in
          let next_withdrawal =
            Rollup_state.Inner_action_state.With_length.length_var
              withdrawal_ase.source
          in
          let* () =
            assert_var __LOC__
              Checked32.Checked.(
                fun () -> prev_next_withdrawal < next_withdrawal)
          in
          (* make sure that commit ase is connected to commit *)
          let* () =
            let* commit_ase_source' =
              Rollup_state.Outer_action.push_commit_var commit before_commit
            in
            assert_equal ~label:__LOC__ Rollup_state.Outer_action_state.typ
              commit_ase.source commit_ase_source'
          in
          let base_params = Withdrawal_params.base withdrawal_params in
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
                      { next_withdrawal = Some next_withdrawal
                      ; next_cancelled_deposit = None
                      }
                    |> var_to_app_state_fine
                }
            ; preconditions =
                { default_account_update.preconditions with
                  account =
                    { default_account_update.preconditions.account with
                      state =
                        Outer_user_state.fine
                          { next_withdrawal = Some prev_next_withdrawal
                          ; next_cancelled_deposit = None
                          }
                        |> var_to_precondition_fine
                    }
                }
            }
          in
          let* lower =
            Slot.Checked.add commit.slot_range.upper
              (constant Mina_numbers.Global_slot_span.typ withdrawal_delay)
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
                          ; paused =
                              Some Boolean.false_ (* must not be paused *)
                          ; ledger_hash = None
                          ; inner_action_state =
                              { state =
                                  Rollup_state.Inner_action_state.With_length
                                  .state_var withdrawal_ase.target
                                  |> Some
                              ; length =
                                  Rollup_state.Inner_action_state.With_length
                                  .length_var withdrawal_ase.target
                                  |> Some
                              }
                          ; sequencer = None
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
          let*| out =
            make_outputs account_update
              [ (helper_account, []); (witness_outer, []) ]
          in
          Compile_simple.
            { prevs = Two_prevs (verify_commit_ase, verify_withdrawal_ase)
            ; out
            } )

    let rule : _ Compile_simple.branch =
      { branch_name = "finalize withdrawal"
      ; tags = Two_tags (Ase.Without_length.tag, Ase.With_length.tag)
      ; main
      }
  end

  module Rule_disable = struct
    module Witness = struct
      type t =
        { public_key : PC.t
        ; vk_hash : F.t
        ; may_use_token : May_use_token.t
        ; disable_offset_lower : Slot.t
        ; disable_offset_upper : Slot.t
        ; disable_period : Slot_span.t
        ; disabled_vk : F.t
        ; idx : Slot_span.t
        }
      [@@deriving snarky]
    end

    let main (w : Witness.t V.t) =
      let@ () = with_label ("main " ^ __LOC__) in
      let* Witness.
             { public_key
             ; vk_hash
             ; may_use_token
             ; disable_offset_lower
             ; disable_offset_upper
             ; disable_period
             ; disabled_vk
             ; idx
             } =
        exists Witness.typ ~compute:(V.get w)
      in
      let* lower =
        Slot_span.Checked.mul idx disable_period
        >>= Slot.Checked.add disable_offset_lower
      in
      let* upper =
        Slot_span.Checked.mul idx disable_period
        >>= Slot.Checked.add disable_offset_upper
      in
      let account_update =
        { default_account_update with
          public_key
        ; token_id = constant Token_id.typ token_id_l1
        ; may_use_token
        ; authorization_kind = authorization_vk_hash vk_hash
        ; update =
            { default_account_update.update with
              permissions =
                Zkapp_basic.Set_or_keep.Checked.make_unsafe Boolean.true_
                  (Mina_base.Permissions.Checked.constant
                     holder_account_l1_permissions_disabled )
            ; verification_key =
                Zkapp_basic.Set_or_keep.Checked.make_unsafe Boolean.true_
                  ( { is_some = Boolean.true_
                    ; data = Data_as_hash.make_unsafe disabled_vk (ref None)
                    }
                    : _ Zkapp_basic.Flagged_option.t )
            }
        ; preconditions =
            { default_account_update.preconditions with
              account =
                { default_account_update.preconditions.account with
                  state =
                    Outer_bridge_state.fine
                      { disable_offset_lower = Some disable_offset_lower
                      ; disable_offset_upper = Some disable_offset_upper
                      ; disable_period = Some disable_period
                      ; enable_offset_lower = None
                      ; enable_offset_upper = None
                      ; enable_period = None
                      ; enabled_vk = None
                      ; disabled_vk = Some disabled_vk
                      }
                    |> var_to_precondition_fine
                }
            ; valid_while = Slot_range.Checked.to_valid_while { lower; upper }
            }
        }
      in
      let*| out = make_outputs account_update [] in
      Compile_simple.{ prevs = No_prevs; out }

    let rule : _ Compile_simple.branch =
      { branch_name = "disable holder account"; tags = No_tags; main }
  end

  module Rule_enable = struct
    module Witness = struct
      type t =
        { public_key : PC.t
        ; vk_hash : F.t
        ; may_use_token : May_use_token.t
        ; enable_offset_lower : Slot.t
        ; enable_offset_upper : Slot.t
        ; enable_period : Slot_span.t
        ; enabled_vk : F.t
        ; idx : Slot_span.t
        }
      [@@deriving snarky]
    end

    let main (w : Witness.t V.t) =
      let@ () = with_label ("main " ^ __LOC__) in
      let* Witness.
             { public_key
             ; vk_hash
             ; may_use_token
             ; enable_offset_lower
             ; enable_offset_upper
             ; enable_period
             ; enabled_vk
             ; idx
             } =
        exists Witness.typ ~compute:(V.get w)
      in
      let* lower =
        Slot_span.Checked.mul idx enable_period
        >>= Slot.Checked.add enable_offset_lower
      in
      let* upper =
        Slot_span.Checked.mul idx enable_period
        >>= Slot.Checked.add enable_offset_upper
      in
      let account_update =
        { default_account_update with
          public_key
        ; token_id = constant Token_id.typ token_id_l1
        ; may_use_token
        ; authorization_kind = authorization_vk_hash vk_hash
        ; update =
            { default_account_update.update with
              permissions =
                Zkapp_basic.Set_or_keep.Checked.make_unsafe Boolean.true_
                  (Mina_base.Permissions.Checked.constant
                     holder_account_l1_permissions_enabled )
            ; verification_key =
                Zkapp_basic.Set_or_keep.Checked.make_unsafe Boolean.true_
                  ( { is_some = Boolean.true_
                    ; data = Data_as_hash.make_unsafe enabled_vk (ref None)
                    }
                    : _ Zkapp_basic.Flagged_option.t )
            }
        ; preconditions =
            { default_account_update.preconditions with
              account =
                { default_account_update.preconditions.account with
                  state =
                    Outer_bridge_state.fine
                      { enable_offset_lower = Some enable_offset_lower
                      ; enable_offset_upper = Some enable_offset_upper
                      ; enable_period = Some enable_period
                      ; disable_offset_lower = None
                      ; disable_offset_upper = None
                      ; disable_period = None
                      ; disabled_vk = None
                      ; enabled_vk = Some enabled_vk
                      }
                    |> var_to_precondition_fine
                }
            ; valid_while = Slot_range.Checked.to_valid_while { lower; upper }
            }
        }
      in
      let*| out = make_outputs account_update [] in
      Compile_simple.{ prevs = No_prevs; out }

    let rule : _ Compile_simple.branch =
      { branch_name = "enable holder account"; tags = No_tags; main }
  end
end

module Make_mina (Inputs : sig
  val holder_accounts_l1 : PC.t list

  val holder_account_l2 : PC.t

  val zeko_l1 : PC.t

  val zeko_l2 : PC.t

  val withdrawal_delay : Mina_numbers.Global_slot_span.t

  val holder_account_l1_permissions_enabled : Mina_base.Permissions.t

  val holder_account_l1_permissions_disabled : Mina_base.Permissions.t
end) =
Make (struct
  include Inputs

  let token_owner_l1 = None

  let token_owner_l2 = None

  module Deposit_params = Deposit_params_base
  module Withdrawal_params = Withdrawal_params_base
end)

module Make_custom (Inputs : sig
  val token_owner_l1 : Account_id.t

  val token_owner_l2 : Account_id.t

  val holder_accounts_l1 : PC.t list

  val zeko_l1 : PC.t

  val zeko_l2 : PC.t

  val holder_account_l2 : PC.t

  val withdrawal_delay : Mina_numbers.Global_slot_span.t

  val holder_account_l1_permissions_enabled : Mina_base.Permissions.t

  val holder_account_l1_permissions_disabled : Mina_base.Permissions.t
end) =
Make (struct
  include Inputs

  let token_owner_l1 = Some token_owner_l1

  let token_owner_l2 = Some token_owner_l2

  module Deposit_params = Deposit_params_custom
  module Withdrawal_params = Withdrawal_params_custom
end)
