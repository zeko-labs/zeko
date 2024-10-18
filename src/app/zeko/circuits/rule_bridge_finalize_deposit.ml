open Core_kernel
open Mina_base
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Bridge
open Zeko_util
open Checked.Let_syntax

module Make (Inputs : sig
  val token_owner_l1 : Account_id.t option

  val holder_accounts_l1 : PC.t list

  module Deposit_params : sig
    include SnarkType

    val base : var -> Deposit_params_base.var

    val custom : var -> Deposit_params_custom.var option
  end
end) =
struct
  open Inputs

  let token_id_l1 =
    match token_owner_l1 with
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

  let infinite_slot_range : Slot_range.t =
    { lower = Slot.zero; upper = Slot.max_value }

  let deposit_action (params : Deposit_params.var) :
      Outer.Action.Witness.var Checked.t =
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
          Currency.Amount.Signed.Checked.of_unsigned base_params.deposit.amount
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
        ; slot_range = constant Slot_range.typ infinite_slot_range
        }
        : Outer.Action.Witness.var )

  module Check_accepted_definition = struct
    module Stmt = struct
      type t =
        { params : Deposit_params.t
        ; action_state : Outer.Action.State.t
        ; n_steps : Checked32.t
        ; is_rejected : Boolean.t
        ; is_accepted : Boolean.t
        }
      [@@deriving snarky]
    end

    module Elem = Outer.Action
    module ElemOption = Outer.Action

    let elem_to_option x = x

    let elem_option_none = failwith "FIXME"

    module Init = struct
      type t =
        { params : Deposit_params.t
        ; original_action_state : Outer.Action.State.t
        }
      [@@deriving snarky]
    end

    let init ~check:_ ({ params; original_action_state } : Init.var) :
        Stmt.var Checked.t =
      let* witness = deposit_action params in
      let* action_state =
        Outer.Action.push_witness_var witness original_action_state
      in
      Checked.return
        ( { params
          ; action_state
          ; n_steps = Checked32.Checked.zero
          ; is_rejected = Boolean.false_
          ; is_accepted = Boolean.false_
          }
          : Stmt.var )

    let step ~check:_ (action : Outer.Action.var)
        ({ params; action_state; n_steps; is_rejected; is_accepted } : Stmt.var)
        =
      let* n_steps = Checked32.Checked.succ n_steps in
      let* action_state = Outer.Action.push_var action action_state in
      let* valid_while =
        if_ ~typ:Slot_range.typ action.is_witness
          ~then_:action.case_witness.slot_range
          ~else_:action.case_commit.slot_range
      in
      let base_params = Deposit_params.base params in
      let open Boolean in
      let* is_rejected =
        Slot.Checked.(valid_while.lower > base_params.deposit.timeout)
        >>= ( &&& ) (not is_accepted) >>= ( ||| ) is_rejected
      in
      let*| is_accepted =
        Slot.Checked.(valid_while.upper <= base_params.deposit.timeout)
        >>= ( &&& ) action.is_commit >>= ( &&& ) (not is_rejected)
        >>= ( ||| ) is_accepted
      in
      Stmt.{ params; action_state; n_steps; is_rejected; is_accepted }

    let step_option ~check:_ (action : Outer.Action.var)
        ({ params; action_state; n_steps; is_rejected; is_accepted } : Stmt.var)
        =
      failwith "FIXME"

    let name = "deposit acceptance/rejection check"

    let leaf_iterations = Int.pow 2 16

    let leaf_option_iterations = Int.pow 2 15

    let extend_iterations = Int.pow 2 15

    let extend_option_iterations = Int.pow 2 14

    let override_wrap_domain = None
  end

  module Check_accepted_params = struct
    let get_iterations = Int.pow 2 16
  end

  module Check_accepted = Folder.Make (Check_accepted_definition)
  module Check_accepted_inst = Check_accepted.Make (Check_accepted_params)

  module Ase_lengthed_inst = Ase_lengthed.Make (struct
    let get_iterations = Int.pow 2 14
  end)

  module Witness = struct
    type t =
      { token_id : Token_id.t
      ; public_key : PC.t
      ; vk_hash : F.t
      ; may_use_token : May_use_token.t
      ; inner_authorization_kind : A.t
      ; outer_action_state_with_length : Ase_lengthed_inst.t
            (* FIXME: Allow proving length backwards *)
      ; check_accepted : Check_accepted_inst.t
      ; prev_next_deposit : Checked32.t
      }
    [@@deriving snarky]
  end

  include MkHandler (Witness)

  (** Prove that we have submitted a deposit, and that it's been accepted. *)
  let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
    let* Witness.
           { token_id
           ; public_key
           ; vk_hash
           ; may_use_token
           ; inner_authorization_kind
           ; outer_action_state_with_length
           ; check_accepted
           ; prev_next_deposit
           } =
      exists_witness
    in
    let* check_accepted, verify_check_accepted =
      Check_accepted_inst.get check_accepted
    in
    let params = check_accepted.target.params in
    let account_id = Account_id.Checked.create public_key token_id in
    let our_token_id = Account_id.Checked.derive_token_id ~owner:account_id in
    let* () = Boolean.Assert.is_true check_accepted.target.is_accepted in
    let* () =
      Boolean.Assert.(Boolean.false_ = check_accepted.target.is_rejected)
    in
    let* outer_action_state_with_length, verify_outer_action_state_with_length =
      Ase_lengthed_inst.get outer_action_state_with_length
    in
    let outer_action_state =
      Outer.Action.State.of_field_var
        outer_action_state_with_length.target.action_state
    in
    let* () =
      assert_equal ~label:__LOC__ Outer.Action.State.typ outer_action_state
        check_accepted.target.action_state
    in
    let* next_deposit =
      Checked32.Checked.(
        sub outer_action_state_with_length.target.len
          check_accepted.target.n_steps)
    in
    let* () =
      assert_var __LOC__
        Checked32.Checked.(fun () -> prev_next_deposit < next_deposit)
    in
    let base_params = Deposit_params.base params in
    let helper_account =
      { default_account_update with
        public_key = base_params.deposit.recipient
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
                        Outer.Action.State.to_field_var outer_action_state
                        |> Some
                    }
              }
          }
      }
    in
    let account_update =
      { default_account_update with
        public_key
      ; token_id
      ; may_use_token
      ; authorization_kind = authorization_vk_hash vk_hash
      ; balance_change =
          Currency.Amount.Signed.Checked.(
            of_unsigned base_params.deposit.amount |> negate)
      }
    in
    let* public_output, auxiliary_output =
      make_outputs account_update [ (helper_account, []); (witness_inner, []) ]
    in
    let*| auxiliary_output = V.create auxiliary_output in
    Pickles.Inductive_rule.
      { previous_proof_statements =
          [ verify_check_accepted; verify_outer_action_state_with_length ]
      ; public_output
      ; auxiliary_output
      }

  let rule : _ Pickles.Inductive_rule.t =
    { identifier = "zeko action witness"
    ; prevs = [ force Check_accepted.tag; force Ase_lengthed.tag ]
    ; main = (fun x -> main x |> Run.run_checked)
    ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
    }
end

module Make_mina (Inputs : sig
  val holder_accounts_l1 : PC.t list
end) =
Make (struct
  include Inputs

  let token_owner_l1 = None

  module Deposit_params = Deposit_params_base
end)

module Make_custom (Inputs : sig
  val token_owner_l1 : Account_id.t

  val holder_accounts_l1 : PC.t list
end) =
Make (struct
  include Inputs

  let token_owner_l1 = Some token_owner_l1

  module Deposit_params = Deposit_params_custom
end)
