open Core_kernel
open Mina_base
open Zeko_util
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed

module Outer_bridge_state = struct
  type t =
    { disable_offset_lower : Slot.t
    ; disable_offset_upper : Slot.t
    ; disable_period : Slot_span.t
    ; enable_offset_lower : Slot.t
    ; enable_offset_upper : Slot.t
    ; enable_period : Slot_span.t
    ; enabled_vk : F.t
    ; disabled_vk : F.t
    }
  [@@deriving snarky]

  type fine =
    { disable_offset_lower : Slot.var option
    ; disable_offset_upper : Slot.var option
    ; disable_period : Slot_span.var option
    ; enable_offset_lower : Slot.var option
    ; enable_offset_upper : Slot.var option
    ; enable_period : Slot_span.var option
    ; enabled_vk : F.var option
    ; disabled_vk : F.var option
    }

  let fine
      ({ disable_offset_lower
       ; disable_offset_upper
       ; disable_period
       ; enable_offset_lower
       ; enable_offset_upper
       ; enable_period
       ; enabled_vk
       ; disabled_vk
       } :
        fine ) : Fine.t =
    [ Whole (Slot.typ, disable_offset_lower)
    ; Whole (Slot.typ, disable_offset_upper)
    ; Whole (Slot_span.typ, disable_period)
    ; Whole (Slot.typ, enable_offset_lower)
    ; Whole (Slot.typ, enable_offset_upper)
    ; Whole (Slot_span.typ, enable_period)
    ; Whole (F.typ, enabled_vk)
    ; Whole (F.typ, disabled_vk)
    ]
end

module Inner_user_state = struct
  type t = { next_deposit : Checked32.t } [@@deriving snarky]

  type fine = { next_deposit : Checked32.var option }

  let fine (p : fine) : Fine.t = [ Whole (Checked32.typ, p.next_deposit) ]
end

module Outer_user_state = struct
  type t =
    { next_cancelled_deposit : Checked32.t; next_withdrawal : Checked32.t }
  [@@deriving snarky]

  type fine =
    { next_cancelled_deposit : Checked32.var option
    ; next_withdrawal : Checked32.var option
    }

  let fine (p : fine) : Fine.t =
    [ Whole (Checked32.typ, p.next_cancelled_deposit)
    ; Whole (Checked32.typ, p.next_withdrawal)
    ]
end

open struct
  module C = struct
    include Zkapp_call_forest

    type var = Checked.t
  end

  module A = struct
    include Account_update.Authorization_kind

    type var = Checked.t
  end
end

(* When the token is the Mina token. *)
module Deposit_params_base = struct
  type t =
    { children : C.t
    ; holder_account_l1 : PC.t
    ; amount : Currency.Amount.t
    ; recipient : PC.t
    ; timeout : Slot.t
    }
  [@@deriving snarky]

  let base (x : var) = x

  let custom _ = None
end

(* When the token is custom, and we need token owner authorization. *)
module Deposit_params_custom = struct
  type t =
    { authorization_kind : A.t
    ; nested_children : C.t
    ; call_data : F.t
    ; base : Deposit_params_base.t
    }
  [@@deriving snarky]

  let base { base; _ } : Deposit_params_base.var = base

  let custom x = Some x
end

(* When the token is the Mina token. *)
module Withdrawal_params_base = struct
  type t = { children : C.t; amount : Currency.Amount.t; recipient : PC.t }
  [@@deriving snarky]

  let base (x : var) = x

  let custom _ = None
end

(* When the token is custom, and we need token owner authorization. *)
module Withdrawal_params_custom = struct
  type t =
    { authorization_kind : A.t
    ; nested_children : C.t
    ; call_data : F.t
    ; base : Withdrawal_params_base.t
    }
  [@@deriving snarky]

  let base { base; _ } : Withdrawal_params_base.var = base

  let custom x = Some x
end

module type DEPOSIT_PARAMS = sig
  include SnarkType

  val base : var -> Deposit_params_base.var

  val custom : var -> Deposit_params_custom.var option
end

module type WITHDRAWAL_PARAMS = sig
  include SnarkType

  val base : var -> Withdrawal_params_base.var

  val custom : var -> Withdrawal_params_custom.var option
end

let deposit_action (type deposit_params_var) ~chain_l1
    ~(holder_accounts_l1 : PC.t list) ~(token_owner_l1 : Account_id.t option)
    (module Deposit_params : DEPOSIT_PARAMS with type var = deposit_params_var)
    (params : deposit_params_var) :
    Rollup_state.Outer_action.Witness.var Checked.t =
  let open Checked.Let_syntax in
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
    ; token_id = constant Token_id.typ (token_owner_id token_owner_l1)
    ; balance_change =
        Currency.Amount.Signed.Checked.of_unsigned base_params.amount
    ; may_use_token =
        constant Account_update.May_use_token.typ Parents_own_token
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
            public_key = Account_id.public_key token_owner_l1 |> constant PC.typ
          ; token_id =
              Account_id.token_id token_owner_l1 |> constant Token_id.typ
          ; authorization_kind = custom_params.authorization_kind
          ; call_data = custom_params.call_data
          }
        , (a, []) :: Raw custom_params.nested_children )
  in
  let* children' =
    Calls.hash ~chain:chain_l1 ((a', children) :: Raw base_params.children)
  in
  let hash_prefix = "Deposit_params - qFB3jXP*)" in
  let* aux = var_to_hash ~init:hash_prefix Deposit_params.typ params in
  Checked.return
    ( { aux
      ; children = children'
      ; slot_range = constant Slot_range.typ Slot_range.infinite
      }
      : Rollup_state.Outer_action.Witness.var )

let withdrawal_action (type withdrawal_params_var) ~chain_l2
    ~(holder_account_l2 : PC.t) ~(token_owner_l2 : Account_id.t option)
    (module Withdrawal_params : WITHDRAWAL_PARAMS
      with type var = withdrawal_params_var ) (params : Withdrawal_params.var) :
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
    ; token_id = constant Token_id.typ (token_owner_id token_owner_l2)
    ; balance_change =
        Currency.Amount.Signed.Checked.of_unsigned base_params.amount
    ; may_use_token =
        constant Account_update.May_use_token.typ Parents_own_token
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
            public_key = Account_id.public_key token_owner_l2 |> constant PC.typ
          ; token_id =
              Account_id.token_id token_owner_l2 |> constant Token_id.typ
          ; authorization_kind = custom_params.authorization_kind
          ; call_data = custom_params.call_data
          }
        , (a, []) :: Raw custom_params.nested_children )
  in
  let* children' =
    Calls.hash ~chain:chain_l2 ((a', children) :: Raw base_params.children)
  in
  let hash_prefix = "Withdrawal_params - qFB3jXP*)" in
  let* aux = var_to_hash ~init:hash_prefix Withdrawal_params.typ params in
  Checked.return ({ aux; children = children' } : Rollup_state.Inner_action.var)
