open Core_kernel
open Mina_base
open Zeko_util
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed

module Withdrawal_recipient_domain = struct
  type t = Mina | Ethereum

  let of_ethereum_holder_account = function
    | None ->
        Mina
    | Some _ ->
        Ethereum
end

module Ethereum_address = struct
  let bit_length = 160

  let validate ({ PC.Poly.x; is_odd } : PC.t) =
    if is_odd then
      Or_error.error_string "Ethereum withdrawal recipient must be even"
    else if List.drop (Field.to_bits x) bit_length |> List.exists ~f:Fn.id then
      Or_error.error_string
        "Ethereum withdrawal recipient x-coordinate must fit 160 bits"
    else Ok ()

  let assert_valid ({ PC.Poly.x; is_odd } : PC.var) =
    let* () = Boolean.Assert.is_false is_odd in
    let* (_ : Boolean.var list) =
      Field.Checked.choose_preimage_var x ~length:bit_length
    in
    Checked.return ()

  let validate_for domain recipient =
    match domain with
    | Withdrawal_recipient_domain.Mina ->
        Ok ()
    | Withdrawal_recipient_domain.Ethereum ->
        validate recipient

  let assert_valid_for domain recipient =
    match domain with
    | Withdrawal_recipient_domain.Mina ->
        Checked.return ()
    | Withdrawal_recipient_domain.Ethereum ->
        assert_valid recipient
end

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
  (* NB! Order matters, everything here has to match *)
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

  module Set_or_keep = struct
    type t =
      { next_cancelled_deposit : Checked32.var
      ; set_next_cancelled_deposit : Boolean.var
      ; next_withdrawal : Checked32.var
      ; set_next_withdrawal : Boolean.var
      }

    let of_fields
        (fields : F.var Zkapp_basic.Set_or_keep.Checked.t Zkapp_state.V.t) :
        t Checked.t =
      let (next_cancelled_deposit :: next_withdrawal :: rest) = fields in
      (* assert that everything else is kept *)
      let* () =
        let f acc field =
          let* () = acc in
          assert_var __LOC__
          @@ fun () ->
          Checked.return @@ Zkapp_basic.Set_or_keep.Checked.is_keep field
        in
        Pickles_types.Vector.fold ~init:(Checked.return ()) ~f rest
      in
      (* extract is_set and data *)
      let set_next_cancelled_deposit =
        Zkapp_basic.Set_or_keep.Checked.is_set next_cancelled_deposit
      in
      let next_cancelled_deposit' =
        Zkapp_basic.Set_or_keep.Checked.data next_cancelled_deposit
      in
      (* check that it's equal to some Checked32 (basically range check) *)
      let* next_cancelled_deposit =
        exists Checked32.typ
          ~compute:
            ( As_prover.read_var next_cancelled_deposit'
            |> As_prover.map ~f:(fun x ->
                   Field.to_string x |> Checked32.of_string ) )
      in
      let* () =
        assert_equal ~label:__LOC__ F.typ next_cancelled_deposit'
          (Checked32.Checked.to_field next_cancelled_deposit)
      in
      (* do the same but for next_withdrawal instead of next_cancelled_deposit *)
      let set_next_withdrawal =
        Zkapp_basic.Set_or_keep.Checked.is_set next_withdrawal
      in
      let next_withdrawal' =
        Zkapp_basic.Set_or_keep.Checked.data next_withdrawal
      in
      let* next_withdrawal =
        exists Checked32.typ
          ~compute:
            ( As_prover.read_var next_withdrawal'
            |> As_prover.map ~f:(fun x ->
                   Field.to_string x |> Checked32.of_string ) )
      in
      let*| () =
        assert_equal ~label:__LOC__ F.typ next_withdrawal'
          (Checked32.Checked.to_field next_withdrawal)
      in
      { next_cancelled_deposit
      ; set_next_cancelled_deposit
      ; next_withdrawal
      ; set_next_withdrawal
      }
  end
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

  module B = struct
    type t = Account_update.Body.t

    type var = Account_update.Body.Checked.t

    let typ = Account_update.Body.typ ()
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

  let ethereum_salt = Zeko_constants.ethereum_deposit_salt

  let asset_id _ = None

  let registry_binding _ = None
end

module Deposit_params_ethereum_token_v1 = struct
  type t =
    { asset_id_high : F.t; asset_id_low : F.t; base : Deposit_params_base.t }
  [@@deriving snarky]

  let base { base; _ } : Deposit_params_base.var = base

  let custom _ = None

  let ethereum_salt = Zeko_constants.ethereum_erc20_deposit_salt

  let asset_id { asset_id_high; asset_id_low; _ } =
    Some (asset_id_high, asset_id_low)

  let registry_binding _ = None
end

module Deposit_params_ethereum_token = struct
  type t =
    { encoding_version : Checked32.t
    ; registry_index : Checked32.t
    ; record_commitment : F.t
    ; asset_id_high : F.t
    ; asset_id_low : F.t
    ; base : Deposit_params_base.t
    }
  [@@deriving snarky]

  let base { base; _ } : Deposit_params_base.var = base

  let custom _ = None

  let ethereum_salt = Zeko_constants.ethereum_erc20_deposit_v2_salt

  let asset_id { asset_id_high; asset_id_low; _ } =
    Some (asset_id_high, asset_id_low)

  let registry_binding
      { encoding_version; registry_index; record_commitment; _ } =
    Some (encoding_version, registry_index, record_commitment)
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

  let ethereum_salt = Zeko_constants.ethereum_deposit_salt

  let asset_id _ = None

  let registry_binding _ = None
end

(* When the token is the Mina token. *)
module Withdrawal_params_base = struct
  type t = { children : C.t; amount : Currency.Amount.t; recipient : PC.t }
  [@@deriving snarky]

  let base (x : var) = x

  let custom _ = None

  let debit_first = false

  let recipient_domain = Withdrawal_recipient_domain.Mina

  let hash_salt = Zeko_constants.withdrawal_salt

  let asset_id _ = None

  let registry_binding _ = None
end

(* When the token is custom, and we need token owner authorization. *)
module Withdrawal_params_custom = struct
  type t =
    { token_owner_body : B.t
    ; nested_children : C.t
    ; base : Withdrawal_params_base.t
    }
  [@@deriving snarky]

  let base { base; _ } : Withdrawal_params_base.var = base

  let custom x = Some x

  let debit_first = false

  let recipient_domain = Withdrawal_recipient_domain.Mina

  let hash_salt = Zeko_constants.withdrawal_salt

  let asset_id _ = None

  let registry_binding _ = None
end

module Withdrawal_params_ethereum_token_v1 = struct
  type t =
    { asset_id_high : F.t
    ; asset_id_low : F.t
    ; custom : Withdrawal_params_custom.t
    }
  [@@deriving snarky]

  let base { custom; _ } : Withdrawal_params_base.var = custom.base

  let custom { custom; _ } = Some custom

  (* Mina's FungibleToken.approveBase rejects a positive running balance as
     flash minting.  The user debit supplied in [nested_children] therefore
     has to precede the bridge-vault receive synthesized below. *)
  let debit_first = true

  let recipient_domain = Withdrawal_recipient_domain.Ethereum

  let hash_salt = Zeko_constants.ethereum_erc20_withdrawal_salt

  let asset_id { asset_id_high; asset_id_low; _ } =
    Some (asset_id_high, asset_id_low)

  let registry_binding _ = None
end

module Withdrawal_params_ethereum_token = struct
  type t =
    { encoding_version : Checked32.t
    ; registry_index : Checked32.t
    ; record_commitment : F.t
    ; asset_id_high : F.t
    ; asset_id_low : F.t
    ; custom : Withdrawal_params_custom.t
    }
  [@@deriving snarky]

  let base { custom; _ } : Withdrawal_params_base.var = custom.base

  let custom { custom; _ } = Some custom

  let debit_first = true

  let recipient_domain = Withdrawal_recipient_domain.Ethereum

  let hash_salt = Zeko_constants.ethereum_erc20_withdrawal_v2_salt

  let asset_id { asset_id_high; asset_id_low; _ } =
    Some (asset_id_high, asset_id_low)

  let registry_binding
      { encoding_version; registry_index; record_commitment; _ } =
    Some (encoding_version, registry_index, record_commitment)
end

module type DEPOSIT_PARAMS = sig
  include SnarkType

  val base : var -> Deposit_params_base.var

  val custom : var -> Deposit_params_custom.var option

  val ethereum_salt : string

  val asset_id : var -> (F.var * F.var) option

  val registry_binding : var -> (Checked32.var * Checked32.var * F.var) option
end

module type WITHDRAWAL_PARAMS = sig
  include SnarkType

  val base : var -> Withdrawal_params_base.var

  val custom : var -> Withdrawal_params_custom.var option

  val debit_first : bool

  val recipient_domain : Withdrawal_recipient_domain.t

  val hash_salt : string

  val asset_id : var -> (F.var * F.var) option

  val registry_binding : var -> (Checked32.var * Checked32.var * F.var) option
end

let deposit_action (type deposit_params_var) ~chain_l1
    ~(holder_accounts_l1 : PC.t list) ~(token_owner_l1 : Account_id.t option)
    ~(ethereum_holder_account_l1 : PC.t option)
    ~(ethereum_asset_id : (F.t * F.t) option)
    (module Deposit_params : DEPOSIT_PARAMS with type var = deposit_params_var)
    (params : deposit_params_var) ~(bridge_fee_recipient_l1 : PC.var)
    ~(bridge_proof_fee : Currency.Amount.var) :
    Rollup_state.Outer_action.Witness.var Checked.t =
  let open Checked.Let_syntax in
  (* The chosen account must be one of the valid holder accounts.
     NB: If we invalidate an account later on,
     a yet unfinalized deposit will be made unfinalizable.
     Adding an account is however not a problem.
  *)
  let base_params = Deposit_params.base params in
  let* () =
    match Deposit_params.registry_binding params with
    | None ->
        Checked.return ()
    | Some (encoding_version, _, _) ->
        assert_equal ~label:"Ethereum ERC20 deposit encoding version"
          Checked32.typ encoding_version
          (Checked32.Checked.constant (Checked32.of_int 2))
  in
  let* () =
    match
      ( ethereum_holder_account_l1
      , ethereum_asset_id
      , Deposit_params.asset_id params )
    with
    | Some _, Some (expected_high, expected_low), Some (actual_high, actual_low)
      ->
        let* () =
          assert_equal ~label:__LOC__ F.typ actual_high
            (constant F.typ expected_high)
        in
        assert_equal ~label:__LOC__ F.typ actual_low
          (constant F.typ expected_low)
    | Some _, None, Some _ | Some _, None, None | None, None, None ->
        Checked.return ()
    | _ ->
        failwith
          "Ethereum deposit asset schema does not match circuit configuration"
  in
  let@ () = with_label __LOC__ in
  let permitted_holder_accounts =
    match ethereum_holder_account_l1 with
    | Some holder ->
        [ holder ]
    | None ->
        holder_accounts_l1
  in
  let* () =
    Checked.List.map
      ~f:(fun holder_account_l1' ->
        constant PC.typ holder_account_l1'
        |> PC.Checked.equal base_params.holder_account_l1 )
      permitted_holder_accounts
    >>= Boolean.Assert.any
  in
  let@ () = with_label __LOC__ in
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
  let fee_payout =
    { default_account_update with
      public_key = bridge_fee_recipient_l1
    ; token_id = constant Token_id.typ Token_id.default
    ; balance_change =
        Currency.Amount.Signed.Checked.of_unsigned bridge_proof_fee
    ; may_use_token =
        constant Account_update.May_use_token.typ Parents_own_token
    ; authorization_kind =
        constant Account_update.Authorization_kind.typ None_given
    }
  in
  let@ () = with_label __LOC__ in
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
  let* children', aux =
    match ethereum_holder_account_l1 with
    | Some _ ->
        let* aux =
          var_to_hash ~init:Deposit_params.ethereum_salt Deposit_params.typ
            params
        in
        Checked.return (constant C.typ [], aux)
    | None ->
        let@ () = with_label __LOC__ in
        let* children =
          Calls.hash ~chain:chain_l1
            ((a', children) :: (fee_payout, []) :: Raw base_params.children)
        in
        let@ () = with_label __LOC__ in
        let* aux =
          var_to_hash ~init:Zeko_constants.deposit_salt Deposit_params.typ
            params
        in
        Checked.return (children, aux)
  in
  Checked.return
    ( { aux
      ; children = children'
      ; slot_range = constant Slot_range.typ Slot_range.infinite
      }
      : Rollup_state.Outer_action.Witness.var )

let withdrawal_action (type withdrawal_params_var) ~chain_l2
    ~(holder_account_l2 : PC.t) ~(token_owner_l2 : Account_id.t option)
    ~(ethereum_asset_id : (F.t * F.t) option)
    ~(ethereum_registry_binding : (Checked32.t * F.t) option) ~l2_holder_vk_hash
    ~bridge_fee_recipient_l2 ~bridge_proof_fee
    (module Withdrawal_params : WITHDRAWAL_PARAMS
      with type var = withdrawal_params_var ) (params : Withdrawal_params.var) :
    Rollup_state.Inner_action.var Checked.t =
  (* The chosen account must be one of the valid holder accounts.
     NB: If we invalidate an account later on,
     a yet unfinalized withdrawal will be made unfinalizable.
     Adding an account is however not a problem.
  *)
  let base_params = Withdrawal_params.base params in
  let* () =
    Ethereum_address.assert_valid_for Withdrawal_params.recipient_domain
      base_params.recipient
  in
  let* () =
    match
      (ethereum_registry_binding, Withdrawal_params.registry_binding params)
    with
    | None, None ->
        Checked.return ()
    | ( Some (expected_index, expected_commitment)
      , Some (encoding_version, actual_index, actual_commitment) ) ->
        let* () =
          assert_equal ~label:"Ethereum ERC20 withdrawal encoding version"
            Checked32.typ encoding_version
            (Checked32.Checked.constant (Checked32.of_int 2))
        in
        let* () =
          assert_equal ~label:"Ethereum ERC20 withdrawal registry index"
            Checked32.typ actual_index
            (Checked32.Checked.constant expected_index)
        in
        assert_equal ~label:"Ethereum ERC20 withdrawal record commitment" F.typ
          actual_commitment
          (constant F.typ expected_commitment)
    | _ ->
        failwith
          "Withdrawal registry binding does not match circuit configuration"
  in
  let* () =
    match (ethereum_asset_id, Withdrawal_params.asset_id params) with
    | Some (expected_high, expected_low), Some (actual_high, actual_low) ->
        let* () =
          assert_equal ~label:__LOC__ F.typ actual_high
            (constant F.typ expected_high)
        in
        assert_equal ~label:__LOC__ F.typ actual_low
          (constant F.typ expected_low)
    | None, None ->
        Checked.return ()
    | _ ->
        failwith "Withdrawal asset schema does not match circuit configuration"
  in
  let a =
    { default_account_update with
      public_key = constant PC.typ holder_account_l2
    ; token_id = constant Token_id.typ (token_owner_id token_owner_l2)
    ; use_full_commitment = constant Boolean.typ (Option.is_none token_owner_l2)
    ; implicit_account_creation_fee =
        constant Boolean.typ (Option.is_none token_owner_l2)
    ; balance_change =
        Currency.Amount.Signed.Checked.of_unsigned base_params.amount
    ; may_use_token =
        constant Account_update.May_use_token.typ Parents_own_token
    ; authorization_kind =
        (* FIXME: awful hack to get around the fact that in fake mode, we don't have a correct vk hash *)
        ( match Sys.getenv_opt "ZEKO_CIRCUITS_MODE" with
        | Some "fake" ->
            constant Account_update.Authorization_kind.typ None_given
        | Some "real" | _ ->
            { is_signed = Boolean.false_
            ; is_proved = Boolean.true_
            ; verification_key_hash = l2_holder_vk_hash
            } )
    }
  in
  let fee_payout =
    { default_account_update with
      public_key = bridge_fee_recipient_l2
    ; token_id = constant Token_id.typ Token_id.default
    ; balance_change =
        Currency.Amount.Signed.Checked.of_unsigned bridge_proof_fee
    ; may_use_token =
        constant Account_update.May_use_token.typ Parents_own_token
    ; authorization_kind =
        constant Account_update.Authorization_kind.typ None_given
    }
  in
  let* (a', children) : Account_update.Checked.t * Calls.t =
    match token_owner_l2 with
    | None ->
        Checked.return (a, ([] : Calls.t))
    | Some token_owner_l2 ->
        let custom_params =
          Withdrawal_params.custom params
          |> Option.value_exn
               ~message:
                 "If token_id isn't default, then Withdrawal_params_custom \
                  must be used."
        in
        let token_owner = custom_params.token_owner_body in
        let* () =
          assert_equal ~label:__LOC__ PC.typ token_owner.public_key
            (Account_id.public_key token_owner_l2 |> constant PC.typ)
        in
        let* () =
          assert_equal ~label:__LOC__ Token_id.typ token_owner.token_id
            (Account_id.token_id token_owner_l2 |> constant Token_id.typ)
        in
        if Withdrawal_params.debit_first then
          let* (debit, debit_children), tail =
            make_checked (fun () ->
                Zkapp_call_forest.Checked.pop_exn ~signature_kind:chain_l2
                  custom_params.nested_children )
          in
          let* debit_children_empty =
            make_checked (fun () ->
                Zkapp_call_forest.Checked.is_empty debit_children )
          in
          let* () = Boolean.Assert.is_true debit_children_empty in
          let* tail_empty =
            make_checked (fun () -> Zkapp_call_forest.Checked.is_empty tail)
          in
          let* () = Boolean.Assert.is_true tail_empty in
          let children : Calls.t =
            [ (debit.account_update.data, []); (a, []) ]
          in
          Checked.return (token_owner, children)
        else
          let children : Calls.t =
            (a, []) :: Raw custom_params.nested_children
          in
          Checked.return (token_owner, children)
  in
  let* children' =
    Calls.hash ~chain:chain_l2
      ((a', children) :: (fee_payout, []) :: Raw base_params.children)
  in
  let hash_prefix = Withdrawal_params.hash_salt in
  let* aux = var_to_hash ~init:hash_prefix Withdrawal_params.typ params in
  Checked.return ({ aux; children = children' } : Rollup_state.Inner_action.var)
