open Core_kernel

(* if check is false, use x on both sides *)
open Snark_params.Tick
open Mina_base
module PC = Signature_lib.Public_key.Compressed
open Zeko_util
open Checked.Let_syntax

let constraint_constants = Genesis_constants.Compiled.constraint_constants

module Account_set = Indexed_merkle_tree.Make (struct
  open struct
    let add_plonk_constraint c =
      assert_
        { basic =
            Kimchi_backend_common.Plonk_constraint_system.Plonk_constraint.T c
        ; annotation = None
        }

    let ( let- ) var f =
      let+ var = As_prover.read_var var in
      f var

    module Range_check0 = struct
      type t = Zeko_as_prover.range_check0 =
        { v0p0 : F.t (* MSBs *)
        ; v0p1 : F.t (* vpX are 12-bit plookup chunks *)
        ; v0p2 : F.t
        ; v0p3 : F.t
        ; v0p4 : F.t
        ; v0p5 : F.t
        ; v0c0 : F.t (* vcX are 2-bit crumbs *)
        ; v0c1 : F.t
        ; v0c2 : F.t
        ; v0c3 : F.t
        ; v0c4 : F.t
        ; v0c5 : F.t
        ; v0c6 : F.t
        ; v0c7 : F.t (* LSBs *)
        }
      [@@deriving snarky]
    end

    let range_check0 v0 =
      let* ({ v0p0
            ; v0p1
            ; v0p2
            ; v0p3
            ; v0p4
            ; v0p5
            ; v0c0
            ; v0c1
            ; v0c2
            ; v0c3
            ; v0c4
            ; v0c5
            ; v0c6
            ; v0c7
            } :
             Range_check0.var ) =
        exists Range_check0.typ
          ~compute:
            (let- v0 in
             Zeko_as_prover.range_check0 v0 |> As_prover.return )
      in
      let*| () =
        add_plonk_constraint
          (RangeCheck0
             { v0
             ; v0p0
             ; v0p1
             ; v0p2
             ; v0p3
             ; v0p4
             ; v0p5
             ; v0c0
             ; v0c1
             ; v0c2
             ; v0c3
             ; v0c4
             ; v0c5
             ; v0c6
             ; v0c7
             ; compact = Field.zero
             } )
      in
      (v0p4, v0p5)

    module Range_check1 = struct
      type t = Zeko_as_prover.range_check1 =
        { (* Current row *)
          v2c0 : F.t (* MSBs, 2-bit crumb *)
        ; v2p0 : F.t (* vpX are 12-bit plookup chunks *)
        ; v2p1 : F.t
        ; v2p2 : F.t
        ; v2p3 : F.t
        ; v2c1 : F.t (* vcX are 2-bit crumbs *)
        ; v2c2 : F.t
        ; v2c3 : F.t
        ; v2c4 : F.t
        ; v2c5 : F.t
        ; v2c6 : F.t
        ; v2c7 : F.t
        ; v2c8 : F.t (* LSBs *)
        ; (* Next row *) v2c9 : F.t
        ; v2c10 : F.t
        ; v2c11 : F.t
        ; v2c12 : F.t
        ; v2c13 : F.t
        ; v2c14 : F.t
        ; v2c15 : F.t
        ; v2c16 : F.t
        ; v2c17 : F.t
        ; v2c18 : F.t
        ; v2c19 : F.t
        }
      [@@deriving snarky]
    end

    let range_check1 ~v2 ~v0p0 ~v0p1 ~v1p0 ~v1p1 =
      let* { v2c0
           ; v2p0
           ; v2p1
           ; v2p2
           ; v2p3
           ; v2c1
           ; v2c2
           ; v2c3
           ; v2c4
           ; v2c5
           ; v2c6
           ; v2c7
           ; v2c8
           ; v2c9
           ; v2c10
           ; v2c11
           ; v2c12
           ; v2c13
           ; v2c14
           ; v2c15
           ; v2c16
           ; v2c17
           ; v2c18
           ; v2c19
           } =
        exists Range_check1.typ
          ~compute:
            (let- v2 in
             Zeko_as_prover.range_check1 v2 |> As_prover.return )
      in
      add_plonk_constraint
        (RangeCheck1
           { v2
           ; v12 = Field.Var.constant Field.zero
           ; v2c0
           ; v2p0
           ; v2p1
           ; v2p2
           ; v2p3
           ; v2c1
           ; v2c2
           ; v2c3
           ; v2c4
           ; v2c5
           ; v2c6
           ; v2c7
           ; v2c8
           ; v2c9
           ; v2c10
           ; v2c11
           ; v0p0
           ; v0p1
           ; v1p0
           ; v1p1
           ; v2c12
           ; v2c13
           ; v2c14
           ; v2c15
           ; v2c16
           ; v2c17
           ; v2c18
           ; v2c19
           } )

    let multi_range_check x y z =
      let* v0p0, v0p1 = range_check0 x in
      let* v1p0, v1p1 = range_check0 y in
      range_check1 ~v2:z ~v0p0 ~v0p1 ~v1p0 ~v1p1

    let sub_then_dec ~dec ~x0 ~x1 ~x2 ~y0 ~y1 ~y2 =
      let* (z0, z1), z2 =
        exists
          Typ.(F.typ * F.typ * F.typ)
          ~compute:
            (let- x0 in
             let- x1 in
             let- x2 in
             let- y0 in
             let- y1 in
             let- y2 in
             Zeko_as_prover.sub ~x0 ~x1 ~x2 ~y0 ~y1 ~y2 |> As_prover.return )
      in
      let* (w0, w1), w2 =
        exists
          Typ.(F.typ * F.typ * F.typ)
          ~compute:
            (let- z0 in
             let- z1 in
             let- z2 in
             Zeko_as_prover.sub ~x0:z0 ~x1:z1 ~x2:z2 ~y0:Field.one
               ~y1:Field.zero ~y2:Field.zero
             |> As_prover.return )
      in
      let* () =
        add_plonk_constraint
          (ForeignFieldAdd
             { left_input_lo = x0
             ; left_input_mi = x1
             ; left_input_hi = x2
             ; right_input_lo = y0
             ; right_input_mi = y1
             ; right_input_hi = y2
             ; sign = Field.of_int (-1)
             ; carry = Field.(constant typ zero)
             ; field_overflow = Field.(constant typ zero)
             ; foreign_field_modulus0 = Field.zero
             ; foreign_field_modulus1 = Field.zero
             ; foreign_field_modulus2 = Field.zero
             } )
      in
      let* () =
        add_plonk_constraint
          (ForeignFieldAdd
             { left_input_lo = z0
             ; left_input_mi = z1
             ; left_input_hi = z2
             ; right_input_lo = dec
             ; right_input_mi = Field.(constant typ zero)
             ; right_input_hi = Field.(constant typ zero)
             ; sign = Field.of_int (-1)
             ; carry = Field.(constant typ zero)
             ; field_overflow = Field.(constant typ zero)
             ; foreign_field_modulus0 = Field.zero
             ; foreign_field_modulus1 = Field.zero
             ; foreign_field_modulus2 = Field.zero
             } )
      in
      let* () =
        add_plonk_constraint
          (Raw { kind = Zero; values = [| w0; w1; w2 |]; coeffs = [||] })
      in
      multi_range_check z0 z1 z2

    let l =
      Bigint.of_bignum_bigint Bignum_bigint.(of_int 1 |> Fn.flip shift_left 88)
      |> Bigint.to_field

    let l2 = Field.(l * l)

    let field_to_field3 x =
      let* (x0, x1), x2 =
        exists
          Typ.(F.typ * F.typ * F.typ)
          ~compute:
            (let- x in
             Zeko_as_prover.field_to_field3 x |> As_prover.return )
      in
      let* () = multi_range_check x0 x1 x2 in
      let x' = Field.Checked.(x0 + (l * x1) + (l2 * x2)) in
      let*| () = Field.Checked.Assert.equal x' x in
      (x0, x1, x2)

    let assert_greater_than_full ~check x y =
      (* if check is false, use x on both sides *)
      let* y = if_ check ~typ:F.typ ~then_:y ~else_:x in
      let* x0, x1, x2 = field_to_field3 x in
      let* y0, y1, y2 = field_to_field3 y in
      let dec =
        let (Typ typ) = Boolean.typ in
        match typ.var_to_fields check with
        | [| dec |], _ ->
            dec
        | _ ->
            failwith "unreachable"
      in
      (* if check (dec) is false, then we decrement with 0, and expand to greater than or equality check *)
      let* () = sub_then_dec ~dec ~x0 ~x1 ~x2 ~y0 ~y1 ~y2 in
      if
        not
          Bignum_bigint.(
            Field.size
            = of_string
                "28948022309329048855892746252171976963363056481941560715954676764349967630337")
      then failwith "Fp size assumption wrong" ;
      let fp0 = Field.(of_string "93054740644568405314109441") in
      let fp1 = Field.(of_string "147213319177") in
      let fp2 = Field.(of_string "302231454903657293676544") in
      (let c f = Bigint.of_field f |> Bigint.to_bignum_bigint in
       assert (
         Bignum_bigint.(c fp0 + (c fp1 * c l) + (c fp2 * c l2) = Field.size) )
      ) ;
      assert (Field.(fp0 + (fp1 * l) + (fp2 * l2) |> equal (of_int 0))) ;
      let* () =
        sub_then_dec
          ~dec:Field.(constant typ one)
          ~x0:(constant Field.typ fp0) ~x1:(constant Field.typ fp1)
          ~x2:(constant Field.typ fp2) ~y0 ~y1 ~y2
      in
      Checked.return ()
  end

  module Key = struct
    type t = Token_id.t

    type var = Token_id.Checked.t

    let typ = Token_id.typ
  end

  let assert_x_less_than_y_less_than_z ~(x : Key.var) ~(y : Key.var)
      ~(z : Key.var) =
    (* pretty sure of_field is supposed to be of_field_unsafe, and to_field_unsafe is supposed to be to_field *)
    let x = Token_id.Checked.to_field_unsafe x in
    let y = Token_id.Checked.to_field_unsafe y in
    let z = Token_id.Checked.to_field_unsafe z in
    let* () = assert_greater_than_full ~check:Boolean.true_ z y in
    let*| () = assert_greater_than_full ~check:Boolean.true_ y x in
    ()

  let height = 32
end)

module Stack_frame = struct
  include Mina_base.Stack_frame.Digest

  type var = Checked.t
end

module Call_stack = struct
  include Mina_base.Call_stack_digest

  type var = Checked.t
end

module Account_update_index = struct
  include Mina_numbers.Index

  type var = Checked.t
end

module Local_state = struct
  type t =
    { ledger : Ledger_hash.t
    ; stack_frame : Stack_frame.t
    ; call_stack : Call_stack.t
    ; transaction_commitment : F.t
    ; full_transaction_commitment : F.t
    ; excess : Currency.Amount.Signed.t
    ; account_update_index : Account_update_index.t
    }
  [@@deriving snarky, yojson]

  let to_mina_var ~supply_increase
      { ledger
      ; stack_frame
      ; call_stack
      ; transaction_commitment
      ; full_transaction_commitment
      ; excess
      ; account_update_index
      } : Mina_state.Local_state.Checked.t =
    { stack_frame
    ; call_stack
    ; transaction_commitment
    ; full_transaction_commitment
    ; excess
    ; account_update_index
    ; ledger
    ; supply_increase
    ; failure_status_tbl = ()
    ; will_succeed = Boolean.true_
    ; success = Boolean.true_
    }

  let dummy : var =
    { ledger = Ledger_hash.(constant typ empty_hash)
    ; stack_frame =
        Stack_frame.create Mina_base.Stack_frame.empty
        |> constant Stack_frame.typ
    ; call_stack = Call_stack.(constant empty)
    ; transaction_commitment =
        constant F.typ Zkapp_command.Transaction_commitment.empty
    ; full_transaction_commitment =
        constant F.typ Zkapp_command.Transaction_commitment.empty
    ; excess = Currency.Amount.Signed.(constant typ zero)
    ; account_update_index = Account_update_index.(constant typ zero)
    }
end

module Zeko_stmt = struct
  type t =
    { source_ledger : Ledger_hash.t
    ; target_ledger : Ledger_hash.t
    ; source_acc_set : Account_set.t
    ; target_acc_set : Account_set.t
    ; sequencer : Even_PC.t
    ; fee_excess : Currency.Amount.Signed.t
    ; slot_range : Slot_range.t
    ; source_local_state : Local_state.t
    ; target_local_state : Local_state.t
    }
  [@@deriving snarky, yojson]
end

module T = struct
  type t = { stmt : Zeko_stmt.t; proof : Proof_V.t } [@@deriving snarky]

  let to_yojson ({ stmt; proof } : t) =
    `Assoc
      [ ("stmt", Zeko_stmt.to_yojson stmt); ("proof", Proof.to_yojson proof) ]

  let of_yojson json =
    let open Ppx_deriving_yojson_runtime in
    let stmt = Zeko_stmt.of_yojson json in
    let proof = Proof.of_yojson json in
    stmt >>= fun stmt -> proof >>= fun proof -> Ok ({ stmt; proof } : t)
end

type update_acc_set_witness =
  { get_account_set_x : unit -> Token_id.t
  ; get_account_set_z : unit -> Token_id.t
  ; get_account_set_x_path : unit -> Account_set.Path.t
  ; get_account_set_y_path : unit -> Account_set.Path.t
  }

module Base_witness = struct
  type t =
    { ledger_path_handler : Handler.t
    ; update_acc_set_witness : update_acc_set_witness
    }
end

module Base_witness_V = Mk_V (Base_witness)

module Base_input = struct
  type t =
    { source_ledger : Ledger_hash.t
    ; source_acc_set : Account_set.t
    ; sequencer : Even_PC.t
    ; transaction : Mina_transaction.Transaction_union.t
    ; witness : Base_witness_V.t
    }
  [@@deriving snarky]
end

module Merge_input = struct
  type t = { left : T.t; right : T.t } [@@deriving snarky]
end

module Zkapp_witness = struct
  type t =
    { txn_snark_witness : Transaction_snark.Zkapp_command_segment.Witness.t
    ; update_acc_set_witness : update_acc_set_witness
    }
end

module Zkapp_witness_V = Mk_V (Zkapp_witness)

module Zkapp_rule_input = struct
  type t =
    { source_ledger : Ledger_hash.t
    ; target_ledger : Ledger_hash.t
    ; connecting_ledger : Ledger_hash.t
    ; source_local_state : Local_state.t
    ; target_local_state : Local_state.t
    ; fee_excess : Currency.Fee.Signed.t
    ; supply_decrease : Currency.Amount.t
    ; witness : Zkapp_witness_V.t
    ; sequencer : Even_PC.t
    ; source_acc_set : Account_set.t
    }
  [@@deriving snarky]
end

module Zkapp_single_unproved_input = struct
  type t = { base : Zkapp_rule_input.t; shift_action_state : Boolean.t }
  [@@deriving snarky]
end

module Zkapp_double_unproved_input = struct
  type t =
    { base : Zkapp_rule_input.t
    ; shift_action_state_first : Boolean.t
    ; shift_action_state_second : Boolean.t
    }
  [@@deriving snarky]
end

module Verification_key = struct
  include Pickles.Side_loaded.Verification_key

  type var = Checked.t
end

module Zkapp_single_proved_input = struct
  type t =
    { base : Zkapp_rule_input.t
    ; zkapp_vk : Verification_key.t
    ; zkapp_proof : Proof_V.t
    ; shift_action_state : Boolean.t
    }
  [@@deriving snarky]
end

let dummy_pc_init = Pending_coinbase.Stack.empty

let genesis_constants = Genesis_constants.Compiled.genesis_constants

let consensus_constants =
  Consensus.Constants.create ~constraint_constants
    ~protocol_constants:genesis_constants.protocol

(** Dummy state body, network preconditions are disabled anyway *)
let dummy_state_body =
  let compile_time_genesis =
    Mina_state.Genesis_protocol_state.t
      ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
      ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
      ~constraint_constants ~consensus_constants
      ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference
  in
  Mina_state.Protocol_state.body compile_time_genesis.data

let dummy_pc =
  Pending_coinbase.Stack.push_state
    (Mina_state.Protocol_state.Body.hash dummy_state_body)
    Mina_numbers.Global_slot_since_genesis.zero dummy_pc_init

let accumulate (f : ('a -> unit) -> 'b Checked.t) : ('b * 'a list) Checked.t =
  let acc = ref [] in
  let running = ref true in
  let*| r =
    f (fun x ->
        assert !running ;
        acc := x :: !acc )
  in
  running := false ;
  (r, !acc)

let account_with_hash (account : Account.Checked.Unhashed.t) :
    (Account.Checked.Unhashed.t, Field.Var.t lazy_t) With_hash.t =
  With_hash.of_data account ~hash_data:(fun a ->
      lazy
        (let a =
           { a with
             zkapp = (Zkapp_account.Checked.digest a.zkapp, ref (Some None))
           }
         in
         Run.run_checked (Account.Checked.digest a) ) )

let perform ~(shift_action_states : Boolean.var list)
    ~(set_slot_range : Slot_range.var -> unit)
    ~(set_account_new : Account_id.var * Boolean.var Checked.t -> unit) =
  let shift_action_states = ref shift_action_states in
  fun (type r)
      (eff :
        ( r
        , < bool : Boolean.var
          ; account :
              (Account.Checked.Unhashed.t, Field.Var.t lazy_t) With_hash.t
          ; local_state :
              ( Transaction_snark.Base.Zkapp_command_snark.stack_frame
              , Transaction_snark.Base.Zkapp_command_snark.call_stack
              , Currency.Amount.Signed.var
              , Ledger_hash.var * Mina_ledger.Sparse_ledger.t Prover_value.t
              , Boolean.var
              , Field.Var.t
              , Transaction_snark.Base.Zkapp_command_snark.length
              , unit )
              Mina_transaction_logic.Zkapp_command_logic.Local_state.t
          ; .. > )
        Mina_transaction_logic.Zkapp_command_logic.Eff.t ) : r ->
    match eff with
    | Check_valid_while_precondition
        ((valid_while : Zkapp_precondition.Valid_while.Checked.t), _global_state)
      ->
        let ({ lower; upper } : _ Zkapp_precondition.Closed_interval.t) =
          Zkapp_basic.Or_ignore.Checked.data valid_while
        in
        (* NB: We don't need to check whether valid_while is Some, because even if it is
           None, nothing will break.
        *)
        set_slot_range { lower; upper } ;
        (* We always return true because failure doesn't happen here but in the commit rule. *)
        Boolean.true_
    | Check_protocol_state_precondition
        ( (protocol_state_predicate :
            Zkapp_precondition.Protocol_state.Checked.t )
        , _global_state ) ->
        Run.run_checked
          Zkapp_precondition.Protocol_state.(
            assert_equal ~label:__LOC__ typ protocol_state_predicate
              (constant typ accept)) ;
        Boolean.true_
    | Check_account_precondition
        ( ({ account_update; _ } : Zkapp_call_forest.Checked.account_update)
        , (account : _ With_hash.t)
        , new_account
        , local_state ) ->
        let local_state = ref local_state in
        let check _failure b = Run.Boolean.Assert.is_true b in
        Zkapp_precondition.Account.Checked.check ~new_account ~check
          account_update.data.preconditions.account account.data ;
        !local_state
    | Init_account
        { account_update =
            ({ account_update; _ } : Zkapp_call_forest.Checked.account_update)
        ; account : (Account.Checked.Unhashed.t, Field.Var.t lazy_t) With_hash.t
        } ->
        let account_id =
          Account_id.Checked.create account_update.data.public_key
            account_update.data.token_id
        in
        let is_new =
          PC.Checked.equal account.data.public_key PC.(constant typ empty)
        in
        set_account_new (account_id, is_new) ;
        let account' : Account.Checked.Unhashed.t =
          { account.data with
            public_key = account_update.data.public_key
          ; token_id = account_update.data.token_id
          }
        in
        account_with_hash account'
    | Get_shift_action_state _ -> (
        match !shift_action_states with
        | [] ->
            failwith "unexpected"
        | x :: xs ->
            shift_action_states := xs ;
            x )

let derive_token_id ~owner =
  make_checked @@ fun () -> Account_id.Checked.derive_token_id ~owner

let update_acc_set accounts init ~witness =
  Checked.List.fold accounts ~init
    ~f:(fun set (account_id, is_empty_and_writeable) ->
      let open As_prover in
      let* x =
        exists Token_id.typ
          ~compute:(witness >>| fun x -> x.get_account_set_x ())
      in
      let* path_x =
        exists Account_set.Path.typ
          ~compute:(witness >>| fun x -> x.get_account_set_x_path ())
      in
      let* path_y =
        exists Account_set.Path.typ
          ~compute:(witness >>| fun x -> x.get_account_set_y_path ())
      in
      let* z =
        exists Token_id.typ
          ~compute:(witness >>| fun x -> x.get_account_set_z ())
      in
      let* y = derive_token_id ~owner:account_id in
      let* `Before_adding_y set', `After_adding_y new_set =
        Account_set.add_key_var ~x ~path_x ~y ~path_y ~z
          ~check:is_empty_and_writeable ()
      in
      let*| () = assert_equal ~label:__LOC__ Account_set.typ set set' in
      new_set )

let rule_signed_command input =
  let* { source_ledger; source_acc_set; transaction; sequencer; witness } =
    exists Base_input.typ ~compute:(V.get input)
  in
  let* (module Shifted) = Inner_curve.Checked.Shifted.create () in
  let* (target_ledger, fee_excess, _supply_increase), accounts =
    accumulate
    @@ fun set_account_new ->
    Fn.flip handle_as_prover
      As_prover.(
        V.get witness >>| fun { ledger_path_handler; _ } -> ledger_path_handler)
    @@ fun () ->
    Transaction_snark.Base.apply_tagged_transaction ~set_account_new
      ~constraint_constants
      (module Shifted)
      source_ledger Slot.Checked.zero
      (constant Pending_coinbase.Stack.typ dummy_pc_init)
      (constant Pending_coinbase.Stack.typ dummy_pc)
      (constant Pending_coinbase.Stack.typ dummy_pc)
      (constant
         (Mina_state.Protocol_state.Body.typ ~constraint_constants)
         dummy_state_body )
      transaction
  in
  let*| target_acc_set =
    update_acc_set accounts source_acc_set
      ~witness:As_prover.(V.get witness >>| fun x -> x.update_acc_set_witness)
  in
  let out : Zeko_stmt.var =
    { source_ledger
    ; target_ledger
    ; source_acc_set
    ; target_acc_set
    ; sequencer
    ; fee_excess
    ; slot_range = Slot_range.(constant typ infinite)
    ; source_local_state = Local_state.dummy
    ; target_local_state = Local_state.dummy
    }
  in
  Compile_simple.{ prevs = No_prevs; out }

let merge_slot_ranges (x : Slot_range.var) (y : Slot_range.var) :
    Slot_range.var Checked.t =
  let* lower =
    Slot.Checked.(x.lower < y.lower)
    >>= if_ ~typ:Slot.typ ~then_:y.lower ~else_:x.lower
  in
  let*| upper =
    Slot.Checked.(x.upper < y.upper)
    >>= if_ ~typ:Slot.typ ~then_:x.lower ~else_:y.lower
  in
  ({ lower; upper } : Slot_range.var)

let rule_zkapp ~shift_action_states ~spec
    Zkapp_rule_input.
      { source_ledger
      ; target_ledger
      ; connecting_ledger
      ; fee_excess
      ; supply_decrease
      ; source_local_state
      ; target_local_state
      ; witness
      ; sequencer
      ; source_acc_set
      } =
  let source : _ Mina_state.Registers.t =
    { first_pass_ledger = source_ledger
    ; second_pass_ledger = connecting_ledger
    ; pending_coinbase_stack = constant Pending_coinbase.Stack.typ dummy_pc
    ; local_state =
        Local_state.to_mina_var
          ~supply_increase:Currency.Amount.Signed.(constant typ zero)
          source_local_state
    }
  in
  let supply_increase =
    Currency.Amount.Signed.Checked.(of_unsigned supply_decrease |> negate)
  in
  let target : _ Mina_state.Registers.t =
    { first_pass_ledger = connecting_ledger
    ; second_pass_ledger = target_ledger
    ; pending_coinbase_stack = constant Pending_coinbase.Stack.typ dummy_pc
    ; local_state = Local_state.to_mina_var ~supply_increase target_local_state
    }
  in
  let stmt : Transaction_snark.Statement.With_sok.var =
    { source
    ; target
    ; connecting_ledger_left = connecting_ledger
    ; connecting_ledger_right = connecting_ledger
    ; supply_increase = Currency.Amount.Signed.(constant typ zero)
    ; fee_excess =
        { fee_token_l = Token_id.(Checked.constant default)
        ; fee_excess_l = fee_excess
        ; fee_token_r = Token_id.(Checked.constant default)
        ; fee_excess_r = Currency.Fee.Signed.(Checked.constant zero)
        }
    ; sok_digest = Mina_base.Sok_message.Digest.(constant typ default)
    }
  in
  let* ((zkapp_statement, _must_verify_zkapp), slot_ranges), accounts =
    accumulate
    @@ fun set_account_new ->
    accumulate
    @@ fun set_slot_range ->
    make_checked
    @@ fun () ->
    Transaction_snark.Base.Zkapp_command_snark.main
      ?witness:
        ( V.map ~f:(fun (x : Zkapp_witness.t) -> x.txn_snark_witness) witness
        |> V.unsafe_unwrap )
      ~zeko_handler:
        { perform =
            (fun eff ->
              perform ~shift_action_states ~set_slot_range ~set_account_new eff
              )
        }
      ~constraint_constants
      (Transaction_snark.Zkapp_command_segment.Basic.to_single_list spec)
      stmt
  in
  let* accounts =
    Checked.List.map accounts ~f:(fun (account, is_new) ->
        let*| is_new in
        (account, is_new) )
  in
  let* target_acc_set =
    update_acc_set accounts source_acc_set
      ~witness:
        ( V.map
            ~f:(fun (x : Zkapp_witness.t) -> x.update_acc_set_witness)
            witness
        |> V.get )
  in
  let*| slot_range =
    Checked.List.fold ~init:None slot_ranges ~f:(function
      | None ->
          fun x -> Checked.return (Some x)
      | Some x ->
          fun y -> merge_slot_ranges x y >>| fun x -> Some x )
    >>| Option.value ~default:Slot_range.(constant typ infinite)
  in
  let out : Zeko_stmt.var =
    { source_ledger
    ; target_ledger
    ; sequencer
    ; fee_excess = Currency.Amount.Signed.Checked.of_fee fee_excess
    ; slot_range
    ; source_local_state
    ; target_local_state
    ; source_acc_set
    ; target_acc_set
    }
  in
  (zkapp_statement, out)

let rule_merge input =
  let* { left =
           { stmt =
               { source_ledger
               ; target_ledger = left_target_ledger
               ; source_local_state
               ; target_local_state = left_target_local_state
               ; fee_excess = left_fee_excess
               ; sequencer = left_sequencer
               ; slot_range = left_slot_range
               ; source_acc_set
               ; target_acc_set = left_target_acc_set
               } as left_stmt
           ; proof = left_proof
           }
       ; right =
           { stmt =
               { source_ledger = right_source_ledger
               ; target_ledger
               ; source_local_state = right_source_local_state
               ; target_local_state
               ; fee_excess = right_fee_excess
               ; sequencer = right_sequencer
               ; slot_range = right_slot_range
               ; source_acc_set = right_source_acc_set
               ; target_acc_set
               } as right_stmt
           ; proof = right_proof
           }
       } =
    exists Merge_input.typ ~compute:(V.get input)
  in
  let* () =
    assert_equal ~label:__LOC__ Account_set.typ left_target_acc_set
      right_source_acc_set
  in
  let* () = Ledger_hash.assert_equal left_target_ledger right_source_ledger in
  let* () =
    assert_equal ~label:__LOC__ Local_state.typ left_target_local_state
      right_source_local_state
  in
  let* fee_excess =
    Currency.Amount.Signed.Checked.add left_fee_excess right_fee_excess
  in
  let* sequencer =
    assert_equal_safer ~label:__LOC__ Even_PC.typ left_sequencer right_sequencer
  in
  let*| slot_range = merge_slot_ranges left_slot_range right_slot_range in
  Compile_simple.
    { prevs =
        Two_prevs
          ( { public_input = left_stmt
            ; proof = left_proof
            ; proof_must_verify = Boolean.true_
            }
          , { public_input = right_stmt
            ; proof = right_proof
            ; proof_must_verify = Boolean.true_
            } )
    ; out =
        ({ source_ledger
         ; target_ledger
         ; source_local_state
         ; target_local_state
         ; fee_excess
         ; sequencer
         ; slot_range
         ; source_acc_set
         ; target_acc_set
         } : Zeko_stmt.var)
    }

include
  ( val Compile_simple.compile ~override_wrap_domain:`N1
          ~name:"zeko-transaction-snark" ~out_typ:Zeko_stmt.typ
          ~branches:
            [ { branch_name = "single-signed-command"
              ; tags = No_tags
              ; main = rule_signed_command
              }
            ; { branch_name = "single-unproved-zkapp-command"
              ; tags = No_tags
              ; main =
                  (fun input ->
                    let* { base; shift_action_state } =
                      exists Zkapp_single_unproved_input.typ
                        ~compute:(V.get input)
                    in
                    let*| _, out =
                      rule_zkapp base
                        ~shift_action_states:[ shift_action_state ]
                        ~spec:Opt_signed
                    in
                    Compile_simple.{ prevs = No_prevs; out } )
              }
            ; { branch_name = "double-unproved-zkapp-command"
              ; tags = No_tags
              ; main =
                  (fun input ->
                    let* { base
                         ; shift_action_state_first
                         ; shift_action_state_second
                         } =
                      exists Zkapp_double_unproved_input.typ
                        ~compute:(V.get input)
                    in
                    let*| _, out =
                      rule_zkapp base
                        ~shift_action_states:
                          [ shift_action_state_first
                          ; shift_action_state_second
                          ]
                        ~spec:Opt_signed_opt_signed
                    in
                    Compile_simple.{ prevs = No_prevs; out } )
              }
            ; { branch_name = "single-proved-zkapp-command"
              ; tags =
                  One_tag_sideloaded
                    { sideloaded_tag_name =
                        "single-proved-zkapp-command-sideloaded-vk"
                    ; typ = Zkapp_statement.typ
                    ; extract_vk =
                        (fun ({ zkapp_vk; _ } : Zkapp_single_proved_input.t) ->
                          Compile_simple.Verification_key.of_pickles zkapp_vk )
                    }
              ; main =
                  (fun input ->
                    let* { base; zkapp_vk; zkapp_proof; shift_action_state } =
                      exists Zkapp_single_proved_input.typ
                        ~compute:(V.get input)
                    in
                    let*| zkapp_statement, out =
                      rule_zkapp base
                        ~shift_action_states:[ shift_action_state ] ~spec:Proved
                    in
                    Compile_simple.
                      { prevs =
                          One_prev_sideloaded
                            { public_input = Option.value_exn zkapp_statement
                            ; proof = zkapp_proof
                            ; proof_must_verify = Boolean.true_
                            ; vk =
                                Compile_simple.Verification_key.var_of_pickles
                                  zkapp_vk
                            }
                      ; out
                      } )
              }
            ; { branch_name = "merge"; tags = Two_tags_own; main = rule_merge }
            ]
          () )

(* FIXME: remove for lazy compilation *)
let () = Promise.block_on_async_exn (fun () -> Compile_simple.force_tag tag)
