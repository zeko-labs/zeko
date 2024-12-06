open Snark_params.Tick
module PC := Signature_lib.Public_key.Compressed

module Stack_frame : sig
  include module type of Mina_base.Stack_frame.Digest

  type var = Checked.t
end

module Call_stack : sig
  include module type of Mina_base.Call_stack_digest

  type var = Checked.t
end

module Account_update_index : sig
  include module type of Mina_numbers.Index

  type var = Checked.t
end

module Local_state : sig
  type t =
    { ledger : Mina_base.Ledger_hash.t
    ; stack_frame : Stack_frame.t
    ; call_stack : Call_stack.t
    ; transaction_commitment : Zeko_util.F.t
    ; full_transaction_commitment : Zeko_util.F.t
    ; excess : Currency.Amount.Signed.t
    ; account_update_index : Account_update_index.t
    }
  [@@deriving snarky]

  val to_mina_var :
       supply_increase:Currency.Amount.Signed.var
    -> var
    -> Mina_state.Local_state.Checked.t

  val dummy : var
end

module Zeko_stmt : sig
  type t =
    { source_ledger : Mina_base.Ledger_hash.t
    ; target_ledger : Mina_base.Ledger_hash.t
    ; sequencer : PC.t
    ; fee_excess : Currency.Fee.Signed.t
    ; slot_range : Zeko_util.Slot_range.t
    ; source_local_state : Local_state.t
    ; target_local_state : Local_state.t
    }
  [@@deriving snarky]
end

module T : sig
  type t = { stmt : Zeko_stmt.t; proof : Zeko_util.Proof_V.t }
  [@@deriving snarky]
end

module Handler_V : sig
  type t = Handler.t

  type var = t V.t

  val typ : (var, t) Typ.t
end

module Base_input : sig
  type t =
    { source_ledger : Mina_base.Ledger_hash.t
    ; target_ledger : Mina_base.Ledger_hash.t
    ; fee_excess : Currency.Fee.Signed.t
    ; sequencer : PC.t
    ; transaction : Mina_transaction.Transaction_union.t
    ; handler : Handler_V.t
    }
  [@@deriving snarky]
end

module Merge_input : sig
  type t = { left : T.t; right : T.t }

  type var = { left : T.var; right : T.var }

  val typ : (var, t) Typ.t
end

module Witness_V : sig
  type t = Transaction_snark.Zkapp_command_segment.Witness.t

  type var = t V.t

  val typ : (var, t) Typ.t
end

module Zkapp_rule_input : sig
  type t =
    { source_ledger : Mina_base.Ledger_hash.t
    ; target_ledger : Mina_base.Ledger_hash.t
    ; connecting_ledger : Mina_base.Ledger_hash.t
    ; source_local_state : Local_state.t
    ; target_local_state : Local_state.t
    ; fee_excess : Currency.Fee.Signed.t
    ; supply_decrease : Currency.Amount.t
    ; witness : Witness_V.t
    ; sequencer : PC.t
    }
  [@@deriving snarky]
end

module Zkapp_single_unproved_input : sig
  type t =
    { base : Zkapp_rule_input.t; shift_action_state : Zeko_util.Boolean.t }
end

module Zkapp_double_unproved_input : sig
  type t =
    { base : Zkapp_rule_input.t
    ; shift_action_state_first : Zeko_util.Boolean.t
    ; shift_action_state_second : Zeko_util.Boolean.t
    }
end

module Verification_key : sig
  include module type of Pickles.Side_loaded.Verification_key

  type var = Checked.t
end

module Zkapp_single_proved_input : sig
  type t =
    { base : Zkapp_rule_input.t
    ; zkapp_vk : Verification_key.t
    ; zkapp_proof : Zeko_util.Proof_V.t
    ; shift_action_state : Zeko_util.Boolean.t
    }
  [@@deriving snarky]
end

type tag_var

type tag_t

val tag : tag_var Compile_simple.tag

val provers :
  ( Zeko_stmt.t
  , ( Base_input.t
    , ( Zkapp_single_unproved_input.t
      , ( Zkapp_double_unproved_input.t
        , ( Zkapp_single_proved_input.t
          , ( Merge_input.t
            , Compile_simple.nil_branch )
            Compile_simple.cons_branch )
          Compile_simple.cons_branch )
        Compile_simple.cons_branch )
      Compile_simple.cons_branch )
    Compile_simple.cons_branch )
  Compile_simple.provers

type t

type var

val typ : (var, t) Typ.t

val get :
     ?check:Boolean.var
  -> var
  -> (Zeko_stmt.var * tag_var Compile_simple.prev) Checked.t

val make_unchecked : ?proof:Compile_simple.Proof.t -> Zeko_stmt.t -> t
