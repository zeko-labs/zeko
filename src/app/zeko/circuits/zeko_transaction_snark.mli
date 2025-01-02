open Snark_params.Tick
open Zeko_util

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

module Account_set : sig
  type t [@@deriving yojson]

  type var

  val typ : (var, t) Typ.t

  module PathStep : sig
    type t = { hash : F.t; is_left : Boolean.t } [@@deriving snarky]
  end

  module Path : sig
    type t = PathStep.t list

    type var = PathStep.var list

    val typ : (var, t) Typ.t
  end
end

module Zeko_stmt : sig
  type t =
    { source_ledger : Mina_base.Ledger_hash.t
    ; target_ledger : Mina_base.Ledger_hash.t
    ; source_acc_set : Account_set.t
    ; target_acc_set : Account_set.t
    ; sequencer : Even_PC.t
    ; fee_excess : Currency.Amount.Signed.t
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

type update_acc_set_witness =
  { get_account_set_x : unit -> Mina_base.Token_id.t
  ; get_account_set_z : unit -> Mina_base.Token_id.t
  ; get_account_set_x_path : unit -> Account_set.Path.t
  ; get_account_set_y_path : unit -> Account_set.Path.t
  }

module Base_witness : sig
  type t =
    { ledger_path_handler : Handler.t
    ; update_acc_set_witness : update_acc_set_witness
    }
end

module Base_witness_V : Zeko_util.V_S with type t = Base_witness.t

module Base_input : sig
  type t =
    { source_ledger : Mina_base.Ledger_hash.t
    ; source_acc_set : Account_set.t
    ; sequencer : Even_PC.t
    ; transaction : Mina_transaction.Transaction_union.t
    ; witness : Base_witness_V.t
    }
  [@@deriving snarky]
end

module Merge_input : sig
  type t = { left : T.t; right : T.t }

  type var = { left : T.var; right : T.var }

  val typ : (var, t) Typ.t
end

module Zkapp_witness : sig
  type t =
    { txn_snark_witness : Transaction_snark.Zkapp_command_segment.Witness.t
    ; update_acc_set_witness : update_acc_set_witness
    }
end

module Zkapp_witness_V : V_S with type t = Zkapp_witness.t

module Zkapp_rule_input : sig
  type t =
    { source_ledger : Mina_base.Ledger_hash.t
    ; target_ledger : Mina_base.Ledger_hash.t
    ; connecting_ledger : Mina_base.Ledger_hash.t
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
