open Core_kernel
open Snark_params.Tick
open Mina_base
module PC = Signature_lib.Public_key.Compressed
open Zeko_util
open Txn_state

open struct
  let dummy_pc_init = Pending_coinbase.Stack.empty

  let protocol_constants : Genesis_constants.Protocol.t =
    { k = 1
    ; slots_per_epoch = 1000
    ; slots_per_sub_window = 1
    ; grace_period_slots = 1
    ; delta = 1
    ; genesis_state_timestamp = Int64.one
    }

  let consensus_constants =
    Consensus.Constants.create ~constraint_constants ~protocol_constants

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
end

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
    ; global_slot : Slot.t
    ; witness : Base_witness_V.t
    }
  [@@deriving snarky]
end

let main input =
  let* { source_ledger
       ; source_acc_set
       ; transaction
       ; sequencer
       ; global_slot
       ; witness
       } =
    exists Base_input.typ ~compute:(V.get input)
  in
  let* (module Shifted) = Inner_curve.Checked.Shifted.create () in
  let pc_init = constant Pending_coinbase.Stack.typ dummy_pc_init in
  let state_body =
    constant
      (Mina_state.Protocol_state.Body.typ ~constraint_constants)
      dummy_state_body
  in
  (* [apply_tagged_transaction] uses [global_slot] both for timing/[valid_until]
     and to push the protocol state onto the pending-coinbase stack. The latter
     means the "after" stack it checks against is [push_state] at [global_slot],
     so we can no longer use the slot-0 [dummy_pc] constant. We pass [dummy_pc_init]
     as the "before" stack (making the [init = before] branch of its
     valid-init-state check hold) and recompute the "after" stack here at the
     witnessed slot. *)
  let* pending_coinbase_after =
    make_checked
    @@ fun () ->
    let state_body_hash =
      Run.run_checked (Mina_state.Protocol_state.Body.hash_checked state_body)
    in
    Run.run_checked
      (Pending_coinbase.Stack.Checked.push_state state_body_hash global_slot
         pc_init )
  in
  let* (target_ledger, fee_excess, _supply_increase), accounts =
    accumulate
    @@ fun zeko_set_account_new ->
    Fn.flip handle_as_prover
      As_prover.(
        V.get witness >>| fun { ledger_path_handler; _ } -> ledger_path_handler)
    @@ fun () ->
    Transaction_snark.Base.apply_tagged_transaction ~zeko_set_account_new
      ~constraint_constants
      (module Shifted)
      source_ledger global_slot pc_init pc_init pending_coinbase_after
      state_body transaction
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
    ; accumulated_fees = fee_excess
    ; (* [global_slot] is the slot applied to the transaction (timing checks and,
         for signed commands, the [valid_until] bound). Gate the commit on it
         through [slot_range], which becomes the commit [valid_while] and is
         checked by L1 against the inclusion block's slot. *)
      slot_range =
        ({ lower = global_slot; upper = constant Slot.typ Slot.max_value } : Slot_range
                                                                             .var)
    ; global_slot_range = Slot_range.(constant typ infinite)
    ; source_local_state = Local_state.dummy
    ; target_local_state = Local_state.dummy
    }
  in
  Compile_simple.{ prevs = No_prevs; out }
