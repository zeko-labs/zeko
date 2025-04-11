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

  let dummy_pc =
    Pending_coinbase.Stack.push_state
      (Mina_state.Protocol_state.Body.hash dummy_state_body)
      Mina_numbers.Global_slot_since_genesis.zero dummy_pc_init
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
    ; witness : Base_witness_V.t
    }
  [@@deriving snarky]
end

let main input =
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
    ; accumulated_fees = fee_excess
    ; slot_range = Slot_range.(constant typ infinite)
    ; global_slot_range = Slot_range.(constant typ infinite)
    ; source_local_state = Local_state.dummy
    ; target_local_state = Local_state.dummy
    }
  in
  Compile_simple.{ prevs = No_prevs; out }
