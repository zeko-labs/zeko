open Core_kernel
open Snark_params.Tick
open Mina_base
module PC = Signature_lib.Public_key.Compressed
open Zeko_util
open Txn_state

module Merge_input = struct
  type t =
    { left : Zeko_stmt.t
    ; left_proof : Proof_V.t
    ; right : Zeko_stmt.t
    ; right_proof : Proof_V.t
    }
  [@@deriving snarky]
end

let main input =
  let* { left =
           { source_ledger
           ; target_ledger = left_target_ledger
           ; source_local_state
           ; target_local_state = left_target_local_state
           ; accumulated_fees = left_fees
           ; sequencer = left_sequencer
           ; slot_range = left_slot_range
           ; source_acc_set
           ; target_acc_set = left_target_acc_set
           } as left_stmt
       ; left_proof
       ; right =
           { source_ledger = right_source_ledger
           ; target_ledger
           ; source_local_state = right_source_local_state
           ; target_local_state
           ; accumulated_fees = right_fees
           ; sequencer = right_sequencer
           ; slot_range = right_slot_range
           ; source_acc_set = right_source_acc_set
           ; target_acc_set
           } as right_stmt
       ; right_proof
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
  let* accumulated_fees =
    Currency.Amount.Signed.Checked.add left_fees right_fees
  in
  let* sequencer =
    assert_equal_safer ~label:__LOC__ Even_PC.typ left_sequencer right_sequencer
  in
  let*| slot_range = slot_range_intersection left_slot_range right_slot_range in
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
         ; accumulated_fees
         ; sequencer
         ; slot_range
         ; source_acc_set
         ; target_acc_set
         } : Zeko_stmt.var)
    }
