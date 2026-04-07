(* Keeps focused sequencer inline tests in a separate module so the main
   sequencer implementation file stays centered on runtime behavior. *)

open Mina_base

let%test_unit "genesis sync labels the first replayed diff as genesis" =
  [%test_eq: bool]
    (Zeko_sequencer.Sequencer.replay_genesis_flag ~source:`Genesis
       ~current_chunk:0 ~current_diff:0 )
    true

let%test_unit "checkpoint sync never relabels replayed diffs as genesis" =
  [%test_eq: bool]
    (Zeko_sequencer.Sequencer.replay_genesis_flag
       ~source:(`Specific Ledger_hash.empty_hash)
       ~current_chunk:0 ~current_diff:0 )
    false
