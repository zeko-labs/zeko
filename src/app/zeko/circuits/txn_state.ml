open Snark_params.Tick
open Mina_base
module PC = Signature_lib.Public_key.Compressed
open Zeko_util

open struct
  module Stack_frame_digest = struct
    include Mina_base.Stack_frame.Digest

    type var = Checked.t
  end

  module Call_stack_digest = struct
    include Mina_base.Call_stack_digest

    type var = Checked.t
  end

  module Account_update_index = struct
    include Mina_numbers.Index

    type var = Checked.t
  end
end

module Local_state = struct
  type t =
    { stack_frame_digest : Stack_frame_digest.t
    ; call_stack_digest : Call_stack_digest.t
    ; transaction_commitment : F.t
    ; full_transaction_commitment : F.t
    ; excess : Currency.Amount.Signed.t
    ; account_update_index : Account_update_index.t
    }
  [@@deriving snarky]

  let dummy : var =
    { stack_frame_digest =
        Stack_frame_digest.create Mina_base.Stack_frame.empty
        |> constant Stack_frame_digest.typ
    ; call_stack_digest = Call_stack_digest.(constant empty)
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
    ; accumulated_fees : Currency.Amount.Signed.t
    ; slot_range : Slot_range.t
    ; source_local_state : Local_state.t
    ; target_local_state : Local_state.t
    }
  [@@deriving snarky]
end
