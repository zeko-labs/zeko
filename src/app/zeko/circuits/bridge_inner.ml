open Mina_base
open Zeko_util
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed

module Next_deposit = struct
  include Mina_numbers.Nat.Make32 ()

  type var = Checked.t
end

module Next_cancelled_deposit = struct
  include Mina_numbers.Nat.Make32 ()

  type var = Checked.t
end

module Next_withdrawal = struct
  include Mina_numbers.Nat.Make32 ()

  type var = Checked.t
end

module Inner_user_state = struct
  type t = { next_deposit : Next_deposit.t } [@@deriving snarky]

  type precondition = { next_deposit : Next_deposit.var option }

  let to_precondition (p : precondition) :
      F.var Or_ignore.Checked.t Zkapp_state.V.t =
    var_to_precondition_fine
      Var_to_precondition_fine.[ (Next_deposit.typ, p.next_deposit) ]
end

module Outer_user_state = struct
  type t =
    { next_cancelled_deposit : Next_cancelled_deposit.t
    ; next_withdrawal : Next_withdrawal.t
    }
  [@@deriving snarky]

  type precondition =
    { next_cancelled_deposit : Next_cancelled_deposit.var option
    ; next_withdrawal : Next_withdrawal.var option
    }

  let to_precondition (p : precondition) :
      F.var Or_ignore.Checked.t Zkapp_state.V.t =
    var_to_precondition_fine
      Var_to_precondition_fine.
        [ (Next_cancelled_deposit.typ, p.next_cancelled_deposit)
        ; (Next_withdrawal.typ, p.next_withdrawal)
        ]
end

module C = struct
  include Zkapp_call_forest

  type var = Checked.t
end

module Deposit = struct
  type t = { amount : Currency.Amount.t; recipient : PC.t; timeout : Slot.t }
  [@@deriving snarky]
end

module Withdrawal = struct
  type t = { amount : Currency.Amount.t; recipient : PC.t } [@@deriving snarky]
end

module A = struct
  include Account_update.Authorization_kind

  type var = Checked.t
end

module Deposit_params = struct
  type t =
    { authorization_kind : A.t
    ; nested_children : C.t
    ; call_data : F.t
    ; children : C.t
    ; deposit : Deposit.t
    ; holder_account_l1 : PC.t
    }
  [@@deriving snarky]
end

module Withdrawal_params = struct
  type t =
    { authorization_kind : A.t
    ; nested_children : C.t
    ; call_data : F.t
    ; children : C.t
    ; withdrawal : Withdrawal.t
    }
  [@@deriving snarky]
end
