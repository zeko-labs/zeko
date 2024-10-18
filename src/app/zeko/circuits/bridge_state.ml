open Mina_base
open Zeko_util
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed

module Outer_bridge_state = struct
  type t =
    { disable_offset_lower : Checked32.t
    ; disable_offset_upper : Checked32.t
    ; disable_period : Checked32.t
    ; enable_offset_lower : Checked32.t
    ; enable_offset_upper : Checked32.t
    ; enable_period : Checked32.t
    }
  [@@deriving snarky]
end

module Inner_user_state = struct
  type t = { next_deposit : Checked32.t } [@@deriving snarky]

  type fine = { next_deposit : Checked32.var option }

  let fine (p : fine) : Fine.t = [ Whole (Checked32.typ, p.next_deposit) ]
end

module Outer_user_state = struct
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

(* When the token is the Mina token. *)
module Deposit_params_base = struct
  type t = { children : C.t; deposit : Deposit.t; holder_account_l1 : PC.t }
  [@@deriving snarky]

  let base (x : var) = x

  let custom _ = None
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
