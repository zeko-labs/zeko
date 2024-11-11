open Mina_base
open Zeko_util
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed

module Outer_bridge_state = struct
  type t =
    { disable_offset_lower : Slot.t
    ; disable_offset_upper : Slot.t
    ; disable_period : Slot_span.t
    ; enable_offset_lower : Slot.t
    ; enable_offset_upper : Slot.t
    ; enable_period : Slot_span.t
    ; enabled_vk : F.t
    ; disabled_vk : F.t
    }
  [@@deriving snarky]

  type fine =
    { disable_offset_lower : Slot.var option
    ; disable_offset_upper : Slot.var option
    ; disable_period : Slot_span.var option
    ; enable_offset_lower : Slot.var option
    ; enable_offset_upper : Slot.var option
    ; enable_period : Slot_span.var option
    ; enabled_vk : F.var option
    ; disabled_vk : F.var option
    }

  let fine
      ({ disable_offset_lower
       ; disable_offset_upper
       ; disable_period
       ; enable_offset_lower
       ; enable_offset_upper
       ; enable_period
       ; enabled_vk
       ; disabled_vk
       } :
        fine ) : Fine.t =
    [ Whole (Slot.typ, disable_offset_lower)
    ; Whole (Slot.typ, disable_offset_upper)
    ; Whole (Slot_span.typ, disable_period)
    ; Whole (Slot.typ, enable_offset_lower)
    ; Whole (Slot.typ, enable_offset_upper)
    ; Whole (Slot_span.typ, enable_period)
    ; Whole (F.typ, enabled_vk)
    ; Whole (F.typ, disabled_vk)
    ]
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

open struct
  module C = struct
    include Zkapp_call_forest

    type var = Checked.t
  end

  module A = struct
    include Account_update.Authorization_kind

    type var = Checked.t
  end
end

(* When the token is the Mina token. *)
module Deposit_params_base = struct
  type t =
    { children : C.t
    ; holder_account_l1 : PC.t
    ; amount : Currency.Amount.t
    ; recipient : PC.t
    ; timeout : Slot.t
    }
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

(* When the token is the Mina token. *)
module Withdrawal_params_base = struct
  type t = { children : C.t; amount : Currency.Amount.t; recipient : PC.t }
  [@@deriving snarky]

  let base (x : var) = x

  let custom _ = None
end

(* When the token is custom, and we need token owner authorization. *)
module Withdrawal_params_custom = struct
  type t =
    { authorization_kind : A.t
    ; nested_children : C.t
    ; call_data : F.t
    ; base : Withdrawal_params_base.t
    }
  [@@deriving snarky]

  let base { base; _ } : Withdrawal_params_base.var = base

  let custom x = Some x
end
