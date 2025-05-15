open Core_kernel
open Mina_base
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Bridge_state
open Zeko_util
open Checked.Let_syntax

module Make (Inputs : sig
  val token_owner_l1 : Account_id.t option

  val holder_account_l1_permissions_enabled : Mina_base.Permissions.t

  val chain_l1 : Mina_signature_kind.t
end) =
struct
  open Inputs

  module May_use_token = struct
    include Account_update.May_use_token

    type var = Checked.t
  end

  module Witness = struct
    type t =
      { public_key : PC.t
      ; vk_hash : F.t
      ; may_use_token : May_use_token.t
      ; enable_offset_lower : Slot.t
      ; enable_offset_upper : Slot.t
      ; enable_period : Slot_span.t
      ; enabled_vk : F.t
      ; idx : Slot_span.t
      }
    [@@deriving snarky]
  end

  let main (w : Witness.t V.t) =
    let@ () = with_label ("main " ^ __LOC__) in
    let* Witness.
           { public_key
           ; vk_hash
           ; may_use_token
           ; enable_offset_lower
           ; enable_offset_upper
           ; enable_period
           ; enabled_vk
           ; idx
           } =
      exists Witness.typ ~compute:(V.get w)
    in
    let* lower =
      Slot_span.Checked.mul idx enable_period
      >>= Slot.Checked.add enable_offset_lower
    in
    let* upper =
      Slot_span.Checked.mul idx enable_period
      >>= Slot.Checked.add enable_offset_upper
    in
    let account_update =
      { default_account_update with
        public_key
      ; token_id = constant Token_id.typ (token_owner_id token_owner_l1)
      ; may_use_token
      ; authorization_kind = authorization_vk_hash vk_hash
      ; update =
          { default_account_update.update with
            permissions =
              Zkapp_basic.Set_or_keep.Checked.make_unsafe Boolean.true_
                (Mina_base.Permissions.Checked.constant
                   holder_account_l1_permissions_enabled )
          ; verification_key =
              Zkapp_basic.Set_or_keep.Checked.make_unsafe Boolean.true_
                ( { is_some = Boolean.true_
                  ; data = Data_as_hash.make_unsafe enabled_vk (ref None)
                  }
                  : _ Zkapp_basic.Flagged_option.t )
          }
      ; preconditions =
          { default_account_update.preconditions with
            account =
              { default_account_update.preconditions.account with
                state =
                  Outer_bridge_state.fine
                    { enable_offset_lower = Some enable_offset_lower
                    ; enable_offset_upper = Some enable_offset_upper
                    ; enable_period = Some enable_period
                    ; disable_offset_lower = None
                    ; disable_offset_upper = None
                    ; disable_period = None
                    ; disabled_vk = None
                    ; enabled_vk = Some enabled_vk
                    }
                  |> var_to_precondition_fine
              }
          ; valid_while = Slot_range.Checked.to_valid_while { lower; upper }
          }
      }
    in
    let*| out = make_outputs ~chain:chain_l1 account_update [] in
    Compile_simple.{ prevs = No_prevs; out }

  let rule : _ Compile_simple.branch =
    { branch_name = "enable holder account"; tags = No_tags; main }
end
