open Core_kernel
open Mina_base
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Bridge_state
open Zeko_util
open Checked.Let_syntax

module Make (Inputs : sig
  val token_owner_l1 : Account_id.t option

  val holder_account_l1_permissions_disabled : Mina_base.Permissions.t

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
      ; disable_offset_lower : Slot.t
      ; disable_offset_upper : Slot.t
      ; disable_period : Slot_span.t
      ; disabled_vk : F.t
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
           ; disable_offset_lower
           ; disable_offset_upper
           ; disable_period
           ; disabled_vk
           ; idx
           } =
      exists Witness.typ ~compute:(V.get w)
    in
    let* lower =
      Slot_span.Checked.mul idx disable_period
      >>= Slot.Checked.add disable_offset_lower
    in
    let* upper =
      Slot_span.Checked.mul idx disable_period
      >>= Slot.Checked.add disable_offset_upper
    in
    let* data_data =
      exists (Typ.prover_value ())
        ~compute:
          (let+| hash = As_prover.read_var disabled_vk in
           { With_hash.data = None; hash } )
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
                   holder_account_l1_permissions_disabled )
          ; verification_key =
              Zkapp_basic.Set_or_keep.Checked.make_unsafe Boolean.true_
                ( { is_some = Boolean.true_
                  ; data = Data_as_hash.make_unsafe disabled_vk data_data
                  }
                  : _ Zkapp_basic.Flagged_option.t )
          }
      ; preconditions =
          { default_account_update.preconditions with
            account =
              { default_account_update.preconditions.account with
                state =
                  Outer_bridge_state.fine
                    { disable_offset_lower = Some disable_offset_lower
                    ; disable_offset_upper = Some disable_offset_upper
                    ; disable_period = Some disable_period
                    ; enable_offset_lower = None
                    ; enable_offset_upper = None
                    ; enable_period = None
                    ; enabled_vk = None
                    ; disabled_vk = Some disabled_vk
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
    { branch_name = "disable holder account"; tags = No_tags; main }
end
