open Core_kernel
open Mina_base
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Bridge_state
open Zeko_util

open struct
  module A = struct
    include Account_update.Body

    type var = Checked.t

    let typ = typ ()
  end
end

module Make (Inputs : sig
  val chain_l1 : Mina_signature_kind.t
end) =
struct
  open Inputs

  module Witness = struct
    type t = { public_key : PC.t; vk_hash : F.t; a : A.t } [@@deriving snarky]
  end

  let sdata = Zkapp_basic.Set_or_keep.Checked.data

  let idata = Zkapp_basic.Or_ignore.Checked.data

  (** Accounts with our token can do anything,
      except the updates to their app state is restricted.
      In a future version, this zkapp would restrict its caller
      to be the outer bridge accounts, but we can't do that,
      so we do the next best thing.
      
      The constraint is specifically that the indices in the app state
      can only go up. The other fields of the app state are also forced to
      not be set.
  *)
  let main (w : Witness.t V.t) =
    with_label ("main " ^ __LOC__)
    @@ fun () ->
    let* Witness.{ public_key; vk_hash; a } =
      exists Witness.typ ~compute:(V.get w)
    in
    let account_update =
      { default_account_update with
        public_key
      ; authorization_kind = authorization_vk_hash vk_hash
      }
    in
    let* { next_cancelled_deposit
         ; set_next_cancelled_deposit
         ; next_withdrawal
         ; set_next_withdrawal
         } =
      Outer_user_state.Set_or_keep.of_fields a.update.app_state
    in
    let* { next_cancelled_deposit = prev_next_cancelled_deposit
         ; set_next_cancelled_deposit = check_prev_next_cancelled_deposit
         ; next_withdrawal = prev_next_withdrawal
         ; set_next_withdrawal = check_prev_next_withdrawal
         } =
      let f or_ignore =
        let check = Zkapp_basic.Or_ignore.Checked.is_check or_ignore in
        let data = Zkapp_basic.Or_ignore.Checked.data or_ignore in
        Zkapp_basic.Set_or_keep.Checked.make_unsafe check data
      in
      Outer_user_state.Set_or_keep.of_fields
        (Pickles_types.Vector.map ~f a.preconditions.account.state)
    in
    (* check that it hasn't moved in reverse *)
    let* () =
      assert_var __LOC__
      @@ fun () ->
      Checked32.Checked.(next_cancelled_deposit >= prev_next_cancelled_deposit)
    in
    (* check that it's only set if it's checked, and not set if it's not checked,
       to prevent the above constraint from working when either of the vars
       aren't actually set to the real value. *)
    let* () =
      assert_equal ~label:__LOC__ Boolean.typ set_next_cancelled_deposit
        check_prev_next_cancelled_deposit
    in
    (* same as above, but for withdrawals *)
    (* check that it hasn't moved in reverse *)
    let* () =
      assert_var __LOC__
      @@ fun () -> Checked32.Checked.(next_withdrawal >= prev_next_withdrawal)
    in
    (* check that it's only set if it's checked, and not set if it's not checked,
       to prevent the above constraint from working when either of the vars
       aren't actually set to the real value. *)
    let* () =
      assert_equal ~label:__LOC__ Boolean.typ set_next_withdrawal
        check_prev_next_withdrawal
    in
    (* approve it *)
    let*| out = make_outputs ~chain:chain_l1 account_update [ (a, []) ] in
    Compile_simple.{ prevs = No_prevs; out }

  let rule : _ Compile_simple.branch lazy_t =
    lazy { branch_name = "outer token owner"; tags = No_tags; main }
end
