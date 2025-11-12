(** Rule to change the permissions of the zkapp. Used in emergency cases. *)

open Zeko_util
open Snark_params.Tick
open Rollup_state
open Mina_base

module Permissions = struct
  include Permissions

  type var = Permissions.Checked.t
end

module Witness = struct
  type t = { public_key : PC.t; vk_hash : F.t; permissions : Permissions.t }
  [@@deriving snarky]
end

module Make (Inputs : sig
  val chain_l1 : Mina_signature_kind.t
end)
() =
struct
  open Inputs

  let%snarkydef_ main (w : Witness.t V.t) =
    let* Witness.{ public_key; vk_hash; permissions } =
      exists ~compute:(V.get w) Witness.typ
    in
    let account_update =
      { default_account_update with
        public_key
      ; authorization_kind = authorization_vk_hash vk_hash
      ; update =
          { default_account_update.update with
            permissions = Zkapp_basic.Set_or_keep.Checked.set permissions
          }
      }
    in
    let*| out = make_outputs ~chain:chain_l1 account_update [] in
    Compile_simple.{ prevs = No_prevs; out }

  let rule : _ Compile_simple.branch lazy_t =
    lazy { branch_name = "zeko change permissions"; tags = No_tags; main }

  include
    ( val Compile_simple.compile ()
            ~out_typ:
              Snark_params.Tick.Typ.(Mina_base.Zkapp_statement.typ * V.typ)
            ~branches:[ rule ] ~name:"Rule_change_permissions" )
end
