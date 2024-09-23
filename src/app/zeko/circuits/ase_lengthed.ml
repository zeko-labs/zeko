(** Rules for proving extensions of action states with length too *)

(* FIXME: Deduplicate and merge into Action_state_extension *)

open Core_kernel
open Mina_base
open Snark_params.Tick
open Zeko_util

module Len = struct
  include Mina_numbers.Nat.Make32 ()

  type var = Checked.t
end

(* FIXME create two separate versions, one with length, one without *)
module M = struct
  module Stmt = struct
    type t = { action_state : F.t; len : Len.t } [@@deriving snarky]
  end

  module Elem = Zkapp_account.Actions
  module ElemOption = F

  let elem_to_option (x : Zkapp_account.Actions.t) =
    Zkapp_account.Actions.hash x

  let elem_option_none = Outside_hash_image.t

  module Init = struct
    type t = unit

    type var = unit

    let typ = Typ.unit
  end

  let init ~check:_ () =
    Checked.return
      { action_state =
          Field.Var.constant Zkapp_account.Actions.empty_state_element
      ; len = Len.Checked.zero
      }

  let step ~check:_ actions ({ action_state; len } : Stmt.var) =
    let*| len = Len.Checked.succ len in
    Stmt.
      { action_state =
          Zkapp_account.Actions.push_events_checked action_state actions
      ; len
      }

  let step_option ~check:_ actions ({ action_state; len } : Stmt.var) =
    let* dummy_ref = As_prover.Ref.create (As_prover.return []) in
    (* Bad unsafe use, with mismatching data and hash, but it works *)
    let actions = Data_as_hash.make_unsafe actions dummy_ref in
    let* is_dummy =
      Field.Checked.equal
        (Data_as_hash.hash actions)
        (constant Field.typ Outside_hash_image.t)
    in
    let* action_state =
      Field.Checked.if_ is_dummy ~then_:action_state
        ~else_:(Zkapp_account.Actions.push_events_checked action_state actions)
    in
    let*| len = Len.Checked.succ_if len (Boolean.not is_dummy) in
    Stmt.{ action_state; len }

  let name = "action state"

  let leaf_iterations = Int.pow 2 16

  let leaf_option_iterations = Int.pow 2 15

  let extend_iterations = Int.pow 2 15

  let extend_option_iterations = Int.pow 2 14

  let override_wrap_domain = None
end

include Folder.Make (M)
module Action_state = M.Stmt
module Stmt = Trans
