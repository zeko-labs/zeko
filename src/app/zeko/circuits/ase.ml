(** Rules for proving extensions of action states with length too *)

open Core_kernel
open Mina_base
open Snark_params.Tick
open Zeko_util
open Checked.Let_syntax

let push_events_checked state actions =
  make_checked (fun () ->
      Zkapp_account.Actions.push_events_checked state actions )

module M (Length : sig
  include SnarkType

  module Checked : sig
    val succ : var -> var Checked.t

    val succ_if : var -> Boolean.var -> var Checked.t
  end
end) (Inputs : sig
  val name : string
end) =
struct
  open Inputs

  module Stmt = struct
    type t = { action_state : F.t; length : Length.t } [@@deriving snarky]
  end

  module Elem = Zkapp_account.Actions
  module ElemOption = F

  let elem_to_option (x : Zkapp_account.Actions.t) =
    Zkapp_account.Actions.hash x

  let elem_option_none = Outside_hash_image.t

  module Init = Stmt

  let init ~check:_ x = Checked.return x

  let step ~check:_ actions ({ action_state; length } : Stmt.var) =
    let* length = Length.Checked.succ length in
    let*| action_state = push_events_checked action_state actions in
    Stmt.{ action_state; length }

  let step_option ~check:_ actions ({ action_state; length } : Stmt.var) =
    let* dummy_ref = As_prover.Ref.create (As_prover.return []) in
    (* Bad unsafe use, with mismatching data and hash, but it works *)
    let actions = Data_as_hash.make_unsafe actions dummy_ref in
    let* is_dummy =
      Field.Checked.equal
        (Data_as_hash.hash actions)
        (constant Field.typ Outside_hash_image.t)
    in
    let* action_state_else = push_events_checked action_state actions in
    let* action_state =
      Field.Checked.if_ is_dummy ~then_:action_state ~else_:action_state_else
    in
    let*| length = Length.Checked.succ_if length (Boolean.not is_dummy) in
    Stmt.{ action_state; length }

  let name = name

  (* FIXME: increase depending on whether length is used or not *)

  let leaf_iterations = Int.pow 2 11

  let leaf_option_iterations = Int.pow 2 10

  let extend_iterations = Int.pow 2 10

  let extend_option_iterations = Int.pow 2 9

  let override_wrap_domain = Some Pickles_base.Proofs_verified.N1
end

module Not_length = struct
  type t = unit

  type var = unit

  let typ = Typ.unit

  module Checked = struct
    let succ () = Checked.return ()

    let succ_if () _ = Checked.return ()
  end
end

module M_with_length =
  M
    (Checked32)
    (struct
      let name = "Ase with length"
    end)

module Made_with_length = Folder.Make (M_with_length)

module M_without_length =
  M
    (Not_length)
    (struct
      let name = "Ase without length"
    end)

module Made_without_length = Folder.Make (M_without_length)

type tag_with_length_t = Made_with_length.Trans.t

type tag_without_length_t = Made_without_length.Trans.t

type tag_with_length_var = Made_with_length.Trans.var

type tag_without_length_var = Made_without_length.Trans.var

let tag_with_length = Made_with_length.tag

let tag_without_length = Made_without_length.tag

module Make_with_length (Inputs : sig
  module Action_state : Rollup_state.Action_state_type

  module Action : sig
    include SnarkType

    val to_actions_var : var -> Mina_base.Zkapp_account.Actions.var Checked.t
  end

  val get_iterations : int
end) =
struct
  open Inputs

  module Made_2 = Made_with_length.Make (struct
    let get_iterations = get_iterations
  end)

  module Stmt = struct
    type t =
      { source : Action_state.With_length.t
      ; target : Action_state.With_length.t
      }
    [@@deriving snarky]
  end

  type t = Made_2.t

  type var = Made_2.var

  let typ = Made_2.typ

  let get ?check t :
      (Stmt.var * _ Pickles.Inductive_rule.Previous_proof_statement.t) Checked.t
      =
    let*| trans, verifier = Made_2.get ?check t in
    let source =
      Action_state.With_length.unsafe_var_of_fields
        ~state:(Action_state.unsafe_var_of_field trans.source.action_state)
        ~length:trans.source.length
    in
    let target =
      Action_state.With_length.unsafe_var_of_fields
        ~state:(Action_state.unsafe_var_of_field trans.target.action_state)
        ~length:trans.target.length
    in
    (({ source; target } : Stmt.var), verifier)

  let prove (action_state : Action_state.With_length.t) (actions : Action.t list)
      =
    let f action =
      run_and_check_exn
        ( constant Action.typ action |> Action.to_actions_var
        >>| As_prover.read Zkapp_account.Actions.typ )
    in
    let actions = List.map ~f actions in
    Made_2.prove
      { action_state = Action_state.With_length.raw action_state
      ; length = Action_state.With_length.length action_state
      }
      actions
end

module Make_without_length (Inputs : sig
  module Action_state : Rollup_state.Action_state_type

  module Action : sig
    include SnarkType

    val to_actions_var : var -> Mina_base.Zkapp_account.Actions.var Checked.t
  end

  val get_iterations : int
end) =
struct
  open Inputs

  module Made_2 = Made_without_length.Make (struct
    let get_iterations = get_iterations
  end)

  module Stmt = struct
    type t = { source : Action_state.t; target : Action_state.t }
    [@@deriving snarky]
  end

  type t = Made_2.t

  type var = Made_2.var

  let typ = Made_2.typ

  let get ?check t :
      (Stmt.var * _ Pickles.Inductive_rule.Previous_proof_statement.t) Checked.t
      =
    let*| trans, verifier = Made_2.get ?check t in
    let source = Action_state.unsafe_var_of_field trans.source.action_state in
    let target = Action_state.unsafe_var_of_field trans.target.action_state in
    (({ source; target } : Stmt.var), verifier)

  let prove (action_state : Action_state.t) (actions : Action.t list) =
    let f action =
      printf "preparing checked computation\n" ;
      let computation =
        constant Action.typ action |> Action.to_actions_var
        >>| As_prover.read Zkapp_account.Actions.typ
      in
      printf "running checked computation\n" ;
      run_and_check_exn computation
    in
    let actions = List.map ~f actions in
    Made_2.prove
      { action_state = Action_state.raw action_state; length = () }
      actions
end
