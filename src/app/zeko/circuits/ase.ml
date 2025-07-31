(** Rules for proving extensions of action states with length too *)

open Core_kernel
open Snark_params.Tick
open Zeko_util

module M_with_length = struct
  module Stmt = struct
    type t = { action_state : F.t; length : Checked32.t } [@@deriving snarky]
  end

  module Elem = F

  let dummy_elem = Field.zero

  module Init = Stmt

  let init ~check:_ x = Checked.return x

  let step actions ({ action_state; length } : Stmt.var) =
    let* length = Checked32.Checked.succ length in
    let*| action_state = push_actions_var action_state ~actions in
    Stmt.{ action_state; length }

  let name = "action state extension with length"

  let leaf_iterations = Int.pow 2 2

  let leaf_option_iterations = Int.pow 2 2

  let extend_iterations = Int.pow 2 2

  let extend_option_iterations = Int.pow 2 2

  let wrap_domain = Some `N14
end

module M_without_length = struct
  module Stmt = F
  module Elem = F

  let dummy_elem = Field.zero

  module Init = Stmt

  let init ~check:_ x = Checked.return x

  let step actions action_state = push_actions_var action_state ~actions

  let name = "action state extension without length"

  let leaf_iterations = Int.pow 2 2

  let leaf_option_iterations = Int.pow 2 2

  let extend_iterations = Int.pow 2 2

  let extend_option_iterations = Int.pow 2 2

  let wrap_domain = Some `N14
end

module Made_without_length = Folder.Make (M_without_length) ()

module Made_with_length = Folder.Make (M_with_length) ()

module With_length = struct
  include M_with_length
  include Made_with_length

  module Make (Inputs : sig
    module Action_state : Rollup_state.Action_state_type

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

    let get ?check t =
      let*| `Source source, `Target target, verifier =
        Made_2.get_full ?check t
      in
      let source =
        Action_state.With_length.unsafe_var_of_fields
          ~state:(Action_state.unsafe_var_of_field source.action_state)
          ~length:source.length
      in
      let target =
        Action_state.With_length.unsafe_var_of_fields
          ~state:(Action_state.unsafe_var_of_field target.action_state)
          ~length:target.length
      in
      (({ source; target } : Stmt.var), verifier)

    let make = Made_2.make
  end
end

module Without_length = struct
  include M_without_length
  include Made_without_length

  module Make (Inputs : sig
    module Action_state : Rollup_state.Action_state_type

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

    let get ?check t =
      let*| `Source source, `Target target, verifier =
        Made_2.get_full ?check t
      in
      let source = Action_state.unsafe_var_of_field source in
      let target = Action_state.unsafe_var_of_field target in
      (({ source; target } : Stmt.var), verifier)

    let make = Made_2.make
  end
end
