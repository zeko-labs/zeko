open Snark_params.Tick
open Zeko_util

module Definition = struct
  module Stmt = struct
    type t =
      { source_action_state : Rollup_state.Outer_action_state.t
      ; target_action_state : Rollup_state.Outer_action_state.t
      ; n_commits : Checked32.t
      }
    [@@deriving snarky]
  end

  module Elem = Rollup_state.Outer_action

  let dummy_elem =
    Rollup_state.Outer_action.Witness
      { aux = Field.zero
      ; children_digest = Rollup_state.Zkapp_call_forest.Digest.empty
      ; slot_range = Slot_range.infinite
      }

  module Init = struct
    type t = { original_action_state : Rollup_state.Outer_action_state.t }
    [@@deriving snarky]
  end

  let init ~check:_ ({ original_action_state } : Init.var) : Stmt.var Checked.t
      =
    Checked.return
      ( { source_action_state = original_action_state
        ; target_action_state = original_action_state
        ; n_commits = Checked32.Checked.zero
        }
        : Stmt.var )

  let step (action : Rollup_state.Outer_action.var)
      ({ source_action_state; target_action_state; n_commits } : Stmt.var) =
    let* target_action_state =
      Rollup_state.Outer_action.push_var action target_action_state
    in
    let*| n_commits =
      let* increment =
        if_ ~typ:Checked32.typ action.is_commit
          ~then_:Checked32.(Checked.constant one)
          ~else_:Checked32.Checked.zero
      in
      Checked32.Checked.add n_commits increment
    in
    Stmt.{ source_action_state; target_action_state; n_commits }

  let name = "count_commits"

  let leaf_iterations =
    Zeko_constants.Folder_iterations.Count_commits.leaf_iterations

  let leaf_option_iterations =
    Zeko_constants.Folder_iterations.Count_commits.leaf_option_iterations

  let extend_iterations =
    Zeko_constants.Folder_iterations.Count_commits.extend_iterations

  let extend_option_iterations =
    Zeko_constants.Folder_iterations.Count_commits.extend_option_iterations

  let wrap_domain = Some `N14
end

include Folder.Make (Definition) ()
