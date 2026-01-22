open Snark_params.Tick
open Mina_base
open Zeko_util

module Stmt = struct
  type t =
    { source_ledger : Ledger_hash.t
    ; target_ledger : Ledger_hash.t
    ; target_action_state : F.t
    }
  [@@deriving snarky]
end

module Elem = struct
  type t = { advance : Boolean.t; action : Rule_emergency_da.Action.t }
  [@@deriving snarky]
end

module M = struct
  module Elem = Elem

  let dummy_elem : Elem.t =
    { advance = false
    ; action =
        { source_ledger_hash = Ledger_hash.empty_hash
        ; target_ledger_hash = Ledger_hash.empty_hash
        ; ledger_index = Checked32.zero
        ; account = Account.empty
        }
    }

  module Stmt = Stmt
  module Init = Stmt

  let init ~check:_ (x : Init.var) = Checked.return x

  let step (elem : Elem.var) (stmt : Stmt.var) =
    let* eq =
      Ledger_hash.equal_var stmt.target_ledger elem.action.source_ledger_hash
    in
    let* ok =
      if_ elem.advance ~typ:Boolean.typ ~then_:eq ~else_:Boolean.true_
    in
    let@ () = with_label __LOC__ in
    let* () = Boolean.Assert.is_true ok in
    let* target_ledger =
      if_ elem.advance ~typ:Ledger_hash.typ
        ~then_:elem.action.target_ledger_hash ~else_:stmt.target_ledger
    in
    let* actions = var_to_actions Rule_emergency_da.Action.typ elem.action in
    let* target_action_state =
      make_checked (fun () ->
          Zkapp_account.Actions.push_events_checked stmt.target_action_state
            actions )
    in
    Checked.return { stmt with target_ledger; target_action_state }

  let leaf_iterations =
    Zeko_constants.Folder_iterations.Emergency_da.leaf_iterations

  let leaf_option_iterations =
    Zeko_constants.Folder_iterations.Emergency_da.leaf_option_iterations

  let extend_iterations =
    Zeko_constants.Folder_iterations.Emergency_da.extend_iterations

  let extend_option_iterations =
    Zeko_constants.Folder_iterations.Emergency_da.extend_option_iterations

  let name = "emergency da action fold"

  let wrap_domain = Some `N15
end

include Folder.Make (M) ()
