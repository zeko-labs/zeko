open Snark_params.Tick
open Mina_base
module PC = Signature_lib.Public_key.Compressed
open Zeko_util
open Txn_state

module Acc_set_witness = struct
  type t = update_acc_set_witness
end

module Acc_set_witness_V = Mk_V (Acc_set_witness)

module Ledger_path = struct
  module Step = struct
    type t = { hash_other : F.t; is_right : Boolean.t } [@@deriving snarky]
  end

  module Path =
    SnarkList
      (Step)
      (struct
        let length = Zeko_constants.constraint_constants.ledger_depth
      end)
end

module Action = struct
  type t =
    { source_ledger_hash : Ledger_hash.t
    ; target_ledger_hash : Ledger_hash.t
    ; source_acc_set : Account_set.t
    ; target_acc_set : Account_set.t
    ; ledger_index : Checked64.t
    ; account : Account.t
    }
  [@@deriving snarky]
end

module Witness = struct
  type t =
    { public_key : PC.t
    ; vk_hash : F.t
    ; old_account : Account.t
    ; new_account : Account.t
    ; ledger_path : Ledger_path.Path.t
    ; source_acc_set : Account_set.t
    ; acc_set_witness : Acc_set_witness_V.t
    }
  [@@deriving snarky]
end

let implied_root (account : Account.var) (path : Ledger_path.Path.var) =
  let* init = Account.Checked.digest account in
  Checked.List.foldi path ~init
    ~f:(fun height acc Ledger_path.Step.{ hash_other; is_right } ->
      let* left = Field.Checked.if_ is_right ~then_:hash_other ~else_:acc in
      let* right = Field.Checked.if_ is_right ~then_:acc ~else_:hash_other in
      make_checked @@ fun () -> Ledger_hash.merge_var ~height left right )

let index_of_path (path : Ledger_path.Path.var) : Checked64.var Checked.t =
  Checked.List.foldi path ~init:Checked64.Checked.zero
    ~f:(fun height acc Ledger_path.Step.{ is_right; _ } ->
      let* add =
        if_ is_right ~typ:Checked64.typ
          ~then_:(Checked64.Checked.constant (Checked64.of_int (1 lsl height)))
          ~else_:Checked64.Checked.zero
      in
      Checked64.Checked.add acc add )

module Make (Inputs : sig
  val chain_l1 : Mina_signature_kind.t
end) =
struct
  open Inputs

  let%snarkydef_ main (w : Witness.t V.t) =
    let* Witness.
           { public_key
           ; vk_hash
           ; old_account
           ; new_account
           ; ledger_path
           ; source_acc_set
           ; acc_set_witness
           } =
      exists Witness.typ ~compute:(V.get w)
    in
    let* source_root = implied_root old_account ledger_path in
    let* target_root = implied_root new_account ledger_path in
    let source_ledger_hash = Ledger_hash.var_of_hash_packed source_root in
    let target_ledger_hash = Ledger_hash.var_of_hash_packed target_root in
    let* is_new_account =
      let* old_account_digest = Account.Checked.digest old_account in
      Field.Checked.equal old_account_digest
        (constant F.typ (Lazy.force Account.empty_digest))
    in
    let* target_acc_set =
      update_acc_set
        [ ( Account_id.Checked.create new_account.public_key new_account.token_id
          , is_new_account )
        ]
        source_acc_set ~witness:(V.get acc_set_witness)
    in
    let* ledger_index = index_of_path ledger_path in
    let* actions =
      var_to_actions Action.typ
        Action.
          { source_ledger_hash
          ; target_ledger_hash
          ; source_acc_set
          ; target_acc_set
          ; ledger_index
          ; account = new_account
          }
    in
    let account_update =
      { default_account_update with
        public_key
      ; authorization_kind = authorization_vk_hash vk_hash
      ; actions
      }
    in
    let*| out = make_outputs ~chain:chain_l1 account_update [] in
    Compile_simple.{ prevs = No_prevs; out }

  let rule : _ Compile_simple.branch lazy_t =
    lazy { branch_name = "emergency da apply"; tags = No_tags; main }
end
