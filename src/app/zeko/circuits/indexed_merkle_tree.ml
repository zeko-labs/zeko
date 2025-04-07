open Snark_params.Tick
open Zeko_util

module Make (Inputs : sig
  module Key : SnarkType

  val assert_x_less_than_y_less_than_z :
    x:Key.var -> y:Key.var -> z:Key.var -> unit Checked.t

  val height : int
end) =
struct
  open Inputs

  module Entry = struct
    type t = { key : Key.t; next_key : Key.t } [@@deriving snarky]
  end

  type t = F.t

  type var = F.var

  let typ = F.typ

  module PathStep = struct
    type t = { hash_other : F.t; is_right : Boolean.t } [@@deriving snarky]
  end

  module Path =
    SnarkList
      (PathStep)
      (struct
        let length = height
      end)

  let hash_entry =
    var_to_hash ~init:Zeko_constants.indexed_merkle_tree_salt Entry.typ

  (* TODO: consider different salt per level. *)
  let implied_root_raw (init : F.var) (path : Path.var) : F.var Checked.t =
    Checked.List.fold path ~init ~f:(fun acc { hash_other; is_right } ->
        let* left, right =
          if_ is_right
            ~typ:Typ.(F.typ * F.typ)
            ~then_:(hash_other, acc) ~else_:(acc, hash_other)
        in
        var_to_hash ~init:"indexed merkle tree" Typ.(F.typ * F.typ) (left, right) )

  let implied_root (entry : Entry.var) (path : Path.var) : F.var Checked.t =
    let* init = hash_entry entry in
    implied_root_raw init path

  let add_key_var ?check ~x ~path_x ~y ~path_y ~z () =
    let* () =
      with_label __LOC__ (fun () -> assert_x_less_than_y_less_than_z ~x ~y ~z)
    in
    let* root = implied_root { key = x; next_key = z } path_x in
    let* root_intermediate = implied_root { key = x; next_key = y } path_x in
    let* root_intermediate' =
      implied_root_raw Field.(constant typ zero) path_y
    in
    let* root_intermediate =
      match check with
      | Some check ->
          if_ check ~typ:F.typ ~then_:root_intermediate
            ~else_:root_intermediate'
      | None ->
          Checked.return root_intermediate
    in
    let* () =
      assert_equal ~label:__LOC__ F.typ root_intermediate root_intermediate'
    in
    let* root_new = implied_root { key = y; next_key = z } path_y in
    let* root_new =
      match check with
      | Some check ->
          if_ check ~typ:F.typ ~then_:root_new ~else_:root
      | None ->
          Checked.return root_new
    in
    Checked.return (`Before_adding_y root, `After_adding_y root_new)
end
