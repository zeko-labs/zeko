open Core_kernel
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

  let dummy = Field.zero

  module PathStep = struct
    type t = { hash_other : F.t; is_right : Boolean.t } [@@deriving snarky]
  end

  module Path = struct
    include
      SnarkList
        (PathStep)
        (struct
          let length = height
        end)
  end

  let hash_entry =
    var_to_hash ~init:Zeko_constants.indexed_merkle_tree_salt Entry.typ

  (** Two leaves are adjacent if and only if:
      1. Their lowest common ancestor is as deep as possible
      2. One leaf is the rightmost leaf of the left subtree
      3. The other is the leftmost leaf of the right subtree
    *)
  let are_paths_neighbors ~(l : Path.var) ~(r : Path.var) =
    let* _in_prefix, seen_div, valid =
      foldl
        (List.zip_exn l r |> List.rev)
        ~init:(Boolean.true_, Boolean.false_, Boolean.true_)
        ~f:(fun (in_prefix, seen_div, valid)
                ( { PathStep.is_right = l_is_right; _ }
                , { PathStep.is_right = r_is_right; _ } ) ->
          let* same = Boolean.equal l_is_right r_is_right in

          (* diverge happens exactly when we were in prefix and now differ *)
          let* diverge_now = Boolean.(in_prefix && not same) in

          (* At the divergence bit we require l=0 and r=1 *)
          let* div_ok =
            Boolean.Expr.(((not !l_is_right) && !r_is_right && !valid) |> eval)
          in

          (* After divergence we require l=1 and r=0 at every step *)
          let* tail_ok =
            Boolean.Expr.((!l_is_right && (not !r_is_right) && !valid) |> eval)
          in

          (* Update validity:
             - before divergence: no constraint
             - at divergence: enforce div_ok
             - after divergence: enforce tail_ok
          *)
          let* if_diverged_then_tail_ok =
            (* if already diverged earlier, enforce tail constraint *)
            let* enforce_tail = Boolean.(seen_div && not diverge_now) in
            if_ enforce_tail ~typ:Boolean.typ ~then_:tail_ok ~else_:valid
          in
          let* valid =
            if_ diverge_now ~typ:Boolean.typ ~then_:div_ok
              ~else_:if_diverged_then_tail_ok
          in

          (* Update flags *)
          let* seen_div = Boolean.(seen_div || diverge_now) in
          let*| in_prefix = Boolean.(in_prefix && same) in
          (in_prefix, seen_div, valid) )
    in
    (* Must have diverged at least once, otherwise same leaf *)
    Boolean.(valid && seen_div)

  (* TODO: consider different salt per level. *)
  (* NB: The first element in the list is the neighbor of init, and the next element
     is a level up, and so on. This is the same as what the Mina code base does. *)
  let implied_root_raw (init : F.var) (path : Path.var) : F.var Checked.t =
    foldl path ~init ~f:(fun acc { PathStep.hash_other; is_right } ->
        let* left, right =
          if_ is_right
            ~typ:Typ.(F.typ * F.typ)
            ~then_:(hash_other, acc) ~else_:(acc, hash_other)
        in
        var_to_hash ~init:Zeko_constants.indexed_merkle_tree_merge_salt
          Typ.(F.typ * F.typ)
          (left, right) )

  let implied_root (entry : Entry.var) (path : Path.var) : F.var Checked.t =
    let* init = hash_entry entry in
    implied_root_raw init path

  let add_key_var ~check ~x ~path_x ~y_prev_hash ~path_y_prev ~y ~path_y ~z () =
    let* () =
      with_label __LOC__ (fun () -> assert_x_less_than_y_less_than_z ~x ~y ~z)
    in
    let* root = implied_root { key = x; next_key = z } path_x in
    let* root_intermediate = implied_root { key = x; next_key = y } path_x in
    let* root_intermediate' =
      implied_root_raw Field.(constant typ zero) path_y
    in
    let* root_intermediate =
      if_ check ~typ:F.typ ~then_:root_intermediate ~else_:root_intermediate'
    in
    let* () =
      assert_equal ~label:__LOC__ F.typ root_intermediate root_intermediate'
    in
    let* root_new = implied_root { key = y; next_key = z } path_y in
    let* root = if_ check ~typ:F.typ ~then_:root ~else_:root_new in
    (* Check that the leaf before y is not empty *)
    let* () =
      assert_not_equal ~label:__LOC__ Field.typ y_prev_hash
        Field.(constant typ zero)
    in
    let* root_new' = implied_root_raw y_prev_hash path_y_prev in
    let* () = assert_equal ~label:__LOC__ F.typ root_new root_new' in
    (* Check that the path to leaf before y is really before y *)
    let* are_neighbors = are_paths_neighbors ~l:path_y_prev ~r:path_y in
    let* () =
      assert_equal ~label:__LOC__ Boolean.typ are_neighbors Boolean.true_
    in
    Checked.return (`Before_adding_y root, `After_adding_y root_new)

  let to_input_var = Random_oracle.Input.Chunked.field
end
