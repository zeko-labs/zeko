open Core_kernel
open Snark_params.Tick
open Zeko_util
open Checked.Let_syntax

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

  let empty_path =
    List.init height ~f:Fn.id
    |> List.fold_map ~init:Field.zero ~f:(fun acc _ ->
           let next =
             Random_oracle.hash
               ~init:
                 (Hash_prefix_create.salt
                    Zeko_constants.indexed_merkle_tree_merge_salt )
               [| acc; acc |]
           in
           (next, constant Field.typ acc) )
    |> snd

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

  let add_key_var ~check ~x ~path_x ~y ~path_y ~z () =
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
    (* Check that no empty indices have been skipped. *)
    let* is_y_most_left =
      foldl (List.zip_exn path_y empty_path) ~init:Boolean.true_
        ~f:(fun acc (PathStep.{ hash_other; is_right }, empty_hash) ->
          let* is_valid_left =
            Field.Checked.equal empty_hash hash_other >>= Boolean.( &&& ) acc
          in
          let* is_valid_right =
            Field.Checked.equal empty_hash hash_other
            >>| Boolean.not >>= Boolean.( &&& ) acc
          in
          if_ is_right ~typ:Boolean.typ ~then_:is_valid_right
            ~else_:is_valid_left )
    in
    let* () =
      if_ check ~typ:Boolean.typ ~then_:is_y_most_left ~else_:Boolean.true_
      >>= Boolean.Assert.is_true
    in
    Checked.return (`Before_adding_y root, `After_adding_y root_new)

  let to_input_var = Random_oracle.Input.Chunked.field
end
