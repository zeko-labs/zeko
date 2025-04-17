open Core_kernel
open Snark_params.Tick

module Impl = struct
  let merge x y =
    Random_oracle.hash
      ~init:(Hash_prefix_create.salt "indexed merkle tree")
      [| x; y |]

  let empty_hash : int -> field =
    let rec go =
      lazy
        (Memo.of_comparable
           (module Int)
           (function
             | 0 ->
                 Field.zero
             | n ->
                 let prev = force go (n - 1) in
                 merge prev prev ) )
    in
    force go

  type index = int

  type t = field list

  (* kind level = level_z | level_s of level *)
  type level_z = |

  type 'level level_s = |

  type 'level level_witness =
    | Level_z : level_z level_witness
    | Level_s : 'level level_witness -> 'level level_s level_witness

  [@@@ocamlformat "disable"]

  include struct
  type level_1 = level_z level_s
  type level_2 = level_1 level_s
  type level_3 = level_2 level_s
  type level_4 = level_3 level_s
  type level_5 = level_4 level_s
  type level_6 = level_5 level_s
  type level_7 = level_6 level_s
  type level_8 = level_7 level_s
  type level_9 = level_8 level_s
  type level_10 = level_9 level_s
  type level_11 = level_10 level_s
  type level_12 = level_11 level_s
  type level_13 = level_12 level_s
  type level_14 = level_13 level_s
  type level_15 = level_14 level_s
  type level_16 = level_15 level_s
  type level_17 = level_16 level_s
  type level_18 = level_17 level_s
  type level_19 = level_18 level_s
  type level_20 = level_19 level_s
  type level_21 = level_20 level_s
  type level_22 = level_21 level_s
  type level_23 = level_22 level_s
  type level_24 = level_23 level_s
  type level_25 = level_24 level_s
  type level_26 = level_25 level_s
  type level_27 = level_26 level_s
  type level_28 = level_27 level_s
  type level_29 = level_28 level_s
  type level_30 = level_29 level_s
  type level_31 = level_30 level_s
  type level_32 = level_31 level_s
  type level_33 = level_32 level_s
  type level_34 = level_33 level_s
  let level_1 : level_1 level_witness = Level_s Level_z
  let level_2 : level_2 level_witness = Level_s level_1
  let level_3 : level_3 level_witness = Level_s level_2
  let level_4 : level_4 level_witness = Level_s level_3
  let level_5 : level_5 level_witness = Level_s level_4
  let level_6 : level_6 level_witness = Level_s level_5
  let level_7 : level_7 level_witness = Level_s level_6
  let level_8 : level_8 level_witness = Level_s level_7
  let level_9 : level_9 level_witness = Level_s level_8
  let level_10 : level_10 level_witness = Level_s level_9
  let level_11 : level_11 level_witness = Level_s level_10
  let level_12 : level_12 level_witness = Level_s level_11
  let level_13 : level_13 level_witness = Level_s level_12
  let level_14 : level_14 level_witness = Level_s level_13
  let level_15 : level_15 level_witness = Level_s level_14
  let level_16 : level_16 level_witness = Level_s level_15
  let level_17 : level_17 level_witness = Level_s level_16
  let level_18 : level_18 level_witness = Level_s level_17
  let level_19 : level_19 level_witness = Level_s level_18
  let level_20 : level_20 level_witness = Level_s level_19
  let level_21 : level_21 level_witness = Level_s level_20
  let level_22 : level_22 level_witness = Level_s level_21
  let level_23 : level_23 level_witness = Level_s level_22
  let level_24 : level_24 level_witness = Level_s level_23
  let level_25 : level_25 level_witness = Level_s level_24
  let level_26 : level_26 level_witness = Level_s level_25
  let level_27 : level_27 level_witness = Level_s level_26
  let level_28 : level_28 level_witness = Level_s level_27
  let level_29 : level_29 level_witness = Level_s level_28
  let level_30 : level_30 level_witness = Level_s level_29
  let level_31 : level_31 level_witness = Level_s level_30
  let level_32 : level_32 level_witness = Level_s level_31
  let level_33 : level_33 level_witness = Level_s level_32
  let level_34 : level_34 level_witness = Level_s level_33
  end

  [@@@ocamlformat "enable"]

  type 'level tree =
    | Empty : 'level tree
    | Node :
        { hash : field
        ; left : 'level tree
        ; right : 'level tree
        ; sparse : [ `Sparse | `Full ]
        }
        -> 'level level_s tree
    | Leaf : { hash : field } -> level_z tree

  type 'level location =
    | Loc_end : level_z location
    | Left_loc : 'level location -> 'level level_s location
    | Right_loc : 'level location -> 'level level_s location

  type 'level path =
    | End : level_z path
    | Left : 'level path * field -> 'level level_s path
    | Right : 'level path * field -> 'level level_s path

  let hash_of =
    let rec level_to_int : type level. level level_witness -> int = function
      | Level_z ->
          0
      | Level_s level ->
          1 + level_to_int level
    in
    let go : type level. level level_witness * level tree -> field = function
      | level, Empty ->
          level_to_int level |> empty_hash
      | _, Node { hash; left = _; right = _; sparse = _ } ->
          hash
      | _, Leaf { hash } ->
          hash
    in
    fun level tree -> go (level, tree)

  let rec get_path :
      type level.
      level level_witness * level tree * level location -> level path = function
    | _, Empty, _ ->
        failwith "invalid location"
    | Level_z, Leaf { hash = _ }, Loc_end ->
        End
    | ( Level_s level
      , Node { hash = _; left; right; sparse = _ }
      , Right_loc location ) ->
        Right (get_path (level, right, location), hash_of level left)
    | ( Level_s level
      , Node { hash = _; left; right; sparse = _ }
      , Left_loc location ) ->
        Left (get_path (level, left, location), hash_of level right)

  let mknode (type level) (level : level level_witness) (left : level tree)
      (right : level tree) : level level_s tree =
    let sparse =
      match (left, right) with
      | Empty, _
      | _, Empty
      | Node { hash = _; left = _; right = _; sparse = `Sparse }, _
      | _, Node { hash = _; left = _; right = _; sparse = `Sparse } ->
          `Sparse
      | (Node _ | Leaf _), (Node _ | Leaf _) ->
          `Full
    in
    Node
      { hash = merge (hash_of level left) (hash_of level right)
      ; left
      ; right
      ; sparse
      }

  type 'level full_tree =
    | Node :
        { hash : field; left : 'level full_tree; right : 'level full_tree }
        -> 'level level_s full_tree
    | Leaf : { hash : field } -> level_z full_tree

  type ('l, 'level) full_trees =
    | Nil : ('l, 'l) full_trees
    | Cons_full_tree :
        'l full_tree * ('l level_s, 'level) full_trees
        -> ('l, 'level) full_trees
    | Cons_empty_tree :
        ('l level_s, 'level) full_trees
        -> ('l, 'level) full_trees

  type 'l some_full_trees =
    | Some_full_trees : ('l, 'level) full_trees -> 'l some_full_trees

  let merge_full : 'l full_tree -> 'l full_tree -> 'l level_s full_tree =
    fun (type l) (left : l full_tree) (right : l full_tree) ->
     match (left, right) with
     | ( (Node { hash = left_hash; _ } | Leaf { hash = left_hash })
       , (Node { hash = right_hash; _ } | Leaf { hash = right_hash }) ) ->
         Node { hash = merge left_hash right_hash; left; right }

  let rec cons_full_trees :
            'l 'level.
            'l full_tree -> ('l, 'level) full_trees -> 'l some_full_trees =
    fun (type l level) (x : l full_tree) (xs : (l, level) full_trees) ->
     match xs with
     | Cons_full_tree (y, xs) ->
         let xy = merge_full x y in
         let (Some_full_trees r) = cons_full_trees xy xs in
         Some_full_trees (Cons_empty_tree r)
     | Cons_empty_tree xs ->
         Some_full_trees (Cons_full_tree (x, xs))
     | Nil ->
         Some_full_trees (Cons_full_tree (x, Nil))

  let rec of_full : 'l. 'l full_tree -> 'l tree =
    fun (type l) (x : l full_tree) : l tree ->
     match x with
     | Leaf { hash } ->
         Leaf { hash }
     | Node { hash; left; right } ->
         Node
           { hash; left = of_full left; right = of_full right; sparse = `Full }

  let rec of_full_trees :
            'l 'level.
               'l level_witness
            -> 'l tree
            -> ('l, 'level) full_trees
            -> 'level tree =
    fun (type l level) (level : l level_witness) (tree : l tree)
        (trees : (l, level) full_trees) : level tree ->
     match trees with
     | Nil ->
         tree
     | Cons_empty_tree xs ->
         of_full_trees (Level_s level) (mknode level tree Empty) xs
     | Cons_full_tree (y, xs) ->
         of_full_trees (Level_s level) (mknode level tree (of_full y)) xs

  let rec to_trees : t -> level_z some_full_trees = function
    | [] ->
        Some_full_trees Nil
    | x :: xs ->
        let (Some_full_trees r) = to_trees xs in
        cons_full_trees (Leaf { hash = x }) r

  type ('x, 'y, 'z) level_add =
    | Z : (level_z, 'y, 'y) level_add
    | S : ('x, 'y level_s, 'z) level_add -> ('x level_s, 'y, 'z) level_add

  let rec repeat_empty_cons :
            'diff 'level 'level_new.
               ('diff, 'level, 'level_new) level_add
            -> ('level, 'level_new) full_trees =
    fun (type diff level level_new)
        (level_add : (diff, level, level_new) level_add) :
        (level, level_new) full_trees ->
     match level_add with
     | Z ->
         Nil
     | S level_add ->
         Cons_empty_tree (repeat_empty_cons level_add)

  type ('x, 'y) eq = Refl : ('x, 'x) eq

  let rec level_add_inc :
            'x 'y 'z.
            ('x, 'y, 'z) level_add -> ('x level_s, 'y, 'z level_s) level_add =
    fun (type x y z) (x : (x, y, z) level_add) :
        (x level_s, y, z level_s) level_add ->
     match x with Z -> S Z | S x -> S (level_add_inc x)

  let rec level_add_id : 'x. 'x level_witness -> ('x, level_z, 'x) level_add =
    fun (type x) (x : x level_witness) : (x, level_z, x) level_add ->
     match x with Level_z -> Z | Level_s x -> level_add_inc (level_add_id x)

  (* when erased, equal to fun x y -> y *)
  let rec invert_level_add :
            'x 'y 'z.
            ('x, 'y, 'z) level_add -> 'y level_witness -> ('y, 'x, 'z) level_add
      =
    fun (type x y z) (level_add : (x, y, z) level_add) (y : y level_witness) :
        (y, x, z) level_add ->
     match level_add with
     | Z ->
         level_add_id y
     | S level_add ->
         let (S r) = invert_level_add level_add (Level_s y) in
         r

  let rec diff :
            'x 'y 'z.
            ('x, 'y, 'z) level_add -> 'z level_witness -> 'y level_witness =
    fun (type x y z) (x : (x, y, z) level_add) (z : z level_witness) :
        y level_witness ->
     match x with
     | Z ->
         z
     | S x ->
         let (Level_s y) = diff x z in
         y

  let rec level_witness_of_add :
            'x 'y 'z. ('x, 'y, 'z) level_add -> 'x level_witness =
    fun (type x y z) (x : (x, y, z) level_add) : x level_witness ->
     match x with Z -> Level_z | S x -> Level_s (level_witness_of_add x)

  let rec extend_full_trees :
            'l 'level 'level_new.
               ('level, 'level_new) full_trees
            -> ('l, 'level) full_trees
            -> ('l, 'level_new) full_trees =
    fun (type l level level_new) (extension : (level, level_new) full_trees)
        (xs : (l, level) full_trees) : (l, level_new) full_trees ->
     match xs with
     | Nil ->
         extension
     | Cons_empty_tree xs ->
         Cons_empty_tree (extend_full_trees extension xs)
     | Cons_full_tree (x, xs) ->
         Cons_full_tree (x, extend_full_trees extension xs)

  let mkext level_new level_add =
    repeat_empty_cons (invert_level_add level_add (diff level_add level_new))

  let rec get_max_level :
            'l 'l_max.
            'l level_witness -> ('l, 'l_max) full_trees -> 'l_max level_witness
      =
    fun (type l l_max) (l : l level_witness) (xs : (l, l_max) full_trees) :
        l_max level_witness ->
     match xs with
     | Nil ->
         l
     | Cons_empty_tree xs ->
         get_max_level (Level_s l) xs
     | Cons_full_tree (_, xs) ->
         get_max_level (Level_s l) xs

  type ('x, 'z) prove_lte_result =
    | Prove_lte_result : ('x, 'y, 'z) level_add -> ('x, 'z) prove_lte_result

  let rec prove_lte :
            'x 'z.
               'x level_witness
            -> 'z level_witness
            -> ('x, 'z) prove_lte_result option =
    fun (type x z) (x : x level_witness) (z : z level_witness) :
        (x, z) prove_lte_result option ->
     match (x, z) with
     | Level_z, _ ->
         Some (Prove_lte_result Z)
     | Level_s x, Level_s z -> (
         match prove_lte x z with
         | Some (Prove_lte_result lte) ->
             Some (Prove_lte_result (level_add_inc lte))
         | None ->
             None )
     | Level_s _, Level_z ->
         None

  let msb_only = Int64.(shift_left 1L 63)

  (* you can interpret the index as a location,
     you start from MSB, and go left if 0, and right if 1.
     You must shift the index the correct amount to the left before calling this function.
  *)
  let rec loc_of_index :
            'level. 'level level_witness -> int64 -> 'level location =
    fun (type level) (level : level level_witness) idx : level location ->
     let open Int64 in
     match level with
     | Level_z ->
         Loc_end
     | Level_s level ->
         let is_left = bit_and idx msb_only = zero in
         let tail = loc_of_index level (shift_left idx 1) in
         if is_left then Left_loc tail else Right_loc tail

  let rec simplify_path :
            'level. 'level path -> [ `Left of field | `Right of field ] list =
    fun (type level) (path : level path) ->
     match path with
     | End ->
         []
     | Left (xs, hash) ->
         `Left hash :: simplify_path xs
     | Right (xs, hash) ->
         `Right hash :: simplify_path xs
end

let hash_simple : field list -> field =
 fun xs ->
  let (Some_full_trees xs) = Impl.to_trees xs in
  let level = Impl.get_max_level Level_z xs in
  let (Prove_lte_result lte) =
    Impl.prove_lte level Impl.level_34 |> Option.value_exn
  in
  let xs = Impl.extend_full_trees (Impl.mkext Impl.level_34 lte) xs in
  let xs = Impl.of_full_trees Level_z Empty xs in
  Impl.hash_of Impl.level_34 xs

let path_simple :
    int64 -> field list -> [ `Left of field | `Right of field ] list =
 fun idx xs ->
  let (Some_full_trees xs) = Impl.to_trees xs in
  let level = Impl.get_max_level Level_z xs in
  let (Prove_lte_result lte) =
    Impl.prove_lte level Impl.level_34 |> Option.value_exn
  in
  let xs = Impl.extend_full_trees (Impl.mkext Impl.level_34 lte) xs in
  let xs = Impl.of_full_trees Level_z Empty xs in
  (* FIXME: is the shift correct? *)
  let loc = Impl.loc_of_index Impl.level_34 (Int64.shift_left idx (64 - 35)) in
  Impl.get_path (Impl.level_34, xs, loc) |> Impl.simplify_path
