open Core_kernel
open Mina_base
module Field = Snark_params.Tick.Field
module Entry = Indexed_merkle_tree.Entry.Stable.Latest
module Sparse = Indexed_merkle_tree.Sparse
module Sparse_tree = Sparse_ledger_lib.Sparse_ledger.Tree
module Sparse_t = Sparse_ledger_lib.Sparse_ledger.T

let new_account_keys ~changed_accounts ~ledger_openings =
  List.filter changed_accounts ~f:(fun (index, _) ->
      Account.equal
        (Mina_ledger.Sparse_ledger.get_exn ledger_openings index)
        Account.empty )
  |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  |> List.map ~f:(fun (_, account) ->
         Account_id.derive_token_id ~owner:(Account.identifier account) )

(** Return the keys whose target-tree paths are needed to reverse [new_keys]
    in insertion order. A later insertion can sit between an earlier key and
    its original predecessor, so skip such later keys when recovering that
    predecessor from the target tree. *)
let opening_keys ~find_lower new_keys =
  let find_predecessor index key =
    let later_keys = List.drop new_keys (index + 1) in
    let rec go key =
      match find_lower key with
      | None ->
          Or_error.errorf "No account-set predecessor for %s"
            (Token_id.to_string key)
      | Some predecessor
        when List.mem later_keys predecessor ~equal:Token_id.equal ->
          go predecessor
      | Some predecessor ->
          Ok predecessor
    in
    go key
  in
  let%map.Or_error predecessors =
    List.mapi new_keys ~f:find_predecessor |> Or_error.combine_errors
  in
  List.dedup_and_sort (new_keys @ predecessors) ~compare:Token_id.compare

let set_empty_leaf_exn (t : Sparse.t) index =
  let hash = Sparse.hash in
  let rec go height tree =
    match (height < 0, tree) with
    | true, Sparse_tree.Account _ ->
        Sparse_tree.Hash Field.zero
    | false, Sparse_tree.Node (_, left, right) ->
        let left, right =
          if index land (1 lsl height) <> 0 then (left, go (height - 1) right)
          else (go (height - 1) left, right)
        in
        Sparse_tree.Node
          ( Indexed_merkle_tree.Hash.merge ~height (hash left) (hash right)
          , left
          , right )
    | _ ->
        failwithf "Account-set opening has no leaf at index %d" index ()
  in
  { t with Sparse_t.tree = go (Sparse.depth t - 1) t.tree }

let find_predecessor_exn openings key =
  let predecessor = ref None in
  Sparse.iteri openings ~f:(fun index entry ->
      if
        (not (Entry.equal entry Entry.empty))
        && Token_id.compare entry.value key < 0
      then
        match !predecessor with
        | None ->
            predecessor := Some (index, entry)
        | Some (_, current) when Token_id.compare current.value entry.value < 0
          ->
            predecessor := Some (index, entry)
        | Some _ ->
            () ) ;
  Option.value_exn !predecessor
    ~message:
      (sprintf "No opened predecessor for account-set key %s"
         (Token_id.to_string key) )

(** Validate that [target_openings] is rooted at a tree obtained by inserting
    exactly [new_keys], in order, into [source_root]. Returns the independently
    recomputed target root. *)
let validate ~source_root ~new_keys target_openings =
  Or_error.try_with (fun () ->
      let target_root = Sparse.merkle_root_without_cache_exn target_openings in
      let source_openings =
        List.fold_right new_keys ~init:target_openings ~f:(fun key openings ->
            let key_index =
              Sparse.find_index_exn openings
                (Indexed_merkle_tree.Account_id.with_empty_key key)
            in
            let key_entry = Sparse.get_exn openings key_index in
            if not (Token_id.equal key_entry.value key) then
              failwithf "Account-set key opening mismatch for %s"
                (Token_id.to_string key) () ;
            let predecessor_index, predecessor =
              find_predecessor_exn openings key
            in
            if not (Token_id.equal predecessor.value_next key) then
              failwithf "Account-set predecessor does not point to %s"
                (Token_id.to_string key) () ;
            if Token_id.compare key key_entry.value_next >= 0 then
              failwithf "Account-set key %s is not below its successor"
                (Token_id.to_string key) () ;
            let openings = set_empty_leaf_exn openings key_index in
            Sparse.set_exn openings predecessor_index
              { predecessor with value_next = key_entry.value_next } )
      in
      let computed_source_root =
        Sparse.merkle_root_without_cache_exn source_openings
      in
      if not (Field.equal computed_source_root source_root) then
        failwith "Source account-set root mismatch" ;
      target_root )

let%test_unit "account-set transition is bound to its source root" =
  let depth = 8 in
  let tree = Indexed_merkle_tree.In_memory.create ~depth () in
  let source_key = Token_id.of_field (Field.of_int 10) in
  Indexed_merkle_tree.In_memory.insert_exn tree source_key ;
  let source_root = Indexed_merkle_tree.In_memory.merkle_root tree in
  (* Insert 20 before 15 so the final predecessor of 20 is not its source
     predecessor. This exercises the lineage reconstruction, not only a
     single insertion. *)
  let new_keys =
    [ Token_id.of_field (Field.of_int 20); Token_id.of_field (Field.of_int 15) ]
  in
  Indexed_merkle_tree.In_memory.insert_batch_exn tree new_keys ;
  let keys =
    opening_keys new_keys
      ~find_lower:(Indexed_merkle_tree.In_memory.find_lower_entry_tid tree)
    |> Or_error.ok_exn
  in
  let openings =
    Indexed_merkle_tree.Sparse.of_in_memory_subset ~logger:(Logger.create ())
      ~db:tree ~keys
  in
  let target_root =
    validate ~source_root ~new_keys openings |> Or_error.ok_exn
  in
  assert (
    Field.equal target_root (Indexed_merkle_tree.In_memory.merkle_root tree) ) ;
  assert (Result.is_error (validate ~source_root:Field.one ~new_keys openings))
