open Core
open Signature_lib
open Merkle_ledger
open Mina_base
module Field = Snark_params.Tick.Field

module Location_at_depth : Merkle_ledger.Location_intf.S =
  Merkle_ledger.Location.T

module Location_binable = struct
  module Arg = struct
    type t = Location_at_depth.t =
      | Generic of Location.Bigstring.Stable.Latest.t
      | Account of Location_at_depth.Addr.Stable.Latest.t
      | Hash of Location_at_depth.Addr.Stable.Latest.t
    [@@deriving bin_io_unversioned, hash, sexp, compare]
  end

  type t = Arg.t =
    | Generic of Location.Bigstring.t
    | Account of Location_at_depth.Addr.t
    | Hash of Location_at_depth.Addr.t
  [@@deriving hash, sexp, compare]

  include Comparable.Make_binable (Arg)
  include Hashable.Make_binable (Arg) [@@deriving sexp, compare, hash, yojson]
end

module Kvdb : Intf.Key_value_database with type config := string =
  Rocksdb.Database

module Storage_locations : Intf.Storage_locations = struct
  let key_value_db_dir = "mina_key_value_db"
end

module Account_id = struct
  include Account_id

  let with_empty_key tid = create Public_key.Compressed.empty tid
end

module Entry = struct
  [%%versioned
  module Stable = struct
    module V2 = struct
      type t =
        { value : Token_id.Stable.V2.t; value_next : Token_id.Stable.V2.t }
      [@@deriving equal, compare, sexp, yojson]

      let to_latest = Fn.id

      let identifier { value; _ } = Account_id.with_empty_key value

      let balance _ = Currency.Balance.zero

      let empty =
        { value = Token_id.of_field Field.zero
        ; value_next = Token_id.of_field Field.zero
        }

      let token { value; _ } = value
    end
  end]

  let data_hash { value; value_next } =
    Random_oracle.hash
      ~init:(Hash_prefix_create.salt Zeko_constants.indexed_merkle_tree_salt)
      [| Token_id.to_field_unsafe value; Token_id.to_field_unsafe value_next |]
end

module Hash = struct
  module Arg = struct
    type t = Ledger_hash.Stable.Latest.t
    [@@deriving sexp, compare, hash, bin_io_unversioned]
  end

  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Ledger_hash.Stable.V1.t
      [@@deriving sexp, compare, hash, equal, yojson]

      let (_ : (t, Arg.t) Type_equal.t) = Type_equal.T

      let to_latest = Fn.id

      include Hashable.Make_binable (Arg)

      let to_base58_check = Ledger_hash.to_base58_check

      let merge ~height:_ (h1 : t) (h2 : t) =
        Random_oracle.hash
          ~init:
            (Hash_prefix_create.salt
               Zeko_constants.indexed_merkle_tree_merge_salt )
          [| (h1 :> Field.t); (h2 :> Field.t) |]
        |> Ledger_hash.of_hash

      let hash_account = Entry.data_hash

      let empty_account = Field.zero
    end
  end]

  let merge = Stable.Latest.merge
end

module Inputs = struct
  module Key = Public_key.Compressed
  module Token_id = Token_id
  module Account_id = Account_id

  module Balance = struct
    include Currency.Balance

    let to_int = to_nanomina_int
  end

  module Account = Entry.Stable.Latest
  module Hash = Hash.Stable.Latest
  module Kvdb = Kvdb
  module Location = Location_at_depth
  module Location_binable = Location_binable
  module Storage_locations = Storage_locations
end

module type Ledger_db_intf =
  Merkle_ledger.Intf.Ledger.DATABASE
    with module Location = Location_at_depth
    with module Addr = Location_at_depth.Addr
    with type root_hash := Ledger_hash.t
     and type hash := Ledger_hash.t
     and type key := Public_key.Compressed.t
     and type token_id := Token_id.t
     and type token_id_set := Token_id.Set.t
     and type account := Entry.t
     and type account_id_set := Account_id.Set.t
     and type account_id := Account_id.t
     and type zeko_kvdb := Kvdb.t

module type Database_intf = sig
  type t

  type index = int

  module Path : Merkle_path.S with type hash := Ledger_hash.t

  type witness =
    [ `X of Token_id.t ]
    * [ `X_path of Path.t ]
    * [ `Y_prev_hash of Field.t ]
    * [ `Y_prev_path of Path.t ]
    * [ `Y of Token_id.t ]
    * [ `Y_path of Path.t ]
    * [ `Z of Token_id.t ]

  val depth : t -> int

  val merkle_root : t -> Hash.t

  val create : ?directory_name:string -> depth:index -> unit -> t

  val create_of_entries_exn :
    ?directory_name:string -> depth:index -> Token_id.t list -> t * witness list

  val get_or_create_entry_exn :
    t -> Token_id.t -> [ `Added | `Existed ] * witness

  val find_lower_entry_tid : t -> Token_id.t -> Token_id.t option

  val close : t -> unit

  val num_entries : t -> int

  val make_checkpoint : t -> directory_name:string -> unit

  val create_checkpoint : t -> directory_name:string -> unit -> t

  val get_entry_by_tid : t -> Token_id.t -> Entry.t option

  val get_path_by_tid : t -> Token_id.t -> Path.t option

  val iteri : t -> f:(int -> Entry.t -> unit) -> unit

  val set_at_index_exn : t -> int -> Entry.t -> unit
end

let lowest_key = Token_id.of_field Field.zero

let highest_key =
  Token_id.of_field
    (Field.of_string Bigint.(Field.size - of_int 1 |> to_string))

let lowest_entry = { Entry.value = lowest_key; value_next = highest_key }

let highest_entry = { Entry.value = highest_key; value_next = highest_key }

let base_entries =
  [ (Account_id.with_empty_key lowest_key, lowest_entry)
  ; (Account_id.with_empty_key highest_key, highest_entry)
  ]

module Db : Database_intf = struct
  include Database.Make (Inputs)

  type witness =
    [ `X of Token_id.t ]
    * [ `X_path of Path.t ]
    * [ `Y_prev_hash of Field.t ]
    * [ `Y_prev_path of Path.t ]
    * [ `Y of Token_id.t ]
    * [ `Y_path of Path.t ]
    * [ `Z of Token_id.t ]

  module Db_error = struct
    [@@@warning "-4"] (* due to deriving sexp below *)

    type t =
      | Account_location_not_found
      | Out_of_leaves
      | Malformed_database of string
    [@@deriving sexp]

    let to_exn e = Exn.create_s ([%sexp_of: t] e)

    let raise e = raise (to_exn e)

    let ok_exn = function Ok x -> x | Error e -> raise e
  end

  let get_raw t location =
    Kvdb.get (zeko_kvdb t)
      ~key:(Location.serialize ~ledger_depth:(depth t) location)

  let get_generic t location =
    assert (Location.is_generic location) ;
    get_raw t location

  module Account_location = struct
    let build_location account_id =
      Location.build_generic
        (Bigstring.of_string
           ( "$"
           ^ Format.sprintf
               !"%{sexp: Public_key.Compressed.t}!%{sexp: Token_id.t}"
               (Account_id.public_key account_id)
               (Account_id.token_id account_id) ) )

    let get mdb key =
      match get_generic mdb (build_location key) with
      | None ->
          Error Db_error.Account_location_not_found
      | Some location_bin ->
          Location.parse ~ledger_depth:(depth mdb) location_bin
          |> Result.map_error ~f:(fun () ->
                 Db_error.Malformed_database "Invalid location" )
  end

  let num_entries = num_accounts

  let close = close

  let find_lower_entry_location_exn t tid =
    if Token_id.equal lowest_key tid then
      failwith "There is no lower key than the 0" ;
    let location_key =
      Account_location.build_location (Account_id.with_empty_key tid)
    in
    let prev_location_key =
      Kvdb.zeko_prev_key (zeko_kvdb t)
        ~key:(Location.serialize ~ledger_depth:(depth t) location_key)
      |> Location.parse ~ledger_depth:(depth t)
      |> Result.map_error ~f:(fun () ->
             Db_error.Malformed_database "Failed to parse prev location key" )
      |> Db_error.ok_exn
    in
    get_generic t prev_location_key
    |> Option.value_exn ~message:"Failed to find prev location"
    |> Location.parse ~ledger_depth:(depth t)
    |> Result.map_error ~f:(fun _ ->
           Db_error.Malformed_database "Failed to parse prev location" )
    |> Db_error.ok_exn

  let find_lower_entry_tid t tid =
    let lower_entry_location = find_lower_entry_location_exn t tid in
    get t lower_entry_location
    |> Option.map ~f:(fun entry -> Entry.(entry.value))

  let create ?directory_name ~depth () =
    let db = create ?directory_name ~depth () in
    let (_ : [ `Added | `Existed ] * Location_at_depth.t) =
      get_or_create_account db
        (Account_id.with_empty_key lowest_key)
        lowest_entry
      |> Or_error.ok_exn
    in
    let (_ : [ `Added | `Existed ] * Location_at_depth.t) =
      get_or_create_account db
        (Account_id.with_empty_key highest_key)
        highest_entry
      |> Or_error.ok_exn
    in
    db

  let get_or_create_entry_exn t tid =
    let lower_entry_location = find_lower_entry_location_exn t tid in
    let x_path = merkle_path t lower_entry_location in
    let lower_entry =
      get t lower_entry_location
      |> Result.of_option
           ~error:(Db_error.Malformed_database "Could not find lower entry")
      |> Db_error.ok_exn
    in
    match lower_entry.value_next with
    | tid' when Token_id.equal tid' tid ->
        let new_location =
          Account_location.get t (Account_id.with_empty_key tid)
          |> Db_error.ok_exn
        in
        let new_entry =
          get t new_location
          |> Result.of_option
               ~error:(Db_error.Malformed_database "Could not find new entry")
          |> Db_error.ok_exn
        in
        let y_prev_location =
          Location_at_depth.prev new_location
          |> Option.value_exn ~message:"Can't get prev Y for first entry"
        in
        let y_prev =
          get t y_prev_location
          |> Result.of_option
               ~error:(Db_error.Malformed_database "Could not find Y prev entry")
          |> Db_error.ok_exn
        in
        assert (
          Token_id.(lower_entry.value < new_entry.value)
          && Token_id.(new_entry.value < new_entry.value_next) ) ;
        ( `Existed
        , ( `X lower_entry.value
          , `X_path x_path
          , `Y_prev_hash (Entry.data_hash y_prev)
          , `Y_prev_path (merkle_path t y_prev_location)
          , `Y new_entry.value
          , `Y_path (merkle_path t new_location)
          , `Z new_entry.value_next ) )
    | z -> (
        let new_entry = { Entry.value = tid; value_next = z } in
        match
          get_or_create_account t (Account_id.with_empty_key tid) new_entry
        with
        | Ok (`Existed, _) ->
            Db_error.(raise (Malformed_database "Entry should've not existed"))
        | Error e ->
            Error.raise e
        | Ok (`Added, new_location) ->
            let lower_entry = { lower_entry with value_next = tid } in
            set t lower_entry_location lower_entry ;
            let y_prev_location =
              Location_at_depth.prev new_location
              |> Option.value_exn ~message:"Can't get prev Y for first entry"
            in
            let y_prev =
              get t y_prev_location
              |> Result.of_option
                   ~error:
                     (Db_error.Malformed_database "Could not find Y prev entry")
              |> Db_error.ok_exn
            in
            assert (
              Token_id.(lower_entry.value < new_entry.value)
              && Token_id.(new_entry.value < new_entry.value_next) ) ;
            ( `Added
            , ( `X lower_entry.value
              , `X_path x_path
              , `Y_prev_hash (Entry.data_hash y_prev)
              , `Y_prev_path (merkle_path t y_prev_location)
              , `Y new_entry.value
              , `Y_path (merkle_path t new_location)
              , `Z new_entry.value_next ) ) )

  let create_of_entries_exn ?directory_name ~depth tids =
    let t = create ?directory_name ~depth () in
    if num_entries t <> 2 then
      failwith "Called create_of_entries_exn on non-empty database"
    else
      ( t
      , List.map tids ~f:(fun tid ->
            let _added, witness = get_or_create_entry_exn t tid in
            witness ) )

  let get_entry_by_tid t tid =
    let%bind.Option location =
      match Account_location.get t (Account_id.with_empty_key tid) with
      | Ok location ->
          Some location
      | Error Db_error.Account_location_not_found ->
          None
      | Error e ->
          Db_error.raise e
    in
    get t location

  let get_path_by_tid t tid =
    let%map.Option location =
      match Account_location.get t (Account_id.with_empty_key tid) with
      | Ok location ->
          Some location
      | Error Db_error.Account_location_not_found ->
          None
      | Error e ->
          Db_error.raise e
    in
    merkle_path t location
end

module In_memory = struct
  module Path = Db.Path

  module Node_key = struct
    module T = struct
      type t = int * int [@@deriving compare, sexp, hash]
    end

    include T
    include Hashable.Make_plain (T)
  end

  type t =
    { depth : int
    ; mutable root : Hash.t
    ; mutable num_entries : int
    ; mutable next_free_leaf_index : int
    ; mutable keys : Token_id.Set.t
    ; key_to_leaf : (Token_id.t, int) Hashtbl.Poly.t
    ; leaf_to_entry : (int, Entry.t) Hashtbl.Poly.t
    ; node_hashes : (Node_key.t, Hash.t) Hashtbl.t
    ; empty_hashes : (int, Hash.t) Hashtbl.Poly.t
    }

  let depth t = t.depth

  let merkle_root t = t.root

  let num_entries t = t.num_entries

  let max_leaves t = 1 lsl t.depth

  let leaf_hash (entry : Entry.t) : Hash.t =
    Ledger_hash.of_hash (Entry.data_hash entry)

  let rec empty_hash t height =
    match Hashtbl.find t.empty_hashes height with
    | Some h ->
        h
    | None ->
        let h =
          if height = 0 then Hash.Stable.Latest.empty_account
          else
            let prev = empty_hash t (height - 1) in
            Hash.Stable.Latest.merge ~height:(height - 1) prev prev
        in
        Hashtbl.set t.empty_hashes ~key:height ~data:h ;
        h

  let get_node_hash t ~height ~index =
    Hashtbl.find t.node_hashes (height, index)
    |> Option.value ~default:(empty_hash t height)

  let set_leaf_entry t ~leaf_index (entry : Entry.t) =
    Hashtbl.set t.leaf_to_entry ~key:leaf_index ~data:entry ;
    Hashtbl.set t.node_hashes ~key:(0, leaf_index) ~data:(leaf_hash entry)

  let recompute_path_to_root t leaf_index =
    let idx = ref leaf_index in
    for height = 0 to t.depth - 1 do
      let parent = !idx / 2 in
      let left_index = if !idx % 2 = 0 then !idx else !idx - 1 in
      let right_index = left_index + 1 in
      let left_hash = get_node_hash t ~height ~index:left_index in
      let right_hash = get_node_hash t ~height ~index:right_index in
      let parent_hash = Hash.Stable.Latest.merge ~height left_hash right_hash in
      Hashtbl.set t.node_hashes ~key:(height + 1, parent) ~data:parent_hash ;
      idx := parent
    done ;
    t.root <- get_node_hash t ~height:t.depth ~index:0

  let recompute_after_leaf_changes t leaf_indices =
    List.iter leaf_indices ~f:(recompute_path_to_root t)

  let recompute_after_leaf_changes_batch t leaf_indices =
    let touched = Hashtbl.create (module Node_key) in
    List.iter leaf_indices ~f:(fun leaf_index ->
        let idx = ref leaf_index in
        for height = 0 to t.depth - 1 do
          let parent = !idx / 2 in
          Hashtbl.set touched ~key:(height + 1, parent) ~data:() ;
          idx := parent
        done ) ;
    for height = 1 to t.depth do
      let level_nodes =
        Hashtbl.keys touched
        |> List.filter ~f:(fun (h, _) -> h = height)
        |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare a b)
      in
      List.iter level_nodes ~f:(fun (_h, index) ->
          let child_height = height - 1 in
          let left_index = index * 2 in
          let right_index = left_index + 1 in
          let left_hash =
            get_node_hash t ~height:child_height ~index:left_index
          in
          let right_hash =
            get_node_hash t ~height:child_height ~index:right_index
          in
          let parent_hash =
            Hash.Stable.Latest.merge ~height:child_height left_hash right_hash
          in
          Hashtbl.set t.node_hashes ~key:(height, index) ~data:parent_hash )
    done ;
    t.root <- get_node_hash t ~height:t.depth ~index:0

  let find_lower_entry_tid t tid =
    let left, present, _right = Set.split t.keys tid in
    match present with Some _ -> Set.max_elt left | None -> Set.max_elt left

  let get_entry_by_tid t tid =
    let%bind.Option leaf = Hashtbl.find t.key_to_leaf tid in
    Hashtbl.find t.leaf_to_entry leaf

  let get_path_by_tid t tid =
    let%map.Option leaf_index = Hashtbl.find t.key_to_leaf tid in
    List.init t.depth ~f:(fun height ->
        let level_index = leaf_index lsr height in
        let bit_set = Int.(level_index land 1 <> 0) in
        let sibling_index = level_index lxor 1 in
        let sibling_hash = get_node_hash t ~height ~index:sibling_index in
        if bit_set then `Right sibling_hash else `Left sibling_hash )

  let alloc_leaf_exn t =
    if t.next_free_leaf_index >= max_leaves t then failwith "Out_of_leaves" ;
    let i = t.next_free_leaf_index in
    t.next_free_leaf_index <- t.next_free_leaf_index + 1 ;
    i

  let insert_bootstrap_leaf_exn t tid entry =
    let leaf = alloc_leaf_exn t in
    t.keys <- Set.add t.keys tid ;
    Hashtbl.set t.key_to_leaf ~key:tid ~data:leaf ;
    set_leaf_entry t ~leaf_index:leaf entry ;
    t.num_entries <- t.num_entries + 1 ;
    recompute_after_leaf_changes t [ leaf ]

  let create ~depth () =
    let t =
      { depth
      ; root = Hash.Stable.Latest.empty_account
      ; num_entries = 0
      ; next_free_leaf_index = 0
      ; keys = Token_id.Set.empty
      ; key_to_leaf = Hashtbl.Poly.create ()
      ; leaf_to_entry = Hashtbl.Poly.create ()
      ; node_hashes = Hashtbl.create (module Node_key)
      ; empty_hashes = Hashtbl.Poly.create ()
      }
    in
    ignore (empty_hash t depth : Hash.t) ;
    insert_bootstrap_leaf_exn t lowest_key lowest_entry ;
    insert_bootstrap_leaf_exn t highest_key highest_entry ;
    t

  let insert_exn t tid =
    let insert_without_recompute_exn t tid =
      if Hashtbl.mem t.key_to_leaf tid then failwith "Duplicate IMT key insert" ;
      let x_tid =
        find_lower_entry_tid t tid
        |> Option.value_exn ~message:"No lower key found for IMT insert"
      in
      let x_leaf = Hashtbl.find_exn t.key_to_leaf x_tid in
      let x_entry = Hashtbl.find_exn t.leaf_to_entry x_leaf in
      let z_tid = x_entry.value_next in
      let y_leaf = alloc_leaf_exn t in
      let y_entry = { Entry.value = tid; value_next = z_tid } in
      let x_entry' = { x_entry with value_next = tid } in
      t.keys <- Set.add t.keys tid ;
      Hashtbl.set t.key_to_leaf ~key:tid ~data:y_leaf ;
      set_leaf_entry t ~leaf_index:y_leaf y_entry ;
      set_leaf_entry t ~leaf_index:x_leaf x_entry' ;
      t.num_entries <- t.num_entries + 1 ;
      [ x_leaf; y_leaf ]
    in
    let touched = insert_without_recompute_exn t tid in
    recompute_after_leaf_changes t touched

  let insert_batch_exn t tids =
    let seen = Hashtbl.Poly.create () in
    List.iter tids ~f:(fun tid ->
        if Hashtbl.mem t.key_to_leaf tid then
          failwith "Duplicate IMT key insert" ;
        if Hashtbl.mem seen tid then failwith "Duplicate IMT key insert (batch)" ;
        Hashtbl.set seen ~key:tid ~data:() ) ;
    let needed = List.length tids in
    if t.next_free_leaf_index + needed > max_leaves t then
      failwith "Out_of_leaves" ;
    let touched =
      List.concat_map tids ~f:(fun tid ->
          (* Inline to avoid per-insert root recomputation *)
          if Hashtbl.mem t.key_to_leaf tid then
            failwith "Duplicate IMT key insert" ;
          let x_tid =
            find_lower_entry_tid t tid
            |> Option.value_exn ~message:"No lower key found for IMT insert"
          in
          let x_leaf = Hashtbl.find_exn t.key_to_leaf x_tid in
          let x_entry = Hashtbl.find_exn t.leaf_to_entry x_leaf in
          let z_tid = x_entry.value_next in
          let y_leaf = alloc_leaf_exn t in
          let y_entry = { Entry.value = tid; value_next = z_tid } in
          let x_entry' = { x_entry with value_next = tid } in
          t.keys <- Set.add t.keys tid ;
          Hashtbl.set t.key_to_leaf ~key:tid ~data:y_leaf ;
          set_leaf_entry t ~leaf_index:y_leaf y_entry ;
          set_leaf_entry t ~leaf_index:x_leaf x_entry' ;
          t.num_entries <- t.num_entries + 1 ;
          [ x_leaf; y_leaf ] )
    in
    recompute_after_leaf_changes_batch t touched
end

module Sparse = struct
  [%%versioned
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V1 = struct
      type t =
        ( Ledger_hash.Stable.V1.t
        , Account_id.Stable.V2.t
        , Entry.Stable.V2.t )
        Sparse_ledger_lib.Sparse_ledger.T.Stable.V2.t
      [@@deriving yojson, sexp]

      let to_latest = Fn.id
    end
  end]

  include Sparse_ledger_lib.Sparse_ledger.Make (Hash) (Account_id) (Entry)

  let of_db_subset ~logger ~db ~keys =
    [%log debug] "Creating sparse ledger from db subset" ;
    let sparse = of_hash ~depth:(Db.depth db) (Db.merkle_root db) in
    [%log debug] "Folding keys" ;
    List.fold keys ~init:sparse ~f:(fun sparse key ->
        let aid = Account_id.with_empty_key key in
        let entry =
          Db.get_entry_by_tid db key
          |> Option.value_exn ~message:"Could not find entry"
        in
        let path =
          Db.get_path_by_tid db key
          |> Option.value_exn ~message:"Could not find path"
        in
        add_path sparse path aid entry )

  let of_in_memory_subset ~logger ~db ~keys =
    [%log debug] "Creating sparse ledger from in-memory imt subset" ;
    let sparse =
      of_hash ~depth:(In_memory.depth db) (In_memory.merkle_root db)
    in
    List.fold keys ~init:sparse ~f:(fun sparse key ->
        let aid = Account_id.with_empty_key key in
        let entry =
          In_memory.get_entry_by_tid db key
          |> Option.value_exn ~message:"Could not find in-memory IMT entry"
        in
        let path =
          In_memory.get_path_by_tid db key
          |> Option.value_exn ~message:"Could not find in-memory IMT path"
        in
        add_path sparse path aid entry )
end

let%test_unit "in-memory imt matches db for deterministic inserts" =
  let depth = 8 in
  let db = Db.create ~depth () in
  let mem = In_memory.create ~depth () in
  let tids =
    [ 7; 2; 15; 3; 20; 11; 19; 4 ]
    |> List.map ~f:(fun i -> Token_id.of_field (Field.of_int i))
  in
  List.iter tids ~f:(fun tid ->
      ignore
        (Db.get_or_create_entry_exn db tid : [ `Added | `Existed ] * Db.witness) ;
      In_memory.insert_exn mem tid ) ;
  [%test_eq: Hash.t] (Db.merkle_root db) (In_memory.merkle_root mem) ;
  let keys = lowest_key :: highest_key :: tids in
  List.iter keys ~f:(fun tid ->
      [%test_eq: Entry.t option]
        (Db.get_entry_by_tid db tid)
        (In_memory.get_entry_by_tid mem tid) ;
      assert (
        Option.equal Db.Path.equal
          (Db.get_path_by_tid db tid)
          (In_memory.get_path_by_tid mem tid) ) ;
      if not (Token_id.equal tid lowest_key) then
        [%test_eq: Token_id.t option]
          (Db.find_lower_entry_tid db tid)
          (In_memory.find_lower_entry_tid mem tid) ) ;
  Db.close db

let%test_unit "in-memory imt duplicate insert errors" =
  let t = In_memory.create ~depth:8 () in
  let tid = Token_id.of_field (Field.of_int 42) in
  In_memory.insert_exn t tid ;
  assert (
    Result.is_error (Or_error.try_with (fun () -> In_memory.insert_exn t tid)) )

let%test_unit "in-memory imt batch insert preserves input order semantics" =
  let depth = 8 in
  let t_seq = In_memory.create ~depth () in
  let t_batch = In_memory.create ~depth () in
  let tids =
    [ 10; 7; 9; 8; 12; 11 ]
    |> List.map ~f:(fun i -> Token_id.of_field (Field.of_int i))
  in
  List.iter tids ~f:(In_memory.insert_exn t_seq) ;
  In_memory.insert_batch_exn t_batch tids ;
  [%test_eq: Hash.t]
    (In_memory.merkle_root t_seq)
    (In_memory.merkle_root t_batch) ;
  List.iter (lowest_key :: highest_key :: tids) ~f:(fun tid ->
      [%test_eq: Entry.t option]
        (In_memory.get_entry_by_tid t_seq tid)
        (In_memory.get_entry_by_tid t_batch tid) ;
      assert (
        Option.equal In_memory.Path.equal
          (In_memory.get_path_by_tid t_seq tid)
          (In_memory.get_path_by_tid t_batch tid) ) )

let%test_unit "in-memory imt out of leaves errors" =
  (* depth=1 has exactly two leaves, already occupied by sentinels *)
  let t = In_memory.create ~depth:1 () in
  let tid = Token_id.of_field (Field.of_int 5) in
  assert (
    Result.is_error (Or_error.try_with (fun () -> In_memory.insert_exn t tid)) )
