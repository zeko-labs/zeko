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

      let merge = Ledger_hash.merge

      let hash_account = Entry.data_hash

      let empty_account =
        Ledger_hash.of_digest (Lazy.force Account.empty_digest)
    end
  end]

  let merge = Ledger_hash.merge
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

module type Indexed_merkle_tree_intf = sig
  include
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

  val get_or_create_entry_exn :
       t
    -> Token_id.t
    -> [> `Added | `Existed ]
       * ( [> `X of Token_id.t ]
         * [> `X_path of path ]
         * [> `Y of Token_id.t ]
         * [> `Y_path of path ]
         * [> `Z of Token_id.t ] )

  val find_lower_entry_aid_exn : t -> Token_id.t -> Token_id.t
end

module Db : Indexed_merkle_tree_intf = struct
  include Database.Make (Inputs)

  module Db_error = struct
    [@@@warning "-4"] (* due to deriving sexp below *)

    type t =
      | Account_location_not_found
      | Out_of_leaves
      | Malformed_database of string
    [@@deriving sexp]

    let ok_exn = function
      | Ok x ->
          x
      | Error e ->
          raise (Exn.create_s ([%sexp_of: t] e))
  end

  let lowest_key = Token_id.of_field Field.zero

  let highest_key =
    Token_id.of_field
      (Field.of_string Bigint.(Field.size - of_int 1 |> to_string))

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
  end

  let find_lower_entry_location_exn t tid =
    if Token_id.equal lowest_key tid then
      failwith "There is no lower key than the 0" ;
    let location_key =
      Account_location.build_location (Account_id.with_empty_key tid)
    in
    let prev_location_key =
      Kvdb.prev_key (zeko_kvdb t)
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

  let find_lower_entry_aid_exn t tid =
    let lower_entry_location = find_lower_entry_location_exn t tid in
    get t lower_entry_location
    |> Result.of_option
         ~error:(Db_error.Malformed_database "Could not find lower entry")
    |> Db_error.ok_exn
    |> fun entry -> entry.value

  let create ?directory_name ~depth () =
    let db = create ?directory_name ~depth () in
    let (_ : [ `Added | `Existed ] * Location_at_depth.t) =
      get_or_create_account db
        (Account_id.with_empty_key lowest_key)
        { Entry.value = lowest_key; value_next = highest_key }
      |> Or_error.ok_exn
    in
    let (_ : [ `Added | `Existed ] * Location_at_depth.t) =
      get_or_create_account db
        (Account_id.with_empty_key highest_key)
        { Entry.value = highest_key; value_next = highest_key }
      |> Or_error.ok_exn
    in
    db

  let get_or_create_entry_exn t tid =
    let lower_entry_location = find_lower_entry_location_exn t tid in
    let lower_entry =
      get t lower_entry_location
      |> Result.of_option
           ~error:(Db_error.Malformed_database "Could not find lower entry")
      |> Db_error.ok_exn
    in
    let new_entry =
      { Entry.value = tid; value_next = lower_entry.value_next }
    in
    match get_or_create_account t (Account_id.with_empty_key tid) new_entry with
    | Ok (`Existed, new_location) ->
        ( `Existed
        , ( `X lower_entry.value
          , `X_path (merkle_path t lower_entry_location)
          , `Y new_entry.value
          , `Y_path (merkle_path t new_location)
          , `Z new_entry.value_next ) )
    | Ok (`Added, new_location) ->
        let lower_entry = { lower_entry with value_next = tid } in
        set t lower_entry_location lower_entry ;
        ( `Added
        , ( `X lower_entry.value
          , `X_path (merkle_path t lower_entry_location)
          , `Y new_entry.value
          , `Y_path (merkle_path t new_location)
          , `Z new_entry.value_next ) )
    | Error e ->
        Error.raise e
end

module Sparse_indexed_merkle_tree = struct
  include Sparse_ledger_lib.Sparse_ledger.Make (Hash) (Account_id) (Entry)

  let of_db_root (db : Db.t) = of_hash ~depth:(Db.depth db) (Db.merkle_root db)

  let of_db_subset_exn_impl ~path_query ~path_add (db : Db.t)
      (tids : Token_id.t list) =
    (*** [iterate_n ~f init n] returns [[f init, f (f init), ..]] of size [n] *)
    let iterate_n ~f =
      let rec impl prev = function
        | 0 ->
            []
        | n ->
            let r = f prev in
            r :: impl r (n - 1)
      in
      impl
    in
    let lower_entries =
      List.map tids ~f:(fun tid -> Db.find_lower_entry_aid_exn db tid)
    in
    let tids = List.concat [ lower_entries; tids ] in
    let locations =
      Db.location_of_account_batch db
        (List.map tids ~f:Account_id.with_empty_key)
    in
    let non_empty_locations = List.filter_map locations ~f:snd in
    let num_new_accounts =
      List.length locations - List.length non_empty_locations
    in
    let entries = Db.get_batch db non_empty_locations in
    let empty_paths, non_empty_paths =
      let next_location_exn loc = Option.value_exn (Db.Location.next loc) in
      let empty_address =
        Db.Addr.of_directions
        @@ List.init (Db.depth db) ~f:(Fn.const Direction.Left)
      in
      let empty_locations =
        if num_new_accounts = 0 then []
        else
          let first_loc =
            Option.value_map ~f:next_location_exn
              ~default:(Db.Location.Account empty_address) (Db.last_filled db)
          in
          first_loc
          :: iterate_n ~f:next_location_exn first_loc (num_new_accounts - 1)
      in
      let paths = path_query db (empty_locations @ non_empty_locations) in
      List.split_n paths num_new_accounts
    in
    let process_location sl key = function
      | Some _, (_, Some entry) :: accs, path :: ne_paths, epaths ->
          (path_add sl path key entry, accs, ne_paths, epaths)
      | None, accs, ne_paths, path :: epaths ->
          ( path_add sl path key Entry.Stable.Latest.empty
          , accs
          , ne_paths
          , epaths )
      | Some _, (_, None) :: _, _, _ ->
          failwith
            "of_ledger_subset_exn: account not found for location returned by \
             location_of_account_batch"
      | _ ->
          failwith "of_ledger_subset_exn: mismatched lengths"
    in
    let sl, _, _, _ =
      List.fold locations
        ~init:(of_db_root db, entries, non_empty_paths, empty_paths)
        ~f:(fun (sl, accs, ne_paths, epaths) (key, mloc) ->
          process_location sl key (mloc, accs, ne_paths, epaths) )
    in
    Debug_assert.debug_assert (fun () ->
        [%test_eq: Ledger_hash.t] (Db.merkle_root db)
          ((merkle_root sl :> Random_oracle.Digest.t) |> Ledger_hash.of_hash) ) ;
    sl

  let of_db_subset_exn =
    of_db_subset_exn_impl ~path_query:Db.wide_merkle_path_batch
      ~path_add:add_wide_path_unsafe

  let find_lower_entry t tid =
    let result = ref None in
    iteri t ~f:(fun i entry ->
        if
          Token_id.equal entry.value_next tid
          && not
               (Hash.equal (Entry.data_hash entry)
                  (Entry.data_hash Entry.Stable.Latest.empty) )
        then result := Some (i, entry) ) ;
    !result

  let get_or_create_entry_exn t tid =
    let lower_entry_location, lower_entry =
      find_lower_entry t tid |> Option.value_exn
    in
    let lower_entry = { lower_entry with value_next = tid } in
    let t = set_exn t lower_entry_location lower_entry in
    let new_entry_location = find_index_exn t (Account_id.with_empty_key tid) in
    let new_entry =
      { Entry.value = tid; value_next = lower_entry.value_next }
    in
    ( set_exn t new_entry_location new_entry
    , ( `X lower_entry.value
      , `X_path (path_exn t lower_entry_location)
      , `Y new_entry.value
      , `Y_path (path_exn t new_entry_location)
      , `Z new_entry.value_next ) )
end
