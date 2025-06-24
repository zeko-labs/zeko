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
    * [ `Y of Token_id.t ]
    * [ `Y_path of Path.t ]
    * [ `Z of Token_id.t ]

  val merkle_root : t -> Hash.t

  val create : ?directory_name:string -> depth:index -> unit -> t

  val create_of_entries_exn :
    ?directory_name:string -> depth:index -> Token_id.t list -> t * witness list

  val get_or_create_entry_exn :
    t -> Token_id.t -> [ `Added | `Existed ] * witness

  val find_lower_entry_tid : t -> Token_id.t -> Token_id.t option

  val close : t -> unit

  val num_entries : t -> int
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
        assert (
          Token_id.(lower_entry.value < new_entry.value)
          && Token_id.(new_entry.value < new_entry.value_next) ) ;
        ( `Existed
        , ( `X lower_entry.value
          , `X_path x_path
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
            assert (
              Token_id.(lower_entry.value < new_entry.value)
              && Token_id.(new_entry.value < new_entry.value_next) ) ;
            ( `Added
            , ( `X lower_entry.value
              , `X_path x_path
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
end
