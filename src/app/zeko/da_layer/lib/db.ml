open Core_kernel
open Mina_base
module Field = Snark_params.Tick.Field

module Key_value = struct
  type _ t =
    | Diff : (Da_state.t * Stored_diff.t) t
    | Legacy_diff : (Ledger_hash.t * Diff.Stable.V4.t) t
    | Migration : (unit * int) t

  let serialize_key : type k v. (k * v) t -> k -> Bigstring.t =
   fun pair_type key ->
    match pair_type with
    | Diff ->
        Bigstring.concat
          [ Bigstring.of_string "state-diff:"
          ; Bigstring.of_string @@ Da_state.to_string key
          ]
    | Legacy_diff ->
        Bigstring.concat
          [ Bigstring.of_string "diff"
          ; Bigstring.of_string @@ Ledger_hash.to_decimal_string key
          ]
    | Migration ->
        Bigstring.of_string "migration"

  let serialize_value : type k v. (k * v) t -> v -> Bigstring.t =
   fun pair_type value ->
    match pair_type with
    | Diff ->
        Stored_diff.to_bigstring value
    | Legacy_diff ->
        Diff.to_bigstring value
    | Migration ->
        Bigstring.of_string @@ Int.to_string value

  let deserialize_value : type k v. (k * v) t -> Bigstring.t -> v =
   fun pair_type data ->
    match pair_type with
    | Diff ->
        Stored_diff.of_bigstring data
    | Legacy_diff ->
        Diff.of_bigstring data |> Or_error.ok_exn
    | Migration ->
        Int.of_string @@ Bigstring.to_string data
end

include Kvdb_base.Make (Key_value)

let has_diff t ~state = get t Diff ~key:state |> Option.is_some

let add_diff t ~(diff : Stored_diff.t) =
  match get t Diff ~key:diff.target_state with
  | None ->
      set t Diff ~key:diff.target_state ~data:diff ;
      `Added
  | Some existing when Stored_diff.same_payload existing diff ->
      `Already_existed
  | Some existing ->
      `Conflicting_diff existing

let get_diff t ~state = get t Diff ~key:state

let get_legacy_diff t ~ledger_hash = get t Legacy_diff ~key:ledger_hash

let set_legacy_diff t ~ledger_hash ~diff =
  set t Legacy_diff ~key:ledger_hash ~data:diff

let get_migration t = get t Migration ~key:() |> Option.value ~default:0

let set_migration t ~migration = set t Migration ~key:() ~data:migration

module Async = struct
  let add_diff t ~diff = Async.return (add_diff t ~diff)

  let get_diff t ~state = Async.return (get_diff t ~state)

  let get_migration t = Async.return (get_migration t)

  let set_migration t ~migration = Async.return (set_migration t ~migration)

  let has_diff t ~state = Async.return (has_diff t ~state)
end

let%test_unit "stores distinct account-set roots for the same ledger hash" =
  let db_dir =
    Filename.concat Filename.temp_dir_name
      ("zeko-da-db-test-" ^ (Uuid_unix.create () |> Uuid.to_string))
  in
  let db = create db_dir in
  Exn.protect
    ~finally:(fun () -> close db)
    ~f:(fun () ->
      let make_diff acc_set : Diff.Stable.V4.t =
        { source_ledger_hash = Ledger_hash.empty_hash
        ; changed_accounts = []
        ; actions = `Actions []
        ; timestamp = Block_time.zero
        ; acc_set
        }
      in
      let ledger_hash = Ledger_hash.empty_hash in
      let source_state =
        Da_state.create ~ledger_hash ~acc_set:(Field.of_int 2)
      in
      let make_stored_diff acc_set =
        let target_state = Da_state.create ~ledger_hash ~acc_set in
        { Stored_diff.source_state; target_state; diff = make_diff acc_set }
      in
      let first = add_diff db ~diff:(make_stored_diff Field.zero) in
      let second = add_diff db ~diff:(make_stored_diff Field.one) in
      let conflicting =
        let stored = make_stored_diff Field.zero in
        add_diff db
          ~diff:
            { stored with
              source_state =
                Da_state.create ~ledger_hash ~acc_set:(Field.of_int 3)
            }
      in
      assert (Poly.equal first `Added) ;
      assert (Poly.equal second `Added) ;
      assert (match conflicting with `Conflicting_diff _ -> true | _ -> false) ;
      assert (
        Option.is_some
          (get_diff db
             ~state:(Da_state.create ~ledger_hash ~acc_set:Field.zero) ) ) ;
      assert (
        Option.is_some
          (get_diff db
             ~state:(Da_state.create ~ledger_hash ~acc_set:Field.one) ) ) )
