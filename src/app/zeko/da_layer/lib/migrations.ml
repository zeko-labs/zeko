open Core_kernel
open Mina_base

type migration = Db.t -> unit

let progress_bar ?(width = 30) progress =
  let filled_length = int_of_float (float_of_int width *. progress) in
  let bar =
    String.make filled_length '#' ^ String.make (width - filled_length) '-'
  in
  let progress_style = Sys.getenv_opt "ZEKO_PROGRESS_STYLE" in
  let () =
    match progress_style with
    | Some "percent" ->
        printf "%.0f%%\n%!" (progress *. 100.0)
    | Some "no" ->
        ()
    | Some "bar" | Some _ | None ->
        printf "\r[%s] %.0f%%%!" bar (progress *. 100.0)
  in
  if Float.(progress >= 1.0) then printf "\n%!"

let add_version_tag db =
  let try_read buff =
    try
      let pos_ref = ref 0 in
      Ok (Diff.Stable.V1.bin_read_t ~pos_ref buff)
    with _ -> Error "Failed to convert diff"
  in
  let all = Db.to_alist db in
  let l = List.length all in
  List.iteri all ~f:(fun i (key, value) ->
      progress_bar (Float.of_int i /. Float.of_int l) ;
      match try_read value with
      | Error _ ->
          ( (* The pair is something different from diff *) )
      | Ok diff ->
          let v2 = Diff.to_bigstring @@ Diff.Stable.V1.to_latest diff in
          Db.set_raw db ~key ~data:v2 )

let migrate_composite_states db =
  let legacy_diffs =
    Db.to_alist db
    |> List.filter_map ~f:(fun (key, value) ->
           let key = Bigstring.to_string key in
           match String.chop_prefix key ~prefix:"diff" with
           | Some ledger_hash when not (String.is_empty ledger_hash) ->
               let ledger_hash = Ledger_hash.of_decimal_string ledger_hash in
               let diff = Diff.of_bigstring value |> Or_error.ok_exn in
               Some (ledger_hash, diff)
           | Some _ | None ->
               None )
  in
  let by_ledger_hash = Map.of_alist_exn (module Ledger_hash) legacy_diffs in
  let empty_state =
    Da_state.empty ~depth:Zeko_constants.constraint_constants.ledger_depth
  in
  List.iter legacy_diffs ~f:(fun (target_ledger_hash, diff) ->
      let target_state =
        Da_state.create ~ledger_hash:target_ledger_hash ~acc_set:diff.acc_set
      in
      let source_state =
        if Ledger_hash.equal diff.source_ledger_hash empty_state.ledger_hash
        then empty_state
        else
          match Map.find by_ledger_hash diff.source_ledger_hash with
          | Some source_diff ->
              Da_state.create ~ledger_hash:diff.source_ledger_hash
                ~acc_set:source_diff.acc_set
          | None ->
              failwithf
                "Cannot migrate DA diff %s: source ledger %s is not present"
                (Da_state.to_string target_state)
                (Ledger_hash.to_decimal_string diff.source_ledger_hash)
                ()
      in
      match
        Db.add_diff db ~diff:{ Stored_diff.source_state; target_state; diff }
      with
      | `Added | `Already_existed ->
          ()
      | `Conflicting_diff _ ->
          failwithf "Conflicting migrated DA diff for state %s"
            (Da_state.to_string target_state)
            () )

let migrations : migration list = [ add_version_tag; migrate_composite_states ]

let latest_migration = List.length migrations

let run_migrations ~logger db =
  let old_migration = Db.get_migration db in
  List.drop migrations old_migration
  |> List.iteri ~f:(fun i migration ->
         [%log info] "Running migration %d" (old_migration + i + 1) ;
         migration db ;
         Db.set_migration db ~migration:(old_migration + i + 1) )
