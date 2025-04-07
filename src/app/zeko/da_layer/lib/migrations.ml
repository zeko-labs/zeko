open Core_kernel

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

let migrations : migration list = [ add_version_tag ]

let latest_migration = List.length migrations

let run_migrations ~logger db =
  let old_migration = Db.get_migration db in
  List.drop migrations old_migration
  |> List.iteri ~f:(fun i migration ->
         [%log info] "Running migration %d" (old_migration + i + 1) ;
         migration db ;
         Db.set_migration db ~migration:(old_migration + i + 1) )
