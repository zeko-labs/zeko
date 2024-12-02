open Core_kernel

type migration = Db.t -> unit

let add_version_tag _db = ()

let migrations : migration list = [ add_version_tag ]

let latest_migration = List.length migrations

let run_migrations db =
  let old_migration = Db.get_migration db in
  List.drop migrations old_migration
  |> List.iteri ~f:(fun i migration ->
         migration db ;
         Db.set_migration db ~migration:(old_migration + i + 1) )
