open Core_kernel
open Async
open Relational_db

let migrations : Db.Migration.t list =
  let open Deferred.Result.Let_syntax in
  [ Db.Migration.make 1 "create_merger_schema"
      (fun (module Conn : CONNECTION) ->
        let%bind () =
          Conn.exec
            (Caqti_request.exec Caqti_type.unit
               {sql| CREATE TABLE parallel_merger (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    tree_id TEXT NOT NULL,
                    witness TEXT NOT NULL
                  ) |sql} )
            ()
        in
        Conn.exec
          (Caqti_request.exec Caqti_type.unit
             {sql| CREATE INDEX idx_tree_id ON parallel_merger (tree_id) |sql} )
          () )
  ]

let create ?db_dir ~logger =
  let pool, `Uri _ =
    Relational_db.(
      Db.create_pool
        ?sqlite_path:
          (Option.map db_dir ~f:(fun db_dir ->
               Filename.concat db_dir "state.db" ) )
        ()
      |> caqti_ok_exn ~msg:"Failed to create db pool: %s")
  in
  let%map () =
    Db.Migration.run ~logger pool migrations
    >>| caqti_ok_exn ~msg:"Failed to run migrations: %s"
  in
  pool
