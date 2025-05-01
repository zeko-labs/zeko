open Async
open Core_kernel
open Relational_db

let migration1 =
  Db.Migration.make 1 "create_schema" (fun (module Conn : CONNECTION) ->
      Conn.exec
        (Caqti_request.exec Caqti_type.unit
           {sql| CREATE TABLE test (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              name TEXT NOT NULL
            ) |sql} )
        () )

let migration2 =
  Db.Migration.make 2 "insert_row" (fun (module Conn : CONNECTION) ->
      Conn.exec
        (Caqti_request.exec Caqti_type.unit
           {sql| INSERT INTO test (name) VALUES ('test') |sql} )
        () )

let migration3 =
  Db.Migration.make 3 "add_column" (fun (module Conn : CONNECTION) ->
      Conn.exec
        (Caqti_request.exec Caqti_type.unit
           {sql| ALTER TABLE test ADD COLUMN age INTEGER NOT NULL DEFAULT 42 |sql} )
        () )

(* Test that migrations are run and that the database is created *)
let () =
  let open Deferred.Result.Let_syntax in
  let logger = Logger.create () in
  Cli_lib.Stdout_log.setup false Logger.Level.Debug ;
  let sqlite_path =
    Filename.concat Cache_dir.autogen_path
      (Uuid.to_string @@ Uuid_unix.create ())
  in
  match
    Thread_safe.block_on_async_exn (fun () ->
        (* Create the database with first migration *)
        let%bind () =
          let%bind pool, `Uri _ =
            Deferred.return (Db.create_pool ~sqlite_path ())
          in
          let%bind () = Db.Migration.run ~logger pool [ migration1 ] in

          (* Test that first migration was run *)
          let%map result =
            Pool.use
              (fun (module Conn : CONNECTION) ->
                Conn.collect_list
                  (Caqti_request.collect Caqti_type.unit
                     Caqti_type.(tup2 int string)
                     {sql| SELECT id, name FROM test |sql} )
                  () )
              pool
          in
          assert (
            List.equal
              (Tuple.T2.equal ~eq1:Int.equal ~eq2:String.equal)
              result [] )
        in

        (* Run the second migration *)
        let%bind () =
          let%bind pool, `Uri _ =
            Deferred.return (Db.create_pool ~sqlite_path ())
          in
          let%bind () =
            Db.Migration.run ~logger pool [ migration1; migration2 ]
          in

          (* Test that second migration was run *)
          let%map result =
            Pool.use
              (fun (module Conn : CONNECTION) ->
                Conn.collect_list
                  (Caqti_request.collect Caqti_type.unit
                     Caqti_type.(tup2 int string)
                     {sql| SELECT id, name FROM test |sql} )
                  () )
              pool
          in
          assert (
            List.equal
              (Tuple.T2.equal ~eq1:Int.equal ~eq2:String.equal)
              result
              [ (1, "test") ] )
        in

        (* Run the third migration *)
        let%bind () =
          let%bind pool, `Uri _ =
            Deferred.return (Db.create_pool ~sqlite_path ())
          in
          let%bind () =
            Db.Migration.run ~logger pool [ migration1; migration2; migration3 ]
          in

          (* Test that third migration was run *)
          let%map result =
            Pool.use
              (fun (module Conn : CONNECTION) ->
                Conn.collect_list
                  (Caqti_request.collect Caqti_type.unit
                     Caqti_type.(tup3 int string int)
                     {sql| SELECT id, name, age FROM test |sql} )
                  () )
              pool
          in
          assert (
            List.equal
              (Tuple.T3.equal ~eq1:Int.equal ~eq2:String.equal ~eq3:Int.equal)
              result
              [ (1, "test", 42) ] )
        in
        return () )
  with
  | Ok () ->
      [%log info] "Finished"
  | Error e ->
      [%log error] "Error: %s" (Caqti_error.show e)
