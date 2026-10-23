open Async
open Core_kernel
open Relational_db

let migrations =
  [ Db.Migration.make 1 "create_schema" (fun (module Conn : CONNECTION) ->
        Conn.exec
          (Caqti_request.exec Caqti_type.unit
             {sql| CREATE TABLE test (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              name TEXT NOT NULL
            ) |sql} )
          () )
  ; Db.Migration.make 2 "insert_row" (fun (module Conn : CONNECTION) ->
        Conn.exec
          (Caqti_request.exec Caqti_type.unit
             {sql| INSERT INTO test (name) VALUES ('test') |sql} )
          () )
  ; Db.Migration.make 3 "add_column" (fun (module Conn : CONNECTION) ->
        Conn.exec
          (Caqti_request.exec Caqti_type.unit
             {sql| ALTER TABLE test ADD COLUMN age INTEGER NOT NULL DEFAULT 42 |sql} )
          () )
  ]

(* Test that migrations are run and that the database is created *)
let () =
  let postgres_uri =
    Thread_safe.block_on_async_exn (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "test" )
  in
  let open Deferred.Result.Let_syntax in
  let logger = Logger.create () in
  Cli_lib.Stdout_log.setup false Logger.Level.Debug ;
  match
    Thread_safe.block_on_async_exn (fun () ->
        (* Create the database with first migration *)
        let%bind () =
          let%bind pool = Deferred.return (Db.create_pool ~postgres_uri ()) in
          let%bind () =
            Db.Migration.run ~logger ~target_version:(`Version 1) pool
              migrations
          in

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
          let%bind pool = Deferred.return (Db.create_pool ~postgres_uri ()) in
          let%bind () =
            Db.Migration.run ~logger ~target_version:(`Version 2) pool
              migrations
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
          let%bind pool = Deferred.return (Db.create_pool ~postgres_uri ()) in
          let%bind () =
            Db.Migration.run ~logger ~target_version:`Latest pool migrations
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

let () =
  let postgres_uri =
    Thread_safe.block_on_async_exn (fun () ->
        Relational_db.For_tests.create_database ~port:5433
          "zeko_da_state_migration" )
  in
  let logger = Logger.create () in
  let acc_set_openings =
    let tree =
      Indexed_merkle_tree.In_memory.create
        ~depth:Zeko_constants.constraint_constants.ledger_depth ()
    in
    Indexed_merkle_tree.Sparse.of_in_memory_subset ~logger ~db:tree ~keys:[]
  in
  let expected_acc_set =
    Indexed_merkle_tree.Sparse.merkle_root acc_set_openings
    |> Snark_params.Tick.Field.to_string
  in
  let openings_json =
    Indexed_merkle_tree.Sparse.to_yojson acc_set_openings
    |> Yojson.Safe.to_string
  in
  let legacy_diff =
    let empty_state =
      Da_layer.Da_state.empty
        ~depth:Zeko_constants.constraint_constants.ledger_depth
    in
    Da_layer.Diff.create_pending ~source_ledger_hash:empty_state.ledger_hash
      ~changed_accounts:[] ~actions:(`Actions [])
    |> Binable.to_bigstring
         (module Da_layer.Diff.Pending.Stable.V1.With_top_version_tag)
    |> Bigstring.to_string
  in
  let target_ledger_hash =
    Mina_base.Ledger_hash.empty_hash |> Snark_params.Tick.Field.to_string
  in
  let open Deferred.Result.Let_syntax in
  match
    Thread_safe.block_on_async_exn (fun () ->
        let%bind pool = Deferred.return (Db.create_pool ~postgres_uri ()) in
        let%bind () =
          Db.Migration.run ~logger ~target_version:(`Version 5) pool
            Sequencer_lib.Db.migrations
        in
        let%bind () =
          Pool.use
            (fun (module Conn : CONNECTION) ->
              let%bind.Deferred.Result () =
                Conn.exec
                  (Caqti_request.exec
                     Caqti_type.(
                       tup2 string (tup4 (option string) octets octets octets))
                     {sql| INSERT INTO da_diff
                              (target_ledger_hash, source_ledger_hash, diff,
                               ledger_openings, acc_set_openings)
                            VALUES (?, ?, ?, ?, ?) |sql} )
                  ( target_ledger_hash
                  , (None, legacy_diff, "legacy-ledger", openings_json) )
              in
              Conn.exec
                (Caqti_request.exec
                   Caqti_type.(tup3 string string octets)
                   {sql| INSERT INTO da_signature
                            (target_ledger_hash, public_key, signature)
                          VALUES (?, ?, ?) |sql} )
                (target_ledger_hash, "legacy-key", "legacy-signature") )
            pool
        in
        let%bind () =
          Db.Migration.run ~logger ~target_version:`Latest pool
            Sequencer_lib.Db.migrations
        in
        let%bind () =
          Pool.use
            (fun (module Conn : CONNECTION) ->
              let%bind.Deferred.Result ( migrated_diff_acc_set
                                       , migrated_source_acc_set ) =
                Conn.find
                  (Caqti_request.find Caqti_type.unit
                     Caqti_type.(tup2 string string)
                     {sql| SELECT target_acc_set, source_acc_set FROM da_diff |sql} )
                  ()
              in
              let%bind.Deferred.Result migrated_signature_acc_set =
                Conn.find
                  (Caqti_request.find Caqti_type.unit Caqti_type.string
                     {sql| SELECT target_acc_set FROM da_signature |sql} )
                  ()
              in
              assert (String.equal migrated_diff_acc_set expected_acc_set) ;
              assert (String.equal migrated_source_acc_set expected_acc_set) ;
              assert (String.equal migrated_signature_acc_set expected_acc_set) ;
              let%bind.Deferred.Result () =
                Conn.exec
                  (Caqti_request.exec
                     Caqti_type.(
                       tup3 string string (tup4 string octets octets octets))
                     {sql| INSERT INTO da_diff
                              (target_ledger_hash, target_acc_set,
                               source_ledger_hash, source_acc_set, diff,
                               ledger_openings, acc_set_openings)
                            VALUES (?, ?, NULL, ?, ?, ?, ?) |sql} )
                  ( target_ledger_hash
                  , "1"
                  , ( expected_acc_set
                    , "second-diff"
                    , "second-ledger"
                    , openings_json ) )
              in
              let%map.Deferred.Result count =
                Conn.find
                  (Caqti_request.find Caqti_type.unit Caqti_type.int
                     {sql| SELECT COUNT(*) FROM da_diff |sql} )
                  ()
              in
              assert (Int.equal count 2) )
            pool
        in
        return () )
  with
  | Ok () ->
      [%log info] "Composite DA state migration test finished"
  | Error e ->
      failwithf "Composite DA state migration test failed: %s"
        (Caqti_error.show e) ()
