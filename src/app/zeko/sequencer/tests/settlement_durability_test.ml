open Core_kernel
open Async
open Relational_db
module Commit_table = Sequencer_lib.Committer.Commit_table

(* This tests the durable handoff, not proof construction. PostgreSQL treats the
   serialized witness as opaque bytes; using a small fixture keeps this test
   independent of provers, RabbitMQ and signer services. *)
let stored_witness = "fake serialized witness: ledger 10 -> ledger 20"

let store_commit (module Conn : CONNECTION) =
  Conn.exec
    (Caqti_request.exec
       Caqti_type.(tup3 string string octets)
       {sql| INSERT INTO "commit" (source_ledger_hash, target_ledger_hash, witness)
             VALUES (?, ?, ?) ON CONFLICT (source_ledger_hash, target_ledger_hash)
             DO NOTHING |sql} )
    ("10", "20", stored_witness)

let seed_witnesses (module Conn : CONNECTION) =
  Deferred.List.iter
    [ ("tree-a", "a1"); ("tree-a", "a2"); ("tree-b", "b-to-c") ]
    ~how:`Sequential
    ~f:(fun row ->
      Conn.exec
        (Caqti_request.exec
           Caqti_type.(tup2 string string)
           {sql| INSERT INTO parallel_merger (tree_id, witness) VALUES (?, ?) |sql} )
        row
      >>| fun result -> caqti_ok_exn result )

let inspect pool =
  Pool.use
    (fun (module Conn : CONNECTION) ->
      let open Deferred.Result.Let_syntax in
      let%bind witnesses =
        Conn.collect_list
          (Caqti_request.collect Caqti_type.unit
             Caqti_type.(tup2 string string)
             {sql| SELECT tree_id, witness FROM parallel_merger ORDER BY id |sql} )
          ()
      in
      let%map commits =
        Conn.collect_list
          (Caqti_request.collect Caqti_type.unit
             Caqti_type.(tup3 string string octets)
             {sql| SELECT source_ledger_hash, target_ledger_hash, witness FROM "commit" ORDER BY id |sql} )
          ()
      in
      (witnesses, commits) )
    pool
  >>| fun result -> caqti_ok_exn result

let test pool =
  let%bind () =
    Pool.use (fun conn -> seed_witnesses conn >>| Result.return) pool
    >>| fun result -> caqti_ok_exn result
  in
  let transaction ~fail_after_handoff =
    Pool.use
      (with_transaction ~f:(fun conn ->
           let open Deferred.Result.Let_syntax in
           let%bind () =
             Commit_table.handoff ~tree_id:"tree-a" ~store_commit conn
           in
           if not fail_after_handoff then return ()
           else
             let module Conn = (val conn : CONNECTION) in
             (* A real SQL error after both writes simulates transaction failure at
                the handoff boundary. Neither write may become durable. *)
             Conn.exec
               (Caqti_request.exec Caqti_type.unit
                  {sql| INSERT INTO "commit" (source_ledger_hash, target_ledger_hash, witness)
                     VALUES ('20', '20', 'must roll back') |sql} )
               () ) )
      pool
  in
  let%bind failed = transaction ~fail_after_handoff:true in
  assert (Result.is_error failed) ;
  let%bind witnesses, commits = inspect pool in
  assert (
    Poly.equal witnesses
      [ ("tree-a", "a1"); ("tree-a", "a2"); ("tree-b", "b-to-c") ] ) ;
  assert (List.is_empty commits) ;
  let%bind () =
    transaction ~fail_after_handoff:false >>| fun result -> caqti_ok_exn result
  in
  let%bind witnesses, commits = inspect pool in
  assert (Poly.equal witnesses [ ("tree-b", "b-to-c") ]) ;
  assert (Poly.equal commits [ ("10", "20", stored_witness) ]) ;
  let source = Mina_base.Ledger_hash.of_decimal_string "10" in
  let target = Mina_base.Ledger_hash.of_decimal_string "20" in
  let command = "immutable signed command bytes" in
  let hash = "0x" ^ String.make 64 'a' in
  let payload =
    `Assoc
      [ ("commandBase64", `String command)
      ; ( "reservation"
        , `Assoc
            [ ("id", `String "reservation-a"); ("fencingToken", `String "7") ]
        )
      ; ("proof", `Assoc [ ("fixture", `String "preserve exactly") ])
      ]
  in
  let%map () =
    Pool.use
      (fun conn ->
        Commit_table.record_attempt conn ~source ~target ~hash ~command
          ~reservation:payload )
      pool
    >>| fun result -> caqti_ok_exn result
  in
  (source, hash, command)

let () =
  let port =
    Stdlib.Sys.getenv_opt "ZEKO_TEST_POSTGRES_PORT"
    |> Option.value_map ~default:5433 ~f:Int.of_string
  in
  let name = "settlement_durability_test" in
  Thread_safe.block_on_async_exn (fun () ->
      let%bind postgres_uri = For_tests.create_database ~port name in
      let logger = Logger.create () in
      let%bind pool =
        Sequencer_lib.Db.create_and_migrate ~postgres_uri ~logger
      in
      let original_drained = ref false in
      Monitor.protect
        (fun () ->
          let%bind source, hash, command = test pool in
          (* Reopen the connection pool as a restarted process would. *)
          let%bind () = Pool.drain pool in
          original_drained := true ;
          let%bind restored =
            Sequencer_lib.Db.create_and_migrate ~postgres_uri ~logger
          in
          Monitor.protect
            (fun () ->
              let%bind witnesses, commits = inspect restored in
              assert (Poly.equal witnesses [ ("tree-b", "b-to-c") ]) ;
              assert (Poly.equal commits [ ("10", "20", stored_witness) ]) ;
              let%map attempt =
                Pool.use
                  (fun conn -> Commit_table.get_attempt conn source)
                  restored
                >>| fun result -> caqti_ok_exn result
              in
              let stored_hash, stored_command, payload =
                Option.value_exn attempt
              in
              assert (String.equal stored_hash hash) ;
              assert (String.equal stored_command command) ;
              let open Yojson.Safe.Util in
              let payload = Yojson.Safe.from_string payload in
              assert (
                String.equal
                  (payload |> member "commandBase64" |> to_string)
                  command ) ;
              assert (
                String.equal
                  ( payload |> member "reservation" |> member "fencingToken"
                  |> to_string )
                  "7" ) ;
              assert (
                String.equal
                  (payload |> member "proof" |> member "fixture" |> to_string)
                  "preserve exactly" ) )
            ~finally:(fun () -> Pool.drain restored) )
        ~finally:(fun () ->
          let%bind () =
            if !original_drained then Deferred.unit else Pool.drain pool
          in
          For_tests.drop_database ~port name ) )
