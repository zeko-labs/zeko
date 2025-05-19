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
                    id SERIAL PRIMARY KEY,
                    tree_id TEXT NOT NULL,
                    witness TEXT NOT NULL
                  ) |sql} )
            ()
        in
        Conn.exec
          (Caqti_request.exec Caqti_type.unit
             {sql| CREATE INDEX idx_tree_id ON parallel_merger (tree_id) |sql} )
          () )
  ; Db.Migration.make 2 "da_client_schema" (fun (module Conn : CONNECTION) ->
        let%bind () =
          Conn.exec
            (Caqti_request.exec Caqti_type.unit
               {sql| CREATE TABLE da_diff (
                    id SERIAL PRIMARY KEY,
                    target_ledger_hash TEXT NOT NULL UNIQUE,
                    source_ledger_hash TEXT,
                    diff BYTEA NOT NULL,
                    ledger_openings BYTEA NOT NULL,

                    FOREIGN KEY (source_ledger_hash)
                      REFERENCES da_diff (target_ledger_hash)
                      ON UPDATE CASCADE
                      ON DELETE SET NULL
                  ) |sql} )
            ()
        in
        let%bind () =
          Conn.exec
            (Caqti_request.exec Caqti_type.unit
               {sql| CREATE INDEX idx_da_diff_source_ledger_hash ON da_diff (source_ledger_hash) |sql} )
            ()
        in
        Conn.exec
          (Caqti_request.exec Caqti_type.unit
             {sql| CREATE TABLE da_signature (
                    id SERIAL PRIMARY KEY,
                    target_ledger_hash TEXT NOT NULL,
                    public_key TEXT NOT NULL,
                    signature BYTEA NOT NULL,

                    FOREIGN KEY (target_ledger_hash)
                      REFERENCES da_diff (target_ledger_hash)
                      ON DELETE CASCADE,
                    
                    UNIQUE (target_ledger_hash, public_key)
                ) |sql} )
          () )
  ]

let create_and_migrate ~postgres_uri ~logger =
  let pool =
    Relational_db.(
      Db.create_pool ~postgres_uri ()
      |> caqti_ok_exn ~msg:"Failed to create db pool: %s")
  in
  let%map () =
    Db.Migration.run ~logger ~target_version:`Latest pool migrations
    >>| caqti_ok_exn ~msg:"Failed to run migrations: %s"
  in
  pool
