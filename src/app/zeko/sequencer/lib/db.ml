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
  ; Db.Migration.make 3 "committer_schema" (fun (module Conn : CONNECTION) ->
        let%bind () =
          Conn.exec
            (Caqti_request.exec Caqti_type.unit
               {sql| CREATE TABLE "commit" (
                        id SERIAL PRIMARY KEY,
                        source_ledger_hash TEXT NOT NULL,
                        target_ledger_hash TEXT NOT NULL,
                        witness BYTEA NOT NULL,
    
                        UNIQUE (source_ledger_hash, target_ledger_hash),
                        CHECK (source_ledger_hash <> target_ledger_hash)
                      ) |sql} )
            ()
        in
        let%bind () =
          Conn.exec
            (Caqti_request.exec Caqti_type.unit
               {sql| CREATE INDEX idx_commit_source ON "commit" (source_ledger_hash) |sql} )
            ()
        in
        Conn.exec
          (Caqti_request.exec Caqti_type.unit
             {sql| CREATE INDEX idx_commit_target ON "commit" (target_ledger_hash) |sql} )
          () )
  ; Db.Migration.make 4 "ase_cache_schema" (fun (module Conn : CONNECTION) ->
        let%bind () =
          let%bind () =
            Conn.exec
              (Caqti_request.exec Caqti_type.unit
                 {sql| CREATE TABLE ase_cache_with_length (
                    id SERIAL PRIMARY KEY,
                    source_hash TEXT NOT NULL,
                    source_length INTEGER NOT NULL,
                    target_hash TEXT NOT NULL,
                    proof BYTEA NOT NULL,
                    extension_length INTEGER NOT NULL,
                    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
                    
                    UNIQUE (source_hash, target_hash),
                    CHECK (source_hash <> target_hash),
                    CHECK (source_length >= 0),
                    CHECK (extension_length >= 0)
                  ) |sql} )
              ()
          in
          let%bind () =
            Conn.exec
              (Caqti_request.exec Caqti_type.unit
                 {sql| CREATE INDEX idx_ase_cache_with_length_source ON ase_cache_with_length (source_hash) |sql} )
              ()
          in
          Conn.exec
            (Caqti_request.exec Caqti_type.unit
               {sql| CREATE INDEX idx_ase_cache_with_length_created_at ON ase_cache_with_length (created_at) |sql} )
            ()
        in
        let%bind () =
          let%bind () =
            Conn.exec
              (Caqti_request.exec Caqti_type.unit
                 {sql| CREATE TABLE ase_cache_without_length (
                    id SERIAL PRIMARY KEY,
                    source_hash TEXT NOT NULL,
                    target_hash TEXT NOT NULL,
                    proof BYTEA NOT NULL,
                    extension_length INTEGER NOT NULL,
                    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
                    
                    UNIQUE (source_hash, target_hash),
                    CHECK (source_hash <> target_hash),
                    CHECK (extension_length >= 0)
                  ) |sql} )
              ()
          in
          let%bind () =
            Conn.exec
              (Caqti_request.exec Caqti_type.unit
                 {sql| CREATE INDEX idx_ase_cache_without_length_source ON ase_cache_without_length (source_hash) |sql} )
              ()
          in
          Conn.exec
            (Caqti_request.exec Caqti_type.unit
               {sql| CREATE INDEX idx_ase_cache_without_length_created_at ON ase_cache_without_length (created_at) |sql} )
            ()
        in
        return () )
  ; Db.Migration.make 5 "add_acc_set_column_to_da_diff"
      (fun (module Conn : CONNECTION) ->
        Conn.exec
          (Caqti_request.exec Caqti_type.unit
             {sql| ALTER TABLE da_diff ADD COLUMN acc_set_openings BYTEA NOT NULL |sql} )
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
