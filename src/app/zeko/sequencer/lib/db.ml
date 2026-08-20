open Async
open Core_kernel
open Relational_db
module Field = Snark_params.Tick.Field

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
  ; Db.Migration.make 6 "key_da_by_ledger_and_account_set"
      (fun (module Conn : CONNECTION) ->
        let exec sql = Conn.exec (Caqti_request.exec Caqti_type.unit sql) () in
        let%bind () =
          exec
            {sql| ALTER TABLE da_diff
                    ADD COLUMN target_acc_set TEXT,
                    ADD COLUMN source_acc_set TEXT |sql}
        in
        let%bind rows =
          Conn.collect_list
            (Caqti_request.collect Caqti_type.unit
               Caqti_type.(tup4 int (option string) octets octets)
               {sql| SELECT id, source_ledger_hash, diff, acc_set_openings FROM da_diff |sql} )
            ()
        in
        let parse_acc_set_root openings =
          let ok_exn = function
            | Ppx_deriving_yojson_runtime.Result.Ok value ->
                value
            | Ppx_deriving_yojson_runtime.Result.Error error ->
                failwithf "Error parsing account-set openings: %s" error ()
          in
          Indexed_merkle_tree.Sparse.of_yojson
            (Yojson.Safe.from_string openings)
          |> ok_exn |> Indexed_merkle_tree.Sparse.merkle_root |> Field.to_string
        in
        let empty_state =
          Da_layer.Da_state.empty
            ~depth:Zeko_constants.constraint_constants.ledger_depth
        in
        let migration_error message =
          Caqti_error.request_failed
            ~uri:(Uri.of_string "zeko://migration")
            ~query:"key_da_by_ledger_and_account_set" (Caqti_error.Msg message)
        in
        let%bind () =
          Deferred.List.fold rows ~init:(Ok ())
            ~f:(fun result (id, source_ledger_hash, diff, openings) ->
              match result with
              | Error _ ->
                  Deferred.return result
              | Ok () -> (
                  let pending_diff =
                    Binable.of_bigstring
                      ( module Da_layer.Diff.Pending.Stable.V1
                               .With_top_version_tag )
                      (Bigstring.of_string diff)
                  in
                  match source_ledger_hash with
                  | None
                    when not
                           (Mina_base.Ledger_hash.equal
                              pending_diff.source_ledger_hash
                              empty_state.ledger_hash ) ->
                      Deferred.return
                        (Error
                           (migration_error
                              (sprintf
                                 "Cannot recover the source account-set root \
                                  for legacy DA queue row %d starting at \
                                  non-genesis ledger %s; rebuild the local DA \
                                  queue from a checkpoint with an explicit \
                                  account-set root"
                                 id
                                 (Mina_base.Ledger_hash.to_decimal_string
                                    pending_diff.source_ledger_hash ) ) ) )
                  | None | Some _ ->
                      Conn.exec
                        (Caqti_request.exec
                           Caqti_type.(tup2 string int)
                           {sql| UPDATE da_diff SET target_acc_set = ? WHERE id = ? |sql} )
                        (parse_acc_set_root openings, id) ) )
        in
        let%bind () =
          exec
            {sql| UPDATE da_diff AS child
                    SET source_acc_set = parent.target_acc_set
                   FROM da_diff AS parent
                  WHERE child.source_ledger_hash = parent.target_ledger_hash |sql}
        in
        let empty_acc_set = Field.to_string empty_state.acc_set in
        let%bind () =
          Conn.exec
            (Caqti_request.exec Caqti_type.string
               {sql| UPDATE da_diff
                       SET source_acc_set = ?
                     WHERE source_acc_set IS NULL |sql} )
            empty_acc_set
        in
        let%bind () =
          exec
            {sql| ALTER TABLE da_signature ADD COLUMN target_acc_set TEXT |sql}
        in
        let%bind () =
          exec
            {sql| UPDATE da_signature AS signature
                    SET target_acc_set = diff.target_acc_set
                   FROM da_diff AS diff
                  WHERE signature.target_ledger_hash = diff.target_ledger_hash |sql}
        in
        let%bind () =
          exec
            {sql| ALTER TABLE da_signature DROP CONSTRAINT da_signature_target_ledger_hash_fkey |sql}
        in
        let%bind () =
          exec
            {sql| ALTER TABLE da_diff DROP CONSTRAINT da_diff_source_ledger_hash_fkey |sql}
        in
        let%bind () =
          exec
            {sql| ALTER TABLE da_signature DROP CONSTRAINT da_signature_target_ledger_hash_public_key_key |sql}
        in
        let%bind () =
          exec
            {sql| ALTER TABLE da_diff DROP CONSTRAINT da_diff_target_ledger_hash_key |sql}
        in
        let%bind () =
          exec
            {sql| ALTER TABLE da_diff
                    ALTER COLUMN target_acc_set SET NOT NULL,
                    ALTER COLUMN source_acc_set SET NOT NULL |sql}
        in
        let%bind () =
          exec
            {sql| ALTER TABLE da_signature ALTER COLUMN target_acc_set SET NOT NULL |sql}
        in
        let%bind () =
          exec
            {sql| ALTER TABLE da_diff
                    ADD CONSTRAINT da_diff_target_state_key
                      UNIQUE (target_ledger_hash, target_acc_set),
                    ADD CONSTRAINT da_diff_source_state_fkey
                      FOREIGN KEY (source_ledger_hash, source_acc_set)
                      REFERENCES da_diff (target_ledger_hash, target_acc_set)
                      ON UPDATE CASCADE
                      ON DELETE RESTRICT |sql}
        in
        let%bind () =
          exec
            {sql| CREATE INDEX idx_da_diff_source_state
                    ON da_diff (source_ledger_hash, source_acc_set) |sql}
        in
        exec
          {sql| ALTER TABLE da_signature
                  ADD CONSTRAINT da_signature_target_state_public_key_key
                    UNIQUE (target_ledger_hash, target_acc_set, public_key),
                  ADD CONSTRAINT da_signature_target_state_fkey
                    FOREIGN KEY (target_ledger_hash, target_acc_set)
                    REFERENCES da_diff (target_ledger_hash, target_acc_set)
                    ON UPDATE CASCADE
                    ON DELETE CASCADE |sql} )
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
