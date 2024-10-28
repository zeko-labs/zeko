open Core
open Async
open Sequencer_lib
open Mina_base

let printf = Core.printf

let print_endline = Core.print_endline

let committer =
  let list =
    ( "commits-list"
    , Command.basic ~summary:"List all of the transactions in the database"
        (let%map_open.Command db_dir =
           flag "--db-dir"
             (optional_with_default "db" string)
             ~doc:"string Directory to store the database"
         in
         fun () ->
           let kvdb = Committer.Store.Kvdb.create db_dir in
           let indices = Committer.Store.get_index kvdb in
           printf "Found %d transactions\n%!" (List.length indices) ;
           List.iter indices ~f:(fun (source, target) ->
               printf "Source: %s\nTarget: %s\n\n%!"
                 (Frozen_ledger_hash.to_decimal_string source)
                 (Frozen_ledger_hash.to_decimal_string target) ) ) )
  in
  let get =
    ( "get"
    , Command.basic ~summary:"Find the command with the given source and target"
        (let%map_open.Command db_dir =
           flag "--db-dir"
             (optional_with_default "db" string)
             ~doc:"string Directory to store the database"
         and source =
           flag "--source" (required string)
             ~doc:"string The source ledger of the transaction"
         and target =
           flag "--target" (required string)
             ~doc:"string The target ledger of the transaction"
         in
         fun () ->
           let kvdb = Committer.Store.Kvdb.create db_dir in
           match
             Committer.Store.get_commit kvdb
               ~source:(Frozen_ledger_hash.of_decimal_string source)
               ~target:(Frozen_ledger_hash.of_decimal_string target)
           with
           | Some commit ->
               print_endline
                 ( Yojson.Safe.pretty_to_string
                 @@ Committer.Commit_witness.to_yojson commit )
           | None ->
               printf "No commit found\n%!" ) )
  in
  ( "committer"
  , Command.group
      ~summary:"Script to manually send commiting transactions to L1"
      [ list; get ] )

let snark_queue =
  let get_state =
    ( "get-state"
    , Command.basic ~summary:"Get state of the snark queue"
        (let%map_open.Command db_dir =
           flag "--db-dir"
             (optional_with_default "db" string)
             ~doc:"string Directory to store the database"
         and pretty =
           flag "--pretty" no_arg ~doc:"string Pretty print the staged commands"
         in
         fun () ->
           let kvdb = Committer.Store.Kvdb.create db_dir in
           let (module T), (module M) =
             Lazy.force Zeko_sequencer.prover_modules
           in
           let module Sequencer = Zeko_sequencer.Make (T) (M) in
           let open Sequencer.Snark_queue in
           match get_state ~kvdb with
           | None ->
               printf "No state in db\n%!"
           | Some state ->
               let to_string : Yojson.Safe.t -> string =
                 if pretty then function
                   | json -> Yojson.Safe.pretty_to_string json
                 else function json -> Yojson.Safe.to_string json
               in
               print_endline @@ to_string @@ State.to_yojson state ) )
  in
  let clear_queued_commands =
    ( "clear-queued-commands"
    , Command.basic ~summary:"Delete queued commands from the snark queue"
        (let%map_open.Command db_dir =
           flag "--db-dir"
             (optional_with_default "db" string)
             ~doc:"string Directory to store the database"
         in
         fun () ->
           let kvdb = Committer.Store.Kvdb.create db_dir in
           let (module T), (module M) =
             Lazy.force Zeko_sequencer.prover_modules
           in
           let module Sequencer = Zeko_sequencer.Make (T) (M) in
           let open Sequencer.Snark_queue in
           match get_state ~kvdb with
           | None ->
               printf "No state in db\n%!"
           | Some state ->
               let new_state = State.clear_queued_commands state in
               persist_state ~kvdb new_state () ) )
  in
  let clear_state =
    ( "clear-state"
    , Command.basic ~summary:"Delete the whole snark queue state"
        (let%map_open.Command db_dir =
           flag "--db-dir"
             (optional_with_default "db" string)
             ~doc:"string Directory to store the database"
         in
         fun () ->
           let db =
             Mina_ledger.Ledger.Db.create ~directory_name:db_dir
               ~depth:Zeko_sequencer.constraint_constants.ledger_depth ()
           in
           let kvdb = Mina_ledger.Ledger.Db.zeko_kvdb db in
           let (module T), (module M) =
             Lazy.force Zeko_sequencer.prover_modules
           in
           let module Sequencer = Zeko_sequencer.Make (T) (M) in
           let open Sequencer.Snark_queue in
           let sparse_ledger =
             Mina_ledger.Sparse_ledger.of_ledger_subset_exn
               Mina_ledger.Ledger.(of_database db)
               [ M.Inner.account_id ]
           in
           let new_state =
             State.(reset_for_new_batch (create ()) sparse_ledger)
           in
           persist_state ~kvdb new_state () ) )
  in
  ( "snark-queue"
  , Command.group
      ~summary:"Script to manually send commiting transactions to L1"
      [ get_state; clear_queued_commands; clear_state ] )

let () =
  Command.group ~summary:"Sequencer CLI" [ committer; snark_queue ]
  |> Command_unix.run
