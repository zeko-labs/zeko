open Core
open Async
open Mina_base
open Sequencer_lib
open Signature_lib
module Sequencer = Zeko_sequencer.Sequencer

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

let generate_even_key =
  ( "generate-even-key"
  , Command.basic ~summary:"Generate a private key with an even public key"
      (Command_unix.Param.return (fun () ->
           let keypair = Zeko_types.Even_PC.generate_even_signer () in
           printf "Private key: %s\n"
             (Private_key.to_base58_check keypair.private_key) ;
           printf "Public key: %s\n"
             ( Public_key.compress keypair.public_key
             |> Public_key.Compressed.to_base58_check ) ) ) )

let () =
  Command.group ~summary:"Sequencer CLI" [ committer; generate_even_key ]
  |> Command_unix.run
