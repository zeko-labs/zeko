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
           let kvdb = Kvdb.of_dir db_dir in
           let indices = Executor.Commits_store.get_index kvdb in
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
           let kvdb = Kvdb.of_dir db_dir in
           match
             Executor.Commits_store.get_commit kvdb
               ~source:(Frozen_ledger_hash.of_decimal_string source)
               ~target:(Frozen_ledger_hash.of_decimal_string target)
           with
           | Some commit ->
               print_endline
                 (Yojson.Safe.pretty_to_string @@ Zkapp_command.to_yojson commit)
           | None ->
               printf "No commit found\n%!" ) )
  in
  let send =
    ( "send"
    , Command.basic ~summary:"Send a transaction to the sequencer"
        (let%map_open.Command db_dir =
           flag "--db-dir"
             (optional_with_default "db" string)
             ~doc:"string Directory to store the database"
         and l1_uri = flag "--l1-uri" (required string) ~doc:"string L1 URI"
         and fee_arg =
           flag "--fee" (required int) ~doc:"int The fee amount in nanomina"
         and nonce_opt =
           flag "--nonce" (optional int)
             ~doc:"int Will be fetched from the network if not provided"
         and source =
           flag "--source" (required string)
             ~doc:"string The source ledger of the transaction"
         and target =
           flag "--target" (required string)
             ~doc:"string The target ledger of the transaction"
         in
         let signer =
           Signature_lib.(
             Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn
             @@ Sys.getenv_exn "MINA_PRIVATE_KEY")
         in
         let l1_uri : Uri.t Cli_lib.Flag.Types.with_name =
           Cli_lib.Flag.Types.{ value = Uri.of_string l1_uri; name = "l1-uri" }
         in
         let kvdb = Kvdb.of_dir db_dir in
         fun () ->
           Thread_safe.block_on_async_exn (fun () ->
               let command =
                 Option.value_exn ~message:"No commit found"
                 @@ Executor.Commits_store.get_commit kvdb
                      ~source:(Frozen_ledger_hash.of_decimal_string source)
                      ~target:(Frozen_ledger_hash.of_decimal_string target)
               in
               let%bind nonce =
                 match nonce_opt with
                 | Some nonce ->
                     return (Account.Nonce.of_int nonce)
                 | None ->
                     Sequencer_lib.Gql_client.infer_nonce l1_uri
                       (Signature_lib.Public_key.compress signer.public_key)
               in
               let executor = Executor.create ~nonce ~l1_uri ~signer ~kvdb () in
               let command =
                 Zkapp_command.
                   { command with
                     fee_payer =
                       { command.fee_payer with
                         body =
                           { command.fee_payer.body with
                             fee = Currency.Fee.of_nanomina_int_exn fee_arg
                           }
                       }
                   }
               in
               Executor.send_zkapp_command executor command ) ) )
  in
  ( "committer"
  , Command.group
      ~summary:"Script to manually send commiting transactions to L1"
      [ list; get; send ] )

let da_layer =
  let bootstrap_commands =
    ( "bootstrap-commands"
    , Command.basic ~summary:"List commands to be applied when bootstrapping"
        (let%map_open.Command target =
           flag "--target" (required string)
             ~doc:"string The target ledger of the transaction (decimal string)"
         and da_contract_address =
           flag "--da-contract-address" (required string)
             ~doc:"string The address of the DA contract"
         in
         fun () ->
           Thread_safe.block_on_async_exn (fun () ->
               let%bind commands =
                 Da_layer.get_batches
                   Da_layer.{ da_contract_address = Some da_contract_address }
                   ~to_:target
               in
               printf "Found %d commands\n%!" (List.length commands) ;
               return
               @@ List.iter commands ~f:(fun command ->
                      printf "%s\n\n%!"
                        ( Yojson.Safe.pretty_to_string
                        @@ User_command.to_yojson command ) ) ) ) )
  in
  ( "da-layer"
  , Command.group ~summary:"Tool to interact with the DA layer of the sequencer"
      [ bootstrap_commands ] )

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
           let kvdb = Kvdb.of_dir db_dir in
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
           let kvdb = Kvdb.of_dir db_dir in
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
           let kvdb = Kvdb.of_dir db_dir in
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
  Command.group ~summary:"Sequencer CLI" [ committer; da_layer; snark_queue ]
  |> Command_unix.run
