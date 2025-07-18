open Core_kernel
open Async
open Sequencer_lib
open Signature_lib
open Cli_lib
open Mina_base
open Mina_ledger
module Sequencer = Zeko_sequencer.Sequencer

let generate_even_key =
  ( "generate-even-key"
  , Command.basic ~summary:"Generate a private key with an even public key"
      (Command_unix.Param.return (fun () ->
           let keypair = Zeko_types.Even_PC.generate_even_signer () in
           Core.printf "Private key: %s\n"
             (Private_key.to_base58_check keypair.private_key) ;
           Core.printf "Public key: %s\n"
             ( Public_key.compress keypair.public_key
             |> Public_key.Compressed.to_base58_check ) ) ) )

let migrate =
  ( "migrate"
  , Command.async ~summary:"Run migrations on the database"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and postgres_uri =
         flag "--postgres-uri" (required string) ~doc:"string Postgres URI"
       and target_version =
         flag "--target-version" (optional int) ~doc:"int Target version"
       in
       fun () ->
         let logger = Logger.create () in
         Stdout_log.setup log_json log_level ;
         let postgres_uri = Uri.of_string postgres_uri in
         let pool =
           Relational_db.Db.create_pool ~postgres_uri ()
           |> Relational_db.caqti_ok_exn ~msg:"Failed to create db pool: %s"
         in
         Relational_db.Db.Migration.run ~logger
           ~target_version:
             (match target_version with None -> `Latest | Some v -> `Version v)
           pool Db.migrations
         >>| Relational_db.caqti_ok_exn ~msg:"Failed to run migrations: %s" ) )

let dump_ledger =
  ( "dump-ledger"
  , Command.basic ~summary:"Dump the ledger"
      (let%map_open.Command target =
         flag "--target" (required string) ~doc:"string Target file json"
       and ledger_dir =
         flag "--ledger-dir" (required string) ~doc:"string Ledger directory"
       in
       fun () ->
         let out = Stdio.Out_channel.create target in
         Stdio.Out_channel.output_string out "[" ;

         let db =
           Ledger.Db.create ~directory_name:ledger_dir
             ~depth:Zeko_constants.constraint_constants.ledger_depth ()
         in
         Ledger.Db.iteri db ~f:(fun index account ->
             let str =
               Yojson.Safe.to_string
                 ([%to_yojson: int * Account.t] (index, account))
             in
             Stdio.Out_channel.output_string out str ;
             if index < Ledger.Db.num_accounts db - 1 then
               Stdio.Out_channel.output_string out "," ) ;

         Stdio.Out_channel.output_string out "]" ;
         Stdio.Out_channel.close out ) )

let prover_load =
  ( "prover-load"
  , Command.async ~summary:"Dump the ledger"
      (let%map_open.Command prover =
         flag "--prover" (required string) ~doc:"string Prover server"
       and timeout =
         flag "--timeout" (optional float) ~doc:"float Timeout in seconds"
       and count =
         flag "--count"
           (optional_with_default 1 int)
           ~doc:"int Count of transactions"
       in
       fun () ->
         let logger = Logger.create () in
         let client =
           Zeko_prover.Client.create ~logger
             [ Tcp.Where_to_connect.of_host_and_port
               @@ Core.Host_and_port.of_string prover
             ]
         in
         let open Zeko_types in
         let witness : Base_input.serializable =
           let open Test_spec in
           let Test_spec.{ init_ledger; specs } =
             Quickcheck.random_value @@ Test_spec.mk_gen ~num_transactions:5 ()
           in
           let ledger =
             Ledger.with_ledger
               ~depth:Zeko_constants.constraint_constants.ledger_depth
               ~f:(fun ledger ->
                 List.iter (Array.to_list init_ledger)
                   ~f:(fun (keypair, balance) ->
                     let pk =
                       Signature_lib.Public_key.compress keypair.public_key
                     in
                     let account_id = Account_id.create pk Token_id.default in
                     let balance = Unsigned.UInt64.of_int64 balance in
                     let account =
                       Account.create account_id
                         (Currency.Balance.of_uint64 balance)
                     in
                     Ledger.create_new_account_exn ledger account_id account ) ;
                 Ledger.commit ledger ;
                 ledger )
           in
           let imt =
             let db =
               Indexed_merkle_tree.Db.create
                 ~depth:Zeko_constants.constraint_constants.ledger_depth ()
             in
             let tids =
               List.map (Array.to_list init_ledger) ~f:(fun (keypair, _) ->
                   let pk =
                     Signature_lib.Public_key.compress keypair.public_key
                   in
                   let owner = Account_id.create pk Token_id.default in
                   Account_id.derive_token_id ~owner )
             in
             List.iter tids ~f:(fun tid ->
                 let _, _ =
                   Indexed_merkle_tree.Db.get_or_create_entry_exn db tid
                 in
                 () ) ;
             db
           in
           let command =
             command_send ~chain:Mina_signature_kind.Testnet (List.hd_exn specs)
           in
           let source_acc_set =
             Account_set.of_fields [| Indexed_merkle_tree.Db.merkle_root imt |]
           in
           let update_acc_set_witness =
             let fee_payer = Signed_command.fee_payer command in
             let receiver = Signed_command.receiver command in
             let tids =
               List.map [ fee_payer; receiver; fee_payer ] ~f:(fun owner ->
                   Account_id.derive_token_id ~owner )
             in
             List.fold tids ~init:Acc_set_witness.empty ~f:(fun acc tid ->
                 let _, witness =
                   Indexed_merkle_tree.Db.get_or_create_entry_exn imt tid
                 in
                 Acc_set_witness.add acc witness )
           in
           { source_ledger = Ledger.merkle_root ledger
           ; source_acc_set
           ; sequencer =
               Even_PC.create_exn
               @@ Public_key.compress
                    (Even_PC.generate_even_signer ()).public_key
           ; transaction = Command command
           ; witness =
               { ledger_path_handler =
                   (let aids = Signed_command.accounts_referenced command in
                    let sparse_ledger =
                      Sparse_ledger.of_ledger_subset_exn ledger aids
                    in
                    sparse_ledger )
               ; update_acc_set_witness
               }
           }
         in
         let witnesses = List.init count ~f:(fun _ -> witness) in
         let%map () =
           Deferred.List.iteri ~how:`Sequential witnesses ~f:(fun i witness ->
               printf "Proving %d/%d\n" i count ;
               let%map _result =
                 Zeko_prover.Client.transaction_snark ?proving_timeout:timeout
                   client (Signed_command witness)
               in
               () )
         in
         print_endline "Done ✅" ) )

let () =
  Command.group ~summary:"Sequencer CLI"
    [ generate_even_key; migrate; dump_ledger; prover_load ]
  |> Command_unix.run
