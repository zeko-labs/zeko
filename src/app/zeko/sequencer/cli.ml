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
           printf "Private key: %s\n"
             (Private_key.to_base58_check keypair.private_key) ;
           printf "Public key: %s\n"
             ( Public_key.compress keypair.public_key
             |> Public_key.Compressed.to_base58_check ) ) ) )

let migrate =
  ( "migrate"
  , Command.async ~summary:"Run migrations on the database"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and db_dir = flag "--db-dir" (required string) ~doc:"string DB directory"
       and target_version =
         flag "--target-version" (optional int) ~doc:"int Target version"
       in
       fun () ->
         let logger = Logger.create () in
         Stdout_log.setup log_json log_level ;
         let pool, `Uri _ =
           Relational_db.Db.create_pool ~sqlite_path:db_dir ()
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

         Ledger.Db.create ~directory_name:ledger_dir
           ~depth:Zeko_constants.constraint_constants.ledger_depth ()
         |> Ledger.Db.iteri ~f:(fun index account ->
                let str =
                  Yojson.Safe.to_string
                    ([%to_yojson: int * Account.t] (index, account))
                in
                Stdio.Out_channel.output_string out str ;
                Stdio.Out_channel.output_string out "," ) ;

         Stdio.Out_channel.output_string out "]" ;
         Stdio.Out_channel.close out ) )

let () =
  Command.group ~summary:"Sequencer CLI"
    [ generate_even_key; migrate; dump_ledger ]
  |> Command_unix.run
