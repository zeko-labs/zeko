open Core
open Async
open Sequencer_lib
module Graphql_cohttp_async =
  Init.Graphql_internal.Make (Graphql_async.Schema) (Cohttp_async.Io)
    (Cohttp_async.Body)

let run ~port ~zkapp_pk ~max_pool_size ~commitment_period ~da_contract_address
    ~db_dir ~l1_uri ~signer ~test_accounts_path ~network_id () =
  let zkapp_pk =
    Option.(
      value ~default:Signature_lib.Public_key.Compressed.empty
      @@ map ~f:Signature_lib.Public_key.Compressed.of_base58_check_exn zkapp_pk)
  in
  let sequencer =
    Thread_safe.block_on_async_exn (fun () ->
        Zeko_sequencer.create ~zkapp_pk ~max_pool_size ~da_contract_address
          ~db_dir:(Some db_dir) ~l1_uri ~test_accounts_path
          ~commitment_period_sec:commitment_period ~network_id
          ~signer:
            Signature_lib.(
              Keypair.of_private_key_exn
              @@ Private_key.of_base58_check_exn signer) )
  in

  Zeko_sequencer.run_committer sequencer ;

  let graphql_callback =
    Graphql_cohttp_async.make_callback
      (fun ~with_seq_no:_ _req -> sequencer)
      Gql.schema
  in
  let () =
    Cohttp_async.Server.create_expert
      ~on_handler_error:
        (`Call
          (fun _ exn ->
            print_endline "Unhandled exception" ;
            print_endline (Exn.to_string exn) ) )
      (Async.Tcp.Where_to_listen.of_port port)
      (fun ~body _sock req -> graphql_callback () req body)
    |> Deferred.ignore_m |> don't_wait_for
  in
  print_endline ("Sequencer listening on port " ^ Int.to_string port) ;
  never_returns (Async.Scheduler.go ())

let () =
  Command.basic ~summary:"Zeko sequencer"
    (let%map_open.Command port =
       flag "-p" (optional_with_default 8080 int) ~doc:"int Port to listen on"
     and zkapp_pk =
       flag "--zkapp-pk" (optional string) ~doc:"string ZkApp public key"
     and l1_uri = Cli_lib.Flag.Uri.Client.rest_graphql
     and commitment_period =
       flag "--commitment-period"
         (optional_with_default 120. float)
         ~doc:"float Commitment period in seconds"
     and max_pool_size =
       flag "--max-pool-size"
         (optional_with_default 10 int)
         ~doc:"int Maximum transaction pool size"
     and da_contract_address =
       flag "--da-contract-address" (optional string)
         ~doc:"string Address of the DA contract"
     and db_dir =
       flag "--db-dir"
         (optional_with_default "db" string)
         ~doc:"string Directory to store the database"
     and test_accounts_path =
       flag "--test-accounts-path" (optional string)
         ~doc:"string Path to the test genesis accounts file"
     and network_id =
       flag "--network-id"
         (optional_with_default "testnet" string)
         ~doc:"string Network id"
     in
     let signer = Sys.getenv_exn "MINA_PRIVATE_KEY" in

     run ~port ~zkapp_pk ~max_pool_size ~commitment_period ~da_contract_address
       ~db_dir ~l1_uri ~signer ~test_accounts_path ~network_id )
  |> Command_unix.run
