open Core
open Async
open Sequencer_lib
module Graphql_cohttp_async =
  Init.Graphql_internal.Make (Graphql_async.Schema) (Cohttp_async.Io)
    (Cohttp_async.Body)

let run ~port ~zkapp_pk ~max_pool_size ~commitment_period ~da_config ~da_quorum
    ~db_dir ~l1_uri ~archive_uri ~signer ~network_id () =
  let (module T), (module M) = Lazy.force Zeko_sequencer.prover_modules in
  let module Sequencer = Zeko_sequencer.Make (T) (M) in
  let module Gql = Gql.Make (T) (M) (Sequencer) in
  let zkapp_pk =
    Option.(
      value ~default:Signature_lib.Public_key.Compressed.empty
      @@ map ~f:Signature_lib.Public_key.Compressed.of_base58_check_exn zkapp_pk)
  in
  let sequencer =
    Thread_safe.block_on_async_exn (fun () ->
        Sequencer.create ~logger:(Logger.create ()) ~zkapp_pk ~max_pool_size
          ~da_config ~da_quorum ~db_dir:(Some db_dir) ~l1_uri ~archive_uri
          ~commitment_period_sec:commitment_period ~network_id
          ~signer:
            Signature_lib.(
              Keypair.of_private_key_exn
              @@ Private_key.of_base58_check_exn signer) )
  in

  Sequencer.run_committer sequencer ;

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
     and l1_uri = flag "--l1-uri" (required string) ~doc:"string L1 URI"
     and archive_uri =
       flag "--archive-uri" (required string) ~doc:"string archive URI"
     and commitment_period =
       flag "--commitment-period"
         (optional_with_default 120. float)
         ~doc:"float Commitment period in seconds"
     and max_pool_size =
       flag "--max-pool-size"
         (optional_with_default 10 int)
         ~doc:"int Maximum transaction pool size"
     and da_nodes =
       flag "--da-node" (listed string)
         ~doc:"string Address of the DA node, can be supplied multiple times"
     and da_quorum =
       flag "--da-quorum" (required int)
         ~doc:"string Quorum for the DA signature count"
     and db_dir =
       flag "--db-dir"
         (optional_with_default "db" string)
         ~doc:"string Directory to store the database"
     and network_id =
       flag "--network-id"
         (optional_with_default "testnet" string)
         ~doc:"string Network id"
     in
     let signer = Sys.getenv_exn "MINA_PRIVATE_KEY" in
     let da_config = Da_layer.Client.Config.of_string_list da_nodes in
     let l1_uri : Uri.t Cli_lib.Flag.Types.with_name =
       Cli_lib.Flag.Types.{ value = Uri.of_string l1_uri; name = "l1-uri" }
     in
     let archive_uri : Uri.t Cli_lib.Flag.Types.with_name =
       Cli_lib.Flag.Types.
         { value = Uri.of_string archive_uri; name = "archive-uri" }
     in
     run ~port ~zkapp_pk ~max_pool_size ~commitment_period ~da_config ~da_quorum
       ~db_dir ~l1_uri ~archive_uri ~signer ~network_id )
  |> Command_unix.run
