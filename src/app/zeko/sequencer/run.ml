open Core
open Async
open Sequencer_lib
open Cli_lib
open Zeko_types
open Signature_lib
module Graphql_cohttp_async =
  Init.Graphql_internal.Make (Graphql_async.Schema) (Cohttp_async.Io)
    (Cohttp_async.Body)
module Sequencer = Zeko_sequencer.Sequencer

let run ~logger ~port ~zkapp_pk ~max_pool_size ~commitment_period ~da_config
    ~da_quorum ~db_dir ~postgres_uri ~l1_uri ~archive_uri ~signer ~l1_network_id
    ~l2_network_id ~deposit_delay_blocks ~provers ~da_key ~fee_modifier
    ~minimum_fee () =
  let zkapp_pk =
    Option.(
      value ~default:Signature_lib.Public_key.Compressed.empty
      @@ map ~f:Signature_lib.Public_key.Compressed.of_base58_check_exn zkapp_pk)
  in
  let sequencer =
    Thread_safe.block_on_async_exn (fun () ->
        Sequencer.create ~logger ~zkapp_pk ~max_pool_size ~da_config ~da_quorum
          ~db_dir:(Some db_dir) ~postgres_uri ~l1_uri ~archive_uri
          ~commitment_period_sec:commitment_period ~l1_network_id ~l2_network_id
          ~deposit_delay_blocks
          ~signer:
            Signature_lib.(
              Keypair.of_private_key_exn
              @@ Private_key.of_base58_check_exn signer)
          ~provers ~da_key ~fee_modifier ~minimum_fee )
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
            [%log error] "Unhandled exception: %s" (Exn.to_string exn) ) )
      (Async.Tcp.Where_to_listen.of_port port)
      (fun ~body _sock req -> graphql_callback () req body)
    |> Deferred.ignore_m |> don't_wait_for
  in
  [%log info] "Sequencer listening on port %d" port ;
  never_returns (Async.Scheduler.go ())

let () =
  Command.basic ~summary:"Zeko sequencer"
    (let%map_open.Command log_json = Flag.Log.json
     and log_level = Flag.Log.level
     and port =
       flag "-p" (optional_with_default 8080 int) ~doc:"int Port to listen on"
     and zkapp_pk =
       flag "--zkapp-pk" (optional string) ~doc:"string ZkApp public key"
     and da_key = flag "--da-key" (required string) ~doc:"string DA key"
     and l1_uri = flag "--l1-uri" (required string) ~doc:"string L1 URI"
     and archive_uri =
       flag "--archive-uri" (required string) ~doc:"string archive URI"
     and commitment_period =
       flag "--commitment-period"
         (optional_with_default 120. float)
         ~doc:"float Commitment period in seconds"
     and max_pool_size =
       flag "--max-pool-size"
         (optional_with_default 20 int)
         ~doc:"int Maximum transaction pool size"
     and da_nodes =
       flag "--da-node" (listed string)
         ~doc:"string Address of the DA node, can be supplied multiple times"
     and da_quorum =
       flag "--da-quorum" (required int)
         ~doc:"string Quorum for the DA signature count"
     and provers =
       flag "--prover" (listed string)
         ~doc:
           "string Address of the prover server, can be supplied multiple times"
     and db_dir =
       flag "--db-dir"
         (optional_with_default "db" string)
         ~doc:"string Directory to store the Ledger database"
     and postgres_uri =
       flag "--postgres-uri" (required string) ~doc:"string Postgres URI"
     and l1_network_id =
       flag "--l1-network-id"
         (optional_with_default "testnet" string)
         ~doc:"string Network id"
     and l2_network_id =
       flag "--l2-network-id"
         (optional_with_default "testnet" string)
         ~doc:"string Network id"
     and deposit_delay_blocks =
       flag "--deposit-delay-blocks"
         (optional_with_default 5 int)
         ~doc:"int Number of blocks to wait before processing deposits"
     and fee_modifier =
       flag "--fee-modifier"
         (optional_with_default 1.0 float)
         ~doc:"float Fee modifier for the sequencer"
     and minimum_fee =
       flag "--minimum-fee"
         (optional_with_default 0.01 float)
         ~doc:"float Minimum fee for the sequencer"
     in
     let signer = Sys.getenv_exn "MINA_PRIVATE_KEY" in
     let da_config = Da_layer.Client.Config.of_string_list da_nodes in
     let da_key =
       Even_PC.create_exn (Public_key.Compressed.of_base58_check_exn da_key)
     in
     let l1_uri : Uri.t Cli_lib.Flag.Types.with_name =
       Cli_lib.Flag.Types.{ value = Uri.of_string l1_uri; name = "l1-uri" }
     in
     let archive_uri : Uri.t Cli_lib.Flag.Types.with_name =
       Cli_lib.Flag.Types.
         { value = Uri.of_string archive_uri; name = "archive-uri" }
     in
     let provers = List.map provers ~f:Host_and_port.of_string in
     let logger = Logger.create () in
     let postgres_uri = Uri.of_string postgres_uri in
     Stdout_log.setup log_json log_level ;
     run ~logger ~port ~zkapp_pk ~max_pool_size ~commitment_period ~da_config
       ~da_quorum ~db_dir ~postgres_uri ~l1_uri ~archive_uri ~signer
       ~l1_network_id ~l2_network_id ~deposit_delay_blocks ~provers ~da_key
       ~fee_modifier ~minimum_fee )
  |> Command_unix.run
