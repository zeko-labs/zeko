open Core
open Mina_base
open Async
open Mina_ledger
module L = Ledger

let constraint_constants = Genesis_constants.Constraint_constants.compiled

module T = Transaction_snark.Make (struct
  let constraint_constants = constraint_constants

  let proof_level = Genesis_constants.Proof_level.Full
end)

module M = Zkapps_rollup.Make (T)

let run ~uri ~sk ~test_accounts_path ~da_nodes () =
  let logger = Logger.create () in
  let sender_keypair =
    Signature_lib.(
      Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk)
  in
  let zkapp_keypair = Signature_lib.Keypair.create () in
  printf "zkapp secret key: %s\n%!"
    (Signature_lib.Private_key.to_base58_check zkapp_keypair.private_key) ;
  printf "zkapp public key: %s\n%!"
    Signature_lib.Public_key.(
      Compressed.to_base58_check @@ compress zkapp_keypair.public_key) ;

  let nonce =
    Thread_safe.block_on_async_exn (fun () ->
        Sequencer_lib.Gql_client.infer_nonce uri
          (Signature_lib.Public_key.compress sender_keypair.public_key) )
  in
  let command, ledger =
    L.with_ledger ~depth:constraint_constants.ledger_depth ~f:(fun ledger ->
        L.create_new_account_exn ledger M.Inner.account_id
          M.Inner.initial_account ;

        ( match test_accounts_path with
        | None ->
            ()
        | Some test_accounts_path ->
            List.iter
              (Sequencer_lib.Zeko_sequencer.Test_accounts.parse_accounts_exn
                 ~test_accounts_path ) ~f:(fun (account_id, account) ->
                L.create_new_account_exn ledger account_id account ) ) ;

        ( Sequencer_lib.Deploy.deploy_command_exn ~signer:sender_keypair
            ~zkapp:zkapp_keypair
            ~fee:(Currency.Fee.of_mina_int_exn 1)
            ~nonce ~constraint_constants ~initial_ledger:ledger
            (module M)
        , ledger ) )
  in

  (* Post genesis batch *)
  Thread_safe.block_on_async_exn (fun () ->
      let config = Da_layer.Client.Config.{ nodes = da_nodes } in
      match%bind
        Da_layer.Client.distribute_genesis_batch ~logger ~config ~ledger
      with
      | Ok _ ->
          return ()
      | Error e ->
          Error.raise e ) ;

  (* Deploy contract *)
  Thread_safe.block_on_async_exn (fun () ->
      match%bind Sequencer_lib.Gql_client.send_zkapp uri command with
      | Ok _ ->
          Deferred.unit
      | Error (`Failed_request err) ->
          eprintf "Failed request: %s\n%!" err ;
          Deferred.unit
      | Error (`Graphql_error err) ->
          eprintf "Graphql error: %s\n%!" err ;
          Deferred.unit )

let () =
  Command_unix.run
  @@ Command.basic ~summary:"Deploy zeko zkapp"
       (let%map_open.Command uri = Cli_lib.Flag.Uri.Client.rest_graphql
        and test_accounts_path =
          flag "--test-accounts-path" (optional string)
            ~doc:"string Path to the test genesis accounts file"
        and da_nodes =
          flag "--da-node" (listed string)
            ~doc:"string Address of the DA node, can be supplied multiple times"
        in
        let sk = Sys.getenv_exn "MINA_PRIVATE_KEY" in
        let da_nodes =
          List.mapi da_nodes ~f:(fun i uri ->
              Cli_lib.Flag.Types.
                { value = Host_and_port.of_string uri
                ; name = sprintf "da-node-%d" i
                } )
        in
        run ~uri ~sk ~test_accounts_path ~da_nodes )
