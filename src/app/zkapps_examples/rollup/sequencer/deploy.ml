open Core
open Mina_base
open Async

let constraint_constants = Genesis_constants.Constraint_constants.compiled

module T = Transaction_snark.Make (struct
  let constraint_constants = constraint_constants

  let proof_level = Genesis_constants.Proof_level.Full
end)

module M = Zkapps_rollup.Make (struct
  let tag = T.tag
end)

let run uri init_state sk () =
  let signer =
    Signature_lib.(
      Keypair.of_private_key_exn @@ Private_key.of_base58_check_exn sk)
  in
  let zkapp_keypair = Signature_lib.Keypair.create () in
  printf "zkapp secret key: %s\n%!"
    (Signature_lib.Private_key.to_base58_check zkapp_keypair.private_key) ;
  printf "zkapp public key: %s\n%!"
    Signature_lib.Public_key.(
      Compressed.to_base58_check @@ compress zkapp_keypair.public_key) ;

  let fee_payer_nonce =
    Account.Nonce.of_int
    @@ Thread_safe.block_on_async_exn (fun () ->
           Sequencer_lib.Gql_client.fetch_nonce uri
             (Signature_lib.Public_key.compress signer.public_key) )
  in
  let deploy_update =
    M.Outer.unsafe_deploy (Ledger_hash.of_decimal_string init_state)
  in
  let command =
    Sequencer_lib.Deploy.deploy deploy_update ~fee_payer:signer ~fee_payer_nonce
      ~zkapp_keypair
  in
  Thread_safe.block_on_async_exn (fun () ->
      let%map result = Sequencer_lib.Gql_client.send_zkapp uri command in
      print_endline result )

let () =
  Command_unix.run
  @@ Command.basic ~summary:"Deploy zeko zkapp"
       (let%map_open.Command uri = Cli_lib.Flag.Uri.Client.rest_graphql
        and init_state =
          flag "--initial-state" (required string) ~doc:"string Ledger hash"
        and sk =
          flag "--signer" (required string) ~doc:"string Signer private key"
        in
        run uri init_state sk )
