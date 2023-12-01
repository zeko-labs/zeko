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

let fetch_nonce uri pk =
  let q =
    object
      method query =
        {|
          query ($pk: PublicKey!) {
            account(publicKey: $pk){
              nonce
            }
          } 
        |}

      method variables =
        `Assoc
          [ ( "pk"
            , `String Signature_lib.Public_key.Compressed.(to_base58_check pk)
            )
          ]
        |> Yojson.Safe.to_basic
    end
  in
  Thread_safe.block_on_async_exn (fun () ->
      let%map result = Init.Graphql_client.query_json_exn q uri in
      print_endline Yojson.Safe.(to_string result) ;
      Yojson.Safe.Util.(
        result |> member "account" |> member "nonce" |> to_string)
      |> Int.of_string )

let run uri init_state sk () =
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
    fetch_nonce uri
      (Signature_lib.Public_key.compress sender_keypair.public_key)
  in
  let command =
    M.Mocked_zkapp.Deploy.deploy ~signer:sender_keypair ~zkapp:zkapp_keypair
      ~fee:(Currency.Fee.of_mina_int_exn 1)
      ~nonce:(Account.Nonce.of_int nonce)
      ~vk:M.Mocked.vk
      ~initial_state:(Frozen_ledger_hash.of_decimal_string init_state)
  in
  let q =
    object
      method query =
        {|
          mutation ($input: SendZkappInput!) {
            sendZkapp(input: $input){
              zkapp {
                id
              }
            }
          } 
        |}

      method variables =
        `Assoc
          [ ("input", `Assoc [ ("zkappCommand", Zkapp_command.to_json command) ])
          ]
        |> Yojson.Safe.to_basic
    end
  in
  Thread_safe.block_on_async_exn (fun () ->
      let%map result = Init.Graphql_client.query_json_exn q uri in
      print_endline Yojson.Safe.(to_string result) )

let () =
  Command_unix.run
  @@ Command.basic ~summary:"Deploy zeko zkapp"
       (let%map_open.Command uri = Cli_lib.Flag.Uri.Client.rest_graphql
        and init_state =
          flag "--initial-state" (required string) ~doc:"string Ledger hash"
        and sk =
          flag "-sk" (required string) ~doc:"string Signer private key"
        in
        run uri init_state sk )
