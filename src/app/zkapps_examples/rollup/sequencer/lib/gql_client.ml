open Core_kernel
open Async
open Async_kernel
open Mina_base

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
  let%map result = Init.Graphql_client.query_json_exn q uri in
  print_endline Yojson.Safe.(to_string result) ;
  Yojson.Safe.Util.(result |> member "account" |> member "nonce" |> to_string)
  |> Int.of_string

let fetch_commited_state uri pk =
  let q =
    object
      method query =
        {|
        query ($pk: PublicKey!) {
          account(publicKey: $pk){
            zkappState
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
  let%map result = Init.Graphql_client.query_json_exn q uri in
  Yojson.Safe.Util.(
    result |> member "account" |> member "zkappState" |> index 0 |> to_string)
  |> Frozen_ledger_hash.of_decimal_string

let send_zkapp uri command =
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
  let%map result = Init.Graphql_client.query_json_exn q uri in
  Yojson.Safe.(to_string result)

let fetch_block_height uri =
  let q =
    object
      method query =
        {|
        query {
          bestChain(maxLength: 1) {
            protocolState {
              consensusState {
                blockHeight
              }
            }
          }
        } 
      |}

      method variables = `Assoc []
    end
  in
  let%map result = Init.Graphql_client.query_json_exn q uri in
  Yojson.Safe.Util.(
    result |> member "bestChain" |> index 0 |> member "protocolState"
    |> member "consensusState" |> member "blockHeight" |> to_string)
  |> Int.of_string
