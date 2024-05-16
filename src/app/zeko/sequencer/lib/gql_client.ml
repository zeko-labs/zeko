open Core_kernel
open Async_kernel
open Mina_base
open Init

let fetch_nonce uri pk =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
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
    end
  in
  let%map result = Graphql_client.query_json_exn q uri in
  Yojson.Safe.Util.(result |> member "account" |> member "nonce" |> to_string)
  |> Int.of_string |> Unsigned.UInt32.of_int

let fetch_pooled_zkapp_commands uri pk =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            query ($pk: PublicKey!) {
              pooledZkappCommands(publicKey: $pk){
                id
              }
            } 
          |}

      method variables =
        `Assoc
          [ ( "pk"
            , `String Signature_lib.Public_key.Compressed.(to_base58_check pk)
            )
          ]
    end
  in
  let%map result = Graphql_client.query_json_exn q uri in
  Yojson.Safe.Util.(
    result
    |> member "pooledZkappCommands"
    |> to_list
    |> List.map ~f:(member "id")
    |> List.map ~f:to_string
    |> List.map ~f:(Fn.compose ok_exn Zkapp_command.of_base64))

let fetch_pooled_signed_commands uri pk =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            query ($pk: PublicKey!) {
              pooledUserCommands(publicKey: $pk){
                id
              }
            } 
          |}

      method variables =
        `Assoc
          [ ( "pk"
            , `String Signature_lib.Public_key.Compressed.(to_base58_check pk)
            )
          ]
    end
  in
  let%map result = Graphql_client.query_json_exn q uri in
  Yojson.Safe.Util.(
    result
    |> member "pooledUserCommands"
    |> to_list
    |> List.map ~f:(member "id")
    |> List.map ~f:to_string
    |> List.map ~f:(Fn.compose ok_exn Signed_command.of_base64))

(* Infers nonce based on pooled commands *)
let infer_nonce uri pk =
  let%bind pooled_zkapp_commands = fetch_pooled_zkapp_commands uri pk
  and pooled_signed_commands = fetch_pooled_signed_commands uri pk in
  let max_pooled_nonce =
    let max_zkapp_commands_nonce =
      List.map pooled_zkapp_commands ~f:(fun command ->
          Zkapp_command.fee_payer_account_update command
          |> Account_update.Fee_payer.body
          |> Account_update.Body.Fee_payer.nonce
          |> Unsigned.UInt32.(add one) )
      |> List.max_elt ~compare:Unsigned.UInt32.compare
      |> Option.value ~default:Unsigned.UInt32.zero
    in
    let max_signed_commands_nonce =
      List.map pooled_signed_commands ~f:(fun command ->
          Signed_command.nonce command |> Unsigned.UInt32.(add one) )
      |> List.max_elt ~compare:Unsigned.UInt32.compare
      |> Option.value ~default:Unsigned.UInt32.zero
    in
    Unsigned.UInt32.(max max_zkapp_commands_nonce max_signed_commands_nonce)
  in
  let%map committed_nonce = fetch_nonce uri pk in
  Unsigned.UInt32.max max_pooled_nonce committed_nonce

let fetch_committed_state uri pk =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
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
    end
  in
  let%map result = Graphql_client.query_json_exn q uri in
  Yojson.Safe.Util.(
    result |> member "account" |> member "zkappState" |> index 0 |> to_string)
  |> Frozen_ledger_hash.of_decimal_string

let infer_committed_state uri ~zkapp_pk ~signer_pk =
  let%bind committed_state = fetch_committed_state uri zkapp_pk
  and pooled_zkapp_commands = fetch_pooled_zkapp_commands uri signer_pk in
  let pooled_zkapp_commands =
    List.sort pooled_zkapp_commands ~compare:(fun a b ->
        Zkapp_command.(
          Account.Nonce.compare (applicable_at_nonce a) (applicable_at_nonce b)) )
  in
  let pooled_state_transitions =
    List.map pooled_zkapp_commands ~f:(Utils.get_state_transition zkapp_pk)
    |> List.filter_opt
  in
  let future_state =
    List.fold_until pooled_state_transitions ~init:committed_state
      ~f:(fun acc (source, target) ->
        if Frozen_ledger_hash.equal acc source then Continue target
        else Stop acc )
      ~finish:Fn.id
  in
  return future_state

let send_zkapp ({ value = uri; _ } : Uri.t Cli_lib.Flag.Types.with_name) command
    =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            mutation ($input: SendZkappInput!) {
              sendZkapp(input: $input){
                zkapp {
                  id
                  failureReason {
                    index
                    failures
                  }
                }
              }
            } 
          |}

      method variables =
        `Assoc
          [ ( "input"
            , `Assoc
                [ ( "zkappCommand"
                  , Yojson.Safe.to_basic @@ Zkapp_command.to_json command )
                ] )
          ]
    end
  in
  let%bind.Deferred.Result result = Graphql_client.Client.query_json q uri in
  return (Ok Yojson.Safe.(to_string result))

let fetch_block_height uri =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
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
  let%map result = Graphql_client.query_json_exn q uri in
  Yojson.Safe.Util.(
    result |> member "bestChain" |> index 0 |> member "protocolState"
    |> member "consensusState" |> member "blockHeight" |> to_string)
  |> Int.of_string

let fetch_best_chain ?(max_length = 10) uri =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            query ($maxLength: Int!) {
              bestChain(maxLength: $maxLength) {
                stateHash
              }
            } 
          |}

      method variables = `Assoc [ ("maxLength", `Int max_length) ]
    end
  in
  let%map result = Graphql_client.query_json_exn q uri in
  Yojson.Safe.Util.(
    result |> member "bestChain"
    |> map (member "stateHash")
    |> to_list |> List.map ~f:to_string)

module For_tests = struct
  let create_account uri pk =
    let q =
      object
        method query =
          String.substr_replace_all ~pattern:"\n" ~with_:" "
            {|
              mutation ($publicKey: PublicKey!) {
                createAccount(publicKey: $publicKey)
              } 
            |}

        method variables =
          `Assoc
            [ ( "publicKey"
              , `String Signature_lib.Public_key.Compressed.(to_base58_check pk)
              )
            ]
      end
    in
    let%map result = Graphql_client.query_json_exn q uri in
    Yojson.Safe.(to_string result)

  let create_new_block uri =
    let q =
      object
        method query =
          String.substr_replace_all ~pattern:"\n" ~with_:" "
            {|
              mutation {
                createNewBlock
              } 
            |}

        method variables = `Assoc []
      end
    in
    let%map result = Graphql_client.query_json_exn q uri in
    Yojson.Safe.(to_string result)
end
