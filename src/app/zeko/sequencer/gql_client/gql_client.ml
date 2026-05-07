open Core_kernel
open Async_kernel
open Mina_base
open Init
module Field = Snark_params.Tick.Field

let query_with_retry ~logger ?(max_attempts = 10) ?(delay = Time.Span.of_sec 5.)
    ~label query_obj uri ~f =
  Utils.retry ~logger ~max_attempts ~delay
    ~f:(fun () ->
      Graphql_client.Client.query_json query_obj uri
      >>| Result.map_error ~f:(function
            | `Failed_request e ->
                Error.createf !"Failed to %s: Failed_request %s" label e
            | `Graphql_error e ->
                Error.createf !"Failed to %s: Graphql_error %s" label e )
      >>| Or_error.map ~f )
    ()

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
  query_with_retry ~label:"fetch nonce" q uri ~f:(fun result ->
      Yojson.Safe.Util.(
        result |> member "account" |> member "nonce" |> to_string)
      |> Int.of_string |> Unsigned.UInt32.of_int )

let fetch_action_state uri pk =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            query ($pk: PublicKey!) {
              account(publicKey: $pk){
                actionState
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
  query_with_retry ~label:"fetch action state" q uri ~f:(fun result ->
      Yojson.Safe.Util.(
        result |> member "account" |> member "actionState" |> index 0
        |> to_string)
      |> Field.of_string )

let fetch_actions ~logger uri ?from_action_state ?end_action_state pk :
    ( Field.t array list
    * [ `Block_height of int ]
    * [ `Distance_from_max_block_height of int ]
    * [ `Before of Field.t ]
    * [ `After of Field.t ] )
    list
    Or_error.t
    Deferred.t =
  let ok_exn = function
    | Ppx_deriving_yojson_runtime.Result.Ok x ->
        x
    | Error e ->
        failwith e
  in
  let module M = struct
    type action_data = { data : string list } [@@deriving yojson]

    type block_info = { height : int; distanceFromMaxBlockHeight : int }
    [@@deriving yojson]

    type action_state = { actionStateOne : string; actionStateTwo : string }
    [@@deriving yojson]

    type action =
      { actionData : action_data list
      ; blockInfo : block_info
      ; actionState : action_state
      }
    [@@deriving yojson]

    type actions = { actions : action list } [@@deriving yojson]
  end in
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            query ($pk: String!, $fromActionState: String, $endActionState: String) {
              actions(
                input: {
                  address: $pk
                  fromActionState: $fromActionState
                  endActionState: $endActionState
                }
              ) {
                actionState {
                  actionStateOne
                  actionStateTwo
                }
                actionData {
                  data
                }
                blockInfo {
                  height
                  distanceFromMaxBlockHeight
                }
              }
            } 
          |}

      method variables =
        `Assoc
          [ ( "pk"
            , `String Signature_lib.Public_key.Compressed.(to_base58_check pk)
            )
          ; ( "fromActionState"
            , match from_action_state with
              | Some from ->
                  `String (Field.to_string from)
              | None ->
                  `Null )
          ; ( "endActionState"
            , match end_action_state with
              | Some end_ ->
                  `String (Field.to_string end_)
              | None ->
                  `Null )
          ]
    end
  in
  query_with_retry ~logger ~label:"fetch actions" q uri ~f:(fun result ->
      let result = M.actions_of_yojson result |> ok_exn in
      List.map result.actions
        ~f:(fun
             { actionData
             ; blockInfo
             ; actionState =
                 { actionStateOne = _; actionStateTwo = action_state_before }
             }
           ->
          let block_height = blockInfo.height in
          let distance_from_max_block_height =
            blockInfo.distanceFromMaxBlockHeight
          in
          List.fold_map actionData ~init:(Field.of_string action_state_before)
            ~f:(fun action_state_before { data } ->
              let fields =
                [ List.map data ~f:Field.of_string |> List.to_array ]
              in
              let action_state_after =
                Zkapp_account.Actions_impl.(
                  push_hash action_state_before (hash fields))
              in
              ( action_state_after
              , ( fields
                , `Block_height block_height
                , `Distance_from_max_block_height distance_from_max_block_height
                , `Before action_state_before
                , `After action_state_after ) ) )
          |> snd )
      |> ( if
           (* Drop the first action if it's not the initial state *)
           Stdlib.(
             from_action_state = Some Zkapp_account.Actions.empty_state_element
             || from_action_state = None)
         then Fn.id
         else function [] -> [] | _ :: tail -> tail )
      |> List.join )

let fetch_events uri pk =
  let ok_exn = function
    | Ppx_deriving_yojson_runtime.Result.Ok x ->
        x
    | Error e ->
        failwith e
  in
  let module M = struct
    type event_data = { data : string list } [@@deriving yojson]

    type event = { eventData : event_data list } [@@deriving yojson]

    type events = { events : event list } [@@deriving yojson]
  end in
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
          query ($pk: String!) {
            events(input: {address: $pk}) {
              eventData {
                data
              }
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
  query_with_retry ~label:"fetch events" q uri ~f:(fun result ->
      let result = M.events_of_yojson result |> ok_exn in
      List.map result.events ~f:(fun { eventData } ->
          List.map eventData ~f:(fun { data } ->
              List.map data ~f:Field.of_string ) ) )

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
  query_with_retry ~label:"fetch pooled zkapp commands" q uri ~f:(fun result ->
      Yojson.Safe.Util.(
        result
        |> member "pooledZkappCommands"
        |> to_list
        |> List.map ~f:(member "id")
        |> List.map ~f:to_string
        |> List.map ~f:(Fn.compose ok_exn Zkapp_command.of_base64)) )

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
  query_with_retry ~label:"fetch pooled signed commands" q uri ~f:(fun result ->
      Yojson.Safe.Util.(
        result
        |> member "pooledUserCommands"
        |> to_list
        |> List.map ~f:(member "id")
        |> List.map ~f:to_string
        |> List.map ~f:(Fn.compose ok_exn Signed_command.of_base64)) )

(* Infers nonce based on pooled commands *)
let infer_nonce ~logger uri pk =
  let%bind.Deferred.Result pooled_zkapp_commands =
    fetch_pooled_zkapp_commands ~logger uri pk
  and pooled_signed_commands = fetch_pooled_signed_commands ~logger uri pk in
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
  let%map.Deferred.Result committed_nonce = fetch_nonce ~logger uri pk in
  Unsigned.UInt32.max max_pooled_nonce committed_nonce

let fetch_state uri aid =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            query ($pk: PublicKey!, $tokenId: TokenId!) {
              account(publicKey: $pk, token: $tokenId){
                zkappState
              }
            }
          |}

      method variables =
        `Assoc
          [ ( "pk"
            , `String
                ( Account_id.public_key aid
                |> Signature_lib.Public_key.Compressed.to_base58_check ) )
          ; ("tokenId", `String (Account_id.token_id aid |> Token_id.to_string))
          ]
    end
  in
  query_with_retry ~label:"fetch state" q uri ~f:(fun result ->
      Yojson.Safe.Util.(
        result |> member "account" |> member "zkappState" |> to_list
        |> List.map ~f:to_string
        |> List.map ~f:Field.of_string
        |> Zkapp_state.V.of_list_exn) )

let fetch_state_opt uri aid =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            query ($pk: PublicKey!, $tokenId: TokenId!) {
              account(publicKey: $pk, token: $tokenId){
                zkappState
              }
            }
          |}

      method variables =
        `Assoc
          [ ( "pk"
            , `String
                ( Account_id.public_key aid
                |> Signature_lib.Public_key.Compressed.to_base58_check ) )
          ; ("tokenId", `String (Account_id.token_id aid |> Token_id.to_string))
          ]
    end
  in
  query_with_retry ~label:"fetch state" q uri ~f:(fun result ->
      Yojson.Safe.Util.(
        result |> member "account"
        |> to_option (fun json ->
               member "zkappState" json |> to_list |> List.map ~f:to_string
               |> List.map ~f:Field.of_string
               |> Zkapp_state.V.of_list_exn )) )

let fetch_nonce_opt uri aid =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            query ($pk: PublicKey!, $tokenId: TokenId!) {
              account(publicKey: $pk, token: $tokenId){
                nonce
              }
            }
          |}

      method variables =
        `Assoc
          [ ( "pk"
            , `String
                ( Account_id.public_key aid
                |> Signature_lib.Public_key.Compressed.to_base58_check ) )
          ; ("tokenId", `String (Account_id.token_id aid |> Token_id.to_string))
          ]
    end
  in
  query_with_retry ~label:"fetch nonce" q uri ~f:(fun result ->
      Yojson.Safe.Util.(
        result |> member "account"
        |> to_option (fun json ->
               member "nonce" json |> to_string |> Int.of_string
               |> Unsigned.UInt32.of_int )) )

let fetch_vk uri aid =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            query ($pk: PublicKey!, $tokenId: TokenId!) {
              account(publicKey: $pk, token: $tokenId){
                verificationKey {
                  verificationKey
                }
              }
            }
          |}

      method variables =
        `Assoc
          [ ( "pk"
            , `String
                ( Account_id.public_key aid
                |> Signature_lib.Public_key.Compressed.to_base58_check ) )
          ; ("tokenId", `String (Account_id.token_id aid |> Token_id.to_string))
          ]
    end
  in
  query_with_retry ~label:"fetch verification key" q uri ~f:(fun result ->
      Yojson.Safe.Util.(
        result |> member "account" |> member "verificationKey"
        |> member "verificationKey" |> to_string
        |> Side_loaded_verification_key.of_base64 |> Or_error.ok_exn) )

let parse_auth_required = function
  | "None" ->
      Permissions.Auth_required.None
  | "Either" ->
      Either
  | "Proof" ->
      Proof
  | "Signature" ->
      Signature
  | "Impossible" ->
      Impossible
  | s ->
      failwithf "fetch_account: unknown AccountAuthRequired %s" s ()

let parse_permissions json =
  let open Yojson.Safe.Util in
  let auth field = parse_auth_required (member field json |> to_string) in
  let svk =
    let svk = member "setVerificationKey" json in
    ( parse_auth_required (member "auth" svk |> to_string)
    , Mina_numbers.Txn_version.of_string (member "txnVersion" svk |> to_string)
    )
  in
  Permissions.Poly.
    { edit_state = auth "editState"
    ; access = auth "access"
    ; send = auth "send"
    ; receive = auth "receive"
    ; set_delegate = auth "setDelegate"
    ; set_permissions = auth "setPermissions"
    ; set_verification_key = svk
    ; set_zkapp_uri = auth "setZkappUri"
    ; edit_action_state = auth "editActionState"
    ; set_token_symbol = auth "setTokenSymbol"
    ; increment_nonce = auth "incrementNonce"
    ; set_voting_for = auth "setVotingFor"
    ; set_timing = auth "setTiming"
    }

(* GraphQL exposes the timing fields flatly. They are all null on an Untimed
   account; on a Timed account they are all set. We use [cliffTime] as the
   discriminator. *)
let parse_timing json : Account_timing.t =
  let open Yojson.Safe.Util in
  match member "cliffTime" json with
  | `Null ->
      Untimed
  | _ ->
      (* Mina graphql's Balance/Amount scalars serialize via the type's
         [to_string], which for [Currency.*] is [Unsigned.to_string] — raw
         nanomina as a decimal string ("1000000000000"). Parse as such, not as
         a mina-formatted decimal. *)
      let initial_minimum_balance =
        member "initialMinimumBalance" json
        |> to_string |> Currency.Balance.of_string
      in
      let cliff_time =
        member "cliffTime" json
        |> to_string |> Mina_numbers.Global_slot_since_genesis.of_string
      in
      let cliff_amount =
        member "cliffAmount" json |> to_string |> Currency.Amount.of_string
      in
      let vesting_period =
        member "vestingPeriod" json
        |> to_string |> Mina_numbers.Global_slot_span.of_string
      in
      let vesting_increment =
        member "vestingIncrement" json |> to_string |> Currency.Amount.of_string
      in
      Timed
        { initial_minimum_balance
        ; cliff_time
        ; cliff_amount
        ; vesting_period
        ; vesting_increment
        }

let parse_zkapp ~logger json : Zkapp_account.t option =
  let open Yojson.Safe.Util in
  match member "zkappUri" json with
  | `Null ->
      None
  | _ ->
      let app_state =
        member "zkappState" json |> to_list |> List.map ~f:to_string
        |> List.map ~f:Field.of_string
        |> Zkapp_state.V.of_list_exn
      in
      let action_state =
        match
          member "actionState" json |> to_list |> List.map ~f:to_string
          |> List.map ~f:Field.of_string
        with
        | [ a; b; c; d; e ] ->
            Pickles_types.Vector.[ a; b; c; d; e ]
        | _ ->
            failwith "fetch_account: actionState must have 5 elements"
      in
      let verification_key =
        match member "verificationKey" json with
        | `Null ->
            None
        | vk ->
            let data =
              member "verificationKey" vk
              |> to_string |> Side_loaded_verification_key.of_base64
              |> Or_error.ok_exn
            in
            let hash =
              member "hash" vk |> to_string |> Field.of_string
            in
            Some { With_hash.data; hash }
      in
      let proved_state = member "provedState" json |> to_bool in
      let zkapp_uri = member "zkappUri" json |> to_string in
      ignore logger ;
      Some
        { Zkapp_account.default with
          app_state
        ; verification_key
        ; action_state
        ; proved_state
        ; zkapp_uri
        }

let fetch_account ~logger uri (aid : Account_id.t) :
    Account.t option Deferred.Or_error.t =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            query ($pk: PublicKey!, $tokenId: TokenId!) {
              account(publicKey: $pk, token: $tokenId) {
                publicKey
                tokenId
                tokenSymbol
                balance { total }
                nonce
                receiptChainHash
                delegate
                votingFor
                timing {
                  initialMinimumBalance
                  cliffTime
                  cliffAmount
                  vestingPeriod
                  vestingIncrement
                }
                permissions {
                  editState
                  access
                  send
                  receive
                  setDelegate
                  setPermissions
                  setVerificationKey { auth txnVersion }
                  setZkappUri
                  editActionState
                  setTokenSymbol
                  incrementNonce
                  setVotingFor
                  setTiming
                }
                zkappState
                zkappUri
                actionState
                verificationKey { verificationKey hash }
                provedState
              }
            }
          |}

      method variables =
        `Assoc
          [ ( "pk"
            , `String
                ( Account_id.public_key aid
                |> Signature_lib.Public_key.Compressed.to_base58_check ) )
          ; ("tokenId", `String (Account_id.token_id aid |> Token_id.to_string))
          ]
    end
  in
  query_with_retry ~label:"fetch account" ~logger q uri ~f:(fun result ->
      let open Yojson.Safe.Util in
      match member "account" result with
      | `Null ->
          None
      | json ->
          let public_key =
            member "publicKey" json
            |> to_string
            |> Signature_lib.Public_key.Compressed.of_base58_check_exn
          in
          let token_id =
            member "tokenId" json |> to_string |> Token_id.of_string
          in
          let token_symbol =
            member "tokenSymbol" json |> to_string
          in
          let balance =
            (* See note above on Balance/Amount scalar serialization. *)
            member "balance" json |> member "total" |> to_string
            |> Currency.Balance.of_string
          in
          let nonce =
            member "nonce" json |> to_string |> Account.Nonce.of_string
          in
          let receipt_chain_hash =
            member "receiptChainHash" json |> to_string
            |> Receipt.Chain_hash.of_base58_check_exn
          in
          let delegate =
            match member "delegate" json with
            | `Null ->
                None
            | d ->
                Some
                  ( to_string d
                  |> Signature_lib.Public_key.Compressed.of_base58_check_exn )
          in
          let voting_for =
            (* Mina graphql declares both [receiptChainHash] and [votingFor]
               as the [chain_hash] scalar (see [mina_graphql/types.ml]). The
               encoding uses [Receipt.Chain_hash]'s version byte (0x0C), even
               though [Account.t.voting_for] is a [State_hash.t]. Both share
               the same underlying [Field.t], so we round-trip through it. *)
            member "votingFor" json |> to_string
            |> Receipt.Chain_hash.of_base58_check_exn
            |> Receipt.Chain_hash.to_field |> State_hash.of_hash
          in
          let timing = parse_timing (member "timing" json) in
          let permissions = parse_permissions (member "permissions" json) in
          let zkapp = parse_zkapp ~logger json in
          Some
            (Account.of_poly
               { public_key
               ; token_id
               ; token_symbol
               ; balance
               ; nonce
               ; receipt_chain_hash
               ; delegate
               ; voting_for
               ; timing
               ; permissions
               ; zkapp
               } ) )

let infer_state ~logger uri ~zkapp_pk ~signer_pk =
  let%map.Deferred.Result committed_state =
    fetch_state ~logger uri
      ( Account_id.of_public_key
      @@ Signature_lib.Public_key.decompress_exn zkapp_pk )
  and pooled_zkapp_commands =
    fetch_pooled_zkapp_commands ~logger uri signer_pk
  in
  let pooled_zkapp_commands =
    List.sort pooled_zkapp_commands ~compare:(fun a b ->
        Zkapp_command.(
          Account.Nonce.compare (applicable_at_nonce a) (applicable_at_nonce b)) )
  in
  List.fold_until pooled_zkapp_commands ~init:committed_state
    ~f:(fun acc command ->
      match Utils.update_state zkapp_pk command acc with
      | `Precondition_failed ->
          Stop acc
      | `Skipped ->
          Continue acc
      | `Updated new_state ->
          Continue new_state )
    ~finish:Fn.id

let send_zkapp (uri : Uri.t) command =
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
  query_with_retry ~label:"fetch block height" q uri ~f:(fun result ->
      Yojson.Safe.Util.(
        result |> member "bestChain" |> index 0 |> member "protocolState"
        |> member "consensusState" |> member "blockHeight" |> to_string)
      |> Int.of_string )

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
  query_with_retry ~label:"fetch best chain" q uri ~f:(fun result ->
      Yojson.Safe.Util.(
        result |> member "bestChain"
        |> map (member "stateHash")
        |> to_list |> List.map ~f:to_string) )

let fetch_account_creation_fee uri =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            query {
              genesisConstants {
                accountCreationFee
              }
            }
          |}

      method variables = `Assoc []
    end
  in
  query_with_retry ~label:"fetch account creation fee" q uri ~f:(fun result ->
      Yojson.Safe.Util.(
        result |> member "genesisConstants" |> member "accountCreationFee"
        |> to_string |> Currency.Fee.of_string) )

let fetch_genesis_timestamp uri =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            query {
              genesisConstants {
                genesisTimestamp
              }
            } 
          |}

      method variables = `Assoc []
    end
  in
  query_with_retry ~label:"fetch genesis timestamp" q uri ~f:(fun result ->
      Yojson.Safe.Util.(
        result |> member "genesisConstants" |> member "genesisTimestamp"
        |> to_string |> Genesis_constants.genesis_timestamp_of_string) )

let fetch_fork_slot uri =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            query RuntimeConfig {
              runtimeConfig
            }
          |}

      method variables = `Assoc []
    end
  in
  query_with_retry ~label:"fetch fork slot" q uri ~f:(fun result ->
      Yojson.Safe.Util.(
        result |> member "runtimeConfig" |> member "proof" |> member "fork"
        |> member "global_slot_since_genesis"
        |> to_int)
      |> Mina_numbers.Global_slot_since_genesis.of_int )

module For_tests = struct
  let create_account ~logger uri pk =
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
    let%map result =
      query_with_retry ~logger ~max_attempts:1 ~label:"create account" q uri
        ~f:Fn.id
      >>| Or_error.ok_exn
    in
    Yojson.Safe.(to_string result)

  let create_new_block ~logger uri =
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
    let%map result =
      query_with_retry ~logger ~max_attempts:1 ~label:"create new block" q uri
        ~f:Fn.id
      >>| Or_error.ok_exn
    in
    Yojson.Safe.(to_string result)

  let clear_pool ~logger uri =
    let q =
      object
        method query =
          String.substr_replace_all ~pattern:"\n" ~with_:" "
            {|
              mutation {
                clearTransactionPool
              } 
            |}

        method variables = `Assoc []
      end
    in
    let%map result =
      query_with_retry ~logger ~max_attempts:1 ~label:"clear pool" q uri
        ~f:Fn.id
      >>| Or_error.ok_exn
    in
    Yojson.Safe.(to_string result)

  let reset_state ~logger uri =
    let q =
      object
        method query =
          String.substr_replace_all ~pattern:"\n" ~with_:" "
            {|
              mutation {
                resetState
              } 
            |}

        method variables = `Assoc []
      end
    in
    let%map result =
      query_with_retry ~logger ~label:"reset state" q uri ~f:Fn.id
      >>| Or_error.ok_exn
    in
    Yojson.Safe.(to_string result)

  let shift_slots ~logger uri slots =
    let q =
      object
        method query =
          String.substr_replace_all ~pattern:"\n" ~with_:" "
            {|
              mutation ($slots: Int!) {
                shiftSlots(slots: $slots)
              } 
            |}

        method variables = `Assoc [ ("slots", `Int slots) ]
      end
    in
    let%map result =
      query_with_retry ~logger ~max_attempts:1 ~label:"shift slots" q uri
        ~f:Fn.id
      >>| Or_error.ok_exn
    in
    Yojson.Safe.(to_string result)

  let get_zkapp_command_status ~logger uri hash =
    let q =
      object
        method query =
          String.substr_replace_all ~pattern:"\n" ~with_:" "
            {|
              query ($hash: String!) {
                zkappCommand(hash: $hash) {
                  failureReason {
                    failures
                  }
                }
              } 
            |}

        method variables =
          `Assoc
            [ ( "hash"
              , `String (Mina_transaction.Transaction_hash.to_base58_check hash)
              )
            ]
      end
    in
    let%map result =
      query_with_retry ~logger ~max_attempts:1 ~label:"get zkapp command status"
        q uri ~f:Fn.id
      >>| Or_error.ok_exn
    in
    Yojson.Safe.Util.(
      result |> member "zkappCommand" |> member "failureReason"
      |> to_option (fun json ->
             to_list json
             |> List.map ~f:(member "failures")
             |> List.map ~f:to_list
             |> List.map ~f:(List.map ~f:to_string) ))
end
