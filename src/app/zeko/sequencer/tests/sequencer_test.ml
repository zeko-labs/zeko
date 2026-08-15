open Core_kernel
open Async
open Mina_base
open Signature_lib
open Sequencer_lib
open Zeko_sequencer
open Zeko_types
open Sequencer
open Test_spec
open Handle.Operator
open Unsigned_extended
module Field = Snark_params.Tick.Field
module Graphql_cohttp_async =
  Init.Graphql_internal.Make (Graphql_async.Schema) (Cohttp_async.Io)
    (Cohttp_async.Body)

let constraint_constants = Zeko_constants.constraint_constants

let start_time = Time.now ()

let logger =
  Cli_lib.Stdout_log.setup false Logger.Level.Spam ;
  Logger.create ()

let gql_uri = Uri.of_string "http://localhost:8080/graphql"

let da_config_with2 =
  Da_layer.Client.Config.of_string_list [ "127.0.0.1:8555"; "127.0.0.1:8556" ]

let da_config_with3 =
  Da_layer.Client.Config.of_string_list
    [ "127.0.0.1:8555"; "127.0.0.1:8556"; "127.0.0.1:8557" ]

let da_keys =
  run (fun () ->
      Da_layer.Client.Config.fetch_public_keys ~logger da_config_with3 )

let da_quorum = 2

let mq_host = Host_and_port.of_string "localhost:5672"

let run = Thread_safe.block_on_async_exn

let get_test_signer () =
  run (fun () ->
      let location =
        Sys.getenv_exn "ZEKO_TEST_SEQUENCER_SIGNER" |> Host_and_port.of_string
      in
      Signer_service.Client.create ~logger ~location
      >>| Signer_service.Signer.of_client )

let free_sequencer (sequencer : Sequencer.t Handle.valid_t) =
  Gc.full_major () ;
  run (fun () -> Sequencer.shutdown !sequencer) ;
  Handle.invalidate sequencer

let slot_acceptance = Time.Span.of_min 60.

let sequential_export_only =
  Option.value_map
    (Stdlib.Sys.getenv_opt "ZEKO_ETHEREUM_SEQUENTIAL_EXPORT_ONLY")
    ~default:false
    ~f:(String.Caseless.equal "true")

let bridge_export_only =
  Option.value_map
    (Stdlib.Sys.getenv_opt "ZEKO_ETHEREUM_BRIDGE_EXPORT_ONLY")
    ~default:false
    ~f:(String.Caseless.equal "true")

let bridge_live_sdk =
  Option.value_map
    (Stdlib.Sys.getenv_opt "ZEKO_ETHEREUM_BRIDGE_LIVE_SDK")
    ~default:false
    ~f:(String.Caseless.equal "true")

type bridge_asset = Native | Ethereum_token

let bridge_asset =
  match Stdlib.Sys.getenv_opt "BRIDGE_ASSET" with
  | None | Some "native" ->
      Native
  | Some "erc20" ->
      Ethereum_token
  | Some asset ->
      failwithf "Unsupported BRIDGE_ASSET: %s" asset ()

let bridge_commit_validity_period =
  Option.value_map
    (Stdlib.Sys.getenv_opt "ZEKO_ETHEREUM_COMMIT_VALIDITY_PERIOD")
    ~default:20 ~f:Int.of_string

let start_graphql_server ~port sequencer ~l1_executor ~l2_executor =
  let graphql_callback =
    Graphql_cohttp_async.make_callback
      (fun ~with_seq_no:_ _req ->
        Gql.Context.{ sequencer; l1_executor; l2_executor } )
      (Gql.schema ~proof_cache_db:sequencer.bridge_prover.proof_cache_db)
  in
  Cohttp_async.Server.create_expert
    ~on_handler_error:
      (`Call
        (fun _ exn ->
          [%log error] "Unhandled bridge SDK GraphQL exception: %s"
            (Exn.to_string exn) ) )
    (Tcp.Where_to_listen.bind_to Tcp.Bind_to_address.Localhost
       (Tcp.Bind_to_port.On_port port) )
    (fun ~body _sock req -> graphql_callback () req body)
  >>| fun _server ->
  printf "Live bridge SDK sequencer listening on port %d\n%!" port

let wait_for_file ~timeout path =
  let deadline = Time.add (Time.now ()) timeout in
  let rec loop () =
    if Stdlib.Sys.file_exists path then return ()
    else if Time.(Time.now () >= deadline) then
      failwithf "Timed out waiting for live bridge SDK marker %s" path ()
    else Clock.after (Time.Span.of_sec 1.) >>= loop
  in
  loop ()

let () =
  if bridge_export_only then (
    print_endline "Started test 'Ethereum bridge settlement export'" ;
    let postgres_uri =
      run (fun () ->
          Relational_db.For_tests.create_database ~port:5433
            "sequencer_ethereum_bridge_export" )
    in
    let open Mina_numbers in
    Quickcheck.test ~trials:1
      (Sequencer_spec.gen ~logger ~number_of_transactions:0 ~postgres_uri
         ~gql_uri ~da_config:da_config_with3 ~da_keys ~da_quorum ~mq_host
         ~slot_acceptance:(Time.Span.of_min 10.)
         ~include_bridge_fee_recipient:true
         ~commit_validity_period:
           (Global_slot_span.of_int bridge_commit_validity_period)
         () )
      ~f:(fun { outer_kp
              ; sequencer
              ; signer_pk
              ; ephemeral_ledger
              ; da_keys
              ; l1_executor
              ; l2_executor
              ; accounts
              ; _
              } ->
        let rec create_even_keypair () =
          let keypair = Keypair.create () in
          if (Public_key.compress keypair.public_key).is_odd then
            create_even_keypair ()
          else keypair
        in
        let recipient =
          match
            Stdlib.Sys.getenv_opt "ZEKO_ETHEREUM_BRIDGE_RECIPIENT_PRIVATE_KEY"
          with
          | None ->
              create_even_keypair ()
          | Some private_key ->
              let keypair =
                Private_key.of_base58_check_exn private_key
                |> Keypair.of_private_key_exn
              in
              if (Public_key.compress keypair.public_key).is_odd then
                failwith
                  "ZEKO_ETHEREUM_BRIDGE_RECIPIENT_PRIVATE_KEY must have an \
                   even public key" ;
              keypair
        in
        let local_bridge_vk_hash =
          Sequencer.get_account !sequencer
            Zeko_circuits_config.Inputs.holder_account_l2 Token_id.default
          |> Option.bind ~f:Account.zkapp
          |> Option.bind ~f:(fun zkapp -> zkapp.verification_key)
          |> Option.map ~f:With_hash.hash
          |> Option.value_exn
               ~message:"L2 bridge holder is missing its verification key"
        in
        let prover_bridge_vk_hash =
          !sequencer.bridge_prover.verification_keys.bridge_mina_l2
        in
        let compiled_bridge_vk_hash =
          run (fun () ->
              Compile_simple.Verification_key.of_tag
                (Lazy.force Bridge_inst_mina.System_L2.tag)
              |> Promise.to_deferred >>| Compile_simple.Verification_key.hash )
        in
        printf "L2 bridge VK hash in genesis: %s\n%!"
          (Field.to_string local_bridge_vk_hash) ;
        printf "L2 bridge VK hash compiled locally: %s\n%!"
          (Field.to_string compiled_bridge_vk_hash) ;
        printf "L2 bridge VK hash from prover: %s\n%!"
          (Field.to_string prover_bridge_vk_hash) ;
        if
          Option.is_some Is_compile_simple_real.is_compile_simple_real
          && ( (not (Field.equal local_bridge_vk_hash compiled_bridge_vk_hash))
             || not (Field.equal local_bridge_vk_hash prover_bridge_vk_hash) )
        then
          failwithf
            "L2 bridge VK mismatch: genesis %s, local compile %s, real prover \
             %s"
            (Field.to_string local_bridge_vk_hash)
            (Field.to_string compiled_bridge_vk_hash)
            (Field.to_string prover_bridge_vk_hash)
            () ;
        if Zeko_circuits_config.Inputs.Ethereum_assets.enabled then (
          let local_registry_vk_hash =
            Sequencer.get_account !sequencer
              Zeko_circuits_config.Inputs.Ethereum_assets.registry_public_key
              Token_id.default
            |> Option.bind ~f:Account.zkapp
            |> Option.bind ~f:(fun zkapp -> zkapp.verification_key)
            |> Option.map ~f:With_hash.hash
            |> Option.value_exn
                 ~message:
                   "Ethereum asset registry is missing its verification key"
          in
          let prover_registry_vk_hash =
            !sequencer.bridge_prover.verification_keys.ethereum_asset_registry
          in
          let compiled_registry_vk_hash =
            run (fun () ->
                Compile_simple.Verification_key.of_tag
                  (Lazy.force Bridge_inst_ethereum_token.Registry.registry_tag)
                |> Promise.to_deferred >>| Compile_simple.Verification_key.hash )
          in
          printf "Ethereum asset registry VK hash in genesis: %s\n%!"
            (Field.to_string local_registry_vk_hash) ;
          printf "Ethereum asset registry VK hash compiled locally: %s\n%!"
            (Field.to_string compiled_registry_vk_hash) ;
          printf "Ethereum asset registry VK hash from prover: %s\n%!"
            (Field.to_string prover_registry_vk_hash) ;
          if
            Option.is_some Is_compile_simple_real.is_compile_simple_real
            && ( (not
                    (Field.equal local_registry_vk_hash
                       compiled_registry_vk_hash ) )
               || not
                    (Field.equal local_registry_vk_hash prover_registry_vk_hash)
               )
          then
            failwithf
              "Ethereum asset registry VK mismatch: genesis %s, local compile \
               %s, real prover %s"
              (Field.to_string local_registry_vk_hash)
              (Field.to_string compiled_registry_vk_hash)
              (Field.to_string prover_registry_vk_hash)
              () ) ;
        let ethereum_token_bridge_verification_key =
          match bridge_asset with
          | Native ->
              None
          | Ethereum_token ->
              if not Zeko_circuits_config.Inputs.Ethereum_assets.enabled then
                failwith
                  "BRIDGE_ASSET=erc20 requires an ethereum_assets circuits \
                   config" ;
              let verification_key =
                run (fun () ->
                    Compile_simple.Verification_key.of_tag
                      (Lazy.force Bridge_inst_ethereum_token.System_L2.tag)
                    |> Promise.to_deferred )
              in
              let expected_hash =
                !sequencer.bridge_prover.verification_keys
                  .bridge_ethereum_token_l2
              in
              let actual_hash =
                Compile_simple.Verification_key.hash verification_key
              in
              if not (Field.equal expected_hash actual_hash) then
                failwithf
                  "Ethereum token bridge VK mismatch: local %s, prover %s"
                  (Field.to_string actual_hash)
                  (Field.to_string expected_hash)
                  () ;
              let pickles_verification_key =
                Compile_simple.Verification_key.to_pickles verification_key
                |> Option.value_exn
                     ~message:
                       "Real ERC20 live integration requires a Pickles \
                        verification key"
              in
              Some
                ( Pickles.Side_loaded.Verification_key.to_base64
                    pickles_verification_key
                , actual_hash )
        in
        let ethereum_holder =
          Option.value_exn
            ~message:
              "ZEKO_ETHEREUM_BRIDGE_ADDRESS must configure the synthetic \
               Ethereum holder"
            Zeko_circuits_config.Inputs.ethereum_holder_account_l1
        in
        let ethereum_withdrawal_recipient =
          Zeko_circuits_config.ethereum_address_to_public_key
            (Option.value
               (Stdlib.Sys.getenv_opt "ZEKO_ETHEREUM_WITHDRAWAL_RECIPIENT")
               ~default:"0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266" )
        in
        let ethereum_asset_records :
            Zeko_circuits.Asset_registry.Asset_record.t list =
          match bridge_asset with
          | Native ->
              []
          | Ethereum_token ->
              List.init 2 ~f:(fun registry_index ->
                  let prefix = sprintf "ERC20_TOKEN_%d_" registry_index in
                  let env suffix = Sys.getenv_exn (prefix ^ suffix) in
                  let asset_id_high, asset_id_low =
                    Zeko_circuits_config.asset_id_limbs_exn (env "ASSET_ID")
                  in
                  let ethereum_token_address =
                    Zeko_circuits_config.ethereum_address_to_public_key
                      (env "ADDRESS")
                  in
                  let token_owner_l2 =
                    Public_key.Compressed.of_base58_check_exn (env "OWNER_L2")
                  in
                  if ethereum_token_address.is_odd || token_owner_l2.is_odd then
                    failwith
                      "Live Ethereum asset addresses and owners must be even"
                  else
                    let owner =
                      Account_id.create token_owner_l2 Token_id.default
                    in
                    ( { schema_version =
                          Zeko_circuits_config.Inputs.Ethereum_assets
                          .registry_schema_version
                      ; registry_index =
                          Zeko_circuits.Zeko_util.Checked32.of_int
                            registry_index
                      ; asset_id_high
                      ; asset_id_low
                      ; ethereum_token_address = ethereum_token_address.x
                      ; token_owner_l2
                      ; token_id_l2 = Account_id.derive_token_id ~owner
                      ; decimals = Zeko_circuits.Zeko_util.Checked32.of_int 9
                      ; inventory_cap =
                          Currency.Amount.of_uint64
                            (Unsigned.UInt64.of_string (env "DEPOSIT_CAP"))
                      ; mft_standard_vk_id =
                          Zeko_circuits_config.Inputs.Ethereum_assets
                          .approved_mft_standard_vk_id
                      ; vault_public_key =
                          Zeko_circuits_config.Inputs.Ethereum_assets
                          .vault_public_key
                      ; universal_bridge_vk_id =
                          Zeko_circuits_config.Inputs.Ethereum_assets
                          .universal_bridge_vk_id
                      }
                      : Zeko_circuits.Asset_registry.Asset_record.t ) )
        in
        let deposit_params : C.Bridge_state.Deposit_params_base.t =
          { children = []
          ; holder_account_l1 = ethereum_holder
          ; recipient = Public_key.compress recipient.public_key
          ; amount = Currency.Amount.of_mina_int_exn 10
          ; timeout = Global_slot_since_genesis.max_value
          }
        in
        let ethereum_token_deposit_params :
            C.Bridge_state.Deposit_params_ethereum_token.t list =
          match bridge_asset with
          | Native ->
              []
          | Ethereum_token ->
              List.map ethereum_asset_records
                ~f:(fun record : C.Bridge_state.Deposit_params_ethereum_token.t
                   ->
                  { asset_id_high = record.asset_id_high
                  ; asset_id_low = record.asset_id_low
                  ; encoding_version = C.Zeko_util.Checked32.of_int 2
                  ; registry_index = record.registry_index
                  ; record_commitment =
                      C.Asset_registry.Asset_record.commitment record
                  ; base = deposit_params
                  } )
        in
        ( match bridge_asset with
        | Native ->
            ()
        | Ethereum_token ->
            let scenario_amount =
              Currency.Amount.to_uint64 deposit_params.amount
            in
            List.iteri ethereum_asset_records ~f:(fun index _record ->
                let configured_amount =
                  UInt64.of_string
                    (Sys.getenv_exn
                       (sprintf "ERC20_TOKEN_%d_DEPOSIT_AMOUNT" index) )
                in
                if not (UInt64.equal configured_amount scenario_amount) then
                  failwithf
                    "ERC20_TOKEN_%d_DEPOSIT_AMOUNT %s does not match scenario \
                     amount %s"
                    index
                    (UInt64.to_string configured_amount)
                    (UInt64.to_string scenario_amount)
                    () ) ) ;
        let deposit_auxes () =
          match ethereum_token_deposit_params with
          | [] ->
              [ Utils.value_to_hash ~init:Zeko_constants.ethereum_deposit_salt
                  C.Bridge_state.Deposit_params_base.typ deposit_params
              ]
          | params ->
              List.map params ~f:(fun params ->
                  Utils.value_to_hash
                    ~init:
                      C.Bridge_state.Deposit_params_ethereum_token.ethereum_salt
                    C.Bridge_state.Deposit_params_ethereum_token.typ params )
        in
        let withdrawal_params : C.Bridge_state.Withdrawal_params_base.t =
          { children = []
          ; recipient = ethereum_withdrawal_recipient
          ; amount = Currency.Amount.of_mina_int_exn 5
          }
        in
        let bridge_scenario_directory () =
          Option.first_some
            (Stdlib.Sys.getenv_opt "ZEKO_ETHEREUM_BRIDGE_SCENARIO_DIR")
            (Stdlib.Sys.getenv_opt "ZEKO_ETHEREUM_SETTLEMENT_FIXTURE_DIR")
        in
        let write_genesis_ledger () =
          match bridge_scenario_directory () with
          | None ->
              ()
          | Some directory ->
              L.to_list_sequential ephemeral_ledger
              |> List.mapi ~f:(fun index account -> (index, account))
              |> [%to_yojson: (int * Account.t) list]
              |> Yojson.Safe.pretty_to_string
              |> fun json ->
              Out_channel.write_all
                (Filename.concat directory "bridge-genesis-ledger.json")
                ~data:(json ^ "\n")
        in
        let write_scenario_manifest ~outer_action_state_before_registration
            ~outer_action_state_before ~outer_action_state_after =
          match bridge_scenario_directory () with
          | None ->
              ()
          | Some directory ->
              let recipient = Public_key.compress recipient.public_key in
              let withdrawal_recipient =
                Ethereum_settlement_export.ethereum_address_of_compressed
                  ethereum_withdrawal_recipient
                |> Option.value_exn
                     ~message:"withdrawal recipient is not an Ethereum address"
              in
              let auxes = deposit_auxes () in
              let aux = List.hd_exn auxes in
              let asset_fields =
                match bridge_asset with
                | Native ->
                    [ ("bridgeAsset", `String "native") ]
                | Ethereum_token ->
                    let assets =
                      List.zip_exn ethereum_asset_records auxes
                      |> List.map ~f:(fun (record, deposit_aux) ->
                             `Assoc
                               [ ( "record"
                                 , Ethereum_settlement_export
                                   .canonical_asset_record_json record )
                               ; ( "tokenOwnerPublicKey"
                                 , `String
                                     (Public_key.Compressed.to_base58_check
                                        record.token_owner_l2 ) )
                               ; ( "tokenIdBase58"
                                 , `String
                                     (Token_id.to_string record.token_id_l2) )
                               ; ( "depositAux"
                                 , `String
                                     (Ethereum_settlement_export.field_to_hex
                                        deposit_aux ) )
                               ] )
                    in
                    [ ("bridgeAsset", `String "erc20")
                    ; ("ethereumAssets", `List assets)
                    ; ( "ethereumAssetRegistryL2"
                      , `String
                          (Public_key.Compressed.to_base58_check
                             Zeko_circuits_config.Inputs.Ethereum_assets
                             .registry_public_key ) )
                    ; ( "ethereumSharedVaultL2"
                      , `String
                          (Public_key.Compressed.to_base58_check
                             Zeko_circuits_config.Inputs.Ethereum_assets
                             .vault_public_key ) )
                    ; ( "ethereumMftStandardVkId"
                      , `String
                          (Field.to_string
                             Zeko_circuits_config.Inputs.Ethereum_assets
                             .approved_mft_standard_vk_id ) )
                    ; ( "ethereumUniversalBridgeVkId"
                      , `String
                          (Field.to_string
                             Zeko_circuits_config.Inputs.Ethereum_assets
                             .universal_bridge_vk_id ) )
                    ]
              in
              let json =
                `Assoc
                  ( [ ("schemaVersion", `Int 3)
                    ; ( "commitValidityPeriod"
                      , `Int bridge_commit_validity_period )
                    ; ( "zekoRecipient"
                      , `String
                          (Ethereum_settlement_export.field_to_hex recipient.x)
                      )
                    ; ( "zekoRecipientPublicKey"
                      , `String
                          (Public_key.Compressed.to_base58_check recipient) )
                    ; ("zekoRecipientIsOdd", `Bool recipient.is_odd)
                    ; ( "depositAmountZeko"
                      , `String
                          ( Currency.Amount.to_uint64 deposit_params.amount
                          |> Unsigned.UInt64.to_string ) )
                    ; ( "depositTimeout"
                      , `String
                          ( Global_slot_since_genesis.to_uint32
                              deposit_params.timeout
                          |> Unsigned.UInt32.to_string ) )
                    ; ( "depositAux"
                      , `String (Ethereum_settlement_export.field_to_hex aux) )
                    ; ( "depositAuxes"
                      , Ethereum_settlement_export.fields_json
                          (Array.of_list auxes) )
                    ; ( "outerActionStateBeforeDeposit"
                      , `String
                          (Ethereum_settlement_export.field_to_hex
                             outer_action_state_before ) )
                    ; ( "outerActionStateBeforeRegistration"
                      , `String
                          (Ethereum_settlement_export.field_to_hex
                             outer_action_state_before_registration ) )
                    ; ( "outerActionStateAfterDeposit"
                      , `String
                          (Ethereum_settlement_export.field_to_hex
                             outer_action_state_after ) )
                    ; ("withdrawalRecipient", `String withdrawal_recipient)
                    ; ( "withdrawalAmountZeko"
                      , `String
                          ( Currency.Amount.to_uint64 withdrawal_params.amount
                          |> Unsigned.UInt64.to_string ) )
                    ; ( "daPublicKeys"
                      , `List
                          (List.map da_keys ~f:(fun public_key ->
                               `String
                                 (Public_key.Compressed.to_base58_check
                                    public_key ) ) ) )
                    ; ( "sequencerPublicKey"
                      , `String
                          (Public_key.Compressed.to_base58_check signer_pk) )
                    ]
                  @ asset_fields )
              in
              Out_channel.write_all
                (Filename.concat directory "bridge-scenario.json")
                ~data:(Yojson.Safe.pretty_to_string json ^ "\n")
        in
        let submit_ethereum_deposit aux =
          let witness : Bridge.Outer_action_witness.serializable =
            { public_key = Zeko_circuits_config.Inputs.zeko_l1
            ; witness =
                { aux
                ; children = []
                ; slot_range = C.Zeko_util.Slot_range.infinite
                }
            }
          in
          let%bind (body, _, calls), proof =
            Zeko_prover.Client.outer_action_witness
              !sequencer.bridge_prover.provers witness
            >>| Or_error.ok_exn
          in
          let account_updates =
            Utils.attach_proof_to_forest
              ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
              ~proof_cache_db:!sequencer.bridge_prover.proof_cache_db ~body
              ~calls ~proof
          in
          let command : Zkapp_command.t =
            { fee_payer =
                Account_update.Fee_payer.make
                  ~body:
                    { public_key =
                        Signer_service.Signer.public_key l1_executor.signer
                    ; fee = !sequencer.bridge_prover.bridge_txn_fee
                    ; valid_until = None
                    ; nonce = Account.Nonce.zero
                    }
                  ~authorization:Signature.dummy
            ; account_updates =
                Zkapp_command.Call_forest.map account_updates
                  ~f:
                    (Account_update.write_all_proofs_to_disk
                       ~proof_cache_db:(Proof_cache_tag.create_identity_db ()) )
            ; memo = Signed_command_memo.empty
            }
          in
          Executor.send_zkapp_command ~logger l1_executor command
          >>| Or_error.ok_exn
        in
        let fund_ethereum_token_operator () =
          match bridge_asset with
          | Native ->
              Deferred.unit
          | Ethereum_token ->
              let source = List.hd_exn accounts in
              let nonce =
                Sequencer.infer_nonce !sequencer
                  (Public_key.compress source.public_key)
              in
              let payment : Transaction_spec.t =
                { fee = Currency.Fee.of_mina_string_exn "0.1"
                ; sender = (source, nonce)
                ; receiver =
                    Public_key.compress recipient.public_key
                    (* Generated test accounts hold exactly 100 MINA. Keep enough
                       headroom for this transfer's fee and the source account's
                       later use while giving the browser operator ample funds for
                       token deployment and account creation. *)
                ; amount = Currency.Amount.of_mina_int_exn 50
                ; actions = None
                }
              in
              apply_user_command !sequencer
                (Signed_command
                   (command_send ~chain:Zeko_circuits_config.Inputs.chain_l2
                      payment ) )
              >>| Or_error.ok_exn
        in
        let require_live_sdk_fee_payer () =
          match bridge_asset with
          | Native ->
              ()
          | Ethereum_token ->
              let (_ : Account.t) =
                Sequencer.get_account !sequencer
                  (Public_key.compress recipient.public_key)
                  Token_id.default
                |> Option.value_exn
                     ~message:
                       "Live SDK fee payer must be funded on L2 before the \
                        readiness manifest is published"
              in
              ()
        in
        let commit_and_check label =
          printf "(* %s *)\n%!" label ;
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash
        in
        let synchronize_accepting_commit () =
          let%map processed_witnesses, _processed_pointer =
            Sequencer.sync_commits_only !sequencer >>| Or_error.ok_exn
          in
          if processed_witnesses <> 0 then
            failwith
              "Synchronizing the accepting commit unexpectedly processed an \
               outer Witness action"
        in
        let finalize_deposit () =
          let%bind actions =
            Gql_client.fetch_actions ~logger gql_uri
              (Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| List.map ~f:(fun (fields, _, _, before, after) ->
                    ( Utils.actions_to_outer_action (List.hd_exn fields)
                    , before
                    , after ) )
          in
          let ( my_deposit_index
              , (_my_deposit, `Before before_my_deposit_action_state, _) ) =
            let hashed_deposit =
              Utils.value_to_hash ~init:Zeko_constants.ethereum_deposit_salt
                C.Bridge_state.Deposit_params_base.typ deposit_params
            in
            List.findi actions ~f:(fun _ (action, _, _) ->
                match action with
                | Commit _ ->
                    false
                | Witness witness ->
                    Field.equal hashed_deposit witness.aux )
            |> Option.value_exn
                 ~message:"Did not find synthetic Ethereum deposit"
          in
          let ( nearest_commit_index
              , (_nearest_commit, _, `After after_nearest_commit_action_state) )
              =
            List.sub actions ~pos:my_deposit_index
              ~len:(List.length actions - my_deposit_index)
            |> List.find_mapi ~f:(fun i (action, before, after) ->
                   match action with
                   | Commit commit ->
                       Some (i, (commit, before, after))
                   | Witness _ ->
                       None )
            |> Option.value_exn ~message:"Did not find accepting commit"
          in
          let nearest_commit_index = nearest_commit_index + my_deposit_index in
          let check_accepted :
              Bridge.Check_accepted_mina.Init.t
              * Bridge.Check_accepted_mina.Elem.t list =
            ( { params = deposit_params
              ; original_action_state =
                  C.Rollup_state.Outer_action_state.unsafe_value_of_field
                    before_my_deposit_action_state
              ; deposit_index = UInt32.of_int my_deposit_index
              }
            , List.sub actions ~pos:my_deposit_index
                ~len:(nearest_commit_index - my_deposit_index + 1)
              |> List.map ~f:(fun (action, _, _) -> action) )
          in
          let current_synced_outer_action_state =
            Sequencer.current_synced_outer_action_state !sequencer
          in
          let%bind ase_actions =
            Gql_client.fetch_actions ~logger gql_uri
              ~from_action_state:after_nearest_commit_action_state
              ~end_action_state:
                C.Rollup_state.Outer_action_state.(
                  With_length.state current_synced_outer_action_state |> raw)
              (Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| List.map ~f:(fun (fields, _, _, _, _) ->
                    Zkapp_account.Actions_impl.hash fields )
          in
          let ase : Ase.With_length.Stmt.t * Field.t list =
            ( { action_state = after_nearest_commit_action_state
              ; length =
                  Zeko_circuits.Zeko_util.(
                    Checked32.sub
                      C.Rollup_state.Outer_action_state.(
                        With_length.length current_synced_outer_action_state)
                      (Checked32.of_int @@ List.length ase_actions)
                    |> Option.value_exn ~message:"Negative ASE length")
              }
            , ase_actions )
          in
          let helper_account =
            Sequencer.get_account !sequencer
              (Public_key.compress recipient.public_key)
              (Account_id.derive_token_id
                 ~owner:
                   (Account_id.of_public_key
                      (Public_key.decompress_exn
                         Zeko_circuits_config.Inputs.holder_account_l2 ) ) )
          in
          let prev_next_deposit =
            Option.value ~default:UInt32.zero
              (let%bind.Option account = helper_account in
               let%map.Option zkapp = Account.zkapp account in
               let (next_deposit :: _ : F.t Zkapp_state.V.t) =
                 Zkapp_account.Poly.app_state zkapp
               in
               UInt32.of_string (Field.to_string next_deposit) )
          in
          let prev_nonce =
            Option.value ~default:UInt32.zero
              (Option.map helper_account ~f:(fun account ->
                   UInt32.of_string (Account_nonce.to_string account.nonce) ) )
          in
          let witness : Bridge_prover.Finalize_deposit.t_ =
            { ase_source = fst ase
            ; ase_elems = snd ase
            ; check_accepted_init = fst check_accepted
            ; check_accepted_elems = snd check_accepted
            ; prev_next_deposit
            ; prev_nonce
            ; helper_account_new = Option.is_none helper_account
            }
          in
          let _forest, `Commitment commitment =
            Bridge_prover.Finalize_deposit.precompute_commitments
              !sequencer.bridge_prover witness
            |> Or_error.ok_exn
          in
          let helper_account_signature =
            Schnorr.Chunked.sign
              ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
              recipient.private_key
              (Random_oracle.Input.Chunked.field commitment)
          in
          Bridge_prover.(
            execute_request ~label:"FinalizeEthereumDeposit" ~logger
              ~executor:l2_executor !sequencer.bridge_prover
              ( Finalize_deposit.f ~t:!sequencer.bridge_prover ~logger witness
                  helper_account_signature
              |> Or_error.ok_exn ))
          >>| Or_error.ok_exn
        in
        let submit_withdrawal () =
          let withdrawal_aux =
            Utils.value_to_hash ~init:Zeko_constants.withdrawal_salt
              C.Bridge_state.Withdrawal_params_base.typ withdrawal_params
          in
          Archive.store_ethereum_withdrawal !sequencer.archive
            ~aux:withdrawal_aux
            { Archive.Ethereum_withdrawal.recipient =
                withdrawal_params.recipient
            ; amount = withdrawal_params.amount
            ; asset = None
            } ;
          let nonce =
            Sequencer.infer_nonce !sequencer
              (Public_key.compress recipient.public_key)
          in
          let expected_amount =
            Currency.Amount.add withdrawal_params.amount
              Zeko_circuits_config.Inputs.bridge_proof_fee
            |> Option.value_exn ~message:"Withdrawal amount overflow"
          in
          let transferrer_update =
            Account_update.with_no_aux
              ~body:
                { Account_update.Body.dummy with
                  public_key = Public_key.compress recipient.public_key
                ; balance_change =
                    Currency.Amount.Signed.(
                      negate @@ of_unsigned expected_amount)
                ; use_full_commitment = false
                ; authorization_kind = Signature
                ; increment_nonce = true
                ; preconditions =
                    { Account_update.Preconditions.accept with
                      account = Zkapp_precondition.Account.nonce nonce
                    }
                }
              ~authorization:(Control.Poly.Signature Signature.dummy)
          in
          let _forest, `Commitment commitment =
            Bridge_prover.Withdrawal_request.precompute_commitments
              !sequencer.bridge_prover
              { withdrawal_params; transferrer = transferrer_update }
            |> Or_error.ok_exn
          in
          let transferrer_update =
            Account_update.with_no_aux ~body:transferrer_update.body
              ~authorization:
                (Control.Poly.Signature
                   (Schnorr.Chunked.sign
                      ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
                      recipient.private_key
                      (Random_oracle.Input.Chunked.field commitment) ) )
          in
          Bridge_prover.(
            execute_request ~label:"SubmitEthereumWithdrawal" ~logger
              ~executor:l2_executor !sequencer.bridge_prover
              ( Withdrawal_request.f ~t:!sequencer.bridge_prover ~logger
                  { withdrawal_params; transferrer = transferrer_update }
              |> Or_error.ok_exn ))
          >>| Or_error.ok_exn
        in
        let live_sdk_paths () =
          let directory = Sys.getenv_exn "ZEKO_ETHEREUM_BRIDGE_LIVE_DIR" in
          ( Filename.concat directory "ready.json"
          , Filename.concat directory "registrations-complete"
          , Filename.concat directory "operations-ready"
          , Filename.concat directory "operations-complete" )
        in
        let registration_marker_paths () =
          let _, request_base, committed_base, _ = live_sdk_paths () in
          List.mapi ethereum_asset_records ~f:(fun index _ ->
              ( sprintf "%s-%d" request_base index
              , sprintf "%s-registration-%d" committed_base index ) )
        in
        let start_live_sdk () =
          let port =
            Option.value_map
              (Stdlib.Sys.getenv_opt "ZEKO_ETHEREUM_BRIDGE_LIVE_PORT")
              ~default:8082 ~f:Int.of_string
          in
          let ( ready_path
              , registration_complete_path
              , operations_ready_path
              , complete_path ) =
            live_sdk_paths ()
          in
          run (fun () ->
              start_graphql_server ~port !sequencer ~l1_executor ~l2_executor ) ;
          let token_verification_key_fields =
            match ethereum_token_bridge_verification_key with
            | None ->
                []
            | Some (data, hash) ->
                [ ("ethereumTokenBridgeVerificationKey", `String data)
                ; ( "ethereumTokenBridgeVerificationKeyHash"
                  , `String (Field.to_string hash) )
                ]
          in
          let registration_marker_fields =
            match bridge_asset with
            | Native ->
                []
            | Ethereum_token ->
                let markers = registration_marker_paths () in
                [ ( "registrationRequestMarkers"
                  , `List (List.map markers ~f:(fun (path, _) -> `String path))
                  )
                ; ( "registrationCommittedMarkers"
                  , `List (List.map markers ~f:(fun (_, path) -> `String path))
                  )
                ]
          in
          let manifest =
            `Assoc
              ( [ ("schemaVersion", `Int 4)
                ; ( "sequencerGraphqlUrl"
                  , `String (sprintf "http://127.0.0.1:%d/graphql" port) )
                ; ("l1GraphqlUrl", `String (Uri.to_string gql_uri))
                ; ( "outerPublicKey"
                  , `String
                      (Public_key.Compressed.to_base58_check
                         (Public_key.compress outer_kp.public_key) ) )
                ; ( "recipientPublicKey"
                  , `String
                      (Public_key.Compressed.to_base58_check
                         (Public_key.compress recipient.public_key) ) )
                ; ("completionMarker", `String complete_path)
                ; ( "registrationCompletionMarker"
                  , `String registration_complete_path )
                ; ("operationsReadyMarker", `String operations_ready_path)
                ]
              @ token_verification_key_fields @ registration_marker_fields )
          in
          Out_channel.write_all ready_path
            ~data:(Yojson.Safe.pretty_to_string manifest ^ "\n") ;
          printf "Live bridge SDK harness ready: %s\n%!" ready_path ;
          match bridge_asset with
          | Native ->
              run (fun () ->
                  wait_for_file ~timeout:(Time.Span.of_min 45.)
                    registration_complete_path )
          | Ethereum_token ->
              ()
        in
        let run_live_sdk () =
          let _, _, operations_ready_path, complete_path = live_sdk_paths () in
          Out_channel.write_all operations_ready_path ~data:"ready\n" ;
          run (fun () ->
              wait_for_file ~timeout:(Time.Span.of_min 45.) complete_path ) ;
          let helper_next_deposit helper_owner =
            let helper_account =
              Sequencer.get_account !sequencer
                (Public_key.compress recipient.public_key)
                (Account_id.derive_token_id ~owner:helper_owner)
              |> Option.value_exn
                   ~message:
                     "Live SDK deposit finalization did not create the helper \
                      account"
            in
            let next_deposit =
              Account.zkapp helper_account
              |> Option.value_exn
                   ~message:"Live SDK helper account is not a zkApp"
              |> fun zkapp ->
              let (next_deposit :: _ : F.t Zkapp_state.V.t) =
                Zkapp_account.Poly.app_state zkapp
              in
              UInt32.of_string (Field.to_string next_deposit)
            in
            next_deposit
          in
          let validate_helper_account ~expected_next_deposit helper_owner =
            let next_deposit = helper_next_deposit helper_owner in
            if not (UInt32.equal next_deposit expected_next_deposit) then
              failwithf "Live SDK finalized deposit index %s, expected %s"
                (UInt32.to_string next_deposit)
                (UInt32.to_string expected_next_deposit)
                ()
          in
          match bridge_asset with
          | Native -> (
              validate_helper_account ~expected_next_deposit:UInt32.one
                (Account_id.of_public_key
                   (Public_key.decompress_exn
                      Zeko_circuits_config.Inputs.holder_account_l2 ) ) ;
              let withdrawal_aux =
                Utils.value_to_hash ~init:Zeko_constants.withdrawal_salt
                  C.Bridge_state.Withdrawal_params_base.typ withdrawal_params
              in
              match
                Archive.find_ethereum_withdrawal !sequencer.archive
                  ~aux:withdrawal_aux
              with
              | Some withdrawal
                when Public_key.Compressed.equal withdrawal.recipient
                       withdrawal_params.recipient
                     && Currency.Amount.equal withdrawal.amount
                          withdrawal_params.amount ->
                  print_endline
                    "Live SDK finalized the deposit and submitted the native \
                     withdrawal"
              | _ ->
                  failwith
                    "Live SDK withdrawal request was not recorded in the OCaml \
                     archive" )
          | Ethereum_token ->
              List.mapi ethereum_asset_records ~f:(fun index record ->
                  let token_id = record.token_id_l2 in
                  let helper_owner =
                    Account_id.create
                      Zeko_circuits_config.Inputs.Ethereum_assets
                      .vault_public_key token_id
                  in
                  let next_deposit = helper_next_deposit helper_owner in
                  let vault =
                    Sequencer.get_account !sequencer
                      Zeko_circuits_config.Inputs.Ethereum_assets
                      .vault_public_key token_id
                    |> Option.value_exn
                         ~message:
                           "Live SDK did not install an ERC20 bridge vault"
                  in
                  let recipient_account =
                    Sequencer.get_account !sequencer
                      (Public_key.compress recipient.public_key)
                      token_id
                    |> Option.value_exn
                         ~message:"Live SDK did not credit an ERC20 recipient"
                  in
                  let cap =
                    UInt64.of_string
                      (Sys.getenv_exn
                         (sprintf "ERC20_TOKEN_%d_DEPOSIT_CAP" index) )
                  in
                  let deposit_amount =
                    Currency.Amount.to_uint64 deposit_params.amount
                  in
                  let withdrawal_amount =
                    Currency.Amount.to_uint64 withdrawal_params.amount
                  in
                  let expected_vault =
                    UInt64.add (UInt64.sub cap deposit_amount) withdrawal_amount
                  in
                  let expected_recipient =
                    UInt64.sub deposit_amount withdrawal_amount
                  in
                  let vault_balance =
                    Currency.Balance.to_uint64 vault.balance
                  in
                  let recipient_balance =
                    Currency.Balance.to_uint64 recipient_account.balance
                  in
                  if
                    (not (UInt64.equal vault_balance expected_vault))
                    || not (UInt64.equal recipient_balance expected_recipient)
                  then
                    failwithf
                      "Unexpected live ERC20[%d] balances: vault=%s \
                       recipient=%s"
                      index
                      (UInt64.to_string vault_balance)
                      (UInt64.to_string recipient_balance)
                      () ;
                  next_deposit )
              |> fun next_deposits ->
              let next_deposits =
                List.sort next_deposits ~compare:UInt32.compare
              in
              let expected_next_deposits =
                List.init (List.length ethereum_asset_records) ~f:(fun index ->
                    (* The registry checkpoint precedes the two ERC20 deposits
                       in the shared outer action stream. *)
                    UInt32.of_int (index + 2) )
              in
              if
                not
                  (List.equal UInt32.equal next_deposits expected_next_deposits)
              then
                failwithf
                  "Live SDK finalized unexpected ERC20 deposit indices: %s"
                  ( List.map next_deposits ~f:UInt32.to_string
                  |> String.concat ~sep:"," )
                  () ;
              print_endline
                "Live SDK deployed two standard tokens, finalized both ERC20 \
                 deposits, and submitted both ERC20 withdrawals"
        in
        let outer_action_state_before_registration =
          run (fun () ->
              Gql_client.fetch_action_state gql_uri
                (Public_key.compress outer_kp.public_key)
                ~logger
              >>| Or_error.ok_exn )
        in
        write_genesis_ledger () ;
        run fund_ethereum_token_operator ;
        require_live_sdk_fee_payer () ;
        if bridge_live_sdk then start_live_sdk () ;
        ( match bridge_asset with
        | Ethereum_token ->
            registration_marker_paths ()
            |> List.iteri ~f:(fun index (request_path, committed_path) ->
                   run (fun () ->
                       wait_for_file ~timeout:(Time.Span.of_min 45.)
                         request_path ) ;
                   run (fun () ->
                       commit_and_check
                         (sprintf
                            "Commit proof-backed Ethereum asset registration %d"
                            index ) ) ;
                   Out_channel.write_all committed_path ~data:"committed\n" )
        | Native ->
            () ) ;
        let outer_action_state_before =
          run (fun () ->
              Gql_client.fetch_action_state gql_uri
                (Public_key.compress outer_kp.public_key)
                ~logger
              >>| Or_error.ok_exn )
        in
        print_endline "(* Append synthetic Ethereum deposit actions *)" ;
        run (fun () ->
            let%bind () =
              Deferred.List.iter (deposit_auxes ()) ~f:(fun aux ->
                  submit_ethereum_deposit aux >>| ignore )
            in
            let%map _created =
              Gql_client.For_tests.create_new_block ~logger gql_uri
            in
            () ) ;
        let outer_action_state_after =
          run (fun () ->
              Gql_client.fetch_action_state gql_uri
                (Public_key.compress outer_kp.public_key)
                ~logger
              >>| Or_error.ok_exn )
        in
        write_scenario_manifest ~outer_action_state_before_registration
          ~outer_action_state_before ~outer_action_state_after ;
        run (fun () -> commit_and_check "Commit synchronized deposit") ;
        print_endline
          "(* Synchronize the accepting commit into the inner account *)" ;
        run synchronize_accepting_commit ;
        ( if bridge_live_sdk then run_live_sdk ()
        else
          match bridge_asset with
          | Native ->
              print_endline "(* Finalize deposit on L2 *)" ;
              run (fun () -> finalize_deposit () >>| ignore) ;
              print_endline "(* Submit native withdrawal on L2 *)" ;
              run (fun () -> submit_withdrawal () >>| ignore)
          | Ethereum_token ->
              failwith
                "BRIDGE_ASSET=erc20 requires ZEKO_ETHEREUM_BRIDGE_LIVE_SDK=true"
        ) ;
        run (fun () -> commit_and_check "Commit inner withdrawal action") ;
        let[@warning "-26"] sequencer = free_sequencer sequencer in
        run (fun () ->
            Relational_db.For_tests.drop_database ~port:5433
              "sequencer_ethereum_bridge_export" ) ) ;
    print_endline "Ethereum bridge settlement export completed" ;
    Stdlib.exit 0 )

let () =
  if not bridge_export_only then (
    print_endline "Started test 'apply commands and commit'" ;

    let postgres_uri1 =
      run (fun () ->
          Relational_db.For_tests.create_database ~port:5433 "sequencer1" )
    in
    let postgres_uri2 =
      run (fun () ->
          Relational_db.For_tests.create_database ~port:5433 "sequencer2" )
    in

    Quickcheck.test ~trials:1
      (Sequencer_spec.gen ~logger ~number_of_transactions:5
         ~postgres_uri:postgres_uri1 ~gql_uri ~da_config:da_config_with2
         ~da_keys ~da_quorum ~mq_host ~slot_acceptance () )
      ~f:(fun
           { outer_kp
           ; signer_pk
           ; specs
           ; sequencer
           ; da_keys
           ; accounts
           ; l1_config
           ; _
           }
         ->
        let commands =
          List.mapi specs ~f:(fun i spec ->
              if i % 2 = 0 then
                User_command.Zkapp_command
                  (account_update_send
                     ~chain:Zeko_circuits_config.Inputs.chain_l2 spec )
              else
                Signed_command
                  (command_send ~chain:Zeko_circuits_config.Inputs.chain_l2 spec) )
        in
        let zkapp_command_with_real_proof =
          let fee_signer = List.hd_exn accounts in
          let account_creation_fee =
            Account_update.with_aux
              ~body:
                { Account_update.Body.dummy with
                  public_key = Public_key.compress fee_signer.public_key
                ; balance_change =
                    Currency.Amount.Signed.of_fee @@ Currency.Fee.Signed.negate
                    @@ Currency.Fee.Signed.of_unsigned
                         constraint_constants.account_creation_fee
                ; authorization_kind = Signature
                ; use_full_commitment = true
                }
              ~authorization:(Control.Poly.Signature Signature.dummy)
          in
          let open Initialize_state.Test_module in
          (* First one is the inner account *)
          let call_forest =
            Zkapp_command.Call_forest.cons
              ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
              account_creation_fee
            @@ Zkapp_command.Call_forest.cons
                 ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
                 (Lazy.force Deploy_account_update.account_update)
            @@ Zkapp_command.Call_forest.cons_tree
                 (Lazy.force Initialize_account_update.account_update)
            @@ Zkapp_command.Call_forest.cons_tree
                 (Lazy.force Update_state_account_update.account_update)
                 []
          in
          User_command.Zkapp_command
            (Utils.sign_zkapp_command
               ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
               { fee_payer =
                   { body =
                       { Account_update.Body.Fee_payer.dummy with
                         public_key = Public_key.compress fee_signer.public_key
                       ; fee = Currency.Fee.of_mina_int_exn 1
                       }
                   ; authorization = Signature.dummy
                   }
               ; account_updates = call_forest
               ; memo = Signed_command_memo.empty
               }
               [ fee_signer; Keypair.of_private_key_exn sk ] )
        in
        let batch1, batch2 = List.split_n commands 1 in
        let batch1 = zkapp_command_with_real_proof :: batch1 in
        let batch2, batch3 = List.split_n batch2 2 in

        print_endline "(* Apply first batch *)" ;
        run (fun () ->
            Deferred.List.iter batch1 ~f:(fun command ->
                apply_user_command !sequencer command >>| Or_error.ok_exn ) ) ;

        print_endline "(* First commit *)" ;
        run (fun () ->
            let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
            let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
            let%bind () =
              Executor.wait_to_finish !sequencer.merger_ctx.executor
            in
            let%bind { ledger_hash = committed_ledger_hash; _ } =
              Gql_client.infer_state ~logger gql_uri ~signer_pk
                ~zkapp_pk:(Public_key.compress outer_kp.public_key)
              >>| Or_error.ok_exn
              >>| Utils.value_of_zkapp_state
                    Zeko_circuits.Rollup_state.Outer_state.typ
            in
            let target_ledger_hash = get_root !sequencer in
            [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;

            Deferred.unit ) ;

        (* To test nonce inferring from pool *)
        (* The first commit is still in the pool *)
        Executor.refresh_nonce !sequencer.merger_ctx.executor ;

        print_endline "(* Apply second batch *)" ;
        run (fun () ->
            Deferred.List.iter batch2 ~f:(fun command ->
                apply_user_command !sequencer command >>| Or_error.ok_exn ) ) ;

        print_endline "(* Second commit *)" ;
        run (fun () ->
            let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
            let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
            let%bind () =
              Executor.wait_to_finish !sequencer.merger_ctx.executor
            in
            let%bind _created =
              Gql_client.For_tests.create_new_block ~logger gql_uri
            in
            let%map { ledger_hash = committed_ledger_hash; _ } =
              Gql_client.infer_state ~logger gql_uri ~signer_pk
                ~zkapp_pk:(Public_key.compress outer_kp.public_key)
              >>| Or_error.ok_exn
              >>| Utils.value_of_zkapp_state
                    Zeko_circuits.Rollup_state.Outer_state.typ
            in
            let target_ledger_hash = get_root !sequencer in
            [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

        print_endline "(* Apply third batch *)" ;
        run (fun () ->
            Deferred.List.iter batch3 ~f:(fun command ->
                apply_user_command !sequencer command >>| Or_error.ok_exn ) ) ;

        print_endline "(* Third commit *)" ;
        let final_ledger_hash =
          run (fun () ->
              let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
              let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
              let%bind () =
                Executor.wait_to_finish !sequencer.merger_ctx.executor
              in
              let%bind _created =
                Gql_client.For_tests.create_new_block ~logger gql_uri
              in
              let%bind { ledger_hash = committed_ledger_hash; _ } =
                Gql_client.infer_state ~logger gql_uri ~signer_pk
                  ~zkapp_pk:(Public_key.compress outer_kp.public_key)
                >>| Or_error.ok_exn
                >>| Utils.value_of_zkapp_state
                      Zeko_circuits.Rollup_state.Outer_state.typ
              in
              let target_ledger_hash = get_root !sequencer in
              [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;

              return target_ledger_hash )
        in

        let[@warning "-26"] sequencer = free_sequencer sequencer in

        print_endline "(* Try to bootstrap again *)" ;
        let new_sequencer =
          run (fun () ->
              let%map new_sequencer =
                Sequencer.create ~logger ~max_pool_size:10
                  ~commitment_period_sec:0. ~da_config:da_config_with2 ~da_keys
                  ~da_quorum ~db_dir:None ~checkpoints_dir:None
                  ~postgres_uri:postgres_uri2 ~l1_uri:gql_uri
                  ~archive_uri:gql_uri ~signer:(get_test_signer ())
                  ~deposit_delay_blocks:0 ~mq_host ~fee_modifier:1.0
                  ~minimum_fee:0.01 ~slot_acceptance
                  ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
                  ~l1_config
                  ~commit_validity_period:
                    (Mina_numbers.Global_slot_span.of_int 10)
                  ~commit_fee:(Currency.Fee.of_mina_int_exn 1)
                  ~bridge_txn_fee:(Currency.Fee.of_mina_string_exn "0.1")
              in
              [%test_eq: Frozen_ledger_hash.t] (get_root new_sequencer)
                final_ledger_hash ;
              new_sequencer )
        in

        Gc.full_major () ;
        run (fun () -> Sequencer.shutdown new_sequencer) ;

        print_endline "(* Drop database *)" ;
        run (fun () ->
            Relational_db.For_tests.drop_database ~port:5433 "sequencer1" ) ;
        run (fun () ->
            Relational_db.For_tests.drop_database ~port:5433 "sequencer2" ) ) )

let () =
  if sequential_export_only && not bridge_export_only then (
    print_endline "Sequential Ethereum settlement export completed" ;
    Stdlib.exit 0 )

let () =
  print_endline "Started test 'dummy signature should fail'" ;

  let postgres_uri =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer" )
  in

  Quickcheck.test ~trials:1
    (Sequencer_spec.gen ~logger ~postgres_uri ~gql_uri
       ~da_config:da_config_with2 ~da_keys ~da_quorum ~mq_host ~slot_acceptance
       () ) ~f:(fun { specs; sequencer; _ } ->
      let dummy_signature_command : Zkapp_command.t =
        let command = account_update_send (List.hd_exn specs) in
        { command with
          account_updates =
            Zkapp_command.Call_forest.map command.account_updates
              ~f:(fun account_update ->
                match Account_update.Poly.authorization account_update with
                | Control.Poly.Signature _ ->
                    { account_update with
                      authorization = Control.Poly.Signature Signature.dummy
                    }
                | _ ->
                    account_update )
        }
      in
      let result =
        run (fun () ->
            apply_user_command !sequencer (Zkapp_command dummy_signature_command) )
      in
      match result with
      | Error e
        when String.is_substring ~substring:"Invalid_signature"
               (Error.to_string_hum e) ->
          run (fun () ->
              Gc.full_major () ;
              let%bind () = Sequencer.shutdown !sequencer in
              Relational_db.For_tests.drop_database ~port:5433 "sequencer" )
      | Ok _ ->
          failwith "Transaction should have failed"
      | Error unexpected_error ->
          Error.raise unexpected_error )

let () =
  print_endline "Started test 'restart sequencer and requeue witnesses'" ;
  let db_dir =
    Filename.concat Cache_dir.autogen_path
      (Uuid.to_string @@ Uuid_unix.create ())
  in
  let postgres_uri =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer" )
  in
  Quickcheck.test ~trials:1
    (Sequencer_spec.gen ~logger ~db_dir ~postgres_uri ~gql_uri
       ~da_config:da_config_with2 ~da_keys ~da_quorum ~mq_host ~slot_acceptance
       () )
    ~f:(fun { outer_kp; signer_pk; specs; sequencer; da_keys; l1_config; _ } ->
      let commands =
        List.mapi specs ~f:(fun i spec ->
            if i % 2 = 0 then
              User_command.Zkapp_command
                (account_update_send ~chain:Zeko_circuits_config.Inputs.chain_l2
                   spec )
            else
              Signed_command
                (command_send ~chain:Zeko_circuits_config.Inputs.chain_l2 spec) )
      in
      run (fun () ->
          Deferred.List.iter commands ~f:(fun command ->
              apply_user_command !sequencer command >>| Or_error.ok_exn ) ) ;

      let[@warning "-26"] sequencer = free_sequencer sequencer in

      print_endline "(* Restart sequencer *)" ;
      let new_sequencer =
        run (fun () ->
            Sequencer.create ~logger ~max_pool_size:10 ~commitment_period_sec:0.
              ~da_config:da_config_with3 ~da_quorum ~db_dir:(Some db_dir)
              ~checkpoints_dir:None ~postgres_uri ~l1_uri:gql_uri
              ~archive_uri:gql_uri ~signer:(get_test_signer ())
              ~deposit_delay_blocks:0 ~mq_host ~da_keys ~fee_modifier:1.0
              ~minimum_fee:0.01 ~slot_acceptance
              ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
              ~l1_config
              ~commit_validity_period:(Mina_numbers.Global_slot_span.of_int 10)
              ~commit_fee:(Currency.Fee.of_mina_int_exn 1)
              ~bridge_txn_fee:(Currency.Fee.of_mina_string_exn "0.1") )
      in

      print_endline "(* Requeue witnesses and commit with quorum 3 *)" ;
      let ledger_hash =
        run (fun () ->
            let%bind commit_result = commit new_sequencer >>| Or_error.ok_exn in
            let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
            let%bind () =
              Executor.wait_to_finish new_sequencer.merger_ctx.executor
            in
            let%map { ledger_hash = committed_ledger_hash; _ } =
              Gql_client.infer_state ~logger gql_uri ~signer_pk
                ~zkapp_pk:(Public_key.compress outer_kp.public_key)
              >>| Or_error.ok_exn
              >>| Utils.value_of_zkapp_state
                    Zeko_circuits.Rollup_state.Outer_state.typ
            in
            let target_ledger_hash = get_root new_sequencer in
            [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;
            committed_ledger_hash )
      in

      print_endline "(* Check that all da nodes are synced *)" ;
      run (fun () ->
          let%bind _multisig =
            Da_layer.Client.get_multisig
              { new_sequencer.da_client with quorum = 3 }
              ~ledger_hash
          in
          let%map da_nodes_synced =
            Deferred.List.map da_config_with3.nodes ~f:(fun node ->
                Da_layer.Client.Rpc.has_diff ~logger ~node_location:node
                  ~ledger_hash )
            >>| Result.all >>| Or_error.ok_exn >>| List.for_all ~f:Fn.id
          in
          [%test_eq: bool] da_nodes_synced true ) ;

      print_endline "(* Assert that no witnesses are left in merger *)" ;
      run (fun () ->
          let%map all_witnesses =
            Relational_db.Pool.use
              (fun conn -> Merger.P.Witness_table.get_all conn ())
              new_sequencer.db_pool
            >>| Relational_db.caqti_ok_exn
                  ~msg:"Failed to get all witnesses: %s"
          in
          [%test_eq: int] (List.length all_witnesses) 0 ) ;

      Gc.full_major () ;
      run (fun () -> Sequencer.shutdown new_sequencer) ;

      print_endline "(* Drop database *)" ;
      run (fun () ->
          Relational_db.For_tests.drop_database ~port:5433 "sequencer" ) )

let () =
  print_endline "Started test 'create checkpoints and restart from checkpoint'" ;
  let db_dir1 =
    Filename.concat Cache_dir.autogen_path
      (Uuid.to_string @@ Uuid_unix.create ())
  in
  let db_dir2 =
    Filename.concat Cache_dir.autogen_path
      (Uuid.to_string @@ Uuid_unix.create ())
  in
  let checkpoints_dir =
    Filename.concat Cache_dir.autogen_path
      (Uuid.to_string @@ Uuid_unix.create ())
  in
  let postgres_uri1 =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer1" )
  in
  let postgres_uri2 =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer2" )
  in
  Quickcheck.test ~trials:1
    (Sequencer_spec.gen ~logger ~db_dir:db_dir1 ~checkpoints_dir
       ~postgres_uri:postgres_uri1 ~gql_uri ~da_config:da_config_with2 ~da_keys
       ~da_quorum ~mq_host ~slot_acceptance () )
    ~f:(fun { outer_kp; signer_pk; specs; sequencer; da_keys; l1_config; _ } ->
      let commands =
        List.mapi specs ~f:(fun i spec ->
            if i % 2 = 0 then
              User_command.Zkapp_command
                (account_update_send ~chain:Zeko_circuits_config.Inputs.chain_l2
                   spec )
            else
              Signed_command
                (command_send ~chain:Zeko_circuits_config.Inputs.chain_l2 spec) )
      in
      run (fun () ->
          Deferred.List.iter commands ~f:(fun command ->
              apply_user_command !sequencer command >>| Or_error.ok_exn ) ) ;

      print_endline "(* Commit *)" ;
      let ledger_hash =
        run (fun () ->
            let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
            let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
            let%bind () =
              Executor.wait_to_finish !sequencer.merger_ctx.executor
            in
            let%map { ledger_hash = committed_ledger_hash; _ } =
              Gql_client.infer_state ~logger gql_uri ~signer_pk
                ~zkapp_pk:(Public_key.compress outer_kp.public_key)
              >>| Or_error.ok_exn
              >>| Utils.value_of_zkapp_state
                    Zeko_circuits.Rollup_state.Outer_state.typ
            in
            let target_ledger_hash = get_root !sequencer in
            [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ;
            committed_ledger_hash )
      in

      let[@warning "-26"] sequencer = free_sequencer sequencer in

      print_endline "(* Restart sequencer from checkpoint *)" ;
      let new_sequencer =
        run (fun () ->
            Sequencer.create ~logger ~max_pool_size:10 ~commitment_period_sec:0.
              ~da_config:da_config_with3 ~da_quorum ~db_dir:(Some db_dir2)
              ~checkpoints_dir:(Some checkpoints_dir)
              ~postgres_uri:postgres_uri2 ~l1_uri:gql_uri ~archive_uri:gql_uri
              ~signer:(get_test_signer ()) ~deposit_delay_blocks:0 ~mq_host
              ~da_keys ~fee_modifier:1.0 ~minimum_fee:0.01 ~slot_acceptance
              ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
              ~l1_config
              ~commit_validity_period:(Mina_numbers.Global_slot_span.of_int 10)
              ~commit_fee:(Currency.Fee.of_mina_int_exn 1)
              ~bridge_txn_fee:(Currency.Fee.of_mina_string_exn "0.1") )
      in

      print_endline "(* Check that all da nodes are synced *)" ;
      run (fun () ->
          let%bind _multisig =
            Da_layer.Client.get_multisig
              { new_sequencer.da_client with quorum = 3 }
              ~ledger_hash
          in
          let%map da_nodes_synced =
            Deferred.List.map da_config_with3.nodes ~f:(fun node ->
                Da_layer.Client.Rpc.has_diff ~logger ~node_location:node
                  ~ledger_hash )
            >>| Result.all >>| Or_error.ok_exn >>| List.for_all ~f:Fn.id
          in
          [%test_eq: bool] da_nodes_synced true ) ;

      Gc.full_major () ;
      run (fun () -> Sequencer.shutdown new_sequencer) ;

      print_endline "(* Drop database *)" ;
      run (fun () ->
          Relational_db.For_tests.drop_database ~port:5433 "sequencer1" ) ;
      run (fun () ->
          Relational_db.For_tests.drop_database ~port:5433 "sequencer2" ) )

let () =
  print_endline "Started test 'restart sequencer and recommit'" ;
  let db_dir =
    Filename.concat Cache_dir.autogen_path
      (Uuid.to_string @@ Uuid_unix.create ())
  in
  let postgres_uri =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer" )
  in
  Quickcheck.test ~trials:1
    (Sequencer_spec.gen ~logger ~db_dir ~postgres_uri ~gql_uri
       ~da_config:da_config_with2 ~da_keys ~da_quorum ~mq_host ~slot_acceptance
       () )
    ~f:(fun { outer_kp; signer_pk; specs; sequencer; da_keys; l1_config; _ } ->
      let commands =
        List.mapi specs ~f:(fun i spec ->
            if i % 2 = 0 then
              User_command.Zkapp_command
                (account_update_send ~chain:Zeko_circuits_config.Inputs.chain_l2
                   spec )
            else
              Signed_command
                (command_send ~chain:Zeko_circuits_config.Inputs.chain_l2 spec) )
      in
      let batch1, batch2 = List.split_n commands 3 in
      let initial_ledger_hash = get_root !sequencer in

      print_endline "(* Apply first batch *)" ;
      run (fun () ->
          Deferred.List.iter batch1 ~f:(fun command ->
              apply_user_command !sequencer command >>| Or_error.ok_exn ) ) ;

      print_endline "(* First commit *)" ;
      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          Executor.wait_to_finish !sequencer.merger_ctx.executor ) ;

      print_endline "(* Apply second batch *)" ;
      run (fun () ->
          Deferred.List.iter batch2 ~f:(fun command ->
              apply_user_command !sequencer command >>| Or_error.ok_exn ) ) ;

      print_endline "(* Second commit *)" ;
      let final_ledger_hash =
        run (fun () ->
            let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
            let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
            let%bind () =
              Executor.wait_to_finish !sequencer.merger_ctx.executor
            in
            let%bind _cleared =
              Gql_client.For_tests.clear_pool ~logger gql_uri
            in
            let%map { ledger_hash = committed_ledger_hash; _ } =
              Gql_client.infer_state ~logger gql_uri ~signer_pk
                ~zkapp_pk:(Public_key.compress outer_kp.public_key)
              >>| Or_error.ok_exn
              >>| Utils.value_of_zkapp_state
                    Zeko_circuits.Rollup_state.Outer_state.typ
            in
            [%test_eq: Ledger_hash.t] committed_ledger_hash initial_ledger_hash ;
            get_root !sequencer )
      in

      let[@warning "-26"] sequencer = free_sequencer sequencer in

      print_endline "(* Restart sequencer *)" ;
      let new_sequencer =
        run (fun () ->
            Sequencer.create ~logger ~max_pool_size:10 ~commitment_period_sec:0.
              ~da_config:da_config_with2 ~da_quorum ~db_dir:(Some db_dir)
              ~checkpoints_dir:None ~postgres_uri ~l1_uri:gql_uri
              ~archive_uri:gql_uri ~signer:(get_test_signer ())
              ~deposit_delay_blocks:0 ~mq_host ~da_keys ~fee_modifier:1.0
              ~minimum_fee:0.01 ~slot_acceptance
              ~proof_cache_db:(Proof_cache_tag.create_identity_db ())
              ~l1_config
              ~commit_validity_period:(Mina_numbers.Global_slot_span.of_int 10)
              ~commit_fee:(Currency.Fee.of_mina_int_exn 1)
              ~bridge_txn_fee:(Currency.Fee.of_mina_string_exn "0.1") )
      in

      print_endline "(* Check that after restart it recommited *)" ;
      run (fun () ->
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          [%test_eq: Ledger_hash.t] committed_ledger_hash final_ledger_hash ) ;

      Gc.full_major () ;
      run (fun () -> Sequencer.shutdown new_sequencer) ;

      print_endline "(* Drop database *)" ;
      run (fun () ->
          Relational_db.For_tests.drop_database ~port:5433 "sequencer" ) )

let () =
  print_endline "Started test 'slot range check'" ;
  let postgres_uri =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer" )
  in
  Quickcheck.test ~trials:1
    (Sequencer_spec.gen ~logger ~number_of_transactions:5 ~postgres_uri ~gql_uri
       ~da_config:da_config_with2 ~da_keys ~da_quorum ~mq_host
       ~slot_acceptance:(Time.Span.of_min 10.) () )
    ~f:(fun { specs; sequencer; signer_pk; outer_kp; l1_config; _ } ->
      run (fun () ->
          let open Mina_numbers in
          let spec, specs = (List.hd_exn specs, List.tl_exn specs) in
          let current_slot : Global_slot_since_genesis.t =
            Utils.Slot.global_slot ~l1_config
          in
          let command =
            User_command.Signed_command
              (command_send
                 ~valid_until:
                   Global_slot_since_genesis.(
                     add current_slot (Global_slot_span.of_int 1))
                 ~chain:Zeko_circuits_config.Inputs.chain_l2 spec )
          in
          let%bind result = apply_user_command !sequencer command in
          [%test_eq: unit Or_error.t] result
            (Error
               (Error.of_string
                  "Upper slot has too small margin to be committed" ) ) ;
          let command =
            User_command.Signed_command
              (command_send
                 ~valid_until:
                   Global_slot_since_genesis.(
                     add current_slot (Global_slot_span.of_int 5))
                 ~chain:Zeko_circuits_config.Inputs.chain_l2 spec )
          in
          let%bind result = apply_user_command !sequencer command in
          [%test_eq: unit Or_error.t] result (Ok ()) ;

          let spec, specs = (List.hd_exn specs, List.tl_exn specs) in
          let current_slot : Global_slot_since_genesis.t =
            Utils.Slot.global_slot ~l1_config
          in
          let command =
            User_command.Zkapp_command
              (account_update_send
                 ~global_slot_precondition:
                   ( Check
                       { lower = current_slot
                       ; upper =
                           Global_slot_since_genesis.(
                             add current_slot (Global_slot_span.of_int 1))
                       }
                   , Check
                       { lower =
                           Global_slot_since_genesis.(
                             add current_slot (Global_slot_span.of_int 2))
                       ; upper =
                           Global_slot_since_genesis.(
                             add current_slot (Global_slot_span.of_int 3))
                       } )
                 ~chain:Zeko_circuits_config.Inputs.chain_l2 spec )
          in
          let%bind result = apply_user_command !sequencer command in
          [%test_eq: unit Or_error.t] result
            (Error (Error.of_string "Conflicting slot ranges")) ;

          let spec, specs = (List.hd_exn specs, List.tl_exn specs) in
          let current_slot : Global_slot_since_genesis.t =
            Utils.Slot.global_slot ~l1_config
          in
          let command =
            User_command.Zkapp_command
              (account_update_send
                 ~valid_while:
                   ( Check
                       { lower =
                           Global_slot_since_genesis.(
                             add current_slot (Global_slot_span.of_int 1))
                       ; upper =
                           Global_slot_since_genesis.(
                             add current_slot (Global_slot_span.of_int 10))
                       }
                   , Ignore )
                 ~chain:Zeko_circuits_config.Inputs.chain_l2 spec )
          in
          let%bind result = apply_user_command !sequencer command in
          [%test_eq: unit Or_error.t] result
            (Error (Error.of_string "Lower slot is in the future")) ;

          let spec, _specs = (List.hd_exn specs, List.tl_exn specs) in
          let current_slot : Global_slot_since_genesis.t =
            Utils.Slot.global_slot ~l1_config
          in
          let command =
            User_command.Zkapp_command
              (account_update_send
                 ~valid_until:
                   (Some
                      Global_slot_since_genesis.(
                        add current_slot (Global_slot_span.of_int 10)) )
                 ~valid_while:
                   ( Check
                       { lower = current_slot
                       ; upper =
                           Global_slot_since_genesis.(
                             add current_slot (Global_slot_span.of_int 15))
                       }
                   , Ignore )
                 ~global_slot_precondition:
                   ( Ignore
                   , Check
                       { lower =
                           Global_slot_since_genesis.(
                             sub current_slot (Global_slot_span.of_int 15)
                             |> Option.value_exn)
                       ; upper =
                           Global_slot_since_genesis.(
                             add current_slot (Global_slot_span.of_int 20))
                       } )
                 ~chain:Zeko_circuits_config.Inputs.chain_l2 spec )
          in
          let%bind result = apply_user_command !sequencer command in
          [%test_eq: unit Or_error.t] result (Ok ()) ;

          return () ) ;

      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      let[@warning "-26"] sequencer = free_sequencer sequencer in

      print_endline "(* Drop database *)" ;
      run (fun () ->
          Relational_db.For_tests.drop_database ~port:5433 "sequencer" ) )

let () =
  print_endline "Started test 'deposits'" ;
  let postgres_uri =
    run (fun () ->
        Relational_db.For_tests.create_database ~port:5433 "sequencer" )
  in
  let open Mina_numbers in
  Quickcheck.test ~trials:1
    (Sequencer_spec.gen ~logger ~number_of_transactions:0 ~postgres_uri ~gql_uri
       ~da_config:da_config_with2 ~da_keys ~da_quorum ~mq_host
       ~slot_acceptance:(Time.Span.of_min 10.)
       ~commit_validity_period:(Global_slot_span.of_int 20)
       () )
    ~f:(fun { outer_kp
            ; sequencer
            ; signer_pk
            ; l1_config
            ; l1_executor
            ; l2_executor
            ; _
            } ->
      (* Create l1 accounts *)
      let l1_accounts =
        Array.create ~len:6 ()
        |> Array.map ~f:Signature_lib.Keypair.create
        |> Array.to_list
      in
      run (fun () ->
          Deferred.List.iter l1_accounts ~f:(fun keypair ->
              let%bind _res =
                Gql_client.For_tests.create_account ~logger gql_uri
                  (Signature_lib.Public_key.compress keypair.public_key)
              in
              return () ) ) ;

      let submit_deposit (signer : Keypair.t)
          (deposit_params : C.Bridge_state.Deposit_params_base.t) =
        let%bind nonce =
          Gql_client.infer_nonce ~logger gql_uri
            (Signature_lib.Public_key.compress signer.public_key)
          >>| Or_error.ok_exn
        in
        let bridge_proof_fee = Zeko_circuits_config.Inputs.bridge_proof_fee in
        let transferrer_update =
          Account_update.with_no_aux
            ~body:
              { Account_update.Body.dummy with
                public_key = Public_key.compress signer.public_key
              ; balance_change =
                  Currency.Amount.Signed.(
                    negate @@ of_unsigned
                    @@ Option.value_exn
                         (Currency.Amount.add deposit_params.amount
                            bridge_proof_fee ))
              ; use_full_commitment = false
              ; increment_nonce = true
              ; authorization_kind = Signature
              ; preconditions =
                  { Account_update.Preconditions.accept with
                    account = Zkapp_precondition.Account.nonce nonce
                  }
              }
            ~authorization:(Control.Poly.Signature Signature.dummy)
        in
        let _forest, `Commitment tx_commitment =
          Bridge_prover.Deposit_request.precompute_commitments
            !sequencer.bridge_prover
            { deposit_params; transferrer = transferrer_update }
          |> Or_error.ok_exn
        in
        let transferrer_update =
          Account_update.with_no_aux ~body:transferrer_update.body
            ~authorization:
              (Control.Poly.Signature
                 (Signature_lib.Schnorr.Chunked.sign
                    ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
                    signer.private_key
                    (Random_oracle.Input.Chunked.field tx_commitment) ) )
        in
        Bridge_prover.(
          execute_request ~label:"SubmitDeposit" ~logger ~executor:l1_executor
            !sequencer.bridge_prover
            ( Deposit_request.f ~t:!sequencer.bridge_prover ~logger
                { deposit_params; transferrer = transferrer_update }
            |> Or_error.ok_exn ))
        >>| Or_error.ok_exn
      in
      let deposit ~amount ~(account : Keypair.t) ~timeout :
          C.Bridge_state.Deposit_params_base.t =
        let current_slot : Global_slot_since_genesis.t =
          Utils.Slot.global_slot ~l1_config
        in
        let timeout =
          C.Zeko_util.Slot.add current_slot (Global_slot_span.of_int timeout)
        in
        { children = []
        ; holder_account_l1 =
            List.hd_exn Zeko_circuits_config.Inputs.holder_accounts_l1
        ; recipient = Public_key.compress account.public_key
        ; amount = Currency.Amount.of_mina_int_exn amount
        ; timeout
        }
      in

      let account1 = List.nth_exn l1_accounts 0 in
      let account2 = List.nth_exn l1_accounts 1 in
      let account3 = List.nth_exn l1_accounts 2 in

      print_endline "(* Send 1-3 deposits *)" ;
      let deposits =
        run (fun () ->
            let deposit1 = deposit ~amount:10 ~account:account1 ~timeout:40 in
            let deposit2 = deposit ~amount:20 ~account:account2 ~timeout:40 in
            let deposit3 = deposit ~amount:30 ~account:account3 ~timeout:40 in

            let%bind _hash = submit_deposit account1 deposit1 in
            let%bind _hash = submit_deposit account2 deposit2 in
            let%bind _hash = submit_deposit account3 deposit3 in

            let%bind _created =
              Gql_client.For_tests.create_new_block ~logger gql_uri
            in
            return
              [ (account1, deposit1)
              ; (account2, deposit2)
              ; (account3, deposit3)
              ] )
      in

      print_endline "(* Commit 1-3 deposits *)" ;
      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      print_endline "(* Send 4-6 deposits *)" ;
      let deposits =
        deposits
        @ run (fun () ->
              let deposit4 = deposit ~amount:40 ~account:account1 ~timeout:40 in
              let deposit5 = deposit ~amount:50 ~account:account2 ~timeout:40 in
              let deposit6 = deposit ~amount:60 ~account:account3 ~timeout:40 in

              let%bind _hash = submit_deposit account1 deposit4 in
              let%bind _hash = submit_deposit account2 deposit5 in
              let%bind _hash = submit_deposit account3 deposit6 in

              let%bind _created =
                Gql_client.For_tests.create_new_block ~logger gql_uri
              in
              return
                [ (account1, deposit4)
                ; (account2, deposit5)
                ; (account3, deposit6)
                ] )
      in

      print_endline "(* Commit 4-6 deposits *)" ;
      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      print_endline "(* Sync the latest commit *)" ;
      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      let finalize_deposit (signer : Keypair.t) deposit_params =
        let%bind actions =
          Gql_client.fetch_actions ~logger gql_uri
            (Public_key.compress outer_kp.public_key)
          >>| Or_error.ok_exn
          >>| List.map ~f:(fun (fields, _, _, before, after) ->
                  ( Utils.actions_to_outer_action (List.hd_exn fields)
                  , before
                  , after ) )
        in
        let ( my_deposit_index
            , (_my_deposit, `Before before_my_deposit_action_state, _) ) =
          let hashed_deposit =
            Utils.value_to_hash ~init:Zeko_constants.deposit_salt
              C.Bridge_state.Deposit_params_base.typ deposit_params
          in
          List.findi actions ~f:(fun _ (action, _, _) ->
              match action with
              | Commit _ ->
                  false
              | Witness witness ->
                  Field.equal hashed_deposit witness.aux )
          |> Option.value_exn ~message:"Did not find my deposit"
        in
        let ( nearest_commit_index
            , (_nearest_commit, _, `After after_nearest_commit_action_state) ) =
          List.sub actions ~pos:my_deposit_index
            ~len:(List.length actions - my_deposit_index)
          |> List.find_mapi ~f:(fun i (action, before, after) ->
                 match action with
                 | Commit commit ->
                     Some (i, (commit, before, after))
                 | Witness _ ->
                     None )
          |> Option.value_exn ~message:"Did not find nearest commit"
        in
        let nearest_commit_index = nearest_commit_index + my_deposit_index in
        let check_accepted :
            Bridge.Check_accepted_mina.Init.t
            * Bridge.Check_accepted_mina.Elem.t list =
          ( { params = deposit_params
            ; original_action_state =
                C.Rollup_state.Outer_action_state.unsafe_value_of_field
                  before_my_deposit_action_state
            ; deposit_index = UInt32.of_int my_deposit_index
            }
          , List.sub actions ~pos:my_deposit_index
              ~len:(nearest_commit_index - my_deposit_index + 1)
            |> List.map ~f:(fun (action, _, _) -> action) )
        in
        let current_synced_outer_action_state =
          Sequencer.current_synced_outer_action_state !sequencer
        in
        let%bind ase_actions =
          Gql_client.fetch_actions ~logger gql_uri
            ~from_action_state:after_nearest_commit_action_state
            ~end_action_state:
              C.Rollup_state.Outer_action_state.(
                With_length.state current_synced_outer_action_state |> raw)
            (Public_key.compress outer_kp.public_key)
          >>| Or_error.ok_exn
          >>| List.map ~f:(fun (fields, _, _, _, _) ->
                  Zkapp_account.Actions_impl.hash fields )
        in
        let ase : Ase.With_length.Stmt.t * Field.t list =
          ( { action_state = after_nearest_commit_action_state
            ; length =
                Zeko_circuits.Zeko_util.(
                  Checked32.sub
                    C.Rollup_state.Outer_action_state.(
                      With_length.length current_synced_outer_action_state)
                    (Checked32.of_int @@ List.length ase_actions)
                  |> Option.value_exn ~message:"Negative length")
            }
          , ase_actions )
        in
        let helper_account =
          Sequencer.get_account !sequencer
            (Public_key.compress signer.public_key)
            (Account_id.derive_token_id
               ~owner:
                 (Account_id.of_public_key
                    (Public_key.decompress_exn
                       Zeko_circuits_config.Inputs.holder_account_l2 ) ) )
        in
        let prev_next_deposit =
          Option.value ~default:UInt32.zero
            (let%bind.Option acc = helper_account in
             let%map.Option zkapp = Account.zkapp acc in
             let (next_deposit :: _ : F.t Zkapp_state.V.t) =
               Zkapp_account.Poly.app_state zkapp
             in
             UInt32.of_string (Field.to_string next_deposit) )
        in
        let prev_nonce =
          Option.value ~default:UInt32.zero
            (Option.map helper_account ~f:(fun acc ->
                 UInt32.of_string
                   (Mina_numbers.Account_nonce.to_string acc.nonce) ) )
        in
        let witness : Bridge_prover.Finalize_deposit.t_ =
          { ase_source = fst ase
          ; ase_elems = snd ase
          ; check_accepted_init = fst check_accepted
          ; check_accepted_elems = snd check_accepted
          ; prev_next_deposit
          ; prev_nonce
          ; helper_account_new = Option.is_none helper_account
          }
        in
        let _forest, `Commitment commitment =
          Bridge_prover.Finalize_deposit.precompute_commitments
            !sequencer.bridge_prover witness
          |> Or_error.ok_exn
        in
        let helper_account_signature =
          Signature_lib.Schnorr.Chunked.sign
            ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
            signer.private_key
            (Random_oracle.Input.Chunked.field commitment)
        in
        Bridge_prover.(
          execute_request ~label:"FinalizeDeposit" ~logger ~executor:l2_executor
            !sequencer.bridge_prover
            ( Finalize_deposit.f ~t:!sequencer.bridge_prover ~logger witness
                helper_account_signature
            |> Or_error.ok_exn ))
        >>| Or_error.ok_exn
      in

      print_endline "(* Finalize all deposits *)" ;
      run (fun () ->
          Deferred.List.iteri deposits ~f:(fun i (signer, deposit_params) ->
              printf "(* Finalizing deposit %d *)\n%!" i ;
              finalize_deposit signer deposit_params >>| ignore ) ) ;

      print_endline "(* Send 7-9 deposits *)" ;
      let timeout_deposits =
        run (fun () ->
            let deposit7 = deposit ~amount:70 ~account:account1 ~timeout:10 in
            let deposit8 = deposit ~amount:80 ~account:account2 ~timeout:10 in
            let deposit9 = deposit ~amount:90 ~account:account3 ~timeout:10 in

            let%bind _hash = submit_deposit account1 deposit7 in
            let%bind _hash = submit_deposit account2 deposit8 in
            let%bind _hash = submit_deposit account3 deposit9 in

            let%bind _created =
              Gql_client.For_tests.create_new_block ~logger gql_uri
            in
            return
              [ (account1, deposit7)
              ; (account2, deposit8)
              ; (account3, deposit9)
              ] )
      in

      (* deposit timeout has to be more than commit validity period *)
      (* canceled deposit timeout has to be less than commit validity period *)
      (* shift has to be more than timeout but less than commit validity period for canceled deposit *)
      print_endline "(* Commit 7-9 deposits after timeout *)" ;
      run (fun () ->
          let%bind _shifted =
            Gql_client.For_tests.shift_slots ~logger gql_uri 15
          in
          Utils.Slot.For_tests.add_to_global_slot := 15 ;
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      let cancel_deposit (signer : Keypair.t) deposit_params =
        let%bind actions =
          Gql_client.fetch_actions ~logger gql_uri
            (Public_key.compress outer_kp.public_key)
          >>| Or_error.ok_exn
          >>| List.map ~f:(fun (fields, _, _, before, after) ->
                  ( Utils.actions_to_outer_action (List.hd_exn fields)
                  , before
                  , after ) )
        in
        let ( my_deposit_index
            , (_my_deposit, `Before before_my_deposit_action_state, _) ) =
          let hashed_deposit =
            Utils.value_to_hash ~init:Zeko_constants.deposit_salt
              C.Bridge_state.Deposit_params_base.typ deposit_params
          in
          List.findi actions ~f:(fun _ (action, _, _) ->
              match action with
              | Commit _ ->
                  false
              | Witness witness ->
                  Field.equal hashed_deposit witness.aux )
          |> Option.value_exn ~message:"Did not find my deposit"
        in
        let ( nearest_commit_index
            , ( nearest_commit
              , `Before before_nearest_commit_action_state
              , `After after_nearest_commit_action_state ) ) =
          List.sub actions ~pos:my_deposit_index
            ~len:(List.length actions - my_deposit_index)
          |> List.find_mapi ~f:(fun i (action, before, after) ->
                 match action with
                 | Commit commit ->
                     Some (i, (commit, before, after))
                 | Witness _ ->
                     None )
          |> Option.value_exn ~message:"Did not find nearest commit"
        in
        let nearest_commit_index = nearest_commit_index + my_deposit_index in
        let check_accepted :
            Bridge.Check_accepted_mina.Init.t
            * Bridge.Check_accepted_mina.Elem.t list =
          ( { params = deposit_params
            ; original_action_state =
                C.Rollup_state.Outer_action_state.unsafe_value_of_field
                  before_my_deposit_action_state
            ; deposit_index = UInt32.of_int my_deposit_index
            }
          , List.sub actions ~pos:my_deposit_index
              ~len:(nearest_commit_index - my_deposit_index + 1)
            |> List.map ~f:(fun (action, _, _) -> action) )
        in
        let commit_ase : Ase.Without_length.Stmt.t * Field.t list =
          ( after_nearest_commit_action_state
          , List.sub actions ~pos:(nearest_commit_index + 1)
              ~len:(List.length actions - nearest_commit_index - 1)
            |> List.map ~f:(fun (action, _, _) ->
                   [ Utils.actions_of_outer_action action ]
                   |> Zkapp_account.Actions_impl.hash ) )
        in
        let check_accepted_ase : Ase.With_length.Stmt.t * Field.t list =
          ( { action_state = fst commit_ase
            ; length =
                Zeko_circuits.Zeko_util.Checked32.of_int
                  (nearest_commit_index + 1)
            }
          , snd commit_ase )
        in
        let sync_ase : Ase.With_length.Stmt.t * Field.t list =
          let commit_sync_index =
            ( Zeko_circuits.Rollup_state.Outer_action_state.With_length.length
                nearest_commit.synchronized_outer_action_state
            |> Account_nonce.to_int )
            - 1
          in
          ( { action_state =
                C.Rollup_state.Outer_action_state.(
                  With_length.raw nearest_commit.synchronized_outer_action_state)
            ; length =
                C.Rollup_state.Outer_action_state.(
                  With_length.length
                    nearest_commit.synchronized_outer_action_state)
            }
          , List.sub actions ~pos:(commit_sync_index + 1)
              ~len:(List.length actions - commit_sync_index - 1)
            |> List.map ~f:(fun (action, _, _) ->
                   [ Utils.actions_of_outer_action action ]
                   |> Zkapp_account.Actions_impl.hash ) )
        in
        let helper_aid =
          Account_id.create
            (Public_key.compress signer.public_key)
            (Account_id.derive_token_id
               ~owner:
                 (Account_id.of_public_key
                    (Public_key.decompress_exn
                       Zeko_circuits_config.Inputs.helper_token_owner_l1 ) ) )
        in
        let%bind prev_next_cancelled_deposit =
          match%map
            Gql_client.fetch_state_opt ~logger gql_uri helper_aid
            >>| Or_error.ok_exn
          with
          | Some (next_cancelled_deposit :: _next_withdrawal :: _) ->
              Some (UInt32.of_string (Field.to_string next_cancelled_deposit))
          | None ->
              None
        in
        let%bind prev_nonce =
          match%map
            Gql_client.fetch_nonce_opt ~logger gql_uri helper_aid
            >>| Or_error.ok_exn
          with
          | Some nonce ->
              nonce
          | None ->
              UInt32.zero
        in
        let witness : Bridge_prover.Finalize_cancelled_deposit.t_ =
          { public_key =
              List.hd_exn Zeko_circuits_config.Inputs.holder_accounts_l1
          ; commit = nearest_commit
          ; before_commit =
              C.Rollup_state.Outer_action_state.unsafe_value_of_field
                before_nearest_commit_action_state
          ; commit_ase_source = fst commit_ase
          ; commit_ase_elems = snd commit_ase
          ; sync_ase_source = fst sync_ase
          ; sync_ase_elems = snd sync_ase
          ; check_accepted_init = fst check_accepted
          ; check_accepted_elems = snd check_accepted
          ; check_accepted_ase_source = fst check_accepted_ase
          ; check_accepted_ase_elems = snd check_accepted_ase
          ; prev_next_cancelled_deposit =
              Option.value prev_next_cancelled_deposit ~default:UInt32.zero
          ; prev_nonce
          ; helper_account_new = Option.is_none prev_next_cancelled_deposit
          }
        in
        let _forest, `Commitment commitment =
          Bridge_prover.Finalize_cancelled_deposit.precompute_commitments
            !sequencer.bridge_prover witness
          |> Or_error.ok_exn
        in
        let helper_account_signature =
          Signature_lib.Schnorr.Chunked.sign
            ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
            signer.private_key
            (Random_oracle.Input.Chunked.field commitment)
        in
        Bridge_prover.(
          execute_request ~label:"FinalizeCancelledDeposit" ~logger
            ~executor:l1_executor !sequencer.bridge_prover
            ( Finalize_cancelled_deposit.f ~t:!sequencer.bridge_prover ~logger
                witness helper_account_signature
            |> Or_error.ok_exn ))
        >>| Or_error.ok_exn
      in
      print_endline "(* Cancel timeouted deposits 7-9 *)" ;
      run (fun () ->
          let%bind () =
            Deferred.List.iteri timeout_deposits
              ~f:(fun i (signer, deposit_params) ->
                printf "(* Canceling deposit %d *)\n%!" i ;
                let%bind hash = cancel_deposit signer deposit_params in
                let%bind _created =
                  Gql_client.For_tests.create_new_block ~logger gql_uri
                in
                let%map status =
                  Gql_client.For_tests.get_zkapp_command_status ~logger gql_uri
                    hash
                in
                [%test_eq: string list list option] status None )
          in
          return () ) ;

      print_endline "Started test 'withdrawals'" ;

      let submit_withdrawal (signer : Keypair.t)
          (withdrawal_params : C.Bridge_state.Withdrawal_params_base.t) =
        let nonce =
          Sequencer.infer_nonce !sequencer
            (Signature_lib.Public_key.compress signer.public_key)
        in
        let bridge_proof_fee = Zeko_circuits_config.Inputs.bridge_proof_fee in
        let transferrer_update =
          Account_update.with_no_aux
            ~body:
              { Account_update.Body.dummy with
                public_key = Public_key.compress signer.public_key
              ; balance_change =
                  Currency.Amount.Signed.(
                    negate @@ of_unsigned
                    @@ Option.value_exn
                         (Currency.Amount.add withdrawal_params.amount
                            bridge_proof_fee ))
              ; use_full_commitment = false
              ; authorization_kind = Signature
              ; increment_nonce = true
              ; preconditions =
                  { Account_update.Preconditions.accept with
                    account = Zkapp_precondition.Account.nonce nonce
                  }
              }
            ~authorization:(Control.Poly.Signature Signature.dummy)
        in
        let _forest, `Commitment tx_commitment =
          Bridge_prover.Withdrawal_request.precompute_commitments
            !sequencer.bridge_prover
            { withdrawal_params; transferrer = transferrer_update }
          |> Or_error.ok_exn
        in
        let transferrer_update =
          Account_update.with_no_aux ~body:transferrer_update.body
            ~authorization:
              (Control.Poly.Signature
                 (Signature_lib.Schnorr.Chunked.sign
                    ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
                    signer.private_key
                    (Random_oracle.Input.Chunked.field tx_commitment) ) )
        in
        Bridge_prover.(
          execute_request ~label:"SubmitWithdrawal" ~logger
            ~executor:l2_executor !sequencer.bridge_prover
            ( Withdrawal_request.f ~t:!sequencer.bridge_prover ~logger
                { withdrawal_params; transferrer = transferrer_update }
            |> Or_error.ok_exn ))
        >>| Or_error.ok_exn
      in

      let withdrawal ~amount ~(account : Keypair.t) :
          C.Bridge_state.Withdrawal_params_base.t =
        { children = []
        ; recipient = Public_key.compress account.public_key
        ; amount = Currency.Amount.of_mina_int_exn amount
        }
      in

      let account1 = List.nth_exn l1_accounts 0 in
      let account2 = List.nth_exn l1_accounts 1 in
      let account3 = List.nth_exn l1_accounts 2 in

      print_endline "(* Send 1-3 withdrawals *)" ;
      let withdrawals =
        run (fun () ->
            let withdrawal1 = withdrawal ~amount:5 ~account:account1 in
            let withdrawal2 = withdrawal ~amount:15 ~account:account2 in
            let withdrawal3 = withdrawal ~amount:25 ~account:account3 in

            let%bind _hash = submit_withdrawal account1 withdrawal1 in
            let%bind _hash = submit_withdrawal account2 withdrawal2 in
            let%bind _hash = submit_withdrawal account3 withdrawal3 in

            return
              [ (account1, withdrawal1)
              ; (account2, withdrawal2)
              ; (account3, withdrawal3)
              ] )
      in

      print_endline "(* Commit 1-3 withdrawals *)" ;
      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      print_endline "(* Send 4-6 withdrawals *)" ;
      let withdrawals =
        withdrawals
        @ run (fun () ->
              let withdrawal4 = withdrawal ~amount:35 ~account:account1 in
              let withdrawal5 = withdrawal ~amount:45 ~account:account2 in
              let withdrawal6 = withdrawal ~amount:55 ~account:account3 in

              let%bind _hash = submit_withdrawal account1 withdrawal4 in
              let%bind _hash = submit_withdrawal account2 withdrawal5 in
              let%bind _hash = submit_withdrawal account3 withdrawal6 in

              let%bind _created =
                Gql_client.For_tests.create_new_block ~logger gql_uri
              in
              return
                [ (account1, withdrawal4)
                ; (account2, withdrawal5)
                ; (account3, withdrawal6)
                ] )
      in

      print_endline "(* Commit 4-6 withdrawals *)" ;
      run (fun () ->
          let%bind commit_result = commit !sequencer >>| Or_error.ok_exn in
          let%bind _txn_snark = commit_result >>| Or_error.ok_exn in
          let%bind () =
            Executor.wait_to_finish !sequencer.merger_ctx.executor
          in
          let%bind _created =
            Gql_client.For_tests.create_new_block ~logger gql_uri
          in
          let%map { ledger_hash = committed_ledger_hash; _ } =
            Gql_client.infer_state ~logger gql_uri ~signer_pk
              ~zkapp_pk:(Public_key.compress outer_kp.public_key)
            >>| Or_error.ok_exn
            >>| Utils.value_of_zkapp_state
                  Zeko_circuits.Rollup_state.Outer_state.typ
          in
          let target_ledger_hash = get_root !sequencer in
          [%test_eq: Ledger_hash.t] committed_ledger_hash target_ledger_hash ) ;

      let finalize_withdrawal (signer : Keypair.t) withdrawal_params =
        let%bind l1_actions =
          Gql_client.fetch_actions ~logger gql_uri
            (Public_key.compress outer_kp.public_key)
          >>| Or_error.ok_exn
          >>| List.map ~f:(fun (fields, _, _, before, after) ->
                  ( Utils.actions_to_outer_action (List.hd_exn fields)
                  , before
                  , after ) )
        in
        let ( last_commit_index
            , (last_commit, `Before before_last_commit, `After after_last_commit)
            ) =
          List.rev l1_actions
          |> List.find_mapi ~f:(fun i (action, before, after) ->
                 match action with
                 | Commit commit ->
                     Some (i, (commit, before, after))
                 | Witness _ ->
                     None )
          |> Option.value_exn ~message:"Did not find any commit"
        in
        let last_commit_index =
          List.length l1_actions - last_commit_index - 1
        in
        let commit_ase =
          ( after_last_commit
          , List.sub l1_actions ~pos:(last_commit_index + 1)
              ~len:(List.length l1_actions - last_commit_index - 1)
            |> List.map ~f:(fun (action, _, _) ->
                   [ Utils.actions_of_outer_action action ]
                   |> Zkapp_account.Actions_impl.hash ) )
        in
        let before_withdrawal, withdrawal_ase =
          let actions =
            Archive.get_actions !sequencer.archive
              Zeko_constants.inner_account_id ~from:None
              ~to_:
                (Some
                   C.Rollup_state.Inner_action_state.(
                     With_length.state last_commit.inner_action_state |> raw) )
            |> Result.map_error ~f:Error.of_string
            |> Or_error.ok_exn
            |> List.map ~f:(fun { actions; action_state; _ } ->
                   let (action_state_one :: action_state_two :: _) =
                     action_state
                   in
                   ( Utils.actions_to_inner_action (List.hd_exn actions)
                   , `Before action_state_two
                   , `After action_state_one ) )
          in
          let hashed_withdrawal =
            Utils.value_to_hash ~init:Zeko_constants.withdrawal_salt
              C.Bridge_state.Withdrawal_params_base.typ withdrawal_params
          in
          let ( my_withdrawal_index
              , (_, `Before before_withdrawal, `After after_withdrawal) ) =
            List.findi actions ~f:(fun _ (action, _, _) ->
                Field.equal hashed_withdrawal action.aux )
            |> Option.value_exn ~message:"Did not find my withdrawal"
          in
          let elems =
            List.sub actions ~pos:(my_withdrawal_index + 1)
              ~len:(List.length actions - my_withdrawal_index - 1)
            |> List.map ~f:(fun (action, `Before _, `After _) ->
                   [ Utils.actions_of_inner_action_without_forest action ]
                   |> Zkapp_account.Actions_impl.hash )
          in
          ( C.Rollup_state.Inner_action_state.unsafe_value_of_field
              before_withdrawal
          , ( Ase.With_length.Stmt.
                { action_state = after_withdrawal
                ; length =
                    (let committed_length =
                       C.Zeko_util.Checked32.to_int
                         (C.Rollup_state.Inner_action_state.With_length.length
                            last_commit.inner_action_state )
                     in
                     C.Zeko_util.Checked32.of_int
                       (committed_length - List.length elems) )
                }
            , elems ) )
        in
        let helper_aid =
          Account_id.create
            (Public_key.compress signer.public_key)
            (Account_id.derive_token_id
               ~owner:
                 (Account_id.of_public_key
                    (Public_key.decompress_exn
                       Zeko_circuits_config.Inputs.helper_token_owner_l1 ) ) )
        in
        let%bind prev_next_withdrawal =
          match%map
            Gql_client.fetch_state_opt ~logger gql_uri helper_aid
            >>| Or_error.ok_exn
          with
          | Some (_next_cancelled_deposit :: next_withdrawal :: _) ->
              Some (UInt32.of_string (Field.to_string next_withdrawal))
          | None ->
              None
        in
        let%bind prev_nonce =
          match%map
            Gql_client.fetch_nonce_opt ~logger gql_uri helper_aid
            >>| Or_error.ok_exn
          with
          | Some nonce ->
              nonce
          | None ->
              UInt32.zero
        in
        let witness : Bridge_prover.Finalize_withdrawal.t_ =
          { public_key =
              List.hd_exn Zeko_circuits_config.Inputs.holder_accounts_l1
          ; commit = last_commit
          ; before_commit =
              C.Rollup_state.Outer_action_state.unsafe_value_of_field
                before_last_commit
          ; commit_ase_source = fst commit_ase
          ; commit_ase_elems = snd commit_ase
          ; before_withdrawal
          ; withdrawal_ase_source = fst withdrawal_ase
          ; withdrawal_ase_elems = snd withdrawal_ase
          ; prev_next_withdrawal =
              Option.value prev_next_withdrawal ~default:UInt32.zero
          ; withdrawal_params
          ; prev_nonce
          ; helper_account_new = Option.is_none prev_next_withdrawal
          }
        in
        let _forest, `Commitment commitment =
          Bridge_prover.Finalize_withdrawal.precompute_commitments
            !sequencer.bridge_prover witness
          |> Or_error.ok_exn
        in
        let helper_account_signature =
          Signature_lib.Schnorr.Chunked.sign
            ~signature_kind:Zeko_circuits_config.Inputs.chain_l1
            signer.private_key
            (Random_oracle.Input.Chunked.field commitment)
        in
        Bridge_prover.(
          execute_request ~label:"FinalizeWithdrawal" ~logger
            ~executor:l1_executor !sequencer.bridge_prover
            ( Finalize_withdrawal.f ~t:!sequencer.bridge_prover ~logger witness
                helper_account_signature
            |> Or_error.ok_exn ))
        >>| Or_error.ok_exn
      in

      print_endline "(* Finalize all withdrawals *)" ;
      run (fun () ->
          let%bind _shifted =
            Gql_client.For_tests.shift_slots ~logger gql_uri 200
          in
          (* Keep the sequencer's slot view in sync with the testing-ledger's
             shifted slot, so preverify_l1 simulates against the same slot the
             actual L1 transaction will see (otherwise time-based
             preconditions like [valid_while] on finalize_withdrawal trip). *)
          Utils.Slot.For_tests.add_to_global_slot :=
            Stdlib.( ! ) Utils.Slot.For_tests.add_to_global_slot + 200 ;
          let%bind () =
            Deferred.List.iteri withdrawals
              ~f:(fun i (signer, withdrawal_params) ->
                printf "(* Finalizing withdrawal %d *)\n%!" i ;
                let%bind hash = finalize_withdrawal signer withdrawal_params in
                let%bind _created =
                  Gql_client.For_tests.create_new_block ~logger gql_uri
                in
                let%map status =
                  Gql_client.For_tests.get_zkapp_command_status ~logger gql_uri
                    hash
                in
                [%test_eq: string list list option] status None )
          in
          return () ) ;

      let[@warning "-26"] sequencer = free_sequencer sequencer in

      print_endline "(* Drop database *)" ;
      run (fun () ->
          Relational_db.For_tests.drop_database ~port:5433 "sequencer" ) )

let () =
  Core.printf "Sequencer tests took %s\n%!"
    (Time.Span.to_string (Time.diff (Time.now ()) start_time))
