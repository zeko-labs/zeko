open Core_kernel
open Async_kernel
open Mina_base
open Zeko_circuits
module Field = Snark_params.Tick.Field

type t =
  { vk_json : string
  ; proof_json : string
  ; public_input_skeleton_json : string
  ; app_statement_json : string
  ; outer_account_public_key : string
  ; binding : Yojson.Safe.t
  ; inner_action_batch : Yojson.Safe.t option
  ; asset_registry_batch : Yojson.Safe.t option
  }

let proof_json (t : t) : Yojson.Safe.t =
  let fields =
    [ ("vkJson", `String t.vk_json)
    ; ("proofJson", `String t.proof_json)
    ; ("publicInputSkeletonJson", `String t.public_input_skeleton_json)
    ; ("appStatementJson", `String t.app_statement_json)
    ; ("binding", t.binding)
    ]
  in
  let fields =
    Option.value_map t.inner_action_batch ~default:fields ~f:(fun batch ->
        ("innerActionBatch", batch) :: fields )
  in
  let fields =
    Option.value_map t.asset_registry_batch ~default:fields ~f:(fun batch ->
        ("assetRegistryBatch", batch) :: fields )
  in
  `Assoc fields

let to_gateway_json t (command : Zkapp_command.Stable.Latest.t) =
  let command_base64 = Zkapp_command.to_base64 command in
  let fee_payer = command.fee_payer.body in
  `Assoc
    [ ("schemaVersion", `Int 1)
    ; ( "minaTransactionHash"
      , `String ("0x" ^ Blake2.(digest_string command_base64 |> to_hex)) )
    ; ("outerAccountPublicKey", `String t.outer_account_public_key)
    ; ( "feePayerPublicKey"
      , `String
          (Signature_lib.Public_key.Compressed.to_base58_check
             fee_payer.public_key ) )
    ; ("nonce", `Int (Unsigned.UInt32.to_int fee_payer.nonce))
    ; ("commandBase64", `String command_base64)
    ; ("proof", proof_json t)
    ]

let maybe_write_gateway_fixture t (command : Zkapp_command.Stable.Latest.t) =
  match Sys.getenv_opt "ZEKO_ETHEREUM_SETTLEMENT_FIXTURE_DIR" with
  | None ->
      ()
  | Some directory ->
      if not (Caml.Sys.file_exists directory && Caml.Sys.is_directory directory)
      then
        failwithf
          "ZEKO_ETHEREUM_SETTLEMENT_FIXTURE_DIR is not an existing directory: \
           %s"
          directory () ;
      let command_base64 = Zkapp_command.to_base64 command in
      let transaction_hash = Blake2.(digest_string command_base64 |> to_hex) in
      let nonce = Unsigned.UInt32.to_int command.fee_payer.body.nonce in
      let filename =
        sprintf "settlement-%010d-%s.json" nonce transaction_hash
      in
      to_gateway_json t command |> Yojson.Safe.pretty_to_string
      |> fun data ->
      Out_channel.write_all
        (Filename.concat directory filename)
        ~data:(data ^ "\n")

let field_to_hex field =
  Kimchi_backend.Pasta.Basic.Bigint256.to_hex_string
    (Kimchi_backend.Pasta.Basic.Fp.to_bigint field)

let fields_json fields =
  fields |> Array.to_list
  |> List.map ~f:(fun field -> `String (field_to_hex field))
  |> fun fields -> `List fields

let padded_hex_digits field =
  let digits =
    field_to_hex field |> String.chop_prefix_if_exists ~prefix:"0x"
  in
  String.make (64 - String.length digits) '0' ^ digits

let field_suffix_hex field length =
  let digits = padded_hex_digits field in
  String.suffix digits length

let asset_id_hex high low =
  "0x" ^ field_suffix_hex high 32 ^ field_suffix_hex low 32

let ethereum_address_hex field = "0x" ^ field_suffix_hex field 40

let packed_public_key_hex ({ x; is_odd } : Signature_lib.Public_key.Compressed.t)
    =
  let digits = padded_hex_digits x in
  let first =
    match String.get digits 0 with
    | '0' .. '9' as digit ->
        Char.to_int digit - Char.to_int '0'
    | 'a' .. 'f' as digit ->
        10 + Char.to_int digit - Char.to_int 'a'
    | _ ->
        failwith "invalid field hex digit"
  in
  if first >= 8 then
    failwith "Mina compressed public-key x-coordinate already uses parity bit" ;
  let first = if is_odd then first + 8 else first in
  let packed = Bytes.of_string digits in
  Bytes.set packed 0
    (Char.lowercase
       (Char.of_int_exn (if first < 10 then 48 + first else 87 + first)) ) ;
  "0x" ^ Bytes.to_string packed

let asset_record_of_event fields =
  let (Typ typ) = Asset_registry.Asset_record.typ in
  typ.value_of_fields (fields, typ.constraint_system_auxiliary ())

let canonical_asset_record_json
    ({ schema_version
     ; registry_index
     ; asset_id_high
     ; asset_id_low
     ; ethereum_token_address
     ; token_owner_l2
     ; token_id_l2
     ; decimals
     ; inventory_cap
     ; mft_standard_vk_id
     ; vault_public_key
     ; universal_bridge_vk_id
     } :
      Asset_registry.Asset_record.t ) =
  `Assoc
    [ ("schemaVersion", `Int (Zeko_util.Checked32.to_int schema_version))
    ; ("registryIndex", `Int (Zeko_util.Checked32.to_int registry_index))
    ; ("assetId", `String (asset_id_hex asset_id_high asset_id_low))
    ; ("ethereumToken", `String (ethereum_address_hex ethereum_token_address))
    ; ("tokenOwnerL2", `String (packed_public_key_hex token_owner_l2))
    ; ( "tokenIdL2"
      , `String (field_to_hex (Token_id.to_field_unsafe token_id_l2)) )
    ; ("decimals", `Int (Zeko_util.Checked32.to_int decimals))
    ; ( "inventoryCap"
      , `Intlit
          (Currency.Amount.to_uint64 inventory_cap |> Unsigned.UInt64.to_string)
      )
    ; ("mftStandardVkId", `String (field_to_hex mft_standard_vk_id))
    ; ("vaultPublicKey", `String (packed_public_key_hex vault_public_key))
    ; ("universalBridgeVkId", `String (field_to_hex universal_bridge_vk_id))
    ]

let registry_records_from_archive ~(archive : Archive.t) =
  let account_id =
    Account_id.create
      Zeko_circuits_config.Inputs.Ethereum_assets.registry_public_key
      Token_id.default
  in
  Archive.get_events archive account_id
  |> List.concat_map ~f:(fun { Archive.Account_update_events.events; _ } ->
         events )
  |> List.map ~f:asset_record_of_event

let int_of_field label field =
  Option.try_with (fun () -> Field.to_string field |> Int.of_string)
  |> Option.value_exn
       ~message:(sprintf "%s does not fit an OCaml integer" label)

let asset_registry_batch_json ~(archive : Archive.t) ~old_root ~old_count
    ~old_schema ~new_root ~new_count ~new_schema =
  let old_count = int_of_field "old asset registry count" old_count in
  let new_count = int_of_field "new asset registry count" new_count in
  let old_schema = int_of_field "old asset registry schema" old_schema in
  let new_schema = int_of_field "new asset registry schema" new_schema in
  if Int.equal old_count new_count then None
  else if new_count < old_count then
    failwith "Ethereum asset registry count regressed"
  else if
    not
      ( Int.equal old_schema new_schema
      && Int.equal new_schema
           Zeko_constants.Ethereum_asset_registry.schema_version )
  then failwith "Ethereum asset registry schema drifted"
  else
    let records = registry_records_from_archive ~archive in
    if List.length records < new_count then
      failwithf
        "Ethereum asset registry archive contains %d records, expected at \
         least %d"
        (List.length records) new_count () ;
    let records = List.take records new_count in
    List.iteri records ~f:(fun index record ->
        if
          not
            (Int.equal
               (Zeko_util.Checked32.to_int record.registry_index)
               index )
        then
          failwithf "Ethereum asset registry archive is not dense at index %d"
            index () ) ;
    let tree = ref (Asset_registry.Merkle_list.empty ()) in
    List.take records old_count
    |> List.iter ~f:(fun record ->
           tree := Asset_registry.Merkle_list.append_exn !tree record ) ;
    if not (Field.equal (Asset_registry.Merkle_list.root !tree) old_root) then
      failwith "Ethereum asset registry archive does not match old root" ;
    let appends =
      List.drop records old_count
      |> List.map ~f:(fun record ->
             let index = Zeko_util.Checked32.to_int record.registry_index in
             let append_path = Asset_registry.Merkle_list.path !tree ~index in
             tree := Asset_registry.Merkle_list.append_exn !tree record ;
             `Assoc
               [ ("record", canonical_asset_record_json record)
               ; ("appendPath", fields_json (Array.of_list append_path))
               ] )
    in
    if not (Field.equal (Asset_registry.Merkle_list.root !tree) new_root) then
      failwith "Ethereum asset registry archive does not match new root" ;
    Some
      (`Assoc
        [ ( "registryPublicKey"
          , `String
              (packed_public_key_hex
                 Zeko_circuits_config.Inputs.Ethereum_assets.registry_public_key )
          )
        ; ("checkpointVersion", `Int Asset_registry.Checkpoint.version)
        ; ("root", `String (field_to_hex new_root))
        ; ("count", `Int new_count)
        ; ("schemaVersion", `Int new_schema)
        ; ("oldRoot", `String (field_to_hex old_root))
        ; ("oldCount", `Int old_count)
        ; ("appends", `List appends)
        ] )

let ethereum_address_of_compressed
    ( ({ Signature_lib.Public_key.Compressed.Poly.x; _ } :
        Signature_lib.Public_key.Compressed.t ) as recipient ) =
  match Bridge_state.Ethereum_address.validate recipient with
  | Error _ ->
      None
  | Ok () ->
      let hex = field_to_hex x |> String.chop_prefix_if_exists ~prefix:"0x" in
      let hex = String.make (64 - String.length hex) '0' ^ hex in
      Some ("0x" ^ String.suffix hex 40)

let configured_ethereum_bridge_address () =
  match Zeko_circuits_config.t.ethereum_holder_account_l1 with
  | None ->
      if
        Option.is_some (Sys.getenv_opt "ZEKO_ETHEREUM_GATEWAY_TOKEN")
        || Option.is_some
             (Sys.getenv_opt "ZEKO_ETHEREUM_SETTLEMENT_FIXTURE_DIR")
      then
        failwith
          "Ethereum settlement export requires ethereum_holder_account_l1"
      else "0x0000000000000000000000000000000000000000"
  | Some holder -> (
      match ethereum_address_of_compressed holder with
      | Some address ->
          address
      | None ->
          failwith
            "ethereum_holder_account_l1 is not an even 160-bit Ethereum address"
      )

let ethereum_withdrawal_preimage_json
    ({ recipient; amount; asset } : Archive.Ethereum_withdrawal.t) =
  let recipient =
    ethereum_address_of_compressed recipient
    |> Option.value_exn
         ~message:
           "Ethereum settlement withdrawal recipient is not an even 160-bit \
            address"
  in
  match asset with
  | None ->
      ( "withdrawal"
      , `Assoc
          [ ("recipient", `String recipient)
          ; ( "amount"
            , `Intlit
                (Currency.Amount.to_uint64 amount |> Unsigned.UInt64.to_string)
            )
          ] )
  | Some { token; asset_id; params_fields } ->
      let encoding_version, registry_index, record_commitment =
        match params_fields with
        | encoding_version :: registry_index :: record_commitment :: _ ->
            ( Int.of_string (Field.to_string encoding_version)
            , Int.of_string (Field.to_string registry_index)
            , "0x" ^ padded_hex_digits record_commitment )
        | _ ->
            failwith
              "Ethereum ERC20 withdrawal parameter preimage is truncated"
      in
      ( "tokenWithdrawal"
      , `Assoc
          [ ("encodingVersion", `Int encoding_version)
          ; ("registryIndex", `Int registry_index)
          ; ("recordCommitment", `String record_commitment)
          ; ("token", `String token)
          ; ("assetId", `String asset_id)
          ; ("recipient", `String recipient)
          ; ( "amount"
            , `Intlit
                (Currency.Amount.to_uint64 amount |> Unsigned.UInt64.to_string)
            )
          ; ("paramsFields", fields_json (Array.of_list params_fields))
          ] )

let inner_action_batch_json ~(archive : Archive.t)
    (records : Archive.Account_update_actions.t list) =
  let actions =
    List.map records ~f:(fun record ->
        let fields = List.to_array record.actions in
        if Array.length fields <> 1 then
          failwith "Ethereum settlement requires one event per inner action" ;
        let fields = fields.(0) in
        if Array.length fields <> 3 then
          failwith "Ethereum settlement requires three-field inner actions" ;
        let withdrawal =
          Archive.find_ethereum_withdrawal archive ~aux:fields.(1)
          |> Option.value_exn
               ~message:
                 (sprintf
                    "Ethereum settlement archive has no withdrawal preimage \
                     for action auxiliary %s"
                    (field_to_hex fields.(1)) )
          |> ethereum_withdrawal_preimage_json
        in
        let fields = [ ("fields", fields_json fields) ] in
        `Assoc (withdrawal :: fields) )
  in
  `Assoc
    [ ("bridgeAddress", `String (configured_ethereum_bridge_address ()))
    ; ("actions", `List actions)
    ]

let signature_kind_json = function
  | Mina_signature_kind.Mainnet ->
      Ok (`String "mainnet")
  | Testnet ->
      Ok (`String "testnet")
  | Other_network _ ->
      Or_error.error_string
        "the Ethereum settlement PoC supports mainnet/testnet Mina hash domains"

let account_update_body_input_json (body : Account_update.Body.t) =
  let { Random_oracle_input.Chunked.field_elements; packeds } =
    Account_update.Body.to_input body
  in
  let packed =
    packeds |> Array.to_list
    |> List.map ~f:(fun (value, bits) ->
           `Assoc
             [ ("value", `String (field_to_hex value)); ("bits", `Int bits) ] )
  in
  `Assoc
    [ ("fieldElements", fields_json field_elements); ("packed", `List packed) ]

let rec call_forest_json
    (calls :
      ( Account_update.Stable.V1.t
      , Zkapp_command.Digest.Account_update.t
      , Zkapp_command.Digest.Forest.t )
      Zkapp_command.Call_forest.t ) =
  `List
    (List.map calls ~f:(fun tree ->
         let { Zkapp_command.Call_forest.Tree.account_update; calls; _ } =
           With_stack_hash.elt tree
         in
         `Assoc
           [ ( "accountUpdateBody"
             , account_update_body_input_json account_update.body )
           ; ("calls", call_forest_json calls)
           ] ) )

let binding_json ~signature_kind ~(body : Account_update.Body.t) ~calls
    ~state_before =
  let open Or_error.Let_syntax in
  let%map signature_kind = signature_kind_json signature_kind in
  let state_fields =
    Utils.value_to_fields Rollup_state.Outer_state.typ state_before
  in
  `Assoc
    [ ("minaSignatureKind", signature_kind)
    ; ("accountUpdateBody", account_update_body_input_json body)
    ; ("actions", `List (List.map body.actions ~f:fields_json))
    ; ("stateBefore", `Assoc [ ("fields", fields_json state_fields) ])
    ; ("callForest", call_forest_json calls)
    ]

let app_statement_json ~signature_kind ~body ~calls =
  let statement : Zkapp_statement.t =
    { account_update =
        ( Account_update.Body.digest ~signature_kind body
          :> Zkapp_command.Transaction_commitment.t )
    ; calls =
        ( Zkapp_command.Call_forest.hash calls
          :> Zkapp_command.Transaction_commitment.t )
    }
  in
  Zkapp_statement.to_field_elements statement
  |> Array.to_list
  |> List.map ~f:(fun field -> `String (field_to_hex field))
  |> fun fields -> Yojson.Safe.to_string (`List fields)

let verification_key_json
    (verification_key : Pickles.Side_loaded.Verification_key.t) =
  Pickles.Side_loaded.Verification_key.to_yojson_full verification_key
  |> Result.map ~f:Yojson.Safe.to_string

let proof_wire_json (proof : Pickles.Side_loaded.Proof.t) =
  let side_loaded_proof = Pickles.Side_loaded.Proof.to_yojson_full proof in
  let wrap_proof, public_input_skeleton =
    match side_loaded_proof with
    | `Assoc fields ->
        let wrap_proof =
          List.find_map fields ~f:(fun (name, json) ->
              if String.equal name "proof" then Some json else None )
          |> Option.value_exn
        in
        let public_input_skeleton =
          `Assoc
            (List.filter fields ~f:(fun (name, _) ->
                 not (String.equal name "proof") ) )
        in
        (wrap_proof, public_input_skeleton)
    | _ ->
        failwith "side-loaded proof JSON must be an object"
  in
  let proof_wire =
    `Assoc
      [ ("schemaVersion", `Int 1)
      ; ("proof", wrap_proof)
      ; ("prevChallenges", Pickles.Side_loaded.Proof.accumulator_to_yojson proof)
      ]
  in
  (Yojson.Safe.to_string proof_wire, Yojson.Safe.to_string public_input_skeleton)

let create_with_verification_key ?inner_action_batch ?asset_registry_batch
    ~signature_kind ~(body : Account_update.Body.t) ~calls ~state_before
    ~(proof : Compile_simple.Proof.t)
    ~(verification_key : Compile_simple.Verification_key.t) =
  let open Deferred.Or_error.Let_syntax in
  let%bind proof =
    Compile_simple.Proof.to_pickles proof
    |> Result.of_option ~error:(Error.of_string "cannot export a fake proof")
    |> Deferred.return
  in
  let%bind verification_key =
    Compile_simple.Verification_key.to_pickles verification_key
    |> Result.of_option
         ~error:(Error.of_string "cannot export a fake verification key")
    |> Deferred.return
  in
  let proof_json, public_input_skeleton_json = proof_wire_json proof in
  let%map vk_json = verification_key_json verification_key |> Deferred.return
  and binding =
    binding_json ~signature_kind ~body ~calls ~state_before |> Deferred.return
  in
  { vk_json
  ; proof_json
  ; public_input_skeleton_json
  ; app_statement_json = app_statement_json ~signature_kind ~body ~calls
  ; outer_account_public_key =
      Signature_lib.Public_key.Compressed.to_base58_check body.public_key
  ; binding
  ; inner_action_batch
  ; asset_registry_batch
  }

let create ?inner_action_batch ?asset_registry_batch ~signature_kind
    ~(body : Account_update.Body.t) ~calls ~state_before
    ~(proof : Compile_simple.Proof.t) =
  let open Deferred.Or_error.Let_syntax in
  let%bind verification_key =
    Compile_simple.Verification_key.of_tag
      (Lazy.force Zeko_types.Outer_rules_inst.tag)
    |> Promise.to_deferred |> Deferred.ok
  in
  create_with_verification_key ~signature_kind ~body ~calls ~state_before
    ?inner_action_batch ?asset_registry_batch ~proof ~verification_key
