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
  `Assoc
    ( match t.inner_action_batch with
    | Some batch ->
        ("innerActionBatch", batch) :: fields
    | None ->
        fields )

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

let ethereum_address_of_compressed
    ({ Signature_lib.Public_key.Compressed.Poly.x; is_odd } :
      Signature_lib.Public_key.Compressed.t ) =
  if is_odd then None
  else
    let hex = field_to_hex x |> String.chop_prefix_if_exists ~prefix:"0x" in
    let hex = String.make (64 - String.length hex) '0' ^ hex in
    let high = String.prefix hex 24 in
    if String.for_all high ~f:(Char.equal '0') then
      Some ("0x" ^ String.suffix hex 40)
    else None

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
  ethereum_address_of_compressed recipient
  |> Option.map ~f:(fun recipient ->
         match asset with
         | None ->
             ( "withdrawal"
             , `Assoc
                 [ ("recipient", `String recipient)
                 ; ( "amount"
                   , `Intlit
                       ( Currency.Amount.to_uint64 amount
                       |> Unsigned.UInt64.to_string ) )
                 ] )
         | Some { token; asset_id; params_fields } ->
             ( "tokenWithdrawal"
             , `Assoc
                 [ ("token", `String token)
                 ; ("assetId", `String asset_id)
                 ; ("recipient", `String recipient)
                 ; ( "amount"
                   , `Intlit
                       ( Currency.Amount.to_uint64 amount
                       |> Unsigned.UInt64.to_string ) )
                 ; ("paramsFields", fields_json (Array.of_list params_fields))
                 ] ) )

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
          |> Option.bind ~f:ethereum_withdrawal_preimage_json
        in
        let fields = [ ("fields", fields_json fields) ] in
        `Assoc
          ( match withdrawal with
          | Some withdrawal ->
              withdrawal :: fields
          | None ->
              fields ) )
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

let binding_json ~signature_kind ~(body : Account_update.Body.t) ~state_before =
  let open Or_error.Let_syntax in
  let%map signature_kind = signature_kind_json signature_kind in
  let { Random_oracle_input.Chunked.field_elements; packeds } =
    Account_update.Body.to_input body
  in
  let packed =
    packeds |> Array.to_list
    |> List.map ~f:(fun (value, bits) ->
           `Assoc
             [ ("value", `String (field_to_hex value)); ("bits", `Int bits) ] )
  in
  let state_fields =
    Utils.value_to_fields Rollup_state.Outer_state.typ state_before
  in
  `Assoc
    [ ("minaSignatureKind", signature_kind)
    ; ( "accountUpdateBody"
      , `Assoc
          [ ("fieldElements", fields_json field_elements)
          ; ("packed", `List packed)
          ] )
    ; ("actions", `List (List.map body.actions ~f:fields_json))
    ; ("stateBefore", `Assoc [ ("fields", fields_json state_fields) ])
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

let create_with_verification_key ?inner_action_batch ~signature_kind
    ~(body : Account_update.Body.t) ~calls ~state_before
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
    binding_json ~signature_kind ~body ~state_before |> Deferred.return
  in
  { vk_json
  ; proof_json
  ; public_input_skeleton_json
  ; app_statement_json = app_statement_json ~signature_kind ~body ~calls
  ; outer_account_public_key =
      Signature_lib.Public_key.Compressed.to_base58_check body.public_key
  ; binding
  ; inner_action_batch
  }

let create ?inner_action_batch ~signature_kind ~(body : Account_update.Body.t)
    ~calls ~state_before ~(proof : Compile_simple.Proof.t) =
  let open Deferred.Or_error.Let_syntax in
  let%bind verification_key =
    Compile_simple.Verification_key.of_tag
      (Lazy.force Zeko_types.Outer_rules_inst.tag)
    |> Promise.to_deferred |> Deferred.ok
  in
  create_with_verification_key ~signature_kind ~body ~calls ~state_before
    ?inner_action_batch ~proof ~verification_key
