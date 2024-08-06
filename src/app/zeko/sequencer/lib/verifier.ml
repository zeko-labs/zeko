open Core_kernel
open Async
open Mina_base

type invalid =
  [ `Invalid_keys of Signature_lib.Public_key.Compressed.Stable.Latest.t list
  | `Invalid_signature of
    Signature_lib.Public_key.Compressed.Stable.Latest.t list
  | `Invalid_proof of (Error.t[@to_yojson Error_json.error_to_yojson])
  | `Missing_verification_key of
    Signature_lib.Public_key.Compressed.Stable.Latest.t list
  | `Unexpected_verification_key of
    Signature_lib.Public_key.Compressed.Stable.Latest.t list
  | `Mismatched_authorization_kind of
    Signature_lib.Public_key.Compressed.Stable.Latest.t list ]
[@@deriving to_yojson]

let invalid_to_error (invalid : invalid) : Error.t =
  let keys_to_string keys =
    List.map keys ~f:(fun key ->
        Signature_lib.Public_key.Compressed.to_base58_check key )
    |> String.concat ~sep:";"
  in
  match invalid with
  | `Invalid_keys keys ->
      Error.createf "Invalid_keys: [%s]" (keys_to_string keys)
  | `Invalid_signature keys ->
      Error.createf "Invalid_signature: [%s]" (keys_to_string keys)
  | `Missing_verification_key keys ->
      Error.createf "Missing_verification_key: [%s]" (keys_to_string keys)
  | `Unexpected_verification_key keys ->
      Error.createf "Unexpected_verification_key: [%s]" (keys_to_string keys)
  | `Mismatched_authorization_kind keys ->
      Error.createf "Mismatched_authorization_kind: [%s]" (keys_to_string keys)
  | `Invalid_proof err ->
      Error.tag ~tag:"Invalid_proof" err

let check :
       User_command.Verifiable.t With_status.t
    -> [ `Valid of User_command.Valid.t
       | `Valid_assuming of User_command.Valid.t * _ list
       | invalid ] = function
  | { With_status.data = User_command.Signed_command c; status = _ } -> (
      if not (Signed_command.check_valid_keys c) then
        `Invalid_keys (Signed_command.public_keys c)
      else
        match Signed_command.check_only_for_signature c with
        | Some c ->
            `Valid (User_command.Signed_command c)
        | None ->
            `Invalid_signature (Signed_command.public_keys c) )
  | { With_status.data =
        Zkapp_command
          ({ fee_payer; account_updates; memo } as zkapp_command_with_vk)
    ; status
    } ->
      with_return (fun { return } ->
          let account_updates_hash =
            Zkapp_command.Call_forest.hash account_updates
          in
          let tx_commitment =
            Zkapp_command.Transaction_commitment.create ~account_updates_hash
          in
          let full_tx_commitment =
            Zkapp_command.Transaction_commitment.create_complete tx_commitment
              ~memo_hash:(Signed_command_memo.hash memo)
              ~fee_payer_hash:
                (Zkapp_command.Digest.Account_update.create
                   (Account_update.of_fee_payer fee_payer) )
          in
          let check_signature s pk msg =
            match Signature_lib.Public_key.decompress pk with
            | None ->
                return (`Invalid_keys [ pk ])
            | Some pk ->
                if
                  not
                    (Signature_lib.Schnorr.Chunked.verify s
                       (Backend.Tick.Inner_curve.of_affine pk)
                       (Random_oracle_input.Chunked.field msg) )
                then
                  return
                    (`Invalid_signature [ Signature_lib.Public_key.compress pk ])
                else ()
          in
          check_signature fee_payer.authorization fee_payer.body.public_key
            full_tx_commitment ;
          let zkapp_command_with_hashes_list =
            account_updates |> Zkapp_statement.zkapp_statements_of_forest'
            |> Zkapp_command.Call_forest.With_hashes_and_data
               .to_zkapp_command_with_hashes_list
          in
          let valid_assuming =
            List.filter_map zkapp_command_with_hashes_list
              ~f:(fun ((p, (vk_opt, stmt)), _at_account_update) ->
                let commitment =
                  if p.body.use_full_commitment then full_tx_commitment
                  else tx_commitment
                in
                match (p.authorization, p.body.authorization_kind) with
                | Signature s, Signature ->
                    check_signature s p.body.public_key commitment ;
                    None
                | None_given, None_given ->
                    None
                | Proof pi, Proof vk_hash -> (
                    match status with
                    | Applied -> (
                        match vk_opt with
                        | None ->
                            return
                              (`Missing_verification_key
                                [ Account_id.public_key
                                  @@ Account_update.account_id p
                                ] )
                        | Some (vk : _ With_hash.t) ->
                            if
                              (* check that vk expected for proof is the one being used *)
                              Snark_params.Tick.Field.equal vk_hash
                                (With_hash.hash vk)
                            then Some (vk.data, stmt, pi)
                            else
                              return
                                (`Unexpected_verification_key
                                  [ Account_id.public_key
                                    @@ Account_update.account_id p
                                  ] ) )
                    | Failed _ ->
                        (* Don't verify the proof if it has failed. *)
                        None )
                | _ ->
                    return
                      (`Mismatched_authorization_kind
                        [ Account_id.public_key @@ Account_update.account_id p ]
                        ) )
          in
          let v : User_command.Valid.t =
            (* Verification keys should be present if it reaches here *)
            let zkapp_command =
              Zkapp_command.Valid.of_verifiable zkapp_command_with_vk
            in
            User_command.Poly.Zkapp_command zkapp_command
          in
          match valid_assuming with
          | [] ->
              `Valid v
          | _ :: _ ->
              `Valid_assuming (v, valid_assuming) )

let verify_command (command : User_command.Verifiable.t With_status.t) :
    [ `Valid of Mina_base.User_command.Valid.t
    | `Valid_assuming of
      ( Pickles.Side_loaded.Verification_key.t
      * Mina_base.Zkapp_statement.t
      * Pickles.Side_loaded.Proof.t )
      list
    | invalid ]
    Deferred.Or_error.t =
  let checked_command = check command in
  let to_verify =
    match checked_command with
    | `Valid _ ->
        []
    | `Valid_assuming (_, xs) ->
        xs
    | `Invalid_keys _
    | `Invalid_signature _
    | `Invalid_proof _
    | `Missing_verification_key _
    | `Unexpected_verification_key _
    | `Mismatched_authorization_kind _ ->
        []
  in
  let%map all_verified =
    Pickles.Side_loaded.verify ~typ:Zkapp_statement.typ to_verify
  in
  Ok
    ( match checked_command with
    | `Valid c ->
        `Valid c
    | `Valid_assuming (c, xs) ->
        if Or_error.is_ok all_verified then `Valid c else `Valid_assuming xs
    | `Invalid_keys keys ->
        `Invalid_keys keys
    | `Invalid_signature keys ->
        `Invalid_signature keys
    | `Invalid_proof err ->
        `Invalid_proof err
    | `Missing_verification_key keys ->
        `Missing_verification_key keys
    | `Unexpected_verification_key keys ->
        `Unexpected_verification_key keys
    | `Mismatched_authorization_kind keys ->
        `Mismatched_authorization_kind keys )
