open Core_kernel
open Async
open Mina_base
open Mina_ledger
open Zeko_circuits
open Zeko_types
open Relational_db
module Field = Snark_params.Tick.Field

module Commit_witness = struct
  type t =
    { old_inner_ledger : Sparse_ledger.t
    ; new_inner_ledger : Sparse_ledger.t
    ; processed_actions_pointer : Field.t
    ; da_multisig : Multisig.Witness.t
    ; txn_snark : Txn_snark.serializable
    }
  [@@deriving yojson]
end

module Commit_table = struct
  type t =
    { source_ledger_hash : Ledger_hash.t
    ; target_ledger_hash : Ledger_hash.t
    ; witness : Commit_witness.t
    }
  [@@deriving hlist, fields]

  let make ~source_ledger_hash ~target_ledger_hash ~witness =
    { source_ledger_hash; target_ledger_hash; witness }

  let typ =
    Mina_caqti.Type_spec.custom_type
      ~to_hlist:(fun { source_ledger_hash; target_ledger_hash; witness } ->
        H_list.
          [ Ledger_hash.to_decimal_string source_ledger_hash
          ; Ledger_hash.to_decimal_string target_ledger_hash
          ; Commit_witness.to_yojson witness |> Yojson.Safe.to_string
          ] )
      ~of_hlist:(fun H_list.[ source_ledger_hash; target_ledger_hash; witness ] ->
        let ok_exn = function
          | Ppx_deriving_yojson_runtime.Result.Ok x ->
              x
          | Ppx_deriving_yojson_runtime.Result.Error e ->
              failwithf "Error parsing ledger openings: %s" e ()
        in
        { source_ledger_hash = Ledger_hash.of_decimal_string source_ledger_hash
        ; target_ledger_hash = Ledger_hash.of_decimal_string target_ledger_hash
        ; witness =
            Commit_witness.of_yojson (Yojson.Safe.from_string witness) |> ok_exn
        } )
      Caqti_type.[ string; string; octets ]

  let insert (module Conn : CONNECTION) t =
    Conn.exec
      (Caqti_request.exec typ
         {sql| INSERT INTO "commit" (source_ledger_hash, target_ledger_hash, witness)
                VALUES (?, ?, ?) |sql} )
      t

  let get_by_source (module Conn : CONNECTION) ledger_hash =
    Conn.find_opt
      (Caqti_request.find_opt Caqti_type.string typ
         {sql| SELECT source_ledger_hash, target_ledger_hash, witness FROM "commit" WHERE source_ledger_hash = ? |sql} )
      (Ledger_hash.to_decimal_string ledger_hash)
end

let prove_commit ~logger ~proof_cache_db ~provers ~(executor : Executor.t)
    ~l1_uri ~(archive : Archive.t) ~zkapp_pk ~archive_uri ~l1_config
    ~commit_validity_period ~commit_fee
    ({ old_inner_ledger
     ; new_inner_ledger
     ; processed_actions_pointer
     ; da_multisig
     ; txn_snark
     } :
      Commit_witness.t ) =
  let open Deferred.Result.Let_syntax in
  let get_inner_acc ledger =
    let inner_acc =
      Sparse_ledger.get_exn ledger Zeko_constants.inner_account_index
    in
    let inner_acc_path =
      Sparse_ledger.path_exn ledger Zeko_constants.inner_account_index
      |> List.map ~f:(function
           | `Left hash ->
               ( { right_side = hash }
                 : Outer_rules_inst.Rule_commit_inst.PathElt.t )
           | `Right _ ->
               failwith "The inner account is supposed to be left most" )
    in
    (inner_acc, inner_acc_path)
  in
  let old_inner_acc, old_inner_acc_path = get_inner_acc old_inner_ledger in
  let new_inner_acc, new_inner_acc_path = get_inner_acc new_inner_ledger in
  let ethereum_asset_registry_account ledger =
    if Zeko_circuits_config.Inputs.Ethereum_assets.enabled then
      let account_id =
        Account_id.create
          Zeko_circuits_config.Inputs.Ethereum_assets.registry_public_key
          Token_id.default
      in
      let index = Sparse_ledger.find_index_exn ledger account_id in
      let account = Sparse_ledger.get_exn ledger index in
      let path =
        Sparse_ledger.path_exn ledger index
        |> List.map ~f:(function
             | `Left hash ->
                 ( { hash_other = hash; is_right = false }
                   : Outer_rules_inst.Rule_commit_inst.Registry_path.Step.t )
             | `Right hash ->
                 { hash_other = hash; is_right = true } )
      in
      let state =
        (Option.value_exn account.zkapp).app_state
        |> Asset_registry.Registry_state.value_of_app_state
      in
      (account, path, state)
    else
      let path =
        List.init Zeko_constants.constraint_constants.ledger_depth
          ~f:(fun _ : Outer_rules_inst.Rule_commit_inst.Registry_path.Step.t ->
            { hash_other = Field.zero; is_right = false } )
      in
      ( Account.empty
      , path
      , { Asset_registry.Registry_state.root = Field.zero
        ; leaf_count = Zeko_util.Checked32.zero
        ; schema_version = Zeko_util.Checked32.zero
        } )
  in
  let ( old_ethereum_asset_registry_acc
      , old_ethereum_asset_registry_path
      , old_ethereum_asset_registry_state ) =
    ethereum_asset_registry_account old_inner_ledger
  in
  let ( new_ethereum_asset_registry_acc
      , new_ethereum_asset_registry_path
      , ethereum_asset_registry_state ) =
    ethereum_asset_registry_account new_inner_ledger
  in
  let ethereum_asset_registration =
    let module Registration =
      Outer_rules_inst.Rule_commit_inst.Registration_witness
    in
    let empty_path () =
      List.init Zeko_constants.constraint_constants.ledger_depth
        ~f:(fun _ : Outer_rules_inst.Rule_commit_inst.Registry_path.Step.t ->
          { hash_other = Field.zero; is_right = false } )
    in
    let dummy_candidate : Asset_registry.Asset_record.t =
      { schema_version = Zeko_util.Checked32.zero
      ; registry_index = Zeko_util.Checked32.zero
      ; asset_id_high = Field.zero
      ; asset_id_low = Field.zero
      ; ethereum_token_address = Field.zero
      ; token_owner_l2 = Signature_lib.Public_key.Compressed.empty
      ; token_id_l2 = Token_id.default
      ; decimals = Zeko_util.Checked32.zero
      ; inventory_cap = Currency.Amount.zero
      ; mft_standard_vk_id = Field.zero
      ; vault_public_key = Signature_lib.Public_key.Compressed.empty
      ; universal_bridge_vk_id = Field.zero
      }
    in
    let dummy () : Registration.t =
      { did_append = false
      ; candidate = dummy_candidate
      ; append_path =
          List.init Zeko_constants.Ethereum_asset_registry.depth ~f:(fun _ ->
              Field.zero )
      ; token_owner_acc = Account.empty
      ; token_owner_path = empty_path ()
      ; admin_acc = Account.empty
      ; admin_path = empty_path ()
      ; vault_acc = Account.empty
      ; vault_path = empty_path ()
      ; circulation_acc = Account.empty
      ; circulation_path = empty_path ()
      }
    in
    if not Zeko_circuits_config.Inputs.Ethereum_assets.enabled then dummy ()
    else
      let old_count =
        Zeko_util.Checked32.to_int old_ethereum_asset_registry_state.leaf_count
      in
      let new_count =
        Zeko_util.Checked32.to_int ethereum_asset_registry_state.leaf_count
      in
      match new_count - old_count with
      | 0 ->
          dummy ()
      | 1 ->
          let candidate =
            Ethereum_settlement_export.registry_records_from_archive ~archive
            |> Fn.flip List.nth_exn old_count
          in
          let account_opening account_id =
            let index =
              Sparse_ledger.find_index_exn new_inner_ledger account_id
            in
            let account = Sparse_ledger.get_exn new_inner_ledger index in
            let path =
              Sparse_ledger.path_exn new_inner_ledger index
              |> List.map ~f:(function
                   | `Left hash ->
                       ( { hash_other = hash; is_right = false }
                         : Outer_rules_inst.Rule_commit_inst.Registry_path.Step
                           .t )
                   | `Right hash ->
                       { hash_other = hash; is_right = true } )
            in
            (account, path)
          in
          let owner_id =
            Account_id.create candidate.token_owner_l2 Token_id.default
          in
          let token_owner_acc, token_owner_path = account_opening owner_id in
          let owner_state =
            (Option.value_exn token_owner_acc.zkapp).app_state
            |> Zkapp_state.V.to_list
          in
          let admin_public_key =
            let (Typ typ) = Signature_lib.Public_key.Compressed.typ in
            typ.value_of_fields
              ( [| List.nth_exn owner_state 1; List.nth_exn owner_state 2 |]
              , typ.constraint_system_auxiliary () )
          in
          let admin_acc, admin_path =
            Account_id.create admin_public_key Token_id.default
            |> account_opening
          in
          let vault_acc, vault_path =
            Account_id.create candidate.vault_public_key candidate.token_id_l2
            |> account_opening
          in
          let circulation_acc, circulation_path =
            Account_id.create candidate.token_owner_l2 candidate.token_id_l2
            |> account_opening
          in
          { did_append = true
          ; candidate
          ; append_path =
              (let records =
                 Ethereum_settlement_export.registry_records_from_archive
                   ~archive
               in
               let tree =
                 List.take records old_count
                 |> List.fold
                      ~init:(Asset_registry.Merkle_list.empty ())
                      ~f:Asset_registry.Merkle_list.append_exn
               in
               Asset_registry.Merkle_list.path tree ~index:old_count )
          ; token_owner_acc
          ; token_owner_path
          ; admin_acc
          ; admin_path
          ; vault_acc
          ; vault_path
          ; circulation_acc
          ; circulation_path
          }
      | delta ->
          failwithf
            "PoC settlement supports at most one Ethereum asset registration \
             per commit, observed count delta %d"
            delta ()
  in
  let%bind outer_state, inner_ase_source, emergency_mode =
    let%map outer_account =
      Gql_client.infer_state ~logger l1_uri ~zkapp_pk
        ~signer_pk:(Signer_service.Signer.public_key executor.signer)
    in
    let outer_state : Rollup_state.Outer_state.t =
      Utils.value_of_zkapp_state Rollup_state.Outer_state.typ outer_account
    in
    let ({ Rollup_state.Outer_state.inner_action_state =
             committed_inner_action_state
         ; status_flags
         ; _
         }
          : Rollup_state.Outer_state.t ) =
      outer_state
    in
    let emergency_mode =
      Rollup_state.Outer_state.Status_flags.emergency status_flags
    in
    ( outer_state
    , ( Rollup_state.Inner_action_state.With_length.
          { action_state = raw committed_inner_action_state
          ; length = length committed_inner_action_state
          }
        : Ase.With_length.Stmt.t )
    , emergency_mode )
  in
  let%bind new_inner_action_records =
    let from =
      match (Option.value_exn old_inner_acc.zkapp).action_state with
      | x :: _ ->
          x
    in
    (* Sanity check *)
    let%bind () =
      if Field.equal from (Ase.With_length.Stmt.state inner_ase_source) then
        return ()
      else
        Deferred.return
          (Error
             (Error.of_string
                "old_inner_acc.action_state and \
                 outer_acc.committed_inner_action_state do not match" ) )
    in
    let to_ =
      match (Option.value_exn new_inner_acc.zkapp).action_state with
      | x :: _ ->
          x
    in
    Archive.get_actions archive Zeko_constants.inner_account_id
      ~from:(Some from) ~to_:(Some to_)
    |> Result.map_error ~f:Error.of_string
    |> Result.map ~f:(fun actions ->
           actions
           |> ( if Stdlib.(from = Zkapp_account.Actions.empty_state_element) then
                Option.some
              else List.tl )
           |> Option.value ~default:[] )
    |> Deferred.return
  in
  let new_inner_actions =
    List.map new_inner_action_records ~f:(fun x ->
        Zkapp_account.Actions_impl.hash x.actions )
  in
  let%bind unprocessed_actions =
    Gql_client.fetch_actions ~logger archive_uri
      ~from_action_state:processed_actions_pointer zkapp_pk
    >>| List.map ~f:(fun (fields, _, _, _, _) -> fields)
    >>| List.map ~f:Zkapp_account.Actions_impl.hash
  in
  let unprocessed_actions_state =
    List.fold unprocessed_actions ~init:processed_actions_pointer
      ~f:(fun acc elem -> Zkapp_account.Actions_impl.push_hash acc elem)
  in
  [%log info] "Skipping %d actions from %s to %s"
    (List.length unprocessed_actions)
    (Field.to_string processed_actions_pointer)
    (Field.to_string unprocessed_actions_state) ;
  let%bind forest, settlement_export =
    let slot_range : Slot_range.t =
      let current_slot = Utils.Slot.global_slot ~l1_config in
      let slot_range : Slot_range.t =
        { lower = current_slot
        ; upper =
            Mina_numbers.(
              Global_slot_since_genesis.add current_slot commit_validity_period)
        }
      in
      { lower = current_slot
      ; upper =
          Mina_numbers.Global_slot_since_genesis.min
            (fst txn_snark).slot_range.upper slot_range.upper
      }
    in
    let%bind (body, _, calls), proof =
      Zeko_prover.Client.outer_commit provers ~txn_snark ~public_key:zkapp_pk
        ~inner_ase_source ~new_inner_actions ~old_inner_acc ~old_inner_acc_path
        ~new_inner_acc ~new_inner_acc_path ~unprocessed_actions ~da_multisig
        ~slot_range ~emergency_mode ~old_ethereum_asset_registry_acc
        ~old_ethereum_asset_registry_path ~new_ethereum_asset_registry_acc
        ~new_ethereum_asset_registry_path ~ethereum_asset_registration
    in
    let%map settlement_export =
      match Is_compile_simple_real.is_compile_simple_real with
      | None ->
          Deferred.Or_error.return None
      | Some _
        when Option.is_none
               Zeko_circuits_config.Inputs.ethereum_holder_account_l1 ->
          Deferred.Or_error.return None
      | Some _ ->
          Ethereum_settlement_export.create
            ~signature_kind:executor.signature_kind ~body ~calls
            ~state_before:outer_state ~proof
            ~inner_action_batch:
              (Ethereum_settlement_export.inner_action_batch_json ~archive
                 new_inner_action_records )
            ?asset_registry_batch:
              ( if Zeko_circuits_config.Inputs.Ethereum_assets.enabled then
                Ethereum_settlement_export.asset_registry_batch_json ~archive
                  ~old_root:old_ethereum_asset_registry_state.root
                  ~old_count:
                    (Zeko_util.Checked32.to_field
                       old_ethereum_asset_registry_state.leaf_count )
                  ~old_schema:
                    (Zeko_util.Checked32.to_field
                       old_ethereum_asset_registry_state.schema_version )
                  ~new_root:ethereum_asset_registry_state.root
                  ~new_count:
                    (Zeko_util.Checked32.to_field
                       ethereum_asset_registry_state.leaf_count )
                  ~new_schema:
                    (Zeko_util.Checked32.to_field
                       ethereum_asset_registry_state.schema_version )
              else None )
          >>| Option.some
    in
    (* see #286 *)
    ( Utils.attach_proof_to_forest ~signature_kind:executor.signature_kind
        ~proof_cache_db ~body ~calls ~proof
    , settlement_export )
  in
  let command : Zkapp_command.t =
    { fee_payer =
        { Account_update.Fee_payer.body =
            { public_key = Signer_service.Signer.public_key executor.signer
            ; fee = commit_fee
            ; valid_until = None
            ; nonce = Unsigned.UInt32.zero
            }
        ; authorization = Signature.dummy
        }
    ; account_updates =
        Zkapp_command.Call_forest.map forest
          ~f:(Account_update.write_all_proofs_to_disk ~proof_cache_db)
    ; memo = Signed_command_memo.empty
    }
  in
  return (command, settlement_export)

let recommit_all ~logger ~proof_cache_db ~db_pool ~provers
    ~(executor : Executor.t) ~l1_uri ~archive ~zkapp_pk ~archive_uri ~l1_config
    ~commit_validity_period ~commit_fee =
  let open Deferred.Result.Let_syntax in
  let%bind { ledger_hash; _ } =
    Gql_client.infer_state ~logger l1_uri ~zkapp_pk
      ~signer_pk:(Signer_service.Signer.public_key executor.signer)
    >>| Utils.value_of_zkapp_state Rollup_state.Outer_state.typ
  in
  let rec recommit_next current_state =
    match%bind
      Pool.use
        (fun conn -> Commit_table.get_by_source conn current_state)
        db_pool
      |> Deferred.map ~f:caqti_to_err
    with
    | None ->
        return ()
    | Some { source_ledger_hash; target_ledger_hash; witness } ->
        [%log info] "Recommitting %s -> %s"
          (Frozen_ledger_hash.to_base58_check source_ledger_hash)
          (Frozen_ledger_hash.to_base58_check target_ledger_hash) ;
        let () =
          let l1_global_slot = Utils.Slot.global_slot ~l1_config in
          let ({ upper; _ } : Slot_range.t) =
            (fst witness.txn_snark).slot_range
          in
          if Zeko_util.Slot.(upper <= l1_global_slot) then
            failwithf "Failed to recommit, upper slot is in the past: %s"
              (Zeko_util.Slot.to_string upper)
              ()
        in
        let%bind command, settlement_export =
          prove_commit ~logger ~proof_cache_db ~provers ~executor ~l1_uri
            ~archive ~zkapp_pk ~archive_uri ~l1_config ~commit_validity_period
            ~commit_fee witness
        in
        let%bind _hash =
          Executor.send_zkapp_command ~logger ?settlement_export executor
            command
        in
        recommit_next target_ledger_hash
  in
  recommit_next ledger_hash
