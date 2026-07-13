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
  let%bind outer_state, inner_ase_source, emergency_mode =
    let%map outer_account =
      Gql_client.infer_state ~logger l1_uri ~zkapp_pk
        ~signer_pk:(Signer_service.Signer.public_key executor.signer)
    in
    let outer_state : Rollup_state.Outer_state.t =
      Utils.value_of_zkapp_state Rollup_state.Outer_state.typ outer_account
    in
    let ({ Rollup_state.Outer_state.inner_action_state = committed_inner_action_state
         ; status_flags
         ; _
         } :
          Rollup_state.Outer_state.t ) =
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
        ~slot_range ~emergency_mode
    in
    let%map settlement_export =
      Ethereum_settlement_export.create
        ~signature_kind:executor.signature_kind ~body ~calls ~state_before:outer_state
        ~proof
        ~inner_action_batch:
          (Ethereum_settlement_export.inner_action_batch_json ~archive
             new_inner_action_records )
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
          Executor.send_zkapp_command ~logger ~settlement_export executor command
        in
        recommit_next target_ledger_hash
  in
  recommit_next ledger_hash
