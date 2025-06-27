open Core_kernel
open Async
open Mina_base
open Signature_lib
open Mina_ledger
open Zeko_types
open Zeko_circuits
open Relational_db
module Field = Snark_params.Tick.Field

module Commit_witness = struct
  type t =
    { old_inner_ledger : Sparse_ledger.t
    ; new_inner_ledger : Sparse_ledger.t
    ; processed_actions_pointer : Field.t
    ; signature : Public_key.Compressed.t * Signature.t
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
    ~(archive : Archive.t) ~zkapp_pk ~archive_uri
    ({ old_inner_ledger
     ; new_inner_ledger
     ; processed_actions_pointer
     ; signature
     ; txn_snark
     } :
      Commit_witness.t ) =
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
  let%bind inner_ase_source =
    let%map { inner_action_state = committed_inner_action_state; _ } =
      Gql_client.infer_state
        Executor.(executor.l1_uri)
        ~zkapp_pk
        ~signer_pk:(Public_key.compress executor.signer.public_key)
      >>| Utils.value_of_zkapp_state Rollup_state.Outer_state.typ
    in
    ( Rollup_state.Inner_action_state.With_length.
        { action_state = raw committed_inner_action_state
        ; length = length committed_inner_action_state
        }
      : Ase.With_length.Stmt.t )
  in
  (* TODO: check if it needs to be reversed *)
  let new_inner_actions =
    let from =
      match (Option.value_exn old_inner_acc.zkapp).action_state with
      | x :: _ ->
          x
    in
    let to_ =
      match (Option.value_exn new_inner_acc.zkapp).action_state with
      | x :: _ ->
          x
    in
    Archive.get_actions archive
      (Account_id.of_public_key @@ Public_key.decompress_exn zkapp_pk)
      ~from:(Some from) ~to_:(Some to_)
    |> Result.map_error ~f:Error.of_string
    |> Or_error.ok_exn
    (* Drop the first action if it's not the initial state *)
    |> ( if Stdlib.(from = Zkapp_account.Actions.empty_state_element) then
         List.tl
       else Option.some )
    |> Option.value ~default:[]
    |> List.map ~f:(fun x -> Zkapp_account.Actions_impl.hash x.actions)
  in
  let%bind unprocessed_actions =
    Gql_client.fetch_actions archive_uri
      ~from_action_state:processed_actions_pointer zkapp_pk
    >>| List.map ~f:fst
    >>| List.map ~f:Zkapp_account.Actions_impl.hash
  in
  let unprocessed_actions_state =
    List.fold unprocessed_actions ~init:processed_actions_pointer
      ~f:(fun acc elem -> Zkapp_account.Actions_impl.push_hash acc elem)
  in
  [%log info] "Skipping %d actions to %s"
    (List.length unprocessed_actions)
    (Field.to_string unprocessed_actions_state) ;
  let%bind tree =
    let da_key, da_signature = signature in
    let%map (body, account_update_digest, calls), proof =
      Zeko_prover.Client.outer_commit ~proving_timeout:30. provers ~txn_snark
        ~public_key:zkapp_pk ~inner_ase_source ~new_inner_actions ~old_inner_acc
        ~old_inner_acc_path ~new_inner_acc ~new_inner_acc_path
        ~unprocessed_actions ~da_signature
        ~da_key:(Even_PC.create_exn da_key)
    in
    (* see #286 *)
    match Is_compile_simple_real.is_compile_simple_real with
    | Some eq ->
        let proof_eq, _ = Type_equal.detuple2 eq in
        let account_update : Account_update.t =
          Account_update.with_aux ~body
            ~authorization:
              (Control.Poly.Proof
                 (Proof_cache_tag.write_proof_to_disk proof_cache_db
                    (Type_equal.conv proof_eq proof) ) )
        in
        Zkapp_command.Call_forest.Tree.
          { account_update
          ; account_update_digest
          ; calls =
              Zkapp_command.Call_forest.With_hashes.write_all_proofs_to_disk
                ~proof_cache_db calls
          }
    | None ->
        let account_update : Account_update.t =
          Account_update.with_aux
            ~body:{ body with authorization_kind = None_given }
            ~authorization:Control.Poly.None_given
        in
        Zkapp_command.Call_forest.Tree.
          { account_update
          ; account_update_digest =
              Zkapp_command.Digest.Account_update.create
                ~signature_kind:executor.signature_kind account_update
          ; calls =
              Zkapp_command.Call_forest.With_hashes.write_all_proofs_to_disk
                ~proof_cache_db calls
          }
  in
  let command : Zkapp_command.t =
    { fee_payer =
        { Account_update.Fee_payer.body =
            { public_key = Public_key.compress executor.signer.public_key
            ; fee = Currency.Fee.of_mina_int_exn 1
            ; valid_until = None
            ; nonce = Unsigned.UInt32.zero
            }
        ; authorization = Signature.dummy
        }
    ; account_updates = Zkapp_command.Call_forest.cons_tree tree []
    ; memo = Signed_command_memo.empty
    }
  in
  return command

let recommit_all ~logger ~proof_cache_db ~db_pool ~provers
    ~(executor : Executor.t) ~archive ~zkapp_pk ~archive_uri =
  let%bind { ledger_hash; _ } =
    Gql_client.infer_state executor.l1_uri ~zkapp_pk
      ~signer_pk:(Public_key.compress executor.signer.public_key)
    >>| Utils.value_of_zkapp_state Rollup_state.Outer_state.typ
  in
  let rec recommit_next ~conn current_state =
    match%bind
      Commit_table.get_by_source conn current_state
      >>| caqti_ok_exn ~msg:"Failed to get commit by source: %s"
    with
    | None ->
        return ()
    | Some { source_ledger_hash; target_ledger_hash; witness } ->
        [%log info] "Recommitting %s -> %s"
          (Frozen_ledger_hash.to_base58_check source_ledger_hash)
          (Frozen_ledger_hash.to_base58_check target_ledger_hash) ;
        let%bind command =
          prove_commit ~logger ~proof_cache_db ~provers ~executor ~archive
            ~zkapp_pk ~archive_uri witness
        in
        let%bind () = Executor.send_zkapp_command ~logger executor command in
        recommit_next ~conn target_ledger_hash
  in
  Pool.use
    (fun conn -> recommit_next ~conn ledger_hash >>| Result.return)
    db_pool
  >>| caqti_ok_exn ~msg:"Failed to recommit all: %s"
