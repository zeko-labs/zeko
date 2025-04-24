open Core_kernel
open Async
open Mina_base
open Signature_lib
open Mina_ledger
open Zeko_types
module Field = Snark_params.Tick.Field

module Commit_witness = struct
  type t =
    { old_inner_ledger : Sparse_ledger.t
    ; new_inner_ledger : Sparse_ledger.t
    ; old_deposits_pointer : Frozen_ledger_hash.t
    ; processed_deposits_pointer : Frozen_ledger_hash.t
    ; signatures : (Public_key.Compressed.t * Signature.t) list
    ; txn_snark : Txn_snark.serializable
    }
  [@@deriving yojson]
end

module Store = struct
  let ok_exn x =
    let open Ppx_deriving_yojson_runtime.Result in
    match x with Ok x -> x | Error e -> failwith e

  type commit_id = Frozen_ledger_hash.t * Frozen_ledger_hash.t
  [@@deriving yojson, equal]

  module Kvdb = Kvdb_base.Make_table (struct
    type key = commit_id [@@deriving yojson, equal]

    type value = Commit_witness.t [@@deriving yojson]

    let key = "commit"
  end)

  let store_commit kvdb witness ~source ~target =
    Kvdb.set kvdb ~key:(source, target) ~data:witness

  let get_index kvdb = Kvdb.get_keys kvdb

  let get_commit kvdb commit_id = Kvdb.get kvdb ~key:commit_id

  let get_all kvdb = Kvdb.get_all kvdb
end

let prove_commit ~provers ~(executor : Executor.t) ~zkapp_pk ~archive_uri
    ({ old_inner_ledger
     ; new_inner_ledger
     ; old_deposits_pointer
     ; processed_deposits_pointer
     ; signatures
     ; txn_snark
     } :
      Commit_witness.t ) =
  let%bind new_actions =
    Gql_client.fetch_actions archive_uri ~from_action_state:old_deposits_pointer
      ~end_action_state:processed_deposits_pointer zkapp_pk
    >>| List.map ~f:fst >>| List.rev
    >>| List.map ~f:Account_update.Actions.hash
  in
  let%bind unprocessed_actions =
    Gql_client.fetch_actions archive_uri
      ~from_action_state:processed_deposits_pointer zkapp_pk
    >>| List.map ~f:fst >>| List.rev
    >>| List.map ~f:Account_update.Actions.hash
  in
  let%bind account_update =
    let da_key, da_signature = List.hd_exn signatures in
    Zeko_prover.Client.outer_commit ~proving_timeout:30. provers ~txn_snark
      ~public_key:zkapp_pk ~new_actions ~unprocessed_actions ~old_inner_ledger
      ~new_inner_ledger ~da_signature
      ~da_key:(Even_PC.create_exn da_key)
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
    ; account_updates = Zkapp_command.Call_forest.cons_tree account_update []
    ; memo = Signed_command_memo.empty
    }
  in
  return command

let recommit_all ~provers ~(executor : Executor.t) ~kvdb ~zkapp_pk ~archive_uri
    =
  let%bind current_state =
    Gql_client.infer_committed_state executor.l1_uri ~zkapp_pk
      ~signer_pk:(Public_key.compress executor.signer.public_key)
  in
  let commits = Store.get_index kvdb in
  let rec recommit_next current_state =
    match
      List.find commits ~f:(fun (source, _) ->
          Frozen_ledger_hash.equal source current_state )
    with
    | None ->
        return ()
    | Some (source, target) ->
        printf "Recommitting %s -> %s\n%!"
          (Frozen_ledger_hash.to_base58_check source)
          (Frozen_ledger_hash.to_base58_check target) ;
        let witness =
          Store.get_commit kvdb (source, target) |> Option.value_exn
        in
        let%bind command =
          prove_commit ~provers ~executor ~zkapp_pk ~archive_uri witness
        in
        let%bind () = Executor.send_zkapp_command executor command in
        recommit_next target
  in
  recommit_next current_state
