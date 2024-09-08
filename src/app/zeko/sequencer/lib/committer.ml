open Core_kernel
open Async
open Mina_base
open Signature_lib
open Mina_ledger
module Field = Snark_params.Tick.Field

module Commit_witness = struct
  type t =
    { old_inner_ledger : Sparse_ledger.t
    ; new_inner_ledger : Sparse_ledger.t
    ; old_deposits_pointer : Frozen_ledger_hash.t
    ; processed_deposits_pointer : Frozen_ledger_hash.t
    ; signatures : Signature.t list
    ; last_snark : Zkapps_rollup.t
    }
  [@@deriving yojson]
end

module Store = struct
  let ok_exn x =
    let open Ppx_deriving_yojson_runtime.Result in
    match x with Ok x -> x | Error e -> failwith e

  type commit_id = Frozen_ledger_hash.t * Frozen_ledger_hash.t
  [@@deriving yojson]

  type index = commit_id list [@@deriving yojson]

  module Kvdb = struct
    module Key_value = struct
      type _ t =
        | Commit : (commit_id * Commit_witness.t) t
        | Commit_index : (unit * index) t

      let serialize_key : type k v. (k * v) t -> k -> Bigstring.t =
       fun pair_type key ->
        match pair_type with
        | Commit ->
            let hash1, hash2 = key in
            Bigstring.(
              concat
                [ of_string "commit"
                ; of_string
                    ( Frozen_ledger_hash.to_base58_check hash1
                    ^ "-"
                    ^ Frozen_ledger_hash.to_base58_check hash2 )
                ])
        | Commit_index ->
            Bigstring.of_string "commit_index"

      let serialize_value : type k v. (k * v) t -> v -> Bigstring.t =
       fun pair_type value ->
        match pair_type with
        | Commit ->
            Bigstring.of_string @@ Yojson.Safe.to_string
            @@ Commit_witness.to_yojson value
        | Commit_index ->
            Bigstring.of_string @@ Yojson.Safe.to_string
            @@ index_to_yojson value

      let deserialize_value : type k v. (k * v) t -> Bigstring.t -> v =
       fun pair_type data ->
        match pair_type with
        | Commit ->
            ok_exn @@ Commit_witness.of_yojson @@ Yojson.Safe.from_string
            @@ Bigstring.to_string data
        | Commit_index ->
            ok_exn @@ index_of_yojson @@ Yojson.Safe.from_string
            @@ Bigstring.to_string data
    end

    include Kvdb_base.Make (Key_value)
  end

  let store_commit kvdb witness ~source ~target =
    (* Update index *)
    let index =
      Kvdb.get kvdb Commit_index ~key:() |> Option.value ~default:[]
    in
    let index = (source, target) :: index in
    Kvdb.set kvdb Commit_index ~key:() ~data:index ;

    (* Store commit *)
    let commit_id = (source, target) in
    Kvdb.set kvdb Commit ~key:commit_id ~data:witness

  let load_commit_exn kvdb commit_id =
    Option.value_exn @@ Kvdb.get kvdb Commit ~key:commit_id

  let get_index kvdb =
    Kvdb.get kvdb Commit_index ~key:() |> Option.value ~default:[]

  let get_commit kvdb ~source ~target =
    let index = get_index kvdb in
    let%bind.Option commit_id =
      List.find index ~f:(fun (s, t) ->
          Frozen_ledger_hash.equal s source && Frozen_ledger_hash.equal t target )
    in
    Some (load_commit_exn kvdb commit_id)

  let get_all kvdb =
    let index = get_index kvdb in
    let commits = List.map index ~f:(load_commit_exn kvdb) in
    commits
end

let prove_commit (module M : Zkapps_rollup.S) ~(executor : Executor.t) ~zkapp_pk
    ~archive_uri
    ({ old_inner_ledger
     ; new_inner_ledger
     ; old_deposits_pointer
     ; processed_deposits_pointer
     ; signatures
     ; last_snark
     } :
      Commit_witness.t ) =
  (* FIXME: pass this check into circuit *)
  assert (List.length signatures <> 0) ;
  let%bind new_deposits =
    Gql_client.fetch_transfers archive_uri
      ~from_action_state:old_deposits_pointer
      ~end_action_state:processed_deposits_pointer zkapp_pk
    |> Deferred.map ~f:(List.map ~f:fst)
  in
  let%bind unprocessed_deposits =
    Gql_client.fetch_transfers archive_uri
      ~from_action_state:processed_deposits_pointer zkapp_pk
    |> Deferred.map ~f:(List.map ~f:fst)
  in
  let%bind account_update =
    M.Outer.step last_snark ~outer_public_key:zkapp_pk ~new_deposits
      ~unprocessed_deposits ~old_inner_ledger ~new_inner_ledger
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

let recommit_all (module M : Zkapps_rollup.S) ~(executor : Executor.t) ~db
    ~zkapp_pk ~archive_uri =
  let kvdb = Ledger.Db.zeko_kvdb db in
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
        let witness = Store.load_commit_exn kvdb (source, target) in
        let%bind command =
          prove_commit (module M) ~executor ~zkapp_pk ~archive_uri witness
        in
        let%bind () = Executor.send_zkapp_command executor command in
        recommit_next target
  in
  recommit_next current_state
