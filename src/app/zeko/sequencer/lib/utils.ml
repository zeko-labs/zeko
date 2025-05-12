open Core_kernel
open Mina_base
open Signature_lib
open Snark_params.Tick
open Async

let retry ?(max_attempts = 5) ?(delay = Time.Span.of_sec 1.) ~f () =
  let rec go attempt =
    match%bind f () with
    | Ok x ->
        return (Ok x)
    | Error _ when attempt < max_attempts ->
        let%bind () = after delay in
        go (attempt + 1)
    | Error e ->
        return (Error e)
  in
  go 0

let time (d : 'a Deferred.t) =
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  return (x, Time.diff stop start)

let log_time ~logger label (d : 'a Deferred.t) =
  let%bind x, t = time d in
  [%log info] "%s took: %s" label (Time.Span.to_string_hum t) ;
  return x

let value_to_zkapp_state (some : Field.t -> 'option) (none : 'option)
    (typ : ('var, 'value) Typ.t) (x : 'value) : 'option Zkapp_state.V.t =
  let (Typ typ) = typ in
  let fields, _aux = typ.value_to_fields x in
  assert (Array.length fields <= 8) ;
  let missing = 8 - Array.length fields in
  Zkapp_state.V.of_list_exn
  @@ List.append
       (List.map ~f:(fun f -> some f) @@ Array.to_list fields)
       (List.init missing ~f:(fun _ -> none))

let value_of_zkapp_state (typ : ('var, 'value) Typ.t) (x : field Zkapp_state.V.t)
    : 'value =
  let (Typ typ) = typ in
  typ.value_of_fields
    ( Zkapp_state.V.to_list x |> Array.of_list
    , typ.constraint_system_auxiliary () )

let update_state pk command state =
  let open Zkapp_basic in
  let account_id = Account_id.create pk Token_id.default in
  match
    Zkapp_command.account_updates command
    |> Zkapp_command.Call_forest.to_list
    |> List.find ~f:(fun account_update ->
           Account_update.account_id account_update
           |> Account_id.equal account_id )
  with
  | None ->
      `Skipped
  | Some account_update -> (
      let body = Account_update.body account_update in
      let preconditions =
        body |> Account_update.Body.preconditions
        |> Account_update.Preconditions.account
        |> Zkapp_precondition.Account.state
      in
      let update =
        body |> Account_update.Body.update |> Account_update.Update.app_state
      in
      match
        List.map3_exn (Zkapp_state.V.to_list state)
          (Zkapp_state.V.to_list preconditions) (Zkapp_state.V.to_list update)
          ~f:(fun s p u -> (s, p, u))
        |> List.fold_map ~init:`Updated ~f:(function
             | `Precondition_failed ->
                 fun (s, _, _) -> (`Precondition_failed, s)
             | `Updated -> (
                 fun (s, p, u) ->
                   let u = Set_or_keep.to_option u |> Option.value ~default:s in
                   match Or_ignore.to_option p with
                   | None ->
                       (`Updated, u)
                   | Some p ->
                       if Field.equal s p then (`Updated, u)
                       else (`Precondition_failed, u) ) )
      with
      | `Precondition_failed, _ ->
          `Precondition_failed
      | `Updated, new_state ->
          `Updated (Zkapp_state.V.of_list_exn new_state) )

let get_synced_outer_action_state_exn l =
  let open Zeko_circuits in
  let ({ outer_action_state } : Rollup_state.Inner_state.t) =
    let idx =
      Mina_ledger.Ledger.index_of_account_exn l Zeko_constants.inner_account_id
    in
    let inner_acc = Mina_ledger.Ledger.get_at_index_exn l idx in
    (Option.value_exn inner_acc.zkapp).app_state
    |> Rollup_state.Inner_state.value_of_app_state
  in
  outer_action_state

let sign_zkapp_command ?signature_kind (command : Zkapp_command.t)
    (signers : Keypair.t list) : Zkapp_command.t =
  let full_commitment =
    Zkapp_command.Transaction_commitment.create_complete
      (Zkapp_command.commitment command)
      ~memo_hash:(Signed_command_memo.hash command.memo)
      ~fee_payer_hash:
        (Zkapp_command.Digest.Account_update.create ?chain:signature_kind
           (Account_update.of_fee_payer command.fee_payer) )
  in
  let sign_raw (pk : Public_key.Compressed.t) msg =
    match
      List.find signers ~f:(fun kp ->
          Public_key.Compressed.equal (Public_key.compress kp.public_key) pk )
    with
    | Some kp ->
        Signature_lib.Schnorr.Chunked.sign ?signature_kind kp.private_key
          (Random_oracle.Input.Chunked.field msg)
    | None ->
        failwithf "key not found: %s\n"
          (Public_key.Compressed.to_base58_check pk)
          ()
  in
  let rec sign_tree
      (tree :
        ( Account_update.t
        , Zkapp_command.Digest.Account_update.t
        , Zkapp_command.Digest.Forest.t )
        Zkapp_command.Call_forest.Tree.t ) =
    { tree with
      account_update =
        { tree.account_update with
          authorization =
            ( match tree.account_update.body.authorization_kind with
            | Signature ->
                assert tree.account_update.body.use_full_commitment ;
                Signature
                  (sign_raw tree.account_update.body.public_key full_commitment)
            | _ ->
                tree.account_update.authorization )
        }
    ; calls = sign_forest tree.calls
    }
  and sign_forest forest =
    List.map ~f:(fun tree -> { tree with elt = sign_tree tree.elt }) forest
  in
  { command with
    fee_payer =
      { command.fee_payer with
        authorization =
          ( if
            Public_key.Compressed.(
              equal empty command.fee_payer.body.public_key)
          then command.fee_payer.authorization
          else sign_raw command.fee_payer.body.public_key full_commitment )
      }
  ; account_updates = sign_forest command.account_updates
  }

let rehash_forest ~chain =
  Zkapp_command.Call_forest.accumulate_hashes
    ~hash_account_update:
      (Zkapp_command.Call_forest.Digest.Account_update.create ~chain)

let signature_kind = function
  | "mainnet" ->
      Mina_signature_kind.Mainnet
  | "testnet" ->
      Mina_signature_kind.Testnet
  | network_id ->
      Mina_signature_kind.Other_network network_id
