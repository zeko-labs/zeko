open Core_kernel
open Async
open Mina_base
open Signature_lib
module Field = Snark_params.Tick.Field

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

let print_time label (d : 'a Deferred.t) =
  let%bind x, t = time d in
  printf "%s: %s\n%!" label (Time.Span.to_string_hum t) ;
  return x

let get_state_transition pk command =
  let account_id = Account_id.create pk Token_id.default in
  let%bind.Option account_update =
    Zkapp_command.account_updates command
    |> Zkapp_command.Call_forest.to_list
    |> List.find ~f:(fun account_update ->
           Account_update.account_id account_update
           |> Account_id.equal account_id )
  in
  let body = Account_update.body account_update in
  (* Use the Rollup_state.Outer_state.t to determine which is ledger hash *)
  let third l = List.nth_exn l 2 in
  let source =
    body |> Account_update.Body.preconditions
    |> Account_update.Preconditions.account |> Zkapp_precondition.Account.state
    |> Zkapp_state.V.to_list |> third |> Zkapp_basic.Or_ignore.to_option
    |> Option.value ~default:Field.zero
  in
  let target =
    body |> Account_update.Body.update |> Account_update.Update.app_state
    |> Zkapp_state.V.to_list |> third |> Zkapp_basic.Set_or_keep.to_option
    |> Option.value ~default:Field.zero
  in
  Some (source, target)

let get_inner_deposits_state_exn l =
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
