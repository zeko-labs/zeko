open Core_kernel
open Async
open Mina_base
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

let time label (d : 'a Deferred.t) =
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  printf "%s: %s\n%!" label (Time.Span.to_string_hum @@ Time.diff stop start) ;
  return x

let skip_transfers_before (transfers : Zkapps_rollup.TR.t list)
    (pointer : Field.t) =
  match
    List.fold_right transfers
      ~init:(`Hashing Zkapp_account.Actions.empty_state_element)
      ~f:(fun transfer -> function
      | `Hashing hash ->
          if Field.equal hash pointer then `Accumulating [ transfer ]
          else
            `Hashing
              (Zkapp_account.Actions.push_events hash
                 (Zkapps_rollup.TR.to_actions transfer) )
      | `Accumulating transfers ->
          `Accumulating (transfer :: transfers) )
  with
  | `Hashing _ ->
      []
  | `Accumulating transfers_after_pointer ->
      transfers_after_pointer

(* Finds the account_id's account update and returns the 0th state update *)
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
  let get_ledger_hash_and_location field_to_option state =
    Zkapp_state.V.to_list state
    |> function
    | ledger_hash :: _all_withdrawals :: location :: _ ->
        Some
          ( field_to_option ledger_hash |> Option.value ~default:Field.zero
          , field_to_option location
            |> Option.value ~default:Field.zero
            |> Field.to_string )
    | _ ->
        None
  in
  let%bind.Option source =
    body |> Account_update.Body.preconditions
    |> Account_update.Preconditions.account |> Zkapp_precondition.Account.state
    |> get_ledger_hash_and_location Zkapp_basic.Or_ignore.to_option
  in
  let%bind.Option target =
    body |> Account_update.Body.update |> Account_update.Update.app_state
    |> get_ledger_hash_and_location Zkapp_basic.Set_or_keep.to_option
  in
  Some (source, target)
