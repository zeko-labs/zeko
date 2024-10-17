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

let time (d : 'a Deferred.t) =
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  return (x, Time.diff stop start)

let print_time label (d : 'a Deferred.t) =
  let%bind x, t = time d in
  printf "%s: %s\n%!" label (Time.Span.to_string_hum t) ;
  return x

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
  let zeroth l = List.nth_exn l 0 in
  let source =
    body |> Account_update.Body.preconditions
    |> Account_update.Preconditions.account |> Zkapp_precondition.Account.state
    |> Zkapp_state.V.to_list |> zeroth |> Zkapp_basic.Or_ignore.to_option
    |> Option.value ~default:Field.zero
  in
  let target =
    body |> Account_update.Body.update |> Account_update.Update.app_state
    |> Zkapp_state.V.to_list |> zeroth |> Zkapp_basic.Set_or_keep.to_option
    |> Option.value ~default:Field.zero
  in
  Some (source, target)

let get_inner_deposits_state_exn (module M : Zkapps_rollup.S) l =
  let (old_deposits_commit :: _) =
    let idx = Mina_ledger.Ledger.index_of_account_exn l M.Inner.account_id in
    let inner_acc = Mina_ledger.Ledger.get_at_index_exn l idx in
    (Option.value_exn inner_acc.zkapp).app_state
  in
  old_deposits_commit
