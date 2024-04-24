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
