open Async_kernel
open Core_kernel

let ethereum_gateway_enabled () =
  Option.is_some (Sys.getenv_opt "ZEKO_ETHEREUM_GATEWAY_TOKEN")

let wait_until_idle ?(retry_delay = Time_ns.Span.of_sec 15.)
    ?(sleep = Clock_ns.after) ~has_pending ~on_wait () =
  let rec loop () =
    match%bind has_pending () with
    | Error error ->
        Deferred.return (Error error)
    | Ok false ->
        Deferred.Or_error.return ()
    | Ok true ->
        on_wait () ;
        let%bind () = sleep retry_delay in
        loop ()
  in
  loop ()

let run_after_wait ~wait run =
  match%bind wait () with
  | Error error ->
      Deferred.return (Error error)
  | Ok () ->
      run ()

let prepare_and_enqueue_after_wait ~wait ~prepare ~enqueue job =
  (* Finality polling can take minutes. Keep it outside the transaction
     admission queue so user commands can continue to enter the sequencer.
     State-bound preparation must happen after the wait because finalization
     can advance the outer action state. *)
  run_after_wait ~wait (fun () ->
      prepare () >>| Result.map ~f:(fun prepared -> enqueue (job prepared)) )

(* The Ethereum gateway keeps accepted commands in its Mina-compatible pool
   until their Ethereum transactions are finalized or fail. An empty pool is
   therefore the boundary at which it is safe to build a new state-bound
   settlement proof. *)
let wait_for_previous_settlement ~logger ~l1_uri ~signer_pk ~message =
  if not (ethereum_gateway_enabled ()) then Deferred.Or_error.return ()
  else
    wait_until_idle
      ~has_pending:(fun () ->
        Gql_client.fetch_pooled_zkapp_commands ~logger l1_uri signer_pk
        >>| Result.map ~f:(Fn.non List.is_empty) )
      ~on_wait:(fun () -> [%log info] "%s" message)
      ()
