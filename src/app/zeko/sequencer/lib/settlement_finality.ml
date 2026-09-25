open Async_kernel
open Core_kernel

let ethereum_gateway_enabled () =
  Option.is_some (Stdlib.Sys.getenv_opt "ZEKO_ETHEREUM_GATEWAY_TOKEN")

(* Pool membership is not a settlement receipt: failed jobs leave the pool too.
   Keep gateway coordination separate from the Mina compatibility queries. *)
module Gateway = struct
  type reservation =
    { id : string
    ; fencing_token : string
    ; ledger_hash : string
    ; mutable stopped : bool
    ; mutable renewal_error : Error.t option
    }

  type outcome =
    { status : string
    ; finalized : bool
    ; retryable : bool
    ; error : string option
    ; source : string
    ; target : string
    }

  let request ~l1_uri meth path body =
    let open Async in
    let%map result =
      Clock.with_timeout (Time.Span.of_sec 20.)
        (Monitor.try_with (fun () ->
             let token =
               Stdlib.Sys.getenv_opt "ZEKO_ETHEREUM_GATEWAY_TOKEN"
               |> Option.value_exn ~message:"Ethereum gateway token is missing"
             in
             let headers =
               Cohttp.Header.of_list
                 [ ("Content-Type", "application/json"); ("X-API-Key", token) ]
             in
             let uri =
               Uri.with_path l1_uri path |> fun u -> Uri.with_query u []
             in
             let%bind response, response_body =
               Cohttp_async.Client.call ~headers
                 ~body:
                   (Cohttp_async.Body.of_string (Yojson.Safe.to_string body))
                 meth uri
             in
             let%map body = Cohttp_async.Body.to_string response_body in
             let status =
               Cohttp.Response.status response |> Cohttp.Code.code_of_status
             in
             if status = 404 then Ok None
             else if status >= 200 && status < 300 then
               Ok
                 (Some
                    ( if String.is_empty body then `Null
                    else Yojson.Safe.from_string body ) )
             else
               (* Do not include request headers or proof payloads in diagnostics. *)
               Or_error.errorf "Ethereum gateway %s: HTTP %d: %s" path status
                 body ) )
    in
    match result with
    | `Timeout ->
        Or_error.errorf "Ethereum gateway %s timed out" path
    | `Result result ->
        Result.map_error result ~f:Error.of_exn |> Or_error.join

  let field_string json name =
    Yojson.Safe.Util.(json |> member name |> to_string)

  let optional_string json name =
    match Yojson.Safe.Util.member name json with
    | `String value ->
        Some value
    | _ ->
        None

  let reservation_json t =
    `Assoc [ ("id", `String t.id); ("fencingToken", `String t.fencing_token) ]

  let renew ~l1_uri t =
    let%map result =
      request ~l1_uri `POST
        ("/v1/settlement-reservations/" ^ t.id ^ "/renew")
        (`Assoc
          [ ("fencingToken", `String t.fencing_token)
          ; ("ttlSeconds", `Int 120)
          ] )
    in
    match result with
    | Ok (Some _) ->
        t.renewal_error <- None ;
        Ok ()
    | Ok None ->
        let error = Error.of_string "Settlement reservation no longer exists" in
        t.renewal_error <- Some error ;
        Error error
    | Error error ->
        t.renewal_error <- Some error ;
        Error error

  let acquire ~l1_uri ~owner_id =
    let open Deferred.Or_error.Let_syntax in
    let%bind response =
      request ~l1_uri `POST "/v1/settlement-reservations"
        (`Assoc [ ("ownerId", `String owner_id); ("ttlSeconds", `Int 120) ])
    in
    match
      Or_error.try_with (fun () ->
          let json = Option.value_exn response in
          { id = field_string json "reservationId"
          ; fencing_token = field_string json "fencingToken"
          ; ledger_hash =
              field_string
                (Yojson.Safe.Util.member "checkpoint" json)
                "ledgerHash"
          ; stopped = false
          ; renewal_error = None
          } )
    with
    | Error error ->
        Deferred.return (Error error)
    | Ok reservation ->
        let rec heartbeat () =
          let%bind.Deferred () = Clock_ns.after (Time_ns.Span.of_sec 30.) in
          if reservation.stopped then Deferred.unit
          else
            let%bind.Deferred _ = renew ~l1_uri reservation in
            heartbeat ()
        in
        don't_wait_for (heartbeat ()) ;
        return reservation

  let release ~l1_uri t =
    t.stopped <- true ;
    request ~l1_uri `DELETE
      ("/v1/settlement-reservations/" ^ t.id)
      (`Assoc [ ("fencingToken", `String t.fencing_token) ])
    >>| ignore

  let outcome_of_json json =
    let open Yojson.Safe.Util in
    { status = field_string json "status"
    ; finalized = json |> member "finalized" |> to_bool
    ; retryable = json |> member "retryable" |> to_bool
    ; error = optional_string json "error"
    ; source = field_string json "sourceLedgerHash"
    ; target = field_string json "targetLedgerHash"
    }

  let lookup ~l1_uri hash =
    let%map result =
      request ~l1_uri `GET ("/v1/settlements/by-mina-hash/" ^ hash) `Null
    in
    Or_error.bind result ~f:(fun result ->
        Or_error.try_with (fun () -> Option.map result ~f:outcome_of_json) )

  let terminal = function
    | "confirmed"
    | "failed"
    | "proof_failed"
    | "ethereum_reverted"
    | "reorged"
    | "rejected"
    | "executed" ->
        true
    | _ ->
        false
end

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

module Gate = struct
  type t = unit Mvar.Read_write.t

  let create () =
    let t = Mvar.create () in
    don't_wait_for (Mvar.put t ()) ;
    t

  let with_ t ~f =
    let%bind () = Mvar.take t in
    Monitor.protect f ~finally:(fun () -> Mvar.put t ())

  let with_held_until t ~f ~until =
    let%bind () = Mvar.take t in
    match%bind Monitor.try_with f with
    | Error exn ->
        let%map () = Mvar.put t () in
        raise exn
    | Ok result ->
        don't_wait_for
          (Monitor.protect
             (fun () -> until result)
             ~finally:(fun () -> Mvar.put t ()) ) ;
        return result
end

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
