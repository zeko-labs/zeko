open Async
open Core_kernel

let try_connect where_to_connect =
  match%bind try_with (fun () -> Tcp.connect where_to_connect) with
  | Ok x ->
      return (Ok x)
  | Error exn ->
      return (Error (Error.of_exn exn))

module State = struct
  type lazy_connection =
    ( ([ `Active ], Socket.Address.Inet.t) Socket.t * Reader.t * Writer.t
    , Error.t )
    Result.t
    Deferred.t
    lazy_t

  type prover_state = [ `In_use | `Available ]

  type t =
    { provers :
        (lazy_connection ref * Tcp.Where_to_connect.inet * prover_state ref)
        list
    ; mutable next : int
    }

  let create provers =
    let connections =
      List.map provers ~f:(fun x ->
          (ref (lazy (try_connect x)), x, ref `Available) )
    in
    { provers = connections; next = 0 }

  let rec next_prover (t : t) =
    let rotate l n =
      let left, right = List.split_n l n in
      right @ left
    in
    t.next <- (t.next + 1) mod List.length t.provers ;
    match
      rotate t.provers t.next
      |> List.find ~f:(fun (_, _, status) ->
             match !status with `Available -> true | `In_use -> false )
    with
    | Some prover ->
        let status = trd3 prover in
        status := `In_use ;
        return (prover, fun () -> status := `Available)
    | None ->
        let%bind () = Clock.after (Time.Span.of_sec 1.) in
        next_prover t
end

(* Get the reference of next available prover.
   If it fails to connect or times out, replace the reference with new connection and try whole thing again *)
let rec send ?(proving_timeout = 20.) ?(wait_for_prover_timeout = 600.)
    ?(attempts = 5) t (input : Prover.Input.t) : Prover.Output.t Deferred.t =
  let%bind (connection_ref, where_to_connect, status), release_prover =
    match%bind
      Async.with_timeout
        (Time.Span.of_sec wait_for_prover_timeout)
        (State.next_prover t)
    with
    | `Result x ->
        return x
    | `Timeout ->
        failwith "Timeout while getting prover"
  in
  match%bind
    Async.with_timeout
      (Time.Span.of_sec proving_timeout)
      ( match%bind Lazy.force !connection_ref with
      | Error err ->
          printf "Error connecting to prover: %s\n%!" (Error.to_string_hum err) ;
          return `Connection_error
      | Ok (_, r, w) -> (
          match%bind
            let () =
              Prover.Input.to_yojson input
              |> Yojson.Safe.to_string |> Writer.write_line w
            in
            Reader.really_read_line ~wait_time:(Time.Span.of_sec 1.) r
          with
          | Some response -> (
              match
                Yojson.Safe.from_string response |> Prover.Output.of_yojson
              with
              | Ok output ->
                  return (`Ok output)
              | Error _ ->
                  failwith "Error parsing response" )
          | None ->
              failwith "Timeout while proving" ) )
    >>| fun r -> release_prover () ; r
  with
  | `Result (`Ok r) ->
      return r
  | `Timeout | `Result `Connection_error ->
      printf "Timeout while proving %f, retrying attempts remaining: %d\n%!"
        proving_timeout attempts ;
      if attempts > 0 then (
        connection_ref := lazy (try_connect where_to_connect) ;
        send ~proving_timeout ~attempts:(attempts - 1) t input )
      else failwith "Timeout while proving"

let wrapper_wrap ?proving_timeout t ~txn_snark =
  send ?proving_timeout t (Prover.Input.Wrapper_wrap txn_snark)
  >>| function
  | Prover.Output.Wrapper_wrap x -> x | _ -> failwith "Unexpected response"

let wrapper_merge ?proving_timeout t a b =
  send ?proving_timeout t (Prover.Input.Wrapper_merge (a, b))
  >>| function
  | Prover.Output.Wrapper_merge x -> x | _ -> failwith "Unexpected response"

let transaction_snark_of_signed_command ?proving_timeout t ~statement
    ~user_command_in_block ~sparse_ledger =
  send ?proving_timeout t
    (Prover.Input.Transaction_snark_of_signed_command
       (statement, user_command_in_block, sparse_ledger) )
  >>| function
  | Prover.Output.Transaction_snark_of_signed_command x ->
      x
  | _ ->
      failwith "Unexpected response"

let transaction_snark_of_zkapp_command_segment ?proving_timeout t ~statement
    ~witness ~spec =
  send ?proving_timeout t
    (Prover.Input.Transaction_snark_of_zkapp_command_segment
       (statement, witness, spec) )
  >>| function
  | Prover.Output.Transaction_snark_of_zkapp_command_segment x ->
      x
  | _ ->
      failwith "Unexpected response"

let transaction_snark_merge ?proving_timeout t a b =
  send ?proving_timeout t (Prover.Input.Transaction_snark_merge (a, b))
  >>| function
  | Prover.Output.Transaction_snark_merge x ->
      x
  | _ ->
      failwith "Unexpected response"

let submit_deposit ?proving_timeout t ~outer_pk ~deposit =
  send ?proving_timeout t (Prover.Input.Submit_deposit (outer_pk, deposit))
  >>| function
  | Prover.Output.Submit_deposit x -> x | _ -> failwith "Unexpected response"

let submit_withdrawal ?proving_timeout t ~withdrawal =
  send ?proving_timeout t (Prover.Input.Submit_withdrawal withdrawal)
  >>| function
  | Prover.Output.Submit_withdrawal x -> x | _ -> failwith "Unexpected response"

let process_deposit ?proving_timeout t ~is_new ~pointer ~before ~after ~deposit
    =
  send ?proving_timeout t
    (Prover.Input.Process_deposit (is_new, pointer, before, after, deposit))
  >>| function
  | Prover.Output.Process_deposit x -> x | _ -> failwith "Unexpected response"

let process_withdrawal ?proving_timeout t ~outer_pk ~is_new ~pointer ~before
    ~after ~withdrawal =
  send ?proving_timeout t
    (Prover.Input.Process_withdrawal
       (outer_pk, is_new, pointer, before, after, withdrawal) )
  >>| function
  | Prover.Output.Process_withdrawal x ->
      x
  | _ ->
      failwith "Unexpected response"

let outer_step ?proving_timeout t ~last ~outer_public_key ~new_deposits
    ~unprocessed_deposits ~old_inner_ledger ~new_inner_ledger =
  send ?proving_timeout t
    (Prover.Input.Outer_step
       ( last
       , outer_public_key
       , new_deposits
       , unprocessed_deposits
       , old_inner_ledger
       , new_inner_ledger ) )
  >>| function
  | Prover.Output.Outer_step x -> x | _ -> failwith "Unexpected response"

let inner_step ?proving_timeout t ~all_deposits =
  send ?proving_timeout t (Prover.Input.Inner_step all_deposits)
  >>| function
  | Prover.Output.Inner_step x -> x | _ -> failwith "Unexpected response"
