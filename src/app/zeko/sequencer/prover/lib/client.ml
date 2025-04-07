open Async
open Core_kernel

let try_connect where_to_connect =
  match%bind try_with (fun () -> Tcp.connect where_to_connect) with
  | Ok x ->
      return (Ok x)
  | Error exn ->
      return (Error (Error.of_exn exn))

type lazy_connection =
  ( ([ `Active ], Socket.Address.Inet.t) Socket.t * Reader.t * Writer.t
  , Error.t )
  Result.t
  Deferred.t

type prover = lazy_connection ref * Tcp.Where_to_connect.inet

type t = { q : prover Throttle.t }

let create ?(ping_interval = 15.) ?(ping_timeout = 10.) provers =
  let connections = List.map provers ~f:(fun x -> (ref (try_connect x), x)) in
  let q = Throttle.create_with ~continue_on_error:true connections in
  (* Start pinging *)
  let rec ping_loop () =
    let%map () = after (Time.Span.of_sec ping_interval) in
    List.iter connections ~f:(fun _ ->
        don't_wait_for
        @@ Throttle.enqueue q (fun (connection_ref, _) ->
               match%bind !connection_ref with
               | Error _ ->
                   return ()
               | Ok (_, r, w) ->
                   let () =
                     Prover.Input.to_yojson Prover.Input.Ping
                     |> Yojson.Safe.to_string |> Writer.write_line w
                   in
                   let%map _result =
                     Reader.really_read_line
                       ~wait_time:(Time.Span.of_sec ping_timeout)
                       r
                   in
                   () ) )
  in
  don't_wait_for @@ ping_loop () ;
  { q }

let queue_size t = Throttle.num_jobs_waiting_to_start t.q

(* Get the reference of next available prover.
   If it fails to connect or times out, replace the reference with new connection and try whole thing again *)
let rec send ?(proving_timeout = 20.) ?(attempts = 5) ?(cooldown = 2.) t
    (input : Prover.Input.t) : Prover.Output.t Deferred.t =
  Throttle.enqueue t.q (fun (connection_ref, where_to_connect) ->
      let rec go ~attempts =
        let%bind result =
          match%bind !connection_ref with
          | Error err ->
              printf "Error connecting to prover: %s\n%!"
                (Error.to_string_hum err) ;
              return `Connnection_error
          | Ok (s, r, w) -> (
              match%map
                Async.with_timeout
                  (Time.Span.of_sec proving_timeout)
                  ( Prover.Input.to_yojson input
                    |> Yojson.Safe.to_string |> Writer.write_line w ;
                    Reader.really_read_line
                      ~wait_time:(Time.Span.of_sec proving_timeout)
                      r )
              with
              | `Result (Some response) -> (
                  match
                    Yojson.Safe.from_string response |> Prover.Output.of_yojson
                  with
                  | Ok output ->
                      `Ok output
                  | Error _ ->
                      print_endline "Error parsing response from prover" ;
                      `Parsing_error )
              | `Timeout | `Result None ->
                  Socket.shutdown s `Both ;
                  printf "Timeout from prover, remaining attempts: %d\n%!"
                    (attempts - 1) ;
                  `Timeout )
        in
        match result with
        | `Timeout | `Connnection_error | `Parsing_error ->
            let%bind () = after (Time.Span.of_sec cooldown) in
            connection_ref := try_connect where_to_connect ;
            go ~attempts:(attempts - 1)
        | `Ok result ->
            return result
      in
      go ~attempts )

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
