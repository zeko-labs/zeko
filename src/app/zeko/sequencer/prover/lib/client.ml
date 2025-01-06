open Async
open Core_kernel
open Zeko_circuits

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

let transaction_snark_of_single_signed_command ?proving_timeout t ~source_ledger
    ~source_acc_set ~sequencer_pk ~command ~sparse_ledger =
  send ?proving_timeout t
    (Prover.Input.Txn_snark_single_signed_command
       (source_ledger, source_acc_set, sequencer_pk, command, sparse_ledger) )
  >>| function
  | Prover.Output.Zeko_transaction_snark (stmt, proof) ->
      Zeko_transaction_snark.make_unchecked ~proof stmt
  | _ ->
      failwith "Unexpected response from prover"

let transaction_snark_of_single_unproved_zkapp_command ?proving_timeout t
    ~source_ledger ~target_ledger ~connecting_ledger ~source_local_state
    ~target_local_state ~fee_excess ~supply_decrease ~txn_snark_witness
    ~sequencer ~source_acc_set ~shift_action_state =
  send ?proving_timeout t
    (Prover.Input.Txn_snark_single_unproved_zkapp_command
       ( source_ledger
       , target_ledger
       , connecting_ledger
       , source_local_state
       , target_local_state
       , fee_excess
       , supply_decrease
       , txn_snark_witness
       , sequencer
       , source_acc_set
       , shift_action_state ) )
  >>| function
  | Prover.Output.Zeko_transaction_snark (stmt, proof) ->
      Zeko_transaction_snark.make_unchecked ~proof stmt
  | _ ->
      failwith "Unexpected response from prover"

let transaction_snark_of_double_unproved_zkapp_command ?proving_timeout t
    ~source_ledger ~target_ledger ~connecting_ledger ~source_local_state
    ~target_local_state ~fee_excess ~supply_decrease ~txn_snark_witness
    ~sequencer ~source_acc_set ~shift_action_state_first
    ~shift_action_state_second =
  send ?proving_timeout t
    (Prover.Input.Txn_snark_double_unproved_zkapp_command
       ( source_ledger
       , target_ledger
       , connecting_ledger
       , source_local_state
       , target_local_state
       , fee_excess
       , supply_decrease
       , txn_snark_witness
       , sequencer
       , source_acc_set
       , shift_action_state_first
       , shift_action_state_second ) )
  >>| function
  | Prover.Output.Zeko_transaction_snark (stmt, proof) ->
      Zeko_transaction_snark.make_unchecked ~proof stmt
  | _ ->
      failwith "Unexpected response from prover"

let transaction_snark_of_single_proved_zkapp_command ?proving_timeout t
    ~source_ledger ~target_ledger ~connecting_ledger ~source_local_state
    ~target_local_state ~fee_excess ~supply_decrease ~txn_snark_witness
    ~sequencer ~source_acc_set ~zkapp_vk ~zkapp_proof ~shift_action_state =
  send ?proving_timeout t
    (Prover.Input.Txn_snark_single_proved_zkapp_command
       ( source_ledger
       , target_ledger
       , connecting_ledger
       , source_local_state
       , target_local_state
       , fee_excess
       , supply_decrease
       , txn_snark_witness
       , sequencer
       , source_acc_set
       , zkapp_vk
       , zkapp_proof
       , shift_action_state ) )
  >>| function
  | Prover.Output.Zeko_transaction_snark (stmt, proof) ->
      Zeko_transaction_snark.make_unchecked ~proof stmt
  | _ ->
      failwith "Unexpected response from prover"

let transaction_snark_of_merge ?proving_timeout t
    ~(left : Zeko_transaction_snark.T.t) ~(right : Zeko_transaction_snark.T.t) =
  send ?proving_timeout t
    (Prover.Input.Txn_snark_merge
       (left.stmt, left.proof, right.stmt, right.proof) )
  >>| function
  | Prover.Output.Zeko_transaction_snark (stmt, proof) ->
      Zeko_transaction_snark.make_unchecked ~proof stmt
  | _ ->
      failwith "Unexpected response from prover"

let inner_step ?proving_timeout t ~all_deposits = failwith "Not implemented"

let outer_step ?proving_timeout t ~last ~outer_public_key:zkapp_pk ~new_deposits
    ~unprocessed_deposits ~old_inner_ledger ~new_inner_ledger =
  failwith "Not implemented"

let submit_deposit ?proving_timeout t ~outer_pk ~deposit =
  failwith "Not implemented"

let submit_withdrawal ?proving_timeout t ~withdrawal =
  failwith "Not implemented"

let process_deposit ?proving_timeout t ~is_new ~pointer ~before ~after ~deposit
    =
  failwith "Not implemented"

let process_withdrawal ?proving_timeout t ~outer_pk ~is_new ~pointer ~before
    ~after ~withdrawal =
  failwith "Not implemented"
