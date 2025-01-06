open Async
open Core_kernel
open Zeko_circuits

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
        return prover
    | None ->
        let%bind () = Clock.after (Time.Span.of_sec 1.) in
        next_prover t
end

(* Get the reference of next available prover.
   If it fails to connect or times out, replace the reference with new connection and try whole thing again *)
let rec send ?(proving_timeout = 10.) ?(wait_for_prover_timeout = 600.)
    ?(attempts = 5) t (input : Prover.Input.t) : Prover.Output.t Deferred.t =
  let%bind connection_ref, where_to_connect, status =
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
  status := `In_use ;
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
            Reader.really_read_line ~wait_time:(Time.Span.of_sec 60.) r
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
  with
  | `Result (`Ok r) ->
      status := `Available ;
      return r
  | `Timeout | `Result `Connection_error ->
      status := `Available ;
      printf "Timeout while proving %f, retrying attempts remaining: %d\n%!"
        proving_timeout attempts ;
      if attempts > 0 then (
        connection_ref := lazy (try_connect where_to_connect) ;
        send ~proving_timeout ~attempts:(attempts - 1) t input )
      else failwith "Timeout while proving"

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
