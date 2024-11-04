open Async
open Core_kernel

let send where_to_connect (input : Prover.Input.t) : Prover.Output.t Deferred.t
    =
  match%bind
    Tcp.with_connection where_to_connect (fun _ r w ->
        let () =
          Prover.Input.to_yojson input
          |> Yojson.Safe.to_string |> Writer.write_line w
        in
        Reader.recv r )
  with
  | `Ok response -> (
      match
        Bytes.to_string response |> Yojson.Safe.from_string
        |> Prover.Output.of_yojson
      with
      | Ok output ->
          return output
      | Error _ ->
          failwith "Error parsing response" )
  | `Eof ->
      failwith "EOF"

let where_to_connect =
  Tcp.Where_to_connect.of_host_and_port
    (Host_and_port.create ~host:"localhost" ~port:9990)

let wrapper_wrap txn_snark =
  send where_to_connect (Prover.Input.Wrapper_wrap txn_snark)
  >>| function
  | Prover.Output.Wrapper_wrap x -> x | _ -> failwith "Unexpected response"

let wrapper_merge last wrapped =
  send where_to_connect (Prover.Input.Wrapper_merge (last, wrapped))
  >>| function
  | Prover.Output.Wrapper_merge x -> x | _ -> failwith "Unexpected response"

let transaction_snark_of_signed_command statement user_command_in_block
    sparse_ledger =
  send where_to_connect
    (Prover.Input.Transaction_snark_of_signed_command
       (statement, user_command_in_block, sparse_ledger) )
  >>| function
  | Prover.Output.Transaction_snark_of_signed_command x ->
      x
  | _ ->
      failwith "Unexpected response"

let transaction_snark_of_zkapp_command_segment statement witness spec =
  send where_to_connect
    (Prover.Input.Transaction_snark_of_zkapp_command_segment
       (statement, witness, spec) )
  >>| function
  | Prover.Output.Transaction_snark_of_zkapp_command_segment x ->
      x
  | _ ->
      failwith "Unexpected response"

let transaction_snark_merge a b =
  send where_to_connect (Prover.Input.Transaction_snark_merge (a, b))
  >>| function
  | Prover.Output.Transaction_snark_merge x ->
      x
  | _ ->
      failwith "Unexpected response"

let submit_deposit pk tr =
  send where_to_connect (Prover.Input.Submit_deposit (pk, tr))
  >>| function
  | Prover.Output.Submit_deposit x -> x | _ -> failwith "Unexpected response"

let submit_withdrawal tr =
  send where_to_connect (Prover.Input.Submit_withdrawal tr)
  >>| function
  | Prover.Output.Submit_withdrawal x -> x | _ -> failwith "Unexpected response"
