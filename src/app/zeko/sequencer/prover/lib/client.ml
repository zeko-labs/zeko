open Async
open Core_kernel

type connection = Reader.t * Writer.t

module State = struct
  type t =
    { mutable provers :
        (([ `Active ], Socket.Address.Inet.t) Socket.t * Reader.t * Writer.t)
        Deferred.t
        lazy_t
        array
    ; mutable next : int
    }

  let create provers =
    let connections = List.map provers ~f:(fun x -> lazy (Tcp.connect x)) in
    { provers = Array.of_list connections; next = 0 }

  let next_prover t =
    let n = t.next in
    t.next <- (n + 1) mod Array.length t.provers ;
    let%bind _, r, w = Lazy.force @@ Array.get t.provers n in
    return (r, w)
end

let send ((r, w) : connection) (input : Prover.Input.t) :
    Prover.Output.t Deferred.t =
  match%bind
    let () =
      Prover.Input.to_yojson input
      |> Yojson.Safe.to_string |> Writer.write_line w
    in
    Reader.really_read_line ~wait_time:(Time.Span.of_sec 60.) r
  with
  | Some response -> (
      match Yojson.Safe.from_string response |> Prover.Output.of_yojson with
      | Ok output ->
          return output
      | Error _ ->
          failwith "Error parsing response" )
  | None ->
      failwith "Timeout while proving"

let wrapper_wrap t txn_snark =
  let%bind prover = State.next_prover t in
  send prover (Prover.Input.Wrapper_wrap txn_snark)
  >>| function
  | Prover.Output.Wrapper_wrap x -> x | _ -> failwith "Unexpected response"

let wrapper_merge t last wrapped =
  let%bind prover = State.next_prover t in
  send prover (Prover.Input.Wrapper_merge (last, wrapped))
  >>| function
  | Prover.Output.Wrapper_merge x -> x | _ -> failwith "Unexpected response"

let transaction_snark_of_signed_command t statement user_command_in_block
    sparse_ledger =
  let%bind prover = State.next_prover t in
  send prover
    (Prover.Input.Transaction_snark_of_signed_command
       (statement, user_command_in_block, sparse_ledger) )
  >>| function
  | Prover.Output.Transaction_snark_of_signed_command x ->
      x
  | _ ->
      failwith "Unexpected response"

let transaction_snark_of_zkapp_command_segment t statement witness spec =
  let%bind prover = State.next_prover t in
  send prover
    (Prover.Input.Transaction_snark_of_zkapp_command_segment
       (statement, witness, spec) )
  >>| function
  | Prover.Output.Transaction_snark_of_zkapp_command_segment x ->
      x
  | _ ->
      failwith "Unexpected response"

let transaction_snark_merge t a b =
  let%bind prover = State.next_prover t in
  send prover (Prover.Input.Transaction_snark_merge (a, b))
  >>| function
  | Prover.Output.Transaction_snark_merge x ->
      x
  | _ ->
      failwith "Unexpected response"

let submit_deposit t pk tr =
  let%bind prover = State.next_prover t in
  send prover (Prover.Input.Submit_deposit (pk, tr))
  >>| function
  | Prover.Output.Submit_deposit x -> x | _ -> failwith "Unexpected response"

let submit_withdrawal t tr =
  let%bind prover = State.next_prover t in
  send prover (Prover.Input.Submit_withdrawal tr)
  >>| function
  | Prover.Output.Submit_withdrawal x -> x | _ -> failwith "Unexpected response"
