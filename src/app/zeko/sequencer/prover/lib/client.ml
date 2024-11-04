open Async
open Core_kernel

let prove where_to_connect (input : Prover.Input.t) : Prover.Output.t Deferred.t
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
