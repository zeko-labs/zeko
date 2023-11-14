open Core
open Async
open Mina_base
open Ppx_deriving_yojson_runtime.Result
module Body = Cohttp_async.Body
module Server = Cohttp_async.Server
module Request = Cohttp_async.Request

let port = 8080

module S = Zeko_sequencer.Make (struct
  let max_pool_size = 10
end)

let apply_signed_command_handler _keys _rest _request body =
  let%bind body = Body.to_string body in
  try
    let json = Yojson.Safe.from_string body in
    let signed_command =
      match Signed_command.of_yojson json with
      | Ok res ->
          res
      | Error _ ->
          failwith "Invalid signed command json"
    in
    let hash, id = S.apply_signed_command signed_command in
    let hash = Mina_transaction.Transaction_hash.to_base58_check hash in
    let json = `Assoc [ ("id", `String id); ("hash", `String hash) ] in
    Server.respond_string (Yojson.to_string json)
  with e -> Server.respond_string ~status:`Bad_request (Exn.to_string_mach e)

let routes : (string * (Cohttp.Code.meth * 'f) list) list =
  [ ("/apply_signed_command", [ (`POST, apply_signed_command_handler) ]) ]

let method_handler handlers keys rest request body =
  match Stdlib.List.assoc_opt (Request.meth request) handlers with
  | Some handler ->
      handler keys rest request body
  | None ->
      Server.respond `Method_not_allowed

let path_handler request body =
  let routes =
    Dispatch.DSL.create
      (List.map routes ~f:(fun (path, handlers) ->
           (path, method_handler handlers) ) )
  in
  let path = Request.resource request in
  match Dispatch.dispatch routes path with
  | Some handler ->
      handler request body
  | None ->
      Server.respond `Not_found

let () =
  let pk =
    Signature_lib.Public_key.decompress_exn
    @@ Signature_lib.Public_key.Compressed.of_base58_check_exn
         "B62qkAdonbeqcuVwQJtHbcqMbb4fbuFHJpqvNCfCBt194xSQ1o3i5rt"
  in
  S.add_account pk (Unsigned.UInt64.of_int64 1_000_000_000_000L) ;

  print_endline ("Sequencer listening on port " ^ Int.to_string port) ;
  let open Async_kernel in
  let () =
    Cohttp_async.Server.create
      ~on_handler_error:
        (`Call
          (fun _ exn ->
            print_endline "Unhandled exception" ;
            print_endline (Exn.to_string exn) ) )
      (Async.Tcp.Where_to_listen.of_port port)
      (fun ~body _ req -> path_handler req body)
    |> Deferred.ignore_m |> don't_wait_for
  in
  never_returns (Async.Scheduler.go ())
