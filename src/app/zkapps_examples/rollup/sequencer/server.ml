open Core
open Async
open Mina_base
open Ppx_deriving_yojson_runtime.Result
module Body = Cohttp_async.Body
module Server = Cohttp_async.Server
module Request = Cohttp_async.Request
module Header = Cohttp.Header

module Handlers (Args : Zeko_sequencer.Args_t) = struct
  module S = Zeko_sequencer.Make (Args)

  type t =
       (string * string) list
    -> string option
    -> Request.t
    -> Body.t
    -> Server.response Deferred.t

  let apply_signed_command : t =
   fun _keys _rest _request body ->
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

      Server.respond_string
        ~headers:(Header.init_with "Content-Type" "application/json")
        (Yojson.to_string json)
    with e ->
      Server.respond_string ~status:`Bad_request (Exn.to_string_mach e)

  let apply_zkapp_command : t =
   fun _keys _rest _request body ->
    let%bind body = Body.to_string body in
    try
      let zkapp_command =
        Zkapp_command.of_json @@ Yojson.Safe.from_string @@ body
      in

      let hash, id = S.apply_zkapp_command zkapp_command in
      let hash = Mina_transaction.Transaction_hash.to_base58_check hash in
      let json = `Assoc [ ("id", `String id); ("hash", `String hash) ] in

      Server.respond_string
        ~headers:(Header.init_with "Content-Type" "application/json")
        (Yojson.to_string json)
    with e ->
      Server.respond_string ~status:`Bad_request (Exn.to_string_mach e)

  let get_account : t =
   fun keys _rest _request _body ->
    let pk =
      Signature_lib.Public_key.Compressed.of_base58_check_exn
        (Stdlib.List.assoc "pk" keys)
    in
    let open Option.Let_syntax in
    let token_id =
      Stdlib.List.assoc_opt "token_id" keys
      >>| Token_id.of_string
      |> Option.value ~default:Token_id.default
    in
    match S.get_account ~token_id pk with
    | Some account ->
        let to_json =
          Fields_derivers_zkapps.to_json @@ Mina_base.Account.deriver
          @@ Fields_derivers_zkapps.o ()
        in
        Server.respond_string
          ~headers:(Header.init_with "Content-Type" "application/json")
          (Yojson.Safe.to_string (to_json account))
    | None ->
        Server.respond_string ~status:`Not_found "Account not found"

  let get_root : t =
   fun _keys _rest _request _body ->
    let root =
      Mina_base.Frozen_ledger_hash0.to_decimal_string (S.get_root ())
    in
    let json = `Assoc [ ("root", `String root) ] in
    Server.respond_string
      ~headers:(Header.init_with "Content-Type" "application/json")
      (Yojson.to_string json)

  let routes : (string * (Cohttp.Code.meth * t) list) list =
    [ ("/apply_signed_command", [ (`POST, apply_signed_command) ])
    ; ("/apply_zkapp_command", [ (`POST, apply_zkapp_command) ])
    ; ("/get_account/:pk/:token_id", [ (`GET, get_account) ])
    ; ("/get_account/:pk", [ (`GET, get_account) ])
    ; ("/get_root", [ (`GET, get_root) ])
    ]

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
end

let run port max_pool_size commitment_period () =
  let module Handlers = Handlers (struct
    let max_pool_size = max_pool_size

    let committment_period_sec = commitment_period
  end) in
  Handlers.S.add_account
    (Signature_lib.Public_key.Compressed.of_base58_check_exn
       "B62qkAdonbeqcuVwQJtHbcqMbb4fbuFHJpqvNCfCBt194xSQ1o3i5rt" )
    (Unsigned.UInt64.of_int64 1_000_000_000_000L) ;

  Handlers.S.run_committer () ;

  let open Async_kernel in
  let () =
    Cohttp_async.Server.create
      ~on_handler_error:
        (`Call
          (fun _ exn ->
            print_endline "Unhandled exception" ;
            print_endline (Exn.to_string exn) ) )
      (Async.Tcp.Where_to_listen.of_port port)
      (fun ~body _ req -> Handlers.path_handler req body)
    |> Deferred.ignore_m |> don't_wait_for
  in
  print_endline ("Sequencer listening on port " ^ Int.to_string port) ;
  never_returns (Async.Scheduler.go ())

let () =
  Command.basic ~summary:"Zeko sequencer"
    (let%map_open.Command port =
       flag "-p" (optional_with_default 8080 int) ~doc:"int Port to listen on"
     and commitment_period =
       flag "--committment-period"
         (optional_with_default 120. float)
         ~doc:"float Commitment period in seconds"
     and max_pool_size =
       flag "--max-pool-size"
         (optional_with_default 10 int)
         ~doc:"int Maximum transaction pool size"
     in
     run port max_pool_size commitment_period )
  |> Command_unix.run
