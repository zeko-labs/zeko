open Core_kernel
open Async
open Message_queue
open Cli_lib

let ( let* ) = Deferred.Let_syntax.( >>= )

let ( let*| ) = Deferred.Let_syntax.( >>| )

(* Server *)
let handler ~logger id =
  [%log info] "Received message %s" id ;
  let* () = after (Time.Span.of_sec 1.) in
  return (sprintf "Done %s" id)

let run_server =
  ( "run-server"
  , Command.async ~summary:""
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and mq_host =
         flag "--mq-host" (required string) ~doc:"string Message queue host"
       in
       let logger = Logger.create () in
       Stdout_log.setup log_json log_level ;
       let _server =
         Worker.start (Host_and_port.of_string mq_host) (handler ~logger)
       in
       Deferred.never ) )

(* Client *)
let send_message send_fn ~logger master id =
  let*| response = send_fn master id in
  match response with
  | Ok response ->
      [%log info] "%s" response
  | Error error ->
      [%log error] "Error: %s" (Error.to_string_hum error)

let send_messages ~logger master =
  List.init 10 ~f:Fn.id
  |> List.map ~f:(fun id ->
         [%log info] "Sending message %d" id ;
         send_message
           (if id < 5 then Master.send_exn else Master.send_with_priority_exn)
           ~logger master (Int.to_string id) )

let run_client =
  ( "run-client"
  , Command.async ~summary:""
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and mq_host =
         flag "--mq-host" (required string) ~doc:"string Message queue host"
       in
       fun () ->
         let logger = Logger.create () in
         Stdout_log.setup log_json log_level ;
         let* master = Master.start (Host_and_port.of_string mq_host) in
         let ds = send_messages ~logger master in
         [%log info] "Messages sent" ;
         let*| () = Deferred.all_unit ds in
         [%log info] "Finished" ) )

let () =
  Command.group ~summary:"Zeko prover CLI" [ run_server; run_client ]
  |> Command_unix.run
