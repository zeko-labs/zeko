open Core_kernel
open Async
open Cli_lib

let run_server =
  ( "run-server"
  , Command.async ~summary:"Run prover server"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and mq_host =
         flag "--mq-host" (required string) ~doc:"string Message queue host"
       and fake_proving_time =
         flag "--fake-proving-time" (optional float)
           ~doc:"float Fake proving time in seconds"
       in
       let logger = Logger.create () in
       Stdout_log.setup log_json log_level ;
       [%log info] "Compiling circuits" ;
       Zeko_prover.Prover.run
         ?fake_proving_time:(Option.map ~f:Time.Span.of_sec fake_proving_time)
         ~logger
         ~mq_host:(Host_and_port.of_string mq_host) ) )

let () =
  Command.group ~summary:"Zeko prover CLI" [ run_server ] |> Command_unix.run
