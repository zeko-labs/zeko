open Async

let run_server =
  ( "run-server"
  , Command.async ~summary:"Run prover server"
      (let%map_open.Command port =
         flag "--port" (required int) ~doc:"int Port to listen on"
       in
       let logger = Logger.create () in
       [%log info] "Compiling circuits" ;
       fun () -> Zeko_prover.Prover.run ~logger ~port ) )

let () =
  Command.group ~summary:"Zeko prover CLI" [ run_server ] |> Command_unix.run
