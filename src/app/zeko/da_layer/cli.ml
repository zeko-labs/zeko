open Async

let run_node =
  ( "run-node"
  , Command.async ~summary:"Run da layer node"
      (let%map_open.Command db_dir =
         flag "--db-dir"
           (optional_with_default "da_db" string)
           ~doc:"string Directory to store the database"
       and port =
         flag "--port"
           (optional_with_default 8080 int)
           ~doc:"int Port to listen on"
       in
       fun () ->
         let signer = Sys.getenv_exn "MINA_PRIVATE_KEY" in
         let logger = Logger.create () in
         let%bind () =
           Deferred.ignore_m
           @@ Da_layer.Node.create_server ~logger ~port ~db_dir
                ~signer_sk:signer
         in
         [%log info] "Server started on port $port"
           ~metadata:[ ("port", `Int port) ] ;
         Async.never () ) )

let () = Command.group ~summary:"DA layer CLI" [ run_node ] |> Command_unix.run
