open Core
open Async
open Signature_lib
module Server = Zeko_prover.Prover

let run_server =
  ( "run-server"
  , Command.async ~summary:"Run prover server"
      (let%map_open.Command port =
         flag "--port" (required int) ~doc:"int Port to listen on"
       in
       let module T = Transaction_snark.Make (struct
         let constraint_constants = Server.constraint_constants

         let proof_level = Genesis_constants.Proof_level.Full
       end) in
       let module M = Zkapps_rollup.Make (T) in
       let module S = Server.Make (T) (M) in
       fun () -> S.run ~port ) )

let () =
  Command.group ~summary:"Zeko prover CLI" [ run_server ] |> Command_unix.run
