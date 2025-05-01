open Async
open Sequencer_lib
open Signature_lib
module Sequencer = Zeko_sequencer.Sequencer

let printf = Core.printf

let generate_even_key =
  ( "generate-even-key"
  , Command.basic ~summary:"Generate a private key with an even public key"
      (Command_unix.Param.return (fun () ->
           let keypair = Zeko_types.Even_PC.generate_even_signer () in
           printf "Private key: %s\n"
             (Private_key.to_base58_check keypair.private_key) ;
           printf "Public key: %s\n"
             ( Public_key.compress keypair.public_key
             |> Public_key.Compressed.to_base58_check ) ) ) )

let () =
  Command.group ~summary:"Sequencer CLI" [ generate_even_key ]
  |> Command_unix.run
