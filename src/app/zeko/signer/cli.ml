open Core
open Async
open Cli_lib
open Signature_lib

let run =
  ( "run"
  , Command.async ~summary:"Run minimal signing service"
      (let%map_open.Command log_json = Flag.Log.json
       and log_level = Flag.Log.level
       and port =
         flag "--port" (optional_with_default 9000 int)
           ~doc:"int Port to listen on"
       and private_key =
         flag "--private-key" (optional string)
           ~doc:"string Base58 private key, defaults to MINA_PRIVATE_KEY"
       and allow_field_signing =
         flag "--allow-field-signing" no_arg
           ~doc:"Allow signing raw field elements"
       and allow_zkapp_signing =
         flag "--allow-zkapp-signing" no_arg
           ~doc:"Allow signing zkApp commands"
       and max_fee =
         flag "--max-fee" (optional string)
           ~doc:"string Maximum fee in mina for zkApp signing"
       and max_balance_change =
         flag "--max-balance-change" (optional string)
           ~doc:"string Maximum absolute signed balance change in mina"
       in
       fun () ->
         if (not allow_field_signing) && not allow_zkapp_signing then
           failwith
             "At least one of --allow-field-signing or --allow-zkapp-signing must be enabled" ;
         let private_key =
           match private_key with
           | Some private_key ->
               private_key
           | None ->
               Sys.getenv_exn "MINA_PRIVATE_KEY"
         in
         let logger = Logger.create () in
         Stdout_log.setup log_json log_level ;
         let policy =
           let zkapp =
             if allow_zkapp_signing then
               Some
                 Signer_service.Policy.Zkapp.
                   { max_fee = Option.map max_fee ~f:Currency.Fee.of_mina_string_exn
                   ; max_balance_change =
                       Option.map max_balance_change
                         ~f:Currency.Amount.of_mina_string_exn
                   }
             else None
           in
           Signer_service.Policy.{ allow_field_signing; zkapp }
         in
         let signer =
           Signer_service.Server.create ~logger ~policy
             ~private_key:(Private_key.of_base58_check_exn private_key)
         in
         let%bind () = Signer_service.Server.run ~port signer in
         [%log info] "Signer service listening on port %d" port ;
         never () ) )

let () = Command.group ~summary:"Signer service" [ run ] |> Command_unix.run
