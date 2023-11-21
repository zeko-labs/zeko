open Core_kernel
open Mina_base
open Async

module Da_layer = struct
  type config_t = { da_contract_address : string }

  let command_to_yojson command =
    match command with
    | User_command.Signed_command signed_command ->
        `Assoc
          [ ("commandType", `Int 0)
          ; ( "data"
            , `String
                (Signed_command.to_base64
                   (Signed_command.forget_check signed_command) ) )
          ]
    | User_command.Zkapp_command zkapp_command ->
        `Assoc
          [ ("commandType", `Int 1)
          ; ("data", `String (Zkapp_command.to_base64 zkapp_command))
          ]

  let post_batch config ~commands ~batch_id ~previous_batch_id =
    let payload =
      Yojson.to_string
        (`Assoc
          [ ("address", `String config.da_contract_address)
          ; ("id", `String batch_id)
          ; ("previousId", `String previous_batch_id)
          ; ("commands", `List (List.map commands ~f:command_to_yojson))
          ] )
    in
    let stdin =
      Core.Unix.open_process_out
        "cd ../da-layer && npx hardhat run scripts/postBatch.ts --network dev"
    in
    let pid = UnixLabels.process_out_pid stdin in
    Out_channel.output_string stdin payload ;
    Out_channel.close stdin ;
    Unix.waitpid_exn (Pid.of_int pid)
end

include Da_layer
