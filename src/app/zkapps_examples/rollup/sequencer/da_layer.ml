open Core_kernel
open Mina_base
open Async

module Da_layer = struct
  type config_t = { da_contract_address : string }

  let post_batch config ~commands ~batch_id ~previous_batch_id =
    let payload =
      String.concat ~sep:"\n"
        ( config.da_contract_address
        :: (batch_id ^ " " ^ previous_batch_id)
        :: List.fold commands ~init:[] ~f:(fun accum command ->
               let command_type, command_data =
                 match command with
                 | User_command.Signed_command signed_command ->
                     ( 0
                     , Signed_command.to_base64
                         (Signed_command.forget_check signed_command) )
                 | User_command.Zkapp_command zkapp_command ->
                     (1, Zkapp_command.to_base64 zkapp_command)
               in
               accum @ [ Int.to_string command_type ^ " " ^ command_data ] ) )
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
