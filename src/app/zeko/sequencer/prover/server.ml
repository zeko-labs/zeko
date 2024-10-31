open Core_kernel
open Async
open Mina_base
open Mina_ledger
open Signature_lib

let constraint_constants = Genesis_constants.Constraint_constants.compiled

let time label (d : 'a Deferred.t) =
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  printf "%s: %s\n%!" label (Time.Span.to_string_hum @@ Time.diff stop start) ;
  return x

let sok_digest =
  Sok_message.digest
  @@ Sok_message.create ~fee:Currency.Fee.zero
       ~prover:(Public_key.compress @@ (Keypair.create ()).public_key)

module Command_witness = struct
  type t =
    | Signed_command of
        Sparse_ledger.t
        * Signed_command.With_valid_signature.t Transaction_protocol_state.t
        * Transaction_snark.Statement.With_sok.t
    | Zkapp_command of
        ( Transaction_witness.Zkapp_command_segment_witness.t
        * Transaction_snark.Zkapp_command_segment.Basic.t
        * Mina_state.Snarked_ledger_state.With_sok.t )
        list
        * Zkapp_command.t
  [@@deriving yojson]
end

module Input = struct
  type t =
    { command_witness : Command_witness.t; last : Zkapps_rollup.t option }
  [@@deriving yojson]
end

module Make (T : Transaction_snark.S) (M : Zkapps_rollup.S) = struct
  let wrap_and_merge last txn_snark command =
    let%bind wrapped = M.Wrapper.wrap txn_snark in
    let%bind final_snark =
      match last with
      | Some last' ->
          M.Wrapper.merge last' wrapped
      | None ->
          return wrapped
    in
    return final_snark

  let prove_signed_command last ~sparse_ledger ~user_command_in_block ~statement
      =
    let handler = unstage @@ Sparse_ledger.handler sparse_ledger in
    let%bind txn_snark =
      time "Transaction_snark.of_signed_command"
        (T.of_user_command ~init_stack:Mina_base.Pending_coinbase.Stack.empty
           ~statement user_command_in_block handler )
    in
    wrap_and_merge last txn_snark
      (User_command.Signed_command
         (Signed_command.forget_check user_command_in_block.transaction) )

  let prove_zkapp_command last ~witnesses ~zkapp_command =
    let%bind txn_snark =
      match witnesses with
      | [] ->
          failwith "No witnesses"
      | (witness, spec, statement) :: rest ->
          let%bind p1 =
            time "Transaction_snark.of_zkapp_command_segment"
              (T.of_zkapp_command_segment_exn ~statement ~witness ~spec)
          in
          Deferred.List.fold ~init:p1 rest
            ~f:(fun acc (witness, spec, statement) ->
              let%bind prev = return acc in
              let%bind curr =
                time "Transaction_snark.of_zkapp_command_segment"
                  (T.of_zkapp_command_segment_exn ~statement ~witness ~spec)
              in
              let%bind merged =
                time "Transaction_snark.merge" (T.merge curr prev ~sok_digest)
              in
              return (Or_error.ok_exn merged) )
    in
    wrap_and_merge last txn_snark (User_command.Zkapp_command zkapp_command)

  let prove last command_witness =
    match command_witness with
    | Command_witness.Signed_command
        (sparse_ledger, user_command_in_block, statement) ->
        prove_signed_command last ~sparse_ledger ~user_command_in_block
          ~statement
    | Command_witness.Zkapp_command (witnesses, zkapp_command) ->
        prove_zkapp_command last ~witnesses ~zkapp_command

  let run ~port =
    ignore
    @@ Tcp.Server.create (Tcp.Where_to_listen.of_port port)
         ~on_handler_error:`Ignore (fun _ r w ->
           Pipe.transfer' ~max_queue_length:1 (Reader.pipe r) (Writer.pipe w)
             ~f:
               (Deferred.Queue.map ~how:`Sequential ~f:(fun input ->
                    Yojson.Safe.from_string input
                    |> Input.of_yojson
                    |> function
                    | Ok Input.{ last; command_witness } ->
                        prove last command_witness >>| Zkapps_rollup.to_yojson
                        >>| Yojson.Safe.to_string
                    | Error e ->
                        return e ) ) ) ;
    printf "Listening on port %d\n" port ;
    Deferred.never ()
end
