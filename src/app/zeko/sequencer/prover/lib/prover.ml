open Core_kernel
open Async
open Mina_base
open Mina_ledger
open Signature_lib

let constraint_constants = Genesis_constants.Constraint_constants.compiled

let time ~logger label (d : 'a Deferred.t) =
  [%log info] "Starting %s\n%!" label ;
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  [%log info] "%s: %s\n%!" label
    (Time.Span.to_string_hum @@ Time.diff stop start) ;
  return x

let dummy_sok =
  Sok_message.digest
  @@ Sok_message.create ~fee:Currency.Fee.zero
       ~prover:(Public_key.compress (Keypair.create ()).public_key)

(* Unfortunately yojson doesn't support GADTs so it can't be one type, or maybe I'm just bad *)
module Input = struct
  type t =
    | Wrapper_wrap of Transaction_snark.t
    | Wrapper_merge of (Zkapps_rollup.t * Zkapps_rollup.t)
    | Transaction_snark_of_signed_command of
        ( Mina_state.Snarked_ledger_state.With_sok.t
        * Signed_command.With_valid_signature.t Transaction_protocol_state.t
        * Sparse_ledger.t )
    | Transaction_snark_of_zkapp_command_segment of
        ( Mina_state.Snarked_ledger_state.With_sok.t
        * Transaction_witness.Zkapp_command_segment_witness.t
        * Transaction_snark.Zkapp_command_segment.Basic.t )
    | Transaction_snark_merge of (Transaction_snark.t * Transaction_snark.t)
    | Submit_deposit of (Public_key.Compressed.t * Zkapps_rollup.TR.t)
    | Submit_withdrawal of Zkapps_rollup.TR.t
  [@@deriving yojson]
end

module Output = struct
  type t =
    | Wrapper_wrap of Zkapps_rollup.t
    | Wrapper_merge of Zkapps_rollup.t
    | Transaction_snark_of_signed_command of Transaction_snark.t
    | Transaction_snark_of_zkapp_command_segment of Transaction_snark.t
    | Transaction_snark_merge of Transaction_snark.t
    | Submit_deposit of Zeko_util.call_forest_tree
    | Submit_withdrawal of Zeko_util.call_forest_tree
  [@@deriving yojson]
end

module Make (T : Transaction_snark.S) (M : Zkapps_rollup.S) = struct
  let prove ~logger : Input.t -> Output.t Deferred.t = function
    | Wrapper_wrap txn_snark ->
        time ~logger "Wrapper.wrap" (M.Wrapper.wrap txn_snark)
        >>| fun x -> Output.Wrapper_wrap x
    | Wrapper_merge (last, wrapped) ->
        time ~logger "Wrapper.merge" (M.Wrapper.merge last wrapped)
        >>| fun x -> Output.Wrapper_merge x
    | Transaction_snark_of_signed_command
        (statement, user_command_in_block, sparse_ledger) ->
        let handler = unstage @@ Sparse_ledger.handler sparse_ledger in
        time ~logger "Transaction_snark.of_signed_command"
          (T.of_user_command ~init_stack:Mina_base.Pending_coinbase.Stack.empty
             ~statement user_command_in_block handler )
        >>| fun x -> Output.Transaction_snark_of_signed_command x
    | Transaction_snark_of_zkapp_command_segment (statement, witness, spec) ->
        time ~logger "Transaction_snark.of_zkapp_command_segment"
          (T.of_zkapp_command_segment_exn ~statement ~witness ~spec)
        >>| fun x -> Output.Transaction_snark_of_zkapp_command_segment x
    | Transaction_snark_merge (a, b) ->
        time ~logger "Transaction_snark.merge"
          (T.merge a b ~sok_digest:dummy_sok)
        >>| Or_error.ok_exn
        >>| fun x -> Output.Transaction_snark_merge x
    | Submit_deposit (pk, tr) ->
        time ~logger "Submit_deposit"
          (M.Outer.submit_deposit ~outer_public_key:pk ~deposit:tr)
        >>| fun x -> Output.Submit_deposit x
    | Submit_withdrawal tr ->
        time ~logger "Submit_withdrawal"
          (M.Inner.submit_withdrawal ~withdrawal:tr)
        >>| fun x -> Output.Submit_withdrawal x

  let run ~logger ~port =
    ignore
    @@ Tcp.Server.create (Tcp.Where_to_listen.of_port port)
         ~on_handler_error:`Ignore (fun s r w ->
           [%log info] "Accepted connection from %s"
             (Socket.Address.Inet.to_string s) ;
           let%bind () =
             Pipe.transfer' ~max_queue_length:1 (Reader.pipe r) (Writer.pipe w)
               ~f:
                 (Deferred.Queue.map ~how:`Sequential ~f:(fun input ->
                      Yojson.Safe.from_string input
                      |> Input.of_yojson
                      |> function
                      | Ok input -> (
                          match%bind
                            try_with (fun () -> prove ~logger input)
                          with
                          | Ok output ->
                              Output.to_yojson output |> Yojson.Safe.to_string
                              |> fun s -> String.concat [ s; "\n" ] |> return
                          | Error e ->
                              return (Exn.to_string e) )
                      | Error e ->
                          return e ) )
           in
           return
             ([%log info] "Closed connection from %s"
                (Socket.Address.Inet.to_string s) ) ) ;
    [%log info] "Listening on port %d\n" port ;
    Deferred.never ()
end
