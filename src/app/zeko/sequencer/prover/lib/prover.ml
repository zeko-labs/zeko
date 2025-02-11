open Core_kernel
open Async
open Mina_base
open Mina_ledger
open Signature_lib

(* Only for yojson serialization of Field *)
module Field = Data_hash.Make_full_size (struct
  let description = "Field"

  let version_byte = '\x00'
end)

let constraint_constants = Genesis_constants.Compiled.constraint_constants

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
    | Ping
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
    | Process_deposit of
        ( bool
        * Field.t
        * Zkapps_rollup.TR.t list
        * Zkapps_rollup.TR.t list
        * Zkapps_rollup.TR.t )
    | Process_withdrawal of
        ( Public_key.Compressed.t
        * bool
        * Field.t
        * Zkapps_rollup.TR.t list
        * Zkapps_rollup.TR.t list
        * Zkapps_rollup.TR.t )
    | Outer_step of
        ( Zkapps_rollup.t
        * Public_key.Compressed.t
        * Zkapps_rollup.TR.t list
        * Zkapps_rollup.TR.t list
        * Sparse_ledger.t
        * Sparse_ledger.t )
    | Inner_step of Field.t
  [@@deriving yojson]
end

let asd = Ledger_hash.to_yojson

module Output = struct
  type t =
    | Pong
    | Wrapper_wrap of Zkapps_rollup.t
    | Wrapper_merge of Zkapps_rollup.t
    | Transaction_snark_of_signed_command of Transaction_snark.t
    | Transaction_snark_of_zkapp_command_segment of Transaction_snark.t
    | Transaction_snark_merge of Transaction_snark.t
    | Submit_deposit of Zeko_util.call_forest_tree
    | Submit_withdrawal of Zeko_util.call_forest_tree
    | Process_deposit of Zeko_util.call_forest
    | Process_withdrawal of Zeko_util.call_forest
    | Outer_step of Zeko_util.call_forest_tree
    | Inner_step of Zeko_util.call_forest_tree
  [@@deriving yojson]
end

module Make (T : Transaction_snark.S) (M : Zkapps_rollup.S) = struct
  let prove ~logger : Input.t -> Output.t Deferred.t = function
    | Ping ->
        return Output.Pong
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
    | Submit_deposit (outer_public_key, deposit) ->
        time ~logger "Outer.Submit_deposit"
          (M.Outer.submit_deposit ~outer_public_key ~deposit)
        >>| fun x -> Output.Submit_deposit x
    | Submit_withdrawal withdrawal ->
        time ~logger "Inner.Submit_withdrawal"
          (M.Inner.submit_withdrawal ~withdrawal)
        >>| fun x -> Output.Submit_withdrawal x
    | Process_deposit (is_new, pointer, before, after, deposit) ->
        time ~logger "Inner.Process_deposit"
          (M.Inner.process_deposit ~is_new ~pointer ~before ~after ~deposit)
        >>| fun (_, x) -> Output.Process_deposit x
    | Process_withdrawal
        (outer_public_key, is_new, pointer, before, after, withdrawal) ->
        time ~logger "Outer.Process_withdrawal"
          (M.Outer.process_withdrawal ~outer_public_key ~is_new ~pointer ~before
             ~after ~withdrawal )
        >>| fun (_, x) -> Output.Process_withdrawal x
    | Outer_step
        ( last
        , outer_public_key
        , new_deposits
        , unprocessed_deposits
        , old_inner_ledger
        , new_inner_ledger ) ->
        time ~logger "Outer.step"
          (M.Outer.step last ~outer_public_key ~new_deposits
             ~unprocessed_deposits ~old_inner_ledger ~new_inner_ledger )
        >>| fun x -> Output.Outer_step x
    | Inner_step all_deposits ->
        time ~logger "Inner.step" (M.Inner.step ~all_deposits)
        >>| fun x -> Output.Inner_step x

  let run ~logger ~port =
    ignore
    @@ Tcp.Server.create (Tcp.Where_to_listen.of_port port)
         ~on_handler_error:`Ignore (fun s r w ->
           [%log info] "Accepted connection from %s"
             (Socket.Address.Inet.to_string s) ;
           let rec loop () =
             match%bind
               Reader.really_read_line ~wait_time:(Time.Span.of_sec 30.) r
             with
             | None ->
                 return ()
             | Some input ->
                 Yojson.Safe.from_string input
                 |> Input.of_yojson
                 |> (function
                      | Ok input -> (
                          match%bind
                            try_with (fun () -> prove ~logger input)
                          with
                          | Ok output ->
                              return
                                ( Yojson.Safe.to_string
                                @@ Output.to_yojson output )
                          | Error e ->
                              return (Exn.to_string e) )
                      | Error e ->
                          return e )
                 >>| Writer.write_line w
                 >>= fun () -> loop ()
           in
           let%bind () = loop () in
           return
             ([%log info] "Closed connection from %s"
                (Socket.Address.Inet.to_string s) ) ) ;
    [%log info] "Listening on port %d\n" port ;
    Deferred.never ()
end
