open Core_kernel
open Async
open Async_kernel
open Mina_base
module L = Mina_ledger.Ledger

let time label (d : 'a Deferred.t) =
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  printf "%s: %s\n%!" label (Time.Span.to_string_hum @@ Time.diff stop start) ;
  return x

module Make (Args : sig
  val max_pool_size : int

  val committment_period_sec : float
end) =
struct
  let constraint_constants = Genesis_constants.Constraint_constants.compiled

  let genesis_constants = Genesis_constants.compiled

  let consensus_constants =
    Consensus.Constants.create ~constraint_constants
      ~protocol_constants:genesis_constants.protocol

  let state_body =
    (* FIXME: Use the correct values *)
    let compile_time_genesis =
      Mina_state.Genesis_protocol_state.t
        ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
        ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
        ~constraint_constants ~consensus_constants
        ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference
    in
    Mina_state.Protocol_state.body compile_time_genesis.data

  module T = Transaction_snark.Make (struct
    let constraint_constants = constraint_constants

    let proof_level = Genesis_constants.Proof_level.Full
  end)

  module M = Zkapps_rollup.Make (struct
    let tag = T.tag
  end)

  module Snark_queue = struct
    let q = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1

    let last = ref None

    let staged_commands = ref []

    let committed_commands = ref []

    let queue_size () = Throttle.num_jobs_waiting_to_start q

    let prove_command ~sparse_ledger ~user_command_in_block ~statement =
      Throttle.enqueue q (fun () ->
          let handler =
            unstage @@ Mina_ledger.Sparse_ledger.handler sparse_ledger
          in
          let%bind txn_snark =
            time "Transaction_snark.of_user_command"
              (T.of_user_command
                 ~init_stack:Mina_base.Pending_coinbase.Stack.empty ~statement
                 user_command_in_block handler )
          in
          let%bind wrapped = time "Wrapper.wrap" (M.Wrapper.wrap txn_snark) in
          let%bind final_snark =
            match !last with
            | Some last' ->
                time "Wrapper.merge" (M.Wrapper.merge last' wrapped)
            | None ->
                return wrapped
          in
          last := Some final_snark ;
          staged_commands :=
            user_command_in_block.transaction :: !staged_commands ;
          return () )

    let commit () =
      Throttle.enqueue q (fun () ->
          match List.is_empty !staged_commands with
          | true ->
              print_endline "Nothing to commit" ;
              return ()
          | false ->
              print_endline "Committing..." ;
              (* Add proving here *)
              committed_commands :=
                List.append !staged_commands !committed_commands ;
              staged_commands := [] ;
              return () )
  end

  let l = L.create ~depth:constraint_constants.ledger_depth ()

  let slot = ref 0

  let keypair = Signature_lib.Keypair.create ()

  let run_committer () =
    every (Time_ns.Span.of_sec Args.committment_period_sec) (fun () ->
        don't_wait_for @@ Snark_queue.commit () )

  let add_account ?(token_id = Token_id.default) public_key balance =
    let account_id = Account_id.create public_key token_id in
    let account =
      Account.create account_id (Currency.Balance.of_uint64 balance)
    in
    L.create_new_account_exn l account_id account

  let get_account ?(token_id = Token_id.default) public_key =
    let module Let_syntax = Option in
    let account_id = Account_id.create public_key token_id in

    let%bind location = L.location_of_account l account_id in
    L.get l location

  let get_root () = L.merkle_root l

  let apply_signed_command (signed_command : Signed_command.t) =
    let () =
      match Snark_queue.queue_size () with
      | x when x >= Args.max_pool_size ->
          failwith "Maximum pool size reached, try later"
      | _ ->
          ()
    in
    let with_valid_signature =
      match Signed_command.check_only_for_signature signed_command with
      | Some x ->
          x
      | None ->
          failwith "Signature check failed"
    in
    let txn =
      Mina_transaction.Transaction.Command
        (User_command.Signed_command signed_command)
    in
    let prev_global_slot =
      Mina_numbers.Global_slot_since_genesis.of_int !slot
    in
    slot := !slot + 1 ;
    let curr_global_slot =
      Mina_numbers.Global_slot_since_genesis.of_int !slot
    in
    let source_ledger_hash = L.merkle_root l in
    let sparse_ledger =
      Mina_ledger.Sparse_ledger.of_ledger_subset_exn l
        (Signed_command.accounts_referenced signed_command)
    in

    let txn_applied =
      Result.( >>= )
        (L.apply_transaction_first_pass ~constraint_constants
           ~global_slot:curr_global_slot
           ~txn_state_view:(Mina_state.Protocol_state.Body.view state_body)
           l (Command (Signed_command signed_command)) )
        (L.apply_transaction_second_pass l)
      |> Or_error.ok_exn
    in

    let target_ledger_hash = L.merkle_root l in
    let pc : Transaction_snark.Pending_coinbase_stack_state.t =
      (* No coinbase to add to the stack. *)
      let stack_with_state global_slot =
        Pending_coinbase.Stack.push_state
          (Mina_state.Protocol_state.Body.hash state_body)
          global_slot Pending_coinbase.Stack.empty
      in
      { source = stack_with_state prev_global_slot
      ; target = stack_with_state curr_global_slot
      }
    in
    let sok_digest =
      Sok_message.digest
      @@ Sok_message.create ~fee:Currency.Fee.zero
           ~prover:(Signature_lib.Public_key.compress keypair.public_key)
    in

    let user_command_in_block =
      { Transaction_protocol_state.Poly.transaction = with_valid_signature
      ; block_data = state_body
      ; global_slot = curr_global_slot
      }
    in
    let (statement : Transaction_snark.Statement.With_sok.t) =
      Transaction_snark.Statement.Poly.with_empty_local_state
        ~source_first_pass_ledger:source_ledger_hash
        ~target_first_pass_ledger:target_ledger_hash
        ~source_second_pass_ledger:target_ledger_hash
        ~target_second_pass_ledger:target_ledger_hash
        ~connecting_ledger_left:target_ledger_hash
        ~connecting_ledger_right:target_ledger_hash ~sok_digest
        ~fee_excess:
          (Mina_transaction.Transaction.fee_excess txn |> Or_error.ok_exn)
        ~supply_increase:
          (L.Transaction_applied.supply_increase txn_applied |> Or_error.ok_exn)
        ~pending_coinbase_stack_state:pc
    in

    don't_wait_for
    @@ Snark_queue.prove_command ~sparse_ledger ~user_command_in_block
         ~statement ;

    ( Mina_transaction.Transaction_hash.hash_command
        (Signed_command signed_command)
    , Signed_command.to_base64 signed_command )
end
