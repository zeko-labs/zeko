open Core_kernel
open Mina_base
open Async_kernel
module L = Mina_ledger.Ledger

let time label (d : 'a Deferred.t) =
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  printf "%s: %s\n%!" label (Time.Span.to_string_hum @@ Time.diff stop start) ;
  return x

module Sequencer = struct
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

  let l = L.create ~depth:constraint_constants.ledger_depth ()

  let slot = ref 0

  let prove_signed_command ~sparse_ledger ~user_command_in_block ~statement =
    let handler = unstage @@ Mina_ledger.Sparse_ledger.handler sparse_ledger in

    let%bind next =
      time "Transaction_snark.of_user_command"
        (T.of_user_command ~init_stack:Mina_base.Pending_coinbase.Stack.empty
           ~statement user_command_in_block handler )
    in
    let%bind _wrapped = time "Wrapper.wrap" (M.Wrapper.wrap next) in
    return ()

  let apply_signed_command
      (signed_command : Signed_command.With_valid_signature.t) =
    let txn =
      Mina_transaction.Transaction.Command
        (User_command.Signed_command
           (Signed_command.forget_check signed_command) )
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
        ( Signed_command.accounts_referenced
        @@ Signed_command.forget_check signed_command )
    in

    let txn_applied =
      Result.( >>= )
        (L.apply_transaction_first_pass ~constraint_constants
           ~global_slot:curr_global_slot
           ~txn_state_view:(Mina_state.Protocol_state.Body.view state_body)
           l
           (Command (Signed_command (Signed_command.forget_check signed_command))
           ) )
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
           ~prover:
             Signature_lib.(Public_key.compress (Keypair.create ()).public_key)
    in

    let user_command_in_block =
      { Transaction_protocol_state.Poly.transaction = signed_command
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

    Async_kernel.don't_wait_for
    @@ prove_signed_command ~sparse_ledger ~user_command_in_block ~statement
end

let ok_exn x =
  let open Ppx_deriving_yojson_runtime.Result in
  match x with Ok x -> x | Error e -> failwith e

let () =
  Callback.register "applySignedCommand" (fun (signed_command : string) ->
      let signed_command =
        Signed_command.With_valid_signature.of_yojson
        @@ Yojson.Safe.from_string signed_command
        |> ok_exn
      in
      Sequencer.apply_signed_command signed_command )
