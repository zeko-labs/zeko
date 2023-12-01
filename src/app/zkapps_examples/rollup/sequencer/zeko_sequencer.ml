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

module Sequencer = struct
  type config_t =
    { max_pool_size : int; committment_period_sec : float; db_dir : string }

  type t =
    { db : L.Db.t
    ; archive : Archive.t
    ; mutable slot : int
    ; config : config_t
    ; da_config : Da_layer.config_t
    }

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

  let keypair = Signature_lib.Keypair.create ()

  let sok_digest =
    Sok_message.digest
    @@ Sok_message.create ~fee:Currency.Fee.zero
         ~prover:(Signature_lib.Public_key.compress keypair.public_key)

  module Snark_queue = struct
    let q = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1

    let last = ref None

    let staged_commands = ref []

    let last_committed_ledger_hash = ref None

    let queue_size () = Throttle.num_jobs_waiting_to_start q

    let wrap_and_merge txn_snark command =
      let%bind wrapped = time "Wrapper.wrap" (M.Wrapper.wrap txn_snark) in
      let%bind final_snark =
        match !last with
        | Some last' ->
            time "Wrapper.merge" (M.Wrapper.merge last' wrapped)
        | None ->
            return wrapped
      in
      last := Some final_snark ;
      staged_commands := !staged_commands @ [ command ] ;
      return ()

    let prove_signed_command ~sparse_ledger ~user_command_in_block ~statement =
      Throttle.enqueue q (fun () ->
          let handler =
            unstage @@ Mina_ledger.Sparse_ledger.handler sparse_ledger
          in
          let%bind txn_snark =
            time "Transaction_snark.of_signed_command"
              (T.of_user_command
                 ~init_stack:Mina_base.Pending_coinbase.Stack.empty ~statement
                 user_command_in_block handler )
          in
          wrap_and_merge txn_snark
            (User_command.Signed_command user_command_in_block.transaction) )

    let prove_zkapp_command ~witnesses ~zkapp_command =
      Throttle.enqueue q (fun () ->
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
                        (T.of_zkapp_command_segment_exn ~statement ~witness
                           ~spec )
                    in
                    let%bind merged =
                      time "Transaction_snark.merge"
                        (T.merge curr prev ~sok_digest)
                    in
                    return (Or_error.ok_exn merged) )
          in
          wrap_and_merge txn_snark (User_command.Zkapp_command zkapp_command) )

    let commit t =
      Throttle.enqueue q (fun () ->
          match List.is_empty !staged_commands with
          | true ->
              print_endline "Nothing to commit" ;
              return ()
          | false ->
              print_endline "Committing..." ;

              let ledger_hash = (Option.value_exn !last).stmt.target_ledger in
              let batch_id =
                Frozen_ledger_hash0.to_decimal_string ledger_hash
              in
              let previous_batch_id =
                Frozen_ledger_hash0.to_decimal_string
                  (Option.value
                     ~default:(Frozen_ledger_hash0.of_decimal_string "0")
                     !last_committed_ledger_hash )
              in

              let%bind () =
                Da_layer.post_batch t.da_config ~commands:!staged_commands
                  ~batch_id ~previous_batch_id
              in
              print_endline ("Posted batch " ^ batch_id) ;

              (* Add proving here *)
              staged_commands := [] ;
              last := None ;
              last_committed_ledger_hash := Some ledger_hash ;
              return () )
  end

  let create ~max_pool_size ~committment_period_sec ~da_contract_address ~db_dir
      =
    let db =
      L.Db.create ~directory_name:db_dir
        ~depth:constraint_constants.ledger_depth ()
    in
    { db
    ; archive = Archive.create ~kvdb:(L.Db.kvdb db)
    ; slot = 0
    ; config = { max_pool_size; committment_period_sec; db_dir }
    ; da_config = { da_contract_address }
    }

  let run_committer t =
    every (Time_ns.Span.of_sec t.config.committment_period_sec) (fun () ->
        don't_wait_for @@ Snark_queue.commit t )

  let add_account t public_key token_id balance =
    let account_id = Account_id.create public_key token_id in
    let account =
      Account.create account_id (Currency.Balance.of_uint64 balance)
    in
    ( L.Db.get_or_create_account t.db account_id account |> Or_error.ok_exn
      : [ `Added | `Existed ] * L.Db.Location.t )
    |> ignore

  let get_account t public_key token_id =
    let account_id = Account_id.create public_key token_id in
    let%bind.Option location = L.Db.location_of_account t.db account_id in
    L.Db.get t.db location

  let inferr_nonce t public_key =
    match get_account t public_key Token_id.default with
    | Some account ->
        account.nonce
    | None ->
        Unsigned.UInt32.zero

  let get_root t () = L.Db.merkle_root t.db

  let apply_signed_command t (signed_command : Signed_command.t) =
    let () =
      match Snark_queue.queue_size () with
      | x when x >= t.config.max_pool_size ->
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
      Mina_numbers.Global_slot_since_genesis.of_int t.slot
    in
    let curr_global_slot =
      Mina_numbers.Global_slot_since_genesis.of_int (t.slot + 1)
    in

    let l = L.of_database t.db in
    let source_ledger_hash = L.merkle_root l in
    let sparse_ledger =
      Mina_ledger.Sparse_ledger.of_ledger_subset_exn l
        (Signed_command.accounts_referenced signed_command)
    in
    let%bind.Result txn_applied =
      Result.( >>= )
        (L.apply_transaction_first_pass ~constraint_constants
           ~global_slot:curr_global_slot
           ~txn_state_view:(Mina_state.Protocol_state.Body.view state_body)
           l (Command (Signed_command signed_command)) )
        (L.apply_transaction_second_pass l)
    in
    L.Mask.Attached.commit l ;
    t.slot <- Mina_numbers.Global_slot_since_genesis.to_int curr_global_slot ;

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
    @@ Snark_queue.prove_signed_command ~sparse_ledger ~user_command_in_block
         ~statement ;

    Result.return txn_applied

  let apply_zkapp_command t (zkapp_command : Zkapp_command.t) =
    let () =
      match Snark_queue.queue_size () with
      | x when x >= t.config.max_pool_size ->
          failwith "Maximum pool size reached, try later"
      | _ ->
          ()
    in
    let prev_global_slot =
      Mina_numbers.Global_slot_since_genesis.of_int t.slot
    in
    let curr_global_slot =
      Mina_numbers.Global_slot_since_genesis.of_int (t.slot + 1)
    in
    let%bind.Result first_pass_ledger, second_pass_ledger, txn_applied =
      let l = L.of_database t.db in
      let accounts_referenced =
        Zkapp_command.accounts_referenced zkapp_command
      in

      let first_pass_ledger =
        Mina_ledger.Sparse_ledger.of_ledger_subset_exn l accounts_referenced
      in
      let%bind.Result partialy_applied_txn =
        L.apply_transaction_first_pass ~constraint_constants
          ~global_slot:curr_global_slot
          ~txn_state_view:(Mina_state.Protocol_state.Body.view state_body)
          l (Command (Zkapp_command zkapp_command))
      in

      let second_pass_ledger =
        Mina_ledger.Sparse_ledger.of_ledger_subset_exn l accounts_referenced
      in
      let%map.Result txn_applied =
        let%bind.Result txn_applied =
          L.apply_transaction_second_pass l partialy_applied_txn
        in
        match L.Transaction_applied.transaction_status txn_applied with
        | Failed failure ->
            Error
              ( Error.of_string @@ Yojson.Safe.to_string
              @@ Transaction_status.Failure.Collection.to_yojson failure )
        | Applied ->
            Ok txn_applied
      in

      L.Mask.Attached.commit l ;
      t.slot <- Mina_numbers.Global_slot_since_genesis.to_int curr_global_slot ;

      Zkapp_command.(
        Call_forest.iteri (account_updates zkapp_command) ~f:(fun _ update ->
            let account =
              Option.value_exn
              @@ get_account t
                   (Account_update.public_key update)
                   (Account_update.token_id update)
            in
            Archive.add_account_update t.archive update account
              (Some
                 Archive.Transaction_info.
                   { status = Applied
                   ; hash =
                       Mina_transaction.Transaction_hash.hash_command
                         (Zkapp_command zkapp_command)
                   ; memo = Zkapp_command.memo zkapp_command
                   ; authorization_kind =
                       Account_update.Body.authorization_kind
                       @@ Account_update.body update
                   } ) )) ;

      (first_pass_ledger, second_pass_ledger, txn_applied)
    in

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
    let witnesses =
      Transaction_snark.zkapp_command_witnesses_exn ~constraint_constants
        ~global_slot:curr_global_slot ~state_body
        ~fee_excess:
          ( Currency.Amount.Signed.of_unsigned
          @@ Currency.Amount.of_fee (Zkapp_command.fee zkapp_command) )
        [ ( `Pending_coinbase_init_stack Pending_coinbase.Stack.empty
          , `Pending_coinbase_of_statement pc
          , `Sparse_ledger first_pass_ledger
          , `Sparse_ledger second_pass_ledger
          , `Connecting_ledger_hash
              (Sparse_ledger_base.merkle_root second_pass_ledger)
          , zkapp_command )
        ]
    in
    don't_wait_for @@ Snark_queue.prove_zkapp_command ~witnesses ~zkapp_command ;

    Result.return txn_applied
end

include Sequencer
