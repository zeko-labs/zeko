open Transaction_snark_tests.Util
open Core_kernel
open Mina_base
open Signature_lib
module Impl = Pickles.Impls.Step
module Inner_curve = Snark_params.Tick.Inner_curve
module Nat = Pickles_types.Nat
module Local_state = Mina_state.Local_state
module Zkapp_command_segment = Transaction_snark.Zkapp_command_segment
module Statement = Transaction_snark.Statement
module U = Transaction_snark_tests.Util

module Txs = struct
  open Core
  open Mina_ledger
  open Currency
  open Mina_transaction
  open Mina_base

  let state_body = U.genesis_state_body

  let constraint_constants = U.constraint_constants

  let consensus_constants = U.consensus_constants

  let ledger_depth = U.ledger_depth

  let state_body_hash = U.genesis_state_body_hash

  (* For tests let's just monkey patch ledger and sparse ledger to freeze their
   * ledger_hashes. The nominal type is just so we don't mix this up in our
   * real code. *)
  module Ledger = struct
    include Ledger

    let merkle_root t = Frozen_ledger_hash.of_ledger_hash @@ merkle_root t

    let merkle_root_after_user_command_exn t ~txn_global_slot txn =
      let hash =
        merkle_root_after_user_command_exn
          ~constraint_constants:U.constraint_constants ~txn_global_slot t txn
      in
      Frozen_ledger_hash.of_ledger_hash hash
  end

  module Sparse_ledger = struct
    include Sparse_ledger

    let merkle_root t = Frozen_ledger_hash.of_ledger_hash @@ merkle_root t
  end

  let of_user_command' (sok_digest : Sok_message.Digest.t) ledger
      (user_command : Signed_command.With_valid_signature.t) init_stack
      pending_coinbase_stack_state state_body handler =
    let module T = (val Lazy.force U.snark_module) in
    let source = Ledger.merkle_root ledger in
    let current_global_slot =
      Mina_state.Protocol_state.Body.consensus_state state_body
      |> Consensus.Data.Consensus_state.global_slot_since_genesis
    in
    let target =
      Ledger.merkle_root_after_user_command_exn ledger
        ~txn_global_slot:current_global_slot user_command
    in
    let user_command_in_block =
      { Transaction_protocol_state.Poly.transaction = user_command
      ; block_data = state_body
      ; global_slot = current_global_slot
      }
    in
    let user_command_supply_increase = Currency.Amount.Signed.zero in
    Async.Thread_safe.block_on_async_exn (fun () ->
        let statement =
          let txn =
            Transaction.Command
              (User_command.Signed_command
                 (Signed_command.forget_check user_command) )
          in
          Transaction_snark.Statement.Poly.with_empty_local_state
            ~source_first_pass_ledger:source ~target_first_pass_ledger:target
            ~source_second_pass_ledger:target ~target_second_pass_ledger:target
            ~connecting_ledger_left:target ~connecting_ledger_right:target
            ~sok_digest
            ~fee_excess:(Or_error.ok_exn (Transaction.fee_excess txn))
            ~supply_increase:user_command_supply_increase
            ~pending_coinbase_stack_state
        in
        T.of_user_command ~init_stack ~statement user_command_in_block handler )

  module Pc_with_init_stack = struct
    type t =
      { pc : Transaction_snark.Pending_coinbase_stack_state.t
      ; init_stack : Pending_coinbase.Stack.t
      }
  end

  let test_base_and_merge ~state_hash_and_body1 ~state_hash_and_body2
      ~carryforward1 ~carryforward2 =
    let module T = (val Lazy.force U.snark_module) in
    Test_util.with_randomness 123456789 (fun () ->
        let wallets = U.Wallet.random_wallets () in
        (*let state_body = Lazy.force state_body in
          let state_body_hash = Lazy.force state_body_hash in*)
        let state_body_hash1, state_body1 = state_hash_and_body1 in
        let global_slot1 =
          Mina_state.Protocol_state.Body.consensus_state state_body1
          |> Consensus.Data.Consensus_state.global_slot_since_genesis
        in
        let state_body_hash2, state_body2 = state_hash_and_body2 in
        let global_slot2 =
          Mina_state.Protocol_state.Body.consensus_state state_body2
          |> Consensus.Data.Consensus_state.global_slot_since_genesis
        in
        Ledger.with_ledger ~depth:ledger_depth ~f:(fun ledger ->
            Array.iter wallets ~f:(fun { account; private_key = _ } ->
                Ledger.create_new_account_exn ledger
                  (Account.identifier account)
                  account ) ;
            let memo =
              Signed_command_memo.create_by_digesting_string_exn
                (Test_util.arbitrary_string
                   ~len:Signed_command_memo.max_digestible_string_length )
            in
            let t1 =
              U.Wallet.user_command_with_wallet wallets ~sender:0 ~receiver:1
                8_000_000_000
                (Fee.of_mina_int_exn @@ Random.int 20)
                Account.Nonce.zero memo
            in
            let t2 =
              U.Wallet.user_command_with_wallet wallets ~sender:1 ~receiver:2
                8_000_000_000
                (Fee.of_mina_int_exn @@ Random.int 20)
                Account.Nonce.zero memo
            in
            let sok_digest =
              Sok_message.create ~fee:Fee.zero
                ~prover:wallets.(0).account.public_key
              |> Sok_message.digest
            in
            let sparse_ledger =
              Sparse_ledger.of_ledger_subset_exn ledger
                (List.concat_map
                   ~f:(fun t ->
                     (* NB: Shouldn't assume the same next_available_token
                        for each command normally, but we know statically
                        that these are payments in this test.
                     *)
                     Signed_command.accounts_referenced
                       (Signed_command.forget_check t) )
                   [ t1; t2 ] )
            in
            let init_stack1 = Pending_coinbase.Stack.empty in
            let pending_coinbase_stack_state1 =
              (* No coinbase to add to the stack. *)
              let stack_with_state =
                Pending_coinbase.Stack.push_state state_body_hash1 global_slot1
                  init_stack1
              in
              (* Since protocol state body is added once per block, the
                 source would already have the state if [carryforward=true]
                 from the previous transaction in the sequence of
                 transactions in a block. We add state to [init_stack] and
                 then check that it is equal to the target.
              *)
              let source_stack, target_stack =
                if carryforward1 then (stack_with_state, stack_with_state)
                else (init_stack1, stack_with_state)
              in
              { Pc_with_init_stack.pc =
                  { source = source_stack; target = target_stack }
              ; init_stack = init_stack1
              }
            in
            let proof12 =
              of_user_command' sok_digest ledger t1
                pending_coinbase_stack_state1.init_stack
                pending_coinbase_stack_state1.pc state_body1
                (unstage @@ Sparse_ledger.handler sparse_ledger)
            in
            let current_global_slot =
              Mina_state.Protocol_state.Body.consensus_state state_body1
              |> Consensus.Data.Consensus_state.global_slot_since_genesis
            in
            let sparse_ledger, _ =
              Sparse_ledger.apply_user_command ~constraint_constants
                ~txn_global_slot:current_global_slot sparse_ledger t1
              |> Or_error.ok_exn
            in
            let pending_coinbase_stack_state2, state_body2 =
              let previous_stack = pending_coinbase_stack_state1.pc.target in
              let stack_with_state2 =
                Pending_coinbase.Stack.(
                  push_state state_body_hash2 global_slot2 previous_stack)
              in
              (* No coinbase to add. *)
              let source_stack, target_stack, init_stack, state_body2 =
                if carryforward2 then
                  (* Source and target already have the protocol state,
                     init_stack will be such that
                     [init_stack + state_body_hash1 = target = source].
                  *)
                  (previous_stack, previous_stack, init_stack1, state_body1)
                else
                  (* Add the new state such that
                     [previous_stack + state_body_hash2
                      = init_stack + state_body_hash2
                      = target].
                  *)
                  ( previous_stack
                  , stack_with_state2
                  , previous_stack
                  , state_body2 )
              in
              ( { Pc_with_init_stack.pc =
                    { source = source_stack; target = target_stack }
                ; init_stack
                }
              , state_body2 )
            in
            ignore
              ( Ledger.apply_user_command ~constraint_constants ledger
                  ~txn_global_slot:current_global_slot t1
                |> Or_error.ok_exn
                : Ledger.Transaction_applied.Signed_command_applied.t ) ;
            [%test_eq: Frozen_ledger_hash.t]
              (Ledger.merkle_root ledger)
              (Sparse_ledger.merkle_root sparse_ledger) ;
            let proof23 =
              of_user_command' sok_digest ledger t2
                pending_coinbase_stack_state2.init_stack
                pending_coinbase_stack_state2.pc state_body2
                (unstage @@ Sparse_ledger.handler sparse_ledger)
            in
            let current_global_slot =
              Mina_state.Protocol_state.Body.consensus_state state_body2
              |> Consensus.Data.Consensus_state.global_slot_since_genesis
            in
            let sparse_ledger, _ =
              Sparse_ledger.apply_user_command ~constraint_constants
                ~txn_global_slot:current_global_slot sparse_ledger t2
              |> Or_error.ok_exn
            in
            ignore
              ( Ledger.apply_user_command ledger ~constraint_constants
                  ~txn_global_slot:current_global_slot t2
                |> Or_error.ok_exn
                : Mina_transaction_logic.Transaction_applied
                  .Signed_command_applied
                  .t ) ;
            [%test_eq: Frozen_ledger_hash.t]
              (Ledger.merkle_root ledger)
              (Sparse_ledger.merkle_root sparse_ledger) ;
            let proof13 =
              Async.Thread_safe.block_on_async_exn (fun () ->
                  T.merge ~sok_digest proof12 proof23 )
              |> Or_error.ok_exn
            in
            Async.Thread_safe.block_on_async (fun () ->
                T.verify_against_digest proof13 )
            |> Result.ok_exn |> Or_error.ok_exn ) )

  let ex1 =
    let state_hash_and_body1 = (state_body_hash, state_body) in
    test_base_and_merge ~state_hash_and_body1
      ~state_hash_and_body2:state_hash_and_body1 ~carryforward1:true
      ~carryforward2:true

  (*
     let ex2 =
       let state_hash_and_body1 = (state_body_hash, state_body) in
       test_base_and_merge ~state_hash_and_body1
         ~state_hash_and_body2:state_hash_and_body1 ~carryforward1:false
         ~carryforward2:true

     let ex3 =
       let state_hash_and_body1 =
         let open Staged_ledger_diff in
         let state_body0 =
           Mina_state.Protocol_state.negative_one
             ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
             ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
             ~constraint_constants ~consensus_constants ~genesis_body_reference
           |> Mina_state.Protocol_state.body
         in
         let state_body_hash0 =
           Mina_state.Protocol_state.Body.hash state_body0
         in
         (state_body_hash0, state_body0)
       in
       let state_hash_and_body2 = (state_body_hash, state_body) in
       test_base_and_merge ~state_hash_and_body1 ~state_hash_and_body2
         ~carryforward1:true ~carryforward2:false

     let ex4 =
       let state_hash_and_body1 =
         let state_body0 =
           let open Staged_ledger_diff in
           Mina_state.Protocol_state.negative_one
             ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
             ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
             ~constraint_constants ~consensus_constants ~genesis_body_reference
           |> Mina_state.Protocol_state.body
         in
         let state_body_hash0 =
           Mina_state.Protocol_state.Body.hash state_body0
         in
         (state_body_hash0, state_body0)
       in
       let state_hash_and_body2 = (state_body_hash, state_body) in
       test_base_and_merge ~state_hash_and_body1 ~state_hash_and_body2
         ~carryforward1:false ~carryforward2:false
  *)
end

let gen_keys () =
  let kp = Keypair.create () in
  (Public_key.compress kp.public_key, kp.private_key)

let fee_to_create n =
  Genesis_constants.Constraint_constants.compiled.account_creation_fee
  |> Currency.Amount.of_fee
  |> (fun x -> Currency.Amount.scale x n)
  |> Option.value_exn

let fee_to_create_signed n =
  fee_to_create n |> Currency.Amount.Signed.of_unsigned
  |> Currency.Amount.Signed.negate

let int_to_amount amt =
  let magnitude, sgn = if amt < 0 then (-amt, Sgn.Neg) else (amt, Sgn.Pos) in
  { Currency.Signed_poly.magnitude =
      Currency.Amount.of_nanomina_int_exn magnitude
  ; sgn
  }

let%test_module "Rollup test" =
  ( module struct
    let () = Base.Backtrace.elide := false

    let pk, sk = gen_keys ()

    let token_id = Token_id.default

    let account_id = Account_id.create pk token_id

    (* let owned_token_id = Account_id.derive_token_id ~owner:account_id *)

    module T = Transaction_snark.Make (struct
      let constraint_constants = U.constraint_constants

      let proof_level = Genesis_constants.Proof_level.Full
    end)

    module M = Zkapps_rollup.Make (struct
      let tag = T.tag
    end)

    let mint_to_keys = gen_keys ()

    module Account_updates = struct
      let deploy ~balance_change =
        Zkapps_examples.Deploy_account_update.full ~balance_change
          ~access:Either pk token_id M.vk

      let init =
        let account_update, () =
          Async.Thread_safe.block_on_async_exn
            (M.init
               { public_key = pk
               ; token_id
               ; may_use_token = Inherit_from_parent
               } )
        in
        account_update

      (*
             let step txn_snark txn_snark_proof =
               let account_update, () =
                 Async.Thread_safe.block_on_async_exn
                   (M.step
                      { public_key = pk
                      ; token_id
                      ; may_use_token = Inherit_from_parent
                      ; txn_snark
                      ; txn_snark_proof
                      } )
               in
               account_update
      *)
    end

    let signers = [| (pk, sk); mint_to_keys |]

    let initialize_ledger ledger =
      let balance =
        let open Currency.Balance in
        let add_amount x y = add_amount y x in
        zero
        |> add_amount (Currency.Amount.of_nanomina_int_exn 500)
        |> Option.value_exn
        |> add_amount (fee_to_create 50)
        |> Option.value_exn
      in
      let account = Account.create account_id balance in
      let _, loc =
        Ledger.get_or_create_account ledger account_id account
        |> Or_error.ok_exn
      in
      loc

    let finalize_ledger loc ledger = Ledger.get ledger loc

    let%test_unit "Initialize and mint" =
      let account =
        []
        (* |> Zkapp_command.Call_forest.cons_tree Account_updates.mint *)
        (* How do we set fields not constrained? *)
        |> Zkapp_command.Call_forest.cons_tree Account_updates.init
        |> Zkapp_command.Call_forest.cons
             (Account_updates.deploy ~balance_change:(fee_to_create_signed 1))
        |> test_zkapp_command ~fee_payer_pk:pk ~signers ~initialize_ledger
             ~finalize_ledger
      in
      ignore account
  end )
