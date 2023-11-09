open Transaction_snark_tests.Util
open Core_kernel
open Mina_base
open Signature_lib
module Field = Snark_params.Tick.Run.Field
module Impl = Pickles.Impls.Step
module Inner_curve = Snark_params.Tick.Inner_curve
module Nat = Pickles_types.Nat
module Local_state = Mina_state.Local_state
module Zkapp_command_segment = Transaction_snark.Zkapp_command_segment
module Statement = Transaction_snark.Statement
module U = Transaction_snark_tests.Util

(* Adapted from transaction_snark/test/transaction_union *)
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

  let make_some_txn () =
    let module T = (val Lazy.force U.snark_module) in
    Test_util.with_randomness 502381708 (fun () ->
        let wallets = U.Wallet.random_wallets () in
        let global_slot =
          Mina_state.Protocol_state.Body.consensus_state state_body
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
                     Signed_command.accounts_referenced
                       (Signed_command.forget_check t) )
                   [ t1; t2 ] )
            in
            let init_stack1 = Pending_coinbase.Stack.empty in
            let pending_coinbase_stack_state1 =
              (* No coinbase to add to the stack. *)
              let stack_with_state =
                Pending_coinbase.Stack.push_state state_body_hash global_slot
                  init_stack1
              in
              let source_stack, target_stack =
                (stack_with_state, stack_with_state)
              in
              { Pc_with_init_stack.pc =
                  { source = source_stack; target = target_stack }
              ; init_stack = init_stack1
              }
            in
            let proof12 =
              of_user_command' sok_digest ledger t1
                pending_coinbase_stack_state1.init_stack
                pending_coinbase_stack_state1.pc state_body
                (unstage @@ Sparse_ledger.handler sparse_ledger)
            in
            let stmt = Transaction_snark.statement proof12 in
            print_endline
              (Frozen_ledger_hash0.to_decimal_string stmt.connecting_ledger_left) ;
            print_endline
              (Frozen_ledger_hash0.to_decimal_string
                 stmt.connecting_ledger_right ) ;
            print_endline
              (Frozen_ledger_hash0.to_decimal_string
                 stmt.source.first_pass_ledger ) ;
            print_endline
              (Frozen_ledger_hash0.to_decimal_string
                 stmt.target.first_pass_ledger ) ;
            print_endline
              (Frozen_ledger_hash0.to_decimal_string
                 stmt.source.second_pass_ledger ) ;
            print_endline
              (Frozen_ledger_hash0.to_decimal_string
                 stmt.target.second_pass_ledger ) ;
            proof12 ) )
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
      let deploy, account_update =
        Zkapps_rollup.Deploy.deploy pk M.vk M.Inner.vk

      (* Add step *)
    end

    let signers = [| (pk, sk); mint_to_keys |]

    let initialize_ledger ledger =
      let balance =
        let open Currency.Balance in
        let add_amount x y = add_amount y x in
        zero
        |> add_amount (Currency.Amount.of_nanomina_int_exn 5000)
        |> Option.value_exn
        |> add_amount (fee_to_create 500)
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
      let txn = Txs.make_some_txn () in
      let stmt = Transaction_snark.statement txn in
      let account =
        []
        (*
        |> Zkapp_command.Call_forest.cons_tree (Account_updates.step txn)
        |> Zkapp_command.Call_forest.cons
             (Account_updates.deploy
                ~balance_change:Account_update.Body.dummy.balance_change
                stmt.source.first_pass_ledger )
        |> test_zkapp_command ~fee_payer_pk:pk ~signers ~initialize_ledger
             ~finalize_ledger
                                       *)
      in
      ignore account
  end )
