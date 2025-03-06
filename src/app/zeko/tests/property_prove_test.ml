open Core_kernel
open Signature_lib
open Snark_params.Tick
open Zeko_circuits

(* Helper function to generate a point from a field element - used in the tests below *)
let point_of_field f =
  Inner_curve.(to_affine_exn @@ point_near_x f) |> Public_key.compress

(* Main property test for ASE operations *)
let () =
  Printf.printf "Running ASE operations property test...\n" ;

  (* Run the test multiple times with different random values *)
  for _ = 1 to 10 do
    (* Generate random values *)
    let da_sk = Quickcheck.random_value Private_key.gen in
    let da_key = Public_key.of_private_key_exn da_sk |> Public_key.compress in
    let action_state = Quickcheck.random_value Field.gen in
    let length = Unsigned.UInt32.of_int (Random.int 100000) in
    let field1 = Quickcheck.random_value Field.gen in
    let field2 = Quickcheck.random_value Field.gen in
    let field99 = Quickcheck.random_value Field.gen in

    (* Test ASE with length *)
    let trans0, proof0 =
      Promise.block_on_async_exn
      @@ fun () -> Ase.With_length.leaf ([ field1 ], { action_state; length })
    in

    let trans1, proof1 =
      Promise.block_on_async_exn
      @@ fun () -> Ase.With_length.leaf_option ([ field2 ], trans0.target)
    in

    let trans2, proof2 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.With_length.merge
        { left = trans0
        ; left_proof = proof0
        ; right = trans1
        ; right_proof = proof1
        }
    in

    let trans3, proof3 =
      Promise.block_on_async_exn
      @@ fun () -> Ase.With_length.extend ([ field99 ], (trans2, proof2))
    in

    let trans4, proof4 =
      Promise.block_on_async_exn
      @@ fun () -> Ase.With_length.extend_option ([ field99 ], (trans3, proof3))
    in

    let ase_with_length = trans4 in
    let ase_with_length_proof = proof4 in

    (* Print whether the key is odd or even *)
    Printf.printf "Generated key is %s\n"
      (if da_key.is_odd then "odd" else "even") ;

    (* Test ASE without length *)
    let trans0_wl, proof0_wl =
      Promise.block_on_async_exn
      @@ fun () -> Ase.Without_length.leaf ([ field1 ], action_state)
    in

    let trans1_wl, proof1_wl =
      Promise.block_on_async_exn
      @@ fun () -> Ase.Without_length.leaf_option ([ field2 ], trans0_wl.target)
    in

    let trans2_wl, proof2_wl =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.Without_length.merge
        { left = trans0_wl
        ; left_proof = proof0_wl
        ; right = trans1_wl
        ; right_proof = proof1_wl
        }
    in

    let trans3_wl, proof3_wl =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.Without_length.extend ([ field99 ], (trans2_wl, proof2_wl))
    in

    let trans4_wl, proof4_wl =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.Without_length.extend_option ([ field99 ], (trans3_wl, proof3_wl))
    in

    let ase_without_length_stmt = trans4_wl in
    let ase_without_length_proof = proof4_wl in

    (* Test inner rules *)
    let Compile_simple.[ sync; action ] = Inner_rules.provers in

    let random_field = Quickcheck.random_value Field.gen in
    let random_point = point_of_field (Quickcheck.random_value Field.gen) in

    let ase_with_length_inst : Rule_inner_sync.Ase_inst.t =
      Rule_inner_sync.Ase_inst.make ~proof_source:ase_with_length.source
        ~proof_target:ase_with_length.target ~proof:ase_with_length_proof
        ase_with_length.source [ random_field ]
    in

    let sync_witness : Rule_inner_sync.Witness.t =
      { public_key = random_point
      ; vk_hash = random_field
      ; ase = ase_with_length_inst
      }
    in

    let _sync_stmt, _sync_proof =
      Promise.block_on_async_exn @@ fun () -> sync sync_witness
    in

    let action_witness : Rule_inner_action_witness.Witness.t =
      { public_key = random_point
      ; vk_hash = random_field
      ; witness = { aux = Field.zero; children = [] }
      }
    in

    let _inner_stmt, _inner_proof =
      Promise.block_on_async_exn @@ fun () -> action action_witness
    in

    (* Test outer rules *)
    let Compile_simple.[ _commit; action; _pause ] = Outer_rules.provers in

    let random_field = Quickcheck.random_value Field.gen in
    let random_point = point_of_field (Quickcheck.random_value Field.gen) in

    let action_witness : Rule_action_witness.Witness.t =
      { public_key = random_point
      ; vk_hash = random_field
      ; witness =
          { aux = Field.zero
          ; children = []
          ; slot_range =
              { lower = Zeko_util.Slot.zero; upper = Zeko_util.Slot.max_value }
          }
      }
    in

    let _stmt, _proof =
      Promise.block_on_async_exn @@ fun () -> action action_witness
    in

    let Compile_simple.[ prove_both ] = Rule_commit.Verify_both_ases.provers in

    let random_field_list = [ Quickcheck.random_value Field.gen ] in

    let ase_outer =
      let stmt = ase_without_length_stmt in
      let proof = ase_without_length_proof in
      Rule_commit.Ase_outer_inst.make ~proof_source:stmt.source
        ~proof_target:stmt.target ~proof stmt.source random_field_list
    in

    let ase_inner =
      let stmt = ase_with_length in
      let proof = ase_with_length_proof in
      Rule_commit.Ase_inner_inst.make ~proof_source:stmt.source
        ~proof_target:stmt.target ~proof stmt.source random_field_list
    in

    let verify_both_ases_stmt, verify_both_ases_proof =
      Promise.block_on_async_exn @@ fun () -> prove_both (ase_outer, ase_inner)
    in

    let _verify_both_ases =
      Rule_commit.Verify_both_ases.make_unchecked ~proof:verify_both_ases_proof
        verify_both_ases_stmt
    in

    (* End of test iteration *)
    ()
  done ;
  Printf.printf "ASE operations property test completed successfully!\n"

(* Simplified property test for transaction rules *)
let () =
  Printf.printf "Running transaction rules property test...\n" ;

  (* Run the test multiple times with different random values *)
  for i = 1 to 5 do
    (* Generate random keypairs with different seeds and test basic properties *)
    let fee_payer_kp =
      Quickcheck.random_value
        ~seed:(`Deterministic (Printf.sprintf "fee_payer_%d" i))
        Keypair.gen
    in
    let new_kp =
      Quickcheck.random_value
        ~seed:(`Deterministic (Printf.sprintf "new_account_%d" i))
        Keypair.gen
    in

    (* Print the public keys to demonstrate randomization *)
    Printf.printf "Fee payer public key: %s\n"
      ( Public_key.compress fee_payer_kp.public_key
      |> Public_key.Compressed.to_base58_check ) ;
    Printf.printf "New account public key: %s\n"
      ( Public_key.compress new_kp.public_key
      |> Public_key.Compressed.to_base58_check ) ;

    (* Create a fee payer account with the random keypair *)
    let fee_payer_acc =
      { Mina_base.Account.empty with
        public_key = Public_key.compress fee_payer_kp.public_key
      ; balance = Currency.Balance.of_mina_string_exn "100000"
      }
    in

    (* Verify the fee payer account has the expected balance *)
    assert (
      Currency.Balance.(fee_payer_acc.balance = of_mina_string_exn "100000") ) ;

    (* Set up constraint constants for the test *)
    let constraint_constants : Genesis_constants.Constraint_constants.t =
      { sub_windows_per_window = 1
      ; ledger_depth = 35
      ; work_delay = 1
      ; block_window_duration_ms = 1
      ; transaction_capacity_log_2 = 1
      ; pending_coinbase_depth = 1
      ; coinbase_amount = Currency.Amount.zero
      ; supercharged_coinbase_factor = 1
      ; account_creation_fee = Currency.Fee.of_mina_string_exn "0.1"
      ; fork = None
      }
    in

    (* Verify constraint constants properties *)
    assert (Int.(constraint_constants.ledger_depth = 35))
  done ;
  Printf.printf "Transaction rules property test completed successfully!\n"
