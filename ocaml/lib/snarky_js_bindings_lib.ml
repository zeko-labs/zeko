module Js = Js_of_ocaml.Js

let rollup =
  let open Mina_base in
  object%js
    method compile =
      let constraint_constants =
        Genesis_constants.Constraint_constants.compiled
      in
      let genesis_constants = Genesis_constants.compiled in
      let consensus_constants =
        Consensus.Constants.create ~constraint_constants
          ~protocol_constants:genesis_constants.protocol
      in

      let module T = Transaction_snark.Make (struct
        let constraint_constants = constraint_constants

        let proof_level = Genesis_constants.Proof_level.Full
      end) in
      let module M = Zkapps_rollup.Make (struct
        let tag = T.tag
      end) in
      let module Proof = M.Proof in
      let module L = Mina_ledger.Ledger in
      let step pk token_id txn =
        let account_update, () =
          Promise.block_on_async_exn
            (M.step
               { public_key = pk
               ; token_id
               ; may_use_token = Inherit_from_parent
               ; txn
               } )
        in
        account_update
      in

      let gen_keys () =
        let kp = Signature_lib.Keypair.create () in
        (Signature_lib.Public_key.compress kp.public_key, kp.private_key)
      in
      let token_id = Mina_base.Token_id.default in
      object%js
        val vk = Pickles.Side_loaded.Verification_key.of_compiled T.tag

        val step = step

        method create_zkapp =
          let pk, sk = gen_keys () in
          let l = L.create ~depth:constraint_constants.ledger_depth () in
          let acup : Account_update.t =
            let body =
              { Account_update.Body.dummy with
                public_key = pk
              ; token_id
              ; update =
                  { Account_update.Update.dummy with
                    app_state =
                      [ Set
                          (Mina_base.Frozen_ledger_hash0.to_field
                             (L.merkle_root l) )
                      ; Keep
                      ; Keep
                      ; Keep
                      ; Keep
                      ; Keep
                      ; Keep
                      ; Keep
                      ]
                  ; verification_key =
                      Set
                        { data = M.vk
                        ; hash = Mina_base.Zkapp_account.digest_vk M.vk
                        }
                  ; permissions =
                      Set
                        { edit_state = Proof
                        ; send = Either
                        ; receive = None
                        ; set_delegate = Proof
                        ; set_permissions = Proof
                        ; set_verification_key = Proof
                        ; set_zkapp_uri = Proof
                        ; edit_action_state = Proof
                        ; set_token_symbol = Proof
                        ; increment_nonce = Proof
                        ; set_voting_for = Proof
                        ; set_timing = Proof
                        ; access = Either
                        }
                  }
              ; use_full_commitment = true
              ; preconditions =
                  { Account_update.Preconditions.network =
                      Mina_base.Zkapp_precondition.Protocol_state.accept
                  ; account = Accept
                  ; valid_while = Ignore
                  }
              ; authorization_kind = Signature
              }
            in
            (* FIXME: dummy sig *)
            { body; authorization = Signature Mina_base.Signature.dummy }
          in
          object%js
            val account_update = acup

            val ledger = l

            val sk = sk

            val pk = pk
          end

        method apply_user_command l pk user_command =
          let open Promise.Let_syntax in
          let global_slot = Mina_numbers.Global_slot_since_genesis.zero in
          let source = L.merkle_root l in
          let target =
            (* FIXME: slot is wrong *)
            L.merkle_root_after_user_command_exn l ~constraint_constants
              ~txn_global_slot:global_slot user_command
          in
          let init_stack = Mina_base.Pending_coinbase.Stack.empty in
          let state_body =
            let compile_time_genesis =
              Mina_state.Genesis_protocol_state.t
                ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
                ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
                ~constraint_constants ~consensus_constants
                ~genesis_body_reference:
                  Staged_ledger_diff.genesis_body_reference
            in
            compile_time_genesis.data |> Mina_state.Protocol_state.body
          in
          let user_command_in_block =
            { Transaction_protocol_state.Poly.transaction = user_command
            ; block_data = state_body
            ; global_slot = Mina_numbers.Global_slot_since_genesis.zero
            }
          in
          let state_body_hash =
            Mina_state.Protocol_state.Body.hash state_body
          in
          let pc : Transaction_snark.Pending_coinbase_stack_state.t =
            (* No coinbase to add to the stack. *)
            let stack_with_state =
              Pending_coinbase.Stack.push_state state_body_hash global_slot
                init_stack
            in
            { source = stack_with_state; target = stack_with_state }
          in
          let user_command_supply_increase = Currency.Amount.Signed.zero in
          let txn =
            Mina_transaction.Transaction.Command
              (User_command.Signed_command
                 (Signed_command.forget_check user_command) )
          in
          let sok_digest =
            Sok_message.create ~fee:Currency.Fee.zero ~prover:pk
            |> Sok_message.digest
          in
          let sparse_ledger =
            Mina_ledger.Sparse_ledger.of_ledger_subset_exn l
              (Signed_command.accounts_referenced
                 (Signed_command.forget_check user_command) )
          in
          let statement =
            Transaction_snark.Statement.Poly.with_empty_local_state
              ~source_first_pass_ledger:source ~target_first_pass_ledger:target
              ~source_second_pass_ledger:target
              ~target_second_pass_ledger:target ~connecting_ledger_left:target
              ~connecting_ledger_right:target ~sok_digest
              ~fee_excess:
                (Or_error.ok_exn (Mina_transaction.Transaction.fee_excess txn))
              ~supply_increase:user_command_supply_increase
              ~pending_coinbase_stack_state:pc
          in
          let handler =
            unstage @@ Mina_ledger.Sparse_ledger.handler sparse_ledger
          in
          let txnsnark =
            T.of_user_command ~init_stack ~statement user_command_in_block
              handler
          in
          ()
      end
  end

let export () =
  Js.export "Snarky" Snarky_bindings.snarky ;
  Js.export "Ledger" Local_ledger.ledger_class ;
  Js.export "Pickles" Pickles_bindings.pickles ;
  Js.export "Test" Consistency_test.test ;
  Js.export "Rollup" rollup

let export_global () =
  let snarky_obj =
    Js.Unsafe.(
      let i = inject in
      obj
        [| ("Snarky", i snarky)
         ; ("Ledger", i Ledger.ledger_class)
         ; ("Pickles", i pickles)
         ; ("Test", i test)
         ; ("Rollup", i rollup)
        |])
  in
  Js.Unsafe.(set global (Js.string "__snarky") snarky_obj)
