open Core_kernel
open Signature_lib
open Snark_params.Tick
open Zeko_circuits

open struct
  let signature_kind = Mina_signature_kind.Testnet

  let list_to_fun l =
    let l = ref l in
    fun () ->
      match !l with
      | [] ->
          failwith "empty!"
      | x :: xs ->
          l := xs ;
          x

  let da_sk =
    Quickcheck.random_value ~seed:(`Deterministic "182128381918")
      Private_key.gen

  let da_key = Public_key.of_private_key_exn da_sk |> Public_key.compress

  let () = assert (not da_key.is_odd)

  let ase_with_length, ase_with_length_proof =
    let open struct
      let trans0, proof0 =
        Promise.block_on_async_exn
        @@ fun () ->
        (Lazy.force Ase.With_length.leaf)
          ( [ Field.one ]
          , { action_state = Field.of_string "6"
            ; length = Unsigned.UInt32.of_string "42"
            } )

      let trans1, proof1 =
        Promise.block_on_async_exn
        @@ fun () ->
        (Lazy.force Ase.With_length.leaf_option)
          ([ Field.of_string "2" ], trans0.target)

      let trans2, proof2 =
        Promise.block_on_async_exn
        @@ fun () ->
        (Lazy.force Ase.With_length.merge)
          { left = trans0
          ; left_proof = proof0
          ; right = trans1
          ; right_proof = proof1
          }

      let trans3, proof3 =
        Promise.block_on_async_exn
        @@ fun () ->
        (Lazy.force Ase.With_length.extend)
          ([ Field.of_string "99" ], (trans2, proof2))

      let trans4, proof4 =
        Promise.block_on_async_exn
        @@ fun () ->
        (Lazy.force Ase.With_length.extend_option)
          ([ Field.of_string "99" ], (trans3, proof3))
    end in
    (trans4, proof4)

  let _ase_without_length =
    let open struct
      let trans0, proof0 =
        Promise.block_on_async_exn
        @@ fun () ->
        (Lazy.force Ase.Without_length.leaf) ([ Field.one ], Field.of_string "6")

      let trans1, proof1 =
        Promise.block_on_async_exn
        @@ fun () ->
        (Lazy.force Ase.Without_length.leaf_option)
          ([ Field.of_string "2" ], trans0.target)

      let trans2, proof2 =
        Promise.block_on_async_exn
        @@ fun () ->
        (Lazy.force Ase.Without_length.merge)
          { left = trans0
          ; left_proof = proof0
          ; right = trans1
          ; right_proof = proof1
          }

      let trans3, proof3 =
        Promise.block_on_async_exn
        @@ fun () ->
        (Lazy.force Ase.Without_length.extend)
          ([ Field.of_string "99" ], (trans2, proof2))

      let trans4, proof4 =
        Promise.block_on_async_exn
        @@ fun () ->
        (Lazy.force Ase.Without_length.extend_option)
          ([ Field.of_string "99" ], (trans3, proof3))
    end in
    (trans4, proof4)

  let point_of_string_even s : Zeko_util.Even_PC.t =
    let x, _ =
      Snark_params.Tick.Inner_curve.(
        to_affine_exn @@ point_near_x @@ Snark_params.Tick.Field.of_string s)
    in
    { public_key = x }

  let point_of_string s =
    Snark_params.Tick.Inner_curve.(
      to_affine_exn @@ point_near_x @@ Snark_params.Tick.Field.of_string s)
    |> Public_key.compress

  module Inner_rules_inst =
    Inner_rules.Make
      (struct
        let chain_l2 = Mina_signature_kind.Testnet
      end)
      ()

  let _inner_stmt, _inner_proof =
    let open struct
      let Compile_simple.[ sync; action ] = Lazy.force Inner_rules_inst.provers

      let ase_with_length : Rule_inner_sync.Ase_inst.t =
        Rule_inner_sync.Ase_inst.make ~proof_source:ase_with_length.source
          ~proof_target:ase_with_length.target ~proof:ase_with_length_proof
          ase_with_length.source
          [ Field.of_string "418923791273" ]

      let sync_witness : Rule_inner_sync.Witness.t =
        { public_key = point_of_string "8184848488"
        ; vk_hash = Field.of_string "4819274123"
        ; ase = ase_with_length
        }

      let _stmt, _proof =
        Promise.block_on_async_exn @@ fun () -> sync sync_witness

      let action_witness : Rule_inner_action_witness.Witness.t =
        { public_key = point_of_string "8184848488"
        ; vk_hash = Field.of_string "4819274123"
        ; witness = { aux = Field.zero; children = [] }
        }

      let stmt, proof =
        Promise.block_on_async_exn @@ fun () -> action action_witness
    end in
    (stmt, proof)

  let inner_public_key =
    let pk =
      Snark_params.Tick.Inner_curve.(
        to_affine_exn @@ point_near_x
        @@ Snark_params.Tick.Field.of_int 123456789)
    in
    Signature_lib.Public_key.compress pk

  module Outer_rules_inst =
    Outer_rules.Make
      (struct
        let max_valid_while_size = 1024

        let inner_public_key = inner_public_key

        let chain_l1 = Mina_signature_kind.Testnet

        let max_sequencer_inactivity = 128

        let emergency_da_public_key = point_of_string "223344"
      end)
      ()

  module Emergency_da_rules_inst =
    Emergency_da_rules.Make
      (struct
        let chain_l1 = Mina_signature_kind.Testnet
      end)
      ()

  let _txn_stmt, _txn_proof =
    let open struct
      let Compile_simple.[ commit; emergency_commit; action; _pause ] =
        Lazy.force Outer_rules_inst.provers

      (*
    let pause_witness : Rule_pause.Witness.t =
      { public_key = point_of_string "1238881"
      ; vk_hash = Field.of_string "19944541415"
      ; pause_key = point_of_string_even "1511111121"
      }

    let _stmt, _proof =
      Promise.block_on_async_exn @@ fun () -> pause pause_witness
    *)

      let action_witness : Rule_action_witness.Witness.t =
        { public_key = point_of_string "41889111"
        ; vk_hash = Field.of_string "188188181"
        ; witness =
            { aux = Field.zero
            ; children = []
            ; slot_range =
                { lower = Zeko_util.Slot.zero
                ; upper = Zeko_util.Slot.max_value
                }
            }
        }

      let _stmt, _proof =
        Promise.block_on_async_exn @@ fun () -> action action_witness

      let Compile_simple.[ prove_both ] =
        Lazy.force Rule_commit.Verify_both_ases.provers

      let ase_outer =
        let default = Mina_base.Zkapp_account.Actions.empty_state_element in
        Rule_commit.Ase_outer_inst.make ~proof_source:default
          ~proof_target:default default []

      let ase_inner =
        let default : Ase.With_length.Stmt.t =
          { action_state = Mina_base.Zkapp_account.Actions.empty_state_element
          ; length = Unsigned.UInt32.zero
          }
        in
        Rule_commit.Ase_inner_inst.make ~proof_source:default
          ~proof_target:default default []

      let verify_both_ases_stmt, verify_both_ases_proof =
        Promise.block_on_async_exn @@ fun () -> prove_both (ase_outer, ase_inner)

      let verify_both_ases =
        Rule_commit.Verify_both_ases.make_unchecked
          ~proof:verify_both_ases_proof verify_both_ases_stmt

      let old_inner_acc =
        { Mina_base.Account.empty with
          public_key = inner_public_key
        ; zkapp =
            Some
              { Mina_base.Zkapp_account.default with
                app_state =
                  [ Mina_base.Zkapp_account.Actions.empty_state_element
                  ; Field.zero
                  ; Field.zero
                  ; Field.zero
                  ; Field.zero
                  ; Field.zero
                  ; Field.zero
                  ; Field.zero
                  ]
              ; action_state =
                  (let f =
                     Mina_base.Zkapp_account.Actions.empty_state_element
                   in
                   [ f; f; f; f; f ] )
              }
        }

      let Compile_simple.
            [ _signed_command
            ; _zkapp_single
            ; zkapp_double
            ; _zkapp_proved
            ; merge
            ] =
        Lazy.force Txn_rules.provers

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

      let () = assert (Int.(constraint_constants.ledger_depth = 35))

      let intermediate_ledger_hashes =
        let base = force Mina_base.Account.empty_digest in
        let rec go = function
          | 34, hash ->
              [ (34, hash) ]
          | height, hash ->
              (height, hash)
              :: go (height + 1, Mina_base.Ledger_hash.merge ~height hash hash)
        in
        go (0, base)

      let () = assert (List.length intermediate_ledger_hashes = 35)

      let implied_root (account : Mina_base.Account.t) path : field =
        let init = Mina_base.Account.digest account in
        List.foldi path ~init ~f:(fun height acc -> function
          | `Right left ->
              let acc' = Mina_base.Ledger_hash.merge ~height left acc in
              acc'
          | `Left right ->
              let acc' = Mina_base.Ledger_hash.merge ~height acc right in
              acc' )

      let (fee_payer_kp, new_kp), da_kp =
        Base_quickcheck.Generator.both
          (Base_quickcheck.Generator.both Keypair.gen Keypair.gen)
          Keypair.gen
        |> Quickcheck.random_value

      let fee_payer_acc =
        { Mina_base.Account.empty with
          public_key = Public_key.compress fee_payer_kp.public_key
        ; balance = Currency.Balance.of_mina_string_exn "100000"
        }

      let fee_payer_acc_source = fee_payer_acc

      let path_inner =
        `Left (Mina_base.Account.digest fee_payer_acc)
        :: ( List.map ~f:(fun (_, h) -> `Left h)
           @@ List.drop intermediate_ledger_hashes 1 )

      let old_inner_acc_path = path_inner

      let path_fee_payer =
        `Right (Mina_base.Account.digest old_inner_acc)
        :: ( List.map ~f:(fun (_, h) -> `Left h)
           @@ List.drop intermediate_ledger_hashes 1 )

      let path_new =
        `Left (force Mina_base.Account.empty_digest)
        :: `Right
             Mina_base.Account.(
               Mina_base.Ledger_hash.merge ~height:0 (digest old_inner_acc)
                 (digest fee_payer_acc))
        :: List.map
             ~f:(fun (_, h) -> `Left h)
             (List.drop intermediate_ledger_hashes 2)

      let path_inner_source = path_inner

      let path_fee_payer_source = path_fee_payer

      let path_new_source = path_new

      let source_ledger = implied_root old_inner_acc path_inner

      let () = printf "source_ledger:  %s\n" (Field.to_string source_ledger)

      let id_of account =
        Mina_base.Account_id.create account.Mina_base.Account.public_key
          account.token_id

      let new_pk = new_kp.public_key |> Public_key.compress

      let account_id_new =
        Mina_base.Account_id.create new_pk Mina_base.Token_id.default

      let () =
        printf "Fee  key: %s\n"
          (Public_key.Compressed.to_base58_check fee_payer_acc.public_key)

      let () =
        printf "New  key: %s\n" (Public_key.Compressed.to_base58_check new_pk)

      let () =
        printf "Inner key:                %s\n"
          (Public_key.Compressed.to_base58_check old_inner_acc.public_key)

      let () =
        printf "Empty key:                %s\n"
          (Public_key.Compressed.to_base58_check
             Mina_base.Account.empty.public_key )

      let first_account_update : Mina_base.Account_update.Body.t =
        { Mina_base.Account_update.Body.dummy with
          public_key = fee_payer_acc.public_key
        ; authorization_kind = Signature
        ; increment_nonce = true
        ; use_full_commitment = true
        ; preconditions =
            { Mina_base.Account_update.Body.dummy.preconditions with
              account =
                { Mina_base.Account_update.Body.dummy.preconditions.account with
                  nonce =
                    Check
                      { lower = Unsigned.UInt32.zero
                      ; upper = Unsigned.UInt32.zero
                      }
                }
            }
        }

      let second_account_update : Mina_base.Account_update.Body.t =
        { Mina_base.Account_update.Body.dummy with
          public_key = old_inner_acc.public_key
        ; token_id = old_inner_acc.token_id
        ; authorization_kind = None_given
        }

      let third_account_update : Mina_base.Account_update.Body.t =
        { Mina_base.Account_update.Body.dummy with
          public_key = fee_payer_acc.public_key
        ; token_id = fee_payer_acc.token_id
        ; balance_change =
            Currency.Amount.Signed.(
              of_fee
                Currency.Fee.Signed.(
                  of_unsigned constraint_constants.account_creation_fee)
              + of_unsigned (Currency.Amount.of_mina_string_exn "1")
              |> Option.value_exn |> negate)
        ; authorization_kind = Signature
        ; use_full_commitment = true
        }

      let fourth_account_update : Mina_base.Account_update.Body.t =
        { Mina_base.Account_update.Body.dummy with
          public_key = new_pk
        ; balance_change =
            Currency.Amount.Signed.(
              of_fee
                Currency.Fee.Signed.(
                  of_unsigned constraint_constants.account_creation_fee)
              + of_unsigned (Currency.Amount.of_mina_string_exn "1")
              |> Option.value_exn)
        }

      let full_transaction_commitment =
        let forest =
          Mina_base.Zkapp_command.Call_forest.of_account_updates
            ~account_update_depth:(fun _ -> 0)
            [ second_account_update
            ; third_account_update
            ; fourth_account_update
            ]
          |> Mina_base.Zkapp_command.Call_forest.accumulate_hashes
               ~hash_account_update:
                 (Mina_base.Zkapp_command.Call_forest.Digest.Account_update
                  .create_body ~signature_kind )
          |> Mina_base.Zkapp_command.Call_forest.hash
        in
        Mina_base.Zkapp_command.Transaction_commitment.create_complete
          (forest :> field)
          ~memo_hash:Field.zero
          ~fee_payer_hash:
            (Mina_base.Zkapp_command.Digest.Account_update.create_body
               ~signature_kind first_account_update )

      let signature =
        Signature_lib.Schnorr.Chunked.sign ~signature_kind
          fee_payer_kp.private_key
          (Random_oracle.Input.Chunked.field full_transaction_commitment)

      let () =
        printf "public key out circuit: %s\n"
          (fee_payer_acc.public_key |> Public_key.Compressed.to_base58_check) ;
        printf "commitment out circuit: %s\n"
          (full_transaction_commitment |> Field.to_string)

      let to_account_set x =
        let (Typ typ) = Account_set.typ in
        typ.value_of_fields ([| x |], typ.constraint_system_auxiliary ())

      (* let of_account_set x =
         let (Typ typ) = Account_set.typ in
         let fields, _aux = typ.value_to_fields x in
         match fields with [| f |] -> f | _ -> failwith __LOC__ *)

      let derive pk =
        Mina_base.Account_id.create pk Mina_base.Token_id.default
        |> fun owner -> Mina_base.Account_id.derive_token_id ~owner

      let inner_own_token_id = derive inner_public_key

      let fee_payer_own_token_id =
        derive (Public_key.compress fee_payer_kp.public_key)

      module S = Account_set_data.Merkle_set (struct
        open struct
          let to_ = Mina_base.Token_id.to_field_unsafe

          let of_ = Mina_base.Token_id.of_field
        end

        type t = Mina_base.Token_id.t

        let compare x y = Field.compare (to_ x) (to_ y)

        let min = of_ Field.zero

        let max = of_ (Field.negate Field.one)

        let sexp_of_t x = Field.sexp_of_t (to_ x)

        let t_of_sexp x = Field.t_of_sexp x |> of_

        let to_fields x = [ to_ x ]
      end)

      let acc_set, { S.hash = source_acc_set; _ } =
        S.maybe_add inner_own_token_id S.empty
        |> fun (s, _) -> S.maybe_add fee_payer_own_token_id s

      let acc_set, acc_set_data_0 =
        S.maybe_add (derive first_account_update.public_key) acc_set

      let acc_set, acc_set_data_1 =
        S.maybe_add (derive second_account_update.public_key) acc_set

      let convert_path =
        let f = function
          | `Left right ->
              ({ hash_other = right; is_right = false } : Account_set.PathStep.t)
          | `Right left ->
              { hash_other = left; is_right = true }
        in
        List.map ~f

      let make_update_acc_set_witness first second =
        { Txn_state.get_account_set_x =
            list_to_fun [ first.S.before; second.S.before ]
        ; get_account_set_z = list_to_fun [ first.after; second.after ]
        ; get_account_set_x_path =
            List.map ~f:convert_path [ first.before_path; second.before_path ]
            |> list_to_fun
        ; get_account_set_y_path =
            List.map ~f:convert_path [ first.path; second.path ] |> list_to_fun
        }

      let sparse_source_ledger : Mina_ledger.Sparse_ledger.t =
        Mina_ledger.Sparse_ledger.of_root
          ~depth:constraint_constants.ledger_depth source_ledger
        |> fun x ->
        Mina_ledger.Sparse_ledger.add_path x path_inner (id_of old_inner_acc)
          old_inner_acc
        |> fun x ->
        Mina_ledger.Sparse_ledger.add_path x path_fee_payer
          (id_of fee_payer_acc) fee_payer_acc
        |> fun x ->
        Mina_ledger.Sparse_ledger.add_path x path_new account_id_new
          Mina_base.Account.empty

      let zkapp_double_witness :
          Rule_zkapp_command.Zkapp_double_unproved_input.t =
        { base =
            { source_ledger
            ; source_local_state =
                { stack_frame_digest =
                    Mina_base.Stack_frame.Digest.create
                      Mina_base.Stack_frame.empty
                ; call_stack_digest = Mina_base.Call_stack_digest.empty
                ; transaction_commitment =
                    Mina_base.Zkapp_command.Transaction_commitment.empty
                ; full_transaction_commitment =
                    Mina_base.Zkapp_command.Transaction_commitment.empty
                ; excess = Currency.Amount.Signed.zero
                ; account_update_index = Mina_numbers.Index.zero
                }
            ; sequencer = point_of_string_even "1991991991"
            ; source_acc_set = to_account_set source_acc_set
            ; witness =
                { stack_frame = Mina_base.Stack_frame.empty
                ; call_stack = []
                ; source_ledger_sparse = sparse_source_ledger
                ; update_acc_set_witness =
                    make_update_acc_set_witness acc_set_data_0 acc_set_data_1
                }
            }
        ; first =
            (let account_updates_data =
               List.map ~f:Mina_base.Account_update.reset_aux
                 [ { Mina_base.Account_update.Poly.body = first_account_update
                   ; authorization = Mina_base.Control.Poly.Signature signature
                   ; aux = ()
                   }
                 ; { body = second_account_update
                   ; authorization = None_given
                   ; aux = ()
                   }
                 ; { body = third_account_update
                   ; authorization = Signature signature
                   ; aux = ()
                   }
                 ; { body = fourth_account_update
                   ; authorization = None_given
                   ; aux = ()
                   }
                 ]
               |> Mina_base.Zkapp_command.Call_forest.of_account_updates
                    ~account_update_depth:(fun _ -> 0)
               |> Mina_base.Zkapp_command.Call_forest
                  .accumulate_hashes_predicated ~signature_kind
             in
             { account_updates_data
             ; memo_hash = Field.zero
             ; account_updates =
                 Mina_base.Zkapp_command.Call_forest.hash account_updates_data
             ; shift_action_state = false
             } )
        ; second =
            { account_updates_data =
                Mina_base.Zkapp_command.Call_forest.accumulate_hashes_predicated
                  ~signature_kind []
            ; memo_hash = Field.zero
            ; account_updates =
                Mina_base.Zkapp_command.Call_forest.accumulate_hashes_predicated
                  ~signature_kind []
                |> Mina_base.Zkapp_command.Call_forest.hash
            ; shift_action_state = false
            }
        }

      let stmt0, proof0 =
        Promise.block_on_async_exn
        @@ fun () -> zkapp_double zkapp_double_witness

      let receipt_chain_hash =
        Mina_base.Receipt.Chain_hash.(
          cons_zkapp_command_commitment Unsigned.UInt32.zero
            (Zkapp_command_commitment full_transaction_commitment) empty)

      let receipt_chain_hash_0 = receipt_chain_hash

      let fee_payer_acc =
        { fee_payer_acc with nonce = Unsigned.UInt32.one; receipt_chain_hash }

      let path_inner =
        `Left (Mina_base.Account.digest fee_payer_acc)
        :: ( List.map ~f:(fun (_, h) -> `Left h)
           @@ List.drop intermediate_ledger_hashes 1 )

      let path_new =
        `Left (force Mina_base.Account.empty_digest)
        :: `Right
             Mina_base.Account.(
               Mina_base.Ledger_hash.merge ~height:0 (digest old_inner_acc)
                 (digest fee_payer_acc))
        :: List.map
             ~f:(fun (_, h) -> `Left h)
             (List.drop intermediate_ledger_hashes 2)

      let sparse_source_ledger : Mina_ledger.Sparse_ledger.t =
        Mina_ledger.Sparse_ledger.of_root
          ~depth:constraint_constants.ledger_depth stmt0.target_ledger
        |> fun x ->
        Mina_ledger.Sparse_ledger.add_path x path_inner (id_of old_inner_acc)
          old_inner_acc
        |> fun x ->
        Mina_ledger.Sparse_ledger.add_path x path_fee_payer
          (id_of fee_payer_acc)
          { fee_payer_acc with nonce = Unsigned.UInt32.one }
        |> fun x ->
        Mina_ledger.Sparse_ledger.add_path x path_new account_id_new
          Mina_base.Account.empty

      let () =
        printf "old full_transaction_commitment: %s\n"
          (Field.to_string stmt0.source_local_state.full_transaction_commitment) ;
        printf "new full_transaction_commitment: %s\n"
          (Field.to_string stmt0.target_local_state.full_transaction_commitment)

      let acc_set, acc_set_data_2 =
        S.maybe_add (derive third_account_update.public_key) acc_set

      let acc_set, acc_set_data_3 =
        S.maybe_add (derive fourth_account_update.public_key) acc_set

      let _ = acc_set

      let zkapp_second_double_witness :
          Rule_zkapp_command.Zkapp_double_unproved_input.t =
        { base =
            { source_ledger = stmt0.target_ledger
            ; source_local_state = stmt0.target_local_state
            ; sequencer = point_of_string_even "1991991991"
            ; source_acc_set = stmt0.target_acc_set
            ; witness =
                { stack_frame =
                    { caller = Mina_base.Token_id.default
                    ; caller_caller = Mina_base.Token_id.default
                    ; calls =
                        List.map ~f:Mina_base.Account_update.reset_aux
                          [ { Mina_base.Account_update.Poly.body =
                                third_account_update
                            ; authorization =
                                Mina_base.Control.Poly.Signature signature
                            ; aux = ()
                            }
                          ; { body = fourth_account_update
                            ; authorization = None_given
                            ; aux = ()
                            }
                          ]
                        |> Mina_base.Zkapp_command.Call_forest
                           .of_account_updates ~account_update_depth:(fun _ ->
                               0 )
                        |> Mina_base.Zkapp_command.Call_forest
                           .accumulate_hashes_predicated ~signature_kind
                    }
                ; call_stack = []
                ; source_ledger_sparse = sparse_source_ledger
                ; update_acc_set_witness =
                    make_update_acc_set_witness acc_set_data_2 acc_set_data_3
                }
            }
        ; first =
            { account_updates_data =
                Mina_base.Zkapp_command.Call_forest.accumulate_hashes_predicated
                  ~signature_kind []
            ; memo_hash = Field.zero
            ; account_updates =
                Mina_base.Zkapp_command.Call_forest.accumulate_hashes_predicated
                  ~signature_kind []
                |> Mina_base.Zkapp_command.Call_forest.hash
            ; shift_action_state = false
            }
        ; second =
            { account_updates_data =
                Mina_base.Zkapp_command.Call_forest.accumulate_hashes_predicated
                  ~signature_kind []
            ; memo_hash = Field.zero
            ; account_updates =
                Mina_base.Zkapp_command.Call_forest.accumulate_hashes_predicated
                  ~signature_kind []
                |> Mina_base.Zkapp_command.Call_forest.hash
            ; shift_action_state = false
            }
        }

      let stmt1, proof1 =
        Promise.block_on_async_exn
        @@ fun () -> zkapp_double zkapp_second_double_witness

      let receipt_chain_hash =
        Mina_base.Receipt.Chain_hash.(
          cons_zkapp_command_commitment (Unsigned.UInt32.of_int 2)
            (Zkapp_command_commitment full_transaction_commitment)
            receipt_chain_hash)

      let receipt_chain_hash_1 = receipt_chain_hash

      let fee_payer_acc =
        { fee_payer_acc with nonce = Unsigned.UInt32.one; receipt_chain_hash }

      let stmt, proof =
        Promise.block_on_async_exn
        @@ fun () ->
        merge
          { left = stmt0
          ; left_proof = proof0
          ; right = stmt1
          ; right_proof = proof1
          }

      let convert_path =
        let f = function
          | `Left right_side ->
              ({ right_side } : Outer_rules_inst.Rule_commit_inst.PathElt.t)
          | `Right _ ->
              failwith __LOC__
        in
        List.map ~f

      let new_inner_acc_path =
        ( { right_side =
              Mina_base.Account.digest
                { fee_payer_acc with
                  nonce = Unsigned.UInt32.one
                ; balance =
                    (let b, _ =
                       Currency.Balance.add_signed_amount_flagged
                         fee_payer_acc.balance
                         third_account_update.balance_change
                     in
                     b )
                }
          }
          : Outer_rules_inst.Rule_commit_inst.PathElt.t )
        :: ( { right_side =
                 Mina_base.Account.(
                   Mina_base.Ledger_hash.merge ~height:0
                     (digest
                        { empty with
                          public_key = Public_key.compress new_kp.public_key
                        ; balance = Currency.Balance.of_mina_string_exn "1"
                        ; delegate =
                            Some (Public_key.compress new_kp.public_key)
                        } )
                     (force empty_digest))
             }
             : Outer_rules_inst.Rule_commit_inst.PathElt.t )
        :: ( List.map
               ~f:(fun
                    (_, right_side)
                    :
                    Outer_rules_inst.Rule_commit_inst.PathElt.t
                  -> { right_side } )
           @@ List.drop intermediate_ledger_hashes 2 )

      let da_signature =
        let input =
          let open Random_oracle.Input.Chunked in
          (* append *)
          stmt.target_ledger |> field
          (* (stmt.target_acc_set |> of_account_set |> field) *)
        in
        let payload =
          Random_oracle.hash
            ~init:(Hash_prefix_create.salt Zeko_constants.da_layer_check_salt)
            (Random_oracle.pack_input input)
        in
        Signature_lib.Schnorr.Chunked.sign ~signature_kind da_kp.private_key
          (Random_oracle.Input.Chunked.field payload)

      let () = assert (Int.(List.length old_inner_acc_path = 35))

      let () = assert (Int.(List.length new_inner_acc_path = 35))

      let da_multisig : Multisig.Witness.t =
        { signatures =
            [ { Multisig.Maybe_signature.public_key =
                  Public_key.compress da_kp.public_key
              ; signature = da_signature
              ; is_some = true
              }
            ]
        ; quorum = Field.of_int 1
        }

      let base_witness : Outer_rules_inst.Rule_commit_inst.Base_witness.t =
        { public_key = point_of_string "29421"
        ; vk_hash = Snark_params.Tick.Field.zero
        ; slot_range =
            { lower = Mina_numbers.Global_slot_since_genesis.zero
            ; upper = Mina_numbers.Global_slot_since_genesis.zero
            }
        ; old_inner_acc
        ; new_inner_acc = old_inner_acc
        ; old_inner_acc_path = convert_path old_inner_acc_path
        ; new_inner_acc_path
        ; da_multisig
        }

      let witness : Outer_rules_inst.Rule_commit_inst.Witness.t =
        { txn_snark = Txn_rules.make_unchecked ~proof stmt
        ; base_witness
        ; verify_both_ases
        }

      let commit_stmt, commit_proof =
        Promise.block_on_async_exn @@ fun () -> commit witness

      let Compile_simple.[ emergency_da_apply ] =
        Lazy.force Emergency_da_rules_inst.provers

      let to_emergency_path path =
        List.map
          ~f:(function
            | `Left hash_other ->
                ( { Rule_emergency_da.Ledger_path.Step.hash_other
                  ; is_right = false
                  }
                  : Rule_emergency_da.Ledger_path.Step.t )
            | `Right hash_other ->
                ( { Rule_emergency_da.Ledger_path.Step.hash_other
                  ; is_right = true
                  }
                  : Rule_emergency_da.Ledger_path.Step.t ) )
          path

      let action_fields_of_emergency_output
          ((_, (account_update, _digest, _calls)) :
            Mina_base.Zkapp_statement.t
            * ( Mina_base.Account_update.Body.t
              * Mina_base.Zkapp_command.Digest.Account_update.t
              * ( Mina_base.Account_update.t
                , Mina_base.Zkapp_command.Digest.Account_update.t
                , Rollup_state.Zkapp_call_forest.Digest.t )
                Mina_base.Zkapp_command.Call_forest.t ) ) : Field.t array =
        let Mina_base.Account_update.Body.{ actions; _ } = account_update in
        match actions with
        | [ action_fields ] ->
            action_fields
        | _ ->
            failwith __LOC__

      let emergency_da_action ~source_ledger_hash ~target_ledger_hash
          ~ledger_index ~account : Rule_emergency_da.Action.t =
        { source_ledger_hash; target_ledger_hash; ledger_index; account }

      let assert_action_fields_equal expected actual =
        assert (Int.(Array.length expected = Array.length actual)) ;
        assert (Array.for_all2_exn expected actual ~f:Field.equal)

      let action_to_fields action =
        let (Snark_params.Tick.Typ.Typ action_typ) =
          Rule_emergency_da.Action.typ
        in
        let fields, _aux = action_typ.value_to_fields action in
        fields

      let fee_payer_acc_after_first =
        { fee_payer_acc_source with
          nonce = Unsigned.UInt32.one
        ; receipt_chain_hash = receipt_chain_hash_0
        }

      let fee_payer_balance_after_third =
        let b, _ =
          Currency.Balance.add_signed_amount_flagged
            fee_payer_acc_after_first.balance
            third_account_update.balance_change
        in
        b

      let fee_payer_acc_after_third =
        { fee_payer_acc_after_first with
          receipt_chain_hash = receipt_chain_hash_1
        ; balance = fee_payer_balance_after_third
        }

      let new_account_created =
        { Mina_base.Account.empty with
          public_key = Public_key.compress new_kp.public_key
        ; balance = Currency.Balance.of_mina_string_exn "1"
        ; delegate = Some (Public_key.compress new_kp.public_key)
        }

      let emergency_sparse_ledger : Mina_ledger.Sparse_ledger.t =
        Mina_ledger.Sparse_ledger.of_root
          ~depth:constraint_constants.ledger_depth stmt.source_ledger
        |> fun x ->
        Mina_ledger.Sparse_ledger.add_path x path_inner_source
          (id_of old_inner_acc) old_inner_acc
        |> fun x ->
        Mina_ledger.Sparse_ledger.add_path x path_fee_payer_source
          (id_of fee_payer_acc_source)
          fee_payer_acc_source
        |> fun x ->
        Mina_ledger.Sparse_ledger.add_path x path_new_source account_id_new
          Mina_base.Account.empty

      let update_sparse ledger account_id account =
        let idx = Mina_ledger.Sparse_ledger.find_index_exn ledger account_id in
        Mina_ledger.Sparse_ledger.set_exn ledger idx account

      let emergency_ledger_1 =
        update_sparse emergency_sparse_ledger
          (id_of fee_payer_acc_source)
          fee_payer_acc_after_first

      let emergency_ledger_2 =
        update_sparse emergency_ledger_1 (id_of old_inner_acc) old_inner_acc

      let emergency_ledger_3 =
        update_sparse emergency_ledger_2
          (id_of fee_payer_acc_source)
          fee_payer_acc_after_third

      let emergency_ledger_4 =
        update_sparse emergency_ledger_3 account_id_new new_account_created

      let ledger0 =
        Mina_ledger.Sparse_ledger.merkle_root emergency_sparse_ledger

      let ledger1 = Mina_ledger.Sparse_ledger.merkle_root emergency_ledger_1

      let ledger2 = Mina_ledger.Sparse_ledger.merkle_root emergency_ledger_2

      let ledger3 = Mina_ledger.Sparse_ledger.merkle_root emergency_ledger_3

      let ledger4 = Mina_ledger.Sparse_ledger.merkle_root emergency_ledger_4

      let () = assert (Mina_base.Ledger_hash.equal ledger0 stmt.source_ledger)

      let () = assert (Mina_base.Ledger_hash.equal ledger4 stmt.target_ledger)

      let fee_payer_index =
        Mina_ledger.Sparse_ledger.find_index_exn emergency_sparse_ledger
          (id_of fee_payer_acc_source)

      let inner_index =
        Mina_ledger.Sparse_ledger.find_index_exn emergency_sparse_ledger
          (id_of old_inner_acc)

      let new_index =
        Mina_ledger.Sparse_ledger.find_index_exn emergency_sparse_ledger
          account_id_new

      let fee_payer_path_0 =
        Mina_ledger.Sparse_ledger.path_exn emergency_sparse_ledger
          fee_payer_index

      let inner_path_1 =
        Mina_ledger.Sparse_ledger.path_exn emergency_ledger_1 inner_index

      let fee_payer_path_2 =
        Mina_ledger.Sparse_ledger.path_exn emergency_ledger_2 fee_payer_index

      let new_path_3 =
        Mina_ledger.Sparse_ledger.path_exn emergency_ledger_3 new_index

      let emergency_da_witness_1 : Rule_emergency_da.Witness.t =
        { public_key = point_of_string "281"
        ; vk_hash = Field.zero
        ; old_account = fee_payer_acc_source
        ; new_account = fee_payer_acc_after_first
        ; ledger_path = to_emergency_path fee_payer_path_0
        }

      let emergency_da_witness_2 : Rule_emergency_da.Witness.t =
        { public_key = point_of_string "281"
        ; vk_hash = Field.zero
        ; old_account = old_inner_acc
        ; new_account = old_inner_acc
        ; ledger_path = to_emergency_path inner_path_1
        }

      let emergency_da_witness_3 : Rule_emergency_da.Witness.t =
        { public_key = point_of_string "281"
        ; vk_hash = Field.zero
        ; old_account = fee_payer_acc_after_first
        ; new_account = fee_payer_acc_after_third
        ; ledger_path = to_emergency_path fee_payer_path_2
        }

      let emergency_da_witness_4 : Rule_emergency_da.Witness.t =
        { public_key = point_of_string "281"
        ; vk_hash = Field.zero
        ; old_account = Mina_base.Account.empty
        ; new_account = new_account_created
        ; ledger_path = to_emergency_path new_path_3
        }

      let emergency_da_out_1, _emergency_da_proof_1 =
        Promise.block_on_async_exn
        @@ fun () -> emergency_da_apply emergency_da_witness_1

      let emergency_da_out_2, _emergency_da_proof_2 =
        Promise.block_on_async_exn
        @@ fun () -> emergency_da_apply emergency_da_witness_2

      let emergency_da_out_3, _emergency_da_proof_3 =
        Promise.block_on_async_exn
        @@ fun () -> emergency_da_apply emergency_da_witness_3

      let emergency_da_out_4, _emergency_da_proof_4 =
        Promise.block_on_async_exn
        @@ fun () -> emergency_da_apply emergency_da_witness_4

      let emergency_da_action_fields_1 =
        action_fields_of_emergency_output emergency_da_out_1

      let emergency_da_action_fields_2 =
        action_fields_of_emergency_output emergency_da_out_2

      let emergency_da_action_fields_3 =
        action_fields_of_emergency_output emergency_da_out_3

      let emergency_da_action_fields_4 =
        action_fields_of_emergency_output emergency_da_out_4

      let emergency_da_action_1 =
        emergency_da_action ~source_ledger_hash:ledger0
          ~target_ledger_hash:ledger1
          ~ledger_index:(Zeko_util.Checked32.of_int fee_payer_index)
          ~account:fee_payer_acc_after_first

      let emergency_da_action_2 =
        emergency_da_action ~source_ledger_hash:ledger1
          ~target_ledger_hash:ledger2
          ~ledger_index:(Zeko_util.Checked32.of_int inner_index)
          ~account:old_inner_acc

      let emergency_da_action_3 =
        emergency_da_action ~source_ledger_hash:ledger2
          ~target_ledger_hash:ledger3
          ~ledger_index:(Zeko_util.Checked32.of_int fee_payer_index)
          ~account:fee_payer_acc_after_third

      let emergency_da_action_4 =
        emergency_da_action ~source_ledger_hash:ledger3
          ~target_ledger_hash:ledger4
          ~ledger_index:(Zeko_util.Checked32.of_int new_index)
          ~account:new_account_created

      let expected_action_fields_1 = action_to_fields emergency_da_action_1

      let expected_action_fields_2 = action_to_fields emergency_da_action_2

      let expected_action_fields_3 = action_to_fields emergency_da_action_3

      let expected_action_fields_4 = action_to_fields emergency_da_action_4

      let () =
        assert_action_fields_equal expected_action_fields_1
          emergency_da_action_fields_1

      let () =
        assert_action_fields_equal expected_action_fields_2
          emergency_da_action_fields_2

      let () =
        assert_action_fields_equal expected_action_fields_3
          emergency_da_action_fields_3

      let () =
        assert_action_fields_equal expected_action_fields_4
          emergency_da_action_fields_4

      let () =
        assert (
          Mina_base.Ledger_hash.equal emergency_da_action_1.source_ledger_hash
            ledger0 )

      let () =
        assert (
          Mina_base.Ledger_hash.equal emergency_da_action_1.target_ledger_hash
            ledger1 )

      let () =
        assert (
          Mina_base.Ledger_hash.equal emergency_da_action_2.source_ledger_hash
            ledger1 )

      let () =
        assert (
          Mina_base.Ledger_hash.equal emergency_da_action_2.target_ledger_hash
            ledger2 )

      let () =
        assert (
          Mina_base.Ledger_hash.equal emergency_da_action_3.source_ledger_hash
            ledger2 )

      let () =
        assert (
          Mina_base.Ledger_hash.equal emergency_da_action_3.target_ledger_hash
            ledger3 )

      let () =
        assert (
          Mina_base.Ledger_hash.equal emergency_da_action_4.source_ledger_hash
            ledger3 )

      let () =
        assert (
          Mina_base.Ledger_hash.equal emergency_da_action_4.target_ledger_hash
            ledger4 )

      let emergency_da_actions =
        [ emergency_da_action_1
        ; emergency_da_action_2
        ; emergency_da_action_3
        ; emergency_da_action_4
        ]

      let action_state_from_actions =
        let (Typ typ) = Rule_emergency_da.Action.typ in
        List.fold emergency_da_actions
          ~init:Mina_base.Zkapp_account.Actions.empty_state_element
          ~f:(fun acc action ->
            let action_fields, _aux = typ.value_to_fields action in
            let actions =
              Mina_base.Zkapp_account.Actions.of_event_list [ action_fields ]
            in
            Mina_base.Zkapp_account.Actions.push_events acc actions )

      let emergency_da_source_stmt : Emergency_da_folder.Stmt.t =
        { source_ledger = stmt.source_ledger
        ; target_ledger = stmt.source_ledger
        ; target_action_state =
            Mina_base.Zkapp_account.Actions.empty_state_element
        }

      let ( ({ source = emergency_da_proof_source
             ; target = emergency_da_proof_target
             } :
              Emergency_da_folder.trans )
          , emergency_da_proof ) =
        Promise.block_on_async_exn
        @@ fun () ->
        (Lazy.force Emergency_da_folder.leaf_option)
          ( List.map emergency_da_actions
              ~f:(fun action : Emergency_da_folder.Elem.t ->
                { Emergency_da_folder.Elem.advance = true; action } )
          , emergency_da_source_stmt )

      let () =
        assert (
          Mina_base.Ledger_hash.equal emergency_da_proof_target.target_ledger
            stmt.target_ledger )

      let () =
        assert (
          Field.equal emergency_da_proof_target.target_action_state
            action_state_from_actions )

      let emergency_da_inst : Rule_commit.Emergency_da_inst.t =
        Rule_commit.Emergency_da_inst.make
          ~proof_source:emergency_da_proof_source
          ~proof_target:emergency_da_proof_target ~proof:emergency_da_proof
          emergency_da_source_stmt []

      let last_commit : Rollup_state.Outer_action.Commit.t =
        { ledger = stmt.source_ledger
        ; inner_action_state = Rollup_state.Inner_action_state.With_length.empty
        ; synchronized_outer_action_state =
            Rollup_state.Outer_action_state.With_length.empty
        ; slot_range =
            { lower = Zeko_util.Slot.zero; upper = Zeko_util.Slot.zero }
        }

      let before_last_commit = Rollup_state.Outer_action_state.empty

      let last_commit_actions_hash, after_last_commit =
        let (Typ typ) =
          Typ.(Field.typ * Rollup_state.Outer_action.Commit.typ)
        in
        let action_fields, _aux =
          typ.value_to_fields (Field.of_int 0, last_commit)
        in
        let actions =
          Mina_base.Zkapp_account.Actions.of_event_list [ action_fields ]
        in
        let after_last_commit_field =
          Mina_base.Zkapp_account.Actions.push_events
            (Rollup_state.Outer_action_state.raw before_last_commit)
            actions
        in
        ( actions.hash
        , Rollup_state.Outer_action_state.unsafe_value_of_field
            after_last_commit_field )

      let count_commits_stmt : Count_commits.Definition.Stmt.t =
        { source_action_state = after_last_commit
        ; target_action_state = after_last_commit
        ; n_commits = Zeko_util.Checked32.zero
        }

      let ( ({ source = count_commits_proof_source
             ; target = count_commits_proof_target
             } :
              Count_commits.trans )
          , count_commits_proof ) =
        Promise.block_on_async_exn
        @@ fun () ->
        (Lazy.force Count_commits.leaf_option) ([], count_commits_stmt)

      let count_commits_init : Count_commits.Definition.Init.t =
        { original_action_state = after_last_commit }

      let count_commits_inst : Rule_commit.Count_commits_inst.t =
        Rule_commit.Count_commits_inst.make
          ~proof_source:count_commits_proof_source
          ~proof_target:count_commits_proof_target ~proof:count_commits_proof
          count_commits_init []

      let Compile_simple.[ verify_emergency_folders ] =
        Lazy.force Rule_commit.Verify_emergency_folders.provers

      let verify_emergency_folders_stmt, verify_emergency_folders_proof =
        Promise.block_on_async_exn
        @@ fun () ->
        verify_emergency_folders (count_commits_inst, emergency_da_inst)

      let verify_emergency_folders =
        Rule_commit.Verify_emergency_folders.make_unchecked
          ~proof:verify_emergency_folders_proof verify_emergency_folders_stmt

      let Compile_simple.[ verify_base ] =
        Lazy.force Rule_commit.Verify_base.provers

      let ase_outer_source =
        Rollup_state.Outer_action_state.raw
          Rollup_state.Outer_action_state.empty

      let ase_outer_target =
        Rollup_state.Outer_action_state.raw after_last_commit

      let ase_outer_emergency : Rule_commit.Ase_outer_inst.t =
        Rule_commit.Ase_outer_inst.make ~proof_source:ase_outer_source
          ~proof_target:ase_outer_target ase_outer_source
          [ last_commit_actions_hash ]

      let verify_both_ases_emergency_stmt, verify_both_ases_emergency_proof =
        Promise.block_on_async_exn
        @@ fun () -> prove_both (ase_outer_emergency, ase_inner)

      let verify_both_ases_emergency =
        Rule_commit.Verify_both_ases.make_unchecked
          ~proof:verify_both_ases_emergency_proof
          verify_both_ases_emergency_stmt

      let verify_base_stmt, verify_base_proof =
        Promise.block_on_async_exn
        @@ fun () ->
        verify_base
          (Txn_rules.make_unchecked ~proof stmt, verify_both_ases_emergency)

      let verify_base =
        Rule_commit.Verify_base.make_unchecked ~proof:verify_base_proof
          verify_base_stmt

      let base_witness_emergency =
        { base_witness with
          slot_range =
            { lower = Zeko_util.Slot.of_int 128
            ; upper = Zeko_util.Slot.of_int 128
            }
        }

      let emergency_witness :
          Outer_rules_inst.Rule_commit_inst.Emergency_commit.Witness.t =
        { base_witness = base_witness_emergency
        ; before_last_commit
        ; last_commit
        ; verify_emergency_folders
        ; verify_base
        }

      let _stmt, _proof =
        Promise.block_on_async_exn
        @@ fun () -> emergency_commit emergency_witness
    end in
    (commit_stmt, commit_proof)
end

open struct
  open Mina_base
  open Zeko_util

  let value_to_fields (type var value) (typ : (var, value) Typ.t) (x : value) :
      Field.t array =
    let (Typ typ) = typ in
    let fields, _aux = typ.value_to_fields x in
    fields

  let value_to_hash ~(init : string)
      (typ : ('var, 'value) Snark_params.Tick.Typ.t) (x : 'value) : Field.t =
    let (Typ typ) = typ in
    let fields, _aux = typ.value_to_fields x in
    Random_oracle.hash ~init:(Hash_prefix_create.salt init) fields

  let commit_to_actions x =
    [ value_to_fields
        Typ.(F.typ * Rollup_state.Outer_action.Commit.typ)
        (Field.of_int 0, x)
    ]

  let witness_to_actions x =
    let fields =
      value_to_fields
        Typ.(F.typ * Rollup_state.Outer_action.Witness.typ)
        (Field.of_int 1, x)
    in
    [ fields ]

  let witness_without_forest_to_actions x =
    let fields =
      value_to_fields
        Typ.(F.typ * Rollup_state.Outer_action.Witness.Without_forest.typ)
        (Field.of_int 1, x)
    in
    [ fields ]

  let outer_action_of_actions : field array list -> Rollup_state.Outer_action.t
      = function
    | [ x ] ->
        if Field.equal x.(0) Field.zero then
          let (Typ typ) = Rollup_state.Outer_action.Commit.typ in
          Commit
            (typ.value_of_fields
               ( Array.to_list x |> List.tl_exn |> Array.of_list
               , typ.constraint_system_auxiliary () ) )
        else if Field.equal x.(0) Field.one then
          let (Typ typ) =
            Rollup_state.Outer_action.Witness.Without_forest.typ
          in
          Witness
            (typ.value_of_fields
               ( Array.to_list x |> List.tl_exn |> Array.of_list
               , typ.constraint_system_auxiliary () ) )
        else failwith __LOC__
    | _ ->
        failwith __LOC__

  module Inputs = struct
    let inner_public_key = point_of_string "39992"

    let chain_l1 = Mina_signature_kind.Testnet

    let chain_l2 = Mina_signature_kind.Testnet

    let max_valid_while_size = 128

    let holder_accounts_l1 =
      [ point_of_string "89888"
      ; point_of_string "46532"
      ; point_of_string "46513"
      ]

    let holder_account_l2 = point_of_string "11111"

    let helper_token_owner_l1 = point_of_string "5123111"

    let zeko_l1 = point_of_string "39921"

    let zeko_l2 = inner_public_key

    let emergency_da_public_key = point_of_string "44444"

    let withdrawal_delay = Mina_numbers.Global_slot_span.of_string "5"

    let max_sequencer_inactivity = 128

    let holder_account_l1_permissions_enabled : Mina_base.Permissions.t =
      { edit_state = Proof
      ; access = None
      ; send = Proof
      ; receive = None
      ; set_delegate = Impossible
      ; set_permissions = Proof
      ; set_verification_key =
          (Proof, Mina_numbers.Txn_version.current) (* TODO: correct? *)
      ; set_zkapp_uri = Impossible
      ; edit_action_state = Impossible
      ; set_token_symbol = Impossible
      ; increment_nonce = Impossible
      ; set_voting_for = Impossible
      ; set_timing = Impossible
      }

    let holder_account_l1_permissions_disabled : Mina_base.Permissions.t =
      { edit_state = Proof
      ; access = None
      ; send = Impossible
      ; receive = None
      ; set_delegate = Impossible
      ; set_permissions = Proof
      ; set_verification_key =
          (Proof, Mina_numbers.Txn_version.current) (* TODO: correct? *)
      ; set_zkapp_uri = Impossible
      ; edit_action_state = Impossible
      ; set_token_symbol = Impossible
      ; increment_nonce = Impossible
      ; set_voting_for = Impossible
      ; set_timing = Impossible
      }
  end

  module Bridge = Bridge_rules.Make_mina (Inputs) ()

  module Outer_rules = Outer_rules.Make (Inputs) ()

  module Inner_rules = Inner_rules.Make (Inputs) ()

  let Compile_simple.[ cancel_deposit; finalize_withdrawal; _ ] =
    Lazy.force Bridge.System_L1_enabled.provers

  let Compile_simple.[ finalize_deposit; inner_receive ] =
    Lazy.force Bridge.System_L2.provers

  let Compile_simple.[ outer_token_owner ] =
    Lazy.force Bridge.System_L1_token_owner.provers

  let Compile_simple.[ _; _; outer_action_witness; _ ] =
    Lazy.force Outer_rules.provers

  let Compile_simple.[ _; inner_action_witness ] =
    Lazy.force Inner_rules.provers

  module Deposit = struct
    let recipient = Keypair.create ()

    let amount = Currency.Amount.of_mina_string_exn "1"

    let deposit_params : Bridge_state.Deposit_params_base.t =
      { children = []
      ; holder_account_l1 =
          List.random_element Inputs.holder_accounts_l1 |> Option.value_exn
      ; amount
      ; recipient = Public_key.compress recipient.public_key
      ; timeout = Slot.of_int 50
      }

    let deposit_slot_range : Slot_range.t = Slot_range.infinite

    let commit_slot_range : Slot_range.t =
      { lower = Slot.zero; upper = Slot.of_int 30 }

    let deposit_witness : Rollup_state.Outer_action.Witness.t =
      { aux =
          value_to_hash ~init:Zeko_constants.deposit_salt
            Bridge_state.Deposit_params_base.typ deposit_params
      ; children =
          Zkapp_command.Call_forest.cons ~signature_kind:Inputs.chain_l1
            (Account_update.with_aux
               ~body:
                 { Mina_base.Account_update.Body.dummy with
                   use_full_commitment = true
                 ; public_key = deposit_params.holder_account_l1
                 ; balance_change = Currency.Amount.Signed.(of_unsigned amount)
                 ; may_use_token = Parents_own_token
                 ; authorization_kind = None_given
                 }
               ~authorization:
                 (* Account_update.Checked.t == Account_update.Body.Checked.t so authorization is dropped anyways *)
                 Control.Poly.None_given )
            []
      ; slot_range = deposit_slot_range
      }

    let original_action_state = Zkapp_account.Actions.empty_state_element

    let deposit_action =
      let (_stmt, (au, _au_digest, calls)), _proof =
        Promise.block_on_async_exn
        @@ fun () ->
        outer_action_witness
          { public_key = Inputs.zeko_l1
          ; vk_hash =
              ( Promise.block_on_async_exn
              @@ fun () ->
              Compile_simple.Verification_key.of_tag
                (Lazy.force Outer_rules.tag) )
              |> Compile_simple.Verification_key.hash
          ; witness = deposit_witness
          }
      in
      assert (Public_key.Compressed.equal au.public_key Inputs.zeko_l1) ;
      assert (
        List.equal
          (fun a b ->
            let a = With_stack_hash.stack_hash a in
            let b = With_stack_hash.stack_hash b in
            Zkapp_command.Digest.Forest.equal a b )
          calls deposit_witness.children ) ;
      let h = Zkapp_account.Actions_impl.hash au.actions in
      let actions = witness_to_actions deposit_witness in
      let () =
        match outer_action_of_actions actions with
        | Witness _witness ->
            ()
        | Commit _ ->
            failwith __LOC__
      in
      let h' = Zkapp_account.Actions_impl.hash actions in
      let h'' =
        Zkapp_account.Actions_impl.hash
          (witness_without_forest_to_actions
             { aux = deposit_witness.aux
             ; children_digest =
                 Zkapp_command.Call_forest.hash deposit_witness.children
             ; slot_range = deposit_witness.slot_range
             } )
      in
      assert (Field.equal h h') ;
      assert (Field.equal h h'') ;
      h

    let mid_outer_action_state =
      Rollup_state.Outer_action_state.(
        With_length.unsafe_value_of_fields
          ~state:
            ( Zkapp_account.Actions_impl.push_hash original_action_state
                deposit_action
            |> unsafe_value_of_field )
          ~length:Checked32.one)

    let commit_witness : Rollup_state.Outer_action.Commit.t =
      { ledger = Field.zero
      ; inner_action_state = Rollup_state.Inner_action_state.With_length.empty
      ; synchronized_outer_action_state = mid_outer_action_state
      ; slot_range = commit_slot_range
      }

    let commit_action =
      let actions = commit_to_actions commit_witness in
      let () =
        match outer_action_of_actions actions with
        | Commit _commit_witness ->
            ()
        | Witness _ ->
            failwith __LOC__
      in
      Zkapp_account.Actions_impl.hash actions

    let target_outer_action_state =
      Rollup_state.Outer_action_state.(
        With_length.unsafe_value_of_fields
          ~state:
            ( Zkapp_account.Actions_impl.push_hash
                Rollup_state.Outer_action_state.(
                  With_length.state mid_outer_action_state |> raw)
                commit_action
            |> unsafe_value_of_field )
          ~length:(Checked32.of_int 2))

    let () =
      let check_accepted =
        let Bridge.Check_accepted.{ source; target }, proof =
          Promise.block_on_async_exn
          @@ fun () ->
          (Lazy.force Bridge.Check_accepted.leaf_option)
            ( [ Commit commit_witness ]
            , { params = deposit_params
              ; action_state =
                  Rollup_state.Outer_action_state.With_length.state
                    mid_outer_action_state
              ; deposit_index = Checked32.zero
              ; n_steps = Checked32.zero
              ; is_rejected = false
              ; is_accepted = false
              } )
        in
        assert (Bool.(target.is_accepted = true)) ;
        assert (Bool.(target.is_rejected = false)) ;
        assert (
          Field.equal
            Rollup_state.Outer_action_state.(
              With_length.state target_outer_action_state |> raw)
            (Rollup_state.Outer_action_state.raw target.action_state) ) ;
        Bridge.Rule_bridge_finalize_deposit.Check_accepted_inst.make
          ~proof_source:source ~proof_target:target ~proof
          { params = deposit_params
          ; original_action_state =
              Rollup_state.Outer_action_state.unsafe_value_of_field
                original_action_state
          ; deposit_index = Checked32.zero
          }
          []
      in
      let (_stmt, (au, _au_digest, calls)), _proof =
        Promise.block_on_async_exn
        @@ fun () ->
        finalize_deposit
          { public_key = Inputs.holder_account_l2
          ; vk_hash =
              ( Promise.block_on_async_exn
              @@ fun () ->
              Compile_simple.Verification_key.of_tag
                (Lazy.force Bridge.System_L2.tag) )
              |> Compile_simple.Verification_key.hash
          ; may_use_token = Bridge.Rule_bridge_finalize_deposit.May_use_token.No
          ; inner_authorization_kind = Rule_bridge_finalize_deposit.A.None_given
          ; ase =
              (let action_state : Ase.With_length.Stmt.t =
                 { action_state =
                     Rollup_state.Outer_action_state.With_length.state
                       target_outer_action_state
                     |> Rollup_state.Outer_action_state.raw
                 ; length =
                     Rollup_state.Outer_action_state.With_length.length
                       target_outer_action_state
                 }
               in
               Bridge.Rule_bridge_finalize_deposit.Ase_inst.make
                 ~proof_source:action_state ~proof_target:action_state
                 action_state [] )
          ; check_accepted
          ; prev_next_deposit = Checked32.zero
          }
      in
      assert (
        Currency.Amount.Signed.equal au.balance_change
          Currency.Amount.Signed.(of_unsigned amount |> negate) ) ;
      assert (Public_key.Compressed.equal au.public_key Inputs.holder_account_l2) ;
      let helper_account, witness_inner =
        match Zkapp_command.Call_forest.to_account_updates calls with
        | [ helper_account; witness_inner ] ->
            (helper_account, witness_inner)
        | _ ->
            failwith
              "finalize_deposit calls: no helper account or witness inner"
      in
      assert (
        Account_id.equal
          (Account_update.account_id helper_account)
          (Account_id.create
             (Public_key.compress recipient.public_key)
             (Account_id.derive_token_id
                ~owner:
                  ( Account_id.of_public_key
                  @@ Public_key.decompress_exn Inputs.holder_account_l2 ) ) ) ) ;
      assert (
        Zkapp_state.State_length_vec.equal
          (fun a b -> Zkapp_basic.Or_ignore.equal Field.equal a b)
          helper_account.body.preconditions.account.state
          Zkapp_state.State_length_vec.(
            of_list_exn
              [ Zkapp_basic.Or_ignore.Check Checked32.(to_field zero)
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ]) ) ;
      assert (
        Zkapp_state.State_length_vec.equal
          (fun a b -> Zkapp_basic.Set_or_keep.equal Field.equal a b)
          helper_account.body.update.app_state
          Zkapp_state.State_length_vec.(
            of_list_exn
              [ Zkapp_basic.Set_or_keep.Set Checked32.(to_field one)
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ]) ) ;

      assert (
        Public_key.Compressed.equal witness_inner.body.public_key Inputs.zeko_l2 ) ;
      assert (
        Zkapp_state.State_length_vec.equal
          (fun a b -> Zkapp_basic.Or_ignore.equal Field.equal a b)
          witness_inner.body.preconditions.account.state
          Zkapp_state.State_length_vec.(
            of_list_exn
              [ Zkapp_basic.Or_ignore.Check
                  Rollup_state.Outer_action_state.(
                    With_length.state target_outer_action_state |> raw)
              ; Zkapp_basic.Or_ignore.Check
                  Rollup_state.Outer_action_state.(
                    With_length.length target_outer_action_state
                    |> Checked32.to_field)
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ]) )
  end

  module Cancel_deposit = struct
    let recipient = Keypair.create ()

    let amount = Currency.Amount.of_mina_string_exn "5"

    let deposit_params : Bridge_state.Deposit_params_base.t =
      { children = []
      ; holder_account_l1 =
          List.random_element Inputs.holder_accounts_l1 |> Option.value_exn
      ; amount
      ; recipient = Public_key.compress recipient.public_key
      ; timeout = Slot.of_int 50
      }

    let deposit_slot_range : Slot_range.t = Slot_range.infinite

    (* Lower range later than the timeout of the deposit *)
    let commit_slot_range : Slot_range.t =
      { lower = Slot.of_int 60; upper = Slot.of_int 70 }

    let deposit_witness : Rollup_state.Outer_action.Witness.t =
      { aux =
          value_to_hash ~init:Zeko_constants.deposit_salt
            Bridge_state.Deposit_params_base.typ deposit_params
      ; children =
          Zkapp_command.Call_forest.cons ~signature_kind:Inputs.chain_l1
            (Account_update.with_aux
               ~body:
                 { Mina_base.Account_update.Body.dummy with
                   use_full_commitment = true
                 ; public_key = deposit_params.holder_account_l1
                 ; balance_change = Currency.Amount.Signed.(of_unsigned amount)
                 ; may_use_token = Parents_own_token
                 ; authorization_kind = None_given
                 }
               ~authorization:
                 (* Account_update.Checked.t == Account_update.Body.Checked.t so authorization is dropped anyways *)
                 Control.Poly.None_given )
            []
      ; slot_range = deposit_slot_range
      }

    let original_action_state = Zkapp_account.Actions.empty_state_element

    let deposit_action =
      let (_stmt, (au, _au_digest, calls)), _proof =
        Promise.block_on_async_exn
        @@ fun () ->
        outer_action_witness
          { public_key = Inputs.zeko_l1
          ; vk_hash =
              ( Promise.block_on_async_exn
              @@ fun () ->
              Compile_simple.Verification_key.of_tag
                (Lazy.force Outer_rules.tag) )
              |> Compile_simple.Verification_key.hash
          ; witness = deposit_witness
          }
      in
      assert (Public_key.Compressed.equal au.public_key Inputs.zeko_l1) ;
      assert (
        List.equal
          (fun a b ->
            let a = With_stack_hash.stack_hash a in
            let b = With_stack_hash.stack_hash b in
            Zkapp_command.Digest.Forest.equal a b )
          calls deposit_witness.children ) ;
      let h = Zkapp_account.Actions_impl.hash au.actions in
      let actions = witness_to_actions deposit_witness in
      let () =
        match outer_action_of_actions actions with
        | Witness _witness ->
            ()
        | Commit _ ->
            failwith __LOC__
      in
      let h' = Zkapp_account.Actions_impl.hash actions in
      let h'' =
        Zkapp_account.Actions_impl.hash
          (witness_without_forest_to_actions
             { aux = deposit_witness.aux
             ; children_digest =
                 Zkapp_command.Call_forest.hash deposit_witness.children
             ; slot_range = deposit_witness.slot_range
             } )
      in
      assert (Field.equal h h') ;
      assert (Field.equal h h'') ;
      h

    let mid_outer_action_state =
      Rollup_state.Outer_action_state.(
        With_length.unsafe_value_of_fields
          ~state:
            ( Zkapp_account.Actions_impl.push_hash original_action_state
                deposit_action
            |> unsafe_value_of_field )
          ~length:Checked32.one)

    let commit_witness : Rollup_state.Outer_action.Commit.t =
      { ledger = Field.zero
      ; inner_action_state = Rollup_state.Inner_action_state.With_length.empty
      ; synchronized_outer_action_state = mid_outer_action_state
      ; slot_range = commit_slot_range
      }

    let commit_action =
      let actions = commit_to_actions commit_witness in
      let () =
        match outer_action_of_actions actions with
        | Commit _commit_witness ->
            ()
        | Witness _ ->
            failwith __LOC__
      in
      Zkapp_account.Actions_impl.hash actions

    let target_outer_action_state =
      Rollup_state.Outer_action_state.(
        With_length.unsafe_value_of_fields
          ~state:
            ( Zkapp_account.Actions_impl.push_hash
                Rollup_state.Outer_action_state.(
                  With_length.state mid_outer_action_state |> raw)
                commit_action
            |> unsafe_value_of_field )
          ~length:(Checked32.of_int 2))

    let () =
      let verify_two_outer_ases =
        let commit_ase =
          let action_state =
            Rollup_state.Outer_action_state.With_length.raw
              target_outer_action_state
          in
          Bridge.Rule_bridge_finalize_cancelled_deposit.Ase_outer_inst.make
            ~proof_source:action_state ~proof_target:action_state action_state
            []
        in
        let sync_ase =
          let action_state : Ase.With_length.Stmt.t =
            { action_state =
                Rollup_state.Outer_action_state.With_length.raw
                  mid_outer_action_state
            ; length =
                Rollup_state.Outer_action_state.With_length.length
                  mid_outer_action_state
            }
          in
          Bridge.Rule_bridge_finalize_cancelled_deposit
          .Ase_outer_with_length_inst
          .make ~proof_source:action_state ~proof_target:action_state
            action_state [ commit_action ]
        in
        let [ prover ] =
          Lazy.force
            Bridge.Rule_bridge_finalize_cancelled_deposit.Verify_two_outer_ases
            .provers
        in
        let stmt, proof =
          Promise.block_on_async_exn @@ fun () -> prover (commit_ase, sync_ase)
        in
        Bridge.Rule_bridge_finalize_cancelled_deposit.Verify_two_outer_ases
        .make_unchecked ~proof stmt
      in
      let verify_check_accepted_and_ase =
        let check_accepted =
          let Bridge.Check_accepted.{ source; target }, proof =
            Promise.block_on_async_exn
            @@ fun () ->
            (Lazy.force Bridge.Check_accepted.leaf_option)
              ( [ Commit commit_witness ]
              , { params = deposit_params
                ; action_state =
                    Rollup_state.Outer_action_state.With_length.state
                      mid_outer_action_state
                ; deposit_index = Checked32.zero
                ; n_steps = Checked32.zero
                ; is_rejected = false
                ; is_accepted = false
                } )
          in
          assert (Bool.(target.is_accepted = false)) ;
          assert (Bool.(target.is_rejected = true)) ;
          assert (
            Field.equal
              Rollup_state.Outer_action_state.(
                With_length.state target_outer_action_state |> raw)
              (Rollup_state.Outer_action_state.raw target.action_state) ) ;
          Bridge.Rule_bridge_finalize_cancelled_deposit.Check_accepted_inst.make
            ~proof_source:source ~proof_target:target ~proof
            { params = deposit_params
            ; original_action_state =
                Rollup_state.Outer_action_state.unsafe_value_of_field
                  original_action_state
            ; deposit_index = Checked32.zero
            }
            []
        in
        let check_accepted_ase =
          let action_state : Ase.With_length.Stmt.t =
            { action_state =
                Rollup_state.Outer_action_state.With_length.raw
                  target_outer_action_state
            ; length =
                Rollup_state.Outer_action_state.With_length.length
                  target_outer_action_state
            }
          in
          Bridge.Rule_bridge_finalize_cancelled_deposit
          .Ase_outer_with_length_inst
          .make ~proof_source:action_state ~proof_target:action_state
            action_state []
        in
        let [ prover ] =
          Lazy.force
            Bridge.Rule_bridge_finalize_cancelled_deposit
            .Verify_check_accepted_and_ase
            .provers
        in
        let stmt, proof =
          Promise.block_on_async_exn
          @@ fun () -> prover (check_accepted, check_accepted_ase)
        in
        Bridge.Rule_bridge_finalize_cancelled_deposit
        .Verify_check_accepted_and_ase
        .make_unchecked ~proof stmt
      in
      let holder_l1 =
        Inputs.holder_accounts_l1 |> List.random_element |> Option.value_exn
      in
      let (_stmt, (au, _au_digest, calls)), _proof =
        Promise.block_on_async_exn
        @@ fun () ->
        cancel_deposit
          { public_key = holder_l1
          ; vk_hash =
              ( Promise.block_on_async_exn
              @@ fun () ->
              Compile_simple.Verification_key.of_tag
                (Lazy.force Bridge.System_L1_enabled.tag) )
              |> Compile_simple.Verification_key.hash
          ; may_use_token =
              Bridge.Rule_bridge_finalize_cancelled_deposit.May_use_token.No
          ; outer_authorization_kind =
              Rule_bridge_finalize_cancelled_deposit.A.None_given
          ; commit = commit_witness
          ; before_commit_ase =
              Rollup_state.Outer_action_state.With_length.state
                mid_outer_action_state
          ; verify_two_outer_ases
          ; verify_check_accepted_and_ase
          ; prev_next_cancelled_deposit = Checked32.zero
          ; helper_token_owner_l1_vk_hash =
              ( Promise.block_on_async_exn
              @@ fun () ->
              Compile_simple.Verification_key.of_tag
                (Lazy.force Bridge.System_L1_token_owner.tag) )
              |> Compile_simple.Verification_key.hash
          }
      in
      assert (
        Currency.Amount.Signed.equal au.balance_change
          Currency.Amount.Signed.(of_unsigned amount |> negate) ) ;
      assert (Public_key.Compressed.equal au.public_key holder_l1) ;

      let (helper_token_owner, helper_account), witness_outer =
        match calls with
        | [ { elt =
                { account_update = helper_token_owner
                ; calls =
                    [ { elt = { account_update = helper_account; calls = []; _ }
                      ; _
                      }
                    ]
                ; _
                }
            ; _
            }
          ; { elt = { account_update = witness_outer; calls = []; _ }; _ }
          ] ->
            ((helper_token_owner, helper_account), witness_outer)
        | _ ->
            failwith
              "finalize_withdrawal calls: no helper token owner or witness \
               outer"
      in

      assert (
        Public_key.Compressed.equal witness_outer.body.public_key Inputs.zeko_l1 ) ;
      assert (
        Zkapp_basic.Or_ignore.equal Field.equal
          witness_outer.body.preconditions.account.action_state
          (Zkapp_basic.Or_ignore.Check
             (Rollup_state.Outer_action_state.With_length.raw
                target_outer_action_state ) ) ) ;

      assert (
        Public_key.Compressed.equal helper_account.body.public_key
          (Public_key.compress recipient.public_key) ) ;
      assert (
        Token_id.equal helper_account.body.token_id
          (Account_id.derive_token_id
             ~owner:
               ( Account_id.of_public_key
               @@ Public_key.decompress_exn Inputs.helper_token_owner_l1 ) ) ) ;

      assert (
        Zkapp_state.State_length_vec.equal
          (fun a b -> Zkapp_basic.Or_ignore.equal Field.equal a b)
          helper_account.body.preconditions.account.state
          Zkapp_state.State_length_vec.(
            of_list_exn
              [ Zkapp_basic.Or_ignore.Check Checked32.(to_field zero)
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ]) ) ;
      assert (
        Zkapp_state.State_length_vec.equal
          (fun a b -> Zkapp_basic.Set_or_keep.equal Field.equal a b)
          helper_account.body.update.app_state
          Zkapp_state.State_length_vec.(
            of_list_exn
              [ Zkapp_basic.Set_or_keep.Set Checked32.(to_field one)
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ]) ) ;
      assert (
        Public_key.Compressed.equal helper_token_owner.body.public_key
          Inputs.helper_token_owner_l1 ) ;

      let (_stmt, (helper_token_owner', _au_digest, calls)), _proof =
        Promise.block_on_async_exn
        @@ fun () ->
        outer_token_owner
          { public_key = Inputs.helper_token_owner_l1
          ; vk_hash =
              ( Promise.block_on_async_exn
              @@ fun () ->
              Compile_simple.Verification_key.of_tag
                (Lazy.force Bridge.System_L1_token_owner.tag) )
              |> Compile_simple.Verification_key.hash
          ; a = helper_account.body
          }
      in
      let helper_account' =
        match calls with
        | [ { elt = { account_update; calls = []; _ }; _ } ] ->
            account_update
        | _ ->
            failwith "outer_token_owner calls: no helper account"
      in
      assert (
        Account_update.Body.equal helper_token_owner' helper_token_owner.body ) ;
      assert (Account_update.Body.equal helper_account'.body helper_account.body)
  end

  module Withdrawal = struct
    let recipient = Keypair.create ()

    let amount = Currency.Amount.of_mina_string_exn "2"

    let withdrawal_params : Bridge_state.Withdrawal_params_base.t =
      { children = []
      ; amount
      ; recipient = Public_key.compress recipient.public_key
      }

    let inner_receive_au =
      let (_stmt, (au, _au_digest, calls)), _proof =
        Promise.block_on_async_exn
        @@ fun () ->
        inner_receive
          { public_key = Inputs.holder_account_l2
          ; vk_hash =
              ( Promise.block_on_async_exn
              @@ fun () ->
              Compile_simple.Verification_key.of_tag
                (Lazy.force Inner_rules.tag) )
              |> Compile_simple.Verification_key.hash
          ; amount
          }
      in
      assert (Zkapp_command.Call_forest.is_empty calls) ;
      assert (
        Currency.Amount.Signed.equal au.balance_change
          Currency.Amount.Signed.(of_unsigned amount) ) ;
      assert (Public_key.Compressed.equal au.public_key Inputs.holder_account_l2) ;
      au

    let withdrawal_witness : Rollup_state.Inner_action.t =
      { aux =
          value_to_hash ~init:Zeko_constants.withdrawal_salt
            Bridge_state.Withdrawal_params_base.typ withdrawal_params
      ; children =
          Zkapp_command.Call_forest.cons ~signature_kind:Inputs.chain_l2
            (Account_update.with_aux ~body:inner_receive_au
               ~authorization:
                 (* Account_update.Checked.t == Account_update.Body.Checked.t so authorization is dropped anyways *)
                 Control.Poly.None_given )
            []
      }

    let withdrawal_action =
      let (_stmt, (au, _au_digest, calls)), _proof =
        Promise.block_on_async_exn
        @@ fun () ->
        inner_action_witness
          { public_key = Inputs.zeko_l2
          ; vk_hash =
              ( Promise.block_on_async_exn
              @@ fun () ->
              Compile_simple.Verification_key.of_tag
                (Lazy.force Inner_rules.tag) )
              |> Compile_simple.Verification_key.hash
          ; witness = withdrawal_witness
          }
      in
      assert (
        List.equal
          (fun a b ->
            let a = With_stack_hash.stack_hash a in
            let b = With_stack_hash.stack_hash b in
            Zkapp_command.Digest.Forest.equal a b )
          calls withdrawal_witness.children ) ;
      let h = Zkapp_account.Actions_impl.hash au.actions in
      let h' =
        Zkapp_account.Actions_impl.hash
          [ value_to_fields
              Typ.(F.typ * Rollup_state.Inner_action.typ)
              (Field.of_int 0, withdrawal_witness)
          ]
      in
      assert (Field.equal h h') ;
      h

    let inner_action_state : Rollup_state.Inner_action_state.With_length.t =
      Rollup_state.Inner_action_state.(
        With_length.unsafe_value_of_fields
          ~state:
            ( Zkapp_account.Actions_impl.push_hash
                Zkapp_account.Actions.empty_state_element withdrawal_action
            |> unsafe_value_of_field )
          ~length:Checked32.one)

    let commit : Rollup_state.Outer_action.Commit.t =
      { ledger = Field.zero
      ; inner_action_state
      ; synchronized_outer_action_state =
          Rollup_state.Outer_action_state.With_length.empty
      ; slot_range = { lower = Slot.zero; upper = Slot.one }
      }

    let () =
      let (commit_ase, commit_ase_target)
            : Bridge.Rule_bridge_finalize_withdrawal.Ase_outer_inst.t * Field.t
          =
        let action_state =
          Zkapp_account.Actions_impl.(
            push_hash Zkapp_account.Actions.empty_state_element
              (hash (commit_to_actions commit)))
        in
        ( Bridge.Rule_bridge_finalize_withdrawal.Ase_outer_inst.make
            ~proof_source:action_state ~proof_target:action_state action_state
            []
        , action_state )
      in
      let withdrawal_ase :
          Bridge.Rule_bridge_finalize_withdrawal.Ase_inner_inst.t =
        let action_state : Ase.With_length.Stmt.t =
          { action_state =
              Rollup_state.Inner_action_state.With_length.state
                inner_action_state
              |> Rollup_state.Inner_action_state.raw
          ; length =
              Rollup_state.Inner_action_state.With_length.length
                inner_action_state
          }
        in
        Bridge.Rule_bridge_finalize_withdrawal.Ase_inner_inst.make
          ~proof_source:action_state ~proof_target:action_state action_state []
      in
      let holder_l1 =
        List.random_element Inputs.holder_accounts_l1 |> Option.value_exn
      in
      let (_stmt, (au, _au_digest, calls)), _proof =
        Promise.block_on_async_exn
        @@ fun () ->
        finalize_withdrawal
          { public_key = holder_l1
          ; vk_hash =
              ( Promise.block_on_async_exn
              @@ fun () ->
              Compile_simple.Verification_key.of_tag
                (Lazy.force Bridge.System_L1_enabled.tag) )
              |> Compile_simple.Verification_key.hash
          ; may_use_token = Bridge.Rule_bridge_finalize_deposit.May_use_token.No
          ; outer_authorization_kind =
              Rule_bridge_finalize_withdrawal.A.None_given
          ; commit
          ; before_commit = Rollup_state.Outer_action_state.empty
          ; commit_ase
          ; before_withdrawal = Rollup_state.Inner_action_state.empty
          ; withdrawal_ase
          ; prev_next_withdrawal = Checked32.zero
          ; withdrawal_params
          ; helper_token_owner_l1_vk_hash =
              ( Promise.block_on_async_exn
              @@ fun () ->
              Compile_simple.Verification_key.of_tag
                (Lazy.force Bridge.System_L1_token_owner.tag) )
              |> Compile_simple.Verification_key.hash
          ; l2_holder_vk_hash =
              ( Promise.block_on_async_exn
              @@ fun () ->
              Compile_simple.Verification_key.of_tag
                (Lazy.force Inner_rules.tag) )
              |> Compile_simple.Verification_key.hash
          }
      in
      assert (
        Currency.Amount.Signed.equal au.balance_change
          Currency.Amount.Signed.(of_unsigned amount |> negate) ) ;
      assert (Public_key.Compressed.equal au.public_key holder_l1) ;

      let (helper_token_owner, helper_account), witness_outer =
        match calls with
        | [ { elt =
                { account_update = helper_token_owner
                ; calls =
                    [ { elt = { account_update = helper_account; calls = []; _ }
                      ; _
                      }
                    ]
                ; _
                }
            ; _
            }
          ; { elt = { account_update = witness_outer; calls = []; _ }; _ }
          ] ->
            ((helper_token_owner, helper_account), witness_outer)
        | _ ->
            failwith
              "finalize_withdrawal calls: no helper token owner or witness \
               outer"
      in

      assert (
        Public_key.Compressed.equal witness_outer.body.public_key Inputs.zeko_l1 ) ;
      assert (
        Zkapp_basic.Or_ignore.equal Field.equal
          witness_outer.body.preconditions.account.action_state
          (Zkapp_basic.Or_ignore.Check commit_ase_target) ) ;

      assert (
        Public_key.Compressed.equal helper_account.body.public_key
          (Public_key.compress recipient.public_key) ) ;
      assert (
        Token_id.equal helper_account.body.token_id
          (Account_id.derive_token_id
             ~owner:
               ( Account_id.of_public_key
               @@ Public_key.decompress_exn Inputs.helper_token_owner_l1 ) ) ) ;

      printf
        !"helper_account.preconditions: %{sexp: Field.t \
          Zkapp_basic.Or_ignore.t Zkapp_state.State_length_vec.t}\n\
          %!"
        helper_account.body.preconditions.account.state ;

      assert (
        Zkapp_state.State_length_vec.equal
          (fun a b -> Zkapp_basic.Or_ignore.equal Field.equal a b)
          helper_account.body.preconditions.account.state
          Zkapp_state.State_length_vec.(
            of_list_exn
              [ Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Check Checked32.(to_field zero)
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ; Zkapp_basic.Or_ignore.Ignore
              ]) ) ;
      assert (
        Zkapp_state.State_length_vec.equal
          (fun a b -> Zkapp_basic.Set_or_keep.equal Field.equal a b)
          helper_account.body.update.app_state
          Zkapp_state.State_length_vec.(
            of_list_exn
              [ Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Set Checked32.(to_field one)
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ; Zkapp_basic.Set_or_keep.Keep
              ]) ) ;
      assert (
        Public_key.Compressed.equal helper_token_owner.body.public_key
          Inputs.helper_token_owner_l1 ) ;

      let (_stmt, (helper_token_owner', _au_digest, calls)), _proof =
        Promise.block_on_async_exn
        @@ fun () ->
        outer_token_owner
          { public_key = Inputs.helper_token_owner_l1
          ; vk_hash =
              ( Promise.block_on_async_exn
              @@ fun () ->
              Compile_simple.Verification_key.of_tag
                (Lazy.force Bridge.System_L1_token_owner.tag) )
              |> Compile_simple.Verification_key.hash
          ; a = helper_account.body
          }
      in
      let helper_account' =
        match calls with
        | [ { elt = { account_update; calls = []; _ }; _ } ] ->
            account_update
        | _ ->
            failwith "outer_token_owner calls: no helper account"
      in
      assert (
        Account_update.Body.equal helper_token_owner' helper_token_owner.body ) ;
      assert (Account_update.Body.equal helper_account'.body helper_account.body)
  end

  include Deposit
  include Cancel_deposit
  include Withdrawal
end

let () = print_endline "Done ✅"
