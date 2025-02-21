open Core_kernel
open Signature_lib
open Snark_params.Tick
open Zeko_circuits

let da_sk =
  Quickcheck.random_value ~seed:(`Deterministic "182128381918") Private_key.gen

let da_key = Public_key.of_private_key_exn da_sk |> Public_key.compress

let () = assert (not da_key.is_odd)

let ase_with_length, ase_with_length_proof =
  let open struct
    let trans0, proof0 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.With_length.leaf
        ( [ Field.one ]
        , { action_state = Field.of_string "6"
          ; length = Unsigned.UInt32.of_string "42"
          } )

    let trans1, proof1 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.With_length.leaf_option ([ Field.of_string "2" ], trans0.target)

    let trans2, proof2 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.With_length.merge
        { left = trans0
        ; left_proof = proof0
        ; right = trans1
        ; right_proof = proof1
        }

    let trans3, proof3 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.With_length.extend ([ Field.of_string "99" ], (trans2, proof2))

    let trans4, proof4 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.With_length.extend_option ([ Field.of_string "99" ], (trans3, proof3))
  end in
  (trans4, proof4)

let ase_without_length =
  let open struct
    let trans0, proof0 =
      Promise.block_on_async_exn
      @@ fun () -> Ase.Without_length.leaf ([ Field.one ], Field.of_string "6")

    let trans1, proof1 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.Without_length.leaf_option ([ Field.of_string "2" ], trans0.target)

    let trans2, proof2 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.Without_length.merge
        { left = trans0
        ; left_proof = proof0
        ; right = trans1
        ; right_proof = proof1
        }

    let trans3, proof3 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.Without_length.extend ([ Field.of_string "99" ], (trans2, proof2))

    let trans4, proof4 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.Without_length.extend_option
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

let _inner_stmt, _inner_proof =
  let open struct
    let Compile_simple.[ sync; action ] = Inner_rules.provers

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

let _outer =
  let open struct
    let Compile_simple.[ _commit; action; pause ] = Outer_rules.provers

    let pause_witness : Rule_pause.Witness.t =
      { public_key = point_of_string_even "1238881"
      ; vk_hash = Field.of_string "19944541415"
      ; pause_key = point_of_string_even "1511111121"
      }

    let _stmt, _proof =
      Promise.block_on_async_exn @@ fun () -> pause pause_witness

    let action_witness : Rule_action_witness.Witness.t =
      { public_key = point_of_string "41889111"
      ; vk_hash = Field.of_string "188188181"
      ; witness =
          { aux = Field.zero
          ; children = []
          ; slot_range =
              { lower = Zeko_util.Slot.zero; upper = Zeko_util.Slot.max_value }
          }
      }

    let _stmt, _proof =
      Promise.block_on_async_exn @@ fun () -> action action_witness

    let Compile_simple.[ prove_both ] = Rule_commit.Verify_both_ases.provers

    let ase_outer =
      let stmt, proof = ase_without_length in
      Rule_commit.Ase_outer_inst.make ~proof_source:stmt.source
        ~proof_target:stmt.target ~proof stmt.source
        [ Field.of_string "849812849581123" ]

    let ase_inner =
      let stmt, proof = (ase_with_length, ase_with_length_proof) in
      Rule_commit.Ase_inner_inst.make ~proof_source:stmt.source
        ~proof_target:stmt.target ~proof stmt.source
        [ Field.of_string "849812849581123" ]

    let verify_both_ases_stmt, verify_both_ases_proof =
      Promise.block_on_async_exn @@ fun () -> prove_both (ase_outer, ase_inner)

    let _verify_both_ases =
      Rule_commit.Verify_both_ases.make_unchecked ~proof:verify_both_ases_proof
        verify_both_ases_stmt

    let old_inner_acc =
      { Mina_base.Account.empty with
        public_key = Outer_rules.Inputs.inner_public_key
      ; zkapp =
          Some
            { Mina_base.Zkapp_account.default with
              app_state =
                [ ase_with_length.source.action_state
                ; Unsigned.UInt32.to_string ase_with_length.source.length
                  |> Field.of_string
                ; Field.zero
                ; Field.zero
                ; Field.zero
                ; Field.zero
                ; Field.zero
                ; Field.zero
                ]
            ; action_state =
                (let f = ase_with_length.source.action_state in
                 [ f; f; f; f; f ] )
            }
      }

    let Compile_simple.
          [ _signed_command
          ; _zkapp_single
          ; zkapp_double
          ; _zkapp_proved
          ; _merge
          ] =
      Txn_rules.provers

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

    let protocol_constants : Genesis_constants.Protocol.t =
      { k = 1
      ; slots_per_epoch = 1000
      ; slots_per_sub_window = 1
      ; grace_period_slots = 1
      ; delta = 1
      ; genesis_state_timestamp = Int64.one
      }

    let consensus_constants =
      Consensus.Constants.create ~constraint_constants ~protocol_constants

    let _dummy_state_body =
      let compile_time_genesis =
        Mina_state.Genesis_protocol_state.t
          ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
          ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
          ~constraint_constants ~consensus_constants
          ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference
      in
      Mina_state.Protocol_state.body compile_time_genesis.data

    let () = printf "%i\n%!" constraint_constants.ledger_depth

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

    let implied_root (account : Mina_base.Account.t) : field =
      let init = Mina_base.Account.digest account in
      List.fold intermediate_ledger_hashes ~init
        ~f:(fun acc (height, right_side) ->
          Mina_base.Ledger_hash.merge ~height acc right_side )

    let source_ledger = implied_root old_inner_acc

    let inner_account_id =
      Mina_base.Account_id.create old_inner_acc.public_key
        old_inner_acc.token_id

    let sparse_source_ledger : Mina_ledger.Sparse_ledger.t =
      Mina_ledger.Sparse_ledger.(
        add_path
          (empty ~depth:constraint_constants.ledger_depth ())
          (List.map ~f:(fun (_, h) -> `Right h) intermediate_ledger_hashes)
          inner_account_id old_inner_acc)

    let kp = Keypair.gen |> Quickcheck.random_value

    let pk = kp.public_key |> Signature_lib.Public_key.compress

    let first_account_update : Mina_base.Account_update.t =
      { body =
          { Mina_base.Account_update.Body.dummy with
            public_key = pk
          ; token_id = Mina_base.Token_id.default
          ; authorization_kind = None_given
          ; balance_change =
              Currency.Amount.of_mina_string_exn "1000"
              |> Currency.Amount.Signed.of_unsigned
          }
      ; authorization = None_given
      }

    let second_account_update : Mina_base.Account_update.t =
      { body =
          { Mina_base.Account_update.Body.dummy with
            public_key = old_inner_acc.public_key
          ; token_id = old_inner_acc.token_id
          ; authorization_kind = None_given
          }
      ; authorization = None_given
      }

    type acc_set_entry = { key : field; next_key : field }

    let hash_entry { key; next_key } =
      Random_oracle.hash
        ~init:(Hash_prefix_create.salt "indexed merkle tree entry hash")
        [| key; next_key |]

    let acc_set_merge x y =
      Random_oracle.hash
        ~init:(Hash_prefix_create.salt "indexed merkle tree")
        [| x; y |]

    let acc_set_intermediate_ledger_hashes =
      let base = Field.zero in
      let rec go = function
        | 34, hash ->
            [ (34, hash) ]
        | height, hash ->
            (height, hash)
            :: go (height + 1, Mina_base.Ledger_hash.merge ~height hash hash)
      in
      go (0, base)

    let to_account_set x =
      let (Typ typ) = Account_set.typ in
      typ.value_of_fields ([| x |], typ.constraint_system_auxiliary ())

    let max = Field.negate Field.one

    let base_left = hash_entry { key = Field.zero; next_key = max }

    let base_right = hash_entry { key = max; next_key = max }

    let source_acc_set =
      let init = acc_set_merge base_left base_right in
      List.fold (List.drop acc_set_intermediate_ledger_hashes 1) ~init
        ~f:(fun acc (_, right_side) -> acc_set_merge acc right_side)
      |> to_account_set

    let () = assert (List.length acc_set_intermediate_ledger_hashes = 35)

    let account_set_least_path : Account_set.Path.t =
      { hash = base_right; is_left = false }
      :: ( List.drop acc_set_intermediate_ledger_hashes 1
         |> List.map ~f:(fun (_, hash) : Account_set.PathStep.t ->
                { hash; is_left = false } ) )

    let account_set_y_path : Account_set.Path.t =
      { hash = Field.zero; is_left = false }
      :: { hash = acc_set_merge base_left base_right; is_left = true }
      :: ( List.drop acc_set_intermediate_ledger_hashes 2
         |> List.map ~f:(fun (_, hash) : Account_set.PathStep.t ->
                { hash; is_left = false } ) )

    let list_to_func : 'a list -> unit -> 'a =
     fun xs ->
      let xs = ref xs in
      fun () ->
        match !xs with
        | x :: xs' ->
            xs := xs' ;
            x
        | _ ->
            failwith "empty"

    let new_account_account_id =
      Mina_base.Account_id.derive_token_id
        ~owner:(Mina_base.Account_id.create pk Mina_base.Token_id.default)

    let zkapp_double_witness : Rule_zkapp_command.Zkapp_double_unproved_input.t
        =
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
          ; source_acc_set
          ; witness =
              { stack_frame = Mina_base.Stack_frame.empty
              ; call_stack = []
              ; source_ledger_sparse = sparse_source_ledger
              ; update_acc_set_witness =
                  { get_account_set_x =
                      list_to_func
                        [ Mina_base.Token_id.of_field Field.zero
                        ; new_account_account_id
                        ]
                  ; get_account_set_z =
                      (fun () ->
                        Mina_base.Token_id.of_field (Field.negate Field.one) )
                  ; get_account_set_x_path =
                      list_to_func
                        [ account_set_least_path; account_set_y_path ]
                  ; get_account_set_y_path = (fun () -> account_set_y_path)
                  }
              }
          }
      ; first =
          { account_updates_data =
              Mina_base.Zkapp_command.Call_forest.of_account_updates
                ~account_update_depth:(fun _ -> 0)
                [ first_account_update; second_account_update ]
              |> Mina_base.Zkapp_command.Call_forest.accumulate_hashes'
          ; memo_hash = Field.zero
          ; account_updates =
              Mina_base.Zkapp_command.Call_forest.of_account_updates
                ~account_update_depth:(fun _ -> 0)
                [ first_account_update; second_account_update ]
              |> Mina_base.Zkapp_command.Call_forest.accumulate_hashes'
              |> Mina_base.Zkapp_command.Call_forest.hash
          ; shift_action_state = false
          }
      ; second =
          { account_updates_data =
              Mina_base.Zkapp_command.Call_forest.accumulate_hashes' []
          ; memo_hash = Field.zero
          ; account_updates =
              Mina_base.Zkapp_command.Call_forest.accumulate_hashes' []
              |> Mina_base.Zkapp_command.Call_forest.hash
          ; shift_action_state = false
          }
      }

    let _stmt, _proof =
      Promise.block_on_async_exn @@ fun () -> zkapp_double zkapp_double_witness

    (*
    let inner_acc_path =
      List.map
        ~f:(fun (_, hash) : Outer_rules.Rule_commit_inst.PathElt.t ->
          { right_side = hash } )
        intermediate_ledger_hashes

    let commit_witness : Outer_rules.Rule_commit_inst.Witness.t =
      { txn_snark
      ; public_key = point_of_string "28811121"
      ; vk_hash = Field.of_string "31923919199191"
      ; verify_both_ases
      ; old_inner_acc
      ; old_inner_acc_path = inner_acc_path
      ; new_inner_acc
      ; new_inner_acc_path = inner_acc_path
      ; da_signature
      ; da_key = { public_key = da_key.x }
      }

    let _stmt, _proof =
      Promise.block_on_async_exn @@ fun () -> commit commit_witness
    *)
  end in
  ()
