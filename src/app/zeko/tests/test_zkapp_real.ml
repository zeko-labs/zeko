open Core_kernel
open Signature_lib
open Snark_params.Tick
open Zeko_circuits

let signature_kind = Mina_signature_kind.Testnet

let point_of_string_even s : Zeko_util.Even_PC.t =
  let x, _ =
    Snark_params.Tick.Inner_curve.(
      to_affine_exn @@ point_near_x @@ Snark_params.Tick.Field.of_string s)
  in
  { public_key = x }

let point_of_string s =
  Snark_params.Tick.Inner_curve.(
    to_affine_exn @@ point_near_x @@ Snark_params.Tick.Field.of_string s)

let inner_public_key = point_of_string "123456789" |> Public_key.compress

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
        }
  }

let Compile_simple.
      [ _signed_command; _zkapp_single; zkapp_double; _zkapp_proved; _merge ] =
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

let implied_root (account : Mina_base.Account.t) path : field =
  let init = Mina_base.Account.digest account in
  List.foldi path ~init ~f:(fun height acc -> function
    | `Right left ->
        let acc' = Mina_base.Ledger_hash.merge ~height left acc in
        acc'
    | `Left right ->
        let acc' = Mina_base.Ledger_hash.merge ~height acc right in
        acc' )

let fee_payer_kp, new_kp =
  Base_quickcheck.Generator.both Keypair.gen Keypair.gen
  |> Quickcheck.random_value

let fee_payer_acc =
  { Mina_base.Account.empty with
    public_key = Public_key.compress fee_payer_kp.public_key
  ; balance = Currency.Balance.of_mina_string_exn "100000"
  }

let path_inner =
  `Left (Mina_base.Account.digest fee_payer_acc)
  :: ( List.map ~f:(fun (_, h) -> `Left h)
     @@ List.drop intermediate_ledger_hashes 1 )

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

let () = printf "New  key: %s\n" (Public_key.Compressed.to_base58_check new_pk)

let () =
  printf "Inner key:                %s\n"
    (Public_key.Compressed.to_base58_check old_inner_acc.public_key)

let () =
  printf "Empty key:                %s\n"
    (Public_key.Compressed.to_base58_check Mina_base.Account.empty.public_key)

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
                { lower = Unsigned.UInt32.zero; upper = Unsigned.UInt32.zero }
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
      [ second_account_update; third_account_update; fourth_account_update ]
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
      (Mina_base.Zkapp_command.Digest.Account_update.create_body ~signature_kind
         first_account_update )

let signature =
  Signature_lib.Schnorr.Chunked.sign ~signature_kind fee_payer_kp.private_key
    (Random_oracle.Input.Chunked.field full_transaction_commitment)

let () =
  printf "public key out circuit: %s\n"
    (fee_payer_acc.public_key |> Public_key.Compressed.to_base58_check) ;
  printf "commitment out circuit: %s\n"
    (full_transaction_commitment |> Field.to_string)

let to_account_set x =
  let (Typ typ) = Account_set.typ in
  typ.value_of_fields ([| x |], typ.constraint_system_auxiliary ())

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

let _acc_set, acc_set_data_1 =
  S.maybe_add (derive second_account_update.public_key) acc_set

let convert_path =
  let f = function
    | `Left right ->
        ({ hash_other = right; is_right = false } : Account_set.PathStep.t)
    | `Right left ->
        { hash_other = left; is_right = true }
  in
  List.map ~f

let list_to_fun l =
  let l = ref l in
  fun () ->
    match !l with
    | [] ->
        failwith "empty!"
    | x :: xs ->
        l := xs ;
        x

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
  Mina_ledger.Sparse_ledger.of_root ~depth:constraint_constants.ledger_depth
    source_ledger
  |> fun x ->
  Mina_ledger.Sparse_ledger.add_path x path_inner (id_of old_inner_acc)
    old_inner_acc
  |> fun x ->
  Mina_ledger.Sparse_ledger.add_path x path_fee_payer (id_of fee_payer_acc)
    fee_payer_acc
  |> fun x ->
  Mina_ledger.Sparse_ledger.add_path x path_new account_id_new
    Mina_base.Account.empty

let zkapp_double_witness : Rule_zkapp_command.Zkapp_double_unproved_input.t =
  { base =
      { source_ledger
      ; source_local_state =
          { stack_frame_digest =
              Mina_base.Stack_frame.Digest.create Mina_base.Stack_frame.empty
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
         |> Mina_base.Zkapp_command.Call_forest.accumulate_hashes_predicated
              ~signature_kind
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

let _stmt0, _target_ledger, _proof0 =
  Promise.block_on_async_exn @@ fun () -> zkapp_double zkapp_double_witness
