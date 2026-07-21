open Core
module Field = Snark_params.Tick.Field
module PC = Signature_lib.Public_key.Compressed

let field_to_hex field =
  Kimchi_backend.Pasta.Basic.Bigint256.to_hex_string
    (Kimchi_backend.Pasta.Basic.Fp.to_bigint field)
  |> String.lowercase
  |> String.chop_prefix_if_exists ~prefix:"0x"
  |> fun hex -> String.make (64 - String.length hex) '0' ^ hex

let amount value = Unsigned.UInt64.of_string value |> Currency.Amount.of_uint64

let aux ~amount:value ~recipient_x ~recipient_is_odd =
  let params : Zeko_circuits.Bridge_state.Deposit_params_base.t =
    { children = []
    ; holder_account_l1 = ({ x = Field.one; is_odd = false } : PC.t)
    ; amount = amount value
    ; recipient =
        ({ x = Field.of_string recipient_x; is_odd = recipient_is_odd } : PC.t)
    ; timeout = Mina_numbers.Global_slot_since_genesis.max_value
    }
  in
  Utils.value_to_hash ~init:Zeko_constants.ethereum_deposit_salt
    Zeko_circuits.Bridge_state.Deposit_params_base.typ params
  |> field_to_hex

let erc20_aux ~asset_id_high ~asset_id_low ~amount:value ~recipient_x
    ~recipient_is_odd =
  let base : Zeko_circuits.Bridge_state.Deposit_params_base.t =
    { children = []
    ; holder_account_l1 = ({ x = Field.one; is_odd = false } : PC.t)
    ; amount = amount value
    ; recipient =
        ({ x = Field.of_string recipient_x; is_odd = recipient_is_odd } : PC.t)
    ; timeout = Mina_numbers.Global_slot_since_genesis.max_value
    }
  in
  let params : Zeko_circuits.Bridge_state.Deposit_params_ethereum_token.t =
    { asset_id_high = Field.of_string asset_id_high
    ; asset_id_low = Field.of_string asset_id_low
    ; base
    }
  in
  Utils.value_to_hash ~init:Zeko_constants.ethereum_erc20_deposit_salt
    Zeko_circuits.Bridge_state.Deposit_params_ethereum_token.typ params
  |> field_to_hex

let point_of_string value =
  Snark_params.Tick.Inner_curve.(
    to_affine_exn @@ point_near_x @@ Field.of_string value)
  |> Signature_lib.Public_key.compress

let erc20_deposit_params () =
  let base : Zeko_circuits.Bridge_state.Deposit_params_base.t =
    { children = []
    ; holder_account_l1 = ({ x = Field.one; is_odd = false } : PC.t)
    ; amount = amount "2000000"
    ; recipient = ({ x = Field.of_int 0x01020304; is_odd = false } : PC.t)
    ; timeout = Mina_numbers.Global_slot_since_genesis.max_value
    }
  in
  ( { asset_id_high = Field.one; asset_id_low = Field.of_int 2; base }
    : Zeko_circuits.Bridge_state.Deposit_params_ethereum_token.t )

let run_erc20_deposit_action ~expected_asset_low =
  let open Snark_params.Tick in
  let params = erc20_deposit_params () in
  run_and_check_exn
    (let%bind.Checked params =
       exists Zeko_circuits.Bridge_state.Deposit_params_ethereum_token.typ
         ~compute:(fun _ -> params)
     in
     let%map.Checked action =
       make_checked (fun () ->
           Run.run_checked
             (Zeko_circuits.Bridge_state.deposit_action
                ~chain_l1:Mina_signature_kind.Testnet ~holder_accounts_l1:[]
                ~token_owner_l1:None
                ~ethereum_holder_account_l1:
                  (Some ({ x = Field.one; is_odd = false } : PC.t))
                ~ethereum_asset_id:(Some (Field.one, expected_asset_low))
                (module Zeko_circuits.Bridge_state.Deposit_params_ethereum_token)
                params
                ~bridge_fee_recipient_l1:
                  (constant PC.typ (point_of_string "765431"))
                ~bridge_proof_fee:
                  (constant Currency.Amount.typ Currency.Amount.zero) ) )
     in
     As_prover.read Zeko_circuits.Rollup_state.Outer_action.Witness.typ action
    )

let check_erc20_deposit_witness_action () =
  let witness = run_erc20_deposit_action ~expected_asset_low:(Field.of_int 2) in
  if
    not
      (String.equal (field_to_hex witness.aux)
         "0fc821581944d768e902d37d6527e3011992e5368fb33dca2f5ccc24a6f417f7" )
  then failwith "Ethereum ERC20 deposit circuit/SP1 aux mismatch" ;
  if not (List.is_empty witness.children) then
    failwith "Ethereum ERC20 witness unexpectedly contains Mina L1 calls" ;
  if
    not
      (Exn.does_raise (fun () ->
           let (_ : Zeko_circuits.Rollup_state.Outer_action.Witness.t) =
             run_erc20_deposit_action ~expected_asset_low:(Field.of_int 3)
           in
           () ) )
  then failwith "Ethereum ERC20 deposit circuit accepted the wrong asset"

module Ethereum_token_bridge =
  Zeko_circuits.Bridge_rules.Make_ethereum_token
    (struct
      let token_owner_l2 =
        Mina_base.Account_id.create (point_of_string "344213")
          Mina_base.Token_id.default

      let ethereum_holder_account_l1 : PC.t = { x = Field.one; is_odd = false }

      let ethereum_asset_id_high = Field.one

      let ethereum_asset_id_low = Field.of_int 2

      let zeko_l2 = point_of_string "39921"

      let holder_account_l2 = point_of_string "11111"

      let bridge_fee_recipient_l1 = point_of_string "765431"

      let bridge_fee_recipient_l2 = point_of_string "765432"

      let chain_l1 = Mina_signature_kind.Testnet

      let chain_l2 = Mina_signature_kind.Testnet

      let multisig_key : Zeko_circuits.Multisig.t =
        { public_keys = [ point_of_string "89888" ]; quorum = Field.one }
    end)
    ()

let check_erc20_finalize_deposit () =
  let open Mina_base in
  let open Zeko_circuits in
  let open Zeko_util in
  let params = erc20_deposit_params () in
  let base = params.base in
  let witness : Rollup_state.Outer_action.Witness.t =
    { aux =
        Utils.value_to_hash ~init:Zeko_constants.ethereum_erc20_deposit_salt
          Bridge_state.Deposit_params_ethereum_token.typ params
    ; children = []
    ; slot_range = Slot_range.infinite
    }
  in
  let original_action_state = Zkapp_account.Actions.empty_state_element in
  let deposit_action_hash =
    Zkapp_account.Actions_impl.hash (Utils.witness_to_actions witness)
  in
  let mid_outer_action_state =
    Rollup_state.Outer_action_state.With_length.unsafe_value_of_fields
      ~state:
        ( Zkapp_account.Actions_impl.push_hash original_action_state
            deposit_action_hash
        |> Rollup_state.Outer_action_state.unsafe_value_of_field )
      ~length:Checked32.one
  in
  let commit_witness : Rollup_state.Outer_action.Commit.t =
    { ledger = Field.zero
    ; inner_action_state = Rollup_state.Inner_action_state.With_length.empty
    ; synchronized_outer_action_state = mid_outer_action_state
    ; slot_range = { lower = Slot.zero; upper = Slot.of_int 30 }
    }
  in
  let commit_action_hash =
    Zkapp_account.Actions_impl.hash (Utils.commit_to_actions commit_witness)
  in
  let target_outer_action_state =
    Rollup_state.Outer_action_state.With_length.unsafe_value_of_fields
      ~state:
        ( Zkapp_account.Actions_impl.push_hash
            Rollup_state.Outer_action_state.(
              With_length.state mid_outer_action_state |> raw)
            commit_action_hash
        |> Rollup_state.Outer_action_state.unsafe_value_of_field )
      ~length:(Checked32.of_int 2)
  in
  let check_accepted =
    let Ethereum_token_bridge.Check_accepted.{ source; target }, proof =
      Promise.block_on_async_exn
      @@ fun () ->
      (Lazy.force Ethereum_token_bridge.Check_accepted.leaf_option)
        ( [ Commit commit_witness ]
        , { Ethereum_token_bridge.Check_accepted.Definition.Stmt.params
          ; action_state =
              Rollup_state.Outer_action_state.With_length.state
                mid_outer_action_state
          ; deposit_index = Checked32.zero
          ; n_steps = Checked32.zero
          ; is_rejected = false
          ; is_accepted = false
          } )
    in
    if (not target.is_accepted) || target.is_rejected then
      failwith "Ethereum ERC20 deposit was not accepted" ;
    Ethereum_token_bridge.Rule_bridge_finalize_deposit.Check_accepted_inst.make
      ~proof_source:source ~proof_target:target ~proof
      { params
      ; original_action_state =
          Rollup_state.Outer_action_state.unsafe_value_of_field
            original_action_state
      ; deposit_index = Checked32.zero
      }
      []
  in
  let Compile_simple.[ finalize_deposit; _; _ ] =
    Lazy.force Ethereum_token_bridge.System_L2.provers
  in
  let vk_hash =
    ( Promise.block_on_async_exn
    @@ fun () ->
    Compile_simple.Verification_key.of_tag
      (Lazy.force Ethereum_token_bridge.System_L2.tag) )
    |> Compile_simple.Verification_key.hash
  in
  let action_state : Ase.With_length.Stmt.t =
    { action_state =
        Rollup_state.Outer_action_state.(
          With_length.state target_outer_action_state |> raw)
    ; length =
        Rollup_state.Outer_action_state.With_length.length
          target_outer_action_state
    }
  in
  let (_statement, (vault, _digest, calls)), _proof =
    Promise.block_on_async_exn
    @@ fun () ->
    finalize_deposit
      { public_key = point_of_string "11111"
      ; vk_hash
      ; may_use_token = Parents_own_token
      ; inner_authorization_kind = Rule_bridge_finalize_deposit.A.None_given
      ; ase =
          Ethereum_token_bridge.Rule_bridge_finalize_deposit.Ase_inst.make
            ~proof_source:action_state ~proof_target:action_state action_state
            []
      ; check_accepted
      ; prev_next_deposit = Checked32.zero
      ; prev_nonce = Checked32.zero
      ; helper_account_new = true
      }
  in
  let token_owner =
    Account_id.create (point_of_string "344213") Token_id.default
  in
  let token_id = Account_id.derive_token_id ~owner:token_owner in
  if not (Token_id.equal vault.token_id token_id) then
    failwith "Ethereum ERC20 deposit debited the wrong vault token" ;
  if
    not
      (Currency.Amount.Signed.equal vault.balance_change
         Currency.Amount.Signed.(of_unsigned base.amount |> negate) )
  then failwith "Ethereum ERC20 deposit vault debit mismatch" ;
  if vault.implicit_account_creation_fee then
    failwith "Ethereum ERC20 vault charged an implicit MINA fee" ;
  match Zkapp_command.Call_forest.to_account_updates calls with
  | [ _helper; _inner_witness; recipient; zero_fee ] ->
      if not (Token_id.equal recipient.body.token_id token_id) then
        failwith "Ethereum ERC20 deposit credited the wrong token" ;
      if
        not
          (Account_update.May_use_token.equal recipient.body.may_use_token
             Inherit_from_parent )
      then failwith "Ethereum ERC20 recipient does not inherit the vault token" ;
      if recipient.body.implicit_account_creation_fee then
        failwith "Ethereum ERC20 recipient charged an implicit MINA fee" ;
      if
        not
          (Currency.Amount.Signed.equal recipient.body.balance_change
             Currency.Amount.Signed.(of_unsigned base.amount) )
      then failwith "Ethereum ERC20 recipient credit mismatch" ;
      if
        not
          (Currency.Amount.Signed.equal zero_fee.body.balance_change
             Currency.Amount.Signed.zero )
      then failwith "Ethereum ERC20 circuit emitted a token-denominated fee"
  | _ ->
      failwith "Ethereum ERC20 finalize-deposit forest shape mismatch"

let erc20_withdrawal_params_fields () =
  let open Mina_base in
  let chain = Mina_signature_kind.Testnet in
  let token_owner =
    Account_id.create (point_of_string "344213") Token_id.default
  in
  let token_id = Account_id.derive_token_id ~owner:token_owner in
  let sender = point_of_string "98881" in
  let withdrawal_amount = amount "2000000" in
  let debit_body =
    { Account_update.Body.dummy with
      public_key = sender
    ; token_id
    ; balance_change =
        Currency.Amount.Signed.(of_unsigned withdrawal_amount |> negate)
    ; may_use_token = Parents_own_token
    ; authorization_kind = Signature
    ; use_full_commitment = true
    }
  in
  let debit =
    Account_update.with_aux ~body:debit_body
      ~authorization:Control.Poly.None_given
  in
  let debit_forest =
    Zkapp_command.Call_forest.cons ~signature_kind:chain debit []
  in
  let base : Zeko_circuits.Bridge_state.Withdrawal_params_base.t =
    { children = []
    ; amount = withdrawal_amount
    ; recipient = ({ x = Field.of_int 0x01020304; is_odd = false } : PC.t)
    }
  in
  let custom : Zeko_circuits.Bridge_state.Withdrawal_params_custom.t =
    { token_owner_body =
        { Account_update.Body.dummy with
          public_key = Account_id.public_key token_owner
        ; token_id = Account_id.token_id token_owner
        ; authorization_kind = Proof Field.one
        }
    ; nested_children = debit_forest
    ; base
    }
  in
  let params : Zeko_circuits.Bridge_state.Withdrawal_params_ethereum_token.t =
    { asset_id_high = Field.one; asset_id_low = Field.of_int 2; custom }
  in
  let configured_token_owner =
    Zeko_circuits_config.Inputs.Ethereum_token.token_owner_l2
  in
  let configured_debit =
    Account_update.with_aux
      ~body:
        { debit_body with
          token_id = Account_id.derive_token_id ~owner:configured_token_owner
        }
      ~authorization:Control.Poly.None_given
  in
  let precompute_params :
      Zeko_circuits.Bridge_state.Withdrawal_params_ethereum_token.t =
    { Zeko_circuits.Bridge_state.Withdrawal_params_ethereum_token.asset_id_high =
        Zeko_circuits_config.Inputs.Ethereum_token.ethereum_asset_id_high
    ; asset_id_low =
        Zeko_circuits_config.Inputs.Ethereum_token.ethereum_asset_id_low
    ; custom =
        { custom with
          token_owner_body =
            { custom.token_owner_body with
              public_key = Account_id.public_key configured_token_owner
            ; token_id = Account_id.token_id configured_token_owner
            }
        ; nested_children =
            Zkapp_command.Call_forest.cons
              ~signature_kind:Zeko_circuits_config.Inputs.chain_l2
              configured_debit []
        }
    }
  in
  let (_ : Sequencer_lib.Bridge_prover.precomputed_forest) =
    Sequencer_lib.Bridge_prover.Ethereum_token_withdrawal_request
    .precompute_forest_with_vk_hashes
      ~bridge_ethereum_token_l2_vk_hash:Field.one
      ~inner_rules_vk_hash:(Field.of_int 2) precompute_params
    |> Or_error.ok_exn
  in
  let params_fields =
    Utils.value_to_fields
      Zeko_circuits.Bridge_state.Withdrawal_params_ethereum_token.typ params
    |> Array.to_list
  in
  ( match Zkapp_command.Call_forest.to_account_updates debit_forest with
  | [ debit ] ->
      if
        not
          (Currency.Amount.Signed.equal debit.body.balance_change
             Currency.Amount.Signed.(of_unsigned withdrawal_amount |> negate) )
      then failwith "Ethereum ERC20 withdrawal does not start with the debit"
  | _ ->
      failwith "Ethereum ERC20 withdrawal debit forest is not singular" ) ;
  let withdrawal_aux =
    Utils.value_to_hash ~init:Zeko_constants.ethereum_erc20_withdrawal_salt
      Zeko_circuits.Bridge_state.Withdrawal_params_ethereum_token.typ params
    |> field_to_hex
  in
  (params_fields, withdrawal_aux)

let () =
  let Compile_simple.[ _; _; _ ] =
    Lazy.force Ethereum_token_bridge.System_L2.provers
  in
  check_erc20_deposit_witness_action () ;
  check_erc20_finalize_deposit () ;
  let withdrawal_params_fields, withdrawal_aux =
    erc20_withdrawal_params_fields ()
  in
  if
    not
      (String.equal withdrawal_aux
         "350ed8b22bbaac628364ea5d8ee44a8f12d7814dc9c1432264ece93ccd5b364d" )
  then
    failwithf "Ethereum ERC20 withdrawal circuit/settlement aux mismatch: %s"
      withdrawal_aux () ;
  let token_preimage : Sequencer_lib.Archive.Ethereum_withdrawal.t =
    { recipient = { x = Field.of_int 0x01020304; is_odd = false }
    ; amount = amount "2000000"
    ; asset =
        Some
          { token = "0x0000000000000000000000000000000000000001"
          ; asset_id =
              "0x0000000000000000000000000000000100000000000000000000000000000002"
          ; params_fields = withdrawal_params_fields
          }
    }
  in
  ( match
      Sequencer_lib.Ethereum_settlement_export.ethereum_withdrawal_preimage_json
        token_preimage
    with
  | Some
      ( "tokenWithdrawal"
      , `Assoc
          [ ("token", `String "0x0000000000000000000000000000000000000001")
          ; ( "assetId"
            , `String
                "0x0000000000000000000000000000000100000000000000000000000000000002"
            )
          ; ("recipient", `String "0x0000000000000000000000000000000001020304")
          ; ("amount", `Intlit "2000000")
          ; ("paramsFields", `List params_fields)
          ] )
    when List.length params_fields >= 6 ->
      ()
  | _ ->
      failwith "Ethereum ERC20 withdrawal export mismatch" ) ;
  let vectors =
    [ ( "1000000000"
      , "16909060"
      , false
      , "2e9d1b29cea8eaba8c1dfe6d8c78b21127ce44a8378b3c9d2ee9ba0ddbd7c849" )
    ; ( "2000000000"
      , "84281096"
      , false
      , "1a03b5b4a38e241ee071764a843e5b7bf29aa0e455d7ccd53a83f729885bfb18" )
    ; ( "3000000000"
      , "151653132"
      , true
      , "1adc48d4e3b4478369ec2d8ce4ca72c397c9e75f019b24c9d65c262ae9757fa9" )
    ]
  in
  List.iter vectors ~f:(fun (amount, recipient_x, recipient_is_odd, expected) ->
      let actual = aux ~amount ~recipient_x ~recipient_is_odd in
      if not (String.equal actual expected) then
        failwithf "Ethereum deposit aux mismatch: expected %s, got %s" expected
          actual () ) ;
  let actual =
    erc20_aux ~asset_id_high:"1" ~asset_id_low:"2" ~amount:"2000000"
      ~recipient_x:"16909060" ~recipient_is_odd:false
  in
  let expected =
    "0fc821581944d768e902d37d6527e3011992e5368fb33dca2f5ccc24a6f417f7"
  in
  if not (String.equal actual expected) then
    failwithf "Ethereum ERC20 deposit aux mismatch: expected %s, got %s"
      expected actual ()
