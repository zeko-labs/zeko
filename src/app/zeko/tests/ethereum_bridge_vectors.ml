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
  let params : Zeko_circuits.Bridge_state.Deposit_params_ethereum_token_v1.t =
    { asset_id_high = Field.of_string asset_id_high
    ; asset_id_low = Field.of_string asset_id_low
    ; base
    }
  in
  Utils.value_to_hash ~init:Zeko_constants.ethereum_erc20_deposit_salt
    Zeko_circuits.Bridge_state.Deposit_params_ethereum_token_v1.typ params
  |> field_to_hex

let point_of_string value =
  Snark_params.Tick.Inner_curve.(
    to_affine_exn @@ point_near_x @@ Field.of_string value)
  |> Signature_lib.Public_key.compress

let commit_registry_public_key = point_of_string "61001"

let commit_registration_authority = point_of_string "62001"

let commit_vault_public_key = point_of_string "63001"

let commit_owner_public_key = point_of_string "64001"

let commit_admin_public_key = point_of_string "65001"

let commit_circulation_public_key = commit_owner_public_key

let commit_vk = Mina_base.Side_loaded_verification_key.dummy

let commit_vk_hash = Mina_base.Verification_key_wire.digest_vk commit_vk

module Commit_rule = Zeko_circuits.Rule_commit.Make (struct
  let max_valid_while_size = 1

  let inner_public_key = point_of_string "66001"

  let chain_l1 = Mina_signature_kind.Testnet

  let chain_l2 = Mina_signature_kind.Testnet

  let max_sequencer_inactivity = 1

  let emergency_da_public_key = point_of_string "67001"

  let ethereum_asset_registry_public_key = Some commit_registry_public_key

  let ethereum_asset_registry_schema_version =
    Zeko_circuits.Zeko_util.Checked32.of_int
      Zeko_constants.Ethereum_asset_registry.schema_version

  let ethereum_asset_approved_mft_standard_vk_id = Field.of_int 61008

  let ethereum_asset_approved_mft_token_vk_hash = commit_vk_hash

  let ethereum_asset_approved_mft_admin_vk_hash = commit_vk_hash

  let ethereum_asset_universal_bridge_vk_id = Field.of_int 61009

  let ethereum_asset_universal_bridge_vk_hash = commit_vk_hash

  let ethereum_asset_vault_public_key = commit_vault_public_key
end)

type registration_account_mutation =
  | Valid_registration
  | Wrong_owner_id
  | Wrong_owner_vk
  | Wrong_owner_permissions
  | Wrong_admin_id
  | Wrong_admin_vk
  | Wrong_admin_permissions
  | Wrong_admin_authority
  | Wrong_vault_id
  | Wrong_vault_vk
  | Wrong_vault_permissions
  | Wrong_vault_balance
  | Wrong_circulation_id
  | Wrong_circulation_permissions
  | Wrong_circulation_balance
  | Wrong_inventory_cap

let erc20_deposit_params_v1 ?(asset_id_low = Field.of_int 2) () =
  let base : Zeko_circuits.Bridge_state.Deposit_params_base.t =
    { children = []
    ; holder_account_l1 = ({ x = Field.one; is_odd = false } : PC.t)
    ; amount = amount "2000000"
    ; recipient = ({ x = Field.of_int 0x01020304; is_odd = false } : PC.t)
    ; timeout = Mina_numbers.Global_slot_since_genesis.max_value
    }
  in
  ( { asset_id_high = Field.one; asset_id_low; base }
    : Zeko_circuits.Bridge_state.Deposit_params_ethereum_token_v1.t )

let run_erc20_deposit_action ~expected_asset_low =
  let open Snark_params.Tick in
  let params = erc20_deposit_params_v1 () in
  run_and_check_exn
    (let%bind.Checked params =
       exists Zeko_circuits.Bridge_state.Deposit_params_ethereum_token_v1.typ
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
                ( module Zeko_circuits.Bridge_state
                         .Deposit_params_ethereum_token_v1 )
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
  Zeko_circuits.Bridge_rules.Make_ethereum_assets
    (struct
      let registry_public_key = point_of_string "22222"

      let registration_authority = point_of_string "33333"

      let registry_schema_version =
        Zeko_circuits.Zeko_util.Checked32.of_int
          Zeko_constants.Ethereum_asset_registry.schema_version

      let approved_mft_standard_vk_id = Field.of_int 777

      let universal_bridge_vk_id = Field.of_int 778

      let vault_public_key = point_of_string "11111"

      let ethereum_holder_account_l1 : PC.t = { x = Field.one; is_odd = false }

      let zeko_l2 = point_of_string "39921"

      let bridge_fee_recipient_l1 = point_of_string "765431"

      let bridge_fee_recipient_l2 = point_of_string "765432"

      let chain_l1 = Mina_signature_kind.Testnet

      let chain_l2 = Mina_signature_kind.Testnet

      let multisig_key : Zeko_circuits.Multisig.t =
        { public_keys = [ point_of_string "89888" ]; quorum = Field.one }
    end)
    ()

let registered_asset index token_owner_l2 asset_id_low =
  let open Mina_base in
  let owner = Account_id.create token_owner_l2 Token_id.default in
  ( { schema_version =
        Zeko_circuits.Zeko_util.Checked32.of_int
          Zeko_constants.Ethereum_asset_registry.schema_version
    ; registry_index = Zeko_circuits.Zeko_util.Checked32.of_int index
    ; asset_id_high = Field.one
    ; asset_id_low
    ; ethereum_token_address = Field.of_int (5000 + index)
    ; token_owner_l2
    ; token_id_l2 = Account_id.derive_token_id ~owner
    ; decimals = Zeko_circuits.Zeko_util.Checked32.of_int 6
    ; inventory_cap = amount "100000000"
    ; mft_standard_vk_id = Field.of_int 777
    ; vault_public_key = point_of_string "11111"
    ; universal_bridge_vk_id = Field.of_int 778
    }
    : Zeko_circuits.Asset_registry.Asset_record.t )

let first_registered_asset =
  registered_asset 0 (point_of_string "344213") (Field.of_int 2)

let first_registry_tree =
  Zeko_circuits.Asset_registry.Merkle_list.empty ()
  |> fun tree ->
  Zeko_circuits.Asset_registry.Merkle_list.append_exn tree
    first_registered_asset

let first_membership : Ethereum_token_bridge.Registry.Membership_witness.t =
  { state =
      { root = Zeko_circuits.Asset_registry.Merkle_list.root first_registry_tree
      ; leaf_count = Zeko_circuits.Zeko_util.Checked32.one
      ; schema_version =
          Zeko_circuits.Zeko_util.Checked32.of_int
            Zeko_constants.Ethereum_asset_registry.schema_version
      }
  ; record = first_registered_asset
  ; path =
      Zeko_circuits.Asset_registry.Merkle_list.path first_registry_tree ~index:0
  }

let second_registered_asset =
  registered_asset 1 (point_of_string "344214") (Field.of_int 3)

let second_registry_tree =
  Zeko_circuits.Asset_registry.Merkle_list.append_exn first_registry_tree
    second_registered_asset

let second_membership : Ethereum_token_bridge.Registry.Membership_witness.t =
  { state =
      { root =
          Zeko_circuits.Asset_registry.Merkle_list.root second_registry_tree
      ; leaf_count = Zeko_circuits.Zeko_util.Checked32.of_int 2
      ; schema_version =
          Zeko_circuits.Zeko_util.Checked32.of_int
            Zeko_constants.Ethereum_asset_registry.schema_version
      }
  ; record = second_registered_asset
  ; path =
      Zeko_circuits.Asset_registry.Merkle_list.path second_registry_tree
        ~index:1
  }

type commit_registration_fixture =
  { source_ledger : Mina_base.Ledger_hash.t
  ; target_ledger : Mina_base.Ledger_hash.t
  ; old_registry_acc : Mina_base.Account.t
  ; old_registry_path : Commit_rule.Registry_path.t
  ; new_registry_acc : Mina_base.Account.t
  ; new_registry_path : Commit_rule.Registry_path.t
  ; registration : Commit_rule.Registration_witness.t
  }

let registry_state tree : Zeko_circuits.Asset_registry.Registry_state.t =
  { root = Zeko_circuits.Asset_registry.Merkle_list.root tree
  ; leaf_count =
      Zeko_circuits.Zeko_util.Checked32.of_int
        (Zeko_circuits.Asset_registry.Merkle_list.count tree)
  ; schema_version =
      Zeko_circuits.Zeko_util.Checked32.of_int
        Zeko_constants.Ethereum_asset_registry.schema_version
  }

let commit_candidate inventory_cap : Zeko_circuits.Asset_registry.Asset_record.t
    =
  let owner =
    Mina_base.Account_id.create commit_owner_public_key
      Mina_base.Token_id.default
  in
  { schema_version =
      Zeko_circuits.Zeko_util.Checked32.of_int
        Zeko_constants.Ethereum_asset_registry.schema_version
  ; registry_index = Zeko_circuits.Zeko_util.Checked32.zero
  ; asset_id_high = Field.of_int 61010
  ; asset_id_low = Field.of_int 61011
  ; ethereum_token_address = Field.of_int 61012
  ; token_owner_l2 = commit_owner_public_key
  ; token_id_l2 = Mina_base.Account_id.derive_token_id ~owner
  ; decimals = Zeko_circuits.Zeko_util.Checked32.of_int 6
  ; inventory_cap
  ; mft_standard_vk_id = Field.of_int 61008
  ; vault_public_key = commit_vault_public_key
  ; universal_bridge_vk_id = Field.of_int 61009
  }

let zkapp_with_vk ~app_state ~vk_hash : Mina_base.Zkapp_account.t =
  { Mina_base.Zkapp_account.default with
    app_state = Mina_base.Zkapp_state.V.of_list_exn app_state
  ; verification_key = Some { data = commit_vk; hash = vk_hash }
  }

let registry_account (state : Zeko_circuits.Asset_registry.Registry_state.t) =
  let open Mina_base in
  { Account.empty with
    public_key = commit_registry_public_key
  ; token_id = Token_id.default
  ; zkapp =
      Some
        { Zkapp_account.default with
          app_state =
            Zkapp_state.V.of_list_exn
              [ state.root
              ; Zeko_circuits.Zeko_util.Checked32.to_field state.leaf_count
              ; Zeko_circuits.Zeko_util.Checked32.to_field state.schema_version
              ; Field.zero
              ; Field.zero
              ; Field.zero
              ; Field.zero
              ; Field.zero
              ]
        }
  }

let balance_of_amount value =
  Currency.Amount.to_uint64 value |> Currency.Balance.of_uint64

let make_commit_accounts mutation
    (candidate : Zeko_circuits.Asset_registry.Asset_record.t) =
  let open Mina_base in
  let base_inventory = amount "100000000" in
  let token_id =
    candidate.Zeko_circuits.Asset_registry.Asset_record.token_id_l2
  in
  let owner_zkapp =
    zkapp_with_vk ~vk_hash:commit_vk_hash
      ~app_state:
        [ Zeko_circuits.Zeko_util.Checked32.to_field candidate.decimals
        ; commit_admin_public_key.x
        ; (if commit_admin_public_key.is_odd then Field.one else Field.zero)
        ; Field.zero
        ; Field.zero
        ; Field.zero
        ; Field.zero
        ; Field.zero
        ]
  in
  let admin_zkapp =
    let revoked_authority = PC.empty in
    zkapp_with_vk ~vk_hash:commit_vk_hash
      ~app_state:
        [ revoked_authority.x
        ; (if revoked_authority.is_odd then Field.one else Field.zero)
        ; Field.zero
        ; Field.zero
        ; Field.zero
        ; Field.zero
        ; Field.zero
        ; Field.zero
        ]
  in
  let vault_zkapp =
    zkapp_with_vk ~vk_hash:commit_vk_hash
      ~app_state:(List.init 8 ~f:(fun _ -> Field.zero))
  in
  let owner =
    { Account.empty with
      public_key = commit_owner_public_key
    ; token_id = Token_id.default
    ; permissions = Commit_rule.expected_token_owner_permissions
    ; zkapp = Some owner_zkapp
    }
  in
  let admin =
    { Account.empty with
      public_key = commit_admin_public_key
    ; token_id = Token_id.default
    ; permissions = Commit_rule.expected_token_admin_permissions
    ; zkapp = Some admin_zkapp
    }
  in
  let vault =
    { Account.empty with
      public_key = commit_vault_public_key
    ; token_id
    ; balance = balance_of_amount base_inventory
    ; permissions = Commit_rule.expected_vault_permissions
    ; zkapp = Some vault_zkapp
    }
  in
  let circulation =
    { Account.empty with
      public_key = commit_circulation_public_key
    ; token_id
    ; balance = balance_of_amount base_inventory
    ; permissions = Commit_rule.expected_circulation_permissions
    }
  in
  match mutation with
  | Valid_registration | Wrong_inventory_cap ->
      (owner, admin, vault, circulation)
  | Wrong_owner_id ->
      ( { owner with public_key = point_of_string "61101" }
      , admin
      , vault
      , circulation )
  | Wrong_owner_vk ->
      ( { owner with
          zkapp =
            Some
              (zkapp_with_vk
                 ~vk_hash:Field.(commit_vk_hash + one)
                 ~app_state:(Zkapp_state.V.to_list owner_zkapp.app_state) )
        }
      , admin
      , vault
      , circulation )
  | Wrong_owner_permissions ->
      ( { owner with permissions = Permissions.user_default }
      , admin
      , vault
      , circulation )
  | Wrong_admin_id ->
      ( owner
      , { admin with public_key = point_of_string "61102" }
      , vault
      , circulation )
  | Wrong_admin_vk ->
      ( owner
      , { admin with
          zkapp =
            Some
              (zkapp_with_vk
                 ~vk_hash:Field.(commit_vk_hash + one)
                 ~app_state:(Zkapp_state.V.to_list admin_zkapp.app_state) )
        }
      , vault
      , circulation )
  | Wrong_admin_permissions ->
      ( owner
      , { admin with permissions = Permissions.user_default }
      , vault
      , circulation )
  | Wrong_admin_authority ->
      let bad_admin_zkapp =
        zkapp_with_vk ~vk_hash:commit_vk_hash
          ~app_state:
            [ commit_registration_authority.x
            ; ( if commit_registration_authority.is_odd then Field.one
              else Field.zero )
            ; Field.zero
            ; Field.zero
            ; Field.zero
            ; Field.zero
            ; Field.zero
            ; Field.zero
            ]
      in
      (owner, { admin with zkapp = Some bad_admin_zkapp }, vault, circulation)
  | Wrong_vault_id ->
      ( owner
      , admin
      , { vault with public_key = point_of_string "61103" }
      , circulation )
  | Wrong_vault_vk ->
      ( owner
      , admin
      , { vault with
          zkapp =
            Some
              (zkapp_with_vk
                 ~vk_hash:Field.(commit_vk_hash + one)
                 ~app_state:(Zkapp_state.V.to_list vault_zkapp.app_state) )
        }
      , circulation )
  | Wrong_vault_permissions ->
      ( owner
      , admin
      , { vault with permissions = Permissions.user_default }
      , circulation )
  | Wrong_vault_balance ->
      ( owner
      , admin
      , { vault with balance = Currency.Balance.of_uint64 Unsigned.UInt64.one }
      , circulation )
  | Wrong_circulation_id ->
      ( owner
      , admin
      , vault
      , { circulation with public_key = point_of_string "61104" } )
  | Wrong_circulation_permissions ->
      ( owner
      , admin
      , vault
      , { circulation with permissions = Permissions.user_default } )
  | Wrong_circulation_balance ->
      ( owner
      , admin
      , vault
      , { circulation with
          balance = Currency.Balance.of_uint64 Unsigned.UInt64.one
        } )

let sparse_ledger accounts =
  let ledger =
    Mina_ledger.Ledger.create_ephemeral
      ~depth:Zeko_constants.constraint_constants.ledger_depth ()
  in
  List.iter accounts ~f:(fun account ->
      Mina_ledger.Ledger.create_new_account_exn ledger
        (Mina_base.Account.identifier account)
        account ) ;
  Mina_ledger.Sparse_ledger.of_ledger_subset_exn ledger
    (List.map accounts ~f:Mina_base.Account.identifier)

let commit_opening sparse account =
  let index =
    Mina_ledger.Sparse_ledger.find_index_exn sparse
      (Mina_base.Account.identifier account)
  in
  let account = Mina_ledger.Sparse_ledger.get_exn sparse index in
  let path =
    Mina_ledger.Sparse_ledger.path_exn sparse index
    |> List.map ~f:(function
         | `Left hash ->
             ( { Commit_rule.Registry_path.Step.hash_other = hash
               ; is_right = false
               }
               : Commit_rule.Registry_path.Step.t )
         | `Right hash ->
             { hash_other = hash; is_right = true } )
  in
  (account, path)

let make_commit_registration_fixture ?(mutation = Valid_registration)
    ?(did_append = true) ?(wrong_count = false) ?(wrong_root = false) () =
  let base_inventory = amount "100000000" in
  let candidate_inventory =
    if Poly.equal mutation Wrong_inventory_cap then amount "100000001"
    else base_inventory
  in
  let candidate = commit_candidate candidate_inventory in
  let owner, admin, vault, circulation =
    make_commit_accounts mutation candidate
  in
  let old_tree = Zeko_circuits.Asset_registry.Merkle_list.empty () in
  let new_tree =
    if did_append then
      Zeko_circuits.Asset_registry.Merkle_list.append_exn old_tree candidate
    else old_tree
  in
  let old_state = registry_state old_tree in
  let new_state =
    let state = registry_state new_tree in
    { state with
      root = (if wrong_root then Field.(state.root + one) else state.root)
    ; leaf_count =
        ( if wrong_count then
          Zeko_circuits.Zeko_util.Checked32.(
            if did_append then old_state.leaf_count else one)
        else state.leaf_count )
    }
  in
  let old_registry = registry_account old_state in
  let new_registry = registry_account new_state in
  let source =
    sparse_ledger [ old_registry; owner; admin; vault; circulation ]
  in
  let target =
    sparse_ledger [ new_registry; owner; admin; vault; circulation ]
  in
  let old_registry_acc, old_registry_path =
    commit_opening source old_registry
  in
  let new_registry_acc, new_registry_path =
    commit_opening target new_registry
  in
  let token_owner_acc, token_owner_path = commit_opening target owner in
  let admin_acc, admin_path = commit_opening target admin in
  let vault_acc, vault_path = commit_opening target vault in
  let circulation_acc, circulation_path = commit_opening target circulation in
  { source_ledger = Mina_ledger.Sparse_ledger.merkle_root source
  ; target_ledger = Mina_ledger.Sparse_ledger.merkle_root target
  ; old_registry_acc
  ; old_registry_path
  ; new_registry_acc
  ; new_registry_path
  ; registration =
      { did_append
      ; candidate
      ; append_path =
          ( if did_append then
            Zeko_circuits.Asset_registry.Merkle_list.path new_tree ~index:0
          else
            List.init Zeko_constants.Ethereum_asset_registry.depth ~f:(fun _ ->
                Field.zero ) )
      ; token_owner_acc
      ; token_owner_path
      ; admin_acc
      ; admin_path
      ; vault_acc
      ; vault_path
      ; circulation_acc
      ; circulation_path
      }
  }

let run_commit_registration_fixture fixture =
  let open Snark_params.Tick in
  let open Zeko_circuits.Zeko_util in
  run_and_check_exn
    (let%bind.Checked source_ledger =
       exists Mina_base.Ledger_hash.typ ~compute:(fun _ ->
           fixture.source_ledger )
     in
     let%bind.Checked target_ledger =
       exists Mina_base.Ledger_hash.typ ~compute:(fun _ ->
           fixture.target_ledger )
     in
     let%bind.Checked old_registry_acc =
       exists Mina_base.Account.typ ~compute:(fun _ -> fixture.old_registry_acc)
     in
     let%bind.Checked old_registry_path =
       exists Commit_rule.Registry_path.typ ~compute:(fun _ ->
           fixture.old_registry_path )
     in
     let%bind.Checked new_registry_acc =
       exists Mina_base.Account.typ ~compute:(fun _ -> fixture.new_registry_acc)
     in
     let%bind.Checked new_registry_path =
       exists Commit_rule.Registry_path.typ ~compute:(fun _ ->
           fixture.new_registry_path )
     in
     let%bind.Checked registration =
       exists Commit_rule.Registration_witness.typ ~compute:(fun _ ->
           fixture.registration )
     in
     let%bind.Checked implied_old_root =
       Commit_rule.implied_registry_root old_registry_acc old_registry_path
     in
     let%bind.Checked () =
       assert_equal ~label:"test source registry opening"
         Mina_base.Ledger_hash.typ source_ledger
         (Mina_base.Ledger_hash.var_of_hash_packed implied_old_root)
     in
     let%bind.Checked implied_new_root =
       Commit_rule.implied_registry_root new_registry_acc new_registry_path
     in
     let%bind.Checked () =
       assert_equal ~label:"test target registry opening"
         Mina_base.Ledger_hash.typ target_ledger
         (Mina_base.Ledger_hash.var_of_hash_packed implied_new_root)
     in
     let%bind.Checked old_state =
       Commit_rule.registry_state_of_account
         ~registry_public_key:commit_registry_public_key old_registry_acc
     in
     let%bind.Checked new_state =
       Commit_rule.registry_state_of_account
         ~registry_public_key:commit_registry_public_key new_registry_acc
     in
     let%map.Checked () =
       Commit_rule.validate_registration_accounts ~target_ledger ~old_state
         ~new_state registration
     in
     As_prover.return () )

let corrupt_commit_path (path : Commit_rule.Registry_path.t) :
    Commit_rule.Registry_path.t =
  match path with
  | { Commit_rule.Registry_path.Step.hash_other; is_right } :: rest ->
      { Commit_rule.Registry_path.Step.hash_other = Field.(hash_other + one)
      ; is_right
      }
      :: rest
  | [] ->
      assert false

let expect_commit_failure label fixture =
  if not (Exn.does_raise (fun () -> run_commit_registration_fixture fixture))
  then failwithf "%s unexpectedly satisfied commit constraints" label ()

let check_commit_registration_constraints () =
  run_commit_registration_fixture (make_commit_registration_fixture ()) ;
  run_commit_registration_fixture
    (make_commit_registration_fixture ~did_append:false ()) ;
  expect_commit_failure "no-append registry count mutation"
    (make_commit_registration_fixture ~did_append:false ~wrong_count:true ()) ;
  expect_commit_failure "no-append registry root mutation"
    (make_commit_registration_fixture ~did_append:false ~wrong_root:true ()) ;
  let bad_source_path = make_commit_registration_fixture () in
  expect_commit_failure "forged source registry opening"
    { bad_source_path with
      old_registry_path = corrupt_commit_path bad_source_path.old_registry_path
    } ;
  let bad_target_path = make_commit_registration_fixture () in
  expect_commit_failure "forged target registry opening"
    { bad_target_path with
      new_registry_path = corrupt_commit_path bad_target_path.new_registry_path
    } ;
  expect_commit_failure "forged registry count transition"
    (make_commit_registration_fixture ~wrong_count:true ()) ;
  expect_commit_failure "forged registry root transition"
    (make_commit_registration_fixture ~wrong_root:true ()) ;
  let bad_owner_path = make_commit_registration_fixture () in
  expect_commit_failure "forged token owner opening"
    { bad_owner_path with
      registration =
        { bad_owner_path.registration with
          token_owner_path =
            corrupt_commit_path bad_owner_path.registration.token_owner_path
        }
    } ;
  List.iter
    [ ("wrong token owner ID", Wrong_owner_id)
    ; ("wrong token owner VK", Wrong_owner_vk)
    ; ("wrong token owner permissions", Wrong_owner_permissions)
    ; ("wrong token admin ID", Wrong_admin_id)
    ; ("wrong token admin VK", Wrong_admin_vk)
    ; ("wrong token admin permissions", Wrong_admin_permissions)
    ; ("live registration signer as token admin", Wrong_admin_authority)
    ; ("wrong vault ID", Wrong_vault_id)
    ; ("wrong vault VK", Wrong_vault_vk)
    ; ("wrong vault permissions", Wrong_vault_permissions)
    ; ("wrong vault balance", Wrong_vault_balance)
    ; ("wrong circulation ID", Wrong_circulation_id)
    ; ("wrong circulation permissions", Wrong_circulation_permissions)
    ; ("wrong circulation balance", Wrong_circulation_balance)
    ; ("wrong inventory cap", Wrong_inventory_cap)
    ]
    ~f:(fun (label, mutation) ->
      expect_commit_failure label
        (make_commit_registration_fixture ~mutation ()) )

let check_erc20_finalize_deposit
    ~(membership : Ethereum_token_bridge.Registry.Membership_witness.t) =
  let open Mina_base in
  let open Zeko_circuits in
  let open Zeko_util in
  let registered_asset = membership.record in
  let params : Bridge_state.Deposit_params_ethereum_token.t =
    let base : Bridge_state.Deposit_params_base.t =
      { children = []
      ; holder_account_l1 = ({ x = Field.one; is_odd = false } : PC.t)
      ; amount = amount "2000000"
      ; recipient = ({ x = Field.of_int 0x01020304; is_odd = false } : PC.t)
      ; timeout = Mina_numbers.Global_slot_since_genesis.max_value
      }
    in
    { encoding_version = Checked32.of_int 2
    ; registry_index = registered_asset.registry_index
    ; record_commitment =
        Asset_registry.Asset_record.commitment registered_asset
    ; asset_id_high = registered_asset.asset_id_high
    ; asset_id_low = registered_asset.asset_id_low
    ; base
    }
  in
  let base = params.base in
  let witness : Rollup_state.Outer_action.Witness.t =
    { aux =
        Utils.value_to_hash ~init:Zeko_constants.ethereum_erc20_deposit_v2_salt
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
      { public_key = registered_asset.vault_public_key
      ; vk_hash
      ; asset = membership
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
  let token_id = registered_asset.token_id_l2 in
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
  | [ helper; _inner_witness; recipient; zero_fee; registry ] ->
      let expected_helper_token =
        Account_id.create registered_asset.vault_public_key token_id
        |> fun owner -> Account_id.derive_token_id ~owner
      in
      if not (Token_id.equal helper.body.token_id expected_helper_token) then
        failwith
          "Ethereum ERC20 replay helper did not derive from the full vault \
           account ID" ;
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
      then failwith "Ethereum ERC20 circuit emitted a token-denominated fee" ;
      if not (PC.equal registry.body.public_key (point_of_string "22222")) then
        failwith "Ethereum ERC20 circuit did not authenticate the registry" ;
      if
        not
          (Mina_base.Account_update.Authorization_kind.equal
             registry.body.authorization_kind None_given )
      then
        failwith
          "Ethereum ERC20 registry precondition unexpectedly requires \
           authorization" ;
      helper.body.token_id
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
    { encoding_version = Zeko_circuits.Zeko_util.Checked32.of_int 2
    ; registry_index = first_registered_asset.registry_index
    ; record_commitment =
        Zeko_circuits.Asset_registry.Asset_record.commitment
          first_registered_asset
    ; asset_id_high = Field.one
    ; asset_id_low = Field.of_int 2
    ; custom
    }
  in
  let Compile_simple.[ _finalize; inner_receive; _multisig ] =
    Lazy.force Ethereum_token_bridge.System_L2.provers
  in
  let (_statement, (vault_body, _digest, registry_calls)), _proof =
    Promise.block_on_async_exn
    @@ fun () ->
    inner_receive
      { public_key = first_registered_asset.vault_public_key
      ; vk_hash = Field.one
      ; asset = first_membership
      ; amount = withdrawal_amount
      }
  in
  if vault_body.use_full_commitment then
    failwith "Ethereum ERC20 withdrawal vault uses the full commitment" ;
  if vault_body.implicit_account_creation_fee then
    failwith "Ethereum ERC20 withdrawal vault charges an account-creation fee" ;
  if not (Token_id.equal vault_body.token_id first_registered_asset.token_id_l2)
  then failwith "Ethereum ERC20 withdrawal used the wrong registered token" ;
  if
    not
      (String.equal
         (field_to_hex vault_body.call_data)
         "19a86bb8c106848e3bab8dd79491cbc4589f28b33227007eb60409efa56e12d0" )
  then failwith "Ethereum ERC20 bridge call-data vector mismatch" ;
  ( match Zkapp_command.Call_forest.to_account_updates registry_calls with
  | [ registry ] ->
      if not (PC.equal registry.body.public_key (point_of_string "22222")) then
        failwith "Ethereum ERC20 withdrawal did not authenticate the registry"
      else if not registry.body.use_full_commitment then
        failwith
          "Ethereum ERC20 registry precondition omitted the full commitment"
      else if not registry.body.implicit_account_creation_fee then
        failwith
          "Ethereum ERC20 registry precondition omitted the account-creation \
           fee"
      else if
        not
          (String.equal
             (field_to_hex
                (Account_update.Body.digest
                   ~signature_kind:Mina_signature_kind.Testnet registry.body ) )
             "2c383e7260ea384af96f3c833feee18b10a1d614f852076287c1cb513e5b47d6" )
      then failwith "Ethereum ERC20 registry precondition body vector mismatch"
  | _ ->
      failwith "unexpected ERC20 registry precondition forest" ) ;
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
    Utils.value_to_hash ~init:Zeko_constants.ethereum_erc20_withdrawal_v2_salt
      Zeko_circuits.Bridge_state.Withdrawal_params_ethereum_token.typ params
    |> field_to_hex
  in
  (params_fields, withdrawal_aux)

let run_erc20_withdrawal_action_with_recipient recipient =
  let open Mina_base in
  let open Snark_params.Tick in
  let chain = Mina_signature_kind.Testnet in
  let token_owner =
    Account_id.create (point_of_string "344213") Token_id.default
  in
  let token_id = Account_id.derive_token_id ~owner:token_owner in
  let withdrawal_amount = amount "2000000" in
  let debit =
    Account_update.with_aux
      ~body:
        { Account_update.Body.dummy with
          public_key = point_of_string "98881"
        ; token_id
        ; balance_change =
            Currency.Amount.Signed.(of_unsigned withdrawal_amount |> negate)
        ; may_use_token = Parents_own_token
        ; authorization_kind = Signature
        ; use_full_commitment = true
        }
      ~authorization:Control.Poly.None_given
  in
  let params : Zeko_circuits.Bridge_state.Withdrawal_params_ethereum_token.t =
    { encoding_version = Zeko_circuits.Zeko_util.Checked32.of_int 2
    ; registry_index = first_registered_asset.registry_index
    ; record_commitment =
        Zeko_circuits.Asset_registry.Asset_record.commitment
          first_registered_asset
    ; asset_id_high = first_registered_asset.asset_id_high
    ; asset_id_low = first_registered_asset.asset_id_low
    ; custom =
        { token_owner_body =
            { Account_update.Body.dummy with
              public_key = Account_id.public_key token_owner
            ; token_id = Account_id.token_id token_owner
            ; authorization_kind = Proof Field.one
            }
        ; nested_children =
            Zkapp_command.Call_forest.cons ~signature_kind:chain debit []
        ; base = { children = []; amount = withdrawal_amount; recipient }
        }
    }
  in
  run_and_check_exn
    (let%bind.Checked params =
       exists
         Zeko_circuits.Bridge_state.Withdrawal_params_ethereum_token.typ
         ~compute:(fun _ -> params)
     in
     let%map.Checked _action =
       make_checked (fun () ->
           Run.run_checked
             (Zeko_circuits.Bridge_state.withdrawal_action ~chain_l2:chain
                ~holder_account_l2:first_registered_asset.vault_public_key
                ~token_owner_l2:(Some token_owner)
                ~ethereum_asset_id:
                  (Some
                     ( first_registered_asset.asset_id_high
                     , first_registered_asset.asset_id_low ) )
                ~ethereum_registry_binding:
                  (Some
                     ( first_registered_asset.registry_index
                     , Zeko_circuits.Asset_registry.Asset_record.commitment
                         first_registered_asset ) )
                ~l2_holder_vk_hash:(constant Field.typ Field.one)
                ~bridge_fee_recipient_l2:
                  (constant PC.typ (point_of_string "98882"))
                ~bridge_proof_fee:
                  (constant Currency.Amount.typ Currency.Amount.zero)
                ( module Zeko_circuits.Bridge_state
                         .Withdrawal_params_ethereum_token )
                params ) )
     in
     As_prover.return () )

let run_native_withdrawal_action_with_recipient ~recipient_domain:domain
    recipient =
  let open Snark_params.Tick in
  let module Withdrawal_params = struct
    include Zeko_circuits.Bridge_state.Withdrawal_params_base

    let recipient_domain = domain
  end in
  let params : Withdrawal_params.t =
    { children = []; amount = amount "2000000"; recipient }
  in
  run_and_check_exn
    (let%bind.Checked params =
       exists Withdrawal_params.typ ~compute:(fun _ -> params)
     in
     let%map.Checked _action =
       make_checked (fun () ->
           Run.run_checked
             (Zeko_circuits.Bridge_state.withdrawal_action
                ~chain_l2:Mina_signature_kind.Testnet
                ~holder_account_l2:(point_of_string "98883")
                ~token_owner_l2:None ~ethereum_asset_id:None
                ~ethereum_registry_binding:None
                ~l2_holder_vk_hash:(constant Field.typ Field.one)
                ~bridge_fee_recipient_l2:
                  (constant PC.typ (point_of_string "98884"))
                ~bridge_proof_fee:
                  (constant Currency.Amount.typ Currency.Amount.zero)
                (module Withdrawal_params)
                params ) )
     in
     As_prover.return () )

let run_legacy_erc20_withdrawal_action_with_recipient recipient =
  let open Mina_base in
  let open Snark_params.Tick in
  let chain = Mina_signature_kind.Testnet in
  let token_owner =
    Account_id.create (point_of_string "344213") Token_id.default
  in
  let token_id = Account_id.derive_token_id ~owner:token_owner in
  let withdrawal_amount = amount "2000000" in
  let debit =
    Account_update.with_aux
      ~body:
        { Account_update.Body.dummy with
          public_key = point_of_string "98881"
        ; token_id
        ; balance_change =
            Currency.Amount.Signed.(of_unsigned withdrawal_amount |> negate)
        ; may_use_token = Parents_own_token
        ; authorization_kind = Signature
        ; use_full_commitment = true
        }
      ~authorization:Control.Poly.None_given
  in
  let params :
      Zeko_circuits.Bridge_state.Withdrawal_params_ethereum_token_v1.t =
    { asset_id_high = first_registered_asset.asset_id_high
    ; asset_id_low = first_registered_asset.asset_id_low
    ; custom =
        { token_owner_body =
            { Account_update.Body.dummy with
              public_key = Account_id.public_key token_owner
            ; token_id = Account_id.token_id token_owner
            ; authorization_kind = Proof Field.one
            }
        ; nested_children =
            Zkapp_command.Call_forest.cons ~signature_kind:chain debit []
        ; base = { children = []; amount = withdrawal_amount; recipient }
        }
    }
  in
  run_and_check_exn
    (let%bind.Checked params =
       exists
         Zeko_circuits.Bridge_state.Withdrawal_params_ethereum_token_v1.typ
         ~compute:(fun _ -> params)
     in
     let%map.Checked _action =
       make_checked (fun () ->
           Run.run_checked
             (Zeko_circuits.Bridge_state.withdrawal_action ~chain_l2:chain
                ~holder_account_l2:first_registered_asset.vault_public_key
                ~token_owner_l2:(Some token_owner)
                ~ethereum_asset_id:
                  (Some
                     ( first_registered_asset.asset_id_high
                     , first_registered_asset.asset_id_low ) )
                ~ethereum_registry_binding:None
                ~l2_holder_vk_hash:(constant Field.typ Field.one)
                ~bridge_fee_recipient_l2:
                  (constant PC.typ (point_of_string "98882"))
                ~bridge_proof_fee:
                  (constant Currency.Amount.typ Currency.Amount.zero)
                ( module Zeko_circuits.Bridge_state
                         .Withdrawal_params_ethereum_token_v1 )
                params ) )
     in
     As_prover.return () )

let registry_count_update_command ?(updates = 1) () : Mina_base.User_command.t =
  let open Mina_base in
  let app_state =
    List.init 8 ~f:(fun index ->
        if Int.equal index 1 then Zkapp_basic.Set_or_keep.Set Field.one
        else Zkapp_basic.Set_or_keep.Keep )
    |> Zkapp_state.V.of_list_exn
  in
  let account_updates =
    List.init updates ~f:(fun _ ->
        Account_update.with_aux
          ~body:
            { Account_update.Body.dummy with
              public_key =
                Zeko_circuits_config.Inputs.Ethereum_assets.registry_public_key
            ; update = { Account_update.Update.dummy with app_state }
            }
          ~authorization:Control.Poly.None_given )
    |> List.fold_right ~init:[] ~f:(fun update forest ->
           Zkapp_command.Call_forest.cons
             ~signature_kind:Zeko_circuits_config.Inputs.chain_l2 update forest
       )
  in
  User_command.Zkapp_command
    { fee_payer =
        Account_update.Fee_payer.make
          ~body:Account_update.Body.Fee_payer.dummy
          ~authorization:Signature.dummy
    ; account_updates
    ; memo = Signed_command_memo.empty
    }

let () =
  let packed_x = String.make 63 '0' ^ "1" in
  let even_key : PC.t = { x = Field.one; is_odd = false } in
  let odd_key : PC.t = { x = Field.one; is_odd = true } in
  let checkpoint_state : Zeko_circuits.Asset_registry.Registry_state.t =
    { root = Field.of_int 17
    ; leaf_count = Zeko_circuits.Zeko_util.Checked32.of_int 3
    ; schema_version =
        Zeko_circuits.Zeko_util.Checked32.of_int
          Zeko_constants.Ethereum_asset_registry.schema_version
    }
  in
  if
    Field.equal
      (Zeko_circuits.Asset_registry.Checkpoint.commitment
         ~registry_public_key:even_key checkpoint_state )
      (Zeko_circuits.Asset_registry.Checkpoint.commitment
         ~registry_public_key:odd_key checkpoint_state )
  then failwith "registry checkpoint collides for opposite key parity" ;
  if
    not
      (String.equal
         (Sequencer_lib.Ethereum_settlement_export.packed_public_key_hex
            even_key )
         ("0x" ^ packed_x) )
  then failwith "even Mina public-key settlement packing mismatch" ;
  if
    not
      (String.equal
         (Sequencer_lib.Ethereum_settlement_export.packed_public_key_hex odd_key)
         ("0x8" ^ String.drop_prefix packed_x 1) )
  then failwith "odd Mina public-key settlement packing mismatch" ;
  Zeko_circuits.Bridge_state.Ethereum_address.validate even_key
  |> Or_error.ok_exn ;
  let oversized_key : PC.t =
    { x = Field.project (List.init 161 ~f:(Int.equal 160)); is_odd = false }
  in
  let assert_recipient_rejected label f =
    if Result.is_ok (Or_error.try_with f) then
      failwithf "%s accepted an invalid Ethereum recipient" label ()
  in
  List.iter [ odd_key; oversized_key ] ~f:(fun recipient ->
      if
        Result.is_ok
          (Zeko_circuits.Bridge_state.Ethereum_address.validate recipient)
      then failwith "invalid Ethereum withdrawal recipient was accepted" ;
      assert_recipient_rejected "registry V2 withdrawal circuit" (fun () ->
          run_erc20_withdrawal_action_with_recipient recipient ) ;
      assert_recipient_rejected "legacy V1 withdrawal circuit" (fun () ->
          run_legacy_erc20_withdrawal_action_with_recipient recipient ) ;
      assert_recipient_rejected "native Ethereum withdrawal circuit" (fun () ->
          run_native_withdrawal_action_with_recipient
            ~recipient_domain:
              Zeko_circuits.Bridge_state.Withdrawal_recipient_domain.Ethereum
            recipient ) ;
      run_native_withdrawal_action_with_recipient
        ~recipient_domain:
          Zeko_circuits.Bridge_state.Withdrawal_recipient_domain.Mina
        recipient ) ;
  let valid_ethereum_key : PC.t =
    { x = Field.of_int 0x01020304; is_odd = false }
  in
  run_erc20_withdrawal_action_with_recipient valid_ethereum_key ;
  run_legacy_erc20_withdrawal_action_with_recipient valid_ethereum_key ;
  run_native_withdrawal_action_with_recipient
    ~recipient_domain:
      Zeko_circuits.Bridge_state.Withdrawal_recipient_domain.Ethereum
    valid_ethereum_key ;
  let module Admission =
    Sequencer_lib.Zeko_sequencer.Sequencer.Ethereum_asset_registry_admission
  in
  let single_update_count =
    Admission.count_command_updates_for_account
      ~registry_id:(Admission.account_id ())
      (registry_count_update_command ())
  in
  let batched_update_count =
    Admission.count_command_updates_for_account
      ~registry_id:(Admission.account_id ())
      (registry_count_update_command ~updates:2 ())
  in
  if not (Int.equal single_update_count 1)
  then failwith "registry count update escaped common admission counting" ;
  if not (Int.equal batched_update_count 2)
  then failwith "batched registry count updates collapsed during admission" ;
  if
    Result.is_ok
      (Admission.validate_counts ~committed_count:0 ~current_count:0
         ~registry_update_count:batched_update_count )
  then failwith "two registry updates in one command were accepted" ;
  Admission.validate_counts ~committed_count:0 ~current_count:0
    ~registry_update_count:single_update_count
  |> Or_error.ok_exn ;
  if
    Result.is_ok
      (Admission.validate_counts ~committed_count:0 ~current_count:1
         ~registry_update_count:single_update_count )
  then failwith "second pending registry registration was accepted" ;
  Admission.validate_counts ~committed_count:1 ~current_count:1
    ~registry_update_count:single_update_count
  |> Or_error.ok_exn ;
  let expected_registry_permissions : Mina_base.Permissions.t =
    { Sequencer_lib.Deploy.Z.proof_permissions with access = None }
  in
  if
    not
      (Mina_base.Permissions.equal
         Sequencer_lib.Deploy.Z.proof_permissions_with_unconditional_access
         expected_registry_permissions )
  then
    failwith
      "Ethereum asset registry permissions reject precondition-only access" ;
  let Compile_simple.[ _; _; _ ] =
    Lazy.force Ethereum_token_bridge.System_L2.provers
  in
  check_commit_registration_constraints () ;
  check_erc20_deposit_witness_action () ;
  let first_helper_token =
    check_erc20_finalize_deposit ~membership:first_membership
  in
  let second_helper_token =
    check_erc20_finalize_deposit ~membership:second_membership
  in
  if Mina_base.Token_id.equal first_helper_token second_helper_token then
    failwith "distinct registered assets reused a replay-helper token ID" ;
  let withdrawal_params_fields, withdrawal_aux =
    erc20_withdrawal_params_fields ()
  in
  if
    not
      (String.equal withdrawal_aux
         "3832fb1d782fc05d8aa1a0185046c4f25a4d7fb87d1fb3bb4b71850cd00a455e" )
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
  | ( "tokenWithdrawal"
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
  if
    Result.is_ok
      (Or_error.try_with (fun () ->
           ignore
             ( Sequencer_lib.Ethereum_settlement_export
               .ethereum_withdrawal_preimage_json
                 { token_preimage with recipient = odd_key }
               : string * Yojson.Safe.t ) ) )
  then failwith "invalid withdrawal recipient was silently omitted" ;
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
