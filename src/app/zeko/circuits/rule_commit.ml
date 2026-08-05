open Core_kernel
open Zeko_util
open Snark_params.Tick
module PC = Signature_lib.Public_key.Compressed
open Mina_base
open Rollup_state
open Checked.Let_syntax

(** Used to prove that the synchronized outer action state is a predecessor of the current one. *)
module Ase_outer_inst = Ase.Without_length.Make (struct
  module Action_state = Outer_action_state

  let get_iterations = Zeko_constants.Max_excess_actions.Commit.outer
end)

(** Used to prove the length of the inner action state as stored on the outer account. *)
module Ase_inner_inst = Ase.With_length.Make (struct
  module Action_state = Inner_action_state

  let get_iterations = Zeko_constants.Max_excess_actions.Commit.inner
end)

(** Proves both Ase_outer_inst and Ase_inner_inst, to circumvent limitation of two recursive proof verifications per proof. *)
module Verify_both_ases = struct
  let main (w : (Ase_outer_inst.t * Ase_inner_inst.t) V.t) =
    let* outer, inner =
      exists ~compute:(V.get w) Typ.(Ase_outer_inst.typ * Ase_inner_inst.typ)
    in
    let* outer, verify_outer = Ase_outer_inst.get outer in
    let*| inner, verify_inner = Ase_inner_inst.get inner in
    Compile_simple.
      { prevs = Two_prevs (verify_outer, verify_inner); out = (outer, inner) }

  let rule : _ Compile_simple.branch lazy_t =
    lazy
      { branch_name = "Verify_both_ases"
      ; tags =
          Two_tags
            (Lazy.force Ase.Without_length.tag, Lazy.force Ase.With_length.tag)
      ; main
      }

  include
    ( val Compile_simple.compile ~name:"Verify_both_ases" ~branches:[ rule ]
            ~out_typ:Typ.(Ase_outer_inst.Stmt.typ * Ase_inner_inst.Stmt.typ)
            () )
end

(** Used to prove the number of commits in the emergency case. *)
module Count_commits_inst = Count_commits.Make (struct
  let get_iterations = Zeko_constants.Max_excess_actions.Commit.count_commits
end)

(** Used to verify emergency DA folder proof. *)
module Emergency_da_inst = Emergency_da_folder.Make (struct
  let get_iterations = Zeko_constants.Max_excess_actions.Commit.emergency_da
end)

(** Proves emergency folders (count commits + emergency DA). *)
module Verify_emergency_folders = struct
  let main (w : (Count_commits_inst.t * Emergency_da_inst.t) V.t) =
    let* count_commits, emergency_da =
      exists ~compute:(V.get w)
        Typ.(Count_commits_inst.typ * Emergency_da_inst.typ)
    in
    let* count_commits, verify_count_commits =
      Count_commits_inst.get count_commits
    in
    let*| emergency_da, verify_emergency_da =
      Emergency_da_inst.get emergency_da
    in
    Compile_simple.
      { prevs = Two_prevs (verify_count_commits, verify_emergency_da)
      ; out = (count_commits, emergency_da)
      }

  let rule : _ Compile_simple.branch lazy_t =
    lazy
      { branch_name = "Verify_emergency_folders"
      ; tags =
          Two_tags
            (Lazy.force Count_commits.tag, Lazy.force Emergency_da_folder.tag)
      ; main
      }

  include
    ( val Compile_simple.compile ~name:"Verify_emergency_folders"
            ~branches:[ rule ]
            ~out_typ:
              Typ.(
                Count_commits.Definition.Stmt.typ * Emergency_da_folder.Stmt.typ)
            () )
end

module Verify_base = struct
  let main (w : (Txn_rules.t * Verify_both_ases.t) V.t) =
    let* txn, verify_both_ases =
      exists ~compute:(V.get w) Typ.(Txn_rules.typ * Verify_both_ases.typ)
    in
    let* txn_stmt, verify_txn = Txn_rules.get txn in
    let*| both_ases, verify_both_ases = Verify_both_ases.get verify_both_ases in
    let ase_outer, ase_inner = both_ases in
    Compile_simple.
      { prevs = Two_prevs (verify_txn, verify_both_ases)
      ; out = (txn_stmt, (ase_outer, ase_inner))
      }

  let rule : _ Compile_simple.branch lazy_t =
    lazy
      { branch_name = "Verify_base"
      ; tags =
          Two_tags (Lazy.force Txn_rules.tag, Lazy.force Verify_both_ases.tag)
      ; main
      }

  include
    ( val Compile_simple.compile ~name:"Verify_base_wrappers" ~branches:[ rule ]
            ~out_typ:
              Typ.(
                Txn_state.Zeko_stmt.typ
                * (Ase_outer_inst.Stmt.typ * Ase_inner_inst.Stmt.typ))
            () )
end

module Make (Inputs : sig
  (** max_valid_while_size signifies how big the valid_while can be for commits. *)
  val max_valid_while_size : int

  (** The public key of the inner account *)
  val inner_public_key : PC.t

  val chain_l1 : Mina_signature_kind.t

  val chain_l2 : Mina_signature_kind.t

  val max_sequencer_inactivity : int

  val emergency_da_public_key : PC.t

  val ethereum_asset_registry_public_key : PC.t option

  val ethereum_asset_registry_schema_version : Checked32.t

  val ethereum_asset_approved_mft_standard_vk_id : F.t

  val ethereum_asset_approved_mft_token_vk_hash : F.t

  val ethereum_asset_approved_mft_admin_vk_hash : F.t

  val ethereum_asset_universal_bridge_vk_id : F.t

  val ethereum_asset_universal_bridge_vk_hash : F.t

  val ethereum_asset_vault_public_key : PC.t
end) =
struct
  open Inputs

  module PathElt = struct
    type t = { right_side : F.t } [@@deriving snarky]
  end

  (** Witness for path to inner account. Path is implicitly all left. *)
  module Path =
    SnarkList
      (PathElt)
      (struct
        let length = Account_set.height
      end)

  module Registry_path = struct
    module Step = struct
      type t = { hash_other : F.t; is_right : Zeko_util.Boolean.t }
      [@@deriving snarky]

      let to_yojson ({ hash_other; is_right } : t) =
        `Assoc
          [ ("hash_other", Field.to_yojson hash_other)
          ; ("is_right", `Bool is_right)
          ]

      let of_yojson json =
        let open Yojson.Safe.Util in
        try
          let hash_other =
            match member "hash_other" json |> Field.of_yojson with
            | Ok hash ->
                hash
            | Error error ->
                failwith error
          in
          Ok ({ hash_other; is_right = member "is_right" json |> to_bool } : t)
        with exn -> Error (Exn.to_string exn)
    end

    include
      SnarkList
        (Step)
        (struct
          let length = Zeko_constants.constraint_constants.ledger_depth
        end)

    let to_yojson path = `List (List.map path ~f:Step.to_yojson)

    let of_yojson = function
      | `List values ->
          List.fold_right values ~init:(Ok []) ~f:(fun value acc ->
              match (Step.of_yojson value, acc) with
              | Ok value, Ok rest ->
                  Ok (value :: rest)
              | Error error, _ | _, Error error ->
                  Error error )
      | _ ->
          Error "registry account path must be a JSON list"
  end

  module Registration_witness = struct
    type t =
      { did_append : Zeko_util.Boolean.t
      ; candidate : Asset_registry.Asset_record.t
      ; append_path : Asset_registry.Path.t
      ; token_owner_acc : Account.t
      ; token_owner_path : Registry_path.t
      ; admin_acc : Account.t
      ; admin_path : Registry_path.t
      ; vault_acc : Account.t
      ; vault_path : Registry_path.t
      ; circulation_acc : Account.t
      ; circulation_path : Registry_path.t
      }
    [@@deriving snarky]

    let to_yojson
        ({ did_append
         ; candidate
         ; append_path
         ; token_owner_acc
         ; token_owner_path
         ; admin_acc
         ; admin_path
         ; vault_acc
         ; vault_path
         ; circulation_acc
         ; circulation_path
         } :
          t ) =
      `Assoc
        [ ("did_append", `Bool did_append)
        ; ("candidate", Asset_registry.Asset_record.to_yojson candidate)
        ; ("append_path", Asset_registry.Path.to_yojson append_path)
        ; ("token_owner_acc", Account.to_yojson token_owner_acc)
        ; ("token_owner_path", Registry_path.to_yojson token_owner_path)
        ; ("admin_acc", Account.to_yojson admin_acc)
        ; ("admin_path", Registry_path.to_yojson admin_path)
        ; ("vault_acc", Account.to_yojson vault_acc)
        ; ("vault_path", Registry_path.to_yojson vault_path)
        ; ("circulation_acc", Account.to_yojson circulation_acc)
        ; ("circulation_path", Registry_path.to_yojson circulation_path)
        ]

    let of_yojson json =
      let open Yojson.Safe.Util in
      let get parse name =
        match member name json |> parse with
        | Ok value ->
            value
        | Error error ->
            failwith error
      in
      try
        Ok
          ( { did_append = member "did_append" json |> to_bool
            ; candidate = get Asset_registry.Asset_record.of_yojson "candidate"
            ; append_path = get Asset_registry.Path.of_yojson "append_path"
            ; token_owner_acc = get Account.of_yojson "token_owner_acc"
            ; token_owner_path = get Registry_path.of_yojson "token_owner_path"
            ; admin_acc = get Account.of_yojson "admin_acc"
            ; admin_path = get Registry_path.of_yojson "admin_path"
            ; vault_acc = get Account.of_yojson "vault_acc"
            ; vault_path = get Registry_path.of_yojson "vault_path"
            ; circulation_acc = get Account.of_yojson "circulation_acc"
            ; circulation_path = get Registry_path.of_yojson "circulation_path"
            }
            : t )
      with exn -> Error (Exn.to_string exn)
  end

  module Base_witness = struct
    type t =
      { public_key : PC.t  (** Our public key on the L2 *)
      ; vk_hash : F.t  (** Our vk hash *)
      ; old_inner_acc : Account.t
      ; old_inner_acc_path : Path.t
      ; new_inner_acc : Account.t
      ; new_inner_acc_path : Path.t
      ; da_multisig : Multisig.Witness.t
      ; slot_range : Slot_range.t
      ; emergency_mode : Zeko_util.Boolean.t
      ; old_ethereum_asset_registry_acc : Account.t
      ; old_ethereum_asset_registry_path : Registry_path.t
      ; new_ethereum_asset_registry_acc : Account.t
      ; new_ethereum_asset_registry_path : Registry_path.t
      ; ethereum_asset_registration : Registration_witness.t
      }
    [@@deriving snarky]
  end

  module Witness = struct
    type t =
      { txn_snark : Txn_rules.t
      ; base_witness : Base_witness.t
      ; verify_both_ases : Verify_both_ases.t
      }
    [@@deriving snarky]
  end

  let implied_root (account : Account.var) (path : Path.var) : F.var Checked.t =
    let* init = Account.Checked.digest account in
    Checked.List.foldi path ~init ~f:(fun height acc PathElt.{ right_side } ->
        make_checked @@ fun () -> Ledger_hash.merge_var ~height acc right_side )

  let implied_registry_root (account : Account.var) (path : Registry_path.var) :
      F.var Checked.t =
    let* init = Account.Checked.digest account in
    Checked.List.foldi path ~init
      ~f:(fun height acc Registry_path.Step.{ hash_other; is_right } ->
        let* left = Field.Checked.if_ is_right ~then_:hash_other ~else_:acc in
        let* right = Field.Checked.if_ is_right ~then_:acc ~else_:hash_other in
        make_checked @@ fun () -> Ledger_hash.merge_var ~height left right )

  let get_zkapp (a : Account.var) : Zkapp_account.Checked.t Checked.t =
    let hash, content = a.zkapp in
    let* content =
      exists Zkapp_account.typ
        ~compute:
          (let+| content = As_prover.read (Typ.prover_value ()) content in
           Option.value ~default:Zkapp_account.default content )
    in
    let* digest =
      make_checked @@ fun () -> Zkapp_account.Checked.digest content
    in
    let*| () =
      with_label __LOC__ (fun () -> Field.Checked.Assert.equal hash digest)
    in
    content

  let checked32_of_field field =
    let* value =
      exists Checked32.typ
        ~compute:
          (let+| value = As_prover.read_var field in
           Field.to_string value |> Checked32.of_string )
    in
    let*| () =
      assert_equal ~label:"32-bit registry account state" F.typ field
        (Checked32.Checked.to_field value)
    in
    value

  let registry_state_of_account ~registry_public_key (account : Account.var) =
    let* () =
      PC.Checked.Assert.equal account.public_key
        (constant PC.typ registry_public_key)
    in
    let* () =
      assert_equal ~label:"registry account token ID" Token_id.typ
        account.token_id
        (constant Token_id.typ Token_id.default)
    in
    let* zkapp = get_zkapp account in
    let (root :: leaf_count_field :: schema_version_field :: _) =
      zkapp.app_state
    in
    let* leaf_count = checked32_of_field leaf_count_field in
    let* schema_version = checked32_of_field schema_version_field in
    let* () =
      assert_equal ~label:"registry account schema" Checked32.typ schema_version
        (Checked32.Checked.constant
           (Checked32.of_int
              Zeko_constants.Ethereum_asset_registry.schema_version ) )
    in
    let* has_valid_count =
      Checked32.Checked.(
        leaf_count
        < constant
            (Checked32.of_int
               (Zeko_constants.Ethereum_asset_registry.max_assets + 1) ))
    in
    let*| () = Boolean.Assert.is_true has_valid_count in
    ( { Asset_registry.Registry_state.root; leaf_count; schema_version }
      : Asset_registry.Registry_state.var )

  let assert_equal_if ~label condition typ left right =
    let* equal = var_equal typ left right in
    with_label label (fun () ->
        let open Boolean.Expr in
        ((not !condition) || equal) |> assert_ )

  let assert_true_if ~label condition value =
    with_label label (fun () ->
        let open Boolean.Expr in
        ((not !condition) || !value) |> assert_ )

  let authenticate_account_if ~label ~ledger_root ~active account path =
    let* root = implied_registry_root account path in
    assert_equal_if ~label active Ledger_hash.typ ledger_root
      (Ledger_hash.var_of_hash_packed root)

  let immutable_vk_permission =
    (Permissions.Auth_required.Impossible, Mina_numbers.Txn_version.current)

  let expected_token_owner_permissions : Permissions.t =
    { Permissions.user_default with
      access = Proof
    ; set_permissions = Impossible
    ; set_verification_key = immutable_vk_permission
    }

  let expected_token_admin_permissions : Permissions.t =
    { Permissions.user_default with
      set_permissions = Impossible
    ; set_verification_key = immutable_vk_permission
    }

  let expected_vault_permissions : Permissions.t =
    { Permissions.user_default with
      send = Proof
    ; set_permissions = Impossible
    ; set_verification_key = immutable_vk_permission
    }

  let expected_circulation_permissions : Permissions.t =
    { Permissions.user_default with send = None; set_permissions = Impossible }

  let assert_vk_hash_if ~label active zkapp expected =
    let verification_key = zkapp.Zkapp_account.Poly.verification_key in
    let* () =
      assert_true_if ~label:(label ^ " installed") active
        (Zkapp_basic.Flagged_option.is_some verification_key)
    in
    assert_equal_if ~label active F.typ
      (Data_as_hash.hash (Zkapp_basic.Flagged_option.data verification_key))
      (constant F.typ expected)

  let validate_registration_accounts ~target_ledger
      ~(old_state : Asset_registry.Registry_state.var)
      ~(new_state : Asset_registry.Registry_state.var)
      (registration : Registration_witness.var) =
    let active = registration.did_append in
    let candidate = registration.candidate in
    let* expected_count =
      Checked32.Checked.succ_if old_state.leaf_count active
    in
    let* () =
      assert_equal ~label:"registry count advances by at most one" Checked32.typ
        new_state.leaf_count expected_count
    in
    let* candidate_leaf =
      Asset_registry.Asset_record.commitment_var candidate
    in
    let* old_root =
      Asset_registry.implied_root_var
        ~leaf:(constant F.typ Field.zero)
        ~index:old_state.leaf_count registration.append_path
    in
    let* appended_root =
      Asset_registry.implied_root_var ~leaf:candidate_leaf
        ~index:old_state.leaf_count registration.append_path
    in
    let* expected_root =
      Field.Checked.if_ active ~then_:appended_root ~else_:old_state.root
    in
    let* () =
      assert_equal ~label:"registry append root transition" F.typ new_state.root
        expected_root
    in
    let* () =
      assert_equal_if ~label:"registry empty append slot" active F.typ
        old_state.root old_root
    in
    let* () =
      assert_equal_if ~label:"registry append index" active Checked32.typ
        candidate.registry_index old_state.leaf_count
    in
    let* () =
      assert_equal_if ~label:"registry append schema" active Checked32.typ
        candidate.schema_version
        (Checked32.Checked.constant
           Inputs.ethereum_asset_registry_schema_version )
    in
    let* () =
      assert_equal_if ~label:"registry append MFT standard ID" active F.typ
        candidate.mft_standard_vk_id
        (constant F.typ Inputs.ethereum_asset_approved_mft_standard_vk_id)
    in
    let* () =
      assert_equal_if ~label:"registry append universal bridge ID" active F.typ
        candidate.universal_bridge_vk_id
        (constant F.typ Inputs.ethereum_asset_universal_bridge_vk_id)
    in
    let* () =
      assert_equal_if ~label:"registry append shared vault" active PC.typ
        candidate.vault_public_key
        (constant PC.typ Inputs.ethereum_asset_vault_public_key)
    in
    let owner_id =
      Account_id.Checked.create candidate.token_owner_l2
        (constant Token_id.typ Token_id.default)
    in
    let* derived_token_id =
      make_checked (fun () ->
          Account_id.Checked.derive_token_id ~owner:owner_id )
    in
    let* () =
      assert_equal_if ~label:"registry append derived token ID" active
        Token_id.typ candidate.token_id_l2 derived_token_id
    in
    let* () =
      authenticate_account_if ~label:"registered token owner ledger opening"
        ~ledger_root:target_ledger ~active registration.token_owner_acc
        registration.token_owner_path
    in
    let* () =
      assert_equal_if ~label:"registered token owner public key" active PC.typ
        registration.token_owner_acc.public_key candidate.token_owner_l2
    in
    let* () =
      assert_equal_if ~label:"registered token owner token ID" active
        Token_id.typ registration.token_owner_acc.token_id
        (constant Token_id.typ Token_id.default)
    in
    let* () =
      assert_equal_if ~label:"registered token owner permissions" active
        Permissions.typ registration.token_owner_acc.permissions
        (constant Permissions.typ expected_token_owner_permissions)
    in
    let* token_owner_zkapp = get_zkapp registration.token_owner_acc in
    let* () =
      assert_vk_hash_if ~label:"registered token owner VK" active
        token_owner_zkapp Inputs.ethereum_asset_approved_mft_token_vk_hash
    in
    let (owner_decimals :: admin_x :: admin_is_odd_field :: paused :: _) =
      token_owner_zkapp.app_state
    in
    let* () =
      assert_equal_if ~label:"registered token decimals" active F.typ
        owner_decimals
        (Checked32.Checked.to_field candidate.decimals)
    in
    let* admin_is_odd = Boolean.of_field admin_is_odd_field in
    let admin_public_key : PC.var = { x = admin_x; is_odd = admin_is_odd } in
    let* () =
      assert_equal_if ~label:"registered token is unpaused" active F.typ paused
        (constant F.typ Field.zero)
    in
    let* () =
      authenticate_account_if ~label:"registered token admin ledger opening"
        ~ledger_root:target_ledger ~active registration.admin_acc
        registration.admin_path
    in
    let* () =
      assert_equal_if ~label:"registered token admin public key" active PC.typ
        registration.admin_acc.public_key admin_public_key
    in
    let* () =
      assert_equal_if ~label:"registered token admin token ID" active
        Token_id.typ registration.admin_acc.token_id
        (constant Token_id.typ Token_id.default)
    in
    let* () =
      assert_equal_if ~label:"registered token admin permissions" active
        Permissions.typ registration.admin_acc.permissions
        (constant Permissions.typ expected_token_admin_permissions)
    in
    let* admin_zkapp = get_zkapp registration.admin_acc in
    let* () =
      assert_vk_hash_if ~label:"registered token admin VK" active admin_zkapp
        Inputs.ethereum_asset_approved_mft_admin_vk_hash
    in
    let (authority_x :: authority_is_odd_field :: _) = admin_zkapp.app_state in
    let* authority_is_odd = Boolean.of_field authority_is_odd_field in
    let admin_authority : PC.var =
      { x = authority_x; is_odd = authority_is_odd }
    in
    let* () =
      assert_equal_if ~label:"registered token admin authority revoked" active
        PC.typ admin_authority (constant PC.typ PC.empty)
    in
    let* () =
      authenticate_account_if ~label:"registered vault ledger opening"
        ~ledger_root:target_ledger ~active registration.vault_acc
        registration.vault_path
    in
    let* () =
      assert_equal_if ~label:"registered vault public key" active PC.typ
        registration.vault_acc.public_key candidate.vault_public_key
    in
    let* () =
      assert_equal_if ~label:"registered vault token ID" active Token_id.typ
        registration.vault_acc.token_id derived_token_id
    in
    let* () =
      assert_equal_if ~label:"registered vault inventory" active
        Currency.Amount.typ
        (Currency.Balance.Checked.to_amount registration.vault_acc.balance)
        candidate.inventory_cap
    in
    let* () =
      assert_equal_if ~label:"registered vault permissions" active
        Permissions.typ registration.vault_acc.permissions
        (constant Permissions.typ expected_vault_permissions)
    in
    let* vault_zkapp = get_zkapp registration.vault_acc in
    let* () =
      assert_vk_hash_if ~label:"registered vault VK" active vault_zkapp
        Inputs.ethereum_asset_universal_bridge_vk_hash
    in
    let* () =
      authenticate_account_if ~label:"registered circulation ledger opening"
        ~ledger_root:target_ledger ~active registration.circulation_acc
        registration.circulation_path
    in
    let* () =
      assert_equal_if ~label:"registered circulation public key" active PC.typ
        registration.circulation_acc.public_key candidate.token_owner_l2
    in
    let* () =
      assert_equal_if ~label:"registered circulation token ID" active
        Token_id.typ registration.circulation_acc.token_id derived_token_id
    in
    let* () =
      assert_equal_if ~label:"registered circulation supply" active
        Currency.Amount.typ
        (Currency.Balance.Checked.to_amount registration.circulation_acc.balance)
        candidate.inventory_cap
    in
    assert_equal_if ~label:"registered circulation permissions" active
      Permissions.typ registration.circulation_acc.permissions
      (constant Permissions.typ expected_circulation_permissions)

  type da_mode = Multisig | Emergency of Emergency_da_folder.Stmt.var

  let main ?(check_sequencer_precondition = true) ~da_mode
      (w : Base_witness.var) (txn_stmt : Txn_state.Zeko_stmt.var)
      ((ase_outer, ase_inner) :
        Ase_outer_inst.Stmt.var * Ase_inner_inst.Stmt.var ) =
    with_label __LOC__
    @@ fun () ->
    let ({ public_key
         ; vk_hash
         ; old_inner_acc
         ; old_inner_acc_path
         ; new_inner_acc
         ; new_inner_acc_path
         ; da_multisig
         ; slot_range
         ; emergency_mode
         ; old_ethereum_asset_registry_acc
         ; old_ethereum_asset_registry_path
         ; new_ethereum_asset_registry_acc
         ; new_ethereum_asset_registry_path
         ; ethereum_asset_registration
         }
          : Base_witness.var ) =
      w
    in
    let* status_flags_precondition =
      Outer_state.Status_flags.of_bools_var ~paused:Boolean.false_
        ~emergency:emergency_mode
    in
    with_label __LOC__
    @@ fun () ->
    (* Calculate the root ledger hashes, to be checked against txn snark. *)
    let* implied_root_old = implied_root old_inner_acc old_inner_acc_path in
    let* implied_root_new = implied_root new_inner_acc new_inner_acc_path in

    let ({ source_ledger
         ; target_ledger
         ; source_local_state
         ; target_local_state
         ; sequencer
         ; accumulated_fees
         ; slot_range = txn_snark_slot_range
         ; global_slot_range
         ; source_acc_set
         ; target_acc_set
         }
          : Txn_state.Zeko_stmt.var ) =
      txn_stmt
    in
    let* ethereum_asset_registry_state =
      match Inputs.ethereum_asset_registry_public_key with
      | None ->
          Checked.return None
      | Some registry_public_key ->
          let* old_registry_root =
            implied_registry_root old_ethereum_asset_registry_acc
              old_ethereum_asset_registry_path
          in
          let* new_registry_root =
            implied_registry_root new_ethereum_asset_registry_acc
              new_ethereum_asset_registry_path
          in
          let* () =
            assert_equal ~label:"source registry account ledger opening"
              Ledger_hash.typ source_ledger
              (Ledger_hash.var_of_hash_packed old_registry_root)
          in
          let* () =
            assert_equal ~label:"target registry account ledger opening"
              Ledger_hash.typ target_ledger
              (Ledger_hash.var_of_hash_packed new_registry_root)
          in
          let* old_state =
            registry_state_of_account ~registry_public_key
              old_ethereum_asset_registry_acc
          in
          let* new_state =
            registry_state_of_account ~registry_public_key
              new_ethereum_asset_registry_acc
          in
          let* () =
            validate_registration_accounts ~target_ledger ~old_state ~new_state
              ethereum_asset_registration
          in
          Checked.return (Some new_state)
    in
    with_label __LOC__
    @@ fun () ->
    (* The local states must be empty, ensuring that there is no incomplete zkapp transaction being committed. *)
    let* () =
      Txn_state.Local_state.(
        assert_equal ~label:__LOC__ typ source_local_state dummy)
    in
    let* () =
      Txn_state.Local_state.(
        assert_equal ~label:__LOC__ typ target_local_state dummy)
    in

    let* da_key_opt =
      match da_mode with
      | Multisig ->
          (* DA check, simply see if public key in question has signed our ledger. *)
          let* () =
            with_label __LOC__
            @@ fun () ->
            let input =
              let open Random_oracle.Input.Chunked in
              append
                (Ledger_hash.var_to_field target_ledger |> field)
                (Account_set.to_input_var target_acc_set)
            in
            let* payload =
              make_checked (fun () ->
                  Random_oracle.Checked.hash
                    ~init:
                      (Hash_prefix_create.salt
                         Zeko_constants.da_layer_check_salt )
                    (Random_oracle.Checked.pack_input input) )
            in
            Multisig.check ~signature_kind:chain_l2 da_multisig payload
          in
          let*| da_key = Multisig.of_witness_var da_multisig in
          Some da_key
      | Emergency emergency_da_stmt ->
          let* () =
            assert_equal ~label:__LOC__ Ledger_hash.typ
              emergency_da_stmt.source_ledger source_ledger
          in
          let* () =
            assert_equal ~label:__LOC__ Ledger_hash.typ
              emergency_da_stmt.target_ledger target_ledger
          in
          let* () =
            assert_equal ~label:__LOC__ Account_set.typ
              emergency_da_stmt.source_acc_set source_acc_set
          in
          let* () =
            assert_equal ~label:__LOC__ Account_set.typ
              emergency_da_stmt.target_acc_set target_acc_set
          in
          Checked.return None
    in

    (* Sequencer must take fees. A non-zero magnitude would
       either mean printing or burning L2 MINA. *)
    let* () =
      Currency.Amount.(
        Signed.Checked.magnitude accumulated_fees
        >>= assert_equal ~label:__LOC__ typ (constant typ zero))
    in

    (* We check that the valid while isn't too big. Do note that the slot_range is inclusive surprisingly,
       so there should be no one-off bug below. In the case where lower and upper are equal,
       max_valid_while_size must be at least 1. *)
    let* () =
      assert_var __LOC__ (fun () ->
          let* diff = Slot.Checked.diff slot_range.upper slot_range.lower in
          Mina_numbers.Global_slot_span.Checked.(
            diff
            < constant
                (Global_slot_span (Unsigned.UInt32.of_int max_valid_while_size))) )
    in

    (* Our slot range must be a subset of the txn snark slot range. *)
    let* () =
      assert_var __LOC__
      @@ fun () -> Slot.Checked.(slot_range.lower >= txn_snark_slot_range.lower)
    in
    let* () =
      assert_var __LOC__
      @@ fun () -> Slot.Checked.(slot_range.upper <= txn_snark_slot_range.upper)
    in

    (* We check that the paths provided for the inner account are correct. *)
    let* source_ledger =
      assert_equal_safer ~label:__LOC__ Ledger_hash.typ source_ledger
        (Ledger_hash.var_of_hash_packed implied_root_old)
    in
    let* target_ledger =
      assert_equal_safer ~label:__LOC__ Ledger_hash.typ target_ledger
        (Ledger_hash.var_of_hash_packed implied_root_new)
    in
    with_label __LOC__
    @@ fun () ->
    (* We check that we're dealing with the correct account. *)
    let* () =
      with_label __LOC__ (fun () ->
          PC.Checked.Assert.equal old_inner_acc.public_key
            (constant PC.typ inner_public_key) )
    in
    (* We repeat the above check for the new account. *)
    let* () =
      with_label __LOC__ (fun () ->
          PC.Checked.Assert.equal new_inner_acc.public_key
          @@ constant PC.typ inner_public_key )
    in

    (* Extract the zkapp portion of the accounts. *)
    let* old_inner_zkapp = get_zkapp old_inner_acc in
    let* new_inner_zkapp = get_zkapp new_inner_acc in

    (* Extract the outer action state as synchronized to the new inner account. *)
    let synchronized_outer_action_state =
      (Inner_state.var_of_app_state new_inner_zkapp.app_state)
        .outer_action_state
    in

    let Ase_outer_inst.Stmt.
          { source = synchronized_outer_action_state'
          ; target = outer_action_state
          } =
      ase_outer
    in

    let Ase_inner_inst.Stmt.
          { source = old_inner_action_state; target = new_inner_action_state } =
      ase_inner
    in

    (* The sequencer doesn't have to synchronize all actions immediately.
       They can delay it by an arbitrary amount.
       This checks that the source of the ase_outer proof is equal to the
       synchronized outer action state.
       We don't check the lengths here, since it isn't important for this purpose.
    *)
    let* synchronized_outer_action_state =
      let*| () =
        assert_equal ~label:__LOC__ Outer_action_state.typ
          (Outer_action_state.With_length.state_var
             synchronized_outer_action_state )
          synchronized_outer_action_state'
      in
      synchronized_outer_action_state
    in

    (* Extract the inner action states. *)
    let old_inner_action_state' =
      match old_inner_zkapp.action_state with
      | x :: _ ->
          Inner_action_state.unsafe_var_of_field x
    in
    let new_inner_action_state' =
      match new_inner_zkapp.action_state with
      | x :: _ ->
          Inner_action_state.unsafe_var_of_field x
    in
    (* We check that the above values match with what we got from ase_inner. *)
    let* old_inner_action_state =
      let*| () =
        assert_equal ~label:__LOC__ Inner_action_state.typ
          (Inner_action_state.With_length.state_var old_inner_action_state)
          old_inner_action_state'
      in
      old_inner_action_state
    in
    let* new_inner_action_state =
      let*| () =
        assert_equal ~label:__LOC__ Inner_action_state.typ
          (Inner_action_state.With_length.state_var new_inner_action_state)
          new_inner_action_state'
      in
      new_inner_action_state
    in

    let* status_flags_update =
      match da_mode with
      | Multisig ->
          Outer_state.Status_flags.of_bools_var ~paused:Boolean.false_
            ~emergency:Boolean.false_
      | Emergency _ ->
          Outer_state.Status_flags.of_bools_var ~paused:Boolean.false_
            ~emergency:Boolean.true_
    in

    (* Finalize update  *)
    let update =
      { default_account_update.update with
        app_state =
          Outer_state.fine
            { ledger_hash = Some target_ledger
            ; inner_action_state =
                { state =
                    Some
                      (Inner_action_state.With_length.state_var
                         new_inner_action_state )
                ; length =
                    Some
                      (Inner_action_state.With_length.length_var
                         new_inner_action_state )
                }
                (* The inner action state as recorded now.
                   Other zkapps can match on this, or deduce it
                   directly from `ledger_hash`. There is no real difference
                   currently.
                   However, we also store the length here, which is of importance
                   to many other zkapps.
                *)
            ; sequencer = None (* We don't update the sequencer. *)
            ; status_flags = Some status_flags_update
            ; pause_key = None (* We don't update the pause key. *)
            ; da_key = None
            ; acc_set = Some target_acc_set
            }
          |> var_to_app_state_fine
      }
    in
    let preconditions =
      { Account_update.Preconditions.Checked.account =
          { default_account_update.preconditions.account with
            state =
              Outer_state.fine
                { ledger_hash =
                    Some source_ledger
                    (* The original state of the rollup ledger. *)
                ; inner_action_state =
                    { state =
                        Some
                          (Inner_action_state.With_length.state_var
                             old_inner_action_state )
                    ; length =
                        Some
                          (Inner_action_state.With_length.length_var
                             old_inner_action_state )
                    }
                    (* The inner action state as recorded before.
                       The state we already know from the ledger hash,
                       but the length is information we didn't have before.
                    *)
                ; sequencer =
                    (* We must be the sequencer. *)
                    ( if check_sequencer_precondition then Some sequencer
                    else None )
                ; status_flags =
                    Some status_flags_precondition (* We must not be paused. *)
                ; pause_key =
                    None (* We don't care about who can pause the rollup. *)
                ; da_key = da_key_opt
                ; acc_set = Some source_acc_set
                }
              |> var_to_precondition_fine
          ; action_state =
              Zkapp_basic.Or_ignore.Checked.make_unsafe Boolean.true_
                (Outer_action_state.raw_var outer_action_state)
              (* Our action state must match *)
          }
      ; valid_while = Slot_range.Checked.to_valid_while slot_range
      ; network =
          { default_account_update.preconditions.network with
            global_slot_since_genesis =
              Slot_range.Checked.to_valid_while global_slot_range
          }
      }
    in
    (* We submit an action that summarizes what we did. Used as a way to timestamp when actions were synchronized. *)
    let* actions =
      Outer_action.commit_to_actions_var
        Outer_action.Commit.
          { ledger = target_ledger
          ; inner_action_state = new_inner_action_state
          ; synchronized_outer_action_state
          ; slot_range
          }
    in
    (* Our account update is assembled, specifying our state update, our preconditions, our pk, and our authorization *)
    let account_update =
      { default_account_update with
        public_key
      ; actions
      ; authorization_kind = authorization_vk_hash vk_hash
      ; update
      ; preconditions
      }
    in

    (* Ethereum settlement needs the L2 registry checkpoint in the Pickles-bound
       call forest, but Mina L1 has no registry account whose state can satisfy
       an executable precondition. Bind the checkpoint into the signed
       sequencer child's inert call data instead. *)
    let* ethereum_asset_registry_call_data =
      match
        ( Inputs.ethereum_asset_registry_public_key
        , ethereum_asset_registry_state )
      with
      | None, None ->
          Checked.return (constant F.typ Field.zero)
      | Some registry_public_key, Some registry_state ->
          Asset_registry.Checkpoint.commitment_var ~registry_public_key
            registry_state
      | _ ->
          failwith "inconsistent Ethereum asset registry circuit configuration"
    in
    let sequencer_account_update =
      { default_account_update with
        public_key = Even_PC.to_pc_var sequencer
      ; call_data = ethereum_asset_registry_call_data
      ; authorization_kind = authorization_signed ()
      ; use_full_commitment = Boolean.true_
      }
    in
    let emergency_da_account_update_opt =
      match da_mode with
      | Emergency emergency_da_stmt ->
          let preconditions =
            { default_account_update.preconditions with
              account =
                { default_account_update.preconditions.account with
                  action_state =
                    Zkapp_basic.Or_ignore.Checked.make_unsafe Boolean.true_
                      emergency_da_stmt.target_action_state
                }
            }
          in
          Some
            { default_account_update with
              public_key = constant PC.typ emergency_da_public_key
            ; preconditions
            }
      | Multisig ->
          None
    in

    (* Assemble some stuff to help the prover and calculate public output *)
    let calls : Calls.t =
      match emergency_da_account_update_opt with
      | None ->
          [ (sequencer_account_update, []) ]
      | Some emergency_da ->
          [ (sequencer_account_update, []); (emergency_da, []) ]
    in
    let*| out = make_outputs ~chain:chain_l1 account_update calls in
    out

  let rule : _ Compile_simple.branch lazy_t =
    lazy
      { branch_name = "Rollup step"
      ; tags =
          Two_tags (Lazy.force Txn_rules.tag, Lazy.force Verify_both_ases.tag)
      ; main =
          (fun (w : Witness.t V.t) ->
            let* Witness.{ txn_snark; base_witness; verify_both_ases } =
              exists ~compute:(V.get w) Witness.typ
            in
            let* txn_stmt, verify_txn_snark = Txn_rules.get txn_snark in
            let* (ase_outer, ase_inner), verify_both_ases =
              Verify_both_ases.get verify_both_ases
            in
            let*| out =
              main ~da_mode:Multisig base_witness txn_stmt (ase_outer, ase_inner)
            in
            Compile_simple.
              { prevs = Two_prevs (verify_txn_snark, verify_both_ases); out } )
      }

  (** Specialized version of the main function, used for emergency commits.
      Only usable after [max_sequencer_inactivity] slots of the sequencer not committing. *)
  module Emergency_commit = struct
    module Witness = struct
      type t =
        { base_witness : Base_witness.t
        ; before_last_commit : Rollup_state.Outer_action_state.t
        ; last_commit : Rollup_state.Outer_action.Commit.t
        ; verify_emergency_folders : Verify_emergency_folders.t
        ; verify_base : Verify_base.t
        }
      [@@deriving snarky]
    end

    let rule : _ Compile_simple.branch lazy_t =
      lazy
        { branch_name = "Emergency step"
        ; tags =
            Two_tags
              ( Lazy.force Verify_base.tag
              , Lazy.force Verify_emergency_folders.tag )
        ; main =
            (fun (w : Witness.t V.t) ->
              let* Witness.
                     { base_witness
                     ; before_last_commit
                     ; last_commit
                     ; verify_emergency_folders
                     ; verify_base
                     } =
                exists ~compute:(V.get w) Witness.typ
              in
              let* (txn_stmt, (ase_outer, ase_inner)), verify_base =
                Verify_base.get verify_base
              in
              let* (count_commits, emergency_da_stmt), verify_emergency_folders
                  =
                Verify_emergency_folders.get verify_emergency_folders
              in

              (* Check that at least [max_sequencer_inactivity] slots have passed between last_commit and and this new commit *)
              let* () =
                assert_var __LOC__ (fun () ->
                    let* diff =
                      Slot.Checked.diff base_witness.slot_range.lower
                        last_commit.slot_range.upper
                    in
                    let* inactivity_ok =
                      Mina_numbers.Global_slot_span.Checked.(
                        diff
                        >= constant
                             (Global_slot_span
                                (Unsigned.UInt32.of_int max_sequencer_inactivity)
                             ))
                    in
                    let* ok =
                      if_ base_witness.emergency_mode ~typ:Boolean.typ
                        ~then_:Boolean.true_ ~else_:inactivity_ok
                    in
                    Checked.return ok )
              in

              let Count_commits.Definition.Stmt.
                    { source_action_state; target_action_state; n_commits } =
                count_commits
              in
              (* Apply last_commit to before_last_commit *)
              let* after_last_commit =
                Outer_action.push_commit_var last_commit before_last_commit
              in
              (* Check that counting started immediately after last_commit *)
              let* () =
                assert_equal ~label:__LOC__ Outer_action_state.typ
                  after_last_commit source_action_state
              in
              (* Check that target_action_state is the target of ase_outer, i.e. the outer action state precondition *)
              let* () =
                assert_equal ~label:__LOC__ Outer_action_state.typ
                  target_action_state ase_outer.target
              in
              (* Check that there has been no commits since last_commit *)
              let* () =
                assert_equal ~label:__LOC__ Checked32.typ n_commits
                  Checked32.Checked.zero
              in

              let*| out =
                main ~check_sequencer_precondition:false
                  ~da_mode:(Emergency emergency_da_stmt) base_witness txn_stmt
                  (ase_outer, ase_inner)
              in
              Compile_simple.
                { prevs = Two_prevs (verify_base, verify_emergency_folders)
                ; out
                } )
        }
  end
end
