open Core_kernel
open Mina_base
open Snark_params.Tick
open Zeko_util
module PC = Signature_lib.Public_key.Compressed

module Token_id = struct
  include Token_id

  type var = Checked.t
end

module Asset_record = struct
  type t =
    { schema_version : Checked32.t
    ; registry_index : Checked32.t
    ; asset_id_high : F.t
    ; asset_id_low : F.t
    ; ethereum_token_address : F.t
    ; token_owner_l2 : PC.t
    ; token_id_l2 : Token_id.t
    ; decimals : Checked32.t
    ; inventory_cap : Currency.Amount.t
    ; mft_standard_vk_id : F.t
    ; vault_public_key : PC.t
    ; universal_bridge_vk_id : F.t
    }
  [@@deriving snarky, yojson, equal]

  let fields (record : t) =
    let (Typ typ) = typ in
    typ.value_to_fields record |> fst

  let commitment record =
    Random_oracle.hash
      ~init:
        (Hash_prefix_create.salt
           Zeko_constants.ethereum_asset_registry_leaf_salt )
      (fields record)

  let commitment_var record =
    var_to_hash ~init:Zeko_constants.ethereum_asset_registry_leaf_salt typ
      record

  let derived_token_id ({ token_owner_l2; _ } : t) =
    let owner = Account_id.create token_owner_l2 Token_id.default in
    Account_id.derive_token_id ~owner
end

module Registry_state = struct
  type t =
    { root : F.t; leaf_count : Checked32.t; schema_version : Checked32.t }
  [@@deriving snarky, yojson, equal]

  type fine =
    { root : F.var option
    ; leaf_count : Checked32.var option
    ; schema_version : Checked32.var option
    }

  let _ = function
    | ({ root = _; leaf_count = _; schema_version = _ } : t) ->
        ()

  let fine ({ root; leaf_count; schema_version } : fine) : Fine.t =
    [ Whole (F.typ, root)
    ; Whole (Checked32.typ, leaf_count)
    ; Whole (Checked32.typ, schema_version)
    ]

  let value_of_app_state
      (root :: leaf_count :: schema_version :: _ : F.t Zkapp_state.V.t) : t =
    { root
    ; leaf_count = Field.to_string leaf_count |> Checked32.of_string
    ; schema_version = Field.to_string schema_version |> Checked32.of_string
    }
end

module Path = struct
  include
    SnarkList
      (F)
      (struct
        let length = Zeko_constants.Ethereum_asset_registry.depth
      end)

  let to_yojson path = `List (List.map path ~f:Field.to_yojson)

  let of_yojson = function
    | `List values
      when List.length values = Zeko_constants.Ethereum_asset_registry.depth ->
        List.fold_right values ~init:(Ok []) ~f:(fun json acc ->
            match (Field.of_yojson json, acc) with
            | Ok value, Ok rest ->
                Ok (value :: rest)
            | Error error, _ | _, Error error ->
                Error error )
    | `List _ ->
        Error "asset registry path has the wrong depth"
    | _ ->
        Error "asset registry path must be a JSON list"
end

module Output = struct
  type calls =
    ( Account_update.t
    , Zkapp_command.Digest.Account_update.t
    , Zkapp_command.Digest.Forest.t )
    Zkapp_command.Call_forest.t

  type auxiliary =
    Account_update.Body.t * Zkapp_command.Digest.Account_update.t * calls

  type t = Zkapp_statement.t * auxiliary
end

let merge left right =
  Random_oracle.hash
    ~init:
      (Hash_prefix_create.salt Zeko_constants.ethereum_asset_registry_node_salt)
    [| left; right |]

let merge_var left right =
  var_to_hash ~init:Zeko_constants.ethereum_asset_registry_node_salt
    Typ.(F.typ * F.typ)
    (left, right)

let path_bits_var index =
  Field.Checked.choose_preimage_var
    (Checked32.Checked.to_field index)
    ~length:Zeko_constants.Ethereum_asset_registry.depth

let implied_root_var ~leaf ~index (path : Path.var) =
  let* bits = path_bits_var index in
  foldl (List.zip_exn path bits) ~init:leaf ~f:(fun acc (sibling, is_right) ->
      let* left = if_ is_right ~typ:F.typ ~then_:sibling ~else_:acc in
      let* right = if_ is_right ~typ:F.typ ~then_:acc ~else_:sibling in
      merge_var left right )

let implied_root ~leaf ~index (path : Path.t) =
  let rec go acc level = function
    | [] ->
        acc
    | sibling :: rest ->
        let is_right = Int.(index land (1 lsl level) <> 0) in
        let acc = if is_right then merge sibling acc else merge acc sibling in
        go acc (level + 1) rest
  in
  go leaf 0 path

module Merkle_list = struct
  type t = { leaves : F.t array; count : int }

  let capacity = Zeko_constants.Ethereum_asset_registry.max_assets

  let empty () = { leaves = Array.create ~len:capacity Field.zero; count = 0 }

  let count t = t.count

  let next_level level =
    Array.init
      (Array.length level / 2)
      ~f:(fun i -> merge level.(2 * i) level.((2 * i) + 1))

  let levels t =
    let rec go acc level =
      if Array.length level = 1 then List.rev (level :: acc)
      else go (level :: acc) (next_level level)
    in
    go [] t.leaves

  let root t =
    match List.last (levels t) with
    | Some root ->
        root.(0)
    | None ->
        assert false

  let empty_root = root (empty ())

  let path t ~index =
    if index < 0 || index >= capacity then
      invalid_arg "asset registry path index is outside the tree" ;
    List.drop_last_exn (levels t)
    |> List.mapi ~f:(fun level nodes ->
           let node_index = index lsr level in
           nodes.(node_index lxor 1) )

  let append_exn t (record : Asset_record.t) =
    let index = Checked32.to_int record.registry_index in
    if index <> t.count then
      failwithf "asset registry append index %d does not match leaf count %d"
        index t.count () ;
    if t.count >= capacity then failwith "asset registry is full" ;
    if not (Field.equal t.leaves.(index) Field.zero) then
      failwith "asset registry append slot is not empty" ;
    let leaves = Array.copy t.leaves in
    leaves.(index) <- Asset_record.commitment record ;
    { leaves; count = t.count + 1 }

  let verify ~root (record : Asset_record.t) (path : Path.t) =
    let index = Checked32.to_int record.registry_index in
    List.length path = Zeko_constants.Ethereum_asset_registry.depth
    && index < capacity
    && Field.equal
         (implied_root ~leaf:(Asset_record.commitment record) ~index path)
         root
end

module type CONFIG = sig
  val registry_public_key : PC.t

  val registration_authority : PC.t

  val schema_version : Checked32.t

  val approved_mft_standard_vk_id : F.t

  val universal_bridge_vk_id : F.t

  val vault_public_key : PC.t

  val chain_l2 : Mina_signature_kind.t
end

module Make (Config : CONFIG) () = struct
  let assert_implies condition value =
    let open Boolean.Expr in
    ((not !condition) || !value) |> assert_

  let assert_equal_if ~label condition typ left right =
    let* equal = var_equal typ left right in
    with_label label (fun () ->
        let open Boolean.Expr in
        ((not !condition) || equal) |> assert_ )

  let assert_not_equal_if ~label condition typ left right =
    let* equal = var_equal typ left right in
    with_label label (fun () ->
        let open Boolean.Expr in
        ((not !condition) || not equal) |> assert_ )

  let assert_fits_bits ~label ~length value =
    with_label label (fun () ->
        let* (_ : Boolean.var list) =
          Field.Checked.choose_preimage_var value ~length
        in
        Checked.return () )

  let validate_record ?(check = Boolean.true_) (record : Asset_record.var) =
    let* () =
      assert_equal_if ~label:"asset registry schema version" check Checked32.typ
        record.schema_version
        (Checked32.Checked.constant Config.schema_version)
    in
    let* index_in_range =
      Checked32.Checked.(
        record.registry_index
        < constant
            (Checked32.of_int Zeko_constants.Ethereum_asset_registry.max_assets))
    in
    let* () = assert_implies check index_in_range in
    let* () =
      assert_equal_if ~label:"asset registry MFT standard version" check F.typ
        record.mft_standard_vk_id
        (constant F.typ Config.approved_mft_standard_vk_id)
    in
    let* () =
      assert_equal_if ~label:"asset registry shared vault" check PC.typ
        record.vault_public_key
        (constant PC.typ Config.vault_public_key)
    in
    let* () =
      assert_not_equal_if ~label:"asset registry owner/vault separation" check
        PC.typ record.token_owner_l2 record.vault_public_key
    in
    let owner =
      Account_id.Checked.create record.token_owner_l2
        (constant Token_id.typ Token_id.default)
    in
    let* derived_token_id =
      make_checked (fun () -> Account_id.Checked.derive_token_id ~owner)
    in
    let* () =
      assert_equal_if ~label:"asset registry derived token ID" check
        Token_id.typ record.token_id_l2 derived_token_id
    in
    let* () =
      assert_fits_bits ~label:"asset registry decimals" ~length:8
        (Checked32.Checked.to_field record.decimals)
    in
    let* () =
      let* cap_is_zero =
        Currency.Amount.Checked.equal record.inventory_cap
          (constant Currency.Amount.typ Currency.Amount.zero)
      in
      assert_implies check Boolean.(not cap_is_zero)
    in
    let* () =
      assert_fits_bits ~label:"asset registry asset ID high limb" ~length:128
        record.asset_id_high
    in
    let* () =
      assert_fits_bits ~label:"asset registry asset ID low limb" ~length:128
        record.asset_id_low
    in
    let* () =
      assert_fits_bits ~label:"asset registry Ethereum token address"
        ~length:160 record.ethereum_token_address
    in
    let* () =
      let* address_is_zero =
        Field.Checked.equal record.ethereum_token_address
          (constant F.typ Field.zero)
      in
      assert_implies check Boolean.(not address_is_zero)
    in
    let* () =
      let* high_zero =
        Field.Checked.equal record.asset_id_high (constant F.typ Field.zero)
      in
      let* low_zero =
        Field.Checked.equal record.asset_id_low (constant F.typ Field.zero)
      in
      let* both_zero = Boolean.(high_zero && low_zero) in
      assert_implies check (Boolean.not both_zero)
    in
    let* () =
      assert_equal_if ~label:"asset registry universal bridge version" check
        F.typ record.universal_bridge_vk_id
        (constant F.typ Config.universal_bridge_vk_id)
    in
    Checked.return derived_token_id

  module Membership_witness = struct
    type t =
      { state : Registry_state.t; record : Asset_record.t; path : Path.t }
    [@@deriving snarky, yojson]
  end

  let registry_precondition (state : Registry_state.var) =
    { default_account_update with
      public_key = constant PC.typ Config.registry_public_key
    ; authorization_kind =
        constant Account_update.Authorization_kind.typ None_given
    ; preconditions =
        { default_account_update.preconditions with
          account =
            { default_account_update.preconditions.account with
              state =
                Registry_state.fine
                  { root = Some state.root
                  ; leaf_count = Some state.leaf_count
                  ; schema_version = Some state.schema_version
                  }
                |> var_to_precondition_fine
            }
        }
    }

  module Verified_asset = struct
    type t =
      { record : Asset_record.var
      ; token_id : Token_id.Checked.t
      ; authenticated_registry_call : Account_update.Checked.t
      }

    let record t = t.record

    let token_id t = t.token_id

    let authenticated_registry_call t = t.authenticated_registry_call
  end

  let verify ({ state; record; path } : Membership_witness.var) =
    let* () =
      assert_equal ~label:"asset registry state schema" Checked32.typ
        state.schema_version
        (Checked32.Checked.constant Config.schema_version)
    in
    let* token_id = validate_record record in
    let* record_before_count =
      Checked32.Checked.(record.registry_index < state.leaf_count)
    in
    let* () = Boolean.Assert.is_true record_before_count in
    let* leaf = Asset_record.commitment_var record in
    let* implied_root =
      implied_root_var ~leaf ~index:record.registry_index path
    in
    let* () =
      assert_equal ~label:"asset registry membership root" F.typ state.root
        implied_root
    in
    Checked.return
      { Verified_asset.record
      ; token_id
      ; authenticated_registry_call = registry_precondition state
      }

  module Scan_definition = struct
    module Stmt = struct
      type t =
        { old_root : F.t
        ; leaf_count : Checked32.t
        ; candidate : Asset_record.t
        ; next_expected_index : Checked32.t
        ; traversed_count : Checked32.t
        }
      [@@deriving snarky]
    end

    module Elem = struct
      type t = { active : Boolean.t; record : Asset_record.t; path : Path.t }
      [@@deriving snarky]
    end

    module Init = struct
      type t = { old_state : Registry_state.t; candidate : Asset_record.t }
      [@@deriving snarky]
    end

    let init ~check ({ old_state; candidate } : Init.var) =
      let check = Option.value check ~default:Boolean.true_ in
      let* () =
        assert_equal_if ~label:"asset registry scan schema" check Checked32.typ
          old_state.schema_version
          (Checked32.Checked.constant Config.schema_version)
      in
      Checked.return
        { Stmt.old_root = old_state.root
        ; leaf_count = old_state.leaf_count
        ; candidate
        ; next_expected_index = Checked32.Checked.zero
        ; traversed_count = Checked32.Checked.zero
        }

    let step ({ active; record; path } : Elem.var) (state : Stmt.var) =
      let* () =
        assert_equal_if ~label:"asset registry scan index" active Checked32.typ
          record.registry_index state.next_expected_index
      in
      let* within_committed_list =
        Checked32.Checked.(state.next_expected_index < state.leaf_count)
      in
      let* () = assert_implies active within_committed_list in
      let* leaf = Asset_record.commitment_var record in
      let* root =
        implied_root_var ~leaf ~index:state.next_expected_index path
      in
      let* () =
        assert_equal_if ~label:"asset registry scan membership" active F.typ
          root state.old_root
      in
      let* () =
        assert_not_equal_if ~label:"unique Ethereum token address" active F.typ
          record.ethereum_token_address state.candidate.ethereum_token_address
      in
      let* asset_high_equal =
        Field.Checked.equal record.asset_id_high state.candidate.asset_id_high
      in
      let* asset_low_equal =
        Field.Checked.equal record.asset_id_low state.candidate.asset_id_low
      in
      let* () =
        let* both_equal = Boolean.(asset_high_equal && asset_low_equal) in
        assert_implies active (Boolean.not both_equal)
      in
      let* () =
        assert_not_equal_if ~label:"unique L2 token owner" active PC.typ
          record.token_owner_l2 state.candidate.token_owner_l2
      in
      let* () =
        assert_not_equal_if ~label:"unique derived L2 token ID" active
          Token_id.typ record.token_id_l2 state.candidate.token_id_l2
      in
      let* next_expected_index =
        Checked32.Checked.succ_if state.next_expected_index active
      in
      let* traversed_count =
        Checked32.Checked.succ_if state.traversed_count active
      in
      Checked.return { state with next_expected_index; traversed_count }

    let dummy_elem : Elem.t =
      { active = false
      ; record =
          { schema_version = Config.schema_version
          ; registry_index = Checked32.zero
          ; asset_id_high = Field.zero
          ; asset_id_low = Field.zero
          ; ethereum_token_address = Field.zero
          ; token_owner_l2 = PC.empty
          ; token_id_l2 = Token_id.default
          ; decimals = Checked32.zero
          ; inventory_cap = Currency.Amount.zero
          ; mft_standard_vk_id = Config.approved_mft_standard_vk_id
          ; vault_public_key = Config.vault_public_key
          ; universal_bridge_vk_id = Field.zero
          }
      ; path =
          List.init Zeko_constants.Ethereum_asset_registry.depth ~f:(fun _ ->
              Field.zero )
      }

    let leaf_iterations =
      Zeko_constants.Folder_iterations.Ethereum_asset_registry_scan
      .leaf_iterations

    let leaf_option_iterations =
      Zeko_constants.Folder_iterations.Ethereum_asset_registry_scan
      .leaf_option_iterations

    let extend_iterations =
      Zeko_constants.Folder_iterations.Ethereum_asset_registry_scan
      .extend_iterations

    let extend_option_iterations =
      Zeko_constants.Folder_iterations.Ethereum_asset_registry_scan
      .extend_option_iterations

    let name = "Ethereum asset registry exhaustive scan"

    let wrap_domain = Some `N14
  end

  module Scan = struct
    module Definition = Scan_definition

    include Folder.Make (Definition) ()
  end

  module Scan_inst = Scan.Make (struct
    let get_iterations = 0
  end)

  module Register = struct
    module Witness = struct
      type t =
        { scan : Scan_inst.t; append_path : Path.t; registry_vk_hash : F.t }
      [@@deriving snarky]
    end

    let main (w : Witness.t V.t) =
      let* { scan; append_path; registry_vk_hash } =
        exists Witness.typ ~compute:(V.get w)
      in
      let* `Source source, `Target target, verify_scan =
        Scan_inst.get_full scan
      in
      let* () =
        assert_equal ~label:"asset registry scan candidate" Asset_record.typ
          source.candidate target.candidate
      in
      let* () =
        assert_equal ~label:"asset registry scan old root" F.typ source.old_root
          target.old_root
      in
      let* () =
        assert_equal ~label:"asset registry scan leaf count" Checked32.typ
          source.leaf_count target.leaf_count
      in
      let* () =
        assert_equal ~label:"asset registry exhaustive index" Checked32.typ
          target.next_expected_index target.leaf_count
      in
      let* () =
        assert_equal ~label:"asset registry exhaustive count" Checked32.typ
          target.traversed_count target.leaf_count
      in
      let* derived_token_id = validate_record target.candidate in
      let* () =
        assert_equal ~label:"asset registry registered token ID" Token_id.typ
          target.candidate.token_id_l2 derived_token_id
      in
      let* () =
        assert_equal ~label:"asset registry append index" Checked32.typ
          target.candidate.registry_index target.leaf_count
      in
      let* has_capacity =
        Checked32.Checked.(
          target.leaf_count
          < constant
              (Checked32.of_int
                 Zeko_constants.Ethereum_asset_registry.max_assets ))
      in
      let* () = Boolean.Assert.is_true has_capacity in
      let* old_root_from_empty =
        implied_root_var
          ~leaf:(constant F.typ Field.zero)
          ~index:target.leaf_count append_path
      in
      let* () =
        assert_equal ~label:"asset registry empty append slot" F.typ
          target.old_root old_root_from_empty
      in
      let* candidate_leaf = Asset_record.commitment_var target.candidate in
      let* new_root =
        implied_root_var ~leaf:candidate_leaf ~index:target.leaf_count
          append_path
      in
      let* new_count = Checked32.Checked.succ target.leaf_count in
      let new_state : Registry_state.var =
        { root = new_root
        ; leaf_count = new_count
        ; schema_version = Checked32.Checked.constant Config.schema_version
        }
      in
      let old_state : Registry_state.var =
        { root = target.old_root
        ; leaf_count = target.leaf_count
        ; schema_version = Checked32.Checked.constant Config.schema_version
        }
      in
      let* events = var_to_events Asset_record.typ target.candidate in
      let* registration_call_data =
        var_to_hash
          ~init:Zeko_constants.ethereum_asset_registry_registration_salt
          Typ.(Registry_state.typ * Registry_state.typ * Asset_record.typ)
          ((old_state, new_state), target.candidate)
      in
      let account_update =
        { default_account_update with
          public_key = constant PC.typ Config.registry_public_key
        ; authorization_kind = authorization_vk_hash registry_vk_hash
        ; update =
            { default_account_update.update with
              app_state =
                Registry_state.fine
                  { root = Some new_state.root
                  ; leaf_count = Some new_state.leaf_count
                  ; schema_version = Some new_state.schema_version
                  }
                |> var_to_app_state_fine
            }
        ; preconditions =
            { default_account_update.preconditions with
              account =
                { default_account_update.preconditions.account with
                  state =
                    Registry_state.fine
                      { root = Some old_state.root
                      ; leaf_count = Some old_state.leaf_count
                      ; schema_version = Some old_state.schema_version
                      }
                    |> var_to_precondition_fine
                }
            }
        ; events
        }
      in
      (* The PoC mirrors Ethereum bridge governance with a fixed Mina
         registration authority. Full-commitment signing binds the
         administrator to the candidate event, exact root/count transition,
         and every onboarding-account probe in this transaction. Ethereum
         remains the proposal and activation source of truth. *)
      let registration_authority =
        { default_account_update with
          public_key = constant PC.typ Config.registration_authority
        ; call_data = registration_call_data
        ; authorization_kind = authorization_signed ()
        ; use_full_commitment = Boolean.true_
        ; implicit_account_creation_fee = Boolean.false_
        }
      in
      let*| out =
        make_outputs ~chain:Config.chain_l2 account_update
          [ (registration_authority, []) ]
      in
      Compile_simple.{ prevs = One_prev verify_scan; out }

    let rule : _ Compile_simple.branch lazy_t =
      lazy
        { branch_name = "register Ethereum asset"
        ; tags = One_tag (Lazy.force Scan.tag)
        ; main
        }
  end

  module System =
  ( val Compile_simple.compile ~name:"Ethereum asset registry"
          ~out_typ:Typ.(Mina_base.Zkapp_statement.typ * V.typ)
          ~branches:[ Register.rule ] () )

  type registry_tag_var = System.tag_var

  let registry_tag = System.tag

  let register =
    Lazy.map System.provers ~f:(fun Compile_simple.[ register ] -> register)
end
