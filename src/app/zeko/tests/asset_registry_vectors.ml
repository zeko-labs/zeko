open Core
open Mina_base
open Snark_params.Tick
open Zeko_circuits
open Zeko_util
module PC = Signature_lib.Public_key.Compressed
module AR = Asset_registry

let point_of_string value =
  Inner_curve.(to_affine_exn @@ point_near_x @@ Field.of_string value)
  |> Signature_lib.Public_key.compress

let registry_public_key = point_of_string "91001"

let vault_public_key = point_of_string "91002"

let approved_mft_standard_vk_id = Field.of_int 91003

let registration_authority = point_of_string "91006"

let value_to_hash ~init (typ : ('var, 'value) Snark_params.Tick.Typ.t) value =
  let (Typ typ) = typ in
  let fields, _ = typ.value_to_fields value in
  Random_oracle.hash ~init:(Hash_prefix_create.salt init) fields

module Registry =
  AR.Make
    (struct
      let registry_public_key = registry_public_key

      let registration_authority = registration_authority

      let schema_version =
        Checked32.of_int Zeko_constants.Ethereum_asset_registry.schema_version

      let approved_mft_standard_vk_id = approved_mft_standard_vk_id

      let universal_bridge_vk_id = Field.of_int 91004

      let vault_public_key = vault_public_key

      let chain_l2 = Mina_signature_kind.Testnet
    end)
    ()

let register = Lazy.force Registry.register

let amount value = Unsigned.UInt64.of_int value |> Currency.Amount.of_uint64

let record index =
  let token_owner_l2 = point_of_string (Int.to_string (92000 + index)) in
  let owner = Account_id.create token_owner_l2 AR.Token_id.default in
  ( { schema_version =
        Checked32.of_int Zeko_constants.Ethereum_asset_registry.schema_version
    ; registry_index = Checked32.of_int index
    ; asset_id_high = Field.of_int (1000 + index)
    ; asset_id_low = Field.of_int (2000 + index)
    ; ethereum_token_address = Field.of_int (3000 + index)
    ; token_owner_l2
    ; token_id_l2 = Account_id.derive_token_id ~owner
    ; decimals = Checked32.of_int (6 + (index mod 4))
    ; inventory_cap = amount (1_000_000 + index)
    ; mft_standard_vk_id = approved_mft_standard_vk_id
    ; vault_public_key
    ; universal_bridge_vk_id = Field.of_int 91004
    }
    : AR.Asset_record.t )

let state tree : AR.Registry_state.t =
  { root = AR.Merkle_list.root tree
  ; leaf_count = Checked32.of_int (AR.Merkle_list.count tree)
  ; schema_version =
      Checked32.of_int Zeko_constants.Ethereum_asset_registry.schema_version
  }

let expect_failure label f =
  if not (Exn.does_raise f) then failwithf "%s unexpectedly succeeded" label ()

let run_membership witness =
  run_and_check_exn
    (let* witness =
       exists Registry.Membership_witness.typ ~compute:(fun _ -> witness)
     in
     let*| verified = Registry.verify witness in
     As_prover.read
       Typ.(AR.Token_id.typ * Account_update.Body.typ ())
       ( Registry.Verified_asset.token_id verified
       , Registry.Verified_asset.authenticated_registry_call verified ) )

let membership_witness record =
  let tree = AR.Merkle_list.append_exn (AR.Merkle_list.empty ()) record in
  ( { Registry.Membership_witness.state = state tree
    ; record
    ; path = AR.Merkle_list.path tree ~index:0
    }
    : Registry.Membership_witness.t )

let check_membership () =
  let first = record 0 in
  let witness = membership_witness first in
  let token_id, registry_call = run_membership witness in
  if not (AR.Token_id.equal token_id first.token_id_l2) then
    failwith "verified membership returned the wrong token ID" ;
  if not (PC.equal registry_call.public_key registry_public_key) then
    failwith "registry state was not authenticated against the configured zkApp" ;
  let checked_state =
    Zkapp_state.V.to_list registry_call.preconditions.account.state
    |> List.filter_map ~f:Zkapp_basic.Or_ignore.to_option
  in
  if
    not
      ([%equal: Field.t list] checked_state
         [ (state tree1).root
         ; Checked32.to_field (state tree1).leaf_count
         ; Checked32.to_field (state tree1).schema_version
         ] )
  then failwith "registry account precondition did not bind root/count/version" ;
  let bad_path =
    match witness.path with
    | sibling :: rest ->
        Field.(sibling + one) :: rest
    | [] ->
        assert false
  in
  expect_failure "wrong Merkle path" (fun () ->
      ignore
        ( run_membership { witness with path = bad_path }
          : AR.Token_id.t * Account_update.Body.t ) ) ;
  expect_failure "modified immutable record" (fun () ->
      ignore
        ( run_membership
            { witness with
              record = { first with inventory_cap = amount 2_000_000 }
            }
          : AR.Token_id.t * Account_update.Body.t ) ) ;
  expect_failure "record beyond committed count" (fun () ->
      ignore
        ( run_membership
            { witness with
              state = { witness.state with leaf_count = Checked32.zero }
            }
          : AR.Token_id.t * Account_update.Body.t ) ) ;
  expect_failure "unsupported schema version" (fun () ->
      ignore
        ( run_membership
            { witness with
              state = { witness.state with schema_version = Checked32.of_int 2 }
            }
          : AR.Token_id.t * Account_update.Body.t ) ) ;
  let maximum_decimals = { first with decimals = Checked32.of_int 255 } in
  ignore
    ( run_membership (membership_witness maximum_decimals)
      : AR.Token_id.t * Account_update.Body.t ) ;
  let overflowing_decimals = { first with decimals = Checked32.of_int 256 } in
  expect_failure "decimals outside UInt8" (fun () ->
      ignore
        ( run_membership (membership_witness overflowing_decimals)
          : AR.Token_id.t * Account_update.Body.t ) )

let check_append_only_paths () =
  let tree0 = AR.Merkle_list.empty () in
  if not (Field.equal (AR.Merkle_list.root tree0) AR.Merkle_list.empty_root)
  then failwith "empty registry root is unstable" ;
  let first = record 0 in
  let tree1 = AR.Merkle_list.append_exn tree0 first in
  let first_path_at_one = AR.Merkle_list.path tree1 ~index:0 in
  if
    not
      (AR.Merkle_list.verify
         ~root:(AR.Merkle_list.root tree1)
         first first_path_at_one )
  then failwith "first record is not a member after append" ;
  let second = record 1 in
  let tree2 = AR.Merkle_list.append_exn tree1 second in
  if
    AR.Merkle_list.verify
      ~root:(AR.Merkle_list.root tree2)
      first first_path_at_one
  then failwith "stale membership path unexpectedly verified after append" ;
  let refreshed = AR.Merkle_list.path tree2 ~index:0 in
  if
    not
      (AR.Merkle_list.verify ~root:(AR.Merkle_list.root tree2) first refreshed)
  then failwith "historical record did not survive append with a refreshed path" ;
  if
    not
      (AR.Merkle_list.verify
         ~root:(AR.Merkle_list.root tree2)
         second
         (AR.Merkle_list.path tree2 ~index:1) )
  then failwith "second record is not a member after append"

let run_scan_step elem stmt =
  run_and_check_exn
    (let* elem =
       exists Registry.Scan.Definition.Elem.typ ~compute:(fun _ -> elem)
     in
     let* stmt =
       exists Registry.Scan.Definition.Stmt.typ ~compute:(fun _ -> stmt)
     in
     let*| target = Registry.Scan.Definition.step elem stmt in
     As_prover.read Registry.Scan.Definition.Stmt.typ target )

let scan_stmt tree candidate : Registry.Scan.Definition.Stmt.t =
  { old_root = AR.Merkle_list.root tree
  ; leaf_count = Checked32.of_int (AR.Merkle_list.count tree)
  ; candidate
  ; next_expected_index = Checked32.zero
  ; traversed_count = Checked32.zero
  }

let scan_elem tree (existing : AR.Asset_record.t) :
    Registry.Scan.Definition.Elem.t =
  { active = true
  ; record = existing
  ; path =
      AR.Merkle_list.path tree ~index:(Checked32.to_int existing.registry_index)
  }

let check_exhaustive_scan_step () =
  let first = record 0 in
  let tree = AR.Merkle_list.append_exn (AR.Merkle_list.empty ()) first in
  let candidate = record 1 in
  let stmt = scan_stmt tree candidate in
  let target = run_scan_step (scan_elem tree first) stmt in
  if
    not
      ( Checked32.equal target.next_expected_index Checked32.one
      && Checked32.equal target.traversed_count Checked32.one )
  then failwith "scan did not advance index and count exactly once" ;
  let repeated_index =
    { stmt with
      next_expected_index = Checked32.one
    ; traversed_count = Checked32.one
    }
  in
  expect_failure "repeated or reordered scan leaf" (fun () ->
      ignore
        ( run_scan_step (scan_elem tree first) repeated_index
          : Registry.Scan.Definition.Stmt.t ) ) ;
  expect_failure "wrong scan root" (fun () ->
      ignore
        ( run_scan_step (scan_elem tree first)
            { stmt with old_root = Field.(stmt.old_root + one) }
          : Registry.Scan.Definition.Stmt.t ) ) ;
  expect_failure "duplicate Ethereum token address" (fun () ->
      ignore
        ( run_scan_step (scan_elem tree first)
            { stmt with
              candidate =
                { candidate with
                  ethereum_token_address = first.ethereum_token_address
                }
            }
          : Registry.Scan.Definition.Stmt.t ) ) ;
  expect_failure "duplicate canonical asset ID" (fun () ->
      ignore
        ( run_scan_step (scan_elem tree first)
            { stmt with
              candidate =
                { candidate with
                  asset_id_high = first.asset_id_high
                ; asset_id_low = first.asset_id_low
                }
            }
          : Registry.Scan.Definition.Stmt.t ) ) ;
  expect_failure "duplicate L2 token owner" (fun () ->
      ignore
        ( run_scan_step (scan_elem tree first)
            { stmt with
              candidate =
                { candidate with token_owner_l2 = first.token_owner_l2 }
            }
          : Registry.Scan.Definition.Stmt.t ) ) ;
  expect_failure "duplicate L2 token ID" (fun () ->
      ignore
        ( run_scan_step (scan_elem tree first)
            { stmt with
              candidate = { candidate with token_id_l2 = first.token_id_l2 }
            }
          : Registry.Scan.Definition.Stmt.t ) )

type scan_proof =
  { transition : Registry.Scan.trans; proof : Compile_simple.Proof.t }

let scan_stmt_at (old_state : AR.Registry_state.t) candidate traversed :
    Registry.Scan.Definition.Stmt.t =
  { old_root = old_state.root
  ; leaf_count = old_state.leaf_count
  ; candidate
  ; next_expected_index = Checked32.of_int traversed
  ; traversed_count = Checked32.of_int traversed
  }

let merge_scan_proofs proofs =
  let rec merge_level = function
    | [] ->
        failwith "cannot merge an empty scan-proof list"
    | [ proof ] ->
        proof
    | proofs ->
        let rec pairs acc = function
          | left :: right :: rest ->
              let transition, proof =
                Promise.block_on_async_exn
                @@ fun () ->
                (Lazy.force Registry.Scan.merge)
                  { left = left.transition
                  ; left_proof = left.proof
                  ; right = right.transition
                  ; right_proof = right.proof
                  }
              in
              pairs ({ transition; proof } :: acc) rest
          | [ last ] ->
              List.rev (last :: acc)
          | [] ->
              List.rev acc
        in
        merge_level (pairs [] proofs)
  in
  merge_level proofs

let prove_scan old_state candidate elems =
  match elems with
  | [] ->
      let source = scan_stmt_at old_state candidate 0 in
      let transition, proof =
        Promise.block_on_async_exn
        @@ fun () -> (Lazy.force Registry.Scan.leaf_option) ([], source)
      in
      { transition; proof }
  | elems ->
      List.chunks_of elems ~length:8
      |> List.fold_map ~init:0 ~f:(fun traversed chunk ->
             let source = scan_stmt_at old_state candidate traversed in
             let transition, proof =
               Promise.block_on_async_exn
               @@ fun () -> (Lazy.force Registry.Scan.leaf) (chunk, source)
             in
             ( traversed + List.length chunk
             , ({ transition; proof } : scan_proof) ) )
      |> snd |> merge_scan_proofs

let make_scan ?old_state ?records tree candidate =
  let old_state = Option.value old_state ~default:(state tree) in
  let init : Registry.Scan.Definition.Init.t = { old_state; candidate } in
  let records =
    Option.value records
      ~default:
        (List.init (AR.Merkle_list.count tree) ~f:(fun index -> record index))
  in
  let elems = List.map records ~f:(scan_elem tree) in
  let { transition = { source = proof_source; target = proof_target }; proof } =
    prove_scan old_state candidate elems
  in
  Registry.Scan_inst.make ~proof_source ~proof_target ~proof init []

let register_record ?old_state ?records ?append_path tree candidate =
  let append_path =
    match append_path with
    | Some path ->
        path
    | None ->
        AR.Merkle_list.path tree ~index:(AR.Merkle_list.count tree)
  in
  let witness : Registry.Register.Witness.t =
    { scan = make_scan ?old_state ?records tree candidate
    ; append_path
    ; registry_vk_hash = Field.of_int 91005
    }
  in
  Promise.block_on_async_exn @@ fun () -> register witness

let check_registration_transition () =
  let empty = AR.Merkle_list.empty () in
  let first = record 0 in
  let (_statement, (body, _digest, calls)), _proof =
    register_record empty first
  in
  let expected_tree = AR.Merkle_list.append_exn empty first in
  let updated_state =
    Zkapp_state.V.to_list body.update.app_state
    |> List.filter_map ~f:Zkapp_basic.Set_or_keep.to_option
  in
  if
    not
      ([%equal: Field.t list] updated_state
         [ AR.Merkle_list.root expected_tree
         ; Checked32.to_field Checked32.one
         ; Checked32.to_field
             (Checked32.of_int
                Zeko_constants.Ethereum_asset_registry.schema_version )
         ] )
  then failwith "registration did not expose the expected new root/count" ;
  let authority_calls = Zkapp_command.Call_forest.to_list calls in
  let authority =
    match authority_calls with
    | [ authority ] ->
        authority
    | _ ->
        failwith "registration must emit exactly one authority child"
  in
  if
    not
      ( PC.equal authority.body.public_key registration_authority
      && [%equal: Account_update.Authorization_kind.t]
           authority.body.authorization_kind Signature
      && authority.body.use_full_commitment
      && not authority.body.implicit_account_creation_fee )
  then failwith "registration authority child is malformed" ;
  let expected_call_data =
    value_to_hash ~init:Zeko_constants.ethereum_asset_registry_registration_salt
      Typ.(AR.Registry_state.typ * AR.Registry_state.typ * AR.Asset_record.typ)
      ((state empty, state expected_tree), first)
  in
  if not (Field.equal authority.body.call_data expected_call_data) then
    failwith "registration authority signature is not transition-bound" ;
  let tree1 = expected_tree in
  let (_statement, (_body, _digest, _calls)), _proof =
    register_record tree1 (record 1)
  in
  let tree2 = AR.Merkle_list.append_exn tree1 (record 1) in
  expect_failure "too few traversal witnesses" (fun () ->
      ignore
        ( register_record ~records:[ first ] tree2 (record 2)
          : AR.Output.t * Compile_simple.Proof.t ) ) ;
  expect_failure "too many traversal witnesses" (fun () ->
      ignore
        ( register_record ~records:[ first; record 1 ] tree1 (record 1)
          : AR.Output.t * Compile_simple.Proof.t ) ) ;
  expect_failure "repeated traversal leaf" (fun () ->
      ignore
        ( register_record ~records:[ first; first ] tree2 (record 2)
          : AR.Output.t * Compile_simple.Proof.t ) ) ;
  expect_failure "skipped traversal index" (fun () ->
      ignore
        ( register_record ~records:[ record 1 ] tree2 (record 2)
          : AR.Output.t * Compile_simple.Proof.t ) ) ;
  expect_failure "reordered traversal indices" (fun () ->
      ignore
        ( register_record ~records:[ record 1; first ] tree2 (record 2)
          : AR.Output.t * Compile_simple.Proof.t ) ) ;
  expect_failure "forged committed leaf count" (fun () ->
      let forged_state =
        { (state tree1) with leaf_count = Checked32.of_int 2 }
      in
      ignore
        ( register_record ~old_state:forged_state tree1 (record 1)
          : AR.Output.t * Compile_simple.Proof.t ) ) ;
  expect_failure "wrong old root" (fun () ->
      let wrong_state =
        { (state tree1) with root = Field.((state tree1).root + one) }
      in
      ignore
        ( register_record ~old_state:wrong_state tree1 (record 1)
          : AR.Output.t * Compile_simple.Proof.t ) ) ;
  expect_failure "non-member traversal record" (fun () ->
      let modified = { first with inventory_cap = amount 5_000_000 } in
      ignore
        ( register_record ~records:[ modified ] tree1 (record 1)
          : AR.Output.t * Compile_simple.Proof.t ) ) ;
  expect_failure "non-empty append slot" (fun () ->
      let occupied_as_empty : AR.Registry_state.t =
        { root = AR.Merkle_list.root tree1
        ; leaf_count = Checked32.zero
        ; schema_version =
            Checked32.of_int
              Zeko_constants.Ethereum_asset_registry.schema_version
        }
      in
      ignore
        ( register_record ~old_state:occupied_as_empty ~records:[]
            ~append_path:(AR.Merkle_list.path tree1 ~index:0)
            tree1 first
          : AR.Output.t * Compile_simple.Proof.t ) )

let check_maximum_registration () =
  let started_at = Time_ns.now () in
  let tree =
    List.init (Zeko_constants.Ethereum_asset_registry.max_assets - 1) ~f:Fn.id
    |> List.fold ~init:(AR.Merkle_list.empty ()) ~f:(fun tree index ->
           AR.Merkle_list.append_exn tree (record index) )
  in
  let candidate =
    record (Zeko_constants.Ethereum_asset_registry.max_assets - 1)
  in
  let (_statement, (body, _digest, _calls)), _proof =
    register_record tree candidate
  in
  let updated_state =
    Zkapp_state.V.to_list body.update.app_state
    |> List.filter_map ~f:Zkapp_basic.Set_or_keep.to_option
  in
  match updated_state with
  | _root :: count :: _ ->
      if
        not
          (Field.equal count
             (Checked32.to_field
                (Checked32.of_int
                   Zeko_constants.Ethereum_asset_registry.max_assets ) ) )
      then failwith "maximum-size registration did not reach the configured cap"
      else
        let elapsed_ms =
          Time_ns.diff (Time_ns.now ()) started_at |> Time_ns.Span.to_ms
        in
        let heap_words = (Gc.quick_stat ()).top_heap_words in
        printf
          "asset registry maximum: assets=%d elapsed_ms=%.0f top_heap_words=%d \
           word_bytes=%d\n\
           %!"
          Zeko_constants.Ethereum_asset_registry.max_assets elapsed_ms
          heap_words (Sys.word_size / 8)
  | _ ->
      failwith "maximum-size registration omitted its new state"

let print_membership_json () =
  let first = record 0 in
  let tree = AR.Merkle_list.append_exn (AR.Merkle_list.empty ()) first in
  Registry.Membership_witness.
    { state = state tree
    ; record = first
    ; path = AR.Merkle_list.path tree ~index:0
    }
  |> Registry.Membership_witness.to_yojson |> Yojson.Safe.to_string
  |> printf "%s\n%!"

let () =
  ( match Registry.Scan.Definition.wrap_domain with
  | Some `N14 ->
      ()
  | _ ->
      failwith "asset registry scan must use the real Pickles N14 wrap domain"
  ) ;
  check_append_only_paths () ;
  check_membership () ;
  check_exhaustive_scan_step () ;
  check_registration_transition () ;
  if Array.exists (Sys.get_argv ()) ~f:(String.equal "--max") then
    check_maximum_registration () ;
  if Array.exists (Sys.get_argv ()) ~f:(String.equal "--json") then
    print_membership_json () ;
  printf "asset registry vectors: ok\n%!"
