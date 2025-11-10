open Core_kernel
open Async
open Mina_base
open Account_update
open Signature_lib
module Field = Snark_params.Tick.Field
module L = Mina_ledger.Ledger
open Zeko_circuits

module Z = struct
  open Zeko_circuits
  open Zeko_types

  let proof_permissions : Permissions.t =
    { edit_state = Either
    ; send = Proof
    ; receive = None
    ; set_delegate = Proof
    ; set_permissions = Proof
    ; set_verification_key = (Either, Mina_numbers.Txn_version.current)
    ; set_zkapp_uri = Proof
    ; edit_action_state = Proof
    ; set_token_symbol = Proof
    ; increment_nonce = Proof
    ; set_voting_for = Proof
    ; set_timing = Proof
    ; access = Proof
    }

  let none_permissions : Permissions.t =
    { edit_state = None
    ; send = None
    ; receive = None
    ; set_delegate = None
    ; set_permissions = None
    ; set_verification_key = (None, Mina_numbers.Txn_version.current)
    ; set_zkapp_uri = None
    ; edit_action_state = None
    ; set_token_symbol = None
    ; increment_nonce = None
    ; set_voting_for = None
    ; set_timing = None
    ; access = None
    }

  module Inner = struct
    let initial_accounts () =
      let%bind inner_vk =
        Compile_simple.Verification_key.of_tag (Lazy.force Inner_rules_inst.tag)
        |> Promise.to_deferred
      in
      let%map holder_vk =
        Compile_simple.Verification_key.of_tag
          (Lazy.force Bridge_inst_mina.System_L2.tag)
        |> Promise.to_deferred
      in
      let inner_account =
        { Account.empty with
          public_key = Zeko_constants.inner_public_key
        ; balance = Currency.Balance.max_int
        ; permissions =
            { ( if
                (* see #286 *)
                Option.is_some Is_compile_simple_real.is_compile_simple_real
              then proof_permissions
              else none_permissions )
              with
              access = Permissions.Auth_required.None
            }
        ; zkapp =
            Some
              { Zkapp_account.default with
                app_state =
                  Rollup_state.Inner_state.(
                    Utils.value_to_zkapp_state Fn.id Field.zero typ default)
              ; verification_key =
                  Some
                    (Verification_key_wire.Stable.Latest.M.of_binable
                       ( match Is_compile_simple_real.is_compile_simple_real with
                       | Some eq ->
                           let _, vk_eq = Type_equal.detuple2 eq in
                           Type_equal.conv vk_eq inner_vk
                       | None ->
                           Pickles.Side_loaded.Verification_key.dummy ) )
              }
        }
      in
      let holder_account =
        { Account.empty with
          public_key = Zeko_circuits_config.Inputs.holder_account_l2
        ; balance = Currency.Balance.max_int
        ; permissions =
            ( if
              (* see #286 *)
              Option.is_some Is_compile_simple_real.is_compile_simple_real
            then proof_permissions
            else none_permissions )
        ; zkapp =
            Some
              { Zkapp_account.default with
                verification_key =
                  Some
                    (Verification_key_wire.Stable.Latest.M.of_binable
                       ( match Is_compile_simple_real.is_compile_simple_real with
                       | Some eq ->
                           let _, vk_eq = Type_equal.detuple2 eq in
                           Type_equal.conv vk_eq holder_vk
                       | None ->
                           Pickles.Side_loaded.Verification_key.dummy ) )
              }
        }
      in
      (`Inner inner_account, `Holder holder_account)
  end

  module Outer = struct
    let unsafe_deploy ~pause_key ~ledger_hash ~sequencer ~da_key ~acc_set () =
      let open Zkapp_basic in
      let%bind outer_vk =
        Compile_simple.Verification_key.of_tag (Lazy.force Outer_rules_inst.tag)
        |> Promise.to_deferred
      in
      let outer_update =
        { Update.dummy with
          app_state =
            Rollup_state.Outer_state.(
              Utils.value_to_zkapp_state
                (fun f -> Set_or_keep.Set f)
                Set_or_keep.Keep typ
                ( { pause_key
                  ; paused = false
                  ; ledger_hash
                  ; inner_action_state =
                      Rollup_state.Inner_action_state.With_length.empty
                  ; sequencer
                  ; da_key
                  ; acc_set
                  }
                  : t ))
        ; verification_key =
            Set
              (Verification_key_wire.Stable.Latest.M.of_binable
                 ( match Is_compile_simple_real.is_compile_simple_real with
                 | Some eq ->
                     let _, vk_eq = Type_equal.detuple2 eq in
                     Type_equal.conv vk_eq outer_vk
                 | None ->
                     Pickles.Side_loaded.Verification_key.dummy ) )
        ; permissions =
            Set
              { ( if
                  (* see #286 *)
                  Option.is_some Is_compile_simple_real.is_compile_simple_real
                then proof_permissions
                else none_permissions )
                with
                access = None
              }
        }
      in
      let%bind holder_vk =
        Compile_simple.Verification_key.of_tag
          (Lazy.force Bridge_inst_mina.System_L1_enabled.tag)
        |> Promise.to_deferred
      in
      let holder_update =
        { Update.dummy with
          verification_key =
            Set
              (Verification_key_wire.Stable.Latest.M.of_binable
                 ( match Is_compile_simple_real.is_compile_simple_real with
                 | Some eq ->
                     let _, vk_eq = Type_equal.detuple2 eq in
                     Type_equal.conv vk_eq holder_vk
                 | None ->
                     Pickles.Side_loaded.Verification_key.dummy ) )
        ; permissions =
            Set
              { ( if
                  (* see #286 *)
                  Option.is_some Is_compile_simple_real.is_compile_simple_real
                then proof_permissions
                else none_permissions )
                with
                access = None
              }
        }
      in
      let%map token_owner_vk =
        Compile_simple.Verification_key.of_tag
          (Lazy.force Bridge_inst_mina.System_L1_token_owner.tag)
        |> Promise.to_deferred
      in
      let token_owner_update =
        { Update.dummy with
          verification_key =
            Set
              (Verification_key_wire.Stable.Latest.M.of_binable
                 ( match Is_compile_simple_real.is_compile_simple_real with
                 | Some eq ->
                     let _, vk_eq = Type_equal.detuple2 eq in
                     Type_equal.conv vk_eq token_owner_vk
                 | None ->
                     Pickles.Side_loaded.Verification_key.dummy ) )
        ; permissions =
            Set
              ( if
                (* see #286 *)
                Option.is_some Is_compile_simple_real.is_compile_simple_real
              then proof_permissions
              else none_permissions )
        }
      in
      ( `Outer outer_update
      , `Holder holder_update
      , `Token_owner token_owner_update )

    let deploy_exn (l : L.t) =
      if
        not
          (Public_key.Compressed.equal Zeko_constants.inner_public_key
             (L.get_at_index_exn l 0).public_key )
      then failwith "zeko outer deploy: ledger invalid"
      else () ;
      unsafe_deploy ~ledger_hash:(L.merkle_root l)
  end
end

let deploy_command_exn ~signature_kind ~(signer : Keypair.t)
    ~(fee : Currency.Fee.t) ~(nonce : Account.Nonce.t) ~(outer_kp : Keypair.t)
    ~(holder_kp : Keypair.t) ~(token_holder_kp : Keypair.t)
    ~(initial_ledger : L.t) ~account_set_hash
    ~(account_creation_fee : Currency.Fee.t) ~pause_key ~sequencer ~da_key () =
  let%map ( `Outer outer_update
          , `Holder holder_update
          , `Token_owner token_owner_update ) =
    Z.Outer.deploy_exn ~pause_key ~sequencer ~da_key ~acc_set:account_set_hash
      initial_ledger ()
  in
  let outer_au =
    Account_update.with_aux
      ~body:
        { Body.dummy with
          public_key = Public_key.compress outer_kp.public_key
        ; implicit_account_creation_fee = false
        ; update = outer_update
        ; use_full_commitment = true
        ; authorization_kind = Signature
        }
      ~authorization:(Control.Poly.Signature Signature.dummy)
  in
  let holder_au =
    Account_update.with_aux
      ~body:
        { Body.dummy with
          public_key = Public_key.compress holder_kp.public_key
        ; implicit_account_creation_fee = false
        ; update = holder_update
        ; use_full_commitment = true
        ; authorization_kind = Signature
        }
      ~authorization:(Control.Poly.Signature Signature.dummy)
  in
  let token_owner_au =
    Account_update.with_aux
      ~body:
        { Body.dummy with
          public_key = Public_key.compress token_holder_kp.public_key
        ; implicit_account_creation_fee = false
        ; update = token_owner_update
        ; use_full_commitment = true
        ; authorization_kind = Signature
        }
      ~authorization:(Control.Poly.Signature Signature.dummy)
  in
  let sender_update =
    Account_update.with_aux
      ~body:
        { Body.dummy with
          public_key = Public_key.compress signer.public_key
        ; balance_change =
            Currency.Amount.(
              let ( + ) a b = Currency.Fee.add a b |> Option.value_exn in
              account_creation_fee + account_creation_fee + account_creation_fee
              |> of_fee |> Signed.of_unsigned |> Signed.negate)
        ; use_full_commitment = true
        ; authorization_kind = Signature
        }
      ~authorization:(Control.Poly.Signature Signature.dummy)
  in
  let call_forest =
    Zkapp_command.Call_forest.accumulate_hashes
      ~hash_account_update:
        (Zkapp_command.Call_forest.Digest.Account_update.create ~signature_kind)
    @@ Zkapp_command.Call_forest.of_account_updates
         ~account_update_depth:(fun _ -> 0)
         [ outer_au; holder_au; token_owner_au; sender_update ]
  in
  let command : Zkapp_command.t =
    { fee_payer =
        { Account_update.Fee_payer.body =
            { public_key = Public_key.compress signer.public_key
            ; fee
            ; valid_until = None
            ; nonce
            }
        ; authorization = Signature.dummy
        }
    ; account_updates = call_forest
    ; memo = Signed_command_memo.empty
    }
  in
  Utils.sign_zkapp_command ~signature_kind command
    [ outer_kp; holder_kp; token_holder_kp; signer ]

let update_verification_keys ~signature_kind ~(signer : Keypair.t)
    ~(fee : Currency.Fee.t) ~(nonce : Account.Nonce.t)
    (upgrades : (Compile_simple.Verification_key.t * Keypair.t) list) =
  let aus =
    List.map upgrades ~f:(fun (vk, kp) ->
        Account_update.with_aux
          ~body:
            { Body.dummy with
              public_key = Public_key.compress kp.public_key
            ; update =
                { Update.dummy with
                  verification_key =
                    Set
                      (Verification_key_wire.Stable.Latest.M.of_binable
                         ( match
                             Is_compile_simple_real.is_compile_simple_real
                           with
                         | Some eq ->
                             let _, vk_eq = Type_equal.detuple2 eq in
                             Type_equal.conv vk_eq vk
                         | None ->
                             Pickles.Side_loaded.Verification_key.dummy ) )
                }
            ; use_full_commitment = true
            ; authorization_kind = Signature
            }
          ~authorization:(Control.Poly.Signature Signature.dummy) )
  in
  let call_forest =
    Zkapp_command.Call_forest.accumulate_hashes
      ~hash_account_update:
        (Zkapp_command.Call_forest.Digest.Account_update.create ~signature_kind)
    @@ Zkapp_command.Call_forest.of_account_updates
         ~account_update_depth:(fun _ -> 0)
         aus
  in
  let command : Zkapp_command.t =
    { fee_payer =
        { Account_update.Fee_payer.body =
            { public_key = Public_key.compress signer.public_key
            ; fee
            ; valid_until = None
            ; nonce
            }
        ; authorization = Signature.dummy
        }
    ; account_updates = call_forest
    ; memo = Signed_command_memo.empty
    }
  in
  Utils.sign_zkapp_command ~signature_kind command
    (signer :: List.map upgrades ~f:snd)

let update_outer_state ~signature_kind ~(signer : Keypair.t)
    ~(fee : Currency.Fee.t) ~(nonce : Account.Nonce.t)
    ~(precondition : Rollup_state.Outer_state.fine)
    ~(update : Rollup_state.Outer_state.fine) =
  let update =
    Zeko_util.var_to_optional_fine @@ Rollup_state.Outer_state.fine update
    |> Pickles_types.Vector.Vector_8.map ~f:(function
         | None ->
             Zkapp_basic.Set_or_keep.Keep
         | Some x ->
             Set
               ( Field.Var.to_constant x
               |> Option.value_exn ~message:"Fine fields need to be constants"
               ) )
  in
  let precondition =
    Zeko_util.var_to_optional_fine @@ Rollup_state.Outer_state.fine precondition
    |> Pickles_types.Vector.Vector_8.map ~f:(function
         | None ->
             Zkapp_basic.Or_ignore.Ignore
         | Some x ->
             Check
               ( Field.Var.to_constant x
               |> Option.value_exn ~message:"Fine fields need to be constants"
               ) )
  in
  let au =
    Account_update.with_aux
      ~body:
        { Body.dummy with
          public_key = Zeko_circuits_config.t.zeko_l1
        ; update = { Update.dummy with app_state = update }
        ; preconditions =
            { Preconditions.accept with
              account =
                { Zkapp_precondition.Account.accept with state = precondition }
            }
        ; use_full_commitment = true
        ; authorization_kind = Signature
        }
      ~authorization:(Control.Poly.Signature Signature.dummy)
  in
  let call_forest =
    Zkapp_command.Call_forest.accumulate_hashes
      ~hash_account_update:
        (Zkapp_command.Call_forest.Digest.Account_update.create ~signature_kind)
    @@ Zkapp_command.Call_forest.of_account_updates
         ~account_update_depth:(fun _ -> 0)
         [ au ]
  in
  let command : Zkapp_command.t =
    { fee_payer =
        { Account_update.Fee_payer.body =
            { public_key = Public_key.compress signer.public_key
            ; fee
            ; valid_until = None
            ; nonce
            }
        ; authorization = Signature.dummy
        }
    ; account_updates = call_forest
    ; memo = Signed_command_memo.empty
    }
  in
  Utils.sign_zkapp_command ~signature_kind command
    ( signer
    :: [ Keypair.of_private_key_exn
           (Option.value_exn ~message:"Deploy config not present"
              Zeko_circuits_config.deploy_config )
             .zeko_l1
       ] )

module Change_permissions =
  Zeko_circuits.Rule_change_permissions.Make (Zeko_circuits_config.Inputs) ()

let update_permissions ~signature_kind ~(signer : Keypair.t)
    ~(fee : Currency.Fee.t) ~(nonce : Account.Nonce.t) ~gql_uri
    ~(permissions : Permissions.t) =
  let proof_cache_db = Proof_cache_tag.create_identity_db () in
  let%bind old_vk =
    Gql_client.fetch_vk gql_uri
      ( Account_id.of_public_key
      @@ Public_key.decompress_exn Zeko_circuits_config.t.zeko_l1 )
  in
  let%bind temp_vk =
    let%map vk =
      Compile_simple.Verification_key.of_tag (Lazy.force Change_permissions.tag)
      |> Promise.to_deferred
    in
    Verification_key_wire.Stable.Latest.M.of_binable
    @@
    match Is_compile_simple_real.is_compile_simple_real with
    | Some eq ->
        let _, vk_eq = Type_equal.detuple2 eq in
        Type_equal.conv vk_eq vk
    | None ->
        Pickles.Side_loaded.Verification_key.dummy
  in
  let f1 =
    Account_update.with_aux
      ~body:
        { Body.dummy with
          public_key = Zeko_circuits_config.t.zeko_l1
        ; update = { Update.dummy with verification_key = Set temp_vk }
        ; use_full_commitment = true
        ; authorization_kind = Signature
        }
      ~authorization:(Control.Poly.Signature Signature.dummy)
    |> fun au ->
    Zkapp_command.Call_forest.of_account_updates
      ~account_update_depth:(fun _ -> 0)
      [ au ]
    |> Zkapp_command.Call_forest.accumulate_hashes
         ~hash_account_update:
           (Zkapp_command.Call_forest.Digest.Account_update.create
              ~signature_kind )
  in
  let%map f2 =
    let [ prover ] = Lazy.force Change_permissions.provers in
    let%map (_stmt, (body, _, calls)), proof =
      prover
        { public_key = Zeko_circuits_config.t.zeko_l1
        ; vk_hash = temp_vk.hash
        ; permissions
        }
      |> Promise.to_deferred
    in
    Utils.attach_proof_to_forest ~signature_kind ~proof_cache_db ~body
      ~calls:
        (Zkapp_command.Call_forest.map calls
           ~f:Account_update.read_all_proofs_from_disk )
      ~proof
    |> Zkapp_command.Call_forest.map
         ~f:(Account_update.write_all_proofs_to_disk ~proof_cache_db)
  in
  let f3 =
    Account_update.with_aux
      ~body:
        { Body.dummy with
          public_key = Zeko_circuits_config.t.zeko_l1
        ; update =
            { Update.dummy with
              verification_key =
                Set (Verification_key_wire.Stable.Latest.M.of_binable old_vk)
            }
        ; use_full_commitment = true
        ; authorization_kind = Signature
        }
      ~authorization:(Control.Poly.Signature Signature.dummy)
    |> fun au ->
    Zkapp_command.Call_forest.of_account_updates
      ~account_update_depth:(fun _ -> 0)
      [ au ]
    |> Zkapp_command.Call_forest.accumulate_hashes
         ~hash_account_update:
           (Zkapp_command.Call_forest.Digest.Account_update.create
              ~signature_kind )
  in
  let call_forest = f1 @ f2 @ f3 in
  let command : Zkapp_command.t =
    { fee_payer =
        { Account_update.Fee_payer.body =
            { public_key = Public_key.compress signer.public_key
            ; fee
            ; valid_until = None
            ; nonce
            }
        ; authorization = Signature.dummy
        }
    ; account_updates = Utils.rehash_forest ~signature_kind call_forest
    ; memo = Signed_command_memo.empty
    }
  in
  Utils.sign_zkapp_command ~signature_kind command
    ( signer
    :: [ Keypair.of_private_key_exn
           (Option.value_exn Zeko_circuits_config.deploy_config).zeko_l1
       ] )
