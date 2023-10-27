open Core_kernel
open Pickles_types
open Mina_base
open Snark_params
open Tick
open Run
open Pickles.Inductive_rule
open Zkapp_basic

open struct
  module Nonce = Mina_numbers.Account_nonce
  module Local_state = Mina_state.Local_state

  let constraint_constants = Genesis_constants.Constraint_constants.compiled

  module L = Mina_ledger.Ledger

  (* let (!) = run_checked *)

  let var_of_t (type var value) (typ : (var, value) Typ.t) (x : value) : var =
    let open Snark_params.Tick in
    let (Typ typ) = typ in
    let fields, aux = typ.value_to_fields x in
    let fields = Array.map ~f:Field.Var.constant fields in
    typ.var_of_fields (fields, aux)

  let proof_permissions : Permissions.t =
    { edit_state = Proof
    ; send = Proof
    ; receive = None
    ; set_delegate = Proof
    ; set_permissions = Proof
    ; set_verification_key = Either
    ; set_zkapp_uri = Proof
    ; edit_action_state = Proof
    ; set_token_symbol = Proof
    ; increment_nonce = Proof
    ; set_voting_for = Proof
    ; set_timing = Proof
    ; access = Proof
    }

  module Public_key = Signature_lib.Public_key
end

(* TODO: Extend with helper rules to merge unwrapped transactions for performance *)
module Wrapper_rules = struct
  module S = struct
    module Poly = struct
      type ('ledger_hash, 'local_state) t =
        { source_ledger : 'ledger_hash
        ; source_state : 'local_state
        ; target_ledger : 'ledger_hash
        ; target_state : 'local_state
        }
      [@@deriving hlist]
    end

    type t = (Frozen_ledger_hash.t, Local_state.t) Poly.t

    type var = (Frozen_ledger_hash.var, Local_state.Checked.t) Poly.t

    let typ : (var, t) Tick.Typ.t =
      let open Poly in
      Tick.Typ.of_hlistable
        [ Frozen_ledger_hash.typ
        ; Local_state.typ
        ; Frozen_ledger_hash.typ
        ; Local_state.typ
        ]
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist

    module Checked = struct
      type t = var
    end
  end

  module Wrap = struct
    open struct
      module T = Transaction_snark.Statement.With_sok
    end

    include struct
      open Snarky_backendless.Request

      type _ t +=
        | Statement_to_wrap : T.t t
        | Proof_to_wrap : (Nat.N2.n, Nat.N2.n) Pickles.Proof.t t
    end

    let handler (stmt : T.t) (prf : _ Pickles.Proof.t)
        (Snarky_backendless.Request.With { request; respond }) =
      match request with
      | Statement_to_wrap ->
          respond (Provide stmt)
      | Proof_to_wrap ->
          respond (Provide prf)
      | _ ->
          respond Unhandled

    let%snarkydef_ main { public_input = (stmt : S.Checked.t) } =
      let istmt = exists T.typ ~request:(fun () -> Statement_to_wrap) in
      let prf =
        exists (Typ.Internal.ref ()) ~request:(fun () -> Proof_to_wrap)
      in
      run_checked
      @@ Frozen_ledger_hash.assert_equal stmt.source_ledger
           istmt.source.first_pass_ledger ;
      run_checked
      @@ Frozen_ledger_hash.assert_equal stmt.target_ledger
           istmt.target.second_pass_ledger ;
      Local_state.Checked.assert_equal stmt.source_state
        istmt.source.local_state ;
      Local_state.Checked.assert_equal stmt.target_state
        istmt.target.local_state ;
      run_checked
      @@ Frozen_ledger_hash.assert_equal istmt.target.first_pass_ledger
           istmt.source.second_pass_ledger ;
      (* FIXME: maybe check connecting ledger *)
      (* TODO: Should we check the other fields? Right now we don't care. *)
      { previous_proof_statements =
          [ { public_input = istmt
            ; proof_must_verify = Boolean.true_
            ; proof = prf
            }
          ]
      ; public_output = ()
      ; auxiliary_output = ()
      }

    let rule txn_snark_tag : _ Pickles.Inductive_rule.t =
      { identifier = "zeko wrap"
      ; prevs = [ txn_snark_tag ]
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end

  module Merge = struct
    include struct
      open Snarky_backendless.Request

      type _ t +=
        | Statements_to_merge : (S.t * S.t) t
        | Proofs_to_merge :
            ( (Nat.N2.n, Nat.N2.n) Pickles.Proof.t
            * (Nat.N2.n, Nat.N2.n) Pickles.Proof.t )
            t
    end

    let handler s1 s2 p1 p2
        (Snarky_backendless.Request.With { request; respond }) =
      match request with
      | Statements_to_merge ->
          respond (Provide (s1, s2))
      | Proofs_to_merge ->
          respond (Provide (p1, p2))
      | _ ->
          respond Unhandled

    let%snarkydef_ main { public_input = (s : S.Checked.t) } =
      let s1, s2 =
        exists Typ.(S.typ * S.typ) ~request:(fun () -> Statements_to_merge)
      in
      let p1, p2 =
        Run.exists
          Typ.(Internal.ref () * Internal.ref ())
          ~request:(fun () -> Proofs_to_merge)
      in
      Local_state.Checked.assert_equal s.source_state s1.source_state ;
      Local_state.Checked.assert_equal s1.target_state s2.source_state ;
      Local_state.Checked.assert_equal s2.target_state s.target_state ;
      run_checked
      @@ Frozen_ledger_hash.assert_equal s.source_ledger s1.source_ledger ;
      run_checked
      @@ Frozen_ledger_hash.assert_equal s1.target_ledger s2.source_ledger ;
      run_checked
      @@ Frozen_ledger_hash.assert_equal s2.target_ledger s.target_ledger ;
      { previous_proof_statements =
          [ { public_input = s1; proof_must_verify = Boolean.true_; proof = p1 }
          ; { public_input = s2; proof_must_verify = Boolean.true_; proof = p2 }
          ]
      ; public_output = ()
      ; auxiliary_output = ()
      }

    let rule self : _ Pickles.Inductive_rule.t =
      { identifier = "zeko merge"
      ; prevs = [ self; self ]
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end
end

open struct
  module S = Wrapper_rules.S
end

module Nested_rules = struct
  module Step = struct
    include struct
      open Snarky_backendless.Request

      type _ t +=
        | Public_key : Public_key.Compressed.t Snarky_backendless.Request.t
        | Vk_hash : Field.Constant.t t
    end

    let handler (pk : Public_key.Compressed.t) (vk_hash : Field.Constant.t)
        (Snarky_backendless.Request.With { request; respond }) =
      match request with
      | Public_key ->
          respond (Provide pk)
      | Vk_hash ->
          respond (Provide vk_hash)
      | _ ->
          respond Unhandled

    let main { Pickles.Inductive_rule.public_input = () } =
      let public_key =
        exists Public_key.Compressed.typ ~request:(fun () -> Public_key)
      in
      let vk_hash = exists Field.typ ~request:(fun () -> Vk_hash) in
      let account_update =
        var_of_t (Account_update.Body.typ ()) Account_update.Body.dummy
      in
      let authorization_kind : Account_update.Authorization_kind.Checked.t =
        { is_signed = Boolean.false_
        ; is_proved = Boolean.true_
        ; verification_key_hash = vk_hash
        }
      in
      let update = account_update.update in
      let keep = Set_or_keep.Checked.keep ~dummy:Field.zero in
      let update =
        { update with
          app_state = [ keep; keep; keep; keep; keep; keep; keep; keep ]
        }
      in
      let account_precondition =
        var_of_t
          (Zkapp_precondition.Account.typ ())
          Zkapp_precondition.Account.accept
      in
      let ignore = Or_ignore.Checked.make_unsafe Boolean.false_ Field.zero in
      let account_precondition =
        { account_precondition with
          state =
            [ ignore; ignore; ignore; ignore; ignore; ignore; ignore; ignore ]
        }
      in
      let preconditions =
        var_of_t
          (Account_update.Preconditions.typ ())
          Account_update.Preconditions.accept
      in
      let preconditions =
        { preconditions with account = account_precondition }
      in
      let account_update =
        { account_update with
          public_key
        ; authorization_kind
        ; update
        ; preconditions
        }
      in
      let account_update_digest =
        Zkapp_command.Call_forest.Digest.Account_update.Checked.create
          account_update
      in
      let calls = Zkapp_call_forest.Checked.empty () in
      let public_output : Zkapp_statement.Checked.t =
        { account_update = (account_update_digest :> Field.t)
        ; calls = (Zkapp_call_forest.Checked.hash calls :> Field.t)
        }
      in
      let auxiliary_output =
        Prover_value.create (fun () ->
            let account_update =
              As_prover.read (Account_update.Body.typ ()) account_update
            in
            let account_update_digest =
              As_prover.read Zkapp_command.Call_forest.Digest.Account_update.typ
                account_update_digest
            in
            let calls = Prover_value.get calls.data in
            (account_update, account_update_digest, calls) )
      in
      { previous_proof_statements = []; public_output; auxiliary_output }

    let rule : _ Pickles.Inductive_rule.t =
      { identifier = "Rollup special account step"
      ; prevs = []
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end
end

module Deploy = struct
  let deploy (public_key : Public_key.Compressed.t)
      (vk : Side_loaded_verification_key.t)
      (nested_vk : Side_loaded_verification_key.t) :
      L.t * Account_update.Update.t =
    let l = L.create ~depth:constraint_constants.ledger_depth () in
    let nested_pk =
      Public_key.Compressed.of_base58_check_exn
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
    in
    L.create_new_account_exn l
      (Account_id.of_public_key @@ Public_key.decompress_exn nested_pk)
      { Account.empty with
        public_key = nested_pk
      ; balance = Currency.Balance.max_int
      ; permissions = proof_permissions
      ; zkapp =
          Some
            { Zkapp_account.default with
              app_state =
                [ Field.Constant.zero
                ; Field.Constant.zero
                ; Field.Constant.zero
                ; Field.Constant.zero
                ; Field.Constant.zero
                ; Field.Constant.zero
                ; Field.Constant.zero
                ; Field.Constant.zero
                ]
            ; verification_key =
                Some
                  (Verification_key_wire.Stable.Latest.M.of_binable nested_vk)
            }
      } ;
    if
      not
        (Public_key.Compressed.equal nested_pk
           (L.get_at_index_exn l 0).public_key )
    then failwith "bug"
    else () ;
    let state_0 = L.merkle_root l in
    let update =
      { Account_update.Update.dummy with
        app_state =
          [ Set (Frozen_ledger_hash.to_field (L.merkle_root l))
          ; Set Field.Constant.zero
          ; Set Field.Constant.zero
          ; Set Field.Constant.zero
          ; Set Field.Constant.zero
          ; Set Field.Constant.zero
          ; Set Field.Constant.zero
          ; Set Field.Constant.zero
          ]
      ; verification_key = Set { data = vk; hash = Zkapp_account.digest_vk vk }
      ; permissions = Set proof_permissions
      }
    in
    (l, update)
end

module Rules = struct
  module Step = struct
    include struct
      open Snarky_backendless.Request

      type _ t +=
        | Stmt : S.t t
        | Prf : (Nat.N2.n, Nat.N2.n) Pickles.Proof.t t
        | Public_key : Public_key.Compressed.t Snarky_backendless.Request.t
        | Vk_hash : Field.Constant.t t
    end

    let handler (stmt : S.t) (prf : _ Pickles.Proof.t)
        (pk : Public_key.Compressed.t) (vk_hash : Field.Constant.t)
        (Snarky_backendless.Request.With { request; respond }) =
      match request with
      | Stmt ->
          respond (Provide stmt)
      | Prf ->
          respond (Provide prf)
      | Public_key ->
          respond (Provide pk)
      | Vk_hash ->
          respond (Provide vk_hash)
      | _ ->
          respond Unhandled

    let main { Pickles.Inductive_rule.public_input = () } =
      let stmt = exists S.typ ~request:(fun () -> Stmt) in
      let prf = exists (Typ.Internal.ref ()) ~request:(fun () -> Prf) in
      let public_key =
        exists Public_key.Compressed.typ ~request:(fun () -> Public_key)
      in
      let vk_hash = exists Field.typ ~request:(fun () -> Vk_hash) in
      let depth = constraint_constants.ledger_depth in
      let acc = var_of_t (Account.Index.Unpacked.typ depth) 0 in
      let old_acc =
        run_checked @@ Frozen_ledger_hash.get depth stmt.source_ledger acc
      in
      let new_acc =
        run_checked @@ Frozen_ledger_hash.get depth stmt.target_ledger acc
      in
      let zkapp_hash, zkapp = new_acc.zkapp in
      let zkapp =
        exists Zkapp_account.typ ~compute:(fun () ->
            Option.value_exn @@ As_prover.Ref.get zkapp )
      in
      Field.Assert.equal zkapp_hash @@ Zkapp_account.Checked.digest zkapp ;
      (* FIXME: check more of zkapp state *)
      let () =
        run_checked
        @@ Nonce.Checked.Assert.equal
             (run_checked @@ Nonce.Checked.succ old_acc.nonce)
             new_acc.nonce
      in
      let account_update =
        var_of_t (Account_update.Body.typ ()) Account_update.Body.dummy
      in
      let authorization_kind : Account_update.Authorization_kind.Checked.t =
        { is_signed = Boolean.false_
        ; is_proved = Boolean.true_
        ; verification_key_hash = vk_hash
        }
      in
      let update = account_update.update in
      let keep = Set_or_keep.Checked.keep ~dummy:Field.zero in
      let update =
        { update with
          app_state =
            [ Set_or_keep.Checked.set
                (Frozen_ledger_hash.var_to_field stmt.target_ledger)
            ; keep
            ; keep
            ; keep
            ; keep
            ; keep
            ; keep
            ; keep
            ]
        }
      in
      let account_precondition =
        var_of_t
          (Zkapp_precondition.Account.typ ())
          Zkapp_precondition.Account.accept
      in
      let ignore = Or_ignore.Checked.make_unsafe Boolean.false_ Field.zero in
      let account_precondition =
        { account_precondition with
          state =
            [ Or_ignore.Checked.make_unsafe Boolean.true_
                (Frozen_ledger_hash.var_to_field stmt.source_ledger)
            ; ignore
            ; ignore
            ; ignore
            ; ignore
            ; ignore
            ; ignore
            ; ignore
            ]
        }
      in
      let preconditions =
        var_of_t
          (Account_update.Preconditions.typ ())
          Account_update.Preconditions.accept
      in
      let preconditions =
        { preconditions with account = account_precondition }
      in
      let account_update =
        { account_update with
          public_key
        ; authorization_kind
        ; update
        ; preconditions
        }
      in
      let account_update_digest =
        Zkapp_command.Call_forest.Digest.Account_update.Checked.create
          account_update
      in
      let calls = Zkapp_call_forest.Checked.empty () in
      let public_output : Zkapp_statement.Checked.t =
        { account_update = (account_update_digest :> Field.t)
        ; calls = (Zkapp_call_forest.Checked.hash calls :> Field.t)
        }
      in
      let auxiliary_output =
        Prover_value.create (fun () ->
            let account_update =
              As_prover.read (Account_update.Body.typ ()) account_update
            in
            let account_update_digest =
              As_prover.read Zkapp_command.Call_forest.Digest.Account_update.typ
                account_update_digest
            in
            let calls = Prover_value.get calls.data in
            (account_update, account_update_digest, calls) )
      in
      { previous_proof_statements =
          [ { public_input = stmt
            ; proof_must_verify = Boolean.true_
            ; proof = prf
            }
          ]
      ; public_output
      ; auxiliary_output
      }

    let rule tag : _ Pickles.Inductive_rule.t =
      { identifier = "Rollup step"
      ; prevs = [ tag ]
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end
end

module Make (T : sig
  val tag : Transaction_snark.tag
end) =
struct
  module Wrapper = struct
    let tag, cache_handle, p, Pickles.Provers.[ wrap_; merge_ ] =
      Pickles.compile () ~cache:Cache_dir.cache ~public_input:(Input S.typ)
        ~auxiliary_typ:Typ.unit
        ~branches:(module Nat.N2)
        ~max_proofs_verified:(module Nat.N2)
        ~name:"zeko wrapper"
        ~constraint_constants:
          (Genesis_constants.Constraint_constants.to_snark_keys_header
             constraint_constants )
        ~choices:(fun ~self ->
          [ Wrapper_rules.Wrap.rule T.tag; Wrapper_rules.Merge.rule self ] )

    let vk = Pickles.Side_loaded.Verification_key.of_compiled tag

    let wrap stmt prf = wrap_ ~handler:(Wrapper_rules.Wrap.handler stmt prf)

    let merge s1 s2 p1 p2 =
      merge_ ~handler:(Wrapper_rules.Merge.handler s1 s2 p1 p2)

    module Proof = (val p)
  end

  module Nested = struct
    let tag, cache_handle, p, Pickles.Provers.[ step_ ] =
      Pickles.compile () ~cache:Cache_dir.cache
        ~public_input:(Output Zkapp_statement.typ)
        ~auxiliary_typ:Typ.(Prover_value.typ ())
        ~branches:(module Nat.N1)
        ~max_proofs_verified:(module Nat.N0)
        ~name:"rollup"
        ~constraint_constants:
          (Genesis_constants.Constraint_constants.to_snark_keys_header
             constraint_constants )
        ~choices:(fun ~self:_ -> [ Nested_rules.Step.rule ])

    let vk = Pickles.Side_loaded.Verification_key.of_compiled tag

    let step pk vk_hash = step_ ~handler:(Nested_rules.Step.handler pk vk_hash)

    module Proof = (val p)
  end

  let tag, cache_handle, p, Pickles.Provers.[ step_ ] =
    Pickles.compile () ~cache:Cache_dir.cache
      ~public_input:(Output Zkapp_statement.typ)
      ~auxiliary_typ:Typ.(Prover_value.typ ())
      ~branches:(module Nat.N1)
      ~max_proofs_verified:(module Nat.N1)
      ~name:"rollup"
      ~constraint_constants:
        (Genesis_constants.Constraint_constants.to_snark_keys_header
           constraint_constants )
      ~choices:(fun ~self:_ -> [ Rules.Step.rule Wrapper.tag ])

  let vk = Pickles.Side_loaded.Verification_key.of_compiled tag

  let step stmt prf pk vk_hash =
    step_ ~handler:(Rules.Step.handler stmt prf pk vk_hash)

  module Proof = (val p)
end
