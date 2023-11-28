(* FIXME: switch to simplified zkapps_examples framework *)

open Core_kernel
open Pickles_types
open Mina_base
open Snark_params
open Tick
open Run
open Pickles.Inductive_rule
open Zkapp_basic
open Account_update

open struct
  module Nonce = Mina_numbers.Account_nonce
  module Local_state = Mina_state.Local_state

  let constraint_constants = Genesis_constants.Constraint_constants.compiled

  module L = Mina_ledger.Ledger
  module T = Transaction_snark.Statement.With_sok

  (* let (!) = run_checked *)

  let var_to_fields (type var value) (typ : (var, value) Typ.t) (x : var) :
      Field.t array =
    let (Typ typ) = typ in
    let fields, aux = typ.var_to_fields x in
    fields

  let to_field (type var value) (typ : (var, value) Typ.t) (x : value) : field =
    let (Typ typ) = typ in
    let fields, aux = typ.value_to_fields x in
    fields.(0)

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

  let make_outputs account_update calls =
    let account_update_digest =
      Zkapp_command.Call_forest.Digest.Account_update.Checked.create
        account_update
    in
    let public_output : Zkapp_statement.Checked.t =
      { account_update = (account_update_digest :> Field.t)
      ; calls = (Zkapp_call_forest.Checked.hash calls :> Field.t)
      }
    in
    let auxiliary_output =
      Prover_value.create (fun () ->
          let account_update = As_prover.read (Body.typ ()) account_update in
          let account_update_digest =
            As_prover.read Zkapp_command.Call_forest.Digest.Account_update.typ
              account_update_digest
          in
          let calls = Prover_value.get calls.data in
          (account_update, account_update_digest, calls) )
    in
    (public_output, auxiliary_output)

  let keep = Set_or_keep.Checked.keep ~dummy:Field.zero

  let ignore = Or_ignore.Checked.make_unsafe Boolean.false_ Field.zero

  let run = run_checked

  module Public_key = Signature_lib.Public_key
  module PC = Public_key.Compressed
  module CAS = Currency.Amount.Signed
  module CA = Currency.Amount
end

(* FIXME: Improve performance *)
module Action_state_extension_rule = struct
  module Stmt = struct
    module Poly = struct
      type 'field t =
        { source0 : 'field
        ; target0 : 'field
        ; source1 : 'field
        ; target1 : 'field
        }
      [@@deriving hlist]
    end

    type t = field Poly.t

    type var = Field.t Poly.t

    let typ : (var, t) Tick.Typ.t =
      let open Poly in
      Tick.Typ.of_hlistable
        [ Field.typ; Field.typ; Field.typ; Field.typ ]
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist

    module Checked = struct
      type t = var
    end
  end

  module Base = struct
    open Snarky_backendless.Request

    type _ t += Source0 : field t | Source1 : field t

    let handler (source0 : field) (source1 : field) (With { request; respond })
        =
      match request with
      | Source0 ->
          respond @@ Provide source0
      | Source1 ->
          respond @@ Provide source1
      | _ ->
          respond Unhandled

    let main { public_input = () } =
      let source0 = exists Field.typ ~request:(fun () -> Source0) in
      let source1 = exists Field.typ ~request:(fun () -> Source1) in
      { previous_proof_statements = []
      ; public_output =
          ({ source0; target0 = source0; source1; target1 = source1 } : Stmt.var)
      ; auxiliary_output = ()
      }

    let rule : _ Pickles.Inductive_rule.t =
      { identifier = "action state extension base"
      ; prevs = []
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end

  module StepBoth = struct
    open Snarky_backendless.Request

    type _ t +=
      | Actions0 : Actions.t t
      | Actions1 : Actions.t t
      | Prev : Stmt.t t
      | Proof : (Nat.N1.n, Nat.N1.n) Pickles.Proof.t t

    let handler (actions0 : Actions.t) (actions1 : Actions.t) (prev : Stmt.t)
        (proof : (Nat.N1.n, Nat.N1.n) Pickles.Proof.t)
        (With { request; respond }) =
      match request with
      | Actions0 ->
          respond @@ Provide actions0
      | Actions1 ->
          respond @@ Provide actions1
      | Prev ->
          respond @@ Provide prev
      | Proof ->
          respond @@ Provide proof
      | _ ->
          respond Unhandled

    let main { public_input = () } =
      let actions0 = exists Actions.typ ~request:(fun () -> Actions0) in
      let actions1 = exists Actions.typ ~request:(fun () -> Actions1) in
      let prev = exists Stmt.typ ~request:(fun () -> Prev) in
      let proof = exists (Typ.Internal.ref ()) ~request:(fun () -> Proof) in
      let target0 = Actions.push_events_checked prev.target0 actions0 in
      let target1 = Actions.push_events_checked prev.target1 actions1 in
      { previous_proof_statements =
          [ { public_input = prev; proof_must_verify = Boolean.true_; proof } ]
      ; public_output = { prev with target0; target1 }
      ; auxiliary_output = ()
      }

    let rule tag : _ Pickles.Inductive_rule.t =
      { identifier = "action state extension step"
      ; prevs = [ tag ]
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end

  module Step0 = struct
    open Snarky_backendless.Request

    type _ t +=
      | Actions0 : Actions.t t
      | Prev : Stmt.t t
      | Proof : (Nat.N1.n, Nat.N1.n) Pickles.Proof.t t

    let handler (actions0 : Actions.t) (prev : Stmt.t)
        (proof : (Nat.N1.n, Nat.N1.n) Pickles.Proof.t)
        (With { request; respond }) =
      match request with
      | Actions0 ->
          respond @@ Provide actions0
      | Prev ->
          respond @@ Provide prev
      | Proof ->
          respond @@ Provide proof
      | _ ->
          respond Unhandled

    let main { public_input = () } =
      let actions0 = exists Actions.typ ~request:(fun () -> Actions0) in
      let prev = exists Stmt.typ ~request:(fun () -> Prev) in
      let proof = exists (Typ.Internal.ref ()) ~request:(fun () -> Proof) in
      let target0 = Actions.push_events_checked prev.target0 actions0 in
      { previous_proof_statements =
          [ { public_input = prev; proof_must_verify = Boolean.true_; proof } ]
      ; public_output = { prev with target0 }
      ; auxiliary_output = ()
      }

    let rule tag : _ Pickles.Inductive_rule.t =
      { identifier = "action state extension step"
      ; prevs = [ tag ]
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end

  module Step1 = struct
    open Snarky_backendless.Request

    type _ t +=
      | Actions1 : Actions.t t
      | Prev : Stmt.t t
      | Proof : (Nat.N1.n, Nat.N1.n) Pickles.Proof.t t

    let handler (actions1 : Actions.t) (prev : Stmt.t)
        (proof : (Nat.N1.n, Nat.N1.n) Pickles.Proof.t)
        (With { request; respond }) =
      match request with
      | Actions1 ->
          respond @@ Provide actions1
      | Prev ->
          respond @@ Provide prev
      | Proof ->
          respond @@ Provide proof
      | _ ->
          respond Unhandled

    let main { public_input = () } =
      let actions1 = exists Actions.typ ~request:(fun () -> Actions1) in
      let prev = exists Stmt.typ ~request:(fun () -> Prev) in
      let proof = exists (Typ.Internal.ref ()) ~request:(fun () -> Proof) in
      let target1 = Actions.push_events_checked prev.target1 actions1 in
      { previous_proof_statements =
          [ { public_input = prev; proof_must_verify = Boolean.true_; proof } ]
      ; public_output = { prev with target1 }
      ; auxiliary_output = ()
      }

    let rule tag : _ Pickles.Inductive_rule.t =
      { identifier = "action state extension step"
      ; prevs = [ tag ]
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end
end

(* TODO: Use Zkapp_command_logic here and bypass transaction snark and two pass system *)
module Wrapper_rules = struct
  module S = struct
    module Poly = struct
      type 'ledger_hash t =
        { source_ledger : 'ledger_hash; target_ledger : 'ledger_hash }
      [@@deriving hlist, yojson]
    end

    type t = Frozen_ledger_hash.t Poly.t [@@deriving yojson]

    type var = Frozen_ledger_hash.var Poly.t

    let typ : (var, t) Tick.Typ.t =
      let open Poly in
      Tick.Typ.of_hlistable
        [ Frozen_ledger_hash.typ; Frozen_ledger_hash.typ ]
        ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist

    module Checked = struct
      type t = var
    end

    let of_txn_snark_statement (txn_snark : T.t) : t =
      { source_ledger = txn_snark.source.first_pass_ledger
      ; target_ledger = txn_snark.target.second_pass_ledger
      }
  end

  type t = { statement : S.t; proof : Proof.t } [@@deriving fields]

  module Wrap = struct
    include struct
      open Snarky_backendless.Request

      type _ t += Statement_to_wrap : T.t t | Proof_to_wrap : Proof.t t
    end

    let handler (stmt : T.t) (prf : Proof.t)
        (Snarky_backendless.Request.With { request; respond }) =
      match request with
      | Statement_to_wrap ->
          respond (Provide stmt)
      | Proof_to_wrap ->
          respond (Provide prf)
      | _ ->
          respond Unhandled

    let dummy_pc_init = Pending_coinbase.Stack.empty

    let genesis_constants = Genesis_constants.compiled

    let consensus_constants =
      Consensus.Constants.create ~constraint_constants
        ~protocol_constants:genesis_constants.protocol

    let dummy_state_body =
      let compile_time_genesis =
        Mina_state.Genesis_protocol_state.t
          ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
          ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
          ~constraint_constants ~consensus_constants
          ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference
      in
      Mina_state.Protocol_state.body compile_time_genesis.data

    let dummy_pc =
      Pending_coinbase.Stack.push_state
        (Mina_state.Protocol_state.Body.hash dummy_state_body)
        Mina_numbers.Global_slot_since_genesis.zero dummy_pc_init

    let%snarkydef_ main { public_input = (stmt : S.Checked.t) } =
      let istmt = exists T.typ ~request:(fun () -> Statement_to_wrap) in
      let prf =
        exists (Typ.Internal.ref ()) ~request:(fun () -> Proof_to_wrap)
      in

      (* Check that istmt and stmt match *)
      run
      @@ Frozen_ledger_hash.assert_equal stmt.source_ledger
           istmt.source.first_pass_ledger ;
      run
      @@ Frozen_ledger_hash.assert_equal stmt.target_ledger
           istmt.target.second_pass_ledger ;

      (* Check that pending_coinbase_stack is correctly set *)
      let dummy_pc = constant Pending_coinbase.Stack.typ dummy_pc in
      Boolean.Assert.is_true @@ run
      @@ Pending_coinbase.Stack.equal_var dummy_pc
           istmt.source.pending_coinbase_stack ;
      Boolean.Assert.is_true @@ run
      @@ Pending_coinbase.Stack.equal_var dummy_pc
           istmt.target.pending_coinbase_stack ;

      (* Check that transactions have been completely applied *)
      let empty_state = Local_state.(constant typ @@ empty ()) in
      Local_state.Checked.assert_equal empty_state istmt.source.local_state ;
      Local_state.Checked.assert_equal empty_state istmt.target.local_state ;

      (* Check that first and second passes are connected *)
      run
      @@ Frozen_ledger_hash.assert_equal istmt.target.first_pass_ledger
           istmt.source.second_pass_ledger ;

      (* Check that it's a complete transaction (a "block") *)
      run
      @@ Frozen_ledger_hash.assert_equal istmt.target.first_pass_ledger
           istmt.connecting_ledger_right ;
      run
      @@ Frozen_ledger_hash.assert_equal istmt.source.second_pass_ledger
           istmt.connecting_ledger_left ;

      (* We don't check fee_excess because it's up to the sequencer what they do with it. *)
      (* The supply however must not increase. *)
      let is_neg =
        Sgn.Checked.is_neg @@ run @@ CAS.Checked.sgn istmt.supply_increase
      in
      let is_zero =
        run
        @@ CA.Checked.equal (constant CA.typ CA.zero)
        @@ run
        @@ CAS.Checked.magnitude istmt.supply_increase
      in
      Boolean.Assert.is_true Boolean.(is_neg || is_zero) ;

      { previous_proof_statements =
          (* Proof for istmt using normal txn snark *)
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
        | Proofs_to_merge : (Proof.t * Proof.t) t
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
      run @@ Frozen_ledger_hash.assert_equal s.source_ledger s1.source_ledger ;
      run @@ Frozen_ledger_hash.assert_equal s1.target_ledger s2.source_ledger ;
      run @@ Frozen_ledger_hash.assert_equal s2.target_ledger s.target_ledger ;
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

module TR = struct
  module Poly = struct
    type ('amount, 'recipient) t = { amount : 'amount; recipient : 'recipient }
    [@@deriving hlist]
  end

  type t = (CA.t, PC.t) Poly.t

  type var = (CA.var, PC.var) Poly.t

  let typ : (var, t) Tick.Typ.t =
    let open Poly in
    Tick.Typ.of_hlistable
      [ Currency.Amount.typ; PC.typ ]
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist

  module Checked = struct
    type t = var
  end

  let dummy : t = { amount = CA.zero; recipient = PC.empty }

  let to_actions (tr : var) : Actions.var =
    let empty_actions =
      exists Zkapp_account.Actions.typ ~compute:(fun () -> [])
    in
    Boolean.Assert.is_true @@ run @@ Actions.is_empty_var empty_actions ;
    let actions =
      Actions.push_to_data_as_hash empty_actions (var_to_fields typ tr)
    in
    actions
end

module Process_transfer = struct
  let process_transfer transfers (transfer : TR.var)
      (calls : Zkapp_call_forest.Checked.t) :
      Field.t * Zkapp_call_forest.Checked.t =
    let transfers' =
      Actions.push_events_checked transfers @@ TR.to_actions transfer
    in
    let account_update = constant (Body.typ ()) Body.dummy in
    let balance_change = CAS.Checked.of_unsigned transfer.amount in
    let public_key = transfer.recipient in
    let account_update =
      { account_update with
        balance_change
      ; public_key
      ; implicit_account_creation_fee = Boolean.true_
      }
    in
    let digest =
      Zkapp_command.Call_forest.Digest.Account_update.Checked.create
        account_update
    in
    let account_update : Zkapp_call_forest.Checked.account_update =
      { account_update = { data = account_update; hash = digest }
      ; control = Prover_value.create (fun () -> Control.None_given)
      }
    in
    let calls' =
      Zkapp_call_forest.Checked.push ~account_update
        ~calls:(Zkapp_call_forest.Checked.empty ())
        calls
    in
    if_
      (run @@ CA.Checked.equal transfer.amount CA.(constant typ zero))
      ~typ:Typ.(Field.typ * Zkapp_call_forest.typ)
      ~then_:(transfers, calls) ~else_:(transfers', calls')
end

module Transfer_action_rule = struct
  include struct
    open Snarky_backendless.Request

    type _ t +=
      | Stmt : S.t t
      | Prf : (Nat.N2.n, Nat.N2.n) Pickles.Proof.t t
      | Public_key : PC.t t
      | Vk_hash : field t
      | Amount : CA.t t
      | Recipient : PC.t t
  end

  let handler (pk : PC.t) (vk_hash : field) (amount : CA.t) (recipient : PC.t)
      (Snarky_backendless.Request.With { request; respond }) =
    match request with
    | Public_key ->
        respond (Provide pk)
    | Vk_hash ->
        respond (Provide vk_hash)
    | Amount ->
        respond (Provide amount)
    | Recipient ->
        respond (Provide recipient)
    | _ ->
        respond Unhandled

  let main { public_input = () } =
    let public_key = exists PC.typ ~request:(fun () -> Public_key) in
    let vk_hash = exists Field.typ ~request:(fun () -> Vk_hash) in
    let amount = exists CA.typ ~request:(fun () -> Amount) in
    let recipient = exists PC.typ ~request:(fun () -> Recipient) in
    let account_update = constant (Body.typ ()) Body.dummy in
    let authorization_kind : Authorization_kind.Checked.t =
      { is_signed = Boolean.false_
      ; is_proved = Boolean.true_
      ; verification_key_hash = vk_hash
      }
    in
    let tr : TR.var = { amount; recipient } in
    let empty_actions =
      exists Zkapp_account.Actions.typ ~compute:(fun () -> [])
    in
    Boolean.Assert.is_true @@ run @@ Actions.is_empty_var empty_actions ;
    let actions =
      Actions.push_to_data_as_hash empty_actions (var_to_fields TR.typ tr)
    in
    let account_update =
      { account_update with
        public_key
      ; authorization_kind
      ; balance_change = CAS.Checked.of_unsigned amount
      ; actions
      }
    in
    let public_output, auxiliary_output =
      make_outputs account_update @@ Zkapp_call_forest.Checked.empty ()
    in
    { previous_proof_statements = []; public_output; auxiliary_output }

  let rule : _ Pickles.Inductive_rule.t =
    { identifier = "Rollup step"
    ; prevs = []
    ; main
    ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
    }
end

module Inner_rules = struct
  module Step = struct
    include struct
      open Snarky_backendless.Request

      let processing_transfers_length = 8

      type _ t +=
        | Public_key : PC.t t
        | Vk_hash : field t
        | Action_prf : (Nat.N1.n, Nat.N1.n) Pickles.Proof.t t
        | All_transfers : field t
        | Processed_transfers : field t
        | Processing_transfers : TR.t list t
      (* has length processing_transfers_length *)
    end

    let handler (pk : PC.t) (vk_hash : field)
        (action_prf : (Nat.N1.n, Nat.N1.n) Pickles.Proof.t)
        (all_transfers : field) (processed_transfers : field)
        (processing_transfers : TR.t list)
        (Snarky_backendless.Request.With { request; respond }) =
      match request with
      | Public_key ->
          respond (Provide pk)
      | Vk_hash ->
          respond (Provide vk_hash)
      | Action_prf ->
          respond (Provide action_prf)
      | All_transfers ->
          respond (Provide all_transfers)
      | Processed_transfers ->
          respond (Provide processed_transfers)
      | Processing_transfers ->
          respond (Provide processing_transfers)
      | _ ->
          respond Unhandled

    let main { public_input = () } =
      let public_key = exists PC.typ ~request:(fun () -> Public_key) in
      let vk_hash = exists Field.typ ~request:(fun () -> Vk_hash) in
      let account_update = constant (Body.typ ()) Body.dummy in
      let authorization_kind : Authorization_kind.Checked.t =
        { is_signed = Boolean.false_
        ; is_proved = Boolean.true_
        ; verification_key_hash = vk_hash
        }
      in
      let action_prf =
        exists (Typ.Internal.ref ()) ~request:(fun () -> Action_prf)
      in
      let all_transfers = exists Field.typ ~request:(fun () -> All_transfers) in
      let processed_transfers =
        exists Field.typ ~request:(fun () -> Processed_transfers)
      in
      let trs =
        exists (Typ.list ~length:processing_transfers_length TR.typ)
          ~request:(fun () -> Processing_transfers)
      in
      let calls = Zkapp_call_forest.Checked.empty () in
      (* NOTE: processed_transfers' must be a prefix of all_transfers, checked using action_prf. *)
      let processed_transfers', calls =
        List.fold trs ~init:(processed_transfers, calls)
          ~f:(fun (transfers, calls) tr ->
            Process_transfer.process_transfer transfers tr calls )
      in
      let update = account_update.update in
      let update =
        { update with
          app_state =
            [ Set_or_keep.Checked.set all_transfers
            ; Set_or_keep.Checked.set processed_transfers'
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
        constant
          (Zkapp_precondition.Account.typ ())
          Zkapp_precondition.Account.accept
      in
      let account_precondition =
        { account_precondition with
          state =
            [ ignore
            ; Or_ignore.Checked.make_unsafe Boolean.true_ processed_transfers
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
        constant (Preconditions.typ ()) Preconditions.accept
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
        ; increment_nonce =
            Boolean.true_
            (* NOTE: Nonce **must** be incremented for soundness! *)
        }
      in
      let public_output, auxiliary_output = make_outputs account_update calls in
      { previous_proof_statements =
          [ { public_input =
                ({ source0 = processed_transfers'
                 ; target0 = all_transfers
                 ; source1 = processed_transfers'
                 ; target1 = all_transfers
                 } : Action_state_extension_rule.Stmt.var)
            ; proof_must_verify = Boolean.true_
            ; proof = action_prf
            }
          ]
      ; public_output
      ; auxiliary_output
      }

    let rule tag : _ Pickles.Inductive_rule.t =
      { identifier = "Rollup special account step"
      ; prevs = [ tag ]
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end
end

module Deploy = struct
  let deploy (public_key : PC.t) (vk : Side_loaded_verification_key.t)
      (inner_vk : Side_loaded_verification_key.t) : L.t * Update.t =
    let l = L.create ~depth:constraint_constants.ledger_depth () in
    let inner_pk =
      PC.of_base58_check_exn
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
    in
    L.create_new_account_exn l
      (Account_id.of_public_key @@ Public_key.decompress_exn inner_pk)
      { Account.empty with
        public_key = inner_pk
      ; balance = Currency.Balance.max_int
      ; permissions = proof_permissions
      ; zkapp =
          Some
            { Zkapp_account.default with
              app_state =
                [ Actions.empty_hash (* actions from outer account *)
                ; Actions.empty_hash (* actions from outer account processed *)
                ; Field.Constant.zero
                ; Field.Constant.zero
                ; Field.Constant.zero
                ; Field.Constant.zero
                ; Field.Constant.zero
                ; Field.Constant.zero
                ]
            ; verification_key =
                Some (Verification_key_wire.Stable.Latest.M.of_binable inner_vk)
            }
      } ;
    if not (PC.equal inner_pk (L.get_at_index_exn l 0).public_key) then
      failwith "bug"
    else () ;
    let state_0 = L.merkle_root l in
    let update =
      { Update.dummy with
        app_state =
          [ Set (Frozen_ledger_hash.to_field (L.merkle_root l))
            (* ledger merkle root *)
          ; Set Actions.empty_hash (* actions from inner account *)
          ; Set Actions.empty_hash (* actions from inner account processed *)
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

      let processing_transfers_length = 8

      type _ t +=
        | Stmt : S.t t
        | Prf : Proof.t t
        | Action_prf : (Nat.N1.n, Nat.N1.n) Pickles.Proof.t t
        | Public_key : PC.t t
        | Vk_hash : field t
        | Action_state : field t
        | Processed_transfers : field t
        | Processing_transfers : TR.t list t
      (* has length processing_transfers_length *)
    end

    let handler (stmt : S.t) (prf : Proof.t) (action_prf : _ Pickles.Proof.t)
        (pk : PC.t) (vk_hash : field) (action_state : field)
        (processed_transfers : field) (processing_transfers : TR.t list)
        (Snarky_backendless.Request.With { request; respond }) =
      match request with
      | Stmt ->
          respond (Provide stmt)
      | Prf ->
          respond (Provide prf)
      | Action_prf ->
          respond (Provide action_prf)
      | Public_key ->
          respond (Provide pk)
      | Vk_hash ->
          respond (Provide vk_hash)
      | Action_state ->
          respond (Provide action_state)
      | Processed_transfers ->
          respond (Provide processed_transfers)
      | Processing_transfers ->
          respond (Provide processing_transfers)
      | _ ->
          respond Unhandled

    let main { public_input = () } =
      let stmt = exists S.typ ~request:(fun () -> Stmt) in
      let prf = exists (Typ.Internal.ref ()) ~request:(fun () -> Prf) in
      let public_key = exists PC.typ ~request:(fun () -> Public_key) in
      let vk_hash = exists Field.typ ~request:(fun () -> Vk_hash) in
      let action_state = exists Field.typ ~request:(fun () -> Action_state) in
      let depth = constraint_constants.ledger_depth in
      let acc = constant (Account.Index.Unpacked.typ depth) 0 in
      let old_acc =
        run @@ Frozen_ledger_hash.get depth stmt.source_ledger acc
      in
      let new_acc =
        run @@ Frozen_ledger_hash.get depth stmt.target_ledger acc
      in
      let () =
        run
        @@ Nonce.Checked.Assert.equal
             (run @@ Nonce.Checked.succ old_acc.nonce)
             new_acc.nonce
      in
      let old_zkapp_hash, old_zkapp = old_acc.zkapp in
      let old_zkapp =
        exists Zkapp_account.typ ~compute:(fun () ->
            Option.value_exn @@ As_prover.Ref.get old_zkapp )
      in
      let zkapp_hash, zkapp = new_acc.zkapp in
      let zkapp =
        exists Zkapp_account.typ ~compute:(fun () ->
            Option.value_exn @@ As_prover.Ref.get zkapp )
      in
      Field.Assert.equal zkapp_hash @@ Zkapp_account.Checked.digest zkapp ;
      (* NOTE: outer_action_state_in_inner must be extension of old_outer_action_state_in_inner,
         that is, we synchronise the outer action state to the inner account but make sure that it only goes forward,
         since the action state precondition can actually be one of the last 5 states, meaning you could otherwise
         go backward and perhaps break something *)
      let (old_outer_action_state_in_inner :: _) = old_zkapp.app_state in
      let (outer_action_state_in_inner :: _) = zkapp.app_state in
      let action_prf =
        exists (Typ.Internal.ref ()) ~request:(fun () -> Action_prf)
      in
      Field.Assert.equal outer_action_state_in_inner action_state ;
      let account_update = constant (Body.typ ()) Body.dummy in
      let authorization_kind : Authorization_kind.Checked.t =
        { is_signed = Boolean.false_
        ; is_proved = Boolean.true_
        ; verification_key_hash = vk_hash
        }
      in
      let update = account_update.update in
      let (all_transfers :: _) = zkapp.action_state in
      let processed_transfers =
        exists Field.typ ~request:(fun () -> Processed_transfers)
      in
      let trs =
        exists (Typ.list ~length:processing_transfers_length TR.typ)
          ~request:(fun () -> Processing_transfers)
      in
      let calls = Zkapp_call_forest.Checked.empty () in
      (* NOTE: processed_transfers' must be a prefix of all_transfers, checked also using action_prf. *)
      let processed_transfers', calls =
        List.fold trs ~init:(processed_transfers, calls)
          ~f:(fun (transfers, calls) tr ->
            Process_transfer.process_transfer transfers tr calls )
      in
      let update =
        { update with
          app_state =
            [ Set_or_keep.Checked.set
              @@ Frozen_ledger_hash.var_to_field stmt.target_ledger
            ; Set_or_keep.Checked.set all_transfers
            ; Set_or_keep.Checked.set processed_transfers'
            ; keep
            ; keep
            ; keep
            ; keep
            ; keep
            ]
        }
      in
      let account_precondition =
        constant
          (Zkapp_precondition.Account.typ ())
          Zkapp_precondition.Account.accept
      in
      let account_precondition =
        { account_precondition with
          state =
            [ Or_ignore.Checked.make_unsafe Boolean.true_
                (Frozen_ledger_hash.var_to_field stmt.source_ledger)
            ; ignore
            ; Or_ignore.Checked.make_unsafe Boolean.true_ processed_transfers
            ; ignore
            ; ignore
            ; ignore
            ; ignore
            ; ignore
            ]
        ; action_state =
            Or_ignore.Checked.make_unsafe Boolean.true_ action_state
        }
      in
      let preconditions =
        constant (Preconditions.typ ()) Preconditions.accept
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
      let public_output, auxiliary_output = make_outputs account_update calls in
      { previous_proof_statements =
          [ { public_input = stmt
            ; proof_must_verify = Boolean.true_
            ; proof = prf
            }
          ; { public_input =
                ({ source0 = old_outer_action_state_in_inner
                 ; target0 = outer_action_state_in_inner
                 ; source1 = processed_transfers'
                 ; target1 = all_transfers
                 } : Action_state_extension_rule.Stmt.var)
            ; proof_must_verify = Boolean.true_
            ; proof = action_prf
            }
          ]
      ; public_output
      ; auxiliary_output
      }

    let rule tag action_tag : _ Pickles.Inductive_rule.t =
      { identifier = "Rollup step"
      ; prevs = [ tag; action_tag ]
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end
end

module Make (T : sig
  val tag : Transaction_snark.tag
end) =
struct
  open Async_kernel

  let time lab f =
    let start = Time.now () in
    let x = f () in
    let stop = Time.now () in
    printf "%s: %s\n%!" lab (Time.Span.to_string_hum (Time.diff stop start)) ;
    x

  module Action_state_extension = struct
    open Action_state_extension_rule

    let ( tag
        , cache_handle
        , p
        , Pickles.Provers.[ base_; step_both_; step0_; step1_ ] ) =
      time "Action_state_extension.compile" (fun () ->
          Pickles.compile ()
            ~override_wrap_domain:Pickles_base.Proofs_verified.N1
            ~cache:Cache_dir.cache ~public_input:(Output Stmt.typ)
            ~auxiliary_typ:Typ.unit
            ~branches:(module Nat.N4)
            ~max_proofs_verified:(module Nat.N1)
            ~name:"action state extension"
            ~constraint_constants:
              (Genesis_constants.Constraint_constants.to_snark_keys_header
                 constraint_constants )
            ~choices:(fun ~self ->
              [ Base.rule
              ; StepBoth.rule self
              ; Step0.rule self
              ; Step1.rule self
              ] ) )

    let vk = Pickles.Side_loaded.Verification_key.of_compiled tag

    let base source0 source1 = base_ ~handler:(Base.handler source0 source1)

    let step_both actions0 actions1 prev proof =
      step_both_ ~handler:(StepBoth.handler actions0 actions1 prev proof)

    let step0 actions0 prev proof =
      step0_ ~handler:(Step0.handler actions0 prev proof)

    let step1 actions1 prev proof =
      step1_ ~handler:(Step1.handler actions1 prev proof)

    module Proof = (val p)
  end

  module Wrapper = struct
    open Wrapper_rules

    let tag, cache_handle, p, Pickles.Provers.[ wrap_; merge_ ] =
      time "Wrapper.compile" (fun () ->
          Pickles.compile ()
            ~override_wrap_domain:Pickles_base.Proofs_verified.N1
            ~cache:Cache_dir.cache ~public_input:(Input S.typ)
            ~auxiliary_typ:Typ.unit
            ~branches:(module Nat.N2)
            ~max_proofs_verified:(module Nat.N2)
            ~name:"zeko wrapper"
            ~constraint_constants:
              (Genesis_constants.Constraint_constants.to_snark_keys_header
                 constraint_constants )
            ~choices:(fun ~self -> [ Wrap.rule T.tag; Merge.rule self ]) )

    let vk = Pickles.Side_loaded.Verification_key.of_compiled tag

    let wrap (txn_snark : Transaction_snark.t) : t Deferred.t =
      let statement = Transaction_snark.statement_with_sok txn_snark in
      let proof = Transaction_snark.proof txn_snark in
      let wrapped_statement = S.of_txn_snark_statement statement in
      let%bind (), (), wrapped_proof =
        wrap_ ~handler:(Wrap.handler statement proof) wrapped_statement
      in
      return { statement = wrapped_statement; proof = wrapped_proof }

    let merge (p1 : t) (p2 : t) =
      let wrapped_statement : S.t =
        { source_ledger = p1.statement.source_ledger
        ; target_ledger = p2.statement.target_ledger
        }
      in
      let%bind (), (), wrapped_proof =
        merge_
          ~handler:(Merge.handler p1.statement p2.statement p1.proof p2.proof)
          wrapped_statement
      in
      return { statement = wrapped_statement; proof = wrapped_proof }

    module Proof = (val p)
  end

  module Inner = struct
    let tag, cache_handle, p, Pickles.Provers.[ step_; action_ ] =
      time "Inner.compile" (fun () ->
          Pickles.compile () ~cache:Cache_dir.cache
            ~public_input:(Output Zkapp_statement.typ)
            ~auxiliary_typ:Typ.(Prover_value.typ ())
            ~branches:(module Nat.N2)
            ~max_proofs_verified:(module Nat.N1)
            ~name:"rollup inner account"
            ~constraint_constants:
              (Genesis_constants.Constraint_constants.to_snark_keys_header
                 constraint_constants )
            ~choices:(fun ~self:_ ->
              [ Inner_rules.Step.rule Action_state_extension.tag
              ; Transfer_action_rule.rule
              ] ) )

    let vk = Pickles.Side_loaded.Verification_key.of_compiled tag

    let step pk vk_hash action_prf all processed processing =
      step_
        ~handler:
          (Inner_rules.Step.handler pk vk_hash action_prf all processed
             processing )

    module Proof = (val p)
  end

  let tag, cache_handle, p, Pickles.Provers.[ step_; action_ ] =
    time "Zkapp.compile" (fun () ->
        Pickles.compile () ~override_wrap_domain:Pickles_base.Proofs_verified.N1
          ~cache:Cache_dir.cache ~public_input:(Output Zkapp_statement.typ)
          ~auxiliary_typ:Typ.(Prover_value.typ ())
          ~branches:(module Nat.N2)
          ~max_proofs_verified:(module Nat.N2)
          ~name:"rollup"
          ~constraint_constants:
            (Genesis_constants.Constraint_constants.to_snark_keys_header
               constraint_constants )
          ~choices:(fun ~self:_ ->
            [ Rules.Step.rule Wrapper.tag Action_state_extension.tag
            ; Transfer_action_rule.rule
            ] ) )

  let vk = Pickles.Side_loaded.Verification_key.of_compiled tag

  let step stmt prf action_prf pk vk_hash action_state processed_transfers
      processing_transfers =
    step_
      ~handler:
        (Rules.Step.handler stmt prf action_prf pk vk_hash action_state
           processed_transfers processing_transfers )

  module Proof = (val p)
end
