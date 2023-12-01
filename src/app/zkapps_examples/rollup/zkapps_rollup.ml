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

  module F = struct
    type t = field

    type var = Field.t

    let typ : (var, t) Typ.t = Field.typ
  end

  module MkRef (T : sig
    type t
  end) =
  struct
    type t = T.t

    type var = T.t As_prover.Ref.t

    let typ : (var, t) Typ.t = Typ.Internal.ref ()
  end

  module Proof1 = struct
    type t = (Nat.N1.n, Nat.N1.n) Pickles.Proof.t
  end

  module SnarkList (T : sig
    module T : sig
      type t

      type var

      val typ : (var, t) Typ.t
    end

    val length : int
  end) =
  struct
    type t = T.T.t list

    type var = T.T.var list

    let typ : (var, t) Typ.t = Typ.list ~length:T.length T.T.typ
  end

  module RefProof = MkRef (Proof)
  module RefProof1 = MkRef (Proof1)

  module Boolean = struct
    include Boolean

    type t = bool
  end

  module MkHandler (Witness : sig
    type t

    type var

    val typ : (var, t) Typ.t
  end) =
  struct
    open Snarky_backendless.Request

    open struct
      type _ t += Witness : Witness.t t
    end

    let handler (w : Witness.t) (With { request; respond }) =
      match request with
      | Witness ->
          respond (Provide w)
      | _ ->
          respond Unhandled

    let exists_witness () : Witness.var =
      exists Witness.typ ~request:(fun () -> Witness)
  end

  module Public_key = Signature_lib.Public_key
  module PC = Public_key.Compressed
  module CAS = Currency.Amount.Signed
  module CA = Currency.Amount
end

(* FIXME: Improve performance *)

(** A proof for source0, target0, source1, target1, means the targets are
action states that originate from the sources *)
module Action_state_extension_rule = struct
  module Stmt = struct
    type t = { source0 : F.t; target0 : F.t; source1 : F.t; target1 : F.t }
    [@@deriving snarky]
  end

  module Base = struct
    module Witness = struct
      type t = { source0 : F.t; source1 : F.t } [@@deriving snarky]
    end

    include MkHandler (Witness)

    let main { public_input = () } =
      let Witness.{ source0; source1 } = exists_witness () in
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
    module Witness = struct
      type t =
        { actions0 : Actions.t
        ; actions1 : Actions.t
        ; step0 : Boolean.t
        ; step1 : Boolean.t
        ; prev : Stmt.t
        ; proof : RefProof1.t
        }
      [@@deriving snarky]
    end

    include MkHandler (Witness)

    let main { public_input = () } =
      let Witness.{ actions0; actions1; step0; step1; prev; proof } =
        exists_witness ()
      in
      let target0 =
        Field.if_ step0
          (Actions.push_events_checked prev.target0 actions0)
          prev.target0
      in
      let target1 =
        Field.if_ step1
          (Actions.push_events_checked prev.target1 actions1)
          prev.target1
      in
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
end

(* TODO: Use Zkapp_command_logic here and bypass transaction snark and two pass system *)
module Wrapper_rules = struct
  open struct
    module With_sok = Transaction_snark.Statement.With_sok
  end

  module S = struct
    type t =
      { source_ledger : Frozen_ledger_hash.t
      ; target_ledger : Frozen_ledger_hash.t
      }
    [@@deriving snarky]

    let of_txn_snark_statement (txn_snark : With_sok.t) : t =
      { source_ledger = txn_snark.source.first_pass_ledger
      ; target_ledger = txn_snark.target.second_pass_ledger
      }
  end

  module Snark = struct
    type t = { stmt : S.t; proof : RefProof.t } [@@deriving snarky]
  end

  include Snark

  module Wrap = struct
    module Witness = struct
      module R = MkRef (Transaction_snark)

      type t = { txn_snark : R.t } [@@deriving snarky]
    end

    include MkHandler (Witness)

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

    let%snarkydef_ main { public_input = () } =
      let Witness.{ txn_snark } = exists_witness () in
      let istmt =
        Transaction_snark.(
          exists With_sok.typ ~compute:(fun () ->
              statement_with_sok @@ As_prover.Ref.get txn_snark ))
      in
      let stmt =
        S.(
          exists typ ~compute:(fun () ->
              of_txn_snark_statement @@ As_prover.read With_sok.typ istmt ))
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
            ; proof =
                As_prover.Ref.create (fun () ->
                    Transaction_snark.proof @@ As_prover.Ref.get txn_snark )
            }
          ]
      ; public_output = stmt
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
    module Witness = struct
      type t = { s1 : Snark.t; s2 : Snark.t } [@@deriving snarky]
    end

    include MkHandler (Witness)

    let%snarkydef_ main { public_input = () } =
      let Witness.{ s1; s2 } = exists_witness () in
      let s =
        S.
          { source_ledger = s1.stmt.source_ledger
          ; target_ledger = s2.stmt.target_ledger
          }
      in
      run
      @@ Frozen_ledger_hash.assert_equal s1.stmt.target_ledger
           s2.stmt.source_ledger ;
      { previous_proof_statements =
          [ { public_input = s1.stmt
            ; proof_must_verify = Boolean.true_
            ; proof = s1.proof
            }
          ; { public_input = s2.stmt
            ; proof_must_verify = Boolean.true_
            ; proof = s2.proof
            }
          ]
      ; public_output = s
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
  type t = { amount : CA.t; recipient : PC.t } [@@deriving snarky]

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
  module Witness = struct
    type t =
      { stmt : S.t
      ; prf : RefProof.t
      ; public_key : PC.t
      ; vk_hash : F.t
      ; amount : CA.t
      ; recipient : PC.t
      }
    [@@deriving snarky]
  end

  include MkHandler (Witness)

  let main { public_input = () } =
    let Witness.{ stmt; prf; public_key; vk_hash; amount; recipient } =
      exists_witness ()
    in
    let account_update = Body.(constant (typ ()) dummy) in
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
    let processing_transfers_length = 8

    module TR_8 = struct
      module T = TR

      let length = processing_transfers_length
    end

    module List_TR_8 = SnarkList (TR_8)

    module Witness = struct
      type t =
        { public_key : PC.t
        ; vk_hash : F.t
        ; action_prf : RefProof1.t
        ; all_transfers : F.t
        ; processed_transfers : F.t
        ; trs : List_TR_8.t
        }
      [@@deriving snarky]
    end

    include MkHandler (Witness)

    let main { public_input = () } =
      let Witness.
            { public_key
            ; vk_hash
            ; action_prf
            ; all_transfers
            ; processed_transfers
            ; trs
            } =
        exists_witness ()
      in
      let account_update = constant (Body.typ ()) Body.dummy in
      let authorization_kind : Authorization_kind.Checked.t =
        { is_signed = Boolean.false_
        ; is_proved = Boolean.true_
        ; verification_key_hash = vk_hash
        }
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
    let processing_transfers_length = 8

    module TR_8 = struct
      module T = TR

      let length = processing_transfers_length
    end

    module List_TR_8 = SnarkList (TR_8)

    module Witness = struct
      type t =
        { stmt : S.t
        ; prf : RefProof.t
        ; action_prf : RefProof1.t
        ; public_key : PC.t
        ; vk_hash : F.t
        ; action_state : F.t
        ; processed_transfers : F.t
        ; processing_transfers : List_TR_8.t
        }
      [@@deriving snarky]
    end

    include MkHandler (Witness)

    let main { public_input = () } =
      let ({ stmt
           ; prf
           ; action_prf
           ; public_key
           ; vk_hash
           ; action_state
           ; processed_transfers
           ; processing_transfers = trs
           }
            : Witness.var ) =
        exists_witness ()
      in
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

    let tag, cache_handle, p, Pickles.Provers.[ base_; step_both_ ] =
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
            ~choices:(fun ~self -> [ Base.rule; StepBoth.rule self ]) )

    let vk = Pickles.Side_loaded.Verification_key.of_compiled tag

    let base w = base_ ~handler:(Base.handler w)

    let step_both w = step_both_ ~handler:(StepBoth.handler w)

    module Proof = (val p)
  end

  module Wrapper = struct
    open Wrapper_rules

    let tag, cache_handle, p, Pickles.Provers.[ wrap_; merge_ ] =
      time "Wrapper.compile" (fun () ->
          Pickles.compile ()
            ~override_wrap_domain:Pickles_base.Proofs_verified.N1
            ~cache:Cache_dir.cache ~public_input:(Output S.typ)
            ~auxiliary_typ:Typ.unit
            ~branches:(module Nat.N2)
            ~max_proofs_verified:(module Nat.N2)
            ~name:"zeko wrapper"
            ~constraint_constants:
              (Genesis_constants.Constraint_constants.to_snark_keys_header
                 constraint_constants )
            ~choices:(fun ~self -> [ Wrap.rule T.tag; Merge.rule self ]) )

    let vk = Pickles.Side_loaded.Verification_key.of_compiled tag

    let wrap txn_snark =
      let%bind stmt, _, proof =
        wrap_ ~handler:(Wrap.handler { txn_snark }) ()
      in
      return ({ stmt; proof } : t)

    let merge (s1 : t) (s2 : t) =
      let%bind stmt, _, proof = merge_ ~handler:(Merge.handler { s1; s2 }) () in
      return ({ stmt; proof } : t)

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

    let step w = step_ ~handler:(Inner_rules.Step.handler w)

    module Proof = (val p)
  end

  module Outer = struct
    let tag, cache_handle, p, Pickles.Provers.[ step_; action_ ] =
      time "Zkapp.compile" (fun () ->
          Pickles.compile ()
            ~override_wrap_domain:Pickles_base.Proofs_verified.N1
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

    let step w = step_ ~handler:(Rules.Step.handler w)

    module Proof = (val p)
  end
end
