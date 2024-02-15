open Core_kernel
open Mina_base
open Snark_params.Tick.Run
(* Impure interface to snarky, FIXME: replace with pure one *)

open Zkapp_basic
open Account_update
module Nat = Pickles_types.Nat
module Nonce = Mina_numbers.Account_nonce
module Local_state = Mina_state.Local_state

let constraint_constants = Genesis_constants.Constraint_constants.compiled

module L = Mina_ledger.Ledger
module SL = Mina_ledger.Sparse_ledger

(** Converts a variable to its constituent fields *)
let var_to_fields (type var value) (typ : (var, value) Typ.t) (x : var) :
    Field.t array =
  let (Typ typ) = typ in
  let fields, aux = typ.var_to_fields x in
  fields

(** Default permissions for deployments *)
let proof_permissions : Permissions.t =
  { edit_state = Proof
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
  ; access = None
  }

(** Given calls the zkapp wishes to make, constructs output that can be used to construct a full account update *)
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

(** Takes output from make_outputs and makes it usable *)
let mkforest (account_update, account_update_digest, calls) proof =
  let account_update : Account_update.t =
    { body = account_update
    ; authorization = Proof (Pickles.Side_loaded.Proof.of_proof proof)
    }
  in
  Zkapp_command.Call_forest.cons_tree
    { account_update; account_update_digest; calls }
    []

(** A shorthand function to keep a field in the update for the app state *)
let keep = Set_or_keep.Checked.keep ~dummy:Field.zero

(** A shorthand function to ignore a field in the precondition for the app state *)
let ignore = Or_ignore.Checked.make_unsafe Boolean.false_ Field.zero

(** Generic function for turning variables into 8 of something  *)
let var_to_state (some : Field.t -> 'option) (none : 'option)
    (typ : ('var, 'value) Typ.t) (x : 'var) : 'option Zkapp_state.V.t =
  let fields = var_to_fields typ x in
  assert (Array.length fields <= 8) ;
  let missing = 8 - Array.length fields in
  Zkapp_state.V.of_list_exn
  @@ List.append
       (List.map ~f:(fun f -> some f) @@ Array.to_list fields)
       (List.init missing ~f:(fun _ -> none))

(** Used for turning variables into app state for updates *)
let var_to_app_state typ x =
  var_to_state Set_or_keep.Checked.set
    (Set_or_keep.Checked.keep ~dummy:Field.zero)
    typ x

(** Used for turning variables into preconditions *)
let var_to_precondition_state typ x =
  var_to_state
    (Or_ignore.Checked.make_unsafe Boolean.true_)
    (Or_ignore.Checked.make_unsafe Boolean.false_ Field.zero)
    typ x

(** Same as var_to_state but for values *)
let value_to_state (some : field -> 'option) (none : 'option)
    (typ : ('var, 'value) Typ.t) (x : 'value) : 'option Zkapp_state.V.t =
  let (Typ typ) = typ in
  let fields, aux = typ.value_to_fields x in
  assert (Array.length fields <= 8) ;
  let missing = 8 - Array.length fields in
  Zkapp_state.V.of_list_exn
  @@ List.append
       (List.map ~f:(fun f -> some f) @@ Array.to_list fields)
       (List.init missing ~f:(fun _ -> none))

(** "id" transformation, but if less than 8 fields, rest are turned into zero *)
let value_to_init_state typ x =
  value_to_state (fun f -> f) Field.Constant.zero typ x

let value_to_app_state typ x =
  value_to_state (fun f -> Set_or_keep.Set f) Set_or_keep.Keep typ x

let var_to_actions (typ : ('var, 'value) Typ.t) (x : 'var) : Actions.var =
  let empty_actions = Zkapp_account.Actions.(constant typ []) in
  let actions =
    Actions.push_to_data_as_hash empty_actions (var_to_fields typ x)
  in
  actions

let value_to_actions (typ : ('var, 'value) Typ.t) (x : 'value) : Actions.t =
  let (Typ typ) = typ in
  let fields, _ = typ.value_to_fields x in
  [ fields ]

(** FIXME: Very commonly used, but should be replaced by monadic style *)
let run = run_checked

(** To be used with deriving snarky, a simple field *)
module F = struct
  type t = field

  type var = Field.t

  let typ : (var, t) Typ.t = Field.typ

  let pp : Format.formatter -> field -> unit =
   fun fmt f -> Format.pp_print_string fmt @@ Field.Constant.to_string f
end

(** To be used with deriving snarky, a reference to T with no in-circuit representation *)
module MkRef (T : sig
  type t
end) =
struct
  type t = T.t

  type var = T.t As_prover.Ref.t

  let typ : (var, t) Typ.t = Typ.Internal.ref ()
end

(** A list of `length` `t`s *)
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

(** Reference to Proof  *)
module RefProof = MkRef (Proof)

(** Boolean but monkey-patched to have `t`*)
module Boolean = struct
  include Boolean

  type t = bool
end

(** Helper to construct snarky handler from type *)
module MkHandler (Witness : sig
  type t

  type var

  val typ : (var, t) Typ.t
end) =
struct
  open Snarky_backendless.Request

  type _ t += Witness : Witness.t t

  let handler (w : Witness.t) (With { request; respond }) =
    match request with Witness -> respond (Provide w) | _ -> respond Unhandled

  let exists_witness () : Witness.var =
    exists Witness.typ ~request:(fun () -> Witness)
end

module Public_key = Signature_lib.Public_key
module PC = Public_key.Compressed
module CAS = Currency.Amount.Signed
module CA = Currency.Amount

(** A proof for source0, target0, source1, target1, means the targets are
action states that originate from the sources *)
module Action_state_extension_rule = struct
  module Stmt = struct
    type t = { source0 : F.t; target0 : F.t; source1 : F.t; target1 : F.t }
    [@@deriving show, snarky]

    let zero : t =
      { source0 = Field.Constant.zero
      ; source1 = Field.Constant.zero
      ; target0 = Field.Constant.zero
      ; target1 = Field.Constant.zero
      }
  end

  (** Base case, source equal to target *)
  module Base = struct
    module Witness = struct
      type t = { source0 : F.t; source1 : F.t } [@@deriving snarky]
    end

    include MkHandler (Witness)

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
      let Witness.{ source0; source1 } = exists_witness () in
      Pickles.Inductive_rule.
        { previous_proof_statements = []
        ; public_output =
            ({ source0; target0 = source0; source1; target1 = source1 } : Stmt
                                                                          .var)
        ; auxiliary_output = ()
        }

    let rule : _ Pickles.Inductive_rule.t =
      { identifier = "action state extension base"
      ; prevs = []
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end

  (** Step case, can step once forward in either, both, or neither fields *)
  module StepBoth = struct
    module Witness = struct
      type t =
        { actions0 : Actions.t
        ; actions1 : Actions.t
        ; step0 : Boolean.t
        ; step1 : Boolean.t
        ; prev : Stmt.t
        ; proof : RefProof.t
        }
      [@@deriving snarky]
    end

    include MkHandler (Witness)

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
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
      Pickles.Inductive_rule.
        { previous_proof_statements =
            [ { public_input = prev; proof_must_verify = Boolean.true_; proof }
            ]
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

  module Merge = struct
    module Witness = struct
      type t =
        { left : Stmt.t
        ; right : Stmt.t
        ; left_proof : RefProof.t
        ; right_proof : RefProof.t
        }
      [@@deriving snarky]
    end

    include MkHandler (Witness)

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
      let Witness.{ left; right; left_proof; right_proof } =
        exists_witness ()
      in
      Pickles.Inductive_rule.
        { previous_proof_statements =
            [ { public_input = left
              ; proof_must_verify = Boolean.true_
              ; proof = left_proof
              }
            ; { public_input = right
              ; proof_must_verify = Boolean.true_
              ; proof = right_proof
              }
            ]
        ; public_output =
            ({ source0 = left.source0
             ; source1 = right.source1
             ; target0 = left.target0
             ; target1 = right.target1
             } : Stmt.var)
        ; auxiliary_output = ()
        }

    let rule tag : _ Pickles.Inductive_rule.t =
      { identifier = "action state extension step"
      ; prevs = [ tag; tag ]
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end
end

(** Rules for wrapping txn snarks, necessary because we don't use passes
 TODO: Use Zkapp_command_logic here and bypass transaction snark and two pass system *)
module Wrapper_rules = struct
  module With_sok = Transaction_snark.Statement.With_sok

  (** Statement for this *)
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
    (** Akin to Transaction_snark.t  *)
    type t = { stmt : S.t; proof : RefProof.t } [@@deriving snarky]
  end

  include Snark

  (** Base case, wraps a complete txn snark, morally wraps a block *)
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

    (** Dummy state body, network preconditions are disabled anyway *)
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

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
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

      Pickles.Inductive_rule.
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

  (** Merges two statements that line up *)
  module Merge = struct
    module Witness = struct
      type t = { s1 : Snark.t; s2 : Snark.t } [@@deriving snarky]
    end

    include MkHandler (Witness)

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
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
      Pickles.Inductive_rule.
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

module S = Wrapper_rules.S

(** The type for "transfer requests" put in actions *)
module TR = struct
  type t = { amount : CA.t; recipient : PC.t } [@@deriving snarky]

  let dummy : t = { amount = CA.zero; recipient = PC.empty }

  let is_dummy (t : t) = CA.(equal t.amount zero)
end

(** Partial circuit for processing a single transfer request *)
module Process_transfer = struct
  let%snarkydef_ process_transfer transfers (transfer : TR.var)
      (calls : Zkapp_call_forest.Checked.t) :
      Field.t * Zkapp_call_forest.Checked.t =
    (* Transfers including our new transfer *)
    let transfers' =
      Actions.push_events_checked transfers TR.(var_to_actions typ transfer)
    in
    (* Init child account update *)
    let account_update = Body.(constant (typ ()) dummy) in
    let account_update =
      { account_update with
        (* Send amount money *)
        balance_change =
          CAS.Checked.of_unsigned transfer.amount (* to public_key *)
      ; public_key =
          transfer.recipient (* and take creation fee implicitly if necessary *)
      ; implicit_account_creation_fee = Boolean.true_
      }
    in
    (* Digest child account update *)
    let digest =
      Zkapp_command.Call_forest.Digest.Account_update.Checked.create
        account_update
    in
    (* Attach digest to body *)
    let account_update : Zkapp_call_forest.Checked.account_update =
      { account_update = { data = account_update; hash = digest }
      ; control = Prover_value.create (fun () -> Control.None_given)
      }
    in
    (* Update calls to child account updates *)
    let calls' =
      Zkapp_call_forest.Checked.push ~account_update
        ~calls:(Zkapp_call_forest.Checked.empty ())
        calls
    in
    (* Only do all of this if TR isn't the dummy TR *)
    if_
      (run @@ CA.Checked.equal transfer.amount CA.(constant typ zero))
      ~typ:Typ.(Field.typ * Zkapp_call_forest.typ)
      ~then_:(transfers, calls) ~else_:(transfers', calls')
end

(** Rule used by both inner and outer accounts to validate actions posted to account *)
module Transfer_action_rule = struct
  module Witness = struct
    type t =
      { public_key : PC.t; vk_hash : F.t; amount : CA.t; recipient : PC.t }
    [@@deriving snarky]
  end

  include MkHandler (Witness)

  let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
    let Witness.{ public_key; vk_hash; amount; recipient } =
      exists_witness ()
    in
    (* The amount and the recipient must not be zero, because then it's a dummy *)
    ( Boolean.Assert.is_true @@ Boolean.not @@ run
    @@ CA.(Checked.equal amount (constant typ zero)) ) ;
    ( Boolean.Assert.is_true @@ Boolean.not @@ run
    @@ PC.(Checked.equal recipient (constant typ empty)) ) ;
    let account_creation_fee =
      CA.(constant typ @@ of_fee constraint_constants.account_creation_fee)
    in
    (* We increase amount by the fee necessary to create an account, to ensure
       we can always make an account if it doesn't already exist *)
    let amount = run @@ CA.Checked.add amount account_creation_fee in
    let account_update = Body.(constant (typ ()) dummy) in
    let authorization_kind : Authorization_kind.Checked.t =
      { is_signed = Boolean.false_
      ; is_proved = Boolean.true_
      ; verification_key_hash = vk_hash
      }
    in
    (* The amount will go the recipient on the other side *)
    let tr : TR.var = { amount; recipient } in
    (* We add a single action which is the above amount and recipient *)
    let empty_actions = Zkapp_account.Actions.(constant typ []) in
    let actions =
      Actions.push_to_data_as_hash empty_actions (var_to_fields TR.typ tr)
    in
    let account_update =
      { account_update with
        public_key
      ; authorization_kind
      ; balance_change = CAS.Checked.of_unsigned @@ amount
      ; actions
      }
    in
    let public_output, auxiliary_output =
      make_outputs account_update @@ Zkapp_call_forest.Checked.empty ()
    in
    Pickles.Inductive_rule.
      { previous_proof_statements = []; public_output; auxiliary_output }

  let rule : _ Pickles.Inductive_rule.t =
    { identifier = "Rollup step"
    ; prevs = []
    ; main
    ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
    }
end

(** The rules for the inner account zkapp, that controls the money supply and transfers on the rollup *)
module Inner_rules = struct
  let public_key =
    let pk =
      Snark_params.Tick.Inner_curve.(
        to_affine_exn @@ point_near_x @@ Field.Constant.of_int 123456789)
    in
    Public_key.compress pk

  module State = struct
    (* NB! Don't change this, code depends on the layout, sorry *)
    type t =
      { all_deposits : F.t
            (** All deposits that have been made on the L1. We must pay them out on the L2. *)
      ; deposits_processed : F.t
            (** All deposits that have been processed successfully. *)
      }
    [@@deriving snarky]
  end

  module Step = struct
    module TR_8 = struct
      module T = TR

      let length = 8
    end

    module List_TR_8 = SnarkList (TR_8)

    module Witness = struct
      type t =
        { vk_hash : F.t
        ; action_prf : RefProof.t
              (** action_prf is a proof for Action_state_extension_rule that all_deposits
           is an extension of deposits_processed + new_deposits *)
        ; all_deposits : F.t
        ; deposits_processed : F.t
        ; deposits_processed' : F.t
        ; new_deposits : List_TR_8.t
              (** Deposits that have yet to be processed *)
        }
      [@@deriving snarky]
    end

    include MkHandler (Witness)

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
      let Witness.
            { vk_hash
            ; action_prf
            ; all_deposits
            ; deposits_processed
            ; deposits_processed'
            ; new_deposits
            } =
        exists_witness ()
      in
      (* Init account update *)
      let account_update = Body.(constant (typ ()) dummy) in
      (* We are authorized by a proof *)
      let authorization_kind : Authorization_kind.Checked.t =
        { is_signed = Boolean.false_
        ; is_proved = Boolean.true_
        ; verification_key_hash = vk_hash
        }
      in
      (* Init calls *)
      let calls = Zkapp_call_forest.Checked.empty () in
      (* Process deposits, add a call per deposit *)
      let deposits_processed'', calls =
        List.fold new_deposits ~init:(deposits_processed, calls)
          ~f:(fun (deposits, calls) tr ->
            Process_transfer.process_transfer deposits tr calls )
      in
      let stmt =
        ( { source0 = deposits_processed'
          ; target0 = all_deposits
          ; source1 = deposits_processed'
          ; target1 = all_deposits
          }
          : Action_state_extension_rule.Stmt.var )
      in
      let (_ : unit Prover_value.t) =
        Prover_value.create (fun () ->
            printf "Verifying proof for %s\n"
              Action_state_extension_rule.Stmt.(show (As_prover.read typ stmt)) ;
            printf "deposits_processed: %s\n"
              (Field.Constant.to_string (As_prover.read_var deposits_processed)) ;
            printf "deposits_processed': %s\n"
              (Field.Constant.to_string
                 (As_prover.read_var deposits_processed') ) ;
            printf "deposits_processed'': %s\n"
              (Field.Constant.to_string
                 (As_prover.read_var deposits_processed'') ) )
      in
      with_label __LOC__ (fun () ->
          Field.Assert.equal deposits_processed' deposits_processed'' ) ;
      (* Set all_deposits and deposits_processed' *)
      let update =
        { account_update.update with
          app_state =
            State.(
              var_to_app_state typ
                ( { all_deposits; deposits_processed = deposits_processed' }
                  : var ))
        }
      in
      (* Set account precondition such that deposits_processed must be correct.
         We don't care about the previous all_deposits. *)
      let account_precondition =
        { Zkapp_precondition.Account.(constant (typ ()) accept) with
          state =
            [ ignore
            ; Or_ignore.Checked.make_unsafe Boolean.true_ deposits_processed
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
        { Preconditions.(constant (typ ()) accept) with
          account = account_precondition
        }
      in
      let account_update =
        { account_update with
          public_key = constant PC.typ public_key
        ; authorization_kind
        ; update
        ; preconditions
        ; increment_nonce =
            Boolean.true_
            (* We increment the nonce to signal that a step has been performed to the outer account *)
        }
      in
      let public_output, auxiliary_output = make_outputs account_update calls in
      Pickles.Inductive_rule.
        { previous_proof_statements =
            [ { public_input = stmt
              ; proof_must_verify = Boolean.true_
              ; proof = action_prf
              }
              (* We check that all_deposits is an extension of deposits_processed',
                 which inherently must be an extension of deposits_processed,
                 ensuring continuity and soundness.
                 We duplicate the check since the proof checks two at the same time. *)
            ]
        ; public_output
        ; auxiliary_output
        }

    let rule tag : _ Pickles.Inductive_rule.t =
      { identifier = "Rollup inner account step"
      ; prevs = [ tag ]
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end
end

(** Rules for outer account zkapp *)
module Outer_rules = struct
  module State = struct
    (* NB! Don't change this, code depends on the layout, sorry *)
    type t =
      { ledger_hash : Frozen_ledger_hash.t  (** The ledger hash of the rollup *)
      ; all_withdrawals : F.t
            (** All withdrawals registered on the L2. We must pay them out on the L1. *)
      ; withdrawals_processed : F.t
            (** All withdrawals successfully processed. *)
      }
    [@@deriving snarky]
  end

  module Step = struct
    module TR_8 = struct
      module T = TR

      let length = 8
    end

    module List_TR_8 = SnarkList (TR_8)

    module Ledger_path = SnarkList (struct
      module T = F

      let length = constraint_constants.ledger_depth
    end)

    module Witness = struct
      type t =
        { stmt : S.t  (** The ledger transition we are performing. *)
        ; prf : RefProof.t  (** Proof of ledger transition being valid. *)
        ; action_prf : RefProof.t  (** Proof that actions moved forward. *)
        ; public_key : PC.t  (** Our public key on the L2 *)
        ; vk_hash : F.t  (** Our vk hash *)
        ; all_deposits : F.t  (** Action state on the L1 *)
        ; withdrawals_processed : F.t
        ; new_withdrawals : List_TR_8.t
              (** Withdrawals to be processed this time *)
        ; old_inner_acc : Account.t
        ; old_inner_acc_path : Ledger_path.t
        ; new_inner_acc : Account.t
        ; new_inner_acc_path : Ledger_path.t
        }
      [@@deriving snarky]
    end

    include MkHandler (Witness)

    let check_incl_proof root path account =
      let account_hash = run @@ Mina_base.Account.Checked.digest account in
      let implied_root =
        List.foldi path ~init:account_hash ~f:(fun height acc h ->
            (* To constrain `index` being 0, we are counting on the path being fully on left *)
            (* Normally we would need also `bool list` specifying the path from leaf to the root *)
            (* The code would be adding on few constraints with the `if_` *)
            (*
               let l = Field.if_ b ~then_:h ~else_:acc
               and r = Field.if_ b ~then_:acc ~else_:h in
               Ledger_hash.merge_var ~height l r
            *)
            Ledger_hash.merge_var ~height acc h )
      in
      Field.Assert.equal (Ledger_hash.var_to_hash_packed root) implied_root

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
      let ({ stmt
           ; prf
           ; action_prf
           ; public_key
           ; vk_hash
           ; all_deposits
           ; withdrawals_processed
           ; new_withdrawals
           ; old_inner_acc
           ; old_inner_acc_path
           ; new_inner_acc
           ; new_inner_acc_path
           }
            : Witness.var ) =
        exists_witness ()
      in

      check_incl_proof stmt.source_ledger old_inner_acc_path old_inner_acc ;
      check_incl_proof stmt.target_ledger new_inner_acc_path new_inner_acc ;

      (* There must have been exactly one "step" in the inner account, checked by checking nonce *)
      let () =
        run
        @@ Nonce.Checked.Assert.equal
             (run @@ Nonce.Checked.succ old_inner_acc.nonce)
             new_inner_acc.nonce
      in
      (* FIXME: shouldn't be necessary but who knows whether an account index is reliable.
         We use `old_acc` instead of `new_acc`, because _possibly_ old_acc could be PC.empty while new_acc is not.
         Maybe this isn't the case though. *)
      let () =
        run
        @@ PC.Checked.Assert.equal old_inner_acc.public_key
             (constant PC.typ Inner_rules.public_key)
      in
      (* We get the old & new zkapp state, a bit complicated to get because field is actually hash of contents *)
      let old_zkapp_hash, old_zkapp = old_inner_acc.zkapp in
      let old_zkapp =
        exists Zkapp_account.typ ~compute:(fun () ->
            Option.value_exn @@ As_prover.Ref.get old_zkapp )
      in
      (* We check that witness old zkapp is correct *)
      Field.Assert.equal old_zkapp_hash
      @@ Zkapp_account.Checked.digest old_zkapp ;
      let zkapp_hash, zkapp = new_inner_acc.zkapp in
      let zkapp =
        exists Zkapp_account.typ ~compute:(fun () ->
            Option.value_exn @@ As_prover.Ref.get zkapp )
      in
      (* We check that witness new zkapp is correct *)
      Field.Assert.equal zkapp_hash @@ Zkapp_account.Checked.digest zkapp ;
      (* The inner account keeps track of _our_ action state, that's how it knows
         what transfer requests have been logged on the L1 to the L2.
         We check that it's been set to our current app state,
         and seemingly redundantly, that it's also an extension of the old one.
         It's in fact possible to go _backwards_ if we're not careful, since
         what we think is our action state is in fact just a recent one,
         so you could set the action state precondition to an old state and go backwards.
         I haven't considered whether it's problematic to go backwards, but we disallow it anyway. *)
      let (old_all_deposits :: _) = old_zkapp.app_state in
      let (all_deposits' :: _) = zkapp.app_state in
      (* Check that it matches all_deposits witness *)
      Field.Assert.equal all_deposits all_deposits ;
      (* Init account update *)
      let account_update = Body.(constant (typ ()) dummy) in
      (* Declare that we're authorized by a proof *)
      let authorization_kind : Authorization_kind.Checked.t =
        { is_signed = Boolean.false_
        ; is_proved = Boolean.true_
        ; verification_key_hash = vk_hash
        }
      in
      (* Withdrawals are registered in the inner account's action state *)
      let (all_withdrawals :: _) = zkapp.action_state in
      (* Init calls *)
      let calls = Zkapp_call_forest.Checked.empty () in
      (* NOTE: processed_transfers' must be a prefix of all_transfers, checked also using action_prf. *)
      (* For every transfer, we add a "call", i.e. an account update as child where we inc balance *)
      let withdrawals_processed', calls =
        List.fold new_withdrawals ~init:(withdrawals_processed, calls)
          ~f:(fun (withdrawals, calls) withdrawal ->
            Process_transfer.process_transfer withdrawals withdrawal calls )
      in
      (* Finalize update  *)
      let update =
        { account_update.update with
          app_state =
            State.(
              var_to_app_state typ
                ( { ledger_hash = stmt.target_ledger
                  ; all_withdrawals
                  ; withdrawals_processed = withdrawals_processed'
                  }
                  : var ))
        }
      in
      (* Init account state precondition *)
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
              (* Our previous ledger must be this *)
            ; ignore
              (* This is the old all_withdrawals, we don't need to check this *)
            ; Or_ignore.Checked.make_unsafe Boolean.true_ withdrawals_processed
              (* The list of withdrawals processed before the current update *)
            ; ignore
            ; ignore
            ; ignore
            ; ignore
            ; ignore
            ]
        ; action_state =
            Or_ignore.Checked.make_unsafe Boolean.true_ all_deposits
            (* Our action state must match *)
        }
      in
      let preconditions =
        { Preconditions.(constant (typ ()) accept) with
          account = account_precondition
        }
      in
      (* Our account update is assembled, specifying our state update, our preconditions, our pk, and our authorization *)
      let account_update =
        { account_update with
          public_key
        ; authorization_kind
        ; update
        ; preconditions
        }
      in
      (* Assemble some stuff to help the prover and calculate public output *)
      let public_output, auxiliary_output = make_outputs account_update calls in
      Pickles.Inductive_rule.
        { previous_proof_statements =
            [ { public_input = stmt
              ; proof_must_verify = Boolean.true_
              ; proof = prf
              }
              (* Proof for Wrapper_rules showing there is a valid transition from source to target *)
            ; { public_input =
                  ({ source0 = old_all_deposits
                   ; target0 = all_deposits
                   ; source1 = withdrawals_processed'
                   ; target1 = all_withdrawals
                   } : Action_state_extension_rule.Stmt.var)
              ; proof_must_verify = Boolean.true_
              ; proof = action_prf
              }
              (* Proof that _our_ outer action state, as recorded in the inner account, stepped forward, along
                 with the _inner_ action state, as recorded in the outer account, is an extension of our processed transfers *)
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

  module Step_without_transfers = struct
    (* Version for internal MVP release without transfers *)
    module Witness = struct
      type t =
        { stmt : S.t  (** The ledger transition we are performing. *)
        ; prf : RefProof.t  (** Proof of ledger transition being valid. *)
        ; public_key : PC.t  (** Our public key on the L2 *)
        ; vk_hash : F.t  (** Our vk hash *)
        }
      [@@deriving snarky]
    end

    include MkHandler (Witness)

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
      let ({ stmt; prf; public_key; vk_hash } : Witness.var) =
        exists_witness ()
      in

      (* Init account update *)
      let account_update = Body.(constant (typ ()) dummy) in
      (* Declare that we're authorized by a proof *)
      let authorization_kind : Authorization_kind.Checked.t =
        { is_signed = Boolean.false_
        ; is_proved = Boolean.true_
        ; verification_key_hash = vk_hash
        }
      in

      (* Finalize update  *)
      let update =
        { account_update.update with
          app_state =
            State.(
              var_to_app_state typ
                ( { ledger_hash = stmt.target_ledger
                  ; all_withdrawals = Field.constant Actions.empty_hash
                  ; withdrawals_processed = Field.constant Actions.empty_hash
                  }
                  : var ))
        }
      in
      (* Init account state precondition *)
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
              (* Our previous ledger must be this *)
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
        { Preconditions.(constant (typ ()) accept) with
          account = account_precondition
        }
      in
      (* Our account update is assembled, specifying our state update, our preconditions, our pk, and our authorization *)
      let account_update =
        { account_update with
          public_key
        ; authorization_kind
        ; update
        ; preconditions
        }
      in
      (* Assemble some stuff to help the prover and calculate public output *)
      let public_output, auxiliary_output =
        make_outputs account_update (Zkapp_call_forest.Checked.empty ())
      in
      Pickles.Inductive_rule.
        { previous_proof_statements =
            [ { public_input = stmt
              ; proof_must_verify = Boolean.true_
              ; proof = prf
              }
              (* Proof for Wrapper_rules showing there is a valid transition from source to target *)
            ]
        ; public_output
        ; auxiliary_output
        }

    let rule tag : _ Pickles.Inductive_rule.t =
      { identifier = "Rollup step without transfers"
      ; prevs = [ tag ]
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end
end

(* What we expose from this module *)
type t = Wrapper_rules.t

let source_ledger (t : t) = t.stmt.source_ledger

let target_ledger (t : t) = t.stmt.target_ledger

(** Compile the circuits *)
module Make (T : sig
  (** Tag for transaction snark rules *)
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

    let tag, cache_handle, p, Pickles.Provers.[ base_; step_both_; merge_ ] =
      time "Action_state_extension.compile" (fun () ->
          Pickles.compile ()
            ~override_wrap_domain:Pickles_base.Proofs_verified.N1
            ~cache:Cache_dir.cache ~public_input:(Output Stmt.typ)
            ~auxiliary_typ:Typ.unit
            ~branches:(module Nat.N3)
            ~max_proofs_verified:(module Nat.N2)
            ~name:"action state extension"
            ~constraint_constants:
              (Genesis_constants.Constraint_constants.to_snark_keys_header
                 constraint_constants )
            ~choices:(fun ~self ->
              [ Base.rule; StepBoth.rule self; Merge.rule self ] ) )

    let vk = Pickles.Side_loaded.Verification_key.of_compiled tag

    let base w = base_ ~handler:(Base.handler w) ()

    let step_both w = step_both_ ~handler:(StepBoth.handler w) ()

    let merge w = merge_ ~handler:(Merge.handler w) ()
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
      let%map stmt, _, proof = wrap_ ~handler:(Wrap.handler { txn_snark }) () in
      ({ stmt; proof } : t)

    let merge (s1 : t) (s2 : t) =
      let%map stmt, _, proof = merge_ ~handler:(Merge.handler { s1; s2 }) () in
      ({ stmt; proof } : t)

    module Proof = (val p)

    let verify (s : t) = Proof.verify [ (s.stmt, s.proof) ]
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

    let vk_hash = Zkapp_account.digest_vk vk

    let withdraw ~public_key ~amount ~recipient =
      let%map _, tree, proof =
        action_
          ~handler:
            (Transfer_action_rule.handler
               { vk_hash; public_key; amount; recipient } )
          ()
      in
      mkforest tree proof

    let extend_action_state (action_state : field) (trs : TR.t list) :
        (Action_state_extension_rule.Stmt.t * Proof.t) Deferred.t =
      let%bind stmt, (), proof =
        Action_state_extension.base
          ( { source0 = action_state; source1 = action_state }
            : Action_state_extension_rule.Base.Witness.t )
      in
      let%bind stmt, proof =
        List.fold trs
          ~init:(return (stmt, proof))
          ~f:(fun x tr ->
            if TR.is_dummy tr then return (stmt, proof)
            else
              let%bind stmt, proof = x in
              let action = TR.(value_to_actions typ tr) in
              let%bind stmt, (), proof =
                Action_state_extension.step_both
                  ( { actions0 = action
                    ; actions1 = action
                    ; step0 = true
                    ; step1 = true
                    ; prev = stmt
                    ; proof
                    }
                    : Action_state_extension_rule.StepBoth.Witness.t )
              in
              return (stmt, proof) )
      in
      return (stmt, proof)

    let step ~(deposits_processed : field) ~(remaining_deposits : TR.t list) :
        ( ( Account_update.t
          , Zkapp_command.Digest.Account_update.t
          , Zkapp_command.Digest.Forest.t )
          Zkapp_command.Call_forest.t
        * field
        * TR.t list )
        Deferred.t =
      let new_deposits =
        List.take remaining_deposits Inner_rules.Step.TR_8.length
      in
      let new_deposits =
        List.append new_deposits
          (List.init (8 - List.length new_deposits) ~f:(fun _ -> TR.dummy))
      in
      let remaining_deposits = List.drop remaining_deposits 8 in
      let remaining_deposits =
        List.drop remaining_deposits Inner_rules.Step.TR_8.length
      in
      let%bind { target0 = deposits_processed' }, _ =
        extend_action_state deposits_processed new_deposits
      in
      let%bind { target0 = all_deposits }, action_prf =
        extend_action_state deposits_processed' remaining_deposits
      in
      let%bind _, tree, proof =
        let w : Inner_rules.Step.Witness.t =
          { vk_hash
          ; deposits_processed
          ; deposits_processed'
          ; new_deposits
          ; all_deposits
          ; action_prf
          }
        in
        step_ ~handler:(Inner_rules.Step.handler w) ()
      in
      let tree = mkforest tree proof in
      return (tree, deposits_processed', remaining_deposits)

    let public_key = Inner_rules.public_key

    let account_id =
      Account_id.of_public_key @@ Public_key.decompress_exn public_key

    let initial_account =
      { Account.empty with
        public_key
      ; balance = Currency.Balance.max_int
      ; permissions = proof_permissions
      ; zkapp =
          Some
            { Zkapp_account.default with
              app_state =
                Inner_rules.State.(
                  value_to_init_state typ
                    ( { all_deposits = Actions.empty_hash
                      ; deposits_processed = Actions.empty_hash
                      }
                      : t ))
            ; verification_key =
                Some (Verification_key_wire.Stable.Latest.M.of_binable vk)
            }
      }
  end

  module Outer = struct
    let ( tag
        , cache_handle
        , p
        , Pickles.Provers.[ step_; action_; step_without_transfers_ ] ) =
      time "Zkapp.compile" (fun () ->
          Pickles.compile ()
            ~override_wrap_domain:Pickles_base.Proofs_verified.N1
            ~cache:Cache_dir.cache ~public_input:(Output Zkapp_statement.typ)
            ~auxiliary_typ:Typ.(Prover_value.typ ())
            ~branches:(module Nat.N3)
            ~max_proofs_verified:(module Nat.N2)
            ~name:"rollup"
            ~constraint_constants:
              (Genesis_constants.Constraint_constants.to_snark_keys_header
                 constraint_constants )
            ~choices:(fun ~self:_ ->
              [ Outer_rules.Step.rule Wrapper.tag Action_state_extension.tag
              ; Transfer_action_rule.rule
              ; Outer_rules.Step_without_transfers.rule Wrapper.tag
              ] ) )

    let vk = Pickles.Side_loaded.Verification_key.of_compiled tag

    let vk_hash = Zkapp_account.digest_vk vk

    let deposit ~public_key ~amount ~recipient =
      let%map _, tree, proof =
        action_
          ~handler:
            (Transfer_action_rule.handler
               { vk_hash; public_key; amount; recipient } )
          ()
      in
      mkforest tree proof

    let step (t : t) ~(public_key : PC.t) ~(old_action_state : field)
        ~(new_actions : TR.t list) ~(withdrawals_processed : field)
        ~(remaining_withdrawals : TR.t list) ~(source_ledger : SL.t)
        ~(target_ledger : SL.t) :
        ( ( Account_update.t
          , Zkapp_command.Digest.Account_update.t
          , Zkapp_command.Digest.Forest.t )
          Zkapp_command.Call_forest.t
        * field
        * TR.t list )
        Deferred.t =
      let%bind stmt_deposits, action_prf_deposits =
        Inner.extend_action_state old_action_state new_actions
      in
      let withdrawals_length = Outer_rules.Step.TR_8.length in
      let new_withdrawals =
        List.take remaining_withdrawals withdrawals_length
      in
      let new_withdrawals =
        List.append new_withdrawals
          (List.init
             (withdrawals_length - List.length new_withdrawals)
             ~f:(fun _ -> TR.dummy) )
      in
      let remaining_withdrawals =
        List.drop remaining_withdrawals withdrawals_length
      in
      let%bind { target0 = withdrawals_processed' }, _ =
        Inner.extend_action_state withdrawals_processed new_withdrawals
      in
      let%bind stmt_withdrawals, action_prf_withdrawals =
        Inner.extend_action_state withdrawals_processed' remaining_withdrawals
      in
      let%bind _, (), action_prf =
        Action_state_extension.merge
          { left = stmt_deposits
          ; right = stmt_withdrawals
          ; left_proof = action_prf_deposits
          ; right_proof = action_prf_withdrawals
          }
      in
      let%bind _, tree, proof =
        let w : Outer_rules.Step.Witness.t =
          { vk_hash
          ; all_deposits = stmt_deposits.target0
          ; withdrawals_processed
          ; new_withdrawals
          ; action_prf
          ; stmt = t.stmt
          ; prf = t.proof
          ; public_key
          ; old_inner_acc = SL.get_exn source_ledger 0
          ; old_inner_acc_path =
              List.map (SL.path_exn source_ledger 0) ~f:(function
                | `Left x ->
                    x
                | `Right _ ->
                    failwith "Impossible" )
          ; new_inner_acc = SL.get_exn target_ledger 0
          ; new_inner_acc_path =
              List.map (SL.path_exn target_ledger 0) ~f:(function
                | `Left x ->
                    x
                | `Right _ ->
                    failwith "Impossible" )
          }
        in
        step_ ~handler:(Outer_rules.Step.handler w) ()
      in
      let tree = mkforest tree proof in
      return (tree, withdrawals_processed', remaining_withdrawals)

    let step_without_transfers (t : t) ~(public_key : PC.t) :
        ( Account_update.t
        , Zkapp_command.Digest.Account_update.t
        , Zkapp_command.Digest.Forest.t )
        Zkapp_command.Call_forest.t
        Deferred.t =
      let%bind _, tree, proof =
        let w : Outer_rules.Step_without_transfers.Witness.t =
          { vk_hash; stmt = t.stmt; prf = t.proof; public_key }
        in
        step_without_transfers_
          ~handler:(Outer_rules.Step_without_transfers.handler w)
          ()
      in
      let tree = mkforest tree proof in
      return tree

    let unsafe_deploy_update (ledger_hash : Ledger_hash.t) =
      let update =
        { Update.dummy with
          app_state =
            Outer_rules.State.(
              value_to_app_state typ
                ( { ledger_hash
                  ; all_withdrawals = Actions.empty_hash
                  ; withdrawals_processed = Actions.empty_hash
                  }
                  : t ))
        ; verification_key =
            Set { data = vk; hash = Zkapp_account.digest_vk vk }
        ; permissions = Set proof_permissions
        }
      in
      update

    let deploy_update_exn (l : L.t) =
      if
        not
          (PC.equal Inner_rules.public_key (L.get_at_index_exn l 0).public_key)
      then failwith "zeko outer deploy: ledger invalid"
      else () ;
      unsafe_deploy_update (L.merkle_root l)

    let deploy_command_exn ~(signer : Signature_lib.Keypair.t)
        ~(fee : Currency.Fee.t) ~(nonce : Account.Nonce.t)
        ~(zkapp : Signature_lib.Keypair.t) ~(initial_ledger : L.t) :
        Zkapp_command.t =
      let zkapp_update =
        { body =
            { Body.dummy with
              public_key = Public_key.compress zkapp.public_key
            ; implicit_account_creation_fee = false
            ; update = deploy_update_exn initial_ledger
            ; use_full_commitment = true
            ; authorization_kind = Signature
            }
        ; authorization = Signature Signature.dummy
        }
      in
      let sender_update =
        { body =
            { Body.dummy with
              public_key = Public_key.compress signer.public_key
            ; balance_change =
                CAS.negate @@ CAS.of_unsigned
                @@ CA.of_fee constraint_constants.account_creation_fee
            ; use_full_commitment = true
            ; authorization_kind = Signature
            }
        ; authorization = Signature Signature.dummy
        }
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
        ; account_updates =
            Zkapp_command.Call_forest.accumulate_hashes'
            @@ Zkapp_command.Call_forest.of_account_updates
                 ~account_update_depth:(fun _ -> 0)
                 [ zkapp_update; sender_update ]
        ; memo = Signed_command_memo.empty
        }
      in
      let commitment = Zkapp_command.commitment command in
      let full_commitment =
        Zkapp_command.Transaction_commitment.create_complete
          (Zkapp_command.commitment command)
          ~memo_hash:(Signed_command_memo.hash command.memo)
          ~fee_payer_hash:
            (Zkapp_command.Digest.Account_update.create
               (Account_update.of_fee_payer command.fee_payer) )
      in
      let sender_signature =
        Signature_lib.Schnorr.Chunked.sign
          ~signature_kind:Mina_signature_kind.Testnet signer.private_key
          (Random_oracle.Input.Chunked.field full_commitment)
      in
      let zkapp_signature =
        Signature_lib.Schnorr.Chunked.sign
          ~signature_kind:Mina_signature_kind.Testnet zkapp.private_key
          (Random_oracle.Input.Chunked.field full_commitment)
      in
      { command with
        fee_payer = { command.fee_payer with authorization = sender_signature }
      ; account_updates =
          Zkapp_command.Call_forest.accumulate_hashes
            ~hash_account_update:(fun p ->
              Zkapp_command.Digest.Account_update.create p )
          @@ Zkapp_command.Call_forest.of_account_updates
               ~account_update_depth:(fun _ -> 0)
               [ { zkapp_update with
                   authorization = Control.Signature zkapp_signature
                 }
               ; { sender_update with
                   authorization = Control.Signature sender_signature
                 }
               ]
      }
  end
end
