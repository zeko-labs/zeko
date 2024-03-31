(** Rules for wrapping txn snarks, necessary because we don't use passes
 TODO: Use Zkapp_command_logic here and bypass transaction snark and two pass system *)

open Core_kernel
open Mina_base
open Snark_params.Tick.Run
(* Impure interface to snarky, FIXME: replace with pure one *)

module Nat = Pickles_types.Nat
module Local_state = Mina_state.Local_state
module CAS = Currency.Amount.Signed
module CA = Currency.Amount
open Zeko_util
module With_sok = Transaction_snark.Statement.With_sok

(** Statement for this *)
module Stmt = struct
  type t = { source_ledger : Ledger_hash.t; target_ledger : Ledger_hash.t }
  [@@deriving snarky, yojson]

  let of_txn_snark_statement (txn_snark : With_sok.t) : t =
    { source_ledger = txn_snark.source.first_pass_ledger
    ; target_ledger = txn_snark.target.second_pass_ledger
    }
end

module T = struct
  (** Akin to Transaction_snark.t *)
  type t = { stmt : Stmt.t; proof : RefProof.t } [@@deriving snarky]

  let verify { stmt; proof } =
    Pickles.Inductive_rule.Previous_proof_statement.
      { public_input = stmt; proof_must_verify = Boolean.true_; proof }

  let statement_var { stmt } = stmt

  let statement ({ stmt } : t) = stmt
end

include T

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
    let txn_snark_stmt =
      Transaction_snark.(
        exists With_sok.typ ~compute:(fun () ->
            statement_with_sok @@ As_prover.Ref.get txn_snark ))
    in
    let stmt =
      Stmt.(
        exists typ ~compute:(fun () ->
            of_txn_snark_statement @@ As_prover.read With_sok.typ txn_snark_stmt ))
    in
    (* Check that txn_snark_stmt and stmt match *)
    with_label __LOC__ (fun () ->
        run
        @@ Ledger_hash.assert_equal stmt.source_ledger
             txn_snark_stmt.source.first_pass_ledger ) ;
    with_label __LOC__ (fun () ->
        run
        @@ Ledger_hash.assert_equal stmt.target_ledger
             txn_snark_stmt.target.second_pass_ledger ) ;

    (* Check that pending_coinbase_stack is correctly set *)
    let dummy_pc = constant Pending_coinbase.Stack.typ dummy_pc in
    with_label __LOC__ (fun () ->
        Boolean.Assert.is_true @@ run
        @@ Pending_coinbase.Stack.equal_var dummy_pc
             txn_snark_stmt.source.pending_coinbase_stack ) ;
    with_label __LOC__ (fun () ->
        Boolean.Assert.is_true @@ run
        @@ Pending_coinbase.Stack.equal_var dummy_pc
             txn_snark_stmt.target.pending_coinbase_stack ) ;

    (* Check that transactions have been completely applied *)
    let empty_state = Local_state.(constant typ @@ empty ()) in
    with_label __LOC__ (fun () ->
        Local_state.Checked.assert_equal empty_state
          txn_snark_stmt.source.local_state ) ;
    with_label __LOC__ (fun () ->
        Local_state.Checked.assert_equal empty_state
          txn_snark_stmt.target.local_state ) ;

    (* Check that first and second passes are connected *)
    with_label __LOC__ (fun () ->
        run
        @@ Ledger_hash.assert_equal txn_snark_stmt.target.first_pass_ledger
             txn_snark_stmt.source.second_pass_ledger ) ;

    (* Check that it's a complete transaction (a "block") *)
    with_label __LOC__ (fun () ->
        run
        @@ Ledger_hash.assert_equal txn_snark_stmt.target.first_pass_ledger
             txn_snark_stmt.connecting_ledger_right ) ;
    with_label __LOC__ (fun () ->
        run
        @@ Ledger_hash.assert_equal txn_snark_stmt.source.second_pass_ledger
             txn_snark_stmt.connecting_ledger_left ) ;

    (* We don't check fee_excess because it's up to the sequencer what they do with it. *)
    (* The supply however must not increase. *)
    let is_neg =
      Sgn.Checked.is_neg @@ run
      @@ CAS.Checked.sgn txn_snark_stmt.supply_increase
    in
    let is_zero =
      run
      @@ CA.Checked.equal CA.(constant typ zero)
      @@ run
      @@ CAS.Checked.magnitude txn_snark_stmt.supply_increase
    in
    with_label __LOC__ (fun () ->
        Boolean.Assert.is_true Boolean.(is_neg || is_zero) ) ;

    Pickles.Inductive_rule.
      { previous_proof_statements =
          (* Proof for txn_snark_stmt using normal txn snark *)
          [ { public_input = txn_snark_stmt
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
    type t = { s1 : T.t; s2 : T.t } [@@deriving snarky]
  end

  include MkHandler (Witness)

  let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
    let Witness.{ s1; s2 } = exists_witness () in
    let s =
      Stmt.
        { source_ledger = s1.stmt.source_ledger
        ; target_ledger = s2.stmt.target_ledger
        }
    in
    run @@ Ledger_hash.assert_equal s1.stmt.target_ledger s2.stmt.source_ledger ;
    Pickles.Inductive_rule.
      { previous_proof_statements = [ verify s1; verify s2 ]
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

let dummy_pc_init = Wrap.dummy_pc_init

let dummy_pc = Wrap.dummy_pc

let dummy_state_body = Wrap.dummy_state_body

module Make (Txn : Transaction_snark.S) = struct
  include T
  open Async_kernel

  let tag, _cache_handle, _p, Pickles.Provers.[ wrap_; merge_ ] =
    time "Wrapper.compile" (fun () ->
        Pickles.compile () ~override_wrap_domain:Pickles_base.Proofs_verified.N1
          ~cache:Cache_dir.cache ~public_input:(Output Stmt.typ)
          ~auxiliary_typ:Typ.unit
          ~branches:(module Nat.N2)
          ~max_proofs_verified:(module Nat.N2)
          ~name:"zeko wrapper"
          ~constraint_constants:
            (Genesis_constants.Constraint_constants.to_snark_keys_header
               constraint_constants )
          ~choices:(fun ~self -> [ Wrap.rule Txn.tag; Merge.rule self ]) )

  let wrap txn_snark =
    time_async "Wrapper.wrap" (fun () ->
        let%map stmt, _, proof =
          wrap_ ~handler:(Wrap.handler { txn_snark }) ()
        in
        ({ stmt; proof } : t) )

  let merge (s1 : t) (s2 : t) =
    time_async "Wrapper.merge" (fun () ->
        let%map stmt, _, proof =
          merge_ ~handler:(Merge.handler { s1; s2 }) ()
        in
        ({ stmt; proof } : t) )

  let verify _ = failwith "deprecated"
end
