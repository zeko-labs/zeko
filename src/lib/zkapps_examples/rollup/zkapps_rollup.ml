open Core_kernel
open Pickles_types
open Signature_lib
open Mina_base
open Mina_state
open Snark_params
open Tick
open Run

let initial_state = lazy (List.init 8 ~f:(fun _ -> Field.Constant.zero))

let initialize public_key =
  Zkapps_examples.wrap_main
    ~public_key:(Public_key.Compressed.var_of_t public_key)
    (fun account_update ->
      let initial_state =
        List.map ~f:Field.constant (Lazy.force initial_state)
      in
      account_update#assert_state_unproved ;
      account_update#set_full_state initial_state )

include struct
  open Snarky_backendless.Request

  type _ t +=
    | Txn_snark : Transaction_snark.Statement.With_sok.t t (* SoK not needed now, might be needed later *)
    | Txn_snark_proof : (Nat.N2.n, Nat.N2.n) Pickles.Proof.t t
end

module Witness = struct
  type t =
    { txn_snark : Transaction_snark.Statement.With_sok.t
    ; txn_snark_proof : (Nat.N2.n, Nat.N2.n) Pickles.Proof.t
    }
end

let update_state_handler (w : Witness.t)
    (Snarky_backendless.Request.With { request; respond }) =
  match request with
  | Txn_snark ->
      respond (Provide w.txn_snark)
  | Txn_snark_proof ->
      respond (Provide w.txn_snark_proof)
  | _ ->
      respond Unhandled

let update_state public_key input =
  let txn_snark =
    exists Transaction_snark.Statement.With_sok.typ ~request:(fun () -> Txn_snark)
  in
  let txn_snark_proof =
    exists (Typ.Internal.ref ()) ~request:(fun () -> Txn_snark_proof)
  in
  let prev_state = txn_snark.source.first_pass_ledger in
	let prev_state_raw = Frozen_ledger_hash0.var_to_field prev_state in
  let next_state = txn_snark.target.first_pass_ledger in
	let next_state_raw = Frozen_ledger_hash0.var_to_field next_state in
  let { previous_proof_statements = _ ; public_output = account_update ; auxiliary_output = () } =
    Zkapps_examples.wrap_main
      ~public_key:(Public_key.Compressed.var_of_t public_key)
      (fun account_update ->
        account_update#assert_state_proved ;
        account_update#set_prev_state 0 prev_state_raw ;
        account_update#set_state 0 next_state_raw ;
        ()
      )
      input
  in
  let open Pickles.Inductive_rule in
  { previous_proof_statements =
    [ { public_input = txn_snark
      ; proof_must_verify = Boolean.true_
      ; proof = txn_snark_proof
      }
    ]
  ; public_output = account_update
  ; auxiliary_output = ()
  }

let initialize_rule public_key : _ Pickles.Inductive_rule.t =
  { identifier = "Initialise rollup"
  ; prevs = []
  ; main = initialize public_key
  ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
  }

let update_state_rule txn_snark_tag public_key : _ Pickles.Inductive_rule.t =
  { identifier = "Update state"
  ; prevs = [ txn_snark_tag ]
  ; main = update_state public_key
  ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
  }
