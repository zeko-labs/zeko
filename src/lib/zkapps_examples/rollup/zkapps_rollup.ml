open Pickles_types
open Signature_lib
open Mina_base
open Snark_params
open Tick
open Run

module Rules = struct
  module Init = struct
    include struct
      open Snarky_backendless.Request

      type _ t +=
        | Public_key : Public_key.Compressed.t Snarky_backendless.Request.t
        | Token_id : Token_id.t Snarky_backendless.Request.t
        | May_use_token :
            Account_update.May_use_token.t Snarky_backendless.Request.t
    end

    module Witness = struct
      type t =
        { public_key : Public_key.Compressed.t
        ; token_id : Token_id.t
        ; may_use_token : Account_update.May_use_token.t
        }
    end

    let handler (w : Witness.t)
        (Snarky_backendless.Request.With { request; respond }) =
      match request with
      | Public_key ->
          respond (Provide w.public_key)
      | Token_id ->
          respond (Provide w.token_id)
      | May_use_token ->
          respond (Provide w.may_use_token)
      | _ ->
          respond Unhandled

    let main input =
      let public_key =
        exists Public_key.Compressed.typ ~request:(fun () -> Public_key)
      in
      let token_id = exists Token_id.typ ~request:(fun () -> Token_id) in
      Zkapps_examples.wrap_main ~public_key ~token_id
        (fun account_update -> account_update#assert_state_unproved)
        input

    let rule : _ Pickles.Inductive_rule.t =
      { identifier = "Rollup init"
      ; prevs = []
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end

  module Step = struct
    include struct
      open Snarky_backendless.Request

      type _ t +=
        | (* SoK not needed now, might be needed later *)
            Txn_snark :
            Transaction_snark.Statement.With_sok.t t
        | Txn_snark_proof : (Nat.N2.n, Nat.N2.n) Pickles.Proof.t t
        | Public_key : Public_key.Compressed.t Snarky_backendless.Request.t
        | Token_id : Token_id.t Snarky_backendless.Request.t
        | May_use_token :
            Account_update.May_use_token.t Snarky_backendless.Request.t
    end

    module Witness = struct
      type t =
        { txn : Transaction_snark.t
        ; public_key : Public_key.Compressed.t
        ; token_id : Token_id.t
        ; may_use_token : Account_update.May_use_token.t
        }
    end

    let handler (w : Witness.t)
        (Snarky_backendless.Request.With { request; respond }) =
      match request with
      | Txn_snark ->
          respond (Provide (Transaction_snark.statement_with_sok w.txn))
      | Txn_snark_proof ->
          respond (Provide (Transaction_snark.proof w.txn))
      | Public_key ->
          respond (Provide w.public_key)
      | Token_id ->
          respond (Provide w.token_id)
      | May_use_token ->
          respond (Provide w.may_use_token)
      | _ ->
          respond Unhandled

    (* TODO: Figure out whether we need to check connecting_ledger_hash == connecting_ledger_right *)
    let main { Pickles.Inductive_rule.public_input = vk_hash } =
      let txn_snark =
        exists Transaction_snark.Statement.With_sok.typ ~request:(fun () ->
            Txn_snark )
      in
      let txn_snark_proof =
        exists (Typ.Internal.ref ()) ~request:(fun () -> Txn_snark_proof)
      in
      let public_key =
        exists Public_key.Compressed.typ ~request:(fun () -> Public_key)
      in
      let token_id = exists Token_id.typ ~request:(fun () -> Token_id) in
      let prev_state = txn_snark.source.first_pass_ledger in
      let prev_state_raw = Frozen_ledger_hash0.var_to_field prev_state in
      let next_state = txn_snark.target.second_pass_ledger in
      let next_state_raw = Frozen_ledger_hash0.var_to_field next_state in
      let () =
        Field.Assert.equal
          (Frozen_ledger_hash0.var_to_field txn_snark.target.first_pass_ledger)
          (Frozen_ledger_hash0.var_to_field txn_snark.source.second_pass_ledger)
      in
      let account_update =
        new Zkapps_examples.account_update ~token_id ~public_key ~vk_hash ()
      in
      account_update#set_prev_state 0 prev_state_raw ;
      account_update#set_state 0 next_state_raw ;
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

    let rule txn_snark_tag : _ Pickles.Inductive_rule.t =
      { identifier = "Rollup step"
      ; prevs = [ txn_snark_tag ]
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end
end

module Make (T : sig
  val tag : Transaction_snark.tag
end) =
struct
  let tag, cache_handle, p, Pickles.Provers.[ init_; step_ ] =
    Zkapps_examples.compile () ~cache:Cache_dir.cache
      ~auxiliary_typ:Impl.Typ.unit
      ~branches:(module Nat.N2)
      ~max_proofs_verified:(module Nat.N1)
      ~name:"rollup"
      ~constraint_constants:
        Genesis_constants.Constraint_constants.(to_snark_keys_header compiled)
      ~choices:(fun ~self:_ -> [ Rules.Init.rule; Rules.Step.rule T.tag ])

  let vk = Pickles.Side_loaded.Verification_key.of_compiled tag

  let init w = init_ ~handler:(Rules.Init.handler w)

  let step w = step_ ~handler:(Rules.Step.handler w)

  module Proof = (val p)
end
