open Snark_params.Tick
open Checked.Let_syntax

module Make (Inputs : sig
  val max_valid_while_size : int

  val inner_public_key : Signature_lib.Public_key.Compressed.t

  val chain_l1 : Mina_signature_kind.t

  val chain_l2 : Mina_signature_kind.t

  val multisig_key : Multisig.t

  val max_sequencer_inactivity : int

  val emergency_da_public_key : Signature_lib.Public_key.Compressed.t

  val ethereum_asset_registry_public_key :
    Signature_lib.Public_key.Compressed.t option

  val ethereum_asset_registry_schema_version : Zeko_util.Checked32.t

  val ethereum_asset_approved_mft_standard_vk_id : Snark_params.Tick.Field.t

  val ethereum_asset_approved_mft_token_vk_hash : Snark_params.Tick.Field.t

  val ethereum_asset_approved_mft_admin_vk_hash : Snark_params.Tick.Field.t

  val ethereum_asset_universal_bridge_vk_id : Snark_params.Tick.Field.t

  val ethereum_asset_universal_bridge_vk_hash : Snark_params.Tick.Field.t

  val ethereum_asset_vault_public_key : Signature_lib.Public_key.Compressed.t
end)
() =
struct
  module Rule_commit_inst = Rule_commit.Make (Inputs)
  module Rule_action_witness_inst = Rule_action_witness.Make (Inputs)
  module Rule_pause_inst = Rule_pause.Make (Inputs)

  module Rule_multisig_update_inst = Rule_multisig_update.Make (struct
    let chain = Inputs.chain_l1

    let multisig_key = Inputs.multisig_key
  end)

  (* Registry account validation makes the rollup-step branch larger than the
     one-chunk Pickles limit. Compile that rule independently with two chunks,
     then expose one shallow forwarding proof system for the common outer VK.
     This keeps the final system's branch verification keys the same shape. *)
  let num_chunks =
    if Option.is_some Inputs.ethereum_asset_registry_public_key then Some 2
    else None

  let out_typ = Typ.(Mina_base.Zkapp_statement.typ * V.typ)

  module Commit_rule =
  ( val Compile_simple.compile () ?num_chunks ~out_typ
          ~branches:[ Rule_commit_inst.rule ] ~name:"Outer_rules.commit" )

  module Emergency_commit_rule =
  ( val Compile_simple.compile () ~out_typ
          ~branches:[ Rule_commit_inst.Emergency_commit.rule ]
          ~name:"Outer_rules.emergency_commit" )

  module Action_witness_rule =
  ( val Compile_simple.compile () ~out_typ
          ~branches:[ Rule_action_witness_inst.rule ]
          ~name:"Outer_rules.action_witness" )

  module Pause_rule =
  ( val Compile_simple.compile () ~out_typ ~branches:[ Rule_pause_inst.rule ]
          ~name:"Outer_rules.pause" )

  module Multisig_update_rule =
  ( val Compile_simple.compile () ~out_typ
          ~branches:[ Rule_multisig_update_inst.rule ]
          ~name:"Outer_rules.multisig_update" )

  module Forward_commit = struct
    let main (w : Commit_rule.t V.t) =
      let%bind proof = exists ~compute:(V.get w) Commit_rule.typ in
      let%map out, verify = Commit_rule.get proof in
      Compile_simple.{ prevs = One_prev verify; out }

    let rule : _ Compile_simple.branch lazy_t =
      lazy
        { branch_name = "forward commit"
        ; tags = One_tag (Lazy.force Commit_rule.tag)
        ; main
        }
  end

  module Forward_emergency_commit = struct
    let main (w : Emergency_commit_rule.t V.t) =
      let%bind proof = exists ~compute:(V.get w) Emergency_commit_rule.typ in
      let%map out, verify = Emergency_commit_rule.get proof in
      Compile_simple.{ prevs = One_prev verify; out }

    let rule : _ Compile_simple.branch lazy_t =
      lazy
        { branch_name = "forward emergency commit"
        ; tags = One_tag (Lazy.force Emergency_commit_rule.tag)
        ; main
        }
  end

  module Forward_action_witness = struct
    let main (w : Action_witness_rule.t V.t) =
      let%bind proof = exists ~compute:(V.get w) Action_witness_rule.typ in
      let%map out, verify = Action_witness_rule.get proof in
      Compile_simple.{ prevs = One_prev verify; out }

    let rule : _ Compile_simple.branch lazy_t =
      lazy
        { branch_name = "forward action witness"
        ; tags = One_tag (Lazy.force Action_witness_rule.tag)
        ; main
        }
  end

  module Forward_pause = struct
    let main (w : Pause_rule.t V.t) =
      let%bind proof = exists ~compute:(V.get w) Pause_rule.typ in
      let%map out, verify = Pause_rule.get proof in
      Compile_simple.{ prevs = One_prev verify; out }

    let rule : _ Compile_simple.branch lazy_t =
      lazy
        { branch_name = "forward pause"
        ; tags = One_tag (Lazy.force Pause_rule.tag)
        ; main
        }
  end

  module Forward_multisig_update = struct
    let main (w : Multisig_update_rule.t V.t) =
      let%bind proof = exists ~compute:(V.get w) Multisig_update_rule.typ in
      let%map out, verify = Multisig_update_rule.get proof in
      Compile_simple.{ prevs = One_prev verify; out }

    let rule : _ Compile_simple.branch lazy_t =
      lazy
        { branch_name = "forward multisig update"
        ; tags = One_tag (Lazy.force Multisig_update_rule.tag)
        ; main
        }
  end

  include
    ( val Compile_simple.compile () ~out_typ
            ~branches:
              [ Forward_commit.rule
              ; Forward_emergency_commit.rule
              ; Forward_action_witness.rule
              ; Forward_pause.rule
              ; Forward_multisig_update.rule
              ]
            ~name:"Outer_rules" )

  let forwarding_provers = provers

  let provers =
    lazy
      (let Compile_simple.
             [ forward_commit
             ; forward_emergency_commit
             ; forward_action_witness
             ; forward_pause
             ; forward_multisig_update
             ] =
         Lazy.force forwarding_provers
       in
       let Compile_simple.[ commit ] = Lazy.force Commit_rule.provers in
       let Compile_simple.[ emergency_commit ] =
         Lazy.force Emergency_commit_rule.provers
       in
       let Compile_simple.[ action_witness ] =
         Lazy.force Action_witness_rule.provers
       in
       let Compile_simple.[ pause ] = Lazy.force Pause_rule.provers in
       let Compile_simple.[ multisig_update ] =
         Lazy.force Multisig_update_rule.provers
       in
       Compile_simple.
         [ (fun input ->
             let%bind.Promise out, proof = commit input in
             forward_commit (Commit_rule.make_unchecked ~proof out) )
         ; (fun input ->
             let%bind.Promise out, proof = emergency_commit input in
             forward_emergency_commit
               (Emergency_commit_rule.make_unchecked ~proof out) )
         ; (fun input ->
             let%bind.Promise out, proof = action_witness input in
             forward_action_witness
               (Action_witness_rule.make_unchecked ~proof out) )
         ; (fun input ->
             let%bind.Promise out, proof = pause input in
             forward_pause (Pause_rule.make_unchecked ~proof out) )
         ; (fun input ->
             let%bind.Promise out, proof = multisig_update input in
             forward_multisig_update
               (Multisig_update_rule.make_unchecked ~proof out) )
         ] )
end
