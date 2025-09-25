open Core_kernel
open Async
open Mina_base
open Zeko_circuits
open Zeko_types

let constraint_constants = Genesis_constants.Compiled.constraint_constants

let time ?fake_proving_time ~logger label (d : 'a Deferred.t) =
  [%log info] "Starting %s%!" label ;
  let start = Time.now () in
  let%bind () =
    match fake_proving_time with
    | None ->
        Deferred.unit
    | Some fake_proving_time ->
        after fake_proving_time
  in
  let%bind x = d in
  let stop = Time.now () in
  [%log info] "%s: %s\n%!" label
    (Time.Span.to_string_hum @@ Time.diff stop start) ;
  return x

module Make_folder (System : sig
  module Stmt : sig
    type t [@@deriving yojson]
  end

  module Elem : sig
    type t
  end

  type trans = { source : Stmt.t; target : Stmt.t }

  val leaf : Elem.t list * Stmt.t -> (trans * Compile_simple.Proof.t) Promise.t

  val leaf_option :
    Elem.t list * Stmt.t -> (trans * Compile_simple.Proof.t) Promise.t

  val extend :
       Elem.t list * (trans * Compile_simple.Proof.t)
    -> (trans * Compile_simple.Proof.t) Promise.t

  val extend_option :
       Elem.t list * (trans * Compile_simple.Proof.t)
    -> (trans * Compile_simple.Proof.t) Promise.t

  (* type merge_input =
       { left : trans
       ; left_proof : Compile_simple.Proof.t
       ; right : trans
       ; right_proof : Compile_simple.Proof.t
       }

     val merge : merge_input -> (trans * Compile_simple.Proof.t) Promise.t *)

  val leaf_iterations : int

  val leaf_option_iterations : int

  val extend_iterations : int

  val extend_option_iterations : int
end) =
struct
  type out_t = Compile_simple.Proof.t option * System.Stmt.t [@@deriving yojson]

  let fold
      ~(source :
         [ `Extend of System.trans * Compile_simple.Proof.t
         | `Full of System.Stmt.t ] ) ~elems : out_t Promise.t =
    match elems with
    | [] -> (
        (* No need for folding, everything goes to excess *)
        match source with
        | `Full source ->
            Promise.return (None, source)
        | `Extend (trans, proof) ->
            Promise.return (Some proof, trans.target) )
    | elems_to_prove ->
        (* Need to fold *)
        let%bind.Promise leaf, rest =
          match source with
          | `Full source ->
              let leaf_prover, (leaf_elems, rest) =
                if List.length elems_to_prove > System.leaf_iterations then
                  (* Full leaf *)
                  ( System.leaf
                  , List.split_n elems_to_prove System.leaf_iterations )
                else
                  (* Partial leaf *)
                  ( System.leaf_option
                  , List.split_n elems_to_prove System.leaf_option_iterations )
              in
              let%map.Promise leaf = leaf_prover (leaf_elems, source) in
              (leaf, rest)
          | `Extend (trans, proof) ->
              Promise.return ((trans, proof), elems_to_prove)
        in
        let rec extend_rest elems_to_prove acc =
          match elems_to_prove with
          | [] ->
              Promise.return acc
          | elems_to_prove ->
              let extend_prover, (extend_elems, rest) =
                if List.length elems_to_prove > System.extend_iterations then
                  (* Full node *)
                  ( System.extend
                  , List.split_n elems_to_prove System.extend_iterations )
                else
                  (* Partial node *)
                  ( System.extend_option
                  , List.split_n elems_to_prove System.extend_option_iterations
                  )
              in
              let%bind.Promise acc = extend_prover (extend_elems, acc) in
              extend_rest rest acc
        in
        let%bind.Promise trans, proof = extend_rest rest leaf in
        Promise.return (Some proof, trans.target)
end

module Folder_with_length = Make_folder (Ase.With_length)
module Folder_without_length = Make_folder (Ase.Without_length)
module Folder_check_accepted_mina = Make_folder (Bridge.Check_accepted_mina)

(* Unfortunately yojson doesn't support GADTs so it can't be one type, or maybe I'm just bad *)
module Input = struct
  module Txn_snark = struct
    type t =
      | Signed_command of Base_input.serializable
      | Zkapp_command of Txn_snark_witness.Zkapp_command_segment.t
      | Merge of Merge_input.t
    [@@deriving yojson]
  end

  module Folder = struct
    type t =
      | Ase_with_length of
          ( [ `Full of Ase.With_length.Stmt.t
            | `Extend of Ase.With_length.trans * Compile_simple.Proof.t ]
          * F.t list )
      | Ase_without_length of
          ( [ `Full of Ase.Without_length.Stmt.t
            | `Extend of Ase.Without_length.trans * Compile_simple.Proof.t ]
          * F.t list )
      | Check_accepted_mina of
          ( Bridge.Check_accepted_mina.Stmt.t
          * Bridge.Check_accepted_mina.Elem.t list )
    [@@deriving yojson]
  end

  module Bridge = struct
    type t =
      | Outer_action_witness of Bridge.Outer_action_witness.serializable
      | Inner_action_witness of Bridge.Inner_action_witness.serializable
      | Finalize_deposit of Bridge.Finalize_deposit.serializable
      | Finalize_cancelled_deposit of
          Bridge.Finalize_cancelled_deposit.serializable
      | Inner_receive of Bridge.Inner_receive.serializable
      | Finalize_withdrawal of Bridge.Finalize_withdrawal.serializable
      | Outer_token_owner of Bridge.Outer_token_owner.serializable
    [@@deriving yojson]
  end

  type t =
    | Ping
    | Txn_snark of Txn_snark.t
    | Folder of Folder.t
    | Inner_sync of Inner_sync.Witness.serializable
    | Verify_both_ases_commit of
        ( Outer_commit.Ase_outer_inst.serializable
        * Outer_commit.Ase_inner_inst.serializable )
    | Verify_two_outer_ases_cancelled_deposit of
        ( Zeko_types.Bridge.Finalize_cancelled_deposit.Ase_outer_inst
          .serializable
        * Zeko_types.Bridge.Finalize_cancelled_deposit
          .Ase_outer_with_length_inst
          .serializable )
    | Verify_check_accepted_and_ase_cancelled_deposit of
        ( Zeko_types.Bridge.Check_accepted_mina.serializable
        * Zeko_types.Bridge.Finalize_cancelled_deposit
          .Ase_outer_with_length_inst
          .serializable )
    | Outer_commit of Outer_commit.Witness.serializable
    | Bridge of Bridge.t
  [@@deriving yojson]
end

module Output = struct
  module Folder = struct
    type t =
      | Ase_with_length of Folder_with_length.out_t
      | Ase_without_length of Folder_without_length.out_t
      | Check_accepted_mina of Folder_check_accepted_mina.out_t
    [@@deriving yojson]
  end

  type t =
    | Error of string
    | Pong
    | Txn_snark of (Zeko_stmt.t * Compile_simple.Proof.t)
    | Folder of Folder.t
    | Verify_both_ases_commit of Outer_commit.Verify_both_ases.serializable
    | Verify_two_outer_ases_cancelled_deposit of
        Zeko_types.Bridge.Finalize_cancelled_deposit.Verify_two_outer_ases
        .serializable
    | Verify_check_accepted_and_ase_cancelled_deposit of
        Zeko_types.Bridge.Finalize_cancelled_deposit
        .Verify_check_accepted_and_ase
        .serializable
    | Call_forest of
        ( Account_update.Body.t
        * Zkapp_command.Digest.Account_update.t
        * ( Account_update.Stable.V1.t
          , Zkapp_command.Digest.Account_update.t
          , Zkapp_command.Digest.Forest.t )
          Zkapp_command.Call_forest.t )
        * Compile_simple.Proof.t
  [@@deriving yojson]
end

let prove ?fake_proving_time ~logger ~proof_cache_db :
    Input.t -> Output.t Deferred.t = function
  | Ping ->
      return Output.Pong
  | Txn_snark (Signed_command input) ->
      let Compile_simple.[ prove; _; _; _; _ ] = Txn_rules.provers in
      let%map stmt, proof =
        time ?fake_proving_time ~logger "Txn_rules.single_signed_command"
          (prove (Base_input.of_serializable input) |> Promise.to_deferred)
      in
      Output.Txn_snark (stmt, proof)
  | Txn_snark (Zkapp_command (Single_unproved input)) ->
      let Compile_simple.[ _; prove; _; _; _ ] = Txn_rules.provers in
      let%map stmt, proof =
        time ?fake_proving_time ~logger
          "Txn_rules.single_unproved_zkapp_command"
          ( prove
              (Zkapp_single_unproved_input.of_serializable ~proof_cache_db input)
          |> Promise.to_deferred )
      in
      Output.Txn_snark (stmt, proof)
  | Txn_snark (Zkapp_command (Double_unproved input)) ->
      let Compile_simple.[ _; _; prove; _; _ ] = Txn_rules.provers in
      let%map stmt, proof =
        time ?fake_proving_time ~logger
          "Txn_rules.double_unproved_zkapp_command"
          ( prove
              (Zkapp_double_unproved_input.of_serializable ~proof_cache_db input)
          |> Promise.to_deferred )
      in
      Output.Txn_snark (stmt, proof)
  | Txn_snark (Zkapp_command (Single_proved input)) ->
      let Compile_simple.[ _; _; _; prove; _ ] = Txn_rules.provers in
      let%map stmt, proof =
        time ?fake_proving_time ~logger "Txn_rules.single_proved_zkapp_command"
          ( prove
              (Zkapp_single_proved_input.of_serializable ~proof_cache_db input)
          |> Promise.to_deferred )
      in
      Output.Txn_snark (stmt, proof)
  | Txn_snark (Merge input) ->
      let Compile_simple.[ _; _; _; _; prove ] = Txn_rules.provers in
      let%map stmt, proof =
        time ?fake_proving_time ~logger "Txn_rules.merge"
          (prove input |> Promise.to_deferred)
      in
      Output.Txn_snark (stmt, proof)
  | Inner_sync input ->
      let Compile_simple.[ prove; _ ] = Inner_rules_inst.provers in
      let%bind vk_hash =
        Compile_simple.Verification_key.of_tag Inner_rules_inst.tag
        |> Promise.to_deferred
        (* To make fake tests work *)
        >>| Compile_simple.Verification_key.hash
      in
      let%map (_stmt, parent_with_calls), proof =
        time ?fake_proving_time ~logger "Inner_rules.inner_sync"
          ( prove (Inner_sync.Witness.of_serializable ~vk_hash input)
          |> Promise.to_deferred )
      in
      Output.Call_forest
        ( Tuple3.map_trd parent_with_calls
            ~f:
              (Zkapp_command.Call_forest.map
                 ~f:Account_update.read_all_proofs_from_disk )
        , proof )
  | Folder (Ase_with_length (source, elems)) ->
      let%map snark =
        time ?fake_proving_time ~logger "Folder.Ase_with_length.fold"
          (Folder_with_length.fold ~source ~elems |> Promise.to_deferred)
      in
      Output.(Folder (Ase_with_length snark))
  | Folder (Ase_without_length (source, elems)) ->
      let%map snark =
        time ?fake_proving_time ~logger "Folder.Ase_without_length.fold"
          (Folder_without_length.fold ~source ~elems |> Promise.to_deferred)
      in
      Output.(Folder (Ase_without_length snark))
  | Folder (Check_accepted_mina (source, elems)) ->
      let%map snark =
        time ?fake_proving_time ~logger "Folder.Check_accepted_mina.fold"
          ( Folder_check_accepted_mina.fold ~source:(`Full source) ~elems
          |> Promise.to_deferred )
      in
      Output.(Folder (Check_accepted_mina snark))
  | Verify_both_ases_commit (outer, inner) ->
      let Compile_simple.[ prove ] = Rule_commit.Verify_both_ases.provers in
      let%map stmt, proof =
        time ?fake_proving_time ~logger "Rule_commit.verify_both_ases"
          ( prove
              Outer_commit.
                ( Ase_outer_inst.of_serializable outer
                , Ase_inner_inst.of_serializable inner )
          |> Promise.to_deferred )
      in
      Output.Verify_both_ases_commit (stmt, proof)
  | Verify_two_outer_ases_cancelled_deposit (outer, outer_with_length) ->
      let Compile_simple.[ prove ] =
        Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
        .Verify_two_outer_ases
        .provers
      in
      let%map stmt, proof =
        time ?fake_proving_time ~logger
          "Finalize_cancelled_deposit.Verify_two_outer_ases"
          ( prove
              Bridge.Finalize_cancelled_deposit.
                ( Ase_outer_inst.of_serializable outer
                , Ase_outer_with_length_inst.of_serializable outer_with_length
                )
          |> Promise.to_deferred )
      in
      Output.Verify_two_outer_ases_cancelled_deposit (stmt, proof)
  | Verify_check_accepted_and_ase_cancelled_deposit
      (check_accepted, outer_with_length) ->
      let Compile_simple.[ prove ] =
        Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
        .Verify_check_accepted_and_ase
        .provers
      in
      let%map stmt, proof =
        time ?fake_proving_time ~logger
          "Finalize_cancelled_deposit.Verify_check_accepted_and_ase"
          ( prove
              Bridge.Finalize_cancelled_deposit.
                ( Zeko_types.Bridge.Check_accepted_mina
                  .of_serializable_cancelled_deposit check_accepted
                , Ase_outer_with_length_inst.of_serializable outer_with_length
                )
          |> Promise.to_deferred )
      in
      Output.Verify_check_accepted_and_ase_cancelled_deposit (stmt, proof)
  | Outer_commit input ->
      let Compile_simple.[ prove; _; _ ] = Outer_rules_inst.provers in
      let%bind vk_hash =
        Compile_simple.Verification_key.of_tag Outer_rules_inst.tag
        |> Promise.to_deferred
        (* To make fake tests work *)
        >>| Compile_simple.Verification_key.hash
      in
      let%map (_stmt, parent_with_calls), proof =
        time ?fake_proving_time ~logger "Outer_rules.commit"
          ( prove (Outer_commit.Witness.of_serializable ~vk_hash input)
          |> Promise.to_deferred )
      in
      Output.Call_forest
        ( Tuple3.map_trd parent_with_calls
            ~f:
              (Zkapp_command.Call_forest.map
                 ~f:Account_update.read_all_proofs_from_disk )
        , proof )
  | Bridge (Outer_action_witness input) ->
      let Compile_simple.[ _; prove; _ ] = Outer_rules_inst.provers in
      let%bind vk_hash =
        Compile_simple.Verification_key.of_tag Outer_rules_inst.tag
        |> Promise.to_deferred
        (* To make fake tests work *)
        >>| Compile_simple.Verification_key.hash
      in
      let%map (_stmt, parent_with_calls), proof =
        time ?fake_proving_time ~logger "Outer_rules.action_witness"
          ( prove
              (Bridge.Outer_action_witness.of_serializable ~proof_cache_db
                 ~vk_hash input )
          |> Promise.to_deferred )
      in
      Output.Call_forest
        ( Tuple3.map_trd parent_with_calls
            ~f:
              (Zkapp_command.Call_forest.map
                 ~f:Account_update.read_all_proofs_from_disk )
        , proof )
  | Bridge (Inner_action_witness input) ->
      let Compile_simple.[ _; prove ] = Inner_rules_inst.provers in
      let%bind vk_hash =
        Compile_simple.Verification_key.of_tag Inner_rules_inst.tag
        |> Promise.to_deferred
        (* To make fake tests work *)
        >>| Compile_simple.Verification_key.hash
      in
      let%map (_stmt, parent_with_calls), proof =
        time ?fake_proving_time ~logger "Inner_rules.action_witness"
          ( prove
              (Bridge.Inner_action_witness.of_serializable ~proof_cache_db
                 ~vk_hash input )
          |> Promise.to_deferred )
      in
      Output.Call_forest
        ( Tuple3.map_trd parent_with_calls
            ~f:
              (Zkapp_command.Call_forest.map
                 ~f:Account_update.read_all_proofs_from_disk )
        , proof )
  | Bridge (Finalize_deposit input) ->
      let Compile_simple.[ prove; _ ] = Bridge_inst_mina.System_L2.provers in
      let%bind vk_hash =
        Compile_simple.Verification_key.of_tag Bridge_inst_mina.System_L2.tag
        |> Promise.to_deferred
        (* To make fake tests work *)
        >>| Compile_simple.Verification_key.hash
      in
      let%map (_stmt, parent_with_calls), proof =
        time ?fake_proving_time ~logger "Bridge_mina.System_L2.finalize_deposit"
          ( prove (Bridge.Finalize_deposit.of_serializable ~vk_hash input)
          |> Promise.to_deferred )
      in
      Output.Call_forest
        ( Tuple3.map_trd parent_with_calls
            ~f:
              (Zkapp_command.Call_forest.map
                 ~f:Account_update.read_all_proofs_from_disk )
        , proof )
  | Bridge (Finalize_cancelled_deposit input) ->
      let Compile_simple.[ prove; _; _ ] =
        Bridge_inst_mina.System_L1_enabled.provers
      in
      let%bind vk_hash =
        Compile_simple.Verification_key.of_tag
          Bridge_inst_mina.System_L1_enabled.tag
        |> Promise.to_deferred
        (* To make fake tests work *)
        >>| Compile_simple.Verification_key.hash
      in
      let%bind helper_token_owner_l1_vk_hash =
        Compile_simple.Verification_key.of_tag
          Bridge_inst_mina.System_L1_token_owner.tag
        |> Promise.to_deferred
        (* To make fake tests work *)
        >>| Compile_simple.Verification_key.hash
      in
      let%map (_stmt, parent_with_calls), proof =
        time ?fake_proving_time ~logger
          "Bridge_mina.System_L1.finalize_cancelled_deposit"
          ( prove
              (Bridge.Finalize_cancelled_deposit.of_serializable ~vk_hash
                 ~helper_token_owner_l1_vk_hash input )
          |> Promise.to_deferred )
      in
      Output.Call_forest
        ( Tuple3.map_trd parent_with_calls
            ~f:
              (Zkapp_command.Call_forest.map
                 ~f:Account_update.read_all_proofs_from_disk )
        , proof )
  | Bridge (Inner_receive input) ->
      let Compile_simple.[ _; prove ] = Bridge_inst_mina.System_L2.provers in
      let%bind vk_hash =
        Compile_simple.Verification_key.of_tag Bridge_inst_mina.System_L2.tag
        |> Promise.to_deferred
        (* To make fake tests work *)
        >>| Compile_simple.Verification_key.hash
      in
      let%map (_stmt, parent_with_calls), proof =
        time ?fake_proving_time ~logger "Bridge_mina.System_L2.inner_receive"
          ( prove (Bridge.Inner_receive.of_serializable ~vk_hash input)
          |> Promise.to_deferred )
      in
      Output.Call_forest
        ( Tuple3.map_trd parent_with_calls
            ~f:
              (Zkapp_command.Call_forest.map
                 ~f:Account_update.read_all_proofs_from_disk )
        , proof )
  | Bridge (Finalize_withdrawal input) ->
      let Compile_simple.[ _; prove; _ ] =
        Bridge_inst_mina.System_L1_enabled.provers
      in
      let%bind vk_hash =
        Compile_simple.Verification_key.of_tag
          Bridge_inst_mina.System_L1_enabled.tag
        |> Promise.to_deferred
        (* To make fake tests work *)
        >>| Compile_simple.Verification_key.hash
      in
      let%bind helper_token_owner_l1_vk_hash =
        Compile_simple.Verification_key.of_tag
          Bridge_inst_mina.System_L1_token_owner.tag
        |> Promise.to_deferred
        (* To make fake tests work *)
        >>| Compile_simple.Verification_key.hash
      in
      let%bind l2_holder_vk_hash =
        Compile_simple.Verification_key.of_tag Bridge_inst_mina.System_L2.tag
        |> Promise.to_deferred
        (* To make fake tests work *)
        >>| Compile_simple.Verification_key.hash
      in
      let%map (_stmt, parent_with_calls), proof =
        time ?fake_proving_time ~logger
          "Bridge_mina.System_L1.finalize_withdrawal"
          ( prove
              (Bridge.Finalize_withdrawal.of_serializable ~proof_cache_db
                 ~vk_hash ~helper_token_owner_l1_vk_hash ~l2_holder_vk_hash
                 input )
          |> Promise.to_deferred )
      in
      Output.Call_forest
        ( Tuple3.map_trd parent_with_calls
            ~f:
              (Zkapp_command.Call_forest.map
                 ~f:Account_update.read_all_proofs_from_disk )
        , proof )
  | Bridge (Outer_token_owner input) ->
      let Compile_simple.[ prove ] =
        Bridge_inst_mina.System_L1_token_owner.provers
      in
      let%bind vk_hash =
        Compile_simple.Verification_key.of_tag
          Bridge_inst_mina.System_L1_token_owner.tag
        |> Promise.to_deferred
        (* To make fake tests work *)
        >>| Compile_simple.Verification_key.hash
      in
      let%map (_stmt, parent_with_calls), proof =
        time ?fake_proving_time ~logger
          "Bridge_mina.System_L1_token_owner.allow"
          ( prove (Bridge.Outer_token_owner.of_serializable ~vk_hash input)
          |> Promise.to_deferred )
      in
      Output.Call_forest
        ( Tuple3.map_trd parent_with_calls
            ~f:
              (Zkapp_command.Call_forest.map
                 ~f:Account_update.read_all_proofs_from_disk )
        , proof )

let run ?fake_proving_time ~logger ~port () =
  let proof_cache_db = Proof_cache_tag.create_identity_db () in
  ignore
  @@ Tcp.Server.create (Tcp.Where_to_listen.of_port port)
       ~on_handler_error:`Ignore (fun s r w ->
         [%log info] "Accepted connection from %s"
           (Socket.Address.Inet.to_string s) ;
         let rec loop () =
           match%bind
             Reader.really_read_line ~wait_time:(Time.Span.of_sec 30.) r
           with
           | None ->
               return ()
           | Some input ->
               Yojson.Safe.from_string input
               |> Input.of_yojson
               |> (function
                    | Ok input -> (
                        match%bind
                          try_with (fun () ->
                              prove ?fake_proving_time ~logger ~proof_cache_db
                                input )
                        with
                        | Ok output ->
                            return output
                        | Error e ->
                            let err = Exn.to_string e in
                            [%log error] "Error proving: %s" err ;
                            return (Output.Error err) )
                    | Error e ->
                        return (Output.Error e) )
               >>| Output.to_yojson >>| Yojson.Safe.to_string
               >>| Writer.write_line w
               >>= fun () -> loop ()
         in
         let%bind () = loop () in
         return
           ([%log info] "Closed connection from %s"
              (Socket.Address.Inet.to_string s) ) ) ;
  [%log info] "Listening on port %d\n" port ;
  Deferred.never ()
