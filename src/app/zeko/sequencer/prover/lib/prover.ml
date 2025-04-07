open Core_kernel
open Async
open Mina_base
open Zeko_circuits
open Zeko_types

let mktree (account_update, account_update_digest, calls) proof =
  let account_update : Account_update.t =
    { body = account_update; authorization = Proof proof }
  in
  Zkapp_command.Call_forest.Tree.
    { account_update; account_update_digest; calls }

let constraint_constants = Genesis_constants.Compiled.constraint_constants

let time ~logger label (d : 'a Deferred.t) =
  [%log info] "Starting %s%!" label ;
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  [%log info] "%s: %s\n%!" label
    (Time.Span.to_string_hum @@ Time.diff stop start) ;
  return x

module Make_folder (System : sig
  module Stmt : sig
    type t [@@deriving yojson]
  end

  type trans = { source : Stmt.t; target : Stmt.t }

  val leaf : F.t list * Stmt.t -> (trans * Compile_simple.Proof.t) Promise.t

  val leaf_option :
    F.t list * Stmt.t -> (trans * Compile_simple.Proof.t) Promise.t

  val extend :
       F.t list * (trans * Compile_simple.Proof.t)
    -> (trans * Compile_simple.Proof.t) Promise.t

  val extend_option :
       F.t list * (trans * Compile_simple.Proof.t)
    -> (trans * Compile_simple.Proof.t) Promise.t

  val leaf_iterations : int

  val leaf_option_iterations : int

  val extend_iterations : int

  val extend_option_iterations : int
end) =
struct
  type out_t = Compile_simple.Proof.t option * System.Stmt.t [@@deriving yojson]

  let fold ~source ~elems : out_t Promise.t =
    match elems with
    | [] ->
        (* No need for folding, everything goes to excess *)
        Promise.return (None, source)
    | elems_to_prove ->
        (* Need to fold *)
        let leaf_prover, (leaf_elems, rest) =
          if List.length elems_to_prove > System.leaf_iterations then
            (* Full leaf *)
            (System.leaf, List.split_n elems_to_prove System.leaf_iterations)
          else
            (* Partial leaf *)
            ( System.leaf_option
            , List.split_n elems_to_prove System.leaf_option_iterations )
        in
        let%bind.Promise leaf = leaf_prover (leaf_elems, source) in
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

(* Unfortunately yojson doesn't support GADTs so it can't be one type, or maybe I'm just bad *)
module Input = struct
  module Txn_snark = struct
    type t =
      | Signed_command of Base_input.serializable
      | Zkapp_command of Txn_snark_witness.Zkapp_command_segment.t
      | Merge of Merge_input.t
    [@@deriving yojson]
  end

  module Ase = struct
    type t =
      | With_length of (Ase.With_length.Stmt.t * F.t list)
      | Without_length of (Ase.Without_length.Stmt.t * F.t list)
    [@@deriving yojson]
  end

  type t =
    | Ping
    | Txn_snark of Txn_snark.t
    | Ase of Ase.t
    | Inner_sync of Inner_sync.Witness.serializable
    | Verify_both_ases of
        ( Outer_commit.Ase_outer_inst.serializable
        * Outer_commit.Ase_inner_inst.serializable )
    | Outer_commit of Outer_commit.Witness.serializable
  [@@deriving yojson]
end

module Output = struct
  module Ase = struct
    type t =
      | With_length of Folder_with_length.out_t
      | Without_length of Folder_without_length.out_t
    [@@deriving yojson]
  end

  type t =
    | Pong
    | Txn_snark of (Zeko_stmt.t * Compile_simple.Proof.t)
    | Ase of Ase.t
    | Verify_both_ases of Outer_commit.Verify_both_ases.serializable
    | Call_forest_tree of
        ( Account_update.t
        , Zkapp_command.Digest.Account_update.t
        , Zkapp_command.Digest.Forest.t )
        Zkapp_command.Call_forest.Tree.t
  [@@deriving yojson]
end

let prove ~logger : Input.t -> Output.t Deferred.t = function
  | Ping ->
      let%bind () = time ~logger "Pong" Deferred.unit in
      return Output.Pong
  | Txn_snark (Signed_command input) ->
      let Compile_simple.[ prove; _; _; _; _ ] = Txn_rules.provers in
      let%map stmt, proof =
        time ~logger "Txn_rules.single_signed_command"
          (prove (Base_input.of_serializable input) |> Promise.to_deferred)
      in
      Output.Txn_snark (stmt, proof)
  | Txn_snark (Zkapp_command (Single_unproved input)) ->
      let Compile_simple.[ _; prove; _; _; _ ] = Txn_rules.provers in
      let%map stmt, proof =
        time ~logger "Txn_rules.single_unproved_zkapp_command"
          ( prove (Zkapp_single_unproved_input.of_serializable input)
          |> Promise.to_deferred )
      in
      Output.Txn_snark (stmt, proof)
  | Txn_snark (Zkapp_command (Double_unproved input)) ->
      let Compile_simple.[ _; _; prove; _; _ ] = Txn_rules.provers in
      let%map stmt, proof =
        time ~logger "Txn_rules.double_unproved_zkapp_command"
          ( prove (Zkapp_double_unproved_input.of_serializable input)
          |> Promise.to_deferred )
      in
      Output.Txn_snark (stmt, proof)
  | Txn_snark (Zkapp_command (Single_proved input)) ->
      let Compile_simple.[ _; _; _; prove; _ ] = Txn_rules.provers in
      let%map stmt, proof =
        time ~logger "Txn_rules.single_proved_zkapp_command"
          ( prove (Zkapp_single_proved_input.of_serializable input)
          |> Promise.to_deferred )
      in
      Output.Txn_snark (stmt, proof)
  | Txn_snark (Merge input) ->
      let Compile_simple.[ _; _; _; _; prove ] = Txn_rules.provers in
      let%map stmt, proof =
        time ~logger "Txn_rules.merge" (prove input |> Promise.to_deferred)
      in
      Output.Txn_snark (stmt, proof)
  | Inner_sync input ->
      let Compile_simple.[ prove; _ ] = Inner_rules.provers in
      let%bind vk_hash =
        Compile_simple.Verification_key.of_tag Inner_rules.tag
        |> Promise.to_deferred
        >>| Fn.compose Zkapp_account.digest_vk
              Compile_simple.Verification_key.to_pickles
      in
      let%map (_stmt, au), proof =
        time ~logger "Inner_rules.inner_sync"
          ( prove (Inner_sync.Witness.of_serializable ~vk_hash input)
          |> Promise.to_deferred )
      in
      Output.Call_forest_tree
        (mktree au (Compile_simple.Proof.to_pickles proof))
  | Ase (With_length (source, elems)) ->
      let%map snark =
        time ~logger "Folder_with_length.fold"
          (Folder_with_length.fold ~source ~elems |> Promise.to_deferred)
      in
      Output.(Ase (With_length snark))
  | Ase (Without_length (source, elems)) ->
      let%map snark =
        time ~logger "Folder_without_length.fold"
          (Folder_without_length.fold ~source ~elems |> Promise.to_deferred)
      in
      Output.(Ase (Without_length snark))
  | Verify_both_ases (outer, inner) ->
      let Compile_simple.[ prove ] = Rule_commit.Verify_both_ases.provers in
      let%map stmt, proof =
        time ~logger "Rule_commit.verify_both_ases"
          ( prove
              Outer_commit.
                ( Ase_outer_inst.of_serializable outer
                , Ase_inner_inst.of_serializable inner )
          |> Promise.to_deferred )
      in
      Output.Verify_both_ases (stmt, proof)
  | Outer_commit input ->
      let Compile_simple.[ prove; _; _ ] = Outer_rules.provers in
      let%bind vk_hash =
        Compile_simple.Verification_key.of_tag Outer_rules.tag
        |> Promise.to_deferred
        >>| Fn.compose Zkapp_account.digest_vk
              Compile_simple.Verification_key.to_pickles
      in
      let%map (_stmt, au), proof =
        time ~logger "Outer_rules.commit"
          ( prove (Outer_commit.Witness.of_serializable ~vk_hash input)
          |> Promise.to_deferred )
      in
      Output.Call_forest_tree
        (mktree au (Compile_simple.Proof.to_pickles proof))

let run ~logger ~port =
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
                        match%bind try_with (fun () -> prove ~logger input) with
                        | Ok output ->
                            return
                              (Yojson.Safe.to_string @@ Output.to_yojson output)
                        | Error e ->
                            let err = Exn.to_string e in
                            [%log error] "Error proving: %s" err ;
                            return err )
                    | Error e ->
                        return e )
               >>| Writer.write_line w
               >>= fun () -> loop ()
         in
         let%bind () = loop () in
         return
           ([%log info] "Closed connection from %s"
              (Socket.Address.Inet.to_string s) ) ) ;
  [%log info] "Listening on port %d\n" port ;
  Deferred.never ()
