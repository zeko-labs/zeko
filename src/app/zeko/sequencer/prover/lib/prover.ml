open Core_kernel
open Async
open Mina_base
open Mina_ledger
open Signature_lib
open Zeko_circuits
module Field = Snark_params.Tick.Field

type call_forest =
  ( Account_update.t
  , Zkapp_command.Digest.Account_update.t
  , Zkapp_command.Digest.Forest.t )
  Zkapp_command.Call_forest.t
[@@deriving yojson]

type call_forest_tree =
  ( Account_update.t
  , Zkapp_command.Digest.Account_update.t
  , Zkapp_command.Digest.Forest.t )
  Zkapp_command.Call_forest.Tree.t
[@@deriving yojson]

let mktree (account_update, account_update_digest, calls) proof =
  let account_update : Account_update.t =
    { body = account_update; authorization = Proof proof }
  in
  Zkapp_command.Call_forest.Tree.
    { account_update; account_update_digest; calls }

let constraint_constants = Genesis_constants.Compiled.constraint_constants

let time ~logger label (d : 'a Deferred.t) =
  [%log info] "Starting %s\n%!" label ;
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  [%log info] "%s: %s\n%!" label
    (Time.Span.to_string_hum @@ Time.diff stop start) ;
  return x

let dummy_sok =
  Sok_message.digest
  @@ Sok_message.create ~fee:Currency.Fee.zero
       ~prover:(Public_key.compress (Keypair.create ()).public_key)

(* Unfortunately yojson doesn't support GADTs so it can't be one type, or maybe I'm just bad *)
module Input = struct
  type t =
    | Ping
    | Txn_snark_single_signed_command of
        ( Ledger_hash.t
        * Zeko_transaction_snark.Account_set.t
        * Zeko_util.Even_PC.t
        * Signed_command.t
        * Sparse_ledger.t )
    | Txn_snark_single_unproved_zkapp_command of
        ( Ledger_hash.t
        * Ledger_hash.t
        * Ledger_hash.t
        * Zeko_transaction_snark.Local_state.t
        * Zeko_transaction_snark.Local_state.t
        * Currency.Fee.Signed.t
        * Currency.Amount.t
        * Transaction_snark.Zkapp_command_segment.Witness.t
        * Zeko_util.Even_PC.t
        * Zeko_transaction_snark.Account_set.t
        * bool )
    | Txn_snark_double_unproved_zkapp_command of
        ( Ledger_hash.t
        * Ledger_hash.t
        * Ledger_hash.t
        * Zeko_transaction_snark.Local_state.t
        * Zeko_transaction_snark.Local_state.t
        * Currency.Fee.Signed.t
        * Currency.Amount.t
        * Transaction_snark.Zkapp_command_segment.Witness.t
        * Zeko_util.Even_PC.t
        * Zeko_transaction_snark.Account_set.t
        * bool
        * bool )
    | Txn_snark_single_proved_zkapp_command of
        ( Ledger_hash.t
        * Ledger_hash.t
        * Ledger_hash.t
        * Zeko_transaction_snark.Local_state.t
        * Zeko_transaction_snark.Local_state.t
        * Currency.Fee.Signed.t
        * Currency.Amount.t
        * Transaction_snark.Zkapp_command_segment.Witness.t
        * Zeko_util.Even_PC.t
        * Zeko_transaction_snark.Account_set.t
        * Pickles.Side_loaded.Verification_key.t
        * Compile_simple.Proof.t
        * bool )
    | Txn_snark_merge of
        ( Zeko_transaction_snark.Zeko_stmt.t
        * Compile_simple.Proof.t
        * Zeko_transaction_snark.Zeko_stmt.t
        * Compile_simple.Proof.t )
    | Inner_sync of
        (Public_key.Compressed.t * (Field.t list * Ase.With_length.Stmt.t))
    | Outer_commit of
        ( Zeko_transaction_snark.T.t
        * Public_key.Compressed.t
        * Field.t list
        * Field.t list
        * Sparse_ledger.t
        * Sparse_ledger.t
        * Signature.t
        * Public_key.Compressed.t )
  [@@deriving yojson]
end

module Output = struct
  type t =
    | Pong
    | Submit_deposit of Zeko_util.call_forest_tree
    | Submit_withdrawal of Zeko_util.call_forest_tree
    | Process_deposit of Zeko_util.call_forest
    | Process_withdrawal of Zeko_util.call_forest
    | Outer_step of Zeko_util.call_forest_tree
    | Inner_step of Zeko_util.call_forest_tree
    | Zeko_transaction_snark of
        (Zeko_transaction_snark.Zeko_stmt.t * Compile_simple.Proof.t)
    | Call_forest_tree of call_forest_tree
  [@@deriving yojson]
end

let prove ~logger : Input.t -> Output.t Deferred.t = function
  | Ping ->
      return Output.Pong
  | Txn_snark_single_signed_command
      (source_ledger, source_acc_set, sequencer, command, sparse_ledger) ->
      let open Zeko_transaction_snark in
      let handler = unstage @@ Sparse_ledger.handler sparse_ledger in
      let Compile_simple.[ single_signed_command; _; _; _; _ ] = provers in
      let input : Base_input.t =
        { source_ledger
        ; source_acc_set
        ; sequencer
        ; transaction =
            Mina_transaction.Transaction_union.of_transaction (Command command)
        ; witness =
            { ledger_path_handler = handler
            ; update_acc_set_witness =
                { get_account_set_x = (fun () -> failwith "get_account_set_x")
                ; get_account_set_z = (fun () -> failwith "get_account_set_z")
                ; get_account_set_x_path =
                    (fun () -> failwith "get_account_set_x_path")
                ; get_account_set_y_path =
                    (fun () -> failwith "get_account_set_y_path")
                }
            }
        }
      in
      let%map stmt, proof =
        time ~logger "Zeko_transaction_snark.single_signed_command"
          (single_signed_command input |> Promise.to_deferred)
      in
      Output.Zeko_transaction_snark (stmt, proof)
  | Txn_snark_single_unproved_zkapp_command
      ( source_ledger
      , target_ledger
      , connecting_ledger
      , source_local_state
      , target_local_state
      , fee_excess
      , supply_decrease
      , txn_snark_witness
      , sequencer
      , source_acc_set
      , shift_action_state ) ->
      let open Zeko_transaction_snark in
      let Compile_simple.[ _; single_unproved_zkapp_command; _; _; _ ] =
        provers
      in
      let input : Zkapp_single_unproved_input.t =
        { base =
            { source_ledger
            ; target_ledger
            ; connecting_ledger
            ; source_local_state
            ; target_local_state
            ; fee_excess
            ; supply_decrease
            ; witness =
                { txn_snark_witness
                ; update_acc_set_witness =
                    { get_account_set_x =
                        (fun () -> failwith "get_account_set_x")
                    ; get_account_set_z =
                        (fun () -> failwith "get_account_set_z")
                    ; get_account_set_x_path =
                        (fun () -> failwith "get_account_set_x_path")
                    ; get_account_set_y_path =
                        (fun () -> failwith "get_account_set_y_path")
                    }
                }
            ; sequencer
            ; source_acc_set
            }
        ; shift_action_state
        }
      in
      let%map stmt, proof =
        time ~logger "Zeko_transaction_snark.single_unproved_zkapp_command"
          (single_unproved_zkapp_command input |> Promise.to_deferred)
      in
      Output.Zeko_transaction_snark (stmt, proof)
  | Txn_snark_double_unproved_zkapp_command
      ( source_ledger
      , target_ledger
      , connecting_ledger
      , source_local_state
      , target_local_state
      , fee_excess
      , supply_decrease
      , txn_snark_witness
      , sequencer
      , source_acc_set
      , shift_action_state_first
      , shift_action_state_second ) ->
      let open Zeko_transaction_snark in
      let Compile_simple.[ _; _; double_unproved_zkapp_command; _; _ ] =
        provers
      in
      let input : Zkapp_double_unproved_input.t =
        { base =
            { source_ledger
            ; target_ledger
            ; connecting_ledger
            ; source_local_state
            ; target_local_state
            ; fee_excess
            ; supply_decrease
            ; witness =
                { txn_snark_witness
                ; update_acc_set_witness =
                    { get_account_set_x =
                        (fun () -> failwith "get_account_set_x")
                    ; get_account_set_z =
                        (fun () -> failwith "get_account_set_z")
                    ; get_account_set_x_path =
                        (fun () -> failwith "get_account_set_x_path")
                    ; get_account_set_y_path =
                        (fun () -> failwith "get_account_set_y_path")
                    }
                }
            ; sequencer
            ; source_acc_set
            }
        ; shift_action_state_first
        ; shift_action_state_second
        }
      in
      let%map stmt, proof =
        time ~logger "Zeko_transaction_snark.double_unproved_zkapp_command"
          (double_unproved_zkapp_command input |> Promise.to_deferred)
      in
      Output.Zeko_transaction_snark (stmt, proof)
  | Txn_snark_single_proved_zkapp_command
      ( source_ledger
      , target_ledger
      , connecting_ledger
      , source_local_state
      , target_local_state
      , fee_excess
      , supply_decrease
      , txn_snark_witness
      , sequencer
      , source_acc_set
      , zkapp_vk
      , zkapp_proof
      , shift_action_state ) ->
      let open Zeko_transaction_snark in
      let Compile_simple.[ _; _; _; single_proved_zkapp_command; _ ] =
        provers
      in
      let input : Zkapp_single_proved_input.t =
        { base =
            { source_ledger
            ; target_ledger
            ; connecting_ledger
            ; source_local_state
            ; target_local_state
            ; fee_excess
            ; supply_decrease
            ; witness =
                { txn_snark_witness
                ; update_acc_set_witness =
                    { get_account_set_x =
                        (fun () -> failwith "get_account_set_x")
                    ; get_account_set_z =
                        (fun () -> failwith "get_account_set_z")
                    ; get_account_set_x_path =
                        (fun () -> failwith "get_account_set_x_path")
                    ; get_account_set_y_path =
                        (fun () -> failwith "get_account_set_y_path")
                    }
                }
            ; sequencer
            ; source_acc_set
            }
        ; zkapp_vk
        ; zkapp_proof
        ; shift_action_state
        }
      in
      let%map stmt, proof =
        time ~logger "Zeko_transaction_snark.single_proved_zkapp_command"
          (single_proved_zkapp_command input |> Promise.to_deferred)
      in
      Output.Zeko_transaction_snark (stmt, proof)
  | Txn_snark_merge (left_stmt, left_proof, right_stmt, right_proof) ->
      let open Zeko_transaction_snark in
      let Compile_simple.[ _; _; _; _; merge ] = provers in
      let input : Merge_input.t =
        { left = { stmt = left_stmt; proof = left_proof }
        ; right = { stmt = right_stmt; proof = right_proof }
        }
      in
      let%map stmt, proof =
        time ~logger "Zeko_transaction_snark.merge"
          (merge input |> Promise.to_deferred)
      in
      Output.Zeko_transaction_snark (stmt, proof)
  | Inner_sync (public_key, ase) ->
      let open Inner_rules in
      let Compile_simple.[ inner_sync; _ ] = provers in
      let%bind vk =
        Compile_simple.Verification_key.of_tag Inner_rules.tag
        |> Promise.to_deferred
      in
      let%bind ase =
        Rule_inner_sync.Ase_inst.fold (snd ase) (fst ase) |> Promise.to_deferred
      in
      let input =
        ( { public_key
          ; vk_hash =
              Zkapp_account.digest_vk
                (Compile_simple.Verification_key.to_pickles vk)
          ; ase
          }
          : Rule_inner_sync.Witness.t )
      in
      let%map (a, au), proof =
        time ~logger "Inner_rules.inner_sync"
          (inner_sync input |> Promise.to_deferred)
      in
      Output.Call_forest_tree
        (mktree au (Compile_simple.Proof.to_pickles proof))
  | Outer_commit
      ( txn_snark
      , public_key
      , outer_ase_fields
      , inner_ase_fields
      , old_inner_ledger
      , new_inner_ledger
      , da_signature
      , da_key ) ->
      let open Outer_rules in
      let Compile_simple.[ commit; _; _ ] = provers in
      let%bind vk =
        Compile_simple.Verification_key.of_tag Outer_rules.tag
        |> Promise.to_deferred
      in
      let old_inner_acc =
        Mina_ledger.Sparse_ledger.get_exn old_inner_ledger
          Zeko_constants.inner_account_index
      in
      let old_inner_acc_path =
        List.map ~f:(function
          | `Left _ ->
              ( { right_side = Field.zero }
                : Outer_rules.Rule_commit_inst.PathElt.t )
          | `Right _ ->
              ( { right_side = Field.one }
                : Outer_rules.Rule_commit_inst.PathElt.t ) )
        @@ Mina_ledger.Sparse_ledger.path_exn old_inner_ledger
             Zeko_constants.inner_account_index
      in
      let new_inner_acc =
        Mina_ledger.Sparse_ledger.get_exn new_inner_ledger
          Zeko_constants.inner_account_index
      in
      let new_inner_acc_path =
        List.map ~f:(function
          | `Left _ ->
              ( { right_side = Field.zero }
                : Outer_rules.Rule_commit_inst.PathElt.t )
          | `Right _ ->
              ( { right_side = Field.one }
                : Outer_rules.Rule_commit_inst.PathElt.t ) )
        @@ Mina_ledger.Sparse_ledger.path_exn new_inner_ledger
             Zeko_constants.inner_account_index
      in
      let%bind outer_ase =
        let ({ outer_action_state } : Rollup_state.Inner_state.t) =
          Rollup_state.Inner_state.value_of_app_state
            (Option.value_exn new_inner_acc.zkapp).app_state
        in
        let action_state =
          Rollup_state.Outer_action_state.With_length.raw outer_action_state
        in
        Rule_commit.Ase_outer_inst.fold action_state outer_ase_fields
        |> Promise.to_deferred
      in
      let%bind inner_ase =
        let ({ outer_action_state } : Rollup_state.Inner_state.t) =
          Rollup_state.Inner_state.value_of_app_state
            (Option.value_exn old_inner_acc.zkapp).app_state
        in
        let action_state : Ase.With_length.Stmt.t =
          Rollup_state.Outer_action_state.With_length.
            { action_state = raw outer_action_state
            ; length = length outer_action_state
            }
        in
        Rule_commit.Ase_inner_inst.fold action_state inner_ase_fields
        |> Promise.to_deferred
      in
      let%bind verify_both_ases =
        let Compile_simple.[ prove ] = Rule_commit.Verify_both_ases.provers in
        let%map out, proof =
          prove (outer_ase, inner_ase) |> Promise.to_deferred
        in
        Rule_commit.Verify_both_ases.make_unchecked ~proof out
      in
      let input =
        ( { txn_snark =
              Zeko_transaction_snark.make_unchecked ~proof:txn_snark.proof
                txn_snark.stmt
          ; public_key
          ; vk_hash =
              Zkapp_account.digest_vk
                (Compile_simple.Verification_key.to_pickles vk)
          ; verify_both_ases
          ; old_inner_acc
          ; old_inner_acc_path
          ; new_inner_acc
          ; new_inner_acc_path
          ; da_signature
          ; da_key
          }
          : Outer_rules.Rule_commit_inst.Witness.t )
      in
      let%map (a, au), proof =
        time ~logger "Outer_rules.commit" (commit input |> Promise.to_deferred)
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
                            return (Exn.to_string e) )
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
