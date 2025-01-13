open Async
open Core_kernel
open Zeko_circuits
open Mina_base

let try_connect where_to_connect =
  match%bind try_with (fun () -> Tcp.connect where_to_connect) with
  | Ok x ->
      return (Ok x)
  | Error exn ->
      return (Error (Error.of_exn exn))

module State = struct
  type lazy_connection =
    ( ([ `Active ], Socket.Address.Inet.t) Socket.t * Reader.t * Writer.t
    , Error.t )
    Result.t
    Deferred.t
    lazy_t

  type prover_state = [ `In_use | `Available ]

  type t =
    { provers :
        (lazy_connection ref * Tcp.Where_to_connect.inet * prover_state ref)
        list
    ; mutable next : int
    }

  let create provers =
    let connections =
      List.map provers ~f:(fun x ->
          (ref (lazy (try_connect x)), x, ref `Available) )
    in
    { provers = connections; next = 0 }

  let rec next_prover (t : t) =
    let rotate l n =
      let left, right = List.split_n l n in
      right @ left
    in
    t.next <- (t.next + 1) mod List.length t.provers ;
    match
      rotate t.provers t.next
      |> List.find ~f:(fun (_, _, status) ->
             match !status with `Available -> true | `In_use -> false )
    with
    | Some prover ->
        return prover
    | None ->
        let%bind () = Clock.after (Time.Span.of_sec 1.) in
        next_prover t
end

(* Get the reference of next available prover.
   If it fails to connect or times out, replace the reference with new connection and try whole thing again *)
let rec send ?(proving_timeout = 10.) ?(wait_for_prover_timeout = 600.)
    ?(attempts = 5) t (input : Prover.Input.t) : Prover.Output.t Deferred.t =
  let%bind connection_ref, where_to_connect, status =
    match%bind
      Async.with_timeout
        (Time.Span.of_sec wait_for_prover_timeout)
        (State.next_prover t)
    with
    | `Result x ->
        return x
    | `Timeout ->
        failwith "Timeout while getting prover"
  in
  status := `In_use ;
  match%bind
    Async.with_timeout
      (Time.Span.of_sec proving_timeout)
      ( match%bind Lazy.force !connection_ref with
      | Error err ->
          printf "Error connecting to prover: %s\n%!" (Error.to_string_hum err) ;
          return `Connection_error
      | Ok (_, r, w) -> (
          match%bind
            let () =
              Prover.Input.to_yojson input
              |> Yojson.Safe.to_string |> Writer.write_line w
            in
            Reader.really_read_line ~wait_time:(Time.Span.of_sec 60.) r
          with
          | Some response -> (
              match
                Yojson.Safe.from_string response |> Prover.Output.of_yojson
              with
              | Ok output ->
                  return (`Ok output)
              | Error _ ->
                  failwith "Error parsing response" )
          | None ->
              failwith "Timeout while proving" ) )
  with
  | `Result (`Ok r) ->
      status := `Available ;
      return r
  | `Timeout | `Result `Connection_error ->
      status := `Available ;
      printf "Timeout while proving %f, retrying attempts remaining: %d\n%!"
        proving_timeout attempts ;
      if attempts > 0 then (
        connection_ref := lazy (try_connect where_to_connect) ;
        send ~proving_timeout ~attempts:(attempts - 1) t input )
      else failwith "Timeout while proving"

let transaction_snark_of_single_signed_command ?proving_timeout t ~source_ledger
    ~source_acc_set ~sequencer_pk ~command ~sparse_ledger =
  send ?proving_timeout t
    (Prover.Input.Txn_snark_single_signed_command
       (source_ledger, source_acc_set, sequencer_pk, command, sparse_ledger) )
  >>| function
  | Prover.Output.Zeko_transaction_snark (stmt, proof) ->
      ({ stmt; proof } : Zeko_transaction_snark.T.t)
  | _ ->
      failwith "Unexpected response from prover"

let transaction_snark_of_single_unproved_zkapp_command ?proving_timeout t
    ~source_ledger ~target_ledger ~connecting_ledger ~source_local_state
    ~target_local_state ~fee_excess ~supply_decrease ~txn_snark_witness
    ~sequencer ~source_acc_set ~shift_action_state =
  send ?proving_timeout t
    (Prover.Input.Txn_snark_single_unproved_zkapp_command
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
       , shift_action_state ) )
  >>| function
  | Prover.Output.Zeko_transaction_snark (stmt, proof) ->
      ({ stmt; proof } : Zeko_transaction_snark.T.t)
  | _ ->
      failwith "Unexpected response from prover"

let transaction_snark_of_double_unproved_zkapp_command ?proving_timeout t
    ~source_ledger ~target_ledger ~connecting_ledger ~source_local_state
    ~target_local_state ~fee_excess ~supply_decrease ~txn_snark_witness
    ~sequencer ~source_acc_set ~shift_action_state_first
    ~shift_action_state_second =
  send ?proving_timeout t
    (Prover.Input.Txn_snark_double_unproved_zkapp_command
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
       , shift_action_state_second ) )
  >>| function
  | Prover.Output.Zeko_transaction_snark (stmt, proof) ->
      ({ stmt; proof } : Zeko_transaction_snark.T.t)
  | _ ->
      failwith "Unexpected response from prover"

let transaction_snark_of_single_proved_zkapp_command ?proving_timeout t
    ~source_ledger ~target_ledger ~connecting_ledger ~source_local_state
    ~target_local_state ~fee_excess ~supply_decrease ~txn_snark_witness
    ~sequencer ~source_acc_set ~zkapp_vk ~zkapp_proof ~shift_action_state =
  send ?proving_timeout t
    (Prover.Input.Txn_snark_single_proved_zkapp_command
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
       , shift_action_state ) )
  >>| function
  | Prover.Output.Zeko_transaction_snark (stmt, proof) ->
      ({ stmt; proof } : Zeko_transaction_snark.T.t)
  | _ ->
      failwith "Unexpected response from prover"

let transaction_snark_of_merge ?proving_timeout t
    ~(left : Zeko_transaction_snark.T.t) ~(right : Zeko_transaction_snark.T.t) =
  send ?proving_timeout t
    (Prover.Input.Txn_snark_merge
       (left.stmt, left.proof, right.stmt, right.proof) )
  >>| function
  | Prover.Output.Zeko_transaction_snark (stmt, proof) ->
      ({ stmt; proof } : Zeko_transaction_snark.T.t)
  | _ ->
      failwith "Unexpected response from prover"

let transaction_snark_of_segment ?proving_timeout t ~sequencer_pk
    ~(witness :
       Transaction_witness.Zkapp_command_segment_witness.t
       * Transaction_snark.Zkapp_command_segment.Basic.t
       * Mina_state.Snarked_ledger_state.With_sok.t ) =
  let mina_local_state_to_zeko
      (t :
        Mina_transaction_logic.Zkapp_command_logic.Local_state.Value.Stable.V1.t
        ) : Zeko_transaction_snark.Local_state.t =
    { ledger = t.ledger
    ; stack_frame = t.stack_frame
    ; call_stack = t.call_stack
    ; transaction_commitment = t.transaction_commitment
    ; full_transaction_commitment = t.full_transaction_commitment
    ; excess = t.excess
    ; account_update_index = t.account_update_index
    }
  in
  let first_account_update
      (witness : Transaction_witness.Zkapp_command_segment_witness.t) =
    match witness.local_state_init.stack_frame.calls with
    | [] ->
        with_return (fun { return } ->
            List.iter witness.start_zkapp_command ~f:(fun s ->
                Zkapp_command.Call_forest.iteri
                  ~f:(fun _i x -> return (Some x))
                  s.account_updates.account_updates ) ;
            None )
    | xs ->
        Zkapp_command.Call_forest.hd_account_update xs
  in
  let account_update_proof (p : Account_update.t) =
    match p.authorization with
    | Proof proof ->
        Some proof
    | Signature _ | None_given ->
        None
  in
  let snapp_proof_data
      ~(witness : Transaction_witness.Zkapp_command_segment_witness.t) =
    let open Option.Let_syntax in
    let%bind p = first_account_update witness in
    let%map pi = account_update_proof p in
    let vk =
      let account_id = Account_id.create p.body.public_key p.body.token_id in
      let account : Account.t =
        Mina_ledger.Sparse_ledger.(
          get_exn witness.local_state_init.ledger
            (find_index_exn witness.local_state_init.ledger account_id))
      in
      match
        Option.value_map ~default:None account.zkapp ~f:(fun s ->
            s.verification_key )
      with
      | None ->
          failwith "No verification key found in the account"
      | Some s ->
          s
    in
    (pi, vk)
  in
  match witness with
  | witness, Opt_signed, stmt ->
      transaction_snark_of_single_unproved_zkapp_command ?proving_timeout t
        ~source_ledger:stmt.source.first_pass_ledger
        ~target_ledger:stmt.target.second_pass_ledger
        ~connecting_ledger:stmt.connecting_ledger_left (* left or right? *)
        ~source_local_state:(mina_local_state_to_zeko stmt.source.local_state)
        ~target_local_state:(mina_local_state_to_zeko stmt.target.local_state)
        ~fee_excess:stmt.fee_excess.fee_excess_l
        ~supply_decrease:stmt.supply_increase.magnitude
        ~txn_snark_witness:witness ~sequencer:sequencer_pk
        ~source_acc_set:(failwith "Not implemented")
        ~shift_action_state:true
  | witness, Opt_signed_opt_signed, stmt ->
      transaction_snark_of_double_unproved_zkapp_command ?proving_timeout t
        ~source_ledger:stmt.source.first_pass_ledger
        ~target_ledger:stmt.target.second_pass_ledger
        ~connecting_ledger:stmt.connecting_ledger_left (* left or right? *)
        ~source_local_state:(mina_local_state_to_zeko stmt.source.local_state)
        ~target_local_state:(mina_local_state_to_zeko stmt.target.local_state)
        ~fee_excess:stmt.fee_excess.fee_excess_l
        ~supply_decrease:stmt.supply_increase.magnitude
        ~txn_snark_witness:witness ~sequencer:sequencer_pk
        ~source_acc_set:(failwith "Not implemented")
        ~shift_action_state_first:true ~shift_action_state_second:true
  | witness, Proved, stmt -> (
      match snapp_proof_data ~witness with
      | None ->
          failwith "of_zkapp_command_segment: Expected exactly one proof"
      | Some (p, v) ->
          transaction_snark_of_single_proved_zkapp_command ?proving_timeout t
            ~source_ledger:stmt.source.first_pass_ledger
            ~target_ledger:stmt.target.second_pass_ledger
            ~connecting_ledger:stmt.connecting_ledger_left (* left or right? *)
            ~source_local_state:
              (mina_local_state_to_zeko stmt.source.local_state)
            ~target_local_state:
              (mina_local_state_to_zeko stmt.target.local_state)
            ~fee_excess:stmt.fee_excess.fee_excess_l
            ~supply_decrease:stmt.supply_increase.magnitude
            ~txn_snark_witness:witness ~sequencer:sequencer_pk
            ~source_acc_set:(failwith "Not implemented")
            ~zkapp_vk:v.data
            ~zkapp_proof:(Compile_simple.Proof.of_pickles p)
            ~shift_action_state:true )

let transaction_snark_of_zkapp_command ?proving_timeout t ~sequencer_pk
    ~(witnesses :
       ( Transaction_witness.Zkapp_command_segment_witness.t
       * Transaction_snark.Zkapp_command_segment.Basic.t
       * Mina_state.Snarked_ledger_state.With_sok.t )
       list ) =
  match witnesses with
  | [] ->
      failwith "Empty zkapp command"
  | witness :: rest ->
      let%bind p1 =
        transaction_snark_of_segment ?proving_timeout t ~sequencer_pk ~witness
      in
      Deferred.List.fold ~init:p1 rest ~f:(fun prev witness ->
          let%bind curr =
            transaction_snark_of_segment ?proving_timeout t ~sequencer_pk
              ~witness
          in
          let%bind merged =
            transaction_snark_of_merge ?proving_timeout t ~left:curr ~right:prev
          in
          return merged )

let transaction_snark_of_signed_command ?proving_timeout t ~sequencer_pk
    ~(witness :
       Mina_ledger.Sparse_ledger.t
       * Signed_command.t
       * Transaction_snark.Statement.With_sok.t ) =
  let sparse_ledger, command, stmt = witness in
  transaction_snark_of_single_signed_command ?proving_timeout t
    ~source_ledger:stmt.source.first_pass_ledger
    ~source_acc_set:(failwith "Not implemented")
    ~sequencer_pk ~command ~sparse_ledger

let inner_sync ?proving_timeout t ~public_key ~all_actions =
  send ?proving_timeout t (Prover.Input.Inner_sync (public_key, all_actions))
  >>| function
  | Prover.Output.Call_forest_tree tree ->
      tree
  | _ ->
      failwith "Unexpected response from prover"

let outer_commit ?proving_timeout t ~txn_snark ~public_key ~new_actions
    ~unprocessed_actions ~old_inner_ledger ~new_inner_ledger ~da_signature
    ~da_key =
  send ?proving_timeout t
    (Prover.Input.Outer_commit
       ( txn_snark
       , public_key
       , new_actions
       , unprocessed_actions
       , old_inner_ledger
       , new_inner_ledger
       , da_signature
       , da_key ) )
  >>| function
  | Prover.Output.Call_forest_tree tree ->
      tree
  | _ ->
      failwith "Unexpected response from prover"

let outer_step ?proving_timeout t ~last ~outer_public_key:zkapp_pk ~new_deposits
    ~unprocessed_deposits ~old_inner_ledger ~new_inner_ledger =
  failwith "Not implemented"

let submit_deposit ?proving_timeout t ~outer_pk ~deposit =
  failwith "Not implemented"

let submit_withdrawal ?proving_timeout t ~withdrawal =
  failwith "Not implemented"

let process_deposit ?proving_timeout t ~is_new ~pointer ~before ~after ~deposit
    =
  failwith "Not implemented"

let process_withdrawal ?proving_timeout t ~outer_pk ~is_new ~pointer ~before
    ~after ~withdrawal =
  failwith "Not implemented"
