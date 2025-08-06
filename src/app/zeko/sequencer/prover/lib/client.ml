open Async
open Core_kernel
open Mina_base
open Zeko_circuits
open Zeko_types
module Field = Snark_params.Tick.Field

let try_connect where_to_connect =
  match%bind try_with (fun () -> Tcp.connect where_to_connect) with
  | Ok x ->
      return (Ok x)
  | Error exn ->
      return (Error (Error.of_exn exn))

type lazy_connection =
  ( ([ `Active ], Socket.Address.Inet.t) Socket.t * Reader.t * Writer.t
  , Error.t )
  Result.t
  Deferred.t

type prover = lazy_connection ref * Tcp.Where_to_connect.inet

type t = { q : prover Priority_throttle.t; logger : Logger.t }

let create ?(ping_interval = 15.) ?(ping_timeout = 10.) ~logger provers =
  let connections = List.map provers ~f:(fun x -> (ref (try_connect x), x)) in
  let q = Priority_throttle.create_with ~continue_on_error:true connections in
  (* Start pinging *)
  let ping_loop () =
    List.iter connections ~f:(fun _ ->
        don't_wait_for
        @@ Priority_throttle.enqueue q (fun (connection_ref, _) ->
               match%bind !connection_ref with
               | Error err ->
                   [%log error] "Error pinging prover: %s"
                     (Error.to_string_hum err) ;
                   return ()
               | Ok (_, r, w) ->
                   let () =
                     Prover.Input.to_yojson Prover.Input.Ping
                     |> Yojson.Safe.to_string |> Writer.write_line w
                   in
                   let%bind _result =
                     Reader.really_read_line
                       ~wait_time:(Time.Span.of_sec ping_timeout)
                       r
                   in
                   return () ) )
  in
  every ~continue_on_error:true (Time.Span.of_sec ping_interval) ping_loop ;
  { q; logger }

let queue_size t = Priority_throttle.num_jobs_waiting_to_start t.q

let wait_to_finish t = Priority_throttle.prior_jobs_done t.q

(* Get the reference of next available prover.
   If it fails to connect or times out, replace the reference with new connection and try whole thing again *)
let send' enqueue ?(proving_timeout = 20.) ?(attempts = 5) ?(cooldown = 2.) t
    (input : Prover.Input.t) : Prover.Output.t Deferred.t =
  let logger = t.logger in
  enqueue t.q (fun (connection_ref, where_to_connect) ->
      let rec go ~attempts =
        let%bind result =
          match%bind !connection_ref with
          | Error err ->
              [%log error] "Error connecting to prover: %s"
                (Error.to_string_hum err) ;
              return `Connnection_error
          | Ok (s, r, w) -> (
              match%map
                Async.with_timeout
                  (Time.Span.of_sec proving_timeout)
                  ( Prover.Input.to_yojson input
                    |> Yojson.Safe.to_string |> Writer.write_line w ;
                    Reader.really_read_line
                      ~wait_time:(Time.Span.of_sec proving_timeout)
                      r )
              with
              | `Result (Some response) -> (
                  match
                    Yojson.Safe.from_string response |> Prover.Output.of_yojson
                  with
                  | Ok output ->
                      `Ok output
                  | Error _ ->
                      [%log error] "Error parsing response from prover" ;
                      `Parsing_error )
              | `Timeout | `Result None ->
                  Socket.shutdown s `Both ;
                  [%log warn] "Timeout from prover, remaining attempts: %d"
                    (attempts - 1) ;
                  `Timeout )
        in
        match result with
        | `Timeout | `Connnection_error | `Parsing_error ->
            let%bind () = after (Time.Span.of_sec cooldown) in
            connection_ref := try_connect where_to_connect ;
            if attempts > 0 then go ~attempts:(attempts - 1)
            else return (Prover.Output.Error "Failed to prove")
        | `Ok result ->
            return result
      in
      go ~attempts )

type sendfn =
     ?proving_timeout:float
  -> ?attempts:int
  -> ?cooldown:float
  -> t
  -> Prover.Input.t
  -> Prover.Output.t Deferred.t

let send : sendfn = send' Priority_throttle.enqueue

let send_with_priority : sendfn = send' Priority_throttle.push_front

let transaction_snark ?proving_timeout t input =
  send ?proving_timeout t (Prover.Input.Txn_snark input)
  >>| function
  | Prover.Output.Txn_snark snark ->
      snark
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let ase_with_length ~(sendfn : sendfn) ?proving_timeout t input =
  sendfn ?proving_timeout t (Prover.Input.Folder (Ase_with_length input))
  >>| function
  | Prover.Output.Folder (Ase_with_length ase) ->
      ase
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let ase_without_length ~(sendfn : sendfn) ?proving_timeout t input =
  sendfn ?proving_timeout t (Prover.Input.Folder (Ase_without_length input))
  >>| function
  | Prover.Output.Folder (Ase_without_length ase) ->
      ase
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let check_accepted_mina ?proving_timeout t input =
  send ?proving_timeout t (Prover.Input.Folder (Check_accepted_mina input))
  >>| function
  | Prover.Output.Folder (Check_accepted_mina check_accepted) ->
      check_accepted
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let folder (type target) t ~source ~elems ~max_excess
    (module Folder_iterations : Zeko_constants.FOLDER_ITERATIONS)
    (prover :
         ?proving_timeout:float
      -> _
      -> _
      -> (Compile_simple.Proof.t option * target) Deferred.t ) =
  let elems_to_prove, excess =
    let i = ref 0 in
    let l = List.length elems in
    List.split_while elems ~f:(fun _ ->
        let r = !i < l - max_excess in
        i := !i + 1 ;
        r )
  in
  match elems_to_prove with
  | [] ->
      return (None, source, excess)
  | elems_to_prove ->
      (* TODO: This is a hack to get the number of proofs. *)
      let number_of_proofs =
        (List.length elems_to_prove / Folder_iterations.extend_option_iterations)
        + 1
        |> Float.of_int
      in
      let%bind proof, target =
        prover ~proving_timeout:(20. *. number_of_proofs) t
          (source, elems_to_prove)
      in
      return (proof, target, excess)

let inner_sync ?proving_timeout t ~public_key ~ase_source ~ase_elms =
  let%bind ase =
    let%map proof, target, excess =
      folder t ~source:ase_source ~elems:ase_elms
        ~max_excess:Zeko_constants.Max_excess_actions.Inner_sync.outer
        (module Zeko_constants.Folder_iterations.Ase.With_length)
        (ase_with_length ~sendfn:send_with_priority)
    in
    Inner_sync.Ase_inst.
      { proof; proof_target = target; init = ase_source; excess }
  in
  send_with_priority ?proving_timeout t
    (Prover.Input.Inner_sync { public_key; ase })
  >>| function
  | Prover.Output.Call_forest (parent_with_calls, proof) ->
      (parent_with_calls, proof)
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let verify_both_ases ?proving_timeout t input =
  send_with_priority ?proving_timeout t (Prover.Input.Verify_both_ases input)
  >>| function
  | Prover.Output.Verify_both_ases snark ->
      snark
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let outer_commit ?proving_timeout t ~txn_snark ~public_key ~inner_ase_source
    ~new_inner_actions ~unprocessed_actions ~(old_inner_acc : Account.t)
    ~old_inner_acc_path ~(new_inner_acc : Account.t) ~new_inner_acc_path
    ~da_signature ~da_key ~slot_range =
  (* Counting length of inner action state *)
  let%bind inner_ase =
    let%map proof, target, excess =
      folder t ~source:inner_ase_source ~elems:new_inner_actions
        ~max_excess:Zeko_constants.Max_excess_actions.Commit.inner
        (module Zeko_constants.Folder_iterations.Ase.With_length)
        (ase_with_length ~sendfn:send_with_priority)
    in
    Outer_commit.Ase_inner_inst.
      { proof; proof_target = target; init = inner_ase_source; excess }
  in
  (* Delay ASE *)
  let%bind outer_ase =
    let ({ outer_action_state } : Rollup_state.Inner_state.t) =
      Rollup_state.Inner_state.value_of_app_state
        (Option.value_exn new_inner_acc.zkapp).app_state
    in
    let action_state =
      Rollup_state.Outer_action_state.With_length.raw outer_action_state
    in
    let%map proof, target, excess =
      folder t ~source:action_state ~elems:unprocessed_actions
        ~max_excess:Zeko_constants.Max_excess_actions.Commit.outer
        (module Zeko_constants.Folder_iterations.Ase.Without_length)
        (ase_without_length ~sendfn:send_with_priority)
    in
    Outer_commit.Ase_outer_inst.
      { proof; proof_target = target; init = action_state; excess }
  in
  let%bind verify_both_ases = verify_both_ases t (outer_ase, inner_ase) in
  send_with_priority ?proving_timeout t
    (Prover.Input.Outer_commit
       { txn_snark
       ; public_key
       ; verify_both_ases
       ; old_inner_acc
       ; old_inner_acc_path
       ; new_inner_acc
       ; new_inner_acc_path
       ; da_signature
       ; da_key
       ; slot_range
       } )
  >>| function
  | Prover.Output.Call_forest (parent_with_calls, proof) ->
      (parent_with_calls, proof)
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let outer_action_witness ?proving_timeout t witness =
  send ?proving_timeout t Prover.Input.(Bridge (Outer_action_witness witness))
  >>| function
  | Prover.Output.Call_forest (parent_with_calls, proof) ->
      Ok (parent_with_calls, proof)
  | Prover.Output.Error err ->
      Error err
  | _ ->
      failwith "Unexpected response from prover"

let inner_action_witness ?proving_timeout t witness =
  send ?proving_timeout t Prover.Input.(Bridge (Inner_action_witness witness))
  >>| function
  | Prover.Output.Call_forest (parent_with_calls, proof) ->
      Ok (parent_with_calls, proof)
  | Prover.Output.Error err ->
      Error err
  | _ ->
      failwith "Unexpected response from prover"

let finalize_deposit ?proving_timeout t ~public_key ~may_use_token
    ~inner_authorization_kind ~(ase : Ase.With_length.Stmt.t * Field.t list)
    ~(check_accepted :
       Bridge.Finalize_deposit.Check_accepted_mina.Init.t
       * Field.t
       * Bridge.Finalize_deposit.Check_accepted_mina.Elem.t list )
    ~prev_next_deposit =
  let%bind ase =
    let ase_source, ase_elms = ase in
    let%map proof, target, excess =
      folder t ~source:ase_source ~elems:ase_elms
        ~max_excess:Zeko_constants.Max_excess_actions.Finalize_deposit.outer
        (module Zeko_constants.Folder_iterations.Ase.With_length)
        (ase_with_length ~sendfn:send)
    in
    Bridge.Finalize_deposit.Ase_inst.
      { proof; proof_target = target; init = ase_source; excess }
  in
  let%bind check_accepted =
    let init, deposit_hash, elems = check_accepted in
    let source : Bridge.Finalize_deposit.Check_accepted_mina.Stmt.t =
      { params = init.params
      ; action_state =
          Zkapp_account.Actions_impl.push_hash
            (Rollup_state.Outer_action_state.raw init.original_action_state)
            deposit_hash
          |> Rollup_state.Outer_action_state.unsafe_value_of_field
      ; deposit_index = init.deposit_index
      ; n_steps = Zeko_util.Checked32.zero
      ; is_rejected = false
      ; is_accepted = false
      }
    in
    let%map proof, target, excess =
      folder t ~source ~elems
        ~max_excess:
          Zeko_constants.Max_excess_actions.Finalize_deposit.check_accepted
        (module Zeko_constants.Folder_iterations.Check_accepted)
        check_accepted_mina
    in
    ( { proof; proof_source = source; proof_target = target; init; excess }
      : Bridge.Finalize_deposit.Check_accepted_mina.serializable )
  in
  send ?proving_timeout t
    Prover.Input.(
      Bridge
        (Finalize_deposit
           { public_key
           ; may_use_token
           ; inner_authorization_kind
           ; ase
           ; check_accepted
           ; prev_next_deposit
           } ))
  >>| function
  | Prover.Output.Call_forest (parent_with_calls, proof) ->
      Ok (parent_with_calls, proof)
  | Prover.Output.Error err ->
      Error err
  | _ ->
      failwith "Unexpected response from prover"

let inner_receive ?proving_timeout t witness =
  send ?proving_timeout t Prover.Input.(Bridge (Inner_receive witness))
  >>| function
  | Prover.Output.Call_forest (parent_with_calls, proof) ->
      Ok (parent_with_calls, proof)
  | Prover.Output.Error err ->
      Error err
  | _ ->
      failwith "Unexpected response from prover"

let finalize_withdrawal ?proving_timeout t ~public_key ~may_use_token
    ~outer_authorization_kind ~commit ~before_commit ~commit_ase
    ~before_withdrawal ~withdrawal_ase ~prev_next_withdrawal ~withdrawal_params
    =
  let%bind commit_ase =
    let source, elems = commit_ase in
    let%map proof, target, excess =
      folder t ~source ~elems
        ~max_excess:Zeko_constants.Max_excess_actions.Finalize_withdrawal.outer
        (module Zeko_constants.Folder_iterations.Ase.Without_length)
        (ase_without_length ~sendfn:send)
    in
    Bridge.Finalize_withdrawal.Ase_outer_inst.
      { proof; proof_target = target; init = source; excess }
  in
  let%bind withdrawal_ase =
    let source, elems = withdrawal_ase in
    let%map proof, target, excess =
      folder t ~source ~elems
        ~max_excess:Zeko_constants.Max_excess_actions.Finalize_withdrawal.inner
        (module Zeko_constants.Folder_iterations.Ase.With_length)
        (ase_with_length ~sendfn:send)
    in
    Bridge.Finalize_withdrawal.Ase_inner_inst.
      { proof; proof_target = target; init = source; excess }
  in
  send ?proving_timeout t
    Prover.Input.(
      Bridge
        (Finalize_withdrawal
           { public_key
           ; may_use_token
           ; outer_authorization_kind
           ; commit
           ; before_commit
           ; commit_ase
           ; before_withdrawal
           ; withdrawal_ase
           ; prev_next_withdrawal
           ; withdrawal_params
           } ))
  >>| function
  | Prover.Output.Call_forest (parent_with_calls, proof) ->
      Ok (parent_with_calls, proof)
  | Prover.Output.Error err ->
      Error err
  | _ ->
      failwith "Unexpected response from prover"

let outer_token_owner ?proving_timeout t witness =
  send ?proving_timeout t Prover.Input.(Bridge (Outer_token_owner witness))
  >>| function
  | Prover.Output.Call_forest (parent_with_calls, proof) ->
      Ok (parent_with_calls, proof)
  | Prover.Output.Error err ->
      Error err
  | _ ->
      failwith "Unexpected response from prover"
