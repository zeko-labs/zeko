open Async
open Core_kernel
open Mina_base
open Mina_ledger
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

type t = { q : prover Throttle.t }

let create ?(ping_interval = 15.) ?(ping_timeout = 10.) provers =
  let connections = List.map provers ~f:(fun x -> (ref (try_connect x), x)) in
  let q = Throttle.create_with ~continue_on_error:true connections in
  (* Start pinging *)
  let rec ping_loop () =
    let%map () = after (Time.Span.of_sec ping_interval) in
    List.iter connections ~f:(fun _ ->
        don't_wait_for
        @@ Throttle.enqueue q (fun (connection_ref, _) ->
               match%bind !connection_ref with
               | Error _ ->
                   return ()
               | Ok (_, r, w) ->
                   let () =
                     Prover.Input.to_yojson Prover.Input.Ping
                     |> Yojson.Safe.to_string |> Writer.write_line w
                   in
                   let%map _result =
                     Reader.really_read_line
                       ~wait_time:(Time.Span.of_sec ping_timeout)
                       r
                   in
                   () ) )
  in
  don't_wait_for @@ ping_loop () ;
  { q }

let queue_size t = Throttle.num_jobs_waiting_to_start t.q

(* Get the reference of next available prover.
   If it fails to connect or times out, replace the reference with new connection and try whole thing again *)
let rec send ?(proving_timeout = 20.) ?(attempts = 5) ?(cooldown = 2.) t
    (input : Prover.Input.t) : Prover.Output.t Deferred.t =
  Throttle.enqueue t.q (fun (connection_ref, where_to_connect) ->
      let rec go ~attempts =
        let%bind result =
          match%bind !connection_ref with
          | Error err ->
              printf "Error connecting to prover: %s\n%!"
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
                      print_endline "Error parsing response from prover" ;
                      `Parsing_error )
              | `Timeout | `Result None ->
                  Socket.shutdown s `Both ;
                  printf "Timeout from prover, remaining attempts: %d\n%!"
                    (attempts - 1) ;
                  `Timeout )
        in
        match result with
        | `Timeout | `Connnection_error | `Parsing_error ->
            let%bind () = after (Time.Span.of_sec cooldown) in
            connection_ref := try_connect where_to_connect ;
            go ~attempts:(attempts - 1)
        | `Ok result ->
            return result
      in
      go ~attempts )

let transaction_snark ?proving_timeout t input =
  send ?proving_timeout t (Prover.Input.Txn_snark input)
  >>| function
  | Prover.Output.Txn_snark snark ->
      snark
  | _ ->
      failwith "Unexpected response from prover"

let ase_with_length ?proving_timeout t input =
  send ?proving_timeout t (Prover.Input.Ase (With_length input))
  >>| function
  | Prover.Output.Ase (With_length ase) ->
      ase
  | _ ->
      failwith "Unexpected response from prover"

let ase_without_length ?proving_timeout t input =
  send ?proving_timeout t (Prover.Input.Ase (Without_length input))
  >>| function
  | Prover.Output.Ase (Without_length ase) ->
      ase
  | _ ->
      failwith "Unexpected response from prover"

let ase (type target) t ~source ~elems ~max_excess
    (prover : _ -> _ -> (Compile_simple.Proof.t option * target) Deferred.t) =
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
      let%bind proof, target = prover t (source, elems_to_prove) in
      return (proof, target, excess)

let inner_sync ?proving_timeout t ~public_key ~ase_source ~ase_elms =
  let%bind ase =
    let%map proof, target, excess =
      ase t ~source:ase_source ~elems:ase_elms
        ~max_excess:Inner_sync.Ase_inst.get_iterations ase_with_length
    in
    Inner_sync.Ase_inst.
      { proof; proof_target = target; init = ase_source; excess }
  in
  send ?proving_timeout t (Prover.Input.Inner_sync { public_key; ase })
  >>| function
  | Prover.Output.Call_forest_tree tree ->
      tree
  | _ ->
      failwith "Unexpected response from prover"

let verify_both_ases ?proving_timeout t input =
  send ?proving_timeout t (Prover.Input.Verify_both_ases input)
  >>| function
  | Prover.Output.Verify_both_ases snark ->
      snark
  | _ ->
      failwith "Unexpected response from prover"

let outer_commit ?proving_timeout t ~txn_snark ~public_key ~new_actions
    ~unprocessed_actions ~old_inner_ledger ~new_inner_ledger ~da_signature
    ~da_key =
  let get_inner_acc ledger =
    let inner_acc =
      Sparse_ledger.get_exn ledger Zeko_constants.inner_account_index
    in
    let inner_acc_path =
      Sparse_ledger.path_exn ledger Zeko_constants.inner_account_index
      |> List.map ~f:(function
           | `Left hash ->
               ({ right_side = hash } : Outer_rules.Rule_commit_inst.PathElt.t)
           | `Right hash ->
               failwith "The inner account is supposed to be left most" )
    in
    (inner_acc, inner_acc_path)
  in
  let old_inner_acc, old_inner_acc_path = get_inner_acc old_inner_ledger in
  let new_inner_acc, new_inner_acc_path = get_inner_acc new_inner_ledger in

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
    let%map proof, target, excess =
      ase t ~source:action_state ~elems:new_actions
        ~max_excess:Outer_commit.Ase_inner_inst.get_iterations ase_with_length
    in
    Outer_commit.Ase_inner_inst.
      { proof; proof_target = target; init = action_state; excess }
  in
  let%bind outer_ase =
    let ({ outer_action_state } : Rollup_state.Inner_state.t) =
      Rollup_state.Inner_state.value_of_app_state
        (Option.value_exn new_inner_acc.zkapp).app_state
    in
    let action_state =
      Rollup_state.Outer_action_state.With_length.raw outer_action_state
    in
    let%map proof, target, excess =
      ase t ~source:action_state ~elems:unprocessed_actions
        ~max_excess:Outer_commit.Ase_outer_inst.get_iterations
        ase_without_length
    in
    Outer_commit.Ase_outer_inst.
      { proof; proof_target = target; init = action_state; excess }
  in
  let%bind verify_both_ases = verify_both_ases t (outer_ase, inner_ase) in

  send ?proving_timeout t
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
       } )
  >>| function
  | Prover.Output.Call_forest_tree tree ->
      tree
  | _ ->
      failwith "Unexpected response from prover"

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
