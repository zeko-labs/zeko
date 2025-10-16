open Async
open Core_kernel
open Mina_base
open Zeko_circuits
open Zeko_types
open Relational_db
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

type t =
  { q : prover Priority_throttle.t
  ; logger : Logger.t
  ; db_pool : Db.pool option
  }

let create ?(ping_interval = 15.) ?(ping_timeout = 10.) ?db_pool ~logger provers
    =
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
  { q; logger; db_pool }

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

module Ase_cache_with_length_table = struct
  type t =
    { source_hash : Field.t
    ; source_length : Unsigned.uint32
    ; target_hash : Field.t
    ; proof : Compile_simple.Proof.t
    ; extension_length : int
    }
  [@@deriving hlist, fields]

  let split t : Ase.With_length.trans * Compile_simple.Proof.t * int =
    ( { source = { action_state = t.source_hash; length = t.source_length }
      ; target =
          { action_state = t.target_hash
          ; length =
              Unsigned.UInt32.(add (of_int t.extension_length) t.source_length)
          }
      }
    , t.proof
    , t.extension_length )

  let typ =
    Mina_caqti.Type_spec.custom_type
      ~to_hlist:(fun { source_hash
                     ; source_length
                     ; target_hash
                     ; proof
                     ; extension_length
                     } ->
        H_list.
          [ Field.to_string source_hash
          ; Unsigned.UInt32.to_int source_length
          ; Field.to_string target_hash
          ; Compile_simple.Proof.to_yojson proof |> Yojson.Safe.to_string
          ; extension_length
          ] )
      ~of_hlist:(fun H_list.
                       [ source_hash
                       ; source_length
                       ; target_hash
                       ; proof
                       ; extension_length
                       ] ->
        { source_hash = Field.of_string source_hash
        ; source_length = Unsigned.UInt32.of_int source_length
        ; target_hash = Field.of_string target_hash
        ; proof =
            Compile_simple.Proof.of_yojson (Yojson.Safe.from_string proof)
            |> ok_exn
        ; extension_length
        } )
      Caqti_type.[ string; int; string; octets; int ]

  let insert (module Conn : CONNECTION) t =
    Conn.exec
      (Caqti_request.exec typ
         {sql| INSERT INTO ase_cache_with_length (source_hash, source_length, target_hash, proof, extension_length) 
                VALUES (?, ?, ?, ?, ?) 
                ON CONFLICT (source_hash, target_hash) 
                DO NOTHING |sql} )
      t

  let find_ase_by_source (module Conn : CONNECTION) ~source_hash =
    let open Deferred.Result.Let_syntax in
    Conn.collect_list
      (Caqti_request.collect Caqti_type.string typ
         {sql| SELECT source_hash, source_length, target_hash, proof, extension_length 
                FROM ase_cache_with_length 
                WHERE source_hash = ?
                ORDER BY extension_length DESC |sql} )
      source_hash
    >>| List.map ~f:split
end

module Ase_cache_without_length_table = struct
  type t =
    { source_hash : Field.t
    ; target_hash : Field.t
    ; proof : Compile_simple.Proof.t
    ; extension_length : int
    }
  [@@deriving hlist, fields]

  let split t : Ase.Without_length.trans * Compile_simple.Proof.t * int =
    ( { source = t.source_hash; target = t.target_hash }
    , t.proof
    , t.extension_length )

  let typ =
    Mina_caqti.Type_spec.custom_type
      ~to_hlist:(fun { source_hash; target_hash; proof; extension_length } ->
        H_list.
          [ Field.to_string source_hash
          ; Field.to_string target_hash
          ; Compile_simple.Proof.to_yojson proof |> Yojson.Safe.to_string
          ; extension_length
          ] )
      ~of_hlist:(fun H_list.
                       [ source_hash; target_hash; proof; extension_length ] ->
        { source_hash = Field.of_string source_hash
        ; target_hash = Field.of_string target_hash
        ; proof =
            Compile_simple.Proof.of_yojson (Yojson.Safe.from_string proof)
            |> ok_exn
        ; extension_length
        } )
      Caqti_type.[ string; string; octets; int ]

  let insert (module Conn : CONNECTION) t =
    Conn.exec
      (Caqti_request.exec typ
         {sql| INSERT INTO ase_cache_without_length (source_hash, target_hash, proof, extension_length) 
                VALUES (?, ?, ?, ?) 
                ON CONFLICT (source_hash, target_hash) 
                DO NOTHING |sql} )
      t

  let find_ase_by_source (module Conn : CONNECTION) ~source_hash =
    let open Deferred.Result.Let_syntax in
    Conn.collect_list
      (Caqti_request.collect Caqti_type.string typ
         {sql| SELECT source_hash, target_hash, proof, extension_length 
                FROM ase_cache_without_length 
                WHERE source_hash = ?
                ORDER BY extension_length DESC |sql} )
      source_hash
    >>| List.map ~f:split
end

let cache_ase_with_length t ~(source : Ase.With_length.Stmt.t)
    ~(target : Ase.With_length.Stmt.t) ~proof ~extension_length =
  let logger = t.logger in
  match t.db_pool with
  | None ->
      return ()
  | Some db_pool ->
      let source_hash = source.action_state in
      let source_length = source.length in
      let target_hash = target.action_state in
      let%bind () =
        Pool.use
          (fun conn ->
            Ase_cache_with_length_table.insert conn
              { source_hash
              ; source_length
              ; target_hash
              ; proof
              ; extension_length
              } )
          db_pool
        >>| caqti_ok_exn ~msg:"Failed to cache ASE with length proof: %s"
      in
      [%log debug]
        !"Cached ASE with length proof: source=%{sexp: Field.t}, \
          source_length=%d, target=%{sexp: Field.t}, length=%d"
        source_hash
        (Unsigned.UInt32.to_int source_length)
        target_hash extension_length ;
      return ()

let cache_ase_without_length t ~(source : Ase.Without_length.Stmt.t)
    ~(target : Ase.Without_length.Stmt.t) ~proof ~extension_length =
  let logger = t.logger in
  match t.db_pool with
  | None ->
      return ()
  | Some db_pool ->
      let source_hash = source in
      let target_hash = target in
      let%bind () =
        Pool.use
          (fun conn ->
            Ase_cache_without_length_table.insert conn
              { source_hash; target_hash; proof; extension_length } )
          db_pool
        >>| caqti_ok_exn ~msg:"Failed to cache ASE without length proof: %s"
      in
      [%log debug]
        !"Cached ASE without length proof: source=%{sexp: Field.t}, \
          target=%{sexp: Field.t}, length=%d"
        source_hash target_hash extension_length ;
      return ()

let map_to_cached_source (type trans stmt) t
    (module Trans : Ase.Trans with type t = trans)
    (module Stmt : Ase.Stmt with type t = stmt) ~(source : stmt) ~elems
    ~(find_ase_by_source :
          connection
       -> source_hash:string
       -> ( (trans * Compile_simple.Proof.t * int) list
          , Caqti_error.t )
          Deferred.Result.t ) =
  let logger = t.logger in
  match t.db_pool with
  | None ->
      return (`Full source, elems)
  | Some db_pool -> (
      let source_hash = Stmt.state source in
      let _, targets =
        List.fold_map elems ~init:source_hash ~f:(fun acc elem ->
            let h = Zkapp_account.Actions_impl.push_hash acc elem in
            (h, h) )
      in
      let%map proofs =
        Pool.use
          (fun conn ->
            find_ase_by_source conn ~source_hash:(Field.to_string source_hash)
            )
          db_pool
        >>| caqti_ok_exn ~msg:"Failed to get cached ASE proof: %s"
      in
      match
        List.find proofs ~f:(fun (trans, _, _) ->
            let f = Trans.target_hash trans in
            List.mem targets ~equal:Field.equal f )
      with
      | None ->
          [%log debug] !"Cache miss: %{sexp: Field.t}" source_hash ;
          (`Full source, elems)
      | Some (trans, proof, extension_length) ->
          [%log debug]
            !"Cache hit: %{sexp: Field.t}, extension_length=%d"
            source_hash extension_length ;
          (`Extend (trans, proof), List.drop elems extension_length) )

let folder' (type stmt elem) t ~(source : stmt) ~(elems : elem list) ~max_excess
    (module Folder_iterations : Zeko_constants.FOLDER_ITERATIONS)
    ~(map_to_cached_source :
       source:stmt -> elems:elem list -> ('source * elem list) Deferred.t )
    ~(cache_ase_proof :
          t
       -> source:stmt
       -> target:stmt
       -> proof:Compile_simple.Proof.t
       -> extension_length:int
       -> unit Deferred.t )
    (prover :
         ?proving_timeout:float
      -> t
      -> 'source * elem list
      -> (Compile_simple.Proof.t option * stmt) Deferred.t ) =
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
      let%bind input = map_to_cached_source ~source ~elems:elems_to_prove in
      let%bind proof, target =
        prover ~proving_timeout:(20. *. number_of_proofs) t input
      in
      let%bind () =
        match proof with
        | Some proof ->
            cache_ase_proof t ~source ~target ~proof
              ~extension_length:(List.length elems_to_prove)
        | None ->
            return ()
      in
      return (proof, target, excess)

let folder =
  folder'
    ~map_to_cached_source:(fun ~source ~elems -> return (`Full source, elems))
    ~cache_ase_proof:(fun _ ~source:_ ~target:_ ~proof:_ ~extension_length:_ ->
      return () )

let check_accepted_folder t =
  folder' t
    (module Zeko_constants.Folder_iterations.Check_accepted)
    ~map_to_cached_source:(fun ~source ~elems -> return (source, elems))
    ~cache_ase_proof:(fun _ ~source:_ ~target:_ ~proof:_ ~extension_length:_ ->
      return () )

let ase_cached_folder_with_length t =
  folder' t
    (module Zeko_constants.Folder_iterations.Ase.With_length)
    ~map_to_cached_source:
      (map_to_cached_source t
         (module Ase.With_length.Trans)
         (module Ase.With_length.Stmt)
         ~find_ase_by_source:Ase_cache_with_length_table.find_ase_by_source )
    ~cache_ase_proof:cache_ase_with_length

let ase_cached_folder_without_length t =
  folder' t
    (module Zeko_constants.Folder_iterations.Ase.Without_length)
    ~map_to_cached_source:
      (map_to_cached_source t
         (module Ase.Without_length.Trans)
         (module Ase.Without_length.Stmt)
         ~find_ase_by_source:Ase_cache_without_length_table.find_ase_by_source )
    ~cache_ase_proof:cache_ase_without_length

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

let inner_sync ?proving_timeout t ~public_key ~ase_source ~ase_elms =
  let%bind ase =
    let%map proof, target, excess =
      ase_cached_folder_with_length t ~source:ase_source ~elems:ase_elms
        ~max_excess:Zeko_constants.Max_excess_actions.Inner_sync.outer
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

let verify_both_ases_commit ?proving_timeout t input =
  send_with_priority ?proving_timeout t
    (Prover.Input.Verify_both_ases_commit input)
  >>| function
  | Prover.Output.Verify_both_ases_commit snark ->
      snark
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let verify_two_outer_ases_cancelled_deposit ?proving_timeout t input =
  send_with_priority ?proving_timeout t
    (Prover.Input.Verify_two_outer_ases_cancelled_deposit input)
  >>| function
  | Prover.Output.Verify_two_outer_ases_cancelled_deposit snark ->
      snark
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let verify_check_accepted_and_ase_cancelled_deposit ?proving_timeout t input =
  send_with_priority ?proving_timeout t
    (Prover.Input.Verify_check_accepted_and_ase_cancelled_deposit input)
  >>| function
  | Prover.Output.Verify_check_accepted_and_ase_cancelled_deposit snark ->
      snark
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let outer_commit ?proving_timeout t ~txn_snark ~public_key ~inner_ase_source
    ~new_inner_actions ~unprocessed_actions ~(old_inner_acc : Account.t)
    ~old_inner_acc_path ~(new_inner_acc : Account.t) ~new_inner_acc_path
    ~da_multisig ~slot_range =
  (* Counting length of inner action state *)
  let%bind inner_ase =
    let%map proof, target, excess =
      ase_cached_folder_with_length t ~source:inner_ase_source
        ~elems:new_inner_actions
        ~max_excess:Zeko_constants.Max_excess_actions.Commit.inner
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
      ase_cached_folder_without_length t ~source:action_state
        ~elems:unprocessed_actions
        ~max_excess:Zeko_constants.Max_excess_actions.Commit.outer
        (ase_without_length ~sendfn:send_with_priority)
    in
    Outer_commit.Ase_outer_inst.
      { proof; proof_target = target; init = action_state; excess }
  in
  let%bind verify_both_ases =
    verify_both_ases_commit t (outer_ase, inner_ase)
  in
  send_with_priority ?proving_timeout t
    (Prover.Input.Outer_commit
       { txn_snark
       ; public_key
       ; verify_both_ases
       ; old_inner_acc
       ; old_inner_acc_path
       ; new_inner_acc
       ; new_inner_acc_path
       ; da_multisig
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
       Bridge.Check_accepted_mina.Init.t
       * Field.t
       * Bridge.Check_accepted_mina.Elem.t list ) ~prev_next_deposit =
  let%bind ase =
    let ase_source, ase_elms = ase in
    let%map proof, target, excess =
      ase_cached_folder_with_length t ~source:ase_source ~elems:ase_elms
        ~max_excess:Zeko_constants.Max_excess_actions.Finalize_deposit.outer
        (ase_with_length ~sendfn:send)
    in
    Bridge.Finalize_deposit.Ase_inst.
      { proof; proof_target = target; init = ase_source; excess }
  in
  let%bind check_accepted =
    let init, deposit_hash, elems = check_accepted in
    let source : Bridge.Check_accepted_mina.Stmt.t =
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
      check_accepted_folder t ~source ~elems
        ~max_excess:
          Zeko_constants.Max_excess_actions.Finalize_deposit.check_accepted
        check_accepted_mina
    in
    ( { proof; proof_source = source; proof_target = target; init; excess }
      : Bridge.Check_accepted_mina.serializable )
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

let finalize_cancelled_deposit ?proving_timeout t ~public_key ~may_use_token
    ~outer_authorization_kind ~commit ~before_commit
    ~(commit_ase : Ase.Without_length.Stmt.t * Field.t list)
    ~(sync_ase : Ase.With_length.Stmt.t * Field.t list)
    ~(check_accepted :
       Bridge.Check_accepted_mina.Init.t
       * Field.t
       * Bridge.Check_accepted_mina.Elem.t list )
    ~(check_accepted_ase : Ase.With_length.Stmt.t * Field.t list)
    ~prev_next_cancelled_deposit =
  let%bind commit_ase =
    let ase_source, ase_elms = commit_ase in
    let%map proof, target, excess =
      ase_cached_folder_without_length t ~source:ase_source ~elems:ase_elms
        ~max_excess:
          Zeko_constants.Max_excess_actions.Finalize_cancelled_deposit.outer
        (ase_without_length ~sendfn:send)
    in
    Bridge.Finalize_cancelled_deposit.Ase_outer_inst.
      { proof; proof_target = target; init = ase_source; excess }
  in
  let%bind sync_ase =
    let ase_source, ase_elms = sync_ase in
    let%map proof, target, excess =
      ase_cached_folder_with_length t ~source:ase_source ~elems:ase_elms
        ~max_excess:
          Zeko_constants.Max_excess_actions.Finalize_cancelled_deposit
          .outer_with_length
        (ase_with_length ~sendfn:send)
    in
    Bridge.Finalize_cancelled_deposit.Ase_outer_with_length_inst.
      { proof; proof_target = target; init = ase_source; excess }
  in
  let%bind verify_two_outer_ases =
    verify_two_outer_ases_cancelled_deposit t (commit_ase, sync_ase)
  in
  let%bind check_accepted =
    let init, deposit_hash, elems = check_accepted in
    let source : Bridge.Check_accepted_mina.Stmt.t =
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
      check_accepted_folder t ~source ~elems
        ~max_excess:
          Zeko_constants.Max_excess_actions.Finalize_cancelled_deposit
          .check_accepted check_accepted_mina
    in
    ( { proof; proof_source = source; proof_target = target; init; excess }
      : Bridge.Check_accepted_mina.serializable )
  in
  let%bind check_accepted_ase =
    let ase_source, ase_elms = check_accepted_ase in
    let%map proof, target, excess =
      ase_cached_folder_with_length t ~source:ase_source ~elems:ase_elms
        ~max_excess:
          Zeko_constants.Max_excess_actions.Finalize_cancelled_deposit.outer
        (ase_with_length ~sendfn:send)
    in
    Bridge.Finalize_cancelled_deposit.Ase_outer_with_length_inst.
      { proof; proof_target = target; init = ase_source; excess }
  in
  let%bind verify_check_accepted_and_ase =
    verify_check_accepted_and_ase_cancelled_deposit t
      (check_accepted, check_accepted_ase)
  in
  send ?proving_timeout t
    Prover.Input.(
      Bridge
        (Finalize_cancelled_deposit
           { public_key
           ; may_use_token
           ; outer_authorization_kind
           ; commit
           ; before_commit
           ; verify_two_outer_ases
           ; verify_check_accepted_and_ase
           ; prev_next_cancelled_deposit
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
      ase_cached_folder_without_length t ~source ~elems
        ~max_excess:Zeko_constants.Max_excess_actions.Finalize_withdrawal.outer
        (ase_without_length ~sendfn:send)
    in
    Bridge.Finalize_withdrawal.Ase_outer_inst.
      { proof; proof_target = target; init = source; excess }
  in
  let%bind withdrawal_ase =
    let source, elems = withdrawal_ase in
    let%map proof, target, excess =
      ase_cached_folder_with_length t ~source ~elems
        ~max_excess:Zeko_constants.Max_excess_actions.Finalize_withdrawal.inner
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
