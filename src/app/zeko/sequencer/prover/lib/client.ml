open Async
open Core_kernel
open Mina_base
open Zeko_circuits
open Zeko_types
open Relational_db
module Field = Snark_params.Tick.Field

type t =
  { mq : Message_queue.Master.t; logger : Logger.t; db_pool : Db.pool option }

let create ?db_pool ~logger ~mq_host =
  let%map mq = Message_queue.Master.start mq_host in
  { mq; logger; db_pool }

let queue_size t = Message_queue.Master.get_queue_size t.mq

let send' t ~sendfn (input : Prover.Input.t) : Prover.Output.t Deferred.t =
  match%map
    Utils.retry ~max_attempts:5 ~delay:(Time.Span.of_sec 1.)
      ~f:(fun () ->
        match%map
          sendfn t.mq (Prover.Input.to_yojson input |> Yojson.Safe.to_string)
        with
        | Ok response ->
            Yojson.Safe.from_string response
            |> Prover.Output.of_yojson
            |> Result.map_error ~f:Error.of_string
        | Error err ->
            Error err )
      ()
  with
  | Ok output ->
      output
  | Error err ->
      failwithf "Failed to send job to the message queue: %s"
        (Error.to_string_hum err) ()

let send = send' ~sendfn:Message_queue.Master.send_exn

let send_with_priority =
  send' ~sendfn:Message_queue.Master.send_with_priority_exn

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
         t
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
      let%bind input = map_to_cached_source ~source ~elems:elems_to_prove in
      let%bind proof, target = prover t input in
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

let transaction_snark t input =
  send t (Prover.Input.Txn_snark input)
  >>| function
  | Prover.Output.Txn_snark snark ->
      snark
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let ase_with_length ~sendfn t input =
  sendfn t (Prover.Input.Folder (Ase_with_length input))
  >>| function
  | Prover.Output.Folder (Ase_with_length ase) ->
      ase
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let ase_without_length ~sendfn t input =
  sendfn t (Prover.Input.Folder (Ase_without_length input))
  >>| function
  | Prover.Output.Folder (Ase_without_length ase) ->
      ase
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let check_accepted_mina t input =
  send t (Prover.Input.Folder (Check_accepted_mina input))
  >>| function
  | Prover.Output.Folder (Check_accepted_mina check_accepted) ->
      check_accepted
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let inner_sync t ~public_key ~ase_source ~ase_elms =
  let%bind ase =
    let%map proof, target, excess =
      ase_cached_folder_with_length t ~source:ase_source ~elems:ase_elms
        ~max_excess:Zeko_constants.Max_excess_actions.Inner_sync.outer
        (ase_with_length ~sendfn:send_with_priority)
    in
    Inner_sync.Ase_inst.
      { proof; proof_target = target; init = ase_source; excess }
  in
  send_with_priority t (Prover.Input.Inner_sync { public_key; ase })
  >>| function
  | Prover.Output.Call_forest (parent_with_calls, proof) ->
      (parent_with_calls, proof)
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let verify_both_ases_commit t input =
  send_with_priority t (Prover.Input.Verify_both_ases_commit input)
  >>| function
  | Prover.Output.Verify_both_ases_commit snark ->
      snark
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let verify_two_outer_ases_cancelled_deposit t input =
  send t (Prover.Input.Verify_two_outer_ases_cancelled_deposit input)
  >>| function
  | Prover.Output.Verify_two_outer_ases_cancelled_deposit snark ->
      snark
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let verify_check_accepted_and_ase_cancelled_deposit t input =
  send t (Prover.Input.Verify_check_accepted_and_ase_cancelled_deposit input)
  >>| function
  | Prover.Output.Verify_check_accepted_and_ase_cancelled_deposit snark ->
      snark
  | Prover.Output.Error err ->
      failwith err
  | _ ->
      failwith "Unexpected response from prover"

let outer_commit t ~txn_snark ~public_key ~inner_ase_source ~new_inner_actions
    ~unprocessed_actions ~(old_inner_acc : Account.t) ~old_inner_acc_path
    ~(new_inner_acc : Account.t) ~new_inner_acc_path ~da_multisig ~slot_range =
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
  send_with_priority t
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

let outer_action_witness t witness =
  send t Prover.Input.(Bridge (Outer_action_witness witness))
  >>| function
  | Prover.Output.Call_forest (parent_with_calls, proof) ->
      Ok (parent_with_calls, proof)
  | Prover.Output.Error err ->
      Error err
  | _ ->
      failwith "Unexpected response from prover"

let inner_action_witness t witness =
  send t Prover.Input.(Bridge (Inner_action_witness witness))
  >>| function
  | Prover.Output.Call_forest (parent_with_calls, proof) ->
      Ok (parent_with_calls, proof)
  | Prover.Output.Error err ->
      Error err
  | _ ->
      failwith "Unexpected response from prover"

let finalize_deposit t ~public_key ~may_use_token ~inner_authorization_kind
    ~(ase : Ase.With_length.Stmt.t * Field.t list)
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
  send t
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

let finalize_cancelled_deposit t ~public_key ~may_use_token
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
  send t
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

let inner_receive t witness =
  send t Prover.Input.(Bridge (Inner_receive witness))
  >>| function
  | Prover.Output.Call_forest (parent_with_calls, proof) ->
      Ok (parent_with_calls, proof)
  | Prover.Output.Error err ->
      Error err
  | _ ->
      failwith "Unexpected response from prover"

let finalize_withdrawal t ~public_key ~may_use_token ~outer_authorization_kind
    ~commit ~before_commit ~commit_ase ~before_withdrawal ~withdrawal_ase
    ~prev_next_withdrawal ~withdrawal_params =
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
  send t
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

let outer_token_owner t witness =
  send t Prover.Input.(Bridge (Outer_token_owner witness))
  >>| function
  | Prover.Output.Call_forest (parent_with_calls, proof) ->
      Ok (parent_with_calls, proof)
  | Prover.Output.Error err ->
      Error err
  | _ ->
      failwith "Unexpected response from prover"
