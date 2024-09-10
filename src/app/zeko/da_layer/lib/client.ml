open Core_kernel
open Async_kernel
open Mina_base
open Mina_ledger

module Rpc = struct
  let dispatch ?(max_tries = 5) ~logger
      (node_location : Host_and_port.t Cli_lib.Flag.Types.with_name) rpc data =
    let rec go tries_left errs =
      if Int.( <= ) tries_left 0 then
        let e = Error.of_list (List.rev errs) in
        return
          (Error
             (Error.tag_arg e
                (sprintf
                   "Could not send query to da node after %d tries. The \
                    process may not be running, please check the \
                    daemon-argument"
                   max_tries )
                ( ("host_and_port", node_location.value)
                , ("daemon-argument", node_location.name) )
                [%sexp_of: (string * Host_and_port.t) * (string * string)] ) )
      else
        match%bind Daemon_rpcs.Client.dispatch rpc data node_location.value with
        | Ok result ->
            return (Ok result)
        | Error e ->
            [%log error] "Error sending data to the da node $error. Retrying..."
              ~metadata:[ ("error", `String (Error.to_string_hum e)) ] ;
            go (tries_left - 1) (e :: errs)
    in
    go max_tries []

  let post_diff ~logger ~node_location ~ledger_openings ~diff =
    dispatch ~logger node_location Rpc.Post_diff.v1 { ledger_openings; diff }

  let get_diff ~logger ~node_location ~ledger_hash :
      (Diff.t option, Error.t) result Deferred.t =
    dispatch ~logger node_location Rpc.Get_diff.v1 ledger_hash

  let get_all_keys ~logger ~node_location () =
    dispatch ~logger node_location Rpc.Get_all_keys.v1 ()

  let get_diff_source ~logger ~node_location ~ledger_hash =
    dispatch ~logger node_location Rpc.Get_diff_source.v1 ledger_hash

  let get_node_public_key ~logger ~node_location () =
    dispatch ~logger node_location Rpc.Get_signer_public_key.v1 ()

  let get_signature ~logger ~node_location ~ledger_hash =
    dispatch ~logger node_location Rpc.Get_signature.v1 ledger_hash
end

module Config = struct
  type t = { nodes : Host_and_port.t Cli_lib.Flag.Types.with_name list }
  [@@deriving fields]

  let of_string_list uris =
    { nodes =
        List.mapi uris ~f:(fun i s ->
            Cli_lib.Flag.Types.
              { value = Host_and_port.of_string s
              ; name = sprintf "da-node-%d" i
              } )
    }
end

(** Send the diff to all the nodes in the [~config] *)
let distribute_diff ~logger ~config ~ledger_openings ~diff ~quorum =
  let%bind signatures =
    Deferred.List.map ~how:`Parallel (Config.nodes config)
      ~f:(fun node_location ->
        Rpc.post_diff ~logger ~node_location ~ledger_openings ~diff )
    |> Deferred.map ~f:(List.filter_map ~f:Result.ok)
  in
  if List.length signatures >= quorum then return (Ok signatures)
  else return (Error (Error.of_string "Quorum not reached"))

(** This module ensures that diffes are sent in order. 
    Signatures can be collected as [Deferred.t] via [get_signatures] *)
module Sequencer = struct
  type t =
    { logger : Logger.t
    ; config : Config.t
    ; quorum : int
          (** The amount of signatures needed when distributing diff *)
    ; q : unit Async.Sequencer.t  (** Queue of diffs to be distributed *)
    ; mutable signatures : Signature.t list Deferred.t Ledger_hash.Map.t
          (** Mapping of [target_ledger_hash] to list of deferred signatures *)
    ; mutable last_distributed_diff : Ledger_hash.t option
          (** [target_ledger_hash] of last processed diff in queue *)
    }

  let create ~logger ~config ~quorum =
    { logger
    ; config
    ; quorum
    ; q = Async.Sequencer.create ~continue_on_error:false ()
    ; signatures = Ledger_hash.Map.empty
    ; last_distributed_diff = None
    }

  let enqueue_distribute_diff t ~ledger_openings ~diff ~target_ledger_hash =
    let deferred =
      Throttle.enqueue t.q (fun () ->
          let logger = t.logger in
          match%bind
            distribute_diff ~logger ~config:t.config ~ledger_openings ~diff
              ~quorum:t.quorum
          with
          | Ok signatures ->
              t.last_distributed_diff <- Some target_ledger_hash ;
              return signatures
          | Error e ->
              [%log error] "Error distributing diff: $error"
                ~metadata:[ ("error", `String (Error.to_string_hum e)) ] ;
              Error.raise e )
    in
    t.signatures <-
      Ledger_hash.Map.set t.signatures ~key:target_ledger_hash ~data:deferred

  let get_signatures t ~ledger_hash =
    match Ledger_hash.Map.find t.signatures ledger_hash with
    | Some d ->
        Deferred.map d ~f:Option.some
    | None ->
        let%bind signatures =
          Deferred.List.map ~how:`Parallel (Config.nodes t.config)
            ~f:(fun node_location ->
              Rpc.get_signature ~logger:t.logger ~node_location ~ledger_hash )
          |> Deferred.map ~f:(List.filter_map ~f:Result.ok)
          |> Deferred.map ~f:(List.filter_map ~f:Fn.id)
        in
        if List.length signatures >= t.quorum then return (Some signatures)
        else return None
end

(** Useful for querying data, will fallback to the next node in list in case the first one fails *)
let try_all_nodes ~config ~f =
  let rec try_first ~accum_errors = function
    | [] ->
        return (Error (Error.of_list accum_errors))
    | first_node :: rest -> (
        match%bind f ~node_location:first_node () with
        | Ok result ->
            return (Ok result)
        | Error err ->
            try_first ~accum_errors:(err :: accum_errors) rest )
  in
  try_first ~accum_errors:[] (Config.nodes config)

(** Get the chain of ledger hashes from empty ledger hash to [target_ledger_hash]  *)
let get_ledger_hashes_chain ~logger ~config ~depth ~target_ledger_hash =
  let rec go current =
    if Ledger_hash.equal current (Diff.empty_ledger_hash ~depth) then
      return (Ok [])
    else
      let%bind.Deferred.Result source =
        try_all_nodes ~config ~f:(fun ~node_location () ->
            Rpc.get_diff_source ~logger ~node_location ~ledger_hash:current )
      in
      let%bind.Deferred.Result next = go source in
      return (Ok (current :: next))
  in
  let%bind.Deferred.Result from_target_to_genesis = go target_ledger_hash in
  return (Ok (List.rev from_target_to_genesis))

(** Try to get the diff from the first node in the list, if it fails, try the next one *)
let get_diff ~logger ~config ~ledger_hash =
  try_all_nodes ~config ~f:(fun ~node_location () ->
      Rpc.get_diff ~logger ~node_location ~ledger_hash )

(** Distribute diff of initial accounts *)
let distribute_genesis_diff ~logger ~config ~ledger =
  let%bind account_ids =
    Ledger.accounts ledger |> Deferred.map ~f:Account_id.Set.to_list
  in
  let changed_accounts =
    List.map account_ids ~f:(fun aid ->
        let index = Ledger.index_of_account_exn ledger aid in
        let account = Ledger.get_at_index_exn ledger index in
        (index, account) )
  in
  (* openings lead to empty accounts *)
  let ledger_openings =
    List.fold changed_accounts
      ~init:(Sparse_ledger.of_ledger_subset_exn ledger account_ids)
      ~f:(fun acc (index, _) -> Sparse_ledger.set_exn acc index Account.empty)
  in
  let diff =
    Diff.create
      ~source_ledger_hash:(Diff.empty_ledger_hash ~depth:(Ledger.depth ledger))
      ~changed_accounts ~command_with_action_step_flags:None
  in
  distribute_diff ~logger ~config ~ledger_openings ~diff ~quorum:0
