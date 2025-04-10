open Core_kernel
open Async_kernel
open Mina_base
open Mina_ledger
module Field = Snark_params.Tick.Field

(* FIXME: Don't use Mina_compile_config.For_tests.t *)
let compile_config = Mina_compile_config.For_unit_tests.t

module Rpc = struct
  let dispatch ?(max_tries = 5) ?(timeout = 5.) ~logger
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
        match%bind
          Daemon_rpcs.Client.dispatch ~compile_config rpc data
            node_location.value
        with
        | Ok result ->
            return (Ok result)
        | Error e ->
            if tries_left > 1 then
              [%log error]
                "Error sending data to the da node $error. Retrying..."
                ~metadata:[ ("error", `String (Error.to_string_hum e)) ] ;
            let%bind () = after (Time_ns.Span.of_sec timeout) in
            go (tries_left - 1) (e :: errs)
    in
    go max_tries []

  let post_diff ~logger ~node_location ~ledger_openings ~diff =
    dispatch ~logger node_location Rpc.Post_diff.V1.t { ledger_openings; diff }

  let get_diff ~logger ~node_location ~ledger_hash =
    match%bind
      dispatch ~max_tries:1 ~logger node_location Rpc.Get_diff.V2.t ledger_hash
    with
    | Ok diff ->
        return (Ok diff)
    | Error e ->
        let v2_unimplemented =
          Error.to_string_mach e
          |> String.is_substring
               ~substring:"Unimplemented_rpc Get_diff (Version 2)"
        in
        if v2_unimplemented then
          (* Fallback to older version *)
          let%bind.Deferred.Result result =
            dispatch ~max_tries:1 ~logger node_location Rpc.Get_diff.V1.t
              ledger_hash
          in
          return
            (Ok (Option.map result ~f:(fun x -> Diff.Stable.V1.to_latest x)))
        else return (Error e)

  let get_all_keys ~logger ~node_location () =
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_all_keys.V1.t ()

  let get_diff_source ~logger ~node_location ~ledger_hash =
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_diff_source.V1.t
      ledger_hash

  let get_node_public_key ~logger ~node_location () =
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_signer_public_key.V1.t
      ()

  let get_signature ~logger ~node_location ~ledger_hash =
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_signature.V1.t
      ledger_hash

  let get_ledger_hashes_chain ~logger ~node_location ?max_length ~source ~target
      () =
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_ledger_hashes_chain.V1.t
      { source; target; max_length }

  let get_diffs_chain ~logger ~node_location ?max_length ~source ~target () =
    dispatch ~max_tries:1 ~logger node_location Rpc.Get_diffs_chain.V1.t
      { source; target; max_length }
end

module Config = struct
  type t =
    { mutable nodes : Host_and_port.t Cli_lib.Flag.Types.with_name list
          (** Mutable in case we want to throw out some node *)
    }
  [@@deriving fields]

  let of_string_list uris =
    { nodes =
        List.mapi uris ~f:(fun i s ->
            Cli_lib.Flag.Types.
              { value = Host_and_port.of_string s
              ; name = sprintf "da-node-%d" i
              } )
    }

  let of_node_locations nodes = { nodes }

  let throw_out_node t ~(node : Host_and_port.t Cli_lib.Flag.Types.with_name) =
    let open Cli_lib.Flag.Types in
    t.nodes <-
      List.filter t.nodes ~f:(fun n -> not (String.equal n.name node.name))
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

(** Get the chain of ledger hashes from [source_ledger_hash] hash to [target_ledger_hash] *)
let get_ledger_hashes_chain ~logger ~config ?max_length ~source_ledger_hash
    ~target_ledger_hash () =
  try_all_nodes ~config ~f:(fun ~node_location () ->
      Rpc.get_ledger_hashes_chain ~logger ~node_location ?max_length
        ~source:source_ledger_hash ~target:target_ledger_hash () )

(** Get the chain of diffs from [source_ledger_hash] hash to [target_ledger_hash] *)
let get_diffs_chain ~logger ~config ?max_length ~source_ledger_hash
    ~target_ledger_hash () =
  try_all_nodes ~config ~f:(fun ~node_location () ->
      Rpc.get_diffs_chain ~logger ~node_location ?max_length
        ~source:source_ledger_hash ~target:target_ledger_hash () )

(** Lazily fetch chunks of diffs, used to minimize memory usage *)
let get_lazy_diffs_chunks ~logger ~depth ~config ?(n = 100) ~source_ledger_hash
    ~target_ledger_hash () =
  let source_ledger_hash =
    match source_ledger_hash with
    | `Genesis ->
        Diff.empty_ledger_hash ~depth
    | `Specific h ->
        h
  in
  (* Get ledger hashes intervals of size [n] *)
  let rec get_intervals ~target_ledger_hash =
    let%bind.Deferred.Result chain =
      get_ledger_hashes_chain ~logger ~config ~max_length:n
        ~source_ledger_hash:(`Specific source_ledger_hash) ~target_ledger_hash
        ()
    in
    match chain with
    | [] ->
        return (Ok [])
    | [ last ] ->
        let interval = (source_ledger_hash, last) in
        return (Ok [ interval ])
    | chain ->
        let hd = List.hd_exn chain in
        let tl = List.last_exn chain in
        let interval = (hd, tl) in
        let%bind.Deferred.Result next_intervals =
          get_intervals ~target_ledger_hash:hd
        in
        return (Ok (interval :: next_intervals))
  in
  let%bind.Deferred.Result intervals =
    get_intervals ~target_ledger_hash >>| Result.map ~f:List.rev
  in
  return
  @@ Ok
       (List.map intervals ~f:(fun (source, target) ->
            lazy
              (get_diffs_chain ~logger ~config
                 ~source_ledger_hash:(`Specific source)
                 ~target_ledger_hash:target () ) ) )

let map_diffs ~logger ~depth ~config ~source_ledger_hash ~target_ledger_hash ~f
    =
  let%bind.Deferred.Result lazy_chunks =
    get_lazy_diffs_chunks ~logger ~depth ~config ~source_ledger_hash
      ~target_ledger_hash ()
  in
  let l = List.length lazy_chunks in
  Deferred.List.mapi ~how:`Sequential lazy_chunks ~f:(fun i lazy_chunk ->
      let%bind.Deferred.Result diffs = Lazy.force lazy_chunk in
      Deferred.List.map ~how:`Sequential diffs ~f:(fun diff ->
          f ~current_chunk:i ~chunks_length:l diff )
      >>| Result.return )
  >>| Result.all >>| Result.map ~f:List.join

(** Try to get the diff from the first node in the list, if it fails, try the next one *)
let get_diff ~logger ~config ~ledger_hash =
  try_all_nodes ~config ~f:(fun ~node_location () ->
      match%bind Rpc.get_diff ~logger ~node_location ~ledger_hash with
      | Ok (Some diff) ->
          return (Ok diff)
      | Ok None ->
          return (Error (Error.of_string "Diff not found"))
      | Error e ->
          return (Error e) )

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

let get_openings ~diff ~ledger =
  let changed_accounts =
    Diff.changed_accounts diff
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  in
  let account_ids =
    List.map changed_accounts ~f:snd |> List.map ~f:Account.identifier
  in
  let openings = Sparse_ledger.of_ledger_subset_exn ledger account_ids in
  List.iter changed_accounts ~f:(fun (index, account) ->
      Ledger.set_at_index_exn ledger index account ) ;
  openings

let attach_openings ~diffs ~ledger =
  List.map diffs ~f:(fun diff -> (diff, get_openings ~diff ~ledger))

let check_synced_nodes ~logger ~(config : Config.t) ~target_ledger_hash =
  Deferred.List.iter config.nodes ~f:(fun node ->
      match%bind
        Rpc.get_diff ~logger ~node_location:node ~ledger_hash:target_ledger_hash
      with
      | Ok (Some _) ->
          return ( (* synced node *) )
      | Ok None | Error _ ->
          printf
            !"Node %s is not synced\n%!"
            (Host_and_port.to_string node.value) ;
          return (Config.throw_out_node config ~node) )
