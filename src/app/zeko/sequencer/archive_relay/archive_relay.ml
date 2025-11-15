open Async
open Core_kernel
open Mina_base
open Mina_lib
open Mina_ledger

let constraint_constants = Zeko_constants.constraint_constants

let compile_time_genesis =
  let consensus_constants =
    let protocol_constants : Genesis_constants.Protocol.t =
      { k = 1
      ; slots_per_epoch = 1000
      ; slots_per_sub_window = 1
      ; grace_period_slots = 1
      ; delta = 1
      ; genesis_state_timestamp = Int64.one
      }
    in
    Consensus.Constants.create ~constraint_constants ~protocol_constants
  in
  Mina_state.Genesis_protocol_state.t
    ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
    ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
    ~constraint_constants ~consensus_constants
    ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference

let rec rmrf path =
  match Sys.is_directory path with
  | true ->
      Sys.readdir path
      |> Array.iter ~f:(fun name -> rmrf (Filename.concat path name)) ;
      Sys.rmdir path
  | false ->
      Sys.remove path

let time ~logger label (d : 'a Deferred.t) =
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  [%log info] "%s: %s" label (Time.Span.to_string_hum @@ Time.diff stop start) ;
  return x

module Timestamp : sig
  type t

  val to_time : t -> Time.t

  val of_time : Time.t -> t

  val to_string : t -> string

  val of_string : string -> t
end = struct
  type t = int

  let to_time t = Int.to_float t |> Time.Span.of_ms |> Time.of_span_since_epoch

  let of_time time =
    Time.to_span_since_epoch time |> Time.Span.to_ms |> Int.of_float

  let to_string t = Int.to_string t

  let of_string t = Int.of_string t
end

module Protocol_state = struct
  include Kvdb_base.Make_singleton (struct
    type t = Mina_state.Protocol_state.value [@@deriving yojson]

    let key = "archive_relay_state"
  end)

  let get kvdb =
    match get kvdb with
    | Some state ->
        state
    | None ->
        compile_time_genesis.data
end

type t =
  { logger : Logger.t
  ; db_dir : string
  ; archive_uri : Host_and_port.t
  ; zeko_uri : Uri.t
  ; da_config : Da_layer.Client.Config.t
  ; mutable ledger : Ledger.Db.t
  ; proof_cache_db : Proof_cache_tag.cache_db
  ; chain : Mina_signature_kind.t
  ; max_checkpoint_age : Time.Span.t
  ; checkpoint_periodicity : int
  ; interval_size : int
  }

let checkpoints_dir db_dir = Filename.concat db_dir "checkpoints"

let ledger_dir db_dir = Filename.concat db_dir "ledger"

let make_checkpoint t ~timestamp =
  let rec backup_checkpoint ?(n = 1) path =
    let new_path = path ^ "." ^ Int.to_string n in
    if FileUtil.test Is_dir new_path then backup_checkpoint ~n:(n + 1) path
    else FileUtil.mv path new_path
  in
  let { logger; db_dir; ledger; _ } = t in
  [%log info] "Making checkpoint at timestamp: %s"
    (Timestamp.to_string timestamp) ;
  let checkpoint_path =
    Filename.concat (checkpoints_dir db_dir) (Timestamp.to_string timestamp)
  in
  if FileUtil.test Is_dir checkpoint_path then backup_checkpoint checkpoint_path ;
  Ledger.Db.make_checkpoint ledger ~directory_name:checkpoint_path

let load_newest_checkpoint ~logger ~db_dir =
  rmrf (ledger_dir db_dir) ;
  match
    Sys.readdir (checkpoints_dir db_dir)
    |> Array.to_list
    |> List.sort ~compare:String.compare
    |> List.rev |> List.hd
  with
  | None ->
      [%log info] "No checkpoints found" ;
      Ledger.Db.create ~directory_name:(ledger_dir db_dir)
        ~depth:constraint_constants.ledger_depth ()
  | Some checkpoint_name ->
      [%log info] "Loading checkpoint: %s" checkpoint_name ;
      let checkpoint_path =
        Filename.concat (checkpoints_dir db_dir) checkpoint_name
      in
      let checkpoint_db =
        Ledger.Db.create ~directory_name:checkpoint_path
          ~depth:constraint_constants.ledger_depth ()
      in
      [%log info] "Loaded checkpoint with ledger hash: %s"
        (Ledger.Db.merkle_root checkpoint_db |> Ledger_hash.to_decimal_string) ;
      Ledger.Db.create_checkpoint checkpoint_db
        ~directory_name:(ledger_dir db_dir) ()

let create ~logger ~archive_uri ~zeko_uri ~da_nodes ~db_dir ~chain
    ~max_checkpoint_age ~checkpoint_periodicity ~interval_size =
  Core.Unix.mkdir_p db_dir ;
  Core.Unix.mkdir_p (checkpoints_dir db_dir) ;
  { logger
  ; db_dir
  ; archive_uri
  ; zeko_uri
  ; da_config = Da_layer.Client.Config.of_string_list da_nodes
  ; ledger = load_newest_checkpoint ~logger ~db_dir
  ; proof_cache_db = Proof_cache_tag.create_identity_db ()
  ; chain
  ; max_checkpoint_age
  ; checkpoint_periodicity
  ; interval_size
  }

let reset_ledger t () =
  let { logger; db_dir; ledger; _ } = t in
  Ledger.Db.close ledger ;
  Sys.readdir (ledger_dir db_dir)
  |> Array.iter ~f:(fun file_name ->
         rmrf (Filename.concat (ledger_dir db_dir) file_name) ) ;
  t.ledger <- load_newest_checkpoint ~logger ~db_dir

(** Prune checkpoints older than [max_checkpoint_age], but leave at least one that is older than [max_checkpoint_age] *)
let prune_checkpoints t =
  let { logger; db_dir; max_checkpoint_age; _ } = t in
  let now = Time.now () in
  Sys.readdir (checkpoints_dir db_dir)
  |> Array.to_list
  |> List.sort ~compare:String.compare
  |> List.rev
  |> List.filter ~f:(fun timestamp ->
         let diff =
           Time.diff now (Timestamp.of_string timestamp |> Timestamp.to_time)
         in
         Time.Span.(diff < max_checkpoint_age) )
  |> List.tl
  |> function
  | None ->
      [%log info] "No checkpoints to prune"
  | Some checkpoints ->
      List.iter checkpoints ~f:(fun checkpoint ->
          [%log info] "Pruning checkpoint: %s" checkpoint ;
          rmrf (Filename.concat (checkpoints_dir db_dir) checkpoint) )

let sync_archive (t : t) ~hash =
  let logger = t.logger in
  Da_layer.Client.map_diffs ~interval_size:t.interval_size ~logger
    ~config:t.da_config ~depth:constraint_constants.ledger_depth
    ~source_ledger_hash:(`Specific (Ledger.Db.merkle_root t.ledger))
    ~target_ledger_hash:hash ()
    ~f:(fun ~current_chunk ~current_diff:_ ~chunks_length diff ->
      let ledger = Ledger.of_database t.ledger in
      let changed_accounts =
        Da_layer.Diff.Stable.Latest.changed_accounts diff
      in
      let accounts_created =
        let aids =
          List.map changed_accounts ~f:snd |> List.map ~f:Account.identifier
        in
        Ledger.location_of_account_batch ledger aids
        |> List.filter_map ~f:(fun (aid, opt) ->
               if Option.is_some opt then Some aid else None )
      in
      List.iter changed_accounts ~f:(fun (index, account) ->
          Ledger.set_at_index_exn ledger index account ) ;
      Ledger.commit ledger ;
      match Da_layer.Diff.Stable.Latest.command_with_action_step_flags diff with
      | None ->
          return ()
      | Some (command, _) -> (
          let command =
            User_command.write_all_proofs_to_disk ~signature_kind:t.chain
              ~proof_cache_db:t.proof_cache_db command
          in
          let kvdb = Ledger.Db.zeko_kvdb t.ledger in
          let new_protocol_state, transition_frontier =
            Archive_lib.Diff.Builder.zeko_transaction_added
              ~constraint_constants ~accounts_created
              ~new_state_hash:(Ledger.merkle_root ledger)
              ~protocol_state:(Protocol_state.get kvdb) ~ledger
              ~txn:
                With_status.
                  { data = Mina_transaction.Transaction.Command command
                  ; status = Transaction_status.Applied
                  }
              ~dummy_fee_payer:Zeko_constants.inner_public_key
              ~timestamp:(Da_layer.Diff.Stable.Latest.timestamp diff)
          in
          Protocol_state.set kvdb ~data:new_protocol_state ;

          let height =
            Mina_state.Protocol_state.consensus_state new_protocol_state
            |> Consensus.Proof_of_stake.Exported.Consensus_state
               .blockchain_length |> Unsigned.UInt32.to_int
          in
          if height % t.checkpoint_periodicity = 0 then (
            [%log info] "Progress: %.0f%%, height: %s"
              (Float.of_int current_chunk /. Float.of_int chunks_length *. 100.0)
              (Int.to_string_hum height) ;
            make_checkpoint t
              ~timestamp:
                ( Da_layer.Diff.Stable.Latest.timestamp diff
                |> Block_time.to_time_exn |> Timestamp.of_time ) ;
            prune_checkpoints t ) ;

          match%bind
            Archive_client.dispatch ~logger
              { value = t.archive_uri; name = "archive-uri" }
              (Archive_lib.Diff.Transition_frontier transition_frontier)
          with
          | Ok () ->
              return ()
          | Error e ->
              raise (Error.to_exn e) ) )
  >>| Result.map ~f:ignore

let fetch_current_ledger_hash ~zeko_uri () =
  let query =
    {|
      query {
        stateHashes {
          unprovedLedgerHash
        }
      }
    |}
  in
  let body =
    Yojson.Safe.to_string
    @@ `Assoc [ ("query", `String query); ("variables", `Assoc []) ]
  in
  let headers =
    List.fold ~init:(Cohttp.Header.init ())
      ~f:(fun acc (k, v) -> Cohttp.Header.add acc k v)
      [ ("Accept", "application/json"); ("Content-Type", "application/json") ]
  in
  let%bind.Deferred.Result response, body =
    Deferred.Or_error.try_with ~here:[%here] ~extract_exn:true (fun () ->
        Cohttp_async.Client.post ~headers
          ~body:(Cohttp_async.Body.of_string body)
          zeko_uri )
    |> Deferred.Result.map_error ~f:(fun e -> Error.to_string_hum e)
  in
  let%bind body_str = Cohttp_async.Body.to_string body in
  let%bind.Deferred.Result body_json =
    match
      Cohttp.Code.code_of_status (Cohttp_async.Response.status response)
    with
    | 200 ->
        Deferred.return (Ok (Yojson.Safe.from_string body_str))
    | code ->
        Deferred.return
          (Error (Printf.sprintf "Status code %d -- %s" code body_str))
  in
  let open Yojson.Safe.Util in
  match (member "errors" body_json, member "data" body_json) with
  | `Null, `Null ->
      return (Error "Empty response from graphql query")
  | error, `Null ->
      return (Error (Yojson.Safe.to_string error))
  | _, raw_json ->
      let unproved_ledger_hash =
        member "stateHashes" raw_json
        |> member "unprovedLedgerHash"
        |> to_string |> Ledger_hash.of_decimal_string
      in
      return (Ok unproved_ledger_hash)

let sync (t : t) () =
  let logger = t.logger in
  Thread_safe.block_on_async_exn (fun () ->
      let%bind ledger_hash =
        match%bind fetch_current_ledger_hash ~zeko_uri:t.zeko_uri () with
        | Ok hash ->
            [%log info] "Fetched ledger hash: %s"
              (Ledger_hash.to_decimal_string hash) ;
            return hash
        | Error e ->
            failwith e
      in
      [%log info] "Syncing to ledger hash %s from %s"
        (Ledger_hash.to_decimal_string ledger_hash)
        (Ledger.Db.merkle_root t.ledger |> Ledger_hash.to_decimal_string) ;
      if%bind
        Da_layer.Client.diff_exists ~logger ~config:t.da_config ~ledger_hash ()
        >>| Or_error.ok_exn
      then time ~logger "Synced" (sync_archive t ~hash:ledger_hash)
      else (
        [%log warn] "Diff does not exist yet, skipping sync" ;
        return (Ok ()) ) )

let rec run (t : t) ~sync_period () =
  let { logger; _ } = t in
  let () =
    match
      (try Ok (sync t ()) with e -> Error (Error.of_exn e)) |> Or_error.join
    with
    | Ok () ->
        (* wait *)
        Thread_safe.block_on_async_exn (fun () ->
            after (Time.Span.of_sec sync_period) )
    | Error e ->
        (* ledger_hash_invalidated *)
        [%log error] "Error syncing: %s" (Error.to_string_hum e) ;
        [%log warn] "Invalidating ledger" ;
        reset_ledger t ()
  in
  (* go again *)
  run t ~sync_period ()
