open Core_kernel
open Mina_base
open Signature_lib
open Snark_params.Tick
open Async
open Zeko_circuits.Zeko_util

let retry ?logger ?(max_attempts = 5) ?(delay = Time.Span.of_sec 1.) ~f () =
  let rec go attempt =
    match%bind
      Monitor.try_with ~here:[%here] f
      >>| Result.map_error ~f:Error.of_exn
      >>| Or_error.join
    with
    | Ok x ->
        return (Ok x)
    | Error e when attempt < max_attempts ->
        let () =
          match logger with
          | None ->
              ()
          | Some logger ->
              [%log warn] "Failed to execute function, retrying..."
                ~metadata:[ ("error", `String (Error.to_string_hum e)) ]
        in
        let%bind () = after delay in
        go (attempt + 1)
    | Error e ->
        return (Error e)
  in
  go 1

let time (d : 'a Deferred.t) =
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  return (x, Time.diff stop start)

let log_time ~logger label (d : 'a Deferred.t) =
  let%bind x, t = time d in
  [%log info] "%s took: %s" label (Time.Span.to_string_hum t) ;
  return x

let value_to_zkapp_state (some : Field.t -> 'option) (none : 'option)
    (typ : ('var, 'value) Typ.t) (x : 'value) : 'option Zkapp_state.V.t =
  let (Typ typ) = typ in
  let fields, _aux = typ.value_to_fields x in
  assert (Array.length fields <= 8) ;
  let missing = 8 - Array.length fields in
  Zkapp_state.V.of_list_exn
  @@ List.append
       (List.map ~f:(fun f -> some f) @@ Array.to_list fields)
       (List.init missing ~f:(fun _ -> none))

let value_of_zkapp_state (typ : ('var, 'value) Typ.t) (x : field Zkapp_state.V.t)
    : 'value =
  let (Typ typ) = typ in
  typ.value_of_fields
    ( Zkapp_state.V.to_list x |> Array.of_list
    , typ.constraint_system_auxiliary () )

let value_to_fields (type var value) (typ : (var, value) Typ.t) (x : value) :
    Field.t array =
  let (Typ typ) = typ in
  let fields, _aux = typ.value_to_fields x in
  fields

let value_of_fields (type var value aux) (typ : (var, value, aux) Typ.typ')
    (fields : Field.t array) (aux : aux) : value =
  typ.value_of_fields (fields, aux)

let value_to_hash ~(init : string)
    (typ : ('var, 'value) Snark_params.Tick.Typ.t) (x : 'value) : Field.t =
  let (Typ typ) = typ in
  let fields, _aux = typ.value_to_fields x in
  Random_oracle.hash ~init:(Hash_prefix_create.salt init) fields

let commit_to_actions x =
  [ value_to_fields
      Typ.(F.typ * Zeko_circuits.Rollup_state.Outer_action.Commit.typ)
      (Field.of_int 0, x)
  ]

let witness_to_actions x =
  [ value_to_fields
      Typ.(F.typ * Zeko_circuits.Rollup_state.Outer_action.Witness.typ)
      (Field.of_int 1, x)
  ]

let actions_to_outer_action x : Zeko_circuits.Rollup_state.Outer_action.t =
  if Field.equal x.(0) Field.zero then
    let (Typ typ) = Zeko_circuits.Rollup_state.Outer_action.Commit.typ in
    Commit
      (typ.value_of_fields
         ( Array.sub ~pos:1 ~len:(Array.length x - 1) x
         , typ.constraint_system_auxiliary () ) )
  else if Field.equal x.(0) Field.one then
    let (Typ typ) =
      Zeko_circuits.Rollup_state.Outer_action.Witness.Without_forest.typ
    in
    Witness
      (typ.value_of_fields
         ( Array.sub ~pos:1 ~len:(Array.length x - 1) x
         , typ.constraint_system_auxiliary () ) )
  else failwith "Invalid outer action"

let actions_of_outer_action :
    Zeko_circuits.Rollup_state.Outer_action.t -> field array = function
  | Commit x ->
      let (Typ typ) =
        Typ.(F.typ * Zeko_circuits.Rollup_state.Outer_action.Commit.typ)
      in
      typ.value_to_fields (Field.of_int 0, x) |> fst
  | Witness x ->
      let (Typ typ) =
        Typ.(
          F.typ
          * Zeko_circuits.Rollup_state.Outer_action.Witness.Without_forest.typ)
      in
      typ.value_to_fields (Field.of_int 1, x) |> fst

let actions_to_inner_action x :
    Zeko_circuits.Rollup_state.Inner_action.Without_forest.t =
  if Field.equal x.(0) Field.zero then
    let (Typ typ) =
      Zeko_circuits.Rollup_state.Inner_action.Without_forest.typ
    in
    typ.value_of_fields
      ( Array.sub ~pos:1 ~len:(Array.length x - 1) x
      , typ.constraint_system_auxiliary () )
  else failwith "Invalid inner action"

let actions_of_inner_action x =
  let (Typ typ) = Typ.(F.typ * Zeko_circuits.Rollup_state.Inner_action.typ) in
  typ.value_to_fields (Field.of_int 0, x) |> fst

let actions_of_inner_action_without_forest x =
  let (Typ typ) =
    Typ.(F.typ * Zeko_circuits.Rollup_state.Inner_action.Without_forest.typ)
  in
  typ.value_to_fields (Field.of_int 0, x) |> fst

let update_state pk command state =
  let open Zkapp_basic in
  let account_id = Account_id.create pk Token_id.default in
  match
    Zkapp_command.Poly.account_updates command
    |> Zkapp_command.Call_forest.to_list
    |> List.find ~f:(fun account_update ->
           Account_update.account_id account_update
           |> Account_id.equal account_id )
  with
  | None ->
      `Skipped
  | Some account_update -> (
      let body = Account_update.Poly.body account_update in
      let preconditions =
        body |> Account_update.Body.preconditions
        |> Account_update.Preconditions.account
        |> Zkapp_precondition.Account.state
      in
      let update =
        body |> Account_update.Body.update |> Account_update.Update.app_state
      in
      match
        List.map3_exn (Zkapp_state.V.to_list state)
          (Zkapp_state.V.to_list preconditions) (Zkapp_state.V.to_list update)
          ~f:(fun s p u -> (s, p, u))
        |> List.fold_map ~init:`Updated ~f:(function
             | `Precondition_failed ->
                 fun (s, _, _) -> (`Precondition_failed, s)
             | `Updated -> (
                 fun (s, p, u) ->
                   let u = Set_or_keep.to_option u |> Option.value ~default:s in
                   match Or_ignore.to_option p with
                   | None ->
                       (`Updated, u)
                   | Some p ->
                       if Field.equal s p then (`Updated, u)
                       else (`Precondition_failed, u) ) )
      with
      | `Precondition_failed, _ ->
          `Precondition_failed
      | `Updated, new_state ->
          `Updated (Zkapp_state.V.of_list_exn new_state) )

let get_synced_outer_action_state_exn l =
  let open Zeko_circuits in
  let ({ outer_action_state } : Rollup_state.Inner_state.t) =
    let idx =
      Mina_ledger.Ledger.index_of_account_exn l Zeko_constants.inner_account_id
    in
    let inner_acc = Mina_ledger.Ledger.get_at_index_exn l idx in
    (Option.value_exn inner_acc.zkapp).app_state
    |> Rollup_state.Inner_state.value_of_app_state
  in
  outer_action_state

let sign_zkapp_command ~signature_kind (command : Zkapp_command.t)
    (signers : Keypair.t list) : Zkapp_command.t =
  let tx_commitment = Zkapp_command.commitment command in
  let full_commitment =
    Zkapp_command.Transaction_commitment.create_complete tx_commitment
      ~memo_hash:(Signed_command_memo.hash command.memo)
      ~fee_payer_hash:
        (Zkapp_command.Digest.Account_update.create ~signature_kind
           (Account_update.of_fee_payer command.fee_payer) )
  in
  let sign_raw (pk : Public_key.Compressed.t) msg =
    match
      List.find signers ~f:(fun kp ->
          Public_key.Compressed.equal (Public_key.compress kp.public_key) pk )
    with
    | Some kp ->
        Signature_lib.Schnorr.Chunked.sign ~signature_kind kp.private_key
          (Random_oracle.Input.Chunked.field msg)
    | None ->
        failwithf "key not found: %s\n"
          (Public_key.Compressed.to_base58_check pk)
          ()
  in
  let rec sign_tree
      (tree :
        ( Account_update.t
        , Zkapp_command.Digest.Account_update.t
        , Zkapp_command.Digest.Forest.t )
        Zkapp_command.Call_forest.Tree.t ) =
    { tree with
      account_update =
        { tree.account_update with
          authorization =
            ( match tree.account_update.body.authorization_kind with
            | Signature ->
                let commitment =
                  if tree.account_update.body.use_full_commitment then
                    full_commitment
                  else tx_commitment
                in
                Control.Poly.Signature
                  (sign_raw tree.account_update.body.public_key commitment)
            | _ ->
                tree.account_update.authorization )
        }
    ; calls = sign_forest tree.calls
    }
  and sign_forest forest =
    List.map ~f:(fun tree -> { tree with elt = sign_tree tree.elt }) forest
  in
  { command with
    fee_payer =
      { command.fee_payer with
        authorization =
          ( if
            Public_key.Compressed.(
              equal empty command.fee_payer.body.public_key)
          then command.fee_payer.authorization
          else sign_raw command.fee_payer.body.public_key full_commitment )
      }
  ; account_updates = sign_forest command.account_updates
  }

let rehash_forest ~signature_kind =
  Zkapp_command.Call_forest.accumulate_hashes
    ~hash_account_update:
      (Zkapp_command.Call_forest.Digest.Account_update.create ~signature_kind)

let signature_kind = function
  | "mainnet" ->
      Mina_signature_kind.Mainnet
  | "testnet" ->
      Mina_signature_kind.Testnet
  | network_id ->
      Mina_signature_kind.Other_network network_id

(** minimum_fee * e^(q * 0.1 * modifier) *)
let fee_per_weight_unit ~minimum_fee ~fee_modifier ~jobs_in_queue =
  minimum_fee
  *. exp (jobs_in_queue *. 0.1 *. fee_modifier)
  (* convert to nanomina *)
  *. 10e8

let slot_range_intersection (a : Slot_range.t option) (b : Slot_range.t option)
    =
  let%bind.Option a = a in
  let%bind.Option b = b in
  let lower = Slot.max a.lower b.lower in
  let upper = Slot.min a.upper b.upper in
  if Slot.(lower <= upper) then Some ({ lower; upper } : Slot_range.t) else None

let command_slot_range (command : User_command.t) : Slot_range.t option =
  let precondition_to_range :
      Slot.t Zkapp_precondition.Numeric.t -> Slot_range.t = function
    | Ignore ->
        Slot_range.infinite
    | Check range ->
        { lower = range.lower; upper = range.upper }
  in
  match command with
  | Signed_command command ->
      Some { lower = Slot.zero; upper = Signed_command.valid_until command }
  | Zkapp_command command ->
      Zkapp_command.all_account_updates_list command
      |> List.fold ~init:(Some Slot_range.infinite) ~f:(fun acc au ->
             let valid_while =
               precondition_to_range
                 (Account_update.valid_while_precondition au)
             in
             let global_slot =
               precondition_to_range
                 (Account_update.protocol_state_precondition au)
                   .global_slot_since_genesis
             in
             slot_range_intersection (Some valid_while) (Some global_slot)
             |> slot_range_intersection acc )

module Slot = struct
  type l1_config = { fork_timestamp : Time.t; fork_slot : Slot.t }

  module For_tests = struct
    let add_to_global_slot = ref 0
  end

  let global_slot ~l1_config =
    let after_fork_slot =
      (Time.abs_diff (Time.now ()) l1_config.fork_timestamp |> Time.Span.to_sec)
      /. 180.
      |> Float.to_int
      |> ( + ) !For_tests.add_to_global_slot
      |> Mina_numbers.Global_slot_span.of_int
    in
    Mina_numbers.Global_slot_since_genesis.add l1_config.fork_slot
      after_fork_slot
end

let attach_proof_to_forest ~signature_kind ~proof_cache_db ~body ~calls ~proof =
  match Is_compile_simple_real.is_compile_simple_real with
  | Some eq ->
      let proof_eq, _ = Type_equal.detuple2 eq in
      let account_update =
        Account_update.with_aux ~body
          ~authorization:
            (Control.Poly.Proof
               (Proof_cache_tag.write_proof_to_disk proof_cache_db
                  (Type_equal.conv proof_eq proof) ) )
        |> Account_update.read_all_proofs_from_disk
      in
      Zkapp_command.Call_forest.cons ~signature_kind ~calls account_update []
  | None ->
      let account_update =
        Account_update.with_aux
          ~body:{ body with authorization_kind = None_given }
          ~authorization:Control.Poly.None_given
        |> Account_update.read_all_proofs_from_disk
      in
      Zkapp_command.Call_forest.cons ~signature_kind ~calls account_update []

module Forest_shape = struct
  (** add more fields if needed *)
  type field =
    | Calls of field list list
    | Public_key of Public_key.Compressed.t
    | Token_id of Token_id.t

  let rec check_tree
      ({ account_update = au; calls; _ } as tree :
        (Account_update.t, _, _) Zkapp_command.Call_forest.Tree.t ) = function
    | [] ->
        true
    | Public_key pk :: rest ->
        Public_key.Compressed.equal au.body.public_key pk
        && check_tree tree rest
    | Token_id token_id :: rest ->
        Token_id.equal au.body.token_id token_id && check_tree tree rest
    | Calls calls_spec :: rest -> (
        match List.zip calls calls_spec with
        | List.Or_unequal_lengths.Unequal_lengths ->
            false
        | List.Or_unequal_lengths.Ok l ->
            List.map l ~f:(fun (tree, spec) ->
                check_tree (With_stack_hash.elt tree) spec )
            |> List.for_all ~f:Fn.id
            && check_tree tree rest )

  let matches (forest : (Account_update.t, _, _) Zkapp_command.Call_forest.t)
      (spec : field list list) =
    match List.zip forest spec with
    | List.Or_unequal_lengths.Unequal_lengths ->
        false
    | List.Or_unequal_lengths.Ok l ->
        List.map l ~f:(fun (tree, spec) ->
            check_tree (With_stack_hash.elt tree) spec )
        |> List.for_all ~f:Fn.id
end

let is_deposit_finalization (command : User_command.t) =
  let open Forest_shape in
  let holder_token_id =
    Account_id.derive_token_id
      ~owner:
        ( Account_id.of_public_key
        @@ Public_key.decompress_exn
             Zeko_circuits_config.Inputs.holder_account_l2 )
  in
  match command with
  | Signed_command _ ->
      false
  | Zkapp_command command ->
      matches command.account_updates
        [ [ Public_key Zeko_circuits_config.Inputs.holder_account_l2
          ; Token_id Token_id.default
          ; Calls
              [ [ Token_id holder_token_id ]
              ; [ Public_key Zeko_circuits_config.Inputs.zeko_l2
                ; Token_id Token_id.default
                ]
              ; [ Token_id Token_id.default ]
              ; [ Public_key Zeko_circuits_config.Inputs.bridge_fee_recipient_l2
                ; Token_id Token_id.default
                ]
              ]
          ]
        ]

let sign_fee_payer ~signature_kind (sequencer_signer : Keypair.t)
    (command : User_command.t) : User_command.t =
  match command with
  | Signed_command command ->
      Signed_command command
  | Zkapp_command command ->
      let full_commitment =
        Zkapp_command.Transaction_commitment.create_complete
          (Zkapp_command.commitment command)
          ~memo_hash:(Signed_command_memo.hash command.memo)
          ~fee_payer_hash:
            (Zkapp_command.Digest.Account_update.create ~signature_kind
               (Account_update.of_fee_payer command.fee_payer) )
      in
      Zkapp_command
        { command with
          fee_payer =
            { command.fee_payer with
              authorization =
                ( if
                  Public_key.Compressed.(
                    equal
                      (Public_key.compress sequencer_signer.public_key)
                      command.fee_payer.body.public_key)
                then
                  Signature_lib.Schnorr.Chunked.sign ~signature_kind
                    sequencer_signer.private_key
                    (Random_oracle.Input.Chunked.field full_commitment)
                else command.fee_payer.authorization )
            }
        }

let validate_zkapp_command (command : Zkapp_command.t) =
  let actions_and_events_valid =
    Zkapp_command.account_updates_list command
    |> List.map ~f:Account_update.Poly.body
    |> List.map ~f:(fun body -> [ body.events; body.actions ])
    |> List.concat |> List.concat |> List.map ~f:Array.length
    |> List.for_all ~f:(fun len -> len <= 16)
  in
  let%map.Result () =
    if actions_and_events_valid then Ok ()
    else Error "Actions and events or not valid"
  in
  ()

module type Db_intf = sig
  type t

  val create : ?directory_name:string -> depth:int -> unit -> t

  val create_checkpoint : t -> directory_name:string -> unit -> t
end

let create_db_from_checkpoint (type t) (module Db : Db_intf with type t = t)
    ~depth ~db_dir ~checkpoint_dir =
  let checkpoint = Db.create ~directory_name:checkpoint_dir ~depth () in
  (* we create checkpoint to copy the db *)
  Db.create_checkpoint checkpoint ~directory_name:db_dir ()
