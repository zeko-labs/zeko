(*
[@@@warning "-8-4"] (* ignore partial match and fragile-match warning *)

open Core
open Mina_ledger
open Currency
open Signature_lib
open Mina_base
module For_tests = Mina_transaction_logic.For_tests
open Async_kernel
module Field = Snark_params.Tick.Field
open Genesis_constants.Compiled

let block_exn d = Promise.block_on_async_exn d

let zero_fee_payer (nonce : int) (kp : Keypair.t) : Account_update.Fee_payer.t =
  { body =
      { public_key = Public_key.compress kp.public_key
      ; fee = Fee.zero
      ; valid_until = None
      ; nonce = Account.Nonce.of_int nonce
      }
  ; authorization = Signature.dummy
  }

let outer_fee_payer = ref None

let pay_outer_fee =
  let nonce = ref 0 in
  fun () ->
    let r = zero_fee_payer !nonce (Option.value_exn !outer_fee_payer) in
    nonce := !nonce + 1 ;
    r

let inner_fee_payer : Account_update.Fee_payer.t =
  { body =
      { public_key = Public_key.Compressed.empty
      ; fee = Fee.zero
      ; valid_until = None
      ; nonce = Account.Nonce.zero
      }
  ; authorization = Signature.dummy
  }

let num_accounts = 4

let pretty_print_cmd (cmd : Zkapp_command.t) : unit =
  let rec strip_tree (tree : _ Zkapp_command.Call_forest.Tree.t) :
      _ Zkapp_command.Call_forest.Tree.t =
    { tree with
      account_update =
        Account_update.{ tree.account_update with authorization = None_given }
    ; calls = strip_forest tree.calls
    }
  and strip_forest (forest : _ Zkapp_command.Call_forest.t) :
      _ Zkapp_command.Call_forest.t =
    List.map ~f:(fun tree -> { tree with elt = strip_tree tree.elt }) forest
  in

  let cmd =
    { cmd with
      fee_payer = { cmd.fee_payer with authorization = Signature.dummy }
    ; account_updates = strip_forest cmd.account_updates
    }
  in
  printf "%s\n" @@ Sexp.to_string_hum (Zkapp_command.sexp_of_t cmd)

let check_no_failure
    (applied :
      Mina_transaction_logic.Transaction_applied.Zkapp_command_applied.t ) cmd =
  match applied.command.status with
  | Applied ->
      ()
  | Failed failuress ->
      List.iteri
        ~f:(fun i failures ->
          printf "Failures of account update %i:\n" i ;
          List.iter failures
            ~f:
              Transaction_status.Failure.(
                fun failure ->
                  printf "%s: %s\n" (to_string failure) (describe failure)) )
        failuress ;
      pretty_print_cmd cmd ;
      failwith "check_no_failure failed"

let graphql_uri = "http://localhost:8080/graphql"

include struct
  open struct
    module GQL = Graphql_lib.Client.Make (struct
      let preprocess_variables_string = Fn.id

      let headers = String.Map.empty
    end)
  end

  let send_gql q =
    let r =
      Async.Thread_safe.block_on_async_exn
      @@ fun () -> GQL.query_json q (Uri.of_string graphql_uri)
    in
    ( match r with
    | Error (`Failed_request err) ->
        printf "Failed_request %s" err
    | Error (`Graphql_error err) ->
        printf "Graphql_error %s" err
    | Ok r ->
        Yojson.Safe.pretty_print Format.err_formatter r ) ;
    ()
end

let send_zkapp (command : Zkapp_command.t) =
  pretty_print_cmd command ;
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            mutation ($input: SendZkappInput!) {
              sendZkapp(input: $input){
                zkapp {
                  id
                  failureReason {
                    index
                    failures
                  }
                }
              }
            } 
          |}

      method variables =
        `Assoc
          [ ( "input"
            , `Assoc
                [ ( "zkappCommand"
                  , Yojson.Safe.to_basic @@ Zkapp_command.to_json command )
                ] )
          ]
    end
  in
  send_gql q ; ()

let with_mask (type a) (ledger : Ledger.t) ~(f : Ledger.t -> a) : a =
  let mask = Ledger.Mask.create ~depth:(Ledger.depth ledger) () in
  let ledger' = Ledger.register_mask ledger mask in
  let r = f ledger' in
  let (_ : Ledger.unattached_mask) =
    Ledger.unregister_mask_exn ~loc:__LOC__ ledger'
  in
  r

let to_sparse ledger cmd =
  Sparse_ledger.of_ledger_subset_exn ledger
    (Zkapp_command.accounts_referenced cmd)

let take_from_sparse ledger sparse =
  Sparse_ledger.iteri sparse ~f:(fun i a -> Ledger.set_at_index_exn ledger i a) ;
  assert (
    Field.equal (Ledger.merkle_root ledger) (Sparse_ledger.merkle_root sparse) )

module Account_set = struct
  module Impl = struct
    module Map = Core_kernel.Map.Make (Token_id)

    type location = Unsigned.UInt32.t

    type map = location Map.t

    let merge x y =
      Random_oracle.hash
        ~init:(Hash_prefix_create.salt "indexed merkle tree")
        [| x; y |]

    let empty_hash : int -> Snark_params.Tick.field =
      let rec go =
        lazy
          (Memo.of_comparable
             (module Int)
             (function
               | 0 ->
                   Field.zero
               | n ->
                   let prev = force go (n - 1) in
                   merge prev prev ) )
      in
      force go

    (* kind level = level_z | level_s of level *)
    type level_z = |

    type 'level level_s = |

    type 'level level_witness =
      | Level_z : level_z level_witness
      | Level_s : 'level level_witness -> 'level level_s level_witness

    [@@@ocamlformat "disable"]

    include struct
    type level_1 = level_z level_s
    type level_2 = level_1 level_s
    type level_3 = level_2 level_s
    type level_4 = level_3 level_s
    type level_5 = level_4 level_s
    type level_6 = level_5 level_s
    type level_7 = level_6 level_s
    type level_8 = level_7 level_s
    type level_9 = level_8 level_s
    type level_10 = level_9 level_s
    type level_11 = level_10 level_s
    type level_12 = level_11 level_s
    type level_13 = level_12 level_s
    type level_14 = level_13 level_s
    type level_15 = level_14 level_s
    type level_16 = level_15 level_s
    type level_17 = level_16 level_s
    type level_18 = level_17 level_s
    type level_19 = level_18 level_s
    type level_20 = level_19 level_s
    type level_21 = level_20 level_s
    type level_22 = level_21 level_s
    type level_23 = level_22 level_s
    type level_24 = level_23 level_s
    type level_25 = level_24 level_s
    type level_26 = level_25 level_s
    type level_27 = level_26 level_s
    type level_28 = level_27 level_s
    type level_29 = level_28 level_s
    type level_30 = level_29 level_s
    type level_31 = level_30 level_s
    type level_32 = level_31 level_s
    let level_1 : level_1 level_witness = Level_s Level_z
    let level_2 : level_2 level_witness = Level_s level_1
    let level_3 : level_3 level_witness = Level_s level_2
    let level_4 : level_4 level_witness = Level_s level_3
    let level_5 : level_5 level_witness = Level_s level_4
    let level_6 : level_6 level_witness = Level_s level_5
    let level_7 : level_7 level_witness = Level_s level_6
    let level_8 : level_8 level_witness = Level_s level_7
    let level_9 : level_9 level_witness = Level_s level_8
    let level_10 : level_10 level_witness = Level_s level_9
    let level_11 : level_11 level_witness = Level_s level_10
    let level_12 : level_12 level_witness = Level_s level_11
    let level_13 : level_13 level_witness = Level_s level_12
    let level_14 : level_14 level_witness = Level_s level_13
    let level_15 : level_15 level_witness = Level_s level_14
    let level_16 : level_16 level_witness = Level_s level_15
    let level_17 : level_17 level_witness = Level_s level_16
    let level_18 : level_18 level_witness = Level_s level_17
    let level_19 : level_19 level_witness = Level_s level_18
    let level_20 : level_20 level_witness = Level_s level_19
    let level_21 : level_21 level_witness = Level_s level_20
    let level_22 : level_22 level_witness = Level_s level_21
    let level_23 : level_23 level_witness = Level_s level_22
    let level_24 : level_24 level_witness = Level_s level_23
    let level_25 : level_25 level_witness = Level_s level_24
    let level_26 : level_26 level_witness = Level_s level_25
    let level_27 : level_27 level_witness = Level_s level_26
    let level_28 : level_28 level_witness = Level_s level_27
    let level_29 : level_29 level_witness = Level_s level_28
    let level_30 : level_30 level_witness = Level_s level_29
    let level_31 : level_31 level_witness = Level_s level_30
    let level_32 : level_32 level_witness = Level_s level_31
    end

    [@@@ocamlformat "enable"]

    type 'level tree =
      | Empty : 'level tree
      | Node :
          { hash : Snark_params.Tick.field
          ; left : 'level tree
          ; right : 'level tree
          ; sparse : [ `Sparse | `Full ]
          }
          -> 'level level_s tree
      | Leaf : { hash : Snark_params.Tick.field } -> level_z tree

    type full_tree = Full_tree of level_32 tree

    type 'level location' =
      | Loc_end : level_z location'
      | Left_loc : 'level location' -> 'level level_s location'
      | Right_loc : 'level location' -> 'level level_s location'

    let location_to_list : location -> level_32 location' =
      let rec go :
          type level. location -> level level_witness -> level location' =
       fun (location : location) -> function
        | Level_z ->
            Loc_end
        | Level_s n_remaining ->
            let tail =
              go (Unsigned_extended.UInt32.shift_right location 1) n_remaining
            in
            if Unsigned_extended.UInt32.(logand location (of_string "1") > zero)
            then Right_loc tail
            else Left_loc tail
      in
      fun location -> go location level_32

    type 'level path =
      | End : level_z path
      | Left : 'level path * Snark_params.Tick.field -> 'level level_s path
      | Right : 'level path * Snark_params.Tick.field -> 'level level_s path

    type full_path = Full_path of level_32 path

    let hash_of =
      let rec level_to_int : type level. level level_witness -> int = function
        | Level_z ->
            0
        | Level_s level ->
            1 + level_to_int level
      in
      let go :
          type level.
          level level_witness * level tree -> Snark_params.Tick.field = function
        | level, Empty ->
            level_to_int level |> empty_hash
        | _, Node { hash; left = _; right = _; sparse = _ } ->
            hash
        | _, Leaf { hash } ->
            hash
      in
      fun level tree -> go (level, tree)

    let get_path (location : location) (Full_tree tree) : full_path =
      let location = location_to_list location in
      let rec go :
          type level.
          level level_witness * level tree * level location' -> level path =
        function
        | _, Empty, _ ->
            failwith "invalid location"
        | Level_z, Leaf { hash = _ }, Loc_end ->
            End
        | ( Level_s level
          , Node { hash = _; left; right; sparse = _ }
          , Right_loc location ) ->
            Right (go (level, right, location), hash_of level left)
        | ( Level_s level
          , Node { hash = _; left; right; sparse = _ }
          , Left_loc location ) ->
            Left (go (level, left, location), hash_of level right)
      in
      Full_path (go (level_32, tree, location))

    type t = Account_set of map * full_tree

    let mknode (type level) (level : level level_witness) (left : level tree)
        (right : level tree) : level level_s tree =
      let sparse =
        match (left, right) with
        | Empty, _
        | _, Empty
        | Node { hash = _; left = _; right = _; sparse = `Sparse }, _
        | _, Node { hash = _; left = _; right = _; sparse = `Sparse } ->
            `Sparse
        | _ ->
            `Full
      in
      Node
        { hash = merge (hash_of level left) (hash_of level right)
        ; left
        ; right
        ; sparse
        }

    let hash_entry (key : Token_id.t) (next_key : Token_id.t) =
      let x = Token_id.to_field_unsafe key in
      let y = Token_id.to_field_unsafe next_key in
      Random_oracle.hash
        ~init:(Hash_prefix_create.salt "indexed merkle tree entry hash")
        [| x; y |]

    [@@@ocamlformat "disable"]

    let initial : t =
      let least = Token_id.of_field Snark_params.Tick.Field.zero in
      let most = Token_id.of_field Snark_params.Tick.Field.(negate one) in
      let map =
        let open Map in
        add_exn empty ~key:least ~data:Unsigned.UInt32.zero
        |> add_exn ~key:most ~data:Unsigned.UInt32.one
      in
      let left =
        mknode Level_z
          (Leaf { hash = hash_entry least most })
          (Leaf { hash = hash_entry most least })
      in
      let tree =
        mknode level_1 left Empty |> fun left ->
        mknode level_2 left Empty |> fun left ->
        mknode level_3 left Empty |> fun left ->
        mknode level_4 left Empty |> fun left ->
        mknode level_5 left Empty |> fun left ->
        mknode level_6 left Empty |> fun left ->
        mknode level_7 left Empty |> fun left ->
        mknode level_8 left Empty |> fun left ->
        mknode level_9 left Empty |> fun left ->
        mknode level_10 left Empty |> fun left ->
        mknode level_11 left Empty |> fun left ->
        mknode level_12 left Empty |> fun left ->
        mknode level_13 left Empty |> fun left ->
        mknode level_14 left Empty |> fun left ->
        mknode level_15 left Empty |> fun left ->
        mknode level_16 left Empty |> fun left ->
        mknode level_17 left Empty |> fun left ->
        mknode level_18 left Empty |> fun left ->
        mknode level_19 left Empty |> fun left ->
        mknode level_20 left Empty |> fun left ->
        mknode level_21 left Empty |> fun left ->
        mknode level_22 left Empty |> fun left ->
        mknode level_23 left Empty |> fun left ->
        mknode level_24 left Empty |> fun left ->
        mknode level_25 left Empty |> fun left ->
        mknode level_26 left Empty |> fun left ->
        mknode level_27 left Empty |> fun left ->
        mknode level_28 left Empty |> fun left ->
        mknode level_29 left Empty |> fun left ->
        mknode level_30 left Empty |> fun left ->
        mknode level_31 left Empty
      in
      Account_set (map, Full_tree tree)

    [@@@ocamlformat "enable"]
  end

  open Impl

  type t = Impl.t

  let root
      (Account_set
        (_, Full_tree (Node { hash; sparse = _; left = _; right = _ })) ) =
    hash
end

type staged_ledger =
  { ledger : Ledger.t
  ; proof : Zeko_circuits.Zeko_transaction_snark.T.t option ref
  ; account_set : Account_set.t
  }

let consensus_constants =
  Consensus.Constants.create ~constraint_constants
    ~protocol_constants:genesis_constants.protocol

let state_body =
  let compile_time_genesis =
    Mina_state.Genesis_protocol_state.t
      ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
      ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
      ~constraint_constants ~consensus_constants
      ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference
  in
  Mina_state.Protocol_state.body compile_time_genesis.data

let pc =
  Pending_coinbase.Stack.push_state
    (Mina_state.Protocol_state.Body.hash state_body)
    Mina_numbers.Global_slot_since_genesis.zero Pending_coinbase.Stack.empty

let Zeko_circuits.Compile_simple.
      [ prove_user_command
      ; prove_single_zkapp
      ; prove_double_zkapp
      ; prove_proven_zkapp
      ; prove_merge
      ] =
  Zeko_circuits.Zeko_transaction_snark.provers

let point_of_string s =
  let point =
    Snark_params.Tick.Inner_curve.(
      to_affine_exn @@ point_near_x @@ Field.of_string s)
  in
  let ({ x; is_odd } : Public_key.Compressed.t) = Public_key.compress point in
  assert (not is_odd) ;
  x

let sequencer : Zeko_circuits.Zeko_util.Even_PC.t =
  { public_key = point_of_string "123456789" }

let list_to_unit_function : 'a list -> unit -> 'a =
 fun l ->
  let r = ref l in
  fun () ->
    match !r with
    | x :: xs ->
        r := xs ;
        x
    | [] ->
        failwith "empty"

let prove_zkapp ~source_acc_set
    ~(statement : Mina_state.Snarked_ledger_state.With_sok.t) ~witness
    ~(spec : Transaction_snark.Zkapp_command_segment.Basic.t) =
  let transform :
         Mina_state.Local_state.t
      -> Zeko_circuits.Zeko_transaction_snark.Local_state.t =
   fun { stack_frame
       ; call_stack
       ; transaction_commitment
       ; full_transaction_commitment
       ; excess
       ; supply_increase
       ; ledger
       ; success
       ; account_update_index
       ; failure_status_tbl
       ; will_succeed
       } ->
    { stack_frame
    ; call_stack
    ; transaction_commitment
    ; full_transaction_commitment
    ; excess
    ; ledger
    ; account_update_index
    }
  in
  let xs = failwith "FIXME" in
  let zs = failwith "FIXME" in
  let xs_paths = failwith "FIXME" in
  let ys_paths = failwith "FIXME" in
  let update_acc_set_witness :
      Zeko_circuits.Zeko_transaction_snark.update_acc_set_witness =
    { get_account_set_x = list_to_unit_function xs
    ; get_account_set_z = list_to_unit_function zs
    ; get_account_set_x_path = list_to_unit_function xs_paths
    ; get_account_set_y_path = list_to_unit_function ys_paths
    }
  in
  let witness : Zeko_circuits.Zeko_transaction_snark.Zkapp_witness.t =
    { txn_snark_witness = witness; update_acc_set_witness }
  in
  let stmt, proof =
    block_exn (fun () ->
        match spec with
        | Opt_signed ->
            let shift_action_state = false in
            let base : Zeko_circuits.Zeko_transaction_snark.Zkapp_rule_input.t =
              { source_ledger = statement.source.first_pass_ledger
              ; target_ledger = statement.target.second_pass_ledger
              ; connecting_ledger = statement.connecting_ledger_left
              ; source_local_state = transform statement.source.local_state
              ; target_local_state = transform statement.target.local_state
              ; fee_excess = statement.fee_excess.fee_excess_l
              ; supply_decrease = statement.supply_increase.magnitude
              ; witness
              ; sequencer
              ; source_acc_set
              }
            in
            prove_single_zkapp { base; shift_action_state }
        | _ ->
            failwith "FIXME unimplemented"
        (* FIXME: uncomment and fix
           | Opt_signed_opt_signed ->
               let base : Zeko_circuits.Zeko_transaction_snark.Zkapp_rule_input.t =
                 { source_ledger = statement.source.first_pass_ledger
                 ; target_ledger = statement.target.second_pass_ledger
                 ; connecting_ledger = statement.connecting_ledger_left
                 ; source_local_state = transform statement.source.local_state
                 ; target_local_state = transform statement.target.local_state
                 ; fee_excess = statement.fee_excess.fee_excess_l
                 ; supply_decrease = statement.supply_increase.magnitude
                 ; witness
                 ; sequencer
                 ; source_acc_set
                 }
               in
               prove_double_zkapp
                 { base
                 ; shift_action_state_first = false
                 ; shift_action_state_second = false
                 }
           | Proved ->
               let base : Zeko_circuits.Zeko_transaction_snark.Zkapp_rule_input.t =
                 { source_ledger = statement.source.first_pass_ledger
                 ; target_ledger = statement.target.second_pass_ledger
                 ; connecting_ledger = statement.connecting_ledger_left
                 ; source_local_state = transform statement.source.local_state
                 ; target_local_state = transform statement.target.local_state
                 ; fee_excess = statement.fee_excess.fee_excess_l
                 ; supply_decrease = statement.supply_increase.magnitude
                 ; witness
                 ; sequencer
                 ; source_acc_set
                 }
               in
               prove_proven_zkapp
                 { base
                 ; zkapp_vk = ()
                 ; zkapp_proof = ()
                 ; shift_action_state = false
                 } *) )
  in
  ({ stmt; proof } : Zeko_circuits.Zeko_transaction_snark.T.t)

let prove_merge left right =
  let stmt, proof = block_exn @@ fun () -> prove_merge { left; right } in
  ({ stmt; proof } : Zeko_circuits.Zeko_transaction_snark.T.t)

(*
let zkapp_command_witness_exn (ledger : staged_ledger) (cmd : Zkapp_command.t) =
  let supply_increase = Amount.(Signed.of_unsigned zero) in
  let state_view = Mina_state.Protocol_state.Body.view state_body in

  let sparse_ledger = to_sparse ledger.ledger cmd in
  ()
*)

(* FIXME: allow paying fees *)
let prove_zeko_command (staged_ledger : staged_ledger) ~source_acc_set cmd :
    unit =
  let fee_excess = Amount.Signed.zero in
  let ledger = to_sparse staged_ledger.ledger cmd in
  let state_view = Mina_state.Protocol_state.Body.view state_body in
  let global_slot = state_view.global_slot_since_genesis in
  let supply_increase = Amount.Signed.zero in
  let connecting_ledger, new_ledger =
    let partial_txn, states =
      Or_error.ok_exn
      @@ Sparse_ledger.apply_zkapp_first_pass_unchecked_with_states
           ~constraint_constants ~state_view ~global_slot ~fee_excess
           ~supply_increase ~first_pass_ledger:ledger ~second_pass_ledger:ledger
           cmd
    in
    let txn, states =
      Sparse_ledger.apply_zkapp_second_pass_unchecked_with_states ~init:states
        ledger partial_txn
      |> Or_error.ok_exn
    in
    check_no_failure txn cmd ;
    let last_global, _ = List.last_exn states in
    (last_global.first_pass_ledger, last_global.second_pass_ledger)
  in
  let init_stack = Mina_base.Pending_coinbase.Stack.empty in
  let pending_coinbase_state_stack :
      Transaction_snark.Pending_coinbase_stack_state.t =
    { source = pc; target = pc }
  in
  let witnesses =
    Transaction_snark.zkapp_command_witnesses_exn ~constraint_constants
      ~global_slot ~state_body ~fee_excess
      [ ( `Pending_coinbase_init_stack init_stack
        , `Pending_coinbase_of_statement pending_coinbase_state_stack
        , `Sparse_ledger ledger
        , `Sparse_ledger connecting_ledger
        , `Connecting_ledger_hash (Sparse_ledger.merkle_root connecting_ledger)
        , cmd )
      ]
  in
  (* FIXME: Do merging tree-style *)
  let stmt =
    match List.rev witnesses with
    | [] ->
        failwith "no witnesses generated"
    | (witness, spec, statement) :: rest ->
        let p1 =
          printf "Proving first\n" ;
          prove_zkapp ~source_acc_set ~statement ~witness ~spec
        in
        List.foldi ~init:p1 rest ~f:(fun i prev (witness, spec, statement) ->
            printf "Proving %ith\n" (i + 1) ;
            let curr = prove_zkapp ~source_acc_set ~statement ~witness ~spec in
            let merged = prove_merge prev curr in
            merged )
  in
  take_from_sparse staged_ledger.ledger new_ledger ;
  let proof =
    match !(staged_ledger.proof) with
    | Some proof ->
        prove_merge proof stmt
    | None ->
        stmt
  in
  staged_ledger.proof := Some proof ;
  printf "prove_zeko_command done\n" ;
  ()

(* Only supports use_full_commitment for simplicity *)
let sign_cmd (keys : Keypair.t list) (cmd : Zkapp_command.t) : Zkapp_command.t =
  let keys = Option.value_exn !outer_fee_payer :: keys in
  let full_commitment =
    Zkapp_command.Transaction_commitment.create_complete
      (Zkapp_command.commitment cmd)
      ~memo_hash:(Signed_command_memo.hash cmd.memo)
      ~fee_payer_hash:
        (Zkapp_command.Digest.Account_update.create
           (Account_update.of_fee_payer cmd.fee_payer) )
  in
  let sign_raw (pk : Public_key.Compressed.t) msg =
    printf "Signing for %s\n" (Public_key.Compressed.to_base58_check pk) ;
    let rec go (keys : Keypair.t list) msg =
      match keys with
      | (kp : Keypair.t) :: keys ->
          if Public_key.Compressed.equal (Public_key.compress kp.public_key) pk
          then (
            printf "key found\n" ;
            Signature_lib.Schnorr.Chunked.sign
              ~signature_kind:Mina_signature_kind.Testnet kp.private_key
              (Random_oracle.Input.Chunked.field msg) )
          else (
            printf "not equal to %s\n"
              Public_key.(
                kp.public_key |> compress |> Compressed.to_base58_check) ;
            go keys msg )
      | [] ->
          failwithf "key not found: %s\n"
            (Public_key.Compressed.to_base58_check pk)
            ()
    in
    go keys msg
  in
  let rec sign_tree (tree : _ Zkapp_command.Call_forest.Tree.t) :
      _ Zkapp_command.Call_forest.Tree.t =
    { tree with
      account_update =
        Account_update.
          { tree.account_update with
            authorization =
              ( match tree.account_update.body.authorization_kind with
              | Signature ->
                  assert tree.account_update.body.use_full_commitment ;
                  Signature
                    (sign_raw tree.account_update.body.public_key
                       full_commitment )
              | _ ->
                  tree.account_update.authorization )
          }
    ; calls = sign_forest tree.calls
    }
  and sign_forest (forest : _ Zkapp_command.Call_forest.t) :
      _ Zkapp_command.Call_forest.t =
    List.map ~f:(fun tree -> { tree with elt = sign_tree tree.elt }) forest
  in

  { cmd with
    fee_payer =
      { cmd.fee_payer with
        authorization =
          ( if Public_key.Compressed.(equal empty cmd.fee_payer.body.public_key)
          then cmd.fee_payer.authorization
          else sign_raw cmd.fee_payer.body.public_key full_commitment )
      }
  ; account_updates = sign_forest cmd.account_updates
  }

let get_account' ledger (account_id : Account_id.t) =
  let (`Existed : [ `Added | `Existed ]), loc =
    Or_error.ok_exn
    @@ Ledger.get_or_create_account ledger account_id
         (Account.initialize account_id)
  in
  let account = Option.value_exn @@ Ledger.get ledger loc in
  account

let get_account ledger (kp : Keypair.t) =
  get_account' ledger (Account_id.of_public_key kp.public_key)

let inner_public_key =
  Snark_params.Tick.Inner_curve.(
    to_affine_exn @@ point_near_x @@ Field.of_string "123456789")
  |> Public_key.compress

let inner_account_id = Account_id.create inner_public_key Token_id.default

let inner_initial_account : Account.t =
  let app_state : _ Pickles_types.Vector.t =
    Field.
      [ Zkapp_account.Actions.empty_state_element
      ; zero
      ; zero
      ; zero
      ; zero
      ; zero
      ; zero
      ; zero
      ]
  in
  let verification_key =
    block_exn (fun () ->
        Zeko_circuits.Inner_rules.tag
        |> Zeko_circuits.Compile_simple.Verification_key.of_tag )
    |> Zeko_circuits.Compile_simple.Verification_key.to_pickles_lossy
  in
  { Account.empty with
    public_key = inner_public_key
  ; token_id = Token_id.default
  ; zkapp =
      Some
        { Zkapp_account.default with
          app_state
        ; verification_key =
            Some
              { data = verification_key
              ; hash = Verification_key_wire.digest_vk verification_key
              }
        }
  }

let pause_key : Zeko_circuits.Zeko_util.Even_PC.t =
  { public_key = point_of_string "8888182345" }

let da_key : Zeko_circuits.Zeko_util.Even_PC.t =
  { public_key = point_of_string "1238512311" }

let create_deploy_inner ~(zeko_kp : Keypair.t) () =
  let inner_ledger =
    Ledger.create_ephemeral ~depth:constraint_constants.ledger_depth ()
  in
  Ledger.create_new_account_exn inner_ledger inner_account_id
    inner_initial_account ;
  let deploy_update : Account_update.t =
    let ledger_hash = Ledger.merkle_root inner_ledger in
    let init =
      ( { pause_key
        ; ledger_hash
        ; inner_action_state =
            Zeko_circuits.Rollup_state.Inner_action_state.With_length
            .unsafe_value_of_fields
              ~state:
                ( Zkapp_account.Actions.empty_state_element
                |> Zeko_circuits.Rollup_state.Inner_action_state
                   .unsafe_value_of_field )
              ~length:Unsigned.UInt32.zero
        ; sequencer
        ; da_key
        ; acc_set = failwith "FIXME"
        ; paused = false
        }
        : Zeko_circuits.Rollup_state.Outer_state.t )
    in
    let (Typ typ) = Zeko_circuits.Rollup_state.Outer_state.typ in
    let fields, _ = typ.value_to_fields init in
    let app_state =
      Pickles_types.Vector.Vector_8.of_list_exn
        ( Array.to_list fields
        |> List.map ~f:(fun x -> Zkapp_basic.Set_or_keep.Set x) )
    in
    { body =
        { Account_update.Body.dummy with
          public_key = Public_key.compress zeko_kp.public_key
        ; update = { Account_update.Body.dummy.update with app_state }
        ; authorization_kind = Signature
        ; use_full_commitment = true
        ; implicit_account_creation_fee = false
        }
    ; authorization = Signature Signature.dummy
    }
  in
  let deploy_cmd : Zkapp_command.t =
    { fee_payer = pay_outer_fee ()
    ; account_updates =
        Zkapp_command.Call_forest.(
          accumulate_hashes'
          @@ of_account_updates
               ~account_update_depth:(fun _ -> 0)
               [ deploy_update ])
    ; memo = Signed_command_memo.empty
    }
  in
  send_zkapp deploy_cmd ; inner_ledger

let create_account pk =
  let q =
    object
      method query =
        String.substr_replace_all ~pattern:"\n" ~with_:" "
          {|
            mutation ($publicKey: PublicKey!) {
              createAccount(publicKey: $publicKey)
            } 
          |}

      method variables =
        `Assoc
          [ ( "publicKey"
            , `String (Signature_lib.Public_key.Compressed.to_base58_check pk)
            )
          ]
    end
  in
  send_gql q ; ()

let gen_init ~num_accounts () : For_tests.Init_ledger.t Quickcheck.Generator.t =
  let tbl = Public_key.Compressed.Hash_set.create () in
  let open Quickcheck.Generator in
  let open Let_syntax in
  let rec go acc n =
    if n = 0 then return (Array.of_list acc)
    else
      let%bind kp =
        filter Keypair.gen ~f:(fun kp ->
            not (Hash_set.mem tbl (Public_key.compress kp.public_key)) )
      in
      let amount = Int64.max_value in
      Hash_set.add tbl (Public_key.compress kp.public_key) ;
      go ((kp, amount) :: acc) (n - 1)
  in
  go [] num_accounts

(*
let submit_transfer ~(transfers : Zkapps_rollup.TR.t list ref) (kp : Keypair.t)
    (recipient : Keypair.t)
    ~(prover : Zkapps_rollup.TR.t -> call_forest_tree Deferred.t) amount
    ~(fee_payer : Account_update.Fee_payer.t) =
  let transfer : Zkapps_rollup.TR.t =
    { recipient = Public_key.compress recipient.public_key; amount }
  in
  let transfer_update = wait (fun () -> prover transfer) in
  let transferrer_update : Account_update.t =
    { body =
        { Account_update.Body.dummy with
          public_key = Public_key.compress kp.public_key
        ; balance_change = Amount.Signed.(negate @@ of_unsigned amount)
        ; use_full_commitment = true
        ; authorization_kind = Signature
        }
    ; authorization = Signature Signature.dummy
    }
  in
  let transfer_cmd : Zkapp_command.t =
    { fee_payer
    ; account_updates =
        Zkapp_command.Call_forest.(
          cons_tree transfer_update @@ accumulate_hashes'
          @@ of_account_updates
               ~account_update_depth:(fun _ -> 0)
               [ transferrer_update ])
    ; memo = Signed_command_memo.empty
    }
    |> sign_cmd [ kp ]
  in
  transfers := transfer :: !transfers ;
  transfer_cmd

let submit_deposit ~(zeko_kp : Keypair.t)
    ~(deposits : Zkapps_rollup.TR.t list ref) (kp : Keypair.t)
    (recipient : Keypair.t) amount =
  let module Z = (val force zeko_module) in
  let outer_public_key = Public_key.compress zeko_kp.public_key in
  submit_transfer ~transfers:deposits ~fee_payer:(pay_outer_fee ())
    ~prover:(fun deposit -> Z.Outer.submit_deposit ~outer_public_key ~deposit)
    kp recipient amount

let submit_withdrawal ~(withdrawals : Zkapps_rollup.TR.t list ref)
    (kp : Keypair.t) (recipient : Keypair.t) amount =
  let module Z = (val force zeko_module) in
  submit_transfer ~transfers:withdrawals ~fee_payer:inner_fee_payer
    ~prover:(fun withdrawal -> Z.Inner.submit_withdrawal ~withdrawal)
    kp recipient amount

(* takes list of transfers from new to old, skips all transfers before pointer *)
let skip_transfers_before (transfers : Zkapps_rollup.TR.t list)
    (pointer : Field.t) =
  match
    List.fold_right transfers
      ~init:(`Hashing Zkapp_account.Actions.empty_state_element)
      ~f:(fun transfer -> function
      | `Hashing hash ->
          if Field.equal hash pointer then `Accumulating [ transfer ]
          else
            `Hashing
              (Zkapp_account.Actions.push_events hash
                 (Zkapps_rollup.TR.to_actions transfer) )
      | `Accumulating transfers ->
          `Accumulating (transfer :: transfers) )
  with
  | `Hashing _ ->
      []
  | `Accumulating transfers_after_pointer ->
      transfers_after_pointer

let process_transfer ~is_new ~(ledger : Ledger.t)
    ~(transfers : Zkapps_rollup.TR.t list ref) ~(recipient : Keypair.t) amount
    ~(account_id : Account_id.t) ~(fee_payer : Account_update.Fee_payer.t)
    ~(prover :
          is_new:bool
       -> pointer:Field.t
       -> after:Zkapps_rollup.TR.t list
       -> before:Zkapps_rollup.TR.t list
       -> Zkapps_rollup.TR.t
       -> ([ `Pointer of Field.t ] * call_forest) Deferred.t ) : Zkapp_command.t
    =
  let token_id = Account_id.derive_token_id ~owner:account_id in
  let pointer =
    if is_new then Zkapp_account.Actions.empty_state_element
    else
      let (`Transfers_processed pointer) =
        get_account' ledger
          (Account_id.create
             (Public_key.compress recipient.public_key)
             token_id )
        |> Zkapps_rollup.read_token_account_state
      in
      pointer
  in
  let transfers_after_pointer = skip_transfers_before !transfers pointer in
  let (`After (before, after)
        : [ `After of Zkapps_rollup.TR.t list * Zkapps_rollup.TR.t list
          | `Before of Zkapps_rollup.TR.t list ] ) =
    List.fold_right transfers_after_pointer ~init:(`Before [])
      ~f:(fun transfer -> function
      | `Before before ->
          if
            Public_key.(
              Compressed.equal
                (compress recipient.public_key)
                transfer.recipient)
            && Currency.Amount.equal amount transfer.amount
          then `After (before, [])
          else `Before (transfer :: before)
      | `After (before, after) ->
          `After (before, transfer :: after) )
  in
  let `Pointer _, process_updates =
    wait (fun () ->
        prover ~is_new ~pointer ~after ~before
          { amount; recipient = Public_key.compress recipient.public_key } )
  in
  let process_cmd : Zkapp_command.t =
    { fee_payer
    ; account_updates = process_updates
    ; memo = Signed_command_memo.empty
    }
    |> sign_cmd [ recipient ]
  in
  process_cmd

let process_deposit ~is_new ~staged_ledger ~deposits ~recipient amount =
  let module Z = (val force zeko_module) in
  let cmd =
    process_transfer ~is_new ~ledger:staged_ledger.ledger ~transfers:deposits
      ~recipient amount ~account_id:Zkapps_rollup.inner_account_id
      ~fee_payer:inner_fee_payer
      ~prover:(fun ~is_new ~pointer ~after ~before deposit ->
        Z.Inner.process_deposit ~is_new ~pointer ~after ~before ~deposit )
  in
  prove_zeko_command staged_ledger cmd ;
  ()

let process_withdrawal ~is_new ~outer_ledger ~withdrawals ~recipient
    ~(zeko_kp : Keypair.t) amount =
  let module Z = (val force zeko_module) in
  let cmd =
    process_transfer ~is_new ~ledger:outer_ledger ~transfers:withdrawals
      ~recipient amount
      ~account_id:(Account_id.of_public_key zeko_kp.public_key)
      ~fee_payer:(pay_outer_fee ())
      ~prover:(fun ~is_new ~pointer ~after ~before withdrawal ->
        Z.Outer.process_withdrawal
          ~outer_public_key:(Public_key.compress zeko_kp.public_key)
          ~is_new ~pointer ~after ~before ~withdrawal )
  in
  send_zkapp cmd ; ()

let commit ~zeko_kp ~outer_ledger ~inner_ledger ~(staged_ledger : staged_ledger)
    ~(deposits : Zkapps_rollup.TR.t list ref) =
  let module Z = (val force zeko_module) in
  let (`All_deposits old_all_deposits) =
    get_account' inner_ledger Zkapps_rollup.inner_account_id
    |> Zkapps_rollup.read_inner_state
  in
  let (all_deposits :: _) =
    (Option.value_exn (get_account outer_ledger zeko_kp).zkapp).action_state
  in
  let inner_step_update = wait (fun () -> Z.Inner.step ~all_deposits) in
  let inner_step_cmd : Zkapp_command.t =
    { fee_payer = inner_fee_payer
    ; account_updates =
        Zkapp_command.Call_forest.(cons_tree inner_step_update [])
    ; memo = Signed_command_memo.empty
    }
  in
  prove_zeko_command staged_ledger inner_step_cmd ;
  let new_deposits = skip_transfers_before !deposits old_all_deposits in
  let outer_step_update =
    wait (fun () ->
        Z.Outer.step
          (Option.value_exn !(staged_ledger.proof))
          ~outer_public_key:(Public_key.compress zeko_kp.public_key)
          ~new_deposits ~unprocessed_deposits:[]
          ~new_inner_ledger:(to_sparse staged_ledger.ledger inner_step_cmd)
          ~old_inner_ledger:(to_sparse inner_ledger inner_step_cmd) )
  in
  let outer_step_cmd : Zkapp_command.t =
    { fee_payer = pay_outer_fee ()
    ; account_updates =
        Zkapp_command.Call_forest.(cons_tree outer_step_update [])
    ; memo = Signed_command_memo.empty
    }
  in
  send_zkapp outer_step_cmd ;
  Ledger.commit staged_ledger.ledger ;
  staged_ledger.proof := None ;
  ()
*)

(* FIXME: make shrinkable by making it pure
   and taking all random inputs as a spec.
*)
let main () =
  let zeko_kp = Signature_lib.Keypair.create () in
  let init_ledger = Quickcheck.random_value (gen_init ~num_accounts ()) in
  outer_fee_payer := Some (Quickcheck.random_value Keypair.gen) ;
  let staged_ledger =
    { ledger = create_deploy_inner ~zeko_kp ()
    ; proof = ref None
    ; account_set = Account_set.initial
    }
  in
  let deposits = ref [] in
  let withdrawals = ref [] in
  for i = 1 to 2 do
    printf "i == %i\n" i ;
    let is_new = Int.equal i 1 in
    Array.iter init_ledger ~f:(fun (kp, amount) ->
        let amount =
          Amount.of_uint64
          @@ Unsigned_extended.UInt64.(div (of_int64 amount) (of_int 4))
        in
        send_zkapp (submit_deposit ~zeko_kp ~deposits kp kp amount) ;
        () ) ;
    commit ~zeko_kp ~outer_ledger ~inner_ledger ~staged_ledger ~deposits ;
    Array.iter init_ledger ~f:(fun (kp, amount) ->
        let amount =
          Amount.of_uint64
          @@ Unsigned_extended.UInt64.(div (of_int64 amount) (of_int 4))
        in
        process_deposit ~is_new ~staged_ledger ~deposits ~recipient:kp amount ;
        () ) ;
    commit ~zeko_kp ~outer_ledger ~inner_ledger ~staged_ledger ~deposits ;
    Array.iter init_ledger ~f:(fun (kp, amount) ->
        let amount =
          Amount.of_uint64
          @@ Unsigned_extended.UInt64.(div (of_int64 amount) (of_int 8))
        in
        prove_zeko_command staged_ledger
          (submit_withdrawal ~withdrawals kp kp amount) ;
        () ) ;
    commit ~zeko_kp ~outer_ledger ~inner_ledger ~staged_ledger ~deposits ;
    Array.iter init_ledger ~f:(fun (kp, amount) ->
        let amount =
          Amount.of_uint64
          @@ Unsigned_extended.UInt64.(div (of_int64 amount) (of_int 8))
        in
        process_withdrawal ~zeko_kp ~is_new ~outer_ledger ~withdrawals
          ~recipient:kp amount ;
        () ) ;
    ()
  done

let () = main ()
*)
