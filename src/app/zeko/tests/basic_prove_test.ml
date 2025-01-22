open Core_kernel
open Signature_lib
open Snark_params.Tick
open Zeko_circuits

let ase_with_length, ase_with_length_proof =
  let open struct
    let trans0, proof0 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.With_length.leaf
        ( [ Field.one ]
        , { action_state = Field.of_string "6"
          ; length = Unsigned.UInt32.of_string "42"
          } )

    let trans1, proof1 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.With_length.leaf_option ([ Field.of_string "2" ], trans0.source)

    let trans2, proof2 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.With_length.merge
        { left = trans0
        ; left_proof = proof0
        ; right = trans1
        ; right_proof = proof1
        }

    let trans3, proof3 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.With_length.extend ([ Field.of_string "99" ], (trans2, proof2))

    let trans4, proof4 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.With_length.extend_option ([ Field.of_string "99" ], (trans3, proof3))
  end in
  (trans4, proof4)

let ase_without_length =
  let open struct
    let trans0, proof0 =
      Promise.block_on_async_exn
      @@ fun () -> Ase.Without_length.leaf ([ Field.one ], Field.of_string "6")

    let trans1, proof1 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.Without_length.leaf_option ([ Field.of_string "2" ], trans0.source)

    let trans2, proof2 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.Without_length.merge
        { left = trans0
        ; left_proof = proof0
        ; right = trans1
        ; right_proof = proof1
        }

    let trans3, proof3 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.Without_length.extend ([ Field.of_string "99" ], (trans2, proof2))

    let trans4, proof4 =
      Promise.block_on_async_exn
      @@ fun () ->
      Ase.Without_length.extend_option
        ([ Field.of_string "99" ], (trans3, proof3))
  end in
  (trans4, proof4)

let point_of_string_even s : Zeko_util.Even_PC.t =
  let x, _ =
    Snark_params.Tick.Inner_curve.(
      to_affine_exn @@ point_near_x @@ Snark_params.Tick.Field.of_string s)
  in
  { public_key = x }

let point_of_string s =
  Snark_params.Tick.Inner_curve.(
    to_affine_exn @@ point_near_x @@ Snark_params.Tick.Field.of_string s)
  |> Public_key.compress

let _inner_stmt, _inner_proof =
  let open struct
    let Compile_simple.[ sync; action ] = Inner_rules.provers

    let ase_with_length : Rule_inner_sync.Ase_inst.t =
      Rule_inner_sync.Ase_inst.make ~proof_source:ase_with_length.source
        ~proof_target:ase_with_length.target ~proof:ase_with_length_proof
        ase_with_length.source
        [ Field.of_string "418923791273" ]

    let sync_witness : Rule_inner_sync.Witness.t =
      { public_key = point_of_string "8184848488"
      ; vk_hash = Field.of_string "4819274123"
      ; ase = ase_with_length
      }

    let _stmt, _proof =
      Promise.block_on_async_exn @@ fun () -> sync sync_witness

    let action_witness : Rule_inner_action_witness.Witness.t =
      { public_key = point_of_string "8184848488"
      ; vk_hash = Field.of_string "4819274123"
      ; witness = { aux = Field.zero; children = [] }
      }

    let stmt, proof =
      Promise.block_on_async_exn @@ fun () -> action action_witness
  end in
  (stmt, proof)

let _outer =
  let open struct
    let Compile_simple.[ _commit; action; pause ] = Outer_rules.provers

    let pause_witness : Rule_pause.Witness.t =
      { public_key = point_of_string_even "1238881"
      ; vk_hash = Field.of_string "19944541415"
      ; pause_key = point_of_string_even "1511111121"
      }

    let _stmt, _proof =
      Promise.block_on_async_exn @@ fun () -> pause pause_witness

    let action_witness : Rule_action_witness.Witness.t =
      { public_key = point_of_string "41889111"
      ; vk_hash = Field.of_string "188188181"
      ; witness =
          { aux = Field.zero
          ; children = []
          ; slot_range =
              { lower = Zeko_util.Slot.zero; upper = Zeko_util.Slot.max_value }
          }
      }

    let _stmt, _proof =
      Promise.block_on_async_exn @@ fun () -> action action_witness

    let Compile_simple.[ prove_both ] = Rule_commit.Verify_both_ases.provers

    let ase_outer =
      let stmt, proof = ase_without_length in
      Rule_commit.Ase_outer_inst.make ~proof_source:stmt.source
        ~proof_target:stmt.target ~proof stmt.source
        [ Field.of_string "849812849581123" ]

    let ase_inner =
      let stmt, proof = (ase_with_length, ase_with_length_proof) in
      Rule_commit.Ase_inner_inst.make ~proof_source:stmt.source
        ~proof_target:stmt.target ~proof stmt.source
        [ Field.of_string "849812849581123" ]

    let verify_both_ases_stmt, verify_both_ases_proof =
      Promise.block_on_async_exn @@ fun () -> prove_both (ase_outer, ase_inner)

    let _verify_both_ases =
      Rule_commit.Verify_both_ases.make_unchecked ~proof:verify_both_ases_proof
        verify_both_ases_stmt

    let old_inner_acc =
      { Mina_base.Account.empty with
        public_key = Outer_rules.Inputs.inner_public_key
      ; zkapp =
          Some
            { Mina_base.Zkapp_account.default with
              app_state =
                [ ase_with_length.source.action_state
                ; Unsigned.UInt32.to_string ase_with_length.source.length
                  |> Field.of_string
                ; Field.zero
                ; Field.zero
                ; Field.zero
                ; Field.zero
                ; Field.zero
                ; Field.zero
                ]
            ; action_state =
                (let f = ase_with_length.source.action_state in
                 [ f; f; f; f; f ] )
            }
      }

    let new_inner_acc =
      { Mina_base.Account.empty with
        public_key = Outer_rules.Inputs.inner_public_key
      ; zkapp =
          Some
            { Mina_base.Zkapp_account.default with
              app_state =
                [ ase_with_length.target.action_state
                ; Unsigned.UInt32.to_string ase_with_length.target.length
                  |> Field.of_string
                ; Field.zero
                ; Field.zero
                ; Field.zero
                ; Field.zero
                ; Field.zero
                ; Field.zero
                ]
            ; action_state =
                (let f = ase_with_length.target.action_state in
                 [ f; f; f; f; f ] )
            }
      }

    let da_sk = Quickcheck.random_value Private_key.gen

    let da_key = Public_key.of_private_key_exn da_sk |> Public_key.compress

    let () = assert (not da_key.is_odd)

    let Compile_simple.
          [ _signed_command
          ; _zkapp_single
          ; _zkapp_double
          ; _zkapp_proved
          ; _merge
          ] =
      Zeko_transaction_snark.provers

    let genesis_constants = Genesis_constants.Compiled.genesis_constants

    let constraint_constants = Genesis_constants.Compiled.constraint_constants

    let consensus_constants =
      Consensus.Constants.create ~constraint_constants
        ~protocol_constants:genesis_constants.protocol

    (** Dummy state body, network preconditions are disabled anyway *)
    let _dummy_state_body =
      let compile_time_genesis =
        Mina_state.Genesis_protocol_state.t
          ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
          ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
          ~constraint_constants ~consensus_constants
          ~genesis_body_reference:Staged_ledger_diff.genesis_body_reference
      in
      Mina_state.Protocol_state.body compile_time_genesis.data

    let () = assert (Int.(constraint_constants.ledger_depth = 32))

    let intermediate_ledger_hashes =
      let base = force Mina_base.Account.empty_digest in
      let rec go = function
        | 31, hash ->
            [ (31, hash) ]
        | height, hash ->
            (height, hash)
            :: go (height + 1, Mina_base.Ledger_hash.merge ~height hash hash)
      in
      go (0, base)

    let () = assert (List.length intermediate_ledger_hashes = 32)

    let implied_root (account : Mina_base.Account.t) : field =
      let init = Mina_base.Account.digest account in
      List.fold intermediate_ledger_hashes ~init
        ~f:(fun acc (height, right_side) ->
          Mina_base.Ledger_hash.merge ~height acc right_side )

    let source_ledger = implied_root old_inner_acc

    let _target_ledger = implied_root new_inner_acc

    let _connecting_ledger = source_ledger

    let inner_account_id =
      Mina_base.Account_id.create old_inner_acc.public_key
        old_inner_acc.token_id

    let sparse_source_ledger : Mina_ledger.Sparse_ledger.t =
      Mina_ledger.Sparse_ledger.(
        add_path (empty ~depth:32 ())
          (List.map ~f:(fun (_, h) -> `Right h) intermediate_ledger_hashes)
          inner_account_id old_inner_acc)

    let first_account_update : Mina_base.Account_update.t =
      { body =
          { Mina_base.Account_update.Body.dummy with
            public_key = old_inner_acc.public_key
          ; token_id = old_inner_acc.token_id
          ; authorization_kind = None_given
          }
      ; authorization = None_given
      }

    module Inputs = struct
      open Mina_base
      open Currency
      open Signature_lib

      open struct
        module Global_slot_since_genesis =
          Mina_numbers.Global_slot_since_genesis
        module L = Mina_ledger.Sparse_ledger
      end

      open L

      let with_label ~label:_ f = f ()

      let value_if b ~then_ ~else_ = if b then then_ else else_

      module Global_state = struct
        type t =
          { first_pass_ledger : L.t
          ; second_pass_ledger : L.t
          ; fee_excess : Amount.Signed.t
          ; supply_increase : Amount.Signed.t
          ; block_global_slot : Global_slot_since_genesis.t
          }

        let first_pass_ledger { first_pass_ledger; _ } =
          L.create_masked first_pass_ledger

        let set_first_pass_ledger ~should_update t ledger =
          if should_update then L.apply_mask t.first_pass_ledger ~masked:ledger ;
          t

        let second_pass_ledger { second_pass_ledger; _ } =
          L.create_masked second_pass_ledger

        let set_second_pass_ledger ~should_update t ledger =
          if should_update then L.apply_mask t.second_pass_ledger ~masked:ledger ;
          t

        let fee_excess { fee_excess; _ } = fee_excess

        let set_fee_excess t fee_excess = { t with fee_excess }

        let supply_increase { supply_increase; _ } = supply_increase

        let set_supply_increase t supply_increase = { t with supply_increase }

        let block_global_slot { block_global_slot; _ } = block_global_slot
      end

      module Field = struct
        type t = Snark_params.Tick.Field.t

        let if_ = value_if

        let equal = Snark_params.Tick.Field.equal
      end

      module Bool = struct
        type t = bool

        module Assert = struct
          let is_true ~pos b =
            try assert b
            with Assert_failure _ ->
              let file, line, col, _ecol = pos in
              raise (Assert_failure (file, line, col))

          let any ~pos bs = List.exists ~f:Fn.id bs |> is_true ~pos
        end

        let if_ = value_if

        let true_ = true

        let false_ = false

        let equal = Bool.equal

        let not = not

        let ( ||| ) = ( || )

        let ( &&& ) = ( && )

        let display b ~label = sprintf "%s: %b" label b

        let all = List.for_all ~f:Fn.id

        type failure_status = Transaction_status.Failure.t option

        type failure_status_tbl = Transaction_status.Failure.Collection.t

        let is_empty t = List.join t |> List.is_empty

        let assert_with_failure_status_tbl ~pos b failure_status_tbl =
          let file, line, col, ecol = pos in
          if (not b) && not (is_empty failure_status_tbl) then
            (* Raise a more useful error message if we have a failure
               description. *)
            let failure_msg =
              Yojson.Safe.to_string
              @@ Transaction_status.Failure.Collection.Display.to_yojson
              @@ Transaction_status.Failure.Collection.to_display
                   failure_status_tbl
            in
            Error.raise @@ Error.of_string
            @@ sprintf "File %S, line %d, characters %d-%d: %s" file line col
                 ecol failure_msg
          else
            try assert b
            with Assert_failure _ -> raise (Assert_failure (file, line, col))
      end

      module Account_id = struct
        include Account_id

        let if_ = value_if
      end

      module Ledger = struct
        open L

        type t = L.t

        let if_ = value_if

        let empty = L.empty

        type inclusion_proof = [ `Existing of L.location | `New ]

        let get_with_location ledger account_id =
          match location_of_account ledger account_id with
          | Some location -> (
              match get ledger location with
              | Some account ->
                  Ok (`Existing location, account)
              | None ->
                  failwith "Ledger location with no account" )
          | None ->
              Ok (`New, Account.create account_id Balance.zero)

        let set_with_location ledger location account =
          match location with
          | `Existing location ->
              Ok (L.set ledger location account)
          | `New ->
              L.create_new_account ledger (Account.identifier account) account

        let get_account p l =
          let loc, acct =
            Or_error.ok_exn (get_with_location l (Account_update.account_id p))
          in
          (acct, loc)

        let set_account l (a, loc) =
          Or_error.ok_exn (set_with_location l loc a) ;
          l

        let check_inclusion _ledger (_account, _loc) = ()

        let check_account public_key token_id
            ((account, loc) : Account.t * inclusion_proof) =
          assert (Public_key.Compressed.equal public_key account.public_key) ;
          assert (Token_id.equal token_id account.token_id) ;
          match loc with `Existing _ -> `Is_new false | `New -> `Is_new true
      end

      module Transaction_commitment = struct
        type t = Field.t

        let empty = Zkapp_command.Transaction_commitment.empty

        let commitment ~account_updates =
          let account_updates_hash =
            Mina_base.Zkapp_command.Call_forest.hash account_updates
          in
          Zkapp_command.Transaction_commitment.create ~account_updates_hash

        let full_commitment ~account_update ~memo_hash ~commitment =
          (* when called from Zkapp_command_logic.apply, the account_update is the fee payer *)
          let fee_payer_hash =
            Zkapp_command.Digest.Account_update.create account_update
          in
          Zkapp_command.Transaction_commitment.create_complete commitment
            ~memo_hash ~fee_payer_hash

        let if_ = value_if
      end

      module Index = struct
        type t = Mina_numbers.Index.t

        let zero, succ = Mina_numbers.Index.(zero, succ)

        let if_ = value_if
      end

      module Public_key = struct
        type t = Public_key.Compressed.t

        let if_ = value_if
      end

      module Controller = struct
        type t = Permissions.Auth_required.t

        let if_ = value_if

        let check ~proof_verifies ~signature_verifies perm =
          (* Invariant: We either have a proof, a signature, or neither. *)
          assert (not (proof_verifies && signature_verifies)) ;
          let tag =
            if proof_verifies then Control.Tag.Proof
            else if signature_verifies then Control.Tag.Signature
            else Control.Tag.None_given
          in
          Permissions.Auth_required.check perm tag

        let verification_key_perm_fallback_to_signature_with_older_version =
          Permissions.Auth_required
          .verification_key_perm_fallback_to_signature_with_older_version
      end

      module Txn_version = struct
        type t = Mina_numbers.Txn_version.t

        let if_ = value_if

        let equal_to_current = Mina_numbers.Txn_version.equal_to_current

        let older_than_current = Mina_numbers.Txn_version.older_than_current
      end

      let value_if b ~then_ ~else_ = if b then then_ else else_

      module Global_slot_since_genesis = struct
        include Mina_numbers.Global_slot_since_genesis

        let if_ = value_if
      end

      module Global_slot_span = struct
        include Mina_numbers.Global_slot_span

        let if_ = value_if
      end

      module Nonce = struct
        type t = Account.Nonce.t

        let if_ = value_if

        let succ = Account.Nonce.succ
      end

      module Receipt_chain_hash = struct
        type t = Receipt.Chain_hash.t

        module Elt = struct
          type t = Receipt.Zkapp_command_elt.t

          let of_transaction_commitment tc =
            Receipt.Zkapp_command_elt.Zkapp_command_commitment tc
        end

        let cons_zkapp_command_commitment =
          Receipt.Chain_hash.cons_zkapp_command_commitment

        let if_ = value_if
      end

      module State_hash = struct
        include State_hash

        let if_ = value_if
      end

      module Timing = struct
        type t = Account_update.Update.Timing_info.t option

        let if_ = value_if

        let vesting_period (t : t) =
          match t with
          | Some t ->
              t.vesting_period
          | None ->
              (Account_timing.to_record Untimed).vesting_period
      end

      module Balance = struct
        include Balance

        let if_ = value_if
      end

      module Verification_key = struct
        type t = (Side_loaded_verification_key.t, Field.t) With_hash.t option

        let if_ = value_if
      end

      module Verification_key_hash = struct
        type t = Field.t option

        let equal vk1 vk2 = Option.equal Field.equal vk1 vk2
      end

      module Actions = struct
        type t = Zkapp_account.Actions.t

        let is_empty = List.is_empty

        let push_events = Account_update.Actions.push_events
      end

      module Zkapp_uri = struct
        type t = Bounded_types.String.t

        let if_ = value_if
      end

      module Token_symbol = struct
        type t = Account.Token_symbol.t

        let if_ = value_if
      end

      module Account = struct
        include Account

        module Permissions = struct
          let access : t -> Controller.t = fun a -> a.permissions.access

          let edit_state : t -> Controller.t = fun a -> a.permissions.edit_state

          let send : t -> Controller.t = fun a -> a.permissions.send

          let receive : t -> Controller.t = fun a -> a.permissions.receive

          let set_delegate : t -> Controller.t =
           fun a -> a.permissions.set_delegate

          let set_permissions : t -> Controller.t =
           fun a -> a.permissions.set_permissions

          let set_verification_key_auth : t -> Controller.t =
           fun a -> fst a.permissions.set_verification_key

          let set_verification_key_txn_version : t -> Txn_version.t =
           fun a -> snd a.permissions.set_verification_key

          let set_zkapp_uri : t -> Controller.t =
           fun a -> a.permissions.set_zkapp_uri

          let edit_action_state : t -> Controller.t =
           fun a -> a.permissions.edit_action_state

          let set_token_symbol : t -> Controller.t =
           fun a -> a.permissions.set_token_symbol

          let increment_nonce : t -> Controller.t =
           fun a -> a.permissions.increment_nonce

          let set_voting_for : t -> Controller.t =
           fun a -> a.permissions.set_voting_for

          let set_timing : t -> Controller.t = fun a -> a.permissions.set_timing

          type t = Permissions.t

          let if_ = value_if
        end

        type timing = Account_update.Update.Timing_info.t option

        let timing (a : t) : timing =
          Account_update.Update.Timing_info.of_account_timing a.timing

        let set_timing (a : t) (timing : timing) : t =
          { a with
            timing =
              Option.value_map ~default:Account_timing.Untimed
                ~f:Account_update.Update.Timing_info.to_account_timing timing
          }

        let is_timed (a : t) =
          match a.timing with
          | Account_timing.Untimed ->
              false
          | Timed _ ->
              true

        let set_token_id (a : t) (id : Token_id.t) : t =
          { a with token_id = id }

        let balance (a : t) : Balance.t = a.balance

        let set_balance (balance : Balance.t) (a : t) : t = { a with balance }

        let check_timing ~txn_global_slot account =
          let validate_timing_with_min_balance' ~(account : Account.t)
              ~txn_amount ~txn_global_slot =
            let open Account.Timing.Poly in
            match account.timing with
            | Untimed -> (
                (* no time restrictions *)
                match Balance.(account.balance - txn_amount) with
                | None ->
                    ( `Insufficient_balance true
                    , Untimed
                    , `Min_balance Balance.zero )
                | _ ->
                    (`Invalid_timing false, Untimed, `Min_balance Balance.zero)
                )
            | Timed
                { initial_minimum_balance
                ; cliff_time
                ; cliff_amount
                ; vesting_period
                ; vesting_increment
                } ->
                let invalid_balance, invalid_timing, curr_min_balance =
                  let account_balance = account.balance in
                  match Balance.(account_balance - txn_amount) with
                  | None ->
                      (* NB: The [initial_minimum_balance] here is the incorrect value,
                         but:
                         * we don't use it anywhere in this error case; and
                         * we don't want to waste time computing it if it will be unused.
                      *)
                      (true, false, initial_minimum_balance)
                  | Some proposed_new_balance ->
                      let curr_min_balance =
                        Account.min_balance_at_slot ~global_slot:txn_global_slot
                          ~cliff_time ~cliff_amount ~vesting_period
                          ~vesting_increment ~initial_minimum_balance
                      in
                      if Balance.(proposed_new_balance < curr_min_balance) then
                        (false, true, curr_min_balance)
                      else (false, false, curr_min_balance)
                in
                (* once the calculated minimum balance becomes zero, the account becomes untimed *)
                let possibly_error =
                  if invalid_balance then `Insufficient_balance invalid_balance
                  else `Invalid_timing invalid_timing
                in
                if Balance.(curr_min_balance > zero) then
                  (possibly_error, account.timing, `Min_balance curr_min_balance)
                else (possibly_error, Untimed, `Min_balance Balance.zero)
          in
          let invalid_timing, timing, _ =
            validate_timing_with_min_balance' ~txn_amount:Amount.zero
              ~txn_global_slot ~account
          in
          ( invalid_timing
          , Account_update.Update.Timing_info.of_account_timing timing )

        let receipt_chain_hash (a : t) : Receipt.Chain_hash.t =
          a.receipt_chain_hash

        let set_receipt_chain_hash (a : t) hash =
          { a with receipt_chain_hash = hash }

        let make_zkapp (a : t) =
          let zkapp =
            match a.zkapp with
            | None ->
                Some Zkapp_account.default
            | Some _ as zkapp ->
                zkapp
          in
          { a with zkapp }

        let unmake_zkapp (a : t) : t =
          let zkapp =
            match a.zkapp with
            | None ->
                None
            | Some zkapp ->
                if Zkapp_account.(equal default zkapp) then None else Some zkapp
          in
          { a with zkapp }

        let get_zkapp (a : t) = Option.value_exn a.zkapp

        let set_zkapp (a : t) ~f : t = { a with zkapp = Option.map a.zkapp ~f }

        let proved_state (a : t) = (get_zkapp a).proved_state

        let set_proved_state proved_state (a : t) =
          set_zkapp a ~f:(fun zkapp -> { zkapp with proved_state })

        let app_state (a : t) = (get_zkapp a).app_state

        let set_app_state app_state (a : t) =
          set_zkapp a ~f:(fun zkapp -> { zkapp with app_state })

        let register_verification_key (_ : t) = ()

        let verification_key (a : t) = (get_zkapp a).verification_key

        let set_verification_key verification_key (a : t) =
          set_zkapp a ~f:(fun zkapp -> { zkapp with verification_key })

        let verification_key_hash (a : t) =
          match a.zkapp with
          | None ->
              None
          | Some zkapp ->
              Option.map zkapp.verification_key ~f:With_hash.hash

        let last_action_slot (a : t) = (get_zkapp a).last_action_slot

        let set_last_action_slot last_action_slot (a : t) =
          set_zkapp a ~f:(fun zkapp -> { zkapp with last_action_slot })

        let action_state (a : t) = (get_zkapp a).action_state

        let set_action_state action_state (a : t) =
          set_zkapp a ~f:(fun zkapp -> { zkapp with action_state })

        let zkapp_uri (a : t) =
          Option.value_map a.zkapp ~default:"" ~f:(fun zkapp -> zkapp.zkapp_uri)

        let set_zkapp_uri zkapp_uri (a : t) : t =
          { a with
            zkapp =
              Option.map a.zkapp ~f:(fun zkapp -> { zkapp with zkapp_uri })
          }

        let token_symbol (a : t) = a.token_symbol

        let set_token_symbol token_symbol (a : t) = { a with token_symbol }

        let public_key (a : t) = a.public_key

        let set_public_key public_key (a : t) = { a with public_key }

        let delegate (a : t) = Account.delegate_opt a.delegate

        let set_delegate delegate (a : t) =
          let delegate =
            if Signature_lib.Public_key.Compressed.(equal empty) delegate then
              None
            else Some delegate
          in
          { a with delegate }

        let nonce (a : t) = a.nonce

        let set_nonce nonce (a : t) = { a with nonce }

        let voting_for (a : t) = a.voting_for

        let set_voting_for voting_for (a : t) = { a with voting_for }

        let permissions (a : t) = a.permissions

        let set_permissions permissions (a : t) = { a with permissions }
      end

      module Amount = struct
        open Currency.Amount

        type unsigned = t

        type t = unsigned

        let if_ = value_if

        module Signed = struct
          include Signed

          let if_ = value_if

          (* Correctness of these functions hinges on the fact that zero is
             only ever expressed as {sgn = Pos; magnitude = zero}. Sadly, this
             is not guaranteed by the module's signature, as it's internal
             structure is exposed. Create function never produces this unwanted
             value, but the type's internal structure is still exposed, so it's
             possible theoretically to obtain it.

             For the moment, however, there is some consolation in the fact that
             addition never produces negative zero, even if it was one of its
             arguments. For that reason the risk of this function misbehaving is
             minimal and can probably be safely ignored.

             ZEKO NOTE: ^ not true, you can create negative zero with `negate zero`
             we fix it in the zkapp command logic where it's called
          *)
          let is_non_neg (t : t) = Sgn.equal t.sgn Pos

          let is_neg (t : t) = Sgn.equal t.sgn Neg
        end

        let zero = zero

        let equal = equal

        let add_flagged = add_flagged

        let add_signed_flagged (x1 : t) (x2 : Signed.t) :
            t * [ `Overflow of bool ] =
          let y, `Overflow b = Signed.(add_flagged (of_unsigned x1) x2) in
          match y.sgn with
          | Pos ->
              (y.magnitude, `Overflow b)
          | Neg ->
              (* We want to capture the accurate value so that this will match
                 with the values in the snarked logic.
              *)
              let magnitude =
                Amount.to_uint64 y.magnitude
                |> Unsigned.UInt64.(mul (sub zero one))
                |> Amount.of_uint64
              in
              (magnitude, `Overflow true)

        let of_constant_fee = of_fee
      end

      module Token_id = struct
        include Token_id

        let if_ = value_if
      end

      module Protocol_state_precondition = struct
        include Zkapp_precondition.Protocol_state
      end

      module Valid_while_precondition = struct
        include Zkapp_precondition.Valid_while
      end

      module Account_update = struct
        include Account_update

        module Account_precondition = struct
          include Account_update.Account_precondition

          let nonce (t : Account_update.t) = nonce t.body.preconditions.account
        end

        type 'a or_ignore = 'a Zkapp_basic.Or_ignore.t

        type call_forest = Zkapp_call_forest.t

        type transaction_commitment = Transaction_commitment.t

        let may_use_parents_own_token (p : t) =
          May_use_token.parents_own_token p.body.may_use_token

        let may_use_token_inherited_from_parent (p : t) =
          May_use_token.inherit_from_parent p.body.may_use_token

        let check_authorization ~will_succeed:_ ~commitment:_ ~calls:_
            (account_update : t) =
          (* The transaction's validity should already have been checked before
             this point.
          *)
          match account_update.authorization with
          | Signature _ ->
              (`Proof_verifies false, `Signature_verifies true)
          | Proof _ ->
              (`Proof_verifies true, `Signature_verifies false)
          | None_given ->
              (`Proof_verifies false, `Signature_verifies false)

        let is_proved (account_update : t) =
          match account_update.body.authorization_kind with
          | Proof _ ->
              true
          | Signature | None_given ->
              false

        let is_signed (account_update : t) =
          match account_update.body.authorization_kind with
          | Signature ->
              true
          | Proof _ | None_given ->
              false

        let verification_key_hash (p : t) =
          match p.body.authorization_kind with
          | Proof vk_hash ->
              Some vk_hash
          | None_given | Signature ->
              None

        module Update = struct
          open Zkapp_basic

          type 'a set_or_keep = 'a Zkapp_basic.Set_or_keep.t

          let timing (account_update : t) : Account.timing set_or_keep =
            Set_or_keep.map ~f:Option.some account_update.body.update.timing

          let app_state (account_update : t) =
            account_update.body.update.app_state

          let verification_key (account_update : t) =
            Zkapp_basic.Set_or_keep.map ~f:Option.some
              account_update.body.update.verification_key

          let actions (account_update : t) = account_update.body.actions

          let zkapp_uri (account_update : t) =
            account_update.body.update.zkapp_uri

          let token_symbol (account_update : t) =
            account_update.body.update.token_symbol

          let delegate (account_update : t) =
            account_update.body.update.delegate

          let voting_for (account_update : t) =
            account_update.body.update.voting_for

          let permissions (account_update : t) =
            account_update.body.update.permissions
        end
      end

      module Set_or_keep = struct
        include Zkapp_basic.Set_or_keep

        let set_or_keep ~if_:_ t x = set_or_keep t x
      end

      module Opt = struct
        type 'a t = 'a option

        let is_some = Option.is_some

        let map = Option.map

        let or_default ~if_ x ~default =
          if_ (is_some x) ~then_:(Option.value ~default x) ~else_:default

        let or_exn x = Option.value_exn x
      end

      module Stack (Elt : sig
        type t
      end) =
      struct
        type t = Elt.t list

        let if_ = value_if

        let empty () = []

        let is_empty = List.is_empty

        let pop_exn : t -> Elt.t * t = function
          | [] ->
              failwith "pop_exn"
          | x :: xs ->
              (x, xs)

        let pop : t -> (Elt.t * t) option = function
          | x :: xs ->
              Some (x, xs)
          | _ ->
              None

        let push x ~onto : t = x :: onto
      end

      module Call_forest = Zkapp_call_forest

      module Stack_frame = struct
        include Stack_frame

        type t = value

        let if_ = Zkapp_command.value_if

        let make = Stack_frame.make
      end

      module Call_stack = Stack (Stack_frame)

      module Local_state = struct
        type t =
          ( Stack_frame.t
          , Call_stack.t
          , Amount.Signed.t
          , Ledger.t
          , Bool.t
          , Transaction_commitment.t
          , Index.t
          , Bool.failure_status_tbl )
          Mina_transaction_logic.Zkapp_command_logic.Local_state.t

        let add_check (t : t) failure b =
          let failure_status_tbl =
            match t.failure_status_tbl with
            | hd :: tl when not b ->
                (failure :: hd) :: tl
            | old_failure_status_tbl ->
                old_failure_status_tbl
          in
          { t with failure_status_tbl; success = t.success && b }

        let update_failure_status_tbl (t : t) failure_status b =
          match failure_status with
          | None ->
              { t with success = t.success && b }
          | Some failure ->
              add_check t failure b

        let add_new_failure_status_bucket (t : t) =
          { t with failure_status_tbl = [] :: t.failure_status_tbl }
      end

      module Nonce_precondition = struct
        let is_constant =
          Zkapp_precondition.Numeric.is_constant
            Zkapp_precondition.Numeric.Tc.nonce
      end
    end

    module Logic = Mina_transaction_logic.Zkapp_command_logic.Make (Inputs)

    let initial_state : Inputs.Global_state.t * Inputs.Local_state.t =
      ( { first_pass_ledger = ref sparse_source_ledger
        ; second_pass_ledger =
            (* We stub out the second_pass_ledger initially, and then poke the
               correct value in place after the first pass is finished.
            *)
            ref (Mina_ledger.Sparse_ledger.empty ~depth:0 ())
        ; fee_excess = Currency.Amount.Signed.zero
        ; supply_increase = Currency.Amount.Signed.zero
        ; block_global_slot = Mina_numbers.Global_slot_since_genesis.zero
        }
      , { stack_frame = Mina_base.Stack_frame.empty
        ; call_stack = []
        ; transaction_commitment = Inputs.Transaction_commitment.empty
        ; full_transaction_commitment = Inputs.Transaction_commitment.empty
        ; excess = Currency.Amount.(Signed.of_unsigned zero)
        ; supply_increase = Currency.Amount.(Signed.of_unsigned zero)
        ; ledger = ref (Mina_ledger.Sparse_ledger.empty ~depth:0 ())
        ; success = true
        ; account_update_index = Inputs.Index.zero
        ; failure_status_tbl = []
        ; will_succeed = true
        } )

    module Env = struct
      open Mina_base
      open Inputs

      type t =
        < account_update : Account_update.t
        ; zkapp_command : Zkapp_command.t
        ; account : Account.t
        ; ledger : Ledger.t
        ; amount : Amount.t
        ; signed_amount : Amount.Signed.t
        ; bool : Bool.t
        ; token_id : Token_id.t
        ; global_state : Global_state.t
        ; inclusion_proof : [ `Existing of int | `New ]
        ; local_state :
            ( Stack_frame.t
            , Call_stack.t
            , Amount.Signed.t
            , Mina_ledger.Sparse_ledger.t ref
            , bool
            , Transaction_commitment.t
            , Index.t
            , Transaction_status.Failure.Collection.t )
            Mina_transaction_logic.Zkapp_command_logic.Local_state.t
        ; protocol_state_precondition : Zkapp_precondition.Protocol_state.t
        ; valid_while_precondition : Zkapp_precondition.Valid_while.t
        ; transaction_commitment : Transaction_commitment.t
        ; full_transaction_commitment : Transaction_commitment.t
        ; field : Snark_params.Tick.Field.t
        ; failure : Transaction_status.Failure.t option >

      let perform (type r)
          (eff : (r, t) Mina_transaction_logic.Zkapp_command_logic.Eff.t) : r =
        match eff with
        | Check_valid_while_precondition _ ->
            true
        | Check_protocol_state_precondition _ ->
            true
        | Check_account_precondition
            (account_update, account, new_account, local_state) ->
            let local_state = ref local_state in
            let check failure b =
              local_state := Inputs.Local_state.add_check !local_state failure b
            in
            Zkapp_precondition.Account.check ~new_account ~check
              account_update.body.preconditions.account account ;
            !local_state
        | Init_account { account_update = _; account = a } ->
            a
        | Get_shift_action_state _ ->
            false
    end

    let _global_state, _local_state =
      Logic.start ~constraint_constants
        { account_updates =
            Mina_base.Zkapp_command.Call_forest.of_account_updates
              ~account_update_depth:(fun _ -> 0)
              [ first_account_update; first_account_update ]
            |> Mina_base.Zkapp_command.Call_forest.accumulate_hashes'
        ; memo_hash = Field.zero
        ; will_succeed = true
        }
        Env.{ perform } initial_state
    (*

    let zkapp_single : Zeko_transaction_snark.Zkapp_single_unproved_input.t =
      { shift_action_state = false
      ; base =
          { source_ledger
          ; target_ledger
          ; connecting_ledger
          ; source_local_state =
              { ledger = source_ledger
              ; stack_frame
              ; call_stack
              ; transaction_commitment
              ; full_transaction_commitment
              ; excess
              ; account_update_index
              }
          ; target_local_state =
              { ledger = target_ledger
              ; stack_frame
              ; call_stack
              ; transaction_commitment
              ; full_transaction_commitment
              ; excess
              ; account_update_index
              }
          ; fee_excess = Currency.Fee.Signed.zero
          ; supply_decrease = Currency.Amount.zero
          ; witness =
              { txn_snark_witness =
                  { global_first_pass_ledger = sparse_source_ledger
                  ; global_second_pass_ledger = sparse_source_ledger
                  ; local_state_init =
                      (* Most of these fields aren't used. *)
                      { stack_frame = Mina_base.Stack_frame.empty
                      ; call_stack = []
                      ; transaction_commitment = Field.zero
                      ; full_transaction_commitment = Field.zero
                      ; excess = Currency.Amount.Signed.zero
                      ; supply_increase = Currency.Amount.Signed.zero
                      ; ledger = sparse_source_ledger
                      ; success = true
                      ; account_update_index = Unsigned.UInt32.zero
                      ; failure_status_tbl = []
                      ; will_succeed = true
                      }
                  ; start_zkapp_command = []
                  ; state_body = dummy_state_body
                  ; init_stack = Mina_base.Pending_coinbase.Stack.empty
                  ; block_global_slot =
                      Mina_numbers.Global_slot_since_genesis.zero
                  }
              ; update_acc_set_witness =
                  { get_account_set_x
                  ; get_account_set_z
                  ; get_account_set_x_path
                  ; get_account_set_y_path
                  }
              }
          ; sequencer = point_of_string_even "1991991991"
          ; source_acc_set
          }
      }

    let _stmt, _proof =
      Promise.block_on_async_exn @@ fun () -> zkapp_single zkapp_single_witness
      *)

    (*
    let zkapp_proved_witness :
        Zeko_transaction_snark.Zkapp_single_proved_input.t =
      { zkapp_vk =
          Promise.block_on_async_exn (fun () ->
              Inner_rules.tag |> Compile_simple.Verification_key.of_tag )
          |> Compile_simple.Verification_key.to_pickles_lossy
      ; zkapp_proof = inner_proof
      ; shift_action_state = false
      ; base =
          { source_ledger
          ; target_ledger
          ; connecting_ledger
          ; source_local_state =
              { ledger = source_ledger
              ; stack_frame
              ; call_stack
              ; transaction_commitment
              ; full_transaction_commitment
              ; excess
              ; account_update_index
              }
          ; target_local_state =
              { ledger = target_ledger
              ; stack_frame
              ; call_stack
              ; transaction_commitment
              ; full_transaction_commitment
              ; excess
              ; account_update_index
              }
          ; fee_excess = Currency.Fee.Signed.zero
          ; supply_decrease = Currency.Amount.zero
          ; witness =
              { txn_snark_witness =
                  { global_first_pass_ledger = sparse_source_ledger
                  ; global_second_pass_ledger = sparse_source_ledger
                  ; local_state_init =
                      (* Most of these fields aren't used. *)
                      { stack_frame = Mina_base.Stack_frame.empty
                      ; call_stack = []
                      ; transaction_commitment = Field.zero
                      ; full_transaction_commitment = Field.zero
                      ; excess = Currency.Amount.Signed.zero
                      ; supply_increase = Currency.Amount.Signed.zero
                      ; ledger = sparse_source_ledger
                      ; success = true
                      ; account_update_index = Unsigned.UInt32.zero
                      ; failure_status_tbl = []
                      ; will_succeed = true
                      }
                  ; start_zkapp_command = []
                  ; state_body = dummy_state_body
                  ; init_stack = Mina_base.Pending_coinbase.Stack.empty
                  ; block_global_slot =
                      Mina_numbers.Global_slot_since_genesis.zero
                  }
              ; update_acc_set_witness =
                  { get_account_set_x
                  ; get_account_set_z
                  ; get_account_set_x_path
                  ; get_account_set_y_path
                  }
              }
          ; sequencer = point_of_string_even "1991991991"
          ; source_acc_set
          }
      }

    let _stmt, _proof =
      Promise.block_on_async_exn @@ fun () -> zkapp_proved zkapp_proved_witness

    let inner_acc_path =
      List.map
        ~f:(fun (_, hash) : Outer_rules.Rule_commit_inst.PathElt.t ->
          { right_side = hash } )
        intermediate_ledger_hashes

    let commit_witness : Outer_rules.Rule_commit_inst.Witness.t =
      { txn_snark
      ; public_key = point_of_string "28811121"
      ; vk_hash = Field.of_string "31923919199191"
      ; verify_both_ases
      ; old_inner_acc
      ; old_inner_acc_path = inner_acc_path
      ; new_inner_acc
      ; new_inner_acc_path = inner_acc_path
      ; da_signature
      ; da_key = { public_key = da_key.x }
      }

    let _stmt, _proof =
      Promise.block_on_async_exn @@ fun () -> commit commit_witness
      *)
  end in
  ()
