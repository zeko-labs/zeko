module Util' = Util
module Js = Js_of_ocaml.Js
open Core_kernel
open Mina_base
open Async_kernel
module L = Mina_ledger.Ledger
module S = Signature_lib
module Util = Util'

type t =
  < ledger : L.t Js.readonly_prop
  ; sk : Js.js_string Js.t Js.readonly_prop
  ; slot : int Js.prop
  ; name : Js.js_string Js.t Js.readonly_prop
  ; txnSnark : Transaction_snark.t Deferred.t option Js.prop >
  Js.t

type user_command =
  < signature : Js.js_string Js.t Js.readonly_prop
  ; fromBase58 : Js.js_string Js.t Js.readonly_prop
  ; toBase58 : Js.js_string Js.t Js.readonly_prop
  ; amount : Js.js_string Js.t Js.readonly_prop
  ; fee : Js.js_string Js.t Js.readonly_prop
  ; validUntil : Js.js_string Js.t Js.readonly_prop
  ; nonce : Js.js_string Js.t Js.readonly_prop
  ; memo : Js.js_string Js.t Js.readonly_prop
  ; accountCreationFee : Js.js_string Js.t Js.readonly_prop >
  Js.t

module Step = Pickles.Impls.Step

let rollup =
  object%js
    method compile =
      let constraint_constants =
        Genesis_constants.Constraint_constants.compiled
      in
      let genesis_constants = Genesis_constants.compiled in
      let consensus_constants =
        Consensus.Constants.create ~constraint_constants
          ~protocol_constants:genesis_constants.protocol
      in

      let module T = Transaction_snark.Make (struct
        let constraint_constants = constraint_constants

        let proof_level = Genesis_constants.Proof_level.Full
      end) in
      let module M = Zkapps_rollup.Make (struct
        let tag = T.tag
      end) in
      let module Proof = M.Proof in
      let gen_keys () =
        let kp = S.Keypair.create () in
        (S.Public_key.compress kp.public_key, kp.private_key)
      in
      let token_id = Mina_base.Token_id.default in
      object%js
        val vk = Pickles.Side_loaded.Verification_key.of_compiled T.tag

        (*
        Initialise a rollup with the given name.
        Returns an empty ledger, the keys for the account, and the account update.
        *)
        method createZkapp name
            (genesis_accounts :
              < publicKey : Js.js_string Js.t Js.prop
              ; balance : Js.js_string Js.t Js.prop >
              Js.t
              Js.js_array
              Js.t ) =
          let pk, sk = gen_keys () in
          let l = L.create ~depth:constraint_constants.ledger_depth () in
          let () =
            Array.iter (Js.to_array genesis_accounts) ~f:(fun account ->
                let pk =
                  S.Public_key.Compressed.of_base58_check_exn
                  @@ Js.to_string account##.publicKey
                in
                let account_id =
                  Account_id.of_public_key (S.Public_key.decompress_exn pk)
                in
                let balance =
                  Unsigned.UInt64.of_string @@ Js.to_string account##.balance
                in
                let account =
                  Account.create account_id (Currency.Balance.of_uint64 balance)
                in
                L.create_new_account_exn l account_id account )
          in
          let acup : Account_update.t =
            let body =
              { Account_update.Body.dummy with
                public_key = pk
              ; token_id
              ; implicit_account_creation_fee = false
              ; update =
                  { Account_update.Update.dummy with
                    app_state =
                      [ Set
                          (Mina_base.Frozen_ledger_hash0.to_field
                             (L.merkle_root l) )
                      ; Keep
                      ; Keep
                      ; Keep
                      ; Keep
                      ; Keep
                      ; Keep
                      ; Keep
                      ]
                  ; verification_key =
                      Set
                        { data = M.vk
                        ; hash = Mina_base.Zkapp_account.digest_vk M.vk
                        }
                  ; permissions =
                      Set
                        { edit_state = Proof
                        ; send = Either
                        ; receive = None
                        ; set_delegate = Proof
                        ; set_permissions = Proof
                        ; set_verification_key = Proof
                        ; set_zkapp_uri = Proof
                        ; edit_action_state = Proof
                        ; set_token_symbol = Proof
                        ; increment_nonce = Proof
                        ; set_voting_for = Proof
                        ; set_timing = Proof
                        ; access = Either
                        }
                  }
              ; use_full_commitment = true
              ; preconditions =
                  { Account_update.Preconditions.network =
                      Mina_base.Zkapp_precondition.Protocol_state.accept
                  ; account = Accept
                  ; valid_while = Ignore
                  }
              ; authorization_kind = Signature
              }
            in
            { body; authorization = None_given }
          in
          object%js
            val accountUpdate : Js.js_string Js.t =
              Js.string @@ Yojson.Safe.to_string
              @@ Account_update.Graphql_repr.to_json
              @@ Account_update.to_graphql_repr acup ~call_depth:0

            val rollup : t =
              object%js
                val ledger = l

                val sk = Js.string (S.Private_key.to_base58_check sk)

                val name = name

                val mutable slot = 0

                val mutable txnSnark = None
              end
          end

        (*
        Applies a user command to a ledger immediately with the specified public key.
        *)
        method applyUserCommand (rollup : t) (user_command_js : user_command) =
          let from =
            S.Public_key.Compressed.of_base58_check_exn
            @@ Js.to_string user_command_js##.fromBase58
          in
          let to_ =
            S.Public_key.Compressed.of_base58_check_exn
            @@ Js.to_string user_command_js##.toBase58
          in
          let payload =
            Mina_base.Signed_command.Payload.create
              ~fee:(Currency.Fee.of_string @@ Js.to_string user_command_js##.fee)
              ~fee_payer_pk:from
              ~valid_until:
                ( Option.some @@ Mina_numbers.Global_slot_since_genesis.of_string
                @@ Js.to_string user_command_js##.validUntil )
              ~nonce:
                ( Mina_numbers.Global_slot_legacy.of_string
                @@ Js.to_string user_command_js##.nonce )
              ~memo:
                ( Mina_base.Signed_command_memo.of_base58_check_exn
                @@ Js.to_string user_command_js##.memo )
              ~body:
                (Payment
                   { receiver_pk = to_
                   ; amount =
                       Currency.Amount.of_string
                       @@ Js.to_string user_command_js##.amount
                   } )
          in
          (* todo: change to custom salt when it will be available in snarkyjs and auro wallet *)
          (* let signature_kind =
               Mina_signature_kind.Other_network (Js.to_string rollup##.name)
             in *)
          let signature_kind = Mina_signature_kind.Testnet in
          let user_command =
            match
              Mina_base.Signed_command.create_with_signature_checked
                ~signature_kind
                ( Mina_base.Signature.of_base58_check_exn
                @@ Js.to_string user_command_js##.signature )
                from payload
            with
            | Some x ->
                x
            | None ->
                raise
                  (Failure "apply_user_command failed to create user command")
          in
          let l : L.t = rollup##.ledger in
          let global_slot =
            Mina_numbers.Global_slot_since_genesis.of_int rollup##.slot
          in
          (* let () = rollup##.slot := rollup##.slot + 1 in *)
          let sk =
            S.Private_key.of_base58_check_exn @@ Js.to_string rollup##.sk
          in
          let pk = S.Public_key.(compress @@ of_private_key_exn sk) in
          let source = L.merkle_root l in
          let state_body =
            (* FIXME: Use the correct values *)
            let compile_time_genesis =
              Mina_state.Genesis_protocol_state.t
                ~genesis_ledger:Genesis_ledger.(Packed.t for_unit_tests)
                ~genesis_epoch_data:Consensus.Genesis_epoch_data.for_unit_tests
                ~constraint_constants ~consensus_constants
                ~genesis_body_reference:
                  Staged_ledger_diff.genesis_body_reference
            in
            Mina_state.Protocol_state.body compile_time_genesis.data
          in
          let sparse_ledger =
            Mina_ledger.Sparse_ledger.of_ledger_subset_exn l
              (Signed_command.accounts_referenced
                 (Signed_command.forget_check user_command) )
          in
          let txn =
            Mina_transaction.Transaction.Command
              (User_command.Signed_command
                 (Signed_command.forget_check user_command) )
          in
          let txn_applied =
            match
              Result.( >>= )
                (L.apply_transaction_first_pass ~constraint_constants
                   ~global_slot
                   ~txn_state_view:
                     (Mina_state.Protocol_state.Body.view state_body)
                   l txn )
                (L.apply_transaction_second_pass l)
            with
            | Ok txn_applied ->
                txn_applied
            | Error e ->
                Error.raise e
          in
          let target = L.merkle_root l in
          let prev = rollup##.txnSnark in
          rollup##.txnSnark :=
            Some
              (Async_kernel.schedule' (fun () ->
                   let init_stack = Mina_base.Pending_coinbase.Stack.empty in
                   let user_command_in_block =
                     { Transaction_protocol_state.Poly.transaction =
                         user_command
                     ; block_data = state_body
                     ; global_slot
                     }
                   in
                   let state_body_hash =
                     Mina_state.Protocol_state.Body.hash state_body
                   in
                   let pc : Transaction_snark.Pending_coinbase_stack_state.t =
                     (* No coinbase to add to the stack. *)
                     let stack_with_state =
                       Pending_coinbase.Stack.push_state state_body_hash
                         global_slot init_stack
                     in
                     { source = stack_with_state; target = stack_with_state }
                   in
                   let user_command_supply_increase =
                     match
                       L.Transaction_applied.supply_increase txn_applied
                     with
                     | Ok x ->
                         x
                     | Error e ->
                         Error.raise e
                   in
                   let sok_digest =
                     Sok_message.create ~fee:Currency.Fee.zero ~prover:pk
                     |> Sok_message.digest
                   in
                   let statement =
                     Transaction_snark.Statement.Poly.with_empty_local_state
                       ~source_first_pass_ledger:source
                       ~target_first_pass_ledger:target
                       ~source_second_pass_ledger:target
                       ~target_second_pass_ledger:target
                       ~connecting_ledger_left:target
                       ~connecting_ledger_right:target ~sok_digest
                       ~fee_excess:
                         (Or_error.ok_exn
                            (Mina_transaction.Transaction.fee_excess txn) )
                       ~supply_increase:user_command_supply_increase
                       ~pending_coinbase_stack_state:pc
                   in
                   let handler =
                     unstage @@ Mina_ledger.Sparse_ledger.handler sparse_ledger
                   in
                   let%bind next =
                     T.of_user_command ~init_stack ~statement
                       user_command_in_block handler
                   in
                   match prev with
                   | Some prev -> (
                       let%bind prev = prev in
                       let%bind merged = T.merge prev next ~sok_digest in
                       match merged with
                       | Ok merged' ->
                           return merged'
                       | Error e ->
                           Error.raise e )
                   | None ->
                       return next ) )

        method commit (rollup : t) (k : Js.js_string Js.t -> unit) =
          let sk =
            S.Private_key.of_base58_check_exn @@ Js.to_string rollup##.sk
          in
          let pk =
            S.Public_key.compress @@ S.Public_key.of_private_key_exn sk
          in
          match rollup##.txnSnark with
          | None ->
              raise (Failure "nothing to commit")
          | Some txn ->
              let%bind txn = txn in
              let%bind acup, () =
                M.step
                  { public_key = pk
                  ; token_id
                  ; may_use_token = Inherit_from_parent
                  ; txn
                  }
                  ()
              in
              return
              @@ k
                   ( Js.string @@ Yojson.Safe.to_string
                   @@ Zkapp_command.account_updates_to_json
                   @@ Zkapp_command.Call_forest.(cons_tree acup []) )

        method getAccount (rollup : t) (pk : S.Public_key.Compressed.t)
            (token : Step.field) =
          let module Let_syntax = Option in
          let acid = Account_id.create pk (Token_id.of_field token) in
          let%bind loc = L.location_of_account rollup##.ledger acid in
          let%map ac = L.get rollup##.ledger loc in
          let deriver = Account.deriver @@ Fields_derivers_zkapps.o () in
          ac
          |> Fields_derivers_zkapps.to_json deriver
          |> Yojson.Safe.to_string |> Js.string |> Util.json_parse
      end
  end
