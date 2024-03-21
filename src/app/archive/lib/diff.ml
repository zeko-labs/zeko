open Mina_block
open Core_kernel
open Mina_base
module Breadcrumb = Transition_frontier.Breadcrumb

(* TODO: We should be able to fully deserialize and serialize via json *)

(* these types are serialized for communication between the daemon and archive node,
   which should be compiled with the same sources

   the RPC is itself not versioned, so these types do not need to be versioned
*)

module Transition_frontier = struct
  type t =
    | Breadcrumb_added of
        { block :
            Mina_block.Stable.Latest.t
            State_hash.With_state_hashes.Stable.Latest.t
              (* ledger index, account *)
        ; accounts_accessed : (int * Mina_base.Account.Stable.Latest.t) list
        ; accounts_created :
            (Account_id.Stable.Latest.t * Currency.Fee.Stable.Latest.t) list
        ; tokens_used :
            (Token_id.Stable.Latest.t * Account_id.Stable.Latest.t option) list
        ; sender_receipt_chains_from_parent_ledger :
            (Account_id.Stable.Latest.t * Receipt.Chain_hash.Stable.Latest.t)
            list
        }
    | Root_transitioned of
        Transition_frontier.Diff.Root_transition.Lite.Stable.Latest.t
    | Bootstrap of { lost_blocks : State_hash.Stable.Latest.t list }
  [@@deriving bin_io_unversioned]
end

module Transaction_pool = struct
  type t =
    { added : User_command.Stable.Latest.t list
    ; removed : User_command.Stable.Latest.t list
    }
  [@@deriving bin_io_unversioned]
end

type t = Transition_frontier of Transition_frontier.t
[@@deriving bin_io_unversioned]

module Builder = struct
  let breadcrumb_added ~(precomputed_values : Precomputed_values.t) ~logger
      breadcrumb =
    let validated_block = Breadcrumb.validated_transition breadcrumb in
    let commands = Mina_block.Validated.valid_commands validated_block in
    let staged_ledger = Breadcrumb.staged_ledger breadcrumb in
    let ledger = Staged_ledger.ledger staged_ledger in
    let sender_receipt_chains_from_parent_ledger =
      let senders =
        commands
        |> List.map ~f:(fun { data; _ } ->
               User_command.(fee_payer (forget_check data)) )
        |> Account_id.Set.of_list
      in
      Set.to_list senders
      |> List.map ~f:(fun sender ->
             Option.value_exn
               (let open Option.Let_syntax in
               let%bind ledger_location =
                 Mina_ledger.Ledger.location_of_account ledger sender
               in
               let%map { receipt_chain_hash; _ } =
                 Mina_ledger.Ledger.get ledger ledger_location
               in
               (sender, receipt_chain_hash)) )
    in
    let block_with_hash = Mina_block.Validated.forget validated_block in
    let block = With_hash.data block_with_hash in
    let state_hash = (With_hash.hash block_with_hash).state_hash in
    let start = Time.now () in
    let account_ids_accessed = Mina_block.account_ids_accessed block in
    let accounts_accessed =
      List.filter_map account_ids_accessed ~f:(fun (acct_id, status) ->
          match status with
          | `Not_accessed ->
              None
          | `Accessed ->
              (* an accessed account may not be in the ledger *)
              let%bind.Option index =
                Option.try_with (fun () ->
                    Mina_ledger.Ledger.index_of_account_exn ledger acct_id )
              in
              let account = Mina_ledger.Ledger.get_at_index_exn ledger index in
              Some (index, account) )
    in
    let accounts_accessed_time = Time.now () in
    [%log debug]
      "Archive data generation for $state_hash: accounts-accessed took $time ms"
      ~metadata:
        [ ("state_hash", Mina_base.State_hash.to_yojson state_hash)
        ; ( "time"
          , `Float (Time.Span.to_ms (Time.diff accounts_accessed_time start)) )
        ] ;
    let accounts_created =
      let account_creation_fee =
        precomputed_values.constraint_constants.account_creation_fee
      in
      let previous_block_state_hash =
        Mina_block.header block |> Header.protocol_state
        |> Mina_state.Protocol_state.previous_state_hash
      in
      List.map
        (Staged_ledger.latest_block_accounts_created staged_ledger
           ~previous_block_state_hash ) ~f:(fun acct_id ->
          (acct_id, account_creation_fee) )
    in
    let tokens_used =
      let unique_tokens =
        (* a token is used regardless of txn status *)
        List.map account_ids_accessed ~f:(fun (acct_id, _status) ->
            Account_id.token_id acct_id )
        |> List.dedup_and_sort ~compare:Token_id.compare
      in
      List.map unique_tokens ~f:(fun token_id ->
          let owner = Mina_ledger.Ledger.token_owner ledger token_id in
          (token_id, owner) )
    in
    let account_created_time = Time.now () in
    [%log debug]
      "Archive data generation for $state_hash: accounts-created took $time ms"
      ~metadata:
        [ ("state_hash", Mina_base.State_hash.to_yojson state_hash)
        ; ( "time"
          , `Float
              (Time.Span.to_ms
                 (Time.diff account_created_time accounts_accessed_time) ) )
        ] ;
    Transition_frontier.Breadcrumb_added
      { block = block_with_hash
      ; accounts_accessed
      ; accounts_created
      ; tokens_used
      ; sender_receipt_chains_from_parent_ledger
      }

  let transaction_added
      ~(constraint_constants : Genesis_constants.Constraint_constants.t)
      ~accounts_created ~new_state_hash ~protocol_state ~ledger ~txn =
    let advance_protocol_state ~protocol_state ~new_state_hash
        ~increase_blockchain_length =
      let old_protocol_state = protocol_state in
      let genesis_state_hash =
        Mina_state.Protocol_state.genesis_state_hash old_protocol_state
      in
      let previous_state_hash, blockchain_state =
        let blockchain_state =
          Mina_state.Protocol_state.blockchain_state old_protocol_state
        in
        ( Staged_ledger_hash.ledger_hash blockchain_state.staged_ledger_hash
        , { blockchain_state with
            staged_ledger_hash =
              Staged_ledger_hash.(
                of_aux_ledger_and_coinbase_hash
                  (aux_hash blockchain_state.staged_ledger_hash)
                  new_state_hash
                  ( Or_error.ok_exn
                  @@ Pending_coinbase.create
                       ~depth:constraint_constants.pending_coinbase_depth () ))
          } )
      in
      let consensus_state =
        let consensus_state =
          Mina_state.Protocol_state.consensus_state old_protocol_state
        in
        Consensus.Proof_of_stake.Exported.Consensus_state.Unsafe.dummy_advance
          consensus_state ~increase_epoch_count:false
          ~increase_blockchain_length
          ~new_global_slot_since_genesis:
            Mina_numbers.Global_slot_since_genesis.zero
      in

      let constants = Mina_state.Protocol_state.constants old_protocol_state in
      Mina_state.Protocol_state.create_value ~previous_state_hash
        ~genesis_state_hash ~blockchain_state ~consensus_state ~constants
    in
    let protocol_state =
      advance_protocol_state ~protocol_state ~new_state_hash
        ~increase_blockchain_length:true
    in

    let With_status.{ data = txn; status = txn_status } = txn in
    let account_ids_accessed =
      Mina_transaction.Transaction.account_access_statuses txn txn_status
      |> List.dedup_and_sort
           ~compare:[%compare: Account_id.t * [ `Accessed | `Not_accessed ]]
    in
    let accounts_accessed =
      List.filter_map account_ids_accessed ~f:(fun (acct_id, status) ->
          match status with
          | `Not_accessed ->
              None
          | `Accessed ->
              (* an accessed account may not be in the ledger *)
              let%bind.Option index =
                Option.try_with (fun () ->
                    Mina_ledger.Ledger.index_of_account_exn ledger acct_id )
              in
              let account = Mina_ledger.Ledger.get_at_index_exn ledger index in
              Some (index, account) )
    in
    let tokens_used =
      let unique_tokens =
        (* a token is used regardless of txn status *)
        List.map account_ids_accessed ~f:(fun (acct_id, _status) ->
            Account_id.token_id acct_id )
        |> List.dedup_and_sort ~compare:Token_id.compare
      in
      List.map unique_tokens ~f:(fun token_id ->
          let owner = Mina_ledger.Ledger.token_owner ledger token_id in
          (token_id, owner) )
    in
    let command =
      match txn with
      | Command c ->
          c
      | _ ->
          failwith "Zeko transaction has to be command"
    in
    let sender_receipt_chain_from_parent_ledger =
      let sender = User_command.(fee_payer command) in
      Option.value_exn
        (let open Option.Let_syntax in
        let%bind ledger_location =
          Mina_ledger.Ledger.location_of_account ledger sender
        in
        let%map { receipt_chain_hash; _ } =
          Mina_ledger.Ledger.get ledger ledger_location
        in
        (sender, receipt_chain_hash))
    in
    let header =
      Mina_block.Header.create ~protocol_state
        ~protocol_state_proof:Proof.blockchain_dummy
        ~delta_block_chain_proof:(State_hash.dummy, []) ()
    in
    let body =
      Mina_block.Body.create
      @@ Staged_ledger_diff.
           { diff =
               ( { completed_works = []
                 ; commands =
                     [ With_status.{ data = command; status = txn_status } ]
                 ; coinbase = Zero
                 ; internal_command_statuses = [ Transaction_status.Applied ]
                 }
               , None )
           }
    in
    let block =
      With_hash.of_data ~hash_data:(fun _ ->
          State_hash.State_hashes.
            { state_body_hash = None; state_hash = new_state_hash } )
      @@ Mina_block.create ~header ~body
    in
    ( protocol_state
    , Transition_frontier.Breadcrumb_added
        { block
        ; accounts_created =
            List.map accounts_created ~f:(fun acct_id ->
                (acct_id, constraint_constants.account_creation_fee) )
        ; accounts_accessed
        ; tokens_used
        ; sender_receipt_chains_from_parent_ledger =
            [ sender_receipt_chain_from_parent_ledger ]
        } )
end
