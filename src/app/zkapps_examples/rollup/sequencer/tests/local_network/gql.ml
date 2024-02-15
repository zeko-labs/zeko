open Core
open Async
open Graphql_async
open Mina_base
open Mina_transaction
module Ledger = Mina_ledger.Ledger
open Signature_lib
open Currency
module Schema = Graphql_wrapper.Make (Schema)

type t =
  { db : Ledger.Db.t
  ; commands : (string, User_command.t * Transaction_status.t) Hashtbl.t
  ; genesis_timestamp : int64
  }

let current_slot t =
  let slot =
    Int64.(
      Block_time.(
        ((to_int64 @@ of_time @@ Core.Time.now ()) - t.genesis_timestamp)
        / 1000L / 60L / 3L))
  in
  Mina_numbers.Global_slot_since_genesis.of_int @@ Int64.to_int_exn slot

let constraint_constants = Genesis_constants.Constraint_constants.compiled

let genesis_constants = Genesis_constants.compiled

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

module Types = struct
  open Schema

  include struct
    open Graphql_lib.Scalars

    let public_key = PublicKey.typ ()

    let token_id = TokenId.typ ()

    let balance = Balance.typ ()

    let amount = Amount.typ ()

    let fee = Fee.typ ()

    let global_slot_since_genesis = GlobalSlotSinceGenesis.typ ()

    let global_slot_span = GlobalSlotSpan.typ ()

    let length = Length.typ ()

    let state_hash = StateHash.typ ()

    let account_nonce = AccountNonce.typ ()

    let chain_hash = ChainHash.typ ()

    let transaction_hash = TransactionHash.typ ()

    let transaction_id = TransactionId.typ ()
  end

  let sync_status : ('context, Sync_status.t option) typ =
    enum "SyncStatus" ~doc:"Sync status of daemon"
      ~values:
        (List.map Sync_status.all ~f:(fun status ->
             enum_value
               (String.map ~f:Char.uppercase @@ Sync_status.to_string status)
               ~value:status ) )

  module DaemonStatus = struct
    type t = { chain_id : string }

    let t : ('context, t option) typ =
      obj "DaemonStatus" ~fields:(fun _ ->
          [ field "chainId" ~typ:(non_null string)
              ~args:Arg.[]
              ~resolve:(fun _ v -> v.chain_id)
          ] )
  end

  let merkle_path_element :
      (_, [ `Left of Zkapp_basic.F.t | `Right of Zkapp_basic.F.t ] option) typ =
    let field_elem = Mina_base_unix.Graphql_scalars.FieldElem.typ () in
    obj "MerklePathElement" ~fields:(fun _ ->
        [ field "left" ~typ:field_elem
            ~args:Arg.[]
            ~resolve:(fun _ x ->
              match x with `Left h -> Some h | `Right _ -> None )
        ; field "right" ~typ:field_elem
            ~args:Arg.[]
            ~resolve:(fun _ x ->
              match x with `Left _ -> None | `Right h -> Some h )
        ] )

  let account_timing : (t, Account_timing.t option) typ =
    obj "AccountTiming" ~fields:(fun _ ->
        [ field "initialMinimumBalance" ~typ:balance
            ~doc:"The initial minimum balance for a time-locked account"
            ~args:Arg.[]
            ~resolve:(fun _ timing ->
              match timing with
              | Account_timing.Untimed ->
                  None
              | Timed timing_info ->
                  Some timing_info.initial_minimum_balance )
        ; field "cliffTime" ~typ:global_slot_since_genesis
            ~doc:"The cliff time for a time-locked account"
            ~args:Arg.[]
            ~resolve:(fun _ timing ->
              match timing with
              | Account_timing.Untimed ->
                  None
              | Timed timing_info ->
                  Some timing_info.cliff_time )
        ; field "cliffAmount" ~typ:amount
            ~doc:"The cliff amount for a time-locked account"
            ~args:Arg.[]
            ~resolve:(fun _ timing ->
              match timing with
              | Account_timing.Untimed ->
                  None
              | Timed timing_info ->
                  Some timing_info.cliff_amount )
        ; field "vestingPeriod" ~typ:global_slot_span
            ~doc:"The vesting period for a time-locked account"
            ~args:Arg.[]
            ~resolve:(fun _ timing ->
              match timing with
              | Account_timing.Untimed ->
                  None
              | Timed timing_info ->
                  Some timing_info.vesting_period )
        ; field "vestingIncrement" ~typ:amount
            ~doc:"The vesting increment for a time-locked account"
            ~args:Arg.[]
            ~resolve:(fun _ timing ->
              match timing with
              | Account_timing.Untimed ->
                  None
              | Timed timing_info ->
                  Some timing_info.vesting_increment )
        ] )

  let genesis_constants =
    obj "GenesisConstants" ~fields:(fun _ ->
        [ field "accountCreationFee" ~typ:(non_null fee)
            ~doc:"The fee charged to create a new account"
            ~args:Arg.[]
            ~resolve:(fun _ () -> constraint_constants.account_creation_fee)
        ; field "genesisTimestamp" ~typ:(non_null string)
            ~doc:"The genesis timestamp in ISO 8601 format"
            ~args:Arg.[]
            ~resolve:(fun { ctx = t; _ } () ->
              Genesis_constants.genesis_timestamp_to_string t.genesis_timestamp
              )
        ] )

  module AccountObj = struct
    module AnnotatedBalance = struct
      type t =
        { total : Balance.t
        ; unknown : Balance.t
        ; timing : Mina_base.Account_timing.t
        ; breadcrumb : Transition_frontier.Breadcrumb.t option
        }

      let min_balance (b : t) =
        match (b.timing, b.breadcrumb) with
        | Untimed, _ ->
            Some Balance.zero
        | Timed _, None ->
            None
        | Timed timing_info, Some crumb ->
            let consensus_state =
              Transition_frontier.Breadcrumb.consensus_state crumb
            in
            let global_slot =
              Consensus.Data.Consensus_state.global_slot_since_genesis
                consensus_state
            in
            Some
              (Account.min_balance_at_slot ~global_slot
                 ~cliff_time:timing_info.cliff_time
                 ~cliff_amount:timing_info.cliff_amount
                 ~vesting_period:timing_info.vesting_period
                 ~vesting_increment:timing_info.vesting_increment
                 ~initial_minimum_balance:timing_info.initial_minimum_balance )

      let obj =
        obj "AnnotatedBalance"
          ~doc:
            "A total balance annotated with the amount that is currently \
             unknown with the invariant unknown <= total, as well as the \
             currently liquid and locked balances." ~fields:(fun _ ->
            [ field "total" ~typ:(non_null balance)
                ~doc:"The amount of MINA owned by the account"
                ~args:Arg.[]
                ~resolve:(fun _ (b : t) -> b.total)
            ; field "unknown" ~typ:(non_null balance)
                ~doc:
                  "The amount of MINA owned by the account whose origin is \
                   currently unknown"
                ~deprecated:(Deprecated None)
                ~args:Arg.[]
                ~resolve:(fun _ (b : t) -> b.unknown)
            ; field "liquid" ~typ:balance
                ~doc:
                  "The amount of MINA owned by the account which is currently \
                   available. Can be null if bootstrapping."
                ~deprecated:(Deprecated None)
                ~args:Arg.[]
                ~resolve:(fun _ (b : t) ->
                  Option.map (min_balance b) ~f:(fun min_balance ->
                      let total_balance : uint64 = Balance.to_uint64 b.total in
                      let min_balance_uint64 = Balance.to_uint64 min_balance in
                      Balance.of_uint64
                        ( if
                          Unsigned.UInt64.compare total_balance
                            min_balance_uint64
                          > 0
                        then
                          Unsigned.UInt64.sub total_balance min_balance_uint64
                        else Unsigned.UInt64.zero ) ) )
            ; field "locked" ~typ:balance
                ~doc:
                  "The amount of MINA owned by the account which is currently \
                   locked. Can be null if bootstrapping."
                ~deprecated:(Deprecated None)
                ~args:Arg.[]
                ~resolve:(fun _ (b : t) -> min_balance b)
            ; field "blockHeight" ~typ:(non_null length)
                ~doc:"Block height at which balance was measured"
                ~args:Arg.[]
                ~resolve:(fun _ (b : t) ->
                  match b.breadcrumb with
                  | None ->
                      Unsigned.UInt32.zero
                  | Some crumb ->
                      Transition_frontier.Breadcrumb.consensus_state crumb
                      |> Consensus.Data.Consensus_state.blockchain_length )
              (* TODO: Mutually recurse with "block" instead -- #5396 *)
            ; field "stateHash" ~typ:state_hash
                ~doc:
                  "Hash of block at which balance was measured. Can be null if \
                   bootstrapping. Guaranteed to be non-null for direct account \
                   lookup queries when not bootstrapping. Can also be null \
                   when accessed as nested properties (eg. via delegators). "
                ~args:Arg.[]
                ~resolve:(fun _ (b : t) ->
                  Option.map b.breadcrumb ~f:(fun crumb ->
                      Transition_frontier.Breadcrumb.state_hash crumb ) )
            ] )
    end

    module Partial_account = struct
      let to_full_account
          { Account.Poly.public_key
          ; token_id
          ; token_symbol
          ; nonce
          ; balance
          ; receipt_chain_hash
          ; delegate
          ; voting_for
          ; timing
          ; permissions
          ; zkapp
          } =
        let open Option.Let_syntax in
        let%bind token_symbol = token_symbol in
        let%bind nonce = nonce in
        let%bind receipt_chain_hash = receipt_chain_hash in
        let%bind voting_for = voting_for in
        let%map permissions = permissions in
        { Account.Poly.public_key
        ; token_id
        ; token_symbol
        ; nonce
        ; balance = balance.AnnotatedBalance.total
        ; receipt_chain_hash
        ; delegate
        ; voting_for
        ; timing
        ; permissions
        ; zkapp
        }

      let of_full_account
          { Account.Poly.public_key
          ; token_id
          ; token_symbol
          ; nonce
          ; balance
          ; receipt_chain_hash
          ; delegate
          ; voting_for
          ; timing
          ; permissions
          ; zkapp
          } =
        { Account.Poly.public_key
        ; token_id
        ; token_symbol = Some token_symbol
        ; nonce = Some nonce
        ; balance =
            { AnnotatedBalance.total = balance
            ; unknown = balance
            ; timing
            ; breadcrumb = None
            }
        ; receipt_chain_hash = Some receipt_chain_hash
        ; delegate
        ; voting_for = Some voting_for
        ; timing
        ; permissions = Some permissions
        ; zkapp
        }

      let of_account_id ledger account_id =
        let account =
          Ledger.location_of_account ledger account_id
          |> Option.bind ~f:(Ledger.get ledger)
        in
        match account with
        | Some account ->
            of_full_account account
        | None ->
            Account.
              { Poly.public_key = Account_id.public_key account_id
              ; token_id = Account_id.token_id account_id
              ; token_symbol = None
              ; nonce = None
              ; delegate = None
              ; balance =
                  { AnnotatedBalance.total = Balance.zero
                  ; unknown = Balance.zero
                  ; timing = Timing.Untimed
                  ; breadcrumb = None
                  }
              ; receipt_chain_hash = None
              ; voting_for = None
              ; timing = Timing.Untimed
              ; permissions = None
              ; zkapp = None
              }
    end

    type t =
      { account :
          ( Public_key.Compressed.t
          , Token_id.t
          , Account.Token_symbol.t option
          , AnnotatedBalance.t
          , Account.Nonce.t option
          , Receipt.Chain_hash.t option
          , Public_key.Compressed.t option
          , State_hash.t option
          , Account.Timing.t
          , Permissions.t option
          , Zkapp_account.t option )
          Account.Poly.t
      ; locked : bool option
      ; is_actively_staking : bool
      ; path : string
      ; index : Account.Index.t option
      }

    let lift account =
      { account
      ; locked = None
      ; is_actively_staking = false
      ; path = ""
      ; index = None
      }

    let get_best_ledger_account ledger aid =
      lift (Partial_account.of_account_id ledger aid)

    let auth_required =
      let open Permissions.Auth_required in
      enum "AccountAuthRequired" ~doc:"Kind of authorization required"
        ~values:
          [ enum_value "None" ~value:None
          ; enum_value "Either" ~value:Either
          ; enum_value "Proof" ~value:Proof
          ; enum_value "Signature" ~value:Signature
          ; enum_value "Impossible" ~value:Impossible
          ]

    let set_verification_key_perm =
      obj "VerificationKeyPermission" ~fields:(fun _ ->
          [ field "auth" ~typ:(non_null auth_required)
              ~doc:
                "Authorization required to set the verification key of the \
                 zkApp associated with the account"
              ~args:Arg.[]
              ~resolve:(fun _ (auth, _) -> auth)
          ; field "txnVersion" ~typ:(non_null string)
              ~args:Arg.[]
              ~resolve:(fun _ (_, version) ->
                Mina_numbers.Txn_version.to_string version )
          ] )

    let account_permissions =
      obj "AccountPermissions" ~fields:(fun _ ->
          [ field "editState" ~typ:(non_null auth_required)
              ~doc:"Authorization required to edit zkApp state"
              ~args:Arg.[]
              ~resolve:(fun _ permission ->
                permission.Permissions.Poly.edit_state )
          ; field "send" ~typ:(non_null auth_required)
              ~doc:"Authorization required to send tokens"
              ~args:Arg.[]
              ~resolve:(fun _ permission -> permission.Permissions.Poly.send)
          ; field "receive" ~typ:(non_null auth_required)
              ~doc:"Authorization required to receive tokens"
              ~args:Arg.[]
              ~resolve:(fun _ permission -> permission.Permissions.Poly.receive)
          ; field "access" ~typ:(non_null auth_required)
              ~doc:"Authorization required to access the account"
              ~args:Arg.[]
              ~resolve:(fun _ permission -> permission.Permissions.Poly.access)
          ; field "setDelegate" ~typ:(non_null auth_required)
              ~doc:"Authorization required to set the delegate"
              ~args:Arg.[]
              ~resolve:(fun _ permission ->
                permission.Permissions.Poly.set_delegate )
          ; field "setPermissions" ~typ:(non_null auth_required)
              ~doc:"Authorization required to change permissions"
              ~args:Arg.[]
              ~resolve:(fun _ permission ->
                permission.Permissions.Poly.set_permissions )
          ; field "setVerificationKey"
              ~typ:(non_null set_verification_key_perm)
              ~doc:
                "Authorization required to set the verification key of the \
                 zkApp associated with the account"
              ~args:Arg.[]
              ~resolve:(fun _ permission ->
                permission.Permissions.Poly.set_verification_key )
          ; field "setZkappUri" ~typ:(non_null auth_required)
              ~doc:
                "Authorization required to change the URI of the zkApp \
                 associated with the account "
              ~args:Arg.[]
              ~resolve:(fun _ permission ->
                permission.Permissions.Poly.set_zkapp_uri )
          ; field "editActionState" ~typ:(non_null auth_required)
              ~doc:"Authorization required to edit the action state"
              ~args:Arg.[]
              ~resolve:(fun _ permission ->
                permission.Permissions.Poly.edit_action_state )
          ; field "setTokenSymbol" ~typ:(non_null auth_required)
              ~doc:"Authorization required to set the token symbol"
              ~args:Arg.[]
              ~resolve:(fun _ permission ->
                permission.Permissions.Poly.set_token_symbol )
          ; field "incrementNonce" ~typ:(non_null auth_required)
              ~doc:"Authorization required to increment the nonce"
              ~args:Arg.[]
              ~resolve:(fun _ permission ->
                permission.Permissions.Poly.increment_nonce )
          ; field "setVotingFor" ~typ:(non_null auth_required)
              ~doc:
                "Authorization required to set the state hash the account is \
                 voting for"
              ~args:Arg.[]
              ~resolve:(fun _ permission ->
                permission.Permissions.Poly.set_voting_for )
          ; field "setTiming" ~typ:(non_null auth_required)
              ~doc:"Authorization required to set the timing of the account"
              ~args:Arg.[]
              ~resolve:(fun _ permission ->
                permission.Permissions.Poly.set_timing )
          ] )

    let account_vk =
      obj "AccountVerificationKeyWithHash" ~doc:"Verification key with hash"
        ~fields:(fun _ ->
          [ field "verificationKey" ~doc:"verification key in Base64 format"
              ~typ:
                (non_null @@ Pickles_unix.Graphql_scalars.VerificationKey.typ ())
              ~args:Arg.[]
              ~resolve:(fun _ (vk : _ With_hash.t) -> vk.data)
          ; field "hash" ~doc:"Hash of verification key"
              ~typ:
                ( non_null
                @@ Pickles_unix.Graphql_scalars.VerificationKeyHash.typ () )
              ~args:Arg.[]
              ~resolve:(fun _ (vk : _ With_hash.t) -> vk.hash)
          ] )

    let rec account =
      lazy
        (obj "Account" ~doc:"An account record according to the daemon"
           ~fields:(fun _ ->
             [ field "publicKey" ~typ:(non_null public_key)
                 ~doc:"The public identity of the account"
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } ->
                   account.Account.Poly.public_key )
             ; field "tokenId" ~typ:(non_null token_id)
                 ~doc:"The token associated with this account"
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } -> account.Account.Poly.token_id)
             ; field "token" ~typ:(non_null token_id)
                 ~doc:"The token associated with this account"
                 ~deprecated:(Deprecated (Some "Use tokenId"))
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } -> account.Account.Poly.token_id)
             ; field "timing" ~typ:(non_null account_timing)
                 ~doc:"The timing associated with this account"
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } -> account.Account.Poly.timing)
             ; field "balance"
                 ~typ:(non_null AnnotatedBalance.obj)
                 ~doc:"The amount of MINA owned by the account"
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } -> account.Account.Poly.balance)
             ; field "nonce" ~typ:account_nonce
                 ~doc:
                   "A natural number that increases with each transaction \
                    (stringified uint32)"
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } -> account.Account.Poly.nonce)
             ; field "inferredNonce" ~typ:account_nonce
                 ~doc:
                   "Like the `nonce` field, except it includes the scheduled \
                    transactions (transactions not yet included in a block) \
                    (stringified uint32)"
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } -> account.Account.Poly.nonce)
             ; field "epochDelegateAccount" ~typ:(Lazy.force account)
                 ~doc:
                   "The account that you delegated on the staking ledger of \
                    the current block's epoch"
                 ~args:Arg.[]
                 ~resolve:(fun _ _ -> None)
             ; field "receiptChainHash" ~typ:chain_hash
                 ~doc:"Top hash of the receipt chain Merkle-list"
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } ->
                   account.Account.Poly.receipt_chain_hash )
             ; field "delegate" ~typ:public_key
                 ~doc:
                   "The public key to which you are delegating - if you are \
                    not delegating to anybody, this would return your public \
                    key"
                 ~args:Arg.[]
                 ~deprecated:(Deprecated (Some "use delegateAccount instead"))
                 ~resolve:(fun _ { account; _ } -> account.Account.Poly.delegate)
             ; field "delegateAccount" ~typ:(Lazy.force account)
                 ~doc:
                   "The account to which you are delegating - if you are not \
                    delegating to anybody, this would return your public key"
                 ~args:Arg.[]
                 ~resolve:(fun _ _ -> None)
             ; field "delegators"
                 ~typ:(list @@ non_null @@ Lazy.force account)
                 ~doc:
                   "The list of accounts which are delegating to you (note \
                    that the info is recorded in the last epoch so it might \
                    not be up to date with the current account status)"
                 ~args:Arg.[]
                 ~resolve:(fun _ _ -> None)
             ; field "lastEpochDelegators"
                 ~typ:(list @@ non_null @@ Lazy.force account)
                 ~doc:
                   "The list of accounts which are delegating to you in the \
                    last epoch (note that the info is recorded in the one \
                    before last epoch epoch so it might not be up to date with \
                    the current account status)"
                 ~args:Arg.[]
                 ~resolve:(fun _ _ -> None)
             ; field "votingFor" ~typ:chain_hash
                 ~doc:
                   "The previous epoch lock hash of the chain which you are \
                    voting for"
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } ->
                   account.Account.Poly.voting_for )
             ; field "stakingActive" ~typ:(non_null bool)
                 ~doc:
                   "True if you are actively staking with this account on the \
                    current daemon - this may not yet have been updated if the \
                    staking key was changed recently"
                 ~args:Arg.[]
                 ~resolve:(fun _ { is_actively_staking; _ } ->
                   is_actively_staking )
             ; field "privateKeyPath" ~typ:(non_null string)
                 ~doc:"Path of the private key file for this account"
                 ~args:Arg.[]
                 ~resolve:(fun _ { path; _ } -> path)
             ; field "locked" ~typ:bool
                 ~doc:
                   "True if locked, false if unlocked, null if the account \
                    isn't tracked by the queried daemon"
                 ~args:Arg.[]
                 ~resolve:(fun _ { locked; _ } -> locked)
             ; field "index" ~typ:int
                 ~doc:
                   "The index of this account in the ledger, or null if this \
                    account does not yet have a known position in the best tip \
                    ledger"
                 ~args:Arg.[]
                 ~resolve:(fun _ { index; _ } -> index)
             ; field "zkappUri" ~typ:string
                 ~doc:
                   "The URI associated with this account, usually pointing to \
                    the zkApp source code"
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } ->
                   Option.value_map account.zkapp ~default:None ~f:(fun zkapp ->
                       Some zkapp.zkapp_uri ) )
             ; field "zkappState"
                 ~typ:
                   ( list @@ non_null
                   @@ Mina_base_unix.Graphql_scalars.FieldElem.typ () )
                 ~doc:
                   "The 8 field elements comprising the zkApp state associated \
                    with this account encoded as bignum strings"
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } ->
                   account.Account.Poly.zkapp
                   |> Option.map ~f:(fun zkapp_account ->
                          zkapp_account.app_state |> Zkapp_state.V.to_list ) )
             ; field "provedState" ~typ:bool
                 ~doc:
                   "Boolean indicating whether all 8 fields on zkAppState were \
                    last set by a proof-authorized account update"
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } ->
                   account.Account.Poly.zkapp
                   |> Option.map ~f:(fun zkapp_account ->
                          zkapp_account.proved_state ) )
             ; field "permissions" ~typ:account_permissions
                 ~doc:"Permissions for updating certain fields of this account"
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } ->
                   account.Account.Poly.permissions )
             ; field "tokenSymbol" ~typ:string
                 ~doc:
                   "The symbol for the token owned by this account, if there \
                    is one"
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } ->
                   account.Account.Poly.token_symbol )
             ; field "verificationKey" ~typ:account_vk
                 ~doc:"Verification key associated with this account"
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } ->
                   Option.value_map account.Account.Poly.zkapp ~default:None
                     ~f:(fun zkapp_account -> zkapp_account.verification_key) )
             ; field "actionState"
                 ~doc:"Action state associated with this account"
                 ~typ:
                   (list
                      ( non_null
                      @@ Snark_params_unix.Graphql_scalars.Action.typ () ) )
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } ->
                   Option.map account.Account.Poly.zkapp
                     ~f:(fun zkapp_account ->
                       Pickles_types.Vector.to_list zkapp_account.action_state )
                   )
             ; field "leafHash"
                 ~doc:
                   "The base58Check-encoded hash of this account to bootstrap \
                    the merklePath"
                 ~typ:(Mina_base_unix.Graphql_scalars.FieldElem.typ ())
                 ~args:Arg.[]
                 ~resolve:(fun _ { account; _ } ->
                   let open Option.Let_syntax in
                   let%map account = Partial_account.to_full_account account in
                   Ledger_hash.of_digest (Account.digest account) )
             ; field "merklePath"
                 ~doc:
                   "Merkle path is a list of path elements that are either the \
                    left or right hashes up to the root"
                 ~typ:(list (non_null merkle_path_element))
                 ~args:Arg.[]
                 ~resolve:(fun _ _ -> None)
             ] ) )

    let account = Lazy.force account
  end

  module Command_status = struct
    type t =
      | Applied
      | Enqueued
      | Included_but_failed of Transaction_status.Failure.Collection.t
    [@@warning "-37"]

    let failure_reasons =
      obj "ZkappCommandFailureReason" ~fields:(fun _ ->
          [ field "index" ~typ:(Graphql_basic_scalars.Index.typ ())
              ~args:[] ~doc:"List index of the account update that failed"
              ~resolve:(fun _ (index, _) -> Some index)
          ; field "failures"
              ~typ:
                ( non_null @@ list @@ non_null
                @@ Mina_base_unix.Graphql_scalars.TransactionStatusFailure.typ
                     () )
              ~args:[]
              ~doc:
                "Failure reason for the account update or any nested zkapp \
                 command"
              ~resolve:(fun _ (_, failures) -> failures)
          ] )
  end

  module User_command = struct
    let kind : ('context, [ `Payment | `Stake_delegation ] option) typ =
      scalar "UserCommandKind" ~doc:"The kind of user command" ~coerce:(function
        | `Payment ->
            `String "PAYMENT"
        | `Stake_delegation ->
            `String "STAKE_DELEGATION" )

    let to_kind (t : Signed_command.t) =
      match Signed_command.payload t |> Signed_command_payload.body with
      | Payment _ ->
          `Payment
      | Stake_delegation _ ->
          `Stake_delegation

    let user_command_interface :
        ( 'context
        , ( 'context
          , (Signed_command.t, Transaction_hash.t) With_hash.t )
          abstract_value
          option )
        typ =
      interface "UserCommand" ~doc:"Common interface for user commands"
        ~fields:(fun _ ->
          [ abstract_field "id" ~typ:(non_null transaction_id) ~args:[]
          ; abstract_field "hash" ~typ:(non_null transaction_hash) ~args:[]
          ; abstract_field "kind" ~typ:(non_null kind) ~args:[]
              ~doc:"String describing the kind of user command"
          ; abstract_field "nonce" ~typ:(non_null int) ~args:[]
              ~doc:"Sequence number of command for the fee-payer's account"
          ; abstract_field "source"
              ~typ:(non_null AccountObj.account)
              ~args:[] ~doc:"Account that the command is sent from"
          ; abstract_field "receiver"
              ~typ:(non_null AccountObj.account)
              ~args:[] ~doc:"Account that the command applies to"
          ; abstract_field "feePayer"
              ~typ:(non_null AccountObj.account)
              ~args:[] ~doc:"Account that pays the fees for the command"
          ; abstract_field "validUntil"
              ~typ:(non_null global_slot_since_genesis)
              ~args:[]
              ~doc:
                "The global slot number after which this transaction cannot be \
                 applied"
          ; abstract_field "token" ~typ:(non_null token_id) ~args:[]
              ~doc:"Token used by the command"
          ; abstract_field "amount" ~typ:(non_null amount) ~args:[]
              ~doc:
                "Amount that the source is sending to receiver - 0 for \
                 commands that are not associated with an amount"
          ; abstract_field "feeToken" ~typ:(non_null token_id) ~args:[]
              ~doc:"Token used to pay the fee"
          ; abstract_field "fee" ~typ:(non_null fee) ~args:[]
              ~doc:
                "Fee that the fee-payer is willing to pay for making the \
                 transaction"
          ; abstract_field "memo" ~typ:(non_null string) ~args:[]
              ~doc:"Short arbitrary message provided by the sender"
          ; abstract_field "isDelegation" ~typ:(non_null bool) ~args:[]
              ~doc:
                "If true, this represents a delegation of stake, otherwise it \
                 is a payment"
              ~deprecated:(Deprecated (Some "use kind field instead"))
          ; abstract_field "from" ~typ:(non_null public_key) ~args:[]
              ~doc:"Public key of the sender"
              ~deprecated:(Deprecated (Some "use feePayer field instead"))
          ; abstract_field "fromAccount"
              ~typ:(non_null AccountObj.account)
              ~args:[] ~doc:"Account of the sender"
              ~deprecated:(Deprecated (Some "use feePayer field instead"))
          ; abstract_field "to" ~typ:(non_null public_key) ~args:[]
              ~doc:"Public key of the receiver"
              ~deprecated:(Deprecated (Some "use receiver field instead"))
          ; abstract_field "toAccount"
              ~typ:(non_null AccountObj.account)
              ~args:[] ~doc:"Account of the receiver"
              ~deprecated:(Deprecated (Some "use receiver field instead"))
          ; abstract_field "failureReason"
              ~typ:
                (Mina_base_unix.Graphql_scalars.TransactionStatusFailure.typ ())
              ~args:[] ~doc:"null is no failure, reason for failure otherwise."
          ] )

    module With_status = struct
      type 'a t = { data : 'a; status : Command_status.t }

      let map t ~f = { t with data = f t.data }
    end

    let field_no_status ?doc ?deprecated lab ~typ ~args ~resolve =
      field ?doc ?deprecated lab ~typ ~args ~resolve:(fun c uc ->
          resolve c uc.With_status.data )

    let user_command_shared_fields :
        ( t
        , (Signed_command.t, Transaction_hash.t) With_hash.t With_status.t )
        field
        list =
      [ field_no_status "id" ~typ:(non_null transaction_id) ~args:[]
          ~resolve:(fun _ user_command ->
            Signed_command user_command.With_hash.data )
      ; field_no_status "hash" ~typ:(non_null transaction_hash) ~args:[]
          ~resolve:(fun _ user_command -> user_command.With_hash.hash)
      ; field_no_status "kind" ~typ:(non_null kind) ~args:[]
          ~doc:"String describing the kind of user command"
          ~resolve:(fun _ cmd -> to_kind cmd.With_hash.data)
      ; field_no_status "nonce" ~typ:(non_null int) ~args:[]
          ~doc:"Sequence number of command for the fee-payer's account"
          ~resolve:(fun _ payment ->
            Signed_command_payload.nonce
            @@ Signed_command.payload payment.With_hash.data
            |> Account.Nonce.to_int )
      ; field_no_status "source" ~typ:(non_null AccountObj.account)
          ~args:[] ~doc:"Account that the command is sent from"
          ~resolve:(fun { ctx = t; _ } cmd ->
            AccountObj.get_best_ledger_account (Ledger.of_database t.db)
              (Signed_command.fee_payer cmd.With_hash.data) )
      ; field_no_status "receiver" ~typ:(non_null AccountObj.account)
          ~args:[] ~doc:"Account that the command applies to"
          ~resolve:(fun { ctx = t; _ } cmd ->
            AccountObj.get_best_ledger_account (Ledger.of_database t.db)
              (Signed_command.receiver cmd.With_hash.data) )
      ; field_no_status "feePayer" ~typ:(non_null AccountObj.account)
          ~args:[] ~doc:"Account that pays the fees for the command"
          ~deprecated:(Deprecated (Some "use source field instead"))
          ~resolve:(fun { ctx = t; _ } cmd ->
            AccountObj.get_best_ledger_account (Ledger.of_database t.db)
              (Signed_command.fee_payer cmd.With_hash.data) )
      ; field_no_status "validUntil" ~typ:(non_null global_slot_since_genesis)
          ~args:[]
          ~doc:
            "The global slot number after which this transaction cannot be \
             applied" ~resolve:(fun _ cmd ->
            Signed_command.valid_until cmd.With_hash.data )
      ; field_no_status "token" ~typ:(non_null token_id) ~args:[]
          ~doc:"Token used for the transaction" ~resolve:(fun _ cmd ->
            Signed_command.token cmd.With_hash.data )
      ; field_no_status "amount" ~typ:(non_null amount) ~args:[]
          ~doc:
            "Amount that the source is sending to receiver; 0 for commands \
             without an associated amount" ~resolve:(fun _ cmd ->
            match Signed_command.amount cmd.With_hash.data with
            | Some amount ->
                amount
            | None ->
                Currency.Amount.zero )
      ; field_no_status "feeToken" ~typ:(non_null token_id) ~args:[]
          ~doc:"Token used to pay the fee" ~resolve:(fun _ cmd ->
            Signed_command.fee_token cmd.With_hash.data )
      ; field_no_status "fee" ~typ:(non_null fee) ~args:[]
          ~doc:
            "Fee that the fee-payer is willing to pay for making the \
             transaction" ~resolve:(fun _ cmd ->
            Signed_command.fee cmd.With_hash.data )
      ; field_no_status "memo" ~typ:(non_null string) ~args:[]
          ~doc:
            (sprintf
               "A short message from the sender, encoded with Base58Check, \
                version byte=0x%02X; byte 2 of the decoding is the message \
                length"
               (Char.to_int Base58_check.Version_bytes.user_command_memo) )
          ~resolve:(fun _ payment ->
            Signed_command_payload.memo
            @@ Signed_command.payload payment.With_hash.data
            |> Signed_command_memo.to_base58_check )
      ; field_no_status "isDelegation" ~typ:(non_null bool) ~args:[]
          ~doc:"If true, this command represents a delegation of stake"
          ~deprecated:(Deprecated (Some "use kind field instead"))
          ~resolve:(fun _ user_command ->
            match
              Signed_command.Payload.body
              @@ Signed_command.payload user_command.With_hash.data
            with
            | Stake_delegation _ ->
                true
            | _ ->
                false )
      ; field_no_status "from" ~typ:(non_null public_key) ~args:[]
          ~doc:"Public key of the sender"
          ~deprecated:(Deprecated (Some "use feePayer field instead"))
          ~resolve:(fun _ cmd -> Signed_command.fee_payer_pk cmd.With_hash.data)
      ; field_no_status "fromAccount" ~typ:(non_null AccountObj.account)
          ~args:[] ~doc:"Account of the sender"
          ~deprecated:(Deprecated (Some "use feePayer field instead"))
          ~resolve:(fun { ctx = t; _ } payment ->
            AccountObj.get_best_ledger_account (Ledger.of_database t.db)
            @@ Signed_command.fee_payer payment.With_hash.data )
      ; field_no_status "to" ~typ:(non_null public_key) ~args:[]
          ~doc:"Public key of the receiver"
          ~deprecated:(Deprecated (Some "use receiver field instead"))
          ~resolve:(fun _ cmd -> Signed_command.receiver_pk cmd.With_hash.data)
      ; field_no_status "toAccount"
          ~typ:(non_null AccountObj.account)
          ~doc:"Account of the receiver"
          ~deprecated:(Deprecated (Some "use receiver field instead"))
          ~args:Arg.[]
          ~resolve:(fun { ctx = t; _ } cmd ->
            AccountObj.get_best_ledger_account (Ledger.of_database t.db)
            @@ Signed_command.receiver cmd.With_hash.data )
      ; field "failureReason"
          ~typ:(Mina_base_unix.Graphql_scalars.TransactionStatusFailure.typ ())
          ~args:[]
          ~doc:
            "null is no failure or status unknown, reason for failure \
             otherwise." ~resolve:(fun _ uc ->
            match uc.With_status.status with
            | Applied | Enqueued ->
                None
            | Included_but_failed failures ->
                List.concat failures |> List.hd )
      ]

    let payment =
      obj "UserCommandPayment" ~fields:(fun _ -> user_command_shared_fields)

    let mk_payment = add_type user_command_interface payment

    let user_command = user_command_interface
  end

  module Zkapp_command = struct
    module With_status = struct
      type 'a t = { data : 'a; status : Command_status.t }

      let map t ~f = { t with data = f t.data }
    end

    let field_no_status ?doc ?deprecated lab ~typ ~args ~resolve =
      field ?doc ?deprecated lab ~typ ~args ~resolve:(fun c cmd ->
          resolve c cmd.With_status.data )

    let zkapp_command =
      let conv (x : (t, Zkapp_command.t) Fields_derivers_graphql.Schema.typ) :
          (t, Zkapp_command.t) typ =
        Obj.magic x
      in
      obj "ZkappCommandResult" ~fields:(fun _ ->
          [ field_no_status "id"
              ~doc:"A Base64 string representing the zkApp command"
              ~typ:(non_null transaction_id) ~args:[]
              ~resolve:(fun _ zkapp_command ->
                Zkapp_command zkapp_command.With_hash.data )
          ; field_no_status "hash"
              ~doc:"A cryptographic hash of the zkApp command"
              ~typ:(non_null transaction_hash) ~args:[]
              ~resolve:(fun _ zkapp_command -> zkapp_command.With_hash.hash)
          ; field_no_status "zkappCommand"
              ~typ:(Zkapp_command.typ () |> conv)
              ~args:Arg.[]
              ~doc:"zkApp command representing the transaction"
              ~resolve:(fun _ zkapp_command -> zkapp_command.With_hash.data)
          ; field "failureReason" ~typ:(list @@ Command_status.failure_reasons)
              ~args:[]
              ~doc:
                "The reason for the zkApp transaction failure; null means \
                 success or the status is unknown" ~resolve:(fun _ cmd ->
                match cmd.With_status.status with
                | Applied | Enqueued ->
                    None
                | Included_but_failed failures ->
                    Some
                      (List.map
                         (Transaction_status.Failure.Collection.to_display
                            failures ) ~f:(fun f -> Some f) ) )
          ] )
  end

  module Payload = struct
    let send_payment =
      obj "SendPaymentPayload" ~fields:(fun _ ->
          [ field "payment"
              ~typ:(non_null User_command.user_command)
              ~doc:"Payment that was sent"
              ~args:Arg.[]
              ~resolve:(fun _ -> Fn.id)
          ] )

    let send_zkapp =
      obj "SendZkappPayload" ~fields:(fun _ ->
          [ field "zkapp"
              ~typ:(non_null Zkapp_command.zkapp_command)
              ~doc:"zkApp transaction that was sent"
              ~args:Arg.[]
              ~resolve:(fun _ -> Fn.id)
          ] )
  end

  module Input = struct
    open Schema.Arg

    module PublicKey = struct
      let arg_typ =
        scalar "PublicKey" ~doc:"Public key in Base58Check format"
          ~coerce:(fun pk ->
            match pk with
            | `String s ->
                Result.map_error
                  (Public_key.Compressed.of_base58_check s)
                  ~f:Error.to_string_hum
            | _ ->
                Error "Expected public key as a string in Base58Check format" )
          ~to_json:(function
            | k -> `String (Public_key.Compressed.to_base58_check k) )
    end

    module TokenId = struct
      type input = Token_id.t

      let arg_typ =
        scalar "TokenId" ~doc:"Base58Check representation of a token identifier"
          ~coerce:(fun token ->
            try
              match token with
              | `String token ->
                  Ok (Token_id.of_string token)
              | _ ->
                  Error "Invalid format for token."
            with _ -> Error "Invalid format for token." )
          ~to_json:(function (i : input) -> `String (Token_id.to_string i))
    end

    module type Numeric_type = sig
      type t

      val to_string : t -> string

      val of_string : string -> t

      val of_int : int -> t
    end

    (** Converts a type into a graphql argument type. Expect name to start with uppercase    *)
    let make_numeric_arg (type t) ~name
        (module Numeric : Numeric_type with type t = t) =
      let lower_name = String.lowercase name in
      scalar name
        ~doc:
          (sprintf
             "String or Integer representation of a %s number. If the input is \
              a string, it must represent the number in base 10"
             lower_name )
        ~to_json:(function n -> `String (Numeric.to_string n))
        ~coerce:(fun key ->
          match key with
          | `String s -> (
              try
                let n = Numeric.of_string s in
                let s' = Numeric.to_string n in
                (* Here, we check that the string that was passed converts to
                     the numeric type, and that it is in range, by converting
                     back to a string and checking that it is equal to the one
                     passed. This prevents the following weirdnesses in the
                     [Unsigned.UInt*] parsers:
                     * if the absolute value is greater than [max_int], the value
                       returned is [max_int]
                   - ["99999999999999999999999999999999999"] is [max_int]
                   - ["-99999999999999999999999999999999999"] is [max_int]
                     * if otherwise the value is negative, the value returned is
                       [max_int - (x - 1)]
                   - ["-1"] is [max_int]
                     * if there is a non-numeric character part-way through the
                       string, the numeric prefix is treated as a number
                   - ["1_000_000"] is [1]
                   - ["-1_000_000"] is [max_int]
                   - ["1.1"] is [1]
                   - ["0x15"] is [0]
                     * leading spaces are ignored
                   - [" 1"] is [1]
                     This is annoying to document, none of these behaviors are
                     useful to users, and unexpectedly triggering one of them
                     could have nasty consequences. Thus, we raise an error
                     rather than silently misinterpreting their input.
                *)
                assert (String.equal s s') ;
                Ok n
                (* TODO: We need a better error message to the user here *)
              with _ -> Error (sprintf "Could not decode %s." lower_name) )
          | `Int n ->
              if n < 0 then
                Error
                  (sprintf "Could not convert negative number to %s." lower_name)
              else Ok (Numeric.of_int n)
          | _ ->
              Error (sprintf "Invalid format for %s type." lower_name) )

    module UInt64 = struct
      let arg_typ = make_numeric_arg ~name:"UInt64" (module Unsigned.UInt64)
    end

    module UInt32 = struct
      type input = Unsigned.UInt32.t

      let arg_typ = make_numeric_arg ~name:"UInt32" (module Unsigned.UInt32)
    end

    module SignatureInput = struct
      open Snark_params.Tick

      type input =
        | Raw of Signature.t
        | Field_and_scalar of Field.t * Inner_curve.Scalar.t
      [@@warning "-37"]

      let arg_typ =
        obj "SignatureInput"
          ~coerce:(fun field scalar rawSignature ->
            match rawSignature with
            | Some signature ->
                Result.of_option
                  (Signature.Raw.decode signature)
                  ~error:"rawSignature decoding error"
            | None -> (
                match (field, scalar) with
                | Some field, Some scalar ->
                    Ok
                      ( Field.of_string field
                      , Inner_curve.Scalar.of_string scalar )
                | _ ->
                    Error "Either field+scalar or rawSignature must by non-null"
                ) )
          ~doc:
            "A cryptographic signature -- you must provide either field+scalar \
             or rawSignature"
          ~fields:
            [ arg "field" ~typ:string ~doc:"Field component of signature"
            ; arg "scalar" ~typ:string ~doc:"Scalar component of signature"
            ; arg "rawSignature" ~typ:string ~doc:"Raw encoded signature"
            ]
          ~split:(fun f (input : input) ->
            match input with
            | Raw (s : Signature.t) ->
                f None None (Some (Signature.Raw.encode s))
            | Field_and_scalar (field, scalar) ->
                f
                  (Some (Field.to_string field))
                  (Some (Inner_curve.Scalar.to_string scalar))
                  None )
    end

    module Fields = struct
      let from ~doc = arg "from" ~typ:(non_null PublicKey.arg_typ) ~doc

      let to_ ~doc = arg "to" ~typ:(non_null PublicKey.arg_typ) ~doc

      let fee ~doc = arg "fee" ~typ:(non_null UInt64.arg_typ) ~doc

      let amount ~doc = arg "amount" ~typ:(non_null UInt64.arg_typ) ~doc

      let memo =
        arg "memo" ~typ:string
          ~doc:"Short arbitrary message provided by the sender"

      let valid_until =
        arg "validUntil" ~typ:UInt32.arg_typ
          ~doc:
            "The global slot since genesis after which this transaction cannot \
             be applied"

      let nonce =
        arg "nonce" ~typ:UInt32.arg_typ
          ~doc:
            "Should only be set when cancelling transactions, otherwise a \
             nonce is determined automatically"

      let signature =
        arg "signature" ~typ:SignatureInput.arg_typ
          ~doc:
            "If a signature is provided, this transaction is considered signed \
             and will be broadcasted to the network without requiring a \
             private key"
    end

    module SendPaymentInput = struct
      type input =
        { from : (Epoch_seed.t, bool) Public_key.Compressed.Poly.t
        ; to_ : Account.key
        ; amount : Currency.Amount.t
        ; fee : Currency.Fee.t
        ; valid_until : UInt32.input option
        ; memo : string option
        ; nonce : Mina_numbers.Account_nonce.t option
        }

      let arg_typ =
        let open Fields in
        obj "SendPaymentInput"
          ~coerce:(fun from to_ amount fee valid_until memo nonce ->
            (from, to_, amount, fee, valid_until, memo, nonce) )
          ~split:(fun f (x : input) ->
            f x.from x.to_
              (Currency.Amount.to_uint64 x.amount)
              (Currency.Fee.to_uint64 x.fee)
              x.valid_until x.memo x.nonce )
          ~fields:
            [ from ~doc:"Public key of sender of payment"
            ; to_ ~doc:"Public key of recipient of payment"
            ; amount ~doc:"Amount of MINA to send to receiver"
            ; fee ~doc:"Fee amount in order to send payment"
            ; valid_until
            ; memo
            ; nonce
            ]
    end

    module SendZkappInput = struct
      type input = Mina_base.Zkapp_command.t

      let arg_typ =
        let conv
            (x :
              Mina_base.Zkapp_command.t
              Fields_derivers_graphql.Schema.Arg.arg_typ ) :
            Mina_base.Zkapp_command.t Graphql_async.Schema.Arg.arg_typ =
          Obj.magic x
        in
        let arg_typ =
          { arg_typ = Mina_base.Zkapp_command.arg_typ () |> conv
          ; to_json =
              (function
              | x ->
                  Yojson.Safe.to_basic
                    (Mina_base.Zkapp_command.zkapp_command_to_json x) )
          }
        in
        obj "SendZkappInput" ~coerce:Fn.id
          ~split:(fun f (x : input) -> f x)
          ~fields:
            [ arg "zkappCommand"
                ~doc:"zkApp command structure representing the transaction"
                ~typ:arg_typ
            ]
    end
  end
end

module Mutations = struct
  open Schema

  let send_payment =
    io_field "sendPayment" ~doc:"Send a payment"
      ~typ:(non_null Types.Payload.send_payment)
      ~args:
        Arg.
          [ arg "input" ~typ:(non_null Types.Input.SendPaymentInput.arg_typ)
          ; Types.Input.Fields.signature
          ]
      ~resolve:(fun { ctx = t; _ } ()
                    (from, to_, amount, fee, valid_until, memo, nonce_opt)
                    signature ->
        let payload =
          Signed_command.Payload.create ~fee:(Fee.of_uint64 fee)
            ~fee_payer_pk:from
            ~nonce:(Option.value ~default:Unsigned.UInt32.zero nonce_opt)
            ~valid_until:
              (Option.map valid_until
                 ~f:Mina_numbers.Global_slot_since_genesis.of_uint32 )
            ~memo:
              (Option.value ~default:Signed_command_memo.empty
                 (Option.map memo ~f:Signed_command_memo.create_from_string_exn) )
            ~body:
              (Signed_command_payload.Body.Payment
                 { receiver_pk = to_; amount = Amount.of_uint64 amount } )
        in
        let%bind.Deferred.Result command =
          match signature with
          | None ->
              return (Error "Signature needed")
          | Some signature -> (
              let%bind.Deferred.Result signature =
                signature |> Deferred.return
              in
              match
                Signed_command.create_with_signature_checked
                  ~signature_kind:Mina_signature_kind.Testnet signature from
                  payload
              with
              | Some command ->
                  return (Ok (Signed_command.forget_check command))
              | None ->
                  return (Error "Signature verification failed") )
        in
        let l = Ledger.of_database t.db in
        match
          Result.( >>= )
            (Ledger.apply_transaction_first_pass ~constraint_constants
               ~global_slot:(current_slot t)
               ~txn_state_view:(Mina_state.Protocol_state.Body.view state_body)
               l (Command (Signed_command command)) )
            (Ledger.apply_transaction_second_pass l)
        with
        | Error err ->
            return (Error (Error.to_string_mach err))
        | Ok txn_applied ->
            Ledger.Mask.Attached.commit l ;

            let txn_hash =
              Transaction_hash.to_base58_check
              @@ Transaction_hash.hash_command (Signed_command command)
            in
            Hashtbl.add_exn t.commands ~key:txn_hash
              ~data:
                ( Signed_command command
                , Ledger.Transaction_applied.transaction_status txn_applied ) ;

            print_endline @@ "applied payment: " ^ txn_hash ^ " "
            ^ Yojson.Safe.pretty_to_string @@ Transaction_status.to_yojson
            @@ Ledger.Transaction_applied.transaction_status txn_applied ;

            let cmd =
              { Types.User_command.With_status.data = command
              ; status = Applied
              }
            in
            let cmd_with_hash =
              Types.User_command.With_status.map cmd ~f:(fun cmd ->
                  { With_hash.data = cmd
                  ; hash = Transaction_hash.hash_command (Signed_command cmd)
                  } )
            in
            Deferred.Result.return (Types.User_command.mk_payment cmd_with_hash)
        )

  let send_zkapp =
    io_field "sendZkapp" ~doc:"Send a zkApp transaction"
      ~typ:(non_null Types.Payload.send_zkapp)
      ~args:
        Arg.[ arg "input" ~typ:(non_null Types.Input.SendZkappInput.arg_typ) ]
      ~resolve:(fun { ctx = t; _ } () zkapp_command ->
        let l = Ledger.of_database t.db in
        let%bind.Deferred.Result partialy_applied_txn =
          match
            Ledger.apply_transaction_first_pass ~constraint_constants
              ~global_slot:(current_slot t)
              ~txn_state_view:(Mina_state.Protocol_state.Body.view state_body)
              l (Command (Zkapp_command zkapp_command))
          with
          | Error err ->
              return (Error (Error.to_string_mach err))
          | Ok partialy_applied_txn ->
              return (Ok partialy_applied_txn)
        in

        let%bind.Deferred.Result txn_applied =
          match Ledger.apply_transaction_second_pass l partialy_applied_txn with
          | Error err ->
              return (Error (Error.to_string_mach err))
          | Ok txn_applied ->
              return (Ok txn_applied)
        in

        Ledger.Mask.Attached.commit l ;

        let txn_hash =
          Transaction_hash.to_base58_check
          @@ Transaction_hash.hash_command (Zkapp_command zkapp_command)
        in
        Hashtbl.add_exn t.commands ~key:txn_hash
          ~data:
            ( Zkapp_command zkapp_command
            , Ledger.Transaction_applied.transaction_status txn_applied ) ;

        print_endline @@ "applied zkapp command: " ^ txn_hash ^ " "
        ^ Yojson.Safe.pretty_to_string @@ Transaction_status.to_yojson
        @@ Ledger.Transaction_applied.transaction_status txn_applied ;

        let cmd =
          { Types.Zkapp_command.With_status.data = zkapp_command
          ; status = Applied
          }
        in
        let cmd_with_hash =
          Types.Zkapp_command.With_status.map cmd ~f:(fun cmd ->
              { With_hash.data = cmd
              ; hash = Transaction_hash.hash_command (Zkapp_command cmd)
              } )
        in
        return (Ok cmd_with_hash) )

  let create_account =
    io_field "createAccount" ~doc:"Create test account" ~typ:(non_null string)
      ~args:
        Arg.[ arg "publicKey" ~typ:(non_null Types.Input.PublicKey.arg_typ) ]
      ~resolve:(fun { ctx = t; _ } () pk ->
        let account_id = Account_id.create pk Token_id.default in
        let account =
          Account.create account_id
            (Currency.Balance.of_uint64
               (Unsigned.UInt64.of_int64 1_000_000_000_000L) )
        in
        match Ledger.Db.get_or_create_account t.db account_id account with
        | Ok (`Added, _) ->
            return (Ok "Added")
        | Ok (`Existed, _) ->
            return (Ok "Existed")
        | Error err ->
            return (Error (Error.to_string_mach err)) )

  let commands = [ send_payment; send_zkapp; create_account ]
end

module Queries = struct
  open Schema

  let sync_status =
    io_field "syncStatus" ~doc:"Network sync status" ~args:[]
      ~typ:(non_null Types.sync_status) ~resolve:(fun { ctx = _; _ } () ->
        return (Ok `Synced) )

  let daemon_status =
    io_field "daemonStatus" ~doc:"Get running daemon status" ~args:[]
      ~typ:(non_null Types.DaemonStatus.t) ~resolve:(fun { ctx = _; _ } () ->
        let open Types.DaemonStatus in
        return (Ok { chain_id = "69420" }) )

  let genesis_constants =
    field "genesisConstants"
      ~doc:
        "The constants used to determine the configuration of the genesis \
         block and all of its transitive dependencies"
      ~args:Arg.[]
      ~typ:(non_null Types.genesis_constants)
      ~resolve:(fun _ () -> ())

  let account =
    field "account" ~doc:"Find any account via a public key and token"
      ~typ:Types.AccountObj.account
      ~args:
        Arg.
          [ arg "publicKey" ~doc:"Public key of account being retrieved"
              ~typ:(non_null Types.Input.PublicKey.arg_typ)
          ; arg' "token"
              ~doc:"Token of account being retrieved (defaults to MINA)"
              ~typ:Types.Input.TokenId.arg_typ ~default:Token_id.default
          ]
      ~resolve:(fun { ctx = t; _ } () public_key token_id ->
        let%bind.Option location =
          Ledger.Db.location_of_account t.db
            (Account_id.create public_key token_id)
        in
        let%map.Option account = Ledger.Db.get t.db location in
        Types.AccountObj.Partial_account.of_full_account account
        |> Types.AccountObj.lift )

  let user_command =
    field "userCommand" ~doc:"Get applied command"
      ~typ:Types.User_command.user_command
      ~args:
        Arg.
          [ arg "hash" ~doc:"Hash of command being retrieved"
              ~typ:(non_null string)
          ]
      ~resolve:(fun { ctx = t; _ } () hash ->
        let%bind.Option command, status =
          match Hashtbl.find t.commands hash with
          | Some (Signed_command command, status) ->
              Some (command, status)
          | _ ->
              None
        in
        let cmd =
          { Types.User_command.With_status.data = command
          ; status =
              ( match status with
              | Applied ->
                  Applied
              | Failed reason ->
                  Included_but_failed reason )
          }
        in
        let cmd_with_hash =
          Types.User_command.With_status.map cmd ~f:(fun cmd ->
              { With_hash.data = cmd
              ; hash = Transaction_hash.hash_command (Signed_command cmd)
              } )
        in
        Some (Types.User_command.mk_payment cmd_with_hash) )

  let zkapp_command =
    field "zkappCommand" ~doc:"Get applied command"
      ~typ:Types.Zkapp_command.zkapp_command
      ~args:
        Arg.
          [ arg "hash" ~doc:"Hash of command being retrieved"
              ~typ:(non_null string)
          ]
      ~resolve:(fun { ctx = t; _ } () hash ->
        let%bind.Option command, status =
          match Hashtbl.find t.commands hash with
          | Some (Zkapp_command command, status) ->
              Some (command, status)
          | _ ->
              None
        in
        let cmd =
          { Types.Zkapp_command.With_status.data = command
          ; status =
              ( match status with
              | Applied ->
                  Applied
              | Failed reason ->
                  Included_but_failed reason )
          }
        in
        let cmd_with_hash =
          Types.Zkapp_command.With_status.map cmd ~f:(fun cmd ->
              { With_hash.data = cmd
              ; hash = Transaction_hash.hash_command (Zkapp_command cmd)
              } )
        in
        Some cmd_with_hash )

  let commands =
    [ sync_status
    ; daemon_status
    ; genesis_constants
    ; account
    ; user_command
    ; zkapp_command
    ]
end

let schema =
  Graphql_async.Schema.(
    schema Queries.commands ~mutations:Mutations.commands ~subscriptions:[])
