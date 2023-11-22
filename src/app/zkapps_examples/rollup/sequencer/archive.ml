open Mina_base
open Mina_transaction
module Account_map = Map.Make (Account_id)

module Block_info = struct
  type t =
    { height : int
    ; state_hash : string
    ; parent_hash : string
    ; ledger_hash : string
    ; chain_status : string
    ; timestamp : int
    ; global_slot_since_hardfork : int
    ; global_slot_since_genesis : int
    ; distance_from_max_block_height : int
    }

  let dummy =
    { height = 0
    ; state_hash = ""
    ; parent_hash = ""
    ; ledger_hash = ""
    ; chain_status = ""
    ; timestamp = 0
    ; global_slot_since_hardfork = 0
    ; global_slot_since_genesis = 0
    ; distance_from_max_block_height = 1
    }
end

module Transaction_info = struct
  type t =
    { status : Transaction_status.t
    ; hash : Transaction_hash.t
    ; memo : Signed_command_memo.t
    ; authorization_kind : Account_update.Authorization_kind.t
    }
end

module Event = struct
  type t = Snark_params.Tick.Field.t array
end

module Account_update_events = struct
  type t =
    { block_info : Block_info.t option
    ; transaction_info : Transaction_info.t option
    ; events : Event.t list
    }
end

module Action = Event

module Account_update_actions = struct
  type t =
    { block_info : Block_info.t option
    ; transaction_info : Transaction_info.t option
    ; action_state : Snark_params.Tick.Field.t Pickles_types.Vector.Vector_5.t
    ; account_update_id : int
    ; actions : Action.t list
    }
end

module Archive = struct
  type t =
    { mutable actions_map : Account_update_actions.t list Account_map.t
    ; mutable events_map : Account_update_events.t list Account_map.t
    }

  let create () =
    { actions_map = Account_map.empty; events_map = Account_map.empty }

  let add_actions t (account_update : Account_update.t) transaction_info
      (account : Account.t) =
    match account_update.body.actions with
    | [] ->
        ()
    | actions ->
        let zkapp : Zkapp_account.t =
          Core_kernel.Option.value_exn account.zkapp
        in
        let action =
          { Account_update_actions.block_info = Some Block_info.dummy
          ; transaction_info
          ; action_state = zkapp.action_state
          ; account_update_id = 0
          ; actions
          }
        in
        let account : Account_id.t =
          Account_id.create account_update.body.public_key
            account_update.body.token_id
        in
        let previous =
          Option.value ~default:[] (Account_map.find_opt account t.actions_map)
        in
        t.actions_map <-
          Account_map.add account (action :: previous) t.actions_map

  let add_events t (account_update : Account_update.t) transaction_info =
    match account_update.body.events with
    | [] ->
        ()
    | events ->
        let event =
          { Account_update_events.block_info = Some Block_info.dummy
          ; transaction_info
          ; events
          }
        in
        let account : Account_id.t =
          Account_id.create account_update.body.public_key
            account_update.body.token_id
        in
        let previous =
          Option.value ~default:[] (Account_map.find_opt account t.events_map)
        in
        t.events_map <- Account_map.add account (event :: previous) t.events_map

  let add_account_update t (account_update : Account_update.t) account
      transaction_info =
    add_actions t account_update transaction_info account ;
    add_events t account_update transaction_info

  let get_actions t account_id from =
    let open Account_update_actions in
    let rec filter_start from actions =
      match actions with
      | [] ->
          []
      | ({ action_state; _ } as action) :: rest
        when Pickles_types.Vector.nth action_state 0 = from ->
          action :: rest
      | _ :: rest ->
          filter_start from rest
    in
    let all =
      List.rev
      @@ Option.value ~default:[]
           (Account_map.find_opt account_id t.actions_map)
    in
    (* If the `from` wasn't found return all *)
    (* Weird, but it's the same way in archive node *)
    match filter_start from all with [] -> all | actions -> actions

  let get_events t account_id =
    List.rev
    @@ Option.value ~default:[] (Account_map.find_opt account_id t.events_map)
end

include Archive
