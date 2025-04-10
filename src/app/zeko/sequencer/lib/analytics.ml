open Core_kernel
open Async
open Mina_base
open Signature_lib
open Mina_ledger
module Field = Snark_params.Tick.Field

module User_activity = struct
  type t =
    { total_accounts : int; new_accounts_30d : int; active_accounts_30d : int }
end

module Zkapp_activity = struct
  type t = { total_zkapps : int; new_zkapps_30d : int; active_zkapps_30d : int }
end

module Transaction_activity = struct
  type t =
    { total_deposits : int
    ; total_signed_commands : int
    ; total_zkapp_commands : int
    }
end

module Transaction_statistics = struct
  type t = { avg_fee : float; avg_pfs : float (* proofs per second *) }
end

module Top_zkapps = struct
  type t = (string * int) list
end

module Lumina_activity = struct
  type t = { total_swaps : int; total_liquidity_pools : int }

  let get ~archive_uri ~zkapp_pk ~lumina_factory =
    let%bind lps =
      Gql_client.fetch_events archive_uri lumina_factory
      >>| List.join
      >>| List.filter_map ~f:(function
            | _sender_x :: _sender_oddity :: pool_x :: pool_oddity :: _ ->
                (* only one event type, so no index *)
                (* https://github.com/Lumina-DEX/lumina-mvp/blob/284341acc924bf21ac6947514e23cce7088e68ed/contracts/src/PoolFactory.ts#L39 *)
                Some
                  ( { x = pool_x
                    ; is_odd =
                        ( if Field.equal pool_oddity Field.zero then false
                        else true )
                    }
                    : Public_key.Compressed.t )
            | _ ->
                None )
    in
    let%bind swaps =
      Deferred.List.map ~how:`Parallel lps ~f:(fun lp ->
          Gql_client.fetch_events archive_uri lp
          >>| List.join
          >>| List.filter ~f:(function
                | event_type :: _ ->
                    (* [1] is event type [addLiquidity] *)
                    (* https://github.com/Lumina-DEX/lumina-mvp/blob/284341acc924bf21ac6947514e23cce7088e68ed/contracts/src/PoolMina.ts#L61 *)
                    if Field.equal event_type Field.one then true else false
                | _ ->
                    false ) )
      >>| List.length
    in
    return { total_swaps = swaps; total_liquidity_pools = List.length lps }
end

module State = struct
  type timestamps = { created_at : Time.t; last_active_at : Time.t }

  type avg_fee = { total_fee : int; total_commands : int }

  type t =
    { account_map : timestamps Account_id.Map.t
    ; zkapp_map : timestamps Account_id.Map.t
    ; total_signed_commands : int
    ; total_zkapp_commands : int
    ; avg_fee : avg_fee
    }

  let empty =
    { account_map = Account_id.Map.empty
    ; zkapp_map = Account_id.Map.empty
    ; total_signed_commands = 0
    ; total_zkapp_commands = 0
    ; avg_fee = { total_fee = 0; total_commands = 0 }
    }

  let update_with_command t command ledger =
    let now = Time.now () in
    let accounts = User_command.accounts_accessed command Applied in
    let t =
      List.fold accounts ~init:t ~f:(fun t (account_id, _) ->
          { t with
            account_map =
              Map.update t.account_map account_id ~f:(function
                | None ->
                    { created_at = now; last_active_at = now }
                | Some ts ->
                    { ts with last_active_at = now } )
          ; zkapp_map =
              (let account =
                 let%bind.Option location =
                   Ledger.location_of_account ledger account_id
                 in
                 Ledger.get ledger location
               in
               match account with
               | Some { zkapp = Some zkapp; _ } ->
                   Map.update t.zkapp_map account_id ~f:(function
                     | None ->
                         { created_at = now; last_active_at = now }
                     | Some ts ->
                         { ts with last_active_at = now } )
               | _ ->
                   t.zkapp_map )
          } )
    in
    let t =
      { t with
        avg_fee =
          { total_fee =
              t.avg_fee.total_fee
              + (Currency.Fee.to_mina_int @@ User_command.fee command)
          ; total_commands = t.avg_fee.total_commands + 1
          }
      }
    in
    match command with
    | Signed_command _ ->
        { t with total_signed_commands = t.total_signed_commands + 1 }
    | Zkapp_command _ ->
        { t with total_zkapp_commands = t.total_zkapp_commands + 1 }
end

type t =
  { user_activity : User_activity.t
  ; zkapp_activity : Zkapp_activity.t
  ; transaction_activity : Transaction_activity.t
  ; transaction_statistics : Transaction_statistics.t
  ; top_zkapps : Top_zkapps.t
  ; lumina_activity : Lumina_activity.t
  }

let get (state : State.t) ~archive_uri ~zkapp_pk ~lumina_factory =
  let now = Time.now () in
  let before_30d = Time.(sub (now ()) (Span.of_day 30.)) in
  let user_activity =
    User_activity.
      { total_accounts = Map.length state.account_map
      ; new_accounts_30d =
          Map.filter state.account_map ~f:(fun ts ->
              Time.between ts.created_at ~low:before_30d ~high:now )
          |> Map.length
      ; active_accounts_30d =
          Map.filter state.account_map ~f:(fun ts ->
              Time.between ts.last_active_at ~low:before_30d ~high:now )
          |> Map.length
      }
  in
  let zkapp_activity =
    Zkapp_activity.
      { total_zkapps = Map.length state.zkapp_map
      ; new_zkapps_30d =
          Map.filter state.zkapp_map ~f:(fun ts ->
              Time.between ts.created_at ~low:before_30d ~high:now )
          |> Map.length
      ; active_zkapps_30d =
          Map.filter state.zkapp_map ~f:(fun ts ->
              Time.between ts.last_active_at ~low:before_30d ~high:now )
          |> Map.length
      }
  in
  let%bind deposits =
    Gql_client.fetch_transfers archive_uri zkapp_pk >>| List.length
  in
  let transaction_activity =
    Transaction_activity.
      { total_deposits = deposits
      ; total_signed_commands = state.total_signed_commands
      ; total_zkapp_commands = state.total_zkapp_commands
      }
  in
  let transaction_statistics =
    Transaction_statistics.
      { avg_fee =
          Float.( / )
            (Int.to_float state.avg_fee.total_fee)
            (Int.to_float state.avg_fee.total_commands)
      ; avg_pfs = 0. (* TODO: when I do parallel proving will update this *)
      }
  in
  let top_zkapps = Top_zkapps.[] in
  let%bind lumina_activity =
    Lumina_activity.get ~archive_uri ~zkapp_pk ~lumina_factory
  in
  return
    { user_activity
    ; zkapp_activity
    ; transaction_activity
    ; transaction_statistics
    ; top_zkapps
    ; lumina_activity
    }
