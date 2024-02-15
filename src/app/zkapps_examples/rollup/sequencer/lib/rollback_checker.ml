open Core_kernel
open Async_kernel
open Signature_lib
open Mina_base

module Rollback_checker = struct
  type t =
    { mutable last_state_hash : string
    ; mutable last_rollup_state : Frozen_ledger_hash.t
    ; zkapp_pk : Public_key.Compressed.t
    ; interval : Time_ns.Span.t
    ; uri : Uri.t Cli_lib.Flag.Types.with_name
    }

  let create zkapp_pk interval uri =
    let%bind chain = Gql_client.fetch_best_chain uri in
    let%bind last_rollup_state = Gql_client.fetch_commited_state uri zkapp_pk in
    let last_rollup_state =
      Frozen_ledger_hash.of_decimal_string @@ List.nth_exn last_rollup_state 0
    in
    let last_state_hash = List.last_exn chain in
    return { last_state_hash; last_rollup_state; zkapp_pk; interval; uri }

  let check t =
    let%bind.Deferred.Result chain =
      try_with (fun () -> Gql_client.fetch_best_chain t.uri)
    in
    let%bind.Deferred.Result last_rollup_state =
      try_with (fun () -> Gql_client.fetch_commited_state t.uri t.zkapp_pk)
    in
    let last_rollup_state =
      Frozen_ledger_hash.of_decimal_string @@ List.nth_exn last_rollup_state 0
    in

    let last_state_hash = List.last_exn chain in
    let chain_rollback_happened =
      not @@ List.mem chain t.last_state_hash ~equal:String.equal
    in
    t.last_state_hash <- last_state_hash ;

    let rollup_state_changed =
      not @@ Frozen_ledger_hash.equal t.last_rollup_state last_rollup_state
    in
    t.last_rollup_state <- last_rollup_state ;

    return (Ok (chain_rollback_happened && rollup_state_changed))

  let run_checker t ~on_rollback =
    every ~start:(after t.interval) t.interval (fun () ->
        don't_wait_for
          (let%bind rollback_happened =
             match%bind check t with
             | Ok rollback_happened ->
                 return rollback_happened
             | Error err ->
                 print_endline (Exn.to_string err) ;
                 return false
           in
           print_endline
             ("Rollback happened: " ^ Bool.to_string rollback_happened) ;
           if rollback_happened then on_rollback () else Deferred.unit ) )
end

include Rollback_checker
