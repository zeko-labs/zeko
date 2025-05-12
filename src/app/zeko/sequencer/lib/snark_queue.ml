open Core_kernel
open Async
open Mina_base

type t =
  { q : unit Throttle.t
  ; transfers_memory : Transfer.Transfers_memory.t
  ; provers : Zeko_prover.Client.t
  }

let create ~provers =
  { q = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1
  ; transfers_memory =
      Transfer.Transfers_memory.create ~lifetime:Float.(60. * 10.)
  ; provers
  }

let queue_size t = Throttle.num_jobs_waiting_to_start t.q

let enqueue t f =
  Throttle.enqueue t.q (fun () ->
      let%map result = f () in
      result )

let enqueue_prove_transfer_request t ~key ~zkapp_pk ~(transfer : Transfer.t) =
  Throttle.enqueue t.q (fun () ->
      let%bind result =
        try_with (fun () ->
            match transfer with
            | { direction = Deposit; transfer } ->
                Zeko_prover.Client.submit_deposit t.provers ~outer_pk:zkapp_pk
                  ~deposit:transfer
            | { direction = Withdraw; transfer } ->
                Zeko_prover.Client.submit_withdrawal t.provers
                  ~withdrawal:transfer )
      in
      let () =
        match result with
        | Ok tree ->
            Transfer.Transfers_memory.add t.transfers_memory key
              (Ok (Zkapp_command.Call_forest.cons_tree tree []))
        | Error e ->
            printf "Warning: prove_transfer_request failed %s\n%!"
              (Exn.to_string e) ;
            Transfer.Transfers_memory.add t.transfers_memory key
              (Error (Exn.to_string e))
      in
      return () )

let enqueue_prove_transfer_claim t ~key ~zkapp_pk ~(claim : Transfer.claim) =
  Throttle.enqueue t.q (fun () ->
      let%bind result =
        try_with (fun () ->
            match claim with
            | { transfer = { direction = Deposit; transfer }
              ; is_new
              ; pointer
              ; before
              ; after
              } ->
                Zeko_prover.Client.process_deposit t.provers ~is_new ~pointer
                  ~before ~after ~deposit:transfer
            | { transfer = { direction = Withdraw; transfer }
              ; is_new
              ; pointer
              ; before
              ; after
              } ->
                Zeko_prover.Client.process_withdrawal t.provers
                  ~outer_pk:zkapp_pk ~is_new ~pointer ~before ~after
                  ~withdrawal:transfer )
      in
      let () =
        match result with
        | Ok forest ->
            Transfer.Transfers_memory.add t.transfers_memory key (Ok forest)
        | Error e ->
            printf "Warning: prove_transfer_claim failed %s\n%!"
              (Exn.to_string e) ;
            Transfer.Transfers_memory.add t.transfers_memory key
              (Error (Exn.to_string e))
      in
      return () )

let wait_to_finish t = Throttle.capacity_available t.q
