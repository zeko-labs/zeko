open Core_kernel
open Async_kernel

let dispatch ?(max_tries = 5) ~logger
    (node_location : Host_and_port.t Cli_lib.Flag.Types.with_name) rpc data =
  let rec go tries_left errs =
    if Int.( <= ) tries_left 0 then
      let e = Error.of_list (List.rev errs) in
      return
        (Error
           (Error.tag_arg e
              (sprintf
                 "Could not send query to da node after %d tries. The process \
                  may not be running, please check the daemon-argument"
                 max_tries )
              ( ("host_and_port", node_location.value)
              , ("daemon-argument", node_location.name) )
              [%sexp_of: (string * Host_and_port.t) * (string * string)] ) )
    else
      match%bind Daemon_rpcs.Client.dispatch rpc data node_location.value with
      | Ok result ->
          return (Ok result)
      | Error e ->
          [%log error] "Error sending data to the da node $error. Retrying..."
            ~metadata:[ ("error", `String (Error.to_string_hum e)) ] ;
          go (tries_left - 1) (e :: errs)
  in
  go max_tries []

let post_batch ~logger ~node_location ~ledger_openings ~batch =
  dispatch ~logger node_location Rpc.Post_batch.v1 { ledger_openings; batch }

let get_batch ~logger ~node_location ~ledger_hash :
    (Batch.t option, Error.t) result Deferred.t =
  dispatch ~logger node_location Rpc.Get_batch.v1 ledger_hash

let get_all_keys ~logger ~node_location () =
  dispatch ~logger node_location Rpc.Get_all_keys.v1 ()
