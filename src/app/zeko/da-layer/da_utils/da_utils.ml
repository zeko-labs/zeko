open Async
open Core

external caml_get_batch_data :
  string -> string -> string -> (string, string) result = "caml_get_batch_data"

let get_batch_data da_websocket da_contract_address location :
    (string, Error.t) result Deferred.t =
  In_thread.run (fun () ->
      caml_get_batch_data da_websocket da_contract_address location
      |> Result.map_error ~f:(fun e -> Error.of_string e) )
