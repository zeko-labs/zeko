open Async
open Core

external caml_post_batch :
  string -> string -> string -> string -> string list -> (string, string) result
  = "caml_post_batch"

let post_batch ~da_websocket ~da_contract_address ~da_private_key ~batch_data
    ~sig_data : (string, Error.t) result Deferred.t =
  In_thread.run (fun () ->
      caml_post_batch da_websocket da_contract_address da_private_key batch_data
        sig_data
      |> Result.map_error ~f:(fun e -> Error.of_string e) )

external caml_get_batch_data :
  string -> string -> string -> (string, string) result = "caml_get_batch_data"

let get_batch_data ~da_websocket ~da_contract_address ~location :
    (string, Error.t) result Deferred.t =
  In_thread.run (fun () ->
      caml_get_batch_data da_websocket da_contract_address location
      |> Result.map_error ~f:(fun e -> Error.of_string e) )

external caml_get_genesis_state : string -> string -> (string, string) result
  = "caml_get_genesis_state"

let get_genesis_state ~da_websocket ~da_contract_address :
    (string, Error.t) result Deferred.t =
  In_thread.run (fun () ->
      caml_get_genesis_state da_websocket da_contract_address
      |> Result.map_error ~f:(fun e -> Error.of_string e) )
