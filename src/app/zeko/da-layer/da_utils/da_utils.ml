(** 
  These functions call external C functions that call rust functions defined in rust library
  These are long running functions with IO operations, so they are run in separate threads,
  C code calls `caml_release_runtime_system` so that other threads can run in parallel.    
*)
open Async

open Core

external caml_post_batch :
  string -> string -> string -> string -> string list -> (string, string) result
  = "caml_post_batch"

let post_batch ~da_websocket ~da_contract_address ~da_private_key ~batch_data
    ~sig_data_without_location : (string, Error.t) result Deferred.t =
  In_thread.run (fun () ->
      caml_post_batch da_websocket da_contract_address da_private_key batch_data
        sig_data_without_location
      |> Result.map_error ~f:(fun e -> Error.of_string e) )

external caml_get_batch_data :
  string -> string -> string -> (string, string) result = "caml_get_batch_data"

let get_batch_data ~da_websocket ~da_contract_address ~location :
    (string, Error.t) result Deferred.t =
  In_thread.run (fun () ->
      caml_get_batch_data da_websocket da_contract_address location
      |> Result.map_error ~f:(fun e -> Error.of_string e) )

external caml_init_genesis_state :
  string -> string -> string -> string -> (unit, string) result
  = "caml_init_genesis_state"

let init_genesis_state ~da_websocket ~da_contract_address ~da_private_key ~data
    : (unit, Error.t) result Deferred.t =
  In_thread.run (fun () ->
      caml_init_genesis_state da_websocket da_contract_address da_private_key
        data
      |> Result.map_error ~f:(fun e -> Error.of_string e) )

external caml_get_genesis_state : string -> string -> (string, string) result
  = "caml_get_genesis_state"

let get_genesis_state ~da_websocket ~da_contract_address :
    (string, Error.t) result Deferred.t =
  In_thread.run (fun () ->
      caml_get_genesis_state da_websocket da_contract_address
      |> Result.map_error ~f:(fun e -> Error.of_string e) )

external caml_deploy :
  string -> string -> Unsigned.uint64 -> string list -> (string, string) result
  = "caml_deploy"

let deploy ~da_websocket ~da_private_key ~quorum ~validators :
    (string, Error.t) result Deferred.t =
  In_thread.run (fun () ->
      caml_deploy da_websocket da_private_key quorum validators
      |> Result.map_error ~f:(fun e -> Error.of_string e) )

external caml_post_batch_signature :
     string
  -> string
  -> string
  -> string
  -> string
  -> string
  -> string
  -> (unit, string) result
  = "caml_post_batch_signature_bytecode" "caml_post_batch_signature"

let post_batch_signature ~da_websocket ~da_contract_address ~da_private_key
    ~location ~mina_pk ~sig_rx ~sig_s : (unit, Error.t) result Deferred.t =
  In_thread.run (fun () ->
      caml_post_batch_signature da_websocket da_contract_address da_private_key
        location mina_pk sig_rx sig_s
      |> Result.map_error ~f:(fun e -> Error.of_string e) )

external caml_get_batch_signatures :
  string -> string -> string -> (string, string) result
  = "caml_get_batch_signatures"

let get_batch_signatures ~da_websocket ~da_contract_address ~location :
    (string, Error.t) result Deferred.t =
  In_thread.run (fun () ->
      caml_get_batch_signatures da_websocket da_contract_address location
      |> Result.map_error ~f:(fun e -> Error.of_string e) )
