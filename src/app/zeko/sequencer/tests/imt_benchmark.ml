open Core_kernel
open Mina_base
open Indexed_merkle_tree

let n = 5_000

let tids = List.init n ~f:(fun _ -> Field.random () |> Token_id.of_field)

let () =
  print_endline "In_memory" ;
  let start_time = Time.now () in
  let t = In_memory.create ~depth:35 () in
  List.iter tids ~f:(fun tid -> In_memory.insert_exn t tid) ;
  let duration = Time.diff (Time.now ()) start_time in
  printf !"Time taken: %s\n%!" (Time.Span.to_string_hum duration) ;
  printf !"Merkle root: %s\n\n%!"
    (In_memory.merkle_root t |> Ledger_hash.to_decimal_string)

let () =
  print_endline "Db" ;
  let start_time = Time.now () in
  let t = Db.create ~depth:35 () in
  List.iter tids ~f:(fun tid ->
      ignore
        (Db.get_or_create_entry_exn t tid : [ `Added | `Existed ] * Db.witness) ) ;
  let duration = Time.diff (Time.now ()) start_time in
  printf !"Db Time taken: %s\n%!" (Time.Span.to_string_hum duration) ;
  printf
    !"Db Merkle root: %s\n\n%!"
    (Db.merkle_root t |> Ledger_hash.to_decimal_string)

let () =
  print_endline "Batch In_memory" ;
  let start_time = Time.now () in
  let t = In_memory.create ~depth:35 () in
  In_memory.insert_batch_exn t tids ;
  let duration = Time.diff (Time.now ()) start_time in
  printf !"Time taken: %s\n%!" (Time.Span.to_string_hum duration) ;
  printf !"Merkle root: %s\n\n%!"
    (In_memory.merkle_root t |> Ledger_hash.to_decimal_string)
