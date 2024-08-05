open Core
open Async
open Sequencer_lib
open Mina_base
module L = Mina_ledger.Ledger

let constraint_constants = Genesis_constants.Constraint_constants.compiled

let run f = Thread_safe.block_on_async_exn f

type account_record = L.Db.Location.t * Mina_base.Account.t [@@deriving sexp]

let dump =
  Command.basic ~summary:""
    (let%map_open.Command db_dir = flag "--db-dir" (required string) ~doc:"" in
     fun () ->
       let db =
         L.Db.create ~directory_name:db_dir
           ~depth:constraint_constants.ledger_depth ()
       in
       let account_ids = run (fun () -> L.Db.accounts db) |> Set.to_list in
       let accounts =
         List.map account_ids ~f:(fun aid ->
             let location =
               L.Db.location_of_account db aid |> Option.value_exn
             in
             let account = L.Db.get db location |> Option.value_exn in
             (location, account) )
       in
       Core.eprintf "Ledger root: %s\n%!"
         (Ledger_hash.to_decimal_string @@ L.Db.merkle_root db) ;
       Core.printf !"%{sexp: account_record list}\n%!" accounts )

let create =
  Command.basic ~summary:""
    (let%map_open.Command db_dir = flag "--db-dir" (required string) ~doc:""
     and dump_file = flag "--dump-file" (required string) ~doc:"" in
     fun () ->
       let db =
         L.Db.create ~directory_name:db_dir
           ~depth:constraint_constants.ledger_depth ()
       in
       let accounts =
         Sexp.load_sexp dump_file |> [%of_sexp: account_record list]
       in
       L.Db.set_batch db accounts ;
       Core.printf "Done with root: %s\n%!"
         (Ledger_hash.to_decimal_string @@ L.Db.merkle_root db) )

let () =
  Command_unix.run
    (Command.group ~summary:"" [ ("dump", dump); ("create", create) ])
