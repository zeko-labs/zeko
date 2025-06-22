open Core_kernel
open Async
open Relational_db

module Context = struct
  type t = unit
end

module Merge = struct
  type t = int64

  let process _ x y = return Int64.(x + y)
end

module Base = struct
  type t = { data : int32; ready : unit Ivar.t }

  let to_yojson t : Yojson.Safe.t =
    `Assoc
      [ ("data", `Int (Int32.to_int_exn t.data))
      ; ("ready", `Bool (Ivar.is_empty t.ready))
      ]

  let of_yojson (json : Yojson.Safe.t) : t Ppx_deriving_yojson_runtime.error_or
      =
    let open Ppx_deriving_yojson_runtime in
    match json with
    | `Assoc [ ("data", `Int data); ("ready", `Bool ready) ] ->
        Ok
          { data = Int32.of_int data
          ; ready = (if ready then Ivar.create_full () else Ivar.create ())
          }
    | _ ->
        Error "Invalid JSON"

  let process _ x =
    let%map () = Ivar.read x.ready in
    Int64.(of_int32 x.data)
end

module Commit = struct
  type t = int

  type out = unit -> (unit, Caqti_error.t) Result.t Deferred.t

  let process _ _ _ = return (fun () -> return (Ok ()))
end

module Merger = Parallel_merger.Persisted.Make (struct
  include Parallel_merger.Make (Context) (Merge) (Base) (Commit)
  module Context = Context
  module Merge = Merge
  module Base = Base
  module Commit = Commit
end)

let logger =
  let l = Logger.create () in
  let () = Cli_lib.Stdout_log.setup false Logger.Level.Debug in
  l

let () =
  printf "Test basic functionality\n%!" ;
  let data =
    Quickcheck.random_value
      Quickcheck.Generator.(
        list_with_length 10 (list_with_length 20 Int32.quickcheck_generator))
  in
  Thread_safe.block_on_async_exn (fun () ->
      (* Calculate expected results *)
      let expected_results =
        List.map data ~f:(fun data ->
            List.sum (module Int64) ~f:Int64.of_int32_exn data )
      in

      (* Create merger *)
      let pool =
        Db.create_pool
          ~postgres_uri:
            (Uri.of_string
               "postgresql://postgres:postgres@localhost:5433/sequencer" )
          ()
        |> caqti_ok_exn ~msg:"Failed to create pool: %s"
      in
      let%bind state = Merger.create_and_requeue ~logger () pool in

      let data =
        List.map data ~f:(fun data ->
            List.map ~f:(fun data -> Base.{ data; ready = Ivar.create () }) data )
      in

      let all_results =
        Deferred.List.mapi ~how:`Sequential data ~f:(fun i data ->
            let%bind () =
              Deferred.List.iter ~how:`Sequential data ~f:(fun data ->
                  Merger.add_job pool state () ~data
                  >>| caqti_ok_exn ~msg:"Failed to add job: %s" )
            in
            Merger.commit_exn pool state () ~commit_witness:i )
      in

      let all_ivars =
        List.concat_map data ~f:(fun data ->
            List.map data ~f:(fun data -> data.ready) )
      in
      List.iter all_ivars ~f:(fun ivar -> Ivar.fill ivar ()) ;

      let%map all_results = all_results in
      [%test_eq: int64 list] all_results expected_results )

let () =
  printf "Test deleting of old trees\n%!" ;
  let data =
    Quickcheck.random_value
      Quickcheck.Generator.(
        list_with_length 10 (list_with_length 20 Int32.quickcheck_generator))
  in
  let data1, data2 = List.split_n data 5 in

  let pool =
    Db.create_pool
      ~postgres_uri:
        (Uri.of_string "postgresql://postgres:postgres@localhost:5433/sequencer")
      ()
    |> caqti_ok_exn ~msg:"Failed to create pool: %s"
  in

  Thread_safe.block_on_async_exn (fun () ->
      (* Calculate expected results *)
      let expected1 =
        List.map data1 ~f:(fun data ->
            List.sum (module Int64) ~f:Int64.of_int32_exn data )
      in
      let expected2 =
        List.map data2 ~f:(fun data ->
            List.sum (module Int64) ~f:Int64.of_int32_exn data )
      in

      (* Process first half *)
      let%bind () =
        (* Create merger *)
        let%bind state = Merger.create_and_requeue ~logger () pool in

        let data1 =
          List.map data1 ~f:(fun data ->
              List.map
                ~f:(fun data -> Base.{ data; ready = Ivar.create () })
                data )
        in
        let data2 =
          List.map data2 ~f:(fun data ->
              List.map
                ~f:(fun data -> Base.{ data; ready = Ivar.create () })
                data )
        in

        let results1 =
          Deferred.List.mapi ~how:`Sequential data1 ~f:(fun i data ->
              let%bind () =
                Deferred.List.iter ~how:`Sequential data ~f:(fun data ->
                    Merger.add_job pool state () ~data
                    >>| caqti_ok_exn ~msg:"Failed to add job: %s" )
              in
              Merger.commit_exn pool state () ~commit_witness:i )
        in

        (* These will not resolve now *)
        let results2 =
          let%bind _results1 = results1 in
          Deferred.List.iteri ~how:`Sequential data2 ~f:(fun i data ->
              let%map () =
                Deferred.List.iter ~how:`Sequential data ~f:(fun data ->
                    Merger.add_job pool state () ~data
                    >>| caqti_ok_exn ~msg:"Failed to add job: %s" )
              in
              don't_wait_for @@ Deferred.ignore_m
              @@ Merger.commit_exn pool state () ~commit_witness:i )
        in

        let ivars1 =
          List.concat_map data1 ~f:(fun data ->
              List.map data ~f:(fun data -> data.ready) )
        in
        List.iter ivars1 ~f:(fun ivar -> Ivar.fill ivar ()) ;

        let ivars2 =
          List.concat_map data2 ~f:(fun data ->
              List.map data ~f:(fun data -> data.ready) )
        in
        List.iter ivars2 ~f:(fun ivar -> assert (Ivar.is_empty ivar)) ;

        let%bind results1 = results1 in
        let%map () = results2 in
        [%test_eq: int64 list] expected1 results1
      in

      (* Process second half *)
      let%bind () =
        (* Create merger *)
        let%bind state = Merger.create_and_requeue ~logger () pool in

        let%map result = Merger.commit_exn pool state () ~commit_witness:0 in

        [%test_eq: int64] result (List.sum (module Int64) expected2 ~f:Fn.id)
      in

      return () )

let () = Core.print_endline "Done"
