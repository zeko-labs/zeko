open Core_kernel
open Async

let generate_id () = Uuid_unix.create () |> Uuid.to_string

module Make (Context : sig
  type t

  val created_new_tree : t -> unit
end) (Merge : sig
  type t [@@deriving yojson]

  val process : Context.t -> t -> t -> t Deferred.t
end) (Base : sig
  type t [@@deriving yojson]

  val process : Context.t -> t -> Merge.t Deferred.t
end) (Commit : sig
  type t

  val process : Context.t -> t -> Merge.t -> unit Deferred.t
end) =
struct
  module Available_job = struct
    type t = Base of Base.t | Merge of Merge.t * Merge.t [@@deriving yojson]
  end

  (* Finished job will be always of type `Merge.t` *)
  module Finished_job = struct
    type t = Merge.t [@@deriving yojson]
  end

  module Job_status = struct
    type t = Todo of Available_job.t | Done of Finished_job.t
    [@@deriving yojson]
  end

  module With_id = struct
    type 'd t = { id : string; value : 'd } [@@deriving sexp, yojson]
  end

  module Ivar = struct
    include Ivar

    let to_yojson _ _ = `String "<opaque>"

    let of_yojson _ _ = Ok (Ivar.create ())
  end

  (* We store the jobs in a list because we care only about the actionable jobs, not the old ones *)
  module Tree = struct
    type t =
      { mutable jobs : Job_status.t With_id.t list
      ; mutable closed : bool  (** No new jobs can be added *)
      ; finished : Finished_job.t Ivar.t  (** All jobs are done *)
      ; ready_to_commit : unit Ivar.t
            (** All jobs are done and ready to commit *)
      }
    [@@deriving yojson]

    let create () =
      { jobs = []
      ; closed = false
      ; finished = Ivar.create ()
      ; ready_to_commit = Ivar.create ()
      }

    let close t = t.closed <- true

    let wait_till_finished t = Ivar.read t.finished

    let append_base t ~id ~(data : Base.t) =
      t.jobs <- t.jobs @ [ With_id.{ id; value = Job_status.Todo (Base data) } ]

    let check_if_it's_ready_to_commit t =
      match t with
      | { closed = true
        ; jobs = [ { value = Done _; _ } ] (* One job with status done *)
        ; _
        } ->
          Ivar.fill_if_empty t.ready_to_commit ()
      | _ ->
          ()

    let commit t ctx ~commit_witness =
      close t ;
      check_if_it's_ready_to_commit t ;
      let%bind () = Ivar.read t.ready_to_commit in
      match t with
      | { closed = true
        ; jobs = [ { value = Done job; _ } ] (* One job with status done *)
        ; _
        } ->
          let%bind () = Commit.process ctx commit_witness job in
          let () = Ivar.fill t.finished job in
          return job
      | _ ->
          failwith "Invalid state"

    (* If finishing job created opportunity to merge, start merging *)
    (* If it's already closed and it's last job, mark as finished *)
    let rec finish_job_exn t ctx ~id ~(data : Merge.t) =
      (* Mark job as Done *)
      let found, new_jobs =
        List.fold_map t.jobs ~init:false ~f:(fun found job ->
            match job with
            | With_id.{ value = Job_status.Todo _; _ } when String.(job.id = id)
              ->
                (true, { job with value = Job_status.Done data })
            | _ ->
                (found, job) )
      in
      if not found then failwithf "Job with id '%s' not found" id () ;
      t.jobs <- new_jobs ;

      (* merge only the first opportunity *)
      (* finishing job can't produce more than 1 opportunity to merge *)
      let rec merge_done_jobs l =
        let open With_id in
        match l with
        (* first done && second done *)
        | { value = Job_status.Done fst_data; _ }
          :: { value = Done snd_data; _ } :: rest ->
            let job =
              { id = generate_id ()
              ; value = Job_status.Todo (Merge (fst_data, snd_data))
              }
            in
            (Some job, job :: rest)
        | head :: tail ->
            let merge_job_opt, jobs = merge_done_jobs tail in
            (merge_job_opt, head :: jobs)
        | [] ->
            (None, [])
      in
      (* Create Todo merge job *)
      let merge_job_opt, new_jobs = merge_done_jobs t.jobs in
      t.jobs <- new_jobs ;
      match merge_job_opt with
      | None ->
          check_if_it's_ready_to_commit t ;
          return ()
      | Some { value = Todo (Merge (fst, snd)); id } ->
          let%bind result = Merge.process ctx fst snd in
          finish_job_exn t ctx ~id ~data:result
      | Some _ ->
          failwith "Invalid merge job"

    let add_job_exn t ctx ~id ~(data : Base.t) =
      if t.closed then failwith "Tree has been already closed" ;
      append_base t ~id ~data ;
      let%bind result = Base.process ctx data in
      finish_job_exn t ctx ~id ~data:result

    let result t =
      match t with
      | [ With_id.{ value = Job_status.Done result; _ } ] ->
          Some result
      | _ ->
          None

    let number_of_wip_jobs t =
      List.count t.jobs ~f:(function
        | { value = Todo _; _ } ->
            true
        | _ ->
            false )

    let is_empty t = List.is_empty t.jobs
  end

  type t = { mutable trees : Tree.t list } [@@deriving yojson]

  let pp t = Core.printf "%s\n%!" (Yojson.Safe.pretty_to_string @@ to_yojson t)

  let create () = { trees = [] }

  let start_new_tree t ctx =
    Context.created_new_tree ctx ;
    t.trees <- t.trees @ [ Tree.create () ]

  let commit_exn t ctx ~commit_witness =
    (* Create new tree before waiting, so new transactions go there *)
    start_new_tree t ctx ;
    match List.rev t.trees with
    | _just_created :: last :: rest ->
        Tree.close last ;
        let%bind _ =
          Deferred.List.iter rest ~f:(fun tree ->
              let%bind _ = Tree.wait_till_finished tree in
              return () )
        in
        Tree.commit last ctx ~commit_witness
    | _ ->
        failwith "No trees to commit"

  let rec add_job t context ~(data : Base.t) =
    match List.last t.trees with
    | None ->
        start_new_tree t context ; add_job t context ~data
    | Some last ->
        Tree.add_job_exn last context ~id:(generate_id ()) ~data

  let pending_jobs t =
    List.concat t
    |> List.filter_map ~f:(function
         | With_id.{ id; value = Job_status.Todo job } ->
             Some With_id.{ id; value = job }
         | _ ->
             None )

  let number_of_wip_jobs t =
    List.sum (module Int) t.trees ~f:(fun tree -> Tree.number_of_wip_jobs tree)

  let current_tree t = List.last t.trees
end

let%test_module "parallel_merge on (+)" =
  ( module struct
    let () = Backtrace.elide := false

    let printf = Core.printf

    module Context = struct
      type t = int list ref

      let create () = ref []

      let get t = !t

      (* let pp () = printf !"%{sexp: int list}\n%!" (get ()) *)

      let add t x = t := !t @ [ x ]

      let created_new_tree _ = ()
    end

    module Merge = struct
      type t = int64 [@@deriving yojson]

      let process _ x y =
        let time = Quickcheck.random_value (Float.gen_incl 0.0 0.1) in
        let%bind () = Clock.after (Time.Span.of_sec time) in
        return Int64.(x + y)
    end

    module Base = struct
      type t = int32 [@@deriving yojson]

      let process _ x =
        let time = Quickcheck.random_value (Float.gen_incl 0.0 0.1) in
        let%bind () = Clock.after (Time.Span.of_sec time) in
        return Int64.(of_int32_exn x)
    end

    module Commit = struct
      type t = int [@@deriving yojson]

      let process ctx commit_witness _ =
        let time = Quickcheck.random_value (Float.gen_incl 0.0 0.1) in
        let%bind () = Clock.after (Time.Span.of_sec time) in
        Context.add ctx commit_witness ;
        return ()
    end

    module Merger = Make (Context) (Merge) (Base) (Commit)

    let%test_unit "one tree" =
      printf "Testing one tree\n%!" ;
      let g = Quickcheck.Generator.list_non_empty Int32.quickcheck_generator in
      Quickcheck.test g ~trials:20 ~f:(fun data ->
          let ctx = Context.create () in
          let expected_result =
            List.sum (module Int64) ~f:Int64.of_int32_exn data
          in
          let state = Merger.create () in

          let final_result =
            Thread_safe.block_on_async_exn (fun () ->
                (* Create jobs *)
                let%bind () =
                  Deferred.List.iter ~how:`Parallel data ~f:(fun data ->
                      Merger.add_job state ctx ~data )
                in

                Merger.commit_exn state ctx ~commit_witness:42 )
          in

          [%test_eq: int64] final_result expected_result )

    let%test_unit "multiple trees" =
      printf "Testing multiple trees\n%!" ;
      let g =
        Quickcheck.Generator.(
          list_non_empty @@ list_non_empty Int32.quickcheck_generator)
      in
      Quickcheck.test g ~trials:20 ~f:(fun data ->
          let ctx = Context.create () in
          let expected_results =
            List.map data ~f:(fun data ->
                List.sum (module Int64) ~f:Int64.of_int32_exn data )
          in

          let state = Merger.create () in

          let results =
            Thread_safe.block_on_async_exn (fun () ->
                Deferred.List.mapi ~how:`Parallel data ~f:(fun i data ->
                    let () =
                      List.iter data ~f:(fun data ->
                          don't_wait_for @@ Merger.add_job state ctx ~data )
                    in
                    Merger.commit_exn state ctx ~commit_witness:i ) )
          in

          let expected_order = List.mapi data ~f:(fun i _ -> i) in
          [%test_eq: int list] (Context.get ctx) expected_order ;

          [%test_eq: int64 list] results expected_results )

    let%test_unit "test fast batch after slow batch" =
      printf "Testing fast batch after slow batch\n%!" ;
      let ctx = Context.create () in

      let state = Merger.create () in

      Thread_safe.block_on_async_exn (fun () ->
          don't_wait_for @@ Merger.add_job state ctx ~data:(Int32.of_int_exn 42) ;
          don't_wait_for @@ Merger.add_job state ctx ~data:(Int32.of_int_exn 42) ;
          don't_wait_for @@ Merger.add_job state ctx ~data:(Int32.of_int_exn 42) ;
          don't_wait_for @@ Merger.add_job state ctx ~data:(Int32.of_int_exn 42) ;
          don't_wait_for @@ Merger.add_job state ctx ~data:(Int32.of_int_exn 42) ;
          ( don't_wait_for
          @@ let%map _ = Merger.commit_exn state ctx ~commit_witness:0 in
             () ) ;

          don't_wait_for @@ Merger.add_job state ctx ~data:(Int32.of_int_exn 42) ;
          let%bind _ = Merger.commit_exn state ctx ~commit_witness:1 in

          return () ) ;

      let expected_order = [ 0; 1 ] in
      [%test_eq: int list] (Context.get ctx) expected_order
  end )
