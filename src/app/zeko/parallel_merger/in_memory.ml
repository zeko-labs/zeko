open Core_kernel
open Async

let generate_id () = Uuid_unix.create () |> Uuid.to_string

module type Intf = sig
  module Context : sig
    type t
  end

  module Merge : sig
    type t

    val process : Context.t -> t -> t -> t Deferred.t
  end

  module Base : sig
    type t [@@deriving yojson]

    val process : Context.t -> t -> Merge.t Deferred.t
  end

  module Tree : sig
    type t

    type id = string

    val wait_till_finished : t -> Merge.t Deferred.t

    val base_jobs_count : t -> int
  end

  module Commit : sig
    type t

    type out

    val process : Context.t -> t -> Merge.t -> out Deferred.t
  end

  module With_id : sig
    type 'd t = { id : string; value : 'd }
  end

  type t

  val create : unit -> t

  val add_job : t -> Context.t -> data:Base.t -> Tree.id

  val commit_exn :
       t
    -> Context.t
    -> commit_witness:Commit.t
    -> (Commit.out * Tree.id * Merge.t) Deferred.t

  val current_tree : t -> Tree.t With_id.t option
end

module Make (Context : sig
  type t
end) (Merge : sig
  type t

  val process : Context.t -> t -> t -> t Deferred.t
end) (Base : sig
  type t [@@deriving yojson]

  val process : Context.t -> t -> Merge.t Deferred.t
end) (Commit : sig
  type t

  type out

  val process : Context.t -> t -> Merge.t -> out Deferred.t
end) :
  Intf
    with module Context := Context
     and module Merge := Merge
     and module Base := Base
     and module Commit := Commit = struct
  module Available_job = struct
    type t = Base of Base.t | Merge of Merge.t * Merge.t
  end

  (* Finished job will be always of type `Merge.t` *)
  module Finished_job = struct
    type t = Merge.t
  end

  module Job_status = struct
    type t = Todo of Available_job.t | Done of Finished_job.t
  end

  module With_id = struct
    type 'd t = { id : string; value : 'd }
  end

  (* We store the jobs in a list because we care only about the actionable jobs, not the old ones *)
  module Tree = struct
    type t =
      { mutable jobs : Job_status.t With_id.t list
      ; mutable closed : bool  (** No new jobs can be added *)
      ; finished : Finished_job.t Ivar.t  (** All jobs are done *)
      ; ready_to_commit : unit Ivar.t
            (** All jobs are done and ready to commit *)
      ; mutable base_jobs : int  (** Number of base jobs added to the tree *)
      }

    type id = string

    let create () =
      { jobs = []
      ; closed = false
      ; finished = Ivar.create ()
      ; ready_to_commit = Ivar.create ()
      ; base_jobs = 0
      }

    let close t = t.closed <- true

    let wait_till_finished t = Ivar.read t.finished

    let append_base t ~id ~(data : Base.t) =
      t.jobs <- t.jobs @ [ With_id.{ id; value = Job_status.Todo (Base data) } ] ;
      t.base_jobs <- t.base_jobs + 1

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
          let%bind result = Commit.process ctx commit_witness job in
          let () = Ivar.fill t.finished job in
          return (result, job)
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

    let base_jobs_count t = t.base_jobs
  end

  type t = { mutable trees : Tree.t With_id.t list }

  let create () = { trees = [] }

  let start_new_tree t =
    t.trees <- t.trees @ [ { id = generate_id (); value = Tree.create () } ]

  let commit_exn t ctx ~commit_witness =
    (* Create new tree before waiting, so new transactions go there *)
    start_new_tree t ;
    match List.rev t.trees with
    | _just_created :: ({ value = { jobs = []; _ }; _ } as _last) :: _rest ->
        failwith "Nothing to commit"
    | _just_created :: last :: rest ->
        Tree.close last.value ;
        let%bind _ =
          Deferred.List.iter rest ~f:(fun tree ->
              let%bind _ = Tree.wait_till_finished tree.value in
              return () )
        in
        let%map out, last_job = Tree.commit last.value ctx ~commit_witness in
        (out, last.id, last_job)
    | _ ->
        failwith "No trees to commit"

  let rec add_job t context ~(data : Base.t) =
    match List.last t.trees with
    | None ->
        start_new_tree t ; add_job t context ~data
    | Some last ->
        let () =
          don't_wait_for
            (Tree.add_job_exn last.value context ~id:(generate_id ()) ~data)
        in
        last.id

  let current_tree t = List.last t.trees
end

let%test_module "in_memory parallel_merge on (+)" =
  ( module struct
    let () = Backtrace.elide := false

    let printf = Core.printf

    module Context = struct
      type t = int list ref

      let create () = ref []

      let get t = !t

      (* let pp () = printf !"%{sexp: int list}\n%!" (get ()) *)

      let add t x = t := !t @ [ x ]
    end

    module Merge = struct
      type t = int64

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
      type t = int

      type out = int64

      let process ctx commit_witness out =
        let time = Quickcheck.random_value (Float.gen_incl 0.0 0.1) in
        let%bind () = Clock.after (Time.Span.of_sec time) in
        Context.add ctx commit_witness ;
        return out
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

          let _out, _tid, final_result =
            Thread_safe.block_on_async_exn (fun () ->
                (* Create jobs *)
                let () =
                  List.iter data ~f:(fun data ->
                      (Merger.add_job state ctx ~data : Merger.Tree.id)
                      |> ignore )
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
                          (Merger.add_job state ctx ~data : Merger.Tree.id)
                          |> ignore )
                    in
                    Merger.commit_exn state ctx ~commit_witness:i )
                >>| List.map ~f:trd3 )
          in

          let expected_order = List.mapi data ~f:(fun i _ -> i) in
          [%test_eq: int list] (Context.get ctx) expected_order ;

          [%test_eq: int64 list] results expected_results )

    let%test_unit "test fast batch after slow batch" =
      printf "Testing fast batch after slow batch\n%!" ;
      let ctx = Context.create () in

      let state = Merger.create () in

      Thread_safe.block_on_async_exn (fun () ->
          (Merger.add_job state ctx ~data:(Int32.of_int_exn 42) : Merger.Tree.id)
          |> ignore ;
          (Merger.add_job state ctx ~data:(Int32.of_int_exn 42) : Merger.Tree.id)
          |> ignore ;
          (Merger.add_job state ctx ~data:(Int32.of_int_exn 42) : Merger.Tree.id)
          |> ignore ;
          (Merger.add_job state ctx ~data:(Int32.of_int_exn 42) : Merger.Tree.id)
          |> ignore ;
          (Merger.add_job state ctx ~data:(Int32.of_int_exn 42) : Merger.Tree.id)
          |> ignore ;
          ( don't_wait_for
          @@ let%map _ = Merger.commit_exn state ctx ~commit_witness:0 in
             () ) ;

          (Merger.add_job state ctx ~data:(Int32.of_int_exn 42) : Merger.Tree.id)
          |> ignore ;
          let%bind _ = Merger.commit_exn state ctx ~commit_witness:1 in

          return () ) ;

      let expected_order = [ 0; 1 ] in
      [%test_eq: int list] (Context.get ctx) expected_order
  end )
