open Core_kernel

module Make (Base : sig
  type t [@@deriving sexp, yojson]

  val derive_id : t -> string
end) (Merge : sig
  type t [@@deriving sexp, yojson]

  val derive_id : t -> t -> string
end) =
struct
  module Available_job = struct
    type t = Base of Base.t | Merge of Merge.t * Merge.t
    [@@deriving sexp, yojson]
  end

  (* Finished job will be always of type `Merge.t` *)
  module Finished_job = struct
    type t = Merge.t [@@deriving sexp, yojson]
  end

  module Job_status = struct
    type t = Todo of Available_job.t | Done of Finished_job.t
    [@@deriving sexp, yojson]
  end

  module With_id = struct
    type 'd t = { id : string; value : 'd } [@@deriving sexp, yojson]
  end

  (* We store the jobs in a list because we care only about the actionable jobs, not the old ones *)
  module Tree = struct
    type t = Job_status.t With_id.t list [@@deriving sexp, yojson]

    let add_job t ~(data : Base.t) =
      t
      @ [ With_id.
            { id = Base.derive_id data; value = Job_status.Todo (Base data) }
        ]

    (* merge only the first opportunity *)
    (* finishing job can't produce more than 1 opportunity to merge *)
    let rec merge_done_jobs t =
      let open With_id in
      match t with
      (* first done && second done *)
      | { value = Job_status.Done fst_data; _ }
        :: { value = Done snd_data; _ } :: rest ->
          { id = Merge.derive_id fst_data snd_data
          ; value = Job_status.Todo (Merge (fst_data, snd_data))
          }
          :: rest
      | head :: tail ->
          head :: merge_done_jobs tail
      | [] ->
          []

    let finish_job t ~id ~(data : Merge.t) =
      let found, t =
        List.fold_map t ~init:false ~f:(fun found job ->
            match job with
            | With_id.{ value = Job_status.Todo _; _ } when String.(job.id = id)
              ->
                (true, { job with value = Job_status.Done data })
            | _ ->
                (found, job) )
      in
      (found, merge_done_jobs t)

    let get_result t =
      match t with
      | [ With_id.{ value = Job_status.Done result; _ } ] ->
          Some result
      | _ ->
          None
  end

  type t = Tree.t list [@@deriving sexp, yojson]

  let create () = []

  let start_new_tree t = t @ [ [] ]

  let rec add_job t ~(data : Base.t) =
    match t with
    | [] ->
        [ Tree.add_job [] ~data ]
    | [ last ] ->
        [ Tree.add_job last ~data ]
    | head :: tail ->
        head :: add_job tail ~data

  let finish_job_exn t ~id ~(data : Merge.t) =
    let found, t =
      List.fold_map t ~init:false ~f:(fun found tree ->
          let found', tree = Tree.finish_job ~id ~data tree in
          (found || found', tree) )
    in
    if found then t else failwithf "Job with id '%s' not found" id ()

  let get_pending_jobs t =
    List.concat t
    |> List.filter_map ~f:(function
         | With_id.{ id; value = Job_status.Todo job } ->
             Some With_id.{ id; value = job }
         | _ ->
             None )

  let pp t = print_endline @@ Yojson.Safe.pretty_to_string @@ to_yojson t
end

let%test_module "parallel_merge on (+)" =
  ( module struct
    module Base = struct
      type t = int32 [@@deriving sexp, yojson]

      let derive_id x = Int64.to_string @@ Int64.of_int32_exn x
    end

    module Merge = struct
      type t = int64 [@@deriving sexp, yojson]

      let derive_id x y = Int64.to_string @@ Int64.(x + y)
    end

    module Forest = Make (Base) (Merge)

    let process_job (job : Forest.Available_job.t) =
      match job with
      | Base x ->
          Int64.of_int32 x
      | Merge (x, y) ->
          Int64.(x + y)

    let process_random_job state =
      match Forest.get_pending_jobs state with
      | [] ->
          state
      | jobs ->
          let Forest.With_id.{ id; value = job } =
            List.nth_exn jobs @@ Quickcheck.random_value
            @@ Int.gen_incl 0 (List.length jobs - 1)
          in
          let result = process_job job in
          Forest.finish_job_exn state ~id ~data:result

    let rec process_n_random_jobs state n =
      match n with
      | 0 ->
          state
      | _ ->
          process_n_random_jobs (process_random_job state) (n - 1)

    let rec process_jobs_in_random_order state =
      match List.length @@ Forest.get_pending_jobs state with
      | 0 ->
          state
      | _ ->
          process_jobs_in_random_order @@ process_random_job state

    let%test_unit "one tree" =
      let g = Quickcheck.Generator.list_non_empty Int32.quickcheck_generator in
      Quickcheck.test g ~trials:100 ~f:(fun data ->
          let expected_result =
            List.sum (module Int64) ~f:Int64.of_int32_exn data
          in
          let state = Forest.create () in
          let state =
            List.fold data ~init:state ~f:(fun state data ->
                Forest.add_job state ~data )
          in
          let final_state = process_jobs_in_random_order state in

          [%test_eq: int64]
            ( Option.value_exn @@ Forest.Tree.get_result
            @@ List.hd_exn final_state )
            expected_result )

    let%test_unit "multiple trees" =
      let g =
        Quickcheck.Generator.(
          list_non_empty @@ list_non_empty Int32.quickcheck_generator)
      in
      Quickcheck.test g ~trials:100 ~f:(fun data ->
          let expected_results =
            List.map data ~f:(fun data ->
                List.sum (module Int64) ~f:Int64.of_int32_exn data )
          in
          let state =
            List.fold data ~init:(Forest.create ()) ~f:(fun state data ->
                let state =
                  List.foldi data ~init:state ~f:(fun i state data ->
                      (* maybe process some jobs *)
                      let number_of_jobs_to_process =
                        Quickcheck.random_value
                          ~seed:(`Deterministic (Int.to_string i))
                        @@ Int.gen_incl 0 3
                      in
                      let state =
                        process_n_random_jobs state number_of_jobs_to_process
                      in
                      Forest.add_job state ~data )
                in
                let state = process_jobs_in_random_order state in
                Forest.start_new_tree state )
          in
          (* last tree will be empty *)
          let state = List.take state (List.length state - 1) in

          [%test_eq: int64 list]
            (List.map
               ~f:(fun tree -> Option.value_exn @@ Forest.Tree.get_result tree)
               state )
            expected_results )
  end )
