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

  module Finished_job = struct
    type t = Merge.t [@@deriving sexp, yojson]
  end

  module Job_status = struct
    type t = Todo of Available_job.t | Done of Finished_job.t
    [@@deriving sexp, yojson]
  end

  module Job = struct
    type t = { id : string; status : Job_status.t; height : int }
    [@@deriving sexp, yojson]
  end

  module Tree = struct
    type t = Job.t list [@@deriving sexp, yojson]

    let add_job t ~(data : 'base) =
      t
      @ [ Job.
            { id = Base.derive_id data; status = Todo (Base data); height = 0 }
        ]

    let rec merge_done_jobs t =
      match t with
      (* first done && second done with the same height *)
      | (Job.{ status = Done fst_data; _ } as fst)
        :: (Job.{ status = Done snd_data; _ } as snd) :: rest
        when fst.height = snd.height ->
          Job.
            { id = Merge.derive_id fst_data snd_data
            ; status = Todo (Merge (fst_data, snd_data))
            ; height = fst.height + 1
            }
          :: rest
      | head :: tail ->
          head :: merge_done_jobs tail
      | [] ->
          []

    let finish_job t ~id ~(data : 'merge) =
      List.map t ~f:(fun job ->
          if String.(Job.(job.id = id)) then { job with status = Done data }
          else job )
      |> merge_done_jobs
  end

  type t = Tree.t list [@@deriving sexp, yojson]

  let create () = []

  let start_new_tree t = t @ [ [] ]

  let rec add_job t ~(data : 'base) =
    match t with
    | [] ->
        [ Tree.add_job [] ~data ]
    | [ last ] ->
        [ Tree.add_job last ~data ]
    | head :: tail ->
        head :: add_job tail ~data

  let finish_job t ~id ~(data : 'merge) =
    List.map t ~f:(Tree.finish_job ~id ~data)

  let get_pending_jobs t =
    List.concat t
    |> List.filter_map ~f:(function
         | Job.{ status = Todo job; _ } ->
             Some job
         | _ ->
             None )
end

let%test_unit "testing" =
  let module Forest =
    Make
      (struct
        type t = int [@@deriving sexp, yojson]

        let derive_id = Int.to_string
      end)
      (struct
        type t = float [@@deriving sexp, yojson]

        let derive_id x y = Float.to_string (x +. y)
      end)
  in
  let print_forest (t : Forest.t) =
    print_endline @@ Yojson.Safe.pretty_to_string @@ Forest.to_yojson t
  in
  let state = Forest.create () in
  let state = Forest.add_job state ~data:1 in
  let state = Forest.add_job state ~data:2 in
  let state = Forest.add_job state ~data:3 in
  let state = Forest.add_job state ~data:4 in

  let state = Forest.finish_job state ~id:"1" ~data:1.0 in
  let state = Forest.finish_job state ~id:"2" ~data:2.0 in
  let state = Forest.finish_job state ~id:"3" ~data:3.0 in

  print_forest state
