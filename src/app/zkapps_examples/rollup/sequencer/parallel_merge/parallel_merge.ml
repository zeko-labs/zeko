open Core_kernel

module Available_job = struct
  type ('base, 'merge) t = Base of 'base | Merge of 'merge * 'merge
  [@@deriving sexp, yojson]
end

module Finished_job = struct
  type 'merge t = 'merge [@@deriving sexp, yojson]
end

module Job_status = struct
  type ('base, 'merge) t =
    | Todo of ('base, 'merge) Available_job.t
    | Done of 'merge Finished_job.t
  [@@deriving sexp, yojson]
end

module Job = struct
  type ('base, 'merge) t =
    { id : string
    ; status : ('base, 'merge) Job_status.t
    ; height : int (* used as a priority in a queue *)
    }
  [@@deriving sexp, yojson]
end

module Tree = struct
  type ('base, 'merge) t = ('base, 'merge) Job.t list [@@deriving sexp, yojson]

  let add_job t ~id ~(data : 'base) =
    t @ [ Job.{ id; status = Todo (Base data); height = 0 } ]

  let rec merge_done_jobs t =
    match t with
    (* first done && second done with the same height *)
    | (Job.{ status = Done fst_status; _ } as fst)
      :: (Job.{ status = Done snd_status; _ } as snd) :: rest
      when fst.height = snd.height ->
        Job.
          { id = fst.id ^ "-" ^ snd.id
          ; status = Todo (Merge (fst_status, snd_status))
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

module Forest = struct
  type ('base, 'merge) t = ('base, 'merge) Tree.t list [@@deriving sexp, yojson]

  let create () = []

  let start_new_tree t = t @ [ [] ]

  let rec add_job t ~id ~(data : 'base) : ('base, 'merge) t =
    match t with
    | [] ->
        [ Tree.add_job [] ~id ~data ]
    | [ last ] ->
        [ Tree.add_job last ~id ~data ]
    | head :: tail ->
        head :: add_job tail ~id ~data

  let finish_job t ~id ~(data : 'merge) =
    List.map t ~f:(Tree.finish_job ~id ~data)

  let get_pending_jobs (t : ('base, 'merge) t) =
    List.concat t
    |> List.filter_map ~f:(function
         | Job.{ status = Todo job; _ } ->
             Some job
         | _ ->
             None )
end

let%test_unit "testing" =
  let print_forest (t : (int, float) Forest.t) =
    print_endline @@ Yojson.Safe.pretty_to_string
    @@ Forest.to_yojson (fun i -> `Int i) (fun f -> `Float f) t
  in
  let state = Forest.create () in
  let state = Forest.add_job state ~id:"1" ~data:1 in
  let state = Forest.add_job state ~id:"2" ~data:2 in
  let state = Forest.add_job state ~id:"3" ~data:3 in
  let state = Forest.add_job state ~id:"4" ~data:4 in

  let state = Forest.finish_job state ~id:"1" ~data:1.0 in
  let state = Forest.finish_job state ~id:"2" ~data:2.0 in
  let state = Forest.finish_job state ~id:"3" ~data:3.0 in
  let state = Forest.finish_job state ~id:"4" ~data:4.0 in

  print_forest state
