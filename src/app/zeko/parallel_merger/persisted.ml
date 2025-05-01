open Core_kernel
open Async
open Relational_db

module Make (Merger : In_memory.Intf) = struct
  let migrations : Db.Migration.t list =
    let open Deferred.Result.Let_syntax in
    [ Db.Migration.make 1 "create_merger_schema"
        (fun (module Conn : CONNECTION) ->
          let%bind () =
            Conn.exec
              (Caqti_request.exec Caqti_type.unit
                 {sql| CREATE TABLE parallel_merger (
                      id INTEGER PRIMARY KEY AUTOINCREMENT,
                      tree_id TEXT NOT NULL,
                      witness TEXT NOT NULL
                    ) |sql} )
              ()
          in
          Conn.exec
            (Caqti_request.exec Caqti_type.unit
               {sql| CREATE INDEX idx_tree_id ON parallel_merger (tree_id) |sql} )
            () )
    ]

  module Witness_row = struct
    type t = { tree_id : Merger.Tree.id; witness : string }
    [@@deriving hlist, fields]

    let make ~tree_id ~witness = { tree_id; witness }

    let typ =
      Mina_caqti.Type_spec.custom_type ~to_hlist ~of_hlist
        Caqti_type.[ string; string ]

    let insert (module Conn : CONNECTION) t =
      Conn.exec
        (Caqti_request.exec typ
           {sql| INSERT INTO parallel_merger (tree_id, witness) VALUES (?, ?) |sql} )
        t

    let remove_tree (module Conn : CONNECTION) tree_id =
      Conn.collect_list
        (Caqti_request.collect Caqti_type.string Caqti_type.int
           {sql| DELETE FROM parallel_merger WHERE tree_id = ? RETURNING id |sql} )
        tree_id

    let get_all (module Conn : CONNECTION) =
      Conn.collect_list
        (Caqti_request.collect Caqti_type.unit typ
           {sql| SELECT tree_id, witness FROM parallel_merger ORDER BY id |sql} )
  end

  let create_and_requeue ~logger ctx pool =
    let%bind () =
      Db.Migration.run ~logger pool migrations
      >>| caqti_ok_exn ~msg:"Failed to run Parallel merger migrations: %s"
    in
    let merger = Merger.create () in
    let%map all_witnesses =
      Pool.use (fun conn -> Witness_row.get_all conn ()) pool
      >>| caqti_ok_exn ~msg:"Failed to get all witnesses: %s"
      >>| List.map ~f:Witness_row.witness
      >>| List.map ~f:(fun s ->
              match Merger.Base.of_yojson (Yojson.Safe.from_string s) with
              | Ok data ->
                  data
              | Error e ->
                  failwithf "Failed to parse witness: %s" e () )
    in
    let () =
      List.iter all_witnesses ~f:(fun data ->
          (Merger.add_job merger ctx ~data : Merger.Tree.id) |> ignore )
    in
    merger

  let add_job pool t ctx ~data =
    let tid = Merger.add_job t ctx ~data in
    Pool.use
      (fun conn ->
        Witness_row.(
          insert conn
            (make ~tree_id:tid
               ~witness:(Yojson.Safe.to_string @@ Merger.Base.to_yojson data) ))
        )
      pool

  let commit_exn pool t ctx ~commit_witness =
    let%bind tree_result, tid = Merger.commit_exn t ctx ~commit_witness in
    let%map _deleted =
      Pool.use (fun conn -> Witness_row.remove_tree conn tid) pool
      >>| caqti_ok_exn ~msg:"Failed to remove tree: %s"
    in
    tree_result
end
