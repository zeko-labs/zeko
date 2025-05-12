open Core_kernel
open Async
open Relational_db

module Make (Merger : In_memory.Intf) = struct
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

    let merge_witnesses_into_tree (module Conn : CONNECTION) tree_id =
      Conn.collect_list
        (Caqti_request.collect Caqti_type.string Caqti_type.int
           {sql| UPDATE parallel_merger SET tree_id = ? RETURNING id |sql} )
        tree_id
  end

  let create_and_requeue ~logger ctx pool =
    let merger = Merger.create () in
    let open Deferred.Result.Let_syntax in
    Pool.use
      (with_transaction ~f:(fun conn ->
           let%bind all_witnesses =
             Witness_row.get_all conn ()
             >>| List.map ~f:Witness_row.witness
             >>| List.map ~f:(fun s ->
                     match
                       Merger.Base.of_yojson (Yojson.Safe.from_string s)
                     with
                     | Ok data ->
                         data
                     | Error e ->
                         failwithf "Failed to parse witness: %s" e () )
           in
           [%log info] "Adding %d jobs to merger" (List.length all_witnesses) ;
           let%map tid, merged_witnesses =
             let tids =
               List.map all_witnesses ~f:(fun data ->
                   Merger.add_job merger ctx ~data )
             in
             match tids with
             | [] ->
                 return (None, [])
             | hd :: tl ->
                 assert (
                   List.fold_until tl ~init:(hd, true)
                     ~f:(fun (acc, _) tid ->
                       if String.equal acc tid then Continue (tid, true)
                       else Stop (tid, false) )
                     ~finish:Fn.id
                   |> snd ) ;
                 let tid = hd in
                 Witness_row.merge_witnesses_into_tree conn tid
                 >>| fun result -> (Some tid, result)
           in
           [%log info]
             !"Merged %d witnesses into tree %{sexp: string option}"
             (List.length merged_witnesses)
             tid ;
           merger ) )
      pool
    |> Deferred.map ~f:(caqti_ok_exn ~msg:"Failed to requeue merger: %s")

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
