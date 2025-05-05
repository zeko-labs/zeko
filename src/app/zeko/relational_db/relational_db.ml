open Core_kernel
open Async
include Caqti_async

let with_transaction ((module Conn : CONNECTION) as conn) ~f =
  let%bind.Deferred.Result () = Conn.start () in
  match%bind.Deferred f conn with
  | Ok result ->
      let%bind.Deferred.Result () = Conn.commit () in
      return (Ok result)
  | Error e ->
      let%bind.Deferred.Result () = Conn.rollback () in
      return (Error e)

let caqti_ok_exn ?msg r =
  match (r, msg) with
  | Ok x, _ ->
      x
  | Error e, Some msg ->
      failwith (sprintf msg (Caqti_error.show e))
  | Error e, None ->
      failwith (Caqti_error.show e)

module Db = struct
  type pool = (connection, Caqti_error.t) Pool.t

  let create_pool ?sqlite_path () =
    let uuid = Uuid_unix.create () in
    let sqlite_uri =
      Uri.of_string
        (sprintf "sqlite3:%s"
           ( match sqlite_path with
           | None ->
               Filename.concat Cache_dir.autogen_path (Uuid.to_string uuid)
           | Some file ->
               file ) )
    in
    let%map.Result pool = Caqti_async.connect_pool ~max_size:30 sqlite_uri in
    (pool, `Uri sqlite_uri)

  module Migration = struct
    type t =
      { version : int
      ; name : string
      ; action : connection -> (unit, Caqti_error.t) Deferred.Result.t
      }

    let make version name action = { version; name; action }

    let ensure_meta_table (module Conn : CONNECTION) =
      Conn.exec
        (Caqti_request.exec Caqti_type.unit
           {sql| CREATE TABLE IF NOT EXISTS schema_migrations (
              version INTEGER PRIMARY KEY
            ) |sql} )
        ()

    let applied_versions (module Conn : CONNECTION) =
      Conn.collect_list
        (Caqti_request.collect Caqti_type.unit Caqti_type.int
           {sql| SELECT version FROM schema_migrations |sql} )
        ()

    let migration_applied (module Conn : CONNECTION) version =
      Conn.exec
        (Caqti_request.exec Caqti_type.int
           {sql| INSERT INTO schema_migrations (version) VALUES (?) |sql} )
        version

    let current_version (module Conn : CONNECTION) =
      match%map.Deferred.Result
        Conn.find_opt
          (Caqti_request.find_opt Caqti_type.unit Caqti_type.int
             {sql| SELECT version FROM schema_migrations ORDER BY version DESC LIMIT 1 |sql} )
          ()
      with
      | Some version ->
          version
      | None ->
          0

    let run ~logger ~target_version pool migrations =
      let migrations =
        List.sort migrations ~compare:(fun a b ->
            Int.compare a.version b.version )
      in
      let target_version =
        match target_version with
        | `Latest ->
            List.last migrations
            |> Option.map ~f:(fun m -> m.version)
            |> Option.value ~default:0
        | `Version v ->
            v
      in
      Pool.use
        (fun conn ->
          let%bind.Deferred.Result () = ensure_meta_table conn in
          let%bind.Deferred.Result applied_list = applied_versions conn in
          let applied_set = Int.Set.of_list applied_list in
          Deferred.List.fold migrations ~init:(Ok ()) ~f:(fun acc m ->
              (* Don't continue if we have an error *)
              let%bind.Deferred.Result () = return acc in

              if Set.mem applied_set m.version || m.version > target_version
              then return (Ok ())
              else
                with_transaction conn ~f:(fun conn ->
                    [%log info] "Running migration %s" m.name ;
                    let%bind.Deferred.Result () = m.action conn in
                    [%log info] "Migration %s applied" m.name ;
                    migration_applied conn m.version ) ) )
        pool
  end
end
