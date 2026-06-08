(* Loads the copied Gherkin feature files and exposes a small parser so the
   acceptance tests can stay anchored to the scenario inventory from the spike. *)

open Core

type t =
  { title : string
  ; scenarios : string list
  }

let rec find_workspace_root dir =
  if Caml.Sys.file_exists (Filename.concat dir "dune-project")
  then Some dir
  else
    let parent = Filename.dirname dir in
    if String.equal parent dir then None else find_workspace_root parent

let workspace_root () =
  let candidates =
    [ Caml.Sys.getcwd ()
    ; Filename.dirname __FILE__
    ; Filename.dirname (Filename.dirname __FILE__)
    ]
  in
  List.find_map candidates ~f:find_workspace_root
  |> Option.value_exn ~message:"Could not locate workspace root"

let feature_path filename =
  Filename.concat (workspace_root ())
    ("src/app/zeko/sequencer/explorer/tests/features/" ^ filename)

let parse_lines lines =
  List.fold lines ~init:{ title = ""; scenarios = [] } ~f:(fun acc line ->
      match String.chop_prefix ~prefix:"Feature: " (String.strip line) with
      | Some title ->
          { acc with title }
      | None -> (
          match String.chop_prefix ~prefix:"Scenario: " (String.strip line) with
          | Some scenario ->
              { acc with scenarios = acc.scenarios @ [ scenario ] }
          | None ->
              acc ) )

let load filename = In_channel.read_lines (feature_path filename) |> parse_lines

let assert_scenario filename scenario =
  let feature = load filename in
  if not (List.mem feature.scenarios scenario ~equal:String.equal)
  then
    failwithf "Missing scenario %S in feature file %s" scenario filename ()
