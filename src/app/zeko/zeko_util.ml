open Mina_base

type call_forest =
  ( Account_update.t
  , Zkapp_command.Digest.Account_update.t
  , Zkapp_command.Digest.Forest.t )
  Zkapp_command.Call_forest.t
[@@deriving yojson]

type call_forest_tree =
  ( Account_update.t
  , Zkapp_command.Digest.Account_update.t
  , Zkapp_command.Digest.Forest.t )
  Zkapp_command.Call_forest.Tree.t
[@@deriving yojson]

let progress_bar ?(width = 30) progress =
  let filled_length = int_of_float (float_of_int width *. progress) in
  let bar =
    String.make filled_length '#' ^ String.make (width - filled_length) '-'
  in
  Printf.printf "\r[%s] %.0f%%%!" bar (progress *. 100.0) ;
  if Float.(progress >= 1.0) then Printf.printf "\n%!"
