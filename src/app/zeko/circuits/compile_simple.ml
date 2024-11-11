open Core_kernel
open Snark_params.Tick
include Compile_simple_intf

let ( let* ) = Checked.Let_syntax.( >>= )

let ( let*| ) = Checked.Let_syntax.( >>| )

let ( let@ ) : (('a -> 'b) -> 'c) -> ('a -> 'b) -> 'c = ( @@ )

let time_promise : string -> (unit -> 'a Promise.t) -> 'a Promise.t =
 fun label f ->
  let start = Time.now () in
  let@ x = f () |> Promise.( >>| ) in
  let stop = Time.now () in
  printf "(time_async) %s: %s\n%!" label
    (Time.Span.to_string_hum (Time.diff stop start)) ;
  x

type 'branches count_branches_result =
  | Count_branches_result :
      ('branches, 'n_branches) branches_length
      -> 'branches count_branches_result

let rec count_branches :
    type out_var branches n_available_branches.
       (out_var, branches, n_available_branches) Branches.t
    -> branches count_branches_result = function
  | [] ->
      Count_branches_result Z
  | _ :: xs ->
      let (Count_branches_result n) = count_branches xs in
      Count_branches_result (S n)

let rec branches_length_to_module :
    type branches n_branches.
       (branches, n_branches) branches_length
    -> (module Pickles_types.Nat.Intf with type n = n_branches) = function
  | Z ->
      (module Pickles_types.Nat.N0)
  | S n ->
      let (module N) = branches_length_to_module n in
      ( module struct
        type n = N.n Pickles_types.Nat.s

        let n : n Pickles_types.Nat.t = S N.n
      end )

let input_for_main (type input)
    (_main : input V.t -> ('out_var, 'prevs) main_return Checked.t) :
    input V.t Checked.t
    * (   input
       -> Snarky_backendless.Request.request
       -> Snarky_backendless.Request.response ) =
  let open struct
    open Snarky_backendless.Request

    type _ t += Input : input t

    let handler (input : input)
        (With { request; respond } : Snarky_backendless.Request.request) =
      match request with
      | Input ->
          respond (Provide input)
      | _ ->
          respond Unhandled

    let exists_input : input V.t Checked.t =
      exists V.typ ~request:(As_prover.return Input)
  end in
  (exists_input, handler)

let transform_main_one (type out_var prev_var prev_width)
    (main : (out_var, (prev_var, prev_width) one_prev) main_return Checked.t) :
    ( prev_var * unit
    , prev_width * unit
    , out_var
    , unit )
    Pickles.Inductive_rule.main_return
    Checked.t =
  let*| ({ out; prevs = One_prev { public_input; proof; proof_must_verify } } :
          _ main_return ) =
    main
  in
  Pickles.Inductive_rule.
    { previous_proof_statements =
        [ { public_input; proof = V.as_ref proof; proof_must_verify } ]
    ; public_output = out
    ; auxiliary_output = ()
    }

let transform_main_two (type out_var left_var left_width right_var right_width)
    (main :
      ( out_var
      , (left_var, left_width, right_var, right_width) two_prevs )
      main_return
      Checked.t ) :
    ( left_var * (right_var * unit)
    , left_width * (right_width * unit)
    , out_var
    , unit )
    Pickles.Inductive_rule.main_return
    Checked.t =
  let*| ({ out
         ; prevs =
             Two_prevs
               ( { public_input = left_public_input
                 ; proof = left_proof
                 ; proof_must_verify = left_proof_must_verify
                 }
               , { public_input = right_public_input
                 ; proof = right_proof
                 ; proof_must_verify = right_proof_must_verify
                 } )
         } :
          _ main_return ) =
    main
  in
  Pickles.Inductive_rule.
    { previous_proof_statements =
        [ { public_input = left_public_input
          ; proof = V.as_ref left_proof
          ; proof_must_verify = left_proof_must_verify
          }
        ; { public_input = right_public_input
          ; proof = V.as_ref right_proof
          ; proof_must_verify = right_proof_must_verify
          }
        ]
    ; public_output = out
    ; auxiliary_output = ()
    }

type ('out_var, 'out_t, 'tag_branches, 'branches) branches_to_choices_return =
  | Choices :
      { rules :
             self:('out_var, 'out_t, self_width, 'tag_branches) Pickles.Tag.t
          -> ( 'prev_varss
             , 'prev_valuess
             , 'widthss
             , 'heightss
             , unit
             , unit
             , 'out_var
             , 'out_t
             , unit
             , unit )
             Pickles_types.Hlist.H4_6.T(Pickles.Inductive_rule.Promise).t
      ; transform_provers :
             ( 'prev_valuess
             , 'widthss
             , 'heightss
             , unit
             , ('out_t * unit * (self_width, self_width) Pickles.Proof.t)
               Promise.t )
             Pickles.Provers.t
          -> ('out_t, 'branches) provers
      }
      -> ('out_var, 'out_t, 'tag_branches, 'branches) branches_to_choices_return

let transform_prover :
       branch_name:string
    -> name:string
    -> (   ?handler:
             (   Snarky_backendless.Request.request
              -> Snarky_backendless.Request.response )
        -> unit
        -> ('out_t * unit * _ Pickles.Proof.t) Promise.t )
    -> (   'input
        -> Snarky_backendless.Request.request
        -> Snarky_backendless.Request.response )
    -> 'input
    -> ('out_t * Pickles.Side_loaded.Proof.t) Promise.t =
 fun ~branch_name ~name prover handler input ->
  let@ () =
    time_promise @@ "(compile_simple) proving " ^ name ^ "." ^ branch_name
  in
  let@ stmt, (), proof =
    prover ~handler:(handler input) () |> Promise.( >>| )
  in
  (stmt, Pickles.Side_loaded.Proof.of_proof proof)

(* TODO: collapse branches *)
let rec branches_to_choices :
    type out_var out_t branches available_branches tag_branches.
       name:string
    -> (out_var, branches, available_branches) Branches.t
    -> (out_var, out_t, tag_branches, branches) branches_to_choices_return =
 fun ~name -> function
  | [] ->
      let open Pickles_types.Hlist.H4_6.T (Pickles.Inductive_rule.Promise) in
      Choices
        { rules = (fun ~self:_ -> []); transform_provers = (fun [] -> []) }
  | { branch_name; tags; main } :: xs -> (
      match branches_to_choices ~name xs with
      | Choices { rules = f; transform_provers = prev_transform_provers } -> (
          let input, handler = input_for_main main in
          let transform_provers (prover :: provers : _ Pickles.Provers.t) :
              _ provers =
            transform_prover ~branch_name ~name prover handler
            :: prev_transform_provers provers
          in
          let feature_flags = Pickles_types.Plonk_types.Features.none_bool in
          match tags with
          | No_tags ->
              Choices
                { transform_provers
                ; rules =
                    (fun ~self ->
                      let rule : _ Pickles.Inductive_rule.Promise.t =
                        { identifier = branch_name
                        ; main =
                            (fun _ ->
                              Run.run_checked
                                (let* i = input in
                                 let*| ({ out; prevs = No_prevs } :
                                         _ main_return ) =
                                   main i
                                 in
                                 Pickles.Inductive_rule.
                                   { previous_proof_statements = []
                                   ; public_output = out
                                   ; auxiliary_output = ()
                                   } )
                              |> Promise.return )
                        ; prevs = []
                        ; feature_flags
                        }
                      in
                      rule :: f ~self )
                }
          | One_tag (Tag tag) ->
              Choices
                { transform_provers
                ; rules =
                    (fun ~self ->
                      let main =
                        Checked.(input >>= main) |> transform_main_one
                      in
                      let rule : _ Pickles.Inductive_rule.Promise.t =
                        { identifier = branch_name
                        ; main =
                            (fun _ -> Run.run_checked main |> Promise.return)
                        ; prevs = [ tag ]
                        ; feature_flags
                        }
                      in
                      rule :: f ~self )
                }
          | One_tag Own_tag ->
              Choices
                { transform_provers
                ; rules =
                    (fun ~self ->
                      let main =
                        Checked.(input >>= main) |> transform_main_one
                      in
                      let rule : _ Pickles.Inductive_rule.Promise.t =
                        { identifier = branch_name
                        ; main =
                            (fun _ -> Run.run_checked main |> Promise.return)
                        ; prevs = [ self ]
                        ; feature_flags
                        }
                      in
                      rule :: f ~self )
                }
          | Two_tags (Tag left_tag, Tag right_tag) ->
              Choices
                { transform_provers
                ; rules =
                    (fun ~self ->
                      let main =
                        Checked.(input >>= main) |> transform_main_two
                      in
                      let rule : _ Pickles.Inductive_rule.Promise.t =
                        { identifier = branch_name
                        ; main =
                            (fun _ -> Run.run_checked main |> Promise.return)
                        ; prevs = [ left_tag; right_tag ]
                        ; feature_flags
                        }
                      in
                      rule :: f ~self )
                }
          | Two_tags (Own_tag, Tag right_tag) ->
              Choices
                { transform_provers
                ; rules =
                    (fun ~self ->
                      let main =
                        Checked.(input >>= main) |> transform_main_two
                      in
                      let rule : _ Pickles.Inductive_rule.Promise.t =
                        { identifier = branch_name
                        ; main =
                            (fun _ -> Run.run_checked main |> Promise.return)
                        ; prevs = [ self; right_tag ]
                        ; feature_flags
                        }
                      in
                      rule :: f ~self )
                }
          | Two_tags (Tag left_tag, Own_tag) ->
              Choices
                { transform_provers
                ; rules =
                    (fun ~self ->
                      let main =
                        Checked.(input >>= main) |> transform_main_two
                      in
                      let rule : _ Pickles.Inductive_rule.Promise.t =
                        { identifier = branch_name
                        ; main =
                            (fun _ -> Run.run_checked main |> Promise.return)
                        ; prevs = [ left_tag; self ]
                        ; feature_flags
                        }
                      in
                      rule :: f ~self )
                }
          | Two_tags (Own_tag, Own_tag) ->
              Choices
                { transform_provers
                ; rules =
                    (fun ~self ->
                      let main =
                        Checked.(input >>= main) |> transform_main_two
                      in
                      let rule : _ Pickles.Inductive_rule.Promise.t =
                        { identifier = branch_name
                        ; main =
                            (fun _ -> Run.run_checked main |> Promise.return)
                        ; prevs = [ self; self ]
                        ; feature_flags
                        }
                      in
                      rule :: f ~self )
                } ) )

let compile ?(override_wrap_domain : [ `N0 | `N1 | `N2 ] option)
    ~(name : string)
    ~(branches :
       ( 'out_var
       , ('first_input, 'branches) cons_branch
       , 'n_available_branches )
       Branches.t ) ~(out_typ : ('out_var, 'out_t) Typ.t) () :
    ('out_var, 'out_t, ('first_input, 'branches) cons_branch) result Promise.t =
  printf "(compile_simple) called for circuit %s\n" name ;
  assert (Run.in_checked_computation () |> not) ;
  assert (Run.in_prover () |> not) ;
  let@ () = time_promise ("(compile_simple) compiling circuit " ^ name) in
  let (Count_branches_result tag_length) = count_branches branches in
  let (module N_branches) = branches_length_to_module tag_length in
  let override_wrap_domain : Pickles_base.Proofs_verified.t option =
    match override_wrap_domain with
    | None ->
        None
    | Some `N0 ->
        Some N0
    | Some `N1 ->
        Some N1
    | Some `N2 ->
        Some N2
  in
  match branches_to_choices ~name branches with
  | Choices { rules; transform_provers } ->
      let tag, _cache, _proof_module, provers =
        Pickles.compile_promise () ?override_wrap_domain ~cache:Cache_dir.cache
          ~public_input:(Output out_typ) ~auxiliary_typ:Typ.unit
          ~branches:(module N_branches)
          ~choices:rules
          ~max_proofs_verified:(module Pickles_types.Nat.N2)
          ~name:("compile_simple of " ^ name)
          ~constraint_constants:
            Genesis_constants.Constraint_constants.(
              to_snark_keys_header compiled)
      in
      let@ (_ : Pickles.Side_loaded.Verification_key.t) =
        Pickles.Side_loaded.Verification_key.of_compiled_promise tag
        |> Promise.( >>| )
      in
      let provers = transform_provers provers in
      Result { tag; provers; tag_length }
