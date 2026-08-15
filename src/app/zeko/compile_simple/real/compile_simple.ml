open Core_kernel
open Snark_params.Tick
open Checked.Let_syntax

type self_width = Pickles_types.Nat.N2.n

module Proof = struct
  include Pickles.Side_loaded.Proof

  let of_pickles x = x

  let to_pickles x = Some x
end

type 'var tag =
  | Tag : ('var, 'value, self_width, 'height) Pickles.Tag.t -> 'var tag

module Verification_key = struct
  include Pickles.Side_loaded.Verification_key

  type var = Checked.t

  let of_pickles x = x

  let to_pickles x = Some x

  let of_tag (Tag tag) = of_compiled_promise tag

  let hash x =
    Random_oracle.(
      hash ~init:Hash_prefix_states.side_loaded_vk
        (pack_input (Pickles.Side_loaded.Verification_key.to_input x)))

  let hash_var x =
    Random_oracle.Checked.(
      hash ~init:Hash_prefix_states.side_loaded_vk
        (pack_input (Pickles.Side_loaded.Verification_key.Checked.to_input x)))
end

let force_tag tag = Promise.map ~f:(fun _ -> ()) (Verification_key.of_tag tag)

include Compile_simple_intf.Make (struct
  type nonrec proof = Proof.t

  type nonrec 'var tag = 'var tag

  type vk_t = Verification_key.t

  type vk_var = Verification_key.var
end)

let ( let* ) = Checked.Let_syntax.( >>= )

let ( let*| ) = Checked.Let_syntax.( >>| )

let ( let@ ) : (('a -> 'b) -> 'c) -> ('a -> 'b) -> 'c = ( @@ )

let time_promise : string -> (unit -> 'a Promise.t) -> 'a Promise.t =
 fun label f ->
  let start = Time.now () in
  let@ x = f () |> Promise.( >>| ) in
  let stop = Time.now () in
  printf "compile_simple.real: %s: %s\n%!" label
    (Time.Span.to_string_hum (Time.diff stop start)) ;
  x

type ('branches, 'n_branches) branches_length =
  | Z : (nil_branch, Pickles_types.Nat.z) branches_length
  | S :
      ('branches, 'n_branches) branches_length
      -> ( ('input, 'branches) cons_branch
         , 'n_branches Pickles_types.Nat.s )
         branches_length

let rec branches_length_functional :
          'x 'y 'z.
             ('x, 'y) branches_length * ('x, 'z) branches_length
          -> ('y, 'z) Type_equal.t =
  fun (type x y z) (xyxz : (x, y) branches_length * (x, z) branches_length) :
      (y, z) Type_equal.t ->
   match xyxz with
   | Z, Z ->
       Type_equal.T
   | S x, S y ->
       let T = branches_length_functional (x, y) in
       T

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

let transform_main_one
    ({ out; prevs = One_prev { public_input; proof; proof_must_verify } } :
      _ main_return ) =
  let*| proof = V.as_prover_value proof in
  Pickles.Inductive_rule.
    { previous_proof_statements = [ { public_input; proof; proof_must_verify } ]
    ; public_output = out
    ; auxiliary_output = ()
    }

let transform_main_one_sideloaded ~sideloaded
    ({ out
     ; prevs =
         One_prev_sideloaded { public_input; proof; proof_must_verify; vk }
     } :
      _ main_return ) =
  let* proof = V.as_prover_value proof in
  let*| () =
    make_checked (fun () -> Pickles.Side_loaded.in_circuit sideloaded vk)
  in
  Pickles.Inductive_rule.
    { previous_proof_statements = [ { public_input; proof; proof_must_verify } ]
    ; public_output = out
    ; auxiliary_output = ()
    }

let transform_main_two
    ({ out
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
  let* left_proof = V.as_prover_value left_proof in
  let*| right_proof = V.as_prover_value right_proof in
  Pickles.Inductive_rule.
    { previous_proof_statements =
        [ { public_input = left_public_input
          ; proof = left_proof
          ; proof_must_verify = left_proof_must_verify
          }
        ; { public_input = right_public_input
          ; proof = right_proof
          ; proof_must_verify = right_proof_must_verify
          }
        ]
    ; public_output = out
    ; auxiliary_output = ()
    }

let transform_main_two_one_sideloaded ~sideloaded
    ({ out
     ; prevs =
         Two_prevs_one_sideloaded
           ( { public_input = left_public_input
             ; proof = left_proof
             ; proof_must_verify = left_proof_must_verify
             ; vk
             }
           , { public_input = right_public_input
             ; proof = right_proof
             ; proof_must_verify = right_proof_must_verify
             } )
     } :
      _ main_return ) =
  let* left_proof = V.as_prover_value left_proof in
  let* right_proof = V.as_prover_value right_proof in
  let*| () =
    make_checked (fun () -> Pickles.Side_loaded.in_circuit sideloaded vk)
  in
  Pickles.Inductive_rule.
    { previous_proof_statements =
        [ { public_input = left_public_input
          ; proof = left_proof
          ; proof_must_verify = left_proof_must_verify
          }
        ; { public_input = right_public_input
          ; proof = right_proof
          ; proof_must_verify = right_proof_must_verify
          }
        ]
    ; public_output = out
    ; auxiliary_output = ()
    }

type ('out_var, 'out_t, 'branches, 'tag_branches) branches_to_choices_return =
  | Choices :
      { branches_length : ('branches, 'n_branches) branches_length
      ; rules :
             self:('out_var, 'out_t, self_width, 'tag_branches) Pickles.Tag.t
          -> ( 'n_branches
             , 'prev_varss
             , 'prev_valuess
             , 'widthss
             , 'heightss
             , unit
             , unit
             , 'out_var
             , 'out_t
             , unit
             , unit )
             Pickles_types.Hlist.H4_6_with_length.T
               (Pickles.Inductive_rule.Promise)
             .t
      ; transform_provers :
             ( 'prev_valuess
             , 'widthss
             , 'heightss
             , unit
             , ('out_t * unit * self_width Pickles.Proof.t) Promise.t )
             Pickles.Provers.t
          -> ('out_t, 'branches) provers
      }
      -> ('out_var, 'out_t, 'branches, 'tag_branches) branches_to_choices_return

let transform_prover :
       ?pre_prove:('input -> unit)
    -> branch_name:string
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
 fun ?pre_prove ~branch_name ~name prover handler input ->
  let@ () =
    time_promise @@ "(compile_simple) proved " ^ name ^ "." ^ branch_name
  in
  (match pre_prove with Some f -> f input | None -> ()) ;
  let@ stmt, (), proof =
    prover ~handler:(handler input) () |> Promise.( >>| )
  in
  (stmt, Pickles.Side_loaded.Proof.of_proof proof)

(* used to figure out what flags are used *)
let bad_fixme_feature_flags = ref None

(* TODO: collapse pattern match, maybe with newer ocaml version *)
let rec branches_to_choices :
    type out_var out_t branches available_branches tag_branches.
       name:string
    -> (out_var, branches, available_branches) Branches.t
    -> (out_var, out_t, branches, tag_branches) branches_to_choices_return
       lazy_t =
 fun ~name -> function
  | [] ->
      let open
        Pickles_types.Hlist.H4_6_with_length.T (Pickles.Inductive_rule.Promise) in
      lazy
        (Choices
           { branches_length = Z
           ; rules = (fun ~self:_ -> [])
           ; transform_provers = (fun [] -> [])
           } )
  | branch :: xs -> (
      let%map.Lazy { branch_name; tags; main } = branch in
      match branches_to_choices ~name xs |> Lazy.force with
      | Choices
          { branches_length
          ; rules = f
          ; transform_provers = prev_transform_provers
          } -> (
          let input, handler = input_for_main main in
          let transform_provers (prover :: provers : _ Pickles.Provers.t) :
              _ provers =
            transform_prover ~branch_name ~name prover handler
            :: prev_transform_provers provers
          in
          (* in the code below we pre-run the circuit once to find out what
             features are used *)
          bad_fixme_feature_flags :=
            Some Pickles_types.Plonk_types.Features.none_bool ;
          let main_wrapper i () =
            Run.run_checked Checked.(main i >>| fun _ -> ())
          in
          let constraint_builder =
            Run.constraint_system_manual ~input_typ:V.typ ~return_typ:Typ.unit
          in
          constraint_builder.run_circuit main_wrapper ;
          let (_ : Run.R1CS_constraint_system.t) =
            constraint_builder.finish_computation ()
          in
          let feature_flags = !bad_fixme_feature_flags |> Option.value_exn in
          bad_fixme_feature_flags := None ;
          (* we now have the features, and reset to be sure it's not used *)
          match tags with
          | No_tags ->
              Choices
                { branches_length = S branches_length
                ; transform_provers
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
                { branches_length = S branches_length
                ; transform_provers
                ; rules =
                    (fun ~self ->
                      let main = input >>= main >>= transform_main_one in
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
          | One_tag_own ->
              Choices
                { branches_length = S branches_length
                ; transform_provers
                ; rules =
                    (fun ~self ->
                      let main = input >>= main >>= transform_main_one in
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
                { branches_length = S branches_length
                ; transform_provers
                ; rules =
                    (fun ~self ->
                      let main = input >>= main >>= transform_main_two in
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
          | Two_tags_one_own (Tag right_tag) ->
              Choices
                { branches_length = S branches_length
                ; transform_provers
                ; rules =
                    (fun ~self ->
                      let main = input >>= main >>= transform_main_two in
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
          | Two_tags_own ->
              Choices
                { branches_length = S branches_length
                ; transform_provers
                ; rules =
                    (fun ~self ->
                      let main = input >>= main >>= transform_main_two in
                      let rule : _ Pickles.Inductive_rule.Promise.t =
                        { identifier = branch_name
                        ; main =
                            (fun _ -> Run.run_checked main |> Promise.return)
                        ; prevs = [ self; self ]
                        ; feature_flags
                        }
                      in
                      rule :: f ~self )
                }
          | One_tag_sideloaded { sideloaded_tag_name; typ; extract_vk } ->
              let sideloaded =
                let feature_flags : Pickles_types.Plonk_types.Features.options =
                  { range_check0 = Maybe
                  ; range_check1 = Maybe
                  ; foreign_field_add = Maybe
                  ; foreign_field_mul = Maybe
                  ; xor = Maybe
                  ; rot = Maybe
                  ; lookup = Maybe
                  ; runtime_tables = Maybe
                  }
                in
                Pickles.Side_loaded.create ~name:sideloaded_tag_name ~typ
                  ~feature_flags
                  ~max_proofs_verified:
                    (module Pickles.Side_loaded.Verification_key.Max_width)
              in
              Choices
                { branches_length = S branches_length
                ; transform_provers =
                    (fun (prover :: provers) ->
                      let pre_prove input =
                        Pickles.Side_loaded.in_prover sideloaded
                          (extract_vk input)
                      in
                      transform_prover ~pre_prove ~branch_name ~name prover
                        handler
                      :: prev_transform_provers provers )
                ; rules =
                    (fun ~self ->
                      let main =
                        input >>= main
                        >>= transform_main_one_sideloaded ~sideloaded
                      in
                      let rule : _ Pickles.Inductive_rule.Promise.t =
                        { identifier = branch_name
                        ; main =
                            (fun _ -> Run.run_checked main |> Promise.return)
                        ; prevs = [ sideloaded ]
                        ; feature_flags
                        }
                      in
                      rule :: f ~self )
                }
          | Two_tags_one_sideloaded
              ({ sideloaded_tag_name; typ; extract_vk }, Tag right_tag) ->
              let sideloaded =
                let feature_flags : Pickles_types.Plonk_types.Features.options =
                  { range_check0 = Maybe
                  ; range_check1 = Maybe
                  ; foreign_field_add = Maybe
                  ; foreign_field_mul = Maybe
                  ; xor = Maybe
                  ; rot = Maybe
                  ; lookup = Maybe
                  ; runtime_tables = Maybe
                  }
                in
                Pickles.Side_loaded.create ~name:sideloaded_tag_name ~typ
                  ~feature_flags
                  ~max_proofs_verified:
                    (module Pickles.Side_loaded.Verification_key.Max_width)
              in
              Choices
                { branches_length = S branches_length
                ; transform_provers =
                    (fun (prover :: provers) ->
                      let pre_prove input =
                        Pickles.Side_loaded.in_prover sideloaded
                          (extract_vk input)
                      in
                      transform_prover ~pre_prove ~branch_name ~name prover
                        handler
                      :: prev_transform_provers provers )
                ; rules =
                    (fun ~self ->
                      let main =
                        input >>= main
                        >>= transform_main_two_one_sideloaded ~sideloaded
                      in
                      let rule : _ Pickles.Inductive_rule.Promise.t =
                        { identifier = branch_name
                        ; main =
                            (fun _ -> Run.run_checked main |> Promise.return)
                        ; prevs = [ sideloaded; right_tag ]
                        ; feature_flags
                        }
                      in
                      rule :: f ~self )
                }
          | Two_tags_sideloaded_own { sideloaded_tag_name; typ; extract_vk } ->
              let sideloaded =
                let feature_flags : Pickles_types.Plonk_types.Features.options =
                  { range_check0 = Maybe
                  ; range_check1 = Maybe
                  ; foreign_field_add = Maybe
                  ; foreign_field_mul = Maybe
                  ; xor = Maybe
                  ; rot = Maybe
                  ; lookup = Maybe
                  ; runtime_tables = Maybe
                  }
                in
                Pickles.Side_loaded.create ~name:sideloaded_tag_name ~typ
                  ~feature_flags
                  ~max_proofs_verified:
                    (module Pickles.Side_loaded.Verification_key.Max_width)
              in
              Choices
                { branches_length = S branches_length
                ; transform_provers =
                    (fun (prover :: provers) ->
                      let pre_prove input =
                        Pickles.Side_loaded.in_prover sideloaded
                          (extract_vk input)
                      in
                      transform_prover ~pre_prove ~branch_name ~name prover
                        handler
                      :: prev_transform_provers provers )
                ; rules =
                    (fun ~self ->
                      let main =
                        input >>= main
                        >>= transform_main_two_one_sideloaded ~sideloaded
                      in
                      let rule : _ Pickles.Inductive_rule.Promise.t =
                        { identifier = branch_name
                        ; main =
                            (fun _ -> Run.run_checked main |> Promise.return)
                        ; prevs = [ sideloaded; self ]
                        ; feature_flags
                        }
                      in
                      rule :: f ~self )
                }
          | Two_tags_sideloaded
              ( { sideloaded_tag_name = left_name
                ; typ = left_typ
                ; extract_vk = left_extract_vk
                }
              , { sideloaded_tag_name = right_name
                ; typ = right_typ
                ; extract_vk = right_extract_vk
                } ) ->
              let left_sideloaded, right_sideloaded =
                let feature_flags : Pickles_types.Plonk_types.Features.options =
                  { range_check0 = Maybe
                  ; range_check1 = Maybe
                  ; foreign_field_add = Maybe
                  ; foreign_field_mul = Maybe
                  ; xor = Maybe
                  ; rot = Maybe
                  ; lookup = Maybe
                  ; runtime_tables = Maybe
                  }
                in
                ( Pickles.Side_loaded.create ~name:left_name ~typ:left_typ
                    ~feature_flags
                    ~max_proofs_verified:
                      (module Pickles.Side_loaded.Verification_key.Max_width)
                , Pickles.Side_loaded.create ~name:right_name ~typ:right_typ
                    ~feature_flags
                    ~max_proofs_verified:
                      (module Pickles.Side_loaded.Verification_key.Max_width) )
              in

              Choices
                { branches_length = S branches_length
                ; transform_provers =
                    (fun (prover :: provers) ->
                      let pre_prove input =
                        Pickles.Side_loaded.in_prover left_sideloaded
                          (left_extract_vk input) ;
                        Pickles.Side_loaded.in_prover right_sideloaded
                          (right_extract_vk input)
                      in
                      transform_prover ~pre_prove ~branch_name ~name prover
                        handler
                      :: prev_transform_provers provers )
                ; rules =
                    (fun ~self ->
                      let main =
                        let f out
                            (Two_prevs_sideloaded
                              ( { public_input = left_public_input
                                ; proof = left_proof
                                ; proof_must_verify = left_proof_must_verify
                                ; vk = left_vk
                                }
                              , { public_input = right_public_input
                                ; proof = right_proof
                                ; proof_must_verify = right_proof_must_verify
                                ; vk = right_vk
                                } ) ) =
                          let* () =
                            make_checked (fun () ->
                                Pickles.Side_loaded.in_circuit left_sideloaded
                                  left_vk ;
                                Pickles.Side_loaded.in_circuit right_sideloaded
                                  right_vk )
                          in
                          let* left_proof = V.as_prover_value left_proof in
                          let*| right_proof = V.as_prover_value right_proof in
                          Pickles.Inductive_rule.
                            { previous_proof_statements =
                                [ { public_input = left_public_input
                                  ; proof = left_proof
                                  ; proof_must_verify = left_proof_must_verify
                                  }
                                ; { public_input = right_public_input
                                  ; proof = right_proof
                                  ; proof_must_verify = right_proof_must_verify
                                  }
                                ]
                            ; public_output = out
                            ; auxiliary_output = ()
                            }
                        in
                        let* { out; prevs } = input >>= main in
                        f out prevs
                      in
                      let rule : _ Pickles.Inductive_rule.Promise.t =
                        { identifier = branch_name
                        ; main =
                            (fun _ -> Run.run_checked main |> Promise.return)
                        ; prevs = [ left_sideloaded; right_sideloaded ]
                        ; feature_flags
                        }
                      in
                      rule :: f ~self )
                } ) )

let compile (type out_t out_var first_input branches n_available_branches)
    ?(wrap_domain : [ `N13 | `N14 | `N15 ] option)
    ?(num_chunks = Plonk_checks.num_chunks_by_default) ~(name : string)
    ~(branches :
       ( out_var
       , (first_input, branches) cons_branch
       , n_available_branches )
       Branches.t ) ~(out_typ : (out_var, out_t) Typ.t) () :
    (module Result
       with type out_t = out_t
        and type out_var = out_var
        and type branches = (first_input, branches) cons_branch ) =
  assert (Run.in_checked_computation () |> not) ;
  assert (Run.in_prover () |> not) ;
  let override_wrap_domain : Pickles_base.Proofs_verified.t option =
    match (wrap_domain, num_chunks) with
    | None, 1 ->
        Some N1
        (* TODO: This should have been None, but pickles is really bad at estimating it. *)
    | None, _ ->
        None
        (* Multi-chunk systems need Pickles to account for the extra verifier
           rows when it computes the wrap domain. *)
    | Some `N13, _ ->
        Some N0
    | Some `N14, _ ->
        Some N1
    | Some `N15, _ ->
        Some N2
  in
  let (Count_branches_result tag_branches) = count_branches branches in
  let (module N_branches) = branches_length_to_module tag_branches in
  let dummy_proof =
    lazy
      Pickles_types.Nat.(
        Pickles.Proof.dummy N2.n N2.n
          ~domain_log2:
            ( match override_wrap_domain with
            | None ->
                14
                (* TODO: probably ok, maybe not?
                   If not ok, need dependency on compilation to
                   figure out override wrap domain. *)
            | Some N0 ->
                13
            | Some N1 ->
                14
            | Some N2 ->
                15 ))
  in
  let tag, provers =
    let lazy_result =
      match%map.Lazy branches_to_choices ~name branches with
      | Choices { branches_length; rules; transform_provers } ->
          let T = branches_length_functional (tag_branches, branches_length) in
          (* pretty sure this shouldn't be needed, but dumb ocaml type checker *)
          let choices :
                 self:(out_var, out_t, self_width, N_branches.n) Pickles.Tag.t
              -> ( _
                 , _
                 , _
                 , _
                 , _
                 , unit
                 , unit
                 , out_var
                 , out_t
                 , unit
                 , unit )
                 Pickles_types.Hlist.H4_6_with_length.T
                   (Pickles.Inductive_rule.Promise)
                 .t =
            rules
          in
          assert (Run.in_checked_computation () |> not) ;
          assert (Run.in_prover () |> not) ;
          let ( (tag : (_, _, _, N_branches.n) Pickles.Tag.t)
              , _cache
              , _proof_module
              , provers ) =
            Pickles.compile_promise () ?override_wrap_domain ~num_chunks
              ~cache:Cache_dir.cache ~public_input:(Output out_typ)
              ~auxiliary_typ:Typ.unit ~choices
              ~max_proofs_verified:(module Pickles_types.Nat.N2)
              ~name:("compile_simple of " ^ name)
          in
          let provers = transform_provers provers in
          (Tag tag, provers)
    in
    (Lazy.map lazy_result ~f:fst, Lazy.map lazy_result ~f:snd)
  in

  let r :
      (module Result
         with type out_t = out_t
          and type out_var = out_var
          and type branches = (first_input, branches) cons_branch ) =
    ( module struct
      type nonrec out_t = out_t

      type nonrec out_var = out_var

      type nonrec branches = (first_input, branches) cons_branch

      type tag_var = out_var

      type tag_t = out_t

      let tag = tag

      let provers = provers

      open struct
        module Out = struct
          type t = out_t

          type var = out_var

          let typ = out_typ
        end

        module Proof_V = struct
          type t = self_width Pickles.Proof.t

          type var = t V.t

          let typ = V.typ
        end
      end

      type t = { out : Out.t; proof : Proof_V.t } [@@deriving snarky]

      let get ?check { out; proof } =
        let prev : _ prev =
          { public_input = out
          ; proof
          ; proof_must_verify =
              (match check with Some b -> b | None -> Boolean.true_)
          }
        in
        Checked.return (out, prev)

      let make_unchecked ?proof out : t =
        { out
        ; proof =
            (match proof with Some proof -> proof | None -> force dummy_proof)
        }
    end )
  in
  r

let add_plonk_constraint ~label c =
  ( match !bad_fixme_feature_flags with
  | None ->
      ()
  | Some feature_flags ->
      let feature_flags =
        match
          ( c
            : _
              Kimchi_backend_common.Plonk_constraint_system.Plonk_constraint
              .basic )
        with
        | Boolean _
        | Equal _
        | Square _
        | R1CS _
        | Basic _
        | Poseidon _
        | EC_add_complete _
        | EC_scale _
        | EC_endoscale _
        | EC_endoscalar _
        | AddFixedLookupTable _
        | Raw
            { kind =
                ( Zero
                | Generic
                | Poseidon
                | CompleteAdd
                | VarBaseMul
                | EndoMul
                | EndoMulScalar
                | CairoClaim
                | CairoInstruction
                | CairoFlags
                | CairoTransition )
            ; values = _
            ; coeffs = _
            } ->
            feature_flags
        | Lookup _ | Raw { kind = Lookup; values = _; coeffs = _ } ->
            { feature_flags with lookup = true }
        | RangeCheck0 _ | Raw { kind = RangeCheck0; values = _; coeffs = _ } ->
            { feature_flags with range_check0 = true }
        | RangeCheck1 _ | Raw { kind = RangeCheck1; values = _; coeffs = _ } ->
            { feature_flags with range_check1 = true }
        | Xor _ | Raw { kind = Xor16; values = _; coeffs = _ } ->
            { feature_flags with xor = true }
        | ForeignFieldAdd _
        | Raw { kind = ForeignFieldAdd; values = _; coeffs = _ } ->
            { feature_flags with foreign_field_add = true }
        | ForeignFieldMul _
        | Raw { kind = ForeignFieldMul; values = _; coeffs = _ } ->
            { feature_flags with foreign_field_mul = true }
        | Rot64 _ | Raw { kind = Rot64; values = _; coeffs = _ } ->
            { feature_flags with rot = true }
        | AddRuntimeTableCfg _ ->
            { feature_flags with runtime_tables = true }
      in
      bad_fixme_feature_flags := Some feature_flags ) ;
  with_label label @@ fun () -> assert_ c
