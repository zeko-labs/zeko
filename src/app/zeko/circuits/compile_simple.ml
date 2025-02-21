module P = Printexc
open Core_kernel
open Snark_params.Tick
open Checked.Let_syntax

type self_width = Pickles_types.Nat.N2.n

module Proof = struct
  include Pickles.Side_loaded.Proof

  let of_pickles x = x
end

type 'var tag =
  | Tag : ('var, 'value, self_width, 'height) Pickles.Tag.t -> 'var tag

module Verification_key = struct
  include Pickles.Side_loaded.Verification_key

  type var = Checked.t

  let of_pickles x = x

  let var_of_pickles x = x

  let of_tag (Tag tag) = of_compiled_promise tag

  let to_pickles_lossy x = x
  
  let hash = Mina_base.Zkapp_account.digest_vk

  let hash_var = Mina_base.Zkapp_account.Checked.digest_vk
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
  printf "%s: %s\n%!" label (Time.Span.to_string_hum (Time.diff stop start)) ;
  x

type ('branches, 'n_branches) branches_length =
  | Z : (nil_branch, Pickles_types.Nat.z) branches_length
  | S :
      ('branches, 'n_branches) branches_length
      -> ( ('input, 'branches) cons_branch
         , 'n_branches Pickles_types.Nat.s )
         branches_length

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
  Pickles.Inductive_rule.
    { previous_proof_statements =
        [ { public_input; proof = V.as_ref proof; proof_must_verify } ]
    ; public_output = out
    ; auxiliary_output = ()
    }

let transform_main_one_sideloaded ~sideloaded
    ({ out
     ; prevs =
         One_prev_sideloaded { public_input; proof; proof_must_verify; vk }
     } :
      _ main_return ) =
  let*| () =
    make_checked (fun () -> Pickles.Side_loaded.in_circuit sideloaded vk)
  in
  Pickles.Inductive_rule.
    { previous_proof_statements =
        [ { public_input; proof = V.as_ref proof; proof_must_verify } ]
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
  let*| () =
    make_checked (fun () -> Pickles.Side_loaded.in_circuit sideloaded vk)
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
                      let main = input >>= main >>| transform_main_one in
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
                { transform_provers
                ; rules =
                    (fun ~self ->
                      let main = input >>= main >>| transform_main_one in
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
                      let main = input >>= main >>| transform_main_two in
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
                { transform_provers
                ; rules =
                    (fun ~self ->
                      let main = input >>= main >>| transform_main_two in
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
                { transform_provers
                ; rules =
                    (fun ~self ->
                      let main = input >>= main >>| transform_main_two in
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
                { transform_provers =
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
                { transform_provers =
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
                { transform_provers =
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
                { transform_provers =
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
                          let*| () =
                            make_checked (fun () ->
                                Pickles.Side_loaded.in_circuit left_sideloaded
                                  left_vk ;
                                Pickles.Side_loaded.in_circuit right_sideloaded
                                  right_vk )
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

let get_first_backtrace_entry b =
  let open P in
  match backtrace_slots b with
  | None ->
      "<invalid>"
  | Some slots -> (
      match Slot.location slots.(1) with
      | None ->
          "<invalid>"
      | Some { filename; line_number; _ } ->
          filename ^ ":" ^ Int.to_string line_number )

let compile (type out_t out_var first_input branches n_available_branches)
    ?(wrap_domain : [ `N13 | `N14 | `N15 ] option) ~(name : string)
    ~(branches :
       ( out_var
       , (first_input, branches) cons_branch
       , n_available_branches )
       Branches.t ) ~(out_typ : (out_var, out_t) Typ.t) () :
    (module Result
       with type out_t = out_t
        and type out_var = out_var
        and type branches = (first_input, branches) cons_branch ) =
  printf "(compile_simple) called for circuit %s from %s\n%!" name
    (P.get_callstack 9999 |> get_first_backtrace_entry) ;
  assert (Run.in_checked_computation () |> not) ;
  assert (Run.in_prover () |> not) ;
  let (Count_branches_result tag_branches) = count_branches branches in
  let (module N_branches) = branches_length_to_module tag_branches in
  let override_wrap_domain : Pickles_base.Proofs_verified.t option =
    match wrap_domain with
    | None ->
        Some N1
        (* TODO: This should have been None, but pickles is really bad at estimating it. *)
    | Some `N13 ->
        Some N0
    | Some `N14 ->
        Some N1
    | Some `N15 ->
        Some N2
  in
  match branches_to_choices ~name branches with
  | Choices { rules; transform_provers } ->
      let dummy_proof =
        lazy
          Pickles_types.Nat.(
            Pickles.Proof.dummy N2.n N2.n N_branches.n
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
      assert (Run.in_checked_computation () |> not) ;
      assert (Run.in_prover () |> not) ;
      let tag, _cache, _proof_module, provers =
        Pickles.compile_promise () ?override_wrap_domain ~cache:Cache_dir.cache
          ~public_input:(Output out_typ) ~auxiliary_typ:Typ.unit
          ~branches:(module N_branches)
          ~choices:rules
          ~max_proofs_verified:(module Pickles_types.Nat.N2)
          ~name:("compile_simple of " ^ name)
          ~constraint_constants:
            (Genesis_constants.Constraint_constants.to_snark_keys_header
               Genesis_constants.Compiled.constraint_constants )
      in
      (* FIXME: Don't do this. Make lazy compilation work. Fix Pickles bug. *)
      Promise.block_on_async_exn (fun () ->
          time_promise ("(compile_simple) compiled " ^ name) (fun () ->
              Verification_key.of_compiled_promise tag )
          |> Promise.map ~f:(fun _ -> ()) ) ;
      let provers = transform_provers provers in
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

          let tag = Tag tag

          let provers = provers

          open struct
            module Out = struct
              type t = out_t

              type var = out_var

              let typ = out_typ
            end

            module Proof_V = struct
              type t = (self_width, self_width) Pickles.Proof.t

              type var = (self_width, self_width) Pickles.Proof.t V.t

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
                ( match proof with
                | Some proof ->
                    proof
                | None ->
                    force dummy_proof )
            }
        end )
      in
      r
