module P = Printexc
open Core_kernel
open Snark_params.Tick

module F = struct
  type t = Field.t

  type var = Field.Var.t

  let typ = Typ.field
end

type 'var tag =
  | Tag : { typ : ('var, 't) Typ.t; circuit_hash : Field.t } -> 'var tag

module Verification_key = struct
  type t =
    | Fake_vk of { circuit_hash : Field.t }
    | Real_vk of Pickles.Side_loaded.Verification_key.t
  [@@deriving yojson]

  module Checked = struct
    module Witness = struct
      type nonrec t = t

      type var = t V.t

      let typ = V.typ
    end

    type t = { vk_hash : F.t; witness : Witness.t } [@@deriving snarky]
  end

  type var = Checked.var

  let hash = function
    | Fake_vk { circuit_hash } ->
        circuit_hash
    | Real_vk vk ->
        Random_oracle.(
          hash ~init:Hash_prefix_states.side_loaded_vk
            (pack_input (Pickles.Side_loaded.Verification_key.to_input vk)))

  let typ =
    let there witness : Checked.t = { vk_hash = hash witness; witness } in
    let back ({ vk_hash = _; witness } : Checked.t) = witness in
    Typ.transport ~there ~back Checked.typ

  let of_tag (Tag { circuit_hash; _ }) =
    Promise.return (Fake_vk { circuit_hash })

  let hash_var { Checked.vk_hash; witness = _ } = vk_hash

  let of_pickles x = Real_vk x
end

module Proof = struct
  type t =
    | Fake_proof of { circuit_hash : Field.t; out_hash : Field.t } (* hash of vk and public input *)
    | Real_proof of Pickles.Side_loaded.Proof.t
  [@@deriving yojson]

  let of_pickles x = Real_proof x
end

let force_tag _ = Promise.return ()

include Compile_simple_intf.Make (struct
  type nonrec proof = Proof.t

  type nonrec 'var tag = 'var tag

  type vk_t = Verification_key.t

  type vk_var = Verification_key.var
end)

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

let make_fake_proof (Tag { circuit_hash; _ }) (Typ.Typ typ) out =
  let fields, _aux = typ.value_to_fields out in
  let out_hash =
    Random_oracle.hash ~init:(Hash_prefix_create.salt "fake proof hash") fields
  in
  Proof.Fake_proof { circuit_hash; out_hash }

let match_tag_prev : 'var tag -> 'var prev -> bool As_prover.t =
 fun (Tag { circuit_hash; typ = Typ typ })
     { public_input; proof; proof_must_verify } ->
  let open As_prover in
  read Boolean.typ proof_must_verify
  >>= fun proof_must_verify ->
  read V.typ proof
  >>= fun proof ->
  read (Typ typ) public_input
  >>= fun public_input ->
  As_prover.return
    ( (not proof_must_verify)
    ||
    match proof with
    | Real_proof _ ->
        false
    | Fake_proof { circuit_hash = circuit_hash'; out_hash } ->
        let fields, _aux = typ.value_to_fields public_input in
        let out_hash' =
          Random_oracle.hash
            ~init:(Hash_prefix_create.salt "fake proof hash")
            fields
        in
        let open Field in
        equal circuit_hash circuit_hash' && equal out_hash out_hash' )

module Proof_valid = struct
  type 'a t_ = Immediate of 'a | Sideloaded of 'a Promise.t

  type t = bool t_

  let immediate x = Immediate x

  let to_promise : 'a t_ -> 'a Promise.t = function
    | Immediate v ->
        Promise.return v
    | Sideloaded dv ->
        dv

  let map (m : 'a t_) ~(f : 'a -> 'b) : 'b t_ =
    match m with
    | Immediate v ->
        Immediate (f v)
    | Sideloaded dv ->
        Sideloaded (Promise.map dv ~f)

  let ( &&& ) (a : t) (b : t) =
    match (a, b) with
    | Immediate x, Immediate y ->
        Immediate (x && y)
    | Immediate _, Sideloaded _
    | Sideloaded _, Immediate _
    | Sideloaded _, Sideloaded _ ->
        let open Promise.Let_syntax in
        Sideloaded
          ( Promise.all [ to_promise a; to_promise b ]
          >>| function [ x; y ] -> x && y | _ -> failwith "Unreachable" )
end

let match_sideloaded_tag_prev :
       'input
    -> ('var, 't, 'input) sideloaded_tag
    -> 'var prev_sideloaded
    -> Proof_valid.t As_prover.t =
 fun input { typ = Typ typ; extract_vk; _ }
     { public_input; proof; proof_must_verify; vk } ->
  let open As_prover in
  read Boolean.typ proof_must_verify
  >>= fun proof_must_verify ->
  read V.typ proof
  >>= fun proof ->
  read (Typ typ) public_input
  >>= fun public_input ->
  read Verification_key.typ vk
  >>= fun vk ->
  As_prover.return
    (Proof_valid.map
       ~f:(fun proof_valid -> (not proof_must_verify) || proof_valid)
       ( match (vk, proof, extract_vk input) with
       | Real_vk vk, Real_proof proof, Real_vk vk' ->
           let open Promise.Let_syntax in
           Proof_valid.Sideloaded
             ( Pickles.Side_loaded.verify_promise ~typ:(Typ typ)
                 [ (vk, public_input, proof) ]
             >>| fun proof_valid ->
             Or_error.is_ok proof_valid
             && Pickles.Side_loaded.Verification_key.equal vk vk' )
       | ( Fake_vk { circuit_hash }
         , Fake_proof { circuit_hash = circuit_hash'; out_hash }
         , Fake_vk { circuit_hash = circuit_hash'' } ) ->
           let fields, _aux = typ.value_to_fields public_input in
           let out_hash' =
             Random_oracle.hash
               ~init:(Hash_prefix_create.salt "fake proof hash")
               fields
           in
           let open Field in
           Immediate
             ( equal circuit_hash circuit_hash'
             && equal circuit_hash circuit_hash''
             && equal out_hash out_hash' )
       | Fake_vk _, Real_proof _, _
       | Real_vk _, Fake_proof _, _
       | Fake_vk _, _, Real_vk _
       | Real_vk _, _, Fake_vk _ ->
           Immediate false ) )

let match_tags_prevs :
      'self_var 'prevs 'input.
         'input
      -> 'self_var tag
      -> ('self_var, 'prevs, 'input) tags
      -> 'prevs prevs
      -> Proof_valid.t As_prover.t =
  fun (type self_var prevs_ input) (input : input) (self_tag : self_var tag)
      (tags : (self_var, prevs_, input) tags) (prevs : prevs_ prevs) :
      Proof_valid.t As_prover.t ->
   match (tags, prevs) with
   | No_tags, No_prevs ->
       As_prover.return (Proof_valid.immediate true)
   | One_tag tag, One_prev prev ->
       match_tag_prev tag prev |> As_prover.map ~f:Proof_valid.immediate
   | One_tag_own, One_prev prev ->
       match_tag_prev self_tag prev |> As_prover.map ~f:Proof_valid.immediate
   | One_tag_sideloaded tag, One_prev_sideloaded prev ->
       match_sideloaded_tag_prev input tag prev
   | Two_tags (tag0, tag1), Two_prevs (prev0, prev1) ->
       As_prover.map2 ~f:( && )
         (match_tag_prev tag0 prev0)
         (match_tag_prev tag1 prev1)
       |> As_prover.map ~f:Proof_valid.immediate
   | Two_tags_one_own tag1, Two_prevs (prev0, prev1) ->
       As_prover.map2 ~f:( && )
         (match_tag_prev self_tag prev0)
         (match_tag_prev tag1 prev1)
       |> As_prover.map ~f:Proof_valid.immediate
   | ( Two_tags_one_sideloaded (tag0, tag1)
     , Two_prevs_one_sideloaded (prev0, prev1) ) ->
       As_prover.map2 ~f:Proof_valid.( &&& )
         (match_sideloaded_tag_prev input tag0 prev0)
         (match_tag_prev tag1 prev1 |> As_prover.map ~f:Proof_valid.immediate)
   | Two_tags_own, Two_prevs (prev0, prev1) ->
       As_prover.map2 ~f:( && )
         (match_tag_prev self_tag prev0)
         (match_tag_prev self_tag prev1)
       |> As_prover.map ~f:Proof_valid.immediate
   | Two_tags_sideloaded (tag0, tag1), Two_prevs_sideloaded (prev0, prev1) ->
       As_prover.map2 ~f:Proof_valid.( &&& )
         (match_sideloaded_tag_prev input tag0 prev0)
         (match_sideloaded_tag_prev input tag1 prev1)
   | Two_tags_sideloaded_own tag0, Two_prevs_one_sideloaded (prev0, prev1) ->
       As_prover.map2 ~f:Proof_valid.( &&& )
         (match_sideloaded_tag_prev input tag0 prev0)
         ( match_tag_prev self_tag prev1
         |> As_prover.map ~f:Proof_valid.immediate )

let branches_to_provers name tag out_typ =
  let rec go :
      type branches available_branches.
         (_, branches, available_branches) Branches.t
      -> (_, branches) provers lazy_t = function
    | branch :: rest ->
        let%map.Lazy { branch_name; tags; main } = branch in
        let prover input =
          printf "compile_simple.fake: proving %s.%s\n" name branch_name ;
          let recursion_valid, out =
            Snark_params.Tick.run_and_check_exn
            @@
            let open Checked in
            exists Typ.unit ~compute:(fun _ -> ())
            >>= fun () ->
            main (V.return input)
            >>| fun { out; prevs } ->
            let open As_prover in
            match_tags_prevs input tag tags prevs
            >>= fun recursion_valid ->
            read out_typ out >>= fun out -> return (recursion_valid, out)
          in
          let open Promise.Let_syntax in
          let%map recursion_valid = Proof_valid.to_promise recursion_valid in
          if not recursion_valid then
            failwith "compile_simple [fake]: recursive proof invalid" ;
          printf "compile_simple.fake: %s.%s done\n" name branch_name ;
          let fake_proof = make_fake_proof tag out_typ out in
          (out, fake_proof)
        in
        prover :: Lazy.force (go rest)
    | [] ->
        lazy []
  in
  go

let rec hash_branches :
    type branches available_branches.
    (_, branches, available_branches) Branches.t -> field lazy_t = function
  | branch :: rest ->
      let%bind.Lazy { branch_name = _; tags = _; main } = branch in
      let%map.Lazy rest_hash = hash_branches rest in
      let main_wrapper input () =
        Run.run_checked Checked.(main input >>| fun _ -> ())
      in
      let constraint_builder =
        Run.constraint_system_manual ~input_typ:V.typ ~return_typ:Typ.unit
      in
      constraint_builder.run_circuit main_wrapper ;
      let cs = constraint_builder.finish_computation () in
      let cs_hash =
        Run.R1CS_constraint_system.digest cs
        |> Md5.to_hex |> Fn.flip String.prefix 7 |> Hash_prefix_create.salt
        |> Random_oracle.digest
      in
      (* FIXME: improve *)
      let tags_hash = Field.zero (* FIXME: fill out *) in
      Random_oracle.hash
        ~init:(Hash_prefix_create.salt "fake circuit hash")
        [| tags_hash; cs_hash; rest_hash |]
  | [] ->
      lazy Field.zero

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
  ignore wrap_domain ;
  printf "compile_simple.fake: called for circuit %s from %s\n%!" name
    (P.get_callstack 9999 |> get_first_backtrace_entry) ;
  (* ZEKO NOTE: ZEKO FIXME: Add back! didn't work very likely because of snarky bug that should be fixed *)
  (* assert (Run.in_checked_computation () |> not) ; *)
  (* assert (Run.in_prover () |> not) ; *)
  let circuit_hash = Lazy.force (hash_branches branches) in
  printf "compile_simple.fake: hashed %s\n" name ;
  let tag = Tag { circuit_hash; typ = out_typ } in
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

      let provers = branches_to_provers name tag out_typ branches

      type t = out_t

      type var = out_var

      let typ = out_typ

      let get ?check out =
        let open Checked in
        V.create
          (let open As_prover in
          read out_typ out >>| fun out -> make_fake_proof tag out_typ out)
        >>= fun proof ->
        let prev : _ prev =
          { public_input = out
          ; proof
          ; proof_must_verify =
              (match check with Some b -> b | None -> Boolean.true_)
          }
        in
        Checked.return (out, prev)

      let make_unchecked ?proof out : t = ignore proof ; out

      let tag = lazy tag
    end )
  in
  r

(* FIXME: check constraint *)
let add_plonk_constraint ~label:_ _ = Checked.return ()
