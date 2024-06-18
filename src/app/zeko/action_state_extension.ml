(** Rules for proving extensions of action states  *)

open Core_kernel
open Mina_base
open Snark_params.Tick.Run
(* Impure interface to snarky, FIXME: replace with pure one *)

open Account_update
module Nat = Pickles_types.Nat
open Zeko_util

(** A proof for source, target, means the target is
an action state that originates from the source *)
module Stmt = struct
  type t = { source : F.t; target : F.t } [@@deriving show, snarky]
end

module Mk(Count : sig val n : int end) = struct
  module Witness = struct
    type t = Proof of Proof.t | List of F.t list
  end
  module RefWitness = MkRef (Witness)
  
  type t = { source : F.t; target: F.t; witness : RefWitness.t } [@@deriving snarky]

  let dummy_proof = Pickles.Proof.dummy Nat.N2.n Nat.N2.n Nat.N1.n ~domain_log2:14

  (* Do some checking here to avoid having two create unnecessary proofs, see issue #101 *)
  (* Creates a constraint between source and target such that if there is some actionss in between
     to link them, we return false (proof needn't be checked), but return true *)
  let proof_must_verify_impl (witness : RefWitness.var) (t : Stmt.var) : Boolean.var =
    let module FList' = struct
      module T = F
      let length = Count.n
    end in
    let module FList = SnarkList (FList') in
    let actionss = exists FList.typ ~compute:(fun () ->
      match As_prover.Ref.get witness with
        | List actionss -> List.append actionss (List.init (Count.n - List.length actionss) ~f:(fun _ -> Outside_hash_image.t))
        | Proof _ -> List.init Count.n ~f:(fun _ -> Outside_hash_image.t)
    ) in
    let f actions target =
      (* Bad unsafe use, with mismatching data and hash, but it works *)
      let actions' =
        Data_as_hash.make_unsafe actions (As_prover.Ref.create (fun () -> []))
      in
      let open Field in
      if_
        (equal actions @@ constant Outside_hash_image.t)
        ~then_:target
        ~else_:(Actions.push_events_checked target actions')
    in
    let target' = List.fold_right ~init:t.source ~f actionss in
    Boolean.not (Field.equal target' t.target)


  (* Do some checking here to avoid having two create unnecessary proofs, see issue #101 *)
  let verify ?check ({ source; target; witness } : var) :
      (Stmt.var, Nat.N2.n) Pickles.Inductive_rule.Previous_proof_statement.t =
    let proof_must_verify = proof_must_verify_impl witness {source; target} in
    { public_input = ({ source; target } : Stmt.var)
    ; proof_must_verify = (match check with
      | Some check -> Boolean.(check &&& proof_must_verify)
      | None -> proof_must_verify)
    ; proof = As_prover.Ref.create (fun () ->
        (match As_prover.Ref.get witness with
          | Proof p -> p
          | List _ -> dummy_proof
        )
    )
    }

  let statement_var ({ source; target } : var) : Stmt.var = { source; target }

  let statement ({ source; target } : t) : Stmt.t = { source; target }
end

module P0 = Mk(struct let n = 0 end)
module P256 = Mk(struct let n = 256 end)

(** Merge two matching statements *)
module MkMerge (Left : module type of P256) (Right : module type of P256) = struct
  module Witness = struct
    type t = { left : Left.t; right : Right.t } [@@deriving snarky]
  end

  include MkHandler (Witness)

  let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
    let Witness.{ left; right } = exists_witness () in
    Field.Assert.equal left.target right.source ;
    Pickles.Inductive_rule.
      { previous_proof_statements = [ Left.verify left; Right.verify right ]
      ; public_output = Stmt.{ source = left.source; target = right.target }
      ; auxiliary_output = ()
      }

  let rule tag : _ Pickles.Inductive_rule.t =
    { identifier = "action state extension step"
    ; prevs = [ tag; tag ]
    ; main
    ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
    }
end
module MergeLeft = MkMerge (P256) (P0)
module MergeRight = MkMerge (P0) (P256)

open Async_kernel

let compilation_result =
  lazy
    (time "Action_state_extension.compile" (fun () ->
         Pickles.compile ()
           ~override_wrap_domain:Pickles_base.Proofs_verified.N1
           ~cache:Cache_dir.cache ~public_input:(Output Stmt.typ)
           ~auxiliary_typ:Typ.unit
           ~branches:(module Nat.N2)
           ~max_proofs_verified:(module Nat.N2)
           ~name:"action state extension"
           ~constraint_constants:
             (Genesis_constants.Constraint_constants.to_snark_keys_header
                constraint_constants )
           ~choices:(fun ~self -> [ MergeLeft.rule self; MergeRight.rule self ]) ) )

let tag = lazy (match force compilation_result with tag, _, _, _ -> tag)

let merge_left_ w =
  match force compilation_result with
  | _, _, _, Pickles.Provers.[ merge_left_; _ ] ->
      time_async "Action_state_extension.step" (fun () ->
          step_ ~handler:(Step.handler w) () )

let merge (left : t) (right : t) : t Deferred.t =
  match force compilation_result with
  | _, _, _, Pickles.Provers.[ _; merge_ ] ->
      let%map stmt, (), proof =
        merge_ ~handler:(Merge.handler { left; right }) ()
      in
      ({ source = stmt.source; target = stmt.target; proof } : t)

(* Head of list should be oldest, tail should be newest *)
let prove ?(dummy = false) ~(source : F.t) (actionss : Actions.t list) :
    t Deferred.t =
  assert (List.length actionss <= N_2_8.n) (* FIXME: support, see FIXME below *) ;
  if List.is_empty actionss then return (dummy_proof source)
  else if dummy then
    let target =
      List.fold_right ~f:(Fun.flip Actions.push_events) ~init:source actionss
    in
    return { (dummy_proof source) with target }
  else
    let%map stmt, (), proof =
      (* FIXME: split up when too big, see FIXME above *)
      step
        Step.Witness.
          { actionss = Step.construct_witness ~actionss
          ; prev = dummy_proof source
          }
    in
    ({ source; target = stmt.target; proof } : t)
