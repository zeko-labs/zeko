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

module T = struct
  type t = { source : F.t; target : F.t; proof : RefProof.t }
  [@@deriving snarky]

  (* Do some checking here to avoid having two create unnecessary proofs, see issue #101 *)
  let verify ?check ({ source; target; proof } : var) :
      (Stmt.var, Nat.N2.n) Pickles.Inductive_rule.Previous_proof_statement.t =
    let proof_must_verify = Boolean.not (Field.equal source target) in
    let proof_must_verify =
      match check with
      | Some check ->
          Boolean.(check &&& proof_must_verify)
      | None ->
          proof_must_verify
    in
    { public_input = { source; target }
    ; proof_must_verify (* Don't check proof if source == target *)
    ; proof = ref_of_v proof
    }

  let statement_var ({ source; target } : var) : Stmt.var = { source; target }

  let statement ({ source; target } : t) : Stmt.t = { source; target }
end

include T

(** Step `n` times *)
module MkStep (N : sig
  val n : int
end) =
struct
  module Actionss = SnarkList (F) (struct let length = N.n end)

  module Witness = struct
    type t = { actionss : Actionss.t; prev : T.t } [@@deriving snarky]
  end

  include MkHandler (Witness)

  (* FIXME: slow, but probably fast enough for now *)
  let construct_witness ~(actionss : Actions.t list) =
    List.append
      (List.map ~f:Actions.hash actionss)
      (List.init
         (N.n - List.length actionss)
         ~f:(fun _ -> Outside_hash_image.t) )

  let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
    let Witness.{ actionss; prev = { source } as prev } = exists_witness () in
    let f actions target' =
      (* Bad unsafe use, with mismatching data and hash, but it works *)
      let actions' =
        Data_as_hash.make_unsafe actions (As_prover.Ref.create (fun () -> []))
      in
      let open Field in
      if_
        (equal actions @@ constant Outside_hash_image.t)
        ~then_:target'
        ~else_:(Actions.push_events_checked target' actions')
    in
    let target' = List.fold_right ~init:source ~f actionss in
    Pickles.Inductive_rule.
      { previous_proof_statements = [ verify prev ]
      ; public_output = Stmt.{ source; target = target' }
      ; auxiliary_output = ()
      }

  let rule tag : _ Pickles.Inductive_rule.t =
    { identifier = "action state extension step"
    ; prevs = [ tag ]
    ; main
    ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
    }
end

(** Merge two matching statements *)
module Merge = struct
  module Witness = struct
    type t = { left : T.t; right : T.t } [@@deriving snarky]
  end

  include MkHandler (Witness)

  let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
    let Witness.{ left; right } = exists_witness () in
    Field.Assert.equal left.target right.source ;
    Pickles.Inductive_rule.
      { previous_proof_statements = [ verify left; verify right ]
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

module N_2_8 = struct
  let n = Int.pow 2 8
end

module Step = MkStep (N_2_8)
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
           ~choices:(fun ~self -> [ Step.rule self; Merge.rule self ]) ) )

let tag = lazy (match force compilation_result with tag, _, _, _ -> tag)

let step w =
  match force compilation_result with
  | _, _, _, Pickles.Provers.[ step_; _ ] ->
      time_async "Action_state_extension.step" (fun () ->
          step_ ~handler:(Step.handler w) () )

let merge (left : t) (right : t) : t Deferred.t =
  match force compilation_result with
  | _, _, _, Pickles.Provers.[ _; merge_ ] ->
      let%map stmt, (), proof =
        merge_ ~handler:(Merge.handler { left; right }) ()
      in
      ({ source = stmt.source; target = stmt.target; proof } : t)

let dummy_proof source : t =
  { source
  ; target = source
  ; proof = Pickles.Proof.dummy Nat.N2.n Nat.N2.n Nat.N1.n ~domain_log2:14
  }

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
