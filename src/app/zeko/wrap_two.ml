open Core_kernel
open Mina_base
open Snark_params.Tick.Run
(* Impure interface to snarky, FIXME: replace with pure one *)

open Zkapp_basic
open Account_update
open Zeko_util

module Opaque (X : CircuitType) = struct
  type t = F.t array
  type var = F.var array
  let typ = 
    let Typ typ' = X.typ in
    Typ.array ~length:typ'.size_in_field_elements F.typ
end

module type Verifiable = sig
  module Stmt : sig
    type t
    type var
    val typ : (var, t) Typ.t
  end
  module Witness : sig
    type t
    val contains_proof : t -> bool
  end
  val verify : check:Boolean.var option -> Stmt.var -> Witness.t option V.t ->
   ( Stmt.var
     , Pickles_types.Nat.N2.n )
     Pickles.Inductive_rule.Previous_proof_statement.t
  type tag_var
  type tag_value
  val tag :
    unit -> (
    tag_var
    , tag_value
    , Pickles_types.Nat.N2.n
    , Pickles_types.Nat.N2.n )
    Pickles.Tag.t
end

let typ_size (Typ typ : ('var, 't) Typ.t) = typ.size_in_field_elements

(** NB: X and Y must have same size in # field elements *)
module WrapTwo (X : Verifiable) (Y : Verifiable) = struct
  let () = assert (Int.(typ_size X.Stmt.typ = typ_size Y.Stmt.typ))
  let stmt_size = typ_size X.Stmt.typ

  module Stmt = struct
    type t = F.t array
    type var = F.var array
    let typ = Typ.array ~length:stmt_size F.typ
    let combine : X.Stmt.t -> Y.Stmt.t -> t
      = fun x y -> 
        let (Typ x_typ) = X.Stmt.typ in
        let (Typ y_typ) = Y.Stmt.typ in
        let x_fields, _ = x_typ.value_to_fields x in
        let y_fields, _ = y_typ.value_to_fields y in
        Array.zip_exn x_fields y_fields
        |> Array.map ~f:(fun (x, y) -> Random_oracle.hash [|x ; y|])
    let combine_var : X.Stmt.var -> Y.Stmt.var -> var
      = fun x y -> 
        let (Typ x_typ) = X.Stmt.typ in
        let (Typ y_typ) = Y.Stmt.typ in
        let x_fields, _ = x_typ.var_to_fields x in
        let y_fields, _ = y_typ.var_to_fields y in
        Array.zip_exn x_fields y_fields
        |> Array.map ~f:(fun (x, y) -> Random_oracle.Checked.hash [|x ; y|])
  end

  module Witness = struct
    (* The witness for the above statement/relation.
       In Proof case, we have generated a proof that succinctly describes
       the above statement.
       In Short_circuit case, we skip generating a proof,
       which is only possible if either X or Y don't need a proof.
       If neither needn't a proof, we don't do need to verify any proof at all.
       The core goal is to always (recursively) verify at most one proof.
    *)
    type t = Proof of Proof.t | Short_circuit of X.Witness.t * Y.Witness.t
  end
  module RefWitness = MkRef(Witness)
    
  type t = { x : X.Stmt.t; y : Y.Stmt.t; witness : RefWitness.t }
  [@@deriving snarky]
  
  let contains_proof = function
    | Witness.Proof _ -> true
    | Witness.Short_circuit (x, y) -> assert (not @@ X.Witness.contains_proof x && Y.Witness.contains_proof y) ; X.Witness.contains_proof x || Y.Witness.contains_proof y

  let verify ~check ({ x; y; witness } : var) :
      (Stmt.var, Pickles_types.Nat.N2.n) Pickles.Inductive_rule.Previous_proof_statement.t =
    let public_input = exists Stmt.typ in
    let proof_must_verify = exists Boolean.typ in
    let verify_x_or_y = exists Boolean.typ in
    let check_x = exists Boolean.typ in
    let verify_x = X.verify x ~check:check_x (V.map witness ~f:(function None -> None | Some x -> )) in
    Boolean.Assert.is_true @@ proof_must_verify ;
    let proof = ref None in
    { public_input = public_input
    ; proof_must_verify (* Don't check proof if source == target *)
    ; proof
    }

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
end
