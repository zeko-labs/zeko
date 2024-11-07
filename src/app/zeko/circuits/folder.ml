open Core_kernel
open Snark_params.Tick
open Zeko_util
open Mina_base
module Max_proofs_verified = Pickles_types.Nat.N2

type tag_max_proofs_verified = Max_proofs_verified.n

module Branches = Pickles_types.Nat.N5

type tag_branches = Branches.n

module Make (Inputs : sig
  module Elem : SnarkType

  val dummy_elem : Elem.t

  module Stmt : SnarkType

  module Init : SnarkType

  val init : check:Boolean.var option -> Init.var -> Stmt.var Checked.t

  val step : Elem.var -> Stmt.var -> Stmt.var Checked.t

  val leaf_iterations : int

  val leaf_option_iterations : int

  val extend_iterations : int

  val extend_option_iterations : int

  val name : string

  val override_wrap_domain : Pickles_base.Proofs_verified.t option
end) =
struct
  open Inputs

  module Trans = struct
    type t = { source : Stmt.t; target : Stmt.t } [@@deriving snarky]
  end

  type t = { source : Stmt.t; target : Stmt.t; proof : Mina_base.Proof.t }

  let fold (middle_or_end : [ `Middle | `End ]) (source : Stmt.var)
      (elems : Elem.var array) (length : int V.t) =
    let f target elem =
      let*| target' = step elem target in
      (target', target')
    in
    let* last_target, targets = Checked.Array.fold_map ~f ~init:source elems in
    match middle_or_end with
    | `Middle ->
        let* target =
          exists Stmt.typ
            ~compute:
              (let+ length = V.get length in
               (* there must be at least one element *)
               assert (Int.(length > 0)) ;
               As_prover.read Stmt.typ targets.(length - 1) )
        in
        (* TODO: Do this with a runtime table in the future. *)
        let* equalities =
          Checked.List.map (Array.to_list targets)
            ~f:(var_equal Stmt.typ target)
        in
        let*| () =
          let open Boolean.Expr in
          any equalities |> assert_
        in
        target
    | `End ->
        Checked.return last_target

  module Make_rule (Inputs : sig
    val branch_name : string

    val iterations : int

    val middle_or_end : [ `Middle | `End ]

    module Source : SnarkType

    type prevs

    val unwrap_source :
      Source.var -> (Stmt.var * prevs Compile_simple.prevs) Checked.t

    val tags : (Trans.var, prevs) Compile_simple.tags
  end) =
  struct
    open Inputs

    module Elems = SnarkArray (struct
      module T = Elem

      let max_length = iterations

      let dummy_filler = dummy_elem
    end)

    module Witness = struct
      type t = { elems : Elems.t; source : Source.t } [@@deriving snarky]
    end

    let%snarkydef_ main (w : Witness.t V.t) =
      let* Witness.{ elems; source } = exists Witness.typ ~compute:(V.get w) in
      let* source, prevs = unwrap_source source in
      let*| target = fold middle_or_end source elems.array elems.length in
      Compile_simple.{ out = ({ source; target } : Trans.var); prevs }

    let rule : _ Compile_simple.branch = { branch_name; tags; main }
  end

  module Rule_leaf = Make_rule (struct
    let branch_name = "Rule_leaf"

    let iterations = leaf_iterations

    let middle_or_end = `End

    module Source = Stmt

    type prevs = Compile_simple.no_prevs

    let unwrap_source stmt = Checked.return (stmt, Compile_simple.No_prevs)

    let tags = Compile_simple.No_tags
  end)

  module Rule_leaf_option = Make_rule (struct
    let branch_name = "Rule_leaf_option"

    let iterations = leaf_option_iterations

    let middle_or_end = `Middle

    module Source = Stmt

    type prevs = Compile_simple.no_prevs

    let unwrap_source stmt = Checked.return (stmt, Compile_simple.No_prevs)

    let tags = Compile_simple.No_tags
  end)

  module Make_rule_extend (Inputs : sig
    val iterations : int

    val middle_or_end : [ `Middle | `End ]
  end) =
  Make_rule (struct
    let branch_name = "Rule_extend"

    let iterations = Inputs.iterations

    let middle_or_end = Inputs.middle_or_end

    module Source = struct
      type t = Trans.t * ProofV.t

      type var = Trans.var * ProofV.var

      let typ = Typ.(Trans.typ * ProofV.typ)
    end

    type prevs = (Trans.var, Compile_simple.self_width) Compile_simple.one_prev

    let unwrap_source ((trans, proof) : Source.var) =
      let prevs =
        Compile_simple.One_prev
          { proof_must_verify = Boolean.true_; public_input = trans; proof }
      in
      Checked.return (trans.target, prevs)

    let tags = Compile_simple.One_tag Own_tag
  end)

  module Rule_extend = Make_rule_extend (struct
    let iterations = extend_iterations

    let middle_or_end = `End
  end)

  module Rule_extend_option = Make_rule_extend (struct
    let iterations = extend_option_iterations

    let middle_or_end = `Middle
  end)

  module Rule_merge = struct
    (* TODO: Allow them to be slightly separated by including an intermediate list of elements. *)
    module Witness = struct
      type t =
        { left : Trans.t
        ; left_proof : ProofV.t
        ; right : Trans.t
        ; right_proof : ProofV.t
        }
      [@@deriving snarky]
    end

    let%snarkydef_ main (w : Witness.t V.t) =
      let* Witness.{ left; left_proof; right; right_proof } =
        exists ~compute:(V.get w) Witness.typ
      in
      let new_stmt : Trans.var =
        { source = left.source; target = right.target }
      in
      Checked.return
        Compile_simple.
          { prevs =
              Two_prevs
                ( { proof_must_verify = Boolean.true_
                  ; public_input = left
                  ; proof = left_proof
                  }
                , { proof_must_verify = Boolean.true_
                  ; public_input = right
                  ; proof = right_proof
                  } )
          ; out = new_stmt
          }

    let rule : _ Compile_simple.branch =
      { branch_name = "Rule_merge"; tags = Two_tags (Own_tag, Own_tag); main }
  end

  let name = "State_machine.Make(" ^ name ^ ")"

  let compilation_result =
    lazy
      (let@ () = Promise.block_on_async_exn in
       Compile_simple.compile ?override_wrap_domain
         ~name:("folder(" ^ name ^ ")")
         ~branches:
           [ Rule_leaf.rule
           ; Rule_leaf_option.rule
           ; Rule_extend.rule
           ; Rule_extend_option.rule
           ; Rule_merge.rule
           ]
         ~out_typ:Trans.typ () )

  type tag_var = Trans.var

  type tag_t = Trans.t

  let tag :
      ( tag_var
      , tag_t
      , Compile_simple.self_width
      , Pickles_types.Nat.N5.n )
      Pickles.Tag.t
      lazy_t =
    lazy
      ( match force compilation_result with
      | Result { tag; provers = _; tag_length = S (S (S (S (S Z)))) } ->
          tag )

  let leaf (source : Stmt.t) (elems : Elem.t list) : t Promise.t =
    match force compilation_result with
    | Result { tag = _; provers = [ leaf; _; _; _; _ ]; tag_length = _ } ->
        let@ ({ source; target } : Trans.t), proof =
          leaf { elems; source } |> Promise.( >>| )
        in
        ({ source; target; proof } : t)

  let leaf_option (source : Stmt.t) (elems : Elem.t list) : t Promise.t =
    match force compilation_result with
    | Result { tag = _; provers = [ _; leaf_option; _; _; _ ]; tag_length = _ }
      ->
        let@ ({ source; target } : Trans.t), proof =
          leaf_option { elems; source } |> Promise.( >>| )
        in
        ({ source; target; proof } : t)

  let extend (prev : t) (elems : Elem.t list) : t Promise.t =
    match force compilation_result with
    | Result { tag = _; provers = [ _; _; extend; _; _ ]; tag_length = _ } ->
        let@ ({ source; target } : Trans.t), proof =
          extend
            { elems
            ; source =
                ({ source = prev.source; target = prev.target }, prev.proof)
            }
          |> Promise.( >>| )
        in
        ({ source; target; proof } : t)

  let extend_option (prev : t) (elems : Elem.t list) : t Promise.t =
    match force compilation_result with
    | Result
        { tag = _; provers = [ _; _; _; extend_option; _ ]; tag_length = _ } ->
        let@ ({ source; target } : Trans.t), proof =
          extend_option
            { elems
            ; source =
                ({ source = prev.source; target = prev.target }, prev.proof)
            }
          |> Promise.( >>| )
        in
        ({ source; target; proof } : t)

  let merge (left : t) (right : t) : t Promise.t =
    match force compilation_result with
    | Result { tag = _; provers = [ _; _; _; _; merge ]; tag_length = _ } ->
        let@ ({ source; target } : Trans.t), proof =
          merge
            { left = { source = left.source; target = left.target }
            ; right = { source = right.source; target = right.target }
            ; left_proof = left.proof
            ; right_proof = right.proof
            }
          |> Promise.( >>| )
        in
        ({ source; target; proof } : t)

  let dummy_proof () =
    let open Pickles_types in
    Pickles.Proof.dummy Nat.N2.n Nat.N2.n Nat.N2.n ~domain_log2:15

  module Make (Inputs : sig
    val get_iterations : int
  end) =
  struct
    open Inputs

    module Elems = SnarkArray (struct
      module T = Elem

      let max_length = get_iterations

      let dummy_filler = dummy_elem
    end)

    type t =
      { init_arg : Init.t
      ; proof_target : Stmt.t
      ; proof : ProofOptionV.t
      ; excess : Elems.t
      }
    [@@deriving snarky]

    let%snarkydef_ get_full ?(check : Boolean.var option)
        ({ init_arg; proof_target; proof; excess } : var) =
      let* has_proof =
        exists Boolean.typ ~compute:As_prover.(V.get proof >>| Option.is_some)
      in
      (* We verify the proof if check is true or None, and there is a proof *)
      let* proof_must_verify =
        match check with
        | Some check ->
            Boolean.(check &&& has_proof)
        | None ->
            Checked.return has_proof
      in
      (* We get the supposed source from the initialization of the state machine. *)
      let* source = init ~check init_arg in
      (* We do some wrangling to get either the proof or a dummy proof. *)
      let* (proof : Proof.t V.t) =
        As_prover.(V.get proof >>| Option.value ~default:(dummy_proof ()))
        |> V.create
      in
      let* excess_init =
        if_ has_proof ~typ:Stmt.typ ~then_:proof_target ~else_:source
      in
      let* target = fold `Middle excess_init excess.array excess.length in
      let stmt : Trans.var = { source; target } in
      Checked.return
        ( `Source stmt.source
        , `Target stmt.target
        , ( { public_input = ({ source; target = proof_target } : Trans.var)
            ; proof_must_verify
            ; proof
            }
            : _ Compile_simple.prev ) )

    let%snarkydef_ get ?check t =
      let*| `Source _source, `Target target, verify = get_full ?check t in
      (target, verify)
  end
end
