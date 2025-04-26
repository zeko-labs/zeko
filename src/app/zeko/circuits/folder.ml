open Core_kernel
open Snark_params.Tick
open Zeko_util
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

  val wrap_domain : [ `N13 | `N14 | `N15 ] option
end)
() =
struct
  include Inputs

  module Trans = struct
    type t = { source : Stmt.t; target : Stmt.t } [@@deriving snarky]
  end

  type trans = Trans.t = { source : Stmt.t; target : Stmt.t }

  let fold (middle_or_end : [ `Middle | `End ]) (source : Stmt.var)
      (elems : Elem.var array) (length : int V.t) =
    let f target elem =
      let*| target' = step elem target in
      (target', target')
    in
    let* last_target, targets = SnarkArray.fold_map ~f ~init:source elems in
    match middle_or_end with
    | `Middle ->
        let* target =
          exists Stmt.typ
            ~compute:
              (let+ length = V.get length in
               As_prover.read Stmt.typ
                 (if Int.(length > 0) then targets.(length - 1) else source) )
        in
        (* TODO: Do this with a runtime table in the future. *)
        let* equalities =
          Checked.List.map (Array.to_list targets)
            ~f:(var_equal Stmt.typ target)
        in
        let* target_is_source = var_equal Stmt.typ source target in
        let*| () =
          let open Boolean.Expr in
          any (target_is_source :: equalities) |> assert_
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

    val tags : (Trans.var, prevs, 'input) Compile_simple.tags
  end) =
  struct
    open Inputs

    module Elems = SnarkArray.Make (struct
      module T = Elem

      let max_length = iterations

      let dummy_filler = dummy_elem
    end)

    let%snarkydef_ main (w : (Elems.t * Source.t) V.t) =
      let* elems, source =
        exists Typ.(Elems.typ * Source.typ) ~compute:(V.get w)
      in
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
    val branch_name : string

    val iterations : int

    val middle_or_end : [ `Middle | `End ]
  end) =
  Make_rule (struct
    let branch_name = Inputs.branch_name

    let iterations = Inputs.iterations

    let middle_or_end = Inputs.middle_or_end

    module Source = struct
      type t = Trans.t * Proof_V.t

      type var = Trans.var * Proof_V.var

      let typ = Typ.(Trans.typ * Proof_V.typ)
    end

    type prevs = Trans.var Compile_simple.one_prev

    let unwrap_source ((trans, proof) : Source.var) =
      let prevs =
        Compile_simple.One_prev
          { proof_must_verify = Boolean.true_; public_input = trans; proof }
      in
      Checked.return (trans.target, prevs)

    let tags = Compile_simple.One_tag_own
  end)

  module Rule_extend = Make_rule_extend (struct
    let branch_name = "Rule_extend"

    let iterations = extend_iterations

    let middle_or_end = `End
  end)

  module Rule_extend_option = Make_rule_extend (struct
    let branch_name = "Rule_extend_option"

    let iterations = extend_option_iterations

    let middle_or_end = `Middle
  end)

  module Rule_merge = struct
    (* TODO: Allow them to be slightly separated by including an intermediate list of elements. *)
    module Witness = struct
      type t =
        { left : Trans.t
        ; left_proof : Proof_V.t
        ; right : Trans.t
        ; right_proof : Proof_V.t
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
      let* () = assert_equal ~label:__LOC__ Stmt.typ left.target right.source in
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
      { branch_name = "Rule_merge"; tags = Two_tags_own; main }
  end

  type merge_input = Rule_merge.Witness.t =
    { left : trans; left_proof : Proof.t; right : trans; right_proof : Proof.t }

  module System =
  ( val Compile_simple.compile ?wrap_domain
          ~name:("folder(" ^ name ^ ")")
          ~branches:
            [ Rule_leaf.rule
            ; Rule_leaf_option.rule
            ; Rule_extend.rule
            ; Rule_extend_option.rule
            ; Rule_merge.rule
            ]
          ~out_typ:Trans.typ () )

  type tag_var = System.tag_var

  type tag_t = System.tag_t

  let tag = System.tag

  let Compile_simple.[ leaf; leaf_option; extend; extend_option; merge ] =
    System.provers

  module Make (Inputs : sig
    val get_iterations : int
  end) =
  struct
    include Inputs

    module Elems = SnarkArray.Make (struct
      module T = Elem

      let max_length = get_iterations

      let dummy_filler = dummy_elem
    end)

    type t =
      { init_arg : Init.t
      ; t : System.t
      ; excess : Elems.t
      ; use_t : Boolean.t (* TODO: Remove this field. *)
      }
    [@@deriving snarky]

    let%snarkydef_ get_full ?(check : Boolean.var option)
        ({ init_arg; t; excess; use_t } : var) =
      (* We get the supposed source from the initialization of the state machine. *)
      let* source = init ~check init_arg in
      let* { source = proof_source; target = proof_target }, verify_proof =
        System.get ~check:use_t t
      in
      let* source =
        assert_equal_safer ~label:__LOC__ Stmt.typ source proof_source
      in
      let* excess_init =
        if_ use_t ~typ:Stmt.typ ~then_:proof_target ~else_:source
      in
      let* target = fold `Middle excess_init excess.array excess.length in
      Checked.return (`Source source, `Target target, verify_proof)

    let%snarkydef_ get ?check t =
      let*| `Source _source, `Target target, verify = get_full ?check t in
      (target, verify)

    let make ~(proof_source : Stmt.t) ~(proof_target : Stmt.t)
        ?(proof : Proof.t option) (init_arg : Init.t) (excess : Elem.t list) : t
        =
      let t =
        System.make_unchecked ?proof
          { source = proof_source; target = proof_target }
      in
      ({ init_arg; t; excess; use_t = Option.is_some proof } : t)
  end
end
