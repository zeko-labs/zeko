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

  module Transition = struct
    module Stmt = struct
      type t = { source : Stmt.t; target : Stmt.t } [@@deriving snarky]
    end

    type t = { stmt : Stmt.t; proof : ProofOptionV.t } [@@deriving snarky]
  end

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

    val tags : (Transition.Stmt.var, prevs) Compile_simple.tags
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
      Compile_simple.{ out = ({ source; target } : Transition.Stmt.var); prevs }

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

  module Rule_extend = Make_rule (struct
    let branch_name = "Rule_extend"

    let iterations = extend_iterations

    let middle_or_end = `End

    module Source = Transition

    type prevs =
      (Transition.Stmt.var, Compile_simple.self_width) Compile_simple.one_prev

    let unwrap_source (trans : Transition.var) =
      let*| proof =
        As_prover.(V.get trans.proof >>| fun x -> Option.value_exn x)
        |> V.create
      in
      let prevs =
        Compile_simple.One_prev
          { proof_must_verify = Boolean.true_
          ; public_input = trans.stmt
          ; proof
          }
      in
      (trans.stmt.target, prevs)

    let tags = Compile_simple.One_tag Own_tag
  end)

  module Rule_extend_option = Make_rule (struct
    let branch_name = "Rule_extend_option"

    let iterations = extend_option_iterations

    let middle_or_end = `Middle

    module Source = Transition

    type prevs =
      (Transition.Stmt.var, Compile_simple.self_width) Compile_simple.one_prev

    let unwrap_source (trans : Transition.var) =
      let*| proof =
        As_prover.(V.get trans.proof >>| fun x -> Option.value_exn x)
        |> V.create
      in
      let prevs =
        Compile_simple.One_prev
          { proof_must_verify = Boolean.true_
          ; public_input = trans.stmt
          ; proof
          }
      in
      (trans.stmt.target, prevs)

    let tags = Compile_simple.One_tag Own_tag
  end)

  module Rule_merge = struct
    module Witness = struct
      type t = { left : Transition.t; right : Transition.t } [@@deriving snarky]
    end

    let%snarkydef_ main (w : Witness.t V.t) =
      let* Witness.{ left; right } = exists ~compute:(V.get w) Witness.typ in
      let new_stmt : Transition.Stmt.var =
        { source = left.stmt.source; target = right.stmt.target }
      in
      let* left_proof =
        As_prover.(V.get left.proof >>| fun x -> Option.value_exn x) |> V.create
      in
      let* right_proof =
        As_prover.(V.get right.proof >>| fun x -> Option.value_exn x)
        |> V.create
      in
      Checked.return
        Compile_simple.
          { prevs =
              Two_prevs
                ( { proof_must_verify = Boolean.true_
                  ; public_input = left.stmt
                  ; proof = left_proof
                  }
                , { proof_must_verify = Boolean.true_
                  ; public_input = right.stmt
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
       compile_simple ?override_wrap_domain
         ~name:("folder(" ^ name ^ ")")
         ~branches:
           [ Rule_leaf.rule
           ; Rule_leaf_option.rule
           ; Rule_extend.rule
           ; Rule_extend_option.rule
           ; Rule_merge.rule
           ]
         ~out_typ:Transition.Stmt.typ () )

  let tag :
      ( Transition.Stmt.var
      , Transition.Stmt.t
      , Compile_simple.self_width
      , Pickles_types.Nat.N5.n )
      Pickles.Tag.t
      lazy_t =
    lazy
      ( match force compilation_result with
      | Result { tag; provers = _; tag_length = S (S (S (S (S Z)))) } ->
          tag )

  let leaf (source : Stmt.t) (elems : Elem.t list) : Transition.t Promise.t =
    match force compilation_result with
    | Result { tag = _; provers = [ leaf; _; _; _; _ ]; tag_length = _ } ->
        let@ stmt, proof = leaf { elems; source } |> Promise.( >>| ) in
        ({ stmt; proof = Some proof } : Transition.t)

  let leaf_option (source : Stmt.t) (elems : Elem.t list) :
      Transition.t Promise.t =
    match force compilation_result with
    | Result { tag = _; provers = [ _; leaf_option; _; _; _ ]; tag_length = _ }
      ->
        let@ stmt, proof = leaf_option { elems; source } |> Promise.( >>| ) in
        ({ stmt; proof = Some proof } : Transition.t)

  let extend (prev : Transition.t) (elems : Elem.t list) :
      Transition.t Promise.t =
    match force compilation_result with
    | Result { tag = _; provers = [ _; _; extend; _; _ ]; tag_length = _ } ->
        let@ stmt, proof = extend { elems; source = prev } |> Promise.( >>| ) in
        ({ stmt; proof = Some proof } : Transition.t)

  let extend_option (prev : Transition.t) (elems : Elem.t list) :
      Transition.t Promise.t =
    match force compilation_result with
    | Result
        { tag = _; provers = [ _; _; _; extend_option; _ ]; tag_length = _ } ->
        let@ stmt, proof =
          extend_option { elems; source = prev } |> Promise.( >>| )
        in
        ({ stmt; proof = Some proof } : Transition.t)

  let _merge (left : Transition.t) (right : Transition.t) :
      Transition.t Promise.t =
    match force compilation_result with
    | Result { tag = _; provers = [ _; _; _; _; merge ]; tag_length = _ } ->
        let@ stmt, proof = merge { left; right } |> Promise.( >>| ) in
        ({ stmt; proof = Some proof } : Transition.t)

  let dummy_proof () =
    let open Pickles_types in
    Pickles.Proof.dummy Nat.N2.n Nat.N2.n Nat.N2.n ~domain_log2:15

  module Trans = Transition.Stmt

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

    let%snarkydef_ get ?(check : Boolean.var option)
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
      let stmt : Transition.Stmt.var = { source; target } in
      Checked.return
        ( stmt
        , ( { public_input =
                ({ source; target = proof_target } : Transition.Stmt.var)
            ; proof_must_verify
            ; proof
            }
            : _ Compile_simple.prev ) )

    type 'a list_with_length = { list : 'a list; length : int }

    let split_n (xs : 'a list_with_length) (count : int) :
        'a list * 'a list_with_length =
      let left, right = List.split_n xs.list count in
      ( left
      , { list = right; length = max (xs.length - count) 0 |> min xs.length } )

    let split_n_pad (xs : 'a list_with_length) (count : int) ~(f : 'a -> 'b)
        ~(padding : 'b) : 'b list * 'a list_with_length =
      let left, right = List.split_n xs.list count in
      let left =
        List.init (count - xs.length |> max 0 |> min count) ~f:(fun _ -> padding)
        @ List.map ~f left
      in
      ( left
      , { list = right; length = max (xs.length - count) 0 |> min xs.length } )

    let prove (init_arg : Init.t) (elems : Elem.t list) : t Promise.t =
      let source =
        run_and_check_exn
          (let init_arg = constant Init.typ init_arg in
           let* source = init ~check:None init_arg in
           As_prover.read Stmt.typ source |> Checked.return )
      in
      let ( let$ ) = Promise.( >>= ) in
      let ( let$| ) = Promise.( >>| ) in
      let rec go (trans : Transition.t) (elems : Elem.t list_with_length) :
          (Transition.t * Elem.t list_with_length) Promise.t =
        if elems.length <= get_iterations then Promise.return (trans, elems)
        else if elems.length >= extend_iterations then
          let to_process, elems = split_n elems extend_iterations in
          let$ trans = extend trans to_process in
          go trans elems
        else
          let to_process, elems =
            split_n_pad elems extend_option_iterations
              ~f:(fun x -> x)
              ~padding:dummy_elem
          in
          let$ trans = extend_option trans to_process in
          go trans elems
      in
      let elems = { list = elems; length = List.length elems } in
      let$ proof_target, proof, elems =
        if elems.length <= get_iterations then
          Promise.return (source, None, elems)
        else if elems.length >= leaf_iterations then
          let to_process, elems = split_n elems leaf_iterations in
          let$ trans = leaf source to_process in
          let$| trans, elems = go trans elems in
          (trans.stmt.target, trans.proof, elems)
        else if elems.length >= leaf_option_iterations then
          let to_process, elems = split_n elems leaf_iterations in
          let$ trans = leaf source to_process in
          let$| trans, elems = go trans elems in
          (trans.stmt.target, trans.proof, elems)
        else
          let to_process, elems =
            split_n_pad elems leaf_option_iterations
              ~f:(fun x -> x)
              ~padding:dummy_elem
          in
          let$ trans = leaf_option source to_process in
          let$| trans, elems = go trans elems in
          (trans.stmt.target, trans.proof, elems)
      in
      Promise.return ({ init_arg; proof_target; proof; excess = elems.list } : t)
  end
end
