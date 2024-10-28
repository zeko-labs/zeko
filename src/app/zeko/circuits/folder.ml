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

  module ElemOption : SnarkType

  val elem_to_option : Elem.t -> ElemOption.t

  val elem_option_none : ElemOption.t

  module Stmt : SnarkType

  module Init : SnarkType

  val init : check:Boolean.var option -> Init.var -> Stmt.var Checked.t

  val step :
    check:Boolean.var option -> Elem.var -> Stmt.var -> Stmt.var Checked.t

  val step_option :
    check:Boolean.var option -> ElemOption.var -> Stmt.var -> Stmt.var Checked.t

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

  module Make_rule_leaf (Inputs : sig
    val iterations : int

    module E : SnarkType

    val step_e : E.var -> Stmt.var -> Stmt.var Checked.t

    val identifier : string
  end) =
  struct
    open Inputs

    module Es =
      SnarkList
        (E)
        (struct
          let length = iterations
        end)

    module Witness = struct
      type t = { elems : Es.t; source : Stmt.t } [@@deriving snarky]
    end

    let%snarkydef_ main (w : Witness.t V.t) =
      let* Witness.{ elems; source } = exists Witness.typ ~compute:(V.get w) in
      let* target = Checked.List.fold ~f:(Fun.flip step_e) ~init:source elems in
      Checked.return
        Compile_simple.
          { out = ({ source; target } : Transition.Stmt.var); prevs = No_prevs }

    let rule : _ Compile_simple.branch =
      { branch_name = identifier; tags = No_tags; main }
  end

  module Rule_leaf = Make_rule_leaf (struct
    let identifier = "state machine leaf"

    module E = Elem

    let step_e = step ~check:None

    let iterations = leaf_iterations
  end)

  module Rule_leaf_option = Make_rule_leaf (struct
    let identifier = "state machine leaf option"

    module E = ElemOption

    let step_e = step_option ~check:None

    let iterations = leaf_option_iterations
  end)

  module Make_rule_extend (Inputs : sig
    val iterations : int

    module E : SnarkType

    val step_e : E.var -> Stmt.var -> Stmt.var Checked.t

    val identifier : string
  end) =
  struct
    open Inputs

    module Es =
      SnarkList
        (E)
        (struct
          let length = iterations
        end)

    module Witness = struct
      type t = { elems : Es.t; prev : Transition.t } [@@deriving snarky]
    end

    let%snarkydef_ main (w : Witness.t V.t) =
      let* Witness.{ elems; prev } = exists ~compute:(V.get w) Witness.typ in
      let f (state : Stmt.var Checked.t) (elem : E.var) =
        let* state in
        step_e elem state
      in
      let* target =
        List.fold_left ~f ~init:(Checked.return prev.stmt.target) elems
      in
      let* proof =
        As_prover.(V.get prev.proof >>| fun x -> Option.value_exn x) |> V.create
      in
      Checked.return
        Compile_simple.
          { prevs =
              One_prev
                { proof_must_verify = Boolean.true_
                ; public_input = prev.stmt
                ; proof
                }
          ; out = ({ source = prev.stmt.source; target } : Transition.Stmt.var)
          }

    let rule : _ Compile_simple.branch =
      { branch_name = identifier; tags = One_tag Own_tag; main }
  end

  module Rule_extend = Make_rule_extend (struct
    let identifier = "state machine extend"

    module E = Elem

    let step_e = step ~check:None

    let iterations = extend_iterations
  end)

  module Rule_extend_option = Make_rule_extend (struct
    let identifier = "state machine extend option"

    module E = ElemOption

    let step_e = step_option ~check:None

    let iterations = extend_option_iterations
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
      { branch_name = "state machine merge"
      ; tags = Two_tags (Own_tag, Own_tag)
      ; main
      }
  end

  let name = "State_machine.Make(" ^ name ^ ")"

  let compilation_result =
    lazy
      (let@ () = Promise.block_on_async_exn in
       printf "compiling %s\n" name ;
       compile_simple
         ?override_wrap_domain
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

  let leaf_option (source : Stmt.t) (elems : ElemOption.t list) :
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
        let@ stmt, proof = extend { elems; prev } |> Promise.( >>| ) in
        ({ stmt; proof = Some proof } : Transition.t)

  let extend_option (prev : Transition.t) (elems : ElemOption.t list) :
      Transition.t Promise.t =
    match force compilation_result with
    | Result
        { tag = _; provers = [ _; _; _; extend_option; _ ]; tag_length = _ } ->
        let@ stmt, proof = extend_option { elems; prev } |> Promise.( >>| ) in
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

    module Elems =
      SnarkList
        (ElemOption)
        (struct
          let length = get_iterations
        end)

    module IntV = MkV (Int)

    type t =
      { init_arg : Init.t
      ; proof_target : Stmt.t
      ; proof : ProofOptionV.t
      ; excess : Elems.t
            (* FIXME: fix SnarkList such that the out-circuit values don't include dummies. *)
      ; excess_nr_nones : IntV.t
      }
    [@@deriving snarky]

    let%snarkydef_ get ?(check : Boolean.var option)
        ({ init_arg; proof_target; proof; excess; excess_nr_nones = _ } : var) =
      let* () = Checked.return () in
      printf "calling Folder.get\n" ;
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
      let* target =
        Checked.List.fold
          ~f:(fun state elem -> step_option ~check elem state)
          ~init:excess_init excess
      in
      let stmt : Transition.Stmt.var = { source; target } in
      printf "calling Folder.get done\n" ;
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
      printf "calling prove\n" ;
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
        printf "go called\n" ;
        if elems.length <= get_iterations then Promise.return (trans, elems)
        else if elems.length >= extend_iterations then
          let to_process, elems = split_n elems extend_iterations in
          let$ trans = extend trans to_process in
          go trans elems
        else
          let to_process, elems =
            split_n_pad elems extend_option_iterations ~f:elem_to_option
              ~padding:elem_option_none
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
            split_n_pad elems leaf_option_iterations ~f:elem_to_option
              ~padding:elem_option_none
          in
          let$ trans = leaf_option source to_process in
          let$| trans, elems = go trans elems in
          (trans.stmt.target, trans.proof, elems)
      in
      printf "done calling go\n" ;
      let excess_nr_nones = get_iterations - elems.length in
      let nones = List.init excess_nr_nones ~f:(fun _ -> elem_option_none) in
      let excess = nones @ List.map ~f:elem_to_option elems.list in
      printf "returning from Folder.prove\n" ;
      Promise.return
        ({ init_arg; proof_target; proof; excess; excess_nr_nones } : t)
  end
end
