(* TODO: optimize list processing algorithms, maybe use Seq *)

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

    include MkHandler (Witness)

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
      let* Witness.{ elems; source } = exists_witness in
      let f (state : Stmt.var Checked.t) (elem : E.var) =
        let* state in
        step_e elem state
      in
      let* target = List.fold_left ~f ~init:(Checked.return source) elems in
      Checked.return
        Pickles.Inductive_rule.
          { previous_proof_statements = []
          ; public_output = ({ source; target } : Transition.Stmt.var)
          ; auxiliary_output = ()
          }

    let rule : _ Pickles.Inductive_rule.t =
      { identifier
      ; prevs = []
      ; main = (fun x -> main x |> Run.run_checked)
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
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

    include MkHandler (Witness)

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
      let* Witness.{ elems; prev } = exists_witness in
      let f (state : Stmt.var Checked.t) (elem : E.var) =
        let* state in
        step_e elem state
      in
      let* target =
        List.fold_left ~f ~init:(Checked.return prev.stmt.target) elems
      in
      let* proof =
        As_prover.(V.get prev.proof >>| fun x -> Option.value_exn x)
        |> As_prover.Ref.create
      in
      Checked.return
        Pickles.Inductive_rule.
          { previous_proof_statements =
              [ { proof_must_verify = Boolean.true_
                ; public_input = prev.stmt
                ; proof
                }
              ]
          ; public_output =
              ({ source = prev.stmt.source; target } : Transition.Stmt.var)
          ; auxiliary_output = ()
          }

    let rule tag : _ Pickles.Inductive_rule.t =
      { identifier
      ; prevs = [ tag ]
      ; main = (fun x -> main x |> Run.run_checked)
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
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

    include MkHandler (Witness)

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
      let* Witness.{ left; right } = exists_witness in
      let new_stmt : Transition.Stmt.var =
        { source = left.stmt.source; target = right.stmt.target }
      in
      let* (left_proof : Proof.t As_prover.Ref.t) =
        As_prover.(V.get left.proof >>| fun x -> Option.value_exn x)
        |> As_prover.Ref.create
      in
      let* (right_proof : Proof.t As_prover.Ref.t) =
        As_prover.(V.get left.proof >>| fun x -> Option.value_exn x)
        |> As_prover.Ref.create
      in
      Checked.return
        Pickles.Inductive_rule.
          { previous_proof_statements =
              [ { proof_must_verify = Boolean.true_
                ; public_input = left.stmt
                ; proof = left_proof
                }
              ; { proof_must_verify = Boolean.true_
                ; public_input = right.stmt
                ; proof = right_proof
                }
              ]
          ; public_output = new_stmt
          ; auxiliary_output = ()
          }

    let rule tag : _ Pickles.Inductive_rule.t =
      { identifier = "state machine merge"
      ; prevs = [ tag; tag ]
      ; main = (fun x -> main x |> Run.run_checked)
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end

  let name = "State_machine.Make(" ^ name ^ ")"

  let compilation_result =
    lazy
      (time (name ^ ".compile") (fun () ->
           Pickles.compile () ?override_wrap_domain ~cache:Cache_dir.cache
             ~public_input:(Output Transition.Stmt.typ) ~auxiliary_typ:Typ.unit
             ~branches:(module Branches)
             ~max_proofs_verified:(module Max_proofs_verified)
             ~name:(name ^ ".compile")
             ~constraint_constants:
               (Genesis_constants.Constraint_constants.to_snark_keys_header
                  constraint_constants )
             ~choices:(fun ~self ->
               [ Rule_leaf.rule
               ; Rule_leaf_option.rule
               ; Rule_extend.rule self
               ; Rule_extend_option.rule self
               ; Rule_merge.rule self
               ] ) ) )

  let tag = lazy (match force compilation_result with tag, _, _, _ -> tag)

  let leaf (source : Stmt.t) (elems : Elem.t list) :
      Transition.t Async_kernel.Deferred.t =
    let open Async_kernel in
    match force compilation_result with
    | _, _, _, Pickles.Provers.[ leaf_; _; _; _; _ ] ->
        time_async (name ^ ".leaf") (fun () ->
            let%map stmt, (), proof =
              leaf_ ~handler:(Rule_leaf.handler { elems; source }) ()
            in
            ({ stmt; proof = Some proof } : Transition.t) )

  let leaf_option (source : Stmt.t) (elems : ElemOption.t list) :
      Transition.t Async_kernel.Deferred.t =
    let open Async_kernel in
    match force compilation_result with
    | _, _, _, Pickles.Provers.[ _; leaf_option_; _; _; _ ] ->
        time_async (name ^ ".leaf") (fun () ->
            let%map stmt, (), proof =
              leaf_option_
                ~handler:(Rule_leaf_option.handler { elems; source })
                ()
            in
            ({ stmt; proof = Some proof } : Transition.t) )

  let extend (prev : Transition.t) (elems : Elem.t list) :
      Transition.t Async_kernel.Deferred.t =
    let open Async_kernel in
    match force compilation_result with
    | _, _, _, Pickles.Provers.[ _; _; extend_; _; _ ] ->
        time_async (name ^ ".extend") (fun () ->
            let%map stmt, (), proof =
              extend_ ~handler:(Rule_extend.handler { elems; prev }) ()
            in
            ({ stmt; proof = Some proof } : Transition.t) )

  let extend_option (prev : Transition.t) (elems : ElemOption.t list) :
      Transition.t Async_kernel.Deferred.t =
    let open Async_kernel in
    match force compilation_result with
    | _, _, _, Pickles.Provers.[ _; _; _; extend_option_; _ ] ->
        time_async (name ^ ".extend") (fun () ->
            let%map stmt, (), proof =
              extend_option_
                ~handler:(Rule_extend_option.handler { elems; prev })
                ()
            in
            ({ stmt; proof = Some proof } : Transition.t) )

  (*
  let merge (left : Transition.t) (right : Transition.t) :
      Transition.t Async_kernel.Deferred.t =
    let open Async_kernel in
    match force compilation_result with
    | _, _, _, Pickles.Provers.[ _; _; _; _; merge_ ] ->
        time_async (name ^ ".merge") (fun () ->
            let%map stmt, (), proof =
              merge_ ~handler:(Rule_merge.handler { left; right }) ()
            in
            ({ stmt; proof = Some proof } : Transition.t) )
*)

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
        ({ init_arg; proof_target; proof; excess ; excess_nr_nones = _ } : var) =
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
      let* (proof : Proof.t As_prover.Ref.t) =
        As_prover.(V.get proof >>| Option.value ~default:(dummy_proof ()))
        |> As_prover.Ref.create
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
      Checked.return
        ( stmt
        , ( { public_input =
                ({ source; target = proof_target } : Transition.Stmt.var)
            ; proof_must_verify
            ; proof
            }
            : _ Pickles.Inductive_rule.Previous_proof_statement.t ) )

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

    let prove (init_arg : Init.t) (elems : Elem.t list) :
        t Async_kernel.Deferred.t =
      let source =
        run_and_check
          (let init_arg = constant Init.typ init_arg in
           let* source = init ~check:None init_arg in
           As_prover.read Stmt.typ source |> Checked.return )
        |> Or_error.ok_exn
      in
      let ( let$ ) = Async_kernel.( >>= ) in
      let ( let$| ) = Async_kernel.( >>| ) in
      let rec go (trans : Transition.t) (elems : Elem.t list_with_length) :
          (Transition.t * Elem.t list_with_length) Async_kernel.Deferred.t =
        if elems.length <= get_iterations then Async_kernel.return (trans, elems)
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
          Async_kernel.return (source, None, elems)
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
      let excess_nr_nones = get_iterations - elems.length in
      let nones = List.init excess_nr_nones ~f:(fun _ -> elem_option_none) in
      let excess = nones @ List.map ~f:elem_to_option elems.list in
      Async_kernel.return
        ({ init_arg; proof_target; proof; excess; excess_nr_nones } : t)
  end
end
