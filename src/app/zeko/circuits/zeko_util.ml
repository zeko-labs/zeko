open Core_kernel
open Mina_base
open Snark_params.Tick
module Or_ignore = Zkapp_basic.Or_ignore
module Set_or_keep = Zkapp_basic.Set_or_keep

let ( let* ) = Checked.Let_syntax.( >>= )

let ( let*| ) = Checked.Let_syntax.( >>| )

let ( let+ ) = As_prover.Let_syntax.( >>= )

let ( let+| ) = As_prover.Let_syntax.( >>| )

let ( let@ ) : (('a -> 'b) -> 'c) -> ('a -> 'b) -> 'c = ( @@ )

let attach_control_var :
    Account_update.Body.Checked.t -> Zkapp_call_forest.Checked.account_update =
 fun account_update ->
  { account_update =
      { data = account_update
      ; hash =
          Zkapp_command.Call_forest.Digest.Account_update.Checked.create
            account_update
      }
  ; control =
      (let@ () = Mina_base.Prover_value.create in
       Control.None_given )
  }

let attach_control : Account_update.Body.t -> Zkapp_call_forest.account_update =
 fun body ->
  let account_update : Account_update.T.t =
    { body; authorization = Control.None_given }
  in
  { data = account_update
  ; hash = Zkapp_command.Call_forest.Digest.Account_update.create account_update
  }

let constraint_constants = Genesis_constants.Constraint_constants.compiled

(** Converts a value to its constituent fields *)
let value_to_fields (type var value) (typ : (var, value) Typ.t) (x : value) :
    field array =
  let (Typ typ) = typ in
  let fields, _aux = typ.value_to_fields x in
  fields

(** Converts a variable to its constituent fields *)
let var_to_fields (type var value) (typ : (var, value) Typ.t) (x : var) :
    Field.Var.t array =
  let (Typ typ) = typ in
  let fields, _aux = typ.var_to_fields x in
  fields

(** Converts a variable to its constituent field. Fails if typ doesn't fit. *)
let var_to_field (type var value) (typ : (var, value) Typ.t) (x : var) :
    Field.Var.t =
  let (Typ typ) = typ in
  let fields, _aux = typ.var_to_fields x in
  assert (Int.(Array.length fields = 1)) ;
  fields.(0)

(* Intended to be used for custom token accounts *)
let none_permissions : Permissions.t =
  { edit_state = None
  ; send = None
  ; receive = None
  ; set_delegate = None
  ; set_permissions = None
  ; set_verification_key = (None, Mina_numbers.Txn_version.current)
  ; set_zkapp_uri = None
  ; edit_action_state = None
  ; set_token_symbol = None
  ; increment_nonce = None
  ; set_voting_for = None
  ; set_timing = None
  ; access = None
  }

type call_forest =
  ( Account_update.t
  , Zkapp_command.Digest.Account_update.t
  , Zkapp_command.Digest.Forest.t )
  Zkapp_command.Call_forest.t

type call_forest_tree =
  ( Account_update.t
  , Zkapp_command.Digest.Account_update.t
  , Zkapp_command.Digest.Forest.t )
  Zkapp_command.Call_forest.Tree.t

(** Hash a constant string to a field for use as tags. *)
let naive_hash_string_to_field (s : string) =
  Hash_prefix_create.salt s |> Random_oracle.digest

(** Generic function for turning variables into 8 of something  *)
let var_to_state_generic (some : Field.Var.t -> 'option) (none : 'option)
    (typ : ('var, 'value) Typ.t) (x : 'var) : 'option Zkapp_state.V.t =
  let fields = var_to_fields typ x in
  assert (Array.length fields <= 8) ;
  let missing = 8 - Array.length fields in
  Zkapp_state.V.of_list_exn
  @@ List.append
       (List.map ~f:(fun f -> some f) @@ Array.to_list fields)
       (List.init missing ~f:(fun _ -> none))

(** Used for turning variables into app state for updates *)
let var_to_app_state typ x =
  var_to_state_generic Set_or_keep.Checked.set
    (Set_or_keep.Checked.keep ~dummy:Run.Field.zero)
    typ x

(** Used for turning variables into preconditions *)
let var_to_precondition typ x =
  var_to_state_generic
    (Or_ignore.Checked.make_unsafe Boolean.true_)
    (Or_ignore.Checked.make_unsafe Boolean.false_ Run.Field.zero)
    typ x

module Fine = struct
  module Case = struct
    type 'self t =
      | Whole : (('var, 't) Typ.t * 'var option) -> 'self t
      | Recursive : 'self -> 'self t
  end

  type t = [] : t | ( :: ) : t Case.t * t -> t
end

module type Maybe_var_type = sig
  type var

  val some : Field.Var.t -> var

  val none : var
end

let var_to_state_generic_fine :
    type var.
    (module Maybe_var_type with type var = var) -> Fine.t -> var Zkapp_state.V.t
    =
 fun (module Maybe_var : Maybe_var_type with type var = var) ->
  let rec go : Fine.t -> var list = function
    | [] ->
        []
    | Whole (typ, Some var) :: rest ->
        List.append
          (var_to_fields typ var |> Array.to_list |> List.map ~f:Maybe_var.some)
          (go rest)
    | Whole (typ, None) :: rest ->
        List.append
          (let (Typ typ) = typ in
           List.init typ.size_in_field_elements ~f:(fun _ -> Maybe_var.none) )
          (go rest)
    | Recursive maybe_fields :: rest ->
        List.append (go maybe_fields) (go rest)
  in
  fun x ->
    let r = go x in
    if List.length r > 8 then
      failwith
        "var_to_state_generic_fine used with more than 8 fields, too big for \
         zkapp state!"
    else
      let r' =
        List.(append r (init ~f:(fun _ -> Maybe_var.none) (length r - 8)))
      in
      Zkapp_state.V.of_list_exn r'

let var_to_precondition_fine =
  var_to_state_generic_fine
    ( module struct
      type var = Field.Var.t Or_ignore.Checked.t

      let some = Or_ignore.Checked.make_unsafe Boolean.true_

      let none = Or_ignore.Checked.make_unsafe Boolean.false_ Run.Field.zero
    end )

let var_to_app_state_fine =
  var_to_state_generic_fine
    ( module struct
      type var = Field.Var.t Set_or_keep.Checked.t

      let some = Set_or_keep.Checked.make_unsafe Boolean.true_

      let none = Set_or_keep.Checked.make_unsafe Boolean.false_ Run.Field.zero
    end )

(** Same as var_to_state_generic but for values *)
let value_to_state_generic (some : field -> 'option) (none : 'option)
    (typ : ('var, 'value) Typ.t) (x : 'value) : 'option Zkapp_state.V.t =
  let (Typ typ) = typ in
  let fields, _aux = typ.value_to_fields x in
  assert (Array.length fields <= 8) ;
  let missing = 8 - Array.length fields in
  Zkapp_state.V.of_list_exn
  @@ List.append
       (List.map ~f:(fun f -> some f) @@ Array.to_list fields)
       (List.init missing ~f:(fun _ -> none))

(** "id" transformation, but if less than 8 fields, rest are turned into zero *)
let value_to_init_state typ x =
  value_to_state_generic (fun f -> f) Field.zero typ x

let value_to_app_state typ x =
  value_to_state_generic (fun f -> Set_or_keep.Set f) Set_or_keep.Keep typ x

(** To be used with deriving snarky, a simple field *)
module F = struct
  type t = field

  type var = Field.Var.t

  let typ : (var, t) Typ.t = Field.typ

  let pp : Format.formatter -> field -> unit =
   fun fmt f -> Format.pp_print_string fmt @@ Field.to_string f
end

module V = struct
  type 'a t = Circuit_mode | Proving_mode of 'a

  let typ : ('a t, 'a) Typ.t =
    Typ
      { size_in_field_elements = 0
      ; constraint_system_auxiliary = (fun () -> Circuit_mode)
      ; check = (fun _ -> Checked.return ())
      ; var_to_fields = (fun var -> ([||], var))
      ; var_of_fields = (fun (_, var) -> var)
      ; value_to_fields = (fun value -> ([||], Proving_mode value))
      ; value_of_fields =
          (fun (_, aux) ->
            match aux with
            | Circuit_mode ->
                failwith "constraint_system_auxiliary passed to value_of_fields"
            | Proving_mode t ->
                t )
      }

  let get var _ =
    match var with
    | Circuit_mode ->
        failwith "Shouldn't be possible! MkRef.get run with Circuit_mode."
    | Proving_mode t ->
        t

  let create (x : 'a As_prover.t) : 'a t Checked.t =
    let r = ref None in
    let*| () =
      as_prover
        (let+| x in
         r := Some x )
    in
    match !r with None -> Circuit_mode | Some x -> Proving_mode x
end

module type V_S = sig
  type t

  type var = t V.t

  val typ : (var, t) Typ.t
end

(** To be used with deriving snarky, a reference to T with no in-circuit representation *)
module MkV (T : sig
  type t
end) : V_S with type t = T.t = struct
  type t = T.t

  type var = t V.t

  let typ = V.typ
end

module type SnarkType = sig
  type t

  type var

  val typ : (var, t) Typ.t
end

(** A list of `length` `t`s *)
module SnarkList
    (T : SnarkType) (Len : sig
      val length : int
    end) =
struct
  type t = T.t list

  type var = T.var list

  let typ : (var, t) Typ.t = Typ.list ~length:Len.length T.typ
end

module ProofV = MkV (Proof)

module ProofOption = struct
  type t = Proof.t option
end

(** Reference to Proof  *)
module ProofOptionV = MkV (ProofOption)

(** Boolean but monkey-patched to have `t`*)
module Boolean = struct
  include Boolean

  type t = bool
end

let public_key_to_token_id_var :
    Signature_lib.Public_key.Compressed.var -> Token_id.Checked.t =
 fun public_key ->
  Account_id.Checked.derive_token_id
    ~owner:
      (Account_id.Checked.create public_key Token_id.(constant typ default))

let authorization_vk_hash : F.var -> Account_update.Authorization_kind.Checked.t
    =
 fun verification_key_hash ->
  { is_signed = Boolean.false_
  ; is_proved = Boolean.true_
  ; verification_key_hash
  }

let authorization_signed () : Account_update.Authorization_kind.Checked.t =
  { is_signed = Boolean.true_
  ; is_proved = Boolean.false_
  ; verification_key_hash =
      Run.Field.constant (Verification_key_wire.dummy_vk_hash ())
  }

let time : string -> (unit -> 'a) -> 'a =
 fun label f ->
  let start = Time.now () in
  let x = f () in
  let stop = Time.now () in
  printf "(time) %s: %s\n%!" label
    (Time.Span.to_string_hum (Time.diff stop start)) ;
  x

include struct
  open Async_kernel

  let time_async : string -> (unit -> 'a Deferred.t) -> 'a Deferred.t =
   fun label f ->
    let start = Time.now () in
    let%map x = f () in
    let stop = Time.now () in
    printf "(time_async) %s: %s\n%!" label
      (Time.Span.to_string_hum (Time.diff stop start)) ;
    x
end

include struct
  open Promise.Let_syntax

  let time_promise : string -> (unit -> 'a Promise.t) -> 'a Promise.t =
   fun label f ->
    let start = Time.now () in
    let%map x = f () in
    let stop = Time.now () in
    printf "(time_async) %s: %s\n%!" label
      (Time.Span.to_string_hum (Time.diff stop start)) ;
    x
end

let assert_var label expr =
  with_label label Checked.(fun () -> expr () >>= Boolean.Assert.is_true)

let default_account_update =
  let dummy' = { Account_update.Body.dummy with use_full_commitment = true } in
  constant (Account_update.Body.typ ()) dummy'

module Slot = struct
  include Mina_numbers.Global_slot_since_genesis

  type var = Checked.t
end

module Slot_range = struct
  type t = { lower : Slot.t; upper : Slot.t } [@@deriving snarky]

  module Checked = struct
    let to_valid_while (t : var) : Zkapp_precondition.Valid_while.Checked.t =
      Or_ignore.Checked.make_unsafe Boolean.true_
        { Zkapp_precondition.Closed_interval.lower = t.lower; upper = t.upper }
  end
end

let var_to_actions (typ : ('var, 'value) Typ.t) (x : 'var) :
    Zkapp_account.Actions.var Checked.t =
  let@ () = make_checked in
  let empty_actions = Zkapp_account.Actions.(constant typ []) in
  let actions =
    Zkapp_account.Actions.push_to_data_as_hash empty_actions
      (var_to_fields typ x)
  in
  actions

let var_to_hash ~(init : F.t Random_oracle.State.t) (typ : ('var, 'value) Typ.t)
    (x : 'var) : F.var Checked.t =
  let@ () = make_checked in
  let (Typ typ) = typ in
  let fields, _aux = typ.var_to_fields x in
  Random_oracle.Checked.hash ~init fields

let value_to_actions (typ : ('var, 'value) Typ.t) (x : 'value) :
    Zkapp_account.Actions.t =
  let (Typ typ) = typ in
  let fields, _ = typ.value_to_fields x in
  [ fields ]

module Calls = struct
  type t =
    | []
    | ( :: ) of (Account_update.Checked.t * t) * t
    | Raw of Zkapp_call_forest.Checked.t

  let rec hash : t -> Zkapp_call_forest.Checked.t Checked.t = function
    | [] ->
        Checked.return (Zkapp_call_forest.Checked.empty ())
    | (account_update, nested_calls) :: tail ->
        let* calls = hash nested_calls in
        let* tail = hash tail in
        Checked.return
          (Zkapp_call_forest.Checked.push
             ~account_update:(attach_control_var account_update)
             ~calls tail )
    | Raw calls ->
        Checked.return calls
end

let create_prover_value : 'a As_prover.t -> 'a Prover_value.t Checked.t =
 fun x ->
  Checked.return (Prover_value.create (fun () -> x Run.As_prover.read_var))

(** Given calls the zkapp wishes to make, constructs output that can be used to construct a full account update *)
let make_outputs :
       Account_update.Checked.t
    -> Calls.t
    -> ( Zkapp_statement.Checked.t
       * ( Account_update.Body.t
         * Zkapp_command.Digest.Account_update.t
         * call_forest )
         V.t )
       Checked.t =
 fun account_update calls ->
  let* calls = Calls.hash calls in
  let account_update_digest =
    Zkapp_command.Call_forest.Digest.Account_update.Checked.create
      account_update
  in
  let public_output : Zkapp_statement.Checked.t =
    { account_update = (account_update_digest :> Field.Var.t)
    ; calls = (Zkapp_call_forest.Checked.hash calls :> Field.Var.t)
    }
  in
  let auxiliary_output =
    let+ account_update =
      As_prover.read (Account_update.Body.typ ()) account_update
    in
    let+| account_update_digest =
      As_prover.read Zkapp_command.Call_forest.Digest.Account_update.typ
        account_update_digest
    in
    let calls = Prover_value.get calls.data in
    (account_update, account_update_digest, calls)
  in
  let*| auxiliary_output = V.create auxiliary_output in
  (public_output, auxiliary_output)

(** Takes output from make_outputs and makes it usable *)
let mktree (account_update, account_update_digest, calls) proof =
  let account_update : Account_update.t =
    { body = account_update
    ; authorization = Proof (Pickles.Side_loaded.Proof.of_proof proof)
    }
  in
  Zkapp_command.Call_forest.Tree.
    { account_update; account_update_digest; calls }

let assert_equal :
    ?label:string -> ('var, 't) Typ.t -> 'var -> 'var -> unit Checked.t =
 fun ?label (Typ typ) x y ->
  let x, _ = typ.var_to_fields x in
  let y, _ = typ.var_to_fields y in
  let f (x, y) = Constraint.equal ?label x y in
  let constraints =
    List.map ~f (List.zip_exn (Array.to_list x) (Array.to_list y))
  in
  assert_all ?label constraints

module Checked32 = struct
  include Mina_numbers.Nat.Make32 ()

  type var = Checked.t
end

module Compile_simple = struct
  include Compile_simple_intf.Compile_simple (struct
    type 'a v_t = 'a V.t
  end)
end

type 'branches count_branches_result =
  | Count_branches_result :
      ('branches, 'n_branches) Compile_simple.branches_length
      -> 'branches count_branches_result

let rec count_branches :
    type out_var branches n_available_branches.
       (out_var, branches, n_available_branches) Compile_simple.Branches.t
    -> branches count_branches_result = function
  | [] ->
      Count_branches_result Z
  | _ :: xs ->
      let (Count_branches_result n) = count_branches xs in
      Count_branches_result (S n)

let rec branches_length_to_module :
    type branches n_branches.
       (branches, n_branches) Compile_simple.branches_length
    -> (module Pickles_types.Nat.Intf with type n = n_branches) = function
  | Z ->
      (module Pickles_types.Nat.N0)
  | S n ->
      let (module N) = branches_length_to_module n in
      ( module struct
        type n = N.n Pickles_types.Nat.s

        let n : n Pickles_types.Nat.t = S N.n
      end )

let proof_as_ref :
       ('width, 'height) Pickles.Proof.t V.t
    -> ('width, 'height) Pickles.Proof.t As_prover.Ref.t = function
  | Proving_mode proof ->
      ref (Some proof)
  | Circuit_mode ->
      ref None

let input_for_main (type input)
    (_main :
      input V.t -> ('out_var, 'prevs) Compile_simple.main_return Checked.t ) :
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

let transform_main_one (type out_var prev_var prev_width)
    (main :
      ( out_var
      , (prev_var, prev_width) Compile_simple.one_prev )
      Compile_simple.main_return
      Checked.t ) :
    ( prev_var * unit
    , prev_width * unit
    , out_var
    , unit )
    Pickles.Inductive_rule.main_return
    Checked.t =
  let*| ({ out; prevs = One_prev { public_input; proof; proof_must_verify } } :
          _ Compile_simple.main_return ) =
    main
  in
  Pickles.Inductive_rule.
    { previous_proof_statements =
        [ { public_input; proof = proof_as_ref proof; proof_must_verify } ]
    ; public_output = out
    ; auxiliary_output = ()
    }

let transform_main_two (type out_var left_var left_width right_var right_width)
    (main :
      ( out_var
      , (left_var, left_width, right_var, right_width) Compile_simple.two_prevs
      )
      Compile_simple.main_return
      Checked.t ) :
    ( left_var * (right_var * unit)
    , left_width * (right_width * unit)
    , out_var
    , unit )
    Pickles.Inductive_rule.main_return
    Checked.t =
  let*| ({ out
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
          _ Compile_simple.main_return ) =
    main
  in
  Pickles.Inductive_rule.
    { previous_proof_statements =
        [ { public_input = left_public_input
          ; proof = proof_as_ref left_proof
          ; proof_must_verify = left_proof_must_verify
          }
        ; { public_input = right_public_input
          ; proof = proof_as_ref right_proof
          ; proof_must_verify = right_proof_must_verify
          }
        ]
    ; public_output = out
    ; auxiliary_output = ()
    }

type ('out_var, 'out_t, 'tag_branches, 'branches) branches_to_choices_return =
  | Choices :
      { rules :
             self:
               ( 'out_var
               , 'out_t
               , Compile_simple.self_width
               , 'tag_branches )
               Pickles.Tag.t
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
             , ( 'out_t
               * unit
               * ( Compile_simple.self_width
                 , Compile_simple.self_width )
                 Pickles.Proof.t )
               Promise.t )
             Pickles.Provers.t
          -> ('out_t, 'branches) Compile_simple.provers
      }
      -> ('out_var, 'out_t, 'tag_branches, 'branches) branches_to_choices_return

let transform_prover :
       branch_name:string
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
 fun ~branch_name ~name prover handler input ->
  let@ () =
    time_promise @@ "(compile_simple) proving " ^ name ^ "." ^ branch_name
  in
  let@ stmt, (), proof =
    prover ~handler:(handler input) () |> Promise.( >>| )
  in
  (stmt, Pickles.Side_loaded.Proof.of_proof proof)

(* TODO: collapse branches *)
let rec branches_to_choices :
    type out_var out_t branches available_branches tag_branches.
       name:string
    -> (out_var, branches, available_branches) Compile_simple.Branches.t
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
              _ Compile_simple.provers =
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
                                         _ Compile_simple.main_return ) =
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
                      let main =
                        Checked.(input >>= main) |> transform_main_one
                      in
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
          | One_tag Own_tag ->
              Choices
                { transform_provers
                ; rules =
                    (fun ~self ->
                      let main =
                        Checked.(input >>= main) |> transform_main_one
                      in
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
                      let main =
                        Checked.(input >>= main) |> transform_main_two
                      in
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
          | Two_tags (Own_tag, Tag right_tag) ->
              Choices
                { transform_provers
                ; rules =
                    (fun ~self ->
                      let main =
                        Checked.(input >>= main) |> transform_main_two
                      in
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
          | Two_tags (Tag left_tag, Own_tag) ->
              Choices
                { transform_provers
                ; rules =
                    (fun ~self ->
                      let main =
                        Checked.(input >>= main) |> transform_main_two
                      in
                      let rule : _ Pickles.Inductive_rule.Promise.t =
                        { identifier = branch_name
                        ; main =
                            (fun _ -> Run.run_checked main |> Promise.return)
                        ; prevs = [ left_tag; self ]
                        ; feature_flags
                        }
                      in
                      rule :: f ~self )
                }
          | Two_tags (Own_tag, Own_tag) ->
              Choices
                { transform_provers
                ; rules =
                    (fun ~self ->
                      let main =
                        Checked.(input >>= main) |> transform_main_two
                      in
                      let rule : _ Pickles.Inductive_rule.Promise.t =
                        { identifier = branch_name
                        ; main =
                            (fun _ -> Run.run_checked main |> Promise.return)
                        ; prevs = [ self; self ]
                        ; feature_flags
                        }
                      in
                      rule :: f ~self )
                } ) )

let compile_simple
    ?(override_wrap_domain : Pickles_base.Proofs_verified.t option)
    ~(name : string)
    ~(branches :
       ( 'out_var
       , ('first_input, 'branches) Compile_simple.cons_branch
       , 'n_available_branches )
       Compile_simple.Branches.t ) ~(out_typ : ('out_var, 'out_t) Typ.t) () :
    ( 'out_var
    , 'out_t
    , ('first_input, 'branches) Compile_simple.cons_branch )
    Compile_simple.result
    Promise.t =
  printf "(compile_simple) called for circuit %s\n" name ;
  let@ () = time_promise ("(compile_simple) compiling circuit " ^ name) in
  let (Count_branches_result tag_length) = count_branches branches in
  let (module N_branches) = branches_length_to_module tag_length in
  match branches_to_choices ~name branches with
  | Choices { rules; transform_provers } ->
      let tag, _cache, _proof_module, provers =
        Pickles.compile_promise () ?override_wrap_domain ~cache:Cache_dir.cache
          ~public_input:(Output out_typ) ~auxiliary_typ:Typ.unit
          ~branches:(module N_branches)
          ~choices:rules
          ~max_proofs_verified:(module Pickles_types.Nat.N2)
          ~name:("compile_simple of " ^ name)
          ~constraint_constants:
            (Genesis_constants.Constraint_constants.to_snark_keys_header
               constraint_constants )
      in
      let@ (_ : Pickles.Side_loaded.Verification_key.t) =
        Pickles.Side_loaded.Verification_key.of_compiled_promise tag
        |> Promise.( >>| )
      in
      let provers = transform_provers provers in
      Compile_simple.Result { tag; provers; tag_length }
