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

module Proof = Compile_simple.Proof

(** Converts a variable to its constituent fields *)
let var_to_fields (type var value) (typ : (var, value) Typ.t) (x : var) :
    Field.Var.t array =
  let (Typ typ) = typ in
  let fields, _aux = typ.var_to_fields x in
  fields

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
        List.(append r (init ~f:(fun _ -> Maybe_var.none) (8 - length r)))
      in
      assert (List.length r' = 8) ;
      Zkapp_state.V.of_list_exn r'

let value_to_state (some : field -> 'option) (none : 'option)
    (typ : ('var, 'value) Typ.t) (x : 'value) : 'option Zkapp_state.V.t =
  let (Typ typ) = typ in
  let fields, _aux = typ.value_to_fields x in
  assert (Array.length fields <= 8) ;
  let missing = 8 - Array.length fields in
  Zkapp_state.V.of_list_exn
  @@ List.append
       (List.map ~f:(fun f -> some f) @@ Array.to_list fields)
       (List.init missing ~f:(fun _ -> none))

let value_to_init_state typ x = value_to_state (fun f -> f) Field.zero typ x

let value_to_app_state typ x =
  value_to_state (fun f -> Set_or_keep.Set f) Set_or_keep.Keep typ x

let value_of_state (typ : ('var, 'value) Typ.t) (x : field Zkapp_state.V.t) :
    'value =
  let (Typ typ) = typ in
  typ.value_of_fields
    ( Zkapp_state.V.to_list x |> Array.of_list
    , typ.constraint_system_auxiliary () )

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

(** To be used with deriving snarky, a simple field *)
module F = struct
  type t = Field.t

  type var = Field.Var.t

  let typ : (var, t) Typ.t = Field.typ

  let pp : Format.formatter -> field -> unit =
   fun fmt f -> Format.pp_print_string fmt @@ Field.to_string f
end

module type V_S = sig
  type t

  type var = t V.t

  val typ : (var, t) Typ.t
end

(** To be used with deriving snarky, a reference to T with no in-circuit representation *)
module Mk_V (T : sig
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

module SnarkArray (Inputs : sig
  module T : SnarkType

  val max_length : int

  val dummy_filler : T.t
end) =
struct
  open Inputs

  type t = T.t list

  type var = { array : T.var array; length : int V.t }

  let typ : (var, t) Typ.t =
    let pad : int -> T.t list -> T.t array * int =
     fun len list ->
      let arr = Array.create ~len dummy_filler in
      let rec go idx = function
        | x :: xs ->
            Array.set arr idx x ;
            go (idx + 1) xs
        | [] ->
            idx
      in
      let real_len = go 0 list in
      (arr, real_len)
    in
    let rec extract : int -> int -> T.t array -> T.t list =
     fun len offset array ->
      match len with
      | 0 ->
          []
      | _ ->
          array.(offset) :: extract (len - 1) (offset + 1) array
    in
    let open Typ in
    array ~length:max_length T.typ * V.typ
    |> transport
         ~there:(fun xs -> pad max_length xs)
         ~back:(fun (xs, len) -> extract len 0 xs)
    |> transport_var
         ~there:(fun { array; length } -> (array, length))
         ~back:(fun (array, length) -> { array; length })
end

module Proof_V = Mk_V (Proof)

module ProofOption = struct
  type t = Proof.t option
end

(** Reference to Proof  *)
module Proof_Option_V = Mk_V (ProofOption)

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

let assert_var label expr =
  with_label label Checked.(fun () -> expr () >>= Boolean.Assert.is_true)

let default_account_update =
  let dummy' = { Account_update.Body.dummy with use_full_commitment = true } in
  constant (Account_update.Body.typ ()) dummy'

module Slot = struct
  include Mina_numbers.Global_slot_since_genesis

  type var = Checked.t
end

module Slot_span = struct
  include Mina_numbers.Global_slot_span

  type var = Checked.t
end

module Slot_range = struct
  type t = { lower : Slot.t; upper : Slot.t } [@@deriving snarky]

  module Checked = struct
    let to_valid_while (t : var) : Zkapp_precondition.Valid_while.Checked.t =
      Or_ignore.Checked.make_unsafe Boolean.true_
        { Zkapp_precondition.Closed_interval.lower = t.lower; upper = t.upper }
  end

  let infinite : t = { lower = Slot.zero; upper = Slot.max_value }
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

let var_to_hash ~(init : string) (typ : ('var, 'value) Typ.t) (x : 'var) :
    F.var Checked.t =
  let@ () = make_checked in
  let (Typ typ) = typ in
  let fields, _aux = typ.var_to_fields x in
  Random_oracle.Checked.hash ~init:(Hash_prefix_create.salt init) fields

module Calls = struct
  type t =
    | []
    | ( :: ) of (Account_update.Checked.t * t) * t
    | Raw of Zkapp_call_forest.Checked.t

  let rec hash : t -> Zkapp_call_forest.Checked.t Checked.t =
    let attach_control_var :
           Account_update.Body.Checked.t
        -> Zkapp_call_forest.Checked.account_update =
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
    in
    function
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

(** Given calls the zkapp wishes to make, constructs output that can be used to construct a full account update *)
let make_outputs :
       Account_update.Checked.t
    -> Calls.t
    -> ( Zkapp_statement.Checked.t
       * (Account_update.Body.t * Zkapp_command.Digest.Account_update.t * _) V.t
       )
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

let assert_equal :
    ?label:string -> ('var, 't) Typ.t -> 'var -> 'var -> unit Checked.t =
 fun ?label (Typ typ) x y ->
  let x_fields, _ = typ.var_to_fields x in
  let y_fields, _ = typ.var_to_fields y in
  let constraints =
    Array.map2_exn ~f:(Constraint.equal ?label) x_fields y_fields
  in
  Array.to_list constraints |> assert_all ?label

let assert_equal_safer ?label typ x y =
  let*| () = assert_equal ?label typ x y in
  x

let var_equal : ('var, 't) Typ.t -> 'var -> 'var -> Boolean.Expr.t Checked.t =
 fun (Typ typ) x y ->
  let x_fields, _ = typ.var_to_fields x in
  let y_fields, _ = typ.var_to_fields y in
  let*| bools =
    Checked.List.map
      ~f:(fun (x_field, y_field) ->
        let*| b = Field.Checked.equal x_field y_field in
        Boolean.Expr.(!b) )
      (List.zip_exn (Array.to_list x_fields) (Array.to_list y_fields))
  in
  Boolean.Expr.all bools

module Checked32 = struct
  include Mina_numbers.Nat.Make32 ()

  type var = Checked.t
end

let push_actions_var ~actions state =
  let@ () = make_checked in
  Random_oracle.Checked.hash ~init:Hash_prefix_states.zkapp_actions
    [| state; actions |]

let token_owner_id : Account_id.t option -> Token_id.t = function
  | None ->
      Token_id.default
  | Some owner ->
      Account_id.derive_token_id ~owner

module Even_PC = struct
  type t = { public_key : F.t } [@@deriving snarky]

  let to_pc_var { public_key } : Signature_lib.Public_key.Compressed.var =
    { x = public_key; is_odd = Boolean.false_ }
end

let slot_range_intersection (x : Slot_range.var) (y : Slot_range.var) :
    Slot_range.var Checked.t =
  let open Checked.Let_syntax in
  let* lower =
    Slot.Checked.(x.lower < y.lower)
    >>= if_ ~typ:Slot.typ ~then_:y.lower ~else_:x.lower
  in
  let*| upper =
    Slot.Checked.(x.upper < y.upper)
    >>= if_ ~typ:Slot.typ ~then_:x.upper ~else_:y.upper
  in
  ({ lower; upper } : Slot_range.var)

let accumulate (f : ('a -> unit) -> 'b Checked.t) : ('b * 'a list) Checked.t =
  let acc = ref [] in
  let running = ref true in
  let*| r =
    f (fun x ->
        (* if this fails it's because you used the generated function after the
           end of its scope, i.e., use-after-free. *)
        assert !running ;
        acc := x :: !acc )
  in
  running := false ;
  (r, !acc)
