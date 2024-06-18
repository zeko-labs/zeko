open Core_kernel
open Mina_base
open Snark_params.Tick.Run
(* Impure interface to snarky, FIXME: replace with pure one *)

open Zkapp_basic
open Account_update
module V = Prover_value

let attach_control_var :
    Account_update.Body.Checked.t -> Zkapp_call_forest.Checked.account_update =
 fun account_update ->
  { account_update =
      { data = account_update
      ; hash =
          Zkapp_command.Call_forest.Digest.Account_update.Checked.create
            account_update
      }
  ; control = V.create (fun () -> Control.None_given)
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

(** Converts a variable to its constituent fields *)
let var_to_fields (type var value) (typ : (var, value) Typ.t) (x : var) :
    Field.t array =
  let (Typ typ) = typ in
  let fields, _aux = typ.var_to_fields x in
  fields

(** Default permissions for deployments *)
let proof_permissions : Permissions.t =
  { edit_state = Proof
  ; send = Proof
  ; receive = None
  ; set_delegate = Proof
  ; set_permissions = Proof
  ; set_verification_key = (Either, Mina_numbers.Txn_version.current)
  ; set_zkapp_uri = Proof
  ; edit_action_state = Proof
  ; set_token_symbol = Proof
  ; increment_nonce = None
  ; set_voting_for = Proof
  ; set_timing = Proof
  ; access = Proof
  }

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

(** Given calls the zkapp wishes to make, constructs output that can be used to construct a full account update *)
let make_outputs account_update calls =
  let account_update_digest =
    Zkapp_command.Call_forest.Digest.Account_update.Checked.create
      account_update
  in
  let public_output : Zkapp_statement.Checked.t =
    { account_update = (account_update_digest :> Field.t)
    ; calls = (Zkapp_call_forest.Checked.hash calls :> Field.t)
    }
  in
  let auxiliary_output =
    Prover_value.create (fun () ->
        let account_update = As_prover.read (Body.typ ()) account_update in
        let account_update_digest =
          As_prover.read Zkapp_command.Call_forest.Digest.Account_update.typ
            account_update_digest
        in
        let calls = Prover_value.get calls.data in
        (account_update, account_update_digest, calls) )
  in
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

(** A shorthand function to keep a field in the update for the app state *)
let keep = Set_or_keep.Checked.keep ~dummy:Field.zero

(** A shorthand function to ignore a field in the precondition for the app state *)
let ignore = Or_ignore.Checked.make_unsafe Boolean.false_ Field.zero

(** Feature flags used when compiling circuits *)
let feature_flags = Pickles_types.Plonk_types.Features.none_bool

(** Hash a constant string to a field for use as tags. *)
let naive_hash_string_to_field (s : string) =
  Random_oracle.hash
    ( Array.map ~f:(fun c -> Field.Constant.of_int @@ Char.to_int c)
    @@ String.to_array s )

(** Generic function for turning variables into 8 of something  *)
let var_to_state (some : Field.t -> 'option) (none : 'option)
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
  var_to_state Set_or_keep.Checked.set
    (Set_or_keep.Checked.keep ~dummy:Field.zero)
    typ x

(** Used for turning variables into preconditions *)
let var_to_precondition_state typ x =
  var_to_state
    (Or_ignore.Checked.make_unsafe Boolean.true_)
    (Or_ignore.Checked.make_unsafe Boolean.false_ Field.zero)
    typ x

(** Same as var_to_state but for values *)
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

(** "id" transformation, but if less than 8 fields, rest are turned into zero *)
let value_to_init_state typ x =
  value_to_state (fun f -> f) Field.Constant.zero typ x

let value_to_app_state typ x =
  value_to_state (fun f -> Set_or_keep.Set f) Set_or_keep.Keep typ x

let var_to_actions (typ : ('var, 'value) Typ.t) (x : 'var) : Actions.var =
  let empty_actions = Zkapp_account.Actions.(constant typ []) in
  let actions =
    Actions.push_to_data_as_hash empty_actions (var_to_fields typ x)
  in
  actions

let value_to_actions (typ : ('var, 'value) Typ.t) (x : 'value) : Actions.t =
  let (Typ typ) = typ in
  let fields, _ = typ.value_to_fields x in
  [ fields ]

(** FIXME: Very commonly used, but should be replaced by monadic style *)
let run = run_checked

(** To be used with deriving snarky, a simple field *)
module F = struct
  type t = field

  type var = Field.t

  let typ : (var, t) Typ.t = Field.typ

  let pp : Format.formatter -> field -> unit =
   fun fmt f -> Format.pp_print_string fmt @@ Field.Constant.to_string f
end

(** To be used with deriving snarky, a reference to T with no in-circuit representation *)
module MkRef (T : sig
  type t [@@deriving yojson]
end) =
struct
  type t = T.t [@@deriving yojson]

  type var = T.t V.t

  let typ : (var, t) Typ.t = V.typ ()
end

(** A list of `length` `t`s *)
module SnarkList (T : sig
  module T : sig
    type t

    type var

    val typ : (var, t) Typ.t
  end

  val length : int
end) =
struct
  type t = T.T.t list

  type var = T.T.var list

  let typ : (var, t) Typ.t = Typ.list ~length:T.length T.T.typ
end

(** Reference to Proof  *)
module RefProof = MkRef (Proof)

(** Boolean but monkey-patched to have `t`*)
module Boolean = struct
  include Boolean

  type t = bool
end

(** Helper to construct snarky handler from type *)
module MkHandler (Witness : sig
  type t

  type var

  val typ : (var, t) Typ.t
end) =
struct
  open Snarky_backendless.Request

  type _ t += Witness : Witness.t t

  let handler (w : Witness.t) (With { request; respond }) =
    match request with Witness -> respond (Provide w) | _ -> respond Unhandled

  let exists_witness () : Witness.var =
    exists Witness.typ ~request:(fun () -> Witness)
end

let public_key_to_token_id_var :
    Signature_lib.Public_key.Compressed.var -> Token_id.Checked.t =
 fun public_key ->
  Account_id.Checked.derive_token_id
    ~owner:
      (Account_id.Checked.create public_key Token_id.(constant typ default))

let authorization_vk_hash : F.var -> Authorization_kind.Checked.t =
 fun verification_key_hash ->
  { is_signed = Boolean.false_
  ; is_proved = Boolean.true_
  ; verification_key_hash
  }

let authorization_signed () : Authorization_kind.Checked.t =
  { is_signed = Boolean.true_
  ; is_proved = Boolean.false_
  ; verification_key_hash =
      Field.constant (Verification_key_wire.dummy_vk_hash ())
  }

let time : string -> (unit -> 'a) -> 'a =
 fun label f ->
  let start = Time.now () in
  let x = f () in
  let stop = Time.now () in
  printf "%s: %s\n%!" label (Time.Span.to_string_hum (Time.diff stop start)) ;
  x

include struct
  open Async_kernel

  (* Maybe there's a better way of doing this function, or perhaps a built-in way *)
  let time_async : string -> (unit -> 'a Deferred.t) -> 'a Deferred.t =
   fun label f ->
    let start = Time.now () in
    let%map x = f () in
    let stop = Time.now () in
    printf "%s: %s\n%!" label (Time.Span.to_string_hum (Time.diff stop start)) ;
    x
end

let assert_var label expr =
  with_label label (fun () -> expr () |> Boolean.Assert.is_true)

let assert_var_checked label expr =
  with_label label (fun () -> expr () |> run |> Boolean.Assert.is_true)

let ref_of_v (x : 'a V.t) : 'a As_prover.Ref.t =
  As_prover.Ref.create (fun () -> V.get x)
