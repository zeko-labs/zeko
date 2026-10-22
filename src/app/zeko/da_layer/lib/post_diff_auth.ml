open Core_kernel
open Async_kernel
open Mina_base
open Signature_lib
module Field = Snark_params.Tick.Field

(** Authorization attached to [Post_diff].

    A DA node only stores and signs diffs that an allowed poster (the
    sequencer) signed. The message binds the rollup, the source and target DA
    states, and the exact pending diff bytes, so a third party cannot store a
    different variant of the same transition before the sequencer does. *)

[%%versioned
module Stable = struct
  [@@@no_toplevel_latest_type]

  module V1 = struct
    type t =
      { poster : Public_key.Compressed.Stable.V1.t
      ; signature : Signature.Stable.V1.t
      }
    [@@deriving sexp]

    let to_latest = Fn.id
  end
end]

type t = Stable.V1.t =
  { poster : Public_key.Compressed.t; signature : Signature.t }
[@@deriving sexp]

(** The identity of the rollup and the poster that signs its diffs. *)
module Poster = struct
  type t =
    { rollup : Public_key.Compressed.t; signer : Signer_service.Signer.t }
end

let bits_of_bytes s =
  String.to_list s
  |> List.concat_map ~f:(fun c ->
         List.init 8 ~f:(fun i -> (Char.to_int c lsr i) land 1 = 1) )

(** Blake2 of the pending diff serialization, split into two 128-bit halves so
    that each half fits in one field element. *)
let diff_digest (diff : Diff.Pending.t) =
  let digest =
    Blake2.to_raw_string
      (Blake2.digest_string (Diff.Pending.to_binable_string diff))
  in
  let half pos =
    Field.project (bits_of_bytes (String.sub digest ~pos ~len:16))
  in
  [| half 0; half 16 |]

let signing_message ~(rollup : Public_key.Compressed.t)
    ~(source_state : Da_state.t) ~(target_state : Da_state.t) ~diff =
  Random_oracle.hash
    ~init:(Hash_prefix_create.salt Zeko_constants.da_post_diff_salt)
    (Array.append
       [| rollup.x
        ; (if rollup.is_odd then Field.one else Field.zero)
        ; source_state.ledger_hash
        ; source_state.acc_set
        ; target_state.ledger_hash
        ; target_state.acc_set
       |]
       (diff_digest diff) )

let sign ~signature_kind ~(poster : Poster.t) ~source_state ~target_state ~diff
    =
  let message =
    signing_message ~rollup:poster.rollup ~source_state ~target_state ~diff
  in
  Signer_service.Signer.sign_field ~signature_kind poster.signer message
  >>| Result.map ~f:(fun signature ->
          { poster = Signer_service.Signer.public_key poster.signer; signature } )

let verify ~signature_kind ~rollup ~allowed_posters ~source_state ~target_state
    ~diff ({ poster; signature } : t) =
  if not (List.mem allowed_posters poster ~equal:Public_key.Compressed.equal)
  then
    Or_error.error_s
      [%message
        "Post_diff poster is not allowed" (poster : Public_key.Compressed.t)]
  else
    match Public_key.decompress poster with
    | None ->
        Or_error.error_string "Post_diff poster public key is invalid"
    | Some public_key ->
        let message =
          signing_message ~rollup ~source_state ~target_state ~diff
        in
        if
          Schnorr.Chunked.verify ~signature_kind signature
            (Snark_params.Tick.Inner_curve.of_affine public_key)
            (Random_oracle.Input.Chunked.field message)
        then Ok ()
        else Or_error.error_string "Post_diff authorization does not verify"

let%test_module "post-diff authorization" =
  ( module struct
    let signature_kind = Mina_signature_kind.Other_network "da-post-auth-test"

    let rollup = Public_key.compress (Keypair.create ()).public_key

    let keypair = Keypair.create ()

    let poster_pk = Public_key.compress keypair.public_key

    let source_state =
      Da_state.create ~ledger_hash:Ledger_hash.empty_hash ~acc_set:Field.zero

    let target_state = { source_state with acc_set = Field.one }

    let diff =
      Diff.create_pending ~source_ledger_hash:Ledger_hash.empty_hash
        ~changed_accounts:[] ~actions:(`Actions [])

    let auth =
      Async.Thread_safe.block_on_async_exn (fun () ->
          sign ~signature_kind
            ~poster:
              { rollup; signer = Signer_service.Signer.of_keypair keypair }
            ~source_state ~target_state ~diff )
      |> Or_error.ok_exn

    let check ?(signature_kind = signature_kind) ?(rollup = rollup)
        ?(allowed_posters = [ poster_pk ]) ?(source_state = source_state)
        ?(target_state = target_state) ?(diff = diff) auth =
      Result.is_ok
        (verify ~signature_kind ~rollup ~allowed_posters ~source_state
           ~target_state ~diff auth )

    let%test "valid authorization verifies" = check auth

    let%test "poster must be allowed" = not (check ~allowed_posters:[] auth)

    let%test "signature must come from the poster" =
      let other = Keypair.create () in
      not
        (check
           ~allowed_posters:[ Public_key.compress other.public_key ]
           { auth with poster = Public_key.compress other.public_key } )

    let%test "authorization is bound to the rollup" =
      not
        (check
           ~rollup:(Public_key.compress (Keypair.create ()).public_key)
           auth )

    let%test "authorization is bound to the network" =
      not (check ~signature_kind:(Other_network "other") auth)

    let%test "authorization is bound to the source state" =
      not (check ~source_state:target_state auth)

    let%test "authorization is bound to the target state" =
      not (check ~target_state:source_state auth)

    let%test "authorization is bound to the exact diff bytes" =
      let account = Account.empty in
      not (check ~diff:{ diff with changed_accounts = [ (0, account) ] } auth)

    let%test "authorization is bound to account order" =
      let a = { Account.empty with nonce = Account.Nonce.one } in
      let b = Account.empty in
      let ab = { diff with changed_accounts = [ (0, a); (1, b) ] } in
      let ba = { diff with changed_accounts = [ (1, b); (0, a) ] } in
      let auth_ab =
        Async.Thread_safe.block_on_async_exn (fun () ->
            sign ~signature_kind
              ~poster:
                { rollup; signer = Signer_service.Signer.of_keypair keypair }
              ~source_state ~target_state ~diff:ab )
        |> Or_error.ok_exn
      in
      check ~diff:ab auth_ab && not (check ~diff:ba auth_ab)
  end )
