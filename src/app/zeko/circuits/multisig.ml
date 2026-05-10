open Core_kernel
open Snark_params.Tick
open Zeko_util
open Mina_base
open Signature_lib
module PC = Public_key.Compressed

module Maybe_signature = struct
  type t =
    { public_key : PC.t
    ; signature : Schnorr.Chunked.Signature.t
    ; is_some : Boolean.t
    }
  [@@deriving snarky]

  (* It can't be PC.empty because we need to be able to decompress it *)
  let empty_key =
    Public_key.compress
    @@ Snark_params.Tick.Inner_curve.(
         to_affine_exn @@ point_near_x @@ Field.of_int 123123123)

  let dummy : t =
    { public_key = empty_key; signature = Signature.dummy; is_some = false }
end

module Signatures = SnarkArray (struct
  module T = Maybe_signature

  let max_length = Zeko_constants.da_multisig_max_length

  let dummy_filler = Maybe_signature.dummy
end)

module Witness = struct
  type t = { signatures : Signatures.t; quorum : F.t } [@@deriving snarky]
end

type t = { public_keys : PC.t list; quorum : Field.t } [@@deriving yojson]

(** We save only hash to the outer state *)
module Commitment = struct
  type t = F.t

  type var = F.var

  let typ = F.typ
end

let commit { public_keys; quorum } =
  if List.length public_keys > Zeko_constants.da_multisig_max_length then
    failwith "Public keys length exceeds the maximum length"
  else
    let public_keys = List.sort public_keys ~compare:PC.compare in
    let padded =
      public_keys
      @ List.init
          (Zeko_constants.da_multisig_max_length - List.length public_keys)
          ~f:(fun _ -> Maybe_signature.empty_key)
    in
    List.fold_left padded ~init:quorum ~f:(fun acc pk ->
        let input =
          Random_oracle.Input.Chunked.(append (field acc) (PC.to_input pk))
        in
        Random_oracle.hash
          ~init:(Hash_prefix_create.salt Zeko_constants.multisig_salt)
          (Random_oracle.pack_input input) )

let of_witness_var ({ signatures; quorum; _ } : Witness.var) :
    Commitment.var Checked.t =
  let* () =
    let@ () = make_checked in
    let@ () = Run.as_prover in
    let public_keys =
      Run.As_prover.read Signatures.typ signatures
      |> List.map ~f:(fun { public_key; _ } -> public_key)
    in
    let sorted_public_keys = List.sort public_keys ~compare:PC.compare in
    assert (List.equal PC.equal sorted_public_keys public_keys)
  in
  foldl (Array.to_list signatures.array) ~init:quorum
    ~f:(fun acc ({ public_key; _ } : Maybe_signature.var) ->
      let@ () = make_checked in
      let input =
        Random_oracle.Input.Chunked.(
          append (field acc) (PC.Checked.to_input public_key))
      in
      Random_oracle.Checked.hash
        ~init:(Hash_prefix_create.salt Zeko_constants.multisig_salt)
        (Random_oracle.Checked.pack_input input) )

let check ~signature_kind ({ signatures; quorum = _ } : Witness.var) payload =
  let payload = Random_oracle.Input.Chunked.field payload in
  let@ () = with_label __LOC__ in
  let* _valid_signatures_count =
    foldl
      (Array.to_list signatures.array)
      ~init:Field.(Var.constant zero)
      ~f:(fun acc ({ public_key; signature; is_some } : Maybe_signature.var) ->
        let@ () = with_label __LOC__ in
        let* signature_verifies =
          let* pk_uncompressed = Public_key.decompress_var public_key in
          let@ () = with_label __LOC__ in
          let* (module Shifted) = Inner_curve.Checked.Shifted.create () in
          Schnorr.Chunked.Checked.verifies ~signature_kind
            (module Shifted)
            signature pk_uncompressed payload
        in
        let@ () = with_label __LOC__ in
        let* should_increment = Boolean.(is_some && signature_verifies) in
        let@ () = with_label __LOC__ in
        if_ should_increment ~typ:F.typ
          ~then_:Field.Var.(add acc (constant Field.one))
          ~else_:acc )
  in
  let@ () = with_label __LOC__ in
  Checked.return ()
(* Comparison_gadget.assert_greater_than_full valid_signatures_count
   (* sub 1 to do the >= *)
   Field.Var.(sub quorum (constant Field.one)) *)
