module P = Printexc
open Core_kernel
open Snark_params.Tick

module Proof = struct
  type t = Proof of Field.t (* hash of vk and public input *)
  [@@deriving yojson]
end

(* TODO: this should be hash of circuit *)
type vk = Vk of field (* random unique number *)

type 'var tag = Tag : vk -> 'var tag

module Verification_key = struct
  type t = vk (* random number *)

  type var = Var of Field.Var.t

  let typ : (var, t) Typ.t =
    Typ.field
    |> Typ.transport ~there:(fun (Vk x) -> x) ~back:(fun x -> Vk x)
    |> Typ.transport_var ~there:(fun (Var x) -> x) ~back:(fun x -> Var x)

  let of_tag (Tag vk) = Promise.return vk

  let hash (Vk x) = x

  let hash_var (Var x) = x
end

let force_tag _ = Promise.return ()

include Compile_simple_intf.Make (struct
  type nonrec proof = Proof.t

  type nonrec 'var tag = 'var tag

  type vk_t = Verification_key.t

  type vk_var = Verification_key.var
end)

let get_first_backtrace_entry b =
  let open P in
  match backtrace_slots b with
  | None ->
      "<invalid>"
  | Some slots -> (
      match Slot.location slots.(1) with
      | None ->
          "<invalid>"
      | Some { filename; line_number; _ } ->
          filename ^ ":" ^ Int.to_string line_number )

let hash_proof (Tag (Vk vk) : 'out_var tag) (Typ typ : ('out_var, 'out) Typ.t)
    (out : 'out) =
  let fields, _aux = typ.value_to_fields out in
  Random_oracle.hash
    ~init:(Hash_prefix_create.salt "compile_simple_fake proof hash")
    (Array.append [| vk |] fields)

let branches_to_provers name tag out_typ =
  let rec go :
      type branches available_branches.
      (_, branches, available_branches) Branches.t -> (_, branches) provers =
    function
    (* FIXME: verify recursive proofs *)
    | Branches.({ branch_name; tags = _; main } :: rest) ->
        printf "Fake proving %s.%s\n" name branch_name ;
        let prover input =
          let out =
            Snark_params.Tick.run_and_check_exn
            @@
            let open Checked in
            main (V.return input)
            >>| fun { out; prevs = _ } -> As_prover.read out_typ out
          in
          let hash = hash_proof tag out_typ out in
          Promise.return (out, Proof.Proof hash)
        in
        prover :: go rest
    | [] ->
        []
  in
  go

let compile (type out_t out_var first_input branches n_available_branches)
    ?(wrap_domain : [ `N13 | `N14 | `N15 ] option) ~(name : string)
    ~(branches :
       ( out_var
       , (first_input, branches) cons_branch
       , n_available_branches )
       Branches.t ) ~(out_typ : (out_var, out_t) Typ.t) () :
    (module Result
       with type out_t = out_t
        and type out_var = out_var
        and type branches = (first_input, branches) cons_branch ) =
  ignore wrap_domain ;
  printf "(compile_simple [fake]) called for circuit %s from %s\n%!" name
    (P.get_callstack 9999 |> get_first_backtrace_entry) ;
  assert (Run.in_checked_computation () |> not) ;
  assert (Run.in_prover () |> not) ;
  let vk = Vk (Field.gen |> Quickcheck.random_value) in
  let r :
      (module Result
         with type out_t = out_t
          and type out_var = out_var
          and type branches = (first_input, branches) cons_branch ) =
    ( module struct
      type nonrec out_t = out_t

      type nonrec out_var = out_var

      type nonrec branches = (first_input, branches) cons_branch

      type tag_var = out_var

      type tag_t = out_t

      let tag = Tag vk

      let provers = branches_to_provers name (Tag vk) out_typ branches

      type t = out_t

      type var = out_var

      let typ = out_typ

      let get ?check out =
        let open Checked in
        V.create
          (let open As_prover in
          read out_typ out
          >>| fun out -> Proof.Proof (hash_proof (Tag vk) out_typ out))
        >>= fun proof ->
        let prev : _ prev =
          { public_input = out
          ; proof
          ; proof_must_verify =
              (match check with Some b -> b | None -> Boolean.true_)
          }
        in
        Checked.return (out, prev)

      let make_unchecked ?proof out : t = ignore proof ; out
    end )
  in
  r
