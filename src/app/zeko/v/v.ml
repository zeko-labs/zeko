open Snark_params.Tick

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
  let c = As_prover.map x ~f:(fun x -> r := Some x) |> as_prover in
  Checked.map c ~f:(fun () ->
      match !r with None -> Circuit_mode | Some x -> Proving_mode x )

let unsafe_unwrap : 'a t -> 'a option = function
  | Proving_mode proof ->
      Some proof
  | Circuit_mode ->
      None

let as_ref x = ref (unsafe_unwrap x)

let map ~f = function
  | Circuit_mode ->
      Circuit_mode
  | Proving_mode x ->
      Proving_mode (f x)

let return x = Proving_mode x
