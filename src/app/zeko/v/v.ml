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

let get var = As_prover.read typ var

let create (compute : 'a As_prover.t) : 'a t Checked.t = exists typ ~compute

let unsafe_unwrap : 'a t -> 'a option = function
  | Proving_mode proof ->
      Some proof
  | Circuit_mode ->
      None

let as_prover_value x = exists (Typ.prover_value ()) ~compute:(get x)

let map x ~f =
  match x with
  | Circuit_mode ->
      Circuit_mode
  | Proving_mode x ->
      Proving_mode (f x)

let bind x ~f =
  match x with Circuit_mode -> Circuit_mode | Proving_mode x -> f x

let return x = Proving_mode x
