module Backend = Kimchi_backend.Pasta.Vesta_based_plonk
module Other_backend = Kimchi_backend.Pasta.Pallas_based_plonk
module Impl = Pickles.Impls.Step
module Other_impl = Pickles.Impls.Wrap
module Challenge = Limb_vector.Challenge.Make (Impl)
module Sc =
  Pickles.Scalar_challenge.Make (Impl) (Pickles.Step_main_inputs.Inner_curve)
    (Challenge)
    (Pickles.Endo.Step_inner_curve)
module Js = Js_of_ocaml.Js

let _console_log_string s = Js_of_ocaml.Firebug.console##log (Js.string s)

let _console_log s = Js_of_ocaml.Firebug.console##log s

let _console_dir s : unit =
  let f =
    Js.Unsafe.eval_string {js|(function(s) { console.dir(s, {depth: 5}); })|js}
  in
  Js.Unsafe.(fun_call f [| inject s |])

let raise_error s =
  Js.Js_error.(raise_ @@ of_error (new%js Js.error_constr (Js.string s)))

let _raise_errorf fmt = Core_kernel.ksprintf raise_error fmt

external raise_exn_js : exn -> Js.js_string Js.t -> 'a = "custom_reraise_exn"

let raise_exn exn = raise_exn_js exn (Js.string (Core_kernel.Exn.to_string exn))

let log_and_raise_error_with_message ~exn ~msg =
  match Js.Optdef.to_option msg with
  | None ->
      raise_exn exn
  | Some msg ->
      let stack = Printexc.get_backtrace () in
      let msg =
        Printf.sprintf "%s\n%s%s" (Js.to_string msg)
          (Core_kernel.Exn.to_string exn)
          stack
      in
      raise_error msg

let json_parse (str : Js.js_string Js.t) =
  Js.Unsafe.(fun_call global ##. JSON##.parse [| inject str |])

class type field_class =
  object
    method value : Impl.Field.t Js.prop

    method toString : Js.js_string Js.t Js.meth

    method toJSON : < .. > Js.t Js.meth

    method toFields : field_class Js.t Js.js_array Js.t Js.meth
  end

and bool_class =
  object
    method value : Impl.Boolean.var Js.prop

    method toBoolean : bool Js.t Js.meth

    method toField : field_class Js.t Js.meth

    method toJSON : < .. > Js.t Js.meth

    method toFields : field_class Js.t Js.js_array Js.t Js.meth
  end

module As_field = struct
  (* number | string | boolean | field_class | cvar *)
  type t

  let of_field (x : Impl.Field.t) : t = Obj.magic x

  let of_number_exn (value : t) : Impl.Field.t =
    let number : Js.number Js.t = Obj.magic value in
    let float = Js.float_of_number number in
    if Float.is_integer float then
      if float >= 0. then
        Impl.Field.(
          constant @@ Constant.of_string @@ Js.to_string @@ number##toString)
      else
        let number : Js.number Js.t = Obj.magic (-.float) in
        Impl.Field.negate
          Impl.Field.(
            constant @@ Constant.of_string @@ Js.to_string @@ number##toString)
    else raise_error "Cannot convert a float to a field element"

  let of_boolean (value : t) : Impl.Field.t =
    let value = Js.to_bool (Obj.magic value) in
    if value then Impl.Field.one else Impl.Field.zero

  let of_string_exn (value : t) : Impl.Field.t =
    let value : Js.js_string Js.t = Obj.magic value in
    let s = Js.to_string value in
    try
      Impl.Field.constant
        ( if
          String.length s >= 2
          && Char.equal s.[0] '0'
          && Char.equal (Char.lowercase_ascii s.[1]) 'x'
        then Kimchi_pasta.Pasta.Fp.(of_bigint (Bigint.of_hex_string s))
        else if String.length s >= 1 && Char.equal s.[0] '-' then
          String.sub s 1 (String.length s - 1)
          |> Impl.Field.Constant.of_string |> Impl.Field.Constant.negate
        else Impl.Field.Constant.of_string s )
    with Failure e -> raise_error e

  let of_bigint_exn (value : t) : Impl.Field.t =
    let bigint : < toString : Js.js_string Js.t Js.meth > Js.t =
      Obj.magic value
    in
    bigint##toString |> Obj.magic |> of_string_exn

  let value (value : t) : Impl.Field.t =
    match Js.to_string (Js.typeof (Obj.magic value)) with
    | "number" ->
        of_number_exn value
    | "boolean" ->
        of_boolean value
    | "string" ->
        of_string_exn value
    | "bigint" ->
        of_bigint_exn value
    | "object" ->
        let is_array = Js.to_bool (Js.Unsafe.global ##. Array##isArray value) in
        if is_array then
          (* Cvar case *)
          (* TODO: Make this conversion more robust by rejecting invalid cases *)
          Obj.magic value
        else
          (* Object case *)
          Js.Optdef.get
            (Obj.magic value)##.value
            (fun () -> raise_error "Expected object with property \"value\"")
    | s ->
        raise_error
          (Core_kernel.sprintf
             "Type \"%s\" cannot be converted to a field element" s )

  let field_class : < .. > Js.t =
    let f =
      (* We could construct this using Js.wrap_meth_callback, but that returns a
         function that behaves weirdly (from the point-of-view of JS) when partially applied. *)
      Js.Unsafe.eval_string
        {js|
        (function(asFieldValue) {
          return function(x) {
            this.value = asFieldValue(x);
            return this;
          };
        })
      |js}
    in
    Js.Unsafe.(fun_call f [| inject (Js.wrap_callback value) |])

  let field_constr : (t -> field_class Js.t) Js.constr = Obj.magic field_class

  let to_field_obj (x : t) : field_class Js.t =
    match Js.to_string (Js.typeof (Obj.magic value)) with
    | "object" ->
        let is_array = Js.to_bool (Js.Unsafe.global ##. Array##isArray value) in
        if is_array then (* Cvar case *)
          new%js field_constr x else Obj.magic x
    | _ ->
        new%js field_constr x
end

let field_class = As_field.field_class

let field_constr = As_field.field_constr

open Core_kernel

module As_bool = struct
  (* boolean | bool_class | Boolean.var *)
  type t

  let of_boolean (x : Impl.Boolean.var) : t = Obj.magic x

  let of_js_bool (b : bool Js.t) : t = Obj.magic b

  let value (value : t) : Impl.Boolean.var =
    match Js.to_string (Js.typeof (Obj.magic value)) with
    | "boolean" ->
        let value = Js.to_bool (Obj.magic value) in
        Impl.Boolean.var_of_value value
    | "object" ->
        let is_array = Js.to_bool (Js.Unsafe.global ##. Array##isArray value) in
        if is_array then
          (* Cvar case *)
          (* TODO: Make this conversion more robust by rejecting invalid cases *)
          Obj.magic value
        else
          (* Object case *)
          Js.Optdef.get
            (Obj.magic value)##.value
            (fun () -> raise_error "Expected object with property \"value\"")
    | s ->
        raise_error
          (Core_kernel.sprintf "Type \"%s\" cannot be converted to a boolean" s)
end

let bool_class : < .. > Js.t =
  let f =
    Js.Unsafe.eval_string
      {js|
      (function(asBoolValue) {
        return function(x) {
          this.value = asBoolValue(x);
          return this;
        }
      })
    |js}
  in
  Js.Unsafe.(fun_call f [| inject (Js.wrap_callback As_bool.value) |])

let bool_constr : (As_bool.t -> bool_class Js.t) Js.constr =
  Obj.magic bool_class

module Field = Impl.Field
module Boolean = Impl.Boolean
module As_prover = Impl.As_prover
module Constraint = Impl.Constraint
module Bigint = Impl.Bigint
module Keypair = Impl.Keypair
module Verification_key = Impl.Verification_key
module Typ = Impl.Typ

(* helper functions *)

external prover_to_json :
  Kimchi_bindings.Protocol.Index.Fp.t -> Js.js_string Js.t = "prover_to_json"

let singleton_array (type a) (x : a) : a Js.js_array Js.t =
  let arr = new%js Js.array_empty in
  arr##push x |> ignore ;
  arr

let handle_constants f f_constant (x : Field.t) =
  match x with Constant x -> f_constant x | _ -> f x

let handle_constants2 f f_constant (x : Field.t) (y : Field.t) =
  match (x, y) with Constant x, Constant y -> f_constant x y | _ -> f x y

let array_get_exn xs i =
  Js.Optdef.get (Js.array_get xs i) (fun () ->
      raise_error (sprintf "array_get_exn: index=%d, length=%d" i xs##.length) )

let array_check_length xs n =
  if xs##.length <> n then raise_error (sprintf "Expected array of length %d" n)

let method_ class_ (name : string) (f : _ Js.t -> _) =
  let prototype = Js.Unsafe.get class_ (Js.string "prototype") in
  Js.Unsafe.set prototype (Js.string name) (Js.wrap_meth_callback f)

let optdef_arg_method (type a) class_ (name : string)
    (f : _ Js.t -> a Js.Optdef.t -> _) =
  let prototype = Js.Unsafe.get class_ (Js.string "prototype") in
  let meth =
    let wrapper =
      Js.Unsafe.eval_string
        {js|
        (function(f) {
          return function(xOptdef) {
            return f(this, xOptdef);
          };
        })|js}
    in
    Js.Unsafe.(fun_call wrapper [| inject (Js.wrap_callback f) |])
  in
  Js.Unsafe.set prototype (Js.string name) meth

let arg_optdef_arg_method (type a b) class_ (name : string)
    (f : _ Js.t -> b -> a Js.Optdef.t -> _) =
  let prototype = Js.Unsafe.get class_ (Js.string "prototype") in
  let meth =
    let wrapper =
      Js.Unsafe.eval_string
        {js|
        (function(f) {
          return function(argVal, xOptdef) {
            return f(this, argVal, xOptdef);
          };
        })|js}
    in
    Js.Unsafe.(fun_call wrapper [| inject (Js.wrap_callback f) |])
  in
  Js.Unsafe.set prototype (Js.string name) meth

let to_js_bigint =
  let bigint_constr = Js.Unsafe.eval_string {js|BigInt|js} in
  fun (s : Js.js_string Js.t) ->
    Js.Unsafe.fun_call bigint_constr [| Js.Unsafe.inject s |]

let to_js_field x : field_class Js.t = new%js field_constr (As_field.of_field x)

let to_unchecked (x : Field.t) =
  match x with Constant y -> y | y -> Impl.As_prover.read_var y

let () =
  let method_ name (f : field_class Js.t -> _) = method_ field_class name f in
  let to_string (x : Field.t) =
    ( match x with
    | Constant x ->
        x
    | x ->
        (* TODO: Put good error message here. *)
        As_prover.read_var x )
    |> Field.Constant.to_string |> Js.string
  in
  let mk = to_js_field in
  let add_op1 name (f : Field.t -> Field.t) =
    method_ name (fun this : field_class Js.t -> mk (f this##.value))
  in
  let add_op2 name (f : Field.t -> Field.t -> Field.t) =
    method_ name (fun this (y : As_field.t) : field_class Js.t ->
        mk (f this##.value (As_field.value y)) )
  in
  let sub =
    handle_constants2 Field.sub (fun x y ->
        Field.constant (Field.Constant.sub x y) )
  in
  let div =
    handle_constants2 Field.div (fun x y ->
        Field.constant (Field.Constant.( / ) x y) )
  in
  let sqrt =
    handle_constants Field.sqrt (fun x ->
        Field.constant (Field.Constant.sqrt x) )
  in
  add_op2 "add" Field.add ;
  add_op2 "sub" sub ;
  add_op2 "div" div ;
  add_op2 "mul" Field.mul ;
  add_op1 "neg" Field.negate ;
  add_op1 "inv" Field.inv ;
  add_op1 "square" Field.square ;
  add_op1 "sqrt" sqrt ;
  method_ "toString" (fun this : Js.js_string Js.t -> to_string this##.value) ;
  method_ "sizeInFields" (fun _this : int -> 1) ;
  method_ "toFields" (fun this : field_class Js.t Js.js_array Js.t ->
      singleton_array this ) ;
  method_ "toBigInt" (fun this -> to_string this##.value |> to_js_bigint) ;
  ((* TODO: Make this work with arbitrary bit length *)
   let bit_length = Field.size_in_bits - 2 in
   let cmp_method (name, f) =
     arg_optdef_arg_method field_class name
       (fun this (y : As_field.t) (msg : Js.js_string Js.t Js.Optdef.t) : unit
       ->
         try f ~bit_length this##.value (As_field.value y)
         with exn -> log_and_raise_error_with_message ~exn ~msg )
   in
   let bool_cmp_method (name, f) =
     method_ name (fun this (y : As_field.t) : bool_class Js.t ->
         new%js bool_constr
           (As_bool.of_boolean
              (f (Field.compare ~bit_length this##.value (As_field.value y))) ) )
   in
   (List.iter ~f:bool_cmp_method)
     [ ("lt", fun { less; _ } -> less)
     ; ("lte", fun { less_or_equal; _ } -> less_or_equal)
     ; ("gt", fun { less_or_equal; _ } -> Boolean.not less_or_equal)
     ; ("gte", fun { less; _ } -> Boolean.not less)
     ; ("lessThan", fun { less; _ } -> less)
     ; ("lessThanOrEqual", fun { less_or_equal; _ } -> less_or_equal)
     ; ("greaterThan", fun { less_or_equal; _ } -> Boolean.not less_or_equal)
     ; ("greaterThanOrEqual", fun { less; _ } -> Boolean.not less)
     ] ;
   List.iter ~f:cmp_method
     [ ("assertLt", Field.Assert.lt)
     ; ("assertLte", Field.Assert.lte)
     ; ("assertGt", Field.Assert.gt)
     ; ("assertGte", Field.Assert.gte)
     ; ("assertLessThan", Field.Assert.lt)
     ; ("assertLessThanOrEqual", Field.Assert.lte)
     ; ("assertGreaterThan", Field.Assert.gt)
     ; ("assertGreaterThanOrEqual", Field.Assert.gte)
     ] ) ;

  arg_optdef_arg_method field_class "assertEquals"
    (fun this (y : As_field.t) (msg : Js.js_string Js.t Js.Optdef.t) : unit ->
      try Field.Assert.equal this##.value (As_field.value y)
      with exn -> log_and_raise_error_with_message ~exn ~msg ) ;
  optdef_arg_method field_class "assertBoolean"
    (fun this (msg : Js.js_string Js.t Js.Optdef.t) : unit ->
      try Impl.assert_ (Constraint.boolean this##.value)
      with exn -> log_and_raise_error_with_message ~exn ~msg ) ;
  optdef_arg_method field_class "assertBool"
    (fun this (msg : Js.js_string Js.t Js.Optdef.t) : unit ->
      try Impl.assert_ (Constraint.boolean this##.value)
      with exn -> log_and_raise_error_with_message ~exn ~msg ) ;
  method_ "isZero" (fun this : bool_class Js.t ->
      new%js bool_constr
        (As_bool.of_boolean (Field.equal this##.value Field.zero)) ) ;
  optdef_arg_method field_class "toBits"
    (fun this (length : int Js.Optdef.t) : bool_class Js.t Js.js_array Js.t ->
      let length = Js.Optdef.get length (fun () -> Field.size_in_bits) in
      let k f bits =
        let arr = new%js Js.array_empty in
        List.iter bits ~f:(fun x ->
            arr##push (new%js bool_constr (As_bool.of_boolean (f x))) |> ignore ) ;
        arr
      in
      handle_constants
        (fun v -> k Fn.id (Field.choose_preimage_var ~length v))
        (fun x ->
          let bits = Field.Constant.unpack x in
          let bits, high_bits = List.split_n bits length in
          if List.exists high_bits ~f:Fn.id then
            raise_error
              (sprintf "Value %s did not fit in %d bits"
                 (Field.Constant.to_string x)
                 length ) ;
          k Boolean.var_of_value bits )
        this##.value ) ;
  method_ "equals" (fun this (y : As_field.t) : bool_class Js.t ->
      new%js bool_constr
        (As_bool.of_boolean (Field.equal this##.value (As_field.value y))) ) ;
  let static_op1 name (f : Field.t -> Field.t) =
    Js.Unsafe.set field_class (Js.string name)
      (Js.wrap_callback (fun (x : As_field.t) : field_class Js.t ->
           mk (f (As_field.value x)) ) )
  in
  let static_op2 name (f : Field.t -> Field.t -> Field.t) =
    Js.Unsafe.set field_class (Js.string name)
      (Js.wrap_callback
         (fun (x : As_field.t) (y : As_field.t) : field_class Js.t ->
           mk (f (As_field.value x) (As_field.value y)) ) )
  in
  field_class##.one := mk Field.one ;
  field_class##.zero := mk Field.zero ;
  field_class##.minusOne := mk @@ Field.negate Field.one ;
  Js.Unsafe.set field_class (Js.string "ORDER")
    ( to_js_bigint @@ Js.string @@ Pasta_bindings.BigInt256.to_string
    @@ Pasta_bindings.Fp.size () ) ;
  field_class##.random :=
    Js.wrap_callback (fun () : field_class Js.t ->
        mk (Field.constant (Field.Constant.random ())) ) ;
  static_op2 "add" Field.add ;
  static_op2 "sub" sub ;
  static_op2 "mul" Field.mul ;
  static_op2 "div" div ;
  static_op1 "neg" Field.negate ;
  static_op1 "inv" Field.inv ;
  static_op1 "square" Field.square ;
  static_op1 "sqrt" sqrt ;
  field_class##.toString :=
    Js.wrap_callback (fun (x : As_field.t) : Js.js_string Js.t ->
        to_string (As_field.value x) ) ;
  field_class##.sizeInFields := Js.wrap_callback (fun () : int -> 1) ;
  field_class##.toFields :=
    Js.wrap_callback
      (fun (x : As_field.t) : field_class Js.t Js.js_array Js.t ->
        (As_field.to_field_obj x)##toFields ) ;
  field_class##.fromFields :=
    Js.wrap_callback
      (fun (xs : field_class Js.t Js.js_array Js.t) : field_class Js.t ->
        array_check_length xs 1 ; array_get_exn xs 0 ) ;
  field_class##.assertEqual :=
    Js.wrap_callback (fun (x : As_field.t) (y : As_field.t) : unit ->
        Field.Assert.equal (As_field.value x) (As_field.value y) ) ;
  field_class##.assertBoolean
  := Js.wrap_callback (fun (x : As_field.t) : unit ->
         Impl.assert_ (Constraint.boolean (As_field.value x)) ) ;
  field_class##.isZero :=
    Js.wrap_callback (fun (x : As_field.t) : bool_class Js.t ->
        new%js bool_constr
          (As_bool.of_boolean (Field.equal (As_field.value x) Field.zero)) ) ;
  field_class##.fromBits :=
    Js.wrap_callback
      (fun (bs : As_bool.t Js.js_array Js.t) : field_class Js.t ->
        try
          Array.map (Js.to_array bs) ~f:(fun b ->
              match (As_bool.value b :> Impl.Field.t) with
              | Constant b ->
                  Impl.Field.Constant.(equal one b)
              | _ ->
                  failwith "non-constant" )
          |> Array.to_list |> Field.Constant.project |> Field.constant |> mk
        with _ ->
          mk
            (Field.project
               (List.init bs##.length ~f:(fun i ->
                    Js.Optdef.case (Js.array_get bs i)
                      (fun () -> assert false)
                      As_bool.value ) ) ) ) ;
  (field_class##.toBits :=
     let wrapper =
       Js.Unsafe.eval_string
         {js|
          (function(toField) {
            return function(x, length) {
              return toField(x).toBits(length);
            };
          })|js}
     in
     Js.Unsafe.(
       fun_call wrapper [| inject (Js.wrap_callback As_field.to_field_obj) |])
  ) ;
  field_class##.equal :=
    Js.wrap_callback (fun (x : As_field.t) (y : As_field.t) : bool_class Js.t ->
        new%js bool_constr
          (As_bool.of_boolean
             (Field.equal (As_field.value x) (As_field.value y)) ) ) ;
  let static_method name f =
    Js.Unsafe.set field_class (Js.string name) (Js.wrap_callback f)
  in
  method_ "seal"
    (let seal = Pickles.Util.seal (module Impl) in
     fun (this : field_class Js.t) : field_class Js.t -> mk (seal this##.value)
    ) ;
  method_ "rangeCheckHelper"
    (fun (this : field_class Js.t) (num_bits : int) : field_class Js.t ->
      match this##.value with
      | Constant v ->
          let n = Bigint.of_field v in
          for i = num_bits to Field.size_in_bits - 1 do
            if Bigint.test_bit n i then
              raise_error
                (sprintf
                   !"rangeCheckHelper: Expected %{sexp:Field.Constant.t} to \
                     fit in %d bits"
                   v num_bits )
          done ;
          this
      | v ->
          let _a, _b, n =
            Pickles.Scalar_challenge.to_field_checked' ~num_bits
              (module Impl)
              { inner = v }
          in
          mk n ) ;
  method_ "isConstant" (fun (this : field_class Js.t) : bool Js.t ->
      match this##.value with Constant _ -> Js._true | _ -> Js._false ) ;
  method_ "toConstant" (fun (this : field_class Js.t) : field_class Js.t ->
      let x =
        match this##.value with Constant x -> x | x -> As_prover.read_var x
      in
      mk (Field.constant x) ) ;
  method_ "toJSON" (fun (this : field_class Js.t) : < .. > Js.t ->
      this##toString ) ;
  static_method "toJSON" (fun (this : field_class Js.t) : < .. > Js.t ->
      this##toJSON ) ;
  static_method "fromJSON" (fun (value : Js.Unsafe.any) : field_class Js.t ->
      let return x = Some (new%js field_constr (As_field.of_field x)) in
      let result : field_class Js.t option =
        match Js.to_string (Js.typeof (Js.Unsafe.coerce value)) with
        | "number" ->
            let value = Js.float_of_number (Obj.magic value) in
            if Caml.Float.is_integer value then
              return (Field.of_int (Float.to_int value))
            else None
        | "boolean" ->
            let value = Js.to_bool (Obj.magic value) in
            return (if value then Field.one else Field.zero)
        | "string" -> (
            let value : Js.js_string Js.t = Obj.magic value in
            let s = Js.to_string value in
            try
              return
                (Field.constant
                   ( if
                     String.length s > 1
                     && Char.equal s.[0] '0'
                     && Char.equal (Char.lowercase s.[1]) 'x'
                   then
                     Kimchi_pasta.Pasta.Fp.(of_bigint (Bigint.of_hex_string s))
                   else Field.Constant.of_string s ) )
            with Failure _ -> None )
        | "bigint" ->
            return (Obj.magic value)
        | _ ->
            None
      in
      Option.value_exn ~message:"Field.fromJSON failed" result ) ;
  static_method "check" (fun _x -> ())

let () =
  let handle_constants2 f f_constant (x : Boolean.var) (y : Boolean.var) =
    match ((x :> Field.t), (y :> Field.t)) with
    | Constant x, Constant y ->
        f_constant x y
    | _ ->
        f x y
  in
  let equal =
    handle_constants2 Boolean.equal (fun x y ->
        Boolean.var_of_value (Field.Constant.equal x y) )
  in
  let mk x : bool_class Js.t = new%js bool_constr (As_bool.of_boolean x) in
  let method_ name (f : bool_class Js.t -> _) = method_ bool_class name f in
  let add_op1 name (f : Boolean.var -> Boolean.var) =
    method_ name (fun this : bool_class Js.t -> mk (f this##.value))
  in
  let add_op2 name (f : Boolean.var -> Boolean.var -> Boolean.var) =
    method_ name (fun this (y : As_bool.t) : bool_class Js.t ->
        mk (f this##.value (As_bool.value y)) )
  in
  Js.Unsafe.set bool_class (Js.string "true") (mk Boolean.true_) ;
  Js.Unsafe.set bool_class (Js.string "false") (mk Boolean.false_) ;
  method_ "toField" (fun this : field_class Js.t ->
      new%js field_constr (As_field.of_field (this##.value :> Field.t)) ) ;
  add_op1 "not" Boolean.not ;
  add_op2 "and" Boolean.( &&& ) ;
  add_op2 "or" Boolean.( ||| ) ;
  arg_optdef_arg_method bool_class "assertEquals"
    (fun this (y : As_bool.t) (msg : Js.js_string Js.t Js.Optdef.t) : unit ->
      try Boolean.Assert.( = ) this##.value (As_bool.value y)
      with exn -> log_and_raise_error_with_message ~exn ~msg ) ;
  optdef_arg_method bool_class "assertTrue"
    (fun this (msg : Js.js_string Js.t Js.Optdef.t) : unit ->
      try Boolean.Assert.is_true this##.value
      with exn -> log_and_raise_error_with_message ~exn ~msg ) ;
  optdef_arg_method bool_class "assertFalse"
    (fun this (msg : Js.js_string Js.t Js.Optdef.t) : unit ->
      try Boolean.Assert.( = ) this##.value Boolean.false_
      with exn -> log_and_raise_error_with_message ~exn ~msg ) ;
  add_op2 "equals" equal ;
  method_ "toBoolean" (fun this : bool Js.t ->
      match (this##.value :> Field.t) with
      | Constant x ->
          Js.bool Field.Constant.(equal one x)
      | _ -> (
          try Js.bool (As_prover.read Boolean.typ this##.value)
          with _ ->
            raise_error
              "Bool.toBoolean can only be called on non-witness values." ) ) ;
  method_ "sizeInFields" (fun _this : int -> 1) ;
  method_ "toString" (fun this ->
      let x =
        match (this##.value :> Field.t) with
        | Constant x ->
            x
        | x ->
            As_prover.read_var x
      in
      if Field.Constant.(equal one) x then "true" else "false" ) ;
  method_ "toFields" (fun this : field_class Js.t Js.js_array Js.t ->
      let arr = new%js Js.array_empty in
      arr##push this##toField |> ignore ;
      arr ) ;
  let static_method name f =
    Js.Unsafe.set bool_class (Js.string name) (Js.wrap_callback f)
  in
  let static_op1 name (f : Boolean.var -> Boolean.var) =
    static_method name (fun (x : As_bool.t) : bool_class Js.t ->
        mk (f (As_bool.value x)) )
  in
  let static_op2 name (f : Boolean.var -> Boolean.var -> Boolean.var) =
    static_method name (fun (x : As_bool.t) (y : As_bool.t) : bool_class Js.t ->
        mk (f (As_bool.value x) (As_bool.value y)) )
  in
  static_method "toField" (fun (x : As_bool.t) ->
      new%js field_constr (As_field.of_field (As_bool.value x :> Field.t)) ) ;
  Js.Unsafe.set bool_class (Js.string "Unsafe")
    (object%js
       method ofField (x : As_field.t) : bool_class Js.t =
         new%js bool_constr
           (As_bool.of_boolean (Boolean.Unsafe.of_cvar (As_field.value x)))
    end ) ;
  static_op1 "not" Boolean.not ;
  static_op2 "and" Boolean.( &&& ) ;
  static_op2 "or" Boolean.( ||| ) ;
  static_method "assertEqual" (fun (x : As_bool.t) (y : As_bool.t) : unit ->
      Boolean.Assert.( = ) (As_bool.value x) (As_bool.value y) ) ;
  static_op2 "equal" equal ;
  static_method "count"
    (fun (bs : As_bool.t Js.js_array Js.t) : field_class Js.t ->
      new%js field_constr
        (As_field.of_field
           (Field.sum
              (List.init bs##.length ~f:(fun i ->
                   ( Js.Optdef.case (Js.array_get bs i)
                       (fun () -> assert false)
                       As_bool.value
                     :> Field.t ) ) ) ) ) ) ;
  static_method "sizeInFields" (fun () : int -> 1) ;
  static_method "toFields"
    (fun (x : As_bool.t) : field_class Js.t Js.js_array Js.t ->
      singleton_array
        (new%js field_constr (As_field.of_field (As_bool.value x :> Field.t))) ) ;
  static_method "fromFields"
    (fun (xs : field_class Js.t Js.js_array Js.t) : bool_class Js.t ->
      if xs##.length = 1 then
        Js.Optdef.case (Js.array_get xs 0)
          (fun () -> assert false)
          (fun x -> mk (Boolean.Unsafe.of_cvar x##.value))
      else raise_error "Expected array of length 1" ) ;
  static_method "check" (fun (x : bool_class Js.t) : unit ->
      Impl.assert_ (Constraint.boolean (x##.value :> Field.t)) ) ;
  method_ "toJSON" (fun (this : bool_class Js.t) : < .. > Js.t ->
      Js.Unsafe.coerce this##toBoolean ) ;
  static_method "toJSON" (fun (this : bool_class Js.t) : < .. > Js.t ->
      this##toJSON ) ;
  static_method "fromJSON" (fun (value : Js.Unsafe.any) : bool_class Js.t ->
      match Js.to_string (Js.typeof (Js.Unsafe.coerce value)) with
      | "boolean" ->
          new%js bool_constr (As_bool.of_js_bool (Js.Unsafe.coerce value))
      | s ->
          failwith ("Bool.fromJSON: expected boolean, got " ^ s) )

let array_iter t1 ~f =
  for i = 0 to t1##.length - 1 do
    f (array_get_exn t1 i)
  done

module Poseidon_sponge_checked =
  Sponge.Make_sponge (Pickles.Step_main_inputs.Sponge.Permutation)
module Poseidon_sponge =
  Sponge.Make_sponge (Sponge.Poseidon (Pickles.Tick_field_sponge.Inputs))

let sponge_params_checked =
  Sponge.Params.(
    map pasta_p_kimchi ~f:(Fn.compose Field.constant Field.Constant.of_string))

let sponge_params =
  Sponge.Params.(map pasta_p_kimchi ~f:Field.Constant.of_string)

type sponge =
  | Checked of Poseidon_sponge_checked.t
  | Unchecked of Poseidon_sponge.t

let hash_array (xs : Field.t array) (is_checked : bool Js.t) : Field.t =
  if Js.to_bool is_checked then Random_oracle.Checked.hash xs
  else Random_oracle.hash (Array.map ~f:to_unchecked xs) |> Field.constant

let poseidon =
  object%js
    (* this could be removed eventually since it's easily implemented using `update` *)
    method hash input is_checked = hash_array input is_checked

    method hashToGroup (xs : Field.t array) (is_checked : bool Js.t) =
      let input = hash_array xs is_checked in
      let digest =
        if Js.to_bool is_checked then
          Snark_params.Group_map.Checked.to_group input
        else
          let x, y = Snark_params.Group_map.to_group (to_unchecked input) in
          (Field.constant @@ x, Field.constant @@ y)
      in
      digest

    method update (state : Field.t Random_oracle.State.t)
        (input : Field.t array) (is_checked : bool Js.t)
        : Field.t Random_oracle.State.t =
      if Js.to_bool is_checked then Random_oracle.Checked.update ~state input
      else
        Random_oracle.update
          ~state:(Random_oracle.State.map ~f:to_unchecked state)
          (Array.map ~f:to_unchecked input)
        |> Random_oracle.State.map ~f:Field.constant

    (* returns a "sponge" that stays opaque to JS *)
    method spongeCreate (is_checked : bool Js.t) : sponge =
      if Js.to_bool is_checked then
        Checked
          (Poseidon_sponge_checked.create ?init:None sponge_params_checked)
      else Unchecked (Poseidon_sponge.create ?init:None sponge_params)

    method spongeAbsorb (sponge : sponge) (field : Field.t) : unit =
      match sponge with
      | Checked s ->
          Poseidon_sponge_checked.absorb s field
      | Unchecked s ->
          Poseidon_sponge.absorb s (to_unchecked @@ field)

    method spongeSqueeze (sponge : sponge) : Field.t =
      match sponge with
      | Checked s ->
          Poseidon_sponge_checked.squeeze s
      | Unchecked s ->
          Poseidon_sponge.squeeze s |> Field.constant

    val prefixes =
      let open Hash_prefixes in
      object%js
        val event = Js.string (zkapp_event :> string)

        val events = Js.string (zkapp_events :> string)

        val sequenceEvents = Js.string (zkapp_actions :> string)

        val body = Js.string (zkapp_body :> string)

        val accountUpdateCons = Js.string (account_update_cons :> string)

        val accountUpdateNode = Js.string (account_update_node :> string)

        val zkappMemo = Js.string (zkapp_memo :> string)
      end
  end

(* light-weight wrapper around snarky-ml core *)

module Snarky = struct
  let typ (size_in_fields : int) = Typ.array ~length:size_in_fields Field.typ

  let exists (size_in_fields : int) (compute : unit -> Field.Constant.t array) =
    Impl.exists (typ size_in_fields) ~compute

  let exists_var (compute : unit -> Field.Constant.t) =
    Impl.exists Field.typ ~compute

  let as_prover = Impl.as_prover

  let run_and_check (f : unit -> unit) =
    try
      Impl.run_and_check_exn (fun () ->
          f () ;
          fun () -> () )
    with exn -> raise_exn exn

  let run_unchecked (f : unit -> unit) =
    try
      Impl.run_and_check_exn (fun () ->
          Snarky_backendless.Snark0.set_eval_constraints false ;
          f () ;
          Snarky_backendless.Snark0.set_eval_constraints true ;
          fun () -> () )
    with exn -> raise_exn exn

  let constraint_system (main : unit -> unit) =
    let cs =
      Impl.constraint_system ~input_typ:Impl.Typ.unit ~return_typ:Impl.Typ.unit
        (fun () -> main)
    in
    object%js
      val rows = Backend.R1CS_constraint_system.get_rows_len cs

      val digest =
        Backend.R1CS_constraint_system.digest cs |> Md5.to_hex |> Js.string

      val json =
        Backend.R1CS_constraint_system.to_json cs |> Js.string |> json_parse
    end

  module Field = struct
    type t = Field.t

    (** add x, y to get a new AST node Add(x, y); handles if x, y are constants *)
    let add x y = Field.add x y

    (** scale x by a constant to get a new AST node Scale(c, x); handles if x is a constant; handles c=0,1 *)
    let scale c x = Field.scale x c

    (** witnesses z = x*y and constrains it with [assert_r1cs]; handles constants *)
    let mul x y = Field.mul x y

    (** evaluates a CVar by unfolding the AST and reading Vars from a list of public input + aux values *)
    let read_var (x : Field.t) = As_prover.read_var x

    (** x === y without handling of constants *)
    let assert_equal x y = Impl.assert_ (Impl.Constraint.equal x y)

    (** x*y === z without handling of constants *)
    let assert_mul x y z = Impl.assert_ (Impl.Constraint.r1cs x y z)

    (** x*x === y without handling of constants *)
    let assert_square x y = Impl.assert_ (Impl.Constraint.square x y)

    (** x*x === x without handling of constants *)
    let assert_boolean x = Impl.assert_ (Impl.Constraint.boolean x)

    (** check x < y and x <= y.
        this is used in all comparisons, including with assert *)
    let compare (bit_length : int) x y =
      let ({ less; less_or_equal } : Field.comparison_result) =
        Field.compare ~bit_length x y
      in
      (less, less_or_equal)

    let to_bits (length : int) x =
      Field.choose_preimage_var ~length x |> Array.of_list

    let from_bits bits = Array.to_list bits |> Field.project

    (** returns x truncated to the lowest [16 * length_div_16] bits
       => can be used to assert that x fits in [16 * length_div_16] bits.

       more efficient than [to_bits] because it uses the [EC_endoscalar] gate;
       does 16 bits per row (vs 1 bits per row that you can do with generic gates).
    *)
    let truncate_to_bits16 (length_div_16 : int) x =
      let _a, _b, x0 =
        Pickles.Scalar_challenge.to_field_checked' ~num_bits:(length_div_16 * 16)
          (module Impl)
          { inner = x }
      in
      x0

    (* can be implemented with Field.to_constant_and_terms *)
    let seal x = Pickles.Util.seal (module Impl) x

    let to_constant_and_terms x = Field.to_constant_and_terms x
  end

  module Group = struct
    type t = Pickles.Step_main_inputs.Inner_curve.t

    (** p1 + p2; handles variables *)
    let add p1 p2 = Pickles.Step_main_inputs.Ops.add_fast p1 p2

    let on_curve p = Pickles.Step_main_inputs.Inner_curve.assert_on_curve p

    let scale p (scalar_bits : Boolean.var array) =
      Pickles.Step_main_inputs.Ops.scale_fast_msb_bits p
        (Shifted_value scalar_bits)

    let equals ((x1, y1) : t) ((x2, y2) : t) =
      Boolean.all [ Impl.Field.equal x1 x2; Impl.Field.equal y1 y2 ]
  end

  module Circuit = struct
    module Main = struct
      let of_js (main : Field.t array -> unit) =
        let main' public_input () = main public_input in
        main'
    end

    let compile main public_input_size =
      let input_typ = typ public_input_size in
      let return_typ = Impl.Typ.unit in
      let cs =
        Impl.constraint_system ~input_typ ~return_typ (Main.of_js main)
      in
      Impl.Keypair.generate ~prev_challenges:0 cs

    let prove main public_input_size public_input keypair =
      let pk = Impl.Keypair.pk keypair in
      let input_typ = typ public_input_size in
      let return_typ = Impl.Typ.unit in
      Impl.generate_witness_conv ~input_typ ~return_typ
        ~f:(fun { Impl.Proof_inputs.auxiliary_inputs; public_inputs } () ->
          Backend.Proof.create pk ~auxiliary:auxiliary_inputs
            ~primary:public_inputs )
        (Main.of_js main) public_input

    let verify public_input proof vk =
      let public_input_vec = Backend.Field.Vector.create () in
      Array.iter public_input ~f:(fun x ->
          Backend.Field.Vector.emplace_back public_input_vec x ) ;
      Backend.Proof.verify proof vk public_input_vec |> Js.bool

    module Keypair = struct
      let get_vk t = Impl.Keypair.vk t

      let get_cs_json t =
        (Impl.Keypair.pk t).index |> prover_to_json |> json_parse
    end
  end
end

let snarky =
  object%js
    method exists = Snarky.exists

    method existsVar = Snarky.exists_var

    method asProver = Snarky.as_prover

    method runAndCheck = Snarky.run_and_check

    method runUnchecked = Snarky.run_unchecked

    method constraintSystem = Snarky.constraint_system

    val field =
      object%js
        method add = Snarky.Field.add

        method scale = Snarky.Field.scale

        method mul = Snarky.Field.mul

        method readVar = Snarky.Field.read_var

        method assertEqual = Snarky.Field.assert_equal

        method assertMul = Snarky.Field.assert_mul

        method assertSquare = Snarky.Field.assert_square

        method assertBoolean = Snarky.Field.assert_boolean

        method compare = Snarky.Field.compare

        method toBits = Snarky.Field.to_bits

        method fromBits = Snarky.Field.from_bits

        method truncateToBits16 = Snarky.Field.truncate_to_bits16

        method seal = Snarky.Field.seal

        method toConstantAndTerms = Snarky.Field.to_constant_and_terms
      end

    val group =
      object%js
        method add = Snarky.Group.add

        method onCurve = Snarky.Group.on_curve

        method scale = Snarky.Group.scale

        method equals = Snarky.Group.equals
      end

    val circuit =
      object%js
        method compile = Snarky.Circuit.compile

        method prove = Snarky.Circuit.prove

        method verify = Snarky.Circuit.verify

        val keypair =
          object%js
            method getVerificationKey = Snarky.Circuit.Keypair.get_vk

            method getConstraintSystemJSON = Snarky.Circuit.Keypair.get_cs_json
          end
      end
  end

(* helpers for pickles_compile *)

module Public_input = struct
  type t = Field.t array

  module Constant = struct
    type t = Field.Constant.t array
  end
end

type 'a statement = 'a array * 'a array

module Statement = struct
  type t = Field.t statement

  module Constant = struct
    type t = Field.Constant.t statement
  end
end

let public_input_typ (i : int) = Typ.array ~length:i Field.typ

let statement_typ (input_size : int) (output_size : int) =
  Typ.(array ~length:input_size Field.typ * array ~length:output_size Field.typ)

type ('prev_proof, 'proof) js_prover =
     Public_input.Constant.t
  -> 'prev_proof array
  -> (Public_input.Constant.t * 'proof) Promise_js_helpers.js_promise

let dummy_constraints =
  let module Inner_curve = Kimchi_pasta.Pasta.Pallas in
  let module Step_main_inputs = Pickles.Step_main_inputs in
  let inner_curve_typ : (Field.t * Field.t, Inner_curve.t) Typ.t =
    Typ.transport Step_main_inputs.Inner_curve.typ
      ~there:Inner_curve.to_affine_exn ~back:Inner_curve.of_affine
  in
  fun () ->
    let x =
      Impl.exists Field.typ ~compute:(fun () -> Field.Constant.of_int 3)
    in
    let g = Impl.exists inner_curve_typ ~compute:(fun _ -> Inner_curve.one) in
    ignore
      ( Pickles.Scalar_challenge.to_field_checked'
          (module Impl)
          ~num_bits:16
          (Kimchi_backend_common.Scalar_challenge.create x)
        : Field.t * Field.t * Field.t ) ;
    ignore
      ( Step_main_inputs.Ops.scale_fast g ~num_bits:5 (Shifted_value x)
        : Step_main_inputs.Inner_curve.t ) ;
    ignore
      ( Pickles.Step_verifier.Scalar_challenge.endo g ~num_bits:4
          (Kimchi_backend_common.Scalar_challenge.create x)
        : Field.t * Field.t )

type pickles_rule_js =
  < identifier : Js.js_string Js.t Js.prop
  ; main :
      (   Public_input.t
       -> < publicOutput : Public_input.t Js.prop
          ; previousStatements : Statement.t array Js.prop
          ; shouldVerify : Boolean.var array Js.prop >
          Js.t )
      Js.prop
  ; proofsToVerify :
      < isSelf : bool Js.t Js.prop ; tag : Js.Unsafe.any Js.t Js.prop > Js.t
      array
      Js.prop >
  Js.t

module Choices = struct
  open Pickles_types
  open Hlist

  module Prevs = struct
    type ('var, 'value, 'width, 'height) t =
      | Prevs :
          (   self:('var, 'value, 'width, 'height) Pickles.Tag.t
           -> ('prev_var, 'prev_values, 'widths, 'heights) H4.T(Pickles.Tag).t
          )
          -> ('var, 'value, 'width, 'height) t

    let of_rule (rule : pickles_rule_js) =
      let js_prevs = rule##.proofsToVerify in
      let rec get_tags (Prevs prevs) index =
        if index < 0 then Prevs prevs
        else
          let js_tag = Array.get js_prevs index in
          (* We introduce new opaque types to make sure that the type in the tag
             doesn't escape into the environment or have other ill effects.
          *)
          let module Types = struct
            type var

            type value

            type width

            type height
          end in
          let open Types in
          let to_tag ~self tag : (var, value, width, height) Pickles.Tag.t =
            (* The magic here isn't ideal, but it's safe enough if we immediately
               hide it behind [Types].
            *)
            if Js.to_bool tag##.isSelf then Obj.magic self
            else Obj.magic tag##.tag
          in
          let tag = to_tag js_tag in
          let prevs ~self : _ H4.T(Pickles.Tag).t = tag ~self :: prevs ~self in
          get_tags (Prevs prevs) (index - 1)
      in
      get_tags (Prevs (fun ~self:_ -> [])) (Array.length js_prevs - 1)
  end

  module Inductive_rule = struct
    type ( 'var
         , 'value
         , 'width
         , 'height
         , 'arg_var
         , 'arg_value
         , 'ret_var
         , 'ret_value
         , 'auxiliary_var
         , 'auxiliary_value )
         t =
      | Rule :
          (   self:('var, 'value, 'width, 'height) Pickles.Tag.t
           -> ( 'prev_vars
              , 'prev_values
              , 'widths
              , 'heights
              , 'arg_var
              , 'arg_value
              , 'ret_var
              , 'ret_value
              , 'auxiliary_var
              , 'auxiliary_value )
              Pickles.Inductive_rule.t )
          -> ( 'var
             , 'value
             , 'width
             , 'height
             , 'arg_var
             , 'arg_value
             , 'ret_var
             , 'ret_value
             , 'auxiliary_var
             , 'auxiliary_value )
             t

    let rec should_verifys :
        type prev_vars prev_values widths heights.
           int
        -> (prev_vars, prev_values, widths, heights) H4.T(Pickles.Tag).t
        -> Boolean.var array
        -> prev_vars H1.T(E01(Pickles.Inductive_rule.B)).t =
     fun index tags should_verifys_js ->
      match tags with
      | [] ->
          []
      | _ :: tags ->
          let js_bool = Array.get should_verifys_js index in
          let should_verifys =
            should_verifys (index + 1) tags should_verifys_js
          in
          js_bool :: should_verifys

    let should_verifys tags should_verifys_js =
      should_verifys 0 tags should_verifys_js

    let get_typ ~public_input_size ~public_output_size
        (type a1 a2 a3 a4 width height) (tag : (a1, a2, a3, a4) Pickles.Tag.t)
        (self :
          ( Public_input.t * Public_input.t
          , Public_input.Constant.t * Public_input.Constant.t
          , width
          , height )
          Pickles.Tag.t ) =
      match Type_equal.Id.same_witness tag.id self.id with
      | None ->
          Pickles.Types_map.public_input tag
      | Some T ->
          statement_typ public_input_size public_output_size

    let rec prev_statements :
        type prev_vars prev_values widths heights width height.
           public_input_size:int
        -> public_output_size:int
        -> self:
             ( Public_input.t * Public_input.t
             , Public_input.Constant.t * Public_input.Constant.t
             , width
             , height )
             Pickles.Tag.t
        -> int
        -> (prev_vars, prev_values, widths, heights) H4.T(Pickles.Tag).t
        -> Statement.t array
        -> prev_vars H1.T(Id).t =
     fun ~public_input_size ~public_output_size ~self i tags statements ->
      match tags with
      | [] ->
          []
      | tag :: tags ->
          let (Typ typ) =
            get_typ ~public_input_size ~public_output_size tag self
          in
          let input, output = Array.get statements i in
          let fields = Array.concat [ input; output ] in
          let aux = typ.constraint_system_auxiliary () in
          let statement = typ.var_of_fields (fields, aux) in
          statement
          :: prev_statements ~public_input_size ~public_output_size ~self
               (i + 1) tags statements

    let prev_statements ~public_input_size ~public_output_size ~self tags
        statements =
      prev_statements ~public_input_size ~public_output_size ~self 0 tags
        statements

    type _ Snarky_backendless.Request.t +=
      | Get_prev_proof : int -> _ Pickles.Proof.t Snarky_backendless.Request.t

    let create ~public_input_size ~public_output_size (rule : pickles_rule_js) :
        ( _
        , _
        , _
        , _
        , Public_input.t
        , Public_input.Constant.t
        , Public_input.t
        , Public_input.Constant.t
        , unit
        , unit )
        t =
      let (Prevs prevs) = Prevs.of_rule rule in
      Rule
        (fun ~(self :
                ( Field.t array * Field.t array
                , Impl.field array * Impl.field array
                , 'b3
                , 'b4 )
                Pickles.Tag.t ) ->
          let prevs = prevs ~self in
          { Pickles.Inductive_rule.identifier = Js.to_string rule##.identifier
          ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
          ; prevs
          ; main =
              (fun { public_input } ->
                dummy_constraints () ;
                let result = rule##.main public_input in
                let public_output = result##.publicOutput in
                let previous_proofs_should_verify =
                  should_verifys prevs result##.shouldVerify
                in
                let previous_public_inputs =
                  prev_statements ~public_input_size ~public_output_size ~self
                    prevs
                    result##.previousStatements
                in
                let previous_proof_statements =
                  let rec go :
                      type prev_vars prev_values widths heights.
                         int
                      -> prev_vars H1.T(Id).t
                      -> prev_vars H1.T(E01(Pickles.Inductive_rule.B)).t
                      -> ( prev_vars
                         , prev_values
                         , widths
                         , heights )
                         H4.T(Pickles.Tag).t
                      -> ( prev_vars
                         , widths )
                         H2.T(Pickles.Inductive_rule.Previous_proof_statement).t
                      =
                   fun i public_inputs should_verifys tags ->
                    match (public_inputs, should_verifys, tags) with
                    | [], [], [] ->
                        []
                    | ( public_input :: public_inputs
                      , proof_must_verify :: should_verifys
                      , _tag :: tags ) ->
                        let proof =
                          Impl.exists (Impl.Typ.Internal.ref ())
                            ~request:(fun () -> Get_prev_proof i)
                        in
                        { public_input; proof; proof_must_verify }
                        :: go (i + 1) public_inputs should_verifys tags
                  in
                  go 0 previous_public_inputs previous_proofs_should_verify
                    prevs
                in
                { previous_proof_statements
                ; public_output
                ; auxiliary_output = ()
                } )
          } )
  end

  type ( 'var
       , 'value
       , 'width
       , 'height
       , 'arg_var
       , 'arg_value
       , 'ret_var
       , 'ret_value
       , 'auxiliary_var
       , 'auxiliary_value )
       t =
    | Choices :
        (   self:('var, 'value, 'width, 'height) Pickles.Tag.t
         -> ( 'prev_vars
            , 'prev_values
            , 'widths
            , 'heights
            , 'arg_var
            , 'arg_value
            , 'ret_var
            , 'ret_value
            , 'auxiliary_var
            , 'auxiliary_value )
            H4_6.T(Pickles.Inductive_rule).t )
        -> ( 'var
           , 'value
           , 'width
           , 'height
           , 'arg_var
           , 'arg_value
           , 'ret_var
           , 'ret_value
           , 'auxiliary_var
           , 'auxiliary_value )
           t

  let of_js ~public_input_size ~public_output_size js_rules =
    let rec get_rules (Choices rules) index :
        ( _
        , _
        , _
        , _
        , Public_input.t
        , Public_input.Constant.t
        , Public_input.t
        , Public_input.Constant.t
        , unit
        , unit )
        t =
      if index < 0 then Choices rules
      else
        let (Rule rule) =
          Inductive_rule.create ~public_input_size ~public_output_size
            (Array.get js_rules index)
        in
        let rules ~self : _ H4_6.T(Pickles.Inductive_rule).t =
          rule ~self :: rules ~self
        in
        get_rules (Choices rules) (index - 1)
    in
    get_rules (Choices (fun ~self:_ -> [])) (Array.length js_rules - 1)
end

type proof = (Pickles_types.Nat.N0.n, Pickles_types.Nat.N0.n) Pickles.Proof.t

module Public_inputs_with_proofs =
  Pickles_types.Hlist.H3.T (Pickles.Statement_with_proof)

let nat_modules_list : (module Pickles_types.Nat.Intf) list =
  let open Pickles_types.Nat in
  [ (module N0)
  ; (module N1)
  ; (module N2)
  ; (module N3)
  ; (module N4)
  ; (module N5)
  ; (module N6)
  ; (module N7)
  ; (module N8)
  ; (module N9)
  ; (module N10)
  ; (module N11)
  ; (module N12)
  ; (module N13)
  ; (module N14)
  ; (module N15)
  ; (module N16)
  ; (module N17)
  ; (module N18)
  ; (module N19)
  ; (module N20)
  ]

let nat_add_modules_list : (module Pickles_types.Nat.Add.Intf) list =
  let open Pickles_types.Nat in
  [ (module N0)
  ; (module N1)
  ; (module N2)
  ; (module N3)
  ; (module N4)
  ; (module N5)
  ; (module N6)
  ; (module N7)
  ; (module N8)
  ; (module N9)
  ; (module N10)
  ; (module N11)
  ; (module N12)
  ; (module N13)
  ; (module N14)
  ; (module N15)
  ; (module N16)
  ; (module N17)
  ; (module N18)
  ; (module N19)
  ; (module N20)
  ]

let nat_module (i : int) : (module Pickles_types.Nat.Intf) =
  List.nth_exn nat_modules_list i

let nat_add_module (i : int) : (module Pickles_types.Nat.Add.Intf) =
  List.nth_exn nat_add_modules_list i

let name = "smart-contract"

let constraint_constants =
  (* TODO these are dummy values *)
  { Snark_keys_header.Constraint_constants.sub_windows_per_window = 0
  ; ledger_depth = 0
  ; work_delay = 0
  ; block_window_duration_ms = 0
  ; transaction_capacity = Log_2 0
  ; pending_coinbase_depth = 0
  ; coinbase_amount = Unsigned.UInt64.of_int 0
  ; supercharged_coinbase_factor = 0
  ; account_creation_fee = Unsigned.UInt64.of_int 0
  ; fork = None
  }

let pickles_compile (choices : pickles_rule_js array)
    (signature :
      < publicInputSize : int Js.prop ; publicOutputSize : int Js.prop > Js.t )
    =
  (* translate number of branches and recursively verified proofs from JS *)
  let branches = Array.length choices in
  let max_proofs =
    let choices = choices |> Array.to_list in
    List.map choices ~f:(fun c -> c##.proofsToVerify |> Array.length)
    |> List.max_elt ~compare |> Option.value ~default:0
  in
  let (module Branches) = nat_module branches in
  let (module Max_proofs_verified) = nat_add_module max_proofs in

  (* translate method circuits from JS *)
  let public_input_size = signature##.publicInputSize in
  let public_output_size = signature##.publicOutputSize in
  let (Choices choices) =
    Choices.of_js ~public_input_size ~public_output_size choices
  in

  (* call into Pickles *)
  let tag, _cache, p, provers =
    Pickles.compile_promise () ~choices
      ~public_input:
        (Input_and_output
           ( public_input_typ public_input_size
           , public_input_typ public_output_size ) )
      ~auxiliary_typ:Typ.unit
      ~branches:(module Branches)
      ~max_proofs_verified:(module Max_proofs_verified)
      ~name ~constraint_constants
  in

  (* translate returned prover and verify functions to JS *)
  let module Proof = (val p) in
  let to_js_prover prover : ('prev_proof, Proof.t) js_prover =
    let prove (public_input : Public_input.Constant.t)
        (prevs : 'prev_proof array) =
      let handler (Snarky_backendless.Request.With { request; respond }) =
        match request with
        | Choices.Inductive_rule.Get_prev_proof i ->
            respond (Provide (Obj.magic (Array.get prevs i)))
        | _ ->
            respond Unhandled
      in
      prover ?handler:(Some handler) public_input
      |> Promise.map ~f:(fun (output, _, proof) -> (output, proof))
      |> Promise_js_helpers.to_js
    in
    prove
  in
  let rec to_js_provers :
      type a b c.
         ( a
         , b
         , c
         , Public_input.Constant.t
         , (Public_input.Constant.t * unit * Proof.t) Promise.t )
         Pickles.Provers.t
      -> ('prev_proof, Proof.t) js_prover list = function
    | [] ->
        []
    | p :: ps ->
        to_js_prover p :: to_js_provers ps
  in
  let provers : (_, Proof.t) js_prover array =
    provers |> to_js_provers |> Array.of_list
  in
  let verify (statement : Statement.Constant.t) (proof : _ Pickles.Proof.t) =
    Proof.verify_promise [ (statement, proof) ]
    |> Promise.map ~f:(fun x -> Js.bool (Or_error.is_ok x))
    |> Promise_js_helpers.to_js
  in
  object%js
    val provers = Obj.magic provers

    val verify = Obj.magic verify

    val tag = Obj.magic tag

    method getVerificationKey =
      let vk = Pickles.Side_loaded.Verification_key.of_compiled tag in
      let data = Pickles.Side_loaded.Verification_key.to_base64 vk in
      let hash = Mina_base.Zkapp_account.digest_vk vk in
      (data |> Js.string, hash)
  end

module Proof0 = Pickles.Proof.Make (Pickles_types.Nat.N0) (Pickles_types.Nat.N0)
module Proof1 = Pickles.Proof.Make (Pickles_types.Nat.N1) (Pickles_types.Nat.N1)
module Proof2 = Pickles.Proof.Make (Pickles_types.Nat.N2) (Pickles_types.Nat.N2)

type some_proof = Proof0 of Proof0.t | Proof1 of Proof1.t | Proof2 of Proof2.t

let proof_to_base64 = function
  | Proof0 proof ->
      Proof0.to_base64 proof |> Js.string
  | Proof1 proof ->
      Proof1.to_base64 proof |> Js.string
  | Proof2 proof ->
      Proof2.to_base64 proof |> Js.string

let proof_of_base64 str i : some_proof =
  let str = Js.to_string str in
  match i with
  | 0 ->
      Proof0 (Proof0.of_base64 str |> Result.ok_or_failwith)
  | 1 ->
      Proof1 (Proof1.of_base64 str |> Result.ok_or_failwith)
  | 2 ->
      Proof2 (Proof2.of_base64 str |> Result.ok_or_failwith)
  | _ ->
      failwith "invalid proof index"

let verify (statement : Statement.Constant.t) (proof : proof)
    (vk : Js.js_string Js.t) =
  let i, o = statement in
  let typ = statement_typ (Array.length i) (Array.length o) in
  let proof = Pickles.Side_loaded.Proof.of_proof proof in
  let vk =
    match Pickles.Side_loaded.Verification_key.of_base64 (Js.to_string vk) with
    | Ok vk_ ->
        vk_
    | Error err ->
        failwithf "Could not decode base64 verification key: %s"
          (Error.to_string_hum err) ()
  in
  Pickles.Side_loaded.verify_promise ~typ [ (vk, statement, proof) ]
  |> Promise.map ~f:(fun x -> Js.bool (Or_error.is_ok x))
  |> Promise_js_helpers.to_js

let dummy_base64_proof () =
  let n2 = Pickles_types.Nat.N2.n in
  let proof = Pickles.Proof.dummy n2 n2 n2 ~domain_log2:15 in
  Proof2.to_base64 proof |> Js.string

let dummy_verification_key () =
  let vk = Pickles.Side_loaded.Verification_key.dummy in
  let data = Pickles.Side_loaded.Verification_key.to_base64 vk in
  let hash = Mina_base.Zkapp_account.digest_vk vk in
  (data |> Js.string, hash)

let pickles =
  object%js
    val compile = pickles_compile

    val verify = verify

    val dummyBase64Proof = dummy_base64_proof

    val dummyVerificationKey = dummy_verification_key

    val proofToBase64 = proof_to_base64

    val proofOfBase64 = proof_of_base64

    val proofToBase64Transaction =
      fun (proof : proof) ->
        proof |> Pickles.Side_loaded.Proof.of_proof
        |> Pickles.Side_loaded.Proof.to_base64 |> Js.string
  end

type public_key = Signature_lib.Public_key.Compressed.t

type public_key_checked = Signature_lib.Public_key.Compressed.var

module Ledger = struct
  let ledger_class : < .. > Js.t =
    Js.Unsafe.eval_string {js|(function(v) { this.value = v; return this })|js}

  module L : Mina_base.Ledger_intf.S = struct
    module Account = Mina_base.Account
    module Account_id = Mina_base.Account_id
    module Ledger_hash = Mina_base.Ledger_hash
    module Token_id = Mina_base.Token_id

    type t_ =
      { next_location : int
      ; accounts : Account.t Int.Map.t
      ; locations : int Account_id.Map.t
      }

    type t = t_ ref

    type location = int

    let get (t : t) (loc : location) : Account.t option =
      Map.find !t.accounts loc

    let location_of_account (t : t) (a : Account_id.t) : location option =
      Map.find !t.locations a

    let set (t : t) (loc : location) (a : Account.t) : unit =
      t := { !t with accounts = Map.set !t.accounts ~key:loc ~data:a }

    let next_location (t : t) : int =
      let loc = !t.next_location in
      t := { !t with next_location = loc + 1 } ;
      loc

    let get_or_create (t : t) (id : Account_id.t) :
        (Mina_base.Ledger_intf.account_state * Account.t * location) Or_error.t
        =
      let loc = location_of_account t id in
      let res =
        match loc with
        | None ->
            let loc = next_location t in
            let a = Account.create id Currency.Balance.zero in
            t := { !t with locations = Map.set !t.locations ~key:id ~data:loc } ;
            set t loc a ;
            (`Added, a, loc)
        | Some loc ->
            (`Existed, Option.value_exn (get t loc), loc)
      in
      Ok res

    let create_new_account (t : t) (id : Account_id.t) (a : Account.t) :
        unit Or_error.t =
      match location_of_account t id with
      | Some _ ->
          Or_error.errorf !"account %{sexp: Account_id.t} already present" id
      | None ->
          let loc = next_location t in
          t := { !t with locations = Map.set !t.locations ~key:id ~data:loc } ;
          set t loc a ;
          Ok ()

    let remove_accounts_exn (t : t) (ids : Account_id.t list) : unit =
      let locs = List.filter_map ids ~f:(fun id -> Map.find !t.locations id) in
      t :=
        { !t with
          locations = List.fold ids ~init:!t.locations ~f:Map.remove
        ; accounts = List.fold locs ~init:!t.accounts ~f:Map.remove
        }

    (* TODO *)
    let merkle_root (_ : t) : Ledger_hash.t = Field.Constant.zero

    let empty ~depth:_ () : t =
      ref
        { next_location = 0
        ; accounts = Int.Map.empty
        ; locations = Account_id.Map.empty
        }

    let with_ledger (type a) ~depth ~(f : t -> a) : a = f (empty ~depth ())

    let create_masked (t : t) : t = ref !t

    let apply_mask (t : t) ~(masked : t) = t := !masked
  end

  module T = Mina_transaction_logic.Make (L)

  type ledger_class = < value : L.t Js.prop >

  let ledger_constr : (L.t -> ledger_class Js.t) Js.constr =
    Obj.magic ledger_class

  let create_new_account_exn (t : L.t) account_id account =
    L.create_new_account t account_id account |> Or_error.ok_exn

  let token_id_checked (token : Field.t) =
    token |> Mina_base.Token_id.Checked.of_field

  let token_id (token : Impl.field) : Mina_base.Token_id.t =
    token |> Mina_base.Token_id.of_field

  let default_token_id =
    Mina_base.Token_id.default |> Mina_base.Token_id.to_field_unsafe

  let account_id_checked (pk : public_key_checked) token =
    Mina_base.Account_id.Checked.create pk (token_id_checked token)

  let account_id (pk : public_key) token =
    Mina_base.Account_id.create pk (token_id token)

  module Checked = struct
    let fields_to_hash
        (typ : ('var, 'value, Field.Constant.t, _) Impl.Internal_Basic.Typ.typ)
        (digest : 'var -> Field.t) (fields : Field.t array) =
      let (Typ typ) = typ in
      let variable =
        typ.var_of_fields (fields, typ.constraint_system_auxiliary ())
      in
      digest variable
  end

  (* helper function to check whether the fields we produce from JS are correct *)
  let fields_of_json
      (typ : ('var, 'value, Field.Constant.t, 'tmp) Impl.Internal_Basic.Typ.typ)
      of_json (json : Js.js_string Js.t) : Impl.field array =
    let json = json |> Js.to_string |> Yojson.Safe.from_string in
    let value = of_json json in
    let (Typ typ) = typ in
    let fields, _ = typ.value_to_fields value in
    fields

  module To_js = struct
    let option (transform : 'a -> 'b) (x : 'a option) =
      Js.Optdef.option (Option.map x ~f:transform)
  end

  module Account_update = Mina_base.Account_update
  module Zkapp_command = Mina_base.Zkapp_command

  let account_update_of_json, _account_update_to_json =
    let deriver =
      Account_update.Graphql_repr.deriver
      @@ Fields_derivers_zkapps.Derivers.o ()
    in
    let account_update_of_json (account_update : Js.js_string Js.t) :
        Account_update.t =
      Fields_derivers_zkapps.of_json deriver
        (account_update |> Js.to_string |> Yojson.Safe.from_string)
      |> Account_update.of_graphql_repr
    in
    let account_update_to_json (account_update : Account_update.t) :
        Js.js_string Js.t =
      Fields_derivers_zkapps.to_json deriver
        (Account_update.to_graphql_repr account_update ~call_depth:0)
      |> Yojson.Safe.to_string |> Js.string
    in
    (account_update_of_json, account_update_to_json)

  let hash_account_update (p : Js.js_string Js.t) =
    p |> account_update_of_json |> Account_update.digest

  let transaction_commitments (tx_json : Js.js_string Js.t) =
    let tx =
      Zkapp_command.of_json @@ Yojson.Safe.from_string @@ Js.to_string tx_json
    in
    let commitment = Zkapp_command.commitment tx in
    let fee_payer = Account_update.of_fee_payer tx.fee_payer in
    let fee_payer_hash = Zkapp_command.Digest.Account_update.create fee_payer in
    let full_commitment =
      Zkapp_command.Transaction_commitment.create_complete commitment
        ~memo_hash:(Mina_base.Signed_command_memo.hash tx.memo)
        ~fee_payer_hash
    in
    object%js
      val commitment = commitment

      val fullCommitment = full_commitment

      (* for testing *)
      val feePayerHash = (fee_payer_hash :> Impl.field)
    end

  let zkapp_public_input (tx_json : Js.js_string Js.t)
      (account_update_index : int) =
    let tx =
      Zkapp_command.of_json @@ Yojson.Safe.from_string @@ Js.to_string tx_json
    in
    let account_update = List.nth_exn tx.account_updates account_update_index in
    object%js
      val accountUpdate =
        (account_update.elt.account_update_digest :> Impl.field)

      val calls =
        (Zkapp_command.Call_forest.hash account_update.elt.calls :> Impl.field)
    end

  let check_account_update_signatures zkapp_command =
    let ({ fee_payer; account_updates; memo } : Zkapp_command.t) =
      zkapp_command
    in
    let tx_commitment = Zkapp_command.commitment zkapp_command in
    let full_tx_commitment =
      Zkapp_command.Transaction_commitment.create_complete tx_commitment
        ~memo_hash:(Mina_base.Signed_command_memo.hash memo)
        ~fee_payer_hash:
          (Zkapp_command.Digest.Account_update.create
             (Account_update.of_fee_payer fee_payer) )
    in
    let key_to_string = Signature_lib.Public_key.Compressed.to_base58_check in
    let check_signature who s pk msg =
      match Signature_lib.Public_key.decompress pk with
      | None ->
          failwith
            (sprintf "Check signature: Invalid key on %s: %s" who
               (key_to_string pk) )
      | Some pk_ ->
          if
            not
              (Signature_lib.Schnorr.Chunked.verify s
                 (Kimchi_pasta.Pasta.Pallas.of_affine pk_)
                 (Random_oracle_input.Chunked.field msg) )
          then
            failwith
              (sprintf "Check signature: Invalid signature on %s for key %s" who
                 (key_to_string pk) )
          else ()
    in

    check_signature "fee payer" fee_payer.authorization
      fee_payer.body.public_key full_tx_commitment ;
    List.iteri (Zkapp_command.Call_forest.to_account_updates account_updates)
      ~f:(fun i p ->
        let commitment =
          if p.body.use_full_commitment then full_tx_commitment
          else tx_commitment
        in
        match p.authorization with
        | Signature s ->
            check_signature
              (sprintf "account_update %d" i)
              s p.body.public_key commitment
        | Proof _ | None_given ->
            () )

  (* low-level building blocks for encoding *)
  let binary_string_to_base58_check bin_string (version_byte : int) :
      Js.js_string Js.t =
    let module T = struct
      let version_byte = Char.of_int_exn version_byte

      let description = "any"
    end in
    let module B58 = Base58_check.Make (T) in
    bin_string |> B58.encode |> Js.string

  let binary_string_of_base58_check (base58 : Js.js_string Js.t)
      (version_byte : int) =
    let module T = struct
      let version_byte = Char.of_int_exn version_byte

      let description = "any"
    end in
    let module B58 = Base58_check.Make (T) in
    base58 |> Js.to_string |> B58.decode_exn

  let add_account_exn (l : L.t) pk (balance : string) =
    let account_id = account_id pk default_token_id in
    let bal_u64 = Unsigned.UInt64.of_string balance in
    let balance = Currency.Balance.of_uint64 bal_u64 in
    let a : Mina_base.Account.t = Mina_base.Account.create account_id balance in
    create_new_account_exn l account_id a

  let create
      (genesis_accounts :
        < publicKey : public_key Js.prop ; balance : Js.js_string Js.t Js.prop >
        Js.t
        Js.js_array
        Js.t ) : ledger_class Js.t =
    let l = L.empty ~depth:20 () in
    array_iter genesis_accounts ~f:(fun a ->
        add_account_exn l a##.publicKey (Js.to_string a##.balance) ) ;
    new%js ledger_constr l

  let account_to_json =
    let deriver = Mina_base.Account.deriver @@ Fields_derivers_zkapps.o () in
    let to_json' = Fields_derivers_zkapps.to_json deriver in
    let to_json (account : Mina_base.Account.t) : Js.Unsafe.any =
      let str = account |> to_json' |> Yojson.Safe.to_string |> Js.string in
      let json =
        Js.Unsafe.(fun_call global ##. JSON##.parse [| inject str |])
      in
      json
    in
    to_json

  let get_account l (pk : public_key) (token : Impl.field) :
      Js.Unsafe.any Js.optdef =
    let loc = L.location_of_account l##.value (account_id pk token) in
    let account = Option.bind loc ~f:(L.get l##.value) in
    To_js.option account_to_json account

  let add_account l (pk : public_key) (balance : Js.js_string Js.t) =
    add_account_exn l##.value pk (Js.to_string balance)

  let protocol_state_of_json =
    let deriver =
      Mina_base.Zkapp_precondition.Protocol_state.View.deriver
      @@ Fields_derivers_zkapps.o ()
    in
    let of_json = Fields_derivers_zkapps.of_json deriver in
    fun (json : Js.js_string Js.t) :
        Mina_base.Zkapp_precondition.Protocol_state.View.t ->
      json |> Js.to_string |> Yojson.Safe.from_string |> of_json

  let apply_zkapp_command_transaction l (txn : Zkapp_command.t)
      (account_creation_fee : string)
      (network_state : Mina_base.Zkapp_precondition.Protocol_state.View.t) =
    check_account_update_signatures txn ;
    let ledger = l##.value in
    let application_result =
      T.apply_zkapp_command_unchecked
        ~global_slot:network_state.global_slot_since_genesis
        ~state_view:network_state
        ~constraint_constants:
          { Genesis_constants.Constraint_constants.compiled with
            account_creation_fee = Currency.Fee.of_string account_creation_fee
          }
        ledger txn
    in
    let applied, _ =
      match application_result with
      | Ok res ->
          res
      | Error err ->
          raise_error (Error.to_string_hum err)
    in
    let T.Transaction_applied.Zkapp_command_applied.{ command; _ } = applied in
    match command.status with
    | Applied ->
        ()
    | Failed failures ->
        raise_error
          ( Mina_base.Transaction_status.Failure.Collection.to_yojson failures
          |> Yojson.Safe.to_string )

  let apply_json_transaction l (tx_json : Js.js_string Js.t)
      (account_creation_fee : Js.js_string Js.t)
      (network_json : Js.js_string Js.t) =
    let txn =
      Zkapp_command.of_json @@ Yojson.Safe.from_string @@ Js.to_string tx_json
    in
    let network_state = protocol_state_of_json network_json in
    apply_zkapp_command_transaction l txn
      (Js.to_string account_creation_fee)
      network_state

  let check_account_update_signature (account_update_json : Js.js_string Js.t)
      (x : Impl.field) =
    let account_update = account_update_of_json account_update_json in
    let check_signature s pk msg =
      match Signature_lib.Public_key.decompress pk with
      | None ->
          false
      | Some pk_ ->
          Signature_lib.Schnorr.Chunked.verify s
            (Kimchi_pasta.Pasta.Pallas.of_affine pk_)
            (Random_oracle_input.Chunked.field msg)
    in

    let isValid =
      match account_update.authorization with
      | Signature s ->
          check_signature s account_update.body.public_key x
      | Proof _ | None_given ->
          false
    in
    Js.bool isValid

  let create_token_account pk token =
    account_id pk token |> Mina_base.Account_id.public_key
    |> Signature_lib.Public_key.Compressed.to_string |> Js.string

  let custom_token_id_checked pk token =
    Mina_base.Account_id.Checked.derive_token_id
      ~owner:(account_id_checked pk token)
    |> Mina_base.Account_id.Digest.Checked.to_field_unsafe

  let custom_token_id_unchecked pk token =
    Mina_base.Account_id.derive_token_id ~owner:(account_id pk token)
    |> Mina_base.Token_id.to_field_unsafe

  type random_oracle_input = Impl.field Random_oracle_input.Chunked.t

  let pack_input (input : random_oracle_input) : Impl.field array =
    Random_oracle.pack_input input

  (* global *)

  let () =
    let static name thing = Js.Unsafe.set ledger_class (Js.string name) thing in
    let static_method name f =
      Js.Unsafe.set ledger_class (Js.string name) (Js.wrap_callback f)
    in
    let method_ name (f : ledger_class Js.t -> _) =
      method_ ledger_class name f
    in
    static_method "customTokenId" custom_token_id_unchecked ;
    static_method "customTokenIdChecked" custom_token_id_checked ;
    static_method "createTokenAccount" create_token_account ;
    static_method "create" create ;

    static_method "transactionCommitments" transaction_commitments ;
    static_method "zkappPublicInput" zkapp_public_input ;

    (* these are implemented in JS, but kept here for consistency tests *)
    static_method "checkAccountUpdateSignature" check_account_update_signature ;

    let version_bytes =
      let open Base58_check.Version_bytes in
      object%js
        val tokenIdKey = Char.to_int token_id_key

        val receiptChainHash = Char.to_int receipt_chain_hash

        val ledgerHash = Char.to_int ledger_hash

        val epochSeed = Char.to_int epoch_seed

        val stateHash = Char.to_int state_hash

        val publicKey = Char.to_int non_zero_curve_point_compressed

        val userCommandMemo = Char.to_int user_command_memo
      end
    in
    static "encoding"
      (object%js
         val toBase58 = binary_string_to_base58_check

         val ofBase58 = binary_string_of_base58_check

         val versionBytes = version_bytes
      end ) ;

    static_method "hashAccountUpdateFromJson" hash_account_update ;

    let hash_account_update_from_fields =
      Checked.fields_to_hash
        (Mina_base.Account_update.Body.typ ())
        Mina_base.Account_update.Checked.digest
    in
    static_method "hashAccountUpdateFromFields" hash_account_update_from_fields ;

    (* TODO this is for debugging, maybe remove later *)
    let body_deriver =
      Mina_base.Account_update.Body.Graphql_repr.deriver
      @@ Fields_derivers_zkapps.o ()
    in
    let body_of_json json =
      json
      |> Fields_derivers_zkapps.of_json body_deriver
      |> Account_update.Body.of_graphql_repr
    in
    static_method "fieldsOfJson"
      (fields_of_json (Mina_base.Account_update.Body.typ ()) body_of_json) ;

    (* hash inputs for various account_update subtypes *)
    (* TODO: this is for testing against JS impl, remove eventually *)
    let timing_input (json : Js.js_string Js.t) : random_oracle_input =
      let deriver = Account_update.Update.Timing_info.deriver in
      let json = json |> Js.to_string |> Yojson.Safe.from_string in
      let value = Fields_derivers_zkapps.(of_json (deriver @@ o ()) json) in
      let input = Account_update.Update.Timing_info.to_input value in
      input
    in
    let permissions_input (json : Js.js_string Js.t) : random_oracle_input =
      let deriver = Mina_base.Permissions.deriver in
      let json = json |> Js.to_string |> Yojson.Safe.from_string in
      let value = Fields_derivers_zkapps.(of_json (deriver @@ o ()) json) in
      let input = Mina_base.Permissions.to_input value in
      input
    in
    let update_input (json : Js.js_string Js.t) : random_oracle_input =
      let deriver = Account_update.Update.deriver in
      let json = json |> Js.to_string |> Yojson.Safe.from_string in
      let value = Fields_derivers_zkapps.(of_json (deriver @@ o ()) json) in
      let input = Account_update.Update.to_input value in
      input
    in
    let account_precondition_input (json : Js.js_string Js.t) :
        random_oracle_input =
      let deriver = Mina_base.Zkapp_precondition.Account.deriver in
      let json = json |> Js.to_string |> Yojson.Safe.from_string in
      let value = Fields_derivers_zkapps.(of_json (deriver @@ o ()) json) in
      let input = Mina_base.Zkapp_precondition.Account.to_input value in
      input
    in
    let network_precondition_input (json : Js.js_string Js.t) :
        random_oracle_input =
      let deriver = Mina_base.Zkapp_precondition.Protocol_state.deriver in
      let json = json |> Js.to_string |> Yojson.Safe.from_string in
      let value = Fields_derivers_zkapps.(of_json (deriver @@ o ()) json) in
      let input = Mina_base.Zkapp_precondition.Protocol_state.to_input value in
      input
    in
    let body_input (json : Js.js_string Js.t) : random_oracle_input =
      let json = json |> Js.to_string |> Yojson.Safe.from_string in
      let value = body_of_json json in
      let input = Account_update.Body.to_input value in
      input
    in

    static "hashInputFromJson"
      (object%js
         val packInput = pack_input

         val timing = timing_input

         val permissions = permissions_input

         val accountPrecondition = account_precondition_input

         val networkPrecondition = network_precondition_input

         val update = update_input

         val body = body_input
      end ) ;

    method_ "getAccount" get_account ;
    method_ "addAccount" add_account ;
    method_ "applyJsonTransaction" apply_json_transaction
end

module Test = struct
  module Encoding = struct
    let public_key_to_base58 (pk : public_key) : Js.js_string Js.t =
      pk |> Signature_lib.Public_key.Compressed.to_base58_check |> Js.string

    let public_key_of_base58 (pk_base58 : Js.js_string Js.t) : public_key =
      pk_base58 |> Js.to_string
      |> Signature_lib.Public_key.Compressed.of_base58_check_exn

    let private_key_to_base58 (sk : Other_impl.field) : Js.js_string Js.t =
      sk |> Signature_lib.Private_key.to_base58_check |> Js.string

    let private_key_of_base58 (sk_base58 : Js.js_string Js.t) : Other_impl.field
        =
      sk_base58 |> Js.to_string |> Signature_lib.Private_key.of_base58_check_exn

    let token_id_to_base58 (field : Impl.field) : Js.js_string Js.t =
      field |> Mina_base.Account_id.Digest.of_field
      |> Mina_base.Account_id.Digest.to_string |> Js.string

    let token_id_of_base58 (field : Js.js_string Js.t) : Impl.field =
      Mina_base.Account_id.Digest.to_field_unsafe
      @@ Mina_base.Account_id.Digest.of_string @@ Js.to_string field

    let memo_to_base58 (memo : Js.js_string Js.t) : Js.js_string Js.t =
      Js.string @@ Mina_base.Signed_command_memo.to_base58_check
      @@ Mina_base.Signed_command_memo.create_from_string_exn
      @@ Js.to_string memo

    let memo_hash_base58 (memo_base58 : Js.js_string Js.t) : Impl.field =
      memo_base58 |> Js.to_string
      |> Mina_base.Signed_command_memo.of_base58_check_exn
      |> Mina_base.Signed_command_memo.hash
  end

  module Signature = struct
    let sign_field_element (x : Impl.field) (key : Other_impl.field)
        (is_mainnet : bool Js.t) =
      let network_id =
        Mina_signature_kind.(if Js.to_bool is_mainnet then Mainnet else Testnet)
      in
      Signature_lib.Schnorr.Chunked.sign ~signature_kind:network_id key
        (Random_oracle.Input.Chunked.field x)
      |> Mina_base.Signature.to_base58_check |> Js.string

    let dummy_signature () =
      Mina_base.Signature.(dummy |> to_base58_check) |> Js.string
  end
end

let test =
  let module Signed_command = Mina_base.Signed_command in
  let module Signed_command_payload = Mina_base.Signed_command_payload in
  let ok_exn result =
    let open Ppx_deriving_yojson_runtime.Result in
    match result with Ok c -> c | Error e -> failwith ("not ok: " ^ e)
  in

  let keypair () = Signature_lib.Keypair.create () in
  object%js
    val encoding =
      object%js
        method publicKeyToBase58 = Test.Encoding.public_key_to_base58

        method publicKeyOfBase58 = Test.Encoding.public_key_of_base58

        method privateKeyToBase58 = Test.Encoding.private_key_to_base58

        method privateKeyOfBase58 = Test.Encoding.private_key_of_base58

        method tokenIdToBase58 = Test.Encoding.token_id_to_base58

        method tokenIdOfBase58 = Test.Encoding.token_id_of_base58

        method memoToBase58 = Test.Encoding.memo_to_base58

        method memoHashBase58 = Test.Encoding.memo_hash_base58
      end

    val signature =
      object%js
        method signFieldElement = Test.Signature.sign_field_element

        val dummySignature = Test.Signature.dummy_signature
      end

    val transactionHash =
      object%js
        method hashPayment (command : Js.js_string Js.t) =
          let command : Signed_command.t =
            command |> Js.to_string |> Yojson.Safe.from_string
            |> Signed_command.of_yojson |> ok_exn
          in
          Mina_transaction.Transaction_hash.(
            command |> hash_signed_command |> to_base58_check |> Js.string)

        method hashPaymentV1 (command : Js.js_string Js.t) =
          let command : Signed_command.t_v1 =
            command |> Js.to_string |> Yojson.Safe.from_string
            |> Signed_command.Stable.V1.of_yojson |> ok_exn
          in
          let b58 = Signed_command.to_base58_check_v1 command in
          Mina_transaction.Transaction_hash.(
            b58 |> digest_string |> to_base58_check)
          |> Js.string

        method serializeCommon (command : Js.js_string Js.t) =
          let command : Signed_command_payload.Common.t =
            command |> Js.to_string |> Yojson.Safe.from_string
            |> Signed_command_payload.Common.of_yojson |> ok_exn
          in
          Binable.to_bigstring
            (module Signed_command_payload.Common.Stable.Latest)
            command

        method serializePayment (command : Js.js_string Js.t) =
          let command : Signed_command.t =
            command |> Js.to_string |> Yojson.Safe.from_string
            |> Signed_command.of_yojson |> ok_exn
          in
          Binable.to_bigstring (module Signed_command.Stable.Latest) command

        method serializePaymentV1 (command : Js.js_string Js.t) =
          let command : Signed_command.t_v1 =
            command |> Js.to_string |> Yojson.Safe.from_string
            |> Signed_command.Stable.V1.of_yojson |> ok_exn
          in
          Signed_command.to_base58_check_v1 command |> Js.string

        method examplePayment =
          let kp = keypair () in
          let payload : Signed_command_payload.t =
            { Signed_command_payload.dummy with
              body =
                Payment
                  { Mina_base.Payment_payload.dummy with
                    source_pk = Signature_lib.Public_key.compress kp.public_key
                  }
            }
          in
          let payment = Signed_command.sign kp payload in
          (payment :> Signed_command.t)
          |> Signed_command.to_yojson |> Yojson.Safe.to_string |> Js.string
      end
  end

(* export stuff *)

let export () =
  Js.export "Field" field_class ;
  Js.export "Bool" bool_class ;
  Js.export "Poseidon" poseidon ;
  Js.export "Snarky" snarky ;
  Js.export "Ledger" Ledger.ledger_class ;
  Js.export "Pickles" pickles ;
  Js.export "Test" test

let export_global () =
  let snarky_obj =
    Js.Unsafe.(
      let i = inject in
      obj
        [| ("Field", i field_class)
         ; ("Bool", i bool_class)
         ; ("Poseidon", i poseidon)
         ; ("Snarky", i snarky)
         ; ("Ledger", i Ledger.ledger_class)
         ; ("Pickles", i pickles)
         ; ("Test", i test)
        |])
  in
  Js.Unsafe.(set global (Js.string "__snarky") snarky_obj)
