open Core_kernel
open Snark_params.Tick
open Mina_base
module PC = Signature_lib.Public_key.Compressed
open Zeko_util

include Indexed_merkle_tree.Make (struct
  open struct
    let add_plonk_constraint c =
      assert_
        { basic =
            Kimchi_backend_common.Plonk_constraint_system.Plonk_constraint.T c
        ; annotation = None
        }

    let ( let- ) var f =
      let+ var = As_prover.read_var var in
      f var

    module Range_check0 = struct
      type t = Zeko_as_prover.range_check0 =
        { v0p0 : F.t (* MSBs *)
        ; v0p1 : F.t (* vpX are 12-bit plookup chunks *)
        ; v0p2 : F.t
        ; v0p3 : F.t
        ; v0p4 : F.t
        ; v0p5 : F.t
        ; v0c0 : F.t (* vcX are 2-bit crumbs *)
        ; v0c1 : F.t
        ; v0c2 : F.t
        ; v0c3 : F.t
        ; v0c4 : F.t
        ; v0c5 : F.t
        ; v0c6 : F.t
        ; v0c7 : F.t (* LSBs *)
        }
      [@@deriving snarky]
    end

    let range_check0 v0 =
      let* ({ v0p0
            ; v0p1
            ; v0p2
            ; v0p3
            ; v0p4
            ; v0p5
            ; v0c0
            ; v0c1
            ; v0c2
            ; v0c3
            ; v0c4
            ; v0c5
            ; v0c6
            ; v0c7
            } :
             Range_check0.var ) =
        exists Range_check0.typ
          ~compute:
            (let- v0 in
             Zeko_as_prover.range_check0 v0 |> As_prover.return )
      in
      let*| () =
        add_plonk_constraint
          (RangeCheck0
             { v0
             ; v0p0
             ; v0p1
             ; v0p2
             ; v0p3
             ; v0p4
             ; v0p5
             ; v0c0
             ; v0c1
             ; v0c2
             ; v0c3
             ; v0c4
             ; v0c5
             ; v0c6
             ; v0c7
             ; compact = Field.zero
             } )
      in
      (v0p4, v0p5)

    module Range_check1 = struct
      type t = Zeko_as_prover.range_check1 =
        { (* Current row *)
          v2c0 : F.t (* MSBs, 2-bit crumb *)
        ; v2p0 : F.t (* vpX are 12-bit plookup chunks *)
        ; v2p1 : F.t
        ; v2p2 : F.t
        ; v2p3 : F.t
        ; v2c1 : F.t (* vcX are 2-bit crumbs *)
        ; v2c2 : F.t
        ; v2c3 : F.t
        ; v2c4 : F.t
        ; v2c5 : F.t
        ; v2c6 : F.t
        ; v2c7 : F.t
        ; v2c8 : F.t (* LSBs *)
        ; (* Next row *) v2c9 : F.t
        ; v2c10 : F.t
        ; v2c11 : F.t
        ; v2c12 : F.t
        ; v2c13 : F.t
        ; v2c14 : F.t
        ; v2c15 : F.t
        ; v2c16 : F.t
        ; v2c17 : F.t
        ; v2c18 : F.t
        ; v2c19 : F.t
        }
      [@@deriving snarky]
    end

    let range_check1 ~v2 ~v0p0 ~v0p1 ~v1p0 ~v1p1 =
      let* { v2c0
           ; v2p0
           ; v2p1
           ; v2p2
           ; v2p3
           ; v2c1
           ; v2c2
           ; v2c3
           ; v2c4
           ; v2c5
           ; v2c6
           ; v2c7
           ; v2c8
           ; v2c9
           ; v2c10
           ; v2c11
           ; v2c12
           ; v2c13
           ; v2c14
           ; v2c15
           ; v2c16
           ; v2c17
           ; v2c18
           ; v2c19
           } =
        exists Range_check1.typ
          ~compute:
            (let- v2 in
             Zeko_as_prover.range_check1 v2 |> As_prover.return )
      in
      add_plonk_constraint
        (RangeCheck1
           { v2
           ; v12 = Field.Var.constant Field.zero
           ; v2c0
           ; v2p0
           ; v2p1
           ; v2p2
           ; v2p3
           ; v2c1
           ; v2c2
           ; v2c3
           ; v2c4
           ; v2c5
           ; v2c6
           ; v2c7
           ; v2c8
           ; v2c9
           ; v2c10
           ; v2c11
           ; v0p0
           ; v0p1
           ; v1p0
           ; v1p1
           ; v2c12
           ; v2c13
           ; v2c14
           ; v2c15
           ; v2c16
           ; v2c17
           ; v2c18
           ; v2c19
           } )

    let multi_range_check x y z =
      let* v0p0, v0p1 = range_check0 x in
      let* v1p0, v1p1 = range_check0 y in
      range_check1 ~v2:z ~v0p0 ~v0p1 ~v1p0 ~v1p1

    let sub_then_dec ~dec ~x0 ~x1 ~x2 ~y0 ~y1 ~y2 =
      let* (z0, z1), z2 =
        exists
          Typ.(F.typ * F.typ * F.typ)
          ~compute:
            (let- x0 in
             let- x1 in
             let- x2 in
             let- y0 in
             let- y1 in
             let- y2 in
             Zeko_as_prover.sub ~x0 ~x1 ~x2 ~y0 ~y1 ~y2 |> As_prover.return )
      in
      let* (w0, w1), w2 =
        exists
          Typ.(F.typ * F.typ * F.typ)
          ~compute:
            (let- z0 in
             let- z1 in
             let- z2 in
             Zeko_as_prover.sub ~x0:z0 ~x1:z1 ~x2:z2 ~y0:Field.one
               ~y1:Field.zero ~y2:Field.zero
             |> As_prover.return )
      in
      let* () =
        add_plonk_constraint
          (ForeignFieldAdd
             { left_input_lo = x0
             ; left_input_mi = x1
             ; left_input_hi = x2
             ; right_input_lo = y0
             ; right_input_mi = y1
             ; right_input_hi = y2
             ; sign = Field.of_int (-1)
             ; carry = Field.(constant typ zero)
             ; field_overflow = Field.(constant typ zero)
             ; foreign_field_modulus0 = Field.zero
             ; foreign_field_modulus1 = Field.zero
             ; foreign_field_modulus2 = Field.zero
             } )
      in
      let* () =
        add_plonk_constraint
          (ForeignFieldAdd
             { left_input_lo = z0
             ; left_input_mi = z1
             ; left_input_hi = z2
             ; right_input_lo = dec
             ; right_input_mi = Field.(constant typ zero)
             ; right_input_hi = Field.(constant typ zero)
             ; sign = Field.of_int (-1)
             ; carry = Field.(constant typ zero)
             ; field_overflow = Field.(constant typ zero)
             ; foreign_field_modulus0 = Field.zero
             ; foreign_field_modulus1 = Field.zero
             ; foreign_field_modulus2 = Field.zero
             } )
      in
      let* () =
        add_plonk_constraint
          (Raw { kind = Zero; values = [| w0; w1; w2 |]; coeffs = [||] })
      in
      multi_range_check z0 z1 z2

    let l =
      Bigint.of_bignum_bigint Bignum_bigint.(of_int 1 |> Fn.flip shift_left 88)
      |> Bigint.to_field

    let l2 = Field.(l * l)

    let field_to_field3 x =
      let* (x0, x1), x2 =
        exists
          Typ.(F.typ * F.typ * F.typ)
          ~compute:
            (let- x in
             Zeko_as_prover.field_to_field3 x |> As_prover.return )
      in
      let* () = multi_range_check x0 x1 x2 in
      let x' = Field.Checked.(x0 + (l * x1) + (l2 * x2)) in
      let*| () = Field.Checked.Assert.equal x' x in
      (x0, x1, x2)

    let assert_greater_than_full ~check x y =
      (* if check is false, use x on both sides *)
      let* y = if_ check ~typ:F.typ ~then_:y ~else_:x in
      let* x0, x1, x2 = field_to_field3 x in
      let* y0, y1, y2 = field_to_field3 y in
      let dec =
        let (Typ typ) = Boolean.typ in
        match typ.var_to_fields check with
        | [| dec |], _ ->
            dec
        | _ ->
            failwith "unreachable"
      in
      (* if check (dec) is false, then we decrement with 0, and expand to greater than or equality check *)
      let* () = sub_then_dec ~dec ~x0 ~x1 ~x2 ~y0 ~y1 ~y2 in
      assert (
        Bignum_bigint.(
          Field.size
          = of_string
              "28948022309329048855892746252171976963363056481941560715954676764349967630337") ) ;
      let fp0 = Field.(of_string "93054740644568405314109441") in
      let fp1 = Field.(of_string "147213319177") in
      let fp2 = Field.(of_string "302231454903657293676544") in
      (let c f = Bigint.of_field f |> Bigint.to_bignum_bigint in
       assert (
         Bignum_bigint.(c fp0 + (c fp1 * c l) + (c fp2 * c l2) = Field.size) )
      ) ;
      assert (Field.(fp0 + (fp1 * l) + (fp2 * l2) |> equal (of_int 0))) ;
      let* () =
        sub_then_dec
          ~dec:Field.(constant typ one)
          ~x0:(constant Field.typ fp0) ~x1:(constant Field.typ fp1)
          ~x2:(constant Field.typ fp2) ~y0 ~y1 ~y2
      in
      Checked.return ()
  end

  module Key = struct
    type t = Token_id.t

    type var = Token_id.Checked.t

    let typ = Token_id.typ
  end

  let assert_x_less_than_y_less_than_z ~(x : Key.var) ~(y : Key.var)
      ~(z : Key.var) =
    (* pretty sure of_field is supposed to be of_field_unsafe, and to_field_unsafe is supposed to be to_field *)
    let x = Token_id.Checked.to_field_unsafe x in
    let y = Token_id.Checked.to_field_unsafe y in
    let z = Token_id.Checked.to_field_unsafe z in
    let* () = assert_greater_than_full ~check:Boolean.true_ z y in
    let*| () = assert_greater_than_full ~check:Boolean.true_ y x in
    ()

  let height = 32
end)
