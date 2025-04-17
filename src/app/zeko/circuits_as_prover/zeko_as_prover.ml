open Core_kernel
open Snark_params.Tick

type range_check0 =
  { v0p0 : field (* MSBs *)
  ; v0p1 : field (* vpX are 12-bit plookup chunks *)
  ; v0p2 : field
  ; v0p3 : field
  ; v0p4 : field
  ; v0p5 : field
  ; v0c0 : field (* vcX are 2-bit crumbs *)
  ; v0c1 : field
  ; v0c2 : field
  ; v0c3 : field
  ; v0c4 : field
  ; v0c5 : field
  ; v0c6 : field
  ; v0c7 : field (* LSBs *)
  }

type range_check1 =
  { (* Current row *)
    v2c0 : field (* MSBs, 2-bit crumb *)
  ; v2p0 : field (* vpX are 12-bit plookup chunks *)
  ; v2p1 : field
  ; v2p2 : field
  ; v2p3 : field
  ; v2c1 : field (* vcX are 2-bit crumbs *)
  ; v2c2 : field
  ; v2c3 : field
  ; v2c4 : field
  ; v2c5 : field
  ; v2c6 : field
  ; v2c7 : field
  ; v2c8 : field (* LSBs *)
  ; (* Next row *) v2c9 : field
  ; v2c10 : field
  ; v2c11 : field
  ; v2c12 : field
  ; v2c13 : field
  ; v2c14 : field
  ; v2c15 : field
  ; v2c16 : field
  ; v2c17 : field
  ; v2c18 : field
  ; v2c19 : field
  }

open struct
  let to_ x = Bigint.(of_field x |> to_bignum_bigint)

  let of_ x = Bigint.(of_bignum_bigint x |> to_field)

  let slice_bigint (f : Bignum_bigint.t) (first_bit : int) (n_bits : int) =
    let open Bignum_bigint in
    assert (is_non_negative f) ;
    shift_right f first_bit |> bit_and (shift_left (of_int 1) n_bits - one)

  let slice (f : field) (first_bit : int) (n_bits : int) =
    slice_bigint (to_ f) first_bit n_bits |> of_
end

let range_check0 f =
  { v0p0 = slice f 76 12
  ; v0p1 = slice f 64 12
  ; v0p2 = slice f 52 12
  ; v0p3 = slice f 40 12
  ; v0p4 = slice f 28 12
  ; v0p5 = slice f 16 12
  ; v0c0 = slice f 14 2
  ; v0c1 = slice f 12 2
  ; v0c2 = slice f 10 2
  ; v0c3 = slice f 8 2
  ; v0c4 = slice f 6 2
  ; v0c5 = slice f 4 2
  ; v0c6 = slice f 2 2
  ; v0c7 = slice f 0 2
  }

let range_check1 f =
  { v2c0 = slice f 86 2
  ; v2p0 = slice f 74 12
  ; v2p1 = slice f 62 12
  ; v2p2 = slice f 50 12
  ; v2p3 = slice f 38 12
  ; v2c1 = slice f 36 2
  ; v2c2 = slice f 34 2
  ; v2c3 = slice f 32 2
  ; v2c4 = slice f 30 2
  ; v2c5 = slice f 28 2
  ; v2c6 = slice f 26 2
  ; v2c7 = slice f 24 2
  ; v2c8 = slice f 22 2
  ; v2c9 = slice f 20 2
  ; v2c10 = slice f 18 2
  ; v2c11 = slice f 16 2
  ; v2c12 = slice f 14 2
  ; v2c13 = slice f 12 2
  ; v2c14 = slice f 10 2
  ; v2c15 = slice f 8 2
  ; v2c16 = slice f 6 2
  ; v2c17 = slice f 4 2
  ; v2c18 = slice f 2 2
  ; v2c19 = slice f 0 2
  }

let sub ~x0 ~x1 ~x2 ~y0 ~y1 ~y2 =
  let ( let- ) x f = f (to_ x) in
  let- x0 in
  let- x1 in
  let- x2 in
  let- y0 in
  let- y1 in
  let- y2 in
  let open Bignum_bigint in
  let l = of_int 1 |> Fn.flip shift_left 88 in
  let l2 = of_int 1 |> Fn.flip shift_left 176 in
  let x = x0 + (x1 * l) + (x2 * l2) in
  let y = y0 + (y1 * l) + (y2 * l2) in
  let r = x - y in
  ( (slice_bigint r 0 88 |> of_, slice_bigint r 88 88 |> of_)
  , slice_bigint r 176 88 |> of_ )

let field_to_field3 f = ((slice f 0 88, slice f 88 88), slice f 176 88)

let carry ~x0 ~x1 ~y0 ~y1 =
  let ( let- ) x f = f (to_ x) in
  let- x0 in
  let- x1 in
  let- y0 in
  let- y1 in
  let open Bignum_bigint in
  let l = of_int 1 |> Fn.flip shift_left 88 in
  let x = x0 + (x1 * l) in
  let y = y0 + (y1 * l) in
  let r = x - y in
  if r < zero then Field.(negate one) else Field.zero
