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

let range_check0 _ = failwith "FIXME"

let range_check1 _ = failwith "FIXME"

let sub ~x0:_ ~x1:_ ~x2:_ ~y0:_ ~y1:_ ~y2:_ = failwith "FIXME"

let field_to_field3 _ = failwith "FIXME"
