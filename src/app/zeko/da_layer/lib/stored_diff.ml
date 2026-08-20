open Core_kernel

[%%versioned
module Stable = struct
  [@@@with_top_version_tag]

  [@@@no_toplevel_latest_type]

  module V1 = struct
    type t =
      { source_state : Da_state.Stable.V1.t
      ; target_state : Da_state.Stable.V1.t
      ; diff : Diff.Stable.V4.t
      }
    [@@deriving compare, sexp_of]

    let to_latest = Fn.id
  end
end]

type t = Stable.V1.t =
  { source_state : Da_state.t
  ; target_state : Da_state.t
  ; diff : Diff.Stable.V4.t
  }
[@@deriving compare, sexp_of]

let equal left right = Int.equal (compare left right) 0

let same_payload left right =
  Da_state.equal left.source_state right.source_state
  && Da_state.equal left.target_state right.target_state
  &&
  let right_diff =
    { right.diff with Diff.Stable.V4.timestamp = left.diff.timestamp }
  in
  Int.equal (Diff.Stable.V4.compare left.diff right_diff) 0

let to_bigstring = Binable.to_bigstring (module Stable.V1.With_top_version_tag)

let of_bigstring = Binable.of_bigstring (module Stable.V1.With_top_version_tag)
