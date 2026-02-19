(* peer.ml -- peer with libp2p port and peer id *)

open Core_kernel

(** A libp2p PeerID is more or less a hash of a public key. *)
module Id = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Bounded_types.String.Stable.V1.t
      [@@deriving compare, hash, equal, sexp]

      let to_latest = Fn.id
    end
  end]

  (** Convert to the libp2p-defined base58 string *)
  let to_string (x : t) = x

  (** Create a Peer ID from a string, without checking if it is well-formed. *)
  let unsafe_of_string (s : string) : t = s
end

module Inet_addr = struct
  [%%versioned_binable
  module Stable = struct
    module V1 = struct
      type t = Caml_unix.inet_addr

      let to_latest = Fn.id

      let compare t1 t2 =
        String.compare
          (Caml_unix.string_of_inet_addr t1)
          (Caml_unix.string_of_inet_addr t2)

      let hash t = Hashtbl.hash (Caml_unix.string_of_inet_addr t)

      let t_of_sexp = function
        | Sexp.Atom s ->
            Caml_unix.inet_addr_of_string s
        | _ ->
            failwith "Network_peer.Peer.Inet_addr.t_of_sexp: expected atom"

      let sexp_of_t t = Sexp.Atom (Caml_unix.string_of_inet_addr t)

      let of_yojson = function
        | `String s ->
            Ok (Caml_unix.inet_addr_of_string s)
        | _ ->
            Error "expected string"

      let to_yojson ip_addr = `String (Caml_unix.string_of_inet_addr ip_addr)

      include Bounded_types.String.Of_stringable (struct
        type nonrec t = t

        let to_string = Caml_unix.string_of_inet_addr

        let of_string = Caml_unix.inet_addr_of_string
      end)
    end
  end]

  [%%define_locally Stable.V1.(to_yojson, of_yojson)]
  [%%define_locally Stable.V1.(compare)]
end

[%%versioned
module Stable = struct
  [@@@no_toplevel_latest_type]

  module V1 = struct
    type t =
      { host : Inet_addr.Stable.V1.t (* IPv4 or IPv6 address *)
      ; libp2p_port : int (* TCP *)
      ; peer_id : Id.Stable.V1.t
      }
    [@@deriving compare, sexp]

    let to_latest = Fn.id

    let equal t t' = compare t t' = 0

    (* these hash functions come from the implementation of Inet_addr,
         though they're not exposed *)
    let hash_fold_t hash t = hash_fold_int hash (Hashtbl.hash t)

    let hash : t -> int = Ppx_hash_lib.Std.Hash.of_fold hash_fold_t

    let to_yojson { host; peer_id; libp2p_port } =
      `Assoc
        [ ("host", `String (Caml_unix.string_of_inet_addr host))
        ; ("peer_id", `String peer_id)
        ; ("libp2p_port", `Int libp2p_port)
        ]

    let of_yojson =
      let lift_string = function `String s -> Some s | _ -> None in
      let lift_int = function `Int n -> Some n | _ -> None in
      function
      | `Assoc ls ->
          let open Option.Let_syntax in
          Result.of_option ~error:"missing keys"
            (let%bind host_str =
               List.Assoc.find ls "host" ~equal:String.equal >>= lift_string
             in
             let%bind peer_id =
               List.Assoc.find ls "peer_id" ~equal:String.equal >>= lift_string
             in
             let%map libp2p_port =
               List.Assoc.find ls "libp2p_port" ~equal:String.equal >>= lift_int
             in
             let host = Caml_unix.inet_addr_of_string host_str in
             { host; peer_id; libp2p_port } )
      | _ ->
          Error "expected object"
  end
end]

type t = Stable.Latest.t =
  { host : Caml_unix.inet_addr; libp2p_port : int; peer_id : string }

[%%define_locally Stable.Latest.(of_yojson, to_yojson)]

let sexp_of_t = Stable.Latest.sexp_of_t

let t_of_sexp = Stable.Latest.t_of_sexp

include Hashable.Make (Stable.Latest)
include Comparable.Make_binable (Stable.Latest)

let create host ~libp2p_port ~peer_id = { host; libp2p_port; peer_id }

let to_discovery_host_and_port t =
  Host_and_port.create
    ~host:(Caml_unix.string_of_inet_addr t.host)
    ~port:t.libp2p_port

let to_string { host; libp2p_port; peer_id } =
  sprintf
    !"[host : %s, libp2p_port : %s, peer_id : %s]"
    (Caml_unix.string_of_inet_addr host)
    (Int.to_string libp2p_port)
    peer_id

let to_multiaddr_string { host; libp2p_port; peer_id } =
  sprintf "/ip4/%s/tcp/%d/p2p/%s"
    (Caml_unix.string_of_inet_addr host)
    libp2p_port peer_id

let pretty_list peers = String.concat ~sep:"," @@ List.map peers ~f:to_string

module Event = struct
  type t =
    | Connect of Stable.Latest.t list
    | Disconnect of Stable.Latest.t list
  [@@deriving sexp]
end

module Display = struct
  [%%versioned
  module Stable = struct
    [@@@no_toplevel_latest_type]

    module V1 = struct
      type t =
        { host : Bounded_types.String.Stable.V1.t
        ; libp2p_port : int
        ; peer_id : Bounded_types.String.Stable.V1.t
        }
      [@@deriving yojson, version, sexp, fields]

      let to_latest = Fn.id
    end
  end]

  type t = Stable.Latest.t =
    { host : string; libp2p_port : int; peer_id : string }
  [@@deriving yojson, sexp]

  module Fields = Stable.Latest.Fields
end

let ip { host; _ } = host

let to_display { host; libp2p_port; peer_id } =
  Display.
    { host = Caml_unix.string_of_inet_addr host
    ; libp2p_port
    ; peer_id = Id.to_string peer_id
    }

let of_display { Display.host; libp2p_port; peer_id } =
  { host = Caml_unix.inet_addr_of_string host
  ; libp2p_port
  ; peer_id = Id.unsafe_of_string peer_id
  }
