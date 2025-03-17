open Core_kernel
open Async
open Mina_ledger
open Mina_base
open Signature_lib

(* val post_diff : ledger_openings:Sparse_ledger.t -> diff:Diff.t -> Signature.t *)
module Post_diff = struct
  module V1 = struct
    module Query = struct
      (* Use Diff.V1 without timestamp, the node determines the timestamp itself *)
      type t =
        { ledger_openings : Sparse_ledger.Stable.V2.t; diff : Diff.Stable.V1.t }
      [@@deriving bin_io_unversioned]
    end

    module Response = struct
      type t = Public_key.Compressed.Stable.V1.t * Signature.Stable.V1.t
      [@@deriving bin_io_unversioned]
    end

    let t : (Query.t, Response.t) Rpc.Rpc.t =
      Rpc.Rpc.create ~name:"Post_diff" ~version:1 ~bin_query:Query.bin_t
        ~bin_response:Response.bin_t
  end
end

(* val get_diff : Ledger_hash.t -> Diff.t option *)
module Get_diff = struct
  module V1 = struct
    module Response = struct
      type t = Diff.Stable.V1.t option [@@deriving bin_io_unversioned]
    end

    let t : (Ledger_hash.t, Response.t) Rpc.Rpc.t =
      Rpc.Rpc.create ~name:"Get_diff" ~version:1
        ~bin_query:Ledger_hash.Stable.V1.bin_t ~bin_response:Response.bin_t
  end

  module V2 = struct
    module Response = struct
      type t = Diff.Stable.V2.t option [@@deriving bin_io_unversioned]
    end

    let t : (Ledger_hash.t, Response.t) Rpc.Rpc.t =
      Rpc.Rpc.create ~name:"Get_diff" ~version:2
        ~bin_query:Ledger_hash.Stable.V1.bin_t ~bin_response:Response.bin_t
  end
end

(* val get_all_keys : unit -> Ledger_hash.t list *)
module Get_all_keys = struct
  module V1 = struct
    module Response = struct
      type t = Ledger_hash.Stable.V1.t list [@@deriving bin_io_unversioned]
    end

    let t : (unit, Response.t) Rpc.Rpc.t =
      Rpc.Rpc.create ~name:"Get_all_keys" ~version:1 ~bin_query:Unit.bin_t
        ~bin_response:Response.bin_t
  end
end

(* val get_diff_source : Ledger_hash.t -> Ledger_hash.t *)
module Get_diff_source = struct
  module V1 = struct
    let t : (Ledger_hash.t, Ledger_hash.t) Rpc.Rpc.t =
      Rpc.Rpc.create ~name:"Get_diff_source" ~version:1
        ~bin_query:Ledger_hash.Stable.V1.bin_t
        ~bin_response:Ledger_hash.Stable.V1.bin_t
  end
end

(* val get_staged_ledger_aux_and_pending_coinbases_at_hash : Ledger_hash.t -> Staged_ledger.Staged_ledger_aux_and_pending_coinbases.t option *)

(* val get_signer_public_key : unit -> Public_key.Compressed.t *)
module Get_signer_public_key = struct
  module V1 = struct
    let t : (unit, Public_key.Compressed.t) Rpc.Rpc.t =
      Rpc.Rpc.create ~name:"Get_signer_public_key" ~version:1
        ~bin_query:Unit.bin_t
        ~bin_response:Public_key.Compressed.Stable.V1.bin_t
  end
end

(* val get_signature : Ledger_hash.t -> Signature.t option *)
module Get_signature = struct
  module V1 = struct
    module Response = struct
      type t =
        (Public_key.Compressed.Stable.V1.t * Signature.Stable.V1.t) option
      [@@deriving bin_io_unversioned]
    end

    let t :
        ( Ledger_hash.t
        , (Public_key.Compressed.t * Signature.t) option )
        Rpc.Rpc.t =
      Rpc.Rpc.create ~name:"Get_signature" ~version:1
        ~bin_query:Ledger_hash.Stable.V1.bin_t ~bin_response:Response.bin_t
  end
end

(* val get_ledger_hashes_chain : source:Ledger_hash.t option -> target:Ledger_hash.t -> Ledger_hash.t list *)
module Get_ledger_hashes_chain = struct
  module V1 = struct
    module Query = struct
      type t =
        { source : [ `Genesis | `Specific of Ledger_hash.Stable.V1.t ]
        ; target : Ledger_hash.Stable.V1.t
        ; max_length : int option
        }
      [@@deriving bin_io_unversioned]
    end

    module Response = struct
      type t = Ledger_hash.Stable.V1.t list [@@deriving bin_io_unversioned]
    end

    let t : (Query.t, Response.t) Rpc.Rpc.t =
      Rpc.Rpc.create ~name:"Get_ledger_hashes_chain" ~version:1
        ~bin_query:Query.bin_t ~bin_response:Response.bin_t
  end
end

(* val get_diffs_chain : source:Ledger_hash.t option -> target:Ledger_hash.t -> Diff.t *)
module Get_diffs_chain = struct
  module V1 = struct
    module Query = struct
      type t =
        { source : [ `Genesis | `Specific of Ledger_hash.Stable.V1.t ]
        ; target : Ledger_hash.Stable.V1.t
        ; max_length : int option
        }
      [@@deriving bin_io_unversioned]
    end

    module Response = struct
      type t = Diff.Stable.V2.t list [@@deriving bin_io_unversioned]
    end

    let t : (Query.t, Response.t) Rpc.Rpc.t =
      Rpc.Rpc.create ~name:"Get_diffs_chain" ~version:1 ~bin_query:Query.bin_t
        ~bin_response:Response.bin_t
  end
end
