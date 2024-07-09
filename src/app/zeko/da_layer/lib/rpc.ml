open Core_kernel
open Async
open Mina_ledger
open Mina_base

(* val post_batch : ledger_openings:Sparse_ledger.t -> batch:Batch.t -> Signature.t *)
module Post_batch = struct
  module Query = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t =
          { ledger_openings : Sparse_ledger.Stable.V2.t
          ; batch : Batch.Stable.V1.t
          }

        let to_latest = Fn.id
      end
    end]
  end

  let v1 : (Query.t, Signature.t) Rpc.Rpc.t =
    Rpc.Rpc.create ~name:"Post_batch" ~version:1
      ~bin_query:Query.Stable.V1.bin_t ~bin_response:Signature.Stable.V1.bin_t
end

(* val get_batch : Ledger_hash.t -> Batch.t option *)
module Get_batch = struct
  module Response = struct
    [%%versioned
    module Stable = struct
      module V1 = struct
        type t = Batch.Stable.V1.t option

        let to_latest = Fn.id
      end
    end]
  end

  let v1 : (Ledger_hash.t, Response.t) Rpc.Rpc.t =
    Rpc.Rpc.create ~name:"Get_batch" ~version:1
      ~bin_query:Ledger_hash.Stable.V1.bin_t
      ~bin_response:Response.Stable.V1.bin_t
end
