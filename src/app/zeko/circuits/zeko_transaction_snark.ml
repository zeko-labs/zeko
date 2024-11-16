open Core_kernel
open Zeko_util
open Snark_params.Tick

let constraint_constants = Genesis_constants.Constraint_constants.compiled

module HandlerV = MkV (Handler)

module Base_input = struct
  type t =
    { stmt : Transaction_snark.Statement.With_sok.t; handler : HandlerV.t }
  [@@deriving snarky]
end

module Merge_input = struct
  type t =
    { stmt : Transaction_snark.Statement.With_sok.t
    ; handler : HandlerV.t
    ; left_proof : ProofV.t
    ; right_proof : ProofV.t
    }
  [@@deriving snarky]
end

module Zeko_env = struct
  type t =
    < zeko_mark_shifted_and_get_previous_shiftedness :
        Mina_base.Account_id.t -> bool >
end

module Zeko_env_V = MkV (Zeko_env)
module Witness_V = MkV (Transaction_snark.Zkapp_command_segment.Witness)

module Zkapp_single_unproved_input = struct
  type t =
    { stmt : Transaction_snark.Statement.With_sok.t
    ; handler : HandlerV.t
    ; witness : Witness_V.t
    ; zeko_env : Zeko_env_V.t
    }
  [@@deriving snarky]
end

let compilation_result =
  Compile_simple.compile ~override_wrap_domain:`N1
    ~name:"zeko-transaction-snark"
    ~out_typ:Transaction_snark.Statement.With_sok.typ
    ~branches:
      [ { branch_name = "base (user commands)"
        ; tags = No_tags
        ; main =
            (fun input ->
              let* { stmt; handler } =
                exists Base_input.typ ~compute:(V.get input)
              in
              let*| () =
                handle_as_prover
                  (fun () ->
                    Transaction_snark.Base.main ~constraint_constants stmt )
                  (V.get handler)
              in
              Compile_simple.{ prevs = No_prevs; out = stmt } )
        }
      ; { branch_name = "single-unproved-zkapp"
        ; tags = No_tags
        ; main =
            (fun input ->
              let* { stmt; handler; witness; zeko_env } =
                exists Zkapp_input.typ ~compute:(V.get input)
              in
              handle_as_prover
                (fun () ->
                  Transaction_snark.Base.Zkapp_command_snark.main
                    ?witness:(V.unsafe_unwrap witness)
                    ?zeko_env:(V.unsafe_unwrap zeko_env) ~constraint_constants
                    spec stmt )
                (V.get handler) )
        }
      ; { branch_name = "merge"
        ; tags = Two_tags (Own_tag, Own_tag)
        ; main =
            (fun input ->
              let* { stmt; handler; left_proof; right_proof } =
                exists Merge_input.typ ~compute:(V.get input)
              in
              let*| left_stmt, right_stmt =
                handle_as_prover
                  (fun () -> Transaction_snark.Merge.main stmt)
                  (V.get handler)
              in
              Compile_simple.
                { prevs =
                    Two_prevs
                      ( { public_input = left_stmt
                        ; proof = left_proof
                        ; proof_must_verify = Boolean.true_
                        }
                      , { public_input = right_stmt
                        ; proof = right_proof
                        ; proof_must_verify = Boolean.true_
                        } )
                ; out = stmt
                } )
        }
      ]
    ()
