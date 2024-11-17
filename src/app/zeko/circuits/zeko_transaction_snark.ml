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

module Verification_key = struct
  include Pickles.Side_loaded.Verification_key

  type var = Checked.t
end

module Zkapp_single_proved_input = struct
  type t =
    { stmt : Transaction_snark.Statement.With_sok.t
    ; handler : HandlerV.t
    ; witness : Witness_V.t
    ; zeko_env : Zeko_env_V.t
    ; zkapp_vk : Verification_key.t
    ; zkapp_proof : ProofV.t
    }
  [@@deriving snarky]
end

let compilation_result =
  lazy
    (let@ () = Promise.block_on_async_exn in
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
                   exists Zkapp_single_unproved_input.typ ~compute:(V.get input)
                 in
                 let*| must_be_none, _must_verify_zkapp =
                   handle_as_prover
                     (fun () ->
                       let@ () = make_checked in
                       Transaction_snark.Base.Zkapp_command_snark.main
                         ?witness:(V.unsafe_unwrap witness)
                         ?zeko_env:(V.unsafe_unwrap zeko_env)
                         ~constraint_constants
                         (Transaction_snark.Zkapp_command_segment.Basic
                          .to_single_list Opt_signed )
                         stmt )
                     (V.get handler)
                 in
                 assert (Option.is_none must_be_none) ;
                 Compile_simple.{ prevs = No_prevs; out = stmt } )
           }
         ; { branch_name = "double-unproved-zkapp"
           ; tags = No_tags
           ; main =
               (fun input ->
                 let* { stmt; handler; witness; zeko_env } =
                   exists Zkapp_single_unproved_input.typ ~compute:(V.get input)
                 in
                 let*| must_be_none, _must_verify_zkapp =
                   handle_as_prover
                     (fun () ->
                       let@ () = make_checked in
                       Transaction_snark.Base.Zkapp_command_snark.main
                         ?witness:(V.unsafe_unwrap witness)
                         ?zeko_env:(V.unsafe_unwrap zeko_env)
                         ~constraint_constants
                         (Transaction_snark.Zkapp_command_segment.Basic
                          .to_single_list Opt_signed_opt_signed )
                         stmt )
                     (V.get handler)
                 in
                 assert (Option.is_none must_be_none) ;
                 Compile_simple.{ prevs = No_prevs; out = stmt } )
           }
         ; { branch_name = "proved-zkapp"
           ; tags =
               One_tag_sideloaded
                 { sideloaded_tag_name = "proved-zkapp"
                 ; typ = Mina_base.Zkapp_statement.typ
                 ; extract_vk =
                     (fun ({ zkapp_vk; _ } : Zkapp_single_proved_input.t) ->
                       zkapp_vk )
                 }
           ; main =
               (fun input ->
                 let* { stmt
                      ; handler
                      ; witness
                      ; zeko_env
                      ; zkapp_vk
                      ; zkapp_proof
                      } =
                   exists Zkapp_single_proved_input.typ ~compute:(V.get input)
                 in
                 let*| zkapp_statement, `Must_verify proof_must_verify =
                   handle_as_prover
                     (fun () ->
                       let@ () = make_checked in
                       Transaction_snark.Base.Zkapp_command_snark.main
                         ?witness:(V.unsafe_unwrap witness)
                         ?zeko_env:(V.unsafe_unwrap zeko_env)
                         ~constraint_constants
                         (Transaction_snark.Zkapp_command_segment.Basic
                          .to_single_list Opt_signed_opt_signed )
                         stmt )
                     (V.get handler)
                 in
                 Compile_simple.
                   { prevs =
                       One_prev_sideloaded
                         { public_input = Option.value_exn zkapp_statement
                         ; proof = zkapp_proof
                         ; proof_must_verify
                         ; vk = zkapp_vk
                         }
                   ; out = stmt
                   } )
           }
         ; { branch_name = "merge"
           ; tags = Two_tags_own
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
       () )

let tag : (_, _, _, Pickles_types.Nat.N5.n) Pickles.Tag.t lazy_t =
  lazy
    (let (Result { tag; provers = _; tag_length = S (S (S (S (S Z)))) }) =
       force compilation_result
     in
     tag )

let prove_base input =
  let (Result { tag = _; provers = [ base; _; _; _; _ ]; tag_length = _ }) =
    force compilation_result
  in
  base input

let prove_single input =
  let (Result
        { tag = _; provers = [ _; prove_single; _; _; _ ]; tag_length = _ } ) =
    force compilation_result
  in
  prove_single input

let prove_double input =
  let (Result
        { tag = _; provers = [ _; _; prove_double; _; _ ]; tag_length = _ } ) =
    force compilation_result
  in
  prove_double input

let prove_zkapp input =
  let (Result { tag = _; provers = [ _; _; _; prove_zkapp; _ ]; tag_length = _ })
      =
    force compilation_result
  in
  prove_zkapp input

let prove_merge input =
  let (Result { tag = _; provers = [ _; _; _; _; prove_merge ]; tag_length = _ })
      =
    force compilation_result
  in
  prove_merge input
