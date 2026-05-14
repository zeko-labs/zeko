open Core_kernel
open Mina_base
open Mina_ledger
open Zeko_circuits
open Zeko_util
open Snark_params.Tick
open Signature_lib

let ok_exn = function
  | Ppx_deriving_yojson_runtime.Result.Ok x ->
      x
  | Error e ->
      failwith e

module type SnarkTypeWithoutAux = sig
  type t

  type var

  val typ : (var, t) Typ.t
end

(** Use only for snarky types that are not using auxiliary system *)
module Snarky_serializable_unsafe (Typ : SnarkType) = struct
  let of_fields fields =
    let (Typ typ) = Typ.typ in
    typ.value_of_fields (fields, typ.constraint_system_auxiliary ())

  let to_fields t =
    let (Typ typ) = Typ.typ in
    let fields, _aux = typ.value_to_fields t in
    fields

  let to_yojson t : Yojson.Safe.t =
    to_fields t |> Array.to_list
    |> List.map ~f:Field.to_yojson
    |> fun fs -> `List fs

  let of_yojson json : Typ.t Ppx_deriving_yojson_runtime.error_or =
    let module M = Ppx_deriving_yojson_runtime in
    let open Yojson.Safe.Util in
    try
      let fields =
        to_list json
        |> List.map ~f:Field.of_yojson
        |> List.map ~f:ok_exn |> Array.of_list
      in
      Ok (of_fields fields)
    with e -> Error (Exn.to_string e)
end

module Inner_rules_inst = Inner_rules.Make (Zeko_circuits_config.Inputs) ()

module Outer_rules_inst = Outer_rules.Make (Zeko_circuits_config.Inputs) ()

module Bridge_inst_mina =
  Bridge_rules.Make_mina (Zeko_circuits_config.Inputs) ()

module F = struct
  include F

  type t = Field.t [@@deriving yojson]
end

module Make_serializable_path (Inputs : sig
  module PathStep : sig
    type t [@@deriving yojson]
  end
end) =
struct
  open Inputs

  type t = PathStep.t list [@@deriving yojson]
end

module Account_set = struct
  include Account_set
  include Snarky_serializable_unsafe (Account_set)
end

module Acc_set_witness = struct
  module Path = Make_serializable_path (struct
    module PathStep = struct
      type t = Account_set.PathStep.t

      let to_yojson ({ hash_other; is_right } : t) =
        `Assoc
          [ ("hash_other", Field.to_yojson hash_other)
          ; ("is_right", `Bool is_right)
          ]

      let of_yojson step_json =
        let open Yojson.Safe.Util in
        try
          Ok
            ( { hash_other =
                  member "hash_other" step_json |> Field.of_yojson |> ok_exn
              ; is_right = member "is_right" step_json |> to_bool
              }
              : Account_set.PathStep.t )
        with e -> Error (Exn.to_string e)
    end
  end)

  type t = Txn_state.update_acc_set_witness

  type serializable =
    { x : Token_id.t list
    ; z : Token_id.t list
    ; y_prev_hash : Field.t list
    ; y_prev_path : Path.t list
    ; x_path : Path.t list
    ; y_path : Path.t list
    }
  [@@deriving yojson]

  let of_serializable
      ({ x; z; y_prev_hash; y_prev_path; x_path; y_path } : serializable) : t =
    let list_to_fun l =
      let l = ref l in
      fun () ->
        match !l with
        | [] ->
            failwith "empty!"
        | x :: xs ->
            l := xs ;
            x
    in
    { get_account_set_x = list_to_fun x
    ; get_account_set_z = list_to_fun z
    ; get_account_set_y_prev_hash = list_to_fun y_prev_hash
    ; get_account_set_y_prev_path = list_to_fun y_prev_path
    ; get_account_set_x_path = list_to_fun x_path
    ; get_account_set_y_path = list_to_fun y_path
    }

  let empty =
    { x = []
    ; x_path = []
    ; y_prev_hash = []
    ; y_prev_path = []
    ; y_path = []
    ; z = []
    }

  let add t
      ((x, x_path, y_prev_hash, y_prev_path, _, y_path, z) :
        [ `X of Token_id.t ]
        * [ `X_path of Ledger.Path.t ]
        * [ `Y_prev_hash of Field.t ]
        * [ `Y_prev_path of Ledger.Path.t ]
        * [ `Y of Token_id.t ]
        * [ `Y_path of [ `Left of Field.t | `Right of Field.t ] list ]
        * [ `Z of Token_id.t ] ) =
    let mina_path_to_zeko_path path =
      let open Account_set in
      List.map path ~f:(function
        | `Left hash_other ->
            ({ PathStep.hash_other; is_right = false } : PathStep.t)
        | `Right hash_other ->
            ({ PathStep.hash_other; is_right = true } : PathStep.t) )
    in
    let x = match x with `X x -> x in
    let x_path = match x_path with `X_path path -> path in
    let y_prev_hash = match y_prev_hash with `Y_prev_hash hash -> hash in
    let y_prev_path = match y_prev_path with `Y_prev_path path -> path in
    let y_path = match y_path with `Y_path path -> path in
    let z = match z with `Z z -> z in
    { x = t.x @ [ x ]
    ; x_path = t.x_path @ [ mina_path_to_zeko_path x_path ]
    ; y_prev_hash = t.y_prev_hash @ [ y_prev_hash ]
    ; y_prev_path = t.y_prev_path @ [ mina_path_to_zeko_path y_prev_path ]
    ; y_path = t.y_path @ [ mina_path_to_zeko_path y_path ]
    ; z = t.z @ [ z ]
    }

  let join t1 t2 =
    { x = t1.x @ t2.x
    ; x_path = t1.x_path @ t2.x_path
    ; y_prev_hash = t1.y_prev_hash @ t2.y_prev_hash
    ; y_prev_path = t1.y_prev_path @ t2.y_prev_path
    ; y_path = t1.y_path @ t2.y_path
    ; z = t1.z @ t2.z
    }
end

module Zkapp_rule_input_witness = struct
  type t = Rule_zkapp_command.Zkapp_rule_input_witness.t

  type serializable =
    { stack_frame :
        ( Token_id.Stable.V2.t
        , Zkapp_command.Call_forest.With_hashes.Stable.V1.t )
        Stack_frame.Stable.V1.t
    ; call_stack :
        ( ( ( Token_id.Stable.V2.t
            , Zkapp_command.Call_forest.With_hashes.Stable.V1.t )
            Stack_frame.Stable.V1.t
          , Stack_frame.Digest.Stable.V1.t )
          With_hash.t
        , Call_stack_digest.Stable.V1.t )
        With_stack_hash.Stable.V1.t
        list
    ; source_ledger_sparse : Mina_ledger.Sparse_ledger.t
    ; update_acc_set_witness : Acc_set_witness.serializable
    }
  [@@deriving yojson]

  let of_serializable ~proof_cache_db
      ({ stack_frame; call_stack; source_ledger_sparse; update_acc_set_witness } :
        serializable ) : t =
    let write_stack_frame stack_frame =
      Stack_frame.
        { caller = stack_frame.caller
        ; caller_caller = stack_frame.caller_caller
        ; calls =
            Zkapp_command.Call_forest.With_hashes.write_all_proofs_to_disk
              ~proof_cache_db stack_frame.calls
        }
    in
    let call_stack =
      List.map call_stack
        ~f:(With_stack_hash.map ~f:(With_hash.map ~f:write_stack_frame))
    in
    { stack_frame = write_stack_frame stack_frame
    ; call_stack
    ; source_ledger_sparse
    ; update_acc_set_witness =
        Acc_set_witness.of_serializable update_acc_set_witness
    }
end

module Even_PC = struct
  include Zeko_util.Even_PC

  type t = Zeko_util.Even_PC.t = { public_key : F.t } [@@deriving yojson]

  let create (public_key : Signature_lib.Public_key.Compressed.t) =
    if public_key.is_odd then Error (Error.of_string "Odd public key")
    else Ok ({ public_key = public_key.x } : t)

  let create_exn (public_key : Signature_lib.Public_key.Compressed.t) =
    match create public_key with Ok pc -> pc | Error e -> Error.raise e

  let rec generate_even_signer () =
    let signer = Keypair.create () in
    let compressed = Public_key.compress signer.public_key in
    if compressed.is_odd then generate_even_signer () else signer

  let to_pc { public_key } : Public_key.Compressed.t =
    { x = public_key; is_odd = false }
end

module Local_state = struct
  type t = Txn_state.Local_state.t =
    { stack_frame_digest : Stack_frame.Digest.t
    ; call_stack_digest : Call_stack_digest.t
    ; transaction_commitment : F.t
    ; full_transaction_commitment : F.t
    ; excess : Currency.Amount.Signed.t
    ; account_update_index : Mina_numbers.Index.t
    }
  [@@deriving yojson]
end

module Zkapp_rule_input = struct
  type t = Rule_zkapp_command.Zkapp_rule_input.t

  type serializable =
    { source_ledger : Ledger_hash.t
    ; source_local_state : Local_state.t
    ; sequencer : Even_PC.t
    ; source_acc_set : Account_set.t
    ; witness : Zkapp_rule_input_witness.serializable
    }
  [@@deriving yojson]

  let of_serializable ~proof_cache_db
      ({ source_ledger; source_local_state; sequencer; source_acc_set; witness } :
        serializable ) : t =
    { source_ledger
    ; source_local_state
    ; sequencer
    ; source_acc_set
    ; witness = Zkapp_rule_input_witness.of_serializable ~proof_cache_db witness
    }
end

module Per_account_update = struct
  type t = Rule_zkapp_command.Per_account_update.t

  type serializable =
    { account_updates : Zkapp_command.Digest.Forest.t
    ; memo_hash : F.t
    ; account_updates_data : Zkapp_command.Call_forest.With_hashes.Stable.V1.t
    ; shift_action_state : bool
    }
  [@@deriving yojson]

  let of_serializable ~proof_cache_db
      ({ account_updates; memo_hash; account_updates_data; shift_action_state } :
        serializable ) : t =
    { account_updates
    ; memo_hash
    ; account_updates_data =
        Zkapp_command.Call_forest.With_hashes.write_all_proofs_to_disk
          ~proof_cache_db account_updates_data
    ; shift_action_state
    }
end

module Verification_key = struct
  type t = Compile_simple.Verification_key.t

  type serializable = Pickles.Side_loaded.Verification_key.t [@@deriving yojson]

  let of_serializable x = Compile_simple.Verification_key.of_pickles x
end

module Zkapp_single_proved_input = struct
  type t = Rule_zkapp_command.Zkapp_single_proved_input.t

  type serializable =
    { base : Zkapp_rule_input.serializable
    ; vk : Verification_key.serializable
    ; zkapp_proof : Proof.t
    ; first : Per_account_update.serializable
    }
  [@@deriving yojson]

  let of_serializable ~proof_cache_db
      ({ base; vk; zkapp_proof; first } : serializable) : t =
    { base = Zkapp_rule_input.of_serializable ~proof_cache_db base
    ; vk = Verification_key.of_serializable vk
    ; zkapp_proof
    ; first = Per_account_update.of_serializable ~proof_cache_db first
    }
end

module Zkapp_single_unproved_input = struct
  type t = Rule_zkapp_command.Zkapp_single_unproved_input.t

  type serializable =
    { base : Zkapp_rule_input.serializable
    ; first : Per_account_update.serializable
    }
  [@@deriving yojson]

  let of_serializable ~proof_cache_db ({ base; first } : serializable) : t =
    { base = Zkapp_rule_input.of_serializable ~proof_cache_db base
    ; first = Per_account_update.of_serializable ~proof_cache_db first
    }
end

module Zkapp_double_unproved_input = struct
  type t = Rule_zkapp_command.Zkapp_double_unproved_input.t

  type serializable =
    { base : Zkapp_rule_input.serializable
    ; first : Per_account_update.serializable
    ; second : Per_account_update.serializable
    }
  [@@deriving yojson]

  let of_serializable ~proof_cache_db ({ base; first; second } : serializable) :
      t =
    { base = Zkapp_rule_input.of_serializable ~proof_cache_db base
    ; first = Per_account_update.of_serializable ~proof_cache_db first
    ; second = Per_account_update.of_serializable ~proof_cache_db second
    }
end

module Sparse_ledger_handler = struct
  include Handler

  let of_yojson json : t Ppx_deriving_yojson_runtime.error_or =
    match Sparse_ledger.of_yojson json with
    | Ok sparse_ledger ->
        Ok (unstage @@ Sparse_ledger.handler sparse_ledger)
    | Error e ->
        Error e
end

module Base_witness = struct
  type t = Rule_signed_command.Base_witness.t

  type serializable =
    { ledger_path_handler : Sparse_ledger.t
    ; update_acc_set_witness : Acc_set_witness.serializable
    }
  [@@deriving yojson]

  let of_serializable
      ({ ledger_path_handler; update_acc_set_witness } : serializable) : t =
    { ledger_path_handler = unstage @@ Sparse_ledger.handler ledger_path_handler
    ; update_acc_set_witness =
        Acc_set_witness.of_serializable update_acc_set_witness
    }
end

module Base_input = struct
  type t = Rule_signed_command.Base_input.t

  type transaction_union =
    | Command of Signed_command.t
    | Fee_transfer of Fee_transfer.t
  [@@deriving yojson]

  type serializable =
    { source_ledger : Ledger_hash.t
    ; source_acc_set : Account_set.t
    ; sequencer : Even_PC.t
    ; transaction : transaction_union
    ; witness : Base_witness.serializable
    }
  [@@deriving yojson]

  let of_serializable
      ({ source_ledger; source_acc_set; sequencer; transaction; witness } :
        serializable ) : t =
    { source_ledger
    ; source_acc_set
    ; sequencer
    ; transaction =
        Mina_transaction.Transaction_union.of_transaction
          ( match transaction with
          | Command c ->
              Command c
          | Fee_transfer c ->
              Fee_transfer c )
    ; witness = Base_witness.of_serializable witness
    }
end

module Txn_snark_witness = struct
  module Zkapp_command_segment = struct
    type t =
      | Single_unproved of Zkapp_single_unproved_input.serializable
      | Double_unproved of Zkapp_double_unproved_input.serializable
      | Single_proved of Zkapp_single_proved_input.serializable
    [@@deriving yojson]
  end

  type t =
    | Signed_command of Base_input.serializable
    | Zkapp_command of Zkapp_command_segment.t
  [@@deriving yojson]
end

module Slot_range = struct
  include Zeko_util.Slot_range

  type t = Zeko_util.Slot_range.t = { lower : Slot.t; upper : Slot.t }
  [@@deriving yojson]
end

module Zeko_stmt = struct
  type t = Txn_state.Zeko_stmt.t =
    { source_ledger : Ledger_hash.t
    ; target_ledger : Ledger_hash.t
    ; source_acc_set : Account_set.t
    ; target_acc_set : Account_set.t
    ; sequencer : Even_PC.t
    ; accumulated_fees : Currency.Amount.Signed.t
    ; slot_range : Slot_range.t
    ; global_slot_range : Slot_range.t
    ; source_local_state : Local_state.t
    ; target_local_state : Local_state.t
    }
  [@@deriving yojson]
end

module Merge_input = struct
  type t = Rule_txn_merge.Merge_input.t =
    { left : Zeko_stmt.t
    ; left_proof : Proof.t
    ; right : Zeko_stmt.t
    ; right_proof : Proof.t
    }
  [@@deriving yojson]
end

module Make_serializable_snark (Inputs : sig
  module Stmt : sig
    type t [@@deriving yojson]
  end

  module System : sig
    type t

    val make_unchecked : ?proof:Proof_V.t -> Stmt.t -> t
  end
end) =
struct
  open Inputs

  type t = System.t

  type serializable = Stmt.t * Proof.t [@@deriving yojson]

  let of_serializable (stmt, proof) : t = System.make_unchecked ~proof stmt
end

module Txn_snark = Make_serializable_snark (struct
  module Stmt = Zeko_stmt
  module System = Txn_rules
end)

module Ase = struct
  module type Trans = sig
    type t

    val source_hash : t -> F.t

    val target_hash : t -> F.t
  end

  module type Stmt = sig
    type t [@@deriving yojson]

    val state : t -> F.t
  end

  module With_length = struct
    include Ase.With_length

    module Stmt = struct
      type t = Ase.With_length.Stmt.t =
        { action_state : F.t; length : Checked32.t }
      [@@deriving yojson]

      let state (t : t) = t.action_state
    end

    module Elem = F

    type trans = Ase.With_length.trans = { source : Stmt.t; target : Stmt.t }
    [@@deriving yojson]

    module Trans = struct
      type t = trans [@@deriving yojson]

      let source_hash (x : t) = x.source.action_state

      let target_hash (x : t) = x.target.action_state
    end
  end

  module Without_length = struct
    include Ase.Without_length

    module Stmt = struct
      type t = F.t [@@deriving yojson]

      let state (x : t) = x
    end

    module Elem = F

    type trans = Ase.Without_length.trans = { source : Stmt.t; target : Stmt.t }
    [@@deriving yojson]

    module Trans = struct
      type t = trans [@@deriving yojson]

      let source_hash (x : t) = x.source

      let target_hash (x : t) = x.target
    end
  end

  module type Ase_inst_intf = sig
    type t

    module Ase_state : sig
      type t [@@deriving yojson]
    end

    module Action_state : sig
      type t [@@deriving yojson]
    end

    module Stmt : sig
      type t = { source : Action_state.t; target : Action_state.t }
    end

    val make :
         proof_source:Ase_state.t
      -> proof_target:Ase_state.t
      -> ?proof:Proof_V.t
      -> Ase_state.t
      -> field list
      -> t
  end

  module Make_serializable_ase (Inputs : sig
    module Ase_system : sig
      module Stmt : sig
        type t [@@deriving yojson]
      end
    end

    module Action_state : sig
      type t [@@deriving yojson]
    end

    module Ase_inst :
      Ase_inst_intf
        with module Ase_state := Ase_system.Stmt
         and module Action_state := Action_state
  end) =
  struct
    open Inputs
    include Ase_inst

    type t = Ase_inst.t

    module Stmt = struct
      type t = Ase_inst.Stmt.t =
        { source : Action_state.t; target : Action_state.t }
      [@@deriving yojson]
    end

    type serializable =
      { proof : Compile_simple.Proof.t option
      ; proof_target : Ase_system.Stmt.t
      ; init : Ase_system.Stmt.t
      ; excess : F.t list
      }
    [@@deriving yojson]

    let of_serializable ({ proof; proof_target; init; excess } : serializable) :
        t =
      Ase_inst.make ?proof ~proof_source:init ~proof_target init excess
  end
end

module Make_serializable_action_state
    (Action_state : Rollup_state.Action_state_type) =
struct
  type t = Action_state.t

  include Snarky_serializable_unsafe (Action_state)

  module With_length = struct
    type t = Action_state.With_length.t

    include Snarky_serializable_unsafe (Action_state.With_length)
  end
end

module Inner_action_state =
  Make_serializable_action_state (Rollup_state.Inner_action_state)
module Outer_action_state =
  Make_serializable_action_state (Rollup_state.Outer_action_state)

module Inner_sync = struct
  module Ase_inst = Ase.Make_serializable_ase (struct
    module Ase_system = Ase.With_length
    module Action_state = Outer_action_state.With_length
    module Ase_inst = Rule_inner_sync.Ase_inst
  end)

  module Witness = struct
    type t = Rule_inner_sync.Witness.t

    type serializable =
      { public_key : Public_key.Compressed.t; ase : Ase_inst.serializable }
    [@@deriving yojson]

    let of_serializable ({ public_key; ase } : serializable) ~vk_hash : t =
      { public_key; vk_hash; ase = Ase_inst.of_serializable ase }
  end
end

module Multisig = struct
  include Multisig

  module Maybe_signature = struct
    include Multisig.Maybe_signature

    type t = Multisig.Maybe_signature.t =
      { public_key : Public_key.Compressed.t
      ; signature : Signature.t
      ; is_some : bool
      }
    [@@deriving yojson]
  end

  module Witness = struct
    include Multisig.Witness

    type t = Multisig.Witness.t =
      { signatures : Maybe_signature.t list; quorum : F.t }
    [@@deriving yojson]

    let make ~signatures ~quorum =
      { signatures =
          List.map signatures ~f:(fun (public_key, signature) ->
              Maybe_signature.
                { public_key
                ; signature = Option.value signature ~default:Signature.dummy
                ; is_some = Option.is_some signature
                } )
      ; quorum = Field.of_int quorum
      }
  end
end

module Outer_commit = struct
  module Path = Make_serializable_path (struct
    module PathStep = struct
      type t = Outer_rules_inst.Rule_commit_inst.PathElt.t

      let to_yojson ({ right_side } : t) =
        `Assoc [ ("right_side", Field.to_yojson right_side) ]

      let of_yojson json : t Ppx_deriving_yojson_runtime.error_or =
        let open Yojson.Safe.Util in
        try
          Ok
            { right_side = member "right_side" json |> Field.of_yojson |> ok_exn
            }
        with e -> Error (Exn.to_string e)
    end
  end)

  module Ase_outer_inst = Ase.Make_serializable_ase (struct
    module Ase_system = Ase.Without_length
    module Action_state = Outer_action_state
    module Ase_inst = Rule_commit.Ase_outer_inst
  end)

  module Ase_inner_inst = Ase.Make_serializable_ase (struct
    module Ase_system = Ase.With_length
    module Action_state = Inner_action_state.With_length
    module Ase_inst = Rule_commit.Ase_inner_inst
  end)

  module Verify_both_ases = Make_serializable_snark (struct
    module Stmt = struct
      type t = Ase_outer_inst.Stmt.t * Ase_inner_inst.Stmt.t [@@deriving yojson]
    end

    module System = Rule_commit.Verify_both_ases
  end)

  module Witness = struct
    type t = Outer_rules_inst.Rule_commit_inst.Witness.t

    type serializable =
      { txn_snark : Txn_snark.serializable
      ; public_key : Public_key.Compressed.t
      ; emergency_mode : bool
      ; old_inner_acc : Account.t
      ; old_inner_acc_path : Path.t
      ; new_inner_acc : Account.t
      ; new_inner_acc_path : Path.t
      ; da_multisig : Multisig.Witness.t
      ; slot_range : Slot_range.t
      ; verify_both_ases : Verify_both_ases.serializable
      }
    [@@deriving yojson]

    let of_serializable
        ({ txn_snark
         ; public_key
         ; emergency_mode
         ; verify_both_ases
         ; old_inner_acc
         ; old_inner_acc_path
         ; new_inner_acc
         ; new_inner_acc_path
         ; da_multisig
         ; slot_range
         } :
          serializable ) ~vk_hash : t =
      { base_witness =
          { public_key
          ; vk_hash
          ; emergency_mode
          ; old_inner_acc
          ; old_inner_acc_path
          ; new_inner_acc
          ; new_inner_acc_path
          ; da_multisig
          ; slot_range
          }
      ; txn_snark = Txn_snark.of_serializable txn_snark
      ; verify_both_ases = Verify_both_ases.of_serializable verify_both_ases
      }
  end
end

module Bridge = struct
  module Outer_action_witness = struct
    module Witness = struct
      type t = Rollup_state.Outer_action.Witness.t

      type serializable =
        { aux : F.t
        ; children :
            ( Account_update.Stable.Latest.t
            , Zkapp_command.Digest.Account_update.t
            , Zkapp_command.Digest.Forest.t )
            Zkapp_command.Call_forest.t
        ; slot_range : Slot_range.t
        }
      [@@deriving yojson]

      let of_serializable ~proof_cache_db
          ({ aux; children; slot_range } : serializable) : t =
        { aux
        ; children =
            Zkapp_command.Call_forest.With_hashes.write_all_proofs_to_disk
              ~proof_cache_db children
        ; slot_range
        }
    end

    type t = Rule_action_witness.Witness.t

    type serializable =
      { public_key : Public_key.Compressed.t; witness : Witness.serializable }
    [@@deriving yojson]

    let of_serializable ~proof_cache_db ({ public_key; witness } : serializable)
        ~vk_hash : t =
      { public_key
      ; vk_hash
      ; witness = Witness.of_serializable ~proof_cache_db witness
      }
  end

  module Inner_action_witness = struct
    module Witness = struct
      type t = Rollup_state.Inner_action.t

      type serializable =
        { aux : F.t
        ; children :
            ( Account_update.Stable.Latest.t
            , Zkapp_command.Digest.Account_update.t
            , Zkapp_command.Digest.Forest.t )
            Zkapp_command.Call_forest.t
        }
      [@@deriving yojson]

      let of_serializable ~proof_cache_db ({ aux; children } : serializable) : t
          =
        { aux
        ; children =
            Zkapp_command.Call_forest.With_hashes.write_all_proofs_to_disk
              ~proof_cache_db children
        }
    end

    type t = Rule_inner_action_witness.Witness.t

    type serializable =
      { public_key : Public_key.Compressed.t; witness : Witness.serializable }
    [@@deriving yojson]

    let of_serializable ~proof_cache_db ({ public_key; witness } : serializable)
        ~vk_hash : t =
      { public_key
      ; vk_hash
      ; witness = Witness.of_serializable ~proof_cache_db witness
      }
  end

  module Deposit_params_base = struct
    type t = Bridge_state.Deposit_params_base.t

    type serializable =
      { children :
          ( Account_update.Stable.Latest.t
          , Zkapp_command.Digest.Account_update.t
          , Zkapp_command.Digest.Forest.t )
          Zkapp_command.Call_forest.t
      ; holder_account_l1 : Public_key.Compressed.t
      ; amount : Currency.Amount.t
      ; recipient : Public_key.Compressed.t
      ; timeout : Slot.t
      }
    [@@deriving yojson]

    let of_serializable ~proof_cache_db
        ({ children; holder_account_l1; amount; recipient; timeout } :
          serializable ) : t =
      { children =
          Zkapp_command.Call_forest.With_hashes.write_all_proofs_to_disk
            ~proof_cache_db children
      ; holder_account_l1
      ; amount
      ; recipient
      ; timeout
      }

    let to_serializable
        ({ children; holder_account_l1; amount; recipient; timeout } : t) :
        serializable =
      { children =
          Zkapp_command.Call_forest.With_hashes.read_all_proofs_from_disk
            children
      ; holder_account_l1
      ; amount
      ; recipient
      ; timeout
      }

    let to_yojson = Fn.compose serializable_to_yojson to_serializable

    let of_yojson json =
      Ppx_deriving_yojson_runtime.(
        serializable_of_yojson json
        >|= of_serializable
              ~proof_cache_db:(Proof_cache_tag.create_identity_db ()))
  end

  module Check_accepted_mina = struct
    include Bridge_inst_mina.Check_accepted
    include Bridge_inst_mina.Check_accepted.Definition

    module Stmt = struct
      type t = Bridge_inst_mina.Check_accepted.Definition.Stmt.t =
        { params : Deposit_params_base.t
        ; action_state : Outer_action_state.t
        ; deposit_index : Checked32.t
        ; n_steps : Checked32.t
        ; is_rejected : bool
        ; is_accepted : bool
        }
      [@@deriving yojson]
    end

    module Elem = struct
      type t = Bridge_inst_mina.Check_accepted.Definition.Elem.t

      include
        Snarky_serializable_unsafe
          (Bridge_inst_mina.Check_accepted.Definition.Elem)
    end

    module Init = struct
      type t = Bridge_inst_mina.Check_accepted.Definition.Init.t =
        { params : Deposit_params_base.t
        ; original_action_state : Outer_action_state.t
        ; deposit_index : Checked32.t
        }
      [@@deriving yojson]
    end

    type serializable =
      { proof : Compile_simple.Proof.t option
      ; proof_source : Stmt.t
      ; proof_target : Stmt.t
      ; init : Init.t
      ; excess : Elem.t list
      }
    [@@deriving yojson]

    let of_serializable
        ({ proof; proof_source; proof_target; init; excess } : serializable) =
      Bridge_inst_mina.Rule_bridge_finalize_deposit.Check_accepted_inst.make
        ?proof ~proof_source ~proof_target init excess

    let of_serializable_cancelled_deposit
        ({ proof; proof_source; proof_target; init; excess } : serializable) =
      Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
      .Check_accepted_inst
      .make ?proof ~proof_source ~proof_target init excess
  end

  module Finalize_deposit = struct
    module Ase_inst = Ase.Make_serializable_ase (struct
      module Ase_system = Ase.With_length
      module Action_state = Outer_action_state.With_length

      module Ase_inst = Bridge_inst_mina.Rule_bridge_finalize_deposit.Ase_inst
    end)

    type t = Bridge_inst_mina.Rule_bridge_finalize_deposit.Witness.t

    type serializable =
      { public_key : Public_key.Compressed.t
      ; may_use_token :
          Bridge_inst_mina.Rule_bridge_finalize_deposit.May_use_token.t
      ; inner_authorization_kind : Rule_bridge_finalize_deposit.A.t
      ; ase : Ase_inst.serializable
      ; check_accepted : Check_accepted_mina.serializable
      ; prev_next_deposit : Checked32.t
      ; prev_nonce : Checked32.t
      ; helper_account_new : bool
      }
    [@@deriving yojson]

    let of_serializable
        ({ public_key
         ; may_use_token
         ; inner_authorization_kind
         ; ase
         ; check_accepted
         ; prev_next_deposit
         ; prev_nonce
         ; helper_account_new
         } :
          serializable ) ~vk_hash : t =
      { public_key
      ; vk_hash
      ; may_use_token
      ; inner_authorization_kind
      ; ase = Ase_inst.of_serializable ase
      ; check_accepted = Check_accepted_mina.of_serializable check_accepted
      ; prev_next_deposit
      ; prev_nonce =
          Mina_numbers.Account_nonce.of_string (Checked32.to_string prev_nonce)
      ; helper_account_new
      }
  end

  module Commit = struct
    type t = Rollup_state.Outer_action.Commit.t

    include Snarky_serializable_unsafe (Rollup_state.Outer_action.Commit)
  end

  module Finalize_cancelled_deposit = struct
    module Ase_outer_inst = Ase.Make_serializable_ase (struct
      module Ase_system = Ase.Without_length
      module Action_state = Outer_action_state

      module Ase_inst =
        Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit.Ase_outer_inst
    end)

    module Ase_outer_with_length_inst = Ase.Make_serializable_ase (struct
      module Ase_system = Ase.With_length
      module Action_state = Outer_action_state.With_length

      module Ase_inst =
        Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
        .Ase_outer_with_length_inst
    end)

    module Verify_two_outer_ases = Make_serializable_snark (struct
      module Stmt = struct
        type t = Ase_outer_inst.Stmt.t * Ase_outer_with_length_inst.Stmt.t
        [@@deriving yojson]
      end

      module System =
        Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
        .Verify_two_outer_ases
    end)

    module Verify_check_accepted_and_ase = Make_serializable_snark (struct
      module Stmt = struct
        type t = Check_accepted_mina.Stmt.t * Ase_outer_with_length_inst.Stmt.t
        [@@deriving yojson]
      end

      module System =
        Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit
        .Verify_check_accepted_and_ase
    end)

    type t = Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit.Witness.t

    type serializable =
      { public_key : Public_key.Compressed.t
      ; may_use_token :
          Bridge_inst_mina.Rule_bridge_finalize_cancelled_deposit.May_use_token
          .t
      ; outer_authorization_kind : Rule_bridge_finalize_cancelled_deposit.A.t
      ; commit : Commit.t
      ; before_commit : Outer_action_state.t
      ; verify_two_outer_ases : Verify_two_outer_ases.serializable
      ; verify_check_accepted_and_ase :
          Verify_check_accepted_and_ase.serializable
      ; prev_next_cancelled_deposit : Checked32.t
      ; prev_nonce : Checked32.t
      ; helper_account_new : bool
      }
    [@@deriving yojson]

    let of_serializable
        ({ public_key
         ; may_use_token
         ; outer_authorization_kind
         ; commit
         ; before_commit
         ; verify_two_outer_ases
         ; verify_check_accepted_and_ase
         ; prev_next_cancelled_deposit
         ; prev_nonce
         ; helper_account_new
         } :
          serializable ) ~vk_hash ~helper_token_owner_l1_vk_hash : t =
      { public_key
      ; vk_hash
      ; may_use_token
      ; outer_authorization_kind
      ; commit
      ; before_commit_ase = before_commit
      ; verify_two_outer_ases =
          Verify_two_outer_ases.of_serializable verify_two_outer_ases
      ; verify_check_accepted_and_ase =
          Verify_check_accepted_and_ase.of_serializable
            verify_check_accepted_and_ase
      ; prev_next_cancelled_deposit
      ; helper_token_owner_l1_vk_hash
      ; prev_nonce =
          Mina_numbers.Account_nonce.of_string (Checked32.to_string prev_nonce)
      ; helper_account_new
      }
  end

  module Inner_receive = struct
    type t = Bridge_inst_mina.Rule_bridge_inner_receive.Witness.t

    type serializable =
      { public_key : Public_key.Compressed.t; amount : Currency.Amount.t }
    [@@deriving yojson]

    let of_serializable ({ public_key; amount } : serializable) ~vk_hash : t =
      { public_key; vk_hash; amount }
  end

  module Finalize_withdrawal = struct
    module Ase_outer_inst = Ase.Make_serializable_ase (struct
      module Ase_system = Ase.Without_length
      module Action_state = Outer_action_state

      module Ase_inst =
        Bridge_inst_mina.Rule_bridge_finalize_withdrawal.Ase_outer_inst
    end)

    module Ase_inner_inst = Ase.Make_serializable_ase (struct
      module Ase_system = Ase.With_length
      module Action_state = Inner_action_state.With_length

      module Ase_inst =
        Bridge_inst_mina.Rule_bridge_finalize_withdrawal.Ase_inner_inst
    end)

    type t = Bridge_inst_mina.Rule_bridge_finalize_withdrawal.Witness.t

    module Withdrawal_params_base = struct
      type t = Bridge_state.Withdrawal_params_base.t

      type serializable =
        { children :
            ( Account_update.Stable.Latest.t
            , Zkapp_command.Digest.Account_update.t
            , Zkapp_command.Digest.Forest.t )
            Zkapp_command.Call_forest.t
        ; amount : Currency.Amount.t
        ; recipient : Public_key.Compressed.t
        }
      [@@deriving yojson]

      let of_serializable ~proof_cache_db
          ({ children; amount; recipient } : serializable) : t =
        { children =
            Zkapp_command.Call_forest.With_hashes.write_all_proofs_to_disk
              ~proof_cache_db children
        ; amount
        ; recipient
        }

      let to_serializable ({ children; amount; recipient } : t) : serializable =
        { children =
            Zkapp_command.Call_forest.With_hashes.read_all_proofs_from_disk
              children
        ; amount
        ; recipient
        }
    end

    type serializable =
      { public_key : Public_key.Compressed.t
      ; may_use_token :
          Bridge_inst_mina.Rule_bridge_finalize_withdrawal.May_use_token.t
      ; outer_authorization_kind : Rule_bridge_finalize_withdrawal.A.t
      ; commit : Commit.t
      ; before_commit : Outer_action_state.t
      ; commit_ase : Ase_outer_inst.serializable
      ; before_withdrawal : Inner_action_state.t
      ; withdrawal_ase : Ase_inner_inst.serializable
      ; prev_next_withdrawal : Checked32.t
      ; withdrawal_params : Withdrawal_params_base.serializable
      ; prev_nonce : Checked32.t
      ; helper_account_new : bool
      }
    [@@deriving yojson]

    let of_serializable ~proof_cache_db
        ({ public_key
         ; may_use_token
         ; outer_authorization_kind
         ; commit
         ; before_commit
         ; commit_ase
         ; before_withdrawal
         ; withdrawal_ase
         ; prev_next_withdrawal
         ; withdrawal_params
         ; prev_nonce
         ; helper_account_new
         } :
          serializable ) ~vk_hash ~helper_token_owner_l1_vk_hash
        ~l2_holder_vk_hash : t =
      { public_key
      ; vk_hash
      ; may_use_token
      ; outer_authorization_kind
      ; commit
      ; before_commit
      ; commit_ase = Ase_outer_inst.of_serializable commit_ase
      ; before_withdrawal
      ; withdrawal_ase = Ase_inner_inst.of_serializable withdrawal_ase
      ; prev_next_withdrawal
      ; withdrawal_params =
          Withdrawal_params_base.of_serializable withdrawal_params
            ~proof_cache_db
      ; helper_token_owner_l1_vk_hash
      ; l2_holder_vk_hash
      ; prev_nonce =
          Mina_numbers.Account_nonce.of_string (Checked32.to_string prev_nonce)
      ; helper_account_new
      }
  end

  module Outer_token_owner = struct
    type t = Bridge_inst_mina.Rule_bridge_outer_token_owner.Witness.t

    type serializable =
      { public_key : Public_key.Compressed.t; a : Account_update.Body.t }
    [@@deriving yojson]

    let of_serializable ({ public_key; a } : serializable) ~vk_hash : t =
      { public_key; vk_hash; a }
  end
end
