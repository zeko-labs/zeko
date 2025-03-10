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

  type t = F.t [@@deriving yojson]
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

  type t = Txn_state.update_acc_set_witness =
    { get_account_set_x : unit -> Token_id.t
    ; get_account_set_z : unit -> Token_id.t
    ; get_account_set_x_path : unit -> Path.t
    ; get_account_set_y_path : unit -> Path.t
    }

  type serializable =
    { x : Token_id.t list
    ; z : Token_id.t list
    ; x_path : Path.t list
    ; y_path : Path.t list
    }
  [@@deriving yojson]

  let to_serializable
      ({ get_account_set_x
       ; get_account_set_z
       ; get_account_set_x_path
       ; get_account_set_y_path
       } :
        t ) : serializable =
    let rec fun_to_list f = try f () :: fun_to_list f with _ -> [] in
    { x = fun_to_list get_account_set_x
    ; z = fun_to_list get_account_set_z
    ; x_path = fun_to_list get_account_set_x_path
    ; y_path = fun_to_list get_account_set_y_path
    }

  let of_serializable ({ x; z; x_path; y_path } : serializable) : t =
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
    ; get_account_set_x_path = list_to_fun x_path
    ; get_account_set_y_path = list_to_fun y_path
    }

  let to_yojson : t -> Yojson.Safe.t =
    Fn.compose serializable_to_yojson to_serializable

  let of_yojson json : t Ppx_deriving_yojson_runtime.error_or =
    match serializable_of_yojson json with
    | Ok as_list ->
        Ok (of_serializable as_list)
    | Error e ->
        Error e

  let empty = { x = []; x_path = []; y_path = []; z = [] }

  let add t
      ((x, x_path, y, y_path, z) :
        [ `X of Token_id.t ]
        * [ `X_path of Ledger.Path.t ]
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
    let y_path = match y_path with `Y_path path -> path in
    let z = match z with `Z z -> z in
    { x = t.x @ [ x ]
    ; x_path = t.x_path @ [ mina_path_to_zeko_path x_path ]
    ; y_path = t.y_path @ [ mina_path_to_zeko_path y_path ]
    ; z = t.z @ [ z ]
    }

  let join t1 t2 =
    let t1 = to_serializable t1 in
    let t2 = to_serializable t2 in
    of_serializable
      { x = t1.x @ t2.x
      ; x_path = t1.x_path @ t2.x_path
      ; y_path = t1.y_path @ t2.y_path
      ; z = t1.z @ t2.z
      }
end

module Zkapp_rule_input_witness = struct
  type t = Rule_zkapp_command.Zkapp_rule_input_witness.t =
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
    ; update_acc_set_witness : Acc_set_witness.t
    }
  [@@deriving yojson]
end

module Even_PC = struct
  type t = Zeko_util.Even_PC.t = { public_key : F.t } [@@deriving yojson]
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
  type t = Rule_zkapp_command.Zkapp_rule_input.t =
    { source_ledger : Ledger_hash.t
    ; source_local_state : Local_state.t
    ; sequencer : Even_PC.t
    ; source_acc_set : Account_set.t
    ; witness : Zkapp_rule_input_witness.t
    }
  [@@deriving yojson]
end

module Per_account_update = struct
  type t = Rule_zkapp_command.Per_account_update.t =
    { account_updates : Zkapp_command.Digest.Forest.t
    ; memo_hash : F.t
    ; account_updates_data : Mina_base.Zkapp_command.Call_forest.With_hashes.t
    ; shift_action_state : bool
    }
  [@@deriving yojson]
end

module Verification_key = struct
  type t = Compile_simple.Verification_key.t

  let to_yojson =
    Fn.compose Pickles.Side_loaded.Verification_key.to_yojson
      Compile_simple.Verification_key.to_pickles

  let of_yojson json : t Ppx_deriving_yojson_runtime.error_or =
    match Pickles.Side_loaded.Verification_key.of_yojson json with
    | Ok vk ->
        Ok (Compile_simple.Verification_key.of_pickles vk)
    | Error e ->
        Error e
end

module Zkapp_single_proved_input = struct
  type t = Rule_zkapp_command.Zkapp_single_proved_input.t =
    { base : Zkapp_rule_input.t
    ; vk : Verification_key.t
    ; zkapp_proof : Proof.t
    ; first : Per_account_update.t
    }
  [@@deriving yojson]
end

module Zkapp_single_unproved_input = struct
  type t = Rule_zkapp_command.Zkapp_single_unproved_input.t =
    { base : Zkapp_rule_input.t; first : Per_account_update.t }
  [@@deriving yojson]
end

module Zkapp_double_unproved_input = struct
  type t = Rule_zkapp_command.Zkapp_double_unproved_input.t =
    { base : Zkapp_rule_input.t
    ; first : Per_account_update.t
    ; second : Per_account_update.t
    }
  [@@deriving yojson]
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
  type t = Rule_signed_command.Base_witness.t =
    { ledger_path_handler : Handler.t
    ; update_acc_set_witness : Acc_set_witness.t
    }

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
  type t = Rule_signed_command.Base_input.t =
    { source_ledger : Ledger_hash.t
    ; source_acc_set : Account_set.t
    ; sequencer : Even_PC.t
    ; transaction : Mina_transaction.Transaction_union.t
    ; witness : Base_witness.t
    }

  type serializable =
    { source_ledger : Ledger_hash.t
    ; source_acc_set : Account_set.t
    ; sequencer : Even_PC.t
    ; transaction : Signed_command.t
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
        Mina_transaction.Transaction_union.of_transaction (Command transaction)
    ; witness = Base_witness.of_serializable witness
    }
end

module Command_witness = struct
  module Zkapp_command_segment = struct
    type t =
      | Single_unproved of Zkapp_single_unproved_input.t
      | Double_unproved of Zkapp_double_unproved_input.t
      | Single_proved of Zkapp_single_proved_input.t
    [@@deriving yojson]
  end

  type t =
    | Signed_command of Base_input.serializable
    | Zkapp_command of Zkapp_command_segment.t list
  [@@deriving yojson]
end

module Slot_range = struct
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
  module With_length = struct
    include Ase.With_length

    module Stmt = struct
      type t = Ase.With_length.Stmt.t =
        { action_state : F.t; length : Checked32.t }
      [@@deriving yojson]
    end

    type trans = Ase.With_length.trans = { source : Stmt.t; target : Stmt.t }
    [@@deriving yojson]
  end

  module Without_length = struct
    include Ase.Without_length

    module Stmt = struct
      type t = F.t [@@deriving yojson]
    end

    type trans = Ase.Without_length.trans = { source : Stmt.t; target : Stmt.t }
    [@@deriving yojson]
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

    val get_iterations : int
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
  type t = F.t [@@deriving yojson]

  module With_length = struct
    type t = Action_state.With_length.t = { state : F.t; length : Checked32.t }
    [@@deriving yojson]
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
    type t = Rule_inner_sync.Witness.t =
      { public_key : Public_key.Compressed.t; vk_hash : F.t; ase : Ase_inst.t }

    type serializable =
      { public_key : Public_key.Compressed.t; ase : Ase_inst.serializable }
    [@@deriving yojson]

    let of_serializable ({ public_key; ase } : serializable) ~vk_hash : t =
      { public_key; vk_hash; ase = Ase_inst.of_serializable ase }
  end
end

module Outer_commit = struct
  module Path = Make_serializable_path (struct
    module PathStep = struct
      type t = Outer_rules.Rule_commit_inst.PathElt.t

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
    type t = Outer_rules.Rule_commit_inst.Witness.t =
      { txn_snark : Txn_snark.t
      ; public_key : Public_key.Compressed.t
      ; vk_hash : F.t
      ; verify_both_ases : Verify_both_ases.t
      ; old_inner_acc : Account.t
      ; old_inner_acc_path : Path.t
      ; new_inner_acc : Account.t
      ; new_inner_acc_path : Path.t
      ; da_signature : Signature_lib.Schnorr.Chunked.Signature.t
      ; da_key : Even_PC.t
      }

    type serializable =
      { txn_snark : Txn_snark.serializable
      ; public_key : Public_key.Compressed.t
      ; verify_both_ases : Verify_both_ases.serializable
      ; old_inner_acc : Account.t
      ; old_inner_acc_path : Path.t
      ; new_inner_acc : Account.t
      ; new_inner_acc_path : Path.t
      ; da_signature : Signature.t
      ; da_key : Even_PC.t
      }
    [@@deriving yojson]

    let of_serializable
        ({ txn_snark
         ; public_key
         ; verify_both_ases
         ; old_inner_acc
         ; old_inner_acc_path
         ; new_inner_acc
         ; new_inner_acc_path
         ; da_signature
         ; da_key
         } :
          serializable ) ~vk_hash : t =
      { txn_snark = Txn_snark.of_serializable txn_snark
      ; public_key
      ; vk_hash
      ; verify_both_ases = Verify_both_ases.of_serializable verify_both_ases
      ; old_inner_acc
      ; old_inner_acc_path
      ; new_inner_acc
      ; new_inner_acc_path
      ; da_signature
      ; da_key
      }
  end
end
