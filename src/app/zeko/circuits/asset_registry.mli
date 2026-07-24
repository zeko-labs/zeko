[@@@warning "-67"]

open Mina_base
open Snark_params.Tick
open Zeko_util

module PC = Signature_lib.Public_key.Compressed
module Token_id : sig
  include module type of Mina_base.Token_id

  type var = Checked.t
end

(** The immutable, versioned identity shared by the OCaml circuit, settlement
    adapter, Solidity registry, indexer, and SDK. Field order is part of the V1
    wire and leaf commitment. *)
module Asset_record : sig
  type t =
    { schema_version : Checked32.t
    ; registry_index : Checked32.t
    ; asset_id_high : F.t
    ; asset_id_low : F.t
    ; ethereum_token_address : F.t
    ; token_owner_l2 : PC.t
    ; token_id_l2 : Token_id.t
    ; decimals : Checked32.t
    ; inventory_cap : Currency.Amount.t
    ; mft_standard_vk_id : F.t
    ; vault_public_key : PC.t
    ; universal_bridge_vk_id : F.t
    }
  [@@deriving snarky, yojson, equal]

  val commitment : t -> F.t

  val commitment_var : var -> F.var Checked.t

  val derived_token_id : t -> Token_id.t
end

module Registry_state : sig
  type t =
    { root : F.t; leaf_count : Checked32.t; schema_version : Checked32.t }
  [@@deriving snarky, yojson, equal]

  type fine =
    { root : F.var option
    ; leaf_count : Checked32.var option
    ; schema_version : Checked32.var option
    }

  val fine : fine -> Fine.t
end

module Path : sig
  type t = F.t list

  type var = F.var list

  val typ : (var, t) Typ.t

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
end

module Output : sig
  type calls =
    ( Account_update.t
    , Zkapp_command.Digest.Account_update.t
    , Zkapp_command.Digest.Forest.t )
    Zkapp_command.Call_forest.t

  type auxiliary =
    Account_update.Body.t
    * Zkapp_command.Digest.Account_update.t
    * calls

  type t = Zkapp_statement.t * auxiliary
end

(** Pure append-only tree implementation used by witness preparation and tests.
    Paths are always relative to the current root and must be refreshed after an
    append. *)
module Merkle_list : sig
  type t

  val empty : unit -> t

  val count : t -> int

  val root : t -> F.t

  val path : t -> index:int -> Path.t

  val append_exn : t -> Asset_record.t -> t

  val verify : root:F.t -> Asset_record.t -> Path.t -> bool

  val empty_root : F.t
end

module type CONFIG = sig
  val registry_public_key : PC.t

  val schema_version : Checked32.t

  val approved_mft_standard_vk_id : F.t

  val universal_bridge_vk_id : F.t

  val vault_public_key : PC.t

  val chain_l2 : Mina_signature_kind.t
end

(** Verified-registry seam. A caller receives an abstract [Verified_asset.t]
    only after record invariants and Merkle membership have been constrained.
    [authenticated_registry_call] is the account-state precondition that binds
    the witnessed root/count/version to the dedicated registry zkApp. *)
module Make (Config : CONFIG) () : sig
  module Membership_witness : sig
    type t =
      { state : Registry_state.t; record : Asset_record.t; path : Path.t }
    [@@deriving snarky, yojson]
  end

  module Verified_asset : sig
    type t

    val record : t -> Asset_record.var

    val token_id : t -> Token_id.Checked.t

    val authenticated_registry_call : t -> Account_update.Checked.t
  end

  val verify : Membership_witness.var -> Verified_asset.t Checked.t

  module Scan : sig
    module Definition : sig
      module Stmt : sig
        type t =
          { old_root : F.t
          ; leaf_count : Checked32.t
          ; candidate : Asset_record.t
          ; next_expected_index : Checked32.t
          ; traversed_count : Checked32.t
          }
        [@@deriving snarky]
      end

      module Elem : sig
        type t =
          { active : Boolean.t; record : Asset_record.t; path : Path.t }
        [@@deriving snarky]
      end

      module Init : sig
        type t =
          { old_state : Registry_state.t; candidate : Asset_record.t }
        [@@deriving snarky]
      end

      val init :
        check:Boolean.var option -> Init.var -> Stmt.var Checked.t

      val step : Elem.var -> Stmt.var -> Stmt.var Checked.t

      val dummy_elem : Elem.t

      val leaf_iterations : int

      val leaf_option_iterations : int

      val extend_iterations : int

      val extend_option_iterations : int

      val wrap_domain : [ `N13 | `N14 | `N15 ] option
    end

    type trans =
      { source : Definition.Stmt.t; target : Definition.Stmt.t }

    type t := trans * Proof.t

    val leaf :
      (Definition.Elem.t list * Definition.Stmt.t -> t Promise.t) lazy_t

    val leaf_option :
      (Definition.Elem.t list * Definition.Stmt.t -> t Promise.t) lazy_t

    val extend : (Definition.Elem.t list * t -> t Promise.t) lazy_t

    val extend_option : (Definition.Elem.t list * t -> t Promise.t) lazy_t

    type merge_input =
      { left : trans
      ; left_proof : Proof.t
      ; right : trans
      ; right_proof : Proof.t
      }

    val merge : (merge_input -> t Promise.t) lazy_t

    type tag_var

    val tag : tag_var Compile_simple.tag lazy_t

    module Make (Inputs : sig
      val get_iterations : int
    end) : sig
      include SnarkType

      val get_full :
           ?check:Boolean.var
        -> var
        -> ( [ `Source of Definition.Stmt.var ]
           * [ `Target of Definition.Stmt.var ]
           * tag_var Compile_simple.prev )
           Checked.t

      val make :
           proof_source:Definition.Stmt.t
        -> proof_target:Definition.Stmt.t
        -> ?proof:Proof.t
        -> Definition.Init.t
        -> Definition.Elem.t list
        -> t
    end
  end

  module Scan_inst : sig
    include SnarkType

    val make :
         proof_source:Scan.Definition.Stmt.t
      -> proof_target:Scan.Definition.Stmt.t
      -> ?proof:Proof.t
      -> Scan.Definition.Init.t
      -> Scan.Definition.Elem.t list
      -> t
  end

  module Register : sig
    module Witness : sig
      type t =
        { scan : Scan_inst.t
        ; append_path : Path.t
        ; registry_vk_hash : F.t
        }
      [@@deriving snarky]
    end

  end

  type registry_tag_var

  val registry_tag : registry_tag_var Compile_simple.tag lazy_t

  val register :
    (Register.Witness.t -> (Output.t * Proof.t) Promise.t) lazy_t
end
