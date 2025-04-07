module Proof : sig
  type t [@@deriving yojson]

  val to_pickles : t -> Pickles.Side_loaded.Proof.t

  val of_pickles : Pickles.Side_loaded.Proof.t -> t
end

type 'tag_var tag

val force_tag : 'tag_var tag -> unit Promise.t

module Verification_key : sig
  type t

  type var

  val typ : (var, t) Snark_params.Tick.Typ.t

  val of_pickles : Pickles.Side_loaded.Verification_key.t -> t

  val to_pickles : t -> Pickles.Side_loaded.Verification_key.t

  val of_tag : 'tag_var tag -> t Promise.t

  val hash : t -> Snark_params.Tick.Field.t

  val hash_var : var -> Snark_params.Tick.Field.Var.t
end

include module type of Compile_simple_intf.Make (struct
  type nonrec proof = Proof.t

  type nonrec vk_t = Verification_key.t

  type nonrec vk_var = Verification_key.var

  type nonrec 'tag_var tag = 'tag_var tag
end)

val compile :
     ?wrap_domain:[ `N13 | `N14 | `N15 ]
  -> name:string
  -> branches:
       ( 'out_var
       , ('first_input, 'branches) cons_branch
       , 'n_available_branches )
       Branches.t
  -> out_typ:('out_var, 'out_t) Snark_params.Tick.Typ.t
  -> unit
  -> (module Result
        with type out_t = 'out_t
         and type out_var = 'out_var
         and type branches = ('first_input, 'branches) cons_branch )
