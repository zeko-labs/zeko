open Snark_params.Tick

type no_prevs = |

type ('var, 'width) one_prev = |

type 'var one_prev_sideloaded = |

type ('left_var, 'left_width, 'right_var, 'right_width) two_prevs = |

type ('left_var, 'right_var, 'right_width) two_prevs_one_sideloaded = |

(* This needs a constructor otherwise warnings appear. *)
type ('left_var, 'right_var) two_prevs_sideloaded =
  | Internal_two_prevs_sideloaded

type ('var, 'max_proofs_verified) prev =
  { public_input : 'var
  ; proof : ('max_proofs_verified, 'max_proofs_verified) Pickles.Proof.t V.t
  ; proof_must_verify : Boolean.var
  }

type sideloaded_width = Pickles_types.Nat.N2.n

type 'var prev_sideloaded =
  { public_input : 'var
  ; proof : (sideloaded_width, sideloaded_width) Pickles.Proof.t V.t
  ; proof_must_verify : Boolean.var
  ; vk : Pickles.Side_loaded.Verification_key.Checked.t
  }

type _ prevs =
  | No_prevs : no_prevs prevs
  | One_prev : ('var, 'width) prev -> ('var, 'width) one_prev prevs
  | One_prev_sideloaded : 'var prev_sideloaded -> 'var one_prev_sideloaded prevs
  | Two_prevs :
      (('left_var, 'left_width) prev * ('right_var, 'right_width) prev)
      -> ('left_var, 'left_width, 'right_var, 'right_width) two_prevs prevs
  | Two_prevs_sideloaded :
      ('left_var prev_sideloaded * 'right_var prev_sideloaded)
      -> ('left_var, 'right_var) two_prevs_sideloaded prevs
  | Two_prevs_one_sideloaded :
      ('left_var prev_sideloaded * ('right_var, 'right_width) prev)
      -> ('left_var, 'right_var, 'right_width) two_prevs_one_sideloaded prevs

type self_width = Pickles_types.Nat.N2.n

type ('var, 't, 'input) sideloaded_tag =
  { sideloaded_tag_name : string
  ; typ : ('var, 't) Typ.t
  ; extract_vk : 'input -> Pickles.Side_loaded.Verification_key.t
  }

type no_sideloaded = |

type one_sideloaded = |

type two_sideloaded = |

type ('self_var, 'prevs, 'input) tags =
  | No_tags : ('self_var, no_prevs, 'input) tags
  | One_tag :
      ('var, 't, 'width, 'branches) Pickles.Tag.t
      -> ('self_var, ('var, 'width) one_prev, 'input) tags
  | One_tag_own : ('self_var, ('self_var, self_width) one_prev, 'input) tags
  | One_tag_sideloaded :
      ('var, 't, 'input) sideloaded_tag
      -> ('self_var, 'var one_prev_sideloaded, 'input) tags
  | Two_tags :
      ('left_var, 'left_t, 'left_width, 'left_branches) Pickles.Tag.t
      * ('right_var, 'right_t, 'right_width, 'right_branches) Pickles.Tag.t
      -> ( 'self_var
         , ('left_var, 'left_width, 'right_var, 'right_width) two_prevs
         , 'input )
         tags
  | Two_tags_one_sideloaded :
      ('left_var, 'left_t, 'input) sideloaded_tag
      * ('right_var, 'right_t, 'right_width, 'right_branches) Pickles.Tag.t
      -> ( 'self_var
         , ('left_var, 'right_var, 'right_width) two_prevs_one_sideloaded
         , 'input )
         tags
  | Two_tags_one_own :
      ('right_var, 'right_t, 'right_width, 'right_branches) Pickles.Tag.t
      -> ( 'self_var
         , ('self_var, self_width, 'right_var, 'right_width) two_prevs
         , 'input )
         tags
  | Two_tags_sideloaded :
      ('left_var, 'left_t, 'input) sideloaded_tag
      * ('right_var, 'right_t, 'input) sideloaded_tag
      -> ('self_var, ('left_var, 'right_var) two_prevs_sideloaded, 'input) tags
  | Two_tags_sideloaded_own :
      ('left_var, 'left_t, 'input) sideloaded_tag
      -> ( 'self_var
         , ('left_var, 'self_var, self_width) two_prevs_one_sideloaded
         , 'input )
         tags
  | Two_tags_own
      : ( 'self_var
        , ('self_var, self_width, 'self_var, self_width) two_prevs
        , 'input )
        tags

type ('var, 'prevs) main_return = { out : 'var; prevs : 'prevs prevs }

type max_branches = |

type _ available_branch = |

type all_branches_available =
  max_branches available_branch available_branch available_branch
  available_branch
  available_branch
  available_branch
  available_branch
  available_branch

type nil_branch = |

type ('x, 'xs) cons_branch = |

type ('out_t, 'branches) provers =
  | [] : ('out_var, nil_branch) provers
  | ( :: ) :
      ('input -> ('out_t * Pickles.Side_loaded.Proof.t) Promise.t)
      * ('out_t, 'branches) provers
      -> ('out_t, ('input, 'branches) cons_branch) provers

type ('input, 'out_var, 'prevs) branch =
  { branch_name : string
  ; tags : ('out_var, 'prevs, 'input) tags
  ; main : 'input V.t -> ('out_var, 'prevs) main_return Checked.t
  }

module Branches = struct
  type ('out_var, 'branches, 'n_available_branches) t =
    | [] : ('out_var, nil_branch, all_branches_available) t
    | ( :: ) :
        ('input, 'out_var, 'prevs) branch
        * ('out_var, 'branches, 'n_available_branches available_branch) t
        -> ('out_var, ('input, 'branches) cons_branch, 'n_available_branches) t
end

type ('branches, 'n_branches) branches_length =
  | Z : (nil_branch, Pickles_types.Nat.z) branches_length
  | S :
      ('branches, 'n_branches) branches_length
      -> ( ('input, 'branches) cons_branch
         , 'n_branches Pickles_types.Nat.s )
         branches_length

module type Result = sig
  (* inputs *)

  type out_t

  type out_var

  type branches

  (* outputs *)

  type n_branches

  type tag_var

  type tag_t

  val tag_branches : (branches, n_branches) branches_length
  
  val tag : (tag_var, tag_t, self_width, n_branches) Pickles.Tag.t
  
  val provers : (out_t, branches) provers

  type t

  type var

  val typ : (var, t) Typ.t

  val get :
       ?check:Boolean.var
    -> var
    -> (out_var * (tag_var, self_width) prev) Checked.t

  val make_unchecked : ?proof:Pickles.Side_loaded.Proof.t -> out_t -> t
end
