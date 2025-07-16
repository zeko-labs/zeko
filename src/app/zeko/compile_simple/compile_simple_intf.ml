open Snark_params.Tick

module Make (Inputs : sig
  type proof

  type 'var tag

  type vk_t

  type vk_var
end) =
struct
  open Inputs

  type no_prevs = |

  type 'var one_prev = |

  type 'var one_prev_sideloaded = |

  type ('left_var, 'right_var) two_prevs = |

  type ('left_var, 'right_var) two_prevs_one_sideloaded = |

  (* This needs a constructor otherwise warnings appear. *)
  type ('left_var, 'right_var) two_prevs_sideloaded =
    | Internal_two_prevs_sideloaded

  type 'var prev =
    { public_input : 'var; proof : proof V.t; proof_must_verify : Boolean.var }

  type 'var prev_sideloaded =
    { public_input : 'var
    ; proof : proof V.t
    ; proof_must_verify : Boolean.var
    ; vk : vk_var
    }

  type _ prevs =
    | No_prevs : no_prevs prevs
    | One_prev : 'var prev -> 'var one_prev prevs
    | One_prev_sideloaded :
        'var prev_sideloaded
        -> 'var one_prev_sideloaded prevs
    | Two_prevs :
        ('left_var prev * 'right_var prev)
        -> ('left_var, 'right_var) two_prevs prevs
    | Two_prevs_sideloaded :
        ('left_var prev_sideloaded * 'right_var prev_sideloaded)
        -> ('left_var, 'right_var) two_prevs_sideloaded prevs
    | Two_prevs_one_sideloaded :
        ('left_var prev_sideloaded * 'right_var prev)
        -> ('left_var, 'right_var) two_prevs_one_sideloaded prevs

  type ('var, 't, 'input) sideloaded_tag =
    { sideloaded_tag_name : string
    ; typ : ('var, 't) Typ.t
    ; extract_vk : 'input -> vk_t
    }

  type no_sideloaded = |

  type one_sideloaded = |

  type two_sideloaded = |

  type ('self_var, 'prevs, 'input) tags =
    | No_tags : ('self_var, no_prevs, 'input) tags
    | One_tag : 'var tag -> ('self_var, 'var one_prev, 'input) tags
    | One_tag_own : ('self_var, 'self_var one_prev, 'input) tags
    | One_tag_sideloaded :
        ('var, 't, 'input) sideloaded_tag
        -> ('self_var, 'var one_prev_sideloaded, 'input) tags
    | Two_tags :
        'left_var tag * 'right_var tag
        -> ('self_var, ('left_var, 'right_var) two_prevs, 'input) tags
    | Two_tags_one_sideloaded :
        ('left_var, 'left_t, 'input) sideloaded_tag * 'right_var tag
        -> ( 'self_var
           , ('left_var, 'right_var) two_prevs_one_sideloaded
           , 'input )
           tags
    | Two_tags_one_own :
        'right_var tag
        -> ('self_var, ('self_var, 'right_var) two_prevs, 'input) tags
    | Two_tags_sideloaded :
        ('left_var, 'left_t, 'input) sideloaded_tag
        * ('right_var, 'right_t, 'input) sideloaded_tag
        -> ( 'self_var
           , ('left_var, 'right_var) two_prevs_sideloaded
           , 'input )
           tags
    | Two_tags_sideloaded_own :
        ('left_var, 'left_t, 'input) sideloaded_tag
        -> ( 'self_var
           , ('left_var, 'self_var) two_prevs_one_sideloaded
           , 'input )
           tags
    | Two_tags_own : ('self_var, ('self_var, 'self_var) two_prevs, 'input) tags

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
        ('input -> ('out_t * proof) Promise.t) * ('out_t, 'branches) provers
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
          -> ( 'out_var
             , ('input, 'branches) cons_branch
             , 'n_available_branches )
             t
  end

  module type Result = sig
    (* inputs *)

    type out_t

    type out_var

    type branches

    (* outputs *)

    type tag_var

    type tag_t

    val tag : tag_var tag

    val provers : (out_t, branches) provers

    type t

    type var

    val typ : (var, t) Typ.t

    (* TODO: maybe this should take a default output,
       and not a check, such that if the underlying thing
       isn't proven, we just get the default. *)
    val get : ?check:Boolean.var -> var -> (out_var * tag_var prev) Checked.t

    val make_unchecked : ?proof:proof -> out_t -> t
  end
end
