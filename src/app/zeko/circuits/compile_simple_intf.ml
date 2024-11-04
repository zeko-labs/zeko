open Snark_params.Tick

type no_prevs = |

type ('var, 'width) one_prev = |

type ('left_var, 'left_width, 'right_var, 'right_width) two_prevs = |

type ('var, 'max_proofs_verified) prev =
  { public_input : 'var
  ; proof : ('max_proofs_verified, 'max_proofs_verified) Pickles.Proof.t V.t
  ; proof_must_verify : Boolean.var
  }

type _ prevs =
  | No_prevs : no_prevs prevs
  | One_prev : ('var, 'width) prev -> ('var, 'width) one_prev prevs
  | Two_prevs :
      (('left_var, 'left_width) prev * ('right_var, 'right_width) prev)
      -> ('left_var, 'left_width, 'right_var, 'right_width) two_prevs prevs

type self_width = Pickles_types.Nat.N2.n

type ('self_var, 'var, 'width) pickles_tag_or_self =
  | Tag :
      ('var, 't, 'width, 'branches) Pickles.Tag.t
      -> ('self_var, 'var, 'width) pickles_tag_or_self
  | Own_tag : ('self_var, 'self_var, self_width) pickles_tag_or_self

type ('self_var, 'prevs) tags =
  | No_tags : ('self_var, no_prevs) tags
  | One_tag :
      ('self_var, 'var, 'width) pickles_tag_or_self
      -> ('self_var, ('var, 'width) one_prev) tags
  | Two_tags :
      ('self_var, 'left_var, 'left_width) pickles_tag_or_self
      * ('self_var, 'right_var, 'right_width) pickles_tag_or_self
      -> ( 'self_var
         , ('left_var, 'left_width, 'right_var, 'right_width) two_prevs )
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
  ; tags : ('out_var, 'prevs) tags
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

type ('branches, 'n_branches) branches_length =
  | Z : (nil_branch, Pickles_types.Nat.z) branches_length
  | S :
      ('branches, 'n_branches) branches_length
      -> ( ('input, 'branches) cons_branch
         , 'n_branches Pickles_types.Nat.s )
         branches_length

type ('out_var, 'out_t, 'branches) result =
  | Result :
      { tag : ('out_var, 'out_t, self_width, 'n_branches) Pickles.Tag.t
      ; provers : ('out_t, 'branches) provers
      ; tag_length : ('branches, 'n_branches) branches_length
      }
      -> ('out_var, 'out_t, 'branches) result
