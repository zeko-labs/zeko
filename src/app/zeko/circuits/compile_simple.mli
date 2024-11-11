include module type of Compile_simple_intf

val compile :
     ?override_wrap_domain:[ `N0 | `N1 | `N2 ]
  -> name:string
  -> branches:
       ( 'out_var
       , ('first_input, 'branches) Compile_simple_intf.cons_branch
       , 'n_available_branches )
       Compile_simple_intf.Branches.t
  -> out_typ:('out_var, 'out_t) Snark_params.Tick.Typ.t
  -> unit
  -> ( 'out_var
     , 'out_t
     , ('first_input, 'branches) Compile_simple_intf.cons_branch )
     Compile_simple_intf.result
     Promise.t
