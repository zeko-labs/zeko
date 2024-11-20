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
  -> (module Result
        with type out_t = 'out_t
         and type out_var = 'out_var
         and type branches = ( 'first_input
                             , 'branches )
                             Compile_simple_intf.cons_branch )
