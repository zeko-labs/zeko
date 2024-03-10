open Ppxlib

let deriver_name = "snarky"

let str_decl ~loc (decl : type_declaration) : structure =
  let open Ast_builder.Default in
  match decl with
  | { ptype_kind =
        Ptype_record fields
    ; ptype_name = { txt = "t" | "var"; _ } as name
      (* FIXME [name]
         For some reason when passed to the deriver the name if `t` becomes `var`.
         This is certainly a bug. It makes no sense to me.
         The code otherwise seems to work correctly. It's probably a small oversight somewhere.
      *)
    ; _
    } ->
      let modules : module_expr list =
        List.map
          (fun { pld_type; pld_loc = loc; _ } ->
            match pld_type.ptyp_desc with
            (* FIXME: Support matching on forms F(M).t *)
            | Ptyp_constr ({ txt = Ldot (m, "t"); loc }, []) ->
                pmod_ident ~loc { txt = m; loc }
            | _ ->
                pmod_extension ~loc
                @@ Location.error_extensionf ~loc
                     "Must be of form <M>.t for deriving %s" deriver_name )
          fields
      in
      let fields' =
        List.map
          (fun field ->
            match field.pld_type.ptyp_desc with
            | Ptyp_constr ({ txt = Ldot (m, "t"); loc = tloc }, []) ->
                { field with
                  pld_type =
                    { field.pld_type with
                      ptyp_desc =
                        Ptyp_constr ({ txt = Ldot (m, "var"); loc = tloc }, [])
                    }
                }
            | _ ->
                { field with
                  pld_type =
                    ptyp_extension ~loc
                    @@ Location.error_extensionf ~loc
                         "Cannot derive %s for this type: Must be of form \
                          <MODULE>.t"
                         deriver_name
                } )
          fields
      in
      let decl' =
        { decl with
          ptype_name = { name with txt = "var" }
        ; ptype_kind = Ptype_record fields'
        ; ptype_loc = loc
        ; ptype_attributes = []
        }
      in
      let tuple_typ =
        List.fold_right
          (fun m acc ->
            [%expr
              Typ.tuple2
                [%e pexp_letmodule ~loc { txt = Some "M"; loc } m [%expr M.typ]]
                [%e acc]] )
          modules [%expr Typ.unit]
      in
      let pat_tuple : pattern =
        List.fold_right
          (fun field acc -> [%pat? [%p ppat_var ~loc field.pld_name], [%p acc]])
          fields
          [%pat? ()]
      in
      let con_tuple : expression =
        List.fold_right
          (fun field acc ->
            [%expr
              [%e
                pexp_ident ~loc
                  { field.pld_name with txt = Lident field.pld_name.txt }]
              , [%e acc]] )
          fields [%expr ()]
      in
      let pat_record_var : pattern =
        ppat_record ~loc
          (List.map
             (fun field ->
               ( { loc = loc; txt = Lident field.pld_name.txt }
               , ppat_var ~loc field.pld_name ) )
             fields )
          Closed
      in
      let pat_record : pattern =
        ppat_constraint ~loc pat_record_var
          (ptyp_constr ~loc { loc; txt = Lident "t" } [])
      in
      let con_record_var : expression =
        pexp_record ~loc
          (List.map
             (fun field ->
               let i = { loc; txt = Lident field.pld_name.txt } in
               (i, pexp_ident ~loc i) )
             fields )
          None
      in
      let con_record : expression =
        pexp_constraint ~loc con_record_var
          (ptyp_constr ~loc { loc; txt = Lident "t" } [])
      in
      let typ =
        [%expr
          Typ.transport [%e tuple_typ]
            ~there:(fun [%p pat_record] -> [%e con_tuple])
            ~back:(fun [%p pat_tuple] -> [%e con_record])]
      in
      let typ =
        [%expr
          Typ.transport_var [%e typ]
            ~there:(fun [%p pat_record_var] -> [%e con_tuple])
            ~back:(fun [%p pat_tuple] -> [%e con_record_var])]
      in
      pstr_type ~loc Recursive [ decl' ] :: [%str
        let typ = [%e typ]
      (* FIXME: add this and fix it *)
      (* module Constant = struct type nonrec t = t = {...} end *)
      ]
  | { ptype_loc = loc; ptype_kind; ptype_name; _ } ->
      let i =
        match ptype_kind with
        | Ptype_abstract ->
            0
        | Ptype_variant _ ->
            1
        | Ptype_record _ ->
            2
        | Ptype_open ->
            3
      in
      [ pstr_extension ~loc
          (Location.error_extensionf ~loc
             "Cannot derive %s for this type: Must be record type named t %d %s"
             deriver_name i ptype_name.txt )
          []
      ]

let sig_decl ~loc (decl : type_declaration) : signature =
  let open Ast_builder.Default in
  match decl with
  | { ptype_kind = Ptype_record fields
    ; ptype_name = { txt = "t" | "var"; _ } as name (* FIXME: see [name] note *)
    ; _
    } ->
      let fields' =
        List.map
          (fun field ->
            match field.pld_type.ptyp_desc with
            | Ptyp_constr ({ txt = Ldot (m, "t"); loc = tloc }, []) ->
                { field with
                  pld_type =
                    { field.pld_type with
                      ptyp_desc =
                        Ptyp_constr ({ txt = Ldot (m, "var"); loc = tloc }, [])
                    }
                }
            | _ ->
                { field with
                  pld_type =
                    ptyp_extension ~loc
                    @@ Location.error_extensionf ~loc
                         "Cannot derive %s for this type" deriver_name
                } )
          fields
      in
      let decl' =
        { decl with
          ptype_name = { name with txt = "var" }
        ; ptype_kind = Ptype_record fields'
        ; ptype_loc = loc
        ; ptype_attributes = []
        }
      in
      psig_type ~loc Recursive [ decl' ] :: [%sig: val typ : (var, t) Typ.t]
  | { ptype_kind = Ptype_abstract
    ; ptype_name = { txt = "t" | "var"; _ } as name (* FIXME: see [name] note *)
    ; _
    } ->
      let decl' =
        { decl with
          ptype_name = { name with txt = "var" }
        ; ptype_loc = loc
        ; ptype_attributes = []
        }
      in
      psig_type ~loc Recursive [ decl' ] :: [%sig: val typ : (var, t) Typ.t]
  | { ptype_loc = loc; _ } ->
      [ psig_extension ~loc
          (Location.error_extensionf ~loc "Cannot derive %s signature for this type; must be record or abstract"
             deriver_name )
          []
      ]

let str_type_decl ~loc ~path:_ (_rec_flag, decls) : structure =
  List.map (fun decl -> str_decl ~loc decl) decls |> List.concat

let sig_type_decl ~loc ~path:_ (_rec_flag, decls) : signature =
  List.map (fun decl -> sig_decl ~loc decl) decls |> List.concat

let deriver =
  Deriving.add
    ~str_type_decl:(Deriving.Generator.make_noarg str_type_decl)
    ~sig_type_decl:(Deriving.Generator.make_noarg sig_type_decl)
    "snarky"
