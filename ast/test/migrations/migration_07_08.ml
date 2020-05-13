open Ppx_ast

let ok () = print_string "OK"
let ko () = print_string "KO"

let pos pos_fname =
  Astlib.Position.{pos_fname; pos_lnum = 0; pos_bol = 0; pos_cnum = 0}

let loc n =
  let pos = pos n in
  Astlib.Location.{loc_start = pos; loc_end = pos; loc_ghost = false}

let%expect_test "upgrade attribute" =
  let attribute_07 =
    let open V4_07 in
    Attribute.create
      ({txt = "x"; loc = loc "name_loc"}, Payload.pstr (Structure.create []))
  in
  ( match V4_08.Attribute.to_concrete attribute_07 with
    | { attr_loc = {loc_start = {pos_fname = "name_loc"; _}; _}; _ } -> ok ()
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "downgrade attribute" =
  let attribute_08 =
    let open V4_08 in
    Attribute.create
      ~attr_name:{txt = "x"; loc = loc "name_loc"}
      ~attr_payload:(Payload.pstr (Structure.create []))
      ~attr_loc:(loc "attr_loc")
  in
  ( match V4_07.Attribute.to_concrete attribute_08 with
    | ({loc = {loc_start = {pos_fname = "name_loc"; _}; _}; _}, _payload) ->
      ok ()
    | _  -> ko () );
  [%expect {|OK|}]

let%expect_test "upgrade core_type" =
  let core_type_07 =
    let open V4_07 in
    Core_type.create
      ~ptyp_loc:(loc "a")
      ~ptyp_attributes:(Attributes.create [])
      ~ptyp_desc:Core_type_desc.ptyp_any
  in
  ( match V4_08.Core_type.to_concrete core_type_07 with
    | { ptyp_loc_stack = []; _ } -> ok ()
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "downgrade core_type" =
  let core_type_08 =
    let open V4_08 in
    Core_type.create
      ~ptyp_loc:(loc "a")
      ~ptyp_attributes:(Attributes.create [])
      ~ptyp_desc:Core_type_desc.ptyp_any
      ~ptyp_loc_stack:[loc "b"; loc "c"]
  in
  ( match V4_07.Core_type.to_concrete core_type_08 with
    | _ -> ok () );
  [%expect {|OK|}]

let%expect_test "upgrade rinherit" =
  let rinherit_07 =
    let open V4_07 in
    Row_field.rinherit (ptyp_any ~loc:(loc "a"))
  in
  let open V4_08 in
  ( match Row_field.to_concrete rinherit_07 with
    | { prf_loc = { loc_start = { pos_fname = "a"; _ }; _ }
      ; prf_attributes
      ; prf_desc } ->
      let concrete_attr = Attributes.to_concrete prf_attributes in
      let concrete_desc = Row_field_desc.to_concrete prf_desc in
      ( match concrete_attr, concrete_desc with
        | [], Rinherit _ -> ok ()
        | _ -> ko () )
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "upgrade rtag" =
  let rtag_07 =
    let open V4_07 in
    Row_field.rtag
      {txt = "A"; loc = loc "tag"}
      ( Attributes.create
         [ Attribute.create
             ( {txt = "x"; loc = loc "attr"}
             , (Payload.pstr (Structure.create [])) ) ] )
      true
      []
  in
  let open V4_08 in
  ( match Row_field.to_concrete rtag_07 with
    | { prf_loc = { loc_start = { pos_fname = "tag"; _ }; _ }
      ; prf_attributes
      ; prf_desc } ->
      let concrete_attr = Attributes.to_concrete prf_attributes in
      let concrete_desc = Row_field_desc.to_concrete prf_desc in
      ( match concrete_attr, concrete_desc with
        | [ _ ], Rtag (_, true, []) -> ok ()
        | _ -> ko () )
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "downgrade rinherit" =
  let rinherit_08 =
    let open V4_08 in
    Row_field.create
      ~prf_loc:(loc "a")
      ~prf_attributes:(Attributes.create [])
      ~prf_desc:(Row_field_desc.rinherit (ptyp_any ~loc:(loc "type")))
  in
  ( match V4_07.Row_field.to_concrete rinherit_08 with
    | Rinherit _ -> ok ()
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "downgrade rtag" =
  let rtag_08 =
    let open V4_08 in
    Row_field.create
      ~prf_loc:(loc "rwo_field")
      ~prf_attributes:
        ( Attributes.create
            [ Attribute.create
                ~attr_name:{txt = "attr"; loc = loc "attr_name"}
                ~attr_loc:(loc "attr")
                ~attr_payload:(Payload.pstr (Structure.create [])) ] )
      ~prf_desc:(Row_field_desc.rtag {txt = "A"; loc = loc "tag"} true [])
  in
  let open V4_07 in
  ( match Row_field.to_concrete rtag_08 with
    | Rtag (_, attributes, true, []) ->
      ( match Attributes.to_concrete attributes with
        | [ _ ] -> ok ()
        | _ -> ko () )
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "upgrade oinherit" =
  let oinherit_07 =
    let open V4_07 in
    Object_field.oinherit (ptyp_any ~loc:(loc "a"))
  in
  let open V4_08 in
  ( match Object_field.to_concrete oinherit_07 with
    | { pof_loc = { loc_start = { pos_fname = "a"; _ }; _ }
      ; pof_attributes
      ; pof_desc } ->
      let concrete_attr = Attributes.to_concrete pof_attributes in
      let concrete_desc = Object_field_desc.to_concrete pof_desc in
      ( match concrete_attr, concrete_desc with
        | [], Oinherit _ -> ok ()
        | _ -> ko () )
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "upgrade otag" =
  let otag_07 =
    let open V4_07 in
    Object_field.otag
      {txt = "A"; loc = loc "tag"}
      ( Attributes.create
         [ Attribute.create
             ( {txt = "x"; loc = loc "attr"}
             , (Payload.pstr (Structure.create [])) ) ] )
      (ptyp_any ~loc:(loc "typ"))
  in
  let open V4_08 in
  ( match Object_field.to_concrete otag_07 with
    | { pof_loc = { loc_start = { pos_fname = "tag"; _ }; _ }
      ; pof_attributes
      ; pof_desc } ->
      let concrete_attr = Attributes.to_concrete pof_attributes in
      let concrete_desc = Object_field_desc.to_concrete pof_desc in
      ( match concrete_attr, concrete_desc with
        | [ _ ], Otag _ -> ok ()
        | _ -> ko () )
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "downgrade oinherit" =
  let oinherit_08 =
    let open V4_08 in
    Object_field.create
      ~pof_loc:(loc "a")
      ~pof_attributes:(Attributes.create [])
      ~pof_desc:(Object_field_desc.oinherit (ptyp_any ~loc:(loc "type")))
  in
  ( match V4_07.Object_field.to_concrete oinherit_08 with
    | Oinherit _ -> ok ()
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "downgrade otag" =
  let oinherit_08 =
    let open V4_08 in
    Object_field.create
      ~pof_loc:(loc "rwo_field")
      ~pof_attributes:
        ( Attributes.create
            [ Attribute.create
                ~attr_name:{txt = "attr"; loc = loc "attr_name"}
                ~attr_loc:(loc "attr")
                ~attr_payload:(Payload.pstr (Structure.create [])) ] )
      ~pof_desc:
        ( Object_field_desc.otag
            {txt = "A"; loc = loc "tag"}
            (ptyp_any ~loc:(loc "typ")) )
  in
  let open V4_07 in
  ( match Object_field.to_concrete oinherit_08 with
    | Otag (_, attributes, _) ->
      ( match Attributes.to_concrete attributes with
        | [ _ ] -> ok ()
        | _ -> ko () )
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "upgrade pattern" =
  let pattern_07 =
    let open V4_07 in
    Pattern.create
      ~ppat_loc:(loc "a")
      ~ppat_attributes:(Attributes.create [])
      ~ppat_desc:Pattern_desc.ppat_any
  in
  ( match V4_08.Pattern.to_concrete pattern_07 with
    | { ppat_loc_stack = []; _ } -> ok ()
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "downgrade pattern" =
  let pattern_08 =
    let open V4_08 in
    Pattern.create
      ~ppat_loc:(loc "a")
      ~ppat_attributes:(Attributes.create [])
      ~ppat_desc:Pattern_desc.ppat_any
      ~ppat_loc_stack:[loc "b"; loc "c"]
  in
  ( match V4_07.Pattern.to_concrete pattern_08 with
    | _ -> ok () );
  [%expect {|OK|}]

let%expect_test "upgrade expression" =
  let expression_07 =
    let open V4_07 in
    Expression.create
      ~pexp_loc:(loc "a")
      ~pexp_attributes:(Attributes.create [])
      ~pexp_desc:Expression_desc.pexp_unreachable
  in
  ( match V4_08.Expression.to_concrete expression_07 with
    | { pexp_loc_stack = []; _ } -> ok ()
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "downgrade expression" =
  let expression_08 =
    let open V4_08 in
    Expression.create
      ~pexp_loc:(loc "a")
      ~pexp_attributes:(Attributes.create [])
      ~pexp_desc:Expression_desc.pexp_unreachable
      ~pexp_loc_stack:[loc "b"; loc "c"]
  in
  ( match V4_07.Expression.to_concrete expression_08 with
    | _ -> ok () );
  [%expect {|OK|}]

let%expect_test "upgrade expression_desc any" =
  let edesc_07 = V4_07.Expression_desc.pexp_unreachable in
  ( match V4_08.Expression_desc.to_concrete edesc_07 with
    | Pexp_unreachable -> ok ()
    | _ -> ko () );
    [%expect {|OK|}]

let%expect_test "downgrade expression_desc any" =
  let edesc_08 = V4_08.Expression_desc.pexp_unreachable in
  ( match V4_07.Expression_desc.to_concrete edesc_08 with
    | Pexp_unreachable -> ok ()
    | _ -> ko () );
    [%expect {|OK|}]

let%expect_test "upgrade pexp_open" =
  let pexp_open_07 =
    let open V4_07 in
    Expression_desc.pexp_open
      Override_flag.fresh
      (Longident_loc.create { txt = Longident.lident "A"; loc = loc "li" })
      (pexp_unreachable ~loc:(loc "expr"))
  in
  let open V4_08 in
  ( match Expression_desc.to_concrete pexp_open_07 with
    | Pexp_open (open_decl, _expr) ->
      let open_decl = Open_declaration.to_concrete open_decl in
      let open_infos = Open_infos.to_concrete open_decl in
      let oi_loc = open_infos.popen_loc.loc_start.pos_fname in
      let attr = Attributes.to_concrete open_infos.popen_attributes in
      let mod_expr = Module_expr.to_concrete open_infos.popen_expr in
      let me_loc = mod_expr.pmod_loc.loc_start.pos_fname in
      let me_desc = Module_expr_desc.to_concrete mod_expr.pmod_desc in
      let me_attr = Attributes.to_concrete mod_expr.pmod_attributes in
      ( match attr, me_attr, oi_loc, me_loc, me_desc with
        | [], [], "li", "li", Pmod_ident _ -> ok ()
        | _ -> ko () )
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "downgrade pexp_open" =
  let pexp_open_08 =
    let open V4_08 in
    Expression_desc.pexp_open
      (Open_declaration.create
         (Open_infos.create
            ~popen_expr:
              (pmod_ident
                 ~loc:(loc "me")
                 (Longident_loc.create
                    { txt = Longident.lident "A"; loc = loc "li" }))
            ~popen_override:Override_flag.fresh
            ~popen_loc:(loc "open_infos")
            ~popen_attributes:(Attributes.create [])))
      (pexp_unreachable ~loc:(loc "expr"))
  in
  ( match V4_07.Expression_desc.to_concrete pexp_open_08 with
    | Pexp_open (_flag, _li, _expr) -> ok ()
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "downgrade pexp_letop" =
  let pexp_letop =
    let open V4_08 in
    Expression_desc.pexp_letop
      (Letop.create
         ~let_:
           (Binding_op.create
              ~pbop_op:{txt = "+"; loc = loc "a"}
              ~pbop_pat:(ppat_any ~loc:(loc "b"))
              ~pbop_exp:(pexp_unreachable ~loc:(loc "c"))
              ~pbop_loc:(loc "d"))
         ~ands:[]
         ~body:(pexp_unreachable ~loc:(loc "e")))
  in
  ( match V4_07.Expression_desc.to_concrete_opt pexp_letop with
    | None -> ok ()
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "upgrade pcty_open" =
  let pcty_open_07 =
    let open V4_07 in
    Class_type_desc.pcty_open
      Override_flag.fresh
      (Longident_loc.create {txt = Longident.lident "A"; loc = loc "li"})
      (Class_type.create
         ~pcty_loc:(loc "ct")
         ~pcty_attributes:(Attributes.create [])
         ~pcty_desc:
           (Class_type_desc.pcty_constr
              (Longident_loc.create
                 {txt = Longident.lident "B"; loc = loc "li_const"})
              []))
  in
  let open V4_08 in
  ( match Class_type_desc.to_concrete pcty_open_07 with
    | Pcty_open (open_desc, _) ->
      let open_desc = Open_description.to_concrete open_desc in
      let open_infos = Open_infos.to_concrete open_desc in
      let attributes = Attributes.to_concrete open_infos.popen_attributes in
      let loc = open_infos.popen_loc.loc_start.pos_fname in
      ( match attributes, loc with
        | [], "li" -> ok ()
        | _ -> ko () )
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "upgrade type_extension" =
  let type_ext_07 =
    let open V4_07 in
    Type_extension.create
      ~ptyext_path:
        (Longident_loc.create
           {txt = Longident.lident "a"; loc = loc "ptyext_path"})
      ~ptyext_params:[]
      ~ptyext_constructors:[]
      ~ptyext_private:Private_flag.public
      ~ptyext_attributes:(Attributes.create [])
  in
  ( match V4_08.Type_extension.to_concrete type_ext_07 with
    | { ptyext_loc = {loc_start = {pos_fname = "ptyext_path"; _}; _}; _} ->
      ok ()
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "downgrade type_extension" =
  let type_ext_07 =
    let open V4_08 in
    Type_extension.create
      ~ptyext_path:
        (Longident_loc.create
           {txt = Longident.lident "a"; loc = loc "ptyext_path"})
      ~ptyext_params:[]
      ~ptyext_constructors:[]
      ~ptyext_private:Private_flag.public
      ~ptyext_loc:(loc "ptyext")
      ~ptyext_attributes:(Attributes.create [])
  in
  match V4_07.Type_extension.to_concrete type_ext_07 with
  | _ -> ok ();
  [%expect {|OK|}]

let%expect_test "upgrade pstr_exception" =
  let pstr_exception_07 =
    let open V4_07 in
    Structure_item_desc.pstr_exception
      (Extension_constructor.create
         ~pext_name:{txt = "a"; loc = loc "pext_name"}
         ~pext_loc:(loc "pext")
         ~pext_attributes:(Attributes.create [])
         ~pext_kind:
           (Extension_constructor_kind.pext_rebind
              (Longident_loc.create
                 {loc = loc "pext_kind"; txt = Longident.lident "i"})))
  in
  let open V4_08 in
  ( match Structure_item_desc.to_concrete pstr_exception_07 with
    | Pstr_exception type_exc ->
      ( match Type_exception.to_concrete type_exc with
        | { ptyexn_constructor
          ; ptyexn_attributes
          ; ptyexn_loc = {loc_start = {pos_fname = "pext"; _}; _} } ->
          let attributes = Attributes.to_concrete ptyexn_attributes in
          let ext_ctor = Extension_constructor.to_concrete ptyexn_constructor in
          ( match attributes, ext_ctor with
            | [], _ -> ok ()
            | _ -> ko () )
        | _ -> ko () )
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "downgrade pstr_exception" =
  let pstr_exception_07 =
    let open V4_08 in
    Structure_item_desc.pstr_exception
      (Type_exception.create
         ~ptyexn_loc:(loc "ptyexn")
         ~ptyexn_attributes:(Attributes.create [])
         ~ptyexn_constructor:
           (Extension_constructor.create
              ~pext_name:{txt = "a"; loc = loc "pext_name"}
              ~pext_loc:(loc "pext")
              ~pext_attributes:(Attributes.create [])
              ~pext_kind:
                (Extension_constructor_kind.pext_rebind
                   (Longident_loc.create
                      {loc = loc "pext_kind"; txt = Longident.lident "i"}))))
  in
  let open V4_07 in
  ( match Structure_item_desc.to_concrete pstr_exception_07 with
    | Pstr_exception _ -> ok ()
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "upgrade pstr_open" =
  let pstr_open_07 =
    let open V4_07 in
    Structure_item_desc.pstr_open
      (Open_description.create
         ~popen_lid:
           (Longident_loc.create {txt = Longident.lident "A"; loc = loc "a"})
         ~popen_loc:(loc "b")
         ~popen_override:Override_flag.fresh
         ~popen_attributes:(Attributes.create []))
  in
  let open V4_08 in
  ( match Structure_item_desc.to_concrete pstr_open_07 with
    | Pstr_open open_decl ->
      let open_decl = Open_declaration.to_concrete open_decl in
      let open_infos = Open_infos.to_concrete open_decl in
      let mod_expr = Module_expr.to_concrete open_infos.popen_expr in
      let loc = mod_expr.pmod_loc.loc_start.pos_fname in
      let attributes = Attributes.to_concrete mod_expr.pmod_attributes in
      let desc = Module_expr_desc.to_concrete mod_expr.pmod_desc in
      ( match loc, attributes, desc with
        | "a", [], Pmod_ident _ -> ok ()
        | _ -> ko () )
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "downgrade pstr_open" =
  let pstr_open_08 =
    let open V4_08 in
    Structure_item_desc.pstr_open
      (Open_declaration.create
         (Open_infos.create
            ~popen_loc:(loc "a")
            ~popen_override:Override_flag.fresh
            ~popen_attributes:(Attributes.create [])
            ~popen_expr:
              (pmod_ident
                ~loc:(loc "b")
                (Longident_loc.create
                   {txt = Longident.lident "A"; loc = loc "c"}))))
  in
  let open V4_07 in
  ( match Structure_item_desc.to_concrete pstr_open_08 with
    | Pstr_open open_desc ->
      let _ = Open_description.to_concrete open_desc in
      ok ()
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "upgrade ptop_dir pdir_none" =
  let ptop_dir_07 =
    let open V4_07 in
    Toplevel_phrase.ptop_dir "a" (Directive_argument.pdir_none)
  in
  let open V4_08 in
  ( match Toplevel_phrase.to_concrete ptop_dir_07 with
    | Ptop_dir directive ->
      ( match Toplevel_directive.to_concrete directive with
        | {pdir_name = {txt = "a"; _}; pdir_arg = None; _} -> ok ()
        | _ -> ko () )
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "downgrade ptop_dir pdir_none" =
  let ptop_dir_08 =
    let open V4_08 in
    Toplevel_phrase.ptop_dir
      (Toplevel_directive.create
         ~pdir_name:{txt = "a"; loc = loc ""}
         ~pdir_loc:(loc "")
         ~pdir_arg:None)
  in
  let open V4_07 in
  ( match Toplevel_phrase.to_concrete ptop_dir_08 with
    | Ptop_dir ("a", arg) ->
      ( match Directive_argument.to_concrete arg with
        | Pdir_none -> ok ()
        | _ -> ko () )
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "upgrade ptop_dir" =
  let ptop_dir_07 =
    let open V4_07 in
    Toplevel_phrase.ptop_dir "a" (Directive_argument.pdir_string "b")
  in
  let open V4_08 in
  ( match Toplevel_phrase.to_concrete ptop_dir_07 with
    | Ptop_dir directive ->
      ( match Toplevel_directive.to_concrete directive with
        | {pdir_name = {txt = "a"; _}; pdir_arg = Some _; _} -> ok ()
        | _ -> ko () )
    | _ -> ko () );
  [%expect {|OK|}]

let%expect_test "downgrade ptop_dir" =
  let ptop_dir_08 =
    let open V4_08 in
    Toplevel_phrase.ptop_dir
      (Toplevel_directive.create
         ~pdir_name:{txt = "a"; loc = loc ""}
         ~pdir_loc:(loc "")
         ~pdir_arg:
           (Some
              (Directive_argument.create
                 ~pdira_loc:(loc "")
                 ~pdira_desc:(Directive_argument_desc.pdir_string "b"))))
  in
  let open V4_07 in
  ( match Toplevel_phrase.to_concrete ptop_dir_08 with
    | Ptop_dir ("a", arg) ->
      ( match Directive_argument.to_concrete arg with
        | Pdir_string "b" -> ok ()
        | _ -> ko () )
    | _ -> ko () );
  [%expect {|OK|}]
