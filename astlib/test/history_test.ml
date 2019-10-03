open Base
open Stdio

let () =
  Expect_test_helpers_base.sexp_style
  := Expect_test_helpers_base.Sexp_style.simple_pretty

let%expect_test "versions" =
  Astlib_ast.History.history
  |> Astlib_ast.History.to_versioned_grammars
  |> List.iter ~f:(fun (version, _) ->
    print_endline version);
  [%expect {|
    V4_07 |}]

let rec sexp_of_data : Astlib_ast.Type.data -> Sexp.t = function
  | Kind name -> Atom (name ^ ".t")
  | Bool -> Atom "bool"
  | Char -> Atom "char"
  | String -> Atom "string"
  | Location -> Atom "Location.t"
  | List data -> List [sexp_of_data data; Atom "list"]
  | Option data -> List [sexp_of_data data; Atom "option"]

let sexp_of_field ({ field_name; data } : Astlib_ast.Type.field) =
  [%sexp (field_name : string), (data : data)]

let sexp_of_clause ({ clause_name; fields } : Astlib_ast.Type.clause) =
  [%sexp (clause_name : string), (fields : field list)]

let sexp_of_kind ({ kind_name; clauses } : Astlib_ast.Type.kind) =
  [%sexp (kind_name : string), (clauses : clause list)]

let sexp_of_grammar = [%sexp_of: kind list]

let%expect_test "grammars" =
  Astlib_ast.History.history
  |> Astlib_ast.History.to_versioned_grammars
  |> List.iter ~f:(fun (version, grammar) ->
    print_endline "";
    print_endline version;
    Expect_test_helpers_base.print_s [%sexp (grammar : grammar)]);
  [%expect {|
    V4_07
    ((Longident
      ((Lident ((a string)))
       (Ldot ((a Longident.t) (b string)))
       (Lapply ((a Longident.t) (b Longident.t)))))
     (Longident_loc ((Longident_loc ((txt Longident.t) (loc Location.t)))))
     (Rec_flag ((Nonrecursive ()) (Recursive ())))
     (Direction_flag ((Upto ()) (Downto ())))
     (Private_flag ((Private ()) (Public ())))
     (Mutable_flag ((Immutable ()) (Mutable ())))
     (Virtual_flag ((Virtual ()) (Concrete ())))
     (Override_flag ((Override ()) (Fresh ())))
     (Closed_flag ((Closed ()) (Open ())))
     (Label ((Label ((a string)))))
     (Label_loc ((Label_loc ((txt Label.t) (loc Location.t)))))
     (String_loc ((String_loc ((txt string) (loc Location.t)))))
     (Arg_label ((Nolabel ()) (Labelled ((a string))) (Optional ((a string)))))
     (Variance ((Covariant ()) (Contravariant ()) (Invariant ())))
     (Constant
      ((Pconst_integer ((a string) (b (char option))))
       (Pconst_char ((a char)))
       (Pconst_string ((a string) (b (string option))))
       (Pconst_float ((a string) (b (char option))))))
     (Attribute ((Attribute ((a String_loc.t) (b Payload.t)))))
     (Extension ((Extension ((a String_loc.t) (b Payload.t)))))
     (Attributes ((Attributes ((a (Attribute.t list))))))
     (Payload
      ((PStr ((a Structure.t)))
       (PSig ((a Signature.t)))
       (PTyp ((a Core_type.t)))
       (PPat ((a Pattern.t) (b (Expression.t option))))))
     (Core_type
      ((
       Core_type
       ((ptyp_desc Core_type_desc.t)
        (ptyp_loc Location.t)
        (ptyp_attributes Attributes.t)))))
     (Core_type_desc
      ((Ptyp_any ())
       (Ptyp_var ((a string)))
       (Ptyp_arrow ((a Arg_label.t) (b Core_type.t) (c Core_type.t)))
       (Ptyp_tuple ((a (Core_type.t list))))
       (Ptyp_constr ((a Longident_loc.t) (b (Core_type.t list))))
       (Ptyp_object ((a (Object_field.t list)) (b Closed_flag.t)))
       (Ptyp_class ((a Longident_loc.t) (b (Core_type.t list))))
       (Ptyp_alias ((a Core_type.t) (b string)))
       (Ptyp_variant
        ((a (Row_field.t list)) (b Closed_flag.t) (c ((Label.t list) option))))
       (Ptyp_poly ((a (String_loc.t list)) (b Core_type.t)))
       (Ptyp_package ((a Package_type.t)))
       (Ptyp_extension ((a Extension.t)))))
     (Package_type
      ((Package_type ((a Longident_loc.t) (b (Package_type_constraint.t list))))))
     (Package_type_constraint
      ((Package_type_constraint ((a Longident_loc.t) (b Core_type.t)))))
     (Row_field
      ((Rtag ((a Label_loc.t) (b Attributes.t) (c bool) (d (Core_type.t list))))
       (Rinherit ((a Core_type.t)))))
     (Object_field
      ((Otag ((a Label_loc.t) (b Attributes.t) (c Core_type.t)))
       (Oinherit ((a Core_type.t)))))
     (Pattern
      ((
       Pattern
       ((ppat_desc Pattern_desc.t)
        (ppat_loc Location.t)
        (ppat_attributes Attributes.t)))))
     (Pattern_desc
      ((Ppat_any ())
       (Ppat_var ((a String_loc.t)))
       (Ppat_alias ((a Pattern.t) (b String_loc.t)))
       (Ppat_constant ((a Constant.t)))
       (Ppat_interval ((a Constant.t) (b Constant.t)))
       (Ppat_tuple ((a (Pattern.t list))))
       (Ppat_construct ((a Longident_loc.t) (b (Pattern.t option))))
       (Ppat_variant ((a Label.t) (b (Pattern.t option))))
       (Ppat_record ((a (Record_field_pattern.t list)) (b Closed_flag.t)))
       (Ppat_array ((a (Pattern.t list))))
       (Ppat_or ((a Pattern.t) (b Pattern.t)))
       (Ppat_constraint ((a Pattern.t) (b Core_type.t)))
       (Ppat_type ((a Longident_loc.t)))
       (Ppat_lazy ((a Pattern.t)))
       (Ppat_unpack ((a String_loc.t)))
       (Ppat_exception ((a Pattern.t)))
       (Ppat_extension ((a Extension.t)))
       (Ppat_open ((a Longident_loc.t) (b Pattern.t)))))
     (Record_field_pattern
      ((Record_field_pattern ((a Longident_loc.t) (b Pattern.t)))))
     (Expression
      ((
       Expression
       ((pexp_desc Expression_desc.t)
        (pexp_loc Location.t)
        (pexp_attributes Attributes.t)))))
     (Expression_desc
      ((Pexp_ident ((a Longident_loc.t)))
       (Pexp_constant ((a Constant.t)))
       (Pexp_let ((a Rec_flag.t) (b (Value_binding.t list)) (c Expression.t)))
       (Pexp_function ((a (Case.t list))))
       (Pexp_fun
        ((a Arg_label.t) (b (Expression.t option)) (c Pattern.t) (d Expression.t)))
       (Pexp_apply ((a Expression.t) (b (Apply_arg.t list))))
       (Pexp_match ((a Expression.t) (b (Case.t list))))
       (Pexp_try ((a Expression.t) (b (Case.t list))))
       (Pexp_tuple ((a (Expression.t list))))
       (Pexp_construct ((a Longident_loc.t) (b (Expression.t option))))
       (Pexp_variant ((a Label.t) (b (Expression.t option))))
       (Pexp_record
        ((a (Record_field_expression.t list)) (b (Expression.t option))))
       (Pexp_field ((a Expression.t) (b Longident_loc.t)))
       (Pexp_setfield ((a Expression.t) (b Longident_loc.t) (c Expression.t)))
       (Pexp_array ((a (Expression.t list))))
       (Pexp_ifthenelse
        ((a Expression.t) (b Expression.t) (c (Expression.t option))))
       (Pexp_sequence ((a Expression.t) (b Expression.t)))
       (Pexp_while ((a Expression.t) (b Expression.t)))
       (Pexp_for
        ((a Pattern.t)
         (b Expression.t)
         (c Expression.t)
         (d Direction_flag.t)
         (e Expression.t)))
       (Pexp_constraint ((a Expression.t) (b Core_type.t)))
       (Pexp_coerce ((a Expression.t) (b (Core_type.t option)) (c Core_type.t)))
       (Pexp_send ((a Expression.t) (b Label_loc.t)))
       (Pexp_new ((a Longident_loc.t)))
       (Pexp_setinstvar ((a Label_loc.t) (b Expression.t)))
       (Pexp_override ((a (Override_expression.t list))))
       (Pexp_letmodule ((a String_loc.t) (b Module_expr.t) (c Expression.t)))
       (Pexp_letexception ((a Extension_constructor.t) (b Expression.t)))
       (Pexp_assert ((a Expression.t)))
       (Pexp_lazy ((a Expression.t)))
       (Pexp_poly ((a Expression.t) (b (Core_type.t option))))
       (Pexp_object ((a Class_structure.t)))
       (Pexp_newtype ((a String_loc.t) (b Expression.t)))
       (Pexp_pack ((a Module_expr.t)))
       (Pexp_open ((a Override_flag.t) (b Longident_loc.t) (c Expression.t)))
       (Pexp_extension ((a Extension.t)))
       (Pexp_unreachable ())))
     (Override_expression
      ((Override_expression ((a Label_loc.t) (b Expression.t)))))
     (Record_field_expression
      ((Record_field_expression ((a Longident_loc.t) (b Expression.t)))))
     (Apply_arg ((Apply_arg ((a Arg_label.t) (b Expression.t)))))
     (Case
      ((
       Case
       ((pc_lhs Pattern.t) (pc_guard (Expression.t option)) (pc_rhs Expression.t)))))
     (Value_description
      ((
       Value_description
       ((pval_name String_loc.t)
        (pval_type Core_type.t)
        (pval_prim (string list))
        (pval_attributes Attributes.t)
        (pval_loc Location.t)))))
     (Type_declaration
      ((
       Type_declaration
       ((ptype_name String_loc.t)
        (ptype_params (Type_param.t list))
        (ptype_cstrs (Type_constraint.t list))
        (ptype_kind Type_kind.t)
        (ptype_private Private_flag.t)
        (ptype_manifest (Core_type.t option))
        (ptype_attributes Attributes.t)
        (ptype_loc Location.t)))))
     (Type_param ((Type_param ((a Core_type.t) (b Variance.t)))))
     (Type_constraint
      ((Type_constraint ((a Core_type.t) (b Core_type.t) (c Location.t)))))
     (Type_kind
      ((Ptype_abstract ())
       (Ptype_variant ((a (Constructor_declaration.t list))))
       (Ptype_record ((a (Label_declaration.t list))))
       (Ptype_open ())))
     (Label_declaration
      ((
       Label_declaration
       ((pld_name String_loc.t)
        (pld_mutable Mutable_flag.t)
        (pld_type Core_type.t)
        (pld_loc Location.t)
        (pld_attributes Attributes.t)))))
     (Constructor_declaration
      ((
       Constructor_declaration
       ((pcd_name String_loc.t)
        (pcd_args Constructor_arguments.t)
        (pcd_res (Core_type.t option))
        (pcd_loc Location.t)
        (pcd_attributes Attributes.t)))))
     (Constructor_arguments
      ((Pcstr_tuple ((a (Core_type.t list))))
       (Pcstr_record ((a (Label_declaration.t list))))))
     (Type_extension
      ((
       Type_extension
       ((ptyext_path Longident_loc.t)
        (ptyext_params (Type_param.t list))
        (ptyext_constructors (Extension_constructor.t list))
        (ptyext_private Private_flag.t)
        (ptyext_attributes Attributes.t)))))
     (Extension_constructor
      ((
       Extension_constructor
       ((pext_name String_loc.t)
        (pext_kind Extension_constructor_kind.t)
        (pext_loc Location.t)
        (pext_attributes Attributes.t)))))
     (Extension_constructor_kind
      ((Pext_decl ((a Constructor_arguments.t) (b (Core_type.t option))))
       (Pext_rebind ((a Longident_loc.t)))))
     (Class_type
      ((
       Class_type
       ((pcty_desc Class_type_desc.t)
        (pcty_loc Location.t)
        (pcty_attributes Attributes.t)))))
     (Class_type_desc
      ((Pcty_constr ((a Longident_loc.t) (b (Core_type.t list))))
       (Pcty_signature ((a Class_signature.t)))
       (Pcty_arrow ((a Arg_label.t) (b Core_type.t) (c Class_type.t)))
       (Pcty_extension ((a Extension.t)))
       (Pcty_open ((a Override_flag.t) (b Longident_loc.t) (c Class_type.t)))))
     (Class_signature
      ((
       Class_signature
       ((pcsig_self Core_type.t) (pcsig_fields (Class_type_field.t list))))))
     (Class_type_field
      ((
       Class_type_field
       ((pctf_desc Class_type_field_desc.t)
        (pctf_loc Location.t)
        (pctf_attributes Attributes.t)))))
     (Class_type_field_desc
      ((Pctf_inherit ((a Class_type.t)))
       (Pctf_val ((a Class_type_value_desc.t)))
       (Pctf_method ((a Class_type_method_desc.t)))
       (Pctf_constraint ((a Class_type_constraint.t)))
       (Pctf_attribute ((a Attribute.t)))
       (Pctf_extension ((a Extension.t)))))
     (Class_type_value_desc
      ((
       Class_type_value_desc
       ((a Label_loc.t) (b Mutable_flag.t) (c Virtual_flag.t) (d Core_type.t)))))
     (Class_type_method_desc
      ((
       Class_type_method_desc
       ((a Label_loc.t) (b Private_flag.t) (c Virtual_flag.t) (d Core_type.t)))))
     (Class_type_constraint
      ((Class_type_constraint ((a Core_type.t) (b Core_type.t)))))
     (Class_description
      ((
       Class_description
       ((pci_virt Virtual_flag.t)
        (pci_params (Type_param.t list))
        (pci_name String_loc.t)
        (pci_expr Class_type.t)
        (pci_loc Location.t)
        (pci_attributes Attributes.t)))))
     (Class_type_declaration
      ((
       Class_type_declaration
       ((pci_virt Virtual_flag.t)
        (pci_params (Type_param.t list))
        (pci_name String_loc.t)
        (pci_expr Class_type.t)
        (pci_loc Location.t)
        (pci_attributes Attributes.t)))))
     (Class_expr
      ((
       Class_expr
       ((pcl_desc Class_expr_desc.t)
        (pcl_loc Location.t)
        (pcl_attributes Attributes.t)))))
     (Class_expr_desc
      ((Pcl_constr ((a Longident_loc.t) (b (Core_type.t list))))
       (Pcl_structure ((a Class_structure.t)))
       (Pcl_fun
        ((a Arg_label.t) (b (Expression.t option)) (c Pattern.t) (d Class_expr.t)))
       (Pcl_apply ((a Class_expr.t) (b (Apply_arg.t list))))
       (Pcl_let ((a Rec_flag.t) (b (Value_binding.t list)) (c Class_expr.t)))
       (Pcl_constraint ((a Class_expr.t) (b Class_type.t)))
       (Pcl_extension ((a Extension.t)))
       (Pcl_open ((a Override_flag.t) (b Longident_loc.t) (c Class_expr.t)))))
     (Class_structure
      ((
       Class_structure
       ((pcstr_self Pattern.t) (pcstr_fields (Class_field.t list))))))
     (Class_field
      ((
       Class_field
       ((pcf_desc Class_field_desc.t)
        (pcf_loc Location.t)
        (pcf_attributes Attributes.t)))))
     (Class_field_desc
      ((Pcf_inherit
        ((a Override_flag.t) (b Class_expr.t) (c (String_loc.t option))))
       (Pcf_val ((a Class_value_desc.t)))
       (Pcf_method ((a Class_method_desc.t)))
       (Pcf_constraint ((a Class_type_constraint.t)))
       (Pcf_initializer ((a Expression.t)))
       (Pcf_attribute ((a Attribute.t)))
       (Pcf_extension ((a Extension.t)))))
     (Class_value_desc
      ((
       Class_value_desc
       ((a Label_loc.t) (b Mutable_flag.t) (c Class_field_kind.t)))))
     (Class_method_desc
      ((
       Class_method_desc
       ((a Label_loc.t) (b Private_flag.t) (c Class_field_kind.t)))))
     (Class_field_kind
      ((Cfk_virtual ((a Core_type.t)))
       (Cfk_concrete ((a Override_flag.t) (b Expression.t)))))
     (Class_declaration
      ((
       Class_declaration
       ((pci_virt Virtual_flag.t)
        (pci_params (Type_param.t list))
        (pci_name String_loc.t)
        (pci_expr Class_expr.t)
        (pci_loc Location.t)
        (pci_attributes Attributes.t)))))
     (Module_type
      ((
       Module_type
       ((pmty_desc Module_type_desc.t)
        (pmty_loc Location.t)
        (pmty_attributes Attributes.t)))))
     (Module_type_desc
      ((Pmty_ident ((a Longident_loc.t)))
       (Pmty_signature ((a Signature.t)))
       (Pmty_functor
        ((a String_loc.t) (b (Module_type.t option)) (c Module_type.t)))
       (Pmty_with ((a Module_type.t) (b (With_constraint.t list))))
       (Pmty_typeof ((a Module_expr.t)))
       (Pmty_extension ((a Extension.t)))
       (Pmty_alias ((a Longident_loc.t)))))
     (Signature ((Signature ((a (Signature_item.t list))))))
     (Signature_item
      ((Signature_item ((psig_desc Signature_item_desc.t) (psig_loc Location.t)))))
     (Signature_item_desc
      ((Psig_value ((a Value_description.t)))
       (Psig_type ((a Rec_flag.t) (b (Type_declaration.t list))))
       (Psig_typext ((a Type_extension.t)))
       (Psig_exception ((a Extension_constructor.t)))
       (Psig_module ((a Module_declaration.t)))
       (Psig_recmodule ((a (Module_declaration.t list))))
       (Psig_modtype ((a Module_type_declaration.t)))
       (Psig_open ((a Open_description.t)))
       (Psig_include ((a Include_description.t)))
       (Psig_class ((a (Class_description.t list))))
       (Psig_class_type ((a (Class_type_declaration.t list))))
       (Psig_attribute ((a Attribute.t)))
       (Psig_extension ((a Extension.t) (b Attributes.t)))))
     (Module_declaration
      ((
       Module_declaration
       ((pmd_name String_loc.t)
        (pmd_type Module_type.t)
        (pmd_attributes Attributes.t)
        (pmd_loc Location.t)))))
     (Module_type_declaration
      ((
       Module_type_declaration
       ((pmtd_name String_loc.t)
        (pmtd_type (Module_type.t option))
        (pmtd_attributes Attributes.t)
        (pmtd_loc Location.t)))))
     (Open_description
      ((
       Open_description
       ((popen_lid Longident_loc.t)
        (popen_override Override_flag.t)
        (popen_loc Location.t)
        (popen_attributes Attributes.t)))))
     (Include_description
      ((
       Include_description
       ((pincl_mod Module_type.t)
        (pincl_loc Location.t)
        (pincl_attributes Attributes.t)))))
     (Include_declaration
      ((
       Include_declaration
       ((pincl_mod Module_expr.t)
        (pincl_loc Location.t)
        (pincl_attributes Attributes.t)))))
     (With_constraint
      ((Pwith_type ((a Longident_loc.t) (b Type_declaration.t)))
       (Pwith_module ((a Longident_loc.t) (b Longident_loc.t)))
       (Pwith_typesubst ((a Longident_loc.t) (b Type_declaration.t)))
       (Pwith_modsubst ((a Longident_loc.t) (b Longident_loc.t)))))
     (Module_expr
      ((
       Module_expr
       ((pmod_desc Module_expr_desc.t)
        (pmod_loc Location.t)
        (pmod_attributes Attributes.t)))))
     (Module_expr_desc
      ((Pmod_ident ((a Longident_loc.t)))
       (Pmod_structure ((a Structure.t)))
       (Pmod_functor
        ((a String_loc.t) (b (Module_type.t option)) (c Module_expr.t)))
       (Pmod_apply ((a Module_expr.t) (b Module_expr.t)))
       (Pmod_constraint ((a Module_expr.t) (b Module_type.t)))
       (Pmod_unpack ((a Expression.t)))
       (Pmod_extension ((a Extension.t)))))
     (Structure ((Structure ((a (Structure_item.t list))))))
     (Structure_item
      ((Structure_item ((pstr_desc Structure_item_desc.t) (pstr_loc Location.t)))))
     (Structure_item_desc
      ((Pstr_eval ((a Expression.t) (b Attributes.t)))
       (Pstr_value ((a Rec_flag.t) (b (Value_binding.t list))))
       (Pstr_primitive ((a Value_description.t)))
       (Pstr_type ((a Rec_flag.t) (b (Type_declaration.t list))))
       (Pstr_typext ((a Type_extension.t)))
       (Pstr_exception ((a Extension_constructor.t)))
       (Pstr_module ((a Module_binding.t)))
       (Pstr_recmodule ((a (Module_binding.t list))))
       (Pstr_modtype ((a Module_type_declaration.t)))
       (Pstr_open ((a Open_description.t)))
       (Pstr_class ((a (Class_declaration.t list))))
       (Pstr_class_type ((a (Class_type_declaration.t list))))
       (Pstr_include ((a Include_declaration.t)))
       (Pstr_attribute ((a Attribute.t)))
       (Pstr_extension ((a Extension.t) (b Attributes.t)))))
     (Value_binding
      ((
       Value_binding
       ((pvb_pat Pattern.t)
        (pvb_expr Expression.t)
        (pvb_attributes Attributes.t)
        (pvb_loc Location.t)))))
     (Module_binding
      ((
       Module_binding
       ((pmb_name String_loc.t)
        (pmb_expr Module_expr.t)
        (pmb_attributes Attributes.t)
        (pmb_loc Location.t)))))
     (Toplevel_phrase
      ((Ptop_def ((a Structure.t)))
       (Ptop_dir ((a string) (b Directive_argument.t)))))
     (Directive_argument
      ((Pdir_none ())
       (Pdir_string ((a string)))
       (Pdir_int ((a string) (b (char option))))
       (Pdir_ident ((a Longident.t)))
       (Pdir_bool ((a bool)))))) |}]

type conversion = Astlib_ast.History.conversion

let sexp_of_conversion (conversion : conversion) : Sexp.t = Atom conversion.name

type conversion_step = Astlib_ast.History.conversion_step =
  { src_version : string
  ; dst_version : string
  ; conversion : conversion
  }
[@@deriving sexp_of]

let%expect_test "conversions" =
  let versions =
    Astlib_ast.History.history
    |> Astlib_ast.History.to_versioned_grammars
    |> List.map ~f:fst
  in
  List.iter versions ~f:(fun from_version ->
    List.iter versions ~f:(fun to_version ->
      match
        Astlib_ast.History.conversion_steps
          Astlib_ast.History.history
          ~from_version
          ~to_version
      with
      | None ->
        Expect_test_helpers_base.print_cr [%here]
          [%message "no conversion steps found" ~from_version ~to_version]
      | Some steps ->
        Expect_test_helpers_base.print_s
          [%message "" ~from_version ~to_version (steps : conversion_step list)]));
  [%expect {|
    ((from_version V4_07) (to_version V4_07) (steps ())) |}]
