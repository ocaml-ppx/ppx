(*$ Astlib_first_draft_test_cinaps.print_completeness_ml () *)
open! Base

module V4_07 = struct
  let rec check_longident_exn x =
    match Astlib_first_draft.V4_07.Longident.to_concrete x with
    | Some (Lident { a }) ->
      ignore a
    | Some (Ldot { a; b }) ->
      check_longident_exn a;
      ignore b
    | Some (Lapply { a; b }) ->
      check_longident_exn a;
      check_longident_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Longident"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Longident.to_ast x))])

  and check_longident_loc_exn x =
    match Astlib_first_draft.V4_07.Longident_loc.to_concrete x with
    | Some (Longident_loc { txt; loc }) ->
      check_longident_exn txt;
      ignore loc
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Longident_loc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Longident_loc.to_ast x))])

  and check_rec_flag_exn x =
    match Astlib_first_draft.V4_07.Rec_flag.to_concrete x with
    | Some (Nonrecursive) -> ()
    | Some (Recursive) -> ()
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Rec_flag"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Rec_flag.to_ast x))])

  and check_direction_flag_exn x =
    match Astlib_first_draft.V4_07.Direction_flag.to_concrete x with
    | Some (Upto) -> ()
    | Some (Downto) -> ()
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Direction_flag"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Direction_flag.to_ast x))])

  and check_private_flag_exn x =
    match Astlib_first_draft.V4_07.Private_flag.to_concrete x with
    | Some (Private) -> ()
    | Some (Public) -> ()
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Private_flag"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Private_flag.to_ast x))])

  and check_mutable_flag_exn x =
    match Astlib_first_draft.V4_07.Mutable_flag.to_concrete x with
    | Some (Immutable) -> ()
    | Some (Mutable) -> ()
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Mutable_flag"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Mutable_flag.to_ast x))])

  and check_virtual_flag_exn x =
    match Astlib_first_draft.V4_07.Virtual_flag.to_concrete x with
    | Some (Virtual) -> ()
    | Some (Concrete) -> ()
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Virtual_flag"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Virtual_flag.to_ast x))])

  and check_override_flag_exn x =
    match Astlib_first_draft.V4_07.Override_flag.to_concrete x with
    | Some (Override) -> ()
    | Some (Fresh) -> ()
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Override_flag"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Override_flag.to_ast x))])

  and check_closed_flag_exn x =
    match Astlib_first_draft.V4_07.Closed_flag.to_concrete x with
    | Some (Closed) -> ()
    | Some (Open) -> ()
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Closed_flag"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Closed_flag.to_ast x))])

  and check_label_exn x =
    match Astlib_first_draft.V4_07.Label.to_concrete x with
    | Some (Label { a }) ->
      ignore a
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Label"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Label.to_ast x))])

  and check_label_loc_exn x =
    match Astlib_first_draft.V4_07.Label_loc.to_concrete x with
    | Some (Label_loc { txt; loc }) ->
      check_label_exn txt;
      ignore loc
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Label_loc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Label_loc.to_ast x))])

  and check_string_loc_exn x =
    match Astlib_first_draft.V4_07.String_loc.to_concrete x with
    | Some (String_loc { txt; loc }) ->
      ignore txt;
      ignore loc
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 String_loc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.String_loc.to_ast x))])

  and check_arg_label_exn x =
    match Astlib_first_draft.V4_07.Arg_label.to_concrete x with
    | Some (Nolabel) -> ()
    | Some (Labelled { a }) ->
      ignore a
    | Some (Optional { a }) ->
      ignore a
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Arg_label"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Arg_label.to_ast x))])

  and check_variance_exn x =
    match Astlib_first_draft.V4_07.Variance.to_concrete x with
    | Some (Covariant) -> ()
    | Some (Contravariant) -> ()
    | Some (Invariant) -> ()
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Variance"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Variance.to_ast x))])

  and check_constant_exn x =
    match Astlib_first_draft.V4_07.Constant.to_concrete x with
    | Some (Pconst_integer { a; b }) ->
      ignore a;
      (Option.iter ~f:ignore) b
    | Some (Pconst_char { a }) ->
      ignore a
    | Some (Pconst_string { a; b }) ->
      ignore a;
      (Option.iter ~f:ignore) b
    | Some (Pconst_float { a; b }) ->
      ignore a;
      (Option.iter ~f:ignore) b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Constant"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Constant.to_ast x))])

  and check_attribute_exn x =
    match Astlib_first_draft.V4_07.Attribute.to_concrete x with
    | Some (Attribute { a; b }) ->
      check_string_loc_exn a;
      check_payload_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Attribute"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Attribute.to_ast x))])

  and check_extension_exn x =
    match Astlib_first_draft.V4_07.Extension.to_concrete x with
    | Some (Extension { a; b }) ->
      check_string_loc_exn a;
      check_payload_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Extension"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Extension.to_ast x))])

  and check_attributes_exn x =
    match Astlib_first_draft.V4_07.Attributes.to_concrete x with
    | Some (Attributes { a }) ->
      (List.iter ~f:check_attribute_exn) a
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Attributes"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Attributes.to_ast x))])

  and check_payload_exn x =
    match Astlib_first_draft.V4_07.Payload.to_concrete x with
    | Some (PStr { a }) ->
      check_structure_exn a
    | Some (PSig { a }) ->
      check_signature_exn a
    | Some (PTyp { a }) ->
      check_core_type_exn a
    | Some (PPat { a; b }) ->
      check_pattern_exn a;
      (Option.iter ~f:check_expression_exn) b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Payload"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Payload.to_ast x))])

  and check_core_type_exn x =
    match Astlib_first_draft.V4_07.Core_type.to_concrete x with
    | Some (Core_type { ptyp_desc; ptyp_loc; ptyp_attributes }) ->
      check_core_type_desc_exn ptyp_desc;
      ignore ptyp_loc;
      check_attributes_exn ptyp_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Core_type"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Core_type.to_ast x))])

  and check_core_type_desc_exn x =
    match Astlib_first_draft.V4_07.Core_type_desc.to_concrete x with
    | Some (Ptyp_any) -> ()
    | Some (Ptyp_var { a }) ->
      ignore a
    | Some (Ptyp_arrow { a; b; c }) ->
      check_arg_label_exn a;
      check_core_type_exn b;
      check_core_type_exn c
    | Some (Ptyp_tuple { a }) ->
      (List.iter ~f:check_core_type_exn) a
    | Some (Ptyp_constr { a; b }) ->
      check_longident_loc_exn a;
      (List.iter ~f:check_core_type_exn) b
    | Some (Ptyp_object { a; b }) ->
      (List.iter ~f:check_object_field_exn) a;
      check_closed_flag_exn b
    | Some (Ptyp_class { a; b }) ->
      check_longident_loc_exn a;
      (List.iter ~f:check_core_type_exn) b
    | Some (Ptyp_alias { a; b }) ->
      check_core_type_exn a;
      ignore b
    | Some (Ptyp_variant { a; b; c }) ->
      (List.iter ~f:check_row_field_exn) a;
      check_closed_flag_exn b;
      (Option.iter ~f:(List.iter ~f:check_label_exn)) c
    | Some (Ptyp_poly { a; b }) ->
      (List.iter ~f:check_string_loc_exn) a;
      check_core_type_exn b
    | Some (Ptyp_package { a }) ->
      check_package_type_exn a
    | Some (Ptyp_extension { a }) ->
      check_extension_exn a
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Core_type_desc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Core_type_desc.to_ast x))])

  and check_package_type_exn x =
    match Astlib_first_draft.V4_07.Package_type.to_concrete x with
    | Some (Package_type { a; b }) ->
      check_longident_loc_exn a;
      (List.iter ~f:check_package_type_constraint_exn) b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Package_type"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Package_type.to_ast x))])

  and check_package_type_constraint_exn x =
    match Astlib_first_draft.V4_07.Package_type_constraint.to_concrete x with
    | Some (Package_type_constraint { a; b }) ->
      check_longident_loc_exn a;
      check_core_type_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Package_type_constraint"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Package_type_constraint.to_ast x))])

  and check_row_field_exn x =
    match Astlib_first_draft.V4_07.Row_field.to_concrete x with
    | Some (Rtag { a; b; c; d }) ->
      check_label_loc_exn a;
      check_attributes_exn b;
      ignore c;
      (List.iter ~f:check_core_type_exn) d
    | Some (Rinherit { a }) ->
      check_core_type_exn a
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Row_field"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Row_field.to_ast x))])

  and check_object_field_exn x =
    match Astlib_first_draft.V4_07.Object_field.to_concrete x with
    | Some (Otag { a; b; c }) ->
      check_label_loc_exn a;
      check_attributes_exn b;
      check_core_type_exn c
    | Some (Oinherit { a }) ->
      check_core_type_exn a
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Object_field"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Object_field.to_ast x))])

  and check_pattern_exn x =
    match Astlib_first_draft.V4_07.Pattern.to_concrete x with
    | Some (Pattern { ppat_desc; ppat_loc; ppat_attributes }) ->
      check_pattern_desc_exn ppat_desc;
      ignore ppat_loc;
      check_attributes_exn ppat_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Pattern"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Pattern.to_ast x))])

  and check_pattern_desc_exn x =
    match Astlib_first_draft.V4_07.Pattern_desc.to_concrete x with
    | Some (Ppat_any) -> ()
    | Some (Ppat_var { a }) ->
      check_string_loc_exn a
    | Some (Ppat_alias { a; b }) ->
      check_pattern_exn a;
      check_string_loc_exn b
    | Some (Ppat_constant { a }) ->
      check_constant_exn a
    | Some (Ppat_interval { a; b }) ->
      check_constant_exn a;
      check_constant_exn b
    | Some (Ppat_tuple { a }) ->
      (List.iter ~f:check_pattern_exn) a
    | Some (Ppat_construct { a; b }) ->
      check_longident_loc_exn a;
      (Option.iter ~f:check_pattern_exn) b
    | Some (Ppat_variant { a; b }) ->
      check_label_exn a;
      (Option.iter ~f:check_pattern_exn) b
    | Some (Ppat_record { a; b }) ->
      (List.iter ~f:check_record_field_pattern_exn) a;
      check_closed_flag_exn b
    | Some (Ppat_array { a }) ->
      (List.iter ~f:check_pattern_exn) a
    | Some (Ppat_or { a; b }) ->
      check_pattern_exn a;
      check_pattern_exn b
    | Some (Ppat_constraint { a; b }) ->
      check_pattern_exn a;
      check_core_type_exn b
    | Some (Ppat_type { a }) ->
      check_longident_loc_exn a
    | Some (Ppat_lazy { a }) ->
      check_pattern_exn a
    | Some (Ppat_unpack { a }) ->
      check_string_loc_exn a
    | Some (Ppat_exception { a }) ->
      check_pattern_exn a
    | Some (Ppat_extension { a }) ->
      check_extension_exn a
    | Some (Ppat_open { a; b }) ->
      check_longident_loc_exn a;
      check_pattern_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Pattern_desc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Pattern_desc.to_ast x))])

  and check_record_field_pattern_exn x =
    match Astlib_first_draft.V4_07.Record_field_pattern.to_concrete x with
    | Some (Record_field_pattern { a; b }) ->
      check_longident_loc_exn a;
      check_pattern_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Record_field_pattern"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Record_field_pattern.to_ast x))])

  and check_expression_exn x =
    match Astlib_first_draft.V4_07.Expression.to_concrete x with
    | Some (Expression { pexp_desc; pexp_loc; pexp_attributes }) ->
      check_expression_desc_exn pexp_desc;
      ignore pexp_loc;
      check_attributes_exn pexp_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Expression"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Expression.to_ast x))])

  and check_expression_desc_exn x =
    match Astlib_first_draft.V4_07.Expression_desc.to_concrete x with
    | Some (Pexp_ident { a }) ->
      check_longident_loc_exn a
    | Some (Pexp_constant { a }) ->
      check_constant_exn a
    | Some (Pexp_let { a; b; c }) ->
      check_rec_flag_exn a;
      (List.iter ~f:check_value_binding_exn) b;
      check_expression_exn c
    | Some (Pexp_function { a }) ->
      (List.iter ~f:check_case_exn) a
    | Some (Pexp_fun { a; b; c; d }) ->
      check_arg_label_exn a;
      (Option.iter ~f:check_expression_exn) b;
      check_pattern_exn c;
      check_expression_exn d
    | Some (Pexp_apply { a; b }) ->
      check_expression_exn a;
      (List.iter ~f:check_apply_arg_exn) b
    | Some (Pexp_match { a; b }) ->
      check_expression_exn a;
      (List.iter ~f:check_case_exn) b
    | Some (Pexp_try { a; b }) ->
      check_expression_exn a;
      (List.iter ~f:check_case_exn) b
    | Some (Pexp_tuple { a }) ->
      (List.iter ~f:check_expression_exn) a
    | Some (Pexp_construct { a; b }) ->
      check_longident_loc_exn a;
      (Option.iter ~f:check_expression_exn) b
    | Some (Pexp_variant { a; b }) ->
      check_label_exn a;
      (Option.iter ~f:check_expression_exn) b
    | Some (Pexp_record { a; b }) ->
      (List.iter ~f:check_record_field_expression_exn) a;
      (Option.iter ~f:check_expression_exn) b
    | Some (Pexp_field { a; b }) ->
      check_expression_exn a;
      check_longident_loc_exn b
    | Some (Pexp_setfield { a; b; c }) ->
      check_expression_exn a;
      check_longident_loc_exn b;
      check_expression_exn c
    | Some (Pexp_array { a }) ->
      (List.iter ~f:check_expression_exn) a
    | Some (Pexp_ifthenelse { a; b; c }) ->
      check_expression_exn a;
      check_expression_exn b;
      (Option.iter ~f:check_expression_exn) c
    | Some (Pexp_sequence { a; b }) ->
      check_expression_exn a;
      check_expression_exn b
    | Some (Pexp_while { a; b }) ->
      check_expression_exn a;
      check_expression_exn b
    | Some (Pexp_for { a; b; c; d; e }) ->
      check_pattern_exn a;
      check_expression_exn b;
      check_expression_exn c;
      check_direction_flag_exn d;
      check_expression_exn e
    | Some (Pexp_constraint { a; b }) ->
      check_expression_exn a;
      check_core_type_exn b
    | Some (Pexp_coerce { a; b; c }) ->
      check_expression_exn a;
      (Option.iter ~f:check_core_type_exn) b;
      check_core_type_exn c
    | Some (Pexp_send { a; b }) ->
      check_expression_exn a;
      check_label_loc_exn b
    | Some (Pexp_new { a }) ->
      check_longident_loc_exn a
    | Some (Pexp_setinstvar { a; b }) ->
      check_label_loc_exn a;
      check_expression_exn b
    | Some (Pexp_override { a }) ->
      (List.iter ~f:check_override_expression_exn) a
    | Some (Pexp_letmodule { a; b; c }) ->
      check_string_loc_exn a;
      check_module_expr_exn b;
      check_expression_exn c
    | Some (Pexp_letexception { a; b }) ->
      check_extension_constructor_exn a;
      check_expression_exn b
    | Some (Pexp_assert { a }) ->
      check_expression_exn a
    | Some (Pexp_lazy { a }) ->
      check_expression_exn a
    | Some (Pexp_poly { a; b }) ->
      check_expression_exn a;
      (Option.iter ~f:check_core_type_exn) b
    | Some (Pexp_object { a }) ->
      check_class_structure_exn a
    | Some (Pexp_newtype { a; b }) ->
      check_string_loc_exn a;
      check_expression_exn b
    | Some (Pexp_pack { a }) ->
      check_module_expr_exn a
    | Some (Pexp_open { a; b; c }) ->
      check_override_flag_exn a;
      check_longident_loc_exn b;
      check_expression_exn c
    | Some (Pexp_extension { a }) ->
      check_extension_exn a
    | Some (Pexp_unreachable) -> ()
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Expression_desc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Expression_desc.to_ast x))])

  and check_override_expression_exn x =
    match Astlib_first_draft.V4_07.Override_expression.to_concrete x with
    | Some (Override_expression { a; b }) ->
      check_label_loc_exn a;
      check_expression_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Override_expression"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Override_expression.to_ast x))])

  and check_record_field_expression_exn x =
    match Astlib_first_draft.V4_07.Record_field_expression.to_concrete x with
    | Some (Record_field_expression { a; b }) ->
      check_longident_loc_exn a;
      check_expression_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Record_field_expression"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Record_field_expression.to_ast x))])

  and check_apply_arg_exn x =
    match Astlib_first_draft.V4_07.Apply_arg.to_concrete x with
    | Some (Apply_arg { a; b }) ->
      check_arg_label_exn a;
      check_expression_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Apply_arg"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Apply_arg.to_ast x))])

  and check_case_exn x =
    match Astlib_first_draft.V4_07.Case.to_concrete x with
    | Some (Case { pc_lhs; pc_guard; pc_rhs }) ->
      check_pattern_exn pc_lhs;
      (Option.iter ~f:check_expression_exn) pc_guard;
      check_expression_exn pc_rhs
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Case"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Case.to_ast x))])

  and check_value_description_exn x =
    match Astlib_first_draft.V4_07.Value_description.to_concrete x with
    | Some (Value_description { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }) ->
      check_string_loc_exn pval_name;
      check_core_type_exn pval_type;
      (List.iter ~f:ignore) pval_prim;
      check_attributes_exn pval_attributes;
      ignore pval_loc
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Value_description"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Value_description.to_ast x))])

  and check_type_declaration_exn x =
    match Astlib_first_draft.V4_07.Type_declaration.to_concrete x with
    | Some (Type_declaration { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc }) ->
      check_string_loc_exn ptype_name;
      (List.iter ~f:check_type_param_exn) ptype_params;
      (List.iter ~f:check_type_constraint_exn) ptype_cstrs;
      check_type_kind_exn ptype_kind;
      check_private_flag_exn ptype_private;
      (Option.iter ~f:check_core_type_exn) ptype_manifest;
      check_attributes_exn ptype_attributes;
      ignore ptype_loc
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Type_declaration"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Type_declaration.to_ast x))])

  and check_type_param_exn x =
    match Astlib_first_draft.V4_07.Type_param.to_concrete x with
    | Some (Type_param { a; b }) ->
      check_core_type_exn a;
      check_variance_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Type_param"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Type_param.to_ast x))])

  and check_type_constraint_exn x =
    match Astlib_first_draft.V4_07.Type_constraint.to_concrete x with
    | Some (Type_constraint { a; b; c }) ->
      check_core_type_exn a;
      check_core_type_exn b;
      ignore c
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Type_constraint"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Type_constraint.to_ast x))])

  and check_type_kind_exn x =
    match Astlib_first_draft.V4_07.Type_kind.to_concrete x with
    | Some (Ptype_abstract) -> ()
    | Some (Ptype_variant { a }) ->
      (List.iter ~f:check_constructor_declaration_exn) a
    | Some (Ptype_record { a }) ->
      (List.iter ~f:check_label_declaration_exn) a
    | Some (Ptype_open) -> ()
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Type_kind"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Type_kind.to_ast x))])

  and check_label_declaration_exn x =
    match Astlib_first_draft.V4_07.Label_declaration.to_concrete x with
    | Some (Label_declaration { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }) ->
      check_string_loc_exn pld_name;
      check_mutable_flag_exn pld_mutable;
      check_core_type_exn pld_type;
      ignore pld_loc;
      check_attributes_exn pld_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Label_declaration"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Label_declaration.to_ast x))])

  and check_constructor_declaration_exn x =
    match Astlib_first_draft.V4_07.Constructor_declaration.to_concrete x with
    | Some (Constructor_declaration { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }) ->
      check_string_loc_exn pcd_name;
      check_constructor_arguments_exn pcd_args;
      (Option.iter ~f:check_core_type_exn) pcd_res;
      ignore pcd_loc;
      check_attributes_exn pcd_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Constructor_declaration"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Constructor_declaration.to_ast x))])

  and check_constructor_arguments_exn x =
    match Astlib_first_draft.V4_07.Constructor_arguments.to_concrete x with
    | Some (Pcstr_tuple { a }) ->
      (List.iter ~f:check_core_type_exn) a
    | Some (Pcstr_record { a }) ->
      (List.iter ~f:check_label_declaration_exn) a
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Constructor_arguments"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Constructor_arguments.to_ast x))])

  and check_type_extension_exn x =
    match Astlib_first_draft.V4_07.Type_extension.to_concrete x with
    | Some (Type_extension { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes }) ->
      check_longident_loc_exn ptyext_path;
      (List.iter ~f:check_type_param_exn) ptyext_params;
      (List.iter ~f:check_extension_constructor_exn) ptyext_constructors;
      check_private_flag_exn ptyext_private;
      check_attributes_exn ptyext_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Type_extension"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Type_extension.to_ast x))])

  and check_extension_constructor_exn x =
    match Astlib_first_draft.V4_07.Extension_constructor.to_concrete x with
    | Some (Extension_constructor { pext_name; pext_kind; pext_loc; pext_attributes }) ->
      check_string_loc_exn pext_name;
      check_extension_constructor_kind_exn pext_kind;
      ignore pext_loc;
      check_attributes_exn pext_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Extension_constructor"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Extension_constructor.to_ast x))])

  and check_extension_constructor_kind_exn x =
    match Astlib_first_draft.V4_07.Extension_constructor_kind.to_concrete x with
    | Some (Pext_decl { a; b }) ->
      check_constructor_arguments_exn a;
      (Option.iter ~f:check_core_type_exn) b
    | Some (Pext_rebind { a }) ->
      check_longident_loc_exn a
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Extension_constructor_kind"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Extension_constructor_kind.to_ast x))])

  and check_class_type_exn x =
    match Astlib_first_draft.V4_07.Class_type.to_concrete x with
    | Some (Class_type { pcty_desc; pcty_loc; pcty_attributes }) ->
      check_class_type_desc_exn pcty_desc;
      ignore pcty_loc;
      check_attributes_exn pcty_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_type"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type.to_ast x))])

  and check_class_type_desc_exn x =
    match Astlib_first_draft.V4_07.Class_type_desc.to_concrete x with
    | Some (Pcty_constr { a; b }) ->
      check_longident_loc_exn a;
      (List.iter ~f:check_core_type_exn) b
    | Some (Pcty_signature { a }) ->
      check_class_signature_exn a
    | Some (Pcty_arrow { a; b; c }) ->
      check_arg_label_exn a;
      check_core_type_exn b;
      check_class_type_exn c
    | Some (Pcty_extension { a }) ->
      check_extension_exn a
    | Some (Pcty_open { a; b; c }) ->
      check_override_flag_exn a;
      check_longident_loc_exn b;
      check_class_type_exn c
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_type_desc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type_desc.to_ast x))])

  and check_class_signature_exn x =
    match Astlib_first_draft.V4_07.Class_signature.to_concrete x with
    | Some (Class_signature { pcsig_self; pcsig_fields }) ->
      check_core_type_exn pcsig_self;
      (List.iter ~f:check_class_type_field_exn) pcsig_fields
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_signature"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_signature.to_ast x))])

  and check_class_type_field_exn x =
    match Astlib_first_draft.V4_07.Class_type_field.to_concrete x with
    | Some (Class_type_field { pctf_desc; pctf_loc; pctf_attributes }) ->
      check_class_type_field_desc_exn pctf_desc;
      ignore pctf_loc;
      check_attributes_exn pctf_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_type_field"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type_field.to_ast x))])

  and check_class_type_field_desc_exn x =
    match Astlib_first_draft.V4_07.Class_type_field_desc.to_concrete x with
    | Some (Pctf_inherit { a }) ->
      check_class_type_exn a
    | Some (Pctf_val { a }) ->
      check_class_type_value_desc_exn a
    | Some (Pctf_method { a }) ->
      check_class_type_method_desc_exn a
    | Some (Pctf_constraint { a }) ->
      check_class_type_constraint_exn a
    | Some (Pctf_attribute { a }) ->
      check_attribute_exn a
    | Some (Pctf_extension { a }) ->
      check_extension_exn a
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_type_field_desc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type_field_desc.to_ast x))])

  and check_class_type_value_desc_exn x =
    match Astlib_first_draft.V4_07.Class_type_value_desc.to_concrete x with
    | Some (Class_type_value_desc { a; b; c; d }) ->
      check_label_loc_exn a;
      check_mutable_flag_exn b;
      check_virtual_flag_exn c;
      check_core_type_exn d
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_type_value_desc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type_value_desc.to_ast x))])

  and check_class_type_method_desc_exn x =
    match Astlib_first_draft.V4_07.Class_type_method_desc.to_concrete x with
    | Some (Class_type_method_desc { a; b; c; d }) ->
      check_label_loc_exn a;
      check_private_flag_exn b;
      check_virtual_flag_exn c;
      check_core_type_exn d
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_type_method_desc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type_method_desc.to_ast x))])

  and check_class_type_constraint_exn x =
    match Astlib_first_draft.V4_07.Class_type_constraint.to_concrete x with
    | Some (Class_type_constraint { a; b }) ->
      check_core_type_exn a;
      check_core_type_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_type_constraint"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type_constraint.to_ast x))])

  and check_class_description_exn x =
    match Astlib_first_draft.V4_07.Class_description.to_concrete x with
    | Some (Class_description { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }) ->
      check_virtual_flag_exn pci_virt;
      (List.iter ~f:check_type_param_exn) pci_params;
      check_string_loc_exn pci_name;
      check_class_type_exn pci_expr;
      ignore pci_loc;
      check_attributes_exn pci_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_description"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_description.to_ast x))])

  and check_class_type_declaration_exn x =
    match Astlib_first_draft.V4_07.Class_type_declaration.to_concrete x with
    | Some (Class_type_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }) ->
      check_virtual_flag_exn pci_virt;
      (List.iter ~f:check_type_param_exn) pci_params;
      check_string_loc_exn pci_name;
      check_class_type_exn pci_expr;
      ignore pci_loc;
      check_attributes_exn pci_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_type_declaration"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type_declaration.to_ast x))])

  and check_class_expr_exn x =
    match Astlib_first_draft.V4_07.Class_expr.to_concrete x with
    | Some (Class_expr { pcl_desc; pcl_loc; pcl_attributes }) ->
      check_class_expr_desc_exn pcl_desc;
      ignore pcl_loc;
      check_attributes_exn pcl_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_expr"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_expr.to_ast x))])

  and check_class_expr_desc_exn x =
    match Astlib_first_draft.V4_07.Class_expr_desc.to_concrete x with
    | Some (Pcl_constr { a; b }) ->
      check_longident_loc_exn a;
      (List.iter ~f:check_core_type_exn) b
    | Some (Pcl_structure { a }) ->
      check_class_structure_exn a
    | Some (Pcl_fun { a; b; c; d }) ->
      check_arg_label_exn a;
      (Option.iter ~f:check_expression_exn) b;
      check_pattern_exn c;
      check_class_expr_exn d
    | Some (Pcl_apply { a; b }) ->
      check_class_expr_exn a;
      (List.iter ~f:check_apply_arg_exn) b
    | Some (Pcl_let { a; b; c }) ->
      check_rec_flag_exn a;
      (List.iter ~f:check_value_binding_exn) b;
      check_class_expr_exn c
    | Some (Pcl_constraint { a; b }) ->
      check_class_expr_exn a;
      check_class_type_exn b
    | Some (Pcl_extension { a }) ->
      check_extension_exn a
    | Some (Pcl_open { a; b; c }) ->
      check_override_flag_exn a;
      check_longident_loc_exn b;
      check_class_expr_exn c
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_expr_desc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_expr_desc.to_ast x))])

  and check_class_structure_exn x =
    match Astlib_first_draft.V4_07.Class_structure.to_concrete x with
    | Some (Class_structure { pcstr_self; pcstr_fields }) ->
      check_pattern_exn pcstr_self;
      (List.iter ~f:check_class_field_exn) pcstr_fields
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_structure"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_structure.to_ast x))])

  and check_class_field_exn x =
    match Astlib_first_draft.V4_07.Class_field.to_concrete x with
    | Some (Class_field { pcf_desc; pcf_loc; pcf_attributes }) ->
      check_class_field_desc_exn pcf_desc;
      ignore pcf_loc;
      check_attributes_exn pcf_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_field"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_field.to_ast x))])

  and check_class_field_desc_exn x =
    match Astlib_first_draft.V4_07.Class_field_desc.to_concrete x with
    | Some (Pcf_inherit { a; b; c }) ->
      check_override_flag_exn a;
      check_class_expr_exn b;
      (Option.iter ~f:check_string_loc_exn) c
    | Some (Pcf_val { a }) ->
      check_class_value_desc_exn a
    | Some (Pcf_method { a }) ->
      check_class_method_desc_exn a
    | Some (Pcf_constraint { a }) ->
      check_class_type_constraint_exn a
    | Some (Pcf_initializer { a }) ->
      check_expression_exn a
    | Some (Pcf_attribute { a }) ->
      check_attribute_exn a
    | Some (Pcf_extension { a }) ->
      check_extension_exn a
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_field_desc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_field_desc.to_ast x))])

  and check_class_value_desc_exn x =
    match Astlib_first_draft.V4_07.Class_value_desc.to_concrete x with
    | Some (Class_value_desc { a; b; c }) ->
      check_label_loc_exn a;
      check_mutable_flag_exn b;
      check_class_field_kind_exn c
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_value_desc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_value_desc.to_ast x))])

  and check_class_method_desc_exn x =
    match Astlib_first_draft.V4_07.Class_method_desc.to_concrete x with
    | Some (Class_method_desc { a; b; c }) ->
      check_label_loc_exn a;
      check_private_flag_exn b;
      check_class_field_kind_exn c
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_method_desc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_method_desc.to_ast x))])

  and check_class_field_kind_exn x =
    match Astlib_first_draft.V4_07.Class_field_kind.to_concrete x with
    | Some (Cfk_virtual { a }) ->
      check_core_type_exn a
    | Some (Cfk_concrete { a; b }) ->
      check_override_flag_exn a;
      check_expression_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_field_kind"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_field_kind.to_ast x))])

  and check_class_declaration_exn x =
    match Astlib_first_draft.V4_07.Class_declaration.to_concrete x with
    | Some (Class_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }) ->
      check_virtual_flag_exn pci_virt;
      (List.iter ~f:check_type_param_exn) pci_params;
      check_string_loc_exn pci_name;
      check_class_expr_exn pci_expr;
      ignore pci_loc;
      check_attributes_exn pci_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Class_declaration"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_declaration.to_ast x))])

  and check_module_type_exn x =
    match Astlib_first_draft.V4_07.Module_type.to_concrete x with
    | Some (Module_type { pmty_desc; pmty_loc; pmty_attributes }) ->
      check_module_type_desc_exn pmty_desc;
      ignore pmty_loc;
      check_attributes_exn pmty_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Module_type"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Module_type.to_ast x))])

  and check_module_type_desc_exn x =
    match Astlib_first_draft.V4_07.Module_type_desc.to_concrete x with
    | Some (Pmty_ident { a }) ->
      check_longident_loc_exn a
    | Some (Pmty_signature { a }) ->
      check_signature_exn a
    | Some (Pmty_functor { a; b; c }) ->
      check_string_loc_exn a;
      (Option.iter ~f:check_module_type_exn) b;
      check_module_type_exn c
    | Some (Pmty_with { a; b }) ->
      check_module_type_exn a;
      (List.iter ~f:check_with_constraint_exn) b
    | Some (Pmty_typeof { a }) ->
      check_module_expr_exn a
    | Some (Pmty_extension { a }) ->
      check_extension_exn a
    | Some (Pmty_alias { a }) ->
      check_longident_loc_exn a
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Module_type_desc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Module_type_desc.to_ast x))])

  and check_signature_exn x =
    match Astlib_first_draft.V4_07.Signature.to_concrete x with
    | Some (Signature { a }) ->
      (List.iter ~f:check_signature_item_exn) a
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Signature"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Signature.to_ast x))])

  and check_signature_item_exn x =
    match Astlib_first_draft.V4_07.Signature_item.to_concrete x with
    | Some (Signature_item { psig_desc; psig_loc }) ->
      check_signature_item_desc_exn psig_desc;
      ignore psig_loc
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Signature_item"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Signature_item.to_ast x))])

  and check_signature_item_desc_exn x =
    match Astlib_first_draft.V4_07.Signature_item_desc.to_concrete x with
    | Some (Psig_value { a }) ->
      check_value_description_exn a
    | Some (Psig_type { a; b }) ->
      check_rec_flag_exn a;
      (List.iter ~f:check_type_declaration_exn) b
    | Some (Psig_typext { a }) ->
      check_type_extension_exn a
    | Some (Psig_exception { a }) ->
      check_extension_constructor_exn a
    | Some (Psig_module { a }) ->
      check_module_declaration_exn a
    | Some (Psig_recmodule { a }) ->
      (List.iter ~f:check_module_declaration_exn) a
    | Some (Psig_modtype { a }) ->
      check_module_type_declaration_exn a
    | Some (Psig_open { a }) ->
      check_open_description_exn a
    | Some (Psig_include { a }) ->
      check_include_description_exn a
    | Some (Psig_class { a }) ->
      (List.iter ~f:check_class_description_exn) a
    | Some (Psig_class_type { a }) ->
      (List.iter ~f:check_class_type_declaration_exn) a
    | Some (Psig_attribute { a }) ->
      check_attribute_exn a
    | Some (Psig_extension { a; b }) ->
      check_extension_exn a;
      check_attributes_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Signature_item_desc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Signature_item_desc.to_ast x))])

  and check_module_declaration_exn x =
    match Astlib_first_draft.V4_07.Module_declaration.to_concrete x with
    | Some (Module_declaration { pmd_name; pmd_type; pmd_attributes; pmd_loc }) ->
      check_string_loc_exn pmd_name;
      check_module_type_exn pmd_type;
      check_attributes_exn pmd_attributes;
      ignore pmd_loc
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Module_declaration"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Module_declaration.to_ast x))])

  and check_module_type_declaration_exn x =
    match Astlib_first_draft.V4_07.Module_type_declaration.to_concrete x with
    | Some (Module_type_declaration { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }) ->
      check_string_loc_exn pmtd_name;
      (Option.iter ~f:check_module_type_exn) pmtd_type;
      check_attributes_exn pmtd_attributes;
      ignore pmtd_loc
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Module_type_declaration"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Module_type_declaration.to_ast x))])

  and check_open_description_exn x =
    match Astlib_first_draft.V4_07.Open_description.to_concrete x with
    | Some (Open_description { popen_lid; popen_override; popen_loc; popen_attributes }) ->
      check_longident_loc_exn popen_lid;
      check_override_flag_exn popen_override;
      ignore popen_loc;
      check_attributes_exn popen_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Open_description"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Open_description.to_ast x))])

  and check_include_description_exn x =
    match Astlib_first_draft.V4_07.Include_description.to_concrete x with
    | Some (Include_description { pincl_mod; pincl_loc; pincl_attributes }) ->
      check_module_type_exn pincl_mod;
      ignore pincl_loc;
      check_attributes_exn pincl_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Include_description"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Include_description.to_ast x))])

  and check_include_declaration_exn x =
    match Astlib_first_draft.V4_07.Include_declaration.to_concrete x with
    | Some (Include_declaration { pincl_mod; pincl_loc; pincl_attributes }) ->
      check_module_expr_exn pincl_mod;
      ignore pincl_loc;
      check_attributes_exn pincl_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Include_declaration"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Include_declaration.to_ast x))])

  and check_with_constraint_exn x =
    match Astlib_first_draft.V4_07.With_constraint.to_concrete x with
    | Some (Pwith_type { a; b }) ->
      check_longident_loc_exn a;
      check_type_declaration_exn b
    | Some (Pwith_module { a; b }) ->
      check_longident_loc_exn a;
      check_longident_loc_exn b
    | Some (Pwith_typesubst { a; b }) ->
      check_longident_loc_exn a;
      check_type_declaration_exn b
    | Some (Pwith_modsubst { a; b }) ->
      check_longident_loc_exn a;
      check_longident_loc_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 With_constraint"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.With_constraint.to_ast x))])

  and check_module_expr_exn x =
    match Astlib_first_draft.V4_07.Module_expr.to_concrete x with
    | Some (Module_expr { pmod_desc; pmod_loc; pmod_attributes }) ->
      check_module_expr_desc_exn pmod_desc;
      ignore pmod_loc;
      check_attributes_exn pmod_attributes
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Module_expr"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Module_expr.to_ast x))])

  and check_module_expr_desc_exn x =
    match Astlib_first_draft.V4_07.Module_expr_desc.to_concrete x with
    | Some (Pmod_ident { a }) ->
      check_longident_loc_exn a
    | Some (Pmod_structure { a }) ->
      check_structure_exn a
    | Some (Pmod_functor { a; b; c }) ->
      check_string_loc_exn a;
      (Option.iter ~f:check_module_type_exn) b;
      check_module_expr_exn c
    | Some (Pmod_apply { a; b }) ->
      check_module_expr_exn a;
      check_module_expr_exn b
    | Some (Pmod_constraint { a; b }) ->
      check_module_expr_exn a;
      check_module_type_exn b
    | Some (Pmod_unpack { a }) ->
      check_expression_exn a
    | Some (Pmod_extension { a }) ->
      check_extension_exn a
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Module_expr_desc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Module_expr_desc.to_ast x))])

  and check_structure_exn x =
    match Astlib_first_draft.V4_07.Structure.to_concrete x with
    | Some (Structure { a }) ->
      (List.iter ~f:check_structure_item_exn) a
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Structure"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Structure.to_ast x))])

  and check_structure_item_exn x =
    match Astlib_first_draft.V4_07.Structure_item.to_concrete x with
    | Some (Structure_item { pstr_desc; pstr_loc }) ->
      check_structure_item_desc_exn pstr_desc;
      ignore pstr_loc
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Structure_item"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Structure_item.to_ast x))])

  and check_structure_item_desc_exn x =
    match Astlib_first_draft.V4_07.Structure_item_desc.to_concrete x with
    | Some (Pstr_eval { a; b }) ->
      check_expression_exn a;
      check_attributes_exn b
    | Some (Pstr_value { a; b }) ->
      check_rec_flag_exn a;
      (List.iter ~f:check_value_binding_exn) b
    | Some (Pstr_primitive { a }) ->
      check_value_description_exn a
    | Some (Pstr_type { a; b }) ->
      check_rec_flag_exn a;
      (List.iter ~f:check_type_declaration_exn) b
    | Some (Pstr_typext { a }) ->
      check_type_extension_exn a
    | Some (Pstr_exception { a }) ->
      check_extension_constructor_exn a
    | Some (Pstr_module { a }) ->
      check_module_binding_exn a
    | Some (Pstr_recmodule { a }) ->
      (List.iter ~f:check_module_binding_exn) a
    | Some (Pstr_modtype { a }) ->
      check_module_type_declaration_exn a
    | Some (Pstr_open { a }) ->
      check_open_description_exn a
    | Some (Pstr_class { a }) ->
      (List.iter ~f:check_class_declaration_exn) a
    | Some (Pstr_class_type { a }) ->
      (List.iter ~f:check_class_type_declaration_exn) a
    | Some (Pstr_include { a }) ->
      check_include_declaration_exn a
    | Some (Pstr_attribute { a }) ->
      check_attribute_exn a
    | Some (Pstr_extension { a; b }) ->
      check_extension_exn a;
      check_attributes_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Structure_item_desc"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Structure_item_desc.to_ast x))])

  and check_value_binding_exn x =
    match Astlib_first_draft.V4_07.Value_binding.to_concrete x with
    | Some (Value_binding { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }) ->
      check_pattern_exn pvb_pat;
      check_expression_exn pvb_expr;
      check_attributes_exn pvb_attributes;
      ignore pvb_loc
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Value_binding"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Value_binding.to_ast x))])

  and check_module_binding_exn x =
    match Astlib_first_draft.V4_07.Module_binding.to_concrete x with
    | Some (Module_binding { pmb_name; pmb_expr; pmb_attributes; pmb_loc }) ->
      check_string_loc_exn pmb_name;
      check_module_expr_exn pmb_expr;
      check_attributes_exn pmb_attributes;
      ignore pmb_loc
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Module_binding"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Module_binding.to_ast x))])

  and check_toplevel_phrase_exn x =
    match Astlib_first_draft.V4_07.Toplevel_phrase.to_concrete x with
    | Some (Ptop_def { a }) ->
      check_structure_exn a
    | Some (Ptop_dir { a; b }) ->
      ignore a;
      check_directive_argument_exn b
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Toplevel_phrase"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Toplevel_phrase.to_ast x))])

  and check_directive_argument_exn x =
    match Astlib_first_draft.V4_07.Directive_argument.to_concrete x with
    | Some (Pdir_none) -> ()
    | Some (Pdir_string { a }) ->
      ignore a
    | Some (Pdir_int { a; b }) ->
      ignore a;
      (Option.iter ~f:ignore) b
    | Some (Pdir_ident { a }) ->
      check_longident_exn a
    | Some (Pdir_bool { a }) ->
      ignore a
    | None ->
      Base.raise_s
        (Sexp.message
          "invalid V4_07 Directive_argument"
          ["ast", (Ast.sexp_of_t (Astlib_first_draft.V4_07.Directive_argument.to_ast x))])

  module Longident = struct
    let check_exn = check_longident_exn
  end

  module Longident_loc = struct
    let check_exn = check_longident_loc_exn
  end

  module Rec_flag = struct
    let check_exn = check_rec_flag_exn
  end

  module Direction_flag = struct
    let check_exn = check_direction_flag_exn
  end

  module Private_flag = struct
    let check_exn = check_private_flag_exn
  end

  module Mutable_flag = struct
    let check_exn = check_mutable_flag_exn
  end

  module Virtual_flag = struct
    let check_exn = check_virtual_flag_exn
  end

  module Override_flag = struct
    let check_exn = check_override_flag_exn
  end

  module Closed_flag = struct
    let check_exn = check_closed_flag_exn
  end

  module Label = struct
    let check_exn = check_label_exn
  end

  module Label_loc = struct
    let check_exn = check_label_loc_exn
  end

  module String_loc = struct
    let check_exn = check_string_loc_exn
  end

  module Arg_label = struct
    let check_exn = check_arg_label_exn
  end

  module Variance = struct
    let check_exn = check_variance_exn
  end

  module Constant = struct
    let check_exn = check_constant_exn
  end

  module Attribute = struct
    let check_exn = check_attribute_exn
  end

  module Extension = struct
    let check_exn = check_extension_exn
  end

  module Attributes = struct
    let check_exn = check_attributes_exn
  end

  module Payload = struct
    let check_exn = check_payload_exn
  end

  module Core_type = struct
    let check_exn = check_core_type_exn
  end

  module Core_type_desc = struct
    let check_exn = check_core_type_desc_exn
  end

  module Package_type = struct
    let check_exn = check_package_type_exn
  end

  module Package_type_constraint = struct
    let check_exn = check_package_type_constraint_exn
  end

  module Row_field = struct
    let check_exn = check_row_field_exn
  end

  module Object_field = struct
    let check_exn = check_object_field_exn
  end

  module Pattern = struct
    let check_exn = check_pattern_exn
  end

  module Pattern_desc = struct
    let check_exn = check_pattern_desc_exn
  end

  module Record_field_pattern = struct
    let check_exn = check_record_field_pattern_exn
  end

  module Expression = struct
    let check_exn = check_expression_exn
  end

  module Expression_desc = struct
    let check_exn = check_expression_desc_exn
  end

  module Override_expression = struct
    let check_exn = check_override_expression_exn
  end

  module Record_field_expression = struct
    let check_exn = check_record_field_expression_exn
  end

  module Apply_arg = struct
    let check_exn = check_apply_arg_exn
  end

  module Case = struct
    let check_exn = check_case_exn
  end

  module Value_description = struct
    let check_exn = check_value_description_exn
  end

  module Type_declaration = struct
    let check_exn = check_type_declaration_exn
  end

  module Type_param = struct
    let check_exn = check_type_param_exn
  end

  module Type_constraint = struct
    let check_exn = check_type_constraint_exn
  end

  module Type_kind = struct
    let check_exn = check_type_kind_exn
  end

  module Label_declaration = struct
    let check_exn = check_label_declaration_exn
  end

  module Constructor_declaration = struct
    let check_exn = check_constructor_declaration_exn
  end

  module Constructor_arguments = struct
    let check_exn = check_constructor_arguments_exn
  end

  module Type_extension = struct
    let check_exn = check_type_extension_exn
  end

  module Extension_constructor = struct
    let check_exn = check_extension_constructor_exn
  end

  module Extension_constructor_kind = struct
    let check_exn = check_extension_constructor_kind_exn
  end

  module Class_type = struct
    let check_exn = check_class_type_exn
  end

  module Class_type_desc = struct
    let check_exn = check_class_type_desc_exn
  end

  module Class_signature = struct
    let check_exn = check_class_signature_exn
  end

  module Class_type_field = struct
    let check_exn = check_class_type_field_exn
  end

  module Class_type_field_desc = struct
    let check_exn = check_class_type_field_desc_exn
  end

  module Class_type_value_desc = struct
    let check_exn = check_class_type_value_desc_exn
  end

  module Class_type_method_desc = struct
    let check_exn = check_class_type_method_desc_exn
  end

  module Class_type_constraint = struct
    let check_exn = check_class_type_constraint_exn
  end

  module Class_description = struct
    let check_exn = check_class_description_exn
  end

  module Class_type_declaration = struct
    let check_exn = check_class_type_declaration_exn
  end

  module Class_expr = struct
    let check_exn = check_class_expr_exn
  end

  module Class_expr_desc = struct
    let check_exn = check_class_expr_desc_exn
  end

  module Class_structure = struct
    let check_exn = check_class_structure_exn
  end

  module Class_field = struct
    let check_exn = check_class_field_exn
  end

  module Class_field_desc = struct
    let check_exn = check_class_field_desc_exn
  end

  module Class_value_desc = struct
    let check_exn = check_class_value_desc_exn
  end

  module Class_method_desc = struct
    let check_exn = check_class_method_desc_exn
  end

  module Class_field_kind = struct
    let check_exn = check_class_field_kind_exn
  end

  module Class_declaration = struct
    let check_exn = check_class_declaration_exn
  end

  module Module_type = struct
    let check_exn = check_module_type_exn
  end

  module Module_type_desc = struct
    let check_exn = check_module_type_desc_exn
  end

  module Signature = struct
    let check_exn = check_signature_exn
  end

  module Signature_item = struct
    let check_exn = check_signature_item_exn
  end

  module Signature_item_desc = struct
    let check_exn = check_signature_item_desc_exn
  end

  module Module_declaration = struct
    let check_exn = check_module_declaration_exn
  end

  module Module_type_declaration = struct
    let check_exn = check_module_type_declaration_exn
  end

  module Open_description = struct
    let check_exn = check_open_description_exn
  end

  module Include_description = struct
    let check_exn = check_include_description_exn
  end

  module Include_declaration = struct
    let check_exn = check_include_declaration_exn
  end

  module With_constraint = struct
    let check_exn = check_with_constraint_exn
  end

  module Module_expr = struct
    let check_exn = check_module_expr_exn
  end

  module Module_expr_desc = struct
    let check_exn = check_module_expr_desc_exn
  end

  module Structure = struct
    let check_exn = check_structure_exn
  end

  module Structure_item = struct
    let check_exn = check_structure_item_exn
  end

  module Structure_item_desc = struct
    let check_exn = check_structure_item_desc_exn
  end

  module Value_binding = struct
    let check_exn = check_value_binding_exn
  end

  module Module_binding = struct
    let check_exn = check_module_binding_exn
  end

  module Toplevel_phrase = struct
    let check_exn = check_toplevel_phrase_exn
  end

  module Directive_argument = struct
    let check_exn = check_directive_argument_exn
  end
end
(*$*)
