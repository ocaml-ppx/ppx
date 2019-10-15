(*$ Astlib_first_draft_test_cinaps.print_traversal_ml () *)
open! Base

module V4_07 = struct
  let rec copy_longident x =
    match Astlib_first_draft.V4_07.Longident.to_concrete x with
    | Some (Lident { a }) ->
      let a = a in
      Astlib_first_draft.V4_07.Longident.of_concrete (Lident { a })
    | Some (Ldot { a; b }) ->
      let a = copy_longident a in
      let b = b in
      Astlib_first_draft.V4_07.Longident.of_concrete (Ldot { a; b })
    | Some (Lapply { a; b }) ->
      let a = copy_longident a in
      let b = copy_longident b in
      Astlib_first_draft.V4_07.Longident.of_concrete (Lapply { a; b })
    | None -> x

  and copy_longident_loc x =
    match Astlib_first_draft.V4_07.Longident_loc.to_concrete x with
    | Some (Longident_loc { txt; loc }) ->
      let txt = copy_longident txt in
      let loc = loc in
      Astlib_first_draft.V4_07.Longident_loc.of_concrete (Longident_loc { txt; loc })
    | None -> x

  and copy_rec_flag x =
    match Astlib_first_draft.V4_07.Rec_flag.to_concrete x with
    | Some (Nonrecursive) ->
      Astlib_first_draft.V4_07.Rec_flag.of_concrete (Nonrecursive)
    | Some (Recursive) ->
      Astlib_first_draft.V4_07.Rec_flag.of_concrete (Recursive)
    | None -> x

  and copy_direction_flag x =
    match Astlib_first_draft.V4_07.Direction_flag.to_concrete x with
    | Some (Upto) ->
      Astlib_first_draft.V4_07.Direction_flag.of_concrete (Upto)
    | Some (Downto) ->
      Astlib_first_draft.V4_07.Direction_flag.of_concrete (Downto)
    | None -> x

  and copy_private_flag x =
    match Astlib_first_draft.V4_07.Private_flag.to_concrete x with
    | Some (Private) ->
      Astlib_first_draft.V4_07.Private_flag.of_concrete (Private)
    | Some (Public) ->
      Astlib_first_draft.V4_07.Private_flag.of_concrete (Public)
    | None -> x

  and copy_mutable_flag x =
    match Astlib_first_draft.V4_07.Mutable_flag.to_concrete x with
    | Some (Immutable) ->
      Astlib_first_draft.V4_07.Mutable_flag.of_concrete (Immutable)
    | Some (Mutable) ->
      Astlib_first_draft.V4_07.Mutable_flag.of_concrete (Mutable)
    | None -> x

  and copy_virtual_flag x =
    match Astlib_first_draft.V4_07.Virtual_flag.to_concrete x with
    | Some (Virtual) ->
      Astlib_first_draft.V4_07.Virtual_flag.of_concrete (Virtual)
    | Some (Concrete) ->
      Astlib_first_draft.V4_07.Virtual_flag.of_concrete (Concrete)
    | None -> x

  and copy_override_flag x =
    match Astlib_first_draft.V4_07.Override_flag.to_concrete x with
    | Some (Override) ->
      Astlib_first_draft.V4_07.Override_flag.of_concrete (Override)
    | Some (Fresh) ->
      Astlib_first_draft.V4_07.Override_flag.of_concrete (Fresh)
    | None -> x

  and copy_closed_flag x =
    match Astlib_first_draft.V4_07.Closed_flag.to_concrete x with
    | Some (Closed) ->
      Astlib_first_draft.V4_07.Closed_flag.of_concrete (Closed)
    | Some (Open) ->
      Astlib_first_draft.V4_07.Closed_flag.of_concrete (Open)
    | None -> x

  and copy_label x =
    match Astlib_first_draft.V4_07.Label.to_concrete x with
    | Some (Label { a }) ->
      let a = a in
      Astlib_first_draft.V4_07.Label.of_concrete (Label { a })
    | None -> x

  and copy_label_loc x =
    match Astlib_first_draft.V4_07.Label_loc.to_concrete x with
    | Some (Label_loc { txt; loc }) ->
      let txt = copy_label txt in
      let loc = loc in
      Astlib_first_draft.V4_07.Label_loc.of_concrete (Label_loc { txt; loc })
    | None -> x

  and copy_string_loc x =
    match Astlib_first_draft.V4_07.String_loc.to_concrete x with
    | Some (String_loc { txt; loc }) ->
      let txt = txt in
      let loc = loc in
      Astlib_first_draft.V4_07.String_loc.of_concrete (String_loc { txt; loc })
    | None -> x

  and copy_arg_label x =
    match Astlib_first_draft.V4_07.Arg_label.to_concrete x with
    | Some (Nolabel) ->
      Astlib_first_draft.V4_07.Arg_label.of_concrete (Nolabel)
    | Some (Labelled { a }) ->
      let a = a in
      Astlib_first_draft.V4_07.Arg_label.of_concrete (Labelled { a })
    | Some (Optional { a }) ->
      let a = a in
      Astlib_first_draft.V4_07.Arg_label.of_concrete (Optional { a })
    | None -> x

  and copy_variance x =
    match Astlib_first_draft.V4_07.Variance.to_concrete x with
    | Some (Covariant) ->
      Astlib_first_draft.V4_07.Variance.of_concrete (Covariant)
    | Some (Contravariant) ->
      Astlib_first_draft.V4_07.Variance.of_concrete (Contravariant)
    | Some (Invariant) ->
      Astlib_first_draft.V4_07.Variance.of_concrete (Invariant)
    | None -> x

  and copy_constant x =
    match Astlib_first_draft.V4_07.Constant.to_concrete x with
    | Some (Pconst_integer { a; b }) ->
      let a = a in
      let b = b in
      Astlib_first_draft.V4_07.Constant.of_concrete (Pconst_integer { a; b })
    | Some (Pconst_char { a }) ->
      let a = a in
      Astlib_first_draft.V4_07.Constant.of_concrete (Pconst_char { a })
    | Some (Pconst_string { a; b }) ->
      let a = a in
      let b = b in
      Astlib_first_draft.V4_07.Constant.of_concrete (Pconst_string { a; b })
    | Some (Pconst_float { a; b }) ->
      let a = a in
      let b = b in
      Astlib_first_draft.V4_07.Constant.of_concrete (Pconst_float { a; b })
    | None -> x

  and copy_attribute x =
    match Astlib_first_draft.V4_07.Attribute.to_concrete x with
    | Some (Attribute { a; b }) ->
      let a = copy_string_loc a in
      let b = copy_payload b in
      Astlib_first_draft.V4_07.Attribute.of_concrete (Attribute { a; b })
    | None -> x

  and copy_extension x =
    match Astlib_first_draft.V4_07.Extension.to_concrete x with
    | Some (Extension { a; b }) ->
      let a = copy_string_loc a in
      let b = copy_payload b in
      Astlib_first_draft.V4_07.Extension.of_concrete (Extension { a; b })
    | None -> x

  and copy_attributes x =
    match Astlib_first_draft.V4_07.Attributes.to_concrete x with
    | Some (Attributes { a }) ->
      let a = (List.map ~f:copy_attribute) a in
      Astlib_first_draft.V4_07.Attributes.of_concrete (Attributes { a })
    | None -> x

  and copy_payload x =
    match Astlib_first_draft.V4_07.Payload.to_concrete x with
    | Some (PStr { a }) ->
      let a = copy_structure a in
      Astlib_first_draft.V4_07.Payload.of_concrete (PStr { a })
    | Some (PSig { a }) ->
      let a = copy_signature a in
      Astlib_first_draft.V4_07.Payload.of_concrete (PSig { a })
    | Some (PTyp { a }) ->
      let a = copy_core_type a in
      Astlib_first_draft.V4_07.Payload.of_concrete (PTyp { a })
    | Some (PPat { a; b }) ->
      let a = copy_pattern a in
      let b = (Option.map ~f:copy_expression) b in
      Astlib_first_draft.V4_07.Payload.of_concrete (PPat { a; b })
    | None -> x

  and copy_core_type x =
    match Astlib_first_draft.V4_07.Core_type.to_concrete x with
    | Some (Core_type { ptyp_desc; ptyp_loc; ptyp_attributes }) ->
      let ptyp_desc = copy_core_type_desc ptyp_desc in
      let ptyp_loc = ptyp_loc in
      let ptyp_attributes = copy_attributes ptyp_attributes in
      Astlib_first_draft.V4_07.Core_type.of_concrete (Core_type { ptyp_desc; ptyp_loc; ptyp_attributes })
    | None -> x

  and copy_core_type_desc x =
    match Astlib_first_draft.V4_07.Core_type_desc.to_concrete x with
    | Some (Ptyp_any) ->
      Astlib_first_draft.V4_07.Core_type_desc.of_concrete (Ptyp_any)
    | Some (Ptyp_var { a }) ->
      let a = a in
      Astlib_first_draft.V4_07.Core_type_desc.of_concrete (Ptyp_var { a })
    | Some (Ptyp_arrow { a; b; c }) ->
      let a = copy_arg_label a in
      let b = copy_core_type b in
      let c = copy_core_type c in
      Astlib_first_draft.V4_07.Core_type_desc.of_concrete (Ptyp_arrow { a; b; c })
    | Some (Ptyp_tuple { a }) ->
      let a = (List.map ~f:copy_core_type) a in
      Astlib_first_draft.V4_07.Core_type_desc.of_concrete (Ptyp_tuple { a })
    | Some (Ptyp_constr { a; b }) ->
      let a = copy_longident_loc a in
      let b = (List.map ~f:copy_core_type) b in
      Astlib_first_draft.V4_07.Core_type_desc.of_concrete (Ptyp_constr { a; b })
    | Some (Ptyp_object { a; b }) ->
      let a = (List.map ~f:copy_object_field) a in
      let b = copy_closed_flag b in
      Astlib_first_draft.V4_07.Core_type_desc.of_concrete (Ptyp_object { a; b })
    | Some (Ptyp_class { a; b }) ->
      let a = copy_longident_loc a in
      let b = (List.map ~f:copy_core_type) b in
      Astlib_first_draft.V4_07.Core_type_desc.of_concrete (Ptyp_class { a; b })
    | Some (Ptyp_alias { a; b }) ->
      let a = copy_core_type a in
      let b = b in
      Astlib_first_draft.V4_07.Core_type_desc.of_concrete (Ptyp_alias { a; b })
    | Some (Ptyp_variant { a; b; c }) ->
      let a = (List.map ~f:copy_row_field) a in
      let b = copy_closed_flag b in
      let c = (Option.map ~f:(List.map ~f:copy_label)) c in
      Astlib_first_draft.V4_07.Core_type_desc.of_concrete (Ptyp_variant { a; b; c })
    | Some (Ptyp_poly { a; b }) ->
      let a = (List.map ~f:copy_string_loc) a in
      let b = copy_core_type b in
      Astlib_first_draft.V4_07.Core_type_desc.of_concrete (Ptyp_poly { a; b })
    | Some (Ptyp_package { a }) ->
      let a = copy_package_type a in
      Astlib_first_draft.V4_07.Core_type_desc.of_concrete (Ptyp_package { a })
    | Some (Ptyp_extension { a }) ->
      let a = copy_extension a in
      Astlib_first_draft.V4_07.Core_type_desc.of_concrete (Ptyp_extension { a })
    | None -> x

  and copy_package_type x =
    match Astlib_first_draft.V4_07.Package_type.to_concrete x with
    | Some (Package_type { a; b }) ->
      let a = copy_longident_loc a in
      let b = (List.map ~f:copy_package_type_constraint) b in
      Astlib_first_draft.V4_07.Package_type.of_concrete (Package_type { a; b })
    | None -> x

  and copy_package_type_constraint x =
    match Astlib_first_draft.V4_07.Package_type_constraint.to_concrete x with
    | Some (Package_type_constraint { a; b }) ->
      let a = copy_longident_loc a in
      let b = copy_core_type b in
      Astlib_first_draft.V4_07.Package_type_constraint.of_concrete (Package_type_constraint { a; b })
    | None -> x

  and copy_row_field x =
    match Astlib_first_draft.V4_07.Row_field.to_concrete x with
    | Some (Rtag { a; b; c; d }) ->
      let a = copy_label_loc a in
      let b = copy_attributes b in
      let c = c in
      let d = (List.map ~f:copy_core_type) d in
      Astlib_first_draft.V4_07.Row_field.of_concrete (Rtag { a; b; c; d })
    | Some (Rinherit { a }) ->
      let a = copy_core_type a in
      Astlib_first_draft.V4_07.Row_field.of_concrete (Rinherit { a })
    | None -> x

  and copy_object_field x =
    match Astlib_first_draft.V4_07.Object_field.to_concrete x with
    | Some (Otag { a; b; c }) ->
      let a = copy_label_loc a in
      let b = copy_attributes b in
      let c = copy_core_type c in
      Astlib_first_draft.V4_07.Object_field.of_concrete (Otag { a; b; c })
    | Some (Oinherit { a }) ->
      let a = copy_core_type a in
      Astlib_first_draft.V4_07.Object_field.of_concrete (Oinherit { a })
    | None -> x

  and copy_pattern x =
    match Astlib_first_draft.V4_07.Pattern.to_concrete x with
    | Some (Pattern { ppat_desc; ppat_loc; ppat_attributes }) ->
      let ppat_desc = copy_pattern_desc ppat_desc in
      let ppat_loc = ppat_loc in
      let ppat_attributes = copy_attributes ppat_attributes in
      Astlib_first_draft.V4_07.Pattern.of_concrete (Pattern { ppat_desc; ppat_loc; ppat_attributes })
    | None -> x

  and copy_pattern_desc x =
    match Astlib_first_draft.V4_07.Pattern_desc.to_concrete x with
    | Some (Ppat_any) ->
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_any)
    | Some (Ppat_var { a }) ->
      let a = copy_string_loc a in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_var { a })
    | Some (Ppat_alias { a; b }) ->
      let a = copy_pattern a in
      let b = copy_string_loc b in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_alias { a; b })
    | Some (Ppat_constant { a }) ->
      let a = copy_constant a in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_constant { a })
    | Some (Ppat_interval { a; b }) ->
      let a = copy_constant a in
      let b = copy_constant b in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_interval { a; b })
    | Some (Ppat_tuple { a }) ->
      let a = (List.map ~f:copy_pattern) a in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_tuple { a })
    | Some (Ppat_construct { a; b }) ->
      let a = copy_longident_loc a in
      let b = (Option.map ~f:copy_pattern) b in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_construct { a; b })
    | Some (Ppat_variant { a; b }) ->
      let a = copy_label a in
      let b = (Option.map ~f:copy_pattern) b in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_variant { a; b })
    | Some (Ppat_record { a; b }) ->
      let a = (List.map ~f:copy_record_field_pattern) a in
      let b = copy_closed_flag b in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_record { a; b })
    | Some (Ppat_array { a }) ->
      let a = (List.map ~f:copy_pattern) a in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_array { a })
    | Some (Ppat_or { a; b }) ->
      let a = copy_pattern a in
      let b = copy_pattern b in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_or { a; b })
    | Some (Ppat_constraint { a; b }) ->
      let a = copy_pattern a in
      let b = copy_core_type b in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_constraint { a; b })
    | Some (Ppat_type { a }) ->
      let a = copy_longident_loc a in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_type { a })
    | Some (Ppat_lazy { a }) ->
      let a = copy_pattern a in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_lazy { a })
    | Some (Ppat_unpack { a }) ->
      let a = copy_string_loc a in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_unpack { a })
    | Some (Ppat_exception { a }) ->
      let a = copy_pattern a in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_exception { a })
    | Some (Ppat_extension { a }) ->
      let a = copy_extension a in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_extension { a })
    | Some (Ppat_open { a; b }) ->
      let a = copy_longident_loc a in
      let b = copy_pattern b in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete (Ppat_open { a; b })
    | None -> x

  and copy_record_field_pattern x =
    match Astlib_first_draft.V4_07.Record_field_pattern.to_concrete x with
    | Some (Record_field_pattern { a; b }) ->
      let a = copy_longident_loc a in
      let b = copy_pattern b in
      Astlib_first_draft.V4_07.Record_field_pattern.of_concrete (Record_field_pattern { a; b })
    | None -> x

  and copy_expression x =
    match Astlib_first_draft.V4_07.Expression.to_concrete x with
    | Some (Expression { pexp_desc; pexp_loc; pexp_attributes }) ->
      let pexp_desc = copy_expression_desc pexp_desc in
      let pexp_loc = pexp_loc in
      let pexp_attributes = copy_attributes pexp_attributes in
      Astlib_first_draft.V4_07.Expression.of_concrete (Expression { pexp_desc; pexp_loc; pexp_attributes })
    | None -> x

  and copy_expression_desc x =
    match Astlib_first_draft.V4_07.Expression_desc.to_concrete x with
    | Some (Pexp_ident { a }) ->
      let a = copy_longident_loc a in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_ident { a })
    | Some (Pexp_constant { a }) ->
      let a = copy_constant a in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_constant { a })
    | Some (Pexp_let { a; b; c }) ->
      let a = copy_rec_flag a in
      let b = (List.map ~f:copy_value_binding) b in
      let c = copy_expression c in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_let { a; b; c })
    | Some (Pexp_function { a }) ->
      let a = (List.map ~f:copy_case) a in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_function { a })
    | Some (Pexp_fun { a; b; c; d }) ->
      let a = copy_arg_label a in
      let b = (Option.map ~f:copy_expression) b in
      let c = copy_pattern c in
      let d = copy_expression d in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_fun { a; b; c; d })
    | Some (Pexp_apply { a; b }) ->
      let a = copy_expression a in
      let b = (List.map ~f:copy_apply_arg) b in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_apply { a; b })
    | Some (Pexp_match { a; b }) ->
      let a = copy_expression a in
      let b = (List.map ~f:copy_case) b in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_match { a; b })
    | Some (Pexp_try { a; b }) ->
      let a = copy_expression a in
      let b = (List.map ~f:copy_case) b in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_try { a; b })
    | Some (Pexp_tuple { a }) ->
      let a = (List.map ~f:copy_expression) a in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_tuple { a })
    | Some (Pexp_construct { a; b }) ->
      let a = copy_longident_loc a in
      let b = (Option.map ~f:copy_expression) b in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_construct { a; b })
    | Some (Pexp_variant { a; b }) ->
      let a = copy_label a in
      let b = (Option.map ~f:copy_expression) b in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_variant { a; b })
    | Some (Pexp_record { a; b }) ->
      let a = (List.map ~f:copy_record_field_expression) a in
      let b = (Option.map ~f:copy_expression) b in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_record { a; b })
    | Some (Pexp_field { a; b }) ->
      let a = copy_expression a in
      let b = copy_longident_loc b in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_field { a; b })
    | Some (Pexp_setfield { a; b; c }) ->
      let a = copy_expression a in
      let b = copy_longident_loc b in
      let c = copy_expression c in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_setfield { a; b; c })
    | Some (Pexp_array { a }) ->
      let a = (List.map ~f:copy_expression) a in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_array { a })
    | Some (Pexp_ifthenelse { a; b; c }) ->
      let a = copy_expression a in
      let b = copy_expression b in
      let c = (Option.map ~f:copy_expression) c in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_ifthenelse { a; b; c })
    | Some (Pexp_sequence { a; b }) ->
      let a = copy_expression a in
      let b = copy_expression b in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_sequence { a; b })
    | Some (Pexp_while { a; b }) ->
      let a = copy_expression a in
      let b = copy_expression b in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_while { a; b })
    | Some (Pexp_for { a; b; c; d; e }) ->
      let a = copy_pattern a in
      let b = copy_expression b in
      let c = copy_expression c in
      let d = copy_direction_flag d in
      let e = copy_expression e in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_for { a; b; c; d; e })
    | Some (Pexp_constraint { a; b }) ->
      let a = copy_expression a in
      let b = copy_core_type b in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_constraint { a; b })
    | Some (Pexp_coerce { a; b; c }) ->
      let a = copy_expression a in
      let b = (Option.map ~f:copy_core_type) b in
      let c = copy_core_type c in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_coerce { a; b; c })
    | Some (Pexp_send { a; b }) ->
      let a = copy_expression a in
      let b = copy_label_loc b in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_send { a; b })
    | Some (Pexp_new { a }) ->
      let a = copy_longident_loc a in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_new { a })
    | Some (Pexp_setinstvar { a; b }) ->
      let a = copy_label_loc a in
      let b = copy_expression b in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_setinstvar { a; b })
    | Some (Pexp_override { a }) ->
      let a = (List.map ~f:copy_override_expression) a in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_override { a })
    | Some (Pexp_letmodule { a; b; c }) ->
      let a = copy_string_loc a in
      let b = copy_module_expr b in
      let c = copy_expression c in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_letmodule { a; b; c })
    | Some (Pexp_letexception { a; b }) ->
      let a = copy_extension_constructor a in
      let b = copy_expression b in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_letexception { a; b })
    | Some (Pexp_assert { a }) ->
      let a = copy_expression a in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_assert { a })
    | Some (Pexp_lazy { a }) ->
      let a = copy_expression a in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_lazy { a })
    | Some (Pexp_poly { a; b }) ->
      let a = copy_expression a in
      let b = (Option.map ~f:copy_core_type) b in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_poly { a; b })
    | Some (Pexp_object { a }) ->
      let a = copy_class_structure a in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_object { a })
    | Some (Pexp_newtype { a; b }) ->
      let a = copy_string_loc a in
      let b = copy_expression b in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_newtype { a; b })
    | Some (Pexp_pack { a }) ->
      let a = copy_module_expr a in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_pack { a })
    | Some (Pexp_open { a; b; c }) ->
      let a = copy_override_flag a in
      let b = copy_longident_loc b in
      let c = copy_expression c in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_open { a; b; c })
    | Some (Pexp_extension { a }) ->
      let a = copy_extension a in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_extension { a })
    | Some (Pexp_unreachable) ->
      Astlib_first_draft.V4_07.Expression_desc.of_concrete (Pexp_unreachable)
    | None -> x

  and copy_override_expression x =
    match Astlib_first_draft.V4_07.Override_expression.to_concrete x with
    | Some (Override_expression { a; b }) ->
      let a = copy_label_loc a in
      let b = copy_expression b in
      Astlib_first_draft.V4_07.Override_expression.of_concrete (Override_expression { a; b })
    | None -> x

  and copy_record_field_expression x =
    match Astlib_first_draft.V4_07.Record_field_expression.to_concrete x with
    | Some (Record_field_expression { a; b }) ->
      let a = copy_longident_loc a in
      let b = copy_expression b in
      Astlib_first_draft.V4_07.Record_field_expression.of_concrete (Record_field_expression { a; b })
    | None -> x

  and copy_apply_arg x =
    match Astlib_first_draft.V4_07.Apply_arg.to_concrete x with
    | Some (Apply_arg { a; b }) ->
      let a = copy_arg_label a in
      let b = copy_expression b in
      Astlib_first_draft.V4_07.Apply_arg.of_concrete (Apply_arg { a; b })
    | None -> x

  and copy_case x =
    match Astlib_first_draft.V4_07.Case.to_concrete x with
    | Some (Case { pc_lhs; pc_guard; pc_rhs }) ->
      let pc_lhs = copy_pattern pc_lhs in
      let pc_guard = (Option.map ~f:copy_expression) pc_guard in
      let pc_rhs = copy_expression pc_rhs in
      Astlib_first_draft.V4_07.Case.of_concrete (Case { pc_lhs; pc_guard; pc_rhs })
    | None -> x

  and copy_value_description x =
    match Astlib_first_draft.V4_07.Value_description.to_concrete x with
    | Some (Value_description { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }) ->
      let pval_name = copy_string_loc pval_name in
      let pval_type = copy_core_type pval_type in
      let pval_prim = pval_prim in
      let pval_attributes = copy_attributes pval_attributes in
      let pval_loc = pval_loc in
      Astlib_first_draft.V4_07.Value_description.of_concrete (Value_description { pval_name; pval_type; pval_prim; pval_attributes; pval_loc })
    | None -> x

  and copy_type_declaration x =
    match Astlib_first_draft.V4_07.Type_declaration.to_concrete x with
    | Some (Type_declaration { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc }) ->
      let ptype_name = copy_string_loc ptype_name in
      let ptype_params = (List.map ~f:copy_type_param) ptype_params in
      let ptype_cstrs = (List.map ~f:copy_type_constraint) ptype_cstrs in
      let ptype_kind = copy_type_kind ptype_kind in
      let ptype_private = copy_private_flag ptype_private in
      let ptype_manifest = (Option.map ~f:copy_core_type) ptype_manifest in
      let ptype_attributes = copy_attributes ptype_attributes in
      let ptype_loc = ptype_loc in
      Astlib_first_draft.V4_07.Type_declaration.of_concrete (Type_declaration { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc })
    | None -> x

  and copy_type_param x =
    match Astlib_first_draft.V4_07.Type_param.to_concrete x with
    | Some (Type_param { a; b }) ->
      let a = copy_core_type a in
      let b = copy_variance b in
      Astlib_first_draft.V4_07.Type_param.of_concrete (Type_param { a; b })
    | None -> x

  and copy_type_constraint x =
    match Astlib_first_draft.V4_07.Type_constraint.to_concrete x with
    | Some (Type_constraint { a; b; c }) ->
      let a = copy_core_type a in
      let b = copy_core_type b in
      let c = c in
      Astlib_first_draft.V4_07.Type_constraint.of_concrete (Type_constraint { a; b; c })
    | None -> x

  and copy_type_kind x =
    match Astlib_first_draft.V4_07.Type_kind.to_concrete x with
    | Some (Ptype_abstract) ->
      Astlib_first_draft.V4_07.Type_kind.of_concrete (Ptype_abstract)
    | Some (Ptype_variant { a }) ->
      let a = (List.map ~f:copy_constructor_declaration) a in
      Astlib_first_draft.V4_07.Type_kind.of_concrete (Ptype_variant { a })
    | Some (Ptype_record { a }) ->
      let a = (List.map ~f:copy_label_declaration) a in
      Astlib_first_draft.V4_07.Type_kind.of_concrete (Ptype_record { a })
    | Some (Ptype_open) ->
      Astlib_first_draft.V4_07.Type_kind.of_concrete (Ptype_open)
    | None -> x

  and copy_label_declaration x =
    match Astlib_first_draft.V4_07.Label_declaration.to_concrete x with
    | Some (Label_declaration { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }) ->
      let pld_name = copy_string_loc pld_name in
      let pld_mutable = copy_mutable_flag pld_mutable in
      let pld_type = copy_core_type pld_type in
      let pld_loc = pld_loc in
      let pld_attributes = copy_attributes pld_attributes in
      Astlib_first_draft.V4_07.Label_declaration.of_concrete (Label_declaration { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes })
    | None -> x

  and copy_constructor_declaration x =
    match Astlib_first_draft.V4_07.Constructor_declaration.to_concrete x with
    | Some (Constructor_declaration { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }) ->
      let pcd_name = copy_string_loc pcd_name in
      let pcd_args = copy_constructor_arguments pcd_args in
      let pcd_res = (Option.map ~f:copy_core_type) pcd_res in
      let pcd_loc = pcd_loc in
      let pcd_attributes = copy_attributes pcd_attributes in
      Astlib_first_draft.V4_07.Constructor_declaration.of_concrete (Constructor_declaration { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes })
    | None -> x

  and copy_constructor_arguments x =
    match Astlib_first_draft.V4_07.Constructor_arguments.to_concrete x with
    | Some (Pcstr_tuple { a }) ->
      let a = (List.map ~f:copy_core_type) a in
      Astlib_first_draft.V4_07.Constructor_arguments.of_concrete (Pcstr_tuple { a })
    | Some (Pcstr_record { a }) ->
      let a = (List.map ~f:copy_label_declaration) a in
      Astlib_first_draft.V4_07.Constructor_arguments.of_concrete (Pcstr_record { a })
    | None -> x

  and copy_type_extension x =
    match Astlib_first_draft.V4_07.Type_extension.to_concrete x with
    | Some (Type_extension { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes }) ->
      let ptyext_path = copy_longident_loc ptyext_path in
      let ptyext_params = (List.map ~f:copy_type_param) ptyext_params in
      let ptyext_constructors = (List.map ~f:copy_extension_constructor) ptyext_constructors in
      let ptyext_private = copy_private_flag ptyext_private in
      let ptyext_attributes = copy_attributes ptyext_attributes in
      Astlib_first_draft.V4_07.Type_extension.of_concrete (Type_extension { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes })
    | None -> x

  and copy_extension_constructor x =
    match Astlib_first_draft.V4_07.Extension_constructor.to_concrete x with
    | Some (Extension_constructor { pext_name; pext_kind; pext_loc; pext_attributes }) ->
      let pext_name = copy_string_loc pext_name in
      let pext_kind = copy_extension_constructor_kind pext_kind in
      let pext_loc = pext_loc in
      let pext_attributes = copy_attributes pext_attributes in
      Astlib_first_draft.V4_07.Extension_constructor.of_concrete (Extension_constructor { pext_name; pext_kind; pext_loc; pext_attributes })
    | None -> x

  and copy_extension_constructor_kind x =
    match Astlib_first_draft.V4_07.Extension_constructor_kind.to_concrete x with
    | Some (Pext_decl { a; b }) ->
      let a = copy_constructor_arguments a in
      let b = (Option.map ~f:copy_core_type) b in
      Astlib_first_draft.V4_07.Extension_constructor_kind.of_concrete (Pext_decl { a; b })
    | Some (Pext_rebind { a }) ->
      let a = copy_longident_loc a in
      Astlib_first_draft.V4_07.Extension_constructor_kind.of_concrete (Pext_rebind { a })
    | None -> x

  and copy_class_type x =
    match Astlib_first_draft.V4_07.Class_type.to_concrete x with
    | Some (Class_type { pcty_desc; pcty_loc; pcty_attributes }) ->
      let pcty_desc = copy_class_type_desc pcty_desc in
      let pcty_loc = pcty_loc in
      let pcty_attributes = copy_attributes pcty_attributes in
      Astlib_first_draft.V4_07.Class_type.of_concrete (Class_type { pcty_desc; pcty_loc; pcty_attributes })
    | None -> x

  and copy_class_type_desc x =
    match Astlib_first_draft.V4_07.Class_type_desc.to_concrete x with
    | Some (Pcty_constr { a; b }) ->
      let a = copy_longident_loc a in
      let b = (List.map ~f:copy_core_type) b in
      Astlib_first_draft.V4_07.Class_type_desc.of_concrete (Pcty_constr { a; b })
    | Some (Pcty_signature { a }) ->
      let a = copy_class_signature a in
      Astlib_first_draft.V4_07.Class_type_desc.of_concrete (Pcty_signature { a })
    | Some (Pcty_arrow { a; b; c }) ->
      let a = copy_arg_label a in
      let b = copy_core_type b in
      let c = copy_class_type c in
      Astlib_first_draft.V4_07.Class_type_desc.of_concrete (Pcty_arrow { a; b; c })
    | Some (Pcty_extension { a }) ->
      let a = copy_extension a in
      Astlib_first_draft.V4_07.Class_type_desc.of_concrete (Pcty_extension { a })
    | Some (Pcty_open { a; b; c }) ->
      let a = copy_override_flag a in
      let b = copy_longident_loc b in
      let c = copy_class_type c in
      Astlib_first_draft.V4_07.Class_type_desc.of_concrete (Pcty_open { a; b; c })
    | None -> x

  and copy_class_signature x =
    match Astlib_first_draft.V4_07.Class_signature.to_concrete x with
    | Some (Class_signature { pcsig_self; pcsig_fields }) ->
      let pcsig_self = copy_core_type pcsig_self in
      let pcsig_fields = (List.map ~f:copy_class_type_field) pcsig_fields in
      Astlib_first_draft.V4_07.Class_signature.of_concrete (Class_signature { pcsig_self; pcsig_fields })
    | None -> x

  and copy_class_type_field x =
    match Astlib_first_draft.V4_07.Class_type_field.to_concrete x with
    | Some (Class_type_field { pctf_desc; pctf_loc; pctf_attributes }) ->
      let pctf_desc = copy_class_type_field_desc pctf_desc in
      let pctf_loc = pctf_loc in
      let pctf_attributes = copy_attributes pctf_attributes in
      Astlib_first_draft.V4_07.Class_type_field.of_concrete (Class_type_field { pctf_desc; pctf_loc; pctf_attributes })
    | None -> x

  and copy_class_type_field_desc x =
    match Astlib_first_draft.V4_07.Class_type_field_desc.to_concrete x with
    | Some (Pctf_inherit { a }) ->
      let a = copy_class_type a in
      Astlib_first_draft.V4_07.Class_type_field_desc.of_concrete (Pctf_inherit { a })
    | Some (Pctf_val { a }) ->
      let a = copy_class_type_value_desc a in
      Astlib_first_draft.V4_07.Class_type_field_desc.of_concrete (Pctf_val { a })
    | Some (Pctf_method { a }) ->
      let a = copy_class_type_method_desc a in
      Astlib_first_draft.V4_07.Class_type_field_desc.of_concrete (Pctf_method { a })
    | Some (Pctf_constraint { a }) ->
      let a = copy_class_type_constraint a in
      Astlib_first_draft.V4_07.Class_type_field_desc.of_concrete (Pctf_constraint { a })
    | Some (Pctf_attribute { a }) ->
      let a = copy_attribute a in
      Astlib_first_draft.V4_07.Class_type_field_desc.of_concrete (Pctf_attribute { a })
    | Some (Pctf_extension { a }) ->
      let a = copy_extension a in
      Astlib_first_draft.V4_07.Class_type_field_desc.of_concrete (Pctf_extension { a })
    | None -> x

  and copy_class_type_value_desc x =
    match Astlib_first_draft.V4_07.Class_type_value_desc.to_concrete x with
    | Some (Class_type_value_desc { a; b; c; d }) ->
      let a = copy_label_loc a in
      let b = copy_mutable_flag b in
      let c = copy_virtual_flag c in
      let d = copy_core_type d in
      Astlib_first_draft.V4_07.Class_type_value_desc.of_concrete (Class_type_value_desc { a; b; c; d })
    | None -> x

  and copy_class_type_method_desc x =
    match Astlib_first_draft.V4_07.Class_type_method_desc.to_concrete x with
    | Some (Class_type_method_desc { a; b; c; d }) ->
      let a = copy_label_loc a in
      let b = copy_private_flag b in
      let c = copy_virtual_flag c in
      let d = copy_core_type d in
      Astlib_first_draft.V4_07.Class_type_method_desc.of_concrete (Class_type_method_desc { a; b; c; d })
    | None -> x

  and copy_class_type_constraint x =
    match Astlib_first_draft.V4_07.Class_type_constraint.to_concrete x with
    | Some (Class_type_constraint { a; b }) ->
      let a = copy_core_type a in
      let b = copy_core_type b in
      Astlib_first_draft.V4_07.Class_type_constraint.of_concrete (Class_type_constraint { a; b })
    | None -> x

  and copy_class_description x =
    match Astlib_first_draft.V4_07.Class_description.to_concrete x with
    | Some (Class_description { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }) ->
      let pci_virt = copy_virtual_flag pci_virt in
      let pci_params = (List.map ~f:copy_type_param) pci_params in
      let pci_name = copy_string_loc pci_name in
      let pci_expr = copy_class_type pci_expr in
      let pci_loc = pci_loc in
      let pci_attributes = copy_attributes pci_attributes in
      Astlib_first_draft.V4_07.Class_description.of_concrete (Class_description { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes })
    | None -> x

  and copy_class_type_declaration x =
    match Astlib_first_draft.V4_07.Class_type_declaration.to_concrete x with
    | Some (Class_type_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }) ->
      let pci_virt = copy_virtual_flag pci_virt in
      let pci_params = (List.map ~f:copy_type_param) pci_params in
      let pci_name = copy_string_loc pci_name in
      let pci_expr = copy_class_type pci_expr in
      let pci_loc = pci_loc in
      let pci_attributes = copy_attributes pci_attributes in
      Astlib_first_draft.V4_07.Class_type_declaration.of_concrete (Class_type_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes })
    | None -> x

  and copy_class_expr x =
    match Astlib_first_draft.V4_07.Class_expr.to_concrete x with
    | Some (Class_expr { pcl_desc; pcl_loc; pcl_attributes }) ->
      let pcl_desc = copy_class_expr_desc pcl_desc in
      let pcl_loc = pcl_loc in
      let pcl_attributes = copy_attributes pcl_attributes in
      Astlib_first_draft.V4_07.Class_expr.of_concrete (Class_expr { pcl_desc; pcl_loc; pcl_attributes })
    | None -> x

  and copy_class_expr_desc x =
    match Astlib_first_draft.V4_07.Class_expr_desc.to_concrete x with
    | Some (Pcl_constr { a; b }) ->
      let a = copy_longident_loc a in
      let b = (List.map ~f:copy_core_type) b in
      Astlib_first_draft.V4_07.Class_expr_desc.of_concrete (Pcl_constr { a; b })
    | Some (Pcl_structure { a }) ->
      let a = copy_class_structure a in
      Astlib_first_draft.V4_07.Class_expr_desc.of_concrete (Pcl_structure { a })
    | Some (Pcl_fun { a; b; c; d }) ->
      let a = copy_arg_label a in
      let b = (Option.map ~f:copy_expression) b in
      let c = copy_pattern c in
      let d = copy_class_expr d in
      Astlib_first_draft.V4_07.Class_expr_desc.of_concrete (Pcl_fun { a; b; c; d })
    | Some (Pcl_apply { a; b }) ->
      let a = copy_class_expr a in
      let b = (List.map ~f:copy_apply_arg) b in
      Astlib_first_draft.V4_07.Class_expr_desc.of_concrete (Pcl_apply { a; b })
    | Some (Pcl_let { a; b; c }) ->
      let a = copy_rec_flag a in
      let b = (List.map ~f:copy_value_binding) b in
      let c = copy_class_expr c in
      Astlib_first_draft.V4_07.Class_expr_desc.of_concrete (Pcl_let { a; b; c })
    | Some (Pcl_constraint { a; b }) ->
      let a = copy_class_expr a in
      let b = copy_class_type b in
      Astlib_first_draft.V4_07.Class_expr_desc.of_concrete (Pcl_constraint { a; b })
    | Some (Pcl_extension { a }) ->
      let a = copy_extension a in
      Astlib_first_draft.V4_07.Class_expr_desc.of_concrete (Pcl_extension { a })
    | Some (Pcl_open { a; b; c }) ->
      let a = copy_override_flag a in
      let b = copy_longident_loc b in
      let c = copy_class_expr c in
      Astlib_first_draft.V4_07.Class_expr_desc.of_concrete (Pcl_open { a; b; c })
    | None -> x

  and copy_class_structure x =
    match Astlib_first_draft.V4_07.Class_structure.to_concrete x with
    | Some (Class_structure { pcstr_self; pcstr_fields }) ->
      let pcstr_self = copy_pattern pcstr_self in
      let pcstr_fields = (List.map ~f:copy_class_field) pcstr_fields in
      Astlib_first_draft.V4_07.Class_structure.of_concrete (Class_structure { pcstr_self; pcstr_fields })
    | None -> x

  and copy_class_field x =
    match Astlib_first_draft.V4_07.Class_field.to_concrete x with
    | Some (Class_field { pcf_desc; pcf_loc; pcf_attributes }) ->
      let pcf_desc = copy_class_field_desc pcf_desc in
      let pcf_loc = pcf_loc in
      let pcf_attributes = copy_attributes pcf_attributes in
      Astlib_first_draft.V4_07.Class_field.of_concrete (Class_field { pcf_desc; pcf_loc; pcf_attributes })
    | None -> x

  and copy_class_field_desc x =
    match Astlib_first_draft.V4_07.Class_field_desc.to_concrete x with
    | Some (Pcf_inherit { a; b; c }) ->
      let a = copy_override_flag a in
      let b = copy_class_expr b in
      let c = (Option.map ~f:copy_string_loc) c in
      Astlib_first_draft.V4_07.Class_field_desc.of_concrete (Pcf_inherit { a; b; c })
    | Some (Pcf_val { a }) ->
      let a = copy_class_value_desc a in
      Astlib_first_draft.V4_07.Class_field_desc.of_concrete (Pcf_val { a })
    | Some (Pcf_method { a }) ->
      let a = copy_class_method_desc a in
      Astlib_first_draft.V4_07.Class_field_desc.of_concrete (Pcf_method { a })
    | Some (Pcf_constraint { a }) ->
      let a = copy_class_type_constraint a in
      Astlib_first_draft.V4_07.Class_field_desc.of_concrete (Pcf_constraint { a })
    | Some (Pcf_initializer { a }) ->
      let a = copy_expression a in
      Astlib_first_draft.V4_07.Class_field_desc.of_concrete (Pcf_initializer { a })
    | Some (Pcf_attribute { a }) ->
      let a = copy_attribute a in
      Astlib_first_draft.V4_07.Class_field_desc.of_concrete (Pcf_attribute { a })
    | Some (Pcf_extension { a }) ->
      let a = copy_extension a in
      Astlib_first_draft.V4_07.Class_field_desc.of_concrete (Pcf_extension { a })
    | None -> x

  and copy_class_value_desc x =
    match Astlib_first_draft.V4_07.Class_value_desc.to_concrete x with
    | Some (Class_value_desc { a; b; c }) ->
      let a = copy_label_loc a in
      let b = copy_mutable_flag b in
      let c = copy_class_field_kind c in
      Astlib_first_draft.V4_07.Class_value_desc.of_concrete (Class_value_desc { a; b; c })
    | None -> x

  and copy_class_method_desc x =
    match Astlib_first_draft.V4_07.Class_method_desc.to_concrete x with
    | Some (Class_method_desc { a; b; c }) ->
      let a = copy_label_loc a in
      let b = copy_private_flag b in
      let c = copy_class_field_kind c in
      Astlib_first_draft.V4_07.Class_method_desc.of_concrete (Class_method_desc { a; b; c })
    | None -> x

  and copy_class_field_kind x =
    match Astlib_first_draft.V4_07.Class_field_kind.to_concrete x with
    | Some (Cfk_virtual { a }) ->
      let a = copy_core_type a in
      Astlib_first_draft.V4_07.Class_field_kind.of_concrete (Cfk_virtual { a })
    | Some (Cfk_concrete { a; b }) ->
      let a = copy_override_flag a in
      let b = copy_expression b in
      Astlib_first_draft.V4_07.Class_field_kind.of_concrete (Cfk_concrete { a; b })
    | None -> x

  and copy_class_declaration x =
    match Astlib_first_draft.V4_07.Class_declaration.to_concrete x with
    | Some (Class_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }) ->
      let pci_virt = copy_virtual_flag pci_virt in
      let pci_params = (List.map ~f:copy_type_param) pci_params in
      let pci_name = copy_string_loc pci_name in
      let pci_expr = copy_class_expr pci_expr in
      let pci_loc = pci_loc in
      let pci_attributes = copy_attributes pci_attributes in
      Astlib_first_draft.V4_07.Class_declaration.of_concrete (Class_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes })
    | None -> x

  and copy_module_type x =
    match Astlib_first_draft.V4_07.Module_type.to_concrete x with
    | Some (Module_type { pmty_desc; pmty_loc; pmty_attributes }) ->
      let pmty_desc = copy_module_type_desc pmty_desc in
      let pmty_loc = pmty_loc in
      let pmty_attributes = copy_attributes pmty_attributes in
      Astlib_first_draft.V4_07.Module_type.of_concrete (Module_type { pmty_desc; pmty_loc; pmty_attributes })
    | None -> x

  and copy_module_type_desc x =
    match Astlib_first_draft.V4_07.Module_type_desc.to_concrete x with
    | Some (Pmty_ident { a }) ->
      let a = copy_longident_loc a in
      Astlib_first_draft.V4_07.Module_type_desc.of_concrete (Pmty_ident { a })
    | Some (Pmty_signature { a }) ->
      let a = copy_signature a in
      Astlib_first_draft.V4_07.Module_type_desc.of_concrete (Pmty_signature { a })
    | Some (Pmty_functor { a; b; c }) ->
      let a = copy_string_loc a in
      let b = (Option.map ~f:copy_module_type) b in
      let c = copy_module_type c in
      Astlib_first_draft.V4_07.Module_type_desc.of_concrete (Pmty_functor { a; b; c })
    | Some (Pmty_with { a; b }) ->
      let a = copy_module_type a in
      let b = (List.map ~f:copy_with_constraint) b in
      Astlib_first_draft.V4_07.Module_type_desc.of_concrete (Pmty_with { a; b })
    | Some (Pmty_typeof { a }) ->
      let a = copy_module_expr a in
      Astlib_first_draft.V4_07.Module_type_desc.of_concrete (Pmty_typeof { a })
    | Some (Pmty_extension { a }) ->
      let a = copy_extension a in
      Astlib_first_draft.V4_07.Module_type_desc.of_concrete (Pmty_extension { a })
    | Some (Pmty_alias { a }) ->
      let a = copy_longident_loc a in
      Astlib_first_draft.V4_07.Module_type_desc.of_concrete (Pmty_alias { a })
    | None -> x

  and copy_signature x =
    match Astlib_first_draft.V4_07.Signature.to_concrete x with
    | Some (Signature { a }) ->
      let a = (List.map ~f:copy_signature_item) a in
      Astlib_first_draft.V4_07.Signature.of_concrete (Signature { a })
    | None -> x

  and copy_signature_item x =
    match Astlib_first_draft.V4_07.Signature_item.to_concrete x with
    | Some (Signature_item { psig_desc; psig_loc }) ->
      let psig_desc = copy_signature_item_desc psig_desc in
      let psig_loc = psig_loc in
      Astlib_first_draft.V4_07.Signature_item.of_concrete (Signature_item { psig_desc; psig_loc })
    | None -> x

  and copy_signature_item_desc x =
    match Astlib_first_draft.V4_07.Signature_item_desc.to_concrete x with
    | Some (Psig_value { a }) ->
      let a = copy_value_description a in
      Astlib_first_draft.V4_07.Signature_item_desc.of_concrete (Psig_value { a })
    | Some (Psig_type { a; b }) ->
      let a = copy_rec_flag a in
      let b = (List.map ~f:copy_type_declaration) b in
      Astlib_first_draft.V4_07.Signature_item_desc.of_concrete (Psig_type { a; b })
    | Some (Psig_typext { a }) ->
      let a = copy_type_extension a in
      Astlib_first_draft.V4_07.Signature_item_desc.of_concrete (Psig_typext { a })
    | Some (Psig_exception { a }) ->
      let a = copy_extension_constructor a in
      Astlib_first_draft.V4_07.Signature_item_desc.of_concrete (Psig_exception { a })
    | Some (Psig_module { a }) ->
      let a = copy_module_declaration a in
      Astlib_first_draft.V4_07.Signature_item_desc.of_concrete (Psig_module { a })
    | Some (Psig_recmodule { a }) ->
      let a = (List.map ~f:copy_module_declaration) a in
      Astlib_first_draft.V4_07.Signature_item_desc.of_concrete (Psig_recmodule { a })
    | Some (Psig_modtype { a }) ->
      let a = copy_module_type_declaration a in
      Astlib_first_draft.V4_07.Signature_item_desc.of_concrete (Psig_modtype { a })
    | Some (Psig_open { a }) ->
      let a = copy_open_description a in
      Astlib_first_draft.V4_07.Signature_item_desc.of_concrete (Psig_open { a })
    | Some (Psig_include { a }) ->
      let a = copy_include_description a in
      Astlib_first_draft.V4_07.Signature_item_desc.of_concrete (Psig_include { a })
    | Some (Psig_class { a }) ->
      let a = (List.map ~f:copy_class_description) a in
      Astlib_first_draft.V4_07.Signature_item_desc.of_concrete (Psig_class { a })
    | Some (Psig_class_type { a }) ->
      let a = (List.map ~f:copy_class_type_declaration) a in
      Astlib_first_draft.V4_07.Signature_item_desc.of_concrete (Psig_class_type { a })
    | Some (Psig_attribute { a }) ->
      let a = copy_attribute a in
      Astlib_first_draft.V4_07.Signature_item_desc.of_concrete (Psig_attribute { a })
    | Some (Psig_extension { a; b }) ->
      let a = copy_extension a in
      let b = copy_attributes b in
      Astlib_first_draft.V4_07.Signature_item_desc.of_concrete (Psig_extension { a; b })
    | None -> x

  and copy_module_declaration x =
    match Astlib_first_draft.V4_07.Module_declaration.to_concrete x with
    | Some (Module_declaration { pmd_name; pmd_type; pmd_attributes; pmd_loc }) ->
      let pmd_name = copy_string_loc pmd_name in
      let pmd_type = copy_module_type pmd_type in
      let pmd_attributes = copy_attributes pmd_attributes in
      let pmd_loc = pmd_loc in
      Astlib_first_draft.V4_07.Module_declaration.of_concrete (Module_declaration { pmd_name; pmd_type; pmd_attributes; pmd_loc })
    | None -> x

  and copy_module_type_declaration x =
    match Astlib_first_draft.V4_07.Module_type_declaration.to_concrete x with
    | Some (Module_type_declaration { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }) ->
      let pmtd_name = copy_string_loc pmtd_name in
      let pmtd_type = (Option.map ~f:copy_module_type) pmtd_type in
      let pmtd_attributes = copy_attributes pmtd_attributes in
      let pmtd_loc = pmtd_loc in
      Astlib_first_draft.V4_07.Module_type_declaration.of_concrete (Module_type_declaration { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc })
    | None -> x

  and copy_open_description x =
    match Astlib_first_draft.V4_07.Open_description.to_concrete x with
    | Some (Open_description { popen_lid; popen_override; popen_loc; popen_attributes }) ->
      let popen_lid = copy_longident_loc popen_lid in
      let popen_override = copy_override_flag popen_override in
      let popen_loc = popen_loc in
      let popen_attributes = copy_attributes popen_attributes in
      Astlib_first_draft.V4_07.Open_description.of_concrete (Open_description { popen_lid; popen_override; popen_loc; popen_attributes })
    | None -> x

  and copy_include_description x =
    match Astlib_first_draft.V4_07.Include_description.to_concrete x with
    | Some (Include_description { pincl_mod; pincl_loc; pincl_attributes }) ->
      let pincl_mod = copy_module_type pincl_mod in
      let pincl_loc = pincl_loc in
      let pincl_attributes = copy_attributes pincl_attributes in
      Astlib_first_draft.V4_07.Include_description.of_concrete (Include_description { pincl_mod; pincl_loc; pincl_attributes })
    | None -> x

  and copy_include_declaration x =
    match Astlib_first_draft.V4_07.Include_declaration.to_concrete x with
    | Some (Include_declaration { pincl_mod; pincl_loc; pincl_attributes }) ->
      let pincl_mod = copy_module_expr pincl_mod in
      let pincl_loc = pincl_loc in
      let pincl_attributes = copy_attributes pincl_attributes in
      Astlib_first_draft.V4_07.Include_declaration.of_concrete (Include_declaration { pincl_mod; pincl_loc; pincl_attributes })
    | None -> x

  and copy_with_constraint x =
    match Astlib_first_draft.V4_07.With_constraint.to_concrete x with
    | Some (Pwith_type { a; b }) ->
      let a = copy_longident_loc a in
      let b = copy_type_declaration b in
      Astlib_first_draft.V4_07.With_constraint.of_concrete (Pwith_type { a; b })
    | Some (Pwith_module { a; b }) ->
      let a = copy_longident_loc a in
      let b = copy_longident_loc b in
      Astlib_first_draft.V4_07.With_constraint.of_concrete (Pwith_module { a; b })
    | Some (Pwith_typesubst { a; b }) ->
      let a = copy_longident_loc a in
      let b = copy_type_declaration b in
      Astlib_first_draft.V4_07.With_constraint.of_concrete (Pwith_typesubst { a; b })
    | Some (Pwith_modsubst { a; b }) ->
      let a = copy_longident_loc a in
      let b = copy_longident_loc b in
      Astlib_first_draft.V4_07.With_constraint.of_concrete (Pwith_modsubst { a; b })
    | None -> x

  and copy_module_expr x =
    match Astlib_first_draft.V4_07.Module_expr.to_concrete x with
    | Some (Module_expr { pmod_desc; pmod_loc; pmod_attributes }) ->
      let pmod_desc = copy_module_expr_desc pmod_desc in
      let pmod_loc = pmod_loc in
      let pmod_attributes = copy_attributes pmod_attributes in
      Astlib_first_draft.V4_07.Module_expr.of_concrete (Module_expr { pmod_desc; pmod_loc; pmod_attributes })
    | None -> x

  and copy_module_expr_desc x =
    match Astlib_first_draft.V4_07.Module_expr_desc.to_concrete x with
    | Some (Pmod_ident { a }) ->
      let a = copy_longident_loc a in
      Astlib_first_draft.V4_07.Module_expr_desc.of_concrete (Pmod_ident { a })
    | Some (Pmod_structure { a }) ->
      let a = copy_structure a in
      Astlib_first_draft.V4_07.Module_expr_desc.of_concrete (Pmod_structure { a })
    | Some (Pmod_functor { a; b; c }) ->
      let a = copy_string_loc a in
      let b = (Option.map ~f:copy_module_type) b in
      let c = copy_module_expr c in
      Astlib_first_draft.V4_07.Module_expr_desc.of_concrete (Pmod_functor { a; b; c })
    | Some (Pmod_apply { a; b }) ->
      let a = copy_module_expr a in
      let b = copy_module_expr b in
      Astlib_first_draft.V4_07.Module_expr_desc.of_concrete (Pmod_apply { a; b })
    | Some (Pmod_constraint { a; b }) ->
      let a = copy_module_expr a in
      let b = copy_module_type b in
      Astlib_first_draft.V4_07.Module_expr_desc.of_concrete (Pmod_constraint { a; b })
    | Some (Pmod_unpack { a }) ->
      let a = copy_expression a in
      Astlib_first_draft.V4_07.Module_expr_desc.of_concrete (Pmod_unpack { a })
    | Some (Pmod_extension { a }) ->
      let a = copy_extension a in
      Astlib_first_draft.V4_07.Module_expr_desc.of_concrete (Pmod_extension { a })
    | None -> x

  and copy_structure x =
    match Astlib_first_draft.V4_07.Structure.to_concrete x with
    | Some (Structure { a }) ->
      let a = (List.map ~f:copy_structure_item) a in
      Astlib_first_draft.V4_07.Structure.of_concrete (Structure { a })
    | None -> x

  and copy_structure_item x =
    match Astlib_first_draft.V4_07.Structure_item.to_concrete x with
    | Some (Structure_item { pstr_desc; pstr_loc }) ->
      let pstr_desc = copy_structure_item_desc pstr_desc in
      let pstr_loc = pstr_loc in
      Astlib_first_draft.V4_07.Structure_item.of_concrete (Structure_item { pstr_desc; pstr_loc })
    | None -> x

  and copy_structure_item_desc x =
    match Astlib_first_draft.V4_07.Structure_item_desc.to_concrete x with
    | Some (Pstr_eval { a; b }) ->
      let a = copy_expression a in
      let b = copy_attributes b in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete (Pstr_eval { a; b })
    | Some (Pstr_value { a; b }) ->
      let a = copy_rec_flag a in
      let b = (List.map ~f:copy_value_binding) b in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete (Pstr_value { a; b })
    | Some (Pstr_primitive { a }) ->
      let a = copy_value_description a in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete (Pstr_primitive { a })
    | Some (Pstr_type { a; b }) ->
      let a = copy_rec_flag a in
      let b = (List.map ~f:copy_type_declaration) b in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete (Pstr_type { a; b })
    | Some (Pstr_typext { a }) ->
      let a = copy_type_extension a in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete (Pstr_typext { a })
    | Some (Pstr_exception { a }) ->
      let a = copy_extension_constructor a in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete (Pstr_exception { a })
    | Some (Pstr_module { a }) ->
      let a = copy_module_binding a in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete (Pstr_module { a })
    | Some (Pstr_recmodule { a }) ->
      let a = (List.map ~f:copy_module_binding) a in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete (Pstr_recmodule { a })
    | Some (Pstr_modtype { a }) ->
      let a = copy_module_type_declaration a in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete (Pstr_modtype { a })
    | Some (Pstr_open { a }) ->
      let a = copy_open_description a in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete (Pstr_open { a })
    | Some (Pstr_class { a }) ->
      let a = (List.map ~f:copy_class_declaration) a in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete (Pstr_class { a })
    | Some (Pstr_class_type { a }) ->
      let a = (List.map ~f:copy_class_type_declaration) a in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete (Pstr_class_type { a })
    | Some (Pstr_include { a }) ->
      let a = copy_include_declaration a in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete (Pstr_include { a })
    | Some (Pstr_attribute { a }) ->
      let a = copy_attribute a in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete (Pstr_attribute { a })
    | Some (Pstr_extension { a; b }) ->
      let a = copy_extension a in
      let b = copy_attributes b in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete (Pstr_extension { a; b })
    | None -> x

  and copy_value_binding x =
    match Astlib_first_draft.V4_07.Value_binding.to_concrete x with
    | Some (Value_binding { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }) ->
      let pvb_pat = copy_pattern pvb_pat in
      let pvb_expr = copy_expression pvb_expr in
      let pvb_attributes = copy_attributes pvb_attributes in
      let pvb_loc = pvb_loc in
      Astlib_first_draft.V4_07.Value_binding.of_concrete (Value_binding { pvb_pat; pvb_expr; pvb_attributes; pvb_loc })
    | None -> x

  and copy_module_binding x =
    match Astlib_first_draft.V4_07.Module_binding.to_concrete x with
    | Some (Module_binding { pmb_name; pmb_expr; pmb_attributes; pmb_loc }) ->
      let pmb_name = copy_string_loc pmb_name in
      let pmb_expr = copy_module_expr pmb_expr in
      let pmb_attributes = copy_attributes pmb_attributes in
      let pmb_loc = pmb_loc in
      Astlib_first_draft.V4_07.Module_binding.of_concrete (Module_binding { pmb_name; pmb_expr; pmb_attributes; pmb_loc })
    | None -> x

  and copy_toplevel_phrase x =
    match Astlib_first_draft.V4_07.Toplevel_phrase.to_concrete x with
    | Some (Ptop_def { a }) ->
      let a = copy_structure a in
      Astlib_first_draft.V4_07.Toplevel_phrase.of_concrete (Ptop_def { a })
    | Some (Ptop_dir { a; b }) ->
      let a = a in
      let b = copy_directive_argument b in
      Astlib_first_draft.V4_07.Toplevel_phrase.of_concrete (Ptop_dir { a; b })
    | None -> x

  and copy_directive_argument x =
    match Astlib_first_draft.V4_07.Directive_argument.to_concrete x with
    | Some (Pdir_none) ->
      Astlib_first_draft.V4_07.Directive_argument.of_concrete (Pdir_none)
    | Some (Pdir_string { a }) ->
      let a = a in
      Astlib_first_draft.V4_07.Directive_argument.of_concrete (Pdir_string { a })
    | Some (Pdir_int { a; b }) ->
      let a = a in
      let b = b in
      Astlib_first_draft.V4_07.Directive_argument.of_concrete (Pdir_int { a; b })
    | Some (Pdir_ident { a }) ->
      let a = copy_longident a in
      Astlib_first_draft.V4_07.Directive_argument.of_concrete (Pdir_ident { a })
    | Some (Pdir_bool { a }) ->
      let a = a in
      Astlib_first_draft.V4_07.Directive_argument.of_concrete (Pdir_bool { a })
    | None -> x

  module Longident = struct
    let copy = copy_longident
  end

  module Longident_loc = struct
    let copy = copy_longident_loc
  end

  module Rec_flag = struct
    let copy = copy_rec_flag
  end

  module Direction_flag = struct
    let copy = copy_direction_flag
  end

  module Private_flag = struct
    let copy = copy_private_flag
  end

  module Mutable_flag = struct
    let copy = copy_mutable_flag
  end

  module Virtual_flag = struct
    let copy = copy_virtual_flag
  end

  module Override_flag = struct
    let copy = copy_override_flag
  end

  module Closed_flag = struct
    let copy = copy_closed_flag
  end

  module Label = struct
    let copy = copy_label
  end

  module Label_loc = struct
    let copy = copy_label_loc
  end

  module String_loc = struct
    let copy = copy_string_loc
  end

  module Arg_label = struct
    let copy = copy_arg_label
  end

  module Variance = struct
    let copy = copy_variance
  end

  module Constant = struct
    let copy = copy_constant
  end

  module Attribute = struct
    let copy = copy_attribute
  end

  module Extension = struct
    let copy = copy_extension
  end

  module Attributes = struct
    let copy = copy_attributes
  end

  module Payload = struct
    let copy = copy_payload
  end

  module Core_type = struct
    let copy = copy_core_type
  end

  module Core_type_desc = struct
    let copy = copy_core_type_desc
  end

  module Package_type = struct
    let copy = copy_package_type
  end

  module Package_type_constraint = struct
    let copy = copy_package_type_constraint
  end

  module Row_field = struct
    let copy = copy_row_field
  end

  module Object_field = struct
    let copy = copy_object_field
  end

  module Pattern = struct
    let copy = copy_pattern
  end

  module Pattern_desc = struct
    let copy = copy_pattern_desc
  end

  module Record_field_pattern = struct
    let copy = copy_record_field_pattern
  end

  module Expression = struct
    let copy = copy_expression
  end

  module Expression_desc = struct
    let copy = copy_expression_desc
  end

  module Override_expression = struct
    let copy = copy_override_expression
  end

  module Record_field_expression = struct
    let copy = copy_record_field_expression
  end

  module Apply_arg = struct
    let copy = copy_apply_arg
  end

  module Case = struct
    let copy = copy_case
  end

  module Value_description = struct
    let copy = copy_value_description
  end

  module Type_declaration = struct
    let copy = copy_type_declaration
  end

  module Type_param = struct
    let copy = copy_type_param
  end

  module Type_constraint = struct
    let copy = copy_type_constraint
  end

  module Type_kind = struct
    let copy = copy_type_kind
  end

  module Label_declaration = struct
    let copy = copy_label_declaration
  end

  module Constructor_declaration = struct
    let copy = copy_constructor_declaration
  end

  module Constructor_arguments = struct
    let copy = copy_constructor_arguments
  end

  module Type_extension = struct
    let copy = copy_type_extension
  end

  module Extension_constructor = struct
    let copy = copy_extension_constructor
  end

  module Extension_constructor_kind = struct
    let copy = copy_extension_constructor_kind
  end

  module Class_type = struct
    let copy = copy_class_type
  end

  module Class_type_desc = struct
    let copy = copy_class_type_desc
  end

  module Class_signature = struct
    let copy = copy_class_signature
  end

  module Class_type_field = struct
    let copy = copy_class_type_field
  end

  module Class_type_field_desc = struct
    let copy = copy_class_type_field_desc
  end

  module Class_type_value_desc = struct
    let copy = copy_class_type_value_desc
  end

  module Class_type_method_desc = struct
    let copy = copy_class_type_method_desc
  end

  module Class_type_constraint = struct
    let copy = copy_class_type_constraint
  end

  module Class_description = struct
    let copy = copy_class_description
  end

  module Class_type_declaration = struct
    let copy = copy_class_type_declaration
  end

  module Class_expr = struct
    let copy = copy_class_expr
  end

  module Class_expr_desc = struct
    let copy = copy_class_expr_desc
  end

  module Class_structure = struct
    let copy = copy_class_structure
  end

  module Class_field = struct
    let copy = copy_class_field
  end

  module Class_field_desc = struct
    let copy = copy_class_field_desc
  end

  module Class_value_desc = struct
    let copy = copy_class_value_desc
  end

  module Class_method_desc = struct
    let copy = copy_class_method_desc
  end

  module Class_field_kind = struct
    let copy = copy_class_field_kind
  end

  module Class_declaration = struct
    let copy = copy_class_declaration
  end

  module Module_type = struct
    let copy = copy_module_type
  end

  module Module_type_desc = struct
    let copy = copy_module_type_desc
  end

  module Signature = struct
    let copy = copy_signature
  end

  module Signature_item = struct
    let copy = copy_signature_item
  end

  module Signature_item_desc = struct
    let copy = copy_signature_item_desc
  end

  module Module_declaration = struct
    let copy = copy_module_declaration
  end

  module Module_type_declaration = struct
    let copy = copy_module_type_declaration
  end

  module Open_description = struct
    let copy = copy_open_description
  end

  module Include_description = struct
    let copy = copy_include_description
  end

  module Include_declaration = struct
    let copy = copy_include_declaration
  end

  module With_constraint = struct
    let copy = copy_with_constraint
  end

  module Module_expr = struct
    let copy = copy_module_expr
  end

  module Module_expr_desc = struct
    let copy = copy_module_expr_desc
  end

  module Structure = struct
    let copy = copy_structure
  end

  module Structure_item = struct
    let copy = copy_structure_item
  end

  module Structure_item_desc = struct
    let copy = copy_structure_item_desc
  end

  module Value_binding = struct
    let copy = copy_value_binding
  end

  module Module_binding = struct
    let copy = copy_module_binding
  end

  module Toplevel_phrase = struct
    let copy = copy_toplevel_phrase
  end

  module Directive_argument = struct
    let copy = copy_directive_argument
  end
end
(*$*)
