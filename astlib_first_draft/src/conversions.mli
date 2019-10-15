(*$ Astlib_first_draft_src_cinaps.print_conversions_mli () *)
val longident_to_ast : Astlib_first_draft_parsetree.longident -> Stable.V4_07.Longident.t
val longident_of_ast : Stable.V4_07.Longident.t -> Astlib_first_draft_parsetree.longident option

val longident_loc_to_ast : Astlib_first_draft_parsetree.longident_loc -> Stable.V4_07.Longident_loc.t
val longident_loc_of_ast : Stable.V4_07.Longident_loc.t -> Astlib_first_draft_parsetree.longident_loc option

val rec_flag_to_ast : Astlib_first_draft_parsetree.rec_flag -> Stable.V4_07.Rec_flag.t
val rec_flag_of_ast : Stable.V4_07.Rec_flag.t -> Astlib_first_draft_parsetree.rec_flag option

val direction_flag_to_ast : Astlib_first_draft_parsetree.direction_flag -> Stable.V4_07.Direction_flag.t
val direction_flag_of_ast : Stable.V4_07.Direction_flag.t -> Astlib_first_draft_parsetree.direction_flag option

val private_flag_to_ast : Astlib_first_draft_parsetree.private_flag -> Stable.V4_07.Private_flag.t
val private_flag_of_ast : Stable.V4_07.Private_flag.t -> Astlib_first_draft_parsetree.private_flag option

val mutable_flag_to_ast : Astlib_first_draft_parsetree.mutable_flag -> Stable.V4_07.Mutable_flag.t
val mutable_flag_of_ast : Stable.V4_07.Mutable_flag.t -> Astlib_first_draft_parsetree.mutable_flag option

val virtual_flag_to_ast : Astlib_first_draft_parsetree.virtual_flag -> Stable.V4_07.Virtual_flag.t
val virtual_flag_of_ast : Stable.V4_07.Virtual_flag.t -> Astlib_first_draft_parsetree.virtual_flag option

val override_flag_to_ast : Astlib_first_draft_parsetree.override_flag -> Stable.V4_07.Override_flag.t
val override_flag_of_ast : Stable.V4_07.Override_flag.t -> Astlib_first_draft_parsetree.override_flag option

val closed_flag_to_ast : Astlib_first_draft_parsetree.closed_flag -> Stable.V4_07.Closed_flag.t
val closed_flag_of_ast : Stable.V4_07.Closed_flag.t -> Astlib_first_draft_parsetree.closed_flag option

val label_to_ast : Astlib_first_draft_parsetree.label -> Stable.V4_07.Label.t
val label_of_ast : Stable.V4_07.Label.t -> Astlib_first_draft_parsetree.label option

val label_loc_to_ast : Astlib_first_draft_parsetree.label_loc -> Stable.V4_07.Label_loc.t
val label_loc_of_ast : Stable.V4_07.Label_loc.t -> Astlib_first_draft_parsetree.label_loc option

val string_loc_to_ast : Astlib_first_draft_parsetree.string_loc -> Stable.V4_07.String_loc.t
val string_loc_of_ast : Stable.V4_07.String_loc.t -> Astlib_first_draft_parsetree.string_loc option

val arg_label_to_ast : Astlib_first_draft_parsetree.arg_label -> Stable.V4_07.Arg_label.t
val arg_label_of_ast : Stable.V4_07.Arg_label.t -> Astlib_first_draft_parsetree.arg_label option

val variance_to_ast : Astlib_first_draft_parsetree.variance -> Stable.V4_07.Variance.t
val variance_of_ast : Stable.V4_07.Variance.t -> Astlib_first_draft_parsetree.variance option

val constant_to_ast : Astlib_first_draft_parsetree.constant -> Stable.V4_07.Constant.t
val constant_of_ast : Stable.V4_07.Constant.t -> Astlib_first_draft_parsetree.constant option

val attribute_to_ast : Astlib_first_draft_parsetree.attribute -> Stable.V4_07.Attribute.t
val attribute_of_ast : Stable.V4_07.Attribute.t -> Astlib_first_draft_parsetree.attribute option

val extension_to_ast : Astlib_first_draft_parsetree.extension -> Stable.V4_07.Extension.t
val extension_of_ast : Stable.V4_07.Extension.t -> Astlib_first_draft_parsetree.extension option

val attributes_to_ast : Astlib_first_draft_parsetree.attributes -> Stable.V4_07.Attributes.t
val attributes_of_ast : Stable.V4_07.Attributes.t -> Astlib_first_draft_parsetree.attributes option

val payload_to_ast : Astlib_first_draft_parsetree.payload -> Stable.V4_07.Payload.t
val payload_of_ast : Stable.V4_07.Payload.t -> Astlib_first_draft_parsetree.payload option

val core_type_to_ast : Astlib_first_draft_parsetree.core_type -> Stable.V4_07.Core_type.t
val core_type_of_ast : Stable.V4_07.Core_type.t -> Astlib_first_draft_parsetree.core_type option

val core_type_desc_to_ast : Astlib_first_draft_parsetree.core_type_desc -> Stable.V4_07.Core_type_desc.t
val core_type_desc_of_ast : Stable.V4_07.Core_type_desc.t -> Astlib_first_draft_parsetree.core_type_desc option

val package_type_to_ast : Astlib_first_draft_parsetree.package_type -> Stable.V4_07.Package_type.t
val package_type_of_ast : Stable.V4_07.Package_type.t -> Astlib_first_draft_parsetree.package_type option

val package_type_constraint_to_ast : Astlib_first_draft_parsetree.package_type_constraint -> Stable.V4_07.Package_type_constraint.t
val package_type_constraint_of_ast : Stable.V4_07.Package_type_constraint.t -> Astlib_first_draft_parsetree.package_type_constraint option

val row_field_to_ast : Astlib_first_draft_parsetree.row_field -> Stable.V4_07.Row_field.t
val row_field_of_ast : Stable.V4_07.Row_field.t -> Astlib_first_draft_parsetree.row_field option

val object_field_to_ast : Astlib_first_draft_parsetree.object_field -> Stable.V4_07.Object_field.t
val object_field_of_ast : Stable.V4_07.Object_field.t -> Astlib_first_draft_parsetree.object_field option

val pattern_to_ast : Astlib_first_draft_parsetree.pattern -> Stable.V4_07.Pattern.t
val pattern_of_ast : Stable.V4_07.Pattern.t -> Astlib_first_draft_parsetree.pattern option

val pattern_desc_to_ast : Astlib_first_draft_parsetree.pattern_desc -> Stable.V4_07.Pattern_desc.t
val pattern_desc_of_ast : Stable.V4_07.Pattern_desc.t -> Astlib_first_draft_parsetree.pattern_desc option

val record_field_pattern_to_ast : Astlib_first_draft_parsetree.record_field_pattern -> Stable.V4_07.Record_field_pattern.t
val record_field_pattern_of_ast : Stable.V4_07.Record_field_pattern.t -> Astlib_first_draft_parsetree.record_field_pattern option

val expression_to_ast : Astlib_first_draft_parsetree.expression -> Stable.V4_07.Expression.t
val expression_of_ast : Stable.V4_07.Expression.t -> Astlib_first_draft_parsetree.expression option

val expression_desc_to_ast : Astlib_first_draft_parsetree.expression_desc -> Stable.V4_07.Expression_desc.t
val expression_desc_of_ast : Stable.V4_07.Expression_desc.t -> Astlib_first_draft_parsetree.expression_desc option

val override_expression_to_ast : Astlib_first_draft_parsetree.override_expression -> Stable.V4_07.Override_expression.t
val override_expression_of_ast : Stable.V4_07.Override_expression.t -> Astlib_first_draft_parsetree.override_expression option

val record_field_expression_to_ast : Astlib_first_draft_parsetree.record_field_expression -> Stable.V4_07.Record_field_expression.t
val record_field_expression_of_ast : Stable.V4_07.Record_field_expression.t -> Astlib_first_draft_parsetree.record_field_expression option

val apply_arg_to_ast : Astlib_first_draft_parsetree.apply_arg -> Stable.V4_07.Apply_arg.t
val apply_arg_of_ast : Stable.V4_07.Apply_arg.t -> Astlib_first_draft_parsetree.apply_arg option

val case_to_ast : Astlib_first_draft_parsetree.case -> Stable.V4_07.Case.t
val case_of_ast : Stable.V4_07.Case.t -> Astlib_first_draft_parsetree.case option

val value_description_to_ast : Astlib_first_draft_parsetree.value_description -> Stable.V4_07.Value_description.t
val value_description_of_ast : Stable.V4_07.Value_description.t -> Astlib_first_draft_parsetree.value_description option

val type_declaration_to_ast : Astlib_first_draft_parsetree.type_declaration -> Stable.V4_07.Type_declaration.t
val type_declaration_of_ast : Stable.V4_07.Type_declaration.t -> Astlib_first_draft_parsetree.type_declaration option

val type_param_to_ast : Astlib_first_draft_parsetree.type_param -> Stable.V4_07.Type_param.t
val type_param_of_ast : Stable.V4_07.Type_param.t -> Astlib_first_draft_parsetree.type_param option

val type_constraint_to_ast : Astlib_first_draft_parsetree.type_constraint -> Stable.V4_07.Type_constraint.t
val type_constraint_of_ast : Stable.V4_07.Type_constraint.t -> Astlib_first_draft_parsetree.type_constraint option

val type_kind_to_ast : Astlib_first_draft_parsetree.type_kind -> Stable.V4_07.Type_kind.t
val type_kind_of_ast : Stable.V4_07.Type_kind.t -> Astlib_first_draft_parsetree.type_kind option

val label_declaration_to_ast : Astlib_first_draft_parsetree.label_declaration -> Stable.V4_07.Label_declaration.t
val label_declaration_of_ast : Stable.V4_07.Label_declaration.t -> Astlib_first_draft_parsetree.label_declaration option

val constructor_declaration_to_ast : Astlib_first_draft_parsetree.constructor_declaration -> Stable.V4_07.Constructor_declaration.t
val constructor_declaration_of_ast : Stable.V4_07.Constructor_declaration.t -> Astlib_first_draft_parsetree.constructor_declaration option

val constructor_arguments_to_ast : Astlib_first_draft_parsetree.constructor_arguments -> Stable.V4_07.Constructor_arguments.t
val constructor_arguments_of_ast : Stable.V4_07.Constructor_arguments.t -> Astlib_first_draft_parsetree.constructor_arguments option

val type_extension_to_ast : Astlib_first_draft_parsetree.type_extension -> Stable.V4_07.Type_extension.t
val type_extension_of_ast : Stable.V4_07.Type_extension.t -> Astlib_first_draft_parsetree.type_extension option

val extension_constructor_to_ast : Astlib_first_draft_parsetree.extension_constructor -> Stable.V4_07.Extension_constructor.t
val extension_constructor_of_ast : Stable.V4_07.Extension_constructor.t -> Astlib_first_draft_parsetree.extension_constructor option

val extension_constructor_kind_to_ast : Astlib_first_draft_parsetree.extension_constructor_kind -> Stable.V4_07.Extension_constructor_kind.t
val extension_constructor_kind_of_ast : Stable.V4_07.Extension_constructor_kind.t -> Astlib_first_draft_parsetree.extension_constructor_kind option

val class_type_to_ast : Astlib_first_draft_parsetree.class_type -> Stable.V4_07.Class_type.t
val class_type_of_ast : Stable.V4_07.Class_type.t -> Astlib_first_draft_parsetree.class_type option

val class_type_desc_to_ast : Astlib_first_draft_parsetree.class_type_desc -> Stable.V4_07.Class_type_desc.t
val class_type_desc_of_ast : Stable.V4_07.Class_type_desc.t -> Astlib_first_draft_parsetree.class_type_desc option

val class_signature_to_ast : Astlib_first_draft_parsetree.class_signature -> Stable.V4_07.Class_signature.t
val class_signature_of_ast : Stable.V4_07.Class_signature.t -> Astlib_first_draft_parsetree.class_signature option

val class_type_field_to_ast : Astlib_first_draft_parsetree.class_type_field -> Stable.V4_07.Class_type_field.t
val class_type_field_of_ast : Stable.V4_07.Class_type_field.t -> Astlib_first_draft_parsetree.class_type_field option

val class_type_field_desc_to_ast : Astlib_first_draft_parsetree.class_type_field_desc -> Stable.V4_07.Class_type_field_desc.t
val class_type_field_desc_of_ast : Stable.V4_07.Class_type_field_desc.t -> Astlib_first_draft_parsetree.class_type_field_desc option

val class_type_value_desc_to_ast : Astlib_first_draft_parsetree.class_type_value_desc -> Stable.V4_07.Class_type_value_desc.t
val class_type_value_desc_of_ast : Stable.V4_07.Class_type_value_desc.t -> Astlib_first_draft_parsetree.class_type_value_desc option

val class_type_method_desc_to_ast : Astlib_first_draft_parsetree.class_type_method_desc -> Stable.V4_07.Class_type_method_desc.t
val class_type_method_desc_of_ast : Stable.V4_07.Class_type_method_desc.t -> Astlib_first_draft_parsetree.class_type_method_desc option

val class_type_constraint_to_ast : Astlib_first_draft_parsetree.class_type_constraint -> Stable.V4_07.Class_type_constraint.t
val class_type_constraint_of_ast : Stable.V4_07.Class_type_constraint.t -> Astlib_first_draft_parsetree.class_type_constraint option

val class_description_to_ast : Astlib_first_draft_parsetree.class_description -> Stable.V4_07.Class_description.t
val class_description_of_ast : Stable.V4_07.Class_description.t -> Astlib_first_draft_parsetree.class_description option

val class_type_declaration_to_ast : Astlib_first_draft_parsetree.class_type_declaration -> Stable.V4_07.Class_type_declaration.t
val class_type_declaration_of_ast : Stable.V4_07.Class_type_declaration.t -> Astlib_first_draft_parsetree.class_type_declaration option

val class_expr_to_ast : Astlib_first_draft_parsetree.class_expr -> Stable.V4_07.Class_expr.t
val class_expr_of_ast : Stable.V4_07.Class_expr.t -> Astlib_first_draft_parsetree.class_expr option

val class_expr_desc_to_ast : Astlib_first_draft_parsetree.class_expr_desc -> Stable.V4_07.Class_expr_desc.t
val class_expr_desc_of_ast : Stable.V4_07.Class_expr_desc.t -> Astlib_first_draft_parsetree.class_expr_desc option

val class_structure_to_ast : Astlib_first_draft_parsetree.class_structure -> Stable.V4_07.Class_structure.t
val class_structure_of_ast : Stable.V4_07.Class_structure.t -> Astlib_first_draft_parsetree.class_structure option

val class_field_to_ast : Astlib_first_draft_parsetree.class_field -> Stable.V4_07.Class_field.t
val class_field_of_ast : Stable.V4_07.Class_field.t -> Astlib_first_draft_parsetree.class_field option

val class_field_desc_to_ast : Astlib_first_draft_parsetree.class_field_desc -> Stable.V4_07.Class_field_desc.t
val class_field_desc_of_ast : Stable.V4_07.Class_field_desc.t -> Astlib_first_draft_parsetree.class_field_desc option

val class_value_desc_to_ast : Astlib_first_draft_parsetree.class_value_desc -> Stable.V4_07.Class_value_desc.t
val class_value_desc_of_ast : Stable.V4_07.Class_value_desc.t -> Astlib_first_draft_parsetree.class_value_desc option

val class_method_desc_to_ast : Astlib_first_draft_parsetree.class_method_desc -> Stable.V4_07.Class_method_desc.t
val class_method_desc_of_ast : Stable.V4_07.Class_method_desc.t -> Astlib_first_draft_parsetree.class_method_desc option

val class_field_kind_to_ast : Astlib_first_draft_parsetree.class_field_kind -> Stable.V4_07.Class_field_kind.t
val class_field_kind_of_ast : Stable.V4_07.Class_field_kind.t -> Astlib_first_draft_parsetree.class_field_kind option

val class_declaration_to_ast : Astlib_first_draft_parsetree.class_declaration -> Stable.V4_07.Class_declaration.t
val class_declaration_of_ast : Stable.V4_07.Class_declaration.t -> Astlib_first_draft_parsetree.class_declaration option

val module_type_to_ast : Astlib_first_draft_parsetree.module_type -> Stable.V4_07.Module_type.t
val module_type_of_ast : Stable.V4_07.Module_type.t -> Astlib_first_draft_parsetree.module_type option

val module_type_desc_to_ast : Astlib_first_draft_parsetree.module_type_desc -> Stable.V4_07.Module_type_desc.t
val module_type_desc_of_ast : Stable.V4_07.Module_type_desc.t -> Astlib_first_draft_parsetree.module_type_desc option

val signature_to_ast : Astlib_first_draft_parsetree.signature -> Stable.V4_07.Signature.t
val signature_of_ast : Stable.V4_07.Signature.t -> Astlib_first_draft_parsetree.signature option

val signature_item_to_ast : Astlib_first_draft_parsetree.signature_item -> Stable.V4_07.Signature_item.t
val signature_item_of_ast : Stable.V4_07.Signature_item.t -> Astlib_first_draft_parsetree.signature_item option

val signature_item_desc_to_ast : Astlib_first_draft_parsetree.signature_item_desc -> Stable.V4_07.Signature_item_desc.t
val signature_item_desc_of_ast : Stable.V4_07.Signature_item_desc.t -> Astlib_first_draft_parsetree.signature_item_desc option

val module_declaration_to_ast : Astlib_first_draft_parsetree.module_declaration -> Stable.V4_07.Module_declaration.t
val module_declaration_of_ast : Stable.V4_07.Module_declaration.t -> Astlib_first_draft_parsetree.module_declaration option

val module_type_declaration_to_ast : Astlib_first_draft_parsetree.module_type_declaration -> Stable.V4_07.Module_type_declaration.t
val module_type_declaration_of_ast : Stable.V4_07.Module_type_declaration.t -> Astlib_first_draft_parsetree.module_type_declaration option

val open_description_to_ast : Astlib_first_draft_parsetree.open_description -> Stable.V4_07.Open_description.t
val open_description_of_ast : Stable.V4_07.Open_description.t -> Astlib_first_draft_parsetree.open_description option

val include_description_to_ast : Astlib_first_draft_parsetree.include_description -> Stable.V4_07.Include_description.t
val include_description_of_ast : Stable.V4_07.Include_description.t -> Astlib_first_draft_parsetree.include_description option

val include_declaration_to_ast : Astlib_first_draft_parsetree.include_declaration -> Stable.V4_07.Include_declaration.t
val include_declaration_of_ast : Stable.V4_07.Include_declaration.t -> Astlib_first_draft_parsetree.include_declaration option

val with_constraint_to_ast : Astlib_first_draft_parsetree.with_constraint -> Stable.V4_07.With_constraint.t
val with_constraint_of_ast : Stable.V4_07.With_constraint.t -> Astlib_first_draft_parsetree.with_constraint option

val module_expr_to_ast : Astlib_first_draft_parsetree.module_expr -> Stable.V4_07.Module_expr.t
val module_expr_of_ast : Stable.V4_07.Module_expr.t -> Astlib_first_draft_parsetree.module_expr option

val module_expr_desc_to_ast : Astlib_first_draft_parsetree.module_expr_desc -> Stable.V4_07.Module_expr_desc.t
val module_expr_desc_of_ast : Stable.V4_07.Module_expr_desc.t -> Astlib_first_draft_parsetree.module_expr_desc option

val structure_to_ast : Astlib_first_draft_parsetree.structure -> Stable.V4_07.Structure.t
val structure_of_ast : Stable.V4_07.Structure.t -> Astlib_first_draft_parsetree.structure option

val structure_item_to_ast : Astlib_first_draft_parsetree.structure_item -> Stable.V4_07.Structure_item.t
val structure_item_of_ast : Stable.V4_07.Structure_item.t -> Astlib_first_draft_parsetree.structure_item option

val structure_item_desc_to_ast : Astlib_first_draft_parsetree.structure_item_desc -> Stable.V4_07.Structure_item_desc.t
val structure_item_desc_of_ast : Stable.V4_07.Structure_item_desc.t -> Astlib_first_draft_parsetree.structure_item_desc option

val value_binding_to_ast : Astlib_first_draft_parsetree.value_binding -> Stable.V4_07.Value_binding.t
val value_binding_of_ast : Stable.V4_07.Value_binding.t -> Astlib_first_draft_parsetree.value_binding option

val module_binding_to_ast : Astlib_first_draft_parsetree.module_binding -> Stable.V4_07.Module_binding.t
val module_binding_of_ast : Stable.V4_07.Module_binding.t -> Astlib_first_draft_parsetree.module_binding option

val toplevel_phrase_to_ast : Astlib_first_draft_parsetree.toplevel_phrase -> Stable.V4_07.Toplevel_phrase.t
val toplevel_phrase_of_ast : Stable.V4_07.Toplevel_phrase.t -> Astlib_first_draft_parsetree.toplevel_phrase option

val directive_argument_to_ast : Astlib_first_draft_parsetree.directive_argument -> Stable.V4_07.Directive_argument.t
val directive_argument_of_ast : Stable.V4_07.Directive_argument.t -> Astlib_first_draft_parsetree.directive_argument option
(*$*)
