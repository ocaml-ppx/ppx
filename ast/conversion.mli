(*$ Ppx_ast_cinaps.print_conversion_mli () *)
val ast_of_longident :
  Compiler_types.longident
  -> Versions.V4_08.Longident.t

val ast_to_longident :
  Versions.V4_08.Longident.t
  -> Compiler_types.longident

val ast_of_longident_loc :
  Compiler_types.longident_loc
  -> Versions.V4_08.Longident_loc.t

val ast_to_longident_loc :
  Versions.V4_08.Longident_loc.t
  -> Compiler_types.longident_loc

val ast_of_rec_flag :
  Compiler_types.rec_flag
  -> Versions.V4_08.Rec_flag.t

val ast_to_rec_flag :
  Versions.V4_08.Rec_flag.t
  -> Compiler_types.rec_flag

val ast_of_direction_flag :
  Compiler_types.direction_flag
  -> Versions.V4_08.Direction_flag.t

val ast_to_direction_flag :
  Versions.V4_08.Direction_flag.t
  -> Compiler_types.direction_flag

val ast_of_private_flag :
  Compiler_types.private_flag
  -> Versions.V4_08.Private_flag.t

val ast_to_private_flag :
  Versions.V4_08.Private_flag.t
  -> Compiler_types.private_flag

val ast_of_mutable_flag :
  Compiler_types.mutable_flag
  -> Versions.V4_08.Mutable_flag.t

val ast_to_mutable_flag :
  Versions.V4_08.Mutable_flag.t
  -> Compiler_types.mutable_flag

val ast_of_virtual_flag :
  Compiler_types.virtual_flag
  -> Versions.V4_08.Virtual_flag.t

val ast_to_virtual_flag :
  Versions.V4_08.Virtual_flag.t
  -> Compiler_types.virtual_flag

val ast_of_override_flag :
  Compiler_types.override_flag
  -> Versions.V4_08.Override_flag.t

val ast_to_override_flag :
  Versions.V4_08.Override_flag.t
  -> Compiler_types.override_flag

val ast_of_closed_flag :
  Compiler_types.closed_flag
  -> Versions.V4_08.Closed_flag.t

val ast_to_closed_flag :
  Versions.V4_08.Closed_flag.t
  -> Compiler_types.closed_flag

val ast_of_arg_label :
  Compiler_types.arg_label
  -> Versions.V4_08.Arg_label.t

val ast_to_arg_label :
  Versions.V4_08.Arg_label.t
  -> Compiler_types.arg_label

val ast_of_variance :
  Compiler_types.variance
  -> Versions.V4_08.Variance.t

val ast_to_variance :
  Versions.V4_08.Variance.t
  -> Compiler_types.variance

val ast_of_constant :
  Compiler_types.constant
  -> Versions.V4_08.Constant.t

val ast_to_constant :
  Versions.V4_08.Constant.t
  -> Compiler_types.constant

val ast_of_attribute :
  Compiler_types.attribute
  -> Versions.V4_08.Attribute.t

val ast_to_attribute :
  Versions.V4_08.Attribute.t
  -> Compiler_types.attribute

val ast_of_extension :
  Compiler_types.extension
  -> Versions.V4_08.Extension.t

val ast_to_extension :
  Versions.V4_08.Extension.t
  -> Compiler_types.extension

val ast_of_attributes :
  Compiler_types.attributes
  -> Versions.V4_08.Attributes.t

val ast_to_attributes :
  Versions.V4_08.Attributes.t
  -> Compiler_types.attributes

val ast_of_payload :
  Compiler_types.payload
  -> Versions.V4_08.Payload.t

val ast_to_payload :
  Versions.V4_08.Payload.t
  -> Compiler_types.payload

val ast_of_core_type :
  Compiler_types.core_type
  -> Versions.V4_08.Core_type.t

val ast_to_core_type :
  Versions.V4_08.Core_type.t
  -> Compiler_types.core_type

val ast_of_core_type_desc :
  Compiler_types.core_type_desc
  -> Versions.V4_08.Core_type_desc.t

val ast_to_core_type_desc :
  Versions.V4_08.Core_type_desc.t
  -> Compiler_types.core_type_desc

val ast_of_package_type :
  Compiler_types.package_type
  -> Versions.V4_08.Package_type.t

val ast_to_package_type :
  Versions.V4_08.Package_type.t
  -> Compiler_types.package_type

val ast_of_row_field :
  Compiler_types.row_field
  -> Versions.V4_08.Row_field.t

val ast_to_row_field :
  Versions.V4_08.Row_field.t
  -> Compiler_types.row_field

val ast_of_object_field :
  Compiler_types.object_field
  -> Versions.V4_08.Object_field.t

val ast_to_object_field :
  Versions.V4_08.Object_field.t
  -> Compiler_types.object_field

val ast_of_pattern :
  Compiler_types.pattern
  -> Versions.V4_08.Pattern.t

val ast_to_pattern :
  Versions.V4_08.Pattern.t
  -> Compiler_types.pattern

val ast_of_pattern_desc :
  Compiler_types.pattern_desc
  -> Versions.V4_08.Pattern_desc.t

val ast_to_pattern_desc :
  Versions.V4_08.Pattern_desc.t
  -> Compiler_types.pattern_desc

val ast_of_expression :
  Compiler_types.expression
  -> Versions.V4_08.Expression.t

val ast_to_expression :
  Versions.V4_08.Expression.t
  -> Compiler_types.expression

val ast_of_expression_desc :
  Compiler_types.expression_desc
  -> Versions.V4_08.Expression_desc.t

val ast_to_expression_desc :
  Versions.V4_08.Expression_desc.t
  -> Compiler_types.expression_desc

val ast_of_case :
  Compiler_types.case
  -> Versions.V4_08.Case.t

val ast_to_case :
  Versions.V4_08.Case.t
  -> Compiler_types.case

val ast_of_value_description :
  Compiler_types.value_description
  -> Versions.V4_08.Value_description.t

val ast_to_value_description :
  Versions.V4_08.Value_description.t
  -> Compiler_types.value_description

val ast_of_type_declaration :
  Compiler_types.type_declaration
  -> Versions.V4_08.Type_declaration.t

val ast_to_type_declaration :
  Versions.V4_08.Type_declaration.t
  -> Compiler_types.type_declaration

val ast_of_type_kind :
  Compiler_types.type_kind
  -> Versions.V4_08.Type_kind.t

val ast_to_type_kind :
  Versions.V4_08.Type_kind.t
  -> Compiler_types.type_kind

val ast_of_label_declaration :
  Compiler_types.label_declaration
  -> Versions.V4_08.Label_declaration.t

val ast_to_label_declaration :
  Versions.V4_08.Label_declaration.t
  -> Compiler_types.label_declaration

val ast_of_constructor_declaration :
  Compiler_types.constructor_declaration
  -> Versions.V4_08.Constructor_declaration.t

val ast_to_constructor_declaration :
  Versions.V4_08.Constructor_declaration.t
  -> Compiler_types.constructor_declaration

val ast_of_constructor_arguments :
  Compiler_types.constructor_arguments
  -> Versions.V4_08.Constructor_arguments.t

val ast_to_constructor_arguments :
  Versions.V4_08.Constructor_arguments.t
  -> Compiler_types.constructor_arguments

val ast_of_type_extension :
  Compiler_types.type_extension
  -> Versions.V4_08.Type_extension.t

val ast_to_type_extension :
  Versions.V4_08.Type_extension.t
  -> Compiler_types.type_extension

val ast_of_extension_constructor :
  Compiler_types.extension_constructor
  -> Versions.V4_08.Extension_constructor.t

val ast_to_extension_constructor :
  Versions.V4_08.Extension_constructor.t
  -> Compiler_types.extension_constructor

val ast_of_extension_constructor_kind :
  Compiler_types.extension_constructor_kind
  -> Versions.V4_08.Extension_constructor_kind.t

val ast_to_extension_constructor_kind :
  Versions.V4_08.Extension_constructor_kind.t
  -> Compiler_types.extension_constructor_kind

val ast_of_class_type :
  Compiler_types.class_type
  -> Versions.V4_08.Class_type.t

val ast_to_class_type :
  Versions.V4_08.Class_type.t
  -> Compiler_types.class_type

val ast_of_class_type_desc :
  Compiler_types.class_type_desc
  -> Versions.V4_08.Class_type_desc.t

val ast_to_class_type_desc :
  Versions.V4_08.Class_type_desc.t
  -> Compiler_types.class_type_desc

val ast_of_class_signature :
  Compiler_types.class_signature
  -> Versions.V4_08.Class_signature.t

val ast_to_class_signature :
  Versions.V4_08.Class_signature.t
  -> Compiler_types.class_signature

val ast_of_class_type_field :
  Compiler_types.class_type_field
  -> Versions.V4_08.Class_type_field.t

val ast_to_class_type_field :
  Versions.V4_08.Class_type_field.t
  -> Compiler_types.class_type_field

val ast_of_class_type_field_desc :
  Compiler_types.class_type_field_desc
  -> Versions.V4_08.Class_type_field_desc.t

val ast_to_class_type_field_desc :
  Versions.V4_08.Class_type_field_desc.t
  -> Compiler_types.class_type_field_desc

val ast_of_class_infos :
  ('a_ -> 'a Unversioned.Types.node)
  -> 'a_ Compiler_types.class_infos
  -> 'a Unversioned.Types.node Versions.V4_08.Class_infos.t

val ast_to_class_infos :
  ('a Unversioned.Types.node -> 'a_)
  -> 'a Unversioned.Types.node Versions.V4_08.Class_infos.t
  -> 'a_ Compiler_types.class_infos

val ast_of_class_description :
  Compiler_types.class_description
  -> Versions.V4_08.Class_description.t

val ast_to_class_description :
  Versions.V4_08.Class_description.t
  -> Compiler_types.class_description

val ast_of_class_type_declaration :
  Compiler_types.class_type_declaration
  -> Versions.V4_08.Class_type_declaration.t

val ast_to_class_type_declaration :
  Versions.V4_08.Class_type_declaration.t
  -> Compiler_types.class_type_declaration

val ast_of_class_expr :
  Compiler_types.class_expr
  -> Versions.V4_08.Class_expr.t

val ast_to_class_expr :
  Versions.V4_08.Class_expr.t
  -> Compiler_types.class_expr

val ast_of_class_expr_desc :
  Compiler_types.class_expr_desc
  -> Versions.V4_08.Class_expr_desc.t

val ast_to_class_expr_desc :
  Versions.V4_08.Class_expr_desc.t
  -> Compiler_types.class_expr_desc

val ast_of_class_structure :
  Compiler_types.class_structure
  -> Versions.V4_08.Class_structure.t

val ast_to_class_structure :
  Versions.V4_08.Class_structure.t
  -> Compiler_types.class_structure

val ast_of_class_field :
  Compiler_types.class_field
  -> Versions.V4_08.Class_field.t

val ast_to_class_field :
  Versions.V4_08.Class_field.t
  -> Compiler_types.class_field

val ast_of_class_field_desc :
  Compiler_types.class_field_desc
  -> Versions.V4_08.Class_field_desc.t

val ast_to_class_field_desc :
  Versions.V4_08.Class_field_desc.t
  -> Compiler_types.class_field_desc

val ast_of_class_field_kind :
  Compiler_types.class_field_kind
  -> Versions.V4_08.Class_field_kind.t

val ast_to_class_field_kind :
  Versions.V4_08.Class_field_kind.t
  -> Compiler_types.class_field_kind

val ast_of_class_declaration :
  Compiler_types.class_declaration
  -> Versions.V4_08.Class_declaration.t

val ast_to_class_declaration :
  Versions.V4_08.Class_declaration.t
  -> Compiler_types.class_declaration

val ast_of_module_type :
  Compiler_types.module_type
  -> Versions.V4_08.Module_type.t

val ast_to_module_type :
  Versions.V4_08.Module_type.t
  -> Compiler_types.module_type

val ast_of_module_type_desc :
  Compiler_types.module_type_desc
  -> Versions.V4_08.Module_type_desc.t

val ast_to_module_type_desc :
  Versions.V4_08.Module_type_desc.t
  -> Compiler_types.module_type_desc

val ast_of_signature :
  Compiler_types.signature
  -> Versions.V4_08.Signature.t

val ast_to_signature :
  Versions.V4_08.Signature.t
  -> Compiler_types.signature

val ast_of_signature_item :
  Compiler_types.signature_item
  -> Versions.V4_08.Signature_item.t

val ast_to_signature_item :
  Versions.V4_08.Signature_item.t
  -> Compiler_types.signature_item

val ast_of_signature_item_desc :
  Compiler_types.signature_item_desc
  -> Versions.V4_08.Signature_item_desc.t

val ast_to_signature_item_desc :
  Versions.V4_08.Signature_item_desc.t
  -> Compiler_types.signature_item_desc

val ast_of_module_declaration :
  Compiler_types.module_declaration
  -> Versions.V4_08.Module_declaration.t

val ast_to_module_declaration :
  Versions.V4_08.Module_declaration.t
  -> Compiler_types.module_declaration

val ast_of_module_type_declaration :
  Compiler_types.module_type_declaration
  -> Versions.V4_08.Module_type_declaration.t

val ast_to_module_type_declaration :
  Versions.V4_08.Module_type_declaration.t
  -> Compiler_types.module_type_declaration

val ast_of_open_description :
  Compiler_types.open_description
  -> Versions.V4_08.Open_description.t

val ast_to_open_description :
  Versions.V4_08.Open_description.t
  -> Compiler_types.open_description

val ast_of_include_infos :
  ('a_ -> 'a Unversioned.Types.node)
  -> 'a_ Compiler_types.include_infos
  -> 'a Unversioned.Types.node Versions.V4_08.Include_infos.t

val ast_to_include_infos :
  ('a Unversioned.Types.node -> 'a_)
  -> 'a Unversioned.Types.node Versions.V4_08.Include_infos.t
  -> 'a_ Compiler_types.include_infos

val ast_of_include_description :
  Compiler_types.include_description
  -> Versions.V4_08.Include_description.t

val ast_to_include_description :
  Versions.V4_08.Include_description.t
  -> Compiler_types.include_description

val ast_of_include_declaration :
  Compiler_types.include_declaration
  -> Versions.V4_08.Include_declaration.t

val ast_to_include_declaration :
  Versions.V4_08.Include_declaration.t
  -> Compiler_types.include_declaration

val ast_of_with_constraint :
  Compiler_types.with_constraint
  -> Versions.V4_08.With_constraint.t

val ast_to_with_constraint :
  Versions.V4_08.With_constraint.t
  -> Compiler_types.with_constraint

val ast_of_module_expr :
  Compiler_types.module_expr
  -> Versions.V4_08.Module_expr.t

val ast_to_module_expr :
  Versions.V4_08.Module_expr.t
  -> Compiler_types.module_expr

val ast_of_module_expr_desc :
  Compiler_types.module_expr_desc
  -> Versions.V4_08.Module_expr_desc.t

val ast_to_module_expr_desc :
  Versions.V4_08.Module_expr_desc.t
  -> Compiler_types.module_expr_desc

val ast_of_structure :
  Compiler_types.structure
  -> Versions.V4_08.Structure.t

val ast_to_structure :
  Versions.V4_08.Structure.t
  -> Compiler_types.structure

val ast_of_structure_item :
  Compiler_types.structure_item
  -> Versions.V4_08.Structure_item.t

val ast_to_structure_item :
  Versions.V4_08.Structure_item.t
  -> Compiler_types.structure_item

val ast_of_structure_item_desc :
  Compiler_types.structure_item_desc
  -> Versions.V4_08.Structure_item_desc.t

val ast_to_structure_item_desc :
  Versions.V4_08.Structure_item_desc.t
  -> Compiler_types.structure_item_desc

val ast_of_value_binding :
  Compiler_types.value_binding
  -> Versions.V4_08.Value_binding.t

val ast_to_value_binding :
  Versions.V4_08.Value_binding.t
  -> Compiler_types.value_binding

val ast_of_module_binding :
  Compiler_types.module_binding
  -> Versions.V4_08.Module_binding.t

val ast_to_module_binding :
  Versions.V4_08.Module_binding.t
  -> Compiler_types.module_binding

val ast_of_toplevel_phrase :
  Compiler_types.toplevel_phrase
  -> Versions.V4_08.Toplevel_phrase.t

val ast_to_toplevel_phrase :
  Versions.V4_08.Toplevel_phrase.t
  -> Compiler_types.toplevel_phrase

val ast_of_directive_argument :
  Compiler_types.directive_argument
  -> Versions.V4_08.Directive_argument.t

val ast_to_directive_argument :
  Versions.V4_08.Directive_argument.t
  -> Compiler_types.directive_argument
(*$*)
