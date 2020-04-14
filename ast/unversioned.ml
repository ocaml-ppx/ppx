module Types = struct
  type _ node = Node.t

  (*$ Ppx_ast_cinaps.print_unversioned_types () *)
  type arg_label_
  type attribute_
  type attributes_
  type case_
  type class_declaration_
  type class_description_
  type class_expr_
  type class_expr_desc_
  type class_field_
  type class_field_desc_
  type class_field_kind_
  type 'a class_infos_
  type class_signature_
  type class_structure_
  type class_type_
  type class_type_declaration_
  type class_type_desc_
  type class_type_field_
  type class_type_field_desc_
  type closed_flag_
  type constant_
  type constructor_arguments_
  type constructor_declaration_
  type core_type_
  type core_type_desc_
  type direction_flag_
  type directive_argument_
  type expression_
  type expression_desc_
  type extension_
  type extension_constructor_
  type extension_constructor_kind_
  type include_declaration_
  type include_description_
  type 'a include_infos_
  type label_declaration_
  type longident_
  type longident_loc_
  type module_binding_
  type module_declaration_
  type module_expr_
  type module_expr_desc_
  type module_type_
  type module_type_declaration_
  type module_type_desc_
  type mutable_flag_
  type object_field_
  type open_description_
  type override_flag_
  type package_type_
  type pattern_
  type pattern_desc_
  type payload_
  type private_flag_
  type rec_flag_
  type row_field_
  type signature_
  type signature_item_
  type signature_item_desc_
  type structure_
  type structure_item_
  type structure_item_desc_
  type toplevel_phrase_
  type type_declaration_
  type type_extension_
  type type_kind_
  type value_binding_
  type value_description_
  type variance_
  type virtual_flag_
  type with_constraint_

  type arg_label = arg_label_ node
  type attribute = attribute_ node
  type attributes = attributes_ node
  type case = case_ node
  type class_declaration = class_declaration_ node
  type class_description = class_description_ node
  type class_expr = class_expr_ node
  type class_expr_desc = class_expr_desc_ node
  type class_field = class_field_ node
  type class_field_desc = class_field_desc_ node
  type class_field_kind = class_field_kind_ node
  type 'a class_infos = 'a class_infos_ node
  type class_signature = class_signature_ node
  type class_structure = class_structure_ node
  type class_type = class_type_ node
  type class_type_declaration = class_type_declaration_ node
  type class_type_desc = class_type_desc_ node
  type class_type_field = class_type_field_ node
  type class_type_field_desc = class_type_field_desc_ node
  type closed_flag = closed_flag_ node
  type constant = constant_ node
  type constructor_arguments = constructor_arguments_ node
  type constructor_declaration = constructor_declaration_ node
  type core_type = core_type_ node
  type core_type_desc = core_type_desc_ node
  type direction_flag = direction_flag_ node
  type directive_argument = directive_argument_ node
  type expression = expression_ node
  type expression_desc = expression_desc_ node
  type extension = extension_ node
  type extension_constructor = extension_constructor_ node
  type extension_constructor_kind = extension_constructor_kind_ node
  type include_declaration = include_declaration_ node
  type include_description = include_description_ node
  type 'a include_infos = 'a include_infos_ node
  type label_declaration = label_declaration_ node
  type longident = longident_ node
  type longident_loc = longident_loc_ node
  type module_binding = module_binding_ node
  type module_declaration = module_declaration_ node
  type module_expr = module_expr_ node
  type module_expr_desc = module_expr_desc_ node
  type module_type = module_type_ node
  type module_type_declaration = module_type_declaration_ node
  type module_type_desc = module_type_desc_ node
  type mutable_flag = mutable_flag_ node
  type object_field = object_field_ node
  type open_description = open_description_ node
  type override_flag = override_flag_ node
  type package_type = package_type_ node
  type pattern = pattern_ node
  type pattern_desc = pattern_desc_ node
  type payload = payload_ node
  type private_flag = private_flag_ node
  type rec_flag = rec_flag_ node
  type row_field = row_field_ node
  type signature = signature_ node
  type signature_item = signature_item_ node
  type signature_item_desc = signature_item_desc_ node
  type structure = structure_ node
  type structure_item = structure_item_ node
  type structure_item_desc = structure_item_desc_ node
  type toplevel_phrase = toplevel_phrase_ node
  type type_declaration = type_declaration_ node
  type type_extension = type_extension_ node
  type type_kind = type_kind_ node
  type value_binding = value_binding_ node
  type value_description = value_description_ node
  type variance = variance_ node
  type virtual_flag = virtual_flag_ node
  type with_constraint = with_constraint_ node
(*$*)
end

module Private = struct
  let opaque node = node
  let transparent node = node

  exception Cannot_interpret_ast of {
    version : Astlib.Version.t;
    node_name : string;
    node : Node.t;
  }
end
