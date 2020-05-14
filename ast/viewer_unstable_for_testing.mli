open Viewlib

(*$ Ppx_ast_cinaps.print_viewer_mli (Astlib.Version.of_string "unstable_for_testing") *)
open Versions
include module type of Viewer_common

val pdir_bool'const : (bool, 'i, 'o) View.t -> (directive_argument, 'i, 'o) View.t

val pdir_ident'const : (longident, 'i, 'o) View.t -> (directive_argument, 'i, 'o) View.t

val pdir_int'const : ((char option * string), 'i, 'o) View.t -> (directive_argument, 'i, 'o) View.t

val pdir_string'const : (string, 'i, 'o) View.t -> (directive_argument, 'i, 'o) View.t

val pdir_none'const : (directive_argument, 'a, 'a) View.t

val ptop_dir'const : ((directive_argument * string), 'i, 'o) View.t -> (toplevel_phrase, 'i, 'o) View.t

val ptop_def'const : (structure, 'i, 'o) View.t -> (toplevel_phrase, 'i, 'o) View.t

val pmb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (module_binding, 'i, 'o) View.t

val pmb_attributes'match : (attributes, 'i, 'o) View.t -> (module_binding, 'i, 'o) View.t

val pmb_expr'match : (module_expr, 'i, 'o) View.t -> (module_binding, 'i, 'o) View.t

val pmb_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (module_binding, 'i, 'o) View.t

val pvb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (value_binding, 'i, 'o) View.t

val pvb_attributes'match : (attributes, 'i, 'o) View.t -> (value_binding, 'i, 'o) View.t

val pvb_expr'match : (expression, 'i, 'o) View.t -> (value_binding, 'i, 'o) View.t

val pvb_pat'match : (pattern, 'i, 'o) View.t -> (value_binding, 'i, 'o) View.t

val pstr_extension'const : ((attributes * extension), 'i, 'o) View.t -> (structure_item_desc, 'i, 'o) View.t

val strextension'const : ((attributes * extension), 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_attribute'const : (attribute, 'i, 'o) View.t -> (structure_item_desc, 'i, 'o) View.t

val strattribute'const : (attribute, 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_include'const : (include_declaration, 'i, 'o) View.t -> (structure_item_desc, 'i, 'o) View.t

val strinclude'const : (include_declaration, 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_class_type'const : (class_type_declaration list, 'i, 'o) View.t -> (structure_item_desc, 'i, 'o) View.t

val strclass_type'const : (class_type_declaration list, 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_class'const : (class_declaration list, 'i, 'o) View.t -> (structure_item_desc, 'i, 'o) View.t

val strclass'const : (class_declaration list, 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_open'const : (open_description, 'i, 'o) View.t -> (structure_item_desc, 'i, 'o) View.t

val stropen'const : (open_description, 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_modtype'const : (module_type_declaration, 'i, 'o) View.t -> (structure_item_desc, 'i, 'o) View.t

val strmodtype'const : (module_type_declaration, 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_recmodule'const : (module_binding list, 'i, 'o) View.t -> (structure_item_desc, 'i, 'o) View.t

val strrecmodule'const : (module_binding list, 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_module'const : (module_binding, 'i, 'o) View.t -> (structure_item_desc, 'i, 'o) View.t

val strmodule'const : (module_binding, 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_exception'const : (extension_constructor, 'i, 'o) View.t -> (structure_item_desc, 'i, 'o) View.t

val strexception'const : (extension_constructor, 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_typext'const : (type_extension, 'i, 'o) View.t -> (structure_item_desc, 'i, 'o) View.t

val strtypext'const : (type_extension, 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_type'const : ((type_declaration list * rec_flag), 'i, 'o) View.t -> (structure_item_desc, 'i, 'o) View.t

val strtype'const : ((type_declaration list * rec_flag), 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_primitive'const : (value_description, 'i, 'o) View.t -> (structure_item_desc, 'i, 'o) View.t

val strprimitive'const : (value_description, 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_value'const : ((value_binding list * rec_flag), 'i, 'o) View.t -> (structure_item_desc, 'i, 'o) View.t

val strvalue'const : ((value_binding list * rec_flag), 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_eval'const : ((attributes * expression), 'i, 'o) View.t -> (structure_item_desc, 'i, 'o) View.t

val streval'const : ((attributes * expression), 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t

val pstr_desc'match : (structure_item_desc, 'i, 'o) View.t -> (structure_item, 'i, 'o) View.t
val structure'const: (structure_item list, 'i, 'o) View.t -> (structure, 'i, 'o) View.t

val pmod_extension'const : (extension, 'i, 'o) View.t -> (module_expr_desc, 'i, 'o) View.t

val meextension'const : (extension, 'i, 'o) View.t -> (module_expr, 'i, 'o) View.t

val pmod_unpack'const : (expression, 'i, 'o) View.t -> (module_expr_desc, 'i, 'o) View.t

val meunpack'const : (expression, 'i, 'o) View.t -> (module_expr, 'i, 'o) View.t

val pmod_constraint'const : ((module_type * module_expr), 'i, 'o) View.t -> (module_expr_desc, 'i, 'o) View.t

val meconstraint'const : ((module_type * module_expr), 'i, 'o) View.t -> (module_expr, 'i, 'o) View.t

val pmod_apply'const : ((module_expr * module_expr), 'i, 'o) View.t -> (module_expr_desc, 'i, 'o) View.t

val meapply'const : ((module_expr * module_expr), 'i, 'o) View.t -> (module_expr, 'i, 'o) View.t

val pmod_functor'const : ((module_expr * module_type option * string Astlib.Loc.t), 'i, 'o) View.t -> (module_expr_desc, 'i, 'o) View.t

val mefunctor'const : ((module_expr * module_type option * string Astlib.Loc.t), 'i, 'o) View.t -> (module_expr, 'i, 'o) View.t

val pmod_structure'const : (structure, 'i, 'o) View.t -> (module_expr_desc, 'i, 'o) View.t

val mestructure'const : (structure, 'i, 'o) View.t -> (module_expr, 'i, 'o) View.t

val pmod_ident'const : (longident_loc, 'i, 'o) View.t -> (module_expr_desc, 'i, 'o) View.t

val meident'const : (longident_loc, 'i, 'o) View.t -> (module_expr, 'i, 'o) View.t

val pmod_attributes'match : (attributes, 'i, 'o) View.t -> (module_expr, 'i, 'o) View.t

val pmod_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (module_expr, 'i, 'o) View.t

val pmod_desc'match : (module_expr_desc, 'i, 'o) View.t -> (module_expr, 'i, 'o) View.t

val pwith_modsubst'const : ((longident_loc * longident_loc), 'i, 'o) View.t -> (with_constraint, 'i, 'o) View.t

val pwith_typesubst'const : ((type_declaration * longident_loc), 'i, 'o) View.t -> (with_constraint, 'i, 'o) View.t

val pwith_module'const : ((longident_loc * longident_loc), 'i, 'o) View.t -> (with_constraint, 'i, 'o) View.t

val pwith_type'const : ((type_declaration * longident_loc), 'i, 'o) View.t -> (with_constraint, 'i, 'o) View.t
val include_declaration'const: (module_expr include_infos, 'i, 'o) View.t -> (include_declaration, 'i, 'o) View.t
val include_description'const: (module_type include_infos, 'i, 'o) View.t -> (include_description, 'i, 'o) View.t

val pincl_attributes'match : (attributes, 'i, 'o) View.t -> ('a node include_infos, 'i, 'o) View.t

val pincl_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> ('a node include_infos, 'i, 'o) View.t

val pincl_mod'match : ('a node, 'i, 'o) View.t -> ('a node include_infos, 'i, 'o) View.t

val popen_attributes'match : (attributes, 'i, 'o) View.t -> (open_description, 'i, 'o) View.t

val popen_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (open_description, 'i, 'o) View.t

val popen_override'match : (override_flag, 'i, 'o) View.t -> (open_description, 'i, 'o) View.t

val popen_lid'match : (longident_loc, 'i, 'o) View.t -> (open_description, 'i, 'o) View.t

val pmtd_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (module_type_declaration, 'i, 'o) View.t

val pmtd_attributes'match : (attributes, 'i, 'o) View.t -> (module_type_declaration, 'i, 'o) View.t

val pmtd_type'match : (module_type option, 'i, 'o) View.t -> (module_type_declaration, 'i, 'o) View.t

val pmtd_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (module_type_declaration, 'i, 'o) View.t

val pmd_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (module_declaration, 'i, 'o) View.t

val pmd_attributes'match : (attributes, 'i, 'o) View.t -> (module_declaration, 'i, 'o) View.t

val pmd_type'match : (module_type, 'i, 'o) View.t -> (module_declaration, 'i, 'o) View.t

val pmd_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (module_declaration, 'i, 'o) View.t

val psig_extension'const : ((attributes * extension), 'i, 'o) View.t -> (signature_item_desc, 'i, 'o) View.t

val sigextension'const : ((attributes * extension), 'i, 'o) View.t -> (signature_item, 'i, 'o) View.t

val psig_attribute'const : (attribute, 'i, 'o) View.t -> (signature_item_desc, 'i, 'o) View.t

val sigattribute'const : (attribute, 'i, 'o) View.t -> (signature_item, 'i, 'o) View.t

val psig_class_type'const : (class_type_declaration list, 'i, 'o) View.t -> (signature_item_desc, 'i, 'o) View.t

val sigclass_type'const : (class_type_declaration list, 'i, 'o) View.t -> (signature_item, 'i, 'o) View.t

val psig_class'const : (class_description list, 'i, 'o) View.t -> (signature_item_desc, 'i, 'o) View.t

val sigclass'const : (class_description list, 'i, 'o) View.t -> (signature_item, 'i, 'o) View.t

val psig_include'const : (include_description, 'i, 'o) View.t -> (signature_item_desc, 'i, 'o) View.t

val siginclude'const : (include_description, 'i, 'o) View.t -> (signature_item, 'i, 'o) View.t

val psig_open'const : (open_description, 'i, 'o) View.t -> (signature_item_desc, 'i, 'o) View.t

val sigopen'const : (open_description, 'i, 'o) View.t -> (signature_item, 'i, 'o) View.t

val psig_modtype'const : (module_type_declaration, 'i, 'o) View.t -> (signature_item_desc, 'i, 'o) View.t

val sigmodtype'const : (module_type_declaration, 'i, 'o) View.t -> (signature_item, 'i, 'o) View.t

val psig_recmodule'const : (module_declaration list, 'i, 'o) View.t -> (signature_item_desc, 'i, 'o) View.t

val sigrecmodule'const : (module_declaration list, 'i, 'o) View.t -> (signature_item, 'i, 'o) View.t

val psig_module'const : (module_declaration, 'i, 'o) View.t -> (signature_item_desc, 'i, 'o) View.t

val sigmodule'const : (module_declaration, 'i, 'o) View.t -> (signature_item, 'i, 'o) View.t

val psig_exception'const : (extension_constructor, 'i, 'o) View.t -> (signature_item_desc, 'i, 'o) View.t

val sigexception'const : (extension_constructor, 'i, 'o) View.t -> (signature_item, 'i, 'o) View.t

val psig_typext'const : (type_extension, 'i, 'o) View.t -> (signature_item_desc, 'i, 'o) View.t

val sigtypext'const : (type_extension, 'i, 'o) View.t -> (signature_item, 'i, 'o) View.t

val psig_type'const : ((type_declaration list * rec_flag), 'i, 'o) View.t -> (signature_item_desc, 'i, 'o) View.t

val sigtype'const : ((type_declaration list * rec_flag), 'i, 'o) View.t -> (signature_item, 'i, 'o) View.t

val psig_value'const : (value_description, 'i, 'o) View.t -> (signature_item_desc, 'i, 'o) View.t

val sigvalue'const : (value_description, 'i, 'o) View.t -> (signature_item, 'i, 'o) View.t

val psig_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (signature_item, 'i, 'o) View.t

val psig_desc'match : (signature_item_desc, 'i, 'o) View.t -> (signature_item, 'i, 'o) View.t
val signature'const: (signature_item list, 'i, 'o) View.t -> (signature, 'i, 'o) View.t

val pmty_alias'const : (longident_loc, 'i, 'o) View.t -> (module_type_desc, 'i, 'o) View.t

val mtalias'const : (longident_loc, 'i, 'o) View.t -> (module_type, 'i, 'o) View.t

val pmty_extension'const : (extension, 'i, 'o) View.t -> (module_type_desc, 'i, 'o) View.t

val mtextension'const : (extension, 'i, 'o) View.t -> (module_type, 'i, 'o) View.t

val pmty_typeof'const : (module_expr, 'i, 'o) View.t -> (module_type_desc, 'i, 'o) View.t

val mttypeof'const : (module_expr, 'i, 'o) View.t -> (module_type, 'i, 'o) View.t

val pmty_with'const : ((with_constraint list * module_type), 'i, 'o) View.t -> (module_type_desc, 'i, 'o) View.t

val mtwith'const : ((with_constraint list * module_type), 'i, 'o) View.t -> (module_type, 'i, 'o) View.t

val pmty_functor'const : ((module_type * module_type option * string Astlib.Loc.t), 'i, 'o) View.t -> (module_type_desc, 'i, 'o) View.t

val mtfunctor'const : ((module_type * module_type option * string Astlib.Loc.t), 'i, 'o) View.t -> (module_type, 'i, 'o) View.t

val pmty_signature'const : (signature, 'i, 'o) View.t -> (module_type_desc, 'i, 'o) View.t

val mtsignature'const : (signature, 'i, 'o) View.t -> (module_type, 'i, 'o) View.t

val pmty_ident'const : (longident_loc, 'i, 'o) View.t -> (module_type_desc, 'i, 'o) View.t

val mtident'const : (longident_loc, 'i, 'o) View.t -> (module_type, 'i, 'o) View.t

val pmty_attributes'match : (attributes, 'i, 'o) View.t -> (module_type, 'i, 'o) View.t

val pmty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (module_type, 'i, 'o) View.t

val pmty_desc'match : (module_type_desc, 'i, 'o) View.t -> (module_type, 'i, 'o) View.t
val class_declaration'const: (class_expr class_infos, 'i, 'o) View.t -> (class_declaration, 'i, 'o) View.t

val cfk_concrete'const : ((expression * override_flag), 'i, 'o) View.t -> (class_field_kind, 'i, 'o) View.t

val cfk_virtual'const : (core_type, 'i, 'o) View.t -> (class_field_kind, 'i, 'o) View.t

val pcf_extension'const : (extension, 'i, 'o) View.t -> (class_field_desc, 'i, 'o) View.t

val cfextension'const : (extension, 'i, 'o) View.t -> (class_field, 'i, 'o) View.t

val pcf_attribute'const : (attribute, 'i, 'o) View.t -> (class_field_desc, 'i, 'o) View.t

val cfattribute'const : (attribute, 'i, 'o) View.t -> (class_field, 'i, 'o) View.t

val pcf_initializer'const : (expression, 'i, 'o) View.t -> (class_field_desc, 'i, 'o) View.t

val cfinitializer'const : (expression, 'i, 'o) View.t -> (class_field, 'i, 'o) View.t

val pcf_constraint'const : ((core_type * core_type), 'i, 'o) View.t -> (class_field_desc, 'i, 'o) View.t

val cfconstraint'const : ((core_type * core_type), 'i, 'o) View.t -> (class_field, 'i, 'o) View.t

val pcf_method'const : ((class_field_kind * private_flag * string Astlib.Loc.t), 'i, 'o) View.t -> (class_field_desc, 'i, 'o) View.t

val cfmethod'const : ((class_field_kind * private_flag * string Astlib.Loc.t), 'i, 'o) View.t -> (class_field, 'i, 'o) View.t

val pcf_val'const : ((class_field_kind * mutable_flag * string Astlib.Loc.t), 'i, 'o) View.t -> (class_field_desc, 'i, 'o) View.t

val cfval'const : ((class_field_kind * mutable_flag * string Astlib.Loc.t), 'i, 'o) View.t -> (class_field, 'i, 'o) View.t

val pcf_inherit'const : ((string Astlib.Loc.t option * class_expr * override_flag), 'i, 'o) View.t -> (class_field_desc, 'i, 'o) View.t

val cfinherit'const : ((string Astlib.Loc.t option * class_expr * override_flag), 'i, 'o) View.t -> (class_field, 'i, 'o) View.t

val pcf_attributes'match : (attributes, 'i, 'o) View.t -> (class_field, 'i, 'o) View.t

val pcf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (class_field, 'i, 'o) View.t

val pcf_desc'match : (class_field_desc, 'i, 'o) View.t -> (class_field, 'i, 'o) View.t

val pcstr_fields'match : (class_field list, 'i, 'o) View.t -> (class_structure, 'i, 'o) View.t

val pcstr_self'match : (pattern, 'i, 'o) View.t -> (class_structure, 'i, 'o) View.t

val pcl_open'const : ((class_expr * longident_loc * override_flag), 'i, 'o) View.t -> (class_expr_desc, 'i, 'o) View.t

val ceopen'const : ((class_expr * longident_loc * override_flag), 'i, 'o) View.t -> (class_expr, 'i, 'o) View.t

val pcl_extension'const : (extension, 'i, 'o) View.t -> (class_expr_desc, 'i, 'o) View.t

val ceextension'const : (extension, 'i, 'o) View.t -> (class_expr, 'i, 'o) View.t

val pcl_constraint'const : ((class_type * class_expr), 'i, 'o) View.t -> (class_expr_desc, 'i, 'o) View.t

val ceconstraint'const : ((class_type * class_expr), 'i, 'o) View.t -> (class_expr, 'i, 'o) View.t

val pcl_let'const : ((class_expr * value_binding list * rec_flag), 'i, 'o) View.t -> (class_expr_desc, 'i, 'o) View.t

val celet'const : ((class_expr * value_binding list * rec_flag), 'i, 'o) View.t -> (class_expr, 'i, 'o) View.t

val pcl_apply'const : (((expression * arg_label) list * class_expr), 'i, 'o) View.t -> (class_expr_desc, 'i, 'o) View.t

val ceapply'const : (((expression * arg_label) list * class_expr), 'i, 'o) View.t -> (class_expr, 'i, 'o) View.t

val pcl_fun'const : ((class_expr * pattern * expression option * arg_label), 'i, 'o) View.t -> (class_expr_desc, 'i, 'o) View.t

val cefun'const : ((class_expr * pattern * expression option * arg_label), 'i, 'o) View.t -> (class_expr, 'i, 'o) View.t

val pcl_structure'const : (class_structure, 'i, 'o) View.t -> (class_expr_desc, 'i, 'o) View.t

val cestructure'const : (class_structure, 'i, 'o) View.t -> (class_expr, 'i, 'o) View.t

val pcl_constr'const : ((core_type list * longident_loc), 'i, 'o) View.t -> (class_expr_desc, 'i, 'o) View.t

val ceconstr'const : ((core_type list * longident_loc), 'i, 'o) View.t -> (class_expr, 'i, 'o) View.t

val pcl_attributes'match : (attributes, 'i, 'o) View.t -> (class_expr, 'i, 'o) View.t

val pcl_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (class_expr, 'i, 'o) View.t

val pcl_desc'match : (class_expr_desc, 'i, 'o) View.t -> (class_expr, 'i, 'o) View.t
val class_type_declaration'const: (class_type class_infos, 'i, 'o) View.t -> (class_type_declaration, 'i, 'o) View.t
val class_description'const: (class_type class_infos, 'i, 'o) View.t -> (class_description, 'i, 'o) View.t

val pci_attributes'match : (attributes, 'i, 'o) View.t -> ('a node class_infos, 'i, 'o) View.t

val pci_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> ('a node class_infos, 'i, 'o) View.t

val pci_expr'match : ('a node, 'i, 'o) View.t -> ('a node class_infos, 'i, 'o) View.t

val pci_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> ('a node class_infos, 'i, 'o) View.t

val pci_params'match : ((variance * core_type) list, 'i, 'o) View.t -> ('a node class_infos, 'i, 'o) View.t

val pci_virt'match : (virtual_flag, 'i, 'o) View.t -> ('a node class_infos, 'i, 'o) View.t

val pctf_extension'const : (extension, 'i, 'o) View.t -> (class_type_field_desc, 'i, 'o) View.t

val ctfextension'const : (extension, 'i, 'o) View.t -> (class_type_field, 'i, 'o) View.t

val pctf_attribute'const : (attribute, 'i, 'o) View.t -> (class_type_field_desc, 'i, 'o) View.t

val ctfattribute'const : (attribute, 'i, 'o) View.t -> (class_type_field, 'i, 'o) View.t

val pctf_constraint'const : ((core_type * core_type), 'i, 'o) View.t -> (class_type_field_desc, 'i, 'o) View.t

val ctfconstraint'const : ((core_type * core_type), 'i, 'o) View.t -> (class_type_field, 'i, 'o) View.t

val pctf_method'const : ((core_type * virtual_flag * private_flag * string Astlib.Loc.t), 'i, 'o) View.t -> (class_type_field_desc, 'i, 'o) View.t

val ctfmethod'const : ((core_type * virtual_flag * private_flag * string Astlib.Loc.t), 'i, 'o) View.t -> (class_type_field, 'i, 'o) View.t

val pctf_val'const : ((core_type * virtual_flag * mutable_flag * string Astlib.Loc.t), 'i, 'o) View.t -> (class_type_field_desc, 'i, 'o) View.t

val ctfval'const : ((core_type * virtual_flag * mutable_flag * string Astlib.Loc.t), 'i, 'o) View.t -> (class_type_field, 'i, 'o) View.t

val pctf_inherit'const : (class_type, 'i, 'o) View.t -> (class_type_field_desc, 'i, 'o) View.t

val ctfinherit'const : (class_type, 'i, 'o) View.t -> (class_type_field, 'i, 'o) View.t

val pctf_attributes'match : (attributes, 'i, 'o) View.t -> (class_type_field, 'i, 'o) View.t

val pctf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (class_type_field, 'i, 'o) View.t

val pctf_desc'match : (class_type_field_desc, 'i, 'o) View.t -> (class_type_field, 'i, 'o) View.t

val pcsig_fields'match : (class_type_field list, 'i, 'o) View.t -> (class_signature, 'i, 'o) View.t

val pcsig_self'match : (core_type, 'i, 'o) View.t -> (class_signature, 'i, 'o) View.t

val pcty_open'const : ((class_type * longident_loc * override_flag), 'i, 'o) View.t -> (class_type_desc, 'i, 'o) View.t

val ctopen'const : ((class_type * longident_loc * override_flag), 'i, 'o) View.t -> (class_type, 'i, 'o) View.t

val pcty_extension'const : (extension, 'i, 'o) View.t -> (class_type_desc, 'i, 'o) View.t

val ctextension'const : (extension, 'i, 'o) View.t -> (class_type, 'i, 'o) View.t

val pcty_arrow'const : ((class_type * core_type * arg_label), 'i, 'o) View.t -> (class_type_desc, 'i, 'o) View.t

val ctarrow'const : ((class_type * core_type * arg_label), 'i, 'o) View.t -> (class_type, 'i, 'o) View.t

val pcty_signature'const : (class_signature, 'i, 'o) View.t -> (class_type_desc, 'i, 'o) View.t

val ctsignature'const : (class_signature, 'i, 'o) View.t -> (class_type, 'i, 'o) View.t

val pcty_constr'const : ((core_type list * longident_loc), 'i, 'o) View.t -> (class_type_desc, 'i, 'o) View.t

val ctconstr'const : ((core_type list * longident_loc), 'i, 'o) View.t -> (class_type, 'i, 'o) View.t

val pcty_attributes'match : (attributes, 'i, 'o) View.t -> (class_type, 'i, 'o) View.t

val pcty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (class_type, 'i, 'o) View.t

val pcty_desc'match : (class_type_desc, 'i, 'o) View.t -> (class_type, 'i, 'o) View.t

val pext_rebind'const : (longident_loc, 'i, 'o) View.t -> (extension_constructor_kind, 'i, 'o) View.t

val pext_decl'const : ((core_type option * constructor_arguments), 'i, 'o) View.t -> (extension_constructor_kind, 'i, 'o) View.t

val pext_attributes'match : (attributes, 'i, 'o) View.t -> (extension_constructor, 'i, 'o) View.t

val pext_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (extension_constructor, 'i, 'o) View.t

val pext_kind'match : (extension_constructor_kind, 'i, 'o) View.t -> (extension_constructor, 'i, 'o) View.t

val pext_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (extension_constructor, 'i, 'o) View.t

val ptyext_attributes'match : (attributes, 'i, 'o) View.t -> (type_extension, 'i, 'o) View.t

val ptyext_private'match : (private_flag, 'i, 'o) View.t -> (type_extension, 'i, 'o) View.t

val ptyext_constructors'match : (extension_constructor list, 'i, 'o) View.t -> (type_extension, 'i, 'o) View.t

val ptyext_params'match : ((variance * core_type) list, 'i, 'o) View.t -> (type_extension, 'i, 'o) View.t

val ptyext_path'match : (longident_loc, 'i, 'o) View.t -> (type_extension, 'i, 'o) View.t

val pcstr_record'const : (label_declaration list, 'i, 'o) View.t -> (constructor_arguments, 'i, 'o) View.t

val pcstr_tuple'const : (core_type list, 'i, 'o) View.t -> (constructor_arguments, 'i, 'o) View.t

val pcd_attributes'match : (attributes, 'i, 'o) View.t -> (constructor_declaration, 'i, 'o) View.t

val pcd_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (constructor_declaration, 'i, 'o) View.t

val pcd_res'match : (core_type option, 'i, 'o) View.t -> (constructor_declaration, 'i, 'o) View.t

val pcd_args'match : (constructor_arguments, 'i, 'o) View.t -> (constructor_declaration, 'i, 'o) View.t

val pcd_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (constructor_declaration, 'i, 'o) View.t

val pld_attributes'match : (attributes, 'i, 'o) View.t -> (label_declaration, 'i, 'o) View.t

val pld_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (label_declaration, 'i, 'o) View.t

val pld_type'match : (core_type, 'i, 'o) View.t -> (label_declaration, 'i, 'o) View.t

val pld_mutable'match : (mutable_flag, 'i, 'o) View.t -> (label_declaration, 'i, 'o) View.t

val pld_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (label_declaration, 'i, 'o) View.t

val ptype_open'const : (type_kind, 'a, 'a) View.t

val ptype_record'const : (label_declaration list, 'i, 'o) View.t -> (type_kind, 'i, 'o) View.t

val ptype_variant'const : (constructor_declaration list, 'i, 'o) View.t -> (type_kind, 'i, 'o) View.t

val ptype_abstract'const : (type_kind, 'a, 'a) View.t

val ptype_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (type_declaration, 'i, 'o) View.t

val ptype_attributes'match : (attributes, 'i, 'o) View.t -> (type_declaration, 'i, 'o) View.t

val ptype_manifest'match : (core_type option, 'i, 'o) View.t -> (type_declaration, 'i, 'o) View.t

val ptype_private'match : (private_flag, 'i, 'o) View.t -> (type_declaration, 'i, 'o) View.t

val ptype_kind'match : (type_kind, 'i, 'o) View.t -> (type_declaration, 'i, 'o) View.t

val ptype_cstrs'match : ((Astlib.Location.t * core_type * core_type) list, 'i, 'o) View.t -> (type_declaration, 'i, 'o) View.t

val ptype_params'match : ((variance * core_type) list, 'i, 'o) View.t -> (type_declaration, 'i, 'o) View.t

val ptype_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (type_declaration, 'i, 'o) View.t

val pval_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (value_description, 'i, 'o) View.t

val pval_attributes'match : (attributes, 'i, 'o) View.t -> (value_description, 'i, 'o) View.t

val pval_prim'match : (string list, 'i, 'o) View.t -> (value_description, 'i, 'o) View.t

val pval_type'match : (core_type, 'i, 'o) View.t -> (value_description, 'i, 'o) View.t

val pval_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (value_description, 'i, 'o) View.t

val pc_rhs'match : (expression, 'i, 'o) View.t -> (case, 'i, 'o) View.t

val pc_guard'match : (expression option, 'i, 'o) View.t -> (case, 'i, 'o) View.t

val pc_lhs'match : (pattern, 'i, 'o) View.t -> (case, 'i, 'o) View.t

val pexp_unreachable'const : (expression_desc, 'a, 'a) View.t

val eunreachable'const : (expression, 'a, 'a) View.t

val pexp_extension'const : (extension, 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val eextension'const : (extension, 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_open'const : ((expression * longident_loc * override_flag), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val eopen'const : ((expression * longident_loc * override_flag), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_pack'const : (module_expr, 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val epack'const : (module_expr, 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_newtype'const : ((expression * string Astlib.Loc.t), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val enewtype'const : ((expression * string Astlib.Loc.t), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_object'const : (class_structure, 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val eobject'const : (class_structure, 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_poly'const : ((core_type option * expression), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val epoly'const : ((core_type option * expression), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_lazy'const : (expression, 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val elazy'const : (expression, 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_assert'const : (expression, 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val eassert'const : (expression, 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_letexception'const : ((expression * extension_constructor), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val eletexception'const : ((expression * extension_constructor), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_letmodule'const : ((expression * module_expr * string Astlib.Loc.t), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val eletmodule'const : ((expression * module_expr * string Astlib.Loc.t), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_override'const : ((expression * string Astlib.Loc.t) list, 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val eoverride'const : ((expression * string Astlib.Loc.t) list, 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_setinstvar'const : ((expression * string Astlib.Loc.t), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val esetinstvar'const : ((expression * string Astlib.Loc.t), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_new'const : (longident_loc, 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val enew'const : (longident_loc, 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_send'const : ((string Astlib.Loc.t * expression), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val esend'const : ((string Astlib.Loc.t * expression), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_coerce'const : ((core_type * core_type option * expression), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val ecoerce'const : ((core_type * core_type option * expression), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_constraint'const : ((core_type * expression), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val econstraint'const : ((core_type * expression), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_for'const : ((expression * direction_flag * expression * expression * pattern), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val efor'const : ((expression * direction_flag * expression * expression * pattern), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_while'const : ((expression * expression), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val ewhile'const : ((expression * expression), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_sequence'const : ((expression * expression), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val esequence'const : ((expression * expression), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_ifthenelse'const : ((expression option * expression * expression), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val eifthenelse'const : ((expression option * expression * expression), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_array'const : (expression list, 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val earray'const : (expression list, 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_setfield'const : ((expression * longident_loc * expression), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val esetfield'const : ((expression * longident_loc * expression), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_field'const : ((longident_loc * expression), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val efield'const : ((longident_loc * expression), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_record'const : ((expression option * (expression * longident_loc) list), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val erecord'const : ((expression option * (expression * longident_loc) list), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_variant'const : ((expression option * string), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val evariant'const : ((expression option * string), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_construct'const : ((expression option * longident_loc), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val econstruct'const : ((expression option * longident_loc), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_tuple'const : (expression list, 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val etuple'const : (expression list, 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_try'const : ((case list * expression), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val etry'const : ((case list * expression), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_match'const : ((case list * expression), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val ematch'const : ((case list * expression), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_apply'const : (((expression * arg_label) list * expression), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val eapply'const : (((expression * arg_label) list * expression), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_fun'const : ((expression * pattern * expression option * arg_label), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val efun'const : ((expression * pattern * expression option * arg_label), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_function'const : (case list, 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val efunction'const : (case list, 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_let'const : ((expression * value_binding list * rec_flag), 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val elet'const : ((expression * value_binding list * rec_flag), 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_constant'const : (constant, 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val econstant'const : (constant, 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_ident'const : (longident_loc, 'i, 'o) View.t -> (expression_desc, 'i, 'o) View.t

val eident'const : (longident_loc, 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_attributes'match : (attributes, 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val pexp_desc'match : (expression_desc, 'i, 'o) View.t -> (expression, 'i, 'o) View.t

val ppat_open'const : ((pattern * longident_loc), 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val popen'const : ((pattern * longident_loc), 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_extension'const : (extension, 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val pextension'const : (extension, 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_exception'const : (pattern, 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val pexception'const : (pattern, 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_unpack'const : (string Astlib.Loc.t, 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val punpack'const : (string Astlib.Loc.t, 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_lazy'const : (pattern, 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val plazy'const : (pattern, 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_type'const : (longident_loc, 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val ptype'const : (longident_loc, 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_constraint'const : ((core_type * pattern), 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val pconstraint'const : ((core_type * pattern), 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_or'const : ((pattern * pattern), 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val por'const : ((pattern * pattern), 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_array'const : (pattern list, 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val parray'const : (pattern list, 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_record'const : ((closed_flag * (pattern * longident_loc) list), 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val precord'const : ((closed_flag * (pattern * longident_loc) list), 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_variant'const : ((pattern option * string), 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val pvariant'const : ((pattern option * string), 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_construct'const : ((pattern option * longident_loc), 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val pconstruct'const : ((pattern option * longident_loc), 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_tuple'const : (pattern list, 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val ptuple'const : (pattern list, 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_interval'const : ((constant * constant), 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val pinterval'const : ((constant * constant), 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_constant'const : (constant, 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val pconstant'const : (constant, 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_alias'const : ((string Astlib.Loc.t * pattern), 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val palias'const : ((string Astlib.Loc.t * pattern), 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_var'const : (string Astlib.Loc.t, 'i, 'o) View.t -> (pattern_desc, 'i, 'o) View.t

val pvar'const : (string Astlib.Loc.t, 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_any'const : (pattern_desc, 'a, 'a) View.t

val pany'const : (pattern, 'a, 'a) View.t

val ppat_attributes'match : (attributes, 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val ppat_desc'match : (pattern_desc, 'i, 'o) View.t -> (pattern, 'i, 'o) View.t

val oinherit'const : (core_type, 'i, 'o) View.t -> (object_field, 'i, 'o) View.t

val otag'const : ((core_type * attributes * string Astlib.Loc.t), 'i, 'o) View.t -> (object_field, 'i, 'o) View.t

val rinherit'const : (core_type, 'i, 'o) View.t -> (row_field, 'i, 'o) View.t

val rtag'const : ((core_type list * bool * attributes * string Astlib.Loc.t), 'i, 'o) View.t -> (row_field, 'i, 'o) View.t
val package_type'const: (((core_type * longident_loc) list * longident_loc), 'i, 'o) View.t -> (package_type, 'i, 'o) View.t

val ptyp_extension'const : (extension, 'i, 'o) View.t -> (core_type_desc, 'i, 'o) View.t

val textension'const : (extension, 'i, 'o) View.t -> (core_type, 'i, 'o) View.t

val ptyp_package'const : (package_type, 'i, 'o) View.t -> (core_type_desc, 'i, 'o) View.t

val tpackage'const : (package_type, 'i, 'o) View.t -> (core_type, 'i, 'o) View.t

val ptyp_poly'const : ((core_type * string Astlib.Loc.t list), 'i, 'o) View.t -> (core_type_desc, 'i, 'o) View.t

val tpoly'const : ((core_type * string Astlib.Loc.t list), 'i, 'o) View.t -> (core_type, 'i, 'o) View.t

val ptyp_variant'const : ((string list option * closed_flag * row_field list), 'i, 'o) View.t -> (core_type_desc, 'i, 'o) View.t

val tvariant'const : ((string list option * closed_flag * row_field list), 'i, 'o) View.t -> (core_type, 'i, 'o) View.t

val ptyp_alias'const : ((string * core_type), 'i, 'o) View.t -> (core_type_desc, 'i, 'o) View.t

val talias'const : ((string * core_type), 'i, 'o) View.t -> (core_type, 'i, 'o) View.t

val ptyp_class'const : ((core_type list * longident_loc), 'i, 'o) View.t -> (core_type_desc, 'i, 'o) View.t

val tclass'const : ((core_type list * longident_loc), 'i, 'o) View.t -> (core_type, 'i, 'o) View.t

val ptyp_object'const : ((closed_flag * object_field list), 'i, 'o) View.t -> (core_type_desc, 'i, 'o) View.t

val tobject'const : ((closed_flag * object_field list), 'i, 'o) View.t -> (core_type, 'i, 'o) View.t

val ptyp_constr'const : ((core_type list * longident_loc), 'i, 'o) View.t -> (core_type_desc, 'i, 'o) View.t

val tconstr'const : ((core_type list * longident_loc), 'i, 'o) View.t -> (core_type, 'i, 'o) View.t

val ptyp_tuple'const : (core_type list, 'i, 'o) View.t -> (core_type_desc, 'i, 'o) View.t

val ttuple'const : (core_type list, 'i, 'o) View.t -> (core_type, 'i, 'o) View.t

val ptyp_arrow'const : ((core_type * core_type * arg_label), 'i, 'o) View.t -> (core_type_desc, 'i, 'o) View.t

val tarrow'const : ((core_type * core_type * arg_label), 'i, 'o) View.t -> (core_type, 'i, 'o) View.t

val ptyp_var'const : (string, 'i, 'o) View.t -> (core_type_desc, 'i, 'o) View.t

val tvar'const : (string, 'i, 'o) View.t -> (core_type, 'i, 'o) View.t

val ptyp_any'const : (core_type_desc, 'a, 'a) View.t

val tany'const : (core_type, 'a, 'a) View.t

val ptyp_attributes'match : (attributes, 'i, 'o) View.t -> (core_type, 'i, 'o) View.t

val ptyp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (core_type, 'i, 'o) View.t

val ptyp_desc'match : (core_type_desc, 'i, 'o) View.t -> (core_type, 'i, 'o) View.t

val pPat'const : ((expression option * pattern), 'i, 'o) View.t -> (payload, 'i, 'o) View.t

val pTyp'const : (core_type, 'i, 'o) View.t -> (payload, 'i, 'o) View.t

val pSig'const : (signature, 'i, 'o) View.t -> (payload, 'i, 'o) View.t

val pStr'const : (structure, 'i, 'o) View.t -> (payload, 'i, 'o) View.t
val attributes'const: (attribute list, 'i, 'o) View.t -> (attributes, 'i, 'o) View.t
val extension'const: ((payload * string Astlib.Loc.t), 'i, 'o) View.t -> (extension, 'i, 'o) View.t
val attribute'const: ((payload * string Astlib.Loc.t), 'i, 'o) View.t -> (attribute, 'i, 'o) View.t

val pconst_float'const : ((char option * string), 'i, 'o) View.t -> (constant, 'i, 'o) View.t

val pconst_string'const : ((string option * string), 'i, 'o) View.t -> (constant, 'i, 'o) View.t

val pconst_char'const : (char, 'i, 'o) View.t -> (constant, 'i, 'o) View.t

val pconst_integer'const : ((char option * string), 'i, 'o) View.t -> (constant, 'i, 'o) View.t

val invariant'const : (variance, 'a, 'a) View.t

val contravariant'const : (variance, 'a, 'a) View.t

val covariant'const : (variance, 'a, 'a) View.t

val optional'const : (string, 'i, 'o) View.t -> (arg_label, 'i, 'o) View.t

val labelled'const : (string, 'i, 'o) View.t -> (arg_label, 'i, 'o) View.t

val nolabel'const : (arg_label, 'a, 'a) View.t

val open'const : (closed_flag, 'a, 'a) View.t

val closed'const : (closed_flag, 'a, 'a) View.t

val fresh'const : (override_flag, 'a, 'a) View.t

val override'const : (override_flag, 'a, 'a) View.t

val concrete'const : (virtual_flag, 'a, 'a) View.t

val virtual'const : (virtual_flag, 'a, 'a) View.t

val mutable'const : (mutable_flag, 'a, 'a) View.t

val immutable'const : (mutable_flag, 'a, 'a) View.t

val public'const : (private_flag, 'a, 'a) View.t

val private'const : (private_flag, 'a, 'a) View.t

val downto'const : (direction_flag, 'a, 'a) View.t

val upto'const : (direction_flag, 'a, 'a) View.t

val recursive'const : (rec_flag, 'a, 'a) View.t

val nonrecursive'const : (rec_flag, 'a, 'a) View.t
val longident_loc'const: (longident Astlib.Loc.t, 'i, 'o) View.t -> (longident_loc, 'i, 'o) View.t

val lapply'const : ((longident * longident), 'i, 'o) View.t -> (longident, 'i, 'o) View.t

val ldot'const : ((string * longident), 'i, 'o) View.t -> (longident, 'i, 'o) View.t

val lident'const : (string, 'i, 'o) View.t -> (longident, 'i, 'o) View.t
(*$*)
