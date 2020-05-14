open! Import

(*$ Ppxlib_cinaps_helpers.generate_ast_pattern_intf () *)
val lident :
  (string, 't0, 't1) Ast_pattern0.t
  -> (longident, 't0, 't1) Ast_pattern0.t

val ldot :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (string, 't1, 't2) Ast_pattern0.t
  -> (longident, 't0, 't2) Ast_pattern0.t

val lapply :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (longident, 't1, 't2) Ast_pattern0.t
  -> (longident, 't0, 't2) Ast_pattern0.t

val labelled :
  (string, 't0, 't1) Ast_pattern0.t
  -> (arg_label, 't0, 't1) Ast_pattern0.t

val optional :
  (string, 't0, 't1) Ast_pattern0.t
  -> (arg_label, 't0, 't1) Ast_pattern0.t

val pconst_integer :
  (string, 't0, 't1) Ast_pattern0.t
  -> (char option, 't1, 't2) Ast_pattern0.t
  -> (constant, 't0, 't2) Ast_pattern0.t

val pconst_char :
  (char, 't0, 't1) Ast_pattern0.t
  -> (constant, 't0, 't1) Ast_pattern0.t

val pconst_string :
  (string, 't0, 't1) Ast_pattern0.t
  -> (string option, 't1, 't2) Ast_pattern0.t
  -> (constant, 't0, 't2) Ast_pattern0.t

val pconst_float :
  (string, 't0, 't1) Ast_pattern0.t
  -> (char option, 't1, 't2) Ast_pattern0.t
  -> (constant, 't0, 't2) Ast_pattern0.t

val pstr :
  (structure_item list, 't0, 't1) Ast_pattern0.t
  -> (payload, 't0, 't1) Ast_pattern0.t

val psig :
  (signature_item list, 't0, 't1) Ast_pattern0.t
  -> (payload, 't0, 't1) Ast_pattern0.t

val ptyp :
  (core_type, 't0, 't1) Ast_pattern0.t
  -> (payload, 't0, 't1) Ast_pattern0.t

val ppat :
  (pattern, 't0, 't1) Ast_pattern0.t
  -> (expression option, 't1, 't2) Ast_pattern0.t
  -> (payload, 't0, 't2) Ast_pattern0.t

val ptyp_var :
  (string, 't0, 't1) Ast_pattern0.t
  -> (core_type, 't0, 't1) Ast_pattern0.t

val ptyp_arrow :
  (arg_label, 't0, 't1) Ast_pattern0.t
  -> (core_type, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't2, 't3) Ast_pattern0.t
  -> (core_type, 't0, 't3) Ast_pattern0.t

val ptyp_tuple :
  (core_type list, 't0, 't1) Ast_pattern0.t
  -> (core_type, 't0, 't1) Ast_pattern0.t

val ptyp_constr :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (core_type list, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't0, 't2) Ast_pattern0.t

val ptyp_object :
  (object_field list, 't0, 't1) Ast_pattern0.t
  -> (closed_flag, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't0, 't2) Ast_pattern0.t

val ptyp_class :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (core_type list, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't0, 't2) Ast_pattern0.t

val ptyp_alias :
  (core_type, 't0, 't1) Ast_pattern0.t
  -> (string, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't0, 't2) Ast_pattern0.t

val ptyp_variant :
  (row_field list, 't0, 't1) Ast_pattern0.t
  -> (closed_flag, 't1, 't2) Ast_pattern0.t
  -> (string list option, 't2, 't3) Ast_pattern0.t
  -> (core_type, 't0, 't3) Ast_pattern0.t

val ptyp_poly :
  (string Astlib.Loc.t list, 't0, 't1) Ast_pattern0.t
  -> (core_type, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't0, 't2) Ast_pattern0.t

val ptyp_package :
  ((longident_loc * (longident_loc * core_type) list), 't0, 't1) Ast_pattern0.t
  -> (core_type, 't0, 't1) Ast_pattern0.t

val ptyp_extension :
  ((string Astlib.Loc.t * payload), 't0, 't1) Ast_pattern0.t
  -> (core_type, 't0, 't1) Ast_pattern0.t

val ptyp_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't1, 't2) Ast_pattern0.t

val ptyp_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't1, 't2) Ast_pattern0.t

val rtag :
  (string, 't0, 't1) Ast_pattern0.t
  -> (attribute list, 't1, 't2) Ast_pattern0.t
  -> (bool, 't2, 't3) Ast_pattern0.t
  -> (core_type list, 't3, 't4) Ast_pattern0.t
  -> (row_field, 't0, 't4) Ast_pattern0.t

val rinherit :
  (core_type, 't0, 't1) Ast_pattern0.t
  -> (row_field, 't0, 't1) Ast_pattern0.t

val otag :
  (string, 't0, 't1) Ast_pattern0.t
  -> (attribute list, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't2, 't3) Ast_pattern0.t
  -> (object_field, 't0, 't3) Ast_pattern0.t

val oinherit :
  (core_type, 't0, 't1) Ast_pattern0.t
  -> (object_field, 't0, 't1) Ast_pattern0.t

val ppat_var :
  (string, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_alias :
  (pattern, 't0, 't1) Ast_pattern0.t
  -> (string, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_constant :
  (constant, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_interval :
  (constant, 't0, 't1) Ast_pattern0.t
  -> (constant, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_tuple :
  (pattern list, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_construct :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (pattern option, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_variant :
  (string, 't0, 't1) Ast_pattern0.t
  -> (pattern option, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_record :
  ((longident_loc * pattern) list, 't0, 't1) Ast_pattern0.t
  -> (closed_flag, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_array :
  (pattern list, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_or :
  (pattern, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_constraint :
  (pattern, 't0, 't1) Ast_pattern0.t
  -> (core_type, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_type :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_lazy :
  (pattern, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_unpack :
  (string, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_exception :
  (pattern, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_extension :
  ((string Astlib.Loc.t * payload), 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_open :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't1, 't2) Ast_pattern0.t

val ppat_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't1, 't2) Ast_pattern0.t

val pexp_ident :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_constant :
  (constant, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_let :
  (rec_flag, 't0, 't1) Ast_pattern0.t
  -> (value_binding list, 't1, 't2) Ast_pattern0.t
  -> (expression, 't2, 't3) Ast_pattern0.t
  -> (expression, 't0, 't3) Ast_pattern0.t

val pexp_function :
  (case list, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_fun :
  (arg_label, 't0, 't1) Ast_pattern0.t
  -> (expression option, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't2, 't3) Ast_pattern0.t
  -> (expression, 't3, 't4) Ast_pattern0.t
  -> (expression, 't0, 't4) Ast_pattern0.t

val pexp_apply :
  (expression, 't0, 't1) Ast_pattern0.t
  -> ((arg_label * expression) list, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_match :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (case list, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_try :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (case list, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_tuple :
  (expression list, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_construct :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (expression option, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_variant :
  (string, 't0, 't1) Ast_pattern0.t
  -> (expression option, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_record :
  ((longident_loc * expression) list, 't0, 't1) Ast_pattern0.t
  -> (expression option, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_field :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (longident, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_setfield :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (longident, 't1, 't2) Ast_pattern0.t
  -> (expression, 't2, 't3) Ast_pattern0.t
  -> (expression, 't0, 't3) Ast_pattern0.t

val pexp_array :
  (expression list, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_ifthenelse :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (expression, 't1, 't2) Ast_pattern0.t
  -> (expression option, 't2, 't3) Ast_pattern0.t
  -> (expression, 't0, 't3) Ast_pattern0.t

val pexp_sequence :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (expression, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_while :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (expression, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_for :
  (pattern, 't0, 't1) Ast_pattern0.t
  -> (expression, 't1, 't2) Ast_pattern0.t
  -> (expression, 't2, 't3) Ast_pattern0.t
  -> (direction_flag, 't3, 't4) Ast_pattern0.t
  -> (expression, 't4, 't5) Ast_pattern0.t
  -> (expression, 't0, 't5) Ast_pattern0.t

val pexp_constraint :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (core_type, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_coerce :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (core_type option, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't2, 't3) Ast_pattern0.t
  -> (expression, 't0, 't3) Ast_pattern0.t

val pexp_send :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (string, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_new :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_setinstvar :
  (string, 't0, 't1) Ast_pattern0.t
  -> (expression, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_override :
  ((string Astlib.Loc.t * expression) list, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_letmodule :
  (string, 't0, 't1) Ast_pattern0.t
  -> (module_expr, 't1, 't2) Ast_pattern0.t
  -> (expression, 't2, 't3) Ast_pattern0.t
  -> (expression, 't0, 't3) Ast_pattern0.t

val pexp_letexception :
  (extension_constructor, 't0, 't1) Ast_pattern0.t
  -> (expression, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_assert :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_lazy :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_poly :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (core_type option, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_object :
  (class_structure, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_newtype :
  (string, 't0, 't1) Ast_pattern0.t
  -> (expression, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_pack :
  (module_expr, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_open :
  (override_flag, 't0, 't1) Ast_pattern0.t
  -> (longident, 't1, 't2) Ast_pattern0.t
  -> (expression, 't2, 't3) Ast_pattern0.t
  -> (expression, 't0, 't3) Ast_pattern0.t

val pexp_extension :
  ((string Astlib.Loc.t * payload), 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (expression, 't1, 't2) Ast_pattern0.t

val pexp_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (expression, 't1, 't2) Ast_pattern0.t

val pval_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (value_description, 't1, 't2) Ast_pattern0.t

val pval_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (value_description, 't1, 't2) Ast_pattern0.t

val ptype_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (type_declaration, 't1, 't2) Ast_pattern0.t

val ptype_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (type_declaration, 't1, 't2) Ast_pattern0.t

val ptype_variant :
  (constructor_declaration list, 't0, 't1) Ast_pattern0.t
  -> (type_kind, 't0, 't1) Ast_pattern0.t

val ptype_record :
  (label_declaration list, 't0, 't1) Ast_pattern0.t
  -> (type_kind, 't0, 't1) Ast_pattern0.t

val pld_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (label_declaration, 't1, 't2) Ast_pattern0.t

val pld_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (label_declaration, 't1, 't2) Ast_pattern0.t

val pcd_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (constructor_declaration, 't1, 't2) Ast_pattern0.t

val pcd_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (constructor_declaration, 't1, 't2) Ast_pattern0.t

val pcstr_tuple :
  (core_type list, 't0, 't1) Ast_pattern0.t
  -> (constructor_arguments, 't0, 't1) Ast_pattern0.t

val pcstr_record :
  (label_declaration list, 't0, 't1) Ast_pattern0.t
  -> (constructor_arguments, 't0, 't1) Ast_pattern0.t

val ptyext_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (type_extension, 't1, 't2) Ast_pattern0.t

val pext_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (extension_constructor, 't1, 't2) Ast_pattern0.t

val pext_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (extension_constructor, 't1, 't2) Ast_pattern0.t

val pext_decl :
  (constructor_arguments, 't0, 't1) Ast_pattern0.t
  -> (core_type option, 't1, 't2) Ast_pattern0.t
  -> (extension_constructor_kind, 't0, 't2) Ast_pattern0.t

val pext_rebind :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (extension_constructor_kind, 't0, 't1) Ast_pattern0.t

val pcty_constr :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (core_type list, 't1, 't2) Ast_pattern0.t
  -> (class_type, 't0, 't2) Ast_pattern0.t

val pcty_signature :
  (class_signature, 't0, 't1) Ast_pattern0.t
  -> (class_type, 't0, 't1) Ast_pattern0.t

val pcty_arrow :
  (arg_label, 't0, 't1) Ast_pattern0.t
  -> (core_type, 't1, 't2) Ast_pattern0.t
  -> (class_type, 't2, 't3) Ast_pattern0.t
  -> (class_type, 't0, 't3) Ast_pattern0.t

val pcty_extension :
  ((string Astlib.Loc.t * payload), 't0, 't1) Ast_pattern0.t
  -> (class_type, 't0, 't1) Ast_pattern0.t

val pcty_open :
  (override_flag, 't0, 't1) Ast_pattern0.t
  -> (longident, 't1, 't2) Ast_pattern0.t
  -> (class_type, 't2, 't3) Ast_pattern0.t
  -> (class_type, 't0, 't3) Ast_pattern0.t

val pcty_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (class_type, 't1, 't2) Ast_pattern0.t

val pcty_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (class_type, 't1, 't2) Ast_pattern0.t

val pctf_inherit :
  (class_type, 't0, 't1) Ast_pattern0.t
  -> (class_type_field, 't0, 't1) Ast_pattern0.t

val pctf_val :
  ((string Astlib.Loc.t * mutable_flag * virtual_flag * core_type), 't0, 't1) Ast_pattern0.t
  -> (class_type_field, 't0, 't1) Ast_pattern0.t

val pctf_method :
  ((string Astlib.Loc.t * private_flag * virtual_flag * core_type), 't0, 't1) Ast_pattern0.t
  -> (class_type_field, 't0, 't1) Ast_pattern0.t

val pctf_constraint :
  ((core_type * core_type), 't0, 't1) Ast_pattern0.t
  -> (class_type_field, 't0, 't1) Ast_pattern0.t

val pctf_attribute :
  ((string Astlib.Loc.t * payload), 't0, 't1) Ast_pattern0.t
  -> (class_type_field, 't0, 't1) Ast_pattern0.t

val pctf_extension :
  ((string Astlib.Loc.t * payload), 't0, 't1) Ast_pattern0.t
  -> (class_type_field, 't0, 't1) Ast_pattern0.t

val pctf_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (class_type_field, 't1, 't2) Ast_pattern0.t

val pctf_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (class_type_field, 't1, 't2) Ast_pattern0.t

val pci_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> ('a node class_infos, 't1, 't2) Ast_pattern0.t

val pci_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> ('a node class_infos, 't1, 't2) Ast_pattern0.t

val pcl_constr :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (core_type list, 't1, 't2) Ast_pattern0.t
  -> (class_expr, 't0, 't2) Ast_pattern0.t

val pcl_structure :
  (class_structure, 't0, 't1) Ast_pattern0.t
  -> (class_expr, 't0, 't1) Ast_pattern0.t

val pcl_fun :
  (arg_label, 't0, 't1) Ast_pattern0.t
  -> (expression option, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't2, 't3) Ast_pattern0.t
  -> (class_expr, 't3, 't4) Ast_pattern0.t
  -> (class_expr, 't0, 't4) Ast_pattern0.t

val pcl_apply :
  (class_expr, 't0, 't1) Ast_pattern0.t
  -> ((arg_label * expression) list, 't1, 't2) Ast_pattern0.t
  -> (class_expr, 't0, 't2) Ast_pattern0.t

val pcl_let :
  (rec_flag, 't0, 't1) Ast_pattern0.t
  -> (value_binding list, 't1, 't2) Ast_pattern0.t
  -> (class_expr, 't2, 't3) Ast_pattern0.t
  -> (class_expr, 't0, 't3) Ast_pattern0.t

val pcl_constraint :
  (class_expr, 't0, 't1) Ast_pattern0.t
  -> (class_type, 't1, 't2) Ast_pattern0.t
  -> (class_expr, 't0, 't2) Ast_pattern0.t

val pcl_extension :
  ((string Astlib.Loc.t * payload), 't0, 't1) Ast_pattern0.t
  -> (class_expr, 't0, 't1) Ast_pattern0.t

val pcl_open :
  (override_flag, 't0, 't1) Ast_pattern0.t
  -> (longident, 't1, 't2) Ast_pattern0.t
  -> (class_expr, 't2, 't3) Ast_pattern0.t
  -> (class_expr, 't0, 't3) Ast_pattern0.t

val pcl_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (class_expr, 't1, 't2) Ast_pattern0.t

val pcl_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (class_expr, 't1, 't2) Ast_pattern0.t

val pcf_inherit :
  (override_flag, 't0, 't1) Ast_pattern0.t
  -> (class_expr, 't1, 't2) Ast_pattern0.t
  -> (string Astlib.Loc.t option, 't2, 't3) Ast_pattern0.t
  -> (class_field, 't0, 't3) Ast_pattern0.t

val pcf_val :
  ((string Astlib.Loc.t * mutable_flag * class_field_kind), 't0, 't1) Ast_pattern0.t
  -> (class_field, 't0, 't1) Ast_pattern0.t

val pcf_method :
  ((string Astlib.Loc.t * private_flag * class_field_kind), 't0, 't1) Ast_pattern0.t
  -> (class_field, 't0, 't1) Ast_pattern0.t

val pcf_constraint :
  ((core_type * core_type), 't0, 't1) Ast_pattern0.t
  -> (class_field, 't0, 't1) Ast_pattern0.t

val pcf_initializer :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (class_field, 't0, 't1) Ast_pattern0.t

val pcf_attribute :
  ((string Astlib.Loc.t * payload), 't0, 't1) Ast_pattern0.t
  -> (class_field, 't0, 't1) Ast_pattern0.t

val pcf_extension :
  ((string Astlib.Loc.t * payload), 't0, 't1) Ast_pattern0.t
  -> (class_field, 't0, 't1) Ast_pattern0.t

val pcf_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (class_field, 't1, 't2) Ast_pattern0.t

val pcf_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (class_field, 't1, 't2) Ast_pattern0.t

val cfk_virtual :
  (core_type, 't0, 't1) Ast_pattern0.t
  -> (class_field_kind, 't0, 't1) Ast_pattern0.t

val cfk_concrete :
  (override_flag, 't0, 't1) Ast_pattern0.t
  -> (expression, 't1, 't2) Ast_pattern0.t
  -> (class_field_kind, 't0, 't2) Ast_pattern0.t

val pmty_ident :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (module_type, 't0, 't1) Ast_pattern0.t

val pmty_signature :
  (signature_item list, 't0, 't1) Ast_pattern0.t
  -> (module_type, 't0, 't1) Ast_pattern0.t

val pmty_functor :
  (string, 't0, 't1) Ast_pattern0.t
  -> (module_type option, 't1, 't2) Ast_pattern0.t
  -> (module_type, 't2, 't3) Ast_pattern0.t
  -> (module_type, 't0, 't3) Ast_pattern0.t

val pmty_with :
  (module_type, 't0, 't1) Ast_pattern0.t
  -> (with_constraint list, 't1, 't2) Ast_pattern0.t
  -> (module_type, 't0, 't2) Ast_pattern0.t

val pmty_typeof :
  (module_expr, 't0, 't1) Ast_pattern0.t
  -> (module_type, 't0, 't1) Ast_pattern0.t

val pmty_extension :
  ((string Astlib.Loc.t * payload), 't0, 't1) Ast_pattern0.t
  -> (module_type, 't0, 't1) Ast_pattern0.t

val pmty_alias :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (module_type, 't0, 't1) Ast_pattern0.t

val pmty_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (module_type, 't1, 't2) Ast_pattern0.t

val pmty_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (module_type, 't1, 't2) Ast_pattern0.t

val psig_value :
  (value_description, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_type :
  (rec_flag, 't0, 't1) Ast_pattern0.t
  -> (type_declaration list, 't1, 't2) Ast_pattern0.t
  -> (signature_item, 't0, 't2) Ast_pattern0.t

val psig_typext :
  (type_extension, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_exception :
  (extension_constructor, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_module :
  (module_declaration, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_recmodule :
  (module_declaration list, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_modtype :
  (module_type_declaration, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_open :
  (open_description, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_include :
  (module_type include_infos, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_class :
  (class_description list, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_class_type :
  (class_type_declaration list, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_attribute :
  ((string Astlib.Loc.t * payload), 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_extension :
  ((string Astlib.Loc.t * payload), 't0, 't1) Ast_pattern0.t
  -> (attribute list, 't1, 't2) Ast_pattern0.t
  -> (signature_item, 't0, 't2) Ast_pattern0.t

val psig_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (signature_item, 't1, 't2) Ast_pattern0.t

val pmd_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (module_declaration, 't1, 't2) Ast_pattern0.t

val pmd_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (module_declaration, 't1, 't2) Ast_pattern0.t

val pmtd_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (module_type_declaration, 't1, 't2) Ast_pattern0.t

val pmtd_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (module_type_declaration, 't1, 't2) Ast_pattern0.t

val popen_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (open_description, 't1, 't2) Ast_pattern0.t

val popen_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (open_description, 't1, 't2) Ast_pattern0.t

val pincl_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> ('a node include_infos, 't1, 't2) Ast_pattern0.t

val pincl_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> ('a node include_infos, 't1, 't2) Ast_pattern0.t

val pwith_type :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (type_declaration, 't1, 't2) Ast_pattern0.t
  -> (with_constraint, 't0, 't2) Ast_pattern0.t

val pwith_module :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (longident, 't1, 't2) Ast_pattern0.t
  -> (with_constraint, 't0, 't2) Ast_pattern0.t

val pwith_typesubst :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (type_declaration, 't1, 't2) Ast_pattern0.t
  -> (with_constraint, 't0, 't2) Ast_pattern0.t

val pwith_modsubst :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (longident, 't1, 't2) Ast_pattern0.t
  -> (with_constraint, 't0, 't2) Ast_pattern0.t

val pmod_ident :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (module_expr, 't0, 't1) Ast_pattern0.t

val pmod_structure :
  (structure_item list, 't0, 't1) Ast_pattern0.t
  -> (module_expr, 't0, 't1) Ast_pattern0.t

val pmod_functor :
  (string, 't0, 't1) Ast_pattern0.t
  -> (module_type option, 't1, 't2) Ast_pattern0.t
  -> (module_expr, 't2, 't3) Ast_pattern0.t
  -> (module_expr, 't0, 't3) Ast_pattern0.t

val pmod_apply :
  (module_expr, 't0, 't1) Ast_pattern0.t
  -> (module_expr, 't1, 't2) Ast_pattern0.t
  -> (module_expr, 't0, 't2) Ast_pattern0.t

val pmod_constraint :
  (module_expr, 't0, 't1) Ast_pattern0.t
  -> (module_type, 't1, 't2) Ast_pattern0.t
  -> (module_expr, 't0, 't2) Ast_pattern0.t

val pmod_unpack :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (module_expr, 't0, 't1) Ast_pattern0.t

val pmod_extension :
  ((string Astlib.Loc.t * payload), 't0, 't1) Ast_pattern0.t
  -> (module_expr, 't0, 't1) Ast_pattern0.t

val pmod_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (module_expr, 't1, 't2) Ast_pattern0.t

val pmod_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (module_expr, 't1, 't2) Ast_pattern0.t

val pstr_eval :
  (expression, 't0, 't1) Ast_pattern0.t
  -> (attribute list, 't1, 't2) Ast_pattern0.t
  -> (structure_item, 't0, 't2) Ast_pattern0.t

val pstr_value :
  (rec_flag, 't0, 't1) Ast_pattern0.t
  -> (value_binding list, 't1, 't2) Ast_pattern0.t
  -> (structure_item, 't0, 't2) Ast_pattern0.t

val pstr_primitive :
  (value_description, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_type :
  (rec_flag, 't0, 't1) Ast_pattern0.t
  -> (type_declaration list, 't1, 't2) Ast_pattern0.t
  -> (structure_item, 't0, 't2) Ast_pattern0.t

val pstr_typext :
  (type_extension, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_exception :
  (extension_constructor, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_module :
  (module_binding, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_recmodule :
  (module_binding list, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_modtype :
  (module_type_declaration, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_open :
  (open_description, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_class :
  (class_declaration list, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_class_type :
  (class_type_declaration list, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_include :
  (module_expr include_infos, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_attribute :
  ((string Astlib.Loc.t * payload), 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_extension :
  ((string Astlib.Loc.t * payload), 't0, 't1) Ast_pattern0.t
  -> (attribute list, 't1, 't2) Ast_pattern0.t
  -> (structure_item, 't0, 't2) Ast_pattern0.t

val pstr_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (structure_item, 't1, 't2) Ast_pattern0.t

val pvb_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (value_binding, 't1, 't2) Ast_pattern0.t

val pvb_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (value_binding, 't1, 't2) Ast_pattern0.t

val pmb_attributes :
  (attribute list, 't1, 't2) Ast_pattern0.t
  -> (module_binding, 't1, 't2) Ast_pattern0.t

val pmb_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (module_binding, 't1, 't2) Ast_pattern0.t

val ptop_def :
  (structure_item list, 't0, 't1) Ast_pattern0.t
  -> (toplevel_phrase, 't0, 't1) Ast_pattern0.t

val ptop_dir :
  (string, 't0, 't1) Ast_pattern0.t
  -> (directive_argument, 't1, 't2) Ast_pattern0.t
  -> (toplevel_phrase, 't0, 't2) Ast_pattern0.t

val pdir_string :
  (string, 't0, 't1) Ast_pattern0.t
  -> (directive_argument, 't0, 't1) Ast_pattern0.t

val pdir_int :
  (string, 't0, 't1) Ast_pattern0.t
  -> (char option, 't1, 't2) Ast_pattern0.t
  -> (directive_argument, 't0, 't2) Ast_pattern0.t

val pdir_ident :
  (longident, 't0, 't1) Ast_pattern0.t
  -> (directive_argument, 't0, 't1) Ast_pattern0.t

val pdir_bool :
  (bool, 't0, 't1) Ast_pattern0.t
  -> (directive_argument, 't0, 't1) Ast_pattern0.t
(*$*)
