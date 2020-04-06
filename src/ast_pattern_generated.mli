open! Import

(*$ Ppxlib_cinaps_helpers.generate_ast_pattern_intf () *)
val lident :
  (string, 't0, 't1) Ast_pattern0.t
  -> (longident, 't0, 't1) Ast_pattern0.t

val ldot :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (string, 't1, 't2) Ast_pattern0.t
  -> (longident, 't0, 't2) Ast_pattern0.t

val lapply :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (Longident.t, 't1, 't2) Ast_pattern0.t
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
  (Structure_item.t list, 't0, 't1) Ast_pattern0.t
  -> (payload, 't0, 't1) Ast_pattern0.t

val psig :
  (Signature_item.t list, 't0, 't1) Ast_pattern0.t
  -> (payload, 't0, 't1) Ast_pattern0.t

val ptyp :
  (Core_type.t, 't0, 't1) Ast_pattern0.t
  -> (payload, 't0, 't1) Ast_pattern0.t

val ppat :
  (Pattern.t, 't0, 't1) Ast_pattern0.t
  -> (Expression.t option, 't1, 't2) Ast_pattern0.t
  -> (payload, 't0, 't2) Ast_pattern0.t

val ptyp_var :
  (string, 't0, 't1) Ast_pattern0.t
  -> (core_type, 't0, 't1) Ast_pattern0.t

val ptyp_arrow :
  (Arg_label.t, 't0, 't1) Ast_pattern0.t
  -> (Core_type.t, 't1, 't2) Ast_pattern0.t
  -> (Core_type.t, 't2, 't3) Ast_pattern0.t
  -> (core_type, 't0, 't3) Ast_pattern0.t

val ptyp_tuple :
  (Core_type.t list, 't0, 't1) Ast_pattern0.t
  -> (core_type, 't0, 't1) Ast_pattern0.t

val ptyp_constr :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (Core_type.t list, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't0, 't2) Ast_pattern0.t

val ptyp_object :
  (Object_field.t list, 't0, 't1) Ast_pattern0.t
  -> (Closed_flag.t, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't0, 't2) Ast_pattern0.t

val ptyp_class :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (Core_type.t list, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't0, 't2) Ast_pattern0.t

val ptyp_alias :
  (Core_type.t, 't0, 't1) Ast_pattern0.t
  -> (string, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't0, 't2) Ast_pattern0.t

val ptyp_variant :
  (Row_field.t list, 't0, 't1) Ast_pattern0.t
  -> (Closed_flag.t, 't1, 't2) Ast_pattern0.t
  -> (string list option, 't2, 't3) Ast_pattern0.t
  -> (core_type, 't0, 't3) Ast_pattern0.t

val ptyp_poly :
  (string Astlib.Loc.t list, 't0, 't1) Ast_pattern0.t
  -> (Core_type.t, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't0, 't2) Ast_pattern0.t

val ptyp_package :
  ((Longident_loc.t * (Longident_loc.t * Core_type.t) list), 't0, 't1) Ast_pattern0.t
  -> (core_type, 't0, 't1) Ast_pattern0.t

val ptyp_extension :
  ((string Astlib.Loc.t * Payload.t), 't0, 't1) Ast_pattern0.t
  -> (core_type, 't0, 't1) Ast_pattern0.t

val ptyp_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't1, 't2) Ast_pattern0.t

val ptyp_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (core_type, 't1, 't2) Ast_pattern0.t

val rtag :
  (string, 't0, 't1) Ast_pattern0.t
  -> (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (bool, 't2, 't3) Ast_pattern0.t
  -> (Core_type.t list, 't3, 't4) Ast_pattern0.t
  -> (row_field, 't0, 't4) Ast_pattern0.t

val rinherit :
  (Core_type.t, 't0, 't1) Ast_pattern0.t
  -> (row_field, 't0, 't1) Ast_pattern0.t

val otag :
  (string, 't0, 't1) Ast_pattern0.t
  -> (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (Core_type.t, 't2, 't3) Ast_pattern0.t
  -> (object_field, 't0, 't3) Ast_pattern0.t

val oinherit :
  (Core_type.t, 't0, 't1) Ast_pattern0.t
  -> (object_field, 't0, 't1) Ast_pattern0.t

val ppat_var :
  (string, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_alias :
  (Pattern.t, 't0, 't1) Ast_pattern0.t
  -> (string, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_constant :
  (Constant.t, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_interval :
  (Constant.t, 't0, 't1) Ast_pattern0.t
  -> (Constant.t, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_tuple :
  (Pattern.t list, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_construct :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (Pattern.t option, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_variant :
  (string, 't0, 't1) Ast_pattern0.t
  -> (Pattern.t option, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_record :
  ((Longident_loc.t * Pattern.t) list, 't0, 't1) Ast_pattern0.t
  -> (Closed_flag.t, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_array :
  (Pattern.t list, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_or :
  (Pattern.t, 't0, 't1) Ast_pattern0.t
  -> (Pattern.t, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_constraint :
  (Pattern.t, 't0, 't1) Ast_pattern0.t
  -> (Core_type.t, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_type :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_lazy :
  (Pattern.t, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_unpack :
  (string, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_exception :
  (Pattern.t, 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_extension :
  ((string Astlib.Loc.t * Payload.t), 't0, 't1) Ast_pattern0.t
  -> (pattern, 't0, 't1) Ast_pattern0.t

val ppat_open :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (Pattern.t, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't0, 't2) Ast_pattern0.t

val ppat_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't1, 't2) Ast_pattern0.t

val ppat_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (pattern, 't1, 't2) Ast_pattern0.t

val pexp_ident :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_constant :
  (Constant.t, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_let :
  (Rec_flag.t, 't0, 't1) Ast_pattern0.t
  -> (Value_binding.t list, 't1, 't2) Ast_pattern0.t
  -> (Expression.t, 't2, 't3) Ast_pattern0.t
  -> (expression, 't0, 't3) Ast_pattern0.t

val pexp_function :
  (Case.t list, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_fun :
  (Arg_label.t, 't0, 't1) Ast_pattern0.t
  -> (Expression.t option, 't1, 't2) Ast_pattern0.t
  -> (Pattern.t, 't2, 't3) Ast_pattern0.t
  -> (Expression.t, 't3, 't4) Ast_pattern0.t
  -> (expression, 't0, 't4) Ast_pattern0.t

val pexp_apply :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> ((Arg_label.t * Expression.t) list, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_match :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (Case.t list, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_try :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (Case.t list, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_tuple :
  (Expression.t list, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_construct :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (Expression.t option, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_variant :
  (string, 't0, 't1) Ast_pattern0.t
  -> (Expression.t option, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_record :
  ((Longident_loc.t * Expression.t) list, 't0, 't1) Ast_pattern0.t
  -> (Expression.t option, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_field :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (Longident.t, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_setfield :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (Longident.t, 't1, 't2) Ast_pattern0.t
  -> (Expression.t, 't2, 't3) Ast_pattern0.t
  -> (expression, 't0, 't3) Ast_pattern0.t

val pexp_array :
  (Expression.t list, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_ifthenelse :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (Expression.t, 't1, 't2) Ast_pattern0.t
  -> (Expression.t option, 't2, 't3) Ast_pattern0.t
  -> (expression, 't0, 't3) Ast_pattern0.t

val pexp_sequence :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (Expression.t, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_while :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (Expression.t, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_for :
  (Pattern.t, 't0, 't1) Ast_pattern0.t
  -> (Expression.t, 't1, 't2) Ast_pattern0.t
  -> (Expression.t, 't2, 't3) Ast_pattern0.t
  -> (Direction_flag.t, 't3, 't4) Ast_pattern0.t
  -> (Expression.t, 't4, 't5) Ast_pattern0.t
  -> (expression, 't0, 't5) Ast_pattern0.t

val pexp_constraint :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (Core_type.t, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_coerce :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (Core_type.t option, 't1, 't2) Ast_pattern0.t
  -> (Core_type.t, 't2, 't3) Ast_pattern0.t
  -> (expression, 't0, 't3) Ast_pattern0.t

val pexp_send :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (string, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_new :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_setinstvar :
  (string, 't0, 't1) Ast_pattern0.t
  -> (Expression.t, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_override :
  ((string Astlib.Loc.t * Expression.t) list, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_letmodule :
  (string, 't0, 't1) Ast_pattern0.t
  -> (Module_expr.t, 't1, 't2) Ast_pattern0.t
  -> (Expression.t, 't2, 't3) Ast_pattern0.t
  -> (expression, 't0, 't3) Ast_pattern0.t

val pexp_letexception :
  (Extension_constructor.t, 't0, 't1) Ast_pattern0.t
  -> (Expression.t, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_assert :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_lazy :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_poly :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (Core_type.t option, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_object :
  (Class_structure.t, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_newtype :
  (string, 't0, 't1) Ast_pattern0.t
  -> (Expression.t, 't1, 't2) Ast_pattern0.t
  -> (expression, 't0, 't2) Ast_pattern0.t

val pexp_pack :
  (Module_expr.t, 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_open :
  (Override_flag.t, 't0, 't1) Ast_pattern0.t
  -> (Longident.t, 't1, 't2) Ast_pattern0.t
  -> (Expression.t, 't2, 't3) Ast_pattern0.t
  -> (expression, 't0, 't3) Ast_pattern0.t

val pexp_extension :
  ((string Astlib.Loc.t * Payload.t), 't0, 't1) Ast_pattern0.t
  -> (expression, 't0, 't1) Ast_pattern0.t

val pexp_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (expression, 't1, 't2) Ast_pattern0.t

val pexp_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (expression, 't1, 't2) Ast_pattern0.t

val pval_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (value_description, 't1, 't2) Ast_pattern0.t

val pval_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (value_description, 't1, 't2) Ast_pattern0.t

val ptype_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (type_declaration, 't1, 't2) Ast_pattern0.t

val ptype_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (type_declaration, 't1, 't2) Ast_pattern0.t

val ptype_variant :
  (Constructor_declaration.t list, 't0, 't1) Ast_pattern0.t
  -> (type_kind, 't0, 't1) Ast_pattern0.t

val ptype_record :
  (Label_declaration.t list, 't0, 't1) Ast_pattern0.t
  -> (type_kind, 't0, 't1) Ast_pattern0.t

val pld_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (label_declaration, 't1, 't2) Ast_pattern0.t

val pld_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (label_declaration, 't1, 't2) Ast_pattern0.t

val pcd_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (constructor_declaration, 't1, 't2) Ast_pattern0.t

val pcd_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (constructor_declaration, 't1, 't2) Ast_pattern0.t

val pcstr_tuple :
  (Core_type.t list, 't0, 't1) Ast_pattern0.t
  -> (constructor_arguments, 't0, 't1) Ast_pattern0.t

val pcstr_record :
  (Label_declaration.t list, 't0, 't1) Ast_pattern0.t
  -> (constructor_arguments, 't0, 't1) Ast_pattern0.t

val ptyext_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (type_extension, 't1, 't2) Ast_pattern0.t

val pext_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (extension_constructor, 't1, 't2) Ast_pattern0.t

val pext_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (extension_constructor, 't1, 't2) Ast_pattern0.t

val pext_decl :
  (Constructor_arguments.t, 't0, 't1) Ast_pattern0.t
  -> (Core_type.t option, 't1, 't2) Ast_pattern0.t
  -> (extension_constructor_kind, 't0, 't2) Ast_pattern0.t

val pext_rebind :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (extension_constructor_kind, 't0, 't1) Ast_pattern0.t

val pcty_constr :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (Core_type.t list, 't1, 't2) Ast_pattern0.t
  -> (class_type, 't0, 't2) Ast_pattern0.t

val pcty_signature :
  (Class_signature.t, 't0, 't1) Ast_pattern0.t
  -> (class_type, 't0, 't1) Ast_pattern0.t

val pcty_arrow :
  (Arg_label.t, 't0, 't1) Ast_pattern0.t
  -> (Core_type.t, 't1, 't2) Ast_pattern0.t
  -> (Class_type.t, 't2, 't3) Ast_pattern0.t
  -> (class_type, 't0, 't3) Ast_pattern0.t

val pcty_extension :
  ((string Astlib.Loc.t * Payload.t), 't0, 't1) Ast_pattern0.t
  -> (class_type, 't0, 't1) Ast_pattern0.t

val pcty_open :
  (Override_flag.t, 't0, 't1) Ast_pattern0.t
  -> (Longident.t, 't1, 't2) Ast_pattern0.t
  -> (Class_type.t, 't2, 't3) Ast_pattern0.t
  -> (class_type, 't0, 't3) Ast_pattern0.t

val pcty_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (class_type, 't1, 't2) Ast_pattern0.t

val pcty_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (class_type, 't1, 't2) Ast_pattern0.t

val pctf_inherit :
  (Class_type.t, 't0, 't1) Ast_pattern0.t
  -> (class_type_field, 't0, 't1) Ast_pattern0.t

val pctf_val :
  ((string Astlib.Loc.t * Mutable_flag.t * Virtual_flag.t * Core_type.t), 't0, 't1) Ast_pattern0.t
  -> (class_type_field, 't0, 't1) Ast_pattern0.t

val pctf_method :
  ((string Astlib.Loc.t * Private_flag.t * Virtual_flag.t * Core_type.t), 't0, 't1) Ast_pattern0.t
  -> (class_type_field, 't0, 't1) Ast_pattern0.t

val pctf_constraint :
  ((Core_type.t * Core_type.t), 't0, 't1) Ast_pattern0.t
  -> (class_type_field, 't0, 't1) Ast_pattern0.t

val pctf_attribute :
  ((string Astlib.Loc.t * Payload.t), 't0, 't1) Ast_pattern0.t
  -> (class_type_field, 't0, 't1) Ast_pattern0.t

val pctf_extension :
  ((string Astlib.Loc.t * Payload.t), 't0, 't1) Ast_pattern0.t
  -> (class_type_field, 't0, 't1) Ast_pattern0.t

val pctf_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (class_type_field, 't1, 't2) Ast_pattern0.t

val pctf_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (class_type_field, 't1, 't2) Ast_pattern0.t

val pci_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> ('a node class_infos, 't1, 't2) Ast_pattern0.t

val pci_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> ('a node class_infos, 't1, 't2) Ast_pattern0.t

val pcl_constr :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (Core_type.t list, 't1, 't2) Ast_pattern0.t
  -> (class_expr, 't0, 't2) Ast_pattern0.t

val pcl_structure :
  (Class_structure.t, 't0, 't1) Ast_pattern0.t
  -> (class_expr, 't0, 't1) Ast_pattern0.t

val pcl_fun :
  (Arg_label.t, 't0, 't1) Ast_pattern0.t
  -> (Expression.t option, 't1, 't2) Ast_pattern0.t
  -> (Pattern.t, 't2, 't3) Ast_pattern0.t
  -> (Class_expr.t, 't3, 't4) Ast_pattern0.t
  -> (class_expr, 't0, 't4) Ast_pattern0.t

val pcl_apply :
  (Class_expr.t, 't0, 't1) Ast_pattern0.t
  -> ((Arg_label.t * Expression.t) list, 't1, 't2) Ast_pattern0.t
  -> (class_expr, 't0, 't2) Ast_pattern0.t

val pcl_let :
  (Rec_flag.t, 't0, 't1) Ast_pattern0.t
  -> (Value_binding.t list, 't1, 't2) Ast_pattern0.t
  -> (Class_expr.t, 't2, 't3) Ast_pattern0.t
  -> (class_expr, 't0, 't3) Ast_pattern0.t

val pcl_constraint :
  (Class_expr.t, 't0, 't1) Ast_pattern0.t
  -> (Class_type.t, 't1, 't2) Ast_pattern0.t
  -> (class_expr, 't0, 't2) Ast_pattern0.t

val pcl_extension :
  ((string Astlib.Loc.t * Payload.t), 't0, 't1) Ast_pattern0.t
  -> (class_expr, 't0, 't1) Ast_pattern0.t

val pcl_open :
  (Override_flag.t, 't0, 't1) Ast_pattern0.t
  -> (Longident.t, 't1, 't2) Ast_pattern0.t
  -> (Class_expr.t, 't2, 't3) Ast_pattern0.t
  -> (class_expr, 't0, 't3) Ast_pattern0.t

val pcl_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (class_expr, 't1, 't2) Ast_pattern0.t

val pcl_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (class_expr, 't1, 't2) Ast_pattern0.t

val pcf_inherit :
  (Override_flag.t, 't0, 't1) Ast_pattern0.t
  -> (Class_expr.t, 't1, 't2) Ast_pattern0.t
  -> (string Astlib.Loc.t option, 't2, 't3) Ast_pattern0.t
  -> (class_field, 't0, 't3) Ast_pattern0.t

val pcf_val :
  ((string Astlib.Loc.t * Mutable_flag.t * Class_field_kind.t), 't0, 't1) Ast_pattern0.t
  -> (class_field, 't0, 't1) Ast_pattern0.t

val pcf_method :
  ((string Astlib.Loc.t * Private_flag.t * Class_field_kind.t), 't0, 't1) Ast_pattern0.t
  -> (class_field, 't0, 't1) Ast_pattern0.t

val pcf_constraint :
  ((Core_type.t * Core_type.t), 't0, 't1) Ast_pattern0.t
  -> (class_field, 't0, 't1) Ast_pattern0.t

val pcf_initializer :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (class_field, 't0, 't1) Ast_pattern0.t

val pcf_attribute :
  ((string Astlib.Loc.t * Payload.t), 't0, 't1) Ast_pattern0.t
  -> (class_field, 't0, 't1) Ast_pattern0.t

val pcf_extension :
  ((string Astlib.Loc.t * Payload.t), 't0, 't1) Ast_pattern0.t
  -> (class_field, 't0, 't1) Ast_pattern0.t

val pcf_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (class_field, 't1, 't2) Ast_pattern0.t

val pcf_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (class_field, 't1, 't2) Ast_pattern0.t

val cfk_virtual :
  (Core_type.t, 't0, 't1) Ast_pattern0.t
  -> (class_field_kind, 't0, 't1) Ast_pattern0.t

val cfk_concrete :
  (Override_flag.t, 't0, 't1) Ast_pattern0.t
  -> (Expression.t, 't1, 't2) Ast_pattern0.t
  -> (class_field_kind, 't0, 't2) Ast_pattern0.t

val pmty_ident :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (module_type, 't0, 't1) Ast_pattern0.t

val pmty_signature :
  (Signature_item.t list, 't0, 't1) Ast_pattern0.t
  -> (module_type, 't0, 't1) Ast_pattern0.t

val pmty_functor :
  (string, 't0, 't1) Ast_pattern0.t
  -> (Module_type.t option, 't1, 't2) Ast_pattern0.t
  -> (Module_type.t, 't2, 't3) Ast_pattern0.t
  -> (module_type, 't0, 't3) Ast_pattern0.t

val pmty_with :
  (Module_type.t, 't0, 't1) Ast_pattern0.t
  -> (With_constraint.t list, 't1, 't2) Ast_pattern0.t
  -> (module_type, 't0, 't2) Ast_pattern0.t

val pmty_typeof :
  (Module_expr.t, 't0, 't1) Ast_pattern0.t
  -> (module_type, 't0, 't1) Ast_pattern0.t

val pmty_extension :
  ((string Astlib.Loc.t * Payload.t), 't0, 't1) Ast_pattern0.t
  -> (module_type, 't0, 't1) Ast_pattern0.t

val pmty_alias :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (module_type, 't0, 't1) Ast_pattern0.t

val pmty_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (module_type, 't1, 't2) Ast_pattern0.t

val pmty_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (module_type, 't1, 't2) Ast_pattern0.t

val psig_value :
  (Value_description.t, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_type :
  (Rec_flag.t, 't0, 't1) Ast_pattern0.t
  -> (Type_declaration.t list, 't1, 't2) Ast_pattern0.t
  -> (signature_item, 't0, 't2) Ast_pattern0.t

val psig_typext :
  (Type_extension.t, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_exception :
  (Extension_constructor.t, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_module :
  (Module_declaration.t, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_recmodule :
  (Module_declaration.t list, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_modtype :
  (Module_type_declaration.t, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_open :
  (Open_description.t, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_include :
  (Module_type.t Include_infos.t, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_class :
  (Class_description.t list, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_class_type :
  (Class_type_declaration.t list, 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_attribute :
  ((string Astlib.Loc.t * Payload.t), 't0, 't1) Ast_pattern0.t
  -> (signature_item, 't0, 't1) Ast_pattern0.t

val psig_extension :
  ((string Astlib.Loc.t * Payload.t), 't0, 't1) Ast_pattern0.t
  -> (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (signature_item, 't0, 't2) Ast_pattern0.t

val psig_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (signature_item, 't1, 't2) Ast_pattern0.t

val pmd_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (module_declaration, 't1, 't2) Ast_pattern0.t

val pmd_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (module_declaration, 't1, 't2) Ast_pattern0.t

val pmtd_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (module_type_declaration, 't1, 't2) Ast_pattern0.t

val pmtd_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (module_type_declaration, 't1, 't2) Ast_pattern0.t

val popen_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (open_description, 't1, 't2) Ast_pattern0.t

val popen_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (open_description, 't1, 't2) Ast_pattern0.t

val pincl_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> ('a node include_infos, 't1, 't2) Ast_pattern0.t

val pincl_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> ('a node include_infos, 't1, 't2) Ast_pattern0.t

val pwith_type :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (Type_declaration.t, 't1, 't2) Ast_pattern0.t
  -> (with_constraint, 't0, 't2) Ast_pattern0.t

val pwith_module :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (Longident.t, 't1, 't2) Ast_pattern0.t
  -> (with_constraint, 't0, 't2) Ast_pattern0.t

val pwith_typesubst :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (Type_declaration.t, 't1, 't2) Ast_pattern0.t
  -> (with_constraint, 't0, 't2) Ast_pattern0.t

val pwith_modsubst :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (Longident.t, 't1, 't2) Ast_pattern0.t
  -> (with_constraint, 't0, 't2) Ast_pattern0.t

val pmod_ident :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (module_expr, 't0, 't1) Ast_pattern0.t

val pmod_structure :
  (Structure_item.t list, 't0, 't1) Ast_pattern0.t
  -> (module_expr, 't0, 't1) Ast_pattern0.t

val pmod_functor :
  (string, 't0, 't1) Ast_pattern0.t
  -> (Module_type.t option, 't1, 't2) Ast_pattern0.t
  -> (Module_expr.t, 't2, 't3) Ast_pattern0.t
  -> (module_expr, 't0, 't3) Ast_pattern0.t

val pmod_apply :
  (Module_expr.t, 't0, 't1) Ast_pattern0.t
  -> (Module_expr.t, 't1, 't2) Ast_pattern0.t
  -> (module_expr, 't0, 't2) Ast_pattern0.t

val pmod_constraint :
  (Module_expr.t, 't0, 't1) Ast_pattern0.t
  -> (Module_type.t, 't1, 't2) Ast_pattern0.t
  -> (module_expr, 't0, 't2) Ast_pattern0.t

val pmod_unpack :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (module_expr, 't0, 't1) Ast_pattern0.t

val pmod_extension :
  ((string Astlib.Loc.t * Payload.t), 't0, 't1) Ast_pattern0.t
  -> (module_expr, 't0, 't1) Ast_pattern0.t

val pmod_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (module_expr, 't1, 't2) Ast_pattern0.t

val pmod_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (module_expr, 't1, 't2) Ast_pattern0.t

val pstr_eval :
  (Expression.t, 't0, 't1) Ast_pattern0.t
  -> (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (structure_item, 't0, 't2) Ast_pattern0.t

val pstr_value :
  (Rec_flag.t, 't0, 't1) Ast_pattern0.t
  -> (Value_binding.t list, 't1, 't2) Ast_pattern0.t
  -> (structure_item, 't0, 't2) Ast_pattern0.t

val pstr_primitive :
  (Value_description.t, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_type :
  (Rec_flag.t, 't0, 't1) Ast_pattern0.t
  -> (Type_declaration.t list, 't1, 't2) Ast_pattern0.t
  -> (structure_item, 't0, 't2) Ast_pattern0.t

val pstr_typext :
  (Type_extension.t, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_exception :
  (Extension_constructor.t, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_module :
  (Module_binding.t, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_recmodule :
  (Module_binding.t list, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_modtype :
  (Module_type_declaration.t, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_open :
  (Open_description.t, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_class :
  (Class_declaration.t list, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_class_type :
  (Class_type_declaration.t list, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_include :
  (Module_expr.t Include_infos.t, 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_attribute :
  ((string Astlib.Loc.t * Payload.t), 't0, 't1) Ast_pattern0.t
  -> (structure_item, 't0, 't1) Ast_pattern0.t

val pstr_extension :
  ((string Astlib.Loc.t * Payload.t), 't0, 't1) Ast_pattern0.t
  -> (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (structure_item, 't0, 't2) Ast_pattern0.t

val pstr_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (structure_item, 't1, 't2) Ast_pattern0.t

val pvb_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (value_binding, 't1, 't2) Ast_pattern0.t

val pvb_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (value_binding, 't1, 't2) Ast_pattern0.t

val pmb_attributes :
  (Attribute.t list, 't1, 't2) Ast_pattern0.t
  -> (module_binding, 't1, 't2) Ast_pattern0.t

val pmb_loc :
  (Astlib.Location.t, 't1, 't2) Ast_pattern0.t
  -> (module_binding, 't1, 't2) Ast_pattern0.t

val ptop_def :
  (Structure_item.t list, 't0, 't1) Ast_pattern0.t
  -> (toplevel_phrase, 't0, 't1) Ast_pattern0.t

val ptop_dir :
  (string, 't0, 't1) Ast_pattern0.t
  -> (Directive_argument.t, 't1, 't2) Ast_pattern0.t
  -> (toplevel_phrase, 't0, 't2) Ast_pattern0.t

val pdir_string :
  (string, 't0, 't1) Ast_pattern0.t
  -> (directive_argument, 't0, 't1) Ast_pattern0.t

val pdir_int :
  (string, 't0, 't1) Ast_pattern0.t
  -> (char option, 't1, 't2) Ast_pattern0.t
  -> (directive_argument, 't0, 't2) Ast_pattern0.t

val pdir_ident :
  (Longident.t, 't0, 't1) Ast_pattern0.t
  -> (directive_argument, 't0, 't1) Ast_pattern0.t

val pdir_bool :
  (bool, 't0, 't1) Ast_pattern0.t
  -> (directive_argument, 't0, 't1) Ast_pattern0.t
(*$*)
