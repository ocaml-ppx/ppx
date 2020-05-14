(*$ Ppx_ast_cinaps.print_builder_mli (Astlib.Version.of_string "v4_07") *)
open Versions
val ptyp_any :
  loc:Astlib.Location.t
  -> core_type
val ptyp_var :
  loc:Astlib.Location.t
  -> string
  -> core_type
val ptyp_arrow :
  loc:Astlib.Location.t
  -> arg_label
  -> core_type
  -> core_type
  -> core_type
val ptyp_tuple :
  loc:Astlib.Location.t
  -> core_type list
  -> core_type
val ptyp_constr :
  loc:Astlib.Location.t
  -> longident_loc
  -> core_type list
  -> core_type
val ptyp_object :
  loc:Astlib.Location.t
  -> object_field list
  -> closed_flag
  -> core_type
val ptyp_class :
  loc:Astlib.Location.t
  -> longident_loc
  -> core_type list
  -> core_type
val ptyp_alias :
  loc:Astlib.Location.t
  -> core_type
  -> string
  -> core_type
val ptyp_variant :
  loc:Astlib.Location.t
  -> row_field list
  -> closed_flag
  -> string list option
  -> core_type
val ptyp_poly :
  loc:Astlib.Location.t
  -> string Astlib.Loc.t list
  -> core_type
  -> core_type
val ptyp_package :
  loc:Astlib.Location.t
  -> package_type
  -> core_type
val ptyp_extension :
  loc:Astlib.Location.t
  -> extension
  -> core_type
val ppat_any :
  loc:Astlib.Location.t
  -> pattern
val ppat_var :
  loc:Astlib.Location.t
  -> string Astlib.Loc.t
  -> pattern
val ppat_alias :
  loc:Astlib.Location.t
  -> pattern
  -> string Astlib.Loc.t
  -> pattern
val ppat_constant :
  loc:Astlib.Location.t
  -> constant
  -> pattern
val ppat_interval :
  loc:Astlib.Location.t
  -> constant
  -> constant
  -> pattern
val ppat_tuple :
  loc:Astlib.Location.t
  -> pattern list
  -> pattern
val ppat_construct :
  loc:Astlib.Location.t
  -> longident_loc
  -> pattern option
  -> pattern
val ppat_variant :
  loc:Astlib.Location.t
  -> string
  -> pattern option
  -> pattern
val ppat_record :
  loc:Astlib.Location.t
  -> (longident_loc * pattern) list
  -> closed_flag
  -> pattern
val ppat_array :
  loc:Astlib.Location.t
  -> pattern list
  -> pattern
val ppat_or :
  loc:Astlib.Location.t
  -> pattern
  -> pattern
  -> pattern
val ppat_constraint :
  loc:Astlib.Location.t
  -> pattern
  -> core_type
  -> pattern
val ppat_type :
  loc:Astlib.Location.t
  -> longident_loc
  -> pattern
val ppat_lazy :
  loc:Astlib.Location.t
  -> pattern
  -> pattern
val ppat_unpack :
  loc:Astlib.Location.t
  -> string Astlib.Loc.t
  -> pattern
val ppat_exception :
  loc:Astlib.Location.t
  -> pattern
  -> pattern
val ppat_extension :
  loc:Astlib.Location.t
  -> extension
  -> pattern
val ppat_open :
  loc:Astlib.Location.t
  -> longident_loc
  -> pattern
  -> pattern
val pexp_ident :
  loc:Astlib.Location.t
  -> longident_loc
  -> expression
val pexp_constant :
  loc:Astlib.Location.t
  -> constant
  -> expression
val pexp_let :
  loc:Astlib.Location.t
  -> rec_flag
  -> value_binding list
  -> expression
  -> expression
val pexp_function :
  loc:Astlib.Location.t
  -> case list
  -> expression
val pexp_fun :
  loc:Astlib.Location.t
  -> arg_label
  -> expression option
  -> pattern
  -> expression
  -> expression
val pexp_apply :
  loc:Astlib.Location.t
  -> expression
  -> (arg_label * expression) list
  -> expression
val pexp_match :
  loc:Astlib.Location.t
  -> expression
  -> case list
  -> expression
val pexp_try :
  loc:Astlib.Location.t
  -> expression
  -> case list
  -> expression
val pexp_tuple :
  loc:Astlib.Location.t
  -> expression list
  -> expression
val pexp_construct :
  loc:Astlib.Location.t
  -> longident_loc
  -> expression option
  -> expression
val pexp_variant :
  loc:Astlib.Location.t
  -> string
  -> expression option
  -> expression
val pexp_record :
  loc:Astlib.Location.t
  -> (longident_loc * expression) list
  -> expression option
  -> expression
val pexp_field :
  loc:Astlib.Location.t
  -> expression
  -> longident_loc
  -> expression
val pexp_setfield :
  loc:Astlib.Location.t
  -> expression
  -> longident_loc
  -> expression
  -> expression
val pexp_array :
  loc:Astlib.Location.t
  -> expression list
  -> expression
val pexp_ifthenelse :
  loc:Astlib.Location.t
  -> expression
  -> expression
  -> expression option
  -> expression
val pexp_sequence :
  loc:Astlib.Location.t
  -> expression
  -> expression
  -> expression
val pexp_while :
  loc:Astlib.Location.t
  -> expression
  -> expression
  -> expression
val pexp_for :
  loc:Astlib.Location.t
  -> pattern
  -> expression
  -> expression
  -> direction_flag
  -> expression
  -> expression
val pexp_constraint :
  loc:Astlib.Location.t
  -> expression
  -> core_type
  -> expression
val pexp_coerce :
  loc:Astlib.Location.t
  -> expression
  -> core_type option
  -> core_type
  -> expression
val pexp_send :
  loc:Astlib.Location.t
  -> expression
  -> string Astlib.Loc.t
  -> expression
val pexp_new :
  loc:Astlib.Location.t
  -> longident_loc
  -> expression
val pexp_setinstvar :
  loc:Astlib.Location.t
  -> string Astlib.Loc.t
  -> expression
  -> expression
val pexp_override :
  loc:Astlib.Location.t
  -> (string Astlib.Loc.t * expression) list
  -> expression
val pexp_letmodule :
  loc:Astlib.Location.t
  -> string Astlib.Loc.t
  -> module_expr
  -> expression
  -> expression
val pexp_letexception :
  loc:Astlib.Location.t
  -> extension_constructor
  -> expression
  -> expression
val pexp_assert :
  loc:Astlib.Location.t
  -> expression
  -> expression
val pexp_lazy :
  loc:Astlib.Location.t
  -> expression
  -> expression
val pexp_poly :
  loc:Astlib.Location.t
  -> expression
  -> core_type option
  -> expression
val pexp_object :
  loc:Astlib.Location.t
  -> class_structure
  -> expression
val pexp_newtype :
  loc:Astlib.Location.t
  -> string Astlib.Loc.t
  -> expression
  -> expression
val pexp_pack :
  loc:Astlib.Location.t
  -> module_expr
  -> expression
val pexp_open :
  loc:Astlib.Location.t
  -> override_flag
  -> longident_loc
  -> expression
  -> expression
val pexp_extension :
  loc:Astlib.Location.t
  -> extension
  -> expression
val pexp_unreachable :
  loc:Astlib.Location.t
  -> expression
val case :
  guard:expression option
  -> lhs:pattern
  -> rhs:expression
  -> case
val value_description :
  loc:Astlib.Location.t
  -> name:string Astlib.Loc.t
  -> prim:string list
  -> type_:core_type
  -> value_description
val type_declaration :
  loc:Astlib.Location.t
  -> cstrs:(core_type * core_type * Astlib.Location.t) list
  -> kind:type_kind
  -> manifest:core_type option
  -> name:string Astlib.Loc.t
  -> params:(core_type * variance) list
  -> private_:private_flag
  -> type_declaration
val label_declaration :
  loc:Astlib.Location.t
  -> mutable_:mutable_flag
  -> name:string Astlib.Loc.t
  -> type_:core_type
  -> label_declaration
val constructor_declaration :
  loc:Astlib.Location.t
  -> args:constructor_arguments
  -> name:string Astlib.Loc.t
  -> res:core_type option
  -> constructor_declaration
val type_extension :
  constructors:extension_constructor list
  -> params:(core_type * variance) list
  -> path:longident_loc
  -> private_:private_flag
  -> type_extension
val extension_constructor :
  loc:Astlib.Location.t
  -> kind:extension_constructor_kind
  -> name:string Astlib.Loc.t
  -> extension_constructor
val pcty_constr :
  loc:Astlib.Location.t
  -> longident_loc
  -> core_type list
  -> class_type
val pcty_signature :
  loc:Astlib.Location.t
  -> class_signature
  -> class_type
val pcty_arrow :
  loc:Astlib.Location.t
  -> arg_label
  -> core_type
  -> class_type
  -> class_type
val pcty_extension :
  loc:Astlib.Location.t
  -> extension
  -> class_type
val pcty_open :
  loc:Astlib.Location.t
  -> override_flag
  -> longident_loc
  -> class_type
  -> class_type
val class_signature :
  fields:class_type_field list
  -> self:core_type
  -> class_signature
val pctf_inherit :
  loc:Astlib.Location.t
  -> class_type
  -> class_type_field
val pctf_val :
  loc:Astlib.Location.t
  -> (string Astlib.Loc.t * mutable_flag * virtual_flag * core_type)
  -> class_type_field
val pctf_method :
  loc:Astlib.Location.t
  -> (string Astlib.Loc.t * private_flag * virtual_flag * core_type)
  -> class_type_field
val pctf_constraint :
  loc:Astlib.Location.t
  -> (core_type * core_type)
  -> class_type_field
val pctf_attribute :
  loc:Astlib.Location.t
  -> attribute
  -> class_type_field
val pctf_extension :
  loc:Astlib.Location.t
  -> extension
  -> class_type_field
val pcl_constr :
  loc:Astlib.Location.t
  -> longident_loc
  -> core_type list
  -> class_expr
val pcl_structure :
  loc:Astlib.Location.t
  -> class_structure
  -> class_expr
val pcl_fun :
  loc:Astlib.Location.t
  -> arg_label
  -> expression option
  -> pattern
  -> class_expr
  -> class_expr
val pcl_apply :
  loc:Astlib.Location.t
  -> class_expr
  -> (arg_label * expression) list
  -> class_expr
val pcl_let :
  loc:Astlib.Location.t
  -> rec_flag
  -> value_binding list
  -> class_expr
  -> class_expr
val pcl_constraint :
  loc:Astlib.Location.t
  -> class_expr
  -> class_type
  -> class_expr
val pcl_extension :
  loc:Astlib.Location.t
  -> extension
  -> class_expr
val pcl_open :
  loc:Astlib.Location.t
  -> override_flag
  -> longident_loc
  -> class_expr
  -> class_expr
val class_structure :
  fields:class_field list
  -> self:pattern
  -> class_structure
val pcf_inherit :
  loc:Astlib.Location.t
  -> override_flag
  -> class_expr
  -> string Astlib.Loc.t option
  -> class_field
val pcf_val :
  loc:Astlib.Location.t
  -> (string Astlib.Loc.t * mutable_flag * class_field_kind)
  -> class_field
val pcf_method :
  loc:Astlib.Location.t
  -> (string Astlib.Loc.t * private_flag * class_field_kind)
  -> class_field
val pcf_constraint :
  loc:Astlib.Location.t
  -> (core_type * core_type)
  -> class_field
val pcf_initializer :
  loc:Astlib.Location.t
  -> expression
  -> class_field
val pcf_attribute :
  loc:Astlib.Location.t
  -> attribute
  -> class_field
val pcf_extension :
  loc:Astlib.Location.t
  -> extension
  -> class_field
val pmty_ident :
  loc:Astlib.Location.t
  -> longident_loc
  -> module_type
val pmty_signature :
  loc:Astlib.Location.t
  -> signature
  -> module_type
val pmty_functor :
  loc:Astlib.Location.t
  -> string Astlib.Loc.t
  -> module_type option
  -> module_type
  -> module_type
val pmty_with :
  loc:Astlib.Location.t
  -> module_type
  -> with_constraint list
  -> module_type
val pmty_typeof :
  loc:Astlib.Location.t
  -> module_expr
  -> module_type
val pmty_extension :
  loc:Astlib.Location.t
  -> extension
  -> module_type
val pmty_alias :
  loc:Astlib.Location.t
  -> longident_loc
  -> module_type
val psig_value :
  loc:Astlib.Location.t
  -> value_description
  -> signature_item
val psig_type :
  loc:Astlib.Location.t
  -> rec_flag
  -> type_declaration list
  -> signature_item
val psig_typext :
  loc:Astlib.Location.t
  -> type_extension
  -> signature_item
val psig_exception :
  loc:Astlib.Location.t
  -> extension_constructor
  -> signature_item
val psig_module :
  loc:Astlib.Location.t
  -> module_declaration
  -> signature_item
val psig_recmodule :
  loc:Astlib.Location.t
  -> module_declaration list
  -> signature_item
val psig_modtype :
  loc:Astlib.Location.t
  -> module_type_declaration
  -> signature_item
val psig_open :
  loc:Astlib.Location.t
  -> open_description
  -> signature_item
val psig_include :
  loc:Astlib.Location.t
  -> include_description
  -> signature_item
val psig_class :
  loc:Astlib.Location.t
  -> class_description list
  -> signature_item
val psig_class_type :
  loc:Astlib.Location.t
  -> class_type_declaration list
  -> signature_item
val psig_attribute :
  loc:Astlib.Location.t
  -> attribute
  -> signature_item
val psig_extension :
  loc:Astlib.Location.t
  -> extension
  -> attributes
  -> signature_item
val module_declaration :
  loc:Astlib.Location.t
  -> name:string Astlib.Loc.t
  -> type_:module_type
  -> module_declaration
val module_type_declaration :
  loc:Astlib.Location.t
  -> name:string Astlib.Loc.t
  -> type_:module_type option
  -> module_type_declaration
val open_description :
  loc:Astlib.Location.t
  -> lid:longident_loc
  -> override:override_flag
  -> open_description
val pmod_ident :
  loc:Astlib.Location.t
  -> longident_loc
  -> module_expr
val pmod_structure :
  loc:Astlib.Location.t
  -> structure
  -> module_expr
val pmod_functor :
  loc:Astlib.Location.t
  -> string Astlib.Loc.t
  -> module_type option
  -> module_expr
  -> module_expr
val pmod_apply :
  loc:Astlib.Location.t
  -> module_expr
  -> module_expr
  -> module_expr
val pmod_constraint :
  loc:Astlib.Location.t
  -> module_expr
  -> module_type
  -> module_expr
val pmod_unpack :
  loc:Astlib.Location.t
  -> expression
  -> module_expr
val pmod_extension :
  loc:Astlib.Location.t
  -> extension
  -> module_expr
val pstr_eval :
  loc:Astlib.Location.t
  -> expression
  -> attributes
  -> structure_item
val pstr_value :
  loc:Astlib.Location.t
  -> rec_flag
  -> value_binding list
  -> structure_item
val pstr_primitive :
  loc:Astlib.Location.t
  -> value_description
  -> structure_item
val pstr_type :
  loc:Astlib.Location.t
  -> rec_flag
  -> type_declaration list
  -> structure_item
val pstr_typext :
  loc:Astlib.Location.t
  -> type_extension
  -> structure_item
val pstr_exception :
  loc:Astlib.Location.t
  -> extension_constructor
  -> structure_item
val pstr_module :
  loc:Astlib.Location.t
  -> module_binding
  -> structure_item
val pstr_recmodule :
  loc:Astlib.Location.t
  -> module_binding list
  -> structure_item
val pstr_modtype :
  loc:Astlib.Location.t
  -> module_type_declaration
  -> structure_item
val pstr_open :
  loc:Astlib.Location.t
  -> open_description
  -> structure_item
val pstr_class :
  loc:Astlib.Location.t
  -> class_declaration list
  -> structure_item
val pstr_class_type :
  loc:Astlib.Location.t
  -> class_type_declaration list
  -> structure_item
val pstr_include :
  loc:Astlib.Location.t
  -> include_declaration
  -> structure_item
val pstr_attribute :
  loc:Astlib.Location.t
  -> attribute
  -> structure_item
val pstr_extension :
  loc:Astlib.Location.t
  -> extension
  -> attributes
  -> structure_item
val value_binding :
  loc:Astlib.Location.t
  -> expr:expression
  -> pat:pattern
  -> value_binding
val module_binding :
  loc:Astlib.Location.t
  -> expr:module_expr
  -> name:string Astlib.Loc.t
  -> module_binding
(*$*)
