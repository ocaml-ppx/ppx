(*$ Ppx_ast_cinaps.print_builder_mli (Astlib.Version.of_string "v4_07") *)
open Versions.V4_07
val ptyp_any :
  loc:Astlib.Location.t
  -> Core_type.t
val ptyp_var :
  loc:Astlib.Location.t
  -> string
  -> Core_type.t
val ptyp_arrow :
  loc:Astlib.Location.t
  -> Arg_label.t
  -> Core_type.t
  -> Core_type.t
  -> Core_type.t
val ptyp_tuple :
  loc:Astlib.Location.t
  -> Core_type.t list
  -> Core_type.t
val ptyp_constr :
  loc:Astlib.Location.t
  -> Longident_loc.t
  -> Core_type.t list
  -> Core_type.t
val ptyp_object :
  loc:Astlib.Location.t
  -> Object_field.t list
  -> Closed_flag.t
  -> Core_type.t
val ptyp_class :
  loc:Astlib.Location.t
  -> Longident_loc.t
  -> Core_type.t list
  -> Core_type.t
val ptyp_alias :
  loc:Astlib.Location.t
  -> Core_type.t
  -> string
  -> Core_type.t
val ptyp_variant :
  loc:Astlib.Location.t
  -> Row_field.t list
  -> Closed_flag.t
  -> Label.t list option
  -> Core_type.t
val ptyp_poly :
  loc:Astlib.Location.t
  -> string Astlib.Loc.t list
  -> Core_type.t
  -> Core_type.t
val ptyp_package :
  loc:Astlib.Location.t
  -> Package_type.t
  -> Core_type.t
val ptyp_extension :
  loc:Astlib.Location.t
  -> Extension.t
  -> Core_type.t
val ppat_any :
  loc:Astlib.Location.t
  -> Pattern.t
val ppat_var :
  loc:Astlib.Location.t
  -> string Astlib.Loc.t
  -> Pattern.t
val ppat_alias :
  loc:Astlib.Location.t
  -> Pattern.t
  -> string Astlib.Loc.t
  -> Pattern.t
val ppat_constant :
  loc:Astlib.Location.t
  -> Constant.t
  -> Pattern.t
val ppat_interval :
  loc:Astlib.Location.t
  -> Constant.t
  -> Constant.t
  -> Pattern.t
val ppat_tuple :
  loc:Astlib.Location.t
  -> Pattern.t list
  -> Pattern.t
val ppat_construct :
  loc:Astlib.Location.t
  -> Longident_loc.t
  -> Pattern.t option
  -> Pattern.t
val ppat_variant :
  loc:Astlib.Location.t
  -> Label.t
  -> Pattern.t option
  -> Pattern.t
val ppat_record :
  loc:Astlib.Location.t
  -> (Longident_loc.t * Pattern.t) list
  -> Closed_flag.t
  -> Pattern.t
val ppat_array :
  loc:Astlib.Location.t
  -> Pattern.t list
  -> Pattern.t
val ppat_or :
  loc:Astlib.Location.t
  -> Pattern.t
  -> Pattern.t
  -> Pattern.t
val ppat_constraint :
  loc:Astlib.Location.t
  -> Pattern.t
  -> Core_type.t
  -> Pattern.t
val ppat_type :
  loc:Astlib.Location.t
  -> Longident_loc.t
  -> Pattern.t
val ppat_lazy :
  loc:Astlib.Location.t
  -> Pattern.t
  -> Pattern.t
val ppat_unpack :
  loc:Astlib.Location.t
  -> string Astlib.Loc.t
  -> Pattern.t
val ppat_exception :
  loc:Astlib.Location.t
  -> Pattern.t
  -> Pattern.t
val ppat_extension :
  loc:Astlib.Location.t
  -> Extension.t
  -> Pattern.t
val ppat_open :
  loc:Astlib.Location.t
  -> Longident_loc.t
  -> Pattern.t
  -> Pattern.t
val pexp_ident :
  loc:Astlib.Location.t
  -> Longident_loc.t
  -> Expression.t
val pexp_constant :
  loc:Astlib.Location.t
  -> Constant.t
  -> Expression.t
val pexp_let :
  loc:Astlib.Location.t
  -> Rec_flag.t
  -> Value_binding.t list
  -> Expression.t
  -> Expression.t
val pexp_function :
  loc:Astlib.Location.t
  -> Case.t list
  -> Expression.t
val pexp_fun :
  loc:Astlib.Location.t
  -> Arg_label.t
  -> Expression.t option
  -> Pattern.t
  -> Expression.t
  -> Expression.t
val pexp_apply :
  loc:Astlib.Location.t
  -> Expression.t
  -> (Arg_label.t * Expression.t) list
  -> Expression.t
val pexp_match :
  loc:Astlib.Location.t
  -> Expression.t
  -> Case.t list
  -> Expression.t
val pexp_try :
  loc:Astlib.Location.t
  -> Expression.t
  -> Case.t list
  -> Expression.t
val pexp_tuple :
  loc:Astlib.Location.t
  -> Expression.t list
  -> Expression.t
val pexp_construct :
  loc:Astlib.Location.t
  -> Longident_loc.t
  -> Expression.t option
  -> Expression.t
val pexp_variant :
  loc:Astlib.Location.t
  -> Label.t
  -> Expression.t option
  -> Expression.t
val pexp_record :
  loc:Astlib.Location.t
  -> (Longident_loc.t * Expression.t) list
  -> Expression.t option
  -> Expression.t
val pexp_field :
  loc:Astlib.Location.t
  -> Expression.t
  -> Longident_loc.t
  -> Expression.t
val pexp_setfield :
  loc:Astlib.Location.t
  -> Expression.t
  -> Longident_loc.t
  -> Expression.t
  -> Expression.t
val pexp_array :
  loc:Astlib.Location.t
  -> Expression.t list
  -> Expression.t
val pexp_ifthenelse :
  loc:Astlib.Location.t
  -> Expression.t
  -> Expression.t
  -> Expression.t option
  -> Expression.t
val pexp_sequence :
  loc:Astlib.Location.t
  -> Expression.t
  -> Expression.t
  -> Expression.t
val pexp_while :
  loc:Astlib.Location.t
  -> Expression.t
  -> Expression.t
  -> Expression.t
val pexp_for :
  loc:Astlib.Location.t
  -> Pattern.t
  -> Expression.t
  -> Expression.t
  -> Direction_flag.t
  -> Expression.t
  -> Expression.t
val pexp_constraint :
  loc:Astlib.Location.t
  -> Expression.t
  -> Core_type.t
  -> Expression.t
val pexp_coerce :
  loc:Astlib.Location.t
  -> Expression.t
  -> Core_type.t option
  -> Core_type.t
  -> Expression.t
val pexp_send :
  loc:Astlib.Location.t
  -> Expression.t
  -> Label.t Astlib.Loc.t
  -> Expression.t
val pexp_new :
  loc:Astlib.Location.t
  -> Longident_loc.t
  -> Expression.t
val pexp_setinstvar :
  loc:Astlib.Location.t
  -> Label.t Astlib.Loc.t
  -> Expression.t
  -> Expression.t
val pexp_override :
  loc:Astlib.Location.t
  -> (Label.t Astlib.Loc.t * Expression.t) list
  -> Expression.t
val pexp_letmodule :
  loc:Astlib.Location.t
  -> string Astlib.Loc.t
  -> Module_expr.t
  -> Expression.t
  -> Expression.t
val pexp_letexception :
  loc:Astlib.Location.t
  -> Extension_constructor.t
  -> Expression.t
  -> Expression.t
val pexp_assert :
  loc:Astlib.Location.t
  -> Expression.t
  -> Expression.t
val pexp_lazy :
  loc:Astlib.Location.t
  -> Expression.t
  -> Expression.t
val pexp_poly :
  loc:Astlib.Location.t
  -> Expression.t
  -> Core_type.t option
  -> Expression.t
val pexp_object :
  loc:Astlib.Location.t
  -> Class_structure.t
  -> Expression.t
val pexp_newtype :
  loc:Astlib.Location.t
  -> string Astlib.Loc.t
  -> Expression.t
  -> Expression.t
val pexp_pack :
  loc:Astlib.Location.t
  -> Module_expr.t
  -> Expression.t
val pexp_open :
  loc:Astlib.Location.t
  -> Override_flag.t
  -> Longident_loc.t
  -> Expression.t
  -> Expression.t
val pexp_extension :
  loc:Astlib.Location.t
  -> Extension.t
  -> Expression.t
val pexp_unreachable :
  loc:Astlib.Location.t
  -> Expression.t
val case :
  guard:Expression.t option
  -> lhs:Pattern.t
  -> rhs:Expression.t
  -> Case.t
val value_description :
  loc:Astlib.Location.t
  -> name:string Astlib.Loc.t
  -> prim:string list
  -> type_:Core_type.t
  -> Value_description.t
val type_declaration :
  loc:Astlib.Location.t
  -> cstrs:(Core_type.t * Core_type.t * Astlib.Location.t) list
  -> kind:Type_kind.t
  -> manifest:Core_type.t option
  -> name:string Astlib.Loc.t
  -> params:(Core_type.t * Variance.t) list
  -> private_:Private_flag.t
  -> Type_declaration.t
val label_declaration :
  loc:Astlib.Location.t
  -> mutable_:Mutable_flag.t
  -> name:string Astlib.Loc.t
  -> type_:Core_type.t
  -> Label_declaration.t
val constructor_declaration :
  loc:Astlib.Location.t
  -> args:Constructor_arguments.t
  -> name:string Astlib.Loc.t
  -> res:Core_type.t option
  -> Constructor_declaration.t
val type_extension :
  constructors:Extension_constructor.t list
  -> params:(Core_type.t * Variance.t) list
  -> path:Longident_loc.t
  -> private_:Private_flag.t
  -> Type_extension.t
val extension_constructor :
  loc:Astlib.Location.t
  -> kind:Extension_constructor_kind.t
  -> name:string Astlib.Loc.t
  -> Extension_constructor.t
val pcty_constr :
  loc:Astlib.Location.t
  -> Longident_loc.t
  -> Core_type.t list
  -> Class_type.t
val pcty_signature :
  loc:Astlib.Location.t
  -> Class_signature.t
  -> Class_type.t
val pcty_arrow :
  loc:Astlib.Location.t
  -> Arg_label.t
  -> Core_type.t
  -> Class_type.t
  -> Class_type.t
val pcty_extension :
  loc:Astlib.Location.t
  -> Extension.t
  -> Class_type.t
val pcty_open :
  loc:Astlib.Location.t
  -> Override_flag.t
  -> Longident_loc.t
  -> Class_type.t
  -> Class_type.t
val class_signature :
  fields:Class_type_field.t list
  -> self:Core_type.t
  -> Class_signature.t
val pctf_inherit :
  loc:Astlib.Location.t
  -> Class_type.t
  -> Class_type_field.t
val pctf_val :
  loc:Astlib.Location.t
  -> (Label.t Astlib.Loc.t * Mutable_flag.t * Virtual_flag.t * Core_type.t)
  -> Class_type_field.t
val pctf_method :
  loc:Astlib.Location.t
  -> (Label.t Astlib.Loc.t * Private_flag.t * Virtual_flag.t * Core_type.t)
  -> Class_type_field.t
val pctf_constraint :
  loc:Astlib.Location.t
  -> (Core_type.t * Core_type.t)
  -> Class_type_field.t
val pctf_attribute :
  loc:Astlib.Location.t
  -> Attribute.t
  -> Class_type_field.t
val pctf_extension :
  loc:Astlib.Location.t
  -> Extension.t
  -> Class_type_field.t
val pcl_constr :
  loc:Astlib.Location.t
  -> Longident_loc.t
  -> Core_type.t list
  -> Class_expr.t
val pcl_structure :
  loc:Astlib.Location.t
  -> Class_structure.t
  -> Class_expr.t
val pcl_fun :
  loc:Astlib.Location.t
  -> Arg_label.t
  -> Expression.t option
  -> Pattern.t
  -> Class_expr.t
  -> Class_expr.t
val pcl_apply :
  loc:Astlib.Location.t
  -> Class_expr.t
  -> (Arg_label.t * Expression.t) list
  -> Class_expr.t
val pcl_let :
  loc:Astlib.Location.t
  -> Rec_flag.t
  -> Value_binding.t list
  -> Class_expr.t
  -> Class_expr.t
val pcl_constraint :
  loc:Astlib.Location.t
  -> Class_expr.t
  -> Class_type.t
  -> Class_expr.t
val pcl_extension :
  loc:Astlib.Location.t
  -> Extension.t
  -> Class_expr.t
val pcl_open :
  loc:Astlib.Location.t
  -> Override_flag.t
  -> Longident_loc.t
  -> Class_expr.t
  -> Class_expr.t
val class_structure :
  fields:Class_field.t list
  -> self:Pattern.t
  -> Class_structure.t
val pcf_inherit :
  loc:Astlib.Location.t
  -> Override_flag.t
  -> Class_expr.t
  -> string Astlib.Loc.t option
  -> Class_field.t
val pcf_val :
  loc:Astlib.Location.t
  -> (Label.t Astlib.Loc.t * Mutable_flag.t * Class_field_kind.t)
  -> Class_field.t
val pcf_method :
  loc:Astlib.Location.t
  -> (Label.t Astlib.Loc.t * Private_flag.t * Class_field_kind.t)
  -> Class_field.t
val pcf_constraint :
  loc:Astlib.Location.t
  -> (Core_type.t * Core_type.t)
  -> Class_field.t
val pcf_initializer :
  loc:Astlib.Location.t
  -> Expression.t
  -> Class_field.t
val pcf_attribute :
  loc:Astlib.Location.t
  -> Attribute.t
  -> Class_field.t
val pcf_extension :
  loc:Astlib.Location.t
  -> Extension.t
  -> Class_field.t
val pmty_ident :
  loc:Astlib.Location.t
  -> Longident_loc.t
  -> Module_type.t
val pmty_signature :
  loc:Astlib.Location.t
  -> Signature.t
  -> Module_type.t
val pmty_functor :
  loc:Astlib.Location.t
  -> string Astlib.Loc.t
  -> Module_type.t option
  -> Module_type.t
  -> Module_type.t
val pmty_with :
  loc:Astlib.Location.t
  -> Module_type.t
  -> With_constraint.t list
  -> Module_type.t
val pmty_typeof :
  loc:Astlib.Location.t
  -> Module_expr.t
  -> Module_type.t
val pmty_extension :
  loc:Astlib.Location.t
  -> Extension.t
  -> Module_type.t
val pmty_alias :
  loc:Astlib.Location.t
  -> Longident_loc.t
  -> Module_type.t
val psig_value :
  loc:Astlib.Location.t
  -> Value_description.t
  -> Signature_item.t
val psig_type :
  loc:Astlib.Location.t
  -> Rec_flag.t
  -> Type_declaration.t list
  -> Signature_item.t
val psig_typext :
  loc:Astlib.Location.t
  -> Type_extension.t
  -> Signature_item.t
val psig_exception :
  loc:Astlib.Location.t
  -> Extension_constructor.t
  -> Signature_item.t
val psig_module :
  loc:Astlib.Location.t
  -> Module_declaration.t
  -> Signature_item.t
val psig_recmodule :
  loc:Astlib.Location.t
  -> Module_declaration.t list
  -> Signature_item.t
val psig_modtype :
  loc:Astlib.Location.t
  -> Module_type_declaration.t
  -> Signature_item.t
val psig_open :
  loc:Astlib.Location.t
  -> Open_description.t
  -> Signature_item.t
val psig_include :
  loc:Astlib.Location.t
  -> Include_description.t
  -> Signature_item.t
val psig_class :
  loc:Astlib.Location.t
  -> Class_description.t list
  -> Signature_item.t
val psig_class_type :
  loc:Astlib.Location.t
  -> Class_type_declaration.t list
  -> Signature_item.t
val psig_attribute :
  loc:Astlib.Location.t
  -> Attribute.t
  -> Signature_item.t
val psig_extension :
  loc:Astlib.Location.t
  -> Extension.t
  -> Attributes.t
  -> Signature_item.t
val module_declaration :
  loc:Astlib.Location.t
  -> name:string Astlib.Loc.t
  -> type_:Module_type.t
  -> Module_declaration.t
val module_type_declaration :
  loc:Astlib.Location.t
  -> name:string Astlib.Loc.t
  -> type_:Module_type.t option
  -> Module_type_declaration.t
val open_description :
  loc:Astlib.Location.t
  -> lid:Longident_loc.t
  -> override:Override_flag.t
  -> Open_description.t
val pmod_ident :
  loc:Astlib.Location.t
  -> Longident_loc.t
  -> Module_expr.t
val pmod_structure :
  loc:Astlib.Location.t
  -> Structure.t
  -> Module_expr.t
val pmod_functor :
  loc:Astlib.Location.t
  -> string Astlib.Loc.t
  -> Module_type.t option
  -> Module_expr.t
  -> Module_expr.t
val pmod_apply :
  loc:Astlib.Location.t
  -> Module_expr.t
  -> Module_expr.t
  -> Module_expr.t
val pmod_constraint :
  loc:Astlib.Location.t
  -> Module_expr.t
  -> Module_type.t
  -> Module_expr.t
val pmod_unpack :
  loc:Astlib.Location.t
  -> Expression.t
  -> Module_expr.t
val pmod_extension :
  loc:Astlib.Location.t
  -> Extension.t
  -> Module_expr.t
val pstr_eval :
  loc:Astlib.Location.t
  -> Expression.t
  -> Attributes.t
  -> Structure_item.t
val pstr_value :
  loc:Astlib.Location.t
  -> Rec_flag.t
  -> Value_binding.t list
  -> Structure_item.t
val pstr_primitive :
  loc:Astlib.Location.t
  -> Value_description.t
  -> Structure_item.t
val pstr_type :
  loc:Astlib.Location.t
  -> Rec_flag.t
  -> Type_declaration.t list
  -> Structure_item.t
val pstr_typext :
  loc:Astlib.Location.t
  -> Type_extension.t
  -> Structure_item.t
val pstr_exception :
  loc:Astlib.Location.t
  -> Extension_constructor.t
  -> Structure_item.t
val pstr_module :
  loc:Astlib.Location.t
  -> Module_binding.t
  -> Structure_item.t
val pstr_recmodule :
  loc:Astlib.Location.t
  -> Module_binding.t list
  -> Structure_item.t
val pstr_modtype :
  loc:Astlib.Location.t
  -> Module_type_declaration.t
  -> Structure_item.t
val pstr_open :
  loc:Astlib.Location.t
  -> Open_description.t
  -> Structure_item.t
val pstr_class :
  loc:Astlib.Location.t
  -> Class_declaration.t list
  -> Structure_item.t
val pstr_class_type :
  loc:Astlib.Location.t
  -> Class_type_declaration.t list
  -> Structure_item.t
val pstr_include :
  loc:Astlib.Location.t
  -> Include_declaration.t
  -> Structure_item.t
val pstr_attribute :
  loc:Astlib.Location.t
  -> Attribute.t
  -> Structure_item.t
val pstr_extension :
  loc:Astlib.Location.t
  -> Extension.t
  -> Attributes.t
  -> Structure_item.t
val value_binding :
  loc:Astlib.Location.t
  -> expr:Expression.t
  -> pat:Pattern.t
  -> Value_binding.t
val module_binding :
  loc:Astlib.Location.t
  -> expr:Module_expr.t
  -> name:string Astlib.Loc.t
  -> Module_binding.t
(*$*)
