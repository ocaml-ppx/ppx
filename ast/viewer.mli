open Viewlib

module type LOC_TYPES = sig
  val txt'match : ('a, 'i, 'o) View.t -> ('a Astlib.Loc.t, 'i, 'o) View.t
  val loc'match : (Astlib.Location.t, 'i, 'o) View.t -> ('a Astlib.Loc.t, 'i, 'o) View.t

  val loc_start'match : (Astlib.Position.t, 'i, 'o) View.t -> (Astlib.Location.t, 'i, 'o) View.t
  val loc_end'match : (Astlib.Position.t, 'i, 'o) View.t -> (Astlib.Location.t, 'i, 'o) View.t
  val loc_ghost'match : (bool, 'i, 'o) View.t -> (Astlib.Location.t, 'i, 'o) View.t

  val pos_fname'match : (string, 'i, 'o) View.t -> (Astlib.Position.t, 'i, 'o) View.t
  val pos_lnum'match : (int, 'i, 'o) View.t -> (Astlib.Position.t, 'i, 'o) View.t
  val pos_bol'match : (int, 'i, 'o) View.t -> (Astlib.Position.t, 'i, 'o) View.t
  val pos_cnum'match : (int, 'i, 'o) View.t -> (Astlib.Position.t, 'i, 'o) View.t
end

(*$ Ppx_ast_cinaps.print_viewer_mli () *)
module Unstable_for_testing : sig
  open Versions
  open Unstable_for_testing
  include LOC_TYPES
  val pdir_bool'const : (bool, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t
  val pdir_ident'const : (Longident.t, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t
  val pdir_int'const : ((char option * string), 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t
  val pdir_string'const : (string, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t

  val pdir_none'const : (Directive_argument.t, 'a, 'a) View.t
  val ptop_dir'const : ((Directive_argument.t * string), 'i, 'o) View.t -> (Toplevel_phrase.t, 'i, 'o) View.t
  val ptop_def'const : (Structure.t, 'i, 'o) View.t -> (Toplevel_phrase.t, 'i, 'o) View.t

  val pmb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_expr'match : (Module_expr.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pvb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_expr'match : (Expression.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_pat'match : (Pattern.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t
  val pstr_extension'const : ((Attributes.t * Extension.t), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_attribute'const : (Attribute.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_include'const : (Include_declaration.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_class_type'const : (Class_type_declaration.t list, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_class'const : (Class_declaration.t list, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_open'const : (Open_description.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_modtype'const : (Module_type_declaration.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_recmodule'const : (Module_binding.t list, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_module'const : (Module_binding.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_exception'const : (Extension_constructor.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_typext'const : (Type_extension.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_type'const : ((Type_declaration.t list * Rec_flag.t), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_primitive'const : (Value_description.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_value'const : ((Value_binding.t list * Rec_flag.t), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_eval'const : ((Attributes.t * Expression.t), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

  val pstr_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

  val pstr_desc'match : (Structure_item_desc.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pmod_extension'const : (Extension.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_unpack'const : (Expression.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_constraint'const : ((Module_type.t * Module_expr.t), 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_apply'const : ((Module_expr.t * Module_expr.t), 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_functor'const : ((Module_expr.t * Module_type.t option * string Astlib.Loc.t), 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_structure'const : (Structure.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_ident'const : (Longident_loc.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pmod_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pmod_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pmod_desc'match : (Module_expr_desc.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pwith_modsubst'const : ((Longident_loc.t * Longident_loc.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t
  val pwith_typesubst'const : ((Type_declaration.t * Longident_loc.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t
  val pwith_module'const : ((Longident_loc.t * Longident_loc.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t
  val pwith_type'const : ((Type_declaration.t * Longident_loc.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t

  val pincl_attributes'match : (Attributes.t, 'i, 'o) View.t -> ('a node Include_infos.t, 'i, 'o) View.t

  val pincl_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> ('a node Include_infos.t, 'i, 'o) View.t

  val pincl_mod'match : ('a node, 'i, 'o) View.t -> ('a node Include_infos.t, 'i, 'o) View.t

  val popen_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Open_description.t, 'i, 'o) View.t

  val popen_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Open_description.t, 'i, 'o) View.t

  val popen_override'match : (Override_flag.t, 'i, 'o) View.t -> (Open_description.t, 'i, 'o) View.t

  val popen_lid'match : (Longident_loc.t, 'i, 'o) View.t -> (Open_description.t, 'i, 'o) View.t

  val pmtd_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_type_declaration.t, 'i, 'o) View.t

  val pmtd_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_type_declaration.t, 'i, 'o) View.t

  val pmtd_type'match : (Module_type.t option, 'i, 'o) View.t -> (Module_type_declaration.t, 'i, 'o) View.t

  val pmtd_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Module_type_declaration.t, 'i, 'o) View.t

  val pmd_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_declaration.t, 'i, 'o) View.t

  val pmd_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_declaration.t, 'i, 'o) View.t

  val pmd_type'match : (Module_type.t, 'i, 'o) View.t -> (Module_declaration.t, 'i, 'o) View.t

  val pmd_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Module_declaration.t, 'i, 'o) View.t
  val psig_extension'const : ((Attributes.t * Extension.t), 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_attribute'const : (Attribute.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_class_type'const : (Class_type_declaration.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_class'const : (Class_description.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_include'const : (Include_description.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_open'const : (Open_description.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_modtype'const : (Module_type_declaration.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_recmodule'const : (Module_declaration.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_module'const : (Module_declaration.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_exception'const : (Extension_constructor.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_typext'const : (Type_extension.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_type'const : ((Type_declaration.t list * Rec_flag.t), 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_value'const : (Value_description.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

  val psig_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

  val psig_desc'match : (Signature_item_desc.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val pmty_alias'const : (Longident_loc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_extension'const : (Extension.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_typeof'const : (Module_expr.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_with'const : ((With_constraint.t list * Module_type.t), 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_functor'const : ((Module_type.t * Module_type.t option * string Astlib.Loc.t), 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_signature'const : (Signature.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_ident'const : (Longident_loc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val pmty_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val pmty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val pmty_desc'match : (Module_type_desc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val cfk_concrete'const : ((Expression.t * Override_flag.t), 'i, 'o) View.t -> (Class_field_kind.t, 'i, 'o) View.t
  val cfk_virtual'const : (Core_type.t, 'i, 'o) View.t -> (Class_field_kind.t, 'i, 'o) View.t
  val pcf_extension'const : (Extension.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_attribute'const : (Attribute.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_initializer'const : (Expression.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_constraint'const : ((Core_type.t * Core_type.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_method'const : ((Class_field_kind.t * Private_flag.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_val'const : ((Class_field_kind.t * Mutable_flag.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_inherit'const : ((string Astlib.Loc.t option * Class_expr.t * Override_flag.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcf_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcf_desc'match : (Class_field_desc.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcstr_fields'match : (Class_field.t list, 'i, 'o) View.t -> (Class_structure.t, 'i, 'o) View.t

  val pcstr_self'match : (Pattern.t, 'i, 'o) View.t -> (Class_structure.t, 'i, 'o) View.t
  val pcl_open'const : ((Class_expr.t * Longident_loc.t * Override_flag.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_extension'const : (Extension.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_constraint'const : ((Class_type.t * Class_expr.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_let'const : ((Class_expr.t * Value_binding.t list * Rec_flag.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_apply'const : (((Expression.t * Arg_label.t) list * Class_expr.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_fun'const : ((Class_expr.t * Pattern.t * Expression.t option * Arg_label.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_structure'const : (Class_structure.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_constr'const : ((Core_type.t list * Longident_loc.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcl_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcl_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcl_desc'match : (Class_expr_desc.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pci_attributes'match : (Attributes.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_expr'match : ('a node, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_params'match : ((Variance.t * Core_type.t) list, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_virt'match : (Virtual_flag.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t
  val pctf_extension'const : (Extension.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_attribute'const : (Attribute.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_constraint'const : ((Core_type.t * Core_type.t), 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_method'const : ((Core_type.t * Virtual_flag.t * Private_flag.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_val'const : ((Core_type.t * Virtual_flag.t * Mutable_flag.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_inherit'const : (Class_type.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pctf_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pctf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pctf_desc'match : (Class_type_field_desc.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pcsig_fields'match : (Class_type_field.t list, 'i, 'o) View.t -> (Class_signature.t, 'i, 'o) View.t

  val pcsig_self'match : (Core_type.t, 'i, 'o) View.t -> (Class_signature.t, 'i, 'o) View.t
  val pcty_open'const : ((Class_type.t * Longident_loc.t * Override_flag.t), 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_extension'const : (Extension.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_arrow'const : ((Class_type.t * Core_type.t * Arg_label.t), 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_signature'const : (Class_signature.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_constr'const : ((Core_type.t list * Longident_loc.t), 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcty_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcty_desc'match : (Class_type_desc.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pext_rebind'const : (Longident_loc.t, 'i, 'o) View.t -> (Extension_constructor_kind.t, 'i, 'o) View.t
  val pext_decl'const : ((Core_type.t option * Constructor_arguments.t), 'i, 'o) View.t -> (Extension_constructor_kind.t, 'i, 'o) View.t

  val pext_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_kind'match : (Extension_constructor_kind.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val ptyext_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_private'match : (Private_flag.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_constructors'match : (Extension_constructor.t list, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_params'match : ((Variance.t * Core_type.t) list, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_path'match : (Longident_loc.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t
  val pcstr_record'const : (Label_declaration.t list, 'i, 'o) View.t -> (Constructor_arguments.t, 'i, 'o) View.t
  val pcstr_tuple'const : (Core_type.t list, 'i, 'o) View.t -> (Constructor_arguments.t, 'i, 'o) View.t

  val pcd_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Constructor_declaration.t, 'i, 'o) View.t

  val pcd_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Constructor_declaration.t, 'i, 'o) View.t

  val pcd_res'match : (Core_type.t option, 'i, 'o) View.t -> (Constructor_declaration.t, 'i, 'o) View.t

  val pcd_args'match : (Constructor_arguments.t, 'i, 'o) View.t -> (Constructor_declaration.t, 'i, 'o) View.t

  val pcd_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Constructor_declaration.t, 'i, 'o) View.t

  val pld_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Label_declaration.t, 'i, 'o) View.t

  val pld_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Label_declaration.t, 'i, 'o) View.t

  val pld_type'match : (Core_type.t, 'i, 'o) View.t -> (Label_declaration.t, 'i, 'o) View.t

  val pld_mutable'match : (Mutable_flag.t, 'i, 'o) View.t -> (Label_declaration.t, 'i, 'o) View.t

  val pld_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Label_declaration.t, 'i, 'o) View.t

  val ptype_open'const : (Type_kind.t, 'a, 'a) View.t
  val ptype_record'const : (Label_declaration.t list, 'i, 'o) View.t -> (Type_kind.t, 'i, 'o) View.t
  val ptype_variant'const : (Constructor_declaration.t list, 'i, 'o) View.t -> (Type_kind.t, 'i, 'o) View.t

  val ptype_abstract'const : (Type_kind.t, 'a, 'a) View.t

  val ptype_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val ptype_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val ptype_manifest'match : (Core_type.t option, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val ptype_private'match : (Private_flag.t, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val ptype_kind'match : (Type_kind.t, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val ptype_cstrs'match : ((Astlib.Location.t * Core_type.t * Core_type.t) list, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val ptype_params'match : ((Variance.t * Core_type.t) list, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val ptype_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val pval_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Value_description.t, 'i, 'o) View.t

  val pval_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Value_description.t, 'i, 'o) View.t

  val pval_prim'match : (string list, 'i, 'o) View.t -> (Value_description.t, 'i, 'o) View.t

  val pval_type'match : (Core_type.t, 'i, 'o) View.t -> (Value_description.t, 'i, 'o) View.t

  val pval_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Value_description.t, 'i, 'o) View.t

  val pc_rhs'match : (Expression.t, 'i, 'o) View.t -> (Case.t, 'i, 'o) View.t

  val pc_guard'match : (Expression.t option, 'i, 'o) View.t -> (Case.t, 'i, 'o) View.t

  val pc_lhs'match : (Pattern.t, 'i, 'o) View.t -> (Case.t, 'i, 'o) View.t

  val pexp_unreachable'const : (Expression.t, 'a, 'a) View.t
  val pexp_extension'const : (Extension.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_open'const : ((Expression.t * Longident_loc.t * Override_flag.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_pack'const : (Module_expr.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_newtype'const : ((Expression.t * string Astlib.Loc.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_object'const : (Class_structure.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_poly'const : ((Core_type.t option * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_lazy'const : (Expression.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_assert'const : (Expression.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_letexception'const : ((Expression.t * Extension_constructor.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_letmodule'const : ((Expression.t * Module_expr.t * string Astlib.Loc.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_override'const : ((Expression.t * Label.t Astlib.Loc.t) list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_setinstvar'const : ((Expression.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_new'const : (Longident_loc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_send'const : ((Label.t Astlib.Loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_coerce'const : ((Core_type.t * Core_type.t option * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_constraint'const : ((Core_type.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_for'const : ((Expression.t * Direction_flag.t * Expression.t * Expression.t * Pattern.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_while'const : ((Expression.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_sequence'const : ((Expression.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_ifthenelse'const : ((Expression.t option * Expression.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_array'const : (Expression.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_setfield'const : ((Expression.t * Longident_loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_field'const : ((Longident_loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_record'const : ((Expression.t option * (Expression.t * Longident_loc.t) list), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_variant'const : ((Expression.t option * Label.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_construct'const : ((Expression.t option * Longident_loc.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_tuple'const : (Expression.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_try'const : ((Case.t list * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_match'const : ((Case.t list * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_apply'const : (((Expression.t * Arg_label.t) list * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_fun'const : ((Expression.t * Pattern.t * Expression.t option * Arg_label.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_function'const : (Case.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_let'const : ((Expression.t * Value_binding.t list * Rec_flag.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_constant'const : (Constant.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_ident'const : (Longident_loc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_desc'match : (Expression_desc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val ppat_open'const : ((Pattern.t * Longident_loc.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_extension'const : (Extension.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_exception'const : (Pattern.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_unpack'const : (string Astlib.Loc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_lazy'const : (Pattern.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_type'const : (Longident_loc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_constraint'const : ((Core_type.t * Pattern.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_or'const : ((Pattern.t * Pattern.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_array'const : (Pattern.t list, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_record'const : ((Closed_flag.t * (Pattern.t * Longident_loc.t) list), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_variant'const : ((Pattern.t option * Label.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_construct'const : ((Pattern.t option * Longident_loc.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_tuple'const : (Pattern.t list, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_interval'const : ((Constant.t * Constant.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_constant'const : (Constant.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_alias'const : ((string Astlib.Loc.t * Pattern.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_var'const : (string Astlib.Loc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_any'const : (Pattern.t, 'a, 'a) View.t

  val ppat_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_desc'match : (Pattern_desc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val oinherit'const : (Core_type.t, 'i, 'o) View.t -> (Object_field.t, 'i, 'o) View.t
  val otag'const : ((Core_type.t * Attributes.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Object_field.t, 'i, 'o) View.t
  val rinherit'const : (Core_type.t, 'i, 'o) View.t -> (Row_field.t, 'i, 'o) View.t
  val rtag'const : ((Core_type.t list * bool * Attributes.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Row_field.t, 'i, 'o) View.t
  val ptyp_extension'const : (Extension.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_package'const : (Package_type.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_poly'const : ((Core_type.t * string Astlib.Loc.t list), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_variant'const : ((Label.t list option * Closed_flag.t * Row_field.t list), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_alias'const : ((string * Core_type.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_class'const : ((Core_type.t list * Longident_loc.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_object'const : ((Closed_flag.t * Object_field.t list), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_constr'const : ((Core_type.t list * Longident_loc.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_tuple'const : (Core_type.t list, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_arrow'const : ((Core_type.t * Core_type.t * Arg_label.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_var'const : (string, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_any'const : (Core_type.t, 'a, 'a) View.t

  val ptyp_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_desc'match : (Core_type_desc.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ppat'const : ((Expression.t option * Pattern.t), 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t
  val ptyp'const : (Core_type.t, 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t
  val psig'const : (Signature.t, 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t
  val pstr'const : (Structure.t, 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t
  val pconst_float'const : ((char option * string), 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t
  val pconst_string'const : ((string option * string), 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t
  val pconst_char'const : (char, 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t
  val pconst_integer'const : ((char option * string), 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t

  val invariant'const : (Variance.t, 'a, 'a) View.t

  val contravariant'const : (Variance.t, 'a, 'a) View.t

  val covariant'const : (Variance.t, 'a, 'a) View.t
  val optional'const : (string, 'i, 'o) View.t -> (Arg_label.t, 'i, 'o) View.t
  val labelled'const : (string, 'i, 'o) View.t -> (Arg_label.t, 'i, 'o) View.t

  val nolabel'const : (Arg_label.t, 'a, 'a) View.t

  val open'const : (Closed_flag.t, 'a, 'a) View.t

  val closed'const : (Closed_flag.t, 'a, 'a) View.t

  val fresh'const : (Override_flag.t, 'a, 'a) View.t

  val override'const : (Override_flag.t, 'a, 'a) View.t

  val concrete'const : (Virtual_flag.t, 'a, 'a) View.t

  val virtual'const : (Virtual_flag.t, 'a, 'a) View.t

  val mutable'const : (Mutable_flag.t, 'a, 'a) View.t

  val immutable'const : (Mutable_flag.t, 'a, 'a) View.t

  val public'const : (Private_flag.t, 'a, 'a) View.t

  val private'const : (Private_flag.t, 'a, 'a) View.t

  val downto'const : (Direction_flag.t, 'a, 'a) View.t

  val upto'const : (Direction_flag.t, 'a, 'a) View.t

  val recursive'const : (Rec_flag.t, 'a, 'a) View.t

  val nonrecursive'const : (Rec_flag.t, 'a, 'a) View.t
  val lapply'const : ((Longident.t * Longident.t), 'i, 'o) View.t -> (Longident.t, 'i, 'o) View.t
  val ldot'const : ((string * Longident.t), 'i, 'o) View.t -> (Longident.t, 'i, 'o) View.t
  val lident'const : (string, 'i, 'o) View.t -> (Longident.t, 'i, 'o) View.t
end

module V4_07 : sig
  open Versions
  open V4_07
  include LOC_TYPES
  val lident'const : (string, 'i, 'o) View.t -> (Longident.t, 'i, 'o) View.t
  val ldot'const : ((Longident.t * string), 'i, 'o) View.t -> (Longident.t, 'i, 'o) View.t
  val lapply'const : ((Longident.t * Longident.t), 'i, 'o) View.t -> (Longident.t, 'i, 'o) View.t

  val nonrecursive'const : (Rec_flag.t, 'a, 'a) View.t

  val recursive'const : (Rec_flag.t, 'a, 'a) View.t

  val upto'const : (Direction_flag.t, 'a, 'a) View.t

  val downto'const : (Direction_flag.t, 'a, 'a) View.t

  val private'const : (Private_flag.t, 'a, 'a) View.t

  val public'const : (Private_flag.t, 'a, 'a) View.t

  val immutable'const : (Mutable_flag.t, 'a, 'a) View.t

  val mutable'const : (Mutable_flag.t, 'a, 'a) View.t

  val virtual'const : (Virtual_flag.t, 'a, 'a) View.t

  val concrete'const : (Virtual_flag.t, 'a, 'a) View.t

  val override'const : (Override_flag.t, 'a, 'a) View.t

  val fresh'const : (Override_flag.t, 'a, 'a) View.t

  val closed'const : (Closed_flag.t, 'a, 'a) View.t

  val open'const : (Closed_flag.t, 'a, 'a) View.t

  val nolabel'const : (Arg_label.t, 'a, 'a) View.t
  val labelled'const : (string, 'i, 'o) View.t -> (Arg_label.t, 'i, 'o) View.t
  val optional'const : (string, 'i, 'o) View.t -> (Arg_label.t, 'i, 'o) View.t

  val covariant'const : (Variance.t, 'a, 'a) View.t

  val contravariant'const : (Variance.t, 'a, 'a) View.t

  val invariant'const : (Variance.t, 'a, 'a) View.t
  val pconst_integer'const : ((string * char option), 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t
  val pconst_char'const : (char, 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t
  val pconst_string'const : ((string * string option), 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t
  val pconst_float'const : ((string * char option), 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t
  val pstr'const : (Structure.t, 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t
  val psig'const : (Signature.t, 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t
  val ptyp'const : (Core_type.t, 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t
  val ppat'const : ((Pattern.t * Expression.t option), 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t

  val ptyp_desc'match : (Core_type_desc.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_any'const : (Core_type.t, 'a, 'a) View.t
  val ptyp_var'const : (string, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_arrow'const : ((Arg_label.t * Core_type.t * Core_type.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_tuple'const : (Core_type.t list, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_constr'const : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_object'const : ((Object_field.t list * Closed_flag.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_class'const : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_alias'const : ((Core_type.t * string), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_variant'const : ((Row_field.t list * Closed_flag.t * Label.t list option), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_poly'const : ((string Astlib.Loc.t list * Core_type.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_package'const : (Package_type.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_extension'const : (Extension.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val rtag'const : ((Label.t Astlib.Loc.t * Attributes.t * bool * Core_type.t list), 'i, 'o) View.t -> (Row_field.t, 'i, 'o) View.t
  val rinherit'const : (Core_type.t, 'i, 'o) View.t -> (Row_field.t, 'i, 'o) View.t
  val otag'const : ((Label.t Astlib.Loc.t * Attributes.t * Core_type.t), 'i, 'o) View.t -> (Object_field.t, 'i, 'o) View.t
  val oinherit'const : (Core_type.t, 'i, 'o) View.t -> (Object_field.t, 'i, 'o) View.t

  val ppat_desc'match : (Pattern_desc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_any'const : (Pattern.t, 'a, 'a) View.t
  val ppat_var'const : (string Astlib.Loc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_alias'const : ((Pattern.t * string Astlib.Loc.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_constant'const : (Constant.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_interval'const : ((Constant.t * Constant.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_tuple'const : (Pattern.t list, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_construct'const : ((Longident_loc.t * Pattern.t option), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_variant'const : ((Label.t * Pattern.t option), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_record'const : (((Longident_loc.t * Pattern.t) list * Closed_flag.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_array'const : (Pattern.t list, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_or'const : ((Pattern.t * Pattern.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_constraint'const : ((Pattern.t * Core_type.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_type'const : (Longident_loc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_lazy'const : (Pattern.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_unpack'const : (string Astlib.Loc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_exception'const : (Pattern.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_extension'const : (Extension.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_open'const : ((Longident_loc.t * Pattern.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val pexp_desc'match : (Expression_desc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_ident'const : (Longident_loc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_constant'const : (Constant.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_let'const : ((Rec_flag.t * Value_binding.t list * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_function'const : (Case.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_fun'const : ((Arg_label.t * Expression.t option * Pattern.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_apply'const : ((Expression.t * (Arg_label.t * Expression.t) list), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_match'const : ((Expression.t * Case.t list), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_try'const : ((Expression.t * Case.t list), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_tuple'const : (Expression.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_construct'const : ((Longident_loc.t * Expression.t option), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_variant'const : ((Label.t * Expression.t option), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_record'const : (((Longident_loc.t * Expression.t) list * Expression.t option), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_field'const : ((Expression.t * Longident_loc.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_setfield'const : ((Expression.t * Longident_loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_array'const : (Expression.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_ifthenelse'const : ((Expression.t * Expression.t * Expression.t option), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_sequence'const : ((Expression.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_while'const : ((Expression.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_for'const : ((Pattern.t * Expression.t * Expression.t * Direction_flag.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_constraint'const : ((Expression.t * Core_type.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_coerce'const : ((Expression.t * Core_type.t option * Core_type.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_send'const : ((Expression.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_new'const : (Longident_loc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_setinstvar'const : ((Label.t Astlib.Loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_override'const : ((Label.t Astlib.Loc.t * Expression.t) list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_letmodule'const : ((string Astlib.Loc.t * Module_expr.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_letexception'const : ((Extension_constructor.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_assert'const : (Expression.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_lazy'const : (Expression.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_poly'const : ((Expression.t * Core_type.t option), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_object'const : (Class_structure.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_newtype'const : ((string Astlib.Loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_pack'const : (Module_expr.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_open'const : ((Override_flag.t * Longident_loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_extension'const : (Extension.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_unreachable'const : (Expression.t, 'a, 'a) View.t

  val pc_lhs'match : (Pattern.t, 'i, 'o) View.t -> (Case.t, 'i, 'o) View.t

  val pc_guard'match : (Expression.t option, 'i, 'o) View.t -> (Case.t, 'i, 'o) View.t

  val pc_rhs'match : (Expression.t, 'i, 'o) View.t -> (Case.t, 'i, 'o) View.t

  val pval_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Value_description.t, 'i, 'o) View.t

  val pval_type'match : (Core_type.t, 'i, 'o) View.t -> (Value_description.t, 'i, 'o) View.t

  val pval_prim'match : (string list, 'i, 'o) View.t -> (Value_description.t, 'i, 'o) View.t

  val pval_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Value_description.t, 'i, 'o) View.t

  val pval_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Value_description.t, 'i, 'o) View.t

  val ptype_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val ptype_params'match : ((Core_type.t * Variance.t) list, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val ptype_cstrs'match : ((Core_type.t * Core_type.t * Astlib.Location.t) list, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val ptype_kind'match : (Type_kind.t, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val ptype_private'match : (Private_flag.t, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val ptype_manifest'match : (Core_type.t option, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val ptype_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val ptype_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Type_declaration.t, 'i, 'o) View.t

  val ptype_abstract'const : (Type_kind.t, 'a, 'a) View.t
  val ptype_variant'const : (Constructor_declaration.t list, 'i, 'o) View.t -> (Type_kind.t, 'i, 'o) View.t
  val ptype_record'const : (Label_declaration.t list, 'i, 'o) View.t -> (Type_kind.t, 'i, 'o) View.t

  val ptype_open'const : (Type_kind.t, 'a, 'a) View.t

  val pld_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Label_declaration.t, 'i, 'o) View.t

  val pld_mutable'match : (Mutable_flag.t, 'i, 'o) View.t -> (Label_declaration.t, 'i, 'o) View.t

  val pld_type'match : (Core_type.t, 'i, 'o) View.t -> (Label_declaration.t, 'i, 'o) View.t

  val pld_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Label_declaration.t, 'i, 'o) View.t

  val pld_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Label_declaration.t, 'i, 'o) View.t

  val pcd_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Constructor_declaration.t, 'i, 'o) View.t

  val pcd_args'match : (Constructor_arguments.t, 'i, 'o) View.t -> (Constructor_declaration.t, 'i, 'o) View.t

  val pcd_res'match : (Core_type.t option, 'i, 'o) View.t -> (Constructor_declaration.t, 'i, 'o) View.t

  val pcd_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Constructor_declaration.t, 'i, 'o) View.t

  val pcd_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Constructor_declaration.t, 'i, 'o) View.t
  val pcstr_tuple'const : (Core_type.t list, 'i, 'o) View.t -> (Constructor_arguments.t, 'i, 'o) View.t
  val pcstr_record'const : (Label_declaration.t list, 'i, 'o) View.t -> (Constructor_arguments.t, 'i, 'o) View.t

  val ptyext_path'match : (Longident_loc.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_params'match : ((Core_type.t * Variance.t) list, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_constructors'match : (Extension_constructor.t list, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_private'match : (Private_flag.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val pext_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_kind'match : (Extension_constructor_kind.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t
  val pext_decl'const : ((Constructor_arguments.t * Core_type.t option), 'i, 'o) View.t -> (Extension_constructor_kind.t, 'i, 'o) View.t
  val pext_rebind'const : (Longident_loc.t, 'i, 'o) View.t -> (Extension_constructor_kind.t, 'i, 'o) View.t

  val pcty_desc'match : (Class_type_desc.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcty_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_constr'const : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_signature'const : (Class_signature.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_arrow'const : ((Arg_label.t * Core_type.t * Class_type.t), 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_extension'const : (Extension.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_open'const : ((Override_flag.t * Longident_loc.t * Class_type.t), 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcsig_self'match : (Core_type.t, 'i, 'o) View.t -> (Class_signature.t, 'i, 'o) View.t

  val pcsig_fields'match : (Class_type_field.t list, 'i, 'o) View.t -> (Class_signature.t, 'i, 'o) View.t

  val pctf_desc'match : (Class_type_field_desc.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pctf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pctf_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_inherit'const : (Class_type.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_val'const : ((Label.t Astlib.Loc.t * Mutable_flag.t * Virtual_flag.t * Core_type.t), 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_method'const : ((Label.t Astlib.Loc.t * Private_flag.t * Virtual_flag.t * Core_type.t), 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_constraint'const : ((Core_type.t * Core_type.t), 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_attribute'const : (Attribute.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_extension'const : (Extension.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pci_virt'match : (Virtual_flag.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_params'match : ((Core_type.t * Variance.t) list, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_expr'match : ('a node, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_attributes'match : (Attributes.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pcl_desc'match : (Class_expr_desc.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcl_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcl_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_constr'const : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_structure'const : (Class_structure.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_fun'const : ((Arg_label.t * Expression.t option * Pattern.t * Class_expr.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_apply'const : ((Class_expr.t * (Arg_label.t * Expression.t) list), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_let'const : ((Rec_flag.t * Value_binding.t list * Class_expr.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_constraint'const : ((Class_expr.t * Class_type.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_extension'const : (Extension.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_open'const : ((Override_flag.t * Longident_loc.t * Class_expr.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcstr_self'match : (Pattern.t, 'i, 'o) View.t -> (Class_structure.t, 'i, 'o) View.t

  val pcstr_fields'match : (Class_field.t list, 'i, 'o) View.t -> (Class_structure.t, 'i, 'o) View.t

  val pcf_desc'match : (Class_field_desc.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcf_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_inherit'const : ((Override_flag.t * Class_expr.t * string Astlib.Loc.t option), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_val'const : ((Label.t Astlib.Loc.t * Mutable_flag.t * Class_field_kind.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_method'const : ((Label.t Astlib.Loc.t * Private_flag.t * Class_field_kind.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_constraint'const : ((Core_type.t * Core_type.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_initializer'const : (Expression.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_attribute'const : (Attribute.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_extension'const : (Extension.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val cfk_virtual'const : (Core_type.t, 'i, 'o) View.t -> (Class_field_kind.t, 'i, 'o) View.t
  val cfk_concrete'const : ((Override_flag.t * Expression.t), 'i, 'o) View.t -> (Class_field_kind.t, 'i, 'o) View.t

  val pmty_desc'match : (Module_type_desc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val pmty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val pmty_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_ident'const : (Longident_loc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_signature'const : (Signature.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_functor'const : ((string Astlib.Loc.t * Module_type.t option * Module_type.t), 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_with'const : ((Module_type.t * With_constraint.t list), 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_typeof'const : (Module_expr.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_extension'const : (Extension.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_alias'const : (Longident_loc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val psig_desc'match : (Signature_item_desc.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

  val psig_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_value'const : (Value_description.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_type'const : ((Rec_flag.t * Type_declaration.t list), 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_typext'const : (Type_extension.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_exception'const : (Extension_constructor.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_module'const : (Module_declaration.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_recmodule'const : (Module_declaration.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_modtype'const : (Module_type_declaration.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_open'const : (Open_description.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_include'const : (Include_description.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_class'const : (Class_description.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_class_type'const : (Class_type_declaration.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_attribute'const : (Attribute.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_extension'const : ((Extension.t * Attributes.t), 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

  val pmd_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Module_declaration.t, 'i, 'o) View.t

  val pmd_type'match : (Module_type.t, 'i, 'o) View.t -> (Module_declaration.t, 'i, 'o) View.t

  val pmd_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_declaration.t, 'i, 'o) View.t

  val pmd_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_declaration.t, 'i, 'o) View.t

  val pmtd_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Module_type_declaration.t, 'i, 'o) View.t

  val pmtd_type'match : (Module_type.t option, 'i, 'o) View.t -> (Module_type_declaration.t, 'i, 'o) View.t

  val pmtd_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_type_declaration.t, 'i, 'o) View.t

  val pmtd_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_type_declaration.t, 'i, 'o) View.t

  val popen_lid'match : (Longident_loc.t, 'i, 'o) View.t -> (Open_description.t, 'i, 'o) View.t

  val popen_override'match : (Override_flag.t, 'i, 'o) View.t -> (Open_description.t, 'i, 'o) View.t

  val popen_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Open_description.t, 'i, 'o) View.t

  val popen_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Open_description.t, 'i, 'o) View.t

  val pincl_mod'match : ('a node, 'i, 'o) View.t -> ('a node Include_infos.t, 'i, 'o) View.t

  val pincl_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> ('a node Include_infos.t, 'i, 'o) View.t

  val pincl_attributes'match : (Attributes.t, 'i, 'o) View.t -> ('a node Include_infos.t, 'i, 'o) View.t
  val pwith_type'const : ((Longident_loc.t * Type_declaration.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t
  val pwith_module'const : ((Longident_loc.t * Longident_loc.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t
  val pwith_typesubst'const : ((Longident_loc.t * Type_declaration.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t
  val pwith_modsubst'const : ((Longident_loc.t * Longident_loc.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t

  val pmod_desc'match : (Module_expr_desc.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pmod_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pmod_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_ident'const : (Longident_loc.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_structure'const : (Structure.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_functor'const : ((string Astlib.Loc.t * Module_type.t option * Module_expr.t), 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_apply'const : ((Module_expr.t * Module_expr.t), 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_constraint'const : ((Module_expr.t * Module_type.t), 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_unpack'const : (Expression.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_extension'const : (Extension.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pstr_desc'match : (Structure_item_desc.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

  val pstr_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_eval'const : ((Expression.t * Attributes.t), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_value'const : ((Rec_flag.t * Value_binding.t list), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_primitive'const : (Value_description.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_type'const : ((Rec_flag.t * Type_declaration.t list), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_typext'const : (Type_extension.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_exception'const : (Extension_constructor.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_module'const : (Module_binding.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_recmodule'const : (Module_binding.t list, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_modtype'const : (Module_type_declaration.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_open'const : (Open_description.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_class'const : (Class_declaration.t list, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_class_type'const : (Class_type_declaration.t list, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_include'const : (Include_declaration.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_attribute'const : (Attribute.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_extension'const : ((Extension.t * Attributes.t), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

  val pvb_pat'match : (Pattern.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_expr'match : (Expression.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pmb_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_expr'match : (Module_expr.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t
  val ptop_def'const : (Structure.t, 'i, 'o) View.t -> (Toplevel_phrase.t, 'i, 'o) View.t
  val ptop_dir'const : ((string * Directive_argument.t), 'i, 'o) View.t -> (Toplevel_phrase.t, 'i, 'o) View.t

  val pdir_none'const : (Directive_argument.t, 'a, 'a) View.t
  val pdir_string'const : (string, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t
  val pdir_int'const : ((string * char option), 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t
  val pdir_ident'const : (Longident.t, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t
  val pdir_bool'const : (bool, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t
end
(*$*)
