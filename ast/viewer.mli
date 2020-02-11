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
  val pdir_bool : (bool, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t
  val pdir_ident : (Longident.t, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t
  val pdir_int : ((char option * string), 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t
  val pdir_string : (string, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t

  val pdir_none : (Directive_argument.t, 'a, 'a) View.t
  val ptop_dir : ((Directive_argument.t * string), 'i, 'o) View.t -> (Toplevel_phrase.t, 'i, 'o) View.t
  val ptop_def : (Structure.t, 'i, 'o) View.t -> (Toplevel_phrase.t, 'i, 'o) View.t

  val pmb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_expr'match : (Module_expr.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pvb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_expr'match : (Expression.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_pat'match : (Pattern.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t
  val pstr_extension : ((Attributes.t * Extension.t), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_attribute : (Attribute.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_include : (Include_declaration.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_class_type : (Class_type_declaration.t list, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_class : (Class_declaration.t list, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_open : (Open_description.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_modtype : (Module_type_declaration.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_recmodule : (Module_binding.t list, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_module : (Module_binding.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_exception : (Extension_constructor.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_typext : (Type_extension.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_type : ((Type_declaration.t list * Rec_flag.t), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_primitive : (Value_description.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_value : ((Value_binding.t list * Rec_flag.t), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_eval : ((Attributes.t * Expression.t), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

  val pstr_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

  val pstr_desc'match : (Structure_item_desc.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pmod_extension : (Extension.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_unpack : (Expression.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_constraint : ((Module_type.t * Module_expr.t), 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_apply : ((Module_expr.t * Module_expr.t), 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_functor : ((Module_expr.t * Module_type.t option * string Astlib.Loc.t), 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_structure : (Structure.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_ident : (Longident_loc.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pmod_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pmod_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pmod_desc'match : (Module_expr_desc.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pwith_modsubst : ((Longident_loc.t * Longident_loc.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t
  val pwith_typesubst : ((Type_declaration.t * Longident_loc.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t
  val pwith_module : ((Longident_loc.t * Longident_loc.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t
  val pwith_type : ((Type_declaration.t * Longident_loc.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t

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
  val psig_extension : ((Attributes.t * Extension.t), 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_attribute : (Attribute.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_class_type : (Class_type_declaration.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_class : (Class_description.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_include : (Include_description.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_open : (Open_description.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_modtype : (Module_type_declaration.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_recmodule : (Module_declaration.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_module : (Module_declaration.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_exception : (Extension_constructor.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_typext : (Type_extension.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_type : ((Type_declaration.t list * Rec_flag.t), 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_value : (Value_description.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

  val psig_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

  val psig_desc'match : (Signature_item_desc.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val pmty_alias : (Longident_loc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_extension : (Extension.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_typeof : (Module_expr.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_with : ((With_constraint.t list * Module_type.t), 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_functor : ((Module_type.t * Module_type.t option * string Astlib.Loc.t), 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_signature : (Signature.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_ident : (Longident_loc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val pmty_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val pmty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val pmty_desc'match : (Module_type_desc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val cfk_concrete : ((Expression.t * Override_flag.t), 'i, 'o) View.t -> (Class_field_kind.t, 'i, 'o) View.t
  val cfk_virtual : (Core_type.t, 'i, 'o) View.t -> (Class_field_kind.t, 'i, 'o) View.t
  val pcf_extension : (Extension.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_attribute : (Attribute.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_initializer : (Expression.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_constraint : ((Core_type.t * Core_type.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_method : ((Class_field_kind.t * Private_flag.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_val : ((Class_field_kind.t * Mutable_flag.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_inherit : ((string Astlib.Loc.t option * Class_expr.t * Override_flag.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcf_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcf_desc'match : (Class_field_desc.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcstr_fields'match : (Class_field.t list, 'i, 'o) View.t -> (Class_structure.t, 'i, 'o) View.t

  val pcstr_self'match : (Pattern.t, 'i, 'o) View.t -> (Class_structure.t, 'i, 'o) View.t
  val pcl_open : ((Class_expr.t * Longident_loc.t * Override_flag.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_extension : (Extension.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_constraint : ((Class_type.t * Class_expr.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_let : ((Class_expr.t * Value_binding.t list * Rec_flag.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_apply : (((Expression.t * Arg_label.t) list * Class_expr.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_fun : ((Class_expr.t * Pattern.t * Expression.t option * Arg_label.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_structure : (Class_structure.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_constr : ((Core_type.t list * Longident_loc.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcl_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcl_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcl_desc'match : (Class_expr_desc.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pci_attributes'match : (Attributes.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_expr'match : ('a node, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_params'match : ((Variance.t * Core_type.t) list, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_virt'match : (Virtual_flag.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t
  val pctf_extension : (Extension.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_attribute : (Attribute.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_constraint : ((Core_type.t * Core_type.t), 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_method : ((Core_type.t * Virtual_flag.t * Private_flag.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_val : ((Core_type.t * Virtual_flag.t * Mutable_flag.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_inherit : (Class_type.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pctf_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pctf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pctf_desc'match : (Class_type_field_desc.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pcsig_fields'match : (Class_type_field.t list, 'i, 'o) View.t -> (Class_signature.t, 'i, 'o) View.t

  val pcsig_self'match : (Core_type.t, 'i, 'o) View.t -> (Class_signature.t, 'i, 'o) View.t
  val pcty_open : ((Class_type.t * Longident_loc.t * Override_flag.t), 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_extension : (Extension.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_arrow : ((Class_type.t * Core_type.t * Arg_label.t), 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_signature : (Class_signature.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_constr : ((Core_type.t list * Longident_loc.t), 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcty_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcty_desc'match : (Class_type_desc.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pext_rebind : (Longident_loc.t, 'i, 'o) View.t -> (Extension_constructor_kind.t, 'i, 'o) View.t
  val pext_decl : ((Core_type.t option * Constructor_arguments.t), 'i, 'o) View.t -> (Extension_constructor_kind.t, 'i, 'o) View.t

  val pext_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_kind'match : (Extension_constructor_kind.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val ptyext_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_private'match : (Private_flag.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_constructors'match : (Extension_constructor.t list, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_params'match : ((Variance.t * Core_type.t) list, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_path'match : (Longident_loc.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t
  val pcstr_record : (Label_declaration.t list, 'i, 'o) View.t -> (Constructor_arguments.t, 'i, 'o) View.t
  val pcstr_tuple : (Core_type.t list, 'i, 'o) View.t -> (Constructor_arguments.t, 'i, 'o) View.t

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

  val ptype_open : (Type_kind.t, 'a, 'a) View.t
  val ptype_record : (Label_declaration.t list, 'i, 'o) View.t -> (Type_kind.t, 'i, 'o) View.t
  val ptype_variant : (Constructor_declaration.t list, 'i, 'o) View.t -> (Type_kind.t, 'i, 'o) View.t

  val ptype_abstract : (Type_kind.t, 'a, 'a) View.t

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

  val pexp_unreachable : (Expression.t, 'a, 'a) View.t
  val pexp_extension : (Extension.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_open : ((Expression.t * Longident_loc.t * Override_flag.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_pack : (Module_expr.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_newtype : ((Expression.t * string Astlib.Loc.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_object : (Class_structure.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_poly : ((Core_type.t option * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_lazy : (Expression.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_assert : (Expression.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_letexception : ((Expression.t * Extension_constructor.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_letmodule : ((Expression.t * Module_expr.t * string Astlib.Loc.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_override : ((Expression.t * Label.t Astlib.Loc.t) list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_setinstvar : ((Expression.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_new : (Longident_loc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_send : ((Label.t Astlib.Loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_coerce : ((Core_type.t * Core_type.t option * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_constraint : ((Core_type.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_for : ((Expression.t * Direction_flag.t * Expression.t * Expression.t * Pattern.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_while : ((Expression.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_sequence : ((Expression.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_ifthenelse : ((Expression.t option * Expression.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_array : (Expression.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_setfield : ((Expression.t * Longident_loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_field : ((Longident_loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_record : ((Expression.t option * (Expression.t * Longident_loc.t) list), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_variant : ((Expression.t option * Label.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_construct : ((Expression.t option * Longident_loc.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_tuple : (Expression.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_try : ((Case.t list * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_match : ((Case.t list * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_apply : (((Expression.t * Arg_label.t) list * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_fun : ((Expression.t * Pattern.t * Expression.t option * Arg_label.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_function : (Case.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_let : ((Expression.t * Value_binding.t list * Rec_flag.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_constant : (Constant.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_ident : (Longident_loc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_desc'match : (Expression_desc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val ppat_open : ((Pattern.t * Longident_loc.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_extension : (Extension.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_exception : (Pattern.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_unpack : (string Astlib.Loc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_lazy : (Pattern.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_type : (Longident_loc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_constraint : ((Core_type.t * Pattern.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_or : ((Pattern.t * Pattern.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_array : (Pattern.t list, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_record : ((Closed_flag.t * (Pattern.t * Longident_loc.t) list), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_variant : ((Pattern.t option * Label.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_construct : ((Pattern.t option * Longident_loc.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_tuple : (Pattern.t list, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_interval : ((Constant.t * Constant.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_constant : (Constant.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_alias : ((string Astlib.Loc.t * Pattern.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_var : (string Astlib.Loc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_any : (Pattern.t, 'a, 'a) View.t

  val ppat_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_desc'match : (Pattern_desc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val oinherit : (Core_type.t, 'i, 'o) View.t -> (Object_field.t, 'i, 'o) View.t
  val otag : ((Core_type.t * Attributes.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Object_field.t, 'i, 'o) View.t
  val rinherit : (Core_type.t, 'i, 'o) View.t -> (Row_field.t, 'i, 'o) View.t
  val rtag : ((Core_type.t list * bool * Attributes.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Row_field.t, 'i, 'o) View.t
  val ptyp_extension : (Extension.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_package : (Package_type.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_poly : ((Core_type.t * string Astlib.Loc.t list), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_variant : ((Label.t list option * Closed_flag.t * Row_field.t list), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_alias : ((string * Core_type.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_class : ((Core_type.t list * Longident_loc.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_object : ((Closed_flag.t * Object_field.t list), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_constr : ((Core_type.t list * Longident_loc.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_tuple : (Core_type.t list, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_arrow : ((Core_type.t * Core_type.t * Arg_label.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_var : (string, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_any : (Core_type.t, 'a, 'a) View.t

  val ptyp_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_desc'match : (Core_type_desc.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ppat : ((Expression.t option * Pattern.t), 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t
  val ptyp : (Core_type.t, 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t
  val psig : (Signature.t, 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t
  val pstr : (Structure.t, 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t
  val pconst_float : ((char option * string), 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t
  val pconst_string : ((string option * string), 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t
  val pconst_char : (char, 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t
  val pconst_integer : ((char option * string), 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t

  val invariant : (Variance.t, 'a, 'a) View.t

  val contravariant : (Variance.t, 'a, 'a) View.t

  val covariant : (Variance.t, 'a, 'a) View.t
  val optional : (string, 'i, 'o) View.t -> (Arg_label.t, 'i, 'o) View.t
  val labelled : (string, 'i, 'o) View.t -> (Arg_label.t, 'i, 'o) View.t

  val nolabel : (Arg_label.t, 'a, 'a) View.t

  val open_ : (Closed_flag.t, 'a, 'a) View.t

  val closed : (Closed_flag.t, 'a, 'a) View.t

  val fresh : (Override_flag.t, 'a, 'a) View.t

  val override : (Override_flag.t, 'a, 'a) View.t

  val concrete : (Virtual_flag.t, 'a, 'a) View.t

  val virtual_ : (Virtual_flag.t, 'a, 'a) View.t

  val mutable_ : (Mutable_flag.t, 'a, 'a) View.t

  val immutable : (Mutable_flag.t, 'a, 'a) View.t

  val public : (Private_flag.t, 'a, 'a) View.t

  val private_ : (Private_flag.t, 'a, 'a) View.t

  val downto_ : (Direction_flag.t, 'a, 'a) View.t

  val upto : (Direction_flag.t, 'a, 'a) View.t

  val recursive : (Rec_flag.t, 'a, 'a) View.t

  val nonrecursive : (Rec_flag.t, 'a, 'a) View.t
  val lapply : ((Longident.t * Longident.t), 'i, 'o) View.t -> (Longident.t, 'i, 'o) View.t
  val ldot : ((string * Longident.t), 'i, 'o) View.t -> (Longident.t, 'i, 'o) View.t
  val lident : (string, 'i, 'o) View.t -> (Longident.t, 'i, 'o) View.t
end

module V4_07 : sig
  open Versions
  open V4_07
  include LOC_TYPES
  val lident : (string, 'i, 'o) View.t -> (Longident.t, 'i, 'o) View.t
  val ldot : ((Longident.t * string), 'i, 'o) View.t -> (Longident.t, 'i, 'o) View.t
  val lapply : ((Longident.t * Longident.t), 'i, 'o) View.t -> (Longident.t, 'i, 'o) View.t

  val nonrecursive : (Rec_flag.t, 'a, 'a) View.t

  val recursive : (Rec_flag.t, 'a, 'a) View.t

  val upto : (Direction_flag.t, 'a, 'a) View.t

  val downto_ : (Direction_flag.t, 'a, 'a) View.t

  val private_ : (Private_flag.t, 'a, 'a) View.t

  val public : (Private_flag.t, 'a, 'a) View.t

  val immutable : (Mutable_flag.t, 'a, 'a) View.t

  val mutable_ : (Mutable_flag.t, 'a, 'a) View.t

  val virtual_ : (Virtual_flag.t, 'a, 'a) View.t

  val concrete : (Virtual_flag.t, 'a, 'a) View.t

  val override : (Override_flag.t, 'a, 'a) View.t

  val fresh : (Override_flag.t, 'a, 'a) View.t

  val closed : (Closed_flag.t, 'a, 'a) View.t

  val open_ : (Closed_flag.t, 'a, 'a) View.t

  val nolabel : (Arg_label.t, 'a, 'a) View.t
  val labelled : (string, 'i, 'o) View.t -> (Arg_label.t, 'i, 'o) View.t
  val optional : (string, 'i, 'o) View.t -> (Arg_label.t, 'i, 'o) View.t

  val covariant : (Variance.t, 'a, 'a) View.t

  val contravariant : (Variance.t, 'a, 'a) View.t

  val invariant : (Variance.t, 'a, 'a) View.t
  val pconst_integer : ((string * char option), 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t
  val pconst_char : (char, 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t
  val pconst_string : ((string * string option), 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t
  val pconst_float : ((string * char option), 'i, 'o) View.t -> (Constant.t, 'i, 'o) View.t
  val pstr : (Structure.t, 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t
  val psig : (Signature.t, 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t
  val ptyp : (Core_type.t, 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t
  val ppat : ((Pattern.t * Expression.t option), 'i, 'o) View.t -> (Payload.t, 'i, 'o) View.t

  val ptyp_desc'match : (Core_type_desc.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_any : (Core_type.t, 'a, 'a) View.t
  val ptyp_var : (string, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_arrow : ((Arg_label.t * Core_type.t * Core_type.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_tuple : (Core_type.t list, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_constr : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_object : ((Object_field.t list * Closed_flag.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_class : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_alias : ((Core_type.t * string), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_variant : ((Row_field.t list * Closed_flag.t * Label.t list option), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_poly : ((string Astlib.Loc.t list * Core_type.t), 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_package : (Package_type.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val ptyp_extension : (Extension.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t
  val rtag : ((Label.t Astlib.Loc.t * Attributes.t * bool * Core_type.t list), 'i, 'o) View.t -> (Row_field.t, 'i, 'o) View.t
  val rinherit : (Core_type.t, 'i, 'o) View.t -> (Row_field.t, 'i, 'o) View.t
  val otag : ((Label.t Astlib.Loc.t * Attributes.t * Core_type.t), 'i, 'o) View.t -> (Object_field.t, 'i, 'o) View.t
  val oinherit : (Core_type.t, 'i, 'o) View.t -> (Object_field.t, 'i, 'o) View.t

  val ppat_desc'match : (Pattern_desc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_any : (Pattern.t, 'a, 'a) View.t
  val ppat_var : (string Astlib.Loc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_alias : ((Pattern.t * string Astlib.Loc.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_constant : (Constant.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_interval : ((Constant.t * Constant.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_tuple : (Pattern.t list, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_construct : ((Longident_loc.t * Pattern.t option), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_variant : ((Label.t * Pattern.t option), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_record : (((Longident_loc.t * Pattern.t) list * Closed_flag.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_array : (Pattern.t list, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_or : ((Pattern.t * Pattern.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_constraint : ((Pattern.t * Core_type.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_type : (Longident_loc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_lazy : (Pattern.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_unpack : (string Astlib.Loc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_exception : (Pattern.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_extension : (Extension.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t
  val ppat_open : ((Longident_loc.t * Pattern.t), 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val pexp_desc'match : (Expression_desc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_ident : (Longident_loc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_constant : (Constant.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_let : ((Rec_flag.t * Value_binding.t list * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_function : (Case.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_fun : ((Arg_label.t * Expression.t option * Pattern.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_apply : ((Expression.t * (Arg_label.t * Expression.t) list), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_match : ((Expression.t * Case.t list), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_try : ((Expression.t * Case.t list), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_tuple : (Expression.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_construct : ((Longident_loc.t * Expression.t option), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_variant : ((Label.t * Expression.t option), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_record : (((Longident_loc.t * Expression.t) list * Expression.t option), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_field : ((Expression.t * Longident_loc.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_setfield : ((Expression.t * Longident_loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_array : (Expression.t list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_ifthenelse : ((Expression.t * Expression.t * Expression.t option), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_sequence : ((Expression.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_while : ((Expression.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_for : ((Pattern.t * Expression.t * Expression.t * Direction_flag.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_constraint : ((Expression.t * Core_type.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_coerce : ((Expression.t * Core_type.t option * Core_type.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_send : ((Expression.t * Label.t Astlib.Loc.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_new : (Longident_loc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_setinstvar : ((Label.t Astlib.Loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_override : ((Label.t Astlib.Loc.t * Expression.t) list, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_letmodule : ((string Astlib.Loc.t * Module_expr.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_letexception : ((Extension_constructor.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_assert : (Expression.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_lazy : (Expression.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_poly : ((Expression.t * Core_type.t option), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_object : (Class_structure.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_newtype : ((string Astlib.Loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_pack : (Module_expr.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_open : ((Override_flag.t * Longident_loc.t * Expression.t), 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t
  val pexp_extension : (Extension.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_unreachable : (Expression.t, 'a, 'a) View.t

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

  val ptype_abstract : (Type_kind.t, 'a, 'a) View.t
  val ptype_variant : (Constructor_declaration.t list, 'i, 'o) View.t -> (Type_kind.t, 'i, 'o) View.t
  val ptype_record : (Label_declaration.t list, 'i, 'o) View.t -> (Type_kind.t, 'i, 'o) View.t

  val ptype_open : (Type_kind.t, 'a, 'a) View.t

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
  val pcstr_tuple : (Core_type.t list, 'i, 'o) View.t -> (Constructor_arguments.t, 'i, 'o) View.t
  val pcstr_record : (Label_declaration.t list, 'i, 'o) View.t -> (Constructor_arguments.t, 'i, 'o) View.t

  val ptyext_path'match : (Longident_loc.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_params'match : ((Core_type.t * Variance.t) list, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_constructors'match : (Extension_constructor.t list, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_private'match : (Private_flag.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val pext_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_kind'match : (Extension_constructor_kind.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t
  val pext_decl : ((Constructor_arguments.t * Core_type.t option), 'i, 'o) View.t -> (Extension_constructor_kind.t, 'i, 'o) View.t
  val pext_rebind : (Longident_loc.t, 'i, 'o) View.t -> (Extension_constructor_kind.t, 'i, 'o) View.t

  val pcty_desc'match : (Class_type_desc.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcty_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_constr : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_signature : (Class_signature.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_arrow : ((Arg_label.t * Core_type.t * Class_type.t), 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_extension : (Extension.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t
  val pcty_open : ((Override_flag.t * Longident_loc.t * Class_type.t), 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcsig_self'match : (Core_type.t, 'i, 'o) View.t -> (Class_signature.t, 'i, 'o) View.t

  val pcsig_fields'match : (Class_type_field.t list, 'i, 'o) View.t -> (Class_signature.t, 'i, 'o) View.t

  val pctf_desc'match : (Class_type_field_desc.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pctf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pctf_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_inherit : (Class_type.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_val : ((Label.t Astlib.Loc.t * Mutable_flag.t * Virtual_flag.t * Core_type.t), 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_method : ((Label.t Astlib.Loc.t * Private_flag.t * Virtual_flag.t * Core_type.t), 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_constraint : ((Core_type.t * Core_type.t), 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_attribute : (Attribute.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t
  val pctf_extension : (Extension.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pci_virt'match : (Virtual_flag.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_params'match : ((Core_type.t * Variance.t) list, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_expr'match : ('a node, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pci_attributes'match : (Attributes.t, 'i, 'o) View.t -> ('a node Class_infos.t, 'i, 'o) View.t

  val pcl_desc'match : (Class_expr_desc.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcl_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcl_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_constr : ((Longident_loc.t * Core_type.t list), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_structure : (Class_structure.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_fun : ((Arg_label.t * Expression.t option * Pattern.t * Class_expr.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_apply : ((Class_expr.t * (Arg_label.t * Expression.t) list), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_let : ((Rec_flag.t * Value_binding.t list * Class_expr.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_constraint : ((Class_expr.t * Class_type.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_extension : (Extension.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t
  val pcl_open : ((Override_flag.t * Longident_loc.t * Class_expr.t), 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcstr_self'match : (Pattern.t, 'i, 'o) View.t -> (Class_structure.t, 'i, 'o) View.t

  val pcstr_fields'match : (Class_field.t list, 'i, 'o) View.t -> (Class_structure.t, 'i, 'o) View.t

  val pcf_desc'match : (Class_field_desc.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcf_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_inherit : ((Override_flag.t * Class_expr.t * string Astlib.Loc.t option), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_val : ((Label.t Astlib.Loc.t * Mutable_flag.t * Class_field_kind.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_method : ((Label.t Astlib.Loc.t * Private_flag.t * Class_field_kind.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_constraint : ((Core_type.t * Core_type.t), 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_initializer : (Expression.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_attribute : (Attribute.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val pcf_extension : (Extension.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t
  val cfk_virtual : (Core_type.t, 'i, 'o) View.t -> (Class_field_kind.t, 'i, 'o) View.t
  val cfk_concrete : ((Override_flag.t * Expression.t), 'i, 'o) View.t -> (Class_field_kind.t, 'i, 'o) View.t

  val pmty_desc'match : (Module_type_desc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val pmty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val pmty_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_ident : (Longident_loc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_signature : (Signature.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_functor : ((string Astlib.Loc.t * Module_type.t option * Module_type.t), 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_with : ((Module_type.t * With_constraint.t list), 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_typeof : (Module_expr.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_extension : (Extension.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t
  val pmty_alias : (Longident_loc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val psig_desc'match : (Signature_item_desc.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

  val psig_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_value : (Value_description.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_type : ((Rec_flag.t * Type_declaration.t list), 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_typext : (Type_extension.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_exception : (Extension_constructor.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_module : (Module_declaration.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_recmodule : (Module_declaration.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_modtype : (Module_type_declaration.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_open : (Open_description.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_include : (Include_description.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_class : (Class_description.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_class_type : (Class_type_declaration.t list, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_attribute : (Attribute.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t
  val psig_extension : ((Extension.t * Attributes.t), 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

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
  val pwith_type : ((Longident_loc.t * Type_declaration.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t
  val pwith_module : ((Longident_loc.t * Longident_loc.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t
  val pwith_typesubst : ((Longident_loc.t * Type_declaration.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t
  val pwith_modsubst : ((Longident_loc.t * Longident_loc.t), 'i, 'o) View.t -> (With_constraint.t, 'i, 'o) View.t

  val pmod_desc'match : (Module_expr_desc.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pmod_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pmod_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_ident : (Longident_loc.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_structure : (Structure.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_functor : ((string Astlib.Loc.t * Module_type.t option * Module_expr.t), 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_apply : ((Module_expr.t * Module_expr.t), 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_constraint : ((Module_expr.t * Module_type.t), 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_unpack : (Expression.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t
  val pmod_extension : (Extension.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pstr_desc'match : (Structure_item_desc.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

  val pstr_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_eval : ((Expression.t * Attributes.t), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_value : ((Rec_flag.t * Value_binding.t list), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_primitive : (Value_description.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_type : ((Rec_flag.t * Type_declaration.t list), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_typext : (Type_extension.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_exception : (Extension_constructor.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_module : (Module_binding.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_recmodule : (Module_binding.t list, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_modtype : (Module_type_declaration.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_open : (Open_description.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_class : (Class_declaration.t list, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_class_type : (Class_type_declaration.t list, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_include : (Include_declaration.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_attribute : (Attribute.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t
  val pstr_extension : ((Extension.t * Attributes.t), 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

  val pvb_pat'match : (Pattern.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_expr'match : (Expression.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pmb_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_expr'match : (Module_expr.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t
  val ptop_def : (Structure.t, 'i, 'o) View.t -> (Toplevel_phrase.t, 'i, 'o) View.t
  val ptop_dir : ((string * Directive_argument.t), 'i, 'o) View.t -> (Toplevel_phrase.t, 'i, 'o) View.t

  val pdir_none : (Directive_argument.t, 'a, 'a) View.t
  val pdir_string : (string, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t
  val pdir_int : ((string * char option), 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t
  val pdir_ident : (Longident.t, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t
  val pdir_bool : (bool, 'i, 'o) View.t -> (Directive_argument.t, 'i, 'o) View.t
end
(*$*)
