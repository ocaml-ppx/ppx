open Viewlib

(*$ Ppx_ast_cinaps.print_viewer_mli () *)
module V4_07 : sig
  open Versions.V4_07

  val ptyp_desc'match : (Core_type_desc.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ppat_desc'match : (Pattern_desc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val pexp_desc'match : (Expression_desc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

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

  val ptyext_path'match : (Longident_loc.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_params'match : ((Core_type.t * Variance.t) list, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_constructors'match : (Extension_constructor.t list, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_private'match : (Private_flag.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val pext_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_kind'match : (Extension_constructor_kind.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pcty_desc'match : (Class_type_desc.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcty_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcsig_self'match : (Core_type.t, 'i, 'o) View.t -> (Class_signature.t, 'i, 'o) View.t

  val pcsig_fields'match : (Class_type_field.t list, 'i, 'o) View.t -> (Class_signature.t, 'i, 'o) View.t

  val pctf_desc'match : (Class_type_field_desc.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pctf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pctf_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pcl_desc'match : (Class_expr_desc.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcl_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcl_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcstr_self'match : (Pattern.t, 'i, 'o) View.t -> (Class_structure.t, 'i, 'o) View.t

  val pcstr_fields'match : (Class_field.t list, 'i, 'o) View.t -> (Class_structure.t, 'i, 'o) View.t

  val pcf_desc'match : (Class_field_desc.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcf_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pmty_desc'match : (Module_type_desc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val pmty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val pmty_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val psig_desc'match : (Signature_item_desc.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

  val psig_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

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

  val pmod_desc'match : (Module_expr_desc.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pmod_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pmod_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pstr_desc'match : (Structure_item_desc.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

  val pstr_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

  val pvb_pat'match : (Pattern.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_expr'match : (Expression.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pmb_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_expr'match : (Module_expr.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t
end

module V4_06 : sig
  open Versions.V4_06

  val ptyp_desc'match : (Core_type_desc.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ptyp_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Core_type.t, 'i, 'o) View.t

  val ppat_desc'match : (Pattern_desc.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val ppat_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Pattern.t, 'i, 'o) View.t

  val pexp_desc'match : (Expression_desc.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

  val pexp_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Expression.t, 'i, 'o) View.t

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

  val ptyext_path'match : (Longident_loc.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_params'match : ((Core_type.t * Variance.t) list, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_constructors'match : (Extension_constructor.t list, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_private'match : (Private_flag.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val ptyext_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Type_extension.t, 'i, 'o) View.t

  val pext_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_kind'match : (Extension_constructor_kind.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pext_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Extension_constructor.t, 'i, 'o) View.t

  val pcty_desc'match : (Class_type_desc.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcty_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_type.t, 'i, 'o) View.t

  val pcsig_self'match : (Core_type.t, 'i, 'o) View.t -> (Class_signature.t, 'i, 'o) View.t

  val pcsig_fields'match : (Class_type_field.t list, 'i, 'o) View.t -> (Class_signature.t, 'i, 'o) View.t

  val pctf_desc'match : (Class_type_field_desc.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pctf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pctf_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_type_field.t, 'i, 'o) View.t

  val pcl_desc'match : (Class_expr_desc.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcl_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcl_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_expr.t, 'i, 'o) View.t

  val pcstr_self'match : (Pattern.t, 'i, 'o) View.t -> (Class_structure.t, 'i, 'o) View.t

  val pcstr_fields'match : (Class_field.t list, 'i, 'o) View.t -> (Class_structure.t, 'i, 'o) View.t

  val pcf_desc'match : (Class_field_desc.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcf_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pcf_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Class_field.t, 'i, 'o) View.t

  val pmty_desc'match : (Module_type_desc.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val pmty_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val pmty_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_type.t, 'i, 'o) View.t

  val psig_desc'match : (Signature_item_desc.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

  val psig_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Signature_item.t, 'i, 'o) View.t

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

  val pmod_desc'match : (Module_expr_desc.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pmod_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pmod_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_expr.t, 'i, 'o) View.t

  val pstr_desc'match : (Structure_item_desc.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

  val pstr_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Structure_item.t, 'i, 'o) View.t

  val pvb_pat'match : (Pattern.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_expr'match : (Expression.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pvb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Value_binding.t, 'i, 'o) View.t

  val pmb_name'match : (string Astlib.Loc.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_expr'match : (Module_expr.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_attributes'match : (Attributes.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t

  val pmb_loc'match : (Astlib.Location.t, 'i, 'o) View.t -> (Module_binding.t, 'i, 'o) View.t
end
(*$*)
