(*$ Ppx_ast_cinaps.print_builder_ml (Astlib.Version.of_string "v4_08") *)
open Versions.V4_08
let ptyp_any ~loc =
Core_type.create ~ptyp_desc:(Core_type_desc.ptyp_any) ~ptyp_loc:loc ~ptyp_attributes:(Attributes.of_concrete [])
let ptyp_var ~loc a1 =
Core_type.create ~ptyp_desc:(Core_type_desc.ptyp_var a1) ~ptyp_loc:loc ~ptyp_attributes:(Attributes.of_concrete [])
let ptyp_arrow ~loc a1 a2 a3 =
Core_type.create ~ptyp_desc:(Core_type_desc.ptyp_arrow a1 a2 a3) ~ptyp_loc:loc ~ptyp_attributes:(Attributes.of_concrete [])
let ptyp_tuple ~loc a1 =
Core_type.create ~ptyp_desc:(Core_type_desc.ptyp_tuple a1) ~ptyp_loc:loc ~ptyp_attributes:(Attributes.of_concrete [])
let ptyp_constr ~loc a1 a2 =
Core_type.create ~ptyp_desc:(Core_type_desc.ptyp_constr a1 a2) ~ptyp_loc:loc ~ptyp_attributes:(Attributes.of_concrete [])
let ptyp_object ~loc a1 a2 =
Core_type.create ~ptyp_desc:(Core_type_desc.ptyp_object a1 a2) ~ptyp_loc:loc ~ptyp_attributes:(Attributes.of_concrete [])
let ptyp_class ~loc a1 a2 =
Core_type.create ~ptyp_desc:(Core_type_desc.ptyp_class a1 a2) ~ptyp_loc:loc ~ptyp_attributes:(Attributes.of_concrete [])
let ptyp_alias ~loc a1 a2 =
Core_type.create ~ptyp_desc:(Core_type_desc.ptyp_alias a1 a2) ~ptyp_loc:loc ~ptyp_attributes:(Attributes.of_concrete [])
let ptyp_variant ~loc a1 a2 a3 =
Core_type.create ~ptyp_desc:(Core_type_desc.ptyp_variant a1 a2 a3) ~ptyp_loc:loc ~ptyp_attributes:(Attributes.of_concrete [])
let ptyp_poly ~loc a1 a2 =
Core_type.create ~ptyp_desc:(Core_type_desc.ptyp_poly a1 a2) ~ptyp_loc:loc ~ptyp_attributes:(Attributes.of_concrete [])
let ptyp_package ~loc a1 =
Core_type.create ~ptyp_desc:(Core_type_desc.ptyp_package a1) ~ptyp_loc:loc ~ptyp_attributes:(Attributes.of_concrete [])
let ptyp_extension ~loc a1 =
Core_type.create ~ptyp_desc:(Core_type_desc.ptyp_extension a1) ~ptyp_loc:loc ~ptyp_attributes:(Attributes.of_concrete [])
let ppat_any ~loc =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_any) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_var ~loc a1 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_var a1) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_alias ~loc a1 a2 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_alias a1 a2) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_constant ~loc a1 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_constant a1) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_interval ~loc a1 a2 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_interval a1 a2) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_tuple ~loc a1 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_tuple a1) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_construct ~loc a1 a2 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_construct a1 a2) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_variant ~loc a1 a2 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_variant a1 a2) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_record ~loc a1 a2 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_record a1 a2) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_array ~loc a1 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_array a1) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_or ~loc a1 a2 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_or a1 a2) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_constraint ~loc a1 a2 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_constraint a1 a2) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_type ~loc a1 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_type a1) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_lazy ~loc a1 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_lazy a1) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_unpack ~loc a1 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_unpack a1) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_exception ~loc a1 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_exception a1) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_extension ~loc a1 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_extension a1) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let ppat_open ~loc a1 a2 =
Pattern.create ~ppat_desc:(Pattern_desc.ppat_open a1 a2) ~ppat_loc:loc ~ppat_attributes:(Attributes.of_concrete [])
let pexp_ident ~loc a1 =
Expression.create ~pexp_desc:(Expression_desc.pexp_ident a1) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_constant ~loc a1 =
Expression.create ~pexp_desc:(Expression_desc.pexp_constant a1) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_let ~loc a1 a2 a3 =
Expression.create ~pexp_desc:(Expression_desc.pexp_let a1 a2 a3) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_function ~loc a1 =
Expression.create ~pexp_desc:(Expression_desc.pexp_function a1) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_fun ~loc a1 a2 a3 a4 =
Expression.create ~pexp_desc:(Expression_desc.pexp_fun a1 a2 a3 a4) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_apply ~loc a1 a2 =
Expression.create ~pexp_desc:(Expression_desc.pexp_apply a1 a2) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_match ~loc a1 a2 =
Expression.create ~pexp_desc:(Expression_desc.pexp_match a1 a2) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_try ~loc a1 a2 =
Expression.create ~pexp_desc:(Expression_desc.pexp_try a1 a2) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_tuple ~loc a1 =
Expression.create ~pexp_desc:(Expression_desc.pexp_tuple a1) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_construct ~loc a1 a2 =
Expression.create ~pexp_desc:(Expression_desc.pexp_construct a1 a2) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_variant ~loc a1 a2 =
Expression.create ~pexp_desc:(Expression_desc.pexp_variant a1 a2) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_record ~loc a1 a2 =
Expression.create ~pexp_desc:(Expression_desc.pexp_record a1 a2) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_field ~loc a1 a2 =
Expression.create ~pexp_desc:(Expression_desc.pexp_field a1 a2) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_setfield ~loc a1 a2 a3 =
Expression.create ~pexp_desc:(Expression_desc.pexp_setfield a1 a2 a3) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_array ~loc a1 =
Expression.create ~pexp_desc:(Expression_desc.pexp_array a1) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_ifthenelse ~loc a1 a2 a3 =
Expression.create ~pexp_desc:(Expression_desc.pexp_ifthenelse a1 a2 a3) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_sequence ~loc a1 a2 =
Expression.create ~pexp_desc:(Expression_desc.pexp_sequence a1 a2) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_while ~loc a1 a2 =
Expression.create ~pexp_desc:(Expression_desc.pexp_while a1 a2) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_for ~loc a1 a2 a3 a4 a5 =
Expression.create ~pexp_desc:(Expression_desc.pexp_for a1 a2 a3 a4 a5) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_constraint ~loc a1 a2 =
Expression.create ~pexp_desc:(Expression_desc.pexp_constraint a1 a2) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_coerce ~loc a1 a2 a3 =
Expression.create ~pexp_desc:(Expression_desc.pexp_coerce a1 a2 a3) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_send ~loc a1 a2 =
Expression.create ~pexp_desc:(Expression_desc.pexp_send a1 a2) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_new ~loc a1 =
Expression.create ~pexp_desc:(Expression_desc.pexp_new a1) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_setinstvar ~loc a1 a2 =
Expression.create ~pexp_desc:(Expression_desc.pexp_setinstvar a1 a2) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_override ~loc a1 =
Expression.create ~pexp_desc:(Expression_desc.pexp_override a1) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_letmodule ~loc a1 a2 a3 =
Expression.create ~pexp_desc:(Expression_desc.pexp_letmodule a1 a2 a3) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_letexception ~loc a1 a2 =
Expression.create ~pexp_desc:(Expression_desc.pexp_letexception a1 a2) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_assert ~loc a1 =
Expression.create ~pexp_desc:(Expression_desc.pexp_assert a1) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_lazy ~loc a1 =
Expression.create ~pexp_desc:(Expression_desc.pexp_lazy a1) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_poly ~loc a1 a2 =
Expression.create ~pexp_desc:(Expression_desc.pexp_poly a1 a2) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_object ~loc a1 =
Expression.create ~pexp_desc:(Expression_desc.pexp_object a1) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_newtype ~loc a1 a2 =
Expression.create ~pexp_desc:(Expression_desc.pexp_newtype a1 a2) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_pack ~loc a1 =
Expression.create ~pexp_desc:(Expression_desc.pexp_pack a1) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_open ~loc a1 a2 a3 =
Expression.create ~pexp_desc:(Expression_desc.pexp_open a1 a2 a3) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_extension ~loc a1 =
Expression.create ~pexp_desc:(Expression_desc.pexp_extension a1) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let pexp_unreachable ~loc =
Expression.create ~pexp_desc:(Expression_desc.pexp_unreachable) ~pexp_loc:loc ~pexp_attributes:(Attributes.of_concrete [])
let case ~guard ~lhs ~rhs =
Case.create ~pc_lhs:lhs ~pc_guard:guard ~pc_rhs:rhs
let value_description ~loc ~name ~prim ~type_ =
Value_description.create ~pval_name:name ~pval_type:type_ ~pval_prim:prim ~pval_attributes:(Attributes.of_concrete []) ~pval_loc:loc
let type_declaration ~loc ~cstrs ~kind ~manifest ~name ~params ~private_ =
Type_declaration.create ~ptype_name:name ~ptype_params:params ~ptype_cstrs:cstrs ~ptype_kind:kind ~ptype_private:private_ ~ptype_manifest:manifest ~ptype_attributes:(Attributes.of_concrete []) ~ptype_loc:loc
let label_declaration ~loc ~mutable_ ~name ~type_ =
Label_declaration.create ~pld_name:name ~pld_mutable:mutable_ ~pld_type:type_ ~pld_loc:loc ~pld_attributes:(Attributes.of_concrete [])
let constructor_declaration ~loc ~args ~name ~res =
Constructor_declaration.create ~pcd_name:name ~pcd_args:args ~pcd_res:res ~pcd_loc:loc ~pcd_attributes:(Attributes.of_concrete [])
let type_extension ~constructors ~params ~path ~private_ =
Type_extension.create ~ptyext_path:path ~ptyext_params:params ~ptyext_constructors:constructors ~ptyext_private:private_ ~ptyext_attributes:(Attributes.of_concrete [])
let extension_constructor ~loc ~kind ~name =
Extension_constructor.create ~pext_name:name ~pext_kind:kind ~pext_loc:loc ~pext_attributes:(Attributes.of_concrete [])
let pcty_constr ~loc a1 a2 =
Class_type.create ~pcty_desc:(Class_type_desc.pcty_constr a1 a2) ~pcty_loc:loc ~pcty_attributes:(Attributes.of_concrete [])
let pcty_signature ~loc a1 =
Class_type.create ~pcty_desc:(Class_type_desc.pcty_signature a1) ~pcty_loc:loc ~pcty_attributes:(Attributes.of_concrete [])
let pcty_arrow ~loc a1 a2 a3 =
Class_type.create ~pcty_desc:(Class_type_desc.pcty_arrow a1 a2 a3) ~pcty_loc:loc ~pcty_attributes:(Attributes.of_concrete [])
let pcty_extension ~loc a1 =
Class_type.create ~pcty_desc:(Class_type_desc.pcty_extension a1) ~pcty_loc:loc ~pcty_attributes:(Attributes.of_concrete [])
let pcty_open ~loc a1 a2 a3 =
Class_type.create ~pcty_desc:(Class_type_desc.pcty_open a1 a2 a3) ~pcty_loc:loc ~pcty_attributes:(Attributes.of_concrete [])
let class_signature ~fields ~self =
Class_signature.create ~pcsig_self:self ~pcsig_fields:fields
let pctf_inherit ~loc a1 =
Class_type_field.create ~pctf_desc:(Class_type_field_desc.pctf_inherit a1) ~pctf_loc:loc ~pctf_attributes:(Attributes.of_concrete [])
let pctf_val ~loc a1 =
Class_type_field.create ~pctf_desc:(Class_type_field_desc.pctf_val a1) ~pctf_loc:loc ~pctf_attributes:(Attributes.of_concrete [])
let pctf_method ~loc a1 =
Class_type_field.create ~pctf_desc:(Class_type_field_desc.pctf_method a1) ~pctf_loc:loc ~pctf_attributes:(Attributes.of_concrete [])
let pctf_constraint ~loc a1 =
Class_type_field.create ~pctf_desc:(Class_type_field_desc.pctf_constraint a1) ~pctf_loc:loc ~pctf_attributes:(Attributes.of_concrete [])
let pctf_attribute ~loc a1 =
Class_type_field.create ~pctf_desc:(Class_type_field_desc.pctf_attribute a1) ~pctf_loc:loc ~pctf_attributes:(Attributes.of_concrete [])
let pctf_extension ~loc a1 =
Class_type_field.create ~pctf_desc:(Class_type_field_desc.pctf_extension a1) ~pctf_loc:loc ~pctf_attributes:(Attributes.of_concrete [])
let pcl_constr ~loc a1 a2 =
Class_expr.create ~pcl_desc:(Class_expr_desc.pcl_constr a1 a2) ~pcl_loc:loc ~pcl_attributes:(Attributes.of_concrete [])
let pcl_structure ~loc a1 =
Class_expr.create ~pcl_desc:(Class_expr_desc.pcl_structure a1) ~pcl_loc:loc ~pcl_attributes:(Attributes.of_concrete [])
let pcl_fun ~loc a1 a2 a3 a4 =
Class_expr.create ~pcl_desc:(Class_expr_desc.pcl_fun a1 a2 a3 a4) ~pcl_loc:loc ~pcl_attributes:(Attributes.of_concrete [])
let pcl_apply ~loc a1 a2 =
Class_expr.create ~pcl_desc:(Class_expr_desc.pcl_apply a1 a2) ~pcl_loc:loc ~pcl_attributes:(Attributes.of_concrete [])
let pcl_let ~loc a1 a2 a3 =
Class_expr.create ~pcl_desc:(Class_expr_desc.pcl_let a1 a2 a3) ~pcl_loc:loc ~pcl_attributes:(Attributes.of_concrete [])
let pcl_constraint ~loc a1 a2 =
Class_expr.create ~pcl_desc:(Class_expr_desc.pcl_constraint a1 a2) ~pcl_loc:loc ~pcl_attributes:(Attributes.of_concrete [])
let pcl_extension ~loc a1 =
Class_expr.create ~pcl_desc:(Class_expr_desc.pcl_extension a1) ~pcl_loc:loc ~pcl_attributes:(Attributes.of_concrete [])
let pcl_open ~loc a1 a2 a3 =
Class_expr.create ~pcl_desc:(Class_expr_desc.pcl_open a1 a2 a3) ~pcl_loc:loc ~pcl_attributes:(Attributes.of_concrete [])
let class_structure ~fields ~self =
Class_structure.create ~pcstr_self:self ~pcstr_fields:fields
let pcf_inherit ~loc a1 a2 a3 =
Class_field.create ~pcf_desc:(Class_field_desc.pcf_inherit a1 a2 a3) ~pcf_loc:loc ~pcf_attributes:(Attributes.of_concrete [])
let pcf_val ~loc a1 =
Class_field.create ~pcf_desc:(Class_field_desc.pcf_val a1) ~pcf_loc:loc ~pcf_attributes:(Attributes.of_concrete [])
let pcf_method ~loc a1 =
Class_field.create ~pcf_desc:(Class_field_desc.pcf_method a1) ~pcf_loc:loc ~pcf_attributes:(Attributes.of_concrete [])
let pcf_constraint ~loc a1 =
Class_field.create ~pcf_desc:(Class_field_desc.pcf_constraint a1) ~pcf_loc:loc ~pcf_attributes:(Attributes.of_concrete [])
let pcf_initializer ~loc a1 =
Class_field.create ~pcf_desc:(Class_field_desc.pcf_initializer a1) ~pcf_loc:loc ~pcf_attributes:(Attributes.of_concrete [])
let pcf_attribute ~loc a1 =
Class_field.create ~pcf_desc:(Class_field_desc.pcf_attribute a1) ~pcf_loc:loc ~pcf_attributes:(Attributes.of_concrete [])
let pcf_extension ~loc a1 =
Class_field.create ~pcf_desc:(Class_field_desc.pcf_extension a1) ~pcf_loc:loc ~pcf_attributes:(Attributes.of_concrete [])
let pmty_ident ~loc a1 =
Module_type.create ~pmty_desc:(Module_type_desc.pmty_ident a1) ~pmty_loc:loc ~pmty_attributes:(Attributes.of_concrete [])
let pmty_signature ~loc a1 =
Module_type.create ~pmty_desc:(Module_type_desc.pmty_signature a1) ~pmty_loc:loc ~pmty_attributes:(Attributes.of_concrete [])
let pmty_functor ~loc a1 a2 a3 =
Module_type.create ~pmty_desc:(Module_type_desc.pmty_functor a1 a2 a3) ~pmty_loc:loc ~pmty_attributes:(Attributes.of_concrete [])
let pmty_with ~loc a1 a2 =
Module_type.create ~pmty_desc:(Module_type_desc.pmty_with a1 a2) ~pmty_loc:loc ~pmty_attributes:(Attributes.of_concrete [])
let pmty_typeof ~loc a1 =
Module_type.create ~pmty_desc:(Module_type_desc.pmty_typeof a1) ~pmty_loc:loc ~pmty_attributes:(Attributes.of_concrete [])
let pmty_extension ~loc a1 =
Module_type.create ~pmty_desc:(Module_type_desc.pmty_extension a1) ~pmty_loc:loc ~pmty_attributes:(Attributes.of_concrete [])
let pmty_alias ~loc a1 =
Module_type.create ~pmty_desc:(Module_type_desc.pmty_alias a1) ~pmty_loc:loc ~pmty_attributes:(Attributes.of_concrete [])
let psig_value ~loc a1 =
Signature_item.create ~psig_desc:(Signature_item_desc.psig_value a1) ~psig_loc:loc
let psig_type ~loc a1 a2 =
Signature_item.create ~psig_desc:(Signature_item_desc.psig_type a1 a2) ~psig_loc:loc
let psig_typext ~loc a1 =
Signature_item.create ~psig_desc:(Signature_item_desc.psig_typext a1) ~psig_loc:loc
let psig_exception ~loc a1 =
Signature_item.create ~psig_desc:(Signature_item_desc.psig_exception a1) ~psig_loc:loc
let psig_module ~loc a1 =
Signature_item.create ~psig_desc:(Signature_item_desc.psig_module a1) ~psig_loc:loc
let psig_recmodule ~loc a1 =
Signature_item.create ~psig_desc:(Signature_item_desc.psig_recmodule a1) ~psig_loc:loc
let psig_modtype ~loc a1 =
Signature_item.create ~psig_desc:(Signature_item_desc.psig_modtype a1) ~psig_loc:loc
let psig_open ~loc a1 =
Signature_item.create ~psig_desc:(Signature_item_desc.psig_open a1) ~psig_loc:loc
let psig_include ~loc a1 =
Signature_item.create ~psig_desc:(Signature_item_desc.psig_include a1) ~psig_loc:loc
let psig_class ~loc a1 =
Signature_item.create ~psig_desc:(Signature_item_desc.psig_class a1) ~psig_loc:loc
let psig_class_type ~loc a1 =
Signature_item.create ~psig_desc:(Signature_item_desc.psig_class_type a1) ~psig_loc:loc
let psig_attribute ~loc a1 =
Signature_item.create ~psig_desc:(Signature_item_desc.psig_attribute a1) ~psig_loc:loc
let psig_extension ~loc a1 a2 =
Signature_item.create ~psig_desc:(Signature_item_desc.psig_extension a1 a2) ~psig_loc:loc
let module_declaration ~loc ~name ~type_ =
Module_declaration.create ~pmd_name:name ~pmd_type:type_ ~pmd_attributes:(Attributes.of_concrete []) ~pmd_loc:loc
let module_type_declaration ~loc ~name ~type_ =
Module_type_declaration.create ~pmtd_name:name ~pmtd_type:type_ ~pmtd_attributes:(Attributes.of_concrete []) ~pmtd_loc:loc
let open_description ~loc ~lid ~override =
Open_description.create ~popen_lid:lid ~popen_override:override ~popen_loc:loc ~popen_attributes:(Attributes.of_concrete [])
let pmod_ident ~loc a1 =
Module_expr.create ~pmod_desc:(Module_expr_desc.pmod_ident a1) ~pmod_loc:loc ~pmod_attributes:(Attributes.of_concrete [])
let pmod_structure ~loc a1 =
Module_expr.create ~pmod_desc:(Module_expr_desc.pmod_structure a1) ~pmod_loc:loc ~pmod_attributes:(Attributes.of_concrete [])
let pmod_functor ~loc a1 a2 a3 =
Module_expr.create ~pmod_desc:(Module_expr_desc.pmod_functor a1 a2 a3) ~pmod_loc:loc ~pmod_attributes:(Attributes.of_concrete [])
let pmod_apply ~loc a1 a2 =
Module_expr.create ~pmod_desc:(Module_expr_desc.pmod_apply a1 a2) ~pmod_loc:loc ~pmod_attributes:(Attributes.of_concrete [])
let pmod_constraint ~loc a1 a2 =
Module_expr.create ~pmod_desc:(Module_expr_desc.pmod_constraint a1 a2) ~pmod_loc:loc ~pmod_attributes:(Attributes.of_concrete [])
let pmod_unpack ~loc a1 =
Module_expr.create ~pmod_desc:(Module_expr_desc.pmod_unpack a1) ~pmod_loc:loc ~pmod_attributes:(Attributes.of_concrete [])
let pmod_extension ~loc a1 =
Module_expr.create ~pmod_desc:(Module_expr_desc.pmod_extension a1) ~pmod_loc:loc ~pmod_attributes:(Attributes.of_concrete [])
let pstr_eval ~loc a1 a2 =
Structure_item.create ~pstr_desc:(Structure_item_desc.pstr_eval a1 a2) ~pstr_loc:loc
let pstr_value ~loc a1 a2 =
Structure_item.create ~pstr_desc:(Structure_item_desc.pstr_value a1 a2) ~pstr_loc:loc
let pstr_primitive ~loc a1 =
Structure_item.create ~pstr_desc:(Structure_item_desc.pstr_primitive a1) ~pstr_loc:loc
let pstr_type ~loc a1 a2 =
Structure_item.create ~pstr_desc:(Structure_item_desc.pstr_type a1 a2) ~pstr_loc:loc
let pstr_typext ~loc a1 =
Structure_item.create ~pstr_desc:(Structure_item_desc.pstr_typext a1) ~pstr_loc:loc
let pstr_exception ~loc a1 =
Structure_item.create ~pstr_desc:(Structure_item_desc.pstr_exception a1) ~pstr_loc:loc
let pstr_module ~loc a1 =
Structure_item.create ~pstr_desc:(Structure_item_desc.pstr_module a1) ~pstr_loc:loc
let pstr_recmodule ~loc a1 =
Structure_item.create ~pstr_desc:(Structure_item_desc.pstr_recmodule a1) ~pstr_loc:loc
let pstr_modtype ~loc a1 =
Structure_item.create ~pstr_desc:(Structure_item_desc.pstr_modtype a1) ~pstr_loc:loc
let pstr_open ~loc a1 =
Structure_item.create ~pstr_desc:(Structure_item_desc.pstr_open a1) ~pstr_loc:loc
let pstr_class ~loc a1 =
Structure_item.create ~pstr_desc:(Structure_item_desc.pstr_class a1) ~pstr_loc:loc
let pstr_class_type ~loc a1 =
Structure_item.create ~pstr_desc:(Structure_item_desc.pstr_class_type a1) ~pstr_loc:loc
let pstr_include ~loc a1 =
Structure_item.create ~pstr_desc:(Structure_item_desc.pstr_include a1) ~pstr_loc:loc
let pstr_attribute ~loc a1 =
Structure_item.create ~pstr_desc:(Structure_item_desc.pstr_attribute a1) ~pstr_loc:loc
let pstr_extension ~loc a1 a2 =
Structure_item.create ~pstr_desc:(Structure_item_desc.pstr_extension a1 a2) ~pstr_loc:loc
let value_binding ~loc ~expr ~pat =
Value_binding.create ~pvb_pat:pat ~pvb_expr:expr ~pvb_attributes:(Attributes.of_concrete []) ~pvb_loc:loc
let module_binding ~loc ~expr ~name =
Module_binding.create ~pmb_name:name ~pmb_expr:expr ~pmb_attributes:(Attributes.of_concrete []) ~pmb_loc:loc
(*$*)
