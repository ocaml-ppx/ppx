open Viewlib

(*$ Ppx_ast_cinaps.print_viewer_ml (Astlib.Version.of_string "unstable_for_testing") *)
open Versions.Unstable_for_testing
include Viewer_common

let pdir_bool'const view value =
  let concrete = Directive_argument.to_concrete value in
  match concrete with
  | Directive_argument.Pdir_bool arg -> view arg
  | _ -> View.error

let pdir_ident'const view value =
  let concrete = Directive_argument.to_concrete value in
  match concrete with
  | Directive_argument.Pdir_ident arg -> view arg
  | _ -> View.error

let pdir_int'const view value =
  let concrete = Directive_argument.to_concrete value in
  match concrete with
  | Directive_argument.Pdir_int (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pdir_string'const view value =
  let concrete = Directive_argument.to_concrete value in
  match concrete with
  | Directive_argument.Pdir_string arg -> view arg
  | _ -> View.error

let pdir_none'const value =
  let concrete = Directive_argument.to_concrete value in
  match concrete with
  | Directive_argument.Pdir_none -> View.ok
  | _ -> View.error

let ptop_dir'const view value =
  let concrete = Toplevel_phrase.to_concrete value in
  match concrete with
  | Toplevel_phrase.Ptop_dir (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ptop_def'const view value =
  let concrete = Toplevel_phrase.to_concrete value in
  match concrete with
  | Toplevel_phrase.Ptop_def arg -> view arg
  | _ -> View.error

let pmb_loc'match view value =
  let concrete = Module_binding.to_concrete value in
  view concrete.Module_binding.pmb_loc

let pmb_attributes'match view value =
  let concrete = Module_binding.to_concrete value in
  view concrete.Module_binding.pmb_attributes

let pmb_expr'match view value =
  let concrete = Module_binding.to_concrete value in
  view concrete.Module_binding.pmb_expr

let pmb_name'match view value =
  let concrete = Module_binding.to_concrete value in
  view concrete.Module_binding.pmb_name

let pvb_loc'match view value =
  let concrete = Value_binding.to_concrete value in
  view concrete.Value_binding.pvb_loc

let pvb_attributes'match view value =
  let concrete = Value_binding.to_concrete value in
  view concrete.Value_binding.pvb_attributes

let pvb_expr'match view value =
  let concrete = Value_binding.to_concrete value in
  view concrete.Value_binding.pvb_expr

let pvb_pat'match view value =
  let concrete = Value_binding.to_concrete value in
  view concrete.Value_binding.pvb_pat

let pstr_extension'const view value =
  let concrete = Structure_item_desc.to_concrete value in
  match concrete with
  | Structure_item_desc.Pstr_extension (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let strextension'const view value =
  let parent_concrete = Structure_item.to_concrete value in
  let desc = parent_concrete.Structure_item.pstr_desc in
  let concrete = Structure_item_desc.to_concrete desc in
  match concrete with
  | Structure_item_desc.Pstr_extension (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pstr_attribute'const view value =
  let concrete = Structure_item_desc.to_concrete value in
  match concrete with
  | Structure_item_desc.Pstr_attribute arg -> view arg
  | _ -> View.error

let strattribute'const view value =
  let parent_concrete = Structure_item.to_concrete value in
  let desc = parent_concrete.Structure_item.pstr_desc in
  let concrete = Structure_item_desc.to_concrete desc in
  match concrete with
  | Structure_item_desc.Pstr_attribute arg -> view arg
  | _ -> View.error

let pstr_include'const view value =
  let concrete = Structure_item_desc.to_concrete value in
  match concrete with
  | Structure_item_desc.Pstr_include arg -> view arg
  | _ -> View.error

let strinclude'const view value =
  let parent_concrete = Structure_item.to_concrete value in
  let desc = parent_concrete.Structure_item.pstr_desc in
  let concrete = Structure_item_desc.to_concrete desc in
  match concrete with
  | Structure_item_desc.Pstr_include arg -> view arg
  | _ -> View.error

let pstr_class_type'const view value =
  let concrete = Structure_item_desc.to_concrete value in
  match concrete with
  | Structure_item_desc.Pstr_class_type arg -> view arg
  | _ -> View.error

let strclass_type'const view value =
  let parent_concrete = Structure_item.to_concrete value in
  let desc = parent_concrete.Structure_item.pstr_desc in
  let concrete = Structure_item_desc.to_concrete desc in
  match concrete with
  | Structure_item_desc.Pstr_class_type arg -> view arg
  | _ -> View.error

let pstr_class'const view value =
  let concrete = Structure_item_desc.to_concrete value in
  match concrete with
  | Structure_item_desc.Pstr_class arg -> view arg
  | _ -> View.error

let strclass'const view value =
  let parent_concrete = Structure_item.to_concrete value in
  let desc = parent_concrete.Structure_item.pstr_desc in
  let concrete = Structure_item_desc.to_concrete desc in
  match concrete with
  | Structure_item_desc.Pstr_class arg -> view arg
  | _ -> View.error

let pstr_open'const view value =
  let concrete = Structure_item_desc.to_concrete value in
  match concrete with
  | Structure_item_desc.Pstr_open arg -> view arg
  | _ -> View.error

let stropen'const view value =
  let parent_concrete = Structure_item.to_concrete value in
  let desc = parent_concrete.Structure_item.pstr_desc in
  let concrete = Structure_item_desc.to_concrete desc in
  match concrete with
  | Structure_item_desc.Pstr_open arg -> view arg
  | _ -> View.error

let pstr_modtype'const view value =
  let concrete = Structure_item_desc.to_concrete value in
  match concrete with
  | Structure_item_desc.Pstr_modtype arg -> view arg
  | _ -> View.error

let strmodtype'const view value =
  let parent_concrete = Structure_item.to_concrete value in
  let desc = parent_concrete.Structure_item.pstr_desc in
  let concrete = Structure_item_desc.to_concrete desc in
  match concrete with
  | Structure_item_desc.Pstr_modtype arg -> view arg
  | _ -> View.error

let pstr_recmodule'const view value =
  let concrete = Structure_item_desc.to_concrete value in
  match concrete with
  | Structure_item_desc.Pstr_recmodule arg -> view arg
  | _ -> View.error

let strrecmodule'const view value =
  let parent_concrete = Structure_item.to_concrete value in
  let desc = parent_concrete.Structure_item.pstr_desc in
  let concrete = Structure_item_desc.to_concrete desc in
  match concrete with
  | Structure_item_desc.Pstr_recmodule arg -> view arg
  | _ -> View.error

let pstr_module'const view value =
  let concrete = Structure_item_desc.to_concrete value in
  match concrete with
  | Structure_item_desc.Pstr_module arg -> view arg
  | _ -> View.error

let strmodule'const view value =
  let parent_concrete = Structure_item.to_concrete value in
  let desc = parent_concrete.Structure_item.pstr_desc in
  let concrete = Structure_item_desc.to_concrete desc in
  match concrete with
  | Structure_item_desc.Pstr_module arg -> view arg
  | _ -> View.error

let pstr_exception'const view value =
  let concrete = Structure_item_desc.to_concrete value in
  match concrete with
  | Structure_item_desc.Pstr_exception arg -> view arg
  | _ -> View.error

let strexception'const view value =
  let parent_concrete = Structure_item.to_concrete value in
  let desc = parent_concrete.Structure_item.pstr_desc in
  let concrete = Structure_item_desc.to_concrete desc in
  match concrete with
  | Structure_item_desc.Pstr_exception arg -> view arg
  | _ -> View.error

let pstr_typext'const view value =
  let concrete = Structure_item_desc.to_concrete value in
  match concrete with
  | Structure_item_desc.Pstr_typext arg -> view arg
  | _ -> View.error

let strtypext'const view value =
  let parent_concrete = Structure_item.to_concrete value in
  let desc = parent_concrete.Structure_item.pstr_desc in
  let concrete = Structure_item_desc.to_concrete desc in
  match concrete with
  | Structure_item_desc.Pstr_typext arg -> view arg
  | _ -> View.error

let pstr_type'const view value =
  let concrete = Structure_item_desc.to_concrete value in
  match concrete with
  | Structure_item_desc.Pstr_type (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let strtype'const view value =
  let parent_concrete = Structure_item.to_concrete value in
  let desc = parent_concrete.Structure_item.pstr_desc in
  let concrete = Structure_item_desc.to_concrete desc in
  match concrete with
  | Structure_item_desc.Pstr_type (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pstr_primitive'const view value =
  let concrete = Structure_item_desc.to_concrete value in
  match concrete with
  | Structure_item_desc.Pstr_primitive arg -> view arg
  | _ -> View.error

let strprimitive'const view value =
  let parent_concrete = Structure_item.to_concrete value in
  let desc = parent_concrete.Structure_item.pstr_desc in
  let concrete = Structure_item_desc.to_concrete desc in
  match concrete with
  | Structure_item_desc.Pstr_primitive arg -> view arg
  | _ -> View.error

let pstr_value'const view value =
  let concrete = Structure_item_desc.to_concrete value in
  match concrete with
  | Structure_item_desc.Pstr_value (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let strvalue'const view value =
  let parent_concrete = Structure_item.to_concrete value in
  let desc = parent_concrete.Structure_item.pstr_desc in
  let concrete = Structure_item_desc.to_concrete desc in
  match concrete with
  | Structure_item_desc.Pstr_value (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pstr_eval'const view value =
  let concrete = Structure_item_desc.to_concrete value in
  match concrete with
  | Structure_item_desc.Pstr_eval (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let streval'const view value =
  let parent_concrete = Structure_item.to_concrete value in
  let desc = parent_concrete.Structure_item.pstr_desc in
  let concrete = Structure_item_desc.to_concrete desc in
  match concrete with
  | Structure_item_desc.Pstr_eval (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pstr_loc'match view value =
  let concrete = Structure_item.to_concrete value in
  view concrete.Structure_item.pstr_loc

let pstr_desc'match view value =
  let concrete = Structure_item.to_concrete value in
  view concrete.Structure_item.pstr_desc

let structure'const view value =
  let concrete0 = Structure.to_concrete value in
  view concrete0

let pmod_extension'const view value =
  let concrete = Module_expr_desc.to_concrete value in
  match concrete with
  | Module_expr_desc.Pmod_extension arg -> view arg
  | _ -> View.error

let meextension'const view value =
  let parent_concrete = Module_expr.to_concrete value in
  let desc = parent_concrete.Module_expr.pmod_desc in
  let concrete = Module_expr_desc.to_concrete desc in
  match concrete with
  | Module_expr_desc.Pmod_extension arg -> view arg
  | _ -> View.error

let pmod_unpack'const view value =
  let concrete = Module_expr_desc.to_concrete value in
  match concrete with
  | Module_expr_desc.Pmod_unpack arg -> view arg
  | _ -> View.error

let meunpack'const view value =
  let parent_concrete = Module_expr.to_concrete value in
  let desc = parent_concrete.Module_expr.pmod_desc in
  let concrete = Module_expr_desc.to_concrete desc in
  match concrete with
  | Module_expr_desc.Pmod_unpack arg -> view arg
  | _ -> View.error

let pmod_constraint'const view value =
  let concrete = Module_expr_desc.to_concrete value in
  match concrete with
  | Module_expr_desc.Pmod_constraint (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let meconstraint'const view value =
  let parent_concrete = Module_expr.to_concrete value in
  let desc = parent_concrete.Module_expr.pmod_desc in
  let concrete = Module_expr_desc.to_concrete desc in
  match concrete with
  | Module_expr_desc.Pmod_constraint (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pmod_apply'const view value =
  let concrete = Module_expr_desc.to_concrete value in
  match concrete with
  | Module_expr_desc.Pmod_apply (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let meapply'const view value =
  let parent_concrete = Module_expr.to_concrete value in
  let desc = parent_concrete.Module_expr.pmod_desc in
  let concrete = Module_expr_desc.to_concrete desc in
  match concrete with
  | Module_expr_desc.Pmod_apply (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pmod_functor'const view value =
  let concrete = Module_expr_desc.to_concrete value in
  match concrete with
  | Module_expr_desc.Pmod_functor (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let mefunctor'const view value =
  let parent_concrete = Module_expr.to_concrete value in
  let desc = parent_concrete.Module_expr.pmod_desc in
  let concrete = Module_expr_desc.to_concrete desc in
  match concrete with
  | Module_expr_desc.Pmod_functor (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let pmod_structure'const view value =
  let concrete = Module_expr_desc.to_concrete value in
  match concrete with
  | Module_expr_desc.Pmod_structure arg -> view arg
  | _ -> View.error

let mestructure'const view value =
  let parent_concrete = Module_expr.to_concrete value in
  let desc = parent_concrete.Module_expr.pmod_desc in
  let concrete = Module_expr_desc.to_concrete desc in
  match concrete with
  | Module_expr_desc.Pmod_structure arg -> view arg
  | _ -> View.error

let pmod_ident'const view value =
  let concrete = Module_expr_desc.to_concrete value in
  match concrete with
  | Module_expr_desc.Pmod_ident arg -> view arg
  | _ -> View.error

let meident'const view value =
  let parent_concrete = Module_expr.to_concrete value in
  let desc = parent_concrete.Module_expr.pmod_desc in
  let concrete = Module_expr_desc.to_concrete desc in
  match concrete with
  | Module_expr_desc.Pmod_ident arg -> view arg
  | _ -> View.error

let pmod_attributes'match view value =
  let concrete = Module_expr.to_concrete value in
  view concrete.Module_expr.pmod_attributes

let pmod_loc'match view value =
  let concrete = Module_expr.to_concrete value in
  view concrete.Module_expr.pmod_loc

let pmod_desc'match view value =
  let concrete = Module_expr.to_concrete value in
  view concrete.Module_expr.pmod_desc

let pwith_modsubst'const view value =
  let concrete = With_constraint.to_concrete value in
  match concrete with
  | With_constraint.Pwith_modsubst (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pwith_typesubst'const view value =
  let concrete = With_constraint.to_concrete value in
  match concrete with
  | With_constraint.Pwith_typesubst (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pwith_module'const view value =
  let concrete = With_constraint.to_concrete value in
  match concrete with
  | With_constraint.Pwith_module (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pwith_type'const view value =
  let concrete = With_constraint.to_concrete value in
  match concrete with
  | With_constraint.Pwith_type (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let include_declaration'const view value =
  let concrete0 = Include_declaration.to_concrete value in
  view concrete0

let include_description'const view value =
  let concrete0 = Include_description.to_concrete value in
  view concrete0

let pincl_attributes'match view value =
  let concrete = Include_infos.to_concrete value in
  view concrete.Include_infos.pincl_attributes

let pincl_loc'match view value =
  let concrete = Include_infos.to_concrete value in
  view concrete.Include_infos.pincl_loc

let pincl_mod'match view value =
  let concrete = Include_infos.to_concrete value in
  view concrete.Include_infos.pincl_mod

let popen_attributes'match view value =
  let concrete = Open_description.to_concrete value in
  view concrete.Open_description.popen_attributes

let popen_loc'match view value =
  let concrete = Open_description.to_concrete value in
  view concrete.Open_description.popen_loc

let popen_override'match view value =
  let concrete = Open_description.to_concrete value in
  view concrete.Open_description.popen_override

let popen_lid'match view value =
  let concrete = Open_description.to_concrete value in
  view concrete.Open_description.popen_lid

let pmtd_loc'match view value =
  let concrete = Module_type_declaration.to_concrete value in
  view concrete.Module_type_declaration.pmtd_loc

let pmtd_attributes'match view value =
  let concrete = Module_type_declaration.to_concrete value in
  view concrete.Module_type_declaration.pmtd_attributes

let pmtd_type'match view value =
  let concrete = Module_type_declaration.to_concrete value in
  view concrete.Module_type_declaration.pmtd_type

let pmtd_name'match view value =
  let concrete = Module_type_declaration.to_concrete value in
  view concrete.Module_type_declaration.pmtd_name

let pmd_loc'match view value =
  let concrete = Module_declaration.to_concrete value in
  view concrete.Module_declaration.pmd_loc

let pmd_attributes'match view value =
  let concrete = Module_declaration.to_concrete value in
  view concrete.Module_declaration.pmd_attributes

let pmd_type'match view value =
  let concrete = Module_declaration.to_concrete value in
  view concrete.Module_declaration.pmd_type

let pmd_name'match view value =
  let concrete = Module_declaration.to_concrete value in
  view concrete.Module_declaration.pmd_name

let psig_extension'const view value =
  let concrete = Signature_item_desc.to_concrete value in
  match concrete with
  | Signature_item_desc.Psig_extension (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let sigextension'const view value =
  let parent_concrete = Signature_item.to_concrete value in
  let desc = parent_concrete.Signature_item.psig_desc in
  let concrete = Signature_item_desc.to_concrete desc in
  match concrete with
  | Signature_item_desc.Psig_extension (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let psig_attribute'const view value =
  let concrete = Signature_item_desc.to_concrete value in
  match concrete with
  | Signature_item_desc.Psig_attribute arg -> view arg
  | _ -> View.error

let sigattribute'const view value =
  let parent_concrete = Signature_item.to_concrete value in
  let desc = parent_concrete.Signature_item.psig_desc in
  let concrete = Signature_item_desc.to_concrete desc in
  match concrete with
  | Signature_item_desc.Psig_attribute arg -> view arg
  | _ -> View.error

let psig_class_type'const view value =
  let concrete = Signature_item_desc.to_concrete value in
  match concrete with
  | Signature_item_desc.Psig_class_type arg -> view arg
  | _ -> View.error

let sigclass_type'const view value =
  let parent_concrete = Signature_item.to_concrete value in
  let desc = parent_concrete.Signature_item.psig_desc in
  let concrete = Signature_item_desc.to_concrete desc in
  match concrete with
  | Signature_item_desc.Psig_class_type arg -> view arg
  | _ -> View.error

let psig_class'const view value =
  let concrete = Signature_item_desc.to_concrete value in
  match concrete with
  | Signature_item_desc.Psig_class arg -> view arg
  | _ -> View.error

let sigclass'const view value =
  let parent_concrete = Signature_item.to_concrete value in
  let desc = parent_concrete.Signature_item.psig_desc in
  let concrete = Signature_item_desc.to_concrete desc in
  match concrete with
  | Signature_item_desc.Psig_class arg -> view arg
  | _ -> View.error

let psig_include'const view value =
  let concrete = Signature_item_desc.to_concrete value in
  match concrete with
  | Signature_item_desc.Psig_include arg -> view arg
  | _ -> View.error

let siginclude'const view value =
  let parent_concrete = Signature_item.to_concrete value in
  let desc = parent_concrete.Signature_item.psig_desc in
  let concrete = Signature_item_desc.to_concrete desc in
  match concrete with
  | Signature_item_desc.Psig_include arg -> view arg
  | _ -> View.error

let psig_open'const view value =
  let concrete = Signature_item_desc.to_concrete value in
  match concrete with
  | Signature_item_desc.Psig_open arg -> view arg
  | _ -> View.error

let sigopen'const view value =
  let parent_concrete = Signature_item.to_concrete value in
  let desc = parent_concrete.Signature_item.psig_desc in
  let concrete = Signature_item_desc.to_concrete desc in
  match concrete with
  | Signature_item_desc.Psig_open arg -> view arg
  | _ -> View.error

let psig_modtype'const view value =
  let concrete = Signature_item_desc.to_concrete value in
  match concrete with
  | Signature_item_desc.Psig_modtype arg -> view arg
  | _ -> View.error

let sigmodtype'const view value =
  let parent_concrete = Signature_item.to_concrete value in
  let desc = parent_concrete.Signature_item.psig_desc in
  let concrete = Signature_item_desc.to_concrete desc in
  match concrete with
  | Signature_item_desc.Psig_modtype arg -> view arg
  | _ -> View.error

let psig_recmodule'const view value =
  let concrete = Signature_item_desc.to_concrete value in
  match concrete with
  | Signature_item_desc.Psig_recmodule arg -> view arg
  | _ -> View.error

let sigrecmodule'const view value =
  let parent_concrete = Signature_item.to_concrete value in
  let desc = parent_concrete.Signature_item.psig_desc in
  let concrete = Signature_item_desc.to_concrete desc in
  match concrete with
  | Signature_item_desc.Psig_recmodule arg -> view arg
  | _ -> View.error

let psig_module'const view value =
  let concrete = Signature_item_desc.to_concrete value in
  match concrete with
  | Signature_item_desc.Psig_module arg -> view arg
  | _ -> View.error

let sigmodule'const view value =
  let parent_concrete = Signature_item.to_concrete value in
  let desc = parent_concrete.Signature_item.psig_desc in
  let concrete = Signature_item_desc.to_concrete desc in
  match concrete with
  | Signature_item_desc.Psig_module arg -> view arg
  | _ -> View.error

let psig_exception'const view value =
  let concrete = Signature_item_desc.to_concrete value in
  match concrete with
  | Signature_item_desc.Psig_exception arg -> view arg
  | _ -> View.error

let sigexception'const view value =
  let parent_concrete = Signature_item.to_concrete value in
  let desc = parent_concrete.Signature_item.psig_desc in
  let concrete = Signature_item_desc.to_concrete desc in
  match concrete with
  | Signature_item_desc.Psig_exception arg -> view arg
  | _ -> View.error

let psig_typext'const view value =
  let concrete = Signature_item_desc.to_concrete value in
  match concrete with
  | Signature_item_desc.Psig_typext arg -> view arg
  | _ -> View.error

let sigtypext'const view value =
  let parent_concrete = Signature_item.to_concrete value in
  let desc = parent_concrete.Signature_item.psig_desc in
  let concrete = Signature_item_desc.to_concrete desc in
  match concrete with
  | Signature_item_desc.Psig_typext arg -> view arg
  | _ -> View.error

let psig_type'const view value =
  let concrete = Signature_item_desc.to_concrete value in
  match concrete with
  | Signature_item_desc.Psig_type (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let sigtype'const view value =
  let parent_concrete = Signature_item.to_concrete value in
  let desc = parent_concrete.Signature_item.psig_desc in
  let concrete = Signature_item_desc.to_concrete desc in
  match concrete with
  | Signature_item_desc.Psig_type (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let psig_value'const view value =
  let concrete = Signature_item_desc.to_concrete value in
  match concrete with
  | Signature_item_desc.Psig_value arg -> view arg
  | _ -> View.error

let sigvalue'const view value =
  let parent_concrete = Signature_item.to_concrete value in
  let desc = parent_concrete.Signature_item.psig_desc in
  let concrete = Signature_item_desc.to_concrete desc in
  match concrete with
  | Signature_item_desc.Psig_value arg -> view arg
  | _ -> View.error

let psig_loc'match view value =
  let concrete = Signature_item.to_concrete value in
  view concrete.Signature_item.psig_loc

let psig_desc'match view value =
  let concrete = Signature_item.to_concrete value in
  view concrete.Signature_item.psig_desc

let signature'const view value =
  let concrete0 = Signature.to_concrete value in
  view concrete0

let pmty_alias'const view value =
  let concrete = Module_type_desc.to_concrete value in
  match concrete with
  | Module_type_desc.Pmty_alias arg -> view arg
  | _ -> View.error

let mtalias'const view value =
  let parent_concrete = Module_type.to_concrete value in
  let desc = parent_concrete.Module_type.pmty_desc in
  let concrete = Module_type_desc.to_concrete desc in
  match concrete with
  | Module_type_desc.Pmty_alias arg -> view arg
  | _ -> View.error

let pmty_extension'const view value =
  let concrete = Module_type_desc.to_concrete value in
  match concrete with
  | Module_type_desc.Pmty_extension arg -> view arg
  | _ -> View.error

let mtextension'const view value =
  let parent_concrete = Module_type.to_concrete value in
  let desc = parent_concrete.Module_type.pmty_desc in
  let concrete = Module_type_desc.to_concrete desc in
  match concrete with
  | Module_type_desc.Pmty_extension arg -> view arg
  | _ -> View.error

let pmty_typeof'const view value =
  let concrete = Module_type_desc.to_concrete value in
  match concrete with
  | Module_type_desc.Pmty_typeof arg -> view arg
  | _ -> View.error

let mttypeof'const view value =
  let parent_concrete = Module_type.to_concrete value in
  let desc = parent_concrete.Module_type.pmty_desc in
  let concrete = Module_type_desc.to_concrete desc in
  match concrete with
  | Module_type_desc.Pmty_typeof arg -> view arg
  | _ -> View.error

let pmty_with'const view value =
  let concrete = Module_type_desc.to_concrete value in
  match concrete with
  | Module_type_desc.Pmty_with (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let mtwith'const view value =
  let parent_concrete = Module_type.to_concrete value in
  let desc = parent_concrete.Module_type.pmty_desc in
  let concrete = Module_type_desc.to_concrete desc in
  match concrete with
  | Module_type_desc.Pmty_with (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pmty_functor'const view value =
  let concrete = Module_type_desc.to_concrete value in
  match concrete with
  | Module_type_desc.Pmty_functor (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let mtfunctor'const view value =
  let parent_concrete = Module_type.to_concrete value in
  let desc = parent_concrete.Module_type.pmty_desc in
  let concrete = Module_type_desc.to_concrete desc in
  match concrete with
  | Module_type_desc.Pmty_functor (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let pmty_signature'const view value =
  let concrete = Module_type_desc.to_concrete value in
  match concrete with
  | Module_type_desc.Pmty_signature arg -> view arg
  | _ -> View.error

let mtsignature'const view value =
  let parent_concrete = Module_type.to_concrete value in
  let desc = parent_concrete.Module_type.pmty_desc in
  let concrete = Module_type_desc.to_concrete desc in
  match concrete with
  | Module_type_desc.Pmty_signature arg -> view arg
  | _ -> View.error

let pmty_ident'const view value =
  let concrete = Module_type_desc.to_concrete value in
  match concrete with
  | Module_type_desc.Pmty_ident arg -> view arg
  | _ -> View.error

let mtident'const view value =
  let parent_concrete = Module_type.to_concrete value in
  let desc = parent_concrete.Module_type.pmty_desc in
  let concrete = Module_type_desc.to_concrete desc in
  match concrete with
  | Module_type_desc.Pmty_ident arg -> view arg
  | _ -> View.error

let pmty_attributes'match view value =
  let concrete = Module_type.to_concrete value in
  view concrete.Module_type.pmty_attributes

let pmty_loc'match view value =
  let concrete = Module_type.to_concrete value in
  view concrete.Module_type.pmty_loc

let pmty_desc'match view value =
  let concrete = Module_type.to_concrete value in
  view concrete.Module_type.pmty_desc

let class_declaration'const view value =
  let concrete0 = Class_declaration.to_concrete value in
  view concrete0

let cfk_concrete'const view value =
  let concrete = Class_field_kind.to_concrete value in
  match concrete with
  | Class_field_kind.Cfk_concrete (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let cfk_virtual'const view value =
  let concrete = Class_field_kind.to_concrete value in
  match concrete with
  | Class_field_kind.Cfk_virtual arg -> view arg
  | _ -> View.error

let pcf_extension'const view value =
  let concrete = Class_field_desc.to_concrete value in
  match concrete with
  | Class_field_desc.Pcf_extension arg -> view arg
  | _ -> View.error

let cfextension'const view value =
  let parent_concrete = Class_field.to_concrete value in
  let desc = parent_concrete.Class_field.pcf_desc in
  let concrete = Class_field_desc.to_concrete desc in
  match concrete with
  | Class_field_desc.Pcf_extension arg -> view arg
  | _ -> View.error

let pcf_attribute'const view value =
  let concrete = Class_field_desc.to_concrete value in
  match concrete with
  | Class_field_desc.Pcf_attribute arg -> view arg
  | _ -> View.error

let cfattribute'const view value =
  let parent_concrete = Class_field.to_concrete value in
  let desc = parent_concrete.Class_field.pcf_desc in
  let concrete = Class_field_desc.to_concrete desc in
  match concrete with
  | Class_field_desc.Pcf_attribute arg -> view arg
  | _ -> View.error

let pcf_initializer'const view value =
  let concrete = Class_field_desc.to_concrete value in
  match concrete with
  | Class_field_desc.Pcf_initializer arg -> view arg
  | _ -> View.error

let cfinitializer'const view value =
  let parent_concrete = Class_field.to_concrete value in
  let desc = parent_concrete.Class_field.pcf_desc in
  let concrete = Class_field_desc.to_concrete desc in
  match concrete with
  | Class_field_desc.Pcf_initializer arg -> view arg
  | _ -> View.error

let pcf_constraint'const view value =
  let concrete = Class_field_desc.to_concrete value in
  match concrete with
  | Class_field_desc.Pcf_constraint arg -> view arg
  | _ -> View.error

let cfconstraint'const view value =
  let parent_concrete = Class_field.to_concrete value in
  let desc = parent_concrete.Class_field.pcf_desc in
  let concrete = Class_field_desc.to_concrete desc in
  match concrete with
  | Class_field_desc.Pcf_constraint arg -> view arg
  | _ -> View.error

let pcf_method'const view value =
  let concrete = Class_field_desc.to_concrete value in
  match concrete with
  | Class_field_desc.Pcf_method arg -> view arg
  | _ -> View.error

let cfmethod'const view value =
  let parent_concrete = Class_field.to_concrete value in
  let desc = parent_concrete.Class_field.pcf_desc in
  let concrete = Class_field_desc.to_concrete desc in
  match concrete with
  | Class_field_desc.Pcf_method arg -> view arg
  | _ -> View.error

let pcf_val'const view value =
  let concrete = Class_field_desc.to_concrete value in
  match concrete with
  | Class_field_desc.Pcf_val arg -> view arg
  | _ -> View.error

let cfval'const view value =
  let parent_concrete = Class_field.to_concrete value in
  let desc = parent_concrete.Class_field.pcf_desc in
  let concrete = Class_field_desc.to_concrete desc in
  match concrete with
  | Class_field_desc.Pcf_val arg -> view arg
  | _ -> View.error

let pcf_inherit'const view value =
  let concrete = Class_field_desc.to_concrete value in
  match concrete with
  | Class_field_desc.Pcf_inherit (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let cfinherit'const view value =
  let parent_concrete = Class_field.to_concrete value in
  let desc = parent_concrete.Class_field.pcf_desc in
  let concrete = Class_field_desc.to_concrete desc in
  match concrete with
  | Class_field_desc.Pcf_inherit (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let pcf_attributes'match view value =
  let concrete = Class_field.to_concrete value in
  view concrete.Class_field.pcf_attributes

let pcf_loc'match view value =
  let concrete = Class_field.to_concrete value in
  view concrete.Class_field.pcf_loc

let pcf_desc'match view value =
  let concrete = Class_field.to_concrete value in
  view concrete.Class_field.pcf_desc

let pcstr_fields'match view value =
  let concrete = Class_structure.to_concrete value in
  view concrete.Class_structure.pcstr_fields

let pcstr_self'match view value =
  let concrete = Class_structure.to_concrete value in
  view concrete.Class_structure.pcstr_self

let pcl_open'const view value =
  let concrete = Class_expr_desc.to_concrete value in
  match concrete with
  | Class_expr_desc.Pcl_open (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let ceopen'const view value =
  let parent_concrete = Class_expr.to_concrete value in
  let desc = parent_concrete.Class_expr.pcl_desc in
  let concrete = Class_expr_desc.to_concrete desc in
  match concrete with
  | Class_expr_desc.Pcl_open (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let pcl_extension'const view value =
  let concrete = Class_expr_desc.to_concrete value in
  match concrete with
  | Class_expr_desc.Pcl_extension arg -> view arg
  | _ -> View.error

let ceextension'const view value =
  let parent_concrete = Class_expr.to_concrete value in
  let desc = parent_concrete.Class_expr.pcl_desc in
  let concrete = Class_expr_desc.to_concrete desc in
  match concrete with
  | Class_expr_desc.Pcl_extension arg -> view arg
  | _ -> View.error

let pcl_constraint'const view value =
  let concrete = Class_expr_desc.to_concrete value in
  match concrete with
  | Class_expr_desc.Pcl_constraint (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ceconstraint'const view value =
  let parent_concrete = Class_expr.to_concrete value in
  let desc = parent_concrete.Class_expr.pcl_desc in
  let concrete = Class_expr_desc.to_concrete desc in
  match concrete with
  | Class_expr_desc.Pcl_constraint (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pcl_let'const view value =
  let concrete = Class_expr_desc.to_concrete value in
  match concrete with
  | Class_expr_desc.Pcl_let (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let celet'const view value =
  let parent_concrete = Class_expr.to_concrete value in
  let desc = parent_concrete.Class_expr.pcl_desc in
  let concrete = Class_expr_desc.to_concrete desc in
  match concrete with
  | Class_expr_desc.Pcl_let (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let pcl_apply'const view value =
  let concrete = Class_expr_desc.to_concrete value in
  match concrete with
  | Class_expr_desc.Pcl_apply (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ceapply'const view value =
  let parent_concrete = Class_expr.to_concrete value in
  let desc = parent_concrete.Class_expr.pcl_desc in
  let concrete = Class_expr_desc.to_concrete desc in
  match concrete with
  | Class_expr_desc.Pcl_apply (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pcl_fun'const view value =
  let concrete = Class_expr_desc.to_concrete value in
  match concrete with
  | Class_expr_desc.Pcl_fun (arg0, arg1, arg2, arg3) -> view (arg0, arg1, arg2, arg3)
  | _ -> View.error

let cefun'const view value =
  let parent_concrete = Class_expr.to_concrete value in
  let desc = parent_concrete.Class_expr.pcl_desc in
  let concrete = Class_expr_desc.to_concrete desc in
  match concrete with
  | Class_expr_desc.Pcl_fun (arg0, arg1, arg2, arg3) -> view (arg0, arg1, arg2, arg3)
  | _ -> View.error

let pcl_structure'const view value =
  let concrete = Class_expr_desc.to_concrete value in
  match concrete with
  | Class_expr_desc.Pcl_structure arg -> view arg
  | _ -> View.error

let cestructure'const view value =
  let parent_concrete = Class_expr.to_concrete value in
  let desc = parent_concrete.Class_expr.pcl_desc in
  let concrete = Class_expr_desc.to_concrete desc in
  match concrete with
  | Class_expr_desc.Pcl_structure arg -> view arg
  | _ -> View.error

let pcl_constr'const view value =
  let concrete = Class_expr_desc.to_concrete value in
  match concrete with
  | Class_expr_desc.Pcl_constr (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ceconstr'const view value =
  let parent_concrete = Class_expr.to_concrete value in
  let desc = parent_concrete.Class_expr.pcl_desc in
  let concrete = Class_expr_desc.to_concrete desc in
  match concrete with
  | Class_expr_desc.Pcl_constr (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pcl_attributes'match view value =
  let concrete = Class_expr.to_concrete value in
  view concrete.Class_expr.pcl_attributes

let pcl_loc'match view value =
  let concrete = Class_expr.to_concrete value in
  view concrete.Class_expr.pcl_loc

let pcl_desc'match view value =
  let concrete = Class_expr.to_concrete value in
  view concrete.Class_expr.pcl_desc

let class_type_declaration'const view value =
  let concrete0 = Class_type_declaration.to_concrete value in
  view concrete0

let class_description'const view value =
  let concrete0 = Class_description.to_concrete value in
  view concrete0

let pci_attributes'match view value =
  let concrete = Class_infos.to_concrete value in
  view concrete.Class_infos.pci_attributes

let pci_loc'match view value =
  let concrete = Class_infos.to_concrete value in
  view concrete.Class_infos.pci_loc

let pci_expr'match view value =
  let concrete = Class_infos.to_concrete value in
  view concrete.Class_infos.pci_expr

let pci_name'match view value =
  let concrete = Class_infos.to_concrete value in
  view concrete.Class_infos.pci_name

let pci_params'match view value =
  let concrete = Class_infos.to_concrete value in
  view concrete.Class_infos.pci_params

let pci_virt'match view value =
  let concrete = Class_infos.to_concrete value in
  view concrete.Class_infos.pci_virt

let pctf_extension'const view value =
  let concrete = Class_type_field_desc.to_concrete value in
  match concrete with
  | Class_type_field_desc.Pctf_extension arg -> view arg
  | _ -> View.error

let ctfextension'const view value =
  let parent_concrete = Class_type_field.to_concrete value in
  let desc = parent_concrete.Class_type_field.pctf_desc in
  let concrete = Class_type_field_desc.to_concrete desc in
  match concrete with
  | Class_type_field_desc.Pctf_extension arg -> view arg
  | _ -> View.error

let pctf_attribute'const view value =
  let concrete = Class_type_field_desc.to_concrete value in
  match concrete with
  | Class_type_field_desc.Pctf_attribute arg -> view arg
  | _ -> View.error

let ctfattribute'const view value =
  let parent_concrete = Class_type_field.to_concrete value in
  let desc = parent_concrete.Class_type_field.pctf_desc in
  let concrete = Class_type_field_desc.to_concrete desc in
  match concrete with
  | Class_type_field_desc.Pctf_attribute arg -> view arg
  | _ -> View.error

let pctf_constraint'const view value =
  let concrete = Class_type_field_desc.to_concrete value in
  match concrete with
  | Class_type_field_desc.Pctf_constraint arg -> view arg
  | _ -> View.error

let ctfconstraint'const view value =
  let parent_concrete = Class_type_field.to_concrete value in
  let desc = parent_concrete.Class_type_field.pctf_desc in
  let concrete = Class_type_field_desc.to_concrete desc in
  match concrete with
  | Class_type_field_desc.Pctf_constraint arg -> view arg
  | _ -> View.error

let pctf_method'const view value =
  let concrete = Class_type_field_desc.to_concrete value in
  match concrete with
  | Class_type_field_desc.Pctf_method arg -> view arg
  | _ -> View.error

let ctfmethod'const view value =
  let parent_concrete = Class_type_field.to_concrete value in
  let desc = parent_concrete.Class_type_field.pctf_desc in
  let concrete = Class_type_field_desc.to_concrete desc in
  match concrete with
  | Class_type_field_desc.Pctf_method arg -> view arg
  | _ -> View.error

let pctf_val'const view value =
  let concrete = Class_type_field_desc.to_concrete value in
  match concrete with
  | Class_type_field_desc.Pctf_val arg -> view arg
  | _ -> View.error

let ctfval'const view value =
  let parent_concrete = Class_type_field.to_concrete value in
  let desc = parent_concrete.Class_type_field.pctf_desc in
  let concrete = Class_type_field_desc.to_concrete desc in
  match concrete with
  | Class_type_field_desc.Pctf_val arg -> view arg
  | _ -> View.error

let pctf_inherit'const view value =
  let concrete = Class_type_field_desc.to_concrete value in
  match concrete with
  | Class_type_field_desc.Pctf_inherit arg -> view arg
  | _ -> View.error

let ctfinherit'const view value =
  let parent_concrete = Class_type_field.to_concrete value in
  let desc = parent_concrete.Class_type_field.pctf_desc in
  let concrete = Class_type_field_desc.to_concrete desc in
  match concrete with
  | Class_type_field_desc.Pctf_inherit arg -> view arg
  | _ -> View.error

let pctf_attributes'match view value =
  let concrete = Class_type_field.to_concrete value in
  view concrete.Class_type_field.pctf_attributes

let pctf_loc'match view value =
  let concrete = Class_type_field.to_concrete value in
  view concrete.Class_type_field.pctf_loc

let pctf_desc'match view value =
  let concrete = Class_type_field.to_concrete value in
  view concrete.Class_type_field.pctf_desc

let pcsig_fields'match view value =
  let concrete = Class_signature.to_concrete value in
  view concrete.Class_signature.pcsig_fields

let pcsig_self'match view value =
  let concrete = Class_signature.to_concrete value in
  view concrete.Class_signature.pcsig_self

let pcty_open'const view value =
  let concrete = Class_type_desc.to_concrete value in
  match concrete with
  | Class_type_desc.Pcty_open (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let ctopen'const view value =
  let parent_concrete = Class_type.to_concrete value in
  let desc = parent_concrete.Class_type.pcty_desc in
  let concrete = Class_type_desc.to_concrete desc in
  match concrete with
  | Class_type_desc.Pcty_open (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let pcty_extension'const view value =
  let concrete = Class_type_desc.to_concrete value in
  match concrete with
  | Class_type_desc.Pcty_extension arg -> view arg
  | _ -> View.error

let ctextension'const view value =
  let parent_concrete = Class_type.to_concrete value in
  let desc = parent_concrete.Class_type.pcty_desc in
  let concrete = Class_type_desc.to_concrete desc in
  match concrete with
  | Class_type_desc.Pcty_extension arg -> view arg
  | _ -> View.error

let pcty_arrow'const view value =
  let concrete = Class_type_desc.to_concrete value in
  match concrete with
  | Class_type_desc.Pcty_arrow (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let ctarrow'const view value =
  let parent_concrete = Class_type.to_concrete value in
  let desc = parent_concrete.Class_type.pcty_desc in
  let concrete = Class_type_desc.to_concrete desc in
  match concrete with
  | Class_type_desc.Pcty_arrow (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let pcty_signature'const view value =
  let concrete = Class_type_desc.to_concrete value in
  match concrete with
  | Class_type_desc.Pcty_signature arg -> view arg
  | _ -> View.error

let ctsignature'const view value =
  let parent_concrete = Class_type.to_concrete value in
  let desc = parent_concrete.Class_type.pcty_desc in
  let concrete = Class_type_desc.to_concrete desc in
  match concrete with
  | Class_type_desc.Pcty_signature arg -> view arg
  | _ -> View.error

let pcty_constr'const view value =
  let concrete = Class_type_desc.to_concrete value in
  match concrete with
  | Class_type_desc.Pcty_constr (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ctconstr'const view value =
  let parent_concrete = Class_type.to_concrete value in
  let desc = parent_concrete.Class_type.pcty_desc in
  let concrete = Class_type_desc.to_concrete desc in
  match concrete with
  | Class_type_desc.Pcty_constr (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pcty_attributes'match view value =
  let concrete = Class_type.to_concrete value in
  view concrete.Class_type.pcty_attributes

let pcty_loc'match view value =
  let concrete = Class_type.to_concrete value in
  view concrete.Class_type.pcty_loc

let pcty_desc'match view value =
  let concrete = Class_type.to_concrete value in
  view concrete.Class_type.pcty_desc

let pext_rebind'const view value =
  let concrete = Extension_constructor_kind.to_concrete value in
  match concrete with
  | Extension_constructor_kind.Pext_rebind arg -> view arg
  | _ -> View.error

let pext_decl'const view value =
  let concrete = Extension_constructor_kind.to_concrete value in
  match concrete with
  | Extension_constructor_kind.Pext_decl (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pext_attributes'match view value =
  let concrete = Extension_constructor.to_concrete value in
  view concrete.Extension_constructor.pext_attributes

let pext_loc'match view value =
  let concrete = Extension_constructor.to_concrete value in
  view concrete.Extension_constructor.pext_loc

let pext_kind'match view value =
  let concrete = Extension_constructor.to_concrete value in
  view concrete.Extension_constructor.pext_kind

let pext_name'match view value =
  let concrete = Extension_constructor.to_concrete value in
  view concrete.Extension_constructor.pext_name

let ptyext_attributes'match view value =
  let concrete = Type_extension.to_concrete value in
  view concrete.Type_extension.ptyext_attributes

let ptyext_private'match view value =
  let concrete = Type_extension.to_concrete value in
  view concrete.Type_extension.ptyext_private

let ptyext_constructors'match view value =
  let concrete = Type_extension.to_concrete value in
  view concrete.Type_extension.ptyext_constructors

let ptyext_params'match view value =
  let concrete = Type_extension.to_concrete value in
  view concrete.Type_extension.ptyext_params

let ptyext_path'match view value =
  let concrete = Type_extension.to_concrete value in
  view concrete.Type_extension.ptyext_path

let pcstr_record'const view value =
  let concrete = Constructor_arguments.to_concrete value in
  match concrete with
  | Constructor_arguments.Pcstr_record arg -> view arg
  | _ -> View.error

let pcstr_tuple'const view value =
  let concrete = Constructor_arguments.to_concrete value in
  match concrete with
  | Constructor_arguments.Pcstr_tuple arg -> view arg
  | _ -> View.error

let pcd_attributes'match view value =
  let concrete = Constructor_declaration.to_concrete value in
  view concrete.Constructor_declaration.pcd_attributes

let pcd_loc'match view value =
  let concrete = Constructor_declaration.to_concrete value in
  view concrete.Constructor_declaration.pcd_loc

let pcd_res'match view value =
  let concrete = Constructor_declaration.to_concrete value in
  view concrete.Constructor_declaration.pcd_res

let pcd_args'match view value =
  let concrete = Constructor_declaration.to_concrete value in
  view concrete.Constructor_declaration.pcd_args

let pcd_name'match view value =
  let concrete = Constructor_declaration.to_concrete value in
  view concrete.Constructor_declaration.pcd_name

let pld_attributes'match view value =
  let concrete = Label_declaration.to_concrete value in
  view concrete.Label_declaration.pld_attributes

let pld_loc'match view value =
  let concrete = Label_declaration.to_concrete value in
  view concrete.Label_declaration.pld_loc

let pld_type'match view value =
  let concrete = Label_declaration.to_concrete value in
  view concrete.Label_declaration.pld_type

let pld_mutable'match view value =
  let concrete = Label_declaration.to_concrete value in
  view concrete.Label_declaration.pld_mutable

let pld_name'match view value =
  let concrete = Label_declaration.to_concrete value in
  view concrete.Label_declaration.pld_name

let ptype_open'const value =
  let concrete = Type_kind.to_concrete value in
  match concrete with
  | Type_kind.Ptype_open -> View.ok
  | _ -> View.error

let ptype_record'const view value =
  let concrete = Type_kind.to_concrete value in
  match concrete with
  | Type_kind.Ptype_record arg -> view arg
  | _ -> View.error

let ptype_variant'const view value =
  let concrete = Type_kind.to_concrete value in
  match concrete with
  | Type_kind.Ptype_variant arg -> view arg
  | _ -> View.error

let ptype_abstract'const value =
  let concrete = Type_kind.to_concrete value in
  match concrete with
  | Type_kind.Ptype_abstract -> View.ok
  | _ -> View.error

let ptype_loc'match view value =
  let concrete = Type_declaration.to_concrete value in
  view concrete.Type_declaration.ptype_loc

let ptype_attributes'match view value =
  let concrete = Type_declaration.to_concrete value in
  view concrete.Type_declaration.ptype_attributes

let ptype_manifest'match view value =
  let concrete = Type_declaration.to_concrete value in
  view concrete.Type_declaration.ptype_manifest

let ptype_private'match view value =
  let concrete = Type_declaration.to_concrete value in
  view concrete.Type_declaration.ptype_private

let ptype_kind'match view value =
  let concrete = Type_declaration.to_concrete value in
  view concrete.Type_declaration.ptype_kind

let ptype_cstrs'match view value =
  let concrete = Type_declaration.to_concrete value in
  view concrete.Type_declaration.ptype_cstrs

let ptype_params'match view value =
  let concrete = Type_declaration.to_concrete value in
  view concrete.Type_declaration.ptype_params

let ptype_name'match view value =
  let concrete = Type_declaration.to_concrete value in
  view concrete.Type_declaration.ptype_name

let pval_loc'match view value =
  let concrete = Value_description.to_concrete value in
  view concrete.Value_description.pval_loc

let pval_attributes'match view value =
  let concrete = Value_description.to_concrete value in
  view concrete.Value_description.pval_attributes

let pval_prim'match view value =
  let concrete = Value_description.to_concrete value in
  view concrete.Value_description.pval_prim

let pval_type'match view value =
  let concrete = Value_description.to_concrete value in
  view concrete.Value_description.pval_type

let pval_name'match view value =
  let concrete = Value_description.to_concrete value in
  view concrete.Value_description.pval_name

let pc_rhs'match view value =
  let concrete = Case.to_concrete value in
  view concrete.Case.pc_rhs

let pc_guard'match view value =
  let concrete = Case.to_concrete value in
  view concrete.Case.pc_guard

let pc_lhs'match view value =
  let concrete = Case.to_concrete value in
  view concrete.Case.pc_lhs

let pexp_unreachable'const value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_unreachable -> View.ok
  | _ -> View.error

let eunreachable'const value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_unreachable -> View.ok
  | _ -> View.error

let pexp_extension'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_extension arg -> view arg
  | _ -> View.error

let eextension'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_extension arg -> view arg
  | _ -> View.error

let pexp_open'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_open (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let eopen'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_open (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let pexp_pack'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_pack arg -> view arg
  | _ -> View.error

let epack'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_pack arg -> view arg
  | _ -> View.error

let pexp_newtype'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_newtype (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let enewtype'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_newtype (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pexp_object'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_object arg -> view arg
  | _ -> View.error

let eobject'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_object arg -> view arg
  | _ -> View.error

let pexp_poly'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_poly (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let epoly'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_poly (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pexp_lazy'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_lazy arg -> view arg
  | _ -> View.error

let elazy'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_lazy arg -> view arg
  | _ -> View.error

let pexp_assert'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_assert arg -> view arg
  | _ -> View.error

let eassert'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_assert arg -> view arg
  | _ -> View.error

let pexp_letexception'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_letexception (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let eletexception'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_letexception (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pexp_letmodule'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_letmodule (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let eletmodule'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_letmodule (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let pexp_override'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_override arg -> view arg
  | _ -> View.error

let eoverride'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_override arg -> view arg
  | _ -> View.error

let pexp_setinstvar'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_setinstvar (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let esetinstvar'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_setinstvar (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pexp_new'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_new arg -> view arg
  | _ -> View.error

let enew'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_new arg -> view arg
  | _ -> View.error

let pexp_send'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_send (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let esend'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_send (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pexp_coerce'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_coerce (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let ecoerce'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_coerce (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let pexp_constraint'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_constraint (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let econstraint'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_constraint (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pexp_for'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_for (arg0, arg1, arg2, arg3, arg4) -> view (arg0, arg1, arg2, arg3, arg4)
  | _ -> View.error

let efor'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_for (arg0, arg1, arg2, arg3, arg4) -> view (arg0, arg1, arg2, arg3, arg4)
  | _ -> View.error

let pexp_while'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_while (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ewhile'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_while (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pexp_sequence'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_sequence (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let esequence'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_sequence (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pexp_ifthenelse'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_ifthenelse (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let eifthenelse'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_ifthenelse (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let pexp_array'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_array arg -> view arg
  | _ -> View.error

let earray'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_array arg -> view arg
  | _ -> View.error

let pexp_setfield'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_setfield (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let esetfield'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_setfield (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let pexp_field'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_field (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let efield'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_field (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pexp_record'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_record (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let erecord'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_record (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pexp_variant'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_variant (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let evariant'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_variant (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pexp_construct'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_construct (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let econstruct'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_construct (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pexp_tuple'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_tuple arg -> view arg
  | _ -> View.error

let etuple'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_tuple arg -> view arg
  | _ -> View.error

let pexp_try'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_try (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let etry'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_try (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pexp_match'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_match (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ematch'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_match (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pexp_apply'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_apply (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let eapply'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_apply (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pexp_fun'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_fun (arg0, arg1, arg2, arg3) -> view (arg0, arg1, arg2, arg3)
  | _ -> View.error

let efun'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_fun (arg0, arg1, arg2, arg3) -> view (arg0, arg1, arg2, arg3)
  | _ -> View.error

let pexp_function'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_function arg -> view arg
  | _ -> View.error

let efunction'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_function arg -> view arg
  | _ -> View.error

let pexp_let'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_let (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let elet'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_let (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let pexp_constant'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_constant arg -> view arg
  | _ -> View.error

let econstant'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_constant arg -> view arg
  | _ -> View.error

let pexp_ident'const view value =
  let concrete = Expression_desc.to_concrete value in
  match concrete with
  | Expression_desc.Pexp_ident arg -> view arg
  | _ -> View.error

let eident'const view value =
  let parent_concrete = Expression.to_concrete value in
  let desc = parent_concrete.Expression.pexp_desc in
  let concrete = Expression_desc.to_concrete desc in
  match concrete with
  | Expression_desc.Pexp_ident arg -> view arg
  | _ -> View.error

let pexp_attributes'match view value =
  let concrete = Expression.to_concrete value in
  view concrete.Expression.pexp_attributes

let pexp_loc'match view value =
  let concrete = Expression.to_concrete value in
  view concrete.Expression.pexp_loc

let pexp_desc'match view value =
  let concrete = Expression.to_concrete value in
  view concrete.Expression.pexp_desc

let ppat_open'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_open (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let popen'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_open (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ppat_extension'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_extension arg -> view arg
  | _ -> View.error

let pextension'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_extension arg -> view arg
  | _ -> View.error

let ppat_exception'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_exception arg -> view arg
  | _ -> View.error

let pexception'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_exception arg -> view arg
  | _ -> View.error

let ppat_unpack'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_unpack arg -> view arg
  | _ -> View.error

let punpack'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_unpack arg -> view arg
  | _ -> View.error

let ppat_lazy'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_lazy arg -> view arg
  | _ -> View.error

let plazy'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_lazy arg -> view arg
  | _ -> View.error

let ppat_type'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_type arg -> view arg
  | _ -> View.error

let ptype'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_type arg -> view arg
  | _ -> View.error

let ppat_constraint'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_constraint (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pconstraint'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_constraint (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ppat_or'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_or (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let por'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_or (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ppat_array'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_array arg -> view arg
  | _ -> View.error

let parray'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_array arg -> view arg
  | _ -> View.error

let ppat_record'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_record (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let precord'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_record (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ppat_variant'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_variant (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pvariant'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_variant (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ppat_construct'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_construct (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pconstruct'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_construct (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ppat_tuple'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_tuple arg -> view arg
  | _ -> View.error

let ptuple'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_tuple arg -> view arg
  | _ -> View.error

let ppat_interval'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_interval (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pinterval'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_interval (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ppat_constant'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_constant arg -> view arg
  | _ -> View.error

let pconstant'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_constant arg -> view arg
  | _ -> View.error

let ppat_alias'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_alias (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let palias'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_alias (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ppat_var'const view value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_var arg -> view arg
  | _ -> View.error

let pvar'const view value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_var arg -> view arg
  | _ -> View.error

let ppat_any'const value =
  let concrete = Pattern_desc.to_concrete value in
  match concrete with
  | Pattern_desc.Ppat_any -> View.ok
  | _ -> View.error

let pany'const value =
  let parent_concrete = Pattern.to_concrete value in
  let desc = parent_concrete.Pattern.ppat_desc in
  let concrete = Pattern_desc.to_concrete desc in
  match concrete with
  | Pattern_desc.Ppat_any -> View.ok
  | _ -> View.error

let ppat_attributes'match view value =
  let concrete = Pattern.to_concrete value in
  view concrete.Pattern.ppat_attributes

let ppat_loc'match view value =
  let concrete = Pattern.to_concrete value in
  view concrete.Pattern.ppat_loc

let ppat_desc'match view value =
  let concrete = Pattern.to_concrete value in
  view concrete.Pattern.ppat_desc

let oinherit'const view value =
  let concrete = Object_field.to_concrete value in
  match concrete with
  | Object_field.Oinherit arg -> view arg
  | _ -> View.error

let otag'const view value =
  let concrete = Object_field.to_concrete value in
  match concrete with
  | Object_field.Otag (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let rinherit'const view value =
  let concrete = Row_field.to_concrete value in
  match concrete with
  | Row_field.Rinherit arg -> view arg
  | _ -> View.error

let rtag'const view value =
  let concrete = Row_field.to_concrete value in
  match concrete with
  | Row_field.Rtag (arg0, arg1, arg2, arg3) -> view (arg0, arg1, arg2, arg3)
  | _ -> View.error

let package_type'const view value =
  let concrete0 = Package_type.to_concrete value in
  view concrete0

let ptyp_extension'const view value =
  let concrete = Core_type_desc.to_concrete value in
  match concrete with
  | Core_type_desc.Ptyp_extension arg -> view arg
  | _ -> View.error

let textension'const view value =
  let parent_concrete = Core_type.to_concrete value in
  let desc = parent_concrete.Core_type.ptyp_desc in
  let concrete = Core_type_desc.to_concrete desc in
  match concrete with
  | Core_type_desc.Ptyp_extension arg -> view arg
  | _ -> View.error

let ptyp_package'const view value =
  let concrete = Core_type_desc.to_concrete value in
  match concrete with
  | Core_type_desc.Ptyp_package arg -> view arg
  | _ -> View.error

let tpackage'const view value =
  let parent_concrete = Core_type.to_concrete value in
  let desc = parent_concrete.Core_type.ptyp_desc in
  let concrete = Core_type_desc.to_concrete desc in
  match concrete with
  | Core_type_desc.Ptyp_package arg -> view arg
  | _ -> View.error

let ptyp_poly'const view value =
  let concrete = Core_type_desc.to_concrete value in
  match concrete with
  | Core_type_desc.Ptyp_poly (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let tpoly'const view value =
  let parent_concrete = Core_type.to_concrete value in
  let desc = parent_concrete.Core_type.ptyp_desc in
  let concrete = Core_type_desc.to_concrete desc in
  match concrete with
  | Core_type_desc.Ptyp_poly (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ptyp_variant'const view value =
  let concrete = Core_type_desc.to_concrete value in
  match concrete with
  | Core_type_desc.Ptyp_variant (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let tvariant'const view value =
  let parent_concrete = Core_type.to_concrete value in
  let desc = parent_concrete.Core_type.ptyp_desc in
  let concrete = Core_type_desc.to_concrete desc in
  match concrete with
  | Core_type_desc.Ptyp_variant (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let ptyp_alias'const view value =
  let concrete = Core_type_desc.to_concrete value in
  match concrete with
  | Core_type_desc.Ptyp_alias (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let talias'const view value =
  let parent_concrete = Core_type.to_concrete value in
  let desc = parent_concrete.Core_type.ptyp_desc in
  let concrete = Core_type_desc.to_concrete desc in
  match concrete with
  | Core_type_desc.Ptyp_alias (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ptyp_class'const view value =
  let concrete = Core_type_desc.to_concrete value in
  match concrete with
  | Core_type_desc.Ptyp_class (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let tclass'const view value =
  let parent_concrete = Core_type.to_concrete value in
  let desc = parent_concrete.Core_type.ptyp_desc in
  let concrete = Core_type_desc.to_concrete desc in
  match concrete with
  | Core_type_desc.Ptyp_class (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ptyp_object'const view value =
  let concrete = Core_type_desc.to_concrete value in
  match concrete with
  | Core_type_desc.Ptyp_object (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let tobject'const view value =
  let parent_concrete = Core_type.to_concrete value in
  let desc = parent_concrete.Core_type.ptyp_desc in
  let concrete = Core_type_desc.to_concrete desc in
  match concrete with
  | Core_type_desc.Ptyp_object (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ptyp_constr'const view value =
  let concrete = Core_type_desc.to_concrete value in
  match concrete with
  | Core_type_desc.Ptyp_constr (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let tconstr'const view value =
  let parent_concrete = Core_type.to_concrete value in
  let desc = parent_concrete.Core_type.ptyp_desc in
  let concrete = Core_type_desc.to_concrete desc in
  match concrete with
  | Core_type_desc.Ptyp_constr (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ptyp_tuple'const view value =
  let concrete = Core_type_desc.to_concrete value in
  match concrete with
  | Core_type_desc.Ptyp_tuple arg -> view arg
  | _ -> View.error

let ttuple'const view value =
  let parent_concrete = Core_type.to_concrete value in
  let desc = parent_concrete.Core_type.ptyp_desc in
  let concrete = Core_type_desc.to_concrete desc in
  match concrete with
  | Core_type_desc.Ptyp_tuple arg -> view arg
  | _ -> View.error

let ptyp_arrow'const view value =
  let concrete = Core_type_desc.to_concrete value in
  match concrete with
  | Core_type_desc.Ptyp_arrow (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let tarrow'const view value =
  let parent_concrete = Core_type.to_concrete value in
  let desc = parent_concrete.Core_type.ptyp_desc in
  let concrete = Core_type_desc.to_concrete desc in
  match concrete with
  | Core_type_desc.Ptyp_arrow (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
  | _ -> View.error

let ptyp_var'const view value =
  let concrete = Core_type_desc.to_concrete value in
  match concrete with
  | Core_type_desc.Ptyp_var arg -> view arg
  | _ -> View.error

let tvar'const view value =
  let parent_concrete = Core_type.to_concrete value in
  let desc = parent_concrete.Core_type.ptyp_desc in
  let concrete = Core_type_desc.to_concrete desc in
  match concrete with
  | Core_type_desc.Ptyp_var arg -> view arg
  | _ -> View.error

let ptyp_any'const value =
  let concrete = Core_type_desc.to_concrete value in
  match concrete with
  | Core_type_desc.Ptyp_any -> View.ok
  | _ -> View.error

let tany'const value =
  let parent_concrete = Core_type.to_concrete value in
  let desc = parent_concrete.Core_type.ptyp_desc in
  let concrete = Core_type_desc.to_concrete desc in
  match concrete with
  | Core_type_desc.Ptyp_any -> View.ok
  | _ -> View.error

let ptyp_attributes'match view value =
  let concrete = Core_type.to_concrete value in
  view concrete.Core_type.ptyp_attributes

let ptyp_loc'match view value =
  let concrete = Core_type.to_concrete value in
  view concrete.Core_type.ptyp_loc

let ptyp_desc'match view value =
  let concrete = Core_type.to_concrete value in
  view concrete.Core_type.ptyp_desc

let ppat'const view value =
  let concrete = Payload.to_concrete value in
  match concrete with
  | Payload.PPat (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ptyp'const view value =
  let concrete = Payload.to_concrete value in
  match concrete with
  | Payload.PTyp arg -> view arg
  | _ -> View.error

let psig'const view value =
  let concrete = Payload.to_concrete value in
  match concrete with
  | Payload.PSig arg -> view arg
  | _ -> View.error

let pstr'const view value =
  let concrete = Payload.to_concrete value in
  match concrete with
  | Payload.PStr arg -> view arg
  | _ -> View.error

let attributes'const view value =
  let concrete0 = Attributes.to_concrete value in
  view concrete0

let extension'const view value =
  let concrete0 = Extension.to_concrete value in
  view concrete0

let attribute'const view value =
  let concrete0 = Attribute.to_concrete value in
  view concrete0

let pconst_float'const view value =
  let concrete = Constant.to_concrete value in
  match concrete with
  | Constant.Pconst_float (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pconst_string'const view value =
  let concrete = Constant.to_concrete value in
  match concrete with
  | Constant.Pconst_string (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let pconst_char'const view value =
  let concrete = Constant.to_concrete value in
  match concrete with
  | Constant.Pconst_char arg -> view arg
  | _ -> View.error

let pconst_integer'const view value =
  let concrete = Constant.to_concrete value in
  match concrete with
  | Constant.Pconst_integer (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let invariant'const value =
  let concrete = Variance.to_concrete value in
  match concrete with
  | Variance.Invariant -> View.ok
  | _ -> View.error

let contravariant'const value =
  let concrete = Variance.to_concrete value in
  match concrete with
  | Variance.Contravariant -> View.ok
  | _ -> View.error

let covariant'const value =
  let concrete = Variance.to_concrete value in
  match concrete with
  | Variance.Covariant -> View.ok
  | _ -> View.error

let optional'const view value =
  let concrete = Arg_label.to_concrete value in
  match concrete with
  | Arg_label.Optional arg -> view arg
  | _ -> View.error

let labelled'const view value =
  let concrete = Arg_label.to_concrete value in
  match concrete with
  | Arg_label.Labelled arg -> view arg
  | _ -> View.error

let nolabel'const value =
  let concrete = Arg_label.to_concrete value in
  match concrete with
  | Arg_label.Nolabel -> View.ok
  | _ -> View.error

let open'const value =
  let concrete = Closed_flag.to_concrete value in
  match concrete with
  | Closed_flag.Open -> View.ok
  | _ -> View.error

let closed'const value =
  let concrete = Closed_flag.to_concrete value in
  match concrete with
  | Closed_flag.Closed -> View.ok
  | _ -> View.error

let fresh'const value =
  let concrete = Override_flag.to_concrete value in
  match concrete with
  | Override_flag.Fresh -> View.ok
  | _ -> View.error

let override'const value =
  let concrete = Override_flag.to_concrete value in
  match concrete with
  | Override_flag.Override -> View.ok
  | _ -> View.error

let concrete'const value =
  let concrete = Virtual_flag.to_concrete value in
  match concrete with
  | Virtual_flag.Concrete -> View.ok
  | _ -> View.error

let virtual'const value =
  let concrete = Virtual_flag.to_concrete value in
  match concrete with
  | Virtual_flag.Virtual -> View.ok
  | _ -> View.error

let mutable'const value =
  let concrete = Mutable_flag.to_concrete value in
  match concrete with
  | Mutable_flag.Mutable -> View.ok
  | _ -> View.error

let immutable'const value =
  let concrete = Mutable_flag.to_concrete value in
  match concrete with
  | Mutable_flag.Immutable -> View.ok
  | _ -> View.error

let public'const value =
  let concrete = Private_flag.to_concrete value in
  match concrete with
  | Private_flag.Public -> View.ok
  | _ -> View.error

let private'const value =
  let concrete = Private_flag.to_concrete value in
  match concrete with
  | Private_flag.Private -> View.ok
  | _ -> View.error

let downto'const value =
  let concrete = Direction_flag.to_concrete value in
  match concrete with
  | Direction_flag.Downto -> View.ok
  | _ -> View.error

let upto'const value =
  let concrete = Direction_flag.to_concrete value in
  match concrete with
  | Direction_flag.Upto -> View.ok
  | _ -> View.error

let recursive'const value =
  let concrete = Rec_flag.to_concrete value in
  match concrete with
  | Rec_flag.Recursive -> View.ok
  | _ -> View.error

let nonrecursive'const value =
  let concrete = Rec_flag.to_concrete value in
  match concrete with
  | Rec_flag.Nonrecursive -> View.ok
  | _ -> View.error

let longident_loc'const view value =
  let concrete0 = Longident_loc.to_concrete value in
  view concrete0

let lapply'const view value =
  let concrete = Longident.to_concrete value in
  match concrete with
  | Longident.Lapply (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let ldot'const view value =
  let concrete = Longident.to_concrete value in
  match concrete with
  | Longident.Ldot (arg0, arg1) -> view (arg0, arg1)
  | _ -> View.error

let lident'const view value =
  let concrete = Longident.to_concrete value in
  match concrete with
  | Longident.Lident arg -> view arg
  | _ -> View.error
(*$*)
