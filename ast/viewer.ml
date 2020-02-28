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

module Loc_types : LOC_TYPES = struct
  let txt'match view value = view (Astlib.Loc.txt value)
  let loc'match view value = view (Astlib.Loc.loc value)

  let loc_start'match view value = view (Astlib.Location.start value)
  let loc_end'match view value = view (Astlib.Location.end_ value)
  let loc_ghost'match view value = view (Astlib.Location.ghost value)

  let pos_fname'match view value = view (Astlib.Position.fname value)
  let pos_lnum'match view value = view (Astlib.Position.lnum value)
  let pos_bol'match view value = view (Astlib.Position.bol value)
  let pos_cnum'match view value = view (Astlib.Position.cnum value)
end

(*$ Ppx_ast_cinaps.print_viewer_ml () *)
module Unstable_for_testing = struct
  open Versions.Unstable_for_testing
  include Loc_types

  let conversion_failed name = Raise.conversion_failed ~version:"Unstable_for_testing" name

  let pdir_bool'const view value =
    let concrete =
      match Directive_argument.to_concrete value with
      | None -> conversion_failed "directive_argument"
      | Some n -> n
    in
    match concrete with
    | Directive_argument.Pdir_bool arg -> view arg
    | _ -> View.error

  let pdir_ident'const view value =
    let concrete =
      match Directive_argument.to_concrete value with
      | None -> conversion_failed "directive_argument"
      | Some n -> n
    in
    match concrete with
    | Directive_argument.Pdir_ident arg -> view arg
    | _ -> View.error

  let pdir_int'const view value =
    let concrete =
      match Directive_argument.to_concrete value with
      | None -> conversion_failed "directive_argument"
      | Some n -> n
    in
    match concrete with
    | Directive_argument.Pdir_int (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pdir_string'const view value =
    let concrete =
      match Directive_argument.to_concrete value with
      | None -> conversion_failed "directive_argument"
      | Some n -> n
    in
    match concrete with
    | Directive_argument.Pdir_string arg -> view arg
    | _ -> View.error

  let pdir_none'const value =
    let concrete =
      match Directive_argument.to_concrete value with
      | None -> conversion_failed "directive_argument"
      | Some n -> n
    in
    match concrete with
    | Directive_argument.Pdir_none -> View.ok
    | _ -> View.error

  let ptop_dir'const view value =
    let concrete =
      match Toplevel_phrase.to_concrete value with
      | None -> conversion_failed "toplevel_phrase"
      | Some n -> n
    in
    match concrete with
    | Toplevel_phrase.Ptop_dir (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ptop_def'const view value =
    let concrete =
      match Toplevel_phrase.to_concrete value with
      | None -> conversion_failed "toplevel_phrase"
      | Some n -> n
    in
    match concrete with
    | Toplevel_phrase.Ptop_def arg -> view arg
    | _ -> View.error

  let pmb_loc'match view value =
    let concrete =
      match Module_binding.to_concrete value with
      | None -> conversion_failed "module_binding"
      | Some n -> n
    in
    view concrete.Module_binding.pmb_loc

  let pmb_attributes'match view value =
    let concrete =
      match Module_binding.to_concrete value with
      | None -> conversion_failed "module_binding"
      | Some n -> n
    in
    view concrete.Module_binding.pmb_attributes

  let pmb_expr'match view value =
    let concrete =
      match Module_binding.to_concrete value with
      | None -> conversion_failed "module_binding"
      | Some n -> n
    in
    view concrete.Module_binding.pmb_expr

  let pmb_name'match view value =
    let concrete =
      match Module_binding.to_concrete value with
      | None -> conversion_failed "module_binding"
      | Some n -> n
    in
    view concrete.Module_binding.pmb_name

  let pvb_loc'match view value =
    let concrete =
      match Value_binding.to_concrete value with
      | None -> conversion_failed "value_binding"
      | Some n -> n
    in
    view concrete.Value_binding.pvb_loc

  let pvb_attributes'match view value =
    let concrete =
      match Value_binding.to_concrete value with
      | None -> conversion_failed "value_binding"
      | Some n -> n
    in
    view concrete.Value_binding.pvb_attributes

  let pvb_expr'match view value =
    let concrete =
      match Value_binding.to_concrete value with
      | None -> conversion_failed "value_binding"
      | Some n -> n
    in
    view concrete.Value_binding.pvb_expr

  let pvb_pat'match view value =
    let concrete =
      match Value_binding.to_concrete value with
      | None -> conversion_failed "value_binding"
      | Some n -> n
    in
    view concrete.Value_binding.pvb_pat

  let pstr_extension'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_extension (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pstr_attribute'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_attribute arg -> view arg
    | _ -> View.error

  let pstr_include'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_include arg -> view arg
    | _ -> View.error

  let pstr_class_type'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_class_type arg -> view arg
    | _ -> View.error

  let pstr_class'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_class arg -> view arg
    | _ -> View.error

  let pstr_open'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_open arg -> view arg
    | _ -> View.error

  let pstr_modtype'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_modtype arg -> view arg
    | _ -> View.error

  let pstr_recmodule'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_recmodule arg -> view arg
    | _ -> View.error

  let pstr_module'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_module arg -> view arg
    | _ -> View.error

  let pstr_exception'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_exception arg -> view arg
    | _ -> View.error

  let pstr_typext'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_typext arg -> view arg
    | _ -> View.error

  let pstr_type'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_type (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pstr_primitive'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_primitive arg -> view arg
    | _ -> View.error

  let pstr_value'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_value (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pstr_eval'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_eval (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pstr_loc'match view value =
    let concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    view concrete.Structure_item.pstr_loc

  let pstr_desc'match view value =
    let concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    view concrete.Structure_item.pstr_desc

  let pmod_extension'const view value =
    let parent_concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Module_expr.pmod_desc in
    let concrete =
      match Module_expr_desc.to_concrete desc with
      | None -> conversion_failed "module_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Module_expr_desc.Pmod_extension arg -> view arg
    | _ -> View.error

  let pmod_unpack'const view value =
    let parent_concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Module_expr.pmod_desc in
    let concrete =
      match Module_expr_desc.to_concrete desc with
      | None -> conversion_failed "module_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Module_expr_desc.Pmod_unpack arg -> view arg
    | _ -> View.error

  let pmod_constraint'const view value =
    let parent_concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Module_expr.pmod_desc in
    let concrete =
      match Module_expr_desc.to_concrete desc with
      | None -> conversion_failed "module_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Module_expr_desc.Pmod_constraint (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pmod_apply'const view value =
    let parent_concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Module_expr.pmod_desc in
    let concrete =
      match Module_expr_desc.to_concrete desc with
      | None -> conversion_failed "module_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Module_expr_desc.Pmod_apply (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pmod_functor'const view value =
    let parent_concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Module_expr.pmod_desc in
    let concrete =
      match Module_expr_desc.to_concrete desc with
      | None -> conversion_failed "module_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Module_expr_desc.Pmod_functor (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pmod_structure'const view value =
    let parent_concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Module_expr.pmod_desc in
    let concrete =
      match Module_expr_desc.to_concrete desc with
      | None -> conversion_failed "module_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Module_expr_desc.Pmod_structure arg -> view arg
    | _ -> View.error

  let pmod_ident'const view value =
    let parent_concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Module_expr.pmod_desc in
    let concrete =
      match Module_expr_desc.to_concrete desc with
      | None -> conversion_failed "module_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Module_expr_desc.Pmod_ident arg -> view arg
    | _ -> View.error

  let pmod_attributes'match view value =
    let concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    view concrete.Module_expr.pmod_attributes

  let pmod_loc'match view value =
    let concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    view concrete.Module_expr.pmod_loc

  let pmod_desc'match view value =
    let concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    view concrete.Module_expr.pmod_desc

  let pwith_modsubst'const view value =
    let concrete =
      match With_constraint.to_concrete value with
      | None -> conversion_failed "with_constraint"
      | Some n -> n
    in
    match concrete with
    | With_constraint.Pwith_modsubst (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pwith_typesubst'const view value =
    let concrete =
      match With_constraint.to_concrete value with
      | None -> conversion_failed "with_constraint"
      | Some n -> n
    in
    match concrete with
    | With_constraint.Pwith_typesubst (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pwith_module'const view value =
    let concrete =
      match With_constraint.to_concrete value with
      | None -> conversion_failed "with_constraint"
      | Some n -> n
    in
    match concrete with
    | With_constraint.Pwith_module (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pwith_type'const view value =
    let concrete =
      match With_constraint.to_concrete value with
      | None -> conversion_failed "with_constraint"
      | Some n -> n
    in
    match concrete with
    | With_constraint.Pwith_type (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pincl_attributes'match view value =
    let concrete =
      match Include_infos.to_concrete value with
      | None -> conversion_failed "include_infos"
      | Some n -> n
    in
    view concrete.Include_infos.pincl_attributes

  let pincl_loc'match view value =
    let concrete =
      match Include_infos.to_concrete value with
      | None -> conversion_failed "include_infos"
      | Some n -> n
    in
    view concrete.Include_infos.pincl_loc

  let pincl_mod'match view value =
    let concrete =
      match Include_infos.to_concrete value with
      | None -> conversion_failed "include_infos"
      | Some n -> n
    in
    view concrete.Include_infos.pincl_mod

  let popen_attributes'match view value =
    let concrete =
      match Open_description.to_concrete value with
      | None -> conversion_failed "open_description"
      | Some n -> n
    in
    view concrete.Open_description.popen_attributes

  let popen_loc'match view value =
    let concrete =
      match Open_description.to_concrete value with
      | None -> conversion_failed "open_description"
      | Some n -> n
    in
    view concrete.Open_description.popen_loc

  let popen_override'match view value =
    let concrete =
      match Open_description.to_concrete value with
      | None -> conversion_failed "open_description"
      | Some n -> n
    in
    view concrete.Open_description.popen_override

  let popen_lid'match view value =
    let concrete =
      match Open_description.to_concrete value with
      | None -> conversion_failed "open_description"
      | Some n -> n
    in
    view concrete.Open_description.popen_lid

  let pmtd_loc'match view value =
    let concrete =
      match Module_type_declaration.to_concrete value with
      | None -> conversion_failed "module_type_declaration"
      | Some n -> n
    in
    view concrete.Module_type_declaration.pmtd_loc

  let pmtd_attributes'match view value =
    let concrete =
      match Module_type_declaration.to_concrete value with
      | None -> conversion_failed "module_type_declaration"
      | Some n -> n
    in
    view concrete.Module_type_declaration.pmtd_attributes

  let pmtd_type'match view value =
    let concrete =
      match Module_type_declaration.to_concrete value with
      | None -> conversion_failed "module_type_declaration"
      | Some n -> n
    in
    view concrete.Module_type_declaration.pmtd_type

  let pmtd_name'match view value =
    let concrete =
      match Module_type_declaration.to_concrete value with
      | None -> conversion_failed "module_type_declaration"
      | Some n -> n
    in
    view concrete.Module_type_declaration.pmtd_name

  let pmd_loc'match view value =
    let concrete =
      match Module_declaration.to_concrete value with
      | None -> conversion_failed "module_declaration"
      | Some n -> n
    in
    view concrete.Module_declaration.pmd_loc

  let pmd_attributes'match view value =
    let concrete =
      match Module_declaration.to_concrete value with
      | None -> conversion_failed "module_declaration"
      | Some n -> n
    in
    view concrete.Module_declaration.pmd_attributes

  let pmd_type'match view value =
    let concrete =
      match Module_declaration.to_concrete value with
      | None -> conversion_failed "module_declaration"
      | Some n -> n
    in
    view concrete.Module_declaration.pmd_type

  let pmd_name'match view value =
    let concrete =
      match Module_declaration.to_concrete value with
      | None -> conversion_failed "module_declaration"
      | Some n -> n
    in
    view concrete.Module_declaration.pmd_name

  let psig_extension'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_extension (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let psig_attribute'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_attribute arg -> view arg
    | _ -> View.error

  let psig_class_type'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_class_type arg -> view arg
    | _ -> View.error

  let psig_class'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_class arg -> view arg
    | _ -> View.error

  let psig_include'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_include arg -> view arg
    | _ -> View.error

  let psig_open'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_open arg -> view arg
    | _ -> View.error

  let psig_modtype'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_modtype arg -> view arg
    | _ -> View.error

  let psig_recmodule'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_recmodule arg -> view arg
    | _ -> View.error

  let psig_module'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_module arg -> view arg
    | _ -> View.error

  let psig_exception'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_exception arg -> view arg
    | _ -> View.error

  let psig_typext'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_typext arg -> view arg
    | _ -> View.error

  let psig_type'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_type (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let psig_value'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_value arg -> view arg
    | _ -> View.error

  let psig_loc'match view value =
    let concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    view concrete.Signature_item.psig_loc

  let psig_desc'match view value =
    let concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    view concrete.Signature_item.psig_desc

  let pmty_alias'const view value =
    let parent_concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    let desc = parent_concrete.Module_type.pmty_desc in
    let concrete =
      match Module_type_desc.to_concrete desc with
      | None -> conversion_failed "module_type_desc"
      | Some n -> n
    in
    match concrete with
    | Module_type_desc.Pmty_alias arg -> view arg
    | _ -> View.error

  let pmty_extension'const view value =
    let parent_concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    let desc = parent_concrete.Module_type.pmty_desc in
    let concrete =
      match Module_type_desc.to_concrete desc with
      | None -> conversion_failed "module_type_desc"
      | Some n -> n
    in
    match concrete with
    | Module_type_desc.Pmty_extension arg -> view arg
    | _ -> View.error

  let pmty_typeof'const view value =
    let parent_concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    let desc = parent_concrete.Module_type.pmty_desc in
    let concrete =
      match Module_type_desc.to_concrete desc with
      | None -> conversion_failed "module_type_desc"
      | Some n -> n
    in
    match concrete with
    | Module_type_desc.Pmty_typeof arg -> view arg
    | _ -> View.error

  let pmty_with'const view value =
    let parent_concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    let desc = parent_concrete.Module_type.pmty_desc in
    let concrete =
      match Module_type_desc.to_concrete desc with
      | None -> conversion_failed "module_type_desc"
      | Some n -> n
    in
    match concrete with
    | Module_type_desc.Pmty_with (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pmty_functor'const view value =
    let parent_concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    let desc = parent_concrete.Module_type.pmty_desc in
    let concrete =
      match Module_type_desc.to_concrete desc with
      | None -> conversion_failed "module_type_desc"
      | Some n -> n
    in
    match concrete with
    | Module_type_desc.Pmty_functor (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pmty_signature'const view value =
    let parent_concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    let desc = parent_concrete.Module_type.pmty_desc in
    let concrete =
      match Module_type_desc.to_concrete desc with
      | None -> conversion_failed "module_type_desc"
      | Some n -> n
    in
    match concrete with
    | Module_type_desc.Pmty_signature arg -> view arg
    | _ -> View.error

  let pmty_ident'const view value =
    let parent_concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    let desc = parent_concrete.Module_type.pmty_desc in
    let concrete =
      match Module_type_desc.to_concrete desc with
      | None -> conversion_failed "module_type_desc"
      | Some n -> n
    in
    match concrete with
    | Module_type_desc.Pmty_ident arg -> view arg
    | _ -> View.error

  let pmty_attributes'match view value =
    let concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    view concrete.Module_type.pmty_attributes

  let pmty_loc'match view value =
    let concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    view concrete.Module_type.pmty_loc

  let pmty_desc'match view value =
    let concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    view concrete.Module_type.pmty_desc

  let cfk_concrete'const view value =
    let concrete =
      match Class_field_kind.to_concrete value with
      | None -> conversion_failed "class_field_kind"
      | Some n -> n
    in
    match concrete with
    | Class_field_kind.Cfk_concrete (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let cfk_virtual'const view value =
    let concrete =
      match Class_field_kind.to_concrete value with
      | None -> conversion_failed "class_field_kind"
      | Some n -> n
    in
    match concrete with
    | Class_field_kind.Cfk_virtual arg -> view arg
    | _ -> View.error

  let pcf_extension'const view value =
    let parent_concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_field.pcf_desc in
    let concrete =
      match Class_field_desc.to_concrete desc with
      | None -> conversion_failed "class_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_field_desc.Pcf_extension arg -> view arg
    | _ -> View.error

  let pcf_attribute'const view value =
    let parent_concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_field.pcf_desc in
    let concrete =
      match Class_field_desc.to_concrete desc with
      | None -> conversion_failed "class_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_field_desc.Pcf_attribute arg -> view arg
    | _ -> View.error

  let pcf_initializer'const view value =
    let parent_concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_field.pcf_desc in
    let concrete =
      match Class_field_desc.to_concrete desc with
      | None -> conversion_failed "class_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_field_desc.Pcf_initializer arg -> view arg
    | _ -> View.error

  let pcf_constraint'const view value =
    let parent_concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_field.pcf_desc in
    let concrete =
      match Class_field_desc.to_concrete desc with
      | None -> conversion_failed "class_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_field_desc.Pcf_constraint arg -> view arg
    | _ -> View.error

  let pcf_method'const view value =
    let parent_concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_field.pcf_desc in
    let concrete =
      match Class_field_desc.to_concrete desc with
      | None -> conversion_failed "class_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_field_desc.Pcf_method arg -> view arg
    | _ -> View.error

  let pcf_val'const view value =
    let parent_concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_field.pcf_desc in
    let concrete =
      match Class_field_desc.to_concrete desc with
      | None -> conversion_failed "class_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_field_desc.Pcf_val arg -> view arg
    | _ -> View.error

  let pcf_inherit'const view value =
    let parent_concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_field.pcf_desc in
    let concrete =
      match Class_field_desc.to_concrete desc with
      | None -> conversion_failed "class_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_field_desc.Pcf_inherit (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pcf_attributes'match view value =
    let concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    view concrete.Class_field.pcf_attributes

  let pcf_loc'match view value =
    let concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    view concrete.Class_field.pcf_loc

  let pcf_desc'match view value =
    let concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    view concrete.Class_field.pcf_desc

  let pcstr_fields'match view value =
    let concrete =
      match Class_structure.to_concrete value with
      | None -> conversion_failed "class_structure"
      | Some n -> n
    in
    view concrete.Class_structure.pcstr_fields

  let pcstr_self'match view value =
    let concrete =
      match Class_structure.to_concrete value with
      | None -> conversion_failed "class_structure"
      | Some n -> n
    in
    view concrete.Class_structure.pcstr_self

  let pcl_open'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_open (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pcl_extension'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_extension arg -> view arg
    | _ -> View.error

  let pcl_constraint'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_constraint (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pcl_let'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_let (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pcl_apply'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_apply (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pcl_fun'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_fun (arg0, arg1, arg2, arg3) -> view (arg0, arg1, arg2, arg3)
    | _ -> View.error

  let pcl_structure'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_structure arg -> view arg
    | _ -> View.error

  let pcl_constr'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_constr (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pcl_attributes'match view value =
    let concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    view concrete.Class_expr.pcl_attributes

  let pcl_loc'match view value =
    let concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    view concrete.Class_expr.pcl_loc

  let pcl_desc'match view value =
    let concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    view concrete.Class_expr.pcl_desc

  let pci_attributes'match view value =
    let concrete =
      match Class_infos.to_concrete value with
      | None -> conversion_failed "class_infos"
      | Some n -> n
    in
    view concrete.Class_infos.pci_attributes

  let pci_loc'match view value =
    let concrete =
      match Class_infos.to_concrete value with
      | None -> conversion_failed "class_infos"
      | Some n -> n
    in
    view concrete.Class_infos.pci_loc

  let pci_expr'match view value =
    let concrete =
      match Class_infos.to_concrete value with
      | None -> conversion_failed "class_infos"
      | Some n -> n
    in
    view concrete.Class_infos.pci_expr

  let pci_name'match view value =
    let concrete =
      match Class_infos.to_concrete value with
      | None -> conversion_failed "class_infos"
      | Some n -> n
    in
    view concrete.Class_infos.pci_name

  let pci_params'match view value =
    let concrete =
      match Class_infos.to_concrete value with
      | None -> conversion_failed "class_infos"
      | Some n -> n
    in
    view concrete.Class_infos.pci_params

  let pci_virt'match view value =
    let concrete =
      match Class_infos.to_concrete value with
      | None -> conversion_failed "class_infos"
      | Some n -> n
    in
    view concrete.Class_infos.pci_virt

  let pctf_extension'const view value =
    let parent_concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type_field.pctf_desc in
    let concrete =
      match Class_type_field_desc.to_concrete desc with
      | None -> conversion_failed "class_type_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_field_desc.Pctf_extension arg -> view arg
    | _ -> View.error

  let pctf_attribute'const view value =
    let parent_concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type_field.pctf_desc in
    let concrete =
      match Class_type_field_desc.to_concrete desc with
      | None -> conversion_failed "class_type_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_field_desc.Pctf_attribute arg -> view arg
    | _ -> View.error

  let pctf_constraint'const view value =
    let parent_concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type_field.pctf_desc in
    let concrete =
      match Class_type_field_desc.to_concrete desc with
      | None -> conversion_failed "class_type_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_field_desc.Pctf_constraint arg -> view arg
    | _ -> View.error

  let pctf_method'const view value =
    let parent_concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type_field.pctf_desc in
    let concrete =
      match Class_type_field_desc.to_concrete desc with
      | None -> conversion_failed "class_type_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_field_desc.Pctf_method arg -> view arg
    | _ -> View.error

  let pctf_val'const view value =
    let parent_concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type_field.pctf_desc in
    let concrete =
      match Class_type_field_desc.to_concrete desc with
      | None -> conversion_failed "class_type_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_field_desc.Pctf_val arg -> view arg
    | _ -> View.error

  let pctf_inherit'const view value =
    let parent_concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type_field.pctf_desc in
    let concrete =
      match Class_type_field_desc.to_concrete desc with
      | None -> conversion_failed "class_type_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_field_desc.Pctf_inherit arg -> view arg
    | _ -> View.error

  let pctf_attributes'match view value =
    let concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    view concrete.Class_type_field.pctf_attributes

  let pctf_loc'match view value =
    let concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    view concrete.Class_type_field.pctf_loc

  let pctf_desc'match view value =
    let concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    view concrete.Class_type_field.pctf_desc

  let pcsig_fields'match view value =
    let concrete =
      match Class_signature.to_concrete value with
      | None -> conversion_failed "class_signature"
      | Some n -> n
    in
    view concrete.Class_signature.pcsig_fields

  let pcsig_self'match view value =
    let concrete =
      match Class_signature.to_concrete value with
      | None -> conversion_failed "class_signature"
      | Some n -> n
    in
    view concrete.Class_signature.pcsig_self

  let pcty_open'const view value =
    let parent_concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type.pcty_desc in
    let concrete =
      match Class_type_desc.to_concrete desc with
      | None -> conversion_failed "class_type_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_desc.Pcty_open (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pcty_extension'const view value =
    let parent_concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type.pcty_desc in
    let concrete =
      match Class_type_desc.to_concrete desc with
      | None -> conversion_failed "class_type_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_desc.Pcty_extension arg -> view arg
    | _ -> View.error

  let pcty_arrow'const view value =
    let parent_concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type.pcty_desc in
    let concrete =
      match Class_type_desc.to_concrete desc with
      | None -> conversion_failed "class_type_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_desc.Pcty_arrow (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pcty_signature'const view value =
    let parent_concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type.pcty_desc in
    let concrete =
      match Class_type_desc.to_concrete desc with
      | None -> conversion_failed "class_type_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_desc.Pcty_signature arg -> view arg
    | _ -> View.error

  let pcty_constr'const view value =
    let parent_concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type.pcty_desc in
    let concrete =
      match Class_type_desc.to_concrete desc with
      | None -> conversion_failed "class_type_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_desc.Pcty_constr (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pcty_attributes'match view value =
    let concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    view concrete.Class_type.pcty_attributes

  let pcty_loc'match view value =
    let concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    view concrete.Class_type.pcty_loc

  let pcty_desc'match view value =
    let concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    view concrete.Class_type.pcty_desc

  let pext_rebind'const view value =
    let concrete =
      match Extension_constructor_kind.to_concrete value with
      | None -> conversion_failed "extension_constructor_kind"
      | Some n -> n
    in
    match concrete with
    | Extension_constructor_kind.Pext_rebind arg -> view arg
    | _ -> View.error

  let pext_decl'const view value =
    let concrete =
      match Extension_constructor_kind.to_concrete value with
      | None -> conversion_failed "extension_constructor_kind"
      | Some n -> n
    in
    match concrete with
    | Extension_constructor_kind.Pext_decl (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pext_attributes'match view value =
    let concrete =
      match Extension_constructor.to_concrete value with
      | None -> conversion_failed "extension_constructor"
      | Some n -> n
    in
    view concrete.Extension_constructor.pext_attributes

  let pext_loc'match view value =
    let concrete =
      match Extension_constructor.to_concrete value with
      | None -> conversion_failed "extension_constructor"
      | Some n -> n
    in
    view concrete.Extension_constructor.pext_loc

  let pext_kind'match view value =
    let concrete =
      match Extension_constructor.to_concrete value with
      | None -> conversion_failed "extension_constructor"
      | Some n -> n
    in
    view concrete.Extension_constructor.pext_kind

  let pext_name'match view value =
    let concrete =
      match Extension_constructor.to_concrete value with
      | None -> conversion_failed "extension_constructor"
      | Some n -> n
    in
    view concrete.Extension_constructor.pext_name

  let ptyext_attributes'match view value =
    let concrete =
      match Type_extension.to_concrete value with
      | None -> conversion_failed "type_extension"
      | Some n -> n
    in
    view concrete.Type_extension.ptyext_attributes

  let ptyext_private'match view value =
    let concrete =
      match Type_extension.to_concrete value with
      | None -> conversion_failed "type_extension"
      | Some n -> n
    in
    view concrete.Type_extension.ptyext_private

  let ptyext_constructors'match view value =
    let concrete =
      match Type_extension.to_concrete value with
      | None -> conversion_failed "type_extension"
      | Some n -> n
    in
    view concrete.Type_extension.ptyext_constructors

  let ptyext_params'match view value =
    let concrete =
      match Type_extension.to_concrete value with
      | None -> conversion_failed "type_extension"
      | Some n -> n
    in
    view concrete.Type_extension.ptyext_params

  let ptyext_path'match view value =
    let concrete =
      match Type_extension.to_concrete value with
      | None -> conversion_failed "type_extension"
      | Some n -> n
    in
    view concrete.Type_extension.ptyext_path

  let pcstr_record'const view value =
    let concrete =
      match Constructor_arguments.to_concrete value with
      | None -> conversion_failed "constructor_arguments"
      | Some n -> n
    in
    match concrete with
    | Constructor_arguments.Pcstr_record arg -> view arg
    | _ -> View.error

  let pcstr_tuple'const view value =
    let concrete =
      match Constructor_arguments.to_concrete value with
      | None -> conversion_failed "constructor_arguments"
      | Some n -> n
    in
    match concrete with
    | Constructor_arguments.Pcstr_tuple arg -> view arg
    | _ -> View.error

  let pcd_attributes'match view value =
    let concrete =
      match Constructor_declaration.to_concrete value with
      | None -> conversion_failed "constructor_declaration"
      | Some n -> n
    in
    view concrete.Constructor_declaration.pcd_attributes

  let pcd_loc'match view value =
    let concrete =
      match Constructor_declaration.to_concrete value with
      | None -> conversion_failed "constructor_declaration"
      | Some n -> n
    in
    view concrete.Constructor_declaration.pcd_loc

  let pcd_res'match view value =
    let concrete =
      match Constructor_declaration.to_concrete value with
      | None -> conversion_failed "constructor_declaration"
      | Some n -> n
    in
    view concrete.Constructor_declaration.pcd_res

  let pcd_args'match view value =
    let concrete =
      match Constructor_declaration.to_concrete value with
      | None -> conversion_failed "constructor_declaration"
      | Some n -> n
    in
    view concrete.Constructor_declaration.pcd_args

  let pcd_name'match view value =
    let concrete =
      match Constructor_declaration.to_concrete value with
      | None -> conversion_failed "constructor_declaration"
      | Some n -> n
    in
    view concrete.Constructor_declaration.pcd_name

  let pld_attributes'match view value =
    let concrete =
      match Label_declaration.to_concrete value with
      | None -> conversion_failed "label_declaration"
      | Some n -> n
    in
    view concrete.Label_declaration.pld_attributes

  let pld_loc'match view value =
    let concrete =
      match Label_declaration.to_concrete value with
      | None -> conversion_failed "label_declaration"
      | Some n -> n
    in
    view concrete.Label_declaration.pld_loc

  let pld_type'match view value =
    let concrete =
      match Label_declaration.to_concrete value with
      | None -> conversion_failed "label_declaration"
      | Some n -> n
    in
    view concrete.Label_declaration.pld_type

  let pld_mutable'match view value =
    let concrete =
      match Label_declaration.to_concrete value with
      | None -> conversion_failed "label_declaration"
      | Some n -> n
    in
    view concrete.Label_declaration.pld_mutable

  let pld_name'match view value =
    let concrete =
      match Label_declaration.to_concrete value with
      | None -> conversion_failed "label_declaration"
      | Some n -> n
    in
    view concrete.Label_declaration.pld_name

  let ptype_open'const value =
    let concrete =
      match Type_kind.to_concrete value with
      | None -> conversion_failed "type_kind"
      | Some n -> n
    in
    match concrete with
    | Type_kind.Ptype_open -> View.ok
    | _ -> View.error

  let ptype_record'const view value =
    let concrete =
      match Type_kind.to_concrete value with
      | None -> conversion_failed "type_kind"
      | Some n -> n
    in
    match concrete with
    | Type_kind.Ptype_record arg -> view arg
    | _ -> View.error

  let ptype_variant'const view value =
    let concrete =
      match Type_kind.to_concrete value with
      | None -> conversion_failed "type_kind"
      | Some n -> n
    in
    match concrete with
    | Type_kind.Ptype_variant arg -> view arg
    | _ -> View.error

  let ptype_abstract'const value =
    let concrete =
      match Type_kind.to_concrete value with
      | None -> conversion_failed "type_kind"
      | Some n -> n
    in
    match concrete with
    | Type_kind.Ptype_abstract -> View.ok
    | _ -> View.error

  let ptype_loc'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_loc

  let ptype_attributes'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_attributes

  let ptype_manifest'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_manifest

  let ptype_private'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_private

  let ptype_kind'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_kind

  let ptype_cstrs'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_cstrs

  let ptype_params'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_params

  let ptype_name'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_name

  let pval_loc'match view value =
    let concrete =
      match Value_description.to_concrete value with
      | None -> conversion_failed "value_description"
      | Some n -> n
    in
    view concrete.Value_description.pval_loc

  let pval_attributes'match view value =
    let concrete =
      match Value_description.to_concrete value with
      | None -> conversion_failed "value_description"
      | Some n -> n
    in
    view concrete.Value_description.pval_attributes

  let pval_prim'match view value =
    let concrete =
      match Value_description.to_concrete value with
      | None -> conversion_failed "value_description"
      | Some n -> n
    in
    view concrete.Value_description.pval_prim

  let pval_type'match view value =
    let concrete =
      match Value_description.to_concrete value with
      | None -> conversion_failed "value_description"
      | Some n -> n
    in
    view concrete.Value_description.pval_type

  let pval_name'match view value =
    let concrete =
      match Value_description.to_concrete value with
      | None -> conversion_failed "value_description"
      | Some n -> n
    in
    view concrete.Value_description.pval_name

  let pc_rhs'match view value =
    let concrete =
      match Case.to_concrete value with
      | None -> conversion_failed "case"
      | Some n -> n
    in
    view concrete.Case.pc_rhs

  let pc_guard'match view value =
    let concrete =
      match Case.to_concrete value with
      | None -> conversion_failed "case"
      | Some n -> n
    in
    view concrete.Case.pc_guard

  let pc_lhs'match view value =
    let concrete =
      match Case.to_concrete value with
      | None -> conversion_failed "case"
      | Some n -> n
    in
    view concrete.Case.pc_lhs

  let pexp_unreachable'const value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_unreachable -> View.ok
    | _ -> View.error

  let pexp_extension'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_extension arg -> view arg
    | _ -> View.error

  let pexp_open'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_open (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pexp_pack'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_pack arg -> view arg
    | _ -> View.error

  let pexp_newtype'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_newtype (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_object'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_object arg -> view arg
    | _ -> View.error

  let pexp_poly'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_poly (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_lazy'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_lazy arg -> view arg
    | _ -> View.error

  let pexp_assert'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_assert arg -> view arg
    | _ -> View.error

  let pexp_letexception'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_letexception (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_letmodule'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_letmodule (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pexp_override'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_override arg -> view arg
    | _ -> View.error

  let pexp_setinstvar'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_setinstvar (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_new'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_new arg -> view arg
    | _ -> View.error

  let pexp_send'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_send (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_coerce'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_coerce (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pexp_constraint'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_constraint (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_for'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_for (arg0, arg1, arg2, arg3, arg4) -> view (arg0, arg1, arg2, arg3, arg4)
    | _ -> View.error

  let pexp_while'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_while (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_sequence'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_sequence (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_ifthenelse'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_ifthenelse (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pexp_array'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_array arg -> view arg
    | _ -> View.error

  let pexp_setfield'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_setfield (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pexp_field'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_field (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_record'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_record (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_variant'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_variant (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_construct'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_construct (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_tuple'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_tuple arg -> view arg
    | _ -> View.error

  let pexp_try'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_try (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_match'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_match (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_apply'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_apply (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_fun'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_fun (arg0, arg1, arg2, arg3) -> view (arg0, arg1, arg2, arg3)
    | _ -> View.error

  let pexp_function'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_function arg -> view arg
    | _ -> View.error

  let pexp_let'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_let (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pexp_constant'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_constant arg -> view arg
    | _ -> View.error

  let pexp_ident'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_ident arg -> view arg
    | _ -> View.error

  let pexp_attributes'match view value =
    let concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    view concrete.Expression.pexp_attributes

  let pexp_loc'match view value =
    let concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    view concrete.Expression.pexp_loc

  let pexp_desc'match view value =
    let concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    view concrete.Expression.pexp_desc

  let ppat_open'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_open (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ppat_extension'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_extension arg -> view arg
    | _ -> View.error

  let ppat_exception'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_exception arg -> view arg
    | _ -> View.error

  let ppat_unpack'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_unpack arg -> view arg
    | _ -> View.error

  let ppat_lazy'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_lazy arg -> view arg
    | _ -> View.error

  let ppat_type'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_type arg -> view arg
    | _ -> View.error

  let ppat_constraint'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_constraint (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ppat_or'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_or (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ppat_array'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_array arg -> view arg
    | _ -> View.error

  let ppat_record'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_record (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ppat_variant'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_variant (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ppat_construct'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_construct (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ppat_tuple'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_tuple arg -> view arg
    | _ -> View.error

  let ppat_interval'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_interval (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ppat_constant'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_constant arg -> view arg
    | _ -> View.error

  let ppat_alias'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_alias (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ppat_var'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_var arg -> view arg
    | _ -> View.error

  let ppat_any'const value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_any -> View.ok
    | _ -> View.error

  let ppat_attributes'match view value =
    let concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    view concrete.Pattern.ppat_attributes

  let ppat_loc'match view value =
    let concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    view concrete.Pattern.ppat_loc

  let ppat_desc'match view value =
    let concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    view concrete.Pattern.ppat_desc

  let oinherit'const view value =
    let concrete =
      match Object_field.to_concrete value with
      | None -> conversion_failed "object_field"
      | Some n -> n
    in
    match concrete with
    | Object_field.Oinherit arg -> view arg
    | _ -> View.error

  let otag'const view value =
    let concrete =
      match Object_field.to_concrete value with
      | None -> conversion_failed "object_field"
      | Some n -> n
    in
    match concrete with
    | Object_field.Otag (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let rinherit'const view value =
    let concrete =
      match Row_field.to_concrete value with
      | None -> conversion_failed "row_field"
      | Some n -> n
    in
    match concrete with
    | Row_field.Rinherit arg -> view arg
    | _ -> View.error

  let rtag'const view value =
    let concrete =
      match Row_field.to_concrete value with
      | None -> conversion_failed "row_field"
      | Some n -> n
    in
    match concrete with
    | Row_field.Rtag (arg0, arg1, arg2, arg3) -> view (arg0, arg1, arg2, arg3)
    | _ -> View.error

  let ptyp_extension'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_extension arg -> view arg
    | _ -> View.error

  let ptyp_package'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_package arg -> view arg
    | _ -> View.error

  let ptyp_poly'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_poly (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ptyp_variant'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_variant (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let ptyp_alias'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_alias (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ptyp_class'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_class (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ptyp_object'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_object (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ptyp_constr'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_constr (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ptyp_tuple'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_tuple arg -> view arg
    | _ -> View.error

  let ptyp_arrow'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_arrow (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let ptyp_var'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_var arg -> view arg
    | _ -> View.error

  let ptyp_any'const value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_any -> View.ok
    | _ -> View.error

  let ptyp_attributes'match view value =
    let concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    view concrete.Core_type.ptyp_attributes

  let ptyp_loc'match view value =
    let concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    view concrete.Core_type.ptyp_loc

  let ptyp_desc'match view value =
    let concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    view concrete.Core_type.ptyp_desc

  let ppat'const view value =
    let concrete =
      match Payload.to_concrete value with
      | None -> conversion_failed "payload"
      | Some n -> n
    in
    match concrete with
    | Payload.PPat (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ptyp'const view value =
    let concrete =
      match Payload.to_concrete value with
      | None -> conversion_failed "payload"
      | Some n -> n
    in
    match concrete with
    | Payload.PTyp arg -> view arg
    | _ -> View.error

  let psig'const view value =
    let concrete =
      match Payload.to_concrete value with
      | None -> conversion_failed "payload"
      | Some n -> n
    in
    match concrete with
    | Payload.PSig arg -> view arg
    | _ -> View.error

  let pstr'const view value =
    let concrete =
      match Payload.to_concrete value with
      | None -> conversion_failed "payload"
      | Some n -> n
    in
    match concrete with
    | Payload.PStr arg -> view arg
    | _ -> View.error

  let pconst_float'const view value =
    let concrete =
      match Constant.to_concrete value with
      | None -> conversion_failed "constant"
      | Some n -> n
    in
    match concrete with
    | Constant.Pconst_float (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pconst_string'const view value =
    let concrete =
      match Constant.to_concrete value with
      | None -> conversion_failed "constant"
      | Some n -> n
    in
    match concrete with
    | Constant.Pconst_string (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pconst_char'const view value =
    let concrete =
      match Constant.to_concrete value with
      | None -> conversion_failed "constant"
      | Some n -> n
    in
    match concrete with
    | Constant.Pconst_char arg -> view arg
    | _ -> View.error

  let pconst_integer'const view value =
    let concrete =
      match Constant.to_concrete value with
      | None -> conversion_failed "constant"
      | Some n -> n
    in
    match concrete with
    | Constant.Pconst_integer (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let invariant'const value =
    let concrete =
      match Variance.to_concrete value with
      | None -> conversion_failed "variance"
      | Some n -> n
    in
    match concrete with
    | Variance.Invariant -> View.ok
    | _ -> View.error

  let contravariant'const value =
    let concrete =
      match Variance.to_concrete value with
      | None -> conversion_failed "variance"
      | Some n -> n
    in
    match concrete with
    | Variance.Contravariant -> View.ok
    | _ -> View.error

  let covariant'const value =
    let concrete =
      match Variance.to_concrete value with
      | None -> conversion_failed "variance"
      | Some n -> n
    in
    match concrete with
    | Variance.Covariant -> View.ok
    | _ -> View.error

  let optional'const view value =
    let concrete =
      match Arg_label.to_concrete value with
      | None -> conversion_failed "arg_label"
      | Some n -> n
    in
    match concrete with
    | Arg_label.Optional arg -> view arg
    | _ -> View.error

  let labelled'const view value =
    let concrete =
      match Arg_label.to_concrete value with
      | None -> conversion_failed "arg_label"
      | Some n -> n
    in
    match concrete with
    | Arg_label.Labelled arg -> view arg
    | _ -> View.error

  let nolabel'const value =
    let concrete =
      match Arg_label.to_concrete value with
      | None -> conversion_failed "arg_label"
      | Some n -> n
    in
    match concrete with
    | Arg_label.Nolabel -> View.ok
    | _ -> View.error

  let open'const value =
    let concrete =
      match Closed_flag.to_concrete value with
      | None -> conversion_failed "closed_flag"
      | Some n -> n
    in
    match concrete with
    | Closed_flag.Open -> View.ok
    | _ -> View.error

  let closed'const value =
    let concrete =
      match Closed_flag.to_concrete value with
      | None -> conversion_failed "closed_flag"
      | Some n -> n
    in
    match concrete with
    | Closed_flag.Closed -> View.ok
    | _ -> View.error

  let fresh'const value =
    let concrete =
      match Override_flag.to_concrete value with
      | None -> conversion_failed "override_flag"
      | Some n -> n
    in
    match concrete with
    | Override_flag.Fresh -> View.ok
    | _ -> View.error

  let override'const value =
    let concrete =
      match Override_flag.to_concrete value with
      | None -> conversion_failed "override_flag"
      | Some n -> n
    in
    match concrete with
    | Override_flag.Override -> View.ok
    | _ -> View.error

  let concrete'const value =
    let concrete =
      match Virtual_flag.to_concrete value with
      | None -> conversion_failed "virtual_flag"
      | Some n -> n
    in
    match concrete with
    | Virtual_flag.Concrete -> View.ok
    | _ -> View.error

  let virtual'const value =
    let concrete =
      match Virtual_flag.to_concrete value with
      | None -> conversion_failed "virtual_flag"
      | Some n -> n
    in
    match concrete with
    | Virtual_flag.Virtual -> View.ok
    | _ -> View.error

  let mutable'const value =
    let concrete =
      match Mutable_flag.to_concrete value with
      | None -> conversion_failed "mutable_flag"
      | Some n -> n
    in
    match concrete with
    | Mutable_flag.Mutable -> View.ok
    | _ -> View.error

  let immutable'const value =
    let concrete =
      match Mutable_flag.to_concrete value with
      | None -> conversion_failed "mutable_flag"
      | Some n -> n
    in
    match concrete with
    | Mutable_flag.Immutable -> View.ok
    | _ -> View.error

  let public'const value =
    let concrete =
      match Private_flag.to_concrete value with
      | None -> conversion_failed "private_flag"
      | Some n -> n
    in
    match concrete with
    | Private_flag.Public -> View.ok
    | _ -> View.error

  let private'const value =
    let concrete =
      match Private_flag.to_concrete value with
      | None -> conversion_failed "private_flag"
      | Some n -> n
    in
    match concrete with
    | Private_flag.Private -> View.ok
    | _ -> View.error

  let downto'const value =
    let concrete =
      match Direction_flag.to_concrete value with
      | None -> conversion_failed "direction_flag"
      | Some n -> n
    in
    match concrete with
    | Direction_flag.Downto -> View.ok
    | _ -> View.error

  let upto'const value =
    let concrete =
      match Direction_flag.to_concrete value with
      | None -> conversion_failed "direction_flag"
      | Some n -> n
    in
    match concrete with
    | Direction_flag.Upto -> View.ok
    | _ -> View.error

  let recursive'const value =
    let concrete =
      match Rec_flag.to_concrete value with
      | None -> conversion_failed "rec_flag"
      | Some n -> n
    in
    match concrete with
    | Rec_flag.Recursive -> View.ok
    | _ -> View.error

  let nonrecursive'const value =
    let concrete =
      match Rec_flag.to_concrete value with
      | None -> conversion_failed "rec_flag"
      | Some n -> n
    in
    match concrete with
    | Rec_flag.Nonrecursive -> View.ok
    | _ -> View.error

  let lapply'const view value =
    let concrete =
      match Longident.to_concrete value with
      | None -> conversion_failed "longident"
      | Some n -> n
    in
    match concrete with
    | Longident.Lapply (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ldot'const view value =
    let concrete =
      match Longident.to_concrete value with
      | None -> conversion_failed "longident"
      | Some n -> n
    in
    match concrete with
    | Longident.Ldot (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let lident'const view value =
    let concrete =
      match Longident.to_concrete value with
      | None -> conversion_failed "longident"
      | Some n -> n
    in
    match concrete with
    | Longident.Lident arg -> view arg
    | _ -> View.error
end

module V4_07 = struct
  open Versions.V4_07
  include Loc_types

  let conversion_failed name = Raise.conversion_failed ~version:"V4_07" name

  let lident'const view value =
    let concrete =
      match Longident.to_concrete value with
      | None -> conversion_failed "longident"
      | Some n -> n
    in
    match concrete with
    | Longident.Lident arg -> view arg
    | _ -> View.error

  let ldot'const view value =
    let concrete =
      match Longident.to_concrete value with
      | None -> conversion_failed "longident"
      | Some n -> n
    in
    match concrete with
    | Longident.Ldot (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let lapply'const view value =
    let concrete =
      match Longident.to_concrete value with
      | None -> conversion_failed "longident"
      | Some n -> n
    in
    match concrete with
    | Longident.Lapply (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let nonrecursive'const value =
    let concrete =
      match Rec_flag.to_concrete value with
      | None -> conversion_failed "rec_flag"
      | Some n -> n
    in
    match concrete with
    | Rec_flag.Nonrecursive -> View.ok
    | _ -> View.error

  let recursive'const value =
    let concrete =
      match Rec_flag.to_concrete value with
      | None -> conversion_failed "rec_flag"
      | Some n -> n
    in
    match concrete with
    | Rec_flag.Recursive -> View.ok
    | _ -> View.error

  let upto'const value =
    let concrete =
      match Direction_flag.to_concrete value with
      | None -> conversion_failed "direction_flag"
      | Some n -> n
    in
    match concrete with
    | Direction_flag.Upto -> View.ok
    | _ -> View.error

  let downto'const value =
    let concrete =
      match Direction_flag.to_concrete value with
      | None -> conversion_failed "direction_flag"
      | Some n -> n
    in
    match concrete with
    | Direction_flag.Downto -> View.ok
    | _ -> View.error

  let private'const value =
    let concrete =
      match Private_flag.to_concrete value with
      | None -> conversion_failed "private_flag"
      | Some n -> n
    in
    match concrete with
    | Private_flag.Private -> View.ok
    | _ -> View.error

  let public'const value =
    let concrete =
      match Private_flag.to_concrete value with
      | None -> conversion_failed "private_flag"
      | Some n -> n
    in
    match concrete with
    | Private_flag.Public -> View.ok
    | _ -> View.error

  let immutable'const value =
    let concrete =
      match Mutable_flag.to_concrete value with
      | None -> conversion_failed "mutable_flag"
      | Some n -> n
    in
    match concrete with
    | Mutable_flag.Immutable -> View.ok
    | _ -> View.error

  let mutable'const value =
    let concrete =
      match Mutable_flag.to_concrete value with
      | None -> conversion_failed "mutable_flag"
      | Some n -> n
    in
    match concrete with
    | Mutable_flag.Mutable -> View.ok
    | _ -> View.error

  let virtual'const value =
    let concrete =
      match Virtual_flag.to_concrete value with
      | None -> conversion_failed "virtual_flag"
      | Some n -> n
    in
    match concrete with
    | Virtual_flag.Virtual -> View.ok
    | _ -> View.error

  let concrete'const value =
    let concrete =
      match Virtual_flag.to_concrete value with
      | None -> conversion_failed "virtual_flag"
      | Some n -> n
    in
    match concrete with
    | Virtual_flag.Concrete -> View.ok
    | _ -> View.error

  let override'const value =
    let concrete =
      match Override_flag.to_concrete value with
      | None -> conversion_failed "override_flag"
      | Some n -> n
    in
    match concrete with
    | Override_flag.Override -> View.ok
    | _ -> View.error

  let fresh'const value =
    let concrete =
      match Override_flag.to_concrete value with
      | None -> conversion_failed "override_flag"
      | Some n -> n
    in
    match concrete with
    | Override_flag.Fresh -> View.ok
    | _ -> View.error

  let closed'const value =
    let concrete =
      match Closed_flag.to_concrete value with
      | None -> conversion_failed "closed_flag"
      | Some n -> n
    in
    match concrete with
    | Closed_flag.Closed -> View.ok
    | _ -> View.error

  let open'const value =
    let concrete =
      match Closed_flag.to_concrete value with
      | None -> conversion_failed "closed_flag"
      | Some n -> n
    in
    match concrete with
    | Closed_flag.Open -> View.ok
    | _ -> View.error

  let nolabel'const value =
    let concrete =
      match Arg_label.to_concrete value with
      | None -> conversion_failed "arg_label"
      | Some n -> n
    in
    match concrete with
    | Arg_label.Nolabel -> View.ok
    | _ -> View.error

  let labelled'const view value =
    let concrete =
      match Arg_label.to_concrete value with
      | None -> conversion_failed "arg_label"
      | Some n -> n
    in
    match concrete with
    | Arg_label.Labelled arg -> view arg
    | _ -> View.error

  let optional'const view value =
    let concrete =
      match Arg_label.to_concrete value with
      | None -> conversion_failed "arg_label"
      | Some n -> n
    in
    match concrete with
    | Arg_label.Optional arg -> view arg
    | _ -> View.error

  let covariant'const value =
    let concrete =
      match Variance.to_concrete value with
      | None -> conversion_failed "variance"
      | Some n -> n
    in
    match concrete with
    | Variance.Covariant -> View.ok
    | _ -> View.error

  let contravariant'const value =
    let concrete =
      match Variance.to_concrete value with
      | None -> conversion_failed "variance"
      | Some n -> n
    in
    match concrete with
    | Variance.Contravariant -> View.ok
    | _ -> View.error

  let invariant'const value =
    let concrete =
      match Variance.to_concrete value with
      | None -> conversion_failed "variance"
      | Some n -> n
    in
    match concrete with
    | Variance.Invariant -> View.ok
    | _ -> View.error

  let pconst_integer'const view value =
    let concrete =
      match Constant.to_concrete value with
      | None -> conversion_failed "constant"
      | Some n -> n
    in
    match concrete with
    | Constant.Pconst_integer (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pconst_char'const view value =
    let concrete =
      match Constant.to_concrete value with
      | None -> conversion_failed "constant"
      | Some n -> n
    in
    match concrete with
    | Constant.Pconst_char arg -> view arg
    | _ -> View.error

  let pconst_string'const view value =
    let concrete =
      match Constant.to_concrete value with
      | None -> conversion_failed "constant"
      | Some n -> n
    in
    match concrete with
    | Constant.Pconst_string (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pconst_float'const view value =
    let concrete =
      match Constant.to_concrete value with
      | None -> conversion_failed "constant"
      | Some n -> n
    in
    match concrete with
    | Constant.Pconst_float (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pstr'const view value =
    let concrete =
      match Payload.to_concrete value with
      | None -> conversion_failed "payload"
      | Some n -> n
    in
    match concrete with
    | Payload.PStr arg -> view arg
    | _ -> View.error

  let psig'const view value =
    let concrete =
      match Payload.to_concrete value with
      | None -> conversion_failed "payload"
      | Some n -> n
    in
    match concrete with
    | Payload.PSig arg -> view arg
    | _ -> View.error

  let ptyp'const view value =
    let concrete =
      match Payload.to_concrete value with
      | None -> conversion_failed "payload"
      | Some n -> n
    in
    match concrete with
    | Payload.PTyp arg -> view arg
    | _ -> View.error

  let ppat'const view value =
    let concrete =
      match Payload.to_concrete value with
      | None -> conversion_failed "payload"
      | Some n -> n
    in
    match concrete with
    | Payload.PPat (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ptyp_desc'match view value =
    let concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    view concrete.Core_type.ptyp_desc

  let ptyp_loc'match view value =
    let concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    view concrete.Core_type.ptyp_loc

  let ptyp_attributes'match view value =
    let concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    view concrete.Core_type.ptyp_attributes

  let ptyp_any'const value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_any -> View.ok
    | _ -> View.error

  let ptyp_var'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_var arg -> view arg
    | _ -> View.error

  let ptyp_arrow'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_arrow (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let ptyp_tuple'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_tuple arg -> view arg
    | _ -> View.error

  let ptyp_constr'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_constr (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ptyp_object'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_object (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ptyp_class'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_class (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ptyp_alias'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_alias (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ptyp_variant'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_variant (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let ptyp_poly'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_poly (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ptyp_package'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_package arg -> view arg
    | _ -> View.error

  let ptyp_extension'const view value =
    let parent_concrete =
      match Core_type.to_concrete value with
      | None -> conversion_failed "core_type"
      | Some n -> n
    in
    let desc = parent_concrete.Core_type.ptyp_desc in
    let concrete =
      match Core_type_desc.to_concrete desc with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_extension arg -> view arg
    | _ -> View.error

  let rtag'const view value =
    let concrete =
      match Row_field.to_concrete value with
      | None -> conversion_failed "row_field"
      | Some n -> n
    in
    match concrete with
    | Row_field.Rtag (arg0, arg1, arg2, arg3) -> view (arg0, arg1, arg2, arg3)
    | _ -> View.error

  let rinherit'const view value =
    let concrete =
      match Row_field.to_concrete value with
      | None -> conversion_failed "row_field"
      | Some n -> n
    in
    match concrete with
    | Row_field.Rinherit arg -> view arg
    | _ -> View.error

  let otag'const view value =
    let concrete =
      match Object_field.to_concrete value with
      | None -> conversion_failed "object_field"
      | Some n -> n
    in
    match concrete with
    | Object_field.Otag (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let oinherit'const view value =
    let concrete =
      match Object_field.to_concrete value with
      | None -> conversion_failed "object_field"
      | Some n -> n
    in
    match concrete with
    | Object_field.Oinherit arg -> view arg
    | _ -> View.error

  let ppat_desc'match view value =
    let concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    view concrete.Pattern.ppat_desc

  let ppat_loc'match view value =
    let concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    view concrete.Pattern.ppat_loc

  let ppat_attributes'match view value =
    let concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    view concrete.Pattern.ppat_attributes

  let ppat_any'const value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_any -> View.ok
    | _ -> View.error

  let ppat_var'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_var arg -> view arg
    | _ -> View.error

  let ppat_alias'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_alias (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ppat_constant'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_constant arg -> view arg
    | _ -> View.error

  let ppat_interval'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_interval (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ppat_tuple'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_tuple arg -> view arg
    | _ -> View.error

  let ppat_construct'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_construct (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ppat_variant'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_variant (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ppat_record'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_record (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ppat_array'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_array arg -> view arg
    | _ -> View.error

  let ppat_or'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_or (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ppat_constraint'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_constraint (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let ppat_type'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_type arg -> view arg
    | _ -> View.error

  let ppat_lazy'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_lazy arg -> view arg
    | _ -> View.error

  let ppat_unpack'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_unpack arg -> view arg
    | _ -> View.error

  let ppat_exception'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_exception arg -> view arg
    | _ -> View.error

  let ppat_extension'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_extension arg -> view arg
    | _ -> View.error

  let ppat_open'const view value =
    let parent_concrete =
      match Pattern.to_concrete value with
      | None -> conversion_failed "pattern"
      | Some n -> n
    in
    let desc = parent_concrete.Pattern.ppat_desc in
    let concrete =
      match Pattern_desc.to_concrete desc with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_open (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_desc'match view value =
    let concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    view concrete.Expression.pexp_desc

  let pexp_loc'match view value =
    let concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    view concrete.Expression.pexp_loc

  let pexp_attributes'match view value =
    let concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    view concrete.Expression.pexp_attributes

  let pexp_ident'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_ident arg -> view arg
    | _ -> View.error

  let pexp_constant'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_constant arg -> view arg
    | _ -> View.error

  let pexp_let'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_let (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pexp_function'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_function arg -> view arg
    | _ -> View.error

  let pexp_fun'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_fun (arg0, arg1, arg2, arg3) -> view (arg0, arg1, arg2, arg3)
    | _ -> View.error

  let pexp_apply'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_apply (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_match'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_match (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_try'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_try (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_tuple'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_tuple arg -> view arg
    | _ -> View.error

  let pexp_construct'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_construct (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_variant'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_variant (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_record'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_record (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_field'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_field (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_setfield'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_setfield (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pexp_array'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_array arg -> view arg
    | _ -> View.error

  let pexp_ifthenelse'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_ifthenelse (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pexp_sequence'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_sequence (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_while'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_while (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_for'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_for (arg0, arg1, arg2, arg3, arg4) -> view (arg0, arg1, arg2, arg3, arg4)
    | _ -> View.error

  let pexp_constraint'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_constraint (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_coerce'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_coerce (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pexp_send'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_send (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_new'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_new arg -> view arg
    | _ -> View.error

  let pexp_setinstvar'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_setinstvar (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_override'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_override arg -> view arg
    | _ -> View.error

  let pexp_letmodule'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_letmodule (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pexp_letexception'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_letexception (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_assert'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_assert arg -> view arg
    | _ -> View.error

  let pexp_lazy'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_lazy arg -> view arg
    | _ -> View.error

  let pexp_poly'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_poly (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_object'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_object arg -> view arg
    | _ -> View.error

  let pexp_newtype'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_newtype (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pexp_pack'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_pack arg -> view arg
    | _ -> View.error

  let pexp_open'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_open (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pexp_extension'const view value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_extension arg -> view arg
    | _ -> View.error

  let pexp_unreachable'const value =
    let parent_concrete =
      match Expression.to_concrete value with
      | None -> conversion_failed "expression"
      | Some n -> n
    in
    let desc = parent_concrete.Expression.pexp_desc in
    let concrete =
      match Expression_desc.to_concrete desc with
      | None -> conversion_failed "expression_desc"
      | Some n -> n
    in
    match concrete with
    | Expression_desc.Pexp_unreachable -> View.ok
    | _ -> View.error

  let pc_lhs'match view value =
    let concrete =
      match Case.to_concrete value with
      | None -> conversion_failed "case"
      | Some n -> n
    in
    view concrete.Case.pc_lhs

  let pc_guard'match view value =
    let concrete =
      match Case.to_concrete value with
      | None -> conversion_failed "case"
      | Some n -> n
    in
    view concrete.Case.pc_guard

  let pc_rhs'match view value =
    let concrete =
      match Case.to_concrete value with
      | None -> conversion_failed "case"
      | Some n -> n
    in
    view concrete.Case.pc_rhs

  let pval_name'match view value =
    let concrete =
      match Value_description.to_concrete value with
      | None -> conversion_failed "value_description"
      | Some n -> n
    in
    view concrete.Value_description.pval_name

  let pval_type'match view value =
    let concrete =
      match Value_description.to_concrete value with
      | None -> conversion_failed "value_description"
      | Some n -> n
    in
    view concrete.Value_description.pval_type

  let pval_prim'match view value =
    let concrete =
      match Value_description.to_concrete value with
      | None -> conversion_failed "value_description"
      | Some n -> n
    in
    view concrete.Value_description.pval_prim

  let pval_attributes'match view value =
    let concrete =
      match Value_description.to_concrete value with
      | None -> conversion_failed "value_description"
      | Some n -> n
    in
    view concrete.Value_description.pval_attributes

  let pval_loc'match view value =
    let concrete =
      match Value_description.to_concrete value with
      | None -> conversion_failed "value_description"
      | Some n -> n
    in
    view concrete.Value_description.pval_loc

  let ptype_name'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_name

  let ptype_params'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_params

  let ptype_cstrs'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_cstrs

  let ptype_kind'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_kind

  let ptype_private'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_private

  let ptype_manifest'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_manifest

  let ptype_attributes'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_attributes

  let ptype_loc'match view value =
    let concrete =
      match Type_declaration.to_concrete value with
      | None -> conversion_failed "type_declaration"
      | Some n -> n
    in
    view concrete.Type_declaration.ptype_loc

  let ptype_abstract'const value =
    let concrete =
      match Type_kind.to_concrete value with
      | None -> conversion_failed "type_kind"
      | Some n -> n
    in
    match concrete with
    | Type_kind.Ptype_abstract -> View.ok
    | _ -> View.error

  let ptype_variant'const view value =
    let concrete =
      match Type_kind.to_concrete value with
      | None -> conversion_failed "type_kind"
      | Some n -> n
    in
    match concrete with
    | Type_kind.Ptype_variant arg -> view arg
    | _ -> View.error

  let ptype_record'const view value =
    let concrete =
      match Type_kind.to_concrete value with
      | None -> conversion_failed "type_kind"
      | Some n -> n
    in
    match concrete with
    | Type_kind.Ptype_record arg -> view arg
    | _ -> View.error

  let ptype_open'const value =
    let concrete =
      match Type_kind.to_concrete value with
      | None -> conversion_failed "type_kind"
      | Some n -> n
    in
    match concrete with
    | Type_kind.Ptype_open -> View.ok
    | _ -> View.error

  let pld_name'match view value =
    let concrete =
      match Label_declaration.to_concrete value with
      | None -> conversion_failed "label_declaration"
      | Some n -> n
    in
    view concrete.Label_declaration.pld_name

  let pld_mutable'match view value =
    let concrete =
      match Label_declaration.to_concrete value with
      | None -> conversion_failed "label_declaration"
      | Some n -> n
    in
    view concrete.Label_declaration.pld_mutable

  let pld_type'match view value =
    let concrete =
      match Label_declaration.to_concrete value with
      | None -> conversion_failed "label_declaration"
      | Some n -> n
    in
    view concrete.Label_declaration.pld_type

  let pld_loc'match view value =
    let concrete =
      match Label_declaration.to_concrete value with
      | None -> conversion_failed "label_declaration"
      | Some n -> n
    in
    view concrete.Label_declaration.pld_loc

  let pld_attributes'match view value =
    let concrete =
      match Label_declaration.to_concrete value with
      | None -> conversion_failed "label_declaration"
      | Some n -> n
    in
    view concrete.Label_declaration.pld_attributes

  let pcd_name'match view value =
    let concrete =
      match Constructor_declaration.to_concrete value with
      | None -> conversion_failed "constructor_declaration"
      | Some n -> n
    in
    view concrete.Constructor_declaration.pcd_name

  let pcd_args'match view value =
    let concrete =
      match Constructor_declaration.to_concrete value with
      | None -> conversion_failed "constructor_declaration"
      | Some n -> n
    in
    view concrete.Constructor_declaration.pcd_args

  let pcd_res'match view value =
    let concrete =
      match Constructor_declaration.to_concrete value with
      | None -> conversion_failed "constructor_declaration"
      | Some n -> n
    in
    view concrete.Constructor_declaration.pcd_res

  let pcd_loc'match view value =
    let concrete =
      match Constructor_declaration.to_concrete value with
      | None -> conversion_failed "constructor_declaration"
      | Some n -> n
    in
    view concrete.Constructor_declaration.pcd_loc

  let pcd_attributes'match view value =
    let concrete =
      match Constructor_declaration.to_concrete value with
      | None -> conversion_failed "constructor_declaration"
      | Some n -> n
    in
    view concrete.Constructor_declaration.pcd_attributes

  let pcstr_tuple'const view value =
    let concrete =
      match Constructor_arguments.to_concrete value with
      | None -> conversion_failed "constructor_arguments"
      | Some n -> n
    in
    match concrete with
    | Constructor_arguments.Pcstr_tuple arg -> view arg
    | _ -> View.error

  let pcstr_record'const view value =
    let concrete =
      match Constructor_arguments.to_concrete value with
      | None -> conversion_failed "constructor_arguments"
      | Some n -> n
    in
    match concrete with
    | Constructor_arguments.Pcstr_record arg -> view arg
    | _ -> View.error

  let ptyext_path'match view value =
    let concrete =
      match Type_extension.to_concrete value with
      | None -> conversion_failed "type_extension"
      | Some n -> n
    in
    view concrete.Type_extension.ptyext_path

  let ptyext_params'match view value =
    let concrete =
      match Type_extension.to_concrete value with
      | None -> conversion_failed "type_extension"
      | Some n -> n
    in
    view concrete.Type_extension.ptyext_params

  let ptyext_constructors'match view value =
    let concrete =
      match Type_extension.to_concrete value with
      | None -> conversion_failed "type_extension"
      | Some n -> n
    in
    view concrete.Type_extension.ptyext_constructors

  let ptyext_private'match view value =
    let concrete =
      match Type_extension.to_concrete value with
      | None -> conversion_failed "type_extension"
      | Some n -> n
    in
    view concrete.Type_extension.ptyext_private

  let ptyext_attributes'match view value =
    let concrete =
      match Type_extension.to_concrete value with
      | None -> conversion_failed "type_extension"
      | Some n -> n
    in
    view concrete.Type_extension.ptyext_attributes

  let pext_name'match view value =
    let concrete =
      match Extension_constructor.to_concrete value with
      | None -> conversion_failed "extension_constructor"
      | Some n -> n
    in
    view concrete.Extension_constructor.pext_name

  let pext_kind'match view value =
    let concrete =
      match Extension_constructor.to_concrete value with
      | None -> conversion_failed "extension_constructor"
      | Some n -> n
    in
    view concrete.Extension_constructor.pext_kind

  let pext_loc'match view value =
    let concrete =
      match Extension_constructor.to_concrete value with
      | None -> conversion_failed "extension_constructor"
      | Some n -> n
    in
    view concrete.Extension_constructor.pext_loc

  let pext_attributes'match view value =
    let concrete =
      match Extension_constructor.to_concrete value with
      | None -> conversion_failed "extension_constructor"
      | Some n -> n
    in
    view concrete.Extension_constructor.pext_attributes

  let pext_decl'const view value =
    let concrete =
      match Extension_constructor_kind.to_concrete value with
      | None -> conversion_failed "extension_constructor_kind"
      | Some n -> n
    in
    match concrete with
    | Extension_constructor_kind.Pext_decl (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pext_rebind'const view value =
    let concrete =
      match Extension_constructor_kind.to_concrete value with
      | None -> conversion_failed "extension_constructor_kind"
      | Some n -> n
    in
    match concrete with
    | Extension_constructor_kind.Pext_rebind arg -> view arg
    | _ -> View.error

  let pcty_desc'match view value =
    let concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    view concrete.Class_type.pcty_desc

  let pcty_loc'match view value =
    let concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    view concrete.Class_type.pcty_loc

  let pcty_attributes'match view value =
    let concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    view concrete.Class_type.pcty_attributes

  let pcty_constr'const view value =
    let parent_concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type.pcty_desc in
    let concrete =
      match Class_type_desc.to_concrete desc with
      | None -> conversion_failed "class_type_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_desc.Pcty_constr (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pcty_signature'const view value =
    let parent_concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type.pcty_desc in
    let concrete =
      match Class_type_desc.to_concrete desc with
      | None -> conversion_failed "class_type_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_desc.Pcty_signature arg -> view arg
    | _ -> View.error

  let pcty_arrow'const view value =
    let parent_concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type.pcty_desc in
    let concrete =
      match Class_type_desc.to_concrete desc with
      | None -> conversion_failed "class_type_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_desc.Pcty_arrow (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pcty_extension'const view value =
    let parent_concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type.pcty_desc in
    let concrete =
      match Class_type_desc.to_concrete desc with
      | None -> conversion_failed "class_type_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_desc.Pcty_extension arg -> view arg
    | _ -> View.error

  let pcty_open'const view value =
    let parent_concrete =
      match Class_type.to_concrete value with
      | None -> conversion_failed "class_type"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type.pcty_desc in
    let concrete =
      match Class_type_desc.to_concrete desc with
      | None -> conversion_failed "class_type_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_desc.Pcty_open (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pcsig_self'match view value =
    let concrete =
      match Class_signature.to_concrete value with
      | None -> conversion_failed "class_signature"
      | Some n -> n
    in
    view concrete.Class_signature.pcsig_self

  let pcsig_fields'match view value =
    let concrete =
      match Class_signature.to_concrete value with
      | None -> conversion_failed "class_signature"
      | Some n -> n
    in
    view concrete.Class_signature.pcsig_fields

  let pctf_desc'match view value =
    let concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    view concrete.Class_type_field.pctf_desc

  let pctf_loc'match view value =
    let concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    view concrete.Class_type_field.pctf_loc

  let pctf_attributes'match view value =
    let concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    view concrete.Class_type_field.pctf_attributes

  let pctf_inherit'const view value =
    let parent_concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type_field.pctf_desc in
    let concrete =
      match Class_type_field_desc.to_concrete desc with
      | None -> conversion_failed "class_type_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_field_desc.Pctf_inherit arg -> view arg
    | _ -> View.error

  let pctf_val'const view value =
    let parent_concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type_field.pctf_desc in
    let concrete =
      match Class_type_field_desc.to_concrete desc with
      | None -> conversion_failed "class_type_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_field_desc.Pctf_val arg -> view arg
    | _ -> View.error

  let pctf_method'const view value =
    let parent_concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type_field.pctf_desc in
    let concrete =
      match Class_type_field_desc.to_concrete desc with
      | None -> conversion_failed "class_type_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_field_desc.Pctf_method arg -> view arg
    | _ -> View.error

  let pctf_constraint'const view value =
    let parent_concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type_field.pctf_desc in
    let concrete =
      match Class_type_field_desc.to_concrete desc with
      | None -> conversion_failed "class_type_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_field_desc.Pctf_constraint arg -> view arg
    | _ -> View.error

  let pctf_attribute'const view value =
    let parent_concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type_field.pctf_desc in
    let concrete =
      match Class_type_field_desc.to_concrete desc with
      | None -> conversion_failed "class_type_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_field_desc.Pctf_attribute arg -> view arg
    | _ -> View.error

  let pctf_extension'const view value =
    let parent_concrete =
      match Class_type_field.to_concrete value with
      | None -> conversion_failed "class_type_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_type_field.pctf_desc in
    let concrete =
      match Class_type_field_desc.to_concrete desc with
      | None -> conversion_failed "class_type_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_type_field_desc.Pctf_extension arg -> view arg
    | _ -> View.error

  let pci_virt'match view value =
    let concrete =
      match Class_infos.to_concrete value with
      | None -> conversion_failed "class_infos"
      | Some n -> n
    in
    view concrete.Class_infos.pci_virt

  let pci_params'match view value =
    let concrete =
      match Class_infos.to_concrete value with
      | None -> conversion_failed "class_infos"
      | Some n -> n
    in
    view concrete.Class_infos.pci_params

  let pci_name'match view value =
    let concrete =
      match Class_infos.to_concrete value with
      | None -> conversion_failed "class_infos"
      | Some n -> n
    in
    view concrete.Class_infos.pci_name

  let pci_expr'match view value =
    let concrete =
      match Class_infos.to_concrete value with
      | None -> conversion_failed "class_infos"
      | Some n -> n
    in
    view concrete.Class_infos.pci_expr

  let pci_loc'match view value =
    let concrete =
      match Class_infos.to_concrete value with
      | None -> conversion_failed "class_infos"
      | Some n -> n
    in
    view concrete.Class_infos.pci_loc

  let pci_attributes'match view value =
    let concrete =
      match Class_infos.to_concrete value with
      | None -> conversion_failed "class_infos"
      | Some n -> n
    in
    view concrete.Class_infos.pci_attributes

  let pcl_desc'match view value =
    let concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    view concrete.Class_expr.pcl_desc

  let pcl_loc'match view value =
    let concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    view concrete.Class_expr.pcl_loc

  let pcl_attributes'match view value =
    let concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    view concrete.Class_expr.pcl_attributes

  let pcl_constr'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_constr (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pcl_structure'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_structure arg -> view arg
    | _ -> View.error

  let pcl_fun'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_fun (arg0, arg1, arg2, arg3) -> view (arg0, arg1, arg2, arg3)
    | _ -> View.error

  let pcl_apply'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_apply (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pcl_let'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_let (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pcl_constraint'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_constraint (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pcl_extension'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_extension arg -> view arg
    | _ -> View.error

  let pcl_open'const view value =
    let parent_concrete =
      match Class_expr.to_concrete value with
      | None -> conversion_failed "class_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Class_expr.pcl_desc in
    let concrete =
      match Class_expr_desc.to_concrete desc with
      | None -> conversion_failed "class_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Class_expr_desc.Pcl_open (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pcstr_self'match view value =
    let concrete =
      match Class_structure.to_concrete value with
      | None -> conversion_failed "class_structure"
      | Some n -> n
    in
    view concrete.Class_structure.pcstr_self

  let pcstr_fields'match view value =
    let concrete =
      match Class_structure.to_concrete value with
      | None -> conversion_failed "class_structure"
      | Some n -> n
    in
    view concrete.Class_structure.pcstr_fields

  let pcf_desc'match view value =
    let concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    view concrete.Class_field.pcf_desc

  let pcf_loc'match view value =
    let concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    view concrete.Class_field.pcf_loc

  let pcf_attributes'match view value =
    let concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    view concrete.Class_field.pcf_attributes

  let pcf_inherit'const view value =
    let parent_concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_field.pcf_desc in
    let concrete =
      match Class_field_desc.to_concrete desc with
      | None -> conversion_failed "class_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_field_desc.Pcf_inherit (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pcf_val'const view value =
    let parent_concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_field.pcf_desc in
    let concrete =
      match Class_field_desc.to_concrete desc with
      | None -> conversion_failed "class_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_field_desc.Pcf_val arg -> view arg
    | _ -> View.error

  let pcf_method'const view value =
    let parent_concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_field.pcf_desc in
    let concrete =
      match Class_field_desc.to_concrete desc with
      | None -> conversion_failed "class_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_field_desc.Pcf_method arg -> view arg
    | _ -> View.error

  let pcf_constraint'const view value =
    let parent_concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_field.pcf_desc in
    let concrete =
      match Class_field_desc.to_concrete desc with
      | None -> conversion_failed "class_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_field_desc.Pcf_constraint arg -> view arg
    | _ -> View.error

  let pcf_initializer'const view value =
    let parent_concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_field.pcf_desc in
    let concrete =
      match Class_field_desc.to_concrete desc with
      | None -> conversion_failed "class_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_field_desc.Pcf_initializer arg -> view arg
    | _ -> View.error

  let pcf_attribute'const view value =
    let parent_concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_field.pcf_desc in
    let concrete =
      match Class_field_desc.to_concrete desc with
      | None -> conversion_failed "class_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_field_desc.Pcf_attribute arg -> view arg
    | _ -> View.error

  let pcf_extension'const view value =
    let parent_concrete =
      match Class_field.to_concrete value with
      | None -> conversion_failed "class_field"
      | Some n -> n
    in
    let desc = parent_concrete.Class_field.pcf_desc in
    let concrete =
      match Class_field_desc.to_concrete desc with
      | None -> conversion_failed "class_field_desc"
      | Some n -> n
    in
    match concrete with
    | Class_field_desc.Pcf_extension arg -> view arg
    | _ -> View.error

  let cfk_virtual'const view value =
    let concrete =
      match Class_field_kind.to_concrete value with
      | None -> conversion_failed "class_field_kind"
      | Some n -> n
    in
    match concrete with
    | Class_field_kind.Cfk_virtual arg -> view arg
    | _ -> View.error

  let cfk_concrete'const view value =
    let concrete =
      match Class_field_kind.to_concrete value with
      | None -> conversion_failed "class_field_kind"
      | Some n -> n
    in
    match concrete with
    | Class_field_kind.Cfk_concrete (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pmty_desc'match view value =
    let concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    view concrete.Module_type.pmty_desc

  let pmty_loc'match view value =
    let concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    view concrete.Module_type.pmty_loc

  let pmty_attributes'match view value =
    let concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    view concrete.Module_type.pmty_attributes

  let pmty_ident'const view value =
    let parent_concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    let desc = parent_concrete.Module_type.pmty_desc in
    let concrete =
      match Module_type_desc.to_concrete desc with
      | None -> conversion_failed "module_type_desc"
      | Some n -> n
    in
    match concrete with
    | Module_type_desc.Pmty_ident arg -> view arg
    | _ -> View.error

  let pmty_signature'const view value =
    let parent_concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    let desc = parent_concrete.Module_type.pmty_desc in
    let concrete =
      match Module_type_desc.to_concrete desc with
      | None -> conversion_failed "module_type_desc"
      | Some n -> n
    in
    match concrete with
    | Module_type_desc.Pmty_signature arg -> view arg
    | _ -> View.error

  let pmty_functor'const view value =
    let parent_concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    let desc = parent_concrete.Module_type.pmty_desc in
    let concrete =
      match Module_type_desc.to_concrete desc with
      | None -> conversion_failed "module_type_desc"
      | Some n -> n
    in
    match concrete with
    | Module_type_desc.Pmty_functor (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pmty_with'const view value =
    let parent_concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    let desc = parent_concrete.Module_type.pmty_desc in
    let concrete =
      match Module_type_desc.to_concrete desc with
      | None -> conversion_failed "module_type_desc"
      | Some n -> n
    in
    match concrete with
    | Module_type_desc.Pmty_with (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pmty_typeof'const view value =
    let parent_concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    let desc = parent_concrete.Module_type.pmty_desc in
    let concrete =
      match Module_type_desc.to_concrete desc with
      | None -> conversion_failed "module_type_desc"
      | Some n -> n
    in
    match concrete with
    | Module_type_desc.Pmty_typeof arg -> view arg
    | _ -> View.error

  let pmty_extension'const view value =
    let parent_concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    let desc = parent_concrete.Module_type.pmty_desc in
    let concrete =
      match Module_type_desc.to_concrete desc with
      | None -> conversion_failed "module_type_desc"
      | Some n -> n
    in
    match concrete with
    | Module_type_desc.Pmty_extension arg -> view arg
    | _ -> View.error

  let pmty_alias'const view value =
    let parent_concrete =
      match Module_type.to_concrete value with
      | None -> conversion_failed "module_type"
      | Some n -> n
    in
    let desc = parent_concrete.Module_type.pmty_desc in
    let concrete =
      match Module_type_desc.to_concrete desc with
      | None -> conversion_failed "module_type_desc"
      | Some n -> n
    in
    match concrete with
    | Module_type_desc.Pmty_alias arg -> view arg
    | _ -> View.error

  let psig_desc'match view value =
    let concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    view concrete.Signature_item.psig_desc

  let psig_loc'match view value =
    let concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    view concrete.Signature_item.psig_loc

  let psig_value'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_value arg -> view arg
    | _ -> View.error

  let psig_type'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_type (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let psig_typext'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_typext arg -> view arg
    | _ -> View.error

  let psig_exception'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_exception arg -> view arg
    | _ -> View.error

  let psig_module'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_module arg -> view arg
    | _ -> View.error

  let psig_recmodule'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_recmodule arg -> view arg
    | _ -> View.error

  let psig_modtype'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_modtype arg -> view arg
    | _ -> View.error

  let psig_open'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_open arg -> view arg
    | _ -> View.error

  let psig_include'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_include arg -> view arg
    | _ -> View.error

  let psig_class'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_class arg -> view arg
    | _ -> View.error

  let psig_class_type'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_class_type arg -> view arg
    | _ -> View.error

  let psig_attribute'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_attribute arg -> view arg
    | _ -> View.error

  let psig_extension'const view value =
    let parent_concrete =
      match Signature_item.to_concrete value with
      | None -> conversion_failed "signature_item"
      | Some n -> n
    in
    let desc = parent_concrete.Signature_item.psig_desc in
    let concrete =
      match Signature_item_desc.to_concrete desc with
      | None -> conversion_failed "signature_item_desc"
      | Some n -> n
    in
    match concrete with
    | Signature_item_desc.Psig_extension (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pmd_name'match view value =
    let concrete =
      match Module_declaration.to_concrete value with
      | None -> conversion_failed "module_declaration"
      | Some n -> n
    in
    view concrete.Module_declaration.pmd_name

  let pmd_type'match view value =
    let concrete =
      match Module_declaration.to_concrete value with
      | None -> conversion_failed "module_declaration"
      | Some n -> n
    in
    view concrete.Module_declaration.pmd_type

  let pmd_attributes'match view value =
    let concrete =
      match Module_declaration.to_concrete value with
      | None -> conversion_failed "module_declaration"
      | Some n -> n
    in
    view concrete.Module_declaration.pmd_attributes

  let pmd_loc'match view value =
    let concrete =
      match Module_declaration.to_concrete value with
      | None -> conversion_failed "module_declaration"
      | Some n -> n
    in
    view concrete.Module_declaration.pmd_loc

  let pmtd_name'match view value =
    let concrete =
      match Module_type_declaration.to_concrete value with
      | None -> conversion_failed "module_type_declaration"
      | Some n -> n
    in
    view concrete.Module_type_declaration.pmtd_name

  let pmtd_type'match view value =
    let concrete =
      match Module_type_declaration.to_concrete value with
      | None -> conversion_failed "module_type_declaration"
      | Some n -> n
    in
    view concrete.Module_type_declaration.pmtd_type

  let pmtd_attributes'match view value =
    let concrete =
      match Module_type_declaration.to_concrete value with
      | None -> conversion_failed "module_type_declaration"
      | Some n -> n
    in
    view concrete.Module_type_declaration.pmtd_attributes

  let pmtd_loc'match view value =
    let concrete =
      match Module_type_declaration.to_concrete value with
      | None -> conversion_failed "module_type_declaration"
      | Some n -> n
    in
    view concrete.Module_type_declaration.pmtd_loc

  let popen_lid'match view value =
    let concrete =
      match Open_description.to_concrete value with
      | None -> conversion_failed "open_description"
      | Some n -> n
    in
    view concrete.Open_description.popen_lid

  let popen_override'match view value =
    let concrete =
      match Open_description.to_concrete value with
      | None -> conversion_failed "open_description"
      | Some n -> n
    in
    view concrete.Open_description.popen_override

  let popen_loc'match view value =
    let concrete =
      match Open_description.to_concrete value with
      | None -> conversion_failed "open_description"
      | Some n -> n
    in
    view concrete.Open_description.popen_loc

  let popen_attributes'match view value =
    let concrete =
      match Open_description.to_concrete value with
      | None -> conversion_failed "open_description"
      | Some n -> n
    in
    view concrete.Open_description.popen_attributes

  let pincl_mod'match view value =
    let concrete =
      match Include_infos.to_concrete value with
      | None -> conversion_failed "include_infos"
      | Some n -> n
    in
    view concrete.Include_infos.pincl_mod

  let pincl_loc'match view value =
    let concrete =
      match Include_infos.to_concrete value with
      | None -> conversion_failed "include_infos"
      | Some n -> n
    in
    view concrete.Include_infos.pincl_loc

  let pincl_attributes'match view value =
    let concrete =
      match Include_infos.to_concrete value with
      | None -> conversion_failed "include_infos"
      | Some n -> n
    in
    view concrete.Include_infos.pincl_attributes

  let pwith_type'const view value =
    let concrete =
      match With_constraint.to_concrete value with
      | None -> conversion_failed "with_constraint"
      | Some n -> n
    in
    match concrete with
    | With_constraint.Pwith_type (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pwith_module'const view value =
    let concrete =
      match With_constraint.to_concrete value with
      | None -> conversion_failed "with_constraint"
      | Some n -> n
    in
    match concrete with
    | With_constraint.Pwith_module (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pwith_typesubst'const view value =
    let concrete =
      match With_constraint.to_concrete value with
      | None -> conversion_failed "with_constraint"
      | Some n -> n
    in
    match concrete with
    | With_constraint.Pwith_typesubst (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pwith_modsubst'const view value =
    let concrete =
      match With_constraint.to_concrete value with
      | None -> conversion_failed "with_constraint"
      | Some n -> n
    in
    match concrete with
    | With_constraint.Pwith_modsubst (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pmod_desc'match view value =
    let concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    view concrete.Module_expr.pmod_desc

  let pmod_loc'match view value =
    let concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    view concrete.Module_expr.pmod_loc

  let pmod_attributes'match view value =
    let concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    view concrete.Module_expr.pmod_attributes

  let pmod_ident'const view value =
    let parent_concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Module_expr.pmod_desc in
    let concrete =
      match Module_expr_desc.to_concrete desc with
      | None -> conversion_failed "module_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Module_expr_desc.Pmod_ident arg -> view arg
    | _ -> View.error

  let pmod_structure'const view value =
    let parent_concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Module_expr.pmod_desc in
    let concrete =
      match Module_expr_desc.to_concrete desc with
      | None -> conversion_failed "module_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Module_expr_desc.Pmod_structure arg -> view arg
    | _ -> View.error

  let pmod_functor'const view value =
    let parent_concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Module_expr.pmod_desc in
    let concrete =
      match Module_expr_desc.to_concrete desc with
      | None -> conversion_failed "module_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Module_expr_desc.Pmod_functor (arg0, arg1, arg2) -> view (arg0, arg1, arg2)
    | _ -> View.error

  let pmod_apply'const view value =
    let parent_concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Module_expr.pmod_desc in
    let concrete =
      match Module_expr_desc.to_concrete desc with
      | None -> conversion_failed "module_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Module_expr_desc.Pmod_apply (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pmod_constraint'const view value =
    let parent_concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Module_expr.pmod_desc in
    let concrete =
      match Module_expr_desc.to_concrete desc with
      | None -> conversion_failed "module_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Module_expr_desc.Pmod_constraint (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pmod_unpack'const view value =
    let parent_concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Module_expr.pmod_desc in
    let concrete =
      match Module_expr_desc.to_concrete desc with
      | None -> conversion_failed "module_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Module_expr_desc.Pmod_unpack arg -> view arg
    | _ -> View.error

  let pmod_extension'const view value =
    let parent_concrete =
      match Module_expr.to_concrete value with
      | None -> conversion_failed "module_expr"
      | Some n -> n
    in
    let desc = parent_concrete.Module_expr.pmod_desc in
    let concrete =
      match Module_expr_desc.to_concrete desc with
      | None -> conversion_failed "module_expr_desc"
      | Some n -> n
    in
    match concrete with
    | Module_expr_desc.Pmod_extension arg -> view arg
    | _ -> View.error

  let pstr_desc'match view value =
    let concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    view concrete.Structure_item.pstr_desc

  let pstr_loc'match view value =
    let concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    view concrete.Structure_item.pstr_loc

  let pstr_eval'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_eval (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pstr_value'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_value (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pstr_primitive'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_primitive arg -> view arg
    | _ -> View.error

  let pstr_type'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_type (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pstr_typext'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_typext arg -> view arg
    | _ -> View.error

  let pstr_exception'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_exception arg -> view arg
    | _ -> View.error

  let pstr_module'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_module arg -> view arg
    | _ -> View.error

  let pstr_recmodule'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_recmodule arg -> view arg
    | _ -> View.error

  let pstr_modtype'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_modtype arg -> view arg
    | _ -> View.error

  let pstr_open'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_open arg -> view arg
    | _ -> View.error

  let pstr_class'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_class arg -> view arg
    | _ -> View.error

  let pstr_class_type'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_class_type arg -> view arg
    | _ -> View.error

  let pstr_include'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_include arg -> view arg
    | _ -> View.error

  let pstr_attribute'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_attribute arg -> view arg
    | _ -> View.error

  let pstr_extension'const view value =
    let parent_concrete =
      match Structure_item.to_concrete value with
      | None -> conversion_failed "structure_item"
      | Some n -> n
    in
    let desc = parent_concrete.Structure_item.pstr_desc in
    let concrete =
      match Structure_item_desc.to_concrete desc with
      | None -> conversion_failed "structure_item_desc"
      | Some n -> n
    in
    match concrete with
    | Structure_item_desc.Pstr_extension (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pvb_pat'match view value =
    let concrete =
      match Value_binding.to_concrete value with
      | None -> conversion_failed "value_binding"
      | Some n -> n
    in
    view concrete.Value_binding.pvb_pat

  let pvb_expr'match view value =
    let concrete =
      match Value_binding.to_concrete value with
      | None -> conversion_failed "value_binding"
      | Some n -> n
    in
    view concrete.Value_binding.pvb_expr

  let pvb_attributes'match view value =
    let concrete =
      match Value_binding.to_concrete value with
      | None -> conversion_failed "value_binding"
      | Some n -> n
    in
    view concrete.Value_binding.pvb_attributes

  let pvb_loc'match view value =
    let concrete =
      match Value_binding.to_concrete value with
      | None -> conversion_failed "value_binding"
      | Some n -> n
    in
    view concrete.Value_binding.pvb_loc

  let pmb_name'match view value =
    let concrete =
      match Module_binding.to_concrete value with
      | None -> conversion_failed "module_binding"
      | Some n -> n
    in
    view concrete.Module_binding.pmb_name

  let pmb_expr'match view value =
    let concrete =
      match Module_binding.to_concrete value with
      | None -> conversion_failed "module_binding"
      | Some n -> n
    in
    view concrete.Module_binding.pmb_expr

  let pmb_attributes'match view value =
    let concrete =
      match Module_binding.to_concrete value with
      | None -> conversion_failed "module_binding"
      | Some n -> n
    in
    view concrete.Module_binding.pmb_attributes

  let pmb_loc'match view value =
    let concrete =
      match Module_binding.to_concrete value with
      | None -> conversion_failed "module_binding"
      | Some n -> n
    in
    view concrete.Module_binding.pmb_loc

  let ptop_def'const view value =
    let concrete =
      match Toplevel_phrase.to_concrete value with
      | None -> conversion_failed "toplevel_phrase"
      | Some n -> n
    in
    match concrete with
    | Toplevel_phrase.Ptop_def arg -> view arg
    | _ -> View.error

  let ptop_dir'const view value =
    let concrete =
      match Toplevel_phrase.to_concrete value with
      | None -> conversion_failed "toplevel_phrase"
      | Some n -> n
    in
    match concrete with
    | Toplevel_phrase.Ptop_dir (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pdir_none'const value =
    let concrete =
      match Directive_argument.to_concrete value with
      | None -> conversion_failed "directive_argument"
      | Some n -> n
    in
    match concrete with
    | Directive_argument.Pdir_none -> View.ok
    | _ -> View.error

  let pdir_string'const view value =
    let concrete =
      match Directive_argument.to_concrete value with
      | None -> conversion_failed "directive_argument"
      | Some n -> n
    in
    match concrete with
    | Directive_argument.Pdir_string arg -> view arg
    | _ -> View.error

  let pdir_int'const view value =
    let concrete =
      match Directive_argument.to_concrete value with
      | None -> conversion_failed "directive_argument"
      | Some n -> n
    in
    match concrete with
    | Directive_argument.Pdir_int (arg0, arg1) -> view (arg0, arg1)
    | _ -> View.error

  let pdir_ident'const view value =
    let concrete =
      match Directive_argument.to_concrete value with
      | None -> conversion_failed "directive_argument"
      | Some n -> n
    in
    match concrete with
    | Directive_argument.Pdir_ident arg -> view arg
    | _ -> View.error

  let pdir_bool'const view value =
    let concrete =
      match Directive_argument.to_concrete value with
      | None -> conversion_failed "directive_argument"
      | Some n -> n
    in
    match concrete with
    | Directive_argument.Pdir_bool arg -> view arg
    | _ -> View.error
end
(*$*)
