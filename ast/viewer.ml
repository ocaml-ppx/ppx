open Viewlib
(*$ Ppx_ast_cinaps.print_viewer_ml () *)
module V4_07 = struct
  open Versions.V4_07

  let conversion_failed name = Raise.conversion_failed ~version:"V4_07" name

  let nonrecursive value =
    let concrete =
      match Rec_flag.to_concrete value with
      | None -> conversion_failed "rec_flag"
      | Some n -> n
    in
    match concrete with
    | Rec_flag.Nonrecursive -> View.ok
    | _ -> View.error

  let recursive value =
    let concrete =
      match Rec_flag.to_concrete value with
      | None -> conversion_failed "rec_flag"
      | Some n -> n
    in
    match concrete with
    | Rec_flag.Recursive -> View.ok
    | _ -> View.error

  let upto value =
    let concrete =
      match Direction_flag.to_concrete value with
      | None -> conversion_failed "direction_flag"
      | Some n -> n
    in
    match concrete with
    | Direction_flag.Upto -> View.ok
    | _ -> View.error

  let downto_ value =
    let concrete =
      match Direction_flag.to_concrete value with
      | None -> conversion_failed "direction_flag"
      | Some n -> n
    in
    match concrete with
    | Direction_flag.Downto -> View.ok
    | _ -> View.error

  let private_ value =
    let concrete =
      match Private_flag.to_concrete value with
      | None -> conversion_failed "private_flag"
      | Some n -> n
    in
    match concrete with
    | Private_flag.Private -> View.ok
    | _ -> View.error

  let public value =
    let concrete =
      match Private_flag.to_concrete value with
      | None -> conversion_failed "private_flag"
      | Some n -> n
    in
    match concrete with
    | Private_flag.Public -> View.ok
    | _ -> View.error

  let immutable value =
    let concrete =
      match Mutable_flag.to_concrete value with
      | None -> conversion_failed "mutable_flag"
      | Some n -> n
    in
    match concrete with
    | Mutable_flag.Immutable -> View.ok
    | _ -> View.error

  let mutable_ value =
    let concrete =
      match Mutable_flag.to_concrete value with
      | None -> conversion_failed "mutable_flag"
      | Some n -> n
    in
    match concrete with
    | Mutable_flag.Mutable -> View.ok
    | _ -> View.error

  let virtual_ value =
    let concrete =
      match Virtual_flag.to_concrete value with
      | None -> conversion_failed "virtual_flag"
      | Some n -> n
    in
    match concrete with
    | Virtual_flag.Virtual -> View.ok
    | _ -> View.error

  let concrete value =
    let concrete =
      match Virtual_flag.to_concrete value with
      | None -> conversion_failed "virtual_flag"
      | Some n -> n
    in
    match concrete with
    | Virtual_flag.Concrete -> View.ok
    | _ -> View.error

  let override value =
    let concrete =
      match Override_flag.to_concrete value with
      | None -> conversion_failed "override_flag"
      | Some n -> n
    in
    match concrete with
    | Override_flag.Override -> View.ok
    | _ -> View.error

  let fresh value =
    let concrete =
      match Override_flag.to_concrete value with
      | None -> conversion_failed "override_flag"
      | Some n -> n
    in
    match concrete with
    | Override_flag.Fresh -> View.ok
    | _ -> View.error

  let closed value =
    let concrete =
      match Closed_flag.to_concrete value with
      | None -> conversion_failed "closed_flag"
      | Some n -> n
    in
    match concrete with
    | Closed_flag.Closed -> View.ok
    | _ -> View.error

  let open_ value =
    let concrete =
      match Closed_flag.to_concrete value with
      | None -> conversion_failed "closed_flag"
      | Some n -> n
    in
    match concrete with
    | Closed_flag.Open -> View.ok
    | _ -> View.error

  let nolabel value =
    let concrete =
      match Arg_label.to_concrete value with
      | None -> conversion_failed "arg_label"
      | Some n -> n
    in
    match concrete with
    | Arg_label.Nolabel -> View.ok
    | _ -> View.error

  let covariant value =
    let concrete =
      match Variance.to_concrete value with
      | None -> conversion_failed "variance"
      | Some n -> n
    in
    match concrete with
    | Variance.Covariant -> View.ok
    | _ -> View.error

  let contravariant value =
    let concrete =
      match Variance.to_concrete value with
      | None -> conversion_failed "variance"
      | Some n -> n
    in
    match concrete with
    | Variance.Contravariant -> View.ok
    | _ -> View.error

  let invariant value =
    let concrete =
      match Variance.to_concrete value with
      | None -> conversion_failed "variance"
      | Some n -> n
    in
    match concrete with
    | Variance.Invariant -> View.ok
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

  let ptyp_any value =
    let concrete =
      match Core_type_desc.to_concrete value with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_any -> View.ok
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

  let ppat_any value =
    let concrete =
      match Pattern_desc.to_concrete value with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_any -> View.ok
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

  let pexp_unreachable value =
    let concrete =
      match Expression_desc.to_concrete value with
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

  let ptype_abstract value =
    let concrete =
      match Type_kind.to_concrete value with
      | None -> conversion_failed "type_kind"
      | Some n -> n
    in
    match concrete with
    | Type_kind.Ptype_abstract -> View.ok
    | _ -> View.error

  let ptype_open value =
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

  let pdir_none value =
    let concrete =
      match Directive_argument.to_concrete value with
      | None -> conversion_failed "directive_argument"
      | Some n -> n
    in
    match concrete with
    | Directive_argument.Pdir_none -> View.ok
    | _ -> View.error
end

module V4_06 = struct
  open Versions.V4_06

  let conversion_failed name = Raise.conversion_failed ~version:"V4_06" name

  let nonrecursive value =
    let concrete =
      match Rec_flag.to_concrete value with
      | None -> conversion_failed "rec_flag"
      | Some n -> n
    in
    match concrete with
    | Rec_flag.Nonrecursive -> View.ok
    | _ -> View.error

  let recursive value =
    let concrete =
      match Rec_flag.to_concrete value with
      | None -> conversion_failed "rec_flag"
      | Some n -> n
    in
    match concrete with
    | Rec_flag.Recursive -> View.ok
    | _ -> View.error

  let upto value =
    let concrete =
      match Direction_flag.to_concrete value with
      | None -> conversion_failed "direction_flag"
      | Some n -> n
    in
    match concrete with
    | Direction_flag.Upto -> View.ok
    | _ -> View.error

  let downto_ value =
    let concrete =
      match Direction_flag.to_concrete value with
      | None -> conversion_failed "direction_flag"
      | Some n -> n
    in
    match concrete with
    | Direction_flag.Downto -> View.ok
    | _ -> View.error

  let private_ value =
    let concrete =
      match Private_flag.to_concrete value with
      | None -> conversion_failed "private_flag"
      | Some n -> n
    in
    match concrete with
    | Private_flag.Private -> View.ok
    | _ -> View.error

  let public value =
    let concrete =
      match Private_flag.to_concrete value with
      | None -> conversion_failed "private_flag"
      | Some n -> n
    in
    match concrete with
    | Private_flag.Public -> View.ok
    | _ -> View.error

  let immutable value =
    let concrete =
      match Mutable_flag.to_concrete value with
      | None -> conversion_failed "mutable_flag"
      | Some n -> n
    in
    match concrete with
    | Mutable_flag.Immutable -> View.ok
    | _ -> View.error

  let mutable_ value =
    let concrete =
      match Mutable_flag.to_concrete value with
      | None -> conversion_failed "mutable_flag"
      | Some n -> n
    in
    match concrete with
    | Mutable_flag.Mutable -> View.ok
    | _ -> View.error

  let virtual_ value =
    let concrete =
      match Virtual_flag.to_concrete value with
      | None -> conversion_failed "virtual_flag"
      | Some n -> n
    in
    match concrete with
    | Virtual_flag.Virtual -> View.ok
    | _ -> View.error

  let concrete value =
    let concrete =
      match Virtual_flag.to_concrete value with
      | None -> conversion_failed "virtual_flag"
      | Some n -> n
    in
    match concrete with
    | Virtual_flag.Concrete -> View.ok
    | _ -> View.error

  let override value =
    let concrete =
      match Override_flag.to_concrete value with
      | None -> conversion_failed "override_flag"
      | Some n -> n
    in
    match concrete with
    | Override_flag.Override -> View.ok
    | _ -> View.error

  let fresh value =
    let concrete =
      match Override_flag.to_concrete value with
      | None -> conversion_failed "override_flag"
      | Some n -> n
    in
    match concrete with
    | Override_flag.Fresh -> View.ok
    | _ -> View.error

  let closed value =
    let concrete =
      match Closed_flag.to_concrete value with
      | None -> conversion_failed "closed_flag"
      | Some n -> n
    in
    match concrete with
    | Closed_flag.Closed -> View.ok
    | _ -> View.error

  let open_ value =
    let concrete =
      match Closed_flag.to_concrete value with
      | None -> conversion_failed "closed_flag"
      | Some n -> n
    in
    match concrete with
    | Closed_flag.Open -> View.ok
    | _ -> View.error

  let nolabel value =
    let concrete =
      match Arg_label.to_concrete value with
      | None -> conversion_failed "arg_label"
      | Some n -> n
    in
    match concrete with
    | Arg_label.Nolabel -> View.ok
    | _ -> View.error

  let covariant value =
    let concrete =
      match Variance.to_concrete value with
      | None -> conversion_failed "variance"
      | Some n -> n
    in
    match concrete with
    | Variance.Covariant -> View.ok
    | _ -> View.error

  let contravariant value =
    let concrete =
      match Variance.to_concrete value with
      | None -> conversion_failed "variance"
      | Some n -> n
    in
    match concrete with
    | Variance.Contravariant -> View.ok
    | _ -> View.error

  let invariant value =
    let concrete =
      match Variance.to_concrete value with
      | None -> conversion_failed "variance"
      | Some n -> n
    in
    match concrete with
    | Variance.Invariant -> View.ok
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

  let ptyp_any value =
    let concrete =
      match Core_type_desc.to_concrete value with
      | None -> conversion_failed "core_type_desc"
      | Some n -> n
    in
    match concrete with
    | Core_type_desc.Ptyp_any -> View.ok
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

  let ppat_any value =
    let concrete =
      match Pattern_desc.to_concrete value with
      | None -> conversion_failed "pattern_desc"
      | Some n -> n
    in
    match concrete with
    | Pattern_desc.Ppat_any -> View.ok
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

  let pexp_unreachable value =
    let concrete =
      match Expression_desc.to_concrete value with
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

  let ptype_abstract value =
    let concrete =
      match Type_kind.to_concrete value with
      | None -> conversion_failed "type_kind"
      | Some n -> n
    in
    match concrete with
    | Type_kind.Ptype_abstract -> View.ok
    | _ -> View.error

  let ptype_open value =
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

  let pdir_none value =
    let concrete =
      match Directive_argument.to_concrete value with
      | None -> conversion_failed "directive_argument"
      | Some n -> n
    in
    match concrete with
    | Directive_argument.Pdir_none -> View.ok
    | _ -> View.error
end
(*$*)
