open StdLabels

let version = "V4_07"

let decl ~vars body = ({ vars; body } : Grammar.decl)

let mono decl_name body = decl_name, decl ~vars:[] body
let poly decl_name body = decl_name, decl ~vars:["a"] body

let alias decl_name nominal = mono decl_name (Alias nominal)

let tuple decl_name tuple = alias decl_name (Tuple tuple)

let record decl_name record = mono decl_name (Record record)

let variant decl_name clauses =
  let clauses =
    List.map clauses ~f:(fun (name, tuple) ->
      name, (Tuple tuple : Grammar.clause))
  in
  mono decl_name (Variant clauses)

let flag decl_name tags =
  let clauses =
    List.map tags ~f:(fun tag -> (tag, Grammar.Empty))
  in
  mono decl_name (Variant clauses)

let inst decl_name poly arg =
  alias decl_name (Inst { poly; args = [arg] })

let grammar =
  [ poly "loc"
      (Record
         [ "txt", Var "a"
         ; "loc", Location
         ])
  ; variant "longident"
      [ "Lident", [String]
      ; "Ldot", [Name "longident"; String]
      ; "Lapply", [Name "longident"; Name "longident"]
      ]
  ; inst "longident_loc" "loc" (Name "longident")
  ; flag "rec_flag" ["Nonrecursive"; "Recursive"]
  ; flag "direction_flag" ["Upto"; "Downto"]
  ; flag "private_flag" ["Private"; "Public"]
  ; flag "mutable_flag" ["Immutable"; "Mutable"]
  ; flag "virtual_flag" ["Virtual"; "Concrete"]
  ; flag "override_flag" ["Override"; "Fresh"]
  ; flag "closed_flag" ["Closed"; "Open"]
  ; alias "label" String
  ; inst "label_loc" "loc" (Name "label")
  ; inst "string_loc" "loc" String
  ; variant "arg_label"
      [ "Nolabel", []
      ; "Labelled", [String]
      ; "Optional", [String]
      ]
  ; flag "variance" ["Covariant"; "Contravariant"; "Invariant"]
  ; variant "constant"
      [ "Pconst_integer", [String; Option Char]
      ; "Pconst_char", [Char]
      ; "Pconst_string", [String; Option String]
      ; "Pconst_float", [String; Option Char]
      ]
  ; tuple "attribute" [Name "string_loc"; Name "payload"]
  ; tuple "extension" [Name "string_loc"; Name "payload"]
  ; alias "attributes" (List (Name "attribute"))
  ; variant "payload"
      [ "PStr", [Name "structure"]
      ; "PSig", [Name "signature"]
      ; "PTyp", [Name "core_type"]
      ; "PPat", [Name "pattern"; Option (Name "expression")]
      ]
  ; record "core_type"
      [ "ptyp_desc", Name "core_type_desc"
      ; "ptyp_loc", Location
      ; "ptyp_attributes", Name "attributes"
      ]
  ; variant "core_type_desc"
      [ "Ptyp_any", []
      ; "Ptyp_var", [String]
      ; "Ptyp_arrow", [Name "arg_label"; Name "core_type"; Name "core_type"]
      ; "Ptyp_tuple", [List (Name "core_type")]
      ; "Ptyp_constr", [Name "longident_loc"; List (Name "core_type")]
      ; "Ptyp_object", [List (Name "object_field"); Name "closed_flag"]
      ; "Ptyp_class", [Name "longident_loc"; List (Name "core_type")]
      ; "Ptyp_alias", [Name "core_type"; String]
      ; "Ptyp_variant",
        [List (Name "row_field"); Name "closed_flag"; Option (List (Name "label"))]
      ; "Ptyp_poly", [List (Name "string_loc"); Name "core_type"]
      ; "Ptyp_package", [Name "package_type"]
      ; "Ptyp_extension", [Name "extension"]
      ]
  ; tuple "package_type" [Name "longident_loc"; List (Name "package_type_constraint")]
  ; tuple "package_type_constraint" [Name "longident_loc"; Name "core_type"]
  ; variant "row_field"
      [ "Rtag",
        [Name "label_loc"; Name "attributes"; Bool; List (Name "core_type")]
      ; "Rinherit", [Name "core_type"]
      ]
  ; variant "object_field"
      [ "Otag", [Name "label_loc"; Name "attributes"; Name "core_type"]
      ; "Oinherit", [Name "core_type"]
      ]
  ; record "pattern"
      [ "ppat_desc", Name "pattern_desc"
      ; "ppat_loc", Location
      ; "ppat_attributes", Name "attributes"
      ]
  ; variant "pattern_desc"
      [ "Ppat_any", []
      ; "Ppat_var", [Name "string_loc"]
      ; "Ppat_alias", [Name "pattern"; Name "string_loc"]
      ; "Ppat_constant", [Name "constant"]
      ; "Ppat_interval", [Name "constant"; Name "constant"]
      ; "Ppat_tuple", [List (Name "pattern")]
      ; "Ppat_construct", [Name "longident_loc"; Option (Name "pattern")]
      ; "Ppat_variant", [Name "label"; Option (Name "pattern")]
      ; "Ppat_record", [List (Name "record_field_pattern"); Name "closed_flag"]
      ; "Ppat_array", [List (Name "pattern")]
      ; "Ppat_or", [Name "pattern"; Name "pattern"]
      ; "Ppat_constraint", [Name "pattern"; Name "core_type"]
      ; "Ppat_type", [Name "longident_loc"]
      ; "Ppat_lazy", [Name "pattern"]
      ; "Ppat_unpack", [Name "string_loc"]
      ; "Ppat_exception", [Name "pattern"]
      ; "Ppat_extension", [Name "extension"]
      ; "Ppat_open", [Name "longident_loc"; Name "pattern"]
      ]
  ; tuple "record_field_pattern" [Name "longident_loc"; Name "pattern"]
  ; record "expression"
      [ "pexp_desc", Name "expression_desc"
      ; "pexp_loc", Location
      ; "pexp_attributes", Name "attributes"
      ]
  ; variant "expression_desc"
      [ "Pexp_ident", [Name "longident_loc"]
      ; "Pexp_constant", [Name "constant"]
      ; "Pexp_let", [Name "rec_flag"; List (Name "value_binding"); Name "expression"]
      ; "Pexp_function", [List (Name "case")]
      ; "Pexp_fun",
        [Name "arg_label"; Option (Name "expression"); Name "pattern"; Name "expression"]
      ; "Pexp_apply", [Name "expression"; List (Name "apply_arg")]
      ; "Pexp_match", [Name "expression"; List (Name "case")]
      ; "Pexp_try", [Name "expression"; List (Name "case")]
      ; "Pexp_tuple", [List (Name "expression")]
      ; "Pexp_construct", [Name "longident_loc"; Option (Name "expression")]
      ; "Pexp_variant", [Name "label"; Option (Name "expression")]
      ; "Pexp_record", [List (Name "record_field_expression"); Option (Name "expression")]
      ; "Pexp_field", [Name "expression"; Name "longident_loc"]
      ; "Pexp_setfield", [Name "expression"; Name "longident_loc"; Name "expression"]
      ; "Pexp_array", [List (Name "expression")]
      ; "Pexp_ifthenelse",
        [Name "expression"; Name "expression"; Option (Name "expression")]
      ; "Pexp_sequence", [Name "expression"; Name "expression"]
      ; "Pexp_while", [Name "expression"; Name "expression"]
      ; "Pexp_for",
        [ Name "pattern"
        ; Name "expression"
        ; Name "expression"
        ; Name "direction_flag"
        ; Name "expression"
        ]
      ; "Pexp_constraint", [Name "expression"; Name "core_type"]
      ; "Pexp_coerce", [Name "expression"; Option (Name "core_type"); Name "core_type"]
      ; "Pexp_send", [Name "expression"; Name "label_loc"]
      ; "Pexp_new", [Name "longident_loc"]
      ; "Pexp_setinstvar", [Name "label_loc"; Name "expression"]
      ; "Pexp_override", [List (Name "override_expression")]
      ; "Pexp_letmodule", [Name "string_loc"; Name "module_expr"; Name "expression"]
      ; "Pexp_letexception", [Name "extension_constructor"; Name "expression"]
      ; "Pexp_assert", [Name "expression"]
      ; "Pexp_lazy", [Name "expression"]
      ; "Pexp_poly", [Name "expression"; Option (Name "core_type")]
      ; "Pexp_object", [Name "class_structure"]
      ; "Pexp_newtype", [Name "string_loc"; Name "expression"]
      ; "Pexp_pack", [Name "module_expr"]
      ; "Pexp_open", [Name "override_flag"; Name "longident_loc"; Name "expression"]
      ; "Pexp_extension", [Name "extension"]
      ; "Pexp_unreachable", []
      ]
  ; tuple "override_expression" [Name "label_loc"; Name "expression"]
  ; tuple "record_field_expression" [Name "longident_loc"; Name "expression"]
  ; tuple "apply_arg" [Name "arg_label"; Name "expression"]
  ; record "case"
      [ "pc_lhs", Name "pattern"
      ; "pc_guard", Option (Name "expression")
      ; "pc_rhs", Name "expression"
      ]
  ; record "value_description"
      [ "pval_name", Name "string_loc"
      ; "pval_type", Name "core_type"
      ; "pval_prim", List String
      ; "pval_attributes", Name "attributes"
      ; "pval_loc", Location
      ]
  ; record "type_declaration"
      [ "ptype_name", Name "string_loc"
      ; "ptype_params", List (Name "type_param")
      ; "ptype_cstrs", List (Name "type_constraint")
      ; "ptype_kind", Name "type_kind"
      ; "ptype_private", Name "private_flag"
      ; "ptype_manifest", Option (Name "core_type")
      ; "ptype_attributes", Name "attributes"
      ; "ptype_loc", Location
      ]
  ; tuple "type_param" [Name "core_type"; Name "variance"]
  ; tuple "type_constraint" [Name "core_type"; Name "core_type"; Location]
  ; variant "type_kind"
      [ "Ptype_abstract", []
      ; "Ptype_variant", [List (Name "constructor_declaration")]
      ; "Ptype_record", [List (Name "label_declaration")]
      ; "Ptype_open", []
      ]
  ; record "label_declaration"
      [ "pld_name", Name "string_loc"
      ; "pld_mutable", Name "mutable_flag"
      ; "pld_type", Name "core_type"
      ; "pld_loc", Location
      ; "pld_attributes", Name "attributes"
      ]
  ; record "constructor_declaration"
      [ "pcd_name", Name "string_loc"
      ; "pcd_args", Name "constructor_arguments"
      ; "pcd_res", Option (Name "core_type")
      ; "pcd_loc", Location
      ; "pcd_attributes", Name "attributes"
      ]
  ; variant "constructor_arguments"
      [ "Pcstr_tuple", [List (Name "core_type")]
      ; "Pcstr_record", [List (Name "label_declaration")]
      ]
  ; record "type_extension"
      [ "ptyext_path", Name "longident_loc"
      ; "ptyext_params", List (Name "type_param")
      ; "ptyext_constructors", List (Name "extension_constructor")
      ; "ptyext_private", Name "private_flag"
      ; "ptyext_attributes", Name "attributes"
      ]
  ; record "extension_constructor"
      [ "pext_name", Name "string_loc"
      ; "pext_kind", Name "extension_constructor_kind"
      ; "pext_loc", Location
      ; "pext_attributes", Name "attributes"
      ]
  ; variant "extension_constructor_kind"
      [ "Pext_decl", [Name "constructor_arguments"; Option (Name "core_type")]
      ; "Pext_rebind", [Name "longident_loc"]
      ]
  ; record "class_type"
      [ "pcty_desc", Name "class_type_desc"
      ; "pcty_loc", Location
      ; "pcty_attributes", Name "attributes"
      ]
  ; variant "class_type_desc"
      [ "Pcty_constr", [Name "longident_loc"; List (Name "core_type")]
      ; "Pcty_signature", [Name "class_signature"]
      ; "Pcty_arrow", [Name "arg_label"; Name "core_type"; Name "class_type"]
      ; "Pcty_extension", [Name "extension"]
      ; "Pcty_open", [Name "override_flag"; Name "longident_loc"; Name "class_type"]
      ]
  ; record "class_signature"
      [ "pcsig_self", Name "core_type"
      ; "pcsig_fields", List (Name "class_type_field")
      ]
  ; record "class_type_field"
      [ "pctf_desc", Name "class_type_field_desc"
      ; "pctf_loc", Location
      ; "pctf_attributes", Name "attributes"
      ]
  ; variant "class_type_field_desc"
      [ "Pctf_inherit", [Name "class_type"]
      ; "Pctf_val", [Name "class_type_value_desc"]
      ; "Pctf_method", [Name "class_type_method_desc"]
      ; "Pctf_constraint", [Name "class_type_constraint"]
      ; "Pctf_attribute", [Name "attribute"]
      ; "Pctf_extension", [Name "extension"]
      ]
  ; tuple "class_type_value_desc"
      [Name "label_loc"; Name "mutable_flag"; Name "virtual_flag"; Name "core_type"]
  ; tuple "class_type_method_desc"
      [Name "label_loc"; Name "private_flag"; Name "virtual_flag"; Name "core_type"]
  ; tuple "class_type_constraint" [Name "core_type"; Name "core_type"]
  ; poly "class_infos"
      (Record
         [ "pci_virt", Name "virtual_flag"
         ; "pci_params", List (Name "type_param")
         ; "pci_name", Name "string_loc"
         ; "pci_expr", Var "a"
         ; "pci_loc", Location
         ; "pci_attributes", Name "attributes"
         ])
  ; inst "class_description" "class_infos" (Name "class_type")
  ; inst "class_type_declaration" "class_infos" (Name "class_type")
  ; record "class_expr"
      [ "pcl_desc", Name "class_expr_desc"
      ; "pcl_loc", Location
      ; "pcl_attributes", Name "attributes"
      ]
  ; variant "class_expr_desc"
      [ "Pcl_constr", [Name "longident_loc"; List (Name "core_type")]
      ; "Pcl_structure", [Name "class_structure"]
      ; "Pcl_fun", [Name "arg_label"; Option (Name "expression"); Name "pattern"; Name "class_expr"]
      ; "Pcl_apply", [Name "class_expr"; List (Name "apply_arg")]
      ; "Pcl_let", [Name "rec_flag"; List (Name "value_binding"); Name "class_expr"]
      ; "Pcl_constraint", [Name "class_expr"; Name "class_type"]
      ; "Pcl_extension", [Name "extension"]
      ; "Pcl_open", [Name "override_flag"; Name "longident_loc"; Name "class_expr"]
      ]
  ; record "class_structure"
      [ "pcstr_self", Name "pattern"
      ; "pcstr_fields", List (Name "class_field")
      ]
  ; record "class_field"
      [ "pcf_desc", Name "class_field_desc"
      ; "pcf_loc", Location
      ; "pcf_attributes", Name "attributes"
      ]
  ; variant "class_field_desc"
      [ "Pcf_inherit", [Name "override_flag"; Name "class_expr"; Option (Name "string_loc")]
      ; "Pcf_val", [Name "class_value_desc"]
      ; "Pcf_method", [Name "class_method_desc"]
      ; "Pcf_constraint", [Name "class_type_constraint"]
      ; "Pcf_initializer", [Name "expression"]
      ; "Pcf_attribute", [Name "attribute"]
      ; "Pcf_extension", [Name "extension"]
      ]
  ; tuple "class_value_desc" [Name "label_loc"; Name "mutable_flag"; Name "class_field_kind"]
  ; tuple "class_method_desc" [Name "label_loc"; Name "private_flag"; Name "class_field_kind"]
  ; variant "class_field_kind"
      [ "Cfk_virtual", [Name "core_type"]
      ; "Cfk_concrete", [Name "override_flag"; Name "expression"]
      ]
  ; inst "class_declaration" "class_infos" (Name "class_expr")
  ; record "module_type"
      [ "pmty_desc", Name "module_type_desc"
      ; "pmty_loc", Location
      ; "pmty_attributes", Name "attributes"
      ]
  ; variant "module_type_desc"
      [ "Pmty_ident", [Name "longident_loc"]
      ; "Pmty_signature", [Name "signature"]
      ; "Pmty_functor", [Name "string_loc"; Option (Name "module_type"); Name "module_type"]
      ; "Pmty_with", [Name "module_type"; List (Name "with_constraint")]
      ; "Pmty_typeof", [Name "module_expr"]
      ; "Pmty_extension", [Name "extension"]
      ; "Pmty_alias", [Name "longident_loc"]
      ]
  ; alias "signature" (List (Name "signature_item"))
  ; record "signature_item"
      [ "psig_desc", Name "signature_item_desc"
      ; "psig_loc", Location
      ]
  ; variant "signature_item_desc"
      [ "Psig_value", [Name "value_description"]
      ; "Psig_type", [Name "rec_flag"; List (Name "type_declaration")]
      ; "Psig_typext", [Name "type_extension"]
      ; "Psig_exception", [Name "extension_constructor"]
      ; "Psig_module", [Name "module_declaration"]
      ; "Psig_recmodule", [List (Name "module_declaration")]
      ; "Psig_modtype", [Name "module_type_declaration"]
      ; "Psig_open", [Name "open_description"]
      ; "Psig_include", [Name "include_description"]
      ; "Psig_class", [List (Name "class_description")]
      ; "Psig_class_type", [List (Name "class_type_declaration")]
      ; "Psig_attribute", [Name "attribute"]
      ; "Psig_extension", [Name "extension"; Name "attributes"]
      ]
  ; record "module_declaration"
      [ "pmd_name", Name "string_loc"
      ; "pmd_type", Name "module_type"
      ; "pmd_attributes", Name "attributes"
      ; "pmd_loc", Location
      ]
  ; record "module_type_declaration"
      [ "pmtd_name", Name "string_loc"
      ; "pmtd_type", Option (Name "module_type")
      ; "pmtd_attributes", Name "attributes"
      ; "pmtd_loc", Location
      ]
  ; record "open_description"
      [ "popen_lid", Name "longident_loc"
      ; "popen_override", Name "override_flag"
      ; "popen_loc", Location
      ; "popen_attributes", Name "attributes"
      ]
  ; poly "include_infos"
      (Record
         [ "pincl_mod", Var "a"
         ; "pincl_loc", Location
         ; "pincl_attributes", Name "attributes"
         ])
  ; inst "include_description" "include_infos" (Name "module_type")
  ; inst "include_declaration" "include_infos" (Name "module_expr")
  ; variant "with_constraint"
      [ "Pwith_type", [Name "longident_loc"; Name "type_declaration"]
      ; "Pwith_module", [Name "longident_loc"; Name "longident_loc"]
      ; "Pwith_typesubst", [Name "longident_loc"; Name "type_declaration"]
      ; "Pwith_modsubst", [Name "longident_loc"; Name "longident_loc"]
      ]
  ; record "module_expr"
      [ "pmod_desc", Name "module_expr_desc"
      ; "pmod_loc", Location
      ; "pmod_attributes", Name "attributes"
      ]
  ; variant "module_expr_desc"
      [ "Pmod_ident", [Name "longident_loc"]
      ; "Pmod_structure", [Name "structure"]
      ; "Pmod_functor", [Name "string_loc"; Option (Name "module_type"); Name "module_expr"]
      ; "Pmod_apply", [Name "module_expr"; Name "module_expr"]
      ; "Pmod_constraint", [Name "module_expr"; Name "module_type"]
      ; "Pmod_unpack", [Name "expression"]
      ; "Pmod_extension", [Name "extension"]
      ]
  ; alias "structure" (List (Name "structure_item"))
  ; record "structure_item"
      [ "pstr_desc", Name "structure_item_desc"
      ; "pstr_loc", Location
      ]
  ; variant "structure_item_desc"
      [ "Pstr_eval", [Name "expression"; Name "attributes"]
      ; "Pstr_value", [Name "rec_flag"; List (Name "value_binding")]
      ; "Pstr_primitive", [Name "value_description"]
      ; "Pstr_type", [Name "rec_flag"; List (Name "type_declaration")]
      ; "Pstr_typext", [Name "type_extension"]
      ; "Pstr_exception", [Name "extension_constructor"]
      ; "Pstr_module", [Name "module_binding"]
      ; "Pstr_recmodule", [List (Name "module_binding")]
      ; "Pstr_modtype", [Name "module_type_declaration"]
      ; "Pstr_open", [Name "open_description"]
      ; "Pstr_class", [List (Name "class_declaration")]
      ; "Pstr_class_type", [List (Name "class_type_declaration")]
      ; "Pstr_include", [Name "include_declaration"]
      ; "Pstr_attribute", [Name "attribute"]
      ; "Pstr_extension", [Name "extension"; Name "attributes"]
      ]
  ; record "value_binding"
      [ "pvb_pat", Name "pattern"
      ; "pvb_expr", Name "expression"
      ; "pvb_attributes", Name "attributes"
      ; "pvb_loc", Location
      ]
  ; record "module_binding"
      [ "pmb_name", Name "string_loc"
      ; "pmb_expr", Name "module_expr"
      ; "pmb_attributes", Name "attributes"
      ; "pmb_loc", Location
      ]
  ; variant "toplevel_phrase"
      [ "Ptop_def", [Name "structure"]
      ; "Ptop_dir", [String; Name "directive_argument"]
      ]
  ; variant "directive_argument"
      [ "Pdir_none", []
      ; "Pdir_string", [String]
      ; "Pdir_int", [String; Option Char]
      ; "Pdir_ident", [Name "longident"]
      ; "Pdir_bool", [Bool]
      ]
  ]
