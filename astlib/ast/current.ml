let version = "V4_07"

let decl ~vars body = ({ vars; body } : Grammar.decl)

let poly decl_name body = decl_name, decl ~vars:["a"] body

let grammar =
  [ poly "loc"
      (Record
         [ "txt", Var "a"
         ; "loc", Location
         ])
  ; variant "longident" ~src_id:"Longident.t"
      [ "Lident", [String]
      ; "Ldot", [T "longident"; String]
      ; "Lapply", [T "longident"; T "longident"]
      ]
  ; inst "longident_loc" "loc" (T "longident")
  ; flag "rec_flag" ["Nonrecursive"; "Recursive"]
  ; flag "direction_flag" ["Upto"; "Downto"]
  ; flag "private_flag" ["Private"; "Public"]
  ; flag "mutable_flag" ["Immutable"; "Mutable"]
  ; flag "virtual_flag" ["Virtual"; "Concrete"]
  ; flag "override_flag" ["Override"; "Fresh"]
  ; flag "closed_flag" ["Closed"; "Open"]
  ; alias "label" String
  ; inst "label_loc" "loc" (T "label")
  ; inst "string_loc" "loc" String
  ; variant "arg_label" ~src_id:"Asttypes.arg_label"
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
  ; tuple "attribute" [T "string_loc"; T "payload"]
  ; tuple "extension" [T "string_loc"; T "payload"]
  ; alias "attributes" (List (T "attribute"))
  ; variant "payload"
      [ "PStr", [T "structure"]
      ; "PSig", [T "signature"]
      ; "PTyp", [T "core_type"]
      ; "PPat", [T "pattern"; Option (T "expression")]
      ]
  ; record "core_type"
      [ "ptyp_desc", T "core_type_desc"
      ; "ptyp_loc", Location
      ; "ptyp_attributes", T "attributes"
      ]
  ; variant "core_type_desc"
      [ "Ptyp_any", []
      ; "Ptyp_var", [String]
      ; "Ptyp_arrow", [T "arg_label"; T "core_type"; T "core_type"]
      ; "Ptyp_tuple", [List (T "core_type")]
      ; "Ptyp_constr", [T "longident_loc"; List (T "core_type")]
      ; "Ptyp_object", [List (T "object_field"); T "closed_flag"]
      ; "Ptyp_class", [T "longident_loc"; List (T "core_type")]
      ; "Ptyp_alias", [T "core_type"; String]
      ; "Ptyp_variant",
        [List (T "row_field"); T "closed_flag"; Option (List (T "label"))]
      ; "Ptyp_poly", [List (T "string_loc"); T "core_type"]
      ; "Ptyp_package", [T "package_type"]
      ; "Ptyp_extension", [T "extension"]
      ]
  ; tuple "package_type" [T "longident_loc"; List (T "package_type_constraint")]
  ; tuple "package_type_constraint" [T "longident_loc"; T "core_type"]
  ; variant "row_field"
      [ "Rtag",
        [T "label_loc"; T "attributes"; Bool; List (T "core_type")]
      ; "Rinherit", [T "core_type"]
      ]
  ; variant "object_field"
      [ "Otag", [T "label_loc"; T "attributes"; T "core_type"]
      ; "Oinherit", [T "core_type"]
      ]
  ; record "pattern"
      [ "ppat_desc", T "pattern_desc"
      ; "ppat_loc", Location
      ; "ppat_attributes", T "attributes"
      ]
  ; variant "pattern_desc"
      [ "Ppat_any", []
      ; "Ppat_var", [T "string_loc"]
      ; "Ppat_alias", [T "pattern"; T "string_loc"]
      ; "Ppat_constant", [T "constant"]
      ; "Ppat_interval", [T "constant"; T "constant"]
      ; "Ppat_tuple", [List (T "pattern")]
      ; "Ppat_construct", [T "longident_loc"; Option (T "pattern")]
      ; "Ppat_variant", [T "label"; Option (T "pattern")]
      ; "Ppat_record", [List (T "record_field_pattern"); T "closed_flag"]
      ; "Ppat_array", [List (T "pattern")]
      ; "Ppat_or", [T "pattern"; T "pattern"]
      ; "Ppat_constraint", [T "pattern"; T "core_type"]
      ; "Ppat_type", [T "longident_loc"]
      ; "Ppat_lazy", [T "pattern"]
      ; "Ppat_unpack", [T "string_loc"]
      ; "Ppat_exception", [T "pattern"]
      ; "Ppat_extension", [T "extension"]
      ; "Ppat_open", [T "longident_loc"; T "pattern"]
      ]
  ; tuple "record_field_pattern" [T "longident_loc"; T "pattern"]
  ; record "expression"
      [ "pexp_desc", T "expression_desc"
      ; "pexp_loc", Location
      ; "pexp_attributes", T "attributes"
      ]
  ; variant "expression_desc"
      [ "Pexp_ident", [T "longident_loc"]
      ; "Pexp_constant", [T "constant"]
      ; "Pexp_let", [T "rec_flag"; List (T "value_binding"); T "expression"]
      ; "Pexp_function", [List (T "case")]
      ; "Pexp_fun",
        [T "arg_label"; Option (T "expression"); T "pattern"; T "expression"]
      ; "Pexp_apply", [T "expression"; List (T "apply_arg")]
      ; "Pexp_match", [T "expression"; List (T "case")]
      ; "Pexp_try", [T "expression"; List (T "case")]
      ; "Pexp_tuple", [List (T "expression")]
      ; "Pexp_construct", [T "longident_loc"; Option (T "expression")]
      ; "Pexp_variant", [T "label"; Option (T "expression")]
      ; "Pexp_record", [List (T "record_field_expression"); Option (T "expression")]
      ; "Pexp_field", [T "expression"; T "longident_loc"]
      ; "Pexp_setfield", [T "expression"; T "longident_loc"; T "expression"]
      ; "Pexp_array", [List (T "expression")]
      ; "Pexp_ifthenelse",
        [T "expression"; T "expression"; Option (T "expression")]
      ; "Pexp_sequence", [T "expression"; T "expression"]
      ; "Pexp_while", [T "expression"; T "expression"]
      ; "Pexp_for",
        [ T "pattern"
        ; T "expression"
        ; T "expression"
        ; T "direction_flag"
        ; T "expression"
        ]
      ; "Pexp_constraint", [T "expression"; T "core_type"]
      ; "Pexp_coerce", [T "expression"; Option (T "core_type"); T "core_type"]
      ; "Pexp_send", [T "expression"; T "label_loc"]
      ; "Pexp_new", [T "longident_loc"]
      ; "Pexp_setinstvar", [T "label_loc"; T "expression"]
      ; "Pexp_override", [List (T "override_expression")]
      ; "Pexp_letmodule", [T "string_loc"; T "module_expr"; T "expression"]
      ; "Pexp_letexception", [T "extension_constructor"; T "expression"]
      ; "Pexp_assert", [T "expression"]
      ; "Pexp_lazy", [T "expression"]
      ; "Pexp_poly", [T "expression"; Option (T "core_type")]
      ; "Pexp_object", [T "class_structure"]
      ; "Pexp_newtype", [T "string_loc"; T "expression"]
      ; "Pexp_pack", [T "module_expr"]
      ; "Pexp_open", [T "override_flag"; T "longident_loc"; T "expression"]
      ; "Pexp_extension", [T "extension"]
      ; "Pexp_unreachable", []
      ]
  ; tuple "override_expression" [T "label_loc"; T "expression"]
  ; tuple "record_field_expression" [T "longident_loc"; T "expression"]
  ; tuple "apply_arg" [T "arg_label"; T "expression"]
  ; record "case"
      [ "pc_lhs", T "pattern"
      ; "pc_guard", Option (T "expression")
      ; "pc_rhs", T "expression"
      ]
  ; record "value_description"
      [ "pval_name", T "string_loc"
      ; "pval_type", T "core_type"
      ; "pval_prim", List String
      ; "pval_attributes", T "attributes"
      ; "pval_loc", Location
      ]
  ; record "type_declaration"
      [ "ptype_name", T "string_loc"
      ; "ptype_params", List (T "type_param")
      ; "ptype_cstrs", List (T "type_constraint")
      ; "ptype_kind", T "type_kind"
      ; "ptype_private", T "private_flag"
      ; "ptype_manifest", Option (T "core_type")
      ; "ptype_attributes", T "attributes"
      ; "ptype_loc", Location
      ]
  ; tuple "type_param" [T "core_type"; T "variance"]
  ; tuple "type_constraint" [T "core_type"; T "core_type"; Location]
  ; variant "type_kind"
      [ "Ptype_abstract", []
      ; "Ptype_variant", [List (T "constructor_declaration")]
      ; "Ptype_record", [List (T "label_declaration")]
      ; "Ptype_open", []
      ]
  ; record "label_declaration"
      [ "pld_name", T "string_loc"
      ; "pld_mutable", T "mutable_flag"
      ; "pld_type", T "core_type"
      ; "pld_loc", Location
      ; "pld_attributes", T "attributes"
      ]
  ; record "constructor_declaration"
      [ "pcd_name", T "string_loc"
      ; "pcd_args", T "constructor_arguments"
      ; "pcd_res", Option (T "core_type")
      ; "pcd_loc", Location
      ; "pcd_attributes", T "attributes"
      ]
  ; variant "constructor_arguments"
      [ "Pcstr_tuple", [List (T "core_type")]
      ; "Pcstr_record", [List (T "label_declaration")]
      ]
  ; record "type_extension"
      [ "ptyext_path", T "longident_loc"
      ; "ptyext_params", List (T "type_param")
      ; "ptyext_constructors", List (T "extension_constructor")
      ; "ptyext_private", T "private_flag"
      ; "ptyext_attributes", T "attributes"
      ]
  ; record "extension_constructor"
      [ "pext_name", T "string_loc"
      ; "pext_kind", T "extension_constructor_kind"
      ; "pext_loc", Location
      ; "pext_attributes", T "attributes"
      ]
  ; variant "extension_constructor_kind"
      [ "Pext_decl", [T "constructor_arguments"; Option (T "core_type")]
      ; "Pext_rebind", [T "longident_loc"]
      ]
  ; record "class_type"
      [ "pcty_desc", T "class_type_desc"
      ; "pcty_loc", Location
      ; "pcty_attributes", T "attributes"
      ]
  ; variant "class_type_desc"
      [ "Pcty_constr", [T "longident_loc"; List (T "core_type")]
      ; "Pcty_signature", [T "class_signature"]
      ; "Pcty_arrow", [T "arg_label"; T "core_type"; T "class_type"]
      ; "Pcty_extension", [T "extension"]
      ; "Pcty_open", [T "override_flag"; T "longident_loc"; T "class_type"]
      ]
  ; record "class_signature"
      [ "pcsig_self", T "core_type"
      ; "pcsig_fields", List (T "class_type_field")
      ]
  ; record "class_type_field"
      [ "pctf_desc", T "class_type_field_desc"
      ; "pctf_loc", Location
      ; "pctf_attributes", T "attributes"
      ]
  ; variant "class_type_field_desc"
      [ "Pctf_inherit", [T "class_type"]
      ; "Pctf_val", [T "class_type_value_desc"]
      ; "Pctf_method", [T "class_type_method_desc"]
      ; "Pctf_constraint", [T "class_type_constraint"]
      ; "Pctf_attribute", [T "attribute"]
      ; "Pctf_extension", [T "extension"]
      ]
  ; tuple "class_type_value_desc"
      [T "label_loc"; T "mutable_flag"; T "virtual_flag"; T "core_type"]
  ; tuple "class_type_method_desc"
      [T "label_loc"; T "private_flag"; T "virtual_flag"; T "core_type"]
  ; tuple "class_type_constraint" [T "core_type"; T "core_type"]
  ; poly "class_infos"
      (Record
         [ "pci_virt", T "virtual_flag"
         ; "pci_params", List (T "type_param")
         ; "pci_name", T "string_loc"
         ; "pci_expr", Var "a"
         ; "pci_loc", Location
         ; "pci_attributes", T "attributes"
         ])
  ; inst "class_description" "class_infos" (T "class_type")
  ; inst "class_type_declaration" "class_infos" (T "class_type")
  ; record "class_expr"
      [ "pcl_desc", T "class_expr_desc"
      ; "pcl_loc", Location
      ; "pcl_attributes", T "attributes"
      ]
  ; variant "class_expr_desc"
      [ "Pcl_constr", [T "longident_loc"; List (T "core_type")]
      ; "Pcl_structure", [T "class_structure"]
      ; "Pcl_fun", [T "arg_label"; Option (T "expression"); T "pattern"; T "class_expr"]
      ; "Pcl_apply", [T "class_expr"; List (T "apply_arg")]
      ; "Pcl_let", [T "rec_flag"; List (T "value_binding"); T "class_expr"]
      ; "Pcl_constraint", [T "class_expr"; T "class_type"]
      ; "Pcl_extension", [T "extension"]
      ; "Pcl_open", [T "override_flag"; T "longident_loc"; T "class_expr"]
      ]
  ; record "class_structure"
      [ "pcstr_self", T "pattern"
      ; "pcstr_fields", List (T "class_field")
      ]
  ; record "class_field"
      [ "pcf_desc", T "class_field_desc"
      ; "pcf_loc", Location
      ; "pcf_attributes", T "attributes"
      ]
  ; variant "class_field_desc"
      [ "Pcf_inherit", [T "override_flag"; T "class_expr"; Option (T "string_loc")]
      ; "Pcf_val", [T "class_value_desc"]
      ; "Pcf_method", [T "class_method_desc"]
      ; "Pcf_constraint", [T "class_type_constraint"]
      ; "Pcf_initializer", [T "expression"]
      ; "Pcf_attribute", [T "attribute"]
      ; "Pcf_extension", [T "extension"]
      ]
  ; tuple "class_value_desc" [T "label_loc"; T "mutable_flag"; T "class_field_kind"]
  ; tuple "class_method_desc" [T "label_loc"; T "private_flag"; T "class_field_kind"]
  ; variant "class_field_kind"
      [ "Cfk_virtual", [T "core_type"]
      ; "Cfk_concrete", [T "override_flag"; T "expression"]
      ]
  ; inst "class_declaration" "class_infos" (T "class_expr")
  ; record "module_type"
      [ "pmty_desc", T "module_type_desc"
      ; "pmty_loc", Location
      ; "pmty_attributes", T "attributes"
      ]
  ; variant "module_type_desc"
      [ "Pmty_ident", [T "longident_loc"]
      ; "Pmty_signature", [T "signature"]
      ; "Pmty_functor", [T "string_loc"; Option (T "module_type"); T "module_type"]
      ; "Pmty_with", [T "module_type"; List (T "with_constraint")]
      ; "Pmty_typeof", [T "module_expr"]
      ; "Pmty_extension", [T "extension"]
      ; "Pmty_alias", [T "longident_loc"]
      ]
  ; alias "signature" (List (T "signature_item"))
  ; record "signature_item"
      [ "psig_desc", T "signature_item_desc"
      ; "psig_loc", Location
      ]
  ; variant "signature_item_desc"
      [ "Psig_value", [T "value_description"]
      ; "Psig_type", [T "rec_flag"; List (T "type_declaration")]
      ; "Psig_typext", [T "type_extension"]
      ; "Psig_exception", [T "extension_constructor"]
      ; "Psig_module", [T "module_declaration"]
      ; "Psig_recmodule", [List (T "module_declaration")]
      ; "Psig_modtype", [T "module_type_declaration"]
      ; "Psig_open", [T "open_description"]
      ; "Psig_include", [T "include_description"]
      ; "Psig_class", [List (T "class_description")]
      ; "Psig_class_type", [List (T "class_type_declaration")]
      ; "Psig_attribute", [T "attribute"]
      ; "Psig_extension", [T "extension"; T "attributes"]
      ]
  ; record "module_declaration"
      [ "pmd_name", T "string_loc"
      ; "pmd_type", T "module_type"
      ; "pmd_attributes", T "attributes"
      ; "pmd_loc", Location
      ]
  ; record "module_type_declaration"
      [ "pmtd_name", T "string_loc"
      ; "pmtd_type", Option (T "module_type")
      ; "pmtd_attributes", T "attributes"
      ; "pmtd_loc", Location
      ]
  ; record "open_description"
      [ "popen_lid", T "longident_loc"
      ; "popen_override", T "override_flag"
      ; "popen_loc", Location
      ; "popen_attributes", T "attributes"
      ]
  ; poly "include_infos"
      (Record
         [ "pincl_mod", Var "a"
         ; "pincl_loc", Location
         ; "pincl_attributes", T "attributes"
         ])
  ; inst "include_description" "include_infos" (T "module_type")
  ; inst "include_declaration" "include_infos" (T "module_expr")
  ; variant "with_constraint"
      [ "Pwith_type", [T "longident_loc"; T "type_declaration"]
      ; "Pwith_module", [T "longident_loc"; T "longident_loc"]
      ; "Pwith_typesubst", [T "longident_loc"; T "type_declaration"]
      ; "Pwith_modsubst", [T "longident_loc"; T "longident_loc"]
      ]
  ; record "module_expr"
      [ "pmod_desc", T "module_expr_desc"
      ; "pmod_loc", Location
      ; "pmod_attributes", T "attributes"
      ]
  ; variant "module_expr_desc"
      [ "Pmod_ident", [T "longident_loc"]
      ; "Pmod_structure", [T "structure"]
      ; "Pmod_functor", [T "string_loc"; Option (T "module_type"); T "module_expr"]
      ; "Pmod_apply", [T "module_expr"; T "module_expr"]
      ; "Pmod_constraint", [T "module_expr"; T "module_type"]
      ; "Pmod_unpack", [T "expression"]
      ; "Pmod_extension", [T "extension"]
      ]
  ; alias "structure" (List (T "structure_item"))
  ; record "structure_item"
      [ "pstr_desc", T "structure_item_desc"
      ; "pstr_loc", Location
      ]
  ; variant "structure_item_desc"
      [ "Pstr_eval", [T "expression"; T "attributes"]
      ; "Pstr_value", [T "rec_flag"; List (T "value_binding")]
      ; "Pstr_primitive", [T "value_description"]
      ; "Pstr_type", [T "rec_flag"; List (T "type_declaration")]
      ; "Pstr_typext", [T "type_extension"]
      ; "Pstr_exception", [T "extension_constructor"]
      ; "Pstr_module", [T "module_binding"]
      ; "Pstr_recmodule", [List (T "module_binding")]
      ; "Pstr_modtype", [T "module_type_declaration"]
      ; "Pstr_open", [T "open_description"]
      ; "Pstr_class", [List (T "class_declaration")]
      ; "Pstr_class_type", [List (T "class_type_declaration")]
      ; "Pstr_include", [T "include_declaration"]
      ; "Pstr_attribute", [T "attribute"]
      ; "Pstr_extension", [T "extension"; T "attributes"]
      ]
  ; record "value_binding"
      [ "pvb_pat", T "pattern"
      ; "pvb_expr", T "expression"
      ; "pvb_attributes", T "attributes"
      ; "pvb_loc", Location
      ]
  ; record "module_binding"
      [ "pmb_name", T "string_loc"
      ; "pmb_expr", T "module_expr"
      ; "pmb_attributes", T "attributes"
      ; "pmb_loc", Location
      ]
  ; variant "toplevel_phrase"
      [ "Ptop_def", [T "structure"]
      ; "Ptop_dir", [String; T "directive_argument"]
      ]
  ; variant "directive_argument"
      [ "Pdir_none", []
      ; "Pdir_string", [String]
      ; "Pdir_int", [String; Option Char]
      ; "Pdir_ident", [T "longident"]
      ; "Pdir_bool", [Bool]
      ]
  ]
