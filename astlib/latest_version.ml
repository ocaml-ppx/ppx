let version = Version.of_string "v4_07"

let conversions : History.conversion list = []

let grammar : Grammar.t =
  [ ( "longident"
    , Mono
        (Variant
           [ ("Lident", Tuple [String])
           ; ("Ldot", Tuple [Name "longident"; String])
           ; ("Lapply", Tuple [Name "longident"; Name "longident"]) ]))
  ; ("longident_loc", Mono (Wrapper (Loc (Name "longident"))))
  ; ( "rec_flag"
    , Mono
        (Variant [("Nonrecursive", Empty); ("Recursive", Empty)])
    )
  ; ( "direction_flag"
    , Mono (Variant [("Upto", Empty); ("Downto", Empty)]) )
  ; ( "private_flag"
    , Mono (Variant [("Private", Empty); ("Public", Empty)]) )
  ; ( "mutable_flag"
    , Mono (Variant [("Immutable", Empty); ("Mutable", Empty)]) )
  ; ( "virtual_flag"
    , Mono (Variant [("Virtual", Empty); ("Concrete", Empty)]) )
  ; ( "override_flag"
    , Mono (Variant [("Override", Empty); ("Fresh", Empty)]) )
  ; ( "closed_flag"
    , Mono (Variant [("Closed", Empty); ("Open", Empty)]) )
  ; ( "arg_label"
    , Mono
        (Variant
              [ ("Nolabel", Empty)
              ; ("Labelled", Tuple [String])
              ; ("Optional", Tuple [String]) ]) )
  ; ( "variance"
    , Mono
        (Variant
              [ ("Covariant", Empty)
              ; ("Contravariant", Empty)
              ; ("Invariant", Empty) ]) )
  ; ( "constant"
    , Mono
        (Variant
              [ ("Pconst_integer", Tuple [String; Option Char])
              ; ("Pconst_char", Tuple [Char])
              ; ("Pconst_string", Tuple [String; Option String])
              ; ("Pconst_float", Tuple [String; Option Char]) ]) )
  ; ( "attribute"
    , Mono (Wrapper (Tuple [Loc String; Name "payload"])) )
  ; ( "extension"
    , Mono (Wrapper (Tuple [Loc String; Name "payload"])) )
  ; ("attributes", Mono (Wrapper (List (Name "attribute"))))
  ; ( "payload"
    , Mono
        (Variant
              [ ("PStr", Tuple [Name "structure"])
              ; ("PSig", Tuple [Name "signature"])
              ; ("PTyp", Tuple [Name "core_type"])
              ; ("PPat", Tuple [Name "pattern"; Option (Name "expression")]) ])
    )
  ; ( "core_type"
    , Mono
        (Record
              [ ("ptyp_desc", Name "core_type_desc")
              ; ("ptyp_loc", Location)
              ; ("ptyp_attributes", Name "attributes") ]) )
  ; ( "core_type_desc"
    , Mono
        (Variant
              [ ("Ptyp_any", Empty)
              ; ("Ptyp_var", Tuple [String])
              ; ( "Ptyp_arrow"
                , Tuple [Name "arg_label"; Name "core_type"; Name "core_type"]
                )
              ; ("Ptyp_tuple", Tuple [List (Name "core_type")])
              ; ( "Ptyp_constr"
                , Tuple [Name "longident_loc"; List (Name "core_type")] )
              ; ( "Ptyp_object"
                , Tuple [List (Name "object_field"); Name "closed_flag"] )
              ; ( "Ptyp_class"
                , Tuple [Name "longident_loc"; List (Name "core_type")] )
              ; ("Ptyp_alias", Tuple [Name "core_type"; String])
              ; ( "Ptyp_variant"
                , Tuple
                    [ List (Name "row_field")
                    ; Name "closed_flag"
                    ; Option (List String) ] )
              ; ("Ptyp_poly", Tuple [List (Loc String); Name "core_type"])
              ; ("Ptyp_package", Tuple [Name "package_type"])
              ; ("Ptyp_extension", Tuple [Name "extension"]) ]) )
  ; ( "package_type"
    , Mono
        (Wrapper
              (Tuple
                 [ Name "longident_loc"
                 ; List (Tuple [Name "longident_loc"; Name "core_type"]) ]))
    )
  ; ( "row_field"
    , Mono
        (Variant
              [ ( "Rtag"
                , Tuple
                    [ Loc String
                    ; Name "attributes"
                    ; Bool
                    ; List (Name "core_type") ] )
              ; ("Rinherit", Tuple [Name "core_type"]) ]) )
  ; ( "object_field"
    , Mono
        (Variant
              [ ( "Otag"
                , Tuple
                    [Loc String; Name "attributes"; Name "core_type"]
                )
              ; ("Oinherit", Tuple [Name "core_type"]) ]) )
  ; ( "pattern"
    , Mono
        (Record
              [ ("ppat_desc", Name "pattern_desc")
              ; ("ppat_loc", Location)
              ; ("ppat_attributes", Name "attributes") ]) )
  ; ( "pattern_desc"
    , Mono
        (Variant
              [ ("Ppat_any", Empty)
              ; ("Ppat_var", Tuple [Loc String])
              ; ("Ppat_alias", Tuple [Name "pattern"; Loc String])
              ; ("Ppat_constant", Tuple [Name "constant"])
              ; ("Ppat_interval", Tuple [Name "constant"; Name "constant"])
              ; ("Ppat_tuple", Tuple [List (Name "pattern")])
              ; ( "Ppat_construct"
                , Tuple [Name "longident_loc"; Option (Name "pattern")] )
              ; ("Ppat_variant", Tuple [String; Option (Name "pattern")])
              ; ( "Ppat_record"
                , Tuple
                    [ List (Tuple [Name "longident_loc"; Name "pattern"])
                    ; Name "closed_flag" ] )
              ; ("Ppat_array", Tuple [List (Name "pattern")])
              ; ("Ppat_or", Tuple [Name "pattern"; Name "pattern"])
              ; ("Ppat_constraint", Tuple [Name "pattern"; Name "core_type"])
              ; ("Ppat_type", Tuple [Name "longident_loc"])
              ; ("Ppat_lazy", Tuple [Name "pattern"])
              ; ("Ppat_unpack", Tuple [Loc String])
              ; ("Ppat_exception", Tuple [Name "pattern"])
              ; ("Ppat_extension", Tuple [Name "extension"])
              ; ("Ppat_open", Tuple [Name "longident_loc"; Name "pattern"]) ])
    )
  ; ( "expression"
    , Mono
        (Record
              [ ("pexp_desc", Name "expression_desc")
              ; ("pexp_loc", Location)
              ; ("pexp_attributes", Name "attributes") ]) )
  ; ( "expression_desc"
    , Mono
        (Variant
              [ ("Pexp_ident", Tuple [Name "longident_loc"])
              ; ("Pexp_constant", Tuple [Name "constant"])
              ; ( "Pexp_let"
                , Tuple
                    [ Name "rec_flag"
                    ; List (Name "value_binding")
                    ; Name "expression" ] )
              ; ("Pexp_function", Tuple [List (Name "case")])
              ; ( "Pexp_fun"
                , Tuple
                    [ Name "arg_label"
                    ; Option (Name "expression")
                    ; Name "pattern"
                    ; Name "expression" ] )
              ; ( "Pexp_apply"
                , Tuple
                    [ Name "expression"
                    ; List (Tuple [Name "arg_label"; Name "expression"]) ] )
              ; ("Pexp_match", Tuple [Name "expression"; List (Name "case")])
              ; ("Pexp_try", Tuple [Name "expression"; List (Name "case")])
              ; ("Pexp_tuple", Tuple [List (Name "expression")])
              ; ( "Pexp_construct"
                , Tuple [Name "longident_loc"; Option (Name "expression")] )
              ; ( "Pexp_variant"
                , Tuple [String; Option (Name "expression")] )
              ; ( "Pexp_record"
                , Tuple
                    [ List (Tuple [Name "longident_loc"; Name "expression"])
                    ; Option (Name "expression") ] )
              ; ("Pexp_field", Tuple [Name "expression"; Name "longident_loc"])
              ; ( "Pexp_setfield"
                , Tuple
                    [Name "expression"; Name "longident_loc"; Name "expression"]
                )
              ; ("Pexp_array", Tuple [List (Name "expression")])
              ; ( "Pexp_ifthenelse"
                , Tuple
                    [ Name "expression"
                    ; Name "expression"
                    ; Option (Name "expression") ] )
              ; ("Pexp_sequence", Tuple [Name "expression"; Name "expression"])
              ; ("Pexp_while", Tuple [Name "expression"; Name "expression"])
              ; ( "Pexp_for"
                , Tuple
                    [ Name "pattern"
                    ; Name "expression"
                    ; Name "expression"
                    ; Name "direction_flag"
                    ; Name "expression" ] )
              ; ("Pexp_constraint", Tuple [Name "expression"; Name "core_type"])
              ; ( "Pexp_coerce"
                , Tuple
                    [ Name "expression"
                    ; Option (Name "core_type")
                    ; Name "core_type" ] )
              ; ("Pexp_send", Tuple [Name "expression"; Loc String])
              ; ("Pexp_new", Tuple [Name "longident_loc"])
              ; ( "Pexp_setinstvar"
                , Tuple [Loc String; Name "expression"] )
              ; ( "Pexp_override"
                , Tuple [List (Tuple [Loc String; Name "expression"])]
                )
              ; ( "Pexp_letmodule"
                , Tuple [Loc String; Name "module_expr"; Name "expression"] )
              ; ( "Pexp_letexception"
                , Tuple [Name "extension_constructor"; Name "expression"] )
              ; ("Pexp_assert", Tuple [Name "expression"])
              ; ("Pexp_lazy", Tuple [Name "expression"])
              ; ( "Pexp_poly"
                , Tuple [Name "expression"; Option (Name "core_type")] )
              ; ("Pexp_object", Tuple [Name "class_structure"])
              ; ("Pexp_newtype", Tuple [Loc String; Name "expression"])
              ; ("Pexp_pack", Tuple [Name "module_expr"])
              ; ( "Pexp_open"
                , Tuple
                    [ Name "override_flag"
                    ; Name "longident_loc"
                    ; Name "expression" ] )
              ; ("Pexp_extension", Tuple [Name "extension"])
              ; ("Pexp_unreachable", Empty) ]) )
  ; ( "case"
    , Mono
        (Record
              [ ("pc_lhs", Name "pattern")
              ; ("pc_guard", Option (Name "expression"))
              ; ("pc_rhs", Name "expression") ]) )
  ; ( "value_description"
    , Mono
        (Record
              [ ("pval_name", Loc String)
              ; ("pval_type", Name "core_type")
              ; ("pval_prim", List String)
              ; ("pval_attributes", Name "attributes")
              ; ("pval_loc", Location) ]) )
  ; ( "type_declaration"
    , Mono
        (Record
              [ ("ptype_name", Loc String)
              ; ( "ptype_params"
                , List (Tuple [Name "core_type"; Name "variance"]) )
              ; ( "ptype_cstrs"
                , List (Tuple [Name "core_type"; Name "core_type"; Location])
                )
              ; ("ptype_kind", Name "type_kind")
              ; ("ptype_private", Name "private_flag")
              ; ("ptype_manifest", Option (Name "core_type"))
              ; ("ptype_attributes", Name "attributes")
              ; ("ptype_loc", Location) ]) )
  ; ( "type_kind"
    , Mono
        (Variant
              [ ("Ptype_abstract", Empty)
              ; ("Ptype_variant", Tuple [List (Name "constructor_declaration")])
              ; ("Ptype_record", Tuple [List (Name "label_declaration")])
              ; ("Ptype_open", Empty) ]) )
  ; ( "label_declaration"
    , Mono
        (Record
              [ ("pld_name", Loc String)
              ; ("pld_mutable", Name "mutable_flag")
              ; ("pld_type", Name "core_type")
              ; ("pld_loc", Location)
              ; ("pld_attributes", Name "attributes") ]) )
  ; ( "constructor_declaration"
    , Mono
        (Record
              [ ("pcd_name", Loc String)
              ; ("pcd_args", Name "constructor_arguments")
              ; ("pcd_res", Option (Name "core_type"))
              ; ("pcd_loc", Location)
              ; ("pcd_attributes", Name "attributes") ]) )
  ; ( "constructor_arguments"
    , Mono
        (Variant
              [ ("Pcstr_tuple", Tuple [List (Name "core_type")])
              ; ("Pcstr_record", Tuple [List (Name "label_declaration")]) ])
    )
  ; ( "type_extension"
    , Mono
        (Record
              [ ("ptyext_path", Name "longident_loc")
              ; ( "ptyext_params"
                , List (Tuple [Name "core_type"; Name "variance"]) )
              ; ("ptyext_constructors", List (Name "extension_constructor"))
              ; ("ptyext_private", Name "private_flag")
              ; ("ptyext_attributes", Name "attributes") ]) )
  ; ( "extension_constructor"
    , Mono
        (Record
              [ ("pext_name", Loc String)
              ; ("pext_kind", Name "extension_constructor_kind")
              ; ("pext_loc", Location)
              ; ("pext_attributes", Name "attributes") ]) )
  ; ( "extension_constructor_kind"
    , Mono
        (Variant
              [ ( "Pext_decl"
                , Tuple
                    [Name "constructor_arguments"; Option (Name "core_type")]
                )
              ; ("Pext_rebind", Tuple [Name "longident_loc"]) ]) )
  ; ( "class_type"
    , Mono
        (Record
              [ ("pcty_desc", Name "class_type_desc")
              ; ("pcty_loc", Location)
              ; ("pcty_attributes", Name "attributes") ]) )
  ; ( "class_type_desc"
    , Mono
        (Variant
              [ ( "Pcty_constr"
                , Tuple [Name "longident_loc"; List (Name "core_type")] )
              ; ("Pcty_signature", Tuple [Name "class_signature"])
              ; ( "Pcty_arrow"
                , Tuple [Name "arg_label"; Name "core_type"; Name "class_type"]
                )
              ; ("Pcty_extension", Tuple [Name "extension"])
              ; ( "Pcty_open"
                , Tuple
                    [ Name "override_flag"
                    ; Name "longident_loc"
                    ; Name "class_type" ] ) ]) )
  ; ( "class_signature"
    , Mono
        (Record
              [ ("pcsig_self", Name "core_type")
              ; ("pcsig_fields", List (Name "class_type_field")) ]) )
  ; ( "class_type_field"
    , Mono
        (Record
              [ ("pctf_desc", Name "class_type_field_desc")
              ; ("pctf_loc", Location)
              ; ("pctf_attributes", Name "attributes") ]) )
  ; ( "class_type_field_desc"
    , Mono
        (Variant
              [ ("Pctf_inherit", Tuple [Name "class_type"])
              ; ( "Pctf_val"
                , Tuple
                    [ Tuple
                        [ Loc String
                        ; Name "mutable_flag"
                        ; Name "virtual_flag"
                        ; Name "core_type" ] ] )
              ; ( "Pctf_method"
                , Tuple
                    [ Tuple
                        [ Loc String
                        ; Name "private_flag"
                        ; Name "virtual_flag"
                        ; Name "core_type" ] ] )
              ; ( "Pctf_constraint"
                , Tuple [Tuple [Name "core_type"; Name "core_type"]] )
              ; ("Pctf_attribute", Tuple [Name "attribute"])
              ; ("Pctf_extension", Tuple [Name "extension"]) ]) )
  ; ( "class_infos"
    , Poly
        ( ["a"]
        , (Record
               [ ("pci_virt", Name "virtual_flag")
               ; ( "pci_params"
                 , List (Tuple [Name "core_type"; Name "variance"]) )
               ; ("pci_name", Loc String)
               ; ("pci_expr", Var "a")
               ; ("pci_loc", Location)
               ; ("pci_attributes", Name "attributes") ]) ) )
  ; ( "class_description"
    , Mono (Wrapper (Instance ("class_infos", [Tname "class_type"]))) )
  ; ( "class_type_declaration"
    , Mono (Wrapper (Instance ("class_infos", [Tname "class_type"]))) )
  ; ( "class_expr"
    , Mono
        (Record
              [ ("pcl_desc", Name "class_expr_desc")
              ; ("pcl_loc", Location)
              ; ("pcl_attributes", Name "attributes") ]) )
  ; ( "class_expr_desc"
    , Mono
        (Variant
              [ ( "Pcl_constr"
                , Tuple [Name "longident_loc"; List (Name "core_type")] )
              ; ("Pcl_structure", Tuple [Name "class_structure"])
              ; ( "Pcl_fun"
                , Tuple
                    [ Name "arg_label"
                    ; Option (Name "expression")
                    ; Name "pattern"
                    ; Name "class_expr" ] )
              ; ( "Pcl_apply"
                , Tuple
                    [ Name "class_expr"
                    ; List (Tuple [Name "arg_label"; Name "expression"]) ] )
              ; ( "Pcl_let"
                , Tuple
                    [ Name "rec_flag"
                    ; List (Name "value_binding")
                    ; Name "class_expr" ] )
              ; ("Pcl_constraint", Tuple [Name "class_expr"; Name "class_type"])
              ; ("Pcl_extension", Tuple [Name "extension"])
              ; ( "Pcl_open"
                , Tuple
                    [ Name "override_flag"
                    ; Name "longident_loc"
                    ; Name "class_expr" ] ) ]) )
  ; ( "class_structure"
    , Mono
        (Record
              [ ("pcstr_self", Name "pattern")
              ; ("pcstr_fields", List (Name "class_field")) ]) )
  ; ( "class_field"
    , Mono
        (Record
              [ ("pcf_desc", Name "class_field_desc")
              ; ("pcf_loc", Location)
              ; ("pcf_attributes", Name "attributes") ]) )
  ; ( "class_field_desc"
    , Mono
        (Variant
              [ ( "Pcf_inherit"
                , Tuple
                    [ Name "override_flag"
                    ; Name "class_expr"
                    ; Option (Loc String) ] )
              ; ( "Pcf_val"
                , Tuple
                    [ Tuple
                        [ Loc String
                        ; Name "mutable_flag"
                        ; Name "class_field_kind" ] ] )
              ; ( "Pcf_method"
                , Tuple
                    [ Tuple
                        [ Loc String
                        ; Name "private_flag"
                        ; Name "class_field_kind" ] ] )
              ; ( "Pcf_constraint"
                , Tuple [Tuple [Name "core_type"; Name "core_type"]] )
              ; ("Pcf_initializer", Tuple [Name "expression"])
              ; ("Pcf_attribute", Tuple [Name "attribute"])
              ; ("Pcf_extension", Tuple [Name "extension"]) ]) )
  ; ( "class_field_kind"
    , Mono
        (Variant
              [ ("Cfk_virtual", Tuple [Name "core_type"])
              ; ( "Cfk_concrete"
                , Tuple [Name "override_flag"; Name "expression"] ) ]) )
  ; ( "class_declaration"
    , Mono (Wrapper (Instance ("class_infos", [Tname "class_expr"]))) )
  ; ( "module_type"
    , Mono
        (Record
              [ ("pmty_desc", Name "module_type_desc")
              ; ("pmty_loc", Location)
              ; ("pmty_attributes", Name "attributes") ]) )
  ; ( "module_type_desc"
    , Mono
        (Variant
              [ ("Pmty_ident", Tuple [Name "longident_loc"])
              ; ("Pmty_signature", Tuple [Name "signature"])
              ; ( "Pmty_functor"
                , Tuple
                    [ Loc String
                    ; Option (Name "module_type")
                    ; Name "module_type" ] )
              ; ( "Pmty_with"
                , Tuple [Name "module_type"; List (Name "with_constraint")] )
              ; ("Pmty_typeof", Tuple [Name "module_expr"])
              ; ("Pmty_extension", Tuple [Name "extension"])
              ; ("Pmty_alias", Tuple [Name "longident_loc"]) ]) )
  ; ("signature", Mono (Wrapper (List (Name "signature_item"))))
  ; ( "signature_item"
    , Mono
        (Record
              [ ("psig_desc", Name "signature_item_desc")
              ; ("psig_loc", Location) ]) )
  ; ( "signature_item_desc"
    , Mono
        (Variant
              [ ("Psig_value", Tuple [Name "value_description"])
              ; ( "Psig_type"
                , Tuple [Name "rec_flag"; List (Name "type_declaration")] )
              ; ("Psig_typext", Tuple [Name "type_extension"])
              ; ("Psig_exception", Tuple [Name "extension_constructor"])
              ; ("Psig_module", Tuple [Name "module_declaration"])
              ; ("Psig_recmodule", Tuple [List (Name "module_declaration")])
              ; ("Psig_modtype", Tuple [Name "module_type_declaration"])
              ; ("Psig_open", Tuple [Name "open_description"])
              ; ("Psig_include", Tuple [Name "include_description"])
              ; ("Psig_class", Tuple [List (Name "class_description")])
              ; ( "Psig_class_type"
                , Tuple [List (Name "class_type_declaration")] )
              ; ("Psig_attribute", Tuple [Name "attribute"])
              ; ("Psig_extension", Tuple [Name "extension"; Name "attributes"])
              ]) )
  ; ( "module_declaration"
    , Mono
        (Record
              [ ("pmd_name", Loc String)
              ; ("pmd_type", Name "module_type")
              ; ("pmd_attributes", Name "attributes")
              ; ("pmd_loc", Location) ]) )
  ; ( "module_type_declaration"
    , Mono
        (Record
              [ ("pmtd_name", Loc String)
              ; ("pmtd_type", Option (Name "module_type"))
              ; ("pmtd_attributes", Name "attributes")
              ; ("pmtd_loc", Location) ]) )
  ; ( "open_description"
    , Mono
        (Record
              [ ("popen_lid", Name "longident_loc")
              ; ("popen_override", Name "override_flag")
              ; ("popen_loc", Location)
              ; ("popen_attributes", Name "attributes") ]) )
  ; ( "include_infos"
    , Poly
        ( ["a"]
        , (Record
             [ ("pincl_mod", Var "a")
             ; ("pincl_loc", Location)
             ; ("pincl_attributes", Name "attributes") ]) ) )
  ; ( "include_description"
    , Mono (Wrapper (Instance ("include_infos", [Tname "module_type"]))) )
  ; ( "include_declaration"
    , Mono (Wrapper (Instance ("include_infos", [Tname "module_expr"]))) )
  ; ( "with_constraint"
    , Mono
        (Variant
           [ ( "Pwith_type"
             , Tuple [Name "longident_loc"; Name "type_declaration"] )
           ; ( "Pwith_module"
             , Tuple [Name "longident_loc"; Name "longident_loc"] )
           ; ( "Pwith_typesubst"
             , Tuple [Name "longident_loc"; Name "type_declaration"] )
           ; ( "Pwith_modsubst"
             , Tuple [Name "longident_loc"; Name "longident_loc"] ) ]) )
  ; ( "module_expr"
    , Mono
        (Record
           [ ("pmod_desc", Name "module_expr_desc")
           ; ("pmod_loc", Location)
           ; ("pmod_attributes", Name "attributes") ]) )
  ; ( "module_expr_desc"
    , Mono
        (Variant
           [ ("Pmod_ident", Tuple [Name "longident_loc"])
           ; ("Pmod_structure", Tuple [Name "structure"])
           ; ( "Pmod_functor"
             , Tuple
                 [ Loc String
                 ; Option (Name "module_type")
                 ; Name "module_expr" ] )
           ; ("Pmod_apply", Tuple [Name "module_expr"; Name "module_expr"])
           ; ( "Pmod_constraint"
             , Tuple [Name "module_expr"; Name "module_type"] )
           ; ("Pmod_unpack", Tuple [Name "expression"])
           ; ("Pmod_extension", Tuple [Name "extension"]) ]) )
  ; ("structure", Mono (Wrapper (List (Name "structure_item"))))
  ; ( "structure_item"
    , Mono
        (Record
           [ ("pstr_desc", Name "structure_item_desc")
           ; ("pstr_loc", Location) ]) )
  ; ( "structure_item_desc"
    , Mono
        (Variant
           [ ("Pstr_eval", Tuple [Name "expression"; Name "attributes"])
           ; ( "Pstr_value"
             , Tuple [Name "rec_flag"; List (Name "value_binding")] )
           ; ("Pstr_primitive", Tuple [Name "value_description"])
           ; ( "Pstr_type"
             , Tuple [Name "rec_flag"; List (Name "type_declaration")] )
           ; ("Pstr_typext", Tuple [Name "type_extension"])
           ; ("Pstr_exception", Tuple [Name "extension_constructor"])
           ; ("Pstr_module", Tuple [Name "module_binding"])
           ; ("Pstr_recmodule", Tuple [List (Name "module_binding")])
           ; ("Pstr_modtype", Tuple [Name "module_type_declaration"])
           ; ("Pstr_open", Tuple [Name "open_description"])
           ; ("Pstr_class", Tuple [List (Name "class_declaration")])
           ; ( "Pstr_class_type"
             , Tuple [List (Name "class_type_declaration")] )
           ; ("Pstr_include", Tuple [Name "include_declaration"])
           ; ("Pstr_attribute", Tuple [Name "attribute"])
           ; ("Pstr_extension", Tuple [Name "extension"; Name "attributes"])
           ]) )
  ; ( "value_binding"
    , Mono
        (Record
           [ ("pvb_pat", Name "pattern")
           ; ("pvb_expr", Name "expression")
           ; ("pvb_attributes", Name "attributes")
           ; ("pvb_loc", Location) ]) )
  ; ( "module_binding"
    , Mono
        (Record
           [ ("pmb_name", Loc String)
           ; ("pmb_expr", Name "module_expr")
           ; ("pmb_attributes", Name "attributes")
           ; ("pmb_loc", Location) ]) )
  ; ( "toplevel_phrase"
    , Mono
        (Variant
           [ ("Ptop_def", Tuple [Name "structure"])
           ; ("Ptop_dir", Tuple [String; Name "directive_argument"]) ]) )
  ; ( "directive_argument"
    , Mono
        (Variant
           [ ("Pdir_none", Empty)
           ; ("Pdir_string", Tuple [String])
           ; ("Pdir_int", Tuple [String; Option Char])
           ; ("Pdir_ident", Tuple [Name "longident"])
           ; ("Pdir_bool", Tuple [Bool]) ]) ) ]
