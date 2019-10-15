module Base_quickcheck = struct
  module Generator = struct
    include Base_quickcheck.Generator

    (* included in next release of [Base_quickcheck] *)
    let of_lazy lazy_t =
      create (fun ~size ~random ->
        generate (Lazy.force lazy_t) ~size ~random)
  end

  include
    (Base_quickcheck : module type of struct include Base_quickcheck end
     with module Generator := Base_quickcheck.Generator)
end

let with_decremented_size generator =
  let open Base_quickcheck.Generator.Let_syntax in
  let%bind size = Base_quickcheck.Generator.size in
  Base_quickcheck.Generator.with_size generator ~size:(size - 1)

(*$ Astlib_first_draft_test_cinaps.print_parsetree_extended_ml () *)
open! Base
open Base_quickcheck.Export
open Base_quickcheck.Generator.Let_syntax

type 'a loc = 'a Astlib_first_draft_parsetree.loc =
  { txt: 'a
  ; loc: Location_extended.t
  }

and longident = Astlib_first_draft_parsetree.longident =
  | Lident of string
  | Ldot of longident * string
  | Lapply of longident * longident

and longident_loc =
  longident loc

and rec_flag = Astlib_first_draft_parsetree.rec_flag =
  | Nonrecursive
  | Recursive

and direction_flag = Astlib_first_draft_parsetree.direction_flag =
  | Upto
  | Downto

and private_flag = Astlib_first_draft_parsetree.private_flag =
  | Private
  | Public

and mutable_flag = Astlib_first_draft_parsetree.mutable_flag =
  | Immutable
  | Mutable

and virtual_flag = Astlib_first_draft_parsetree.virtual_flag =
  | Virtual
  | Concrete

and override_flag = Astlib_first_draft_parsetree.override_flag =
  | Override
  | Fresh

and closed_flag = Astlib_first_draft_parsetree.closed_flag =
  | Closed
  | Open

and label =
  string

and label_loc =
  label loc

and string_loc =
  string loc

and arg_label = Astlib_first_draft_parsetree.arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string

and variance = Astlib_first_draft_parsetree.variance =
  | Covariant
  | Contravariant
  | Invariant

and constant = Astlib_first_draft_parsetree.constant =
  | Pconst_integer of string * char option
  | Pconst_char of char
  | Pconst_string of string * string option
  | Pconst_float of string * char option

and attribute =
  (string_loc * payload)

and extension =
  (string_loc * payload)

and attributes =
  attribute list

and payload = Astlib_first_draft_parsetree.payload =
  | PStr of structure
  | PSig of signature
  | PTyp of core_type
  | PPat of pattern * expression option

and core_type = Astlib_first_draft_parsetree.core_type =
  { ptyp_desc: core_type_desc
  ; ptyp_loc: Location_extended.t
  ; ptyp_attributes: attributes
  }

and core_type_desc = Astlib_first_draft_parsetree.core_type_desc =
  | Ptyp_any
  | Ptyp_var of string
  | Ptyp_arrow of arg_label * core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_constr of longident_loc * core_type list
  | Ptyp_object of object_field list * closed_flag
  | Ptyp_class of longident_loc * core_type list
  | Ptyp_alias of core_type * string
  | Ptyp_variant of row_field list * closed_flag * label list option
  | Ptyp_poly of string_loc list * core_type
  | Ptyp_package of package_type
  | Ptyp_extension of extension

and package_type =
  (longident_loc * package_type_constraint list)

and package_type_constraint =
  (longident_loc * core_type)

and row_field = Astlib_first_draft_parsetree.row_field =
  | Rtag of label_loc * attributes * bool * core_type list
  | Rinherit of core_type

and object_field = Astlib_first_draft_parsetree.object_field =
  | Otag of label_loc * attributes * core_type
  | Oinherit of core_type

and pattern = Astlib_first_draft_parsetree.pattern =
  { ppat_desc: pattern_desc
  ; ppat_loc: Location_extended.t
  ; ppat_attributes: attributes
  }

and pattern_desc = Astlib_first_draft_parsetree.pattern_desc =
  | Ppat_any
  | Ppat_var of string_loc
  | Ppat_alias of pattern * string_loc
  | Ppat_constant of constant
  | Ppat_interval of constant * constant
  | Ppat_tuple of pattern list
  | Ppat_construct of longident_loc * pattern option
  | Ppat_variant of label * pattern option
  | Ppat_record of record_field_pattern list * closed_flag
  | Ppat_array of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_type
  | Ppat_type of longident_loc
  | Ppat_lazy of pattern
  | Ppat_unpack of string_loc
  | Ppat_exception of pattern
  | Ppat_extension of extension
  | Ppat_open of longident_loc * pattern

and record_field_pattern =
  (longident_loc * pattern)

and expression = Astlib_first_draft_parsetree.expression =
  { pexp_desc: expression_desc
  ; pexp_loc: Location_extended.t
  ; pexp_attributes: attributes
  }

and expression_desc = Astlib_first_draft_parsetree.expression_desc =
  | Pexp_ident of longident_loc
  | Pexp_constant of constant
  | Pexp_let of rec_flag * value_binding list * expression
  | Pexp_function of case list
  | Pexp_fun of arg_label * expression option * pattern * expression
  | Pexp_apply of expression * apply_arg list
  | Pexp_match of expression * case list
  | Pexp_try of expression * case list
  | Pexp_tuple of expression list
  | Pexp_construct of longident_loc * expression option
  | Pexp_variant of label * expression option
  | Pexp_record of record_field_expression list * expression option
  | Pexp_field of expression * longident_loc
  | Pexp_setfield of expression * longident_loc * expression
  | Pexp_array of expression list
  | Pexp_ifthenelse of expression * expression * expression option
  | Pexp_sequence of expression * expression
  | Pexp_while of expression * expression
  | Pexp_for of pattern * expression * expression * direction_flag * expression
  | Pexp_constraint of expression * core_type
  | Pexp_coerce of expression * core_type option * core_type
  | Pexp_send of expression * label_loc
  | Pexp_new of longident_loc
  | Pexp_setinstvar of label_loc * expression
  | Pexp_override of override_expression list
  | Pexp_letmodule of string_loc * module_expr * expression
  | Pexp_letexception of extension_constructor * expression
  | Pexp_assert of expression
  | Pexp_lazy of expression
  | Pexp_poly of expression * core_type option
  | Pexp_object of class_structure
  | Pexp_newtype of string_loc * expression
  | Pexp_pack of module_expr
  | Pexp_open of override_flag * longident_loc * expression
  | Pexp_extension of extension
  | Pexp_unreachable

and override_expression =
  (label_loc * expression)

and record_field_expression =
  (longident_loc * expression)

and apply_arg =
  (arg_label * expression)

and case = Astlib_first_draft_parsetree.case =
  { pc_lhs: pattern
  ; pc_guard: expression option
  ; pc_rhs: expression
  }

and value_description = Astlib_first_draft_parsetree.value_description =
  { pval_name: string_loc
  ; pval_type: core_type
  ; pval_prim: string list
  ; pval_attributes: attributes
  ; pval_loc: Location_extended.t
  }

and type_declaration = Astlib_first_draft_parsetree.type_declaration =
  { ptype_name: string_loc
  ; ptype_params: type_param list
  ; ptype_cstrs: type_constraint list
  ; ptype_kind: type_kind
  ; ptype_private: private_flag
  ; ptype_manifest: core_type option
  ; ptype_attributes: attributes
  ; ptype_loc: Location_extended.t
  }

and type_param =
  (core_type * variance)

and type_constraint =
  (core_type * core_type * Location_extended.t)

and type_kind = Astlib_first_draft_parsetree.type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list
  | Ptype_record of label_declaration list
  | Ptype_open

and label_declaration = Astlib_first_draft_parsetree.label_declaration =
  { pld_name: string_loc
  ; pld_mutable: mutable_flag
  ; pld_type: core_type
  ; pld_loc: Location_extended.t
  ; pld_attributes: attributes
  }

and constructor_declaration = Astlib_first_draft_parsetree.constructor_declaration =
  { pcd_name: string_loc
  ; pcd_args: constructor_arguments
  ; pcd_res: core_type option
  ; pcd_loc: Location_extended.t
  ; pcd_attributes: attributes
  }

and constructor_arguments = Astlib_first_draft_parsetree.constructor_arguments =
  | Pcstr_tuple of core_type list
  | Pcstr_record of label_declaration list

and type_extension = Astlib_first_draft_parsetree.type_extension =
  { ptyext_path: longident_loc
  ; ptyext_params: type_param list
  ; ptyext_constructors: extension_constructor list
  ; ptyext_private: private_flag
  ; ptyext_attributes: attributes
  }

and extension_constructor = Astlib_first_draft_parsetree.extension_constructor =
  { pext_name: string_loc
  ; pext_kind: extension_constructor_kind
  ; pext_loc: Location_extended.t
  ; pext_attributes: attributes
  }

and extension_constructor_kind = Astlib_first_draft_parsetree.extension_constructor_kind =
  | Pext_decl of constructor_arguments * core_type option
  | Pext_rebind of longident_loc

and class_type = Astlib_first_draft_parsetree.class_type =
  { pcty_desc: class_type_desc
  ; pcty_loc: Location_extended.t
  ; pcty_attributes: attributes
  }

and class_type_desc = Astlib_first_draft_parsetree.class_type_desc =
  | Pcty_constr of longident_loc * core_type list
  | Pcty_signature of class_signature
  | Pcty_arrow of arg_label * core_type * class_type
  | Pcty_extension of extension
  | Pcty_open of override_flag * longident_loc * class_type

and class_signature = Astlib_first_draft_parsetree.class_signature =
  { pcsig_self: core_type
  ; pcsig_fields: class_type_field list
  }

and class_type_field = Astlib_first_draft_parsetree.class_type_field =
  { pctf_desc: class_type_field_desc
  ; pctf_loc: Location_extended.t
  ; pctf_attributes: attributes
  }

and class_type_field_desc = Astlib_first_draft_parsetree.class_type_field_desc =
  | Pctf_inherit of class_type
  | Pctf_val of class_type_value_desc
  | Pctf_method of class_type_method_desc
  | Pctf_constraint of class_type_constraint
  | Pctf_attribute of attribute
  | Pctf_extension of extension

and class_type_value_desc =
  (label_loc * mutable_flag * virtual_flag * core_type)

and class_type_method_desc =
  (label_loc * private_flag * virtual_flag * core_type)

and class_type_constraint =
  (core_type * core_type)

and 'a class_infos = 'a Astlib_first_draft_parsetree.class_infos =
  { pci_virt: virtual_flag
  ; pci_params: type_param list
  ; pci_name: string_loc
  ; pci_expr: 'a
  ; pci_loc: Location_extended.t
  ; pci_attributes: attributes
  }

and class_description =
  class_type class_infos

and class_type_declaration =
  class_type class_infos

and class_expr = Astlib_first_draft_parsetree.class_expr =
  { pcl_desc: class_expr_desc
  ; pcl_loc: Location_extended.t
  ; pcl_attributes: attributes
  }

and class_expr_desc = Astlib_first_draft_parsetree.class_expr_desc =
  | Pcl_constr of longident_loc * core_type list
  | Pcl_structure of class_structure
  | Pcl_fun of arg_label * expression option * pattern * class_expr
  | Pcl_apply of class_expr * apply_arg list
  | Pcl_let of rec_flag * value_binding list * class_expr
  | Pcl_constraint of class_expr * class_type
  | Pcl_extension of extension
  | Pcl_open of override_flag * longident_loc * class_expr

and class_structure = Astlib_first_draft_parsetree.class_structure =
  { pcstr_self: pattern
  ; pcstr_fields: class_field list
  }

and class_field = Astlib_first_draft_parsetree.class_field =
  { pcf_desc: class_field_desc
  ; pcf_loc: Location_extended.t
  ; pcf_attributes: attributes
  }

and class_field_desc = Astlib_first_draft_parsetree.class_field_desc =
  | Pcf_inherit of override_flag * class_expr * string_loc option
  | Pcf_val of class_value_desc
  | Pcf_method of class_method_desc
  | Pcf_constraint of class_type_constraint
  | Pcf_initializer of expression
  | Pcf_attribute of attribute
  | Pcf_extension of extension

and class_value_desc =
  (label_loc * mutable_flag * class_field_kind)

and class_method_desc =
  (label_loc * private_flag * class_field_kind)

and class_field_kind = Astlib_first_draft_parsetree.class_field_kind =
  | Cfk_virtual of core_type
  | Cfk_concrete of override_flag * expression

and class_declaration =
  class_expr class_infos

and module_type = Astlib_first_draft_parsetree.module_type =
  { pmty_desc: module_type_desc
  ; pmty_loc: Location_extended.t
  ; pmty_attributes: attributes
  }

and module_type_desc = Astlib_first_draft_parsetree.module_type_desc =
  | Pmty_ident of longident_loc
  | Pmty_signature of signature
  | Pmty_functor of string_loc * module_type option * module_type
  | Pmty_with of module_type * with_constraint list
  | Pmty_typeof of module_expr
  | Pmty_extension of extension
  | Pmty_alias of longident_loc

and signature =
  signature_item list

and signature_item = Astlib_first_draft_parsetree.signature_item =
  { psig_desc: signature_item_desc
  ; psig_loc: Location_extended.t
  }

and signature_item_desc = Astlib_first_draft_parsetree.signature_item_desc =
  | Psig_value of value_description
  | Psig_type of rec_flag * type_declaration list
  | Psig_typext of type_extension
  | Psig_exception of extension_constructor
  | Psig_module of module_declaration
  | Psig_recmodule of module_declaration list
  | Psig_modtype of module_type_declaration
  | Psig_open of open_description
  | Psig_include of include_description
  | Psig_class of class_description list
  | Psig_class_type of class_type_declaration list
  | Psig_attribute of attribute
  | Psig_extension of extension * attributes

and module_declaration = Astlib_first_draft_parsetree.module_declaration =
  { pmd_name: string_loc
  ; pmd_type: module_type
  ; pmd_attributes: attributes
  ; pmd_loc: Location_extended.t
  }

and module_type_declaration = Astlib_first_draft_parsetree.module_type_declaration =
  { pmtd_name: string_loc
  ; pmtd_type: module_type option
  ; pmtd_attributes: attributes
  ; pmtd_loc: Location_extended.t
  }

and open_description = Astlib_first_draft_parsetree.open_description =
  { popen_lid: longident_loc
  ; popen_override: override_flag
  ; popen_loc: Location_extended.t
  ; popen_attributes: attributes
  }

and 'a include_infos = 'a Astlib_first_draft_parsetree.include_infos =
  { pincl_mod: 'a
  ; pincl_loc: Location_extended.t
  ; pincl_attributes: attributes
  }

and include_description =
  module_type include_infos

and include_declaration =
  module_expr include_infos

and with_constraint = Astlib_first_draft_parsetree.with_constraint =
  | Pwith_type of longident_loc * type_declaration
  | Pwith_module of longident_loc * longident_loc
  | Pwith_typesubst of longident_loc * type_declaration
  | Pwith_modsubst of longident_loc * longident_loc

and module_expr = Astlib_first_draft_parsetree.module_expr =
  { pmod_desc: module_expr_desc
  ; pmod_loc: Location_extended.t
  ; pmod_attributes: attributes
  }

and module_expr_desc = Astlib_first_draft_parsetree.module_expr_desc =
  | Pmod_ident of longident_loc
  | Pmod_structure of structure
  | Pmod_functor of string_loc * module_type option * module_expr
  | Pmod_apply of module_expr * module_expr
  | Pmod_constraint of module_expr * module_type
  | Pmod_unpack of expression
  | Pmod_extension of extension

and structure =
  structure_item list

and structure_item = Astlib_first_draft_parsetree.structure_item =
  { pstr_desc: structure_item_desc
  ; pstr_loc: Location_extended.t
  }

and structure_item_desc = Astlib_first_draft_parsetree.structure_item_desc =
  | Pstr_eval of expression * attributes
  | Pstr_value of rec_flag * value_binding list
  | Pstr_primitive of value_description
  | Pstr_type of rec_flag * type_declaration list
  | Pstr_typext of type_extension
  | Pstr_exception of extension_constructor
  | Pstr_module of module_binding
  | Pstr_recmodule of module_binding list
  | Pstr_modtype of module_type_declaration
  | Pstr_open of open_description
  | Pstr_class of class_declaration list
  | Pstr_class_type of class_type_declaration list
  | Pstr_include of include_declaration
  | Pstr_attribute of attribute
  | Pstr_extension of extension * attributes

and value_binding = Astlib_first_draft_parsetree.value_binding =
  { pvb_pat: pattern
  ; pvb_expr: expression
  ; pvb_attributes: attributes
  ; pvb_loc: Location_extended.t
  }

and module_binding = Astlib_first_draft_parsetree.module_binding =
  { pmb_name: string_loc
  ; pmb_expr: module_expr
  ; pmb_attributes: attributes
  ; pmb_loc: Location_extended.t
  }

and toplevel_phrase = Astlib_first_draft_parsetree.toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of string * directive_argument

and directive_argument = Astlib_first_draft_parsetree.directive_argument =
  | Pdir_none
  | Pdir_string of string
  | Pdir_int of string * char option
  | Pdir_ident of longident
  | Pdir_bool of bool
[@@deriving compare, equal, hash, sexp_of]


let rec quickcheck_generator_loc
  : type a . a Base_quickcheck.Generator.t -> a loc Base_quickcheck.Generator.t
  = fun quickcheck_generator_a ->
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map txt =
      quickcheck_generator_a
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and loc =
      Location_extended.quickcheck_generator
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    { txt; loc }

and quickcheck_generator_longident = lazy begin
  let quickcheck_generator_lident =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      quickcheck_generator_string
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Lident (x0)
  and quickcheck_generator_ldot =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      quickcheck_generator_string
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Ldot (x0, x1)
  and quickcheck_generator_lapply =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Lapply (x0, x1)
  in
  let nonrec_list =
    [ quickcheck_generator_lident
    ]
  in
  let rec_list =
    [ quickcheck_generator_ldot
    ; quickcheck_generator_lapply
    ]
    |> List.map ~f:with_decremented_size
  in
  let nonrec_gen = Base_quickcheck.Generator.union nonrec_list in
  let rec_gen = Base_quickcheck.Generator.union rec_list in
  match%bind Base_quickcheck.Generator.size with
  | 0 -> nonrec_gen
  | _ -> rec_gen
end

and quickcheck_generator_longident_loc =
  lazy (quickcheck_generator_loc (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident))

and quickcheck_generator_rec_flag = lazy begin
  let quickcheck_generator_nonrecursive =
    return Astlib_first_draft_parsetree.Nonrecursive
  and quickcheck_generator_recursive =
    return Astlib_first_draft_parsetree.Recursive
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_nonrecursive
    ; quickcheck_generator_recursive
    ]
end

and quickcheck_generator_direction_flag = lazy begin
  let quickcheck_generator_upto =
    return Astlib_first_draft_parsetree.Upto
  and quickcheck_generator_downto =
    return Astlib_first_draft_parsetree.Downto
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_upto
    ; quickcheck_generator_downto
    ]
end

and quickcheck_generator_private_flag = lazy begin
  let quickcheck_generator_private =
    return Astlib_first_draft_parsetree.Private
  and quickcheck_generator_public =
    return Astlib_first_draft_parsetree.Public
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_private
    ; quickcheck_generator_public
    ]
end

and quickcheck_generator_mutable_flag = lazy begin
  let quickcheck_generator_immutable =
    return Astlib_first_draft_parsetree.Immutable
  and quickcheck_generator_mutable =
    return Astlib_first_draft_parsetree.Mutable
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_immutable
    ; quickcheck_generator_mutable
    ]
end

and quickcheck_generator_virtual_flag = lazy begin
  let quickcheck_generator_virtual =
    return Astlib_first_draft_parsetree.Virtual
  and quickcheck_generator_concrete =
    return Astlib_first_draft_parsetree.Concrete
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_virtual
    ; quickcheck_generator_concrete
    ]
end

and quickcheck_generator_override_flag = lazy begin
  let quickcheck_generator_override =
    return Astlib_first_draft_parsetree.Override
  and quickcheck_generator_fresh =
    return Astlib_first_draft_parsetree.Fresh
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_override
    ; quickcheck_generator_fresh
    ]
end

and quickcheck_generator_closed_flag = lazy begin
  let quickcheck_generator_closed =
    return Astlib_first_draft_parsetree.Closed
  and quickcheck_generator_open =
    return Astlib_first_draft_parsetree.Open
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_closed
    ; quickcheck_generator_open
    ]
end

and quickcheck_generator_label = lazy begin
  quickcheck_generator_string
end

and quickcheck_generator_label_loc =
  lazy (quickcheck_generator_loc (Base_quickcheck.Generator.of_lazy quickcheck_generator_label))

and quickcheck_generator_string_loc =
  lazy (quickcheck_generator_loc quickcheck_generator_string)

and quickcheck_generator_arg_label = lazy begin
  let quickcheck_generator_nolabel =
    return Astlib_first_draft_parsetree.Nolabel
  and quickcheck_generator_labelled =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      quickcheck_generator_string
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Labelled (x0)
  and quickcheck_generator_optional =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      quickcheck_generator_string
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Optional (x0)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_nolabel
    ; quickcheck_generator_labelled
    ; quickcheck_generator_optional
    ]
end

and quickcheck_generator_variance = lazy begin
  let quickcheck_generator_covariant =
    return Astlib_first_draft_parsetree.Covariant
  and quickcheck_generator_contravariant =
    return Astlib_first_draft_parsetree.Contravariant
  and quickcheck_generator_invariant =
    return Astlib_first_draft_parsetree.Invariant
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_covariant
    ; quickcheck_generator_contravariant
    ; quickcheck_generator_invariant
    ]
end

and quickcheck_generator_constant = lazy begin
  let quickcheck_generator_pconst_integer =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      quickcheck_generator_string
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option quickcheck_generator_char)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pconst_integer (x0, x1)
  and quickcheck_generator_pconst_char =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      quickcheck_generator_char
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pconst_char (x0)
  and quickcheck_generator_pconst_string =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      quickcheck_generator_string
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option quickcheck_generator_string)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pconst_string (x0, x1)
  and quickcheck_generator_pconst_float =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      quickcheck_generator_string
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option quickcheck_generator_char)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pconst_float (x0, x1)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_pconst_integer
    ; quickcheck_generator_pconst_char
    ; quickcheck_generator_pconst_string
    ; quickcheck_generator_pconst_float
    ]
end

and quickcheck_generator_attribute = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map x0 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and x1 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_payload)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  in
  (x0, x1)
end

and quickcheck_generator_extension = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map x0 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and x1 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_payload)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  in
  (x0, x1)
end

and quickcheck_generator_attributes = lazy begin
  (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_attribute))
end

and quickcheck_generator_payload = lazy begin
  let quickcheck_generator_pstr =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_structure)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.PStr (x0)
  and quickcheck_generator_psig =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_signature)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.PSig (x0)
  and quickcheck_generator_ptyp =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.PTyp (x0)
  and quickcheck_generator_ppat =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.PPat (x0, x1)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_pstr
    ; quickcheck_generator_psig
    ; quickcheck_generator_ptyp
    ; quickcheck_generator_ppat
    ]
end

and quickcheck_generator_core_type = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map ptyp_desc =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type_desc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and ptyp_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and ptyp_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  in
  { ptyp_desc; ptyp_loc; ptyp_attributes }
end

and quickcheck_generator_core_type_desc = lazy begin
  let quickcheck_generator_ptyp_any =
    return Astlib_first_draft_parsetree.Ptyp_any
  and quickcheck_generator_ptyp_var =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      quickcheck_generator_string
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ptyp_var (x0)
  and quickcheck_generator_ptyp_arrow =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_arg_label)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Ptyp_arrow (x0, x1, x2)
  and quickcheck_generator_ptyp_tuple =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ptyp_tuple (x0)
  and quickcheck_generator_ptyp_constr =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Ptyp_constr (x0, x1)
  and quickcheck_generator_ptyp_object =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_object_field))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_closed_flag)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Ptyp_object (x0, x1)
  and quickcheck_generator_ptyp_class =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Ptyp_class (x0, x1)
  and quickcheck_generator_ptyp_alias =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      quickcheck_generator_string
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Ptyp_alias (x0, x1)
  and quickcheck_generator_ptyp_variant =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_row_field))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_closed_flag)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (quickcheck_generator_option (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_label)))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Ptyp_variant (x0, x1, x2)
  and quickcheck_generator_ptyp_poly =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Ptyp_poly (x0, x1)
  and quickcheck_generator_ptyp_package =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_package_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ptyp_package (x0)
  and quickcheck_generator_ptyp_extension =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ptyp_extension (x0)
  in
  let nonrec_list =
    [ quickcheck_generator_ptyp_any
    ; quickcheck_generator_ptyp_var
    ]
  in
  let rec_list =
    [ quickcheck_generator_ptyp_arrow
    ; quickcheck_generator_ptyp_tuple
    ; quickcheck_generator_ptyp_constr
    ; quickcheck_generator_ptyp_object
    ; quickcheck_generator_ptyp_class
    ; quickcheck_generator_ptyp_alias
    ; quickcheck_generator_ptyp_variant
    ; quickcheck_generator_ptyp_poly
    ; quickcheck_generator_ptyp_package
    ; quickcheck_generator_ptyp_extension
    ]
    |> List.map ~f:with_decremented_size
  in
  let nonrec_gen = Base_quickcheck.Generator.union nonrec_list in
  let rec_gen = Base_quickcheck.Generator.union rec_list in
  match%bind Base_quickcheck.Generator.size with
  | 0 -> nonrec_gen
  | _ -> rec_gen
end

and quickcheck_generator_package_type = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map x0 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and x1 =
    (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_package_type_constraint))
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  in
  (x0, x1)
end

and quickcheck_generator_package_type_constraint = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map x0 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and x1 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  in
  (x0, x1)
end

and quickcheck_generator_row_field = lazy begin
  let quickcheck_generator_rtag =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:4 ~max_length:4 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_label_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      quickcheck_generator_bool
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    and x3 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
    in
    Astlib_first_draft_parsetree.Rtag (x0, x1, x2, x3)
  and quickcheck_generator_rinherit =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Rinherit (x0)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_rtag
    ; quickcheck_generator_rinherit
    ]
end

and quickcheck_generator_object_field = lazy begin
  let quickcheck_generator_otag =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_label_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Otag (x0, x1, x2)
  and quickcheck_generator_oinherit =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Oinherit (x0)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_otag
    ; quickcheck_generator_oinherit
    ]
end

and quickcheck_generator_pattern = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map ppat_desc =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern_desc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and ppat_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and ppat_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  in
  { ppat_desc; ppat_loc; ppat_attributes }
end

and quickcheck_generator_pattern_desc = lazy begin
  let quickcheck_generator_ppat_any =
    return Astlib_first_draft_parsetree.Ppat_any
  and quickcheck_generator_ppat_var =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ppat_var (x0)
  and quickcheck_generator_ppat_alias =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Ppat_alias (x0, x1)
  and quickcheck_generator_ppat_constant =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_constant)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ppat_constant (x0)
  and quickcheck_generator_ppat_interval =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_constant)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_constant)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Ppat_interval (x0, x1)
  and quickcheck_generator_ppat_tuple =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ppat_tuple (x0)
  and quickcheck_generator_ppat_construct =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Ppat_construct (x0, x1)
  and quickcheck_generator_ppat_variant =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_label)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Ppat_variant (x0, x1)
  and quickcheck_generator_ppat_record =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_record_field_pattern))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_closed_flag)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Ppat_record (x0, x1)
  and quickcheck_generator_ppat_array =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ppat_array (x0)
  and quickcheck_generator_ppat_or =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Ppat_or (x0, x1)
  and quickcheck_generator_ppat_constraint =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Ppat_constraint (x0, x1)
  and quickcheck_generator_ppat_type =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ppat_type (x0)
  and quickcheck_generator_ppat_lazy =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ppat_lazy (x0)
  and quickcheck_generator_ppat_unpack =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ppat_unpack (x0)
  and quickcheck_generator_ppat_exception =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ppat_exception (x0)
  and quickcheck_generator_ppat_extension =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ppat_extension (x0)
  and quickcheck_generator_ppat_open =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Ppat_open (x0, x1)
  in
  let nonrec_list =
    [ quickcheck_generator_ppat_any
    ]
  in
  let rec_list =
    [ quickcheck_generator_ppat_var
    ; quickcheck_generator_ppat_alias
    ; quickcheck_generator_ppat_constant
    ; quickcheck_generator_ppat_interval
    ; quickcheck_generator_ppat_tuple
    ; quickcheck_generator_ppat_construct
    ; quickcheck_generator_ppat_variant
    ; quickcheck_generator_ppat_record
    ; quickcheck_generator_ppat_array
    ; quickcheck_generator_ppat_or
    ; quickcheck_generator_ppat_constraint
    ; quickcheck_generator_ppat_type
    ; quickcheck_generator_ppat_lazy
    ; quickcheck_generator_ppat_unpack
    ; quickcheck_generator_ppat_exception
    ; quickcheck_generator_ppat_extension
    ; quickcheck_generator_ppat_open
    ]
    |> List.map ~f:with_decremented_size
  in
  let nonrec_gen = Base_quickcheck.Generator.union nonrec_list in
  let rec_gen = Base_quickcheck.Generator.union rec_list in
  match%bind Base_quickcheck.Generator.size with
  | 0 -> nonrec_gen
  | _ -> rec_gen
end

and quickcheck_generator_record_field_pattern = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map x0 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and x1 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  in
  (x0, x1)
end

and quickcheck_generator_expression = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pexp_desc =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression_desc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pexp_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pexp_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  in
  { pexp_desc; pexp_loc; pexp_attributes }
end

and quickcheck_generator_expression_desc = lazy begin
  let quickcheck_generator_pexp_ident =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pexp_ident (x0)
  and quickcheck_generator_pexp_constant =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_constant)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pexp_constant (x0)
  and quickcheck_generator_pexp_let =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_rec_flag)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_value_binding))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Pexp_let (x0, x1, x2)
  and quickcheck_generator_pexp_function =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_case))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pexp_function (x0)
  and quickcheck_generator_pexp_fun =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:4 ~max_length:4 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_arg_label)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    and x3 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
    in
    Astlib_first_draft_parsetree.Pexp_fun (x0, x1, x2, x3)
  and quickcheck_generator_pexp_apply =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_apply_arg))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pexp_apply (x0, x1)
  and quickcheck_generator_pexp_match =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_case))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pexp_match (x0, x1)
  and quickcheck_generator_pexp_try =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_case))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pexp_try (x0, x1)
  and quickcheck_generator_pexp_tuple =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pexp_tuple (x0)
  and quickcheck_generator_pexp_construct =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pexp_construct (x0, x1)
  and quickcheck_generator_pexp_variant =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_label)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pexp_variant (x0, x1)
  and quickcheck_generator_pexp_record =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_record_field_expression))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pexp_record (x0, x1)
  and quickcheck_generator_pexp_field =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pexp_field (x0, x1)
  and quickcheck_generator_pexp_setfield =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Pexp_setfield (x0, x1, x2)
  and quickcheck_generator_pexp_array =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pexp_array (x0)
  and quickcheck_generator_pexp_ifthenelse =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Pexp_ifthenelse (x0, x1, x2)
  and quickcheck_generator_pexp_sequence =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pexp_sequence (x0, x1)
  and quickcheck_generator_pexp_while =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pexp_while (x0, x1)
  and quickcheck_generator_pexp_for =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:5 ~max_length:5 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    and x3 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_direction_flag)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
    and x4 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(4)
    in
    Astlib_first_draft_parsetree.Pexp_for (x0, x1, x2, x3, x4)
  and quickcheck_generator_pexp_constraint =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pexp_constraint (x0, x1)
  and quickcheck_generator_pexp_coerce =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Pexp_coerce (x0, x1, x2)
  and quickcheck_generator_pexp_send =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_label_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pexp_send (x0, x1)
  and quickcheck_generator_pexp_new =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pexp_new (x0)
  and quickcheck_generator_pexp_setinstvar =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_label_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pexp_setinstvar (x0, x1)
  and quickcheck_generator_pexp_override =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_override_expression))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pexp_override (x0)
  and quickcheck_generator_pexp_letmodule =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_expr)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Pexp_letmodule (x0, x1, x2)
  and quickcheck_generator_pexp_letexception =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension_constructor)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pexp_letexception (x0, x1)
  and quickcheck_generator_pexp_assert =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pexp_assert (x0)
  and quickcheck_generator_pexp_lazy =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pexp_lazy (x0)
  and quickcheck_generator_pexp_poly =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pexp_poly (x0, x1)
  and quickcheck_generator_pexp_object =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_structure)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pexp_object (x0)
  and quickcheck_generator_pexp_newtype =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pexp_newtype (x0, x1)
  and quickcheck_generator_pexp_pack =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_expr)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pexp_pack (x0)
  and quickcheck_generator_pexp_open =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_override_flag)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Pexp_open (x0, x1, x2)
  and quickcheck_generator_pexp_extension =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pexp_extension (x0)
  and quickcheck_generator_pexp_unreachable =
    return Astlib_first_draft_parsetree.Pexp_unreachable
  in
  let nonrec_list =
    [ quickcheck_generator_pexp_unreachable
    ]
  in
  let rec_list =
    [ quickcheck_generator_pexp_ident
    ; quickcheck_generator_pexp_constant
    ; quickcheck_generator_pexp_let
    ; quickcheck_generator_pexp_function
    ; quickcheck_generator_pexp_fun
    ; quickcheck_generator_pexp_apply
    ; quickcheck_generator_pexp_match
    ; quickcheck_generator_pexp_try
    ; quickcheck_generator_pexp_tuple
    ; quickcheck_generator_pexp_construct
    ; quickcheck_generator_pexp_variant
    ; quickcheck_generator_pexp_record
    ; quickcheck_generator_pexp_field
    ; quickcheck_generator_pexp_setfield
    ; quickcheck_generator_pexp_array
    ; quickcheck_generator_pexp_ifthenelse
    ; quickcheck_generator_pexp_sequence
    ; quickcheck_generator_pexp_while
    ; quickcheck_generator_pexp_for
    ; quickcheck_generator_pexp_constraint
    ; quickcheck_generator_pexp_coerce
    ; quickcheck_generator_pexp_send
    ; quickcheck_generator_pexp_new
    ; quickcheck_generator_pexp_setinstvar
    ; quickcheck_generator_pexp_override
    ; quickcheck_generator_pexp_letmodule
    ; quickcheck_generator_pexp_letexception
    ; quickcheck_generator_pexp_assert
    ; quickcheck_generator_pexp_lazy
    ; quickcheck_generator_pexp_poly
    ; quickcheck_generator_pexp_object
    ; quickcheck_generator_pexp_newtype
    ; quickcheck_generator_pexp_pack
    ; quickcheck_generator_pexp_open
    ; quickcheck_generator_pexp_extension
    ]
    |> List.map ~f:with_decremented_size
  in
  let nonrec_gen = Base_quickcheck.Generator.union nonrec_list in
  let rec_gen = Base_quickcheck.Generator.union rec_list in
  match%bind Base_quickcheck.Generator.size with
  | 0 -> nonrec_gen
  | _ -> rec_gen
end

and quickcheck_generator_override_expression = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map x0 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_label_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and x1 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  in
  (x0, x1)
end

and quickcheck_generator_record_field_expression = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map x0 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and x1 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  in
  (x0, x1)
end

and quickcheck_generator_apply_arg = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map x0 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_arg_label)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and x1 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  in
  (x0, x1)
end

and quickcheck_generator_case = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pc_lhs =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pc_guard =
    (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression))
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pc_rhs =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  in
  { pc_lhs; pc_guard; pc_rhs }
end

and quickcheck_generator_value_description = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:5 ~max_length:5 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pval_name =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pval_type =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pval_prim =
    (quickcheck_generator_list quickcheck_generator_string)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  and pval_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
  and pval_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(4)
  in
  { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }
end

and quickcheck_generator_type_declaration = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:8 ~max_length:8 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map ptype_name =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and ptype_params =
    (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_type_param))
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and ptype_cstrs =
    (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_type_constraint))
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  and ptype_kind =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_type_kind)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
  and ptype_private =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_private_flag)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(4)
  and ptype_manifest =
    (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type))
    |> Base_quickcheck.Generator.with_size ~size:sizes.(5)
  and ptype_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(6)
  and ptype_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(7)
  in
  { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc }
end

and quickcheck_generator_type_param = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map x0 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and x1 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_variance)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  in
  (x0, x1)
end

and quickcheck_generator_type_constraint = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map x0 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and x1 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and x2 =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  in
  (x0, x1, x2)
end

and quickcheck_generator_type_kind = lazy begin
  let quickcheck_generator_ptype_abstract =
    return Astlib_first_draft_parsetree.Ptype_abstract
  and quickcheck_generator_ptype_variant =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_constructor_declaration))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ptype_variant (x0)
  and quickcheck_generator_ptype_record =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_label_declaration))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ptype_record (x0)
  and quickcheck_generator_ptype_open =
    return Astlib_first_draft_parsetree.Ptype_open
  in
  let nonrec_list =
    [ quickcheck_generator_ptype_abstract
    ; quickcheck_generator_ptype_open
    ]
  in
  let rec_list =
    [ quickcheck_generator_ptype_variant
    ; quickcheck_generator_ptype_record
    ]
    |> List.map ~f:with_decremented_size
  in
  let nonrec_gen = Base_quickcheck.Generator.union nonrec_list in
  let rec_gen = Base_quickcheck.Generator.union rec_list in
  match%bind Base_quickcheck.Generator.size with
  | 0 -> nonrec_gen
  | _ -> rec_gen
end

and quickcheck_generator_label_declaration = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:5 ~max_length:5 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pld_name =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pld_mutable =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_mutable_flag)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pld_type =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  and pld_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
  and pld_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(4)
  in
  { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }
end

and quickcheck_generator_constructor_declaration = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:5 ~max_length:5 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pcd_name =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pcd_args =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_constructor_arguments)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pcd_res =
    (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type))
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  and pcd_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
  and pcd_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(4)
  in
  { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }
end

and quickcheck_generator_constructor_arguments = lazy begin
  let quickcheck_generator_pcstr_tuple =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pcstr_tuple (x0)
  and quickcheck_generator_pcstr_record =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_label_declaration))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pcstr_record (x0)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_pcstr_tuple
    ; quickcheck_generator_pcstr_record
    ]
end

and quickcheck_generator_type_extension = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:5 ~max_length:5 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map ptyext_path =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and ptyext_params =
    (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_type_param))
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and ptyext_constructors =
    (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension_constructor))
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  and ptyext_private =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_private_flag)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
  and ptyext_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(4)
  in
  { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes }
end

and quickcheck_generator_extension_constructor = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:4 ~max_length:4 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pext_name =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pext_kind =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension_constructor_kind)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pext_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  and pext_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
  in
  { pext_name; pext_kind; pext_loc; pext_attributes }
end

and quickcheck_generator_extension_constructor_kind = lazy begin
  let quickcheck_generator_pext_decl =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_constructor_arguments)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pext_decl (x0, x1)
  and quickcheck_generator_pext_rebind =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pext_rebind (x0)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_pext_decl
    ; quickcheck_generator_pext_rebind
    ]
end

and quickcheck_generator_class_type = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pcty_desc =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_desc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pcty_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pcty_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  in
  { pcty_desc; pcty_loc; pcty_attributes }
end

and quickcheck_generator_class_type_desc = lazy begin
  let quickcheck_generator_pcty_constr =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pcty_constr (x0, x1)
  and quickcheck_generator_pcty_signature =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_signature)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pcty_signature (x0)
  and quickcheck_generator_pcty_arrow =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_arg_label)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Pcty_arrow (x0, x1, x2)
  and quickcheck_generator_pcty_extension =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pcty_extension (x0)
  and quickcheck_generator_pcty_open =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_override_flag)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Pcty_open (x0, x1, x2)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_pcty_constr
    ; quickcheck_generator_pcty_signature
    ; quickcheck_generator_pcty_arrow
    ; quickcheck_generator_pcty_extension
    ; quickcheck_generator_pcty_open
    ]
end

and quickcheck_generator_class_signature = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pcsig_self =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pcsig_fields =
    (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_field))
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  in
  { pcsig_self; pcsig_fields }
end

and quickcheck_generator_class_type_field = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pctf_desc =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_field_desc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pctf_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pctf_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  in
  { pctf_desc; pctf_loc; pctf_attributes }
end

and quickcheck_generator_class_type_field_desc = lazy begin
  let quickcheck_generator_pctf_inherit =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pctf_inherit (x0)
  and quickcheck_generator_pctf_val =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_value_desc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pctf_val (x0)
  and quickcheck_generator_pctf_method =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_method_desc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pctf_method (x0)
  and quickcheck_generator_pctf_constraint =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_constraint)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pctf_constraint (x0)
  and quickcheck_generator_pctf_attribute =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_attribute)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pctf_attribute (x0)
  and quickcheck_generator_pctf_extension =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pctf_extension (x0)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_pctf_inherit
    ; quickcheck_generator_pctf_val
    ; quickcheck_generator_pctf_method
    ; quickcheck_generator_pctf_constraint
    ; quickcheck_generator_pctf_attribute
    ; quickcheck_generator_pctf_extension
    ]
end

and quickcheck_generator_class_type_value_desc = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:4 ~max_length:4 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map x0 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_label_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and x1 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_mutable_flag)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and x2 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_virtual_flag)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  and x3 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
  in
  (x0, x1, x2, x3)
end

and quickcheck_generator_class_type_method_desc = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:4 ~max_length:4 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map x0 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_label_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and x1 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_private_flag)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and x2 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_virtual_flag)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  and x3 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
  in
  (x0, x1, x2, x3)
end

and quickcheck_generator_class_type_constraint = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map x0 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and x1 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  in
  (x0, x1)
end

and quickcheck_generator_class_infos
  : type a . a Base_quickcheck.Generator.t -> a class_infos Base_quickcheck.Generator.t
  = fun quickcheck_generator_a ->
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:6 ~max_length:6 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map pci_virt =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_virtual_flag)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and pci_params =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_type_param))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and pci_name =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    and pci_expr =
      quickcheck_generator_a
      |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
    and pci_loc =
      Location_extended.quickcheck_generator
      |> Base_quickcheck.Generator.with_size ~size:sizes.(4)
    and pci_attributes =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(5)
    in
    { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }

and quickcheck_generator_class_description =
  lazy (quickcheck_generator_class_infos (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type))

and quickcheck_generator_class_type_declaration =
  lazy (quickcheck_generator_class_infos (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type))

and quickcheck_generator_class_expr = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pcl_desc =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_expr_desc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pcl_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pcl_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  in
  { pcl_desc; pcl_loc; pcl_attributes }
end

and quickcheck_generator_class_expr_desc = lazy begin
  let quickcheck_generator_pcl_constr =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pcl_constr (x0, x1)
  and quickcheck_generator_pcl_structure =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_structure)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pcl_structure (x0)
  and quickcheck_generator_pcl_fun =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:4 ~max_length:4 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_arg_label)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    and x3 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_expr)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
    in
    Astlib_first_draft_parsetree.Pcl_fun (x0, x1, x2, x3)
  and quickcheck_generator_pcl_apply =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_expr)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_apply_arg))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pcl_apply (x0, x1)
  and quickcheck_generator_pcl_let =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_rec_flag)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_value_binding))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_expr)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Pcl_let (x0, x1, x2)
  and quickcheck_generator_pcl_constraint =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_expr)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pcl_constraint (x0, x1)
  and quickcheck_generator_pcl_extension =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pcl_extension (x0)
  and quickcheck_generator_pcl_open =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_override_flag)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_expr)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Pcl_open (x0, x1, x2)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_pcl_constr
    ; quickcheck_generator_pcl_structure
    ; quickcheck_generator_pcl_fun
    ; quickcheck_generator_pcl_apply
    ; quickcheck_generator_pcl_let
    ; quickcheck_generator_pcl_constraint
    ; quickcheck_generator_pcl_extension
    ; quickcheck_generator_pcl_open
    ]
end

and quickcheck_generator_class_structure = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pcstr_self =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pcstr_fields =
    (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_field))
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  in
  { pcstr_self; pcstr_fields }
end

and quickcheck_generator_class_field = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pcf_desc =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_field_desc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pcf_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pcf_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  in
  { pcf_desc; pcf_loc; pcf_attributes }
end

and quickcheck_generator_class_field_desc = lazy begin
  let quickcheck_generator_pcf_inherit =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_override_flag)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_expr)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Pcf_inherit (x0, x1, x2)
  and quickcheck_generator_pcf_val =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_value_desc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pcf_val (x0)
  and quickcheck_generator_pcf_method =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_method_desc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pcf_method (x0)
  and quickcheck_generator_pcf_constraint =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_constraint)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pcf_constraint (x0)
  and quickcheck_generator_pcf_initializer =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pcf_initializer (x0)
  and quickcheck_generator_pcf_attribute =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_attribute)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pcf_attribute (x0)
  and quickcheck_generator_pcf_extension =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pcf_extension (x0)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_pcf_inherit
    ; quickcheck_generator_pcf_val
    ; quickcheck_generator_pcf_method
    ; quickcheck_generator_pcf_constraint
    ; quickcheck_generator_pcf_initializer
    ; quickcheck_generator_pcf_attribute
    ; quickcheck_generator_pcf_extension
    ]
end

and quickcheck_generator_class_value_desc = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map x0 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_label_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and x1 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_mutable_flag)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and x2 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_field_kind)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  in
  (x0, x1, x2)
end

and quickcheck_generator_class_method_desc = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map x0 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_label_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and x1 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_private_flag)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and x2 =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_field_kind)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  in
  (x0, x1, x2)
end

and quickcheck_generator_class_field_kind = lazy begin
  let quickcheck_generator_cfk_virtual =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Cfk_virtual (x0)
  and quickcheck_generator_cfk_concrete =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_override_flag)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Cfk_concrete (x0, x1)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_cfk_virtual
    ; quickcheck_generator_cfk_concrete
    ]
end

and quickcheck_generator_class_declaration =
  lazy (quickcheck_generator_class_infos (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_expr))

and quickcheck_generator_module_type = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pmty_desc =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_type_desc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pmty_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pmty_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  in
  { pmty_desc; pmty_loc; pmty_attributes }
end

and quickcheck_generator_module_type_desc = lazy begin
  let quickcheck_generator_pmty_ident =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pmty_ident (x0)
  and quickcheck_generator_pmty_signature =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_signature)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pmty_signature (x0)
  and quickcheck_generator_pmty_functor =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_type))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Pmty_functor (x0, x1, x2)
  and quickcheck_generator_pmty_with =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_with_constraint))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pmty_with (x0, x1)
  and quickcheck_generator_pmty_typeof =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_expr)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pmty_typeof (x0)
  and quickcheck_generator_pmty_extension =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pmty_extension (x0)
  and quickcheck_generator_pmty_alias =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pmty_alias (x0)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_pmty_ident
    ; quickcheck_generator_pmty_signature
    ; quickcheck_generator_pmty_functor
    ; quickcheck_generator_pmty_with
    ; quickcheck_generator_pmty_typeof
    ; quickcheck_generator_pmty_extension
    ; quickcheck_generator_pmty_alias
    ]
end

and quickcheck_generator_signature = lazy begin
  (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_signature_item))
end

and quickcheck_generator_signature_item = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map psig_desc =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_signature_item_desc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and psig_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  in
  { psig_desc; psig_loc }
end

and quickcheck_generator_signature_item_desc = lazy begin
  let quickcheck_generator_psig_value =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_value_description)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Psig_value (x0)
  and quickcheck_generator_psig_type =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_rec_flag)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_type_declaration))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Psig_type (x0, x1)
  and quickcheck_generator_psig_typext =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_type_extension)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Psig_typext (x0)
  and quickcheck_generator_psig_exception =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension_constructor)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Psig_exception (x0)
  and quickcheck_generator_psig_module =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_declaration)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Psig_module (x0)
  and quickcheck_generator_psig_recmodule =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_declaration))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Psig_recmodule (x0)
  and quickcheck_generator_psig_modtype =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_type_declaration)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Psig_modtype (x0)
  and quickcheck_generator_psig_open =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_open_description)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Psig_open (x0)
  and quickcheck_generator_psig_include =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_include_description)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Psig_include (x0)
  and quickcheck_generator_psig_class =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_description))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Psig_class (x0)
  and quickcheck_generator_psig_class_type =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_declaration))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Psig_class_type (x0)
  and quickcheck_generator_psig_attribute =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_attribute)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Psig_attribute (x0)
  and quickcheck_generator_psig_extension =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Psig_extension (x0, x1)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_psig_value
    ; quickcheck_generator_psig_type
    ; quickcheck_generator_psig_typext
    ; quickcheck_generator_psig_exception
    ; quickcheck_generator_psig_module
    ; quickcheck_generator_psig_recmodule
    ; quickcheck_generator_psig_modtype
    ; quickcheck_generator_psig_open
    ; quickcheck_generator_psig_include
    ; quickcheck_generator_psig_class
    ; quickcheck_generator_psig_class_type
    ; quickcheck_generator_psig_attribute
    ; quickcheck_generator_psig_extension
    ]
end

and quickcheck_generator_module_declaration = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:4 ~max_length:4 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pmd_name =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pmd_type =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_type)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pmd_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  and pmd_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
  in
  { pmd_name; pmd_type; pmd_attributes; pmd_loc }
end

and quickcheck_generator_module_type_declaration = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:4 ~max_length:4 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pmtd_name =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pmtd_type =
    (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_type))
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pmtd_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  and pmtd_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
  in
  { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }
end

and quickcheck_generator_open_description = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:4 ~max_length:4 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map popen_lid =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and popen_override =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_override_flag)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and popen_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  and popen_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
  in
  { popen_lid; popen_override; popen_loc; popen_attributes }
end

and quickcheck_generator_include_infos
  : type a . a Base_quickcheck.Generator.t -> a include_infos Base_quickcheck.Generator.t
  = fun quickcheck_generator_a ->
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map pincl_mod =
      quickcheck_generator_a
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and pincl_loc =
      Location_extended.quickcheck_generator
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and pincl_attributes =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    { pincl_mod; pincl_loc; pincl_attributes }

and quickcheck_generator_include_description =
  lazy (quickcheck_generator_include_infos (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_type))

and quickcheck_generator_include_declaration =
  lazy (quickcheck_generator_include_infos (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_expr))

and quickcheck_generator_with_constraint = lazy begin
  let quickcheck_generator_pwith_type =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_type_declaration)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pwith_type (x0, x1)
  and quickcheck_generator_pwith_module =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pwith_module (x0, x1)
  and quickcheck_generator_pwith_typesubst =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_type_declaration)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pwith_typesubst (x0, x1)
  and quickcheck_generator_pwith_modsubst =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pwith_modsubst (x0, x1)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_pwith_type
    ; quickcheck_generator_pwith_module
    ; quickcheck_generator_pwith_typesubst
    ; quickcheck_generator_pwith_modsubst
    ]
end

and quickcheck_generator_module_expr = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pmod_desc =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_expr_desc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pmod_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pmod_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  in
  { pmod_desc; pmod_loc; pmod_attributes }
end

and quickcheck_generator_module_expr_desc = lazy begin
  let quickcheck_generator_pmod_ident =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pmod_ident (x0)
  and quickcheck_generator_pmod_structure =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_structure)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pmod_structure (x0)
  and quickcheck_generator_pmod_functor =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_type))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    and x2 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_expr)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
    in
    Astlib_first_draft_parsetree.Pmod_functor (x0, x1, x2)
  and quickcheck_generator_pmod_apply =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_expr)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_expr)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pmod_apply (x0, x1)
  and quickcheck_generator_pmod_constraint =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_expr)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_type)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pmod_constraint (x0, x1)
  and quickcheck_generator_pmod_unpack =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pmod_unpack (x0)
  and quickcheck_generator_pmod_extension =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pmod_extension (x0)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_pmod_ident
    ; quickcheck_generator_pmod_structure
    ; quickcheck_generator_pmod_functor
    ; quickcheck_generator_pmod_apply
    ; quickcheck_generator_pmod_constraint
    ; quickcheck_generator_pmod_unpack
    ; quickcheck_generator_pmod_extension
    ]
end

and quickcheck_generator_structure = lazy begin
  (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_structure_item))
end

and quickcheck_generator_structure_item = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pstr_desc =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_structure_item_desc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pstr_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  in
  { pstr_desc; pstr_loc }
end

and quickcheck_generator_structure_item_desc = lazy begin
  let quickcheck_generator_pstr_eval =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pstr_eval (x0, x1)
  and quickcheck_generator_pstr_value =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_rec_flag)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_value_binding))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pstr_value (x0, x1)
  and quickcheck_generator_pstr_primitive =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_value_description)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pstr_primitive (x0)
  and quickcheck_generator_pstr_type =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_rec_flag)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_type_declaration))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pstr_type (x0, x1)
  and quickcheck_generator_pstr_typext =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_type_extension)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pstr_typext (x0)
  and quickcheck_generator_pstr_exception =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension_constructor)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pstr_exception (x0)
  and quickcheck_generator_pstr_module =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_binding)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pstr_module (x0)
  and quickcheck_generator_pstr_recmodule =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_binding))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pstr_recmodule (x0)
  and quickcheck_generator_pstr_modtype =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_type_declaration)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pstr_modtype (x0)
  and quickcheck_generator_pstr_open =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_open_description)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pstr_open (x0)
  and quickcheck_generator_pstr_class =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_declaration))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pstr_class (x0)
  and quickcheck_generator_pstr_class_type =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (quickcheck_generator_list (Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_declaration))
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pstr_class_type (x0)
  and quickcheck_generator_pstr_include =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_include_declaration)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pstr_include (x0)
  and quickcheck_generator_pstr_attribute =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_attribute)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pstr_attribute (x0)
  and quickcheck_generator_pstr_extension =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_extension)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pstr_extension (x0, x1)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_pstr_eval
    ; quickcheck_generator_pstr_value
    ; quickcheck_generator_pstr_primitive
    ; quickcheck_generator_pstr_type
    ; quickcheck_generator_pstr_typext
    ; quickcheck_generator_pstr_exception
    ; quickcheck_generator_pstr_module
    ; quickcheck_generator_pstr_recmodule
    ; quickcheck_generator_pstr_modtype
    ; quickcheck_generator_pstr_open
    ; quickcheck_generator_pstr_class
    ; quickcheck_generator_pstr_class_type
    ; quickcheck_generator_pstr_include
    ; quickcheck_generator_pstr_attribute
    ; quickcheck_generator_pstr_extension
    ]
end

and quickcheck_generator_value_binding = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:4 ~max_length:4 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pvb_pat =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pvb_expr =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_expression)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pvb_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  and pvb_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
  in
  { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }
end

and quickcheck_generator_module_binding = lazy begin
  let%bind sizes =
    Base_quickcheck.Generator.sizes ~min_length:4 ~max_length:4 ()
    |> Base_quickcheck.Generator.map ~f:Array.of_list
  in
  let%map pmb_name =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
  and pmb_expr =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_module_expr)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
  and pmb_attributes =
    (Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes)
    |> Base_quickcheck.Generator.with_size ~size:sizes.(2)
  and pmb_loc =
    Location_extended.quickcheck_generator
    |> Base_quickcheck.Generator.with_size ~size:sizes.(3)
  in
  { pmb_name; pmb_expr; pmb_attributes; pmb_loc }
end

and quickcheck_generator_toplevel_phrase = lazy begin
  let quickcheck_generator_ptop_def =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_structure)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Ptop_def (x0)
  and quickcheck_generator_ptop_dir =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      quickcheck_generator_string
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_directive_argument)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Ptop_dir (x0, x1)
  in
  Base_quickcheck.Generator.union
    [ quickcheck_generator_ptop_def
    ; quickcheck_generator_ptop_dir
    ]
end

and quickcheck_generator_directive_argument = lazy begin
  let quickcheck_generator_pdir_none =
    return Astlib_first_draft_parsetree.Pdir_none
  and quickcheck_generator_pdir_string =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      quickcheck_generator_string
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pdir_string (x0)
  and quickcheck_generator_pdir_int =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      quickcheck_generator_string
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    and x1 =
      (quickcheck_generator_option quickcheck_generator_char)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(1)
    in
    Astlib_first_draft_parsetree.Pdir_int (x0, x1)
  and quickcheck_generator_pdir_ident =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      (Base_quickcheck.Generator.of_lazy quickcheck_generator_longident)
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pdir_ident (x0)
  and quickcheck_generator_pdir_bool =
    let%bind sizes =
      Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      |> Base_quickcheck.Generator.map ~f:Array.of_list
    in
    let%map x0 =
      quickcheck_generator_bool
      |> Base_quickcheck.Generator.with_size ~size:sizes.(0)
    in
    Astlib_first_draft_parsetree.Pdir_bool (x0)
  in
  let nonrec_list =
    [ quickcheck_generator_pdir_none
    ; quickcheck_generator_pdir_string
    ; quickcheck_generator_pdir_int
    ; quickcheck_generator_pdir_bool
    ]
  in
  let rec_list =
    [ quickcheck_generator_pdir_ident
    ]
    |> List.map ~f:with_decremented_size
  in
  let nonrec_gen = Base_quickcheck.Generator.union nonrec_list in
  let rec_gen = Base_quickcheck.Generator.union rec_list in
  match%bind Base_quickcheck.Generator.size with
  | 0 -> nonrec_gen
  | _ -> rec_gen
end

module Loc = struct
  type 'a t = 'a loc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator = quickcheck_generator_loc
  let quickcheck_observer quickcheck_observer_a =
    let hash_fold_a hash a =
      Base_quickcheck.Observer.observe quickcheck_observer_a a ~size:0 ~hash
    in
    Base_quickcheck.Observer.of_hash_fold (hash_fold_loc hash_fold_a)
    let quickcheck_shrinker _ = Base_quickcheck.Shrinker.atomic
end

module Longident = struct
  type t = longident
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_longident
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_longident
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Longident_loc = struct
  type t = longident_loc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_longident_loc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_longident_loc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Rec_flag = struct
  type t = rec_flag
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_rec_flag
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_rec_flag
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Direction_flag = struct
  type t = direction_flag
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_direction_flag
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_direction_flag
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Private_flag = struct
  type t = private_flag
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_private_flag
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_private_flag
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Mutable_flag = struct
  type t = mutable_flag
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_mutable_flag
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_mutable_flag
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Virtual_flag = struct
  type t = virtual_flag
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_virtual_flag
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_virtual_flag
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Override_flag = struct
  type t = override_flag
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_override_flag
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_override_flag
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Closed_flag = struct
  type t = closed_flag
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_closed_flag
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_closed_flag
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Label = struct
  type t = label
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_label
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_label
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Label_loc = struct
  type t = label_loc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_label_loc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_label_loc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module String_loc = struct
  type t = string_loc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_string_loc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_string_loc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Arg_label = struct
  type t = arg_label
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_arg_label
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_arg_label
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Variance = struct
  type t = variance
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_variance
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_variance
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Constant = struct
  type t = constant
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_constant
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_constant
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Attribute = struct
  type t = attribute
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_attribute
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_attribute
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Extension = struct
  type t = extension
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_extension
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_extension
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Attributes = struct
  type t = attributes
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_attributes
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_attributes
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Payload = struct
  type t = payload
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_payload
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_payload
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Core_type = struct
  type t = core_type
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_core_type
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Core_type_desc = struct
  type t = core_type_desc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_core_type_desc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_core_type_desc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Package_type = struct
  type t = package_type
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_package_type
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_package_type
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Package_type_constraint = struct
  type t = package_type_constraint
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_package_type_constraint
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_package_type_constraint
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Row_field = struct
  type t = row_field
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_row_field
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_row_field
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Object_field = struct
  type t = object_field
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_object_field
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_object_field
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Pattern = struct
  type t = pattern
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_pattern
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Pattern_desc = struct
  type t = pattern_desc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_pattern_desc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_pattern_desc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Record_field_pattern = struct
  type t = record_field_pattern
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_record_field_pattern
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_record_field_pattern
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Expression = struct
  type t = expression
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_expression
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_expression
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Expression_desc = struct
  type t = expression_desc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_expression_desc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_expression_desc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Override_expression = struct
  type t = override_expression
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_override_expression
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_override_expression
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Record_field_expression = struct
  type t = record_field_expression
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_record_field_expression
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_record_field_expression
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Apply_arg = struct
  type t = apply_arg
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_apply_arg
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_apply_arg
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Case = struct
  type t = case
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_case
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_case
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Value_description = struct
  type t = value_description
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_value_description
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_value_description
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Type_declaration = struct
  type t = type_declaration
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_type_declaration
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_type_declaration
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Type_param = struct
  type t = type_param
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_type_param
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_type_param
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Type_constraint = struct
  type t = type_constraint
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_type_constraint
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_type_constraint
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Type_kind = struct
  type t = type_kind
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_type_kind
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_type_kind
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Label_declaration = struct
  type t = label_declaration
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_label_declaration
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_label_declaration
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Constructor_declaration = struct
  type t = constructor_declaration
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_constructor_declaration
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_constructor_declaration
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Constructor_arguments = struct
  type t = constructor_arguments
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_constructor_arguments
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_constructor_arguments
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Type_extension = struct
  type t = type_extension
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_type_extension
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_type_extension
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Extension_constructor = struct
  type t = extension_constructor
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_extension_constructor
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_extension_constructor
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Extension_constructor_kind = struct
  type t = extension_constructor_kind
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_extension_constructor_kind
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_extension_constructor_kind
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_type = struct
  type t = class_type
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_type
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_type_desc = struct
  type t = class_type_desc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_desc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_type_desc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_signature = struct
  type t = class_signature
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_signature
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_signature
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_type_field = struct
  type t = class_type_field
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_field
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_type_field
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_type_field_desc = struct
  type t = class_type_field_desc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_field_desc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_type_field_desc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_type_value_desc = struct
  type t = class_type_value_desc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_value_desc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_type_value_desc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_type_method_desc = struct
  type t = class_type_method_desc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_method_desc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_type_method_desc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_type_constraint = struct
  type t = class_type_constraint
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_constraint
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_type_constraint
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_infos = struct
  type 'a t = 'a class_infos
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator = quickcheck_generator_class_infos
  let quickcheck_observer quickcheck_observer_a =
    let hash_fold_a hash a =
      Base_quickcheck.Observer.observe quickcheck_observer_a a ~size:0 ~hash
    in
    Base_quickcheck.Observer.of_hash_fold (hash_fold_class_infos hash_fold_a)
    let quickcheck_shrinker _ = Base_quickcheck.Shrinker.atomic
end

module Class_description = struct
  type t = class_description
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_description
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_description
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_type_declaration = struct
  type t = class_type_declaration
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_type_declaration
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_type_declaration
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_expr = struct
  type t = class_expr
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_expr
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_expr
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_expr_desc = struct
  type t = class_expr_desc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_expr_desc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_expr_desc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_structure = struct
  type t = class_structure
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_structure
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_structure
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_field = struct
  type t = class_field
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_field
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_field
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_field_desc = struct
  type t = class_field_desc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_field_desc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_field_desc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_value_desc = struct
  type t = class_value_desc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_value_desc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_value_desc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_method_desc = struct
  type t = class_method_desc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_method_desc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_method_desc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_field_kind = struct
  type t = class_field_kind
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_field_kind
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_field_kind
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Class_declaration = struct
  type t = class_declaration
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_class_declaration
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_class_declaration
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Module_type = struct
  type t = module_type
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_module_type
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_module_type
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Module_type_desc = struct
  type t = module_type_desc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_module_type_desc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_module_type_desc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Signature = struct
  type t = signature
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_signature
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_signature
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Signature_item = struct
  type t = signature_item
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_signature_item
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_signature_item
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Signature_item_desc = struct
  type t = signature_item_desc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_signature_item_desc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_signature_item_desc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Module_declaration = struct
  type t = module_declaration
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_module_declaration
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_module_declaration
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Module_type_declaration = struct
  type t = module_type_declaration
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_module_type_declaration
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_module_type_declaration
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Open_description = struct
  type t = open_description
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_open_description
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_open_description
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Include_infos = struct
  type 'a t = 'a include_infos
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator = quickcheck_generator_include_infos
  let quickcheck_observer quickcheck_observer_a =
    let hash_fold_a hash a =
      Base_quickcheck.Observer.observe quickcheck_observer_a a ~size:0 ~hash
    in
    Base_quickcheck.Observer.of_hash_fold (hash_fold_include_infos hash_fold_a)
    let quickcheck_shrinker _ = Base_quickcheck.Shrinker.atomic
end

module Include_description = struct
  type t = include_description
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_include_description
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_include_description
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Include_declaration = struct
  type t = include_declaration
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_include_declaration
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_include_declaration
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module With_constraint = struct
  type t = with_constraint
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_with_constraint
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_with_constraint
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Module_expr = struct
  type t = module_expr
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_module_expr
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_module_expr
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Module_expr_desc = struct
  type t = module_expr_desc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_module_expr_desc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_module_expr_desc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Structure = struct
  type t = structure
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_structure
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_structure
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Structure_item = struct
  type t = structure_item
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_structure_item
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_structure_item
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Structure_item_desc = struct
  type t = structure_item_desc
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_structure_item_desc
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_structure_item_desc
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Value_binding = struct
  type t = value_binding
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_value_binding
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_value_binding
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Module_binding = struct
  type t = module_binding
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_module_binding
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_module_binding
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Toplevel_phrase = struct
  type t = toplevel_phrase
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_toplevel_phrase
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_toplevel_phrase
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

module Directive_argument = struct
  type t = directive_argument
  [@@deriving compare, equal, hash, sexp_of]

  let quickcheck_generator =
    Base_quickcheck.Generator.of_lazy quickcheck_generator_directive_argument
  let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_directive_argument
  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end
(*$*)
