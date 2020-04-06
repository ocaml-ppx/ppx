open Base
open Base_quickcheck
open Ppx_ast

type position = Lexing.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
[@@deriving equal, quickcheck, sexp_of]

type location = Location.t =
  { loc_start : position
  ; loc_end : position
  ; loc_ghost : bool
  }
[@@deriving equal, quickcheck, sexp_of]

type 'a loc = 'a Location.loc = { txt : 'a; loc : location }
[@@deriving equal, quickcheck, sexp_of]

let quickcheck_generator_tuple2 gen1 gen2 =
  Base_quickcheck.Generator.create (fun ~size ~random ->
    let x1 = Base_quickcheck.Generator.generate gen1 ~size ~random in
    let x2 = Base_quickcheck.Generator.generate gen2 ~size ~random in
    (x1, x2))

let quickcheck_generator_tuple3 gen1 gen2 gen3 =
  Base_quickcheck.Generator.create (fun ~size ~random ->
    let x1 = Base_quickcheck.Generator.generate gen1 ~size ~random in
    let x2 = Base_quickcheck.Generator.generate gen2 ~size ~random in
    let x3 = Base_quickcheck.Generator.generate gen3 ~size ~random in
    (x1, x2, x3))

let quickcheck_generator_tuple4 gen1 gen2 gen3 gen4 =
  Base_quickcheck.Generator.create (fun ~size ~random ->
    let x1 = Base_quickcheck.Generator.generate gen1 ~size ~random in
    let x2 = Base_quickcheck.Generator.generate gen2 ~size ~random in
    let x3 = Base_quickcheck.Generator.generate gen3 ~size ~random in
    let x4 = Base_quickcheck.Generator.generate gen4 ~size ~random in
    (x1, x2, x3, x4))

(*$ Ppx_ast_tests_cinaps.print_deriving_ml () *)
type longident =
  Compiler_types.longident =
  | Lident of string
  | Ldot of longident * string
  | Lapply of longident * longident

and longident_loc =
  longident loc

and rec_flag =
  Compiler_types.rec_flag =
  | Nonrecursive
  | Recursive

and direction_flag =
  Compiler_types.direction_flag =
  | Upto
  | Downto

and private_flag =
  Compiler_types.private_flag =
  | Private
  | Public

and mutable_flag =
  Compiler_types.mutable_flag =
  | Immutable
  | Mutable

and virtual_flag =
  Compiler_types.virtual_flag =
  | Virtual
  | Concrete

and override_flag =
  Compiler_types.override_flag =
  | Override
  | Fresh

and closed_flag =
  Compiler_types.closed_flag =
  | Closed
  | Open

and arg_label =
  Compiler_types.arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string

and variance =
  Compiler_types.variance =
  | Covariant
  | Contravariant
  | Invariant

and constant =
  Compiler_types.constant =
  | Pconst_integer of string * char option
  | Pconst_char of char
  | Pconst_string of string * string option
  | Pconst_float of string * char option

and attribute =
  (string loc * payload)

and extension =
  (string loc * payload)

and attributes =
  attribute list

and payload =
  Compiler_types.payload =
  | PStr of structure
  | PSig of signature
  | PTyp of core_type
  | PPat of pattern * expression option

and core_type =
  Compiler_types.core_type =
  { ptyp_desc : core_type_desc
  ; ptyp_loc : location
  ; ptyp_attributes : attributes
  }

and core_type_desc =
  Compiler_types.core_type_desc =
  | Ptyp_any
  | Ptyp_var of string
  | Ptyp_arrow of arg_label * core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_constr of longident_loc * core_type list
  | Ptyp_object of object_field list * closed_flag
  | Ptyp_class of longident_loc * core_type list
  | Ptyp_alias of core_type * string
  | Ptyp_variant of row_field list * closed_flag * string list option
  | Ptyp_poly of string loc list * core_type
  | Ptyp_package of package_type
  | Ptyp_extension of extension

and package_type =
  (longident_loc * (longident_loc * core_type) list)

and row_field =
  Compiler_types.row_field =
  | Rtag of string loc * attributes * bool * core_type list
  | Rinherit of core_type

and object_field =
  Compiler_types.object_field =
  | Otag of string loc * attributes * core_type
  | Oinherit of core_type

and pattern =
  Compiler_types.pattern =
  { ppat_desc : pattern_desc
  ; ppat_loc : location
  ; ppat_attributes : attributes
  }

and pattern_desc =
  Compiler_types.pattern_desc =
  | Ppat_any
  | Ppat_var of string loc
  | Ppat_alias of pattern * string loc
  | Ppat_constant of constant
  | Ppat_interval of constant * constant
  | Ppat_tuple of pattern list
  | Ppat_construct of longident_loc * pattern option
  | Ppat_variant of string * pattern option
  | Ppat_record of (longident_loc * pattern) list * closed_flag
  | Ppat_array of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_type
  | Ppat_type of longident_loc
  | Ppat_lazy of pattern
  | Ppat_unpack of string loc
  | Ppat_exception of pattern
  | Ppat_extension of extension
  | Ppat_open of longident_loc * pattern

and expression =
  Compiler_types.expression =
  { pexp_desc : expression_desc
  ; pexp_loc : location
  ; pexp_attributes : attributes
  }

and expression_desc =
  Compiler_types.expression_desc =
  | Pexp_ident of longident_loc
  | Pexp_constant of constant
  | Pexp_let of rec_flag * value_binding list * expression
  | Pexp_function of case list
  | Pexp_fun of arg_label * expression option * pattern * expression
  | Pexp_apply of expression * (arg_label * expression) list
  | Pexp_match of expression * case list
  | Pexp_try of expression * case list
  | Pexp_tuple of expression list
  | Pexp_construct of longident_loc * expression option
  | Pexp_variant of string * expression option
  | Pexp_record of (longident_loc * expression) list * expression option
  | Pexp_field of expression * longident_loc
  | Pexp_setfield of expression * longident_loc * expression
  | Pexp_array of expression list
  | Pexp_ifthenelse of expression * expression * expression option
  | Pexp_sequence of expression * expression
  | Pexp_while of expression * expression
  | Pexp_for of pattern * expression * expression * direction_flag * expression
  | Pexp_constraint of expression * core_type
  | Pexp_coerce of expression * core_type option * core_type
  | Pexp_send of expression * string loc
  | Pexp_new of longident_loc
  | Pexp_setinstvar of string loc * expression
  | Pexp_override of (string loc * expression) list
  | Pexp_letmodule of string loc * module_expr * expression
  | Pexp_letexception of extension_constructor * expression
  | Pexp_assert of expression
  | Pexp_lazy of expression
  | Pexp_poly of expression * core_type option
  | Pexp_object of class_structure
  | Pexp_newtype of string loc * expression
  | Pexp_pack of module_expr
  | Pexp_open of override_flag * longident_loc * expression
  | Pexp_extension of extension
  | Pexp_unreachable

and case =
  Compiler_types.case =
  { pc_lhs : pattern
  ; pc_guard : expression option
  ; pc_rhs : expression
  }

and value_description =
  Compiler_types.value_description =
  { pval_name : string loc
  ; pval_type : core_type
  ; pval_prim : string list
  ; pval_attributes : attributes
  ; pval_loc : location
  }

and type_declaration =
  Compiler_types.type_declaration =
  { ptype_name : string loc
  ; ptype_params : (core_type * variance) list
  ; ptype_cstrs : (core_type * core_type * location) list
  ; ptype_kind : type_kind
  ; ptype_private : private_flag
  ; ptype_manifest : core_type option
  ; ptype_attributes : attributes
  ; ptype_loc : location
  }

and type_kind =
  Compiler_types.type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list
  | Ptype_record of label_declaration list
  | Ptype_open

and label_declaration =
  Compiler_types.label_declaration =
  { pld_name : string loc
  ; pld_mutable : mutable_flag
  ; pld_type : core_type
  ; pld_loc : location
  ; pld_attributes : attributes
  }

and constructor_declaration =
  Compiler_types.constructor_declaration =
  { pcd_name : string loc
  ; pcd_args : constructor_arguments
  ; pcd_res : core_type option
  ; pcd_loc : location
  ; pcd_attributes : attributes
  }

and constructor_arguments =
  Compiler_types.constructor_arguments =
  | Pcstr_tuple of core_type list
  | Pcstr_record of label_declaration list

and type_extension =
  Compiler_types.type_extension =
  { ptyext_path : longident_loc
  ; ptyext_params : (core_type * variance) list
  ; ptyext_constructors : extension_constructor list
  ; ptyext_private : private_flag
  ; ptyext_attributes : attributes
  }

and extension_constructor =
  Compiler_types.extension_constructor =
  { pext_name : string loc
  ; pext_kind : extension_constructor_kind
  ; pext_loc : location
  ; pext_attributes : attributes
  }

and extension_constructor_kind =
  Compiler_types.extension_constructor_kind =
  | Pext_decl of constructor_arguments * core_type option
  | Pext_rebind of longident_loc

and class_type =
  Compiler_types.class_type =
  { pcty_desc : class_type_desc
  ; pcty_loc : location
  ; pcty_attributes : attributes
  }

and class_type_desc =
  Compiler_types.class_type_desc =
  | Pcty_constr of longident_loc * core_type list
  | Pcty_signature of class_signature
  | Pcty_arrow of arg_label * core_type * class_type
  | Pcty_extension of extension
  | Pcty_open of override_flag * longident_loc * class_type

and class_signature =
  Compiler_types.class_signature =
  { pcsig_self : core_type
  ; pcsig_fields : class_type_field list
  }

and class_type_field =
  Compiler_types.class_type_field =
  { pctf_desc : class_type_field_desc
  ; pctf_loc : location
  ; pctf_attributes : attributes
  }

and class_type_field_desc =
  Compiler_types.class_type_field_desc =
  | Pctf_inherit of class_type
  | Pctf_val of (string loc * mutable_flag * virtual_flag * core_type)
  | Pctf_method of (string loc * private_flag * virtual_flag * core_type)
  | Pctf_constraint of (core_type * core_type)
  | Pctf_attribute of attribute
  | Pctf_extension of extension

and 'a class_infos =
  'a Compiler_types.class_infos =
  { pci_virt : virtual_flag
  ; pci_params : (core_type * variance) list
  ; pci_name : string loc
  ; pci_expr : 'a
  ; pci_loc : location
  ; pci_attributes : attributes
  }

and class_description =
  class_type class_infos

and class_type_declaration =
  class_type class_infos

and class_expr =
  Compiler_types.class_expr =
  { pcl_desc : class_expr_desc
  ; pcl_loc : location
  ; pcl_attributes : attributes
  }

and class_expr_desc =
  Compiler_types.class_expr_desc =
  | Pcl_constr of longident_loc * core_type list
  | Pcl_structure of class_structure
  | Pcl_fun of arg_label * expression option * pattern * class_expr
  | Pcl_apply of class_expr * (arg_label * expression) list
  | Pcl_let of rec_flag * value_binding list * class_expr
  | Pcl_constraint of class_expr * class_type
  | Pcl_extension of extension
  | Pcl_open of override_flag * longident_loc * class_expr

and class_structure =
  Compiler_types.class_structure =
  { pcstr_self : pattern
  ; pcstr_fields : class_field list
  }

and class_field =
  Compiler_types.class_field =
  { pcf_desc : class_field_desc
  ; pcf_loc : location
  ; pcf_attributes : attributes
  }

and class_field_desc =
  Compiler_types.class_field_desc =
  | Pcf_inherit of override_flag * class_expr * string loc option
  | Pcf_val of (string loc * mutable_flag * class_field_kind)
  | Pcf_method of (string loc * private_flag * class_field_kind)
  | Pcf_constraint of (core_type * core_type)
  | Pcf_initializer of expression
  | Pcf_attribute of attribute
  | Pcf_extension of extension

and class_field_kind =
  Compiler_types.class_field_kind =
  | Cfk_virtual of core_type
  | Cfk_concrete of override_flag * expression

and class_declaration =
  class_expr class_infos

and module_type =
  Compiler_types.module_type =
  { pmty_desc : module_type_desc
  ; pmty_loc : location
  ; pmty_attributes : attributes
  }

and module_type_desc =
  Compiler_types.module_type_desc =
  | Pmty_ident of longident_loc
  | Pmty_signature of signature
  | Pmty_functor of string loc * module_type option * module_type
  | Pmty_with of module_type * with_constraint list
  | Pmty_typeof of module_expr
  | Pmty_extension of extension
  | Pmty_alias of longident_loc

and signature =
  signature_item list

and signature_item =
  Compiler_types.signature_item =
  { psig_desc : signature_item_desc
  ; psig_loc : location
  }

and signature_item_desc =
  Compiler_types.signature_item_desc =
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

and module_declaration =
  Compiler_types.module_declaration =
  { pmd_name : string loc
  ; pmd_type : module_type
  ; pmd_attributes : attributes
  ; pmd_loc : location
  }

and module_type_declaration =
  Compiler_types.module_type_declaration =
  { pmtd_name : string loc
  ; pmtd_type : module_type option
  ; pmtd_attributes : attributes
  ; pmtd_loc : location
  }

and open_description =
  Compiler_types.open_description =
  { popen_lid : longident_loc
  ; popen_override : override_flag
  ; popen_loc : location
  ; popen_attributes : attributes
  }

and 'a include_infos =
  'a Compiler_types.include_infos =
  { pincl_mod : 'a
  ; pincl_loc : location
  ; pincl_attributes : attributes
  }

and include_description =
  module_type include_infos

and include_declaration =
  module_expr include_infos

and with_constraint =
  Compiler_types.with_constraint =
  | Pwith_type of longident_loc * type_declaration
  | Pwith_module of longident_loc * longident_loc
  | Pwith_typesubst of longident_loc * type_declaration
  | Pwith_modsubst of longident_loc * longident_loc

and module_expr =
  Compiler_types.module_expr =
  { pmod_desc : module_expr_desc
  ; pmod_loc : location
  ; pmod_attributes : attributes
  }

and module_expr_desc =
  Compiler_types.module_expr_desc =
  | Pmod_ident of longident_loc
  | Pmod_structure of structure
  | Pmod_functor of string loc * module_type option * module_expr
  | Pmod_apply of module_expr * module_expr
  | Pmod_constraint of module_expr * module_type
  | Pmod_unpack of expression
  | Pmod_extension of extension

and structure =
  structure_item list

and structure_item =
  Compiler_types.structure_item =
  { pstr_desc : structure_item_desc
  ; pstr_loc : location
  }

and structure_item_desc =
  Compiler_types.structure_item_desc =
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

and value_binding =
  Compiler_types.value_binding =
  { pvb_pat : pattern
  ; pvb_expr : expression
  ; pvb_attributes : attributes
  ; pvb_loc : location
  }

and module_binding =
  Compiler_types.module_binding =
  { pmb_name : string loc
  ; pmb_expr : module_expr
  ; pmb_attributes : attributes
  ; pmb_loc : location
  }

and toplevel_phrase =
  Compiler_types.toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of string * directive_argument

and directive_argument =
  Compiler_types.directive_argument =
  | Pdir_none
  | Pdir_string of string
  | Pdir_int of string * char option
  | Pdir_ident of longident
  | Pdir_bool of bool
[@@deriving equal, sexp_of]

let rec generate_longident ~size ~random =
  let gen_lident =
    Generator.create (fun ~size ~random ->
      let gen0 = quickcheck_generator_string
      in
      Lident
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ldot =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident)
      and gen1 = quickcheck_generator_string
      in
      Ldot
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_lapply =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident)
      and gen1 = (Generator.create generate_longident)
      in
      Lapply
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  in
  if size = 0
  then
    Generator.generate ~size ~random
      (Base_quickcheck.Generator.union
        [gen_lident])
  else
    Generator.generate ~size:(size-1) ~random
      (Base_quickcheck.Generator.union
        [gen_lident; gen_ldot; gen_lapply])
and generate_longident_loc ~size ~random =
  let gen = (quickcheck_generator_loc (Generator.create generate_longident)) in
  Generator.generate gen ~size ~random
and generate_rec_flag ~size ~random =
  let gen_nonrecursive =
    Generator.return Nonrecursive
  and gen_recursive =
    Generator.return Recursive
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_nonrecursive; gen_recursive])
and generate_direction_flag ~size ~random =
  let gen_upto =
    Generator.return Upto
  and gen_downto =
    Generator.return Downto
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_upto; gen_downto])
and generate_private_flag ~size ~random =
  let gen_private =
    Generator.return Private
  and gen_public =
    Generator.return Public
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_private; gen_public])
and generate_mutable_flag ~size ~random =
  let gen_immutable =
    Generator.return Immutable
  and gen_mutable =
    Generator.return Mutable
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_immutable; gen_mutable])
and generate_virtual_flag ~size ~random =
  let gen_virtual =
    Generator.return Virtual
  and gen_concrete =
    Generator.return Concrete
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_virtual; gen_concrete])
and generate_override_flag ~size ~random =
  let gen_override =
    Generator.return Override
  and gen_fresh =
    Generator.return Fresh
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_override; gen_fresh])
and generate_closed_flag ~size ~random =
  let gen_closed =
    Generator.return Closed
  and gen_open =
    Generator.return Open
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_closed; gen_open])
and generate_arg_label ~size ~random =
  let gen_nolabel =
    Generator.return Nolabel
  and gen_labelled =
    Generator.create (fun ~size ~random ->
      let gen0 = quickcheck_generator_string
      in
      Labelled
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_optional =
    Generator.create (fun ~size ~random ->
      let gen0 = quickcheck_generator_string
      in
      Optional
        ( Generator.generate gen0 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_nolabel; gen_labelled; gen_optional])
and generate_variance ~size ~random =
  let gen_covariant =
    Generator.return Covariant
  and gen_contravariant =
    Generator.return Contravariant
  and gen_invariant =
    Generator.return Invariant
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_covariant; gen_contravariant; gen_invariant])
and generate_constant ~size ~random =
  let gen_pconst_integer =
    Generator.create (fun ~size ~random ->
      let gen0 = quickcheck_generator_string
      and gen1 = (quickcheck_generator_option quickcheck_generator_char)
      in
      Pconst_integer
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pconst_char =
    Generator.create (fun ~size ~random ->
      let gen0 = quickcheck_generator_char
      in
      Pconst_char
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pconst_string =
    Generator.create (fun ~size ~random ->
      let gen0 = quickcheck_generator_string
      and gen1 = (quickcheck_generator_option quickcheck_generator_string)
      in
      Pconst_string
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pconst_float =
    Generator.create (fun ~size ~random ->
      let gen0 = quickcheck_generator_string
      and gen1 = (quickcheck_generator_option quickcheck_generator_char)
      in
      Pconst_float
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_pconst_integer; gen_pconst_char; gen_pconst_string; gen_pconst_float])
and generate_attribute ~size ~random =
  let gen = (quickcheck_generator_tuple2 (quickcheck_generator_loc quickcheck_generator_string) (Generator.create generate_payload)) in
  Generator.generate gen ~size ~random
and generate_extension ~size ~random =
  let gen = (quickcheck_generator_tuple2 (quickcheck_generator_loc quickcheck_generator_string) (Generator.create generate_payload)) in
  Generator.generate gen ~size ~random
and generate_attributes ~size ~random =
  let gen = (quickcheck_generator_list (Generator.create generate_attribute)) in
  Generator.generate gen ~size ~random
and generate_payload ~size ~random =
  let gen_pstr =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_structure)
      in
      PStr
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_psig =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_signature)
      in
      PSig
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ptyp =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_core_type)
      in
      PTyp
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ppat =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_pattern)
      and gen1 = (quickcheck_generator_option (Generator.create generate_expression))
      in
      PPat
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_pstr; gen_psig; gen_ptyp; gen_ppat])
and generate_core_type ~size ~random =
  let gen_ptyp_desc = (Generator.create generate_core_type_desc)
  and gen_ptyp_loc = quickcheck_generator_location
  and gen_ptyp_attributes = (Generator.create generate_attributes)
  in
  { ptyp_desc = Generator.generate gen_ptyp_desc ~size ~random
  ; ptyp_loc = Generator.generate gen_ptyp_loc ~size ~random
  ; ptyp_attributes = Generator.generate gen_ptyp_attributes ~size ~random
  }
and generate_core_type_desc ~size ~random =
  let gen_ptyp_any =
    Generator.return Ptyp_any
  and gen_ptyp_var =
    Generator.create (fun ~size ~random ->
      let gen0 = quickcheck_generator_string
      in
      Ptyp_var
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ptyp_arrow =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_arg_label)
      and gen1 = (Generator.create generate_core_type)
      and gen2 = (Generator.create generate_core_type)
      in
      Ptyp_arrow
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  and gen_ptyp_tuple =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_core_type))
      in
      Ptyp_tuple
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ptyp_constr =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      and gen1 = (quickcheck_generator_list (Generator.create generate_core_type))
      in
      Ptyp_constr
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_ptyp_object =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_object_field))
      and gen1 = (Generator.create generate_closed_flag)
      in
      Ptyp_object
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_ptyp_class =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      and gen1 = (quickcheck_generator_list (Generator.create generate_core_type))
      in
      Ptyp_class
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_ptyp_alias =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_core_type)
      and gen1 = quickcheck_generator_string
      in
      Ptyp_alias
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_ptyp_variant =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_row_field))
      and gen1 = (Generator.create generate_closed_flag)
      and gen2 = (quickcheck_generator_option (quickcheck_generator_list quickcheck_generator_string))
      in
      Ptyp_variant
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  and gen_ptyp_poly =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (quickcheck_generator_loc quickcheck_generator_string))
      and gen1 = (Generator.create generate_core_type)
      in
      Ptyp_poly
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_ptyp_package =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_package_type)
      in
      Ptyp_package
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ptyp_extension =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_extension)
      in
      Ptyp_extension
        ( Generator.generate gen0 ~size ~random
        ))
  in
  if size = 0
  then
    Generator.generate ~size ~random
      (Base_quickcheck.Generator.union
        [gen_ptyp_any; gen_ptyp_var])
  else
    Generator.generate ~size:(size-1) ~random
      (Base_quickcheck.Generator.union
        [gen_ptyp_any; gen_ptyp_var; gen_ptyp_arrow; gen_ptyp_tuple; gen_ptyp_constr; gen_ptyp_object; gen_ptyp_class; gen_ptyp_alias; gen_ptyp_variant; gen_ptyp_poly; gen_ptyp_package; gen_ptyp_extension])
and generate_package_type ~size ~random =
  let gen = (quickcheck_generator_tuple2 (Generator.create generate_longident_loc) (quickcheck_generator_list (quickcheck_generator_tuple2 (Generator.create generate_longident_loc) (Generator.create generate_core_type)))) in
  Generator.generate gen ~size ~random
and generate_row_field ~size ~random =
  let gen_rtag =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_loc quickcheck_generator_string)
      and gen1 = (Generator.create generate_attributes)
      and gen2 = quickcheck_generator_bool
      and gen3 = (quickcheck_generator_list (Generator.create generate_core_type))
      in
      Rtag
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        , Generator.generate gen3 ~size ~random
        ))
  and gen_rinherit =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_core_type)
      in
      Rinherit
        ( Generator.generate gen0 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_rtag; gen_rinherit])
and generate_object_field ~size ~random =
  let gen_otag =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_loc quickcheck_generator_string)
      and gen1 = (Generator.create generate_attributes)
      and gen2 = (Generator.create generate_core_type)
      in
      Otag
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  and gen_oinherit =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_core_type)
      in
      Oinherit
        ( Generator.generate gen0 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_otag; gen_oinherit])
and generate_pattern ~size ~random =
  let gen_ppat_desc = (Generator.create generate_pattern_desc)
  and gen_ppat_loc = quickcheck_generator_location
  and gen_ppat_attributes = (Generator.create generate_attributes)
  in
  { ppat_desc = Generator.generate gen_ppat_desc ~size ~random
  ; ppat_loc = Generator.generate gen_ppat_loc ~size ~random
  ; ppat_attributes = Generator.generate gen_ppat_attributes ~size ~random
  }
and generate_pattern_desc ~size ~random =
  let gen_ppat_any =
    Generator.return Ppat_any
  and gen_ppat_var =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_loc quickcheck_generator_string)
      in
      Ppat_var
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ppat_alias =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_pattern)
      and gen1 = (quickcheck_generator_loc quickcheck_generator_string)
      in
      Ppat_alias
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_ppat_constant =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_constant)
      in
      Ppat_constant
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ppat_interval =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_constant)
      and gen1 = (Generator.create generate_constant)
      in
      Ppat_interval
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_ppat_tuple =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_pattern))
      in
      Ppat_tuple
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ppat_construct =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      and gen1 = (quickcheck_generator_option (Generator.create generate_pattern))
      in
      Ppat_construct
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_ppat_variant =
    Generator.create (fun ~size ~random ->
      let gen0 = quickcheck_generator_string
      and gen1 = (quickcheck_generator_option (Generator.create generate_pattern))
      in
      Ppat_variant
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_ppat_record =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (quickcheck_generator_tuple2 (Generator.create generate_longident_loc) (Generator.create generate_pattern)))
      and gen1 = (Generator.create generate_closed_flag)
      in
      Ppat_record
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_ppat_array =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_pattern))
      in
      Ppat_array
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ppat_or =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_pattern)
      and gen1 = (Generator.create generate_pattern)
      in
      Ppat_or
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_ppat_constraint =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_pattern)
      and gen1 = (Generator.create generate_core_type)
      in
      Ppat_constraint
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_ppat_type =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      in
      Ppat_type
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ppat_lazy =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_pattern)
      in
      Ppat_lazy
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ppat_unpack =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_loc quickcheck_generator_string)
      in
      Ppat_unpack
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ppat_exception =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_pattern)
      in
      Ppat_exception
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ppat_extension =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_extension)
      in
      Ppat_extension
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ppat_open =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      and gen1 = (Generator.create generate_pattern)
      in
      Ppat_open
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  in
  if size = 0
  then
    Generator.generate ~size ~random
      (Base_quickcheck.Generator.union
        [gen_ppat_any; gen_ppat_var; gen_ppat_unpack])
  else
    Generator.generate ~size:(size-1) ~random
      (Base_quickcheck.Generator.union
        [gen_ppat_any; gen_ppat_var; gen_ppat_unpack; gen_ppat_alias; gen_ppat_constant; gen_ppat_interval; gen_ppat_tuple; gen_ppat_construct; gen_ppat_variant; gen_ppat_record; gen_ppat_array; gen_ppat_or; gen_ppat_constraint; gen_ppat_type; gen_ppat_lazy; gen_ppat_exception; gen_ppat_extension; gen_ppat_open])
and generate_expression ~size ~random =
  let gen_pexp_desc = (Generator.create generate_expression_desc)
  and gen_pexp_loc = quickcheck_generator_location
  and gen_pexp_attributes = (Generator.create generate_attributes)
  in
  { pexp_desc = Generator.generate gen_pexp_desc ~size ~random
  ; pexp_loc = Generator.generate gen_pexp_loc ~size ~random
  ; pexp_attributes = Generator.generate gen_pexp_attributes ~size ~random
  }
and generate_expression_desc ~size ~random =
  let gen_pexp_ident =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      in
      Pexp_ident
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pexp_constant =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_constant)
      in
      Pexp_constant
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pexp_let =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_rec_flag)
      and gen1 = (quickcheck_generator_list (Generator.create generate_value_binding))
      and gen2 = (Generator.create generate_expression)
      in
      Pexp_let
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  and gen_pexp_function =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_case))
      in
      Pexp_function
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pexp_fun =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_arg_label)
      and gen1 = (quickcheck_generator_option (Generator.create generate_expression))
      and gen2 = (Generator.create generate_pattern)
      and gen3 = (Generator.create generate_expression)
      in
      Pexp_fun
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        , Generator.generate gen3 ~size ~random
        ))
  and gen_pexp_apply =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      and gen1 = (quickcheck_generator_list (quickcheck_generator_tuple2 (Generator.create generate_arg_label) (Generator.create generate_expression)))
      in
      Pexp_apply
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pexp_match =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      and gen1 = (quickcheck_generator_list (Generator.create generate_case))
      in
      Pexp_match
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pexp_try =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      and gen1 = (quickcheck_generator_list (Generator.create generate_case))
      in
      Pexp_try
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pexp_tuple =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_expression))
      in
      Pexp_tuple
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pexp_construct =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      and gen1 = (quickcheck_generator_option (Generator.create generate_expression))
      in
      Pexp_construct
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pexp_variant =
    Generator.create (fun ~size ~random ->
      let gen0 = quickcheck_generator_string
      and gen1 = (quickcheck_generator_option (Generator.create generate_expression))
      in
      Pexp_variant
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pexp_record =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (quickcheck_generator_tuple2 (Generator.create generate_longident_loc) (Generator.create generate_expression)))
      and gen1 = (quickcheck_generator_option (Generator.create generate_expression))
      in
      Pexp_record
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pexp_field =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      and gen1 = (Generator.create generate_longident_loc)
      in
      Pexp_field
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pexp_setfield =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      and gen1 = (Generator.create generate_longident_loc)
      and gen2 = (Generator.create generate_expression)
      in
      Pexp_setfield
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  and gen_pexp_array =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_expression))
      in
      Pexp_array
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pexp_ifthenelse =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      and gen1 = (Generator.create generate_expression)
      and gen2 = (quickcheck_generator_option (Generator.create generate_expression))
      in
      Pexp_ifthenelse
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  and gen_pexp_sequence =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      and gen1 = (Generator.create generate_expression)
      in
      Pexp_sequence
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pexp_while =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      and gen1 = (Generator.create generate_expression)
      in
      Pexp_while
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pexp_for =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_pattern)
      and gen1 = (Generator.create generate_expression)
      and gen2 = (Generator.create generate_expression)
      and gen3 = (Generator.create generate_direction_flag)
      and gen4 = (Generator.create generate_expression)
      in
      Pexp_for
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        , Generator.generate gen3 ~size ~random
        , Generator.generate gen4 ~size ~random
        ))
  and gen_pexp_constraint =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      and gen1 = (Generator.create generate_core_type)
      in
      Pexp_constraint
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pexp_coerce =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      and gen1 = (quickcheck_generator_option (Generator.create generate_core_type))
      and gen2 = (Generator.create generate_core_type)
      in
      Pexp_coerce
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  and gen_pexp_send =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      and gen1 = (quickcheck_generator_loc quickcheck_generator_string)
      in
      Pexp_send
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pexp_new =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      in
      Pexp_new
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pexp_setinstvar =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_loc quickcheck_generator_string)
      and gen1 = (Generator.create generate_expression)
      in
      Pexp_setinstvar
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pexp_override =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (quickcheck_generator_tuple2 (quickcheck_generator_loc quickcheck_generator_string) (Generator.create generate_expression)))
      in
      Pexp_override
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pexp_letmodule =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_loc quickcheck_generator_string)
      and gen1 = (Generator.create generate_module_expr)
      and gen2 = (Generator.create generate_expression)
      in
      Pexp_letmodule
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  and gen_pexp_letexception =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_extension_constructor)
      and gen1 = (Generator.create generate_expression)
      in
      Pexp_letexception
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pexp_assert =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      in
      Pexp_assert
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pexp_lazy =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      in
      Pexp_lazy
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pexp_poly =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      and gen1 = (quickcheck_generator_option (Generator.create generate_core_type))
      in
      Pexp_poly
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pexp_object =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_class_structure)
      in
      Pexp_object
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pexp_newtype =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_loc quickcheck_generator_string)
      and gen1 = (Generator.create generate_expression)
      in
      Pexp_newtype
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pexp_pack =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_module_expr)
      in
      Pexp_pack
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pexp_open =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_override_flag)
      and gen1 = (Generator.create generate_longident_loc)
      and gen2 = (Generator.create generate_expression)
      in
      Pexp_open
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  and gen_pexp_extension =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_extension)
      in
      Pexp_extension
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pexp_unreachable =
    Generator.return Pexp_unreachable
  in
  if size = 0
  then
    Generator.generate ~size ~random
      (Base_quickcheck.Generator.union
        [gen_pexp_unreachable])
  else
    Generator.generate ~size:(size-1) ~random
      (Base_quickcheck.Generator.union
        [gen_pexp_unreachable; gen_pexp_ident; gen_pexp_constant; gen_pexp_let; gen_pexp_function; gen_pexp_fun; gen_pexp_apply; gen_pexp_match; gen_pexp_try; gen_pexp_tuple; gen_pexp_construct; gen_pexp_variant; gen_pexp_record; gen_pexp_field; gen_pexp_setfield; gen_pexp_array; gen_pexp_ifthenelse; gen_pexp_sequence; gen_pexp_while; gen_pexp_for; gen_pexp_constraint; gen_pexp_coerce; gen_pexp_send; gen_pexp_new; gen_pexp_setinstvar; gen_pexp_override; gen_pexp_letmodule; gen_pexp_letexception; gen_pexp_assert; gen_pexp_lazy; gen_pexp_poly; gen_pexp_object; gen_pexp_newtype; gen_pexp_pack; gen_pexp_open; gen_pexp_extension])
and generate_case ~size ~random =
  let gen_pc_lhs = (Generator.create generate_pattern)
  and gen_pc_guard = (quickcheck_generator_option (Generator.create generate_expression))
  and gen_pc_rhs = (Generator.create generate_expression)
  in
  { pc_lhs = Generator.generate gen_pc_lhs ~size ~random
  ; pc_guard = Generator.generate gen_pc_guard ~size ~random
  ; pc_rhs = Generator.generate gen_pc_rhs ~size ~random
  }
and generate_value_description ~size ~random =
  let gen_pval_name = (quickcheck_generator_loc quickcheck_generator_string)
  and gen_pval_type = (Generator.create generate_core_type)
  and gen_pval_prim = (quickcheck_generator_list quickcheck_generator_string)
  and gen_pval_attributes = (Generator.create generate_attributes)
  and gen_pval_loc = quickcheck_generator_location
  in
  { pval_name = Generator.generate gen_pval_name ~size ~random
  ; pval_type = Generator.generate gen_pval_type ~size ~random
  ; pval_prim = Generator.generate gen_pval_prim ~size ~random
  ; pval_attributes = Generator.generate gen_pval_attributes ~size ~random
  ; pval_loc = Generator.generate gen_pval_loc ~size ~random
  }
and generate_type_declaration ~size ~random =
  let gen_ptype_name = (quickcheck_generator_loc quickcheck_generator_string)
  and gen_ptype_params = (quickcheck_generator_list (quickcheck_generator_tuple2 (Generator.create generate_core_type) (Generator.create generate_variance)))
  and gen_ptype_cstrs = (quickcheck_generator_list (quickcheck_generator_tuple3 (Generator.create generate_core_type) (Generator.create generate_core_type) quickcheck_generator_location))
  and gen_ptype_kind = (Generator.create generate_type_kind)
  and gen_ptype_private = (Generator.create generate_private_flag)
  and gen_ptype_manifest = (quickcheck_generator_option (Generator.create generate_core_type))
  and gen_ptype_attributes = (Generator.create generate_attributes)
  and gen_ptype_loc = quickcheck_generator_location
  in
  { ptype_name = Generator.generate gen_ptype_name ~size ~random
  ; ptype_params = Generator.generate gen_ptype_params ~size ~random
  ; ptype_cstrs = Generator.generate gen_ptype_cstrs ~size ~random
  ; ptype_kind = Generator.generate gen_ptype_kind ~size ~random
  ; ptype_private = Generator.generate gen_ptype_private ~size ~random
  ; ptype_manifest = Generator.generate gen_ptype_manifest ~size ~random
  ; ptype_attributes = Generator.generate gen_ptype_attributes ~size ~random
  ; ptype_loc = Generator.generate gen_ptype_loc ~size ~random
  }
and generate_type_kind ~size ~random =
  let gen_ptype_abstract =
    Generator.return Ptype_abstract
  and gen_ptype_variant =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_constructor_declaration))
      in
      Ptype_variant
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ptype_record =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_label_declaration))
      in
      Ptype_record
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ptype_open =
    Generator.return Ptype_open
  in
  if size = 0
  then
    Generator.generate ~size ~random
      (Base_quickcheck.Generator.union
        [gen_ptype_abstract; gen_ptype_open])
  else
    Generator.generate ~size:(size-1) ~random
      (Base_quickcheck.Generator.union
        [gen_ptype_abstract; gen_ptype_open; gen_ptype_variant; gen_ptype_record])
and generate_label_declaration ~size ~random =
  let gen_pld_name = (quickcheck_generator_loc quickcheck_generator_string)
  and gen_pld_mutable = (Generator.create generate_mutable_flag)
  and gen_pld_type = (Generator.create generate_core_type)
  and gen_pld_loc = quickcheck_generator_location
  and gen_pld_attributes = (Generator.create generate_attributes)
  in
  { pld_name = Generator.generate gen_pld_name ~size ~random
  ; pld_mutable = Generator.generate gen_pld_mutable ~size ~random
  ; pld_type = Generator.generate gen_pld_type ~size ~random
  ; pld_loc = Generator.generate gen_pld_loc ~size ~random
  ; pld_attributes = Generator.generate gen_pld_attributes ~size ~random
  }
and generate_constructor_declaration ~size ~random =
  let gen_pcd_name = (quickcheck_generator_loc quickcheck_generator_string)
  and gen_pcd_args = (Generator.create generate_constructor_arguments)
  and gen_pcd_res = (quickcheck_generator_option (Generator.create generate_core_type))
  and gen_pcd_loc = quickcheck_generator_location
  and gen_pcd_attributes = (Generator.create generate_attributes)
  in
  { pcd_name = Generator.generate gen_pcd_name ~size ~random
  ; pcd_args = Generator.generate gen_pcd_args ~size ~random
  ; pcd_res = Generator.generate gen_pcd_res ~size ~random
  ; pcd_loc = Generator.generate gen_pcd_loc ~size ~random
  ; pcd_attributes = Generator.generate gen_pcd_attributes ~size ~random
  }
and generate_constructor_arguments ~size ~random =
  let gen_pcstr_tuple =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_core_type))
      in
      Pcstr_tuple
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pcstr_record =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_label_declaration))
      in
      Pcstr_record
        ( Generator.generate gen0 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_pcstr_tuple; gen_pcstr_record])
and generate_type_extension ~size ~random =
  let gen_ptyext_path = (Generator.create generate_longident_loc)
  and gen_ptyext_params = (quickcheck_generator_list (quickcheck_generator_tuple2 (Generator.create generate_core_type) (Generator.create generate_variance)))
  and gen_ptyext_constructors = (quickcheck_generator_list (Generator.create generate_extension_constructor))
  and gen_ptyext_private = (Generator.create generate_private_flag)
  and gen_ptyext_attributes = (Generator.create generate_attributes)
  in
  { ptyext_path = Generator.generate gen_ptyext_path ~size ~random
  ; ptyext_params = Generator.generate gen_ptyext_params ~size ~random
  ; ptyext_constructors = Generator.generate gen_ptyext_constructors ~size ~random
  ; ptyext_private = Generator.generate gen_ptyext_private ~size ~random
  ; ptyext_attributes = Generator.generate gen_ptyext_attributes ~size ~random
  }
and generate_extension_constructor ~size ~random =
  let gen_pext_name = (quickcheck_generator_loc quickcheck_generator_string)
  and gen_pext_kind = (Generator.create generate_extension_constructor_kind)
  and gen_pext_loc = quickcheck_generator_location
  and gen_pext_attributes = (Generator.create generate_attributes)
  in
  { pext_name = Generator.generate gen_pext_name ~size ~random
  ; pext_kind = Generator.generate gen_pext_kind ~size ~random
  ; pext_loc = Generator.generate gen_pext_loc ~size ~random
  ; pext_attributes = Generator.generate gen_pext_attributes ~size ~random
  }
and generate_extension_constructor_kind ~size ~random =
  let gen_pext_decl =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_constructor_arguments)
      and gen1 = (quickcheck_generator_option (Generator.create generate_core_type))
      in
      Pext_decl
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pext_rebind =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      in
      Pext_rebind
        ( Generator.generate gen0 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_pext_decl; gen_pext_rebind])
and generate_class_type ~size ~random =
  let gen_pcty_desc = (Generator.create generate_class_type_desc)
  and gen_pcty_loc = quickcheck_generator_location
  and gen_pcty_attributes = (Generator.create generate_attributes)
  in
  { pcty_desc = Generator.generate gen_pcty_desc ~size ~random
  ; pcty_loc = Generator.generate gen_pcty_loc ~size ~random
  ; pcty_attributes = Generator.generate gen_pcty_attributes ~size ~random
  }
and generate_class_type_desc ~size ~random =
  let gen_pcty_constr =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      and gen1 = (quickcheck_generator_list (Generator.create generate_core_type))
      in
      Pcty_constr
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pcty_signature =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_class_signature)
      in
      Pcty_signature
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pcty_arrow =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_arg_label)
      and gen1 = (Generator.create generate_core_type)
      and gen2 = (Generator.create generate_class_type)
      in
      Pcty_arrow
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  and gen_pcty_extension =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_extension)
      in
      Pcty_extension
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pcty_open =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_override_flag)
      and gen1 = (Generator.create generate_longident_loc)
      and gen2 = (Generator.create generate_class_type)
      in
      Pcty_open
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_pcty_constr; gen_pcty_signature; gen_pcty_arrow; gen_pcty_extension; gen_pcty_open])
and generate_class_signature ~size ~random =
  let gen_pcsig_self = (Generator.create generate_core_type)
  and gen_pcsig_fields = (quickcheck_generator_list (Generator.create generate_class_type_field))
  in
  { pcsig_self = Generator.generate gen_pcsig_self ~size ~random
  ; pcsig_fields = Generator.generate gen_pcsig_fields ~size ~random
  }
and generate_class_type_field ~size ~random =
  let gen_pctf_desc = (Generator.create generate_class_type_field_desc)
  and gen_pctf_loc = quickcheck_generator_location
  and gen_pctf_attributes = (Generator.create generate_attributes)
  in
  { pctf_desc = Generator.generate gen_pctf_desc ~size ~random
  ; pctf_loc = Generator.generate gen_pctf_loc ~size ~random
  ; pctf_attributes = Generator.generate gen_pctf_attributes ~size ~random
  }
and generate_class_type_field_desc ~size ~random =
  let gen_pctf_inherit =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_class_type)
      in
      Pctf_inherit
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pctf_val =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_tuple4 (quickcheck_generator_loc quickcheck_generator_string) (Generator.create generate_mutable_flag) (Generator.create generate_virtual_flag) (Generator.create generate_core_type))
      in
      Pctf_val
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pctf_method =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_tuple4 (quickcheck_generator_loc quickcheck_generator_string) (Generator.create generate_private_flag) (Generator.create generate_virtual_flag) (Generator.create generate_core_type))
      in
      Pctf_method
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pctf_constraint =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_tuple2 (Generator.create generate_core_type) (Generator.create generate_core_type))
      in
      Pctf_constraint
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pctf_attribute =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_attribute)
      in
      Pctf_attribute
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pctf_extension =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_extension)
      in
      Pctf_extension
        ( Generator.generate gen0 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_pctf_inherit; gen_pctf_val; gen_pctf_method; gen_pctf_constraint; gen_pctf_attribute; gen_pctf_extension])
and generate_class_infos
  : type a . a Generator.t -> size:int -> random:Splittable_random.State.t -> a class_infos
  = fun quickcheck_generator_a ~size ~random ->
  let gen_pci_virt = (Generator.create generate_virtual_flag)
  and gen_pci_params = (quickcheck_generator_list (quickcheck_generator_tuple2 (Generator.create generate_core_type) (Generator.create generate_variance)))
  and gen_pci_name = (quickcheck_generator_loc quickcheck_generator_string)
  and gen_pci_expr = quickcheck_generator_a
  and gen_pci_loc = quickcheck_generator_location
  and gen_pci_attributes = (Generator.create generate_attributes)
  in
  { pci_virt = Generator.generate gen_pci_virt ~size ~random
  ; pci_params = Generator.generate gen_pci_params ~size ~random
  ; pci_name = Generator.generate gen_pci_name ~size ~random
  ; pci_expr = Generator.generate gen_pci_expr ~size ~random
  ; pci_loc = Generator.generate gen_pci_loc ~size ~random
  ; pci_attributes = Generator.generate gen_pci_attributes ~size ~random
  }
and generate_class_description ~size ~random =
  let gen = (Generator.create (generate_class_infos (Generator.create generate_class_type))) in
  Generator.generate gen ~size ~random
and generate_class_type_declaration ~size ~random =
  let gen = (Generator.create (generate_class_infos (Generator.create generate_class_type))) in
  Generator.generate gen ~size ~random
and generate_class_expr ~size ~random =
  let gen_pcl_desc = (Generator.create generate_class_expr_desc)
  and gen_pcl_loc = quickcheck_generator_location
  and gen_pcl_attributes = (Generator.create generate_attributes)
  in
  { pcl_desc = Generator.generate gen_pcl_desc ~size ~random
  ; pcl_loc = Generator.generate gen_pcl_loc ~size ~random
  ; pcl_attributes = Generator.generate gen_pcl_attributes ~size ~random
  }
and generate_class_expr_desc ~size ~random =
  let gen_pcl_constr =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      and gen1 = (quickcheck_generator_list (Generator.create generate_core_type))
      in
      Pcl_constr
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pcl_structure =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_class_structure)
      in
      Pcl_structure
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pcl_fun =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_arg_label)
      and gen1 = (quickcheck_generator_option (Generator.create generate_expression))
      and gen2 = (Generator.create generate_pattern)
      and gen3 = (Generator.create generate_class_expr)
      in
      Pcl_fun
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        , Generator.generate gen3 ~size ~random
        ))
  and gen_pcl_apply =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_class_expr)
      and gen1 = (quickcheck_generator_list (quickcheck_generator_tuple2 (Generator.create generate_arg_label) (Generator.create generate_expression)))
      in
      Pcl_apply
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pcl_let =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_rec_flag)
      and gen1 = (quickcheck_generator_list (Generator.create generate_value_binding))
      and gen2 = (Generator.create generate_class_expr)
      in
      Pcl_let
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  and gen_pcl_constraint =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_class_expr)
      and gen1 = (Generator.create generate_class_type)
      in
      Pcl_constraint
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pcl_extension =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_extension)
      in
      Pcl_extension
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pcl_open =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_override_flag)
      and gen1 = (Generator.create generate_longident_loc)
      and gen2 = (Generator.create generate_class_expr)
      in
      Pcl_open
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_pcl_constr; gen_pcl_structure; gen_pcl_fun; gen_pcl_apply; gen_pcl_let; gen_pcl_constraint; gen_pcl_extension; gen_pcl_open])
and generate_class_structure ~size ~random =
  let gen_pcstr_self = (Generator.create generate_pattern)
  and gen_pcstr_fields = (quickcheck_generator_list (Generator.create generate_class_field))
  in
  { pcstr_self = Generator.generate gen_pcstr_self ~size ~random
  ; pcstr_fields = Generator.generate gen_pcstr_fields ~size ~random
  }
and generate_class_field ~size ~random =
  let gen_pcf_desc = (Generator.create generate_class_field_desc)
  and gen_pcf_loc = quickcheck_generator_location
  and gen_pcf_attributes = (Generator.create generate_attributes)
  in
  { pcf_desc = Generator.generate gen_pcf_desc ~size ~random
  ; pcf_loc = Generator.generate gen_pcf_loc ~size ~random
  ; pcf_attributes = Generator.generate gen_pcf_attributes ~size ~random
  }
and generate_class_field_desc ~size ~random =
  let gen_pcf_inherit =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_override_flag)
      and gen1 = (Generator.create generate_class_expr)
      and gen2 = (quickcheck_generator_option (quickcheck_generator_loc quickcheck_generator_string))
      in
      Pcf_inherit
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  and gen_pcf_val =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_tuple3 (quickcheck_generator_loc quickcheck_generator_string) (Generator.create generate_mutable_flag) (Generator.create generate_class_field_kind))
      in
      Pcf_val
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pcf_method =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_tuple3 (quickcheck_generator_loc quickcheck_generator_string) (Generator.create generate_private_flag) (Generator.create generate_class_field_kind))
      in
      Pcf_method
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pcf_constraint =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_tuple2 (Generator.create generate_core_type) (Generator.create generate_core_type))
      in
      Pcf_constraint
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pcf_initializer =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      in
      Pcf_initializer
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pcf_attribute =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_attribute)
      in
      Pcf_attribute
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pcf_extension =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_extension)
      in
      Pcf_extension
        ( Generator.generate gen0 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_pcf_inherit; gen_pcf_val; gen_pcf_method; gen_pcf_constraint; gen_pcf_initializer; gen_pcf_attribute; gen_pcf_extension])
and generate_class_field_kind ~size ~random =
  let gen_cfk_virtual =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_core_type)
      in
      Cfk_virtual
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_cfk_concrete =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_override_flag)
      and gen1 = (Generator.create generate_expression)
      in
      Cfk_concrete
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_cfk_virtual; gen_cfk_concrete])
and generate_class_declaration ~size ~random =
  let gen = (Generator.create (generate_class_infos (Generator.create generate_class_expr))) in
  Generator.generate gen ~size ~random
and generate_module_type ~size ~random =
  let gen_pmty_desc = (Generator.create generate_module_type_desc)
  and gen_pmty_loc = quickcheck_generator_location
  and gen_pmty_attributes = (Generator.create generate_attributes)
  in
  { pmty_desc = Generator.generate gen_pmty_desc ~size ~random
  ; pmty_loc = Generator.generate gen_pmty_loc ~size ~random
  ; pmty_attributes = Generator.generate gen_pmty_attributes ~size ~random
  }
and generate_module_type_desc ~size ~random =
  let gen_pmty_ident =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      in
      Pmty_ident
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pmty_signature =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_signature)
      in
      Pmty_signature
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pmty_functor =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_loc quickcheck_generator_string)
      and gen1 = (quickcheck_generator_option (Generator.create generate_module_type))
      and gen2 = (Generator.create generate_module_type)
      in
      Pmty_functor
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  and gen_pmty_with =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_module_type)
      and gen1 = (quickcheck_generator_list (Generator.create generate_with_constraint))
      in
      Pmty_with
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pmty_typeof =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_module_expr)
      in
      Pmty_typeof
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pmty_extension =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_extension)
      in
      Pmty_extension
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pmty_alias =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      in
      Pmty_alias
        ( Generator.generate gen0 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_pmty_ident; gen_pmty_signature; gen_pmty_functor; gen_pmty_with; gen_pmty_typeof; gen_pmty_extension; gen_pmty_alias])
and generate_signature ~size ~random =
  let gen = (quickcheck_generator_list (Generator.create generate_signature_item)) in
  Generator.generate gen ~size ~random
and generate_signature_item ~size ~random =
  let gen_psig_desc = (Generator.create generate_signature_item_desc)
  and gen_psig_loc = quickcheck_generator_location
  in
  { psig_desc = Generator.generate gen_psig_desc ~size ~random
  ; psig_loc = Generator.generate gen_psig_loc ~size ~random
  }
and generate_signature_item_desc ~size ~random =
  let gen_psig_value =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_value_description)
      in
      Psig_value
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_psig_type =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_rec_flag)
      and gen1 = (quickcheck_generator_list (Generator.create generate_type_declaration))
      in
      Psig_type
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_psig_typext =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_type_extension)
      in
      Psig_typext
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_psig_exception =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_extension_constructor)
      in
      Psig_exception
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_psig_module =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_module_declaration)
      in
      Psig_module
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_psig_recmodule =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_module_declaration))
      in
      Psig_recmodule
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_psig_modtype =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_module_type_declaration)
      in
      Psig_modtype
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_psig_open =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_open_description)
      in
      Psig_open
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_psig_include =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_include_description)
      in
      Psig_include
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_psig_class =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_class_description))
      in
      Psig_class
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_psig_class_type =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_class_type_declaration))
      in
      Psig_class_type
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_psig_attribute =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_attribute)
      in
      Psig_attribute
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_psig_extension =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_extension)
      and gen1 = (Generator.create generate_attributes)
      in
      Psig_extension
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_psig_value; gen_psig_type; gen_psig_typext; gen_psig_exception; gen_psig_module; gen_psig_recmodule; gen_psig_modtype; gen_psig_open; gen_psig_include; gen_psig_class; gen_psig_class_type; gen_psig_attribute; gen_psig_extension])
and generate_module_declaration ~size ~random =
  let gen_pmd_name = (quickcheck_generator_loc quickcheck_generator_string)
  and gen_pmd_type = (Generator.create generate_module_type)
  and gen_pmd_attributes = (Generator.create generate_attributes)
  and gen_pmd_loc = quickcheck_generator_location
  in
  { pmd_name = Generator.generate gen_pmd_name ~size ~random
  ; pmd_type = Generator.generate gen_pmd_type ~size ~random
  ; pmd_attributes = Generator.generate gen_pmd_attributes ~size ~random
  ; pmd_loc = Generator.generate gen_pmd_loc ~size ~random
  }
and generate_module_type_declaration ~size ~random =
  let gen_pmtd_name = (quickcheck_generator_loc quickcheck_generator_string)
  and gen_pmtd_type = (quickcheck_generator_option (Generator.create generate_module_type))
  and gen_pmtd_attributes = (Generator.create generate_attributes)
  and gen_pmtd_loc = quickcheck_generator_location
  in
  { pmtd_name = Generator.generate gen_pmtd_name ~size ~random
  ; pmtd_type = Generator.generate gen_pmtd_type ~size ~random
  ; pmtd_attributes = Generator.generate gen_pmtd_attributes ~size ~random
  ; pmtd_loc = Generator.generate gen_pmtd_loc ~size ~random
  }
and generate_open_description ~size ~random =
  let gen_popen_lid = (Generator.create generate_longident_loc)
  and gen_popen_override = (Generator.create generate_override_flag)
  and gen_popen_loc = quickcheck_generator_location
  and gen_popen_attributes = (Generator.create generate_attributes)
  in
  { popen_lid = Generator.generate gen_popen_lid ~size ~random
  ; popen_override = Generator.generate gen_popen_override ~size ~random
  ; popen_loc = Generator.generate gen_popen_loc ~size ~random
  ; popen_attributes = Generator.generate gen_popen_attributes ~size ~random
  }
and generate_include_infos
  : type a . a Generator.t -> size:int -> random:Splittable_random.State.t -> a include_infos
  = fun quickcheck_generator_a ~size ~random ->
  let gen_pincl_mod = quickcheck_generator_a
  and gen_pincl_loc = quickcheck_generator_location
  and gen_pincl_attributes = (Generator.create generate_attributes)
  in
  { pincl_mod = Generator.generate gen_pincl_mod ~size ~random
  ; pincl_loc = Generator.generate gen_pincl_loc ~size ~random
  ; pincl_attributes = Generator.generate gen_pincl_attributes ~size ~random
  }
and generate_include_description ~size ~random =
  let gen = (Generator.create (generate_include_infos (Generator.create generate_module_type))) in
  Generator.generate gen ~size ~random
and generate_include_declaration ~size ~random =
  let gen = (Generator.create (generate_include_infos (Generator.create generate_module_expr))) in
  Generator.generate gen ~size ~random
and generate_with_constraint ~size ~random =
  let gen_pwith_type =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      and gen1 = (Generator.create generate_type_declaration)
      in
      Pwith_type
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pwith_module =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      and gen1 = (Generator.create generate_longident_loc)
      in
      Pwith_module
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pwith_typesubst =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      and gen1 = (Generator.create generate_type_declaration)
      in
      Pwith_typesubst
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pwith_modsubst =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      and gen1 = (Generator.create generate_longident_loc)
      in
      Pwith_modsubst
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_pwith_type; gen_pwith_module; gen_pwith_typesubst; gen_pwith_modsubst])
and generate_module_expr ~size ~random =
  let gen_pmod_desc = (Generator.create generate_module_expr_desc)
  and gen_pmod_loc = quickcheck_generator_location
  and gen_pmod_attributes = (Generator.create generate_attributes)
  in
  { pmod_desc = Generator.generate gen_pmod_desc ~size ~random
  ; pmod_loc = Generator.generate gen_pmod_loc ~size ~random
  ; pmod_attributes = Generator.generate gen_pmod_attributes ~size ~random
  }
and generate_module_expr_desc ~size ~random =
  let gen_pmod_ident =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident_loc)
      in
      Pmod_ident
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pmod_structure =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_structure)
      in
      Pmod_structure
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pmod_functor =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_loc quickcheck_generator_string)
      and gen1 = (quickcheck_generator_option (Generator.create generate_module_type))
      and gen2 = (Generator.create generate_module_expr)
      in
      Pmod_functor
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        , Generator.generate gen2 ~size ~random
        ))
  and gen_pmod_apply =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_module_expr)
      and gen1 = (Generator.create generate_module_expr)
      in
      Pmod_apply
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pmod_constraint =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_module_expr)
      and gen1 = (Generator.create generate_module_type)
      in
      Pmod_constraint
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pmod_unpack =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      in
      Pmod_unpack
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pmod_extension =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_extension)
      in
      Pmod_extension
        ( Generator.generate gen0 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_pmod_ident; gen_pmod_structure; gen_pmod_functor; gen_pmod_apply; gen_pmod_constraint; gen_pmod_unpack; gen_pmod_extension])
and generate_structure ~size ~random =
  let gen = (quickcheck_generator_list (Generator.create generate_structure_item)) in
  Generator.generate gen ~size ~random
and generate_structure_item ~size ~random =
  let gen_pstr_desc = (Generator.create generate_structure_item_desc)
  and gen_pstr_loc = quickcheck_generator_location
  in
  { pstr_desc = Generator.generate gen_pstr_desc ~size ~random
  ; pstr_loc = Generator.generate gen_pstr_loc ~size ~random
  }
and generate_structure_item_desc ~size ~random =
  let gen_pstr_eval =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_expression)
      and gen1 = (Generator.create generate_attributes)
      in
      Pstr_eval
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pstr_value =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_rec_flag)
      and gen1 = (quickcheck_generator_list (Generator.create generate_value_binding))
      in
      Pstr_value
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pstr_primitive =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_value_description)
      in
      Pstr_primitive
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pstr_type =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_rec_flag)
      and gen1 = (quickcheck_generator_list (Generator.create generate_type_declaration))
      in
      Pstr_type
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pstr_typext =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_type_extension)
      in
      Pstr_typext
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pstr_exception =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_extension_constructor)
      in
      Pstr_exception
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pstr_module =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_module_binding)
      in
      Pstr_module
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pstr_recmodule =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_module_binding))
      in
      Pstr_recmodule
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pstr_modtype =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_module_type_declaration)
      in
      Pstr_modtype
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pstr_open =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_open_description)
      in
      Pstr_open
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pstr_class =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_class_declaration))
      in
      Pstr_class
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pstr_class_type =
    Generator.create (fun ~size ~random ->
      let gen0 = (quickcheck_generator_list (Generator.create generate_class_type_declaration))
      in
      Pstr_class_type
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pstr_include =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_include_declaration)
      in
      Pstr_include
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pstr_attribute =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_attribute)
      in
      Pstr_attribute
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pstr_extension =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_extension)
      and gen1 = (Generator.create generate_attributes)
      in
      Pstr_extension
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_pstr_eval; gen_pstr_value; gen_pstr_primitive; gen_pstr_type; gen_pstr_typext; gen_pstr_exception; gen_pstr_module; gen_pstr_recmodule; gen_pstr_modtype; gen_pstr_open; gen_pstr_class; gen_pstr_class_type; gen_pstr_include; gen_pstr_attribute; gen_pstr_extension])
and generate_value_binding ~size ~random =
  let gen_pvb_pat = (Generator.create generate_pattern)
  and gen_pvb_expr = (Generator.create generate_expression)
  and gen_pvb_attributes = (Generator.create generate_attributes)
  and gen_pvb_loc = quickcheck_generator_location
  in
  { pvb_pat = Generator.generate gen_pvb_pat ~size ~random
  ; pvb_expr = Generator.generate gen_pvb_expr ~size ~random
  ; pvb_attributes = Generator.generate gen_pvb_attributes ~size ~random
  ; pvb_loc = Generator.generate gen_pvb_loc ~size ~random
  }
and generate_module_binding ~size ~random =
  let gen_pmb_name = (quickcheck_generator_loc quickcheck_generator_string)
  and gen_pmb_expr = (Generator.create generate_module_expr)
  and gen_pmb_attributes = (Generator.create generate_attributes)
  and gen_pmb_loc = quickcheck_generator_location
  in
  { pmb_name = Generator.generate gen_pmb_name ~size ~random
  ; pmb_expr = Generator.generate gen_pmb_expr ~size ~random
  ; pmb_attributes = Generator.generate gen_pmb_attributes ~size ~random
  ; pmb_loc = Generator.generate gen_pmb_loc ~size ~random
  }
and generate_toplevel_phrase ~size ~random =
  let gen_ptop_def =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_structure)
      in
      Ptop_def
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_ptop_dir =
    Generator.create (fun ~size ~random ->
      let gen0 = quickcheck_generator_string
      and gen1 = (Generator.create generate_directive_argument)
      in
      Ptop_dir
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  in
  Generator.generate ~size ~random
    (Base_quickcheck.Generator.union
      [gen_ptop_def; gen_ptop_dir])
and generate_directive_argument ~size ~random =
  let gen_pdir_none =
    Generator.return Pdir_none
  and gen_pdir_string =
    Generator.create (fun ~size ~random ->
      let gen0 = quickcheck_generator_string
      in
      Pdir_string
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pdir_int =
    Generator.create (fun ~size ~random ->
      let gen0 = quickcheck_generator_string
      and gen1 = (quickcheck_generator_option quickcheck_generator_char)
      in
      Pdir_int
        ( Generator.generate gen0 ~size ~random
        , Generator.generate gen1 ~size ~random
        ))
  and gen_pdir_ident =
    Generator.create (fun ~size ~random ->
      let gen0 = (Generator.create generate_longident)
      in
      Pdir_ident
        ( Generator.generate gen0 ~size ~random
        ))
  and gen_pdir_bool =
    Generator.create (fun ~size ~random ->
      let gen0 = quickcheck_generator_bool
      in
      Pdir_bool
        ( Generator.generate gen0 ~size ~random
        ))
  in
  if size = 0
  then
    Generator.generate ~size ~random
      (Base_quickcheck.Generator.union
        [gen_pdir_none; gen_pdir_string; gen_pdir_int; gen_pdir_bool])
  else
    Generator.generate ~size:(size-1) ~random
      (Base_quickcheck.Generator.union
        [gen_pdir_none; gen_pdir_string; gen_pdir_int; gen_pdir_bool; gen_pdir_ident])

let quickcheck_generator_longident =
  Generator.create generate_longident
let quickcheck_generator_longident_loc =
  Generator.create generate_longident_loc
let quickcheck_generator_rec_flag =
  Generator.create generate_rec_flag
let quickcheck_generator_direction_flag =
  Generator.create generate_direction_flag
let quickcheck_generator_private_flag =
  Generator.create generate_private_flag
let quickcheck_generator_mutable_flag =
  Generator.create generate_mutable_flag
let quickcheck_generator_virtual_flag =
  Generator.create generate_virtual_flag
let quickcheck_generator_override_flag =
  Generator.create generate_override_flag
let quickcheck_generator_closed_flag =
  Generator.create generate_closed_flag
let quickcheck_generator_arg_label =
  Generator.create generate_arg_label
let quickcheck_generator_variance =
  Generator.create generate_variance
let quickcheck_generator_constant =
  Generator.create generate_constant
let quickcheck_generator_attribute =
  Generator.create generate_attribute
let quickcheck_generator_extension =
  Generator.create generate_extension
let quickcheck_generator_attributes =
  Generator.create generate_attributes
let quickcheck_generator_payload =
  Generator.create generate_payload
let quickcheck_generator_core_type =
  Generator.create generate_core_type
let quickcheck_generator_core_type_desc =
  Generator.create generate_core_type_desc
let quickcheck_generator_package_type =
  Generator.create generate_package_type
let quickcheck_generator_row_field =
  Generator.create generate_row_field
let quickcheck_generator_object_field =
  Generator.create generate_object_field
let quickcheck_generator_pattern =
  Generator.create generate_pattern
let quickcheck_generator_pattern_desc =
  Generator.create generate_pattern_desc
let quickcheck_generator_expression =
  Generator.create generate_expression
let quickcheck_generator_expression_desc =
  Generator.create generate_expression_desc
let quickcheck_generator_case =
  Generator.create generate_case
let quickcheck_generator_value_description =
  Generator.create generate_value_description
let quickcheck_generator_type_declaration =
  Generator.create generate_type_declaration
let quickcheck_generator_type_kind =
  Generator.create generate_type_kind
let quickcheck_generator_label_declaration =
  Generator.create generate_label_declaration
let quickcheck_generator_constructor_declaration =
  Generator.create generate_constructor_declaration
let quickcheck_generator_constructor_arguments =
  Generator.create generate_constructor_arguments
let quickcheck_generator_type_extension =
  Generator.create generate_type_extension
let quickcheck_generator_extension_constructor =
  Generator.create generate_extension_constructor
let quickcheck_generator_extension_constructor_kind =
  Generator.create generate_extension_constructor_kind
let quickcheck_generator_class_type =
  Generator.create generate_class_type
let quickcheck_generator_class_type_desc =
  Generator.create generate_class_type_desc
let quickcheck_generator_class_signature =
  Generator.create generate_class_signature
let quickcheck_generator_class_type_field =
  Generator.create generate_class_type_field
let quickcheck_generator_class_type_field_desc =
  Generator.create generate_class_type_field_desc
let quickcheck_generator_class_infos quickcheck_generator_a =
  Generator.create (generate_class_infos quickcheck_generator_a)
let quickcheck_generator_class_description =
  Generator.create generate_class_description
let quickcheck_generator_class_type_declaration =
  Generator.create generate_class_type_declaration
let quickcheck_generator_class_expr =
  Generator.create generate_class_expr
let quickcheck_generator_class_expr_desc =
  Generator.create generate_class_expr_desc
let quickcheck_generator_class_structure =
  Generator.create generate_class_structure
let quickcheck_generator_class_field =
  Generator.create generate_class_field
let quickcheck_generator_class_field_desc =
  Generator.create generate_class_field_desc
let quickcheck_generator_class_field_kind =
  Generator.create generate_class_field_kind
let quickcheck_generator_class_declaration =
  Generator.create generate_class_declaration
let quickcheck_generator_module_type =
  Generator.create generate_module_type
let quickcheck_generator_module_type_desc =
  Generator.create generate_module_type_desc
let quickcheck_generator_signature =
  Generator.create generate_signature
let quickcheck_generator_signature_item =
  Generator.create generate_signature_item
let quickcheck_generator_signature_item_desc =
  Generator.create generate_signature_item_desc
let quickcheck_generator_module_declaration =
  Generator.create generate_module_declaration
let quickcheck_generator_module_type_declaration =
  Generator.create generate_module_type_declaration
let quickcheck_generator_open_description =
  Generator.create generate_open_description
let quickcheck_generator_include_infos quickcheck_generator_a =
  Generator.create (generate_include_infos quickcheck_generator_a)
let quickcheck_generator_include_description =
  Generator.create generate_include_description
let quickcheck_generator_include_declaration =
  Generator.create generate_include_declaration
let quickcheck_generator_with_constraint =
  Generator.create generate_with_constraint
let quickcheck_generator_module_expr =
  Generator.create generate_module_expr
let quickcheck_generator_module_expr_desc =
  Generator.create generate_module_expr_desc
let quickcheck_generator_structure =
  Generator.create generate_structure
let quickcheck_generator_structure_item =
  Generator.create generate_structure_item
let quickcheck_generator_structure_item_desc =
  Generator.create generate_structure_item_desc
let quickcheck_generator_value_binding =
  Generator.create generate_value_binding
let quickcheck_generator_module_binding =
  Generator.create generate_module_binding
let quickcheck_generator_toplevel_phrase =
  Generator.create generate_toplevel_phrase
let quickcheck_generator_directive_argument =
  Generator.create generate_directive_argument

let quickcheck_observer_longident = Observer.opaque
let quickcheck_observer_longident_loc = Observer.opaque
let quickcheck_observer_rec_flag = Observer.opaque
let quickcheck_observer_direction_flag = Observer.opaque
let quickcheck_observer_private_flag = Observer.opaque
let quickcheck_observer_mutable_flag = Observer.opaque
let quickcheck_observer_virtual_flag = Observer.opaque
let quickcheck_observer_override_flag = Observer.opaque
let quickcheck_observer_closed_flag = Observer.opaque
let quickcheck_observer_arg_label = Observer.opaque
let quickcheck_observer_variance = Observer.opaque
let quickcheck_observer_constant = Observer.opaque
let quickcheck_observer_attribute = Observer.opaque
let quickcheck_observer_extension = Observer.opaque
let quickcheck_observer_attributes = Observer.opaque
let quickcheck_observer_payload = Observer.opaque
let quickcheck_observer_core_type = Observer.opaque
let quickcheck_observer_core_type_desc = Observer.opaque
let quickcheck_observer_package_type = Observer.opaque
let quickcheck_observer_row_field = Observer.opaque
let quickcheck_observer_object_field = Observer.opaque
let quickcheck_observer_pattern = Observer.opaque
let quickcheck_observer_pattern_desc = Observer.opaque
let quickcheck_observer_expression = Observer.opaque
let quickcheck_observer_expression_desc = Observer.opaque
let quickcheck_observer_case = Observer.opaque
let quickcheck_observer_value_description = Observer.opaque
let quickcheck_observer_type_declaration = Observer.opaque
let quickcheck_observer_type_kind = Observer.opaque
let quickcheck_observer_label_declaration = Observer.opaque
let quickcheck_observer_constructor_declaration = Observer.opaque
let quickcheck_observer_constructor_arguments = Observer.opaque
let quickcheck_observer_type_extension = Observer.opaque
let quickcheck_observer_extension_constructor = Observer.opaque
let quickcheck_observer_extension_constructor_kind = Observer.opaque
let quickcheck_observer_class_type = Observer.opaque
let quickcheck_observer_class_type_desc = Observer.opaque
let quickcheck_observer_class_signature = Observer.opaque
let quickcheck_observer_class_type_field = Observer.opaque
let quickcheck_observer_class_type_field_desc = Observer.opaque
let quickcheck_observer_class_infos _ = Observer.opaque
let quickcheck_observer_class_description = Observer.opaque
let quickcheck_observer_class_type_declaration = Observer.opaque
let quickcheck_observer_class_expr = Observer.opaque
let quickcheck_observer_class_expr_desc = Observer.opaque
let quickcheck_observer_class_structure = Observer.opaque
let quickcheck_observer_class_field = Observer.opaque
let quickcheck_observer_class_field_desc = Observer.opaque
let quickcheck_observer_class_field_kind = Observer.opaque
let quickcheck_observer_class_declaration = Observer.opaque
let quickcheck_observer_module_type = Observer.opaque
let quickcheck_observer_module_type_desc = Observer.opaque
let quickcheck_observer_signature = Observer.opaque
let quickcheck_observer_signature_item = Observer.opaque
let quickcheck_observer_signature_item_desc = Observer.opaque
let quickcheck_observer_module_declaration = Observer.opaque
let quickcheck_observer_module_type_declaration = Observer.opaque
let quickcheck_observer_open_description = Observer.opaque
let quickcheck_observer_include_infos _ = Observer.opaque
let quickcheck_observer_include_description = Observer.opaque
let quickcheck_observer_include_declaration = Observer.opaque
let quickcheck_observer_with_constraint = Observer.opaque
let quickcheck_observer_module_expr = Observer.opaque
let quickcheck_observer_module_expr_desc = Observer.opaque
let quickcheck_observer_structure = Observer.opaque
let quickcheck_observer_structure_item = Observer.opaque
let quickcheck_observer_structure_item_desc = Observer.opaque
let quickcheck_observer_value_binding = Observer.opaque
let quickcheck_observer_module_binding = Observer.opaque
let quickcheck_observer_toplevel_phrase = Observer.opaque
let quickcheck_observer_directive_argument = Observer.opaque

let quickcheck_shrinker_longident = Shrinker.atomic
let quickcheck_shrinker_longident_loc = Shrinker.atomic
let quickcheck_shrinker_rec_flag = Shrinker.atomic
let quickcheck_shrinker_direction_flag = Shrinker.atomic
let quickcheck_shrinker_private_flag = Shrinker.atomic
let quickcheck_shrinker_mutable_flag = Shrinker.atomic
let quickcheck_shrinker_virtual_flag = Shrinker.atomic
let quickcheck_shrinker_override_flag = Shrinker.atomic
let quickcheck_shrinker_closed_flag = Shrinker.atomic
let quickcheck_shrinker_arg_label = Shrinker.atomic
let quickcheck_shrinker_variance = Shrinker.atomic
let quickcheck_shrinker_constant = Shrinker.atomic
let quickcheck_shrinker_attribute = Shrinker.atomic
let quickcheck_shrinker_extension = Shrinker.atomic
let quickcheck_shrinker_attributes = Shrinker.atomic
let quickcheck_shrinker_payload = Shrinker.atomic
let quickcheck_shrinker_core_type = Shrinker.atomic
let quickcheck_shrinker_core_type_desc = Shrinker.atomic
let quickcheck_shrinker_package_type = Shrinker.atomic
let quickcheck_shrinker_row_field = Shrinker.atomic
let quickcheck_shrinker_object_field = Shrinker.atomic
let quickcheck_shrinker_pattern = Shrinker.atomic
let quickcheck_shrinker_pattern_desc = Shrinker.atomic
let quickcheck_shrinker_expression = Shrinker.atomic
let quickcheck_shrinker_expression_desc = Shrinker.atomic
let quickcheck_shrinker_case = Shrinker.atomic
let quickcheck_shrinker_value_description = Shrinker.atomic
let quickcheck_shrinker_type_declaration = Shrinker.atomic
let quickcheck_shrinker_type_kind = Shrinker.atomic
let quickcheck_shrinker_label_declaration = Shrinker.atomic
let quickcheck_shrinker_constructor_declaration = Shrinker.atomic
let quickcheck_shrinker_constructor_arguments = Shrinker.atomic
let quickcheck_shrinker_type_extension = Shrinker.atomic
let quickcheck_shrinker_extension_constructor = Shrinker.atomic
let quickcheck_shrinker_extension_constructor_kind = Shrinker.atomic
let quickcheck_shrinker_class_type = Shrinker.atomic
let quickcheck_shrinker_class_type_desc = Shrinker.atomic
let quickcheck_shrinker_class_signature = Shrinker.atomic
let quickcheck_shrinker_class_type_field = Shrinker.atomic
let quickcheck_shrinker_class_type_field_desc = Shrinker.atomic
let quickcheck_shrinker_class_infos _ = Shrinker.atomic
let quickcheck_shrinker_class_description = Shrinker.atomic
let quickcheck_shrinker_class_type_declaration = Shrinker.atomic
let quickcheck_shrinker_class_expr = Shrinker.atomic
let quickcheck_shrinker_class_expr_desc = Shrinker.atomic
let quickcheck_shrinker_class_structure = Shrinker.atomic
let quickcheck_shrinker_class_field = Shrinker.atomic
let quickcheck_shrinker_class_field_desc = Shrinker.atomic
let quickcheck_shrinker_class_field_kind = Shrinker.atomic
let quickcheck_shrinker_class_declaration = Shrinker.atomic
let quickcheck_shrinker_module_type = Shrinker.atomic
let quickcheck_shrinker_module_type_desc = Shrinker.atomic
let quickcheck_shrinker_signature = Shrinker.atomic
let quickcheck_shrinker_signature_item = Shrinker.atomic
let quickcheck_shrinker_signature_item_desc = Shrinker.atomic
let quickcheck_shrinker_module_declaration = Shrinker.atomic
let quickcheck_shrinker_module_type_declaration = Shrinker.atomic
let quickcheck_shrinker_open_description = Shrinker.atomic
let quickcheck_shrinker_include_infos _ = Shrinker.atomic
let quickcheck_shrinker_include_description = Shrinker.atomic
let quickcheck_shrinker_include_declaration = Shrinker.atomic
let quickcheck_shrinker_with_constraint = Shrinker.atomic
let quickcheck_shrinker_module_expr = Shrinker.atomic
let quickcheck_shrinker_module_expr_desc = Shrinker.atomic
let quickcheck_shrinker_structure = Shrinker.atomic
let quickcheck_shrinker_structure_item = Shrinker.atomic
let quickcheck_shrinker_structure_item_desc = Shrinker.atomic
let quickcheck_shrinker_value_binding = Shrinker.atomic
let quickcheck_shrinker_module_binding = Shrinker.atomic
let quickcheck_shrinker_toplevel_phrase = Shrinker.atomic
let quickcheck_shrinker_directive_argument = Shrinker.atomic

module Longident = struct
  type t = longident
  [@@deriving equal, quickcheck, sexp_of]
end

module Longident_loc = struct
  type t = longident_loc
  [@@deriving equal, quickcheck, sexp_of]
end

module Rec_flag = struct
  type t = rec_flag
  [@@deriving equal, quickcheck, sexp_of]
end

module Direction_flag = struct
  type t = direction_flag
  [@@deriving equal, quickcheck, sexp_of]
end

module Private_flag = struct
  type t = private_flag
  [@@deriving equal, quickcheck, sexp_of]
end

module Mutable_flag = struct
  type t = mutable_flag
  [@@deriving equal, quickcheck, sexp_of]
end

module Virtual_flag = struct
  type t = virtual_flag
  [@@deriving equal, quickcheck, sexp_of]
end

module Override_flag = struct
  type t = override_flag
  [@@deriving equal, quickcheck, sexp_of]
end

module Closed_flag = struct
  type t = closed_flag
  [@@deriving equal, quickcheck, sexp_of]
end

module Arg_label = struct
  type t = arg_label
  [@@deriving equal, quickcheck, sexp_of]
end

module Variance = struct
  type t = variance
  [@@deriving equal, quickcheck, sexp_of]
end

module Constant = struct
  type t = constant
  [@@deriving equal, quickcheck, sexp_of]
end

module Attribute = struct
  type t = attribute
  [@@deriving equal, quickcheck, sexp_of]
end

module Extension = struct
  type t = extension
  [@@deriving equal, quickcheck, sexp_of]
end

module Attributes = struct
  type t = attributes
  [@@deriving equal, quickcheck, sexp_of]
end

module Payload = struct
  type t = payload
  [@@deriving equal, quickcheck, sexp_of]
end

module Core_type = struct
  type t = core_type
  [@@deriving equal, quickcheck, sexp_of]
end

module Core_type_desc = struct
  type t = core_type_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Package_type = struct
  type t = package_type
  [@@deriving equal, quickcheck, sexp_of]
end

module Row_field = struct
  type t = row_field
  [@@deriving equal, quickcheck, sexp_of]
end

module Object_field = struct
  type t = object_field
  [@@deriving equal, quickcheck, sexp_of]
end

module Pattern = struct
  type t = pattern
  [@@deriving equal, quickcheck, sexp_of]
end

module Pattern_desc = struct
  type t = pattern_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Expression = struct
  type t = expression
  [@@deriving equal, quickcheck, sexp_of]
end

module Expression_desc = struct
  type t = expression_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Case = struct
  type t = case
  [@@deriving equal, quickcheck, sexp_of]
end

module Value_description = struct
  type t = value_description
  [@@deriving equal, quickcheck, sexp_of]
end

module Type_declaration = struct
  type t = type_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module Type_kind = struct
  type t = type_kind
  [@@deriving equal, quickcheck, sexp_of]
end

module Label_declaration = struct
  type t = label_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module Constructor_declaration = struct
  type t = constructor_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module Constructor_arguments = struct
  type t = constructor_arguments
  [@@deriving equal, quickcheck, sexp_of]
end

module Type_extension = struct
  type t = type_extension
  [@@deriving equal, quickcheck, sexp_of]
end

module Extension_constructor = struct
  type t = extension_constructor
  [@@deriving equal, quickcheck, sexp_of]
end

module Extension_constructor_kind = struct
  type t = extension_constructor_kind
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_type = struct
  type t = class_type
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_type_desc = struct
  type t = class_type_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_signature = struct
  type t = class_signature
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_type_field = struct
  type t = class_type_field
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_type_field_desc = struct
  type t = class_type_field_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_infos = struct
  type 'a t = 'a class_infos
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_description = struct
  type t = class_description
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_type_declaration = struct
  type t = class_type_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_expr = struct
  type t = class_expr
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_expr_desc = struct
  type t = class_expr_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_structure = struct
  type t = class_structure
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_field = struct
  type t = class_field
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_field_desc = struct
  type t = class_field_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_field_kind = struct
  type t = class_field_kind
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_declaration = struct
  type t = class_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module Module_type = struct
  type t = module_type
  [@@deriving equal, quickcheck, sexp_of]
end

module Module_type_desc = struct
  type t = module_type_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Signature = struct
  type t = signature
  [@@deriving equal, quickcheck, sexp_of]
end

module Signature_item = struct
  type t = signature_item
  [@@deriving equal, quickcheck, sexp_of]
end

module Signature_item_desc = struct
  type t = signature_item_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Module_declaration = struct
  type t = module_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module Module_type_declaration = struct
  type t = module_type_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module Open_description = struct
  type t = open_description
  [@@deriving equal, quickcheck, sexp_of]
end

module Include_infos = struct
  type 'a t = 'a include_infos
  [@@deriving equal, quickcheck, sexp_of]
end

module Include_description = struct
  type t = include_description
  [@@deriving equal, quickcheck, sexp_of]
end

module Include_declaration = struct
  type t = include_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module With_constraint = struct
  type t = with_constraint
  [@@deriving equal, quickcheck, sexp_of]
end

module Module_expr = struct
  type t = module_expr
  [@@deriving equal, quickcheck, sexp_of]
end

module Module_expr_desc = struct
  type t = module_expr_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Structure = struct
  type t = structure
  [@@deriving equal, quickcheck, sexp_of]
end

module Structure_item = struct
  type t = structure_item
  [@@deriving equal, quickcheck, sexp_of]
end

module Structure_item_desc = struct
  type t = structure_item_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Value_binding = struct
  type t = value_binding
  [@@deriving equal, quickcheck, sexp_of]
end

module Module_binding = struct
  type t = module_binding
  [@@deriving equal, quickcheck, sexp_of]
end

module Toplevel_phrase = struct
  type t = toplevel_phrase
  [@@deriving equal, quickcheck, sexp_of]
end

module Directive_argument = struct
  type t = directive_argument
  [@@deriving equal, quickcheck, sexp_of]
end
(*$*)
