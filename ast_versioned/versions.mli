open Ocaml_common

(*$ Ppx_ast_versioned_cinaps.print_versions_mli () *)
module V4_07 : sig
  module rec Longident : sig
    type t

    type concrete =
      | Lident of string
      | Ldot of Longident.t * string
      | Lapply of Longident.t * Longident.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_lident :
      string
      -> t
    val create_ldot :
      Longident.t
      -> string
      -> t
    val create_lapply :
      Longident.t
      -> Longident.t
      -> t
  end

  and Longident_loc : sig
    type t

    type concrete = Longident.t Location.loc

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Longident.t Location.loc -> t
  end

  and Rec_flag : sig
    type t

    type concrete =
      | Nonrecursive
      | Recursive

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_nonrecursive : t
    val create_recursive : t
  end

  and Direction_flag : sig
    type t

    type concrete =
      | Upto
      | Downto

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_upto : t
    val create_downto : t
  end

  and Private_flag : sig
    type t

    type concrete =
      | Private
      | Public

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_private : t
    val create_public : t
  end

  and Mutable_flag : sig
    type t

    type concrete =
      | Immutable
      | Mutable

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_immutable : t
    val create_mutable : t
  end

  and Virtual_flag : sig
    type t

    type concrete =
      | Virtual
      | Concrete

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_virtual : t
    val create_concrete : t
  end

  and Override_flag : sig
    type t

    type concrete =
      | Override
      | Fresh

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_override : t
    val create_fresh : t
  end

  and Closed_flag : sig
    type t

    type concrete =
      | Closed
      | Open

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_closed : t
    val create_open : t
  end

  and Label : sig
    type t

    type concrete = string

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : string -> t
  end

  and Arg_label : sig
    type t

    type concrete =
      | Nolabel
      | Labelled of string
      | Optional of string

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_nolabel : t
    val create_labelled :
      string
      -> t
    val create_optional :
      string
      -> t
  end

  and Variance : sig
    type t

    type concrete =
      | Covariant
      | Contravariant
      | Invariant

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_covariant : t
    val create_contravariant : t
    val create_invariant : t
  end

  and Constant : sig
    type t

    type concrete =
      | Pconst_integer of string * char option
      | Pconst_char of char
      | Pconst_string of string * string option
      | Pconst_float of string * char option

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pconst_integer :
      string
      -> char option
      -> t
    val create_pconst_char :
      char
      -> t
    val create_pconst_string :
      string
      -> string option
      -> t
    val create_pconst_float :
      string
      -> char option
      -> t
  end

  and Attribute : sig
    type t

    type concrete = (string Location.loc * Payload.t)

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : (string Location.loc * Payload.t) -> t
  end

  and Extension : sig
    type t

    type concrete = (string Location.loc * Payload.t)

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : (string Location.loc * Payload.t) -> t
  end

  and Attributes : sig
    type t

    type concrete = Attribute.t list

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Attribute.t list -> t
  end

  and Payload : sig
    type t

    type concrete =
      | PStr of Structure.t
      | PSig of Signature.t
      | PTyp of Core_type.t
      | PPat of Pattern.t * Expression.t option

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pstr :
      Structure.t
      -> t
    val create_psig :
      Signature.t
      -> t
    val create_ptyp :
      Core_type.t
      -> t
    val create_ppat :
      Pattern.t
      -> Expression.t option
      -> t
  end

  and Core_type : sig
    type t

    type concrete =
      { ptyp_desc : Core_type_desc.t
      ; ptyp_loc : Location.t
      ; ptyp_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      ptyp_desc:Core_type_desc.t
      -> ptyp_loc:Location.t
      -> ptyp_attributes:Attributes.t
      -> t
  end

  and Core_type_desc : sig
    type t

    type concrete =
      | Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of Arg_label.t * Core_type.t * Core_type.t
      | Ptyp_tuple of Core_type.t list
      | Ptyp_constr of Longident_loc.t * Core_type.t list
      | Ptyp_object of Object_field.t list * Closed_flag.t
      | Ptyp_class of Longident_loc.t * Core_type.t list
      | Ptyp_alias of Core_type.t * string
      | Ptyp_variant of Row_field.t list * Closed_flag.t * Label.t list option
      | Ptyp_poly of string Location.loc list * Core_type.t
      | Ptyp_package of Package_type.t
      | Ptyp_extension of Extension.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_ptyp_any : t
    val create_ptyp_var :
      string
      -> t
    val create_ptyp_arrow :
      Arg_label.t
      -> Core_type.t
      -> Core_type.t
      -> t
    val create_ptyp_tuple :
      Core_type.t list
      -> t
    val create_ptyp_constr :
      Longident_loc.t
      -> Core_type.t list
      -> t
    val create_ptyp_object :
      Object_field.t list
      -> Closed_flag.t
      -> t
    val create_ptyp_class :
      Longident_loc.t
      -> Core_type.t list
      -> t
    val create_ptyp_alias :
      Core_type.t
      -> string
      -> t
    val create_ptyp_variant :
      Row_field.t list
      -> Closed_flag.t
      -> Label.t list option
      -> t
    val create_ptyp_poly :
      string Location.loc list
      -> Core_type.t
      -> t
    val create_ptyp_package :
      Package_type.t
      -> t
    val create_ptyp_extension :
      Extension.t
      -> t
  end

  and Package_type : sig
    type t

    type concrete = (Longident_loc.t * (Longident_loc.t * Core_type.t) list)

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : (Longident_loc.t * (Longident_loc.t * Core_type.t) list) -> t
  end

  and Row_field : sig
    type t

    type concrete =
      | Rtag of Label.t Location.loc * Attributes.t * bool * Core_type.t list
      | Rinherit of Core_type.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_rtag :
      Label.t Location.loc
      -> Attributes.t
      -> bool
      -> Core_type.t list
      -> t
    val create_rinherit :
      Core_type.t
      -> t
  end

  and Object_field : sig
    type t

    type concrete =
      | Otag of Label.t Location.loc * Attributes.t * Core_type.t
      | Oinherit of Core_type.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_otag :
      Label.t Location.loc
      -> Attributes.t
      -> Core_type.t
      -> t
    val create_oinherit :
      Core_type.t
      -> t
  end

  and Pattern : sig
    type t

    type concrete =
      { ppat_desc : Pattern_desc.t
      ; ppat_loc : Location.t
      ; ppat_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      ppat_desc:Pattern_desc.t
      -> ppat_loc:Location.t
      -> ppat_attributes:Attributes.t
      -> t
  end

  and Pattern_desc : sig
    type t

    type concrete =
      | Ppat_any
      | Ppat_var of string Location.loc
      | Ppat_alias of Pattern.t * string Location.loc
      | Ppat_constant of Constant.t
      | Ppat_interval of Constant.t * Constant.t
      | Ppat_tuple of Pattern.t list
      | Ppat_construct of Longident_loc.t * Pattern.t option
      | Ppat_variant of Label.t * Pattern.t option
      | Ppat_record of (Longident_loc.t * Pattern.t) list * Closed_flag.t
      | Ppat_array of Pattern.t list
      | Ppat_or of Pattern.t * Pattern.t
      | Ppat_constraint of Pattern.t * Core_type.t
      | Ppat_type of Longident_loc.t
      | Ppat_lazy of Pattern.t
      | Ppat_unpack of string Location.loc
      | Ppat_exception of Pattern.t
      | Ppat_extension of Extension.t
      | Ppat_open of Longident_loc.t * Pattern.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_ppat_any : t
    val create_ppat_var :
      string Location.loc
      -> t
    val create_ppat_alias :
      Pattern.t
      -> string Location.loc
      -> t
    val create_ppat_constant :
      Constant.t
      -> t
    val create_ppat_interval :
      Constant.t
      -> Constant.t
      -> t
    val create_ppat_tuple :
      Pattern.t list
      -> t
    val create_ppat_construct :
      Longident_loc.t
      -> Pattern.t option
      -> t
    val create_ppat_variant :
      Label.t
      -> Pattern.t option
      -> t
    val create_ppat_record :
      (Longident_loc.t * Pattern.t) list
      -> Closed_flag.t
      -> t
    val create_ppat_array :
      Pattern.t list
      -> t
    val create_ppat_or :
      Pattern.t
      -> Pattern.t
      -> t
    val create_ppat_constraint :
      Pattern.t
      -> Core_type.t
      -> t
    val create_ppat_type :
      Longident_loc.t
      -> t
    val create_ppat_lazy :
      Pattern.t
      -> t
    val create_ppat_unpack :
      string Location.loc
      -> t
    val create_ppat_exception :
      Pattern.t
      -> t
    val create_ppat_extension :
      Extension.t
      -> t
    val create_ppat_open :
      Longident_loc.t
      -> Pattern.t
      -> t
  end

  and Expression : sig
    type t

    type concrete =
      { pexp_desc : Expression_desc.t
      ; pexp_loc : Location.t
      ; pexp_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pexp_desc:Expression_desc.t
      -> pexp_loc:Location.t
      -> pexp_attributes:Attributes.t
      -> t
  end

  and Expression_desc : sig
    type t

    type concrete =
      | Pexp_ident of Longident_loc.t
      | Pexp_constant of Constant.t
      | Pexp_let of Rec_flag.t * Value_binding.t list * Expression.t
      | Pexp_function of Case.t list
      | Pexp_fun of Arg_label.t * Expression.t option * Pattern.t * Expression.t
      | Pexp_apply of Expression.t * (Arg_label.t * Expression.t) list
      | Pexp_match of Expression.t * Case.t list
      | Pexp_try of Expression.t * Case.t list
      | Pexp_tuple of Expression.t list
      | Pexp_construct of Longident_loc.t * Expression.t option
      | Pexp_variant of Label.t * Expression.t option
      | Pexp_record of (Longident_loc.t * Expression.t) list * Expression.t option
      | Pexp_field of Expression.t * Longident_loc.t
      | Pexp_setfield of Expression.t * Longident_loc.t * Expression.t
      | Pexp_array of Expression.t list
      | Pexp_ifthenelse of Expression.t * Expression.t * Expression.t option
      | Pexp_sequence of Expression.t * Expression.t
      | Pexp_while of Expression.t * Expression.t
      | Pexp_for of Pattern.t * Expression.t * Expression.t * Direction_flag.t * Expression.t
      | Pexp_constraint of Expression.t * Core_type.t
      | Pexp_coerce of Expression.t * Core_type.t option * Core_type.t
      | Pexp_send of Expression.t * Label.t Location.loc
      | Pexp_new of Longident_loc.t
      | Pexp_setinstvar of Label.t Location.loc * Expression.t
      | Pexp_override of (Label.t Location.loc * Expression.t) list
      | Pexp_letmodule of string Location.loc * Module_expr.t * Expression.t
      | Pexp_letexception of Extension_constructor.t * Expression.t
      | Pexp_assert of Expression.t
      | Pexp_lazy of Expression.t
      | Pexp_poly of Expression.t * Core_type.t option
      | Pexp_object of Class_structure.t
      | Pexp_newtype of string Location.loc * Expression.t
      | Pexp_pack of Module_expr.t
      | Pexp_open of Override_flag.t * Longident_loc.t * Expression.t
      | Pexp_extension of Extension.t
      | Pexp_unreachable

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pexp_ident :
      Longident_loc.t
      -> t
    val create_pexp_constant :
      Constant.t
      -> t
    val create_pexp_let :
      Rec_flag.t
      -> Value_binding.t list
      -> Expression.t
      -> t
    val create_pexp_function :
      Case.t list
      -> t
    val create_pexp_fun :
      Arg_label.t
      -> Expression.t option
      -> Pattern.t
      -> Expression.t
      -> t
    val create_pexp_apply :
      Expression.t
      -> (Arg_label.t * Expression.t) list
      -> t
    val create_pexp_match :
      Expression.t
      -> Case.t list
      -> t
    val create_pexp_try :
      Expression.t
      -> Case.t list
      -> t
    val create_pexp_tuple :
      Expression.t list
      -> t
    val create_pexp_construct :
      Longident_loc.t
      -> Expression.t option
      -> t
    val create_pexp_variant :
      Label.t
      -> Expression.t option
      -> t
    val create_pexp_record :
      (Longident_loc.t * Expression.t) list
      -> Expression.t option
      -> t
    val create_pexp_field :
      Expression.t
      -> Longident_loc.t
      -> t
    val create_pexp_setfield :
      Expression.t
      -> Longident_loc.t
      -> Expression.t
      -> t
    val create_pexp_array :
      Expression.t list
      -> t
    val create_pexp_ifthenelse :
      Expression.t
      -> Expression.t
      -> Expression.t option
      -> t
    val create_pexp_sequence :
      Expression.t
      -> Expression.t
      -> t
    val create_pexp_while :
      Expression.t
      -> Expression.t
      -> t
    val create_pexp_for :
      Pattern.t
      -> Expression.t
      -> Expression.t
      -> Direction_flag.t
      -> Expression.t
      -> t
    val create_pexp_constraint :
      Expression.t
      -> Core_type.t
      -> t
    val create_pexp_coerce :
      Expression.t
      -> Core_type.t option
      -> Core_type.t
      -> t
    val create_pexp_send :
      Expression.t
      -> Label.t Location.loc
      -> t
    val create_pexp_new :
      Longident_loc.t
      -> t
    val create_pexp_setinstvar :
      Label.t Location.loc
      -> Expression.t
      -> t
    val create_pexp_override :
      (Label.t Location.loc * Expression.t) list
      -> t
    val create_pexp_letmodule :
      string Location.loc
      -> Module_expr.t
      -> Expression.t
      -> t
    val create_pexp_letexception :
      Extension_constructor.t
      -> Expression.t
      -> t
    val create_pexp_assert :
      Expression.t
      -> t
    val create_pexp_lazy :
      Expression.t
      -> t
    val create_pexp_poly :
      Expression.t
      -> Core_type.t option
      -> t
    val create_pexp_object :
      Class_structure.t
      -> t
    val create_pexp_newtype :
      string Location.loc
      -> Expression.t
      -> t
    val create_pexp_pack :
      Module_expr.t
      -> t
    val create_pexp_open :
      Override_flag.t
      -> Longident_loc.t
      -> Expression.t
      -> t
    val create_pexp_extension :
      Extension.t
      -> t
    val create_pexp_unreachable : t
  end

  and Case : sig
    type t

    type concrete =
      { pc_lhs : Pattern.t
      ; pc_guard : Expression.t option
      ; pc_rhs : Expression.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pc_lhs:Pattern.t
      -> pc_guard:Expression.t option
      -> pc_rhs:Expression.t
      -> t
  end

  and Value_description : sig
    type t

    type concrete =
      { pval_name : string Location.loc
      ; pval_type : Core_type.t
      ; pval_prim : string list
      ; pval_attributes : Attributes.t
      ; pval_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pval_name:string Location.loc
      -> pval_type:Core_type.t
      -> pval_prim:string list
      -> pval_attributes:Attributes.t
      -> pval_loc:Location.t
      -> t
  end

  and Type_declaration : sig
    type t

    type concrete =
      { ptype_name : string Location.loc
      ; ptype_params : (Core_type.t * Variance.t) list
      ; ptype_cstrs : (Core_type.t * Core_type.t * Location.t) list
      ; ptype_kind : Type_kind.t
      ; ptype_private : Private_flag.t
      ; ptype_manifest : Core_type.t option
      ; ptype_attributes : Attributes.t
      ; ptype_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      ptype_name:string Location.loc
      -> ptype_params:(Core_type.t * Variance.t) list
      -> ptype_cstrs:(Core_type.t * Core_type.t * Location.t) list
      -> ptype_kind:Type_kind.t
      -> ptype_private:Private_flag.t
      -> ptype_manifest:Core_type.t option
      -> ptype_attributes:Attributes.t
      -> ptype_loc:Location.t
      -> t
  end

  and Type_kind : sig
    type t

    type concrete =
      | Ptype_abstract
      | Ptype_variant of Constructor_declaration.t list
      | Ptype_record of Label_declaration.t list
      | Ptype_open

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_ptype_abstract : t
    val create_ptype_variant :
      Constructor_declaration.t list
      -> t
    val create_ptype_record :
      Label_declaration.t list
      -> t
    val create_ptype_open : t
  end

  and Label_declaration : sig
    type t

    type concrete =
      { pld_name : string Location.loc
      ; pld_mutable : Mutable_flag.t
      ; pld_type : Core_type.t
      ; pld_loc : Location.t
      ; pld_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pld_name:string Location.loc
      -> pld_mutable:Mutable_flag.t
      -> pld_type:Core_type.t
      -> pld_loc:Location.t
      -> pld_attributes:Attributes.t
      -> t
  end

  and Constructor_declaration : sig
    type t

    type concrete =
      { pcd_name : string Location.loc
      ; pcd_args : Constructor_arguments.t
      ; pcd_res : Core_type.t option
      ; pcd_loc : Location.t
      ; pcd_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pcd_name:string Location.loc
      -> pcd_args:Constructor_arguments.t
      -> pcd_res:Core_type.t option
      -> pcd_loc:Location.t
      -> pcd_attributes:Attributes.t
      -> t
  end

  and Constructor_arguments : sig
    type t

    type concrete =
      | Pcstr_tuple of Core_type.t list
      | Pcstr_record of Label_declaration.t list

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pcstr_tuple :
      Core_type.t list
      -> t
    val create_pcstr_record :
      Label_declaration.t list
      -> t
  end

  and Type_extension : sig
    type t

    type concrete =
      { ptyext_path : Longident_loc.t
      ; ptyext_params : (Core_type.t * Variance.t) list
      ; ptyext_constructors : Extension_constructor.t list
      ; ptyext_private : Private_flag.t
      ; ptyext_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      ptyext_path:Longident_loc.t
      -> ptyext_params:(Core_type.t * Variance.t) list
      -> ptyext_constructors:Extension_constructor.t list
      -> ptyext_private:Private_flag.t
      -> ptyext_attributes:Attributes.t
      -> t
  end

  and Extension_constructor : sig
    type t

    type concrete =
      { pext_name : string Location.loc
      ; pext_kind : Extension_constructor_kind.t
      ; pext_loc : Location.t
      ; pext_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pext_name:string Location.loc
      -> pext_kind:Extension_constructor_kind.t
      -> pext_loc:Location.t
      -> pext_attributes:Attributes.t
      -> t
  end

  and Extension_constructor_kind : sig
    type t

    type concrete =
      | Pext_decl of Constructor_arguments.t * Core_type.t option
      | Pext_rebind of Longident_loc.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pext_decl :
      Constructor_arguments.t
      -> Core_type.t option
      -> t
    val create_pext_rebind :
      Longident_loc.t
      -> t
  end

  and Class_type : sig
    type t

    type concrete =
      { pcty_desc : Class_type_desc.t
      ; pcty_loc : Location.t
      ; pcty_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pcty_desc:Class_type_desc.t
      -> pcty_loc:Location.t
      -> pcty_attributes:Attributes.t
      -> t
  end

  and Class_type_desc : sig
    type t

    type concrete =
      | Pcty_constr of Longident_loc.t * Core_type.t list
      | Pcty_signature of Class_signature.t
      | Pcty_arrow of Arg_label.t * Core_type.t * Class_type.t
      | Pcty_extension of Extension.t
      | Pcty_open of Override_flag.t * Longident_loc.t * Class_type.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pcty_constr :
      Longident_loc.t
      -> Core_type.t list
      -> t
    val create_pcty_signature :
      Class_signature.t
      -> t
    val create_pcty_arrow :
      Arg_label.t
      -> Core_type.t
      -> Class_type.t
      -> t
    val create_pcty_extension :
      Extension.t
      -> t
    val create_pcty_open :
      Override_flag.t
      -> Longident_loc.t
      -> Class_type.t
      -> t
  end

  and Class_signature : sig
    type t

    type concrete =
      { pcsig_self : Core_type.t
      ; pcsig_fields : Class_type_field.t list
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pcsig_self:Core_type.t
      -> pcsig_fields:Class_type_field.t list
      -> t
  end

  and Class_type_field : sig
    type t

    type concrete =
      { pctf_desc : Class_type_field_desc.t
      ; pctf_loc : Location.t
      ; pctf_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pctf_desc:Class_type_field_desc.t
      -> pctf_loc:Location.t
      -> pctf_attributes:Attributes.t
      -> t
  end

  and Class_type_field_desc : sig
    type t

    type concrete =
      | Pctf_inherit of Class_type.t
      | Pctf_val of (Label.t Location.loc * Mutable_flag.t * Virtual_flag.t * Core_type.t)
      | Pctf_method of (Label.t Location.loc * Private_flag.t * Virtual_flag.t * Core_type.t)
      | Pctf_constraint of (Core_type.t * Core_type.t)
      | Pctf_attribute of Attribute.t
      | Pctf_extension of Extension.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pctf_inherit :
      Class_type.t
      -> t
    val create_pctf_val :
      (Label.t Location.loc * Mutable_flag.t * Virtual_flag.t * Core_type.t)
      -> t
    val create_pctf_method :
      (Label.t Location.loc * Private_flag.t * Virtual_flag.t * Core_type.t)
      -> t
    val create_pctf_constraint :
      (Core_type.t * Core_type.t)
      -> t
    val create_pctf_attribute :
      Attribute.t
      -> t
    val create_pctf_extension :
      Extension.t
      -> t
  end

  and Class_infos : sig
    type 'a t

    type 'a concrete =
      { pci_virt : Virtual_flag.t
      ; pci_params : (Core_type.t * Variance.t) list
      ; pci_name : string Location.loc
      ; pci_expr : 'a
      ; pci_loc : Location.t
      ; pci_attributes : Attributes.t
      }

    val of_concrete_class_expr : Class_expr.t concrete -> Class_expr.t t
    val to_concrete_class_expr : Class_expr.t t -> Class_expr.t concrete option

    val create_class_expr :
      pci_virt:Virtual_flag.t
      -> pci_params:(Core_type.t * Variance.t) list
      -> pci_name:string Location.loc
      -> pci_expr:Class_expr.t
      -> pci_loc:Location.t
      -> pci_attributes:Attributes.t
      -> Class_expr.t t

    val of_concrete_class_type : Class_type.t concrete -> Class_type.t t
    val to_concrete_class_type : Class_type.t t -> Class_type.t concrete option

    val create_class_type :
      pci_virt:Virtual_flag.t
      -> pci_params:(Core_type.t * Variance.t) list
      -> pci_name:string Location.loc
      -> pci_expr:Class_type.t
      -> pci_loc:Location.t
      -> pci_attributes:Attributes.t
      -> Class_type.t t
  end

  and Class_description : sig
    type t

    type concrete = Class_type.t Class_infos.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Class_type.t Class_infos.t -> t
  end

  and Class_type_declaration : sig
    type t

    type concrete = Class_type.t Class_infos.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Class_type.t Class_infos.t -> t
  end

  and Class_expr : sig
    type t

    type concrete =
      { pcl_desc : Class_expr_desc.t
      ; pcl_loc : Location.t
      ; pcl_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pcl_desc:Class_expr_desc.t
      -> pcl_loc:Location.t
      -> pcl_attributes:Attributes.t
      -> t
  end

  and Class_expr_desc : sig
    type t

    type concrete =
      | Pcl_constr of Longident_loc.t * Core_type.t list
      | Pcl_structure of Class_structure.t
      | Pcl_fun of Arg_label.t * Expression.t option * Pattern.t * Class_expr.t
      | Pcl_apply of Class_expr.t * (Arg_label.t * Expression.t) list
      | Pcl_let of Rec_flag.t * Value_binding.t list * Class_expr.t
      | Pcl_constraint of Class_expr.t * Class_type.t
      | Pcl_extension of Extension.t
      | Pcl_open of Override_flag.t * Longident_loc.t * Class_expr.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pcl_constr :
      Longident_loc.t
      -> Core_type.t list
      -> t
    val create_pcl_structure :
      Class_structure.t
      -> t
    val create_pcl_fun :
      Arg_label.t
      -> Expression.t option
      -> Pattern.t
      -> Class_expr.t
      -> t
    val create_pcl_apply :
      Class_expr.t
      -> (Arg_label.t * Expression.t) list
      -> t
    val create_pcl_let :
      Rec_flag.t
      -> Value_binding.t list
      -> Class_expr.t
      -> t
    val create_pcl_constraint :
      Class_expr.t
      -> Class_type.t
      -> t
    val create_pcl_extension :
      Extension.t
      -> t
    val create_pcl_open :
      Override_flag.t
      -> Longident_loc.t
      -> Class_expr.t
      -> t
  end

  and Class_structure : sig
    type t

    type concrete =
      { pcstr_self : Pattern.t
      ; pcstr_fields : Class_field.t list
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pcstr_self:Pattern.t
      -> pcstr_fields:Class_field.t list
      -> t
  end

  and Class_field : sig
    type t

    type concrete =
      { pcf_desc : Class_field_desc.t
      ; pcf_loc : Location.t
      ; pcf_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pcf_desc:Class_field_desc.t
      -> pcf_loc:Location.t
      -> pcf_attributes:Attributes.t
      -> t
  end

  and Class_field_desc : sig
    type t

    type concrete =
      | Pcf_inherit of Override_flag.t * Class_expr.t * string Location.loc option
      | Pcf_val of (Label.t Location.loc * Mutable_flag.t * Class_field_kind.t)
      | Pcf_method of (Label.t Location.loc * Private_flag.t * Class_field_kind.t)
      | Pcf_constraint of (Core_type.t * Core_type.t)
      | Pcf_initializer of Expression.t
      | Pcf_attribute of Attribute.t
      | Pcf_extension of Extension.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pcf_inherit :
      Override_flag.t
      -> Class_expr.t
      -> string Location.loc option
      -> t
    val create_pcf_val :
      (Label.t Location.loc * Mutable_flag.t * Class_field_kind.t)
      -> t
    val create_pcf_method :
      (Label.t Location.loc * Private_flag.t * Class_field_kind.t)
      -> t
    val create_pcf_constraint :
      (Core_type.t * Core_type.t)
      -> t
    val create_pcf_initializer :
      Expression.t
      -> t
    val create_pcf_attribute :
      Attribute.t
      -> t
    val create_pcf_extension :
      Extension.t
      -> t
  end

  and Class_field_kind : sig
    type t

    type concrete =
      | Cfk_virtual of Core_type.t
      | Cfk_concrete of Override_flag.t * Expression.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_cfk_virtual :
      Core_type.t
      -> t
    val create_cfk_concrete :
      Override_flag.t
      -> Expression.t
      -> t
  end

  and Class_declaration : sig
    type t

    type concrete = Class_expr.t Class_infos.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Class_expr.t Class_infos.t -> t
  end

  and Module_type : sig
    type t

    type concrete =
      { pmty_desc : Module_type_desc.t
      ; pmty_loc : Location.t
      ; pmty_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pmty_desc:Module_type_desc.t
      -> pmty_loc:Location.t
      -> pmty_attributes:Attributes.t
      -> t
  end

  and Module_type_desc : sig
    type t

    type concrete =
      | Pmty_ident of Longident_loc.t
      | Pmty_signature of Signature.t
      | Pmty_functor of string Location.loc * Module_type.t option * Module_type.t
      | Pmty_with of Module_type.t * With_constraint.t list
      | Pmty_typeof of Module_expr.t
      | Pmty_extension of Extension.t
      | Pmty_alias of Longident_loc.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pmty_ident :
      Longident_loc.t
      -> t
    val create_pmty_signature :
      Signature.t
      -> t
    val create_pmty_functor :
      string Location.loc
      -> Module_type.t option
      -> Module_type.t
      -> t
    val create_pmty_with :
      Module_type.t
      -> With_constraint.t list
      -> t
    val create_pmty_typeof :
      Module_expr.t
      -> t
    val create_pmty_extension :
      Extension.t
      -> t
    val create_pmty_alias :
      Longident_loc.t
      -> t
  end

  and Signature : sig
    type t

    type concrete = Signature_item.t list

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Signature_item.t list -> t
  end

  and Signature_item : sig
    type t

    type concrete =
      { psig_desc : Signature_item_desc.t
      ; psig_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      psig_desc:Signature_item_desc.t
      -> psig_loc:Location.t
      -> t
  end

  and Signature_item_desc : sig
    type t

    type concrete =
      | Psig_value of Value_description.t
      | Psig_type of Rec_flag.t * Type_declaration.t list
      | Psig_typext of Type_extension.t
      | Psig_exception of Extension_constructor.t
      | Psig_module of Module_declaration.t
      | Psig_recmodule of Module_declaration.t list
      | Psig_modtype of Module_type_declaration.t
      | Psig_open of Open_description.t
      | Psig_include of Include_description.t
      | Psig_class of Class_description.t list
      | Psig_class_type of Class_type_declaration.t list
      | Psig_attribute of Attribute.t
      | Psig_extension of Extension.t * Attributes.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_psig_value :
      Value_description.t
      -> t
    val create_psig_type :
      Rec_flag.t
      -> Type_declaration.t list
      -> t
    val create_psig_typext :
      Type_extension.t
      -> t
    val create_psig_exception :
      Extension_constructor.t
      -> t
    val create_psig_module :
      Module_declaration.t
      -> t
    val create_psig_recmodule :
      Module_declaration.t list
      -> t
    val create_psig_modtype :
      Module_type_declaration.t
      -> t
    val create_psig_open :
      Open_description.t
      -> t
    val create_psig_include :
      Include_description.t
      -> t
    val create_psig_class :
      Class_description.t list
      -> t
    val create_psig_class_type :
      Class_type_declaration.t list
      -> t
    val create_psig_attribute :
      Attribute.t
      -> t
    val create_psig_extension :
      Extension.t
      -> Attributes.t
      -> t
  end

  and Module_declaration : sig
    type t

    type concrete =
      { pmd_name : string Location.loc
      ; pmd_type : Module_type.t
      ; pmd_attributes : Attributes.t
      ; pmd_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pmd_name:string Location.loc
      -> pmd_type:Module_type.t
      -> pmd_attributes:Attributes.t
      -> pmd_loc:Location.t
      -> t
  end

  and Module_type_declaration : sig
    type t

    type concrete =
      { pmtd_name : string Location.loc
      ; pmtd_type : Module_type.t option
      ; pmtd_attributes : Attributes.t
      ; pmtd_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pmtd_name:string Location.loc
      -> pmtd_type:Module_type.t option
      -> pmtd_attributes:Attributes.t
      -> pmtd_loc:Location.t
      -> t
  end

  and Open_description : sig
    type t

    type concrete =
      { popen_lid : Longident_loc.t
      ; popen_override : Override_flag.t
      ; popen_loc : Location.t
      ; popen_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      popen_lid:Longident_loc.t
      -> popen_override:Override_flag.t
      -> popen_loc:Location.t
      -> popen_attributes:Attributes.t
      -> t
  end

  and Include_infos : sig
    type 'a t

    type 'a concrete =
      { pincl_mod : 'a
      ; pincl_loc : Location.t
      ; pincl_attributes : Attributes.t
      }

    val of_concrete_module_expr : Module_expr.t concrete -> Module_expr.t t
    val to_concrete_module_expr : Module_expr.t t -> Module_expr.t concrete option

    val create_module_expr :
      pincl_mod:Module_expr.t
      -> pincl_loc:Location.t
      -> pincl_attributes:Attributes.t
      -> Module_expr.t t

    val of_concrete_module_type : Module_type.t concrete -> Module_type.t t
    val to_concrete_module_type : Module_type.t t -> Module_type.t concrete option

    val create_module_type :
      pincl_mod:Module_type.t
      -> pincl_loc:Location.t
      -> pincl_attributes:Attributes.t
      -> Module_type.t t
  end

  and Include_description : sig
    type t

    type concrete = Module_type.t Include_infos.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Module_type.t Include_infos.t -> t
  end

  and Include_declaration : sig
    type t

    type concrete = Module_expr.t Include_infos.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Module_expr.t Include_infos.t -> t
  end

  and With_constraint : sig
    type t

    type concrete =
      | Pwith_type of Longident_loc.t * Type_declaration.t
      | Pwith_module of Longident_loc.t * Longident_loc.t
      | Pwith_typesubst of Longident_loc.t * Type_declaration.t
      | Pwith_modsubst of Longident_loc.t * Longident_loc.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pwith_type :
      Longident_loc.t
      -> Type_declaration.t
      -> t
    val create_pwith_module :
      Longident_loc.t
      -> Longident_loc.t
      -> t
    val create_pwith_typesubst :
      Longident_loc.t
      -> Type_declaration.t
      -> t
    val create_pwith_modsubst :
      Longident_loc.t
      -> Longident_loc.t
      -> t
  end

  and Module_expr : sig
    type t

    type concrete =
      { pmod_desc : Module_expr_desc.t
      ; pmod_loc : Location.t
      ; pmod_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pmod_desc:Module_expr_desc.t
      -> pmod_loc:Location.t
      -> pmod_attributes:Attributes.t
      -> t
  end

  and Module_expr_desc : sig
    type t

    type concrete =
      | Pmod_ident of Longident_loc.t
      | Pmod_structure of Structure.t
      | Pmod_functor of string Location.loc * Module_type.t option * Module_expr.t
      | Pmod_apply of Module_expr.t * Module_expr.t
      | Pmod_constraint of Module_expr.t * Module_type.t
      | Pmod_unpack of Expression.t
      | Pmod_extension of Extension.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pmod_ident :
      Longident_loc.t
      -> t
    val create_pmod_structure :
      Structure.t
      -> t
    val create_pmod_functor :
      string Location.loc
      -> Module_type.t option
      -> Module_expr.t
      -> t
    val create_pmod_apply :
      Module_expr.t
      -> Module_expr.t
      -> t
    val create_pmod_constraint :
      Module_expr.t
      -> Module_type.t
      -> t
    val create_pmod_unpack :
      Expression.t
      -> t
    val create_pmod_extension :
      Extension.t
      -> t
  end

  and Structure : sig
    type t

    type concrete = Structure_item.t list

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Structure_item.t list -> t
  end

  and Structure_item : sig
    type t

    type concrete =
      { pstr_desc : Structure_item_desc.t
      ; pstr_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pstr_desc:Structure_item_desc.t
      -> pstr_loc:Location.t
      -> t
  end

  and Structure_item_desc : sig
    type t

    type concrete =
      | Pstr_eval of Expression.t * Attributes.t
      | Pstr_value of Rec_flag.t * Value_binding.t list
      | Pstr_primitive of Value_description.t
      | Pstr_type of Rec_flag.t * Type_declaration.t list
      | Pstr_typext of Type_extension.t
      | Pstr_exception of Extension_constructor.t
      | Pstr_module of Module_binding.t
      | Pstr_recmodule of Module_binding.t list
      | Pstr_modtype of Module_type_declaration.t
      | Pstr_open of Open_description.t
      | Pstr_class of Class_declaration.t list
      | Pstr_class_type of Class_type_declaration.t list
      | Pstr_include of Include_declaration.t
      | Pstr_attribute of Attribute.t
      | Pstr_extension of Extension.t * Attributes.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pstr_eval :
      Expression.t
      -> Attributes.t
      -> t
    val create_pstr_value :
      Rec_flag.t
      -> Value_binding.t list
      -> t
    val create_pstr_primitive :
      Value_description.t
      -> t
    val create_pstr_type :
      Rec_flag.t
      -> Type_declaration.t list
      -> t
    val create_pstr_typext :
      Type_extension.t
      -> t
    val create_pstr_exception :
      Extension_constructor.t
      -> t
    val create_pstr_module :
      Module_binding.t
      -> t
    val create_pstr_recmodule :
      Module_binding.t list
      -> t
    val create_pstr_modtype :
      Module_type_declaration.t
      -> t
    val create_pstr_open :
      Open_description.t
      -> t
    val create_pstr_class :
      Class_declaration.t list
      -> t
    val create_pstr_class_type :
      Class_type_declaration.t list
      -> t
    val create_pstr_include :
      Include_declaration.t
      -> t
    val create_pstr_attribute :
      Attribute.t
      -> t
    val create_pstr_extension :
      Extension.t
      -> Attributes.t
      -> t
  end

  and Value_binding : sig
    type t

    type concrete =
      { pvb_pat : Pattern.t
      ; pvb_expr : Expression.t
      ; pvb_attributes : Attributes.t
      ; pvb_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pvb_pat:Pattern.t
      -> pvb_expr:Expression.t
      -> pvb_attributes:Attributes.t
      -> pvb_loc:Location.t
      -> t
  end

  and Module_binding : sig
    type t

    type concrete =
      { pmb_name : string Location.loc
      ; pmb_expr : Module_expr.t
      ; pmb_attributes : Attributes.t
      ; pmb_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pmb_name:string Location.loc
      -> pmb_expr:Module_expr.t
      -> pmb_attributes:Attributes.t
      -> pmb_loc:Location.t
      -> t
  end

  and Toplevel_phrase : sig
    type t

    type concrete =
      | Ptop_def of Structure.t
      | Ptop_dir of string * Directive_argument.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_ptop_def :
      Structure.t
      -> t
    val create_ptop_dir :
      string
      -> Directive_argument.t
      -> t
  end

  and Directive_argument : sig
    type t

    type concrete =
      | Pdir_none
      | Pdir_string of string
      | Pdir_int of string * char option
      | Pdir_ident of Longident.t
      | Pdir_bool of bool

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pdir_none : t
    val create_pdir_string :
      string
      -> t
    val create_pdir_int :
      string
      -> char option
      -> t
    val create_pdir_ident :
      Longident.t
      -> t
    val create_pdir_bool :
      bool
      -> t
  end
end

module V4_06 : sig
  module rec Longident : sig
    type t

    type concrete =
      | Lident of string
      | Ldot of Longident.t * string
      | Lapply of Longident.t * Longident.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_lident :
      string
      -> t
    val create_ldot :
      Longident.t
      -> string
      -> t
    val create_lapply :
      Longident.t
      -> Longident.t
      -> t
  end

  and Longident_loc : sig
    type t

    type concrete = Longident.t Location.loc

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Longident.t Location.loc -> t
  end

  and Rec_flag : sig
    type t

    type concrete =
      | Nonrecursive
      | Recursive

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_nonrecursive : t
    val create_recursive : t
  end

  and Direction_flag : sig
    type t

    type concrete =
      | Upto
      | Downto

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_upto : t
    val create_downto : t
  end

  and Private_flag : sig
    type t

    type concrete =
      | Private
      | Public

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_private : t
    val create_public : t
  end

  and Mutable_flag : sig
    type t

    type concrete =
      | Immutable
      | Mutable

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_immutable : t
    val create_mutable : t
  end

  and Virtual_flag : sig
    type t

    type concrete =
      | Virtual
      | Concrete

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_virtual : t
    val create_concrete : t
  end

  and Override_flag : sig
    type t

    type concrete =
      | Override
      | Fresh

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_override : t
    val create_fresh : t
  end

  and Closed_flag : sig
    type t

    type concrete =
      | Closed
      | Open

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_closed : t
    val create_open : t
  end

  and Label : sig
    type t

    type concrete = string

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : string -> t
  end

  and Arg_label : sig
    type t

    type concrete =
      | Nolabel
      | Labelled of string
      | Optional of string

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_nolabel : t
    val create_labelled :
      string
      -> t
    val create_optional :
      string
      -> t
  end

  and Variance : sig
    type t

    type concrete =
      | Covariant
      | Contravariant
      | Invariant

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_covariant : t
    val create_contravariant : t
    val create_invariant : t
  end

  and Constant : sig
    type t

    type concrete =
      | Pconst_integer of string * char option
      | Pconst_char of char
      | Pconst_string of string * string option
      | Pconst_float of string * char option

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pconst_integer :
      string
      -> char option
      -> t
    val create_pconst_char :
      char
      -> t
    val create_pconst_string :
      string
      -> string option
      -> t
    val create_pconst_float :
      string
      -> char option
      -> t
  end

  and Attribute : sig
    type t

    type concrete = (string Location.loc * Payload.t)

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : (string Location.loc * Payload.t) -> t
  end

  and Extension : sig
    type t

    type concrete = (string Location.loc * Payload.t)

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : (string Location.loc * Payload.t) -> t
  end

  and Attributes : sig
    type t

    type concrete = Attribute.t list

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Attribute.t list -> t
  end

  and Payload : sig
    type t

    type concrete =
      | PStr of Structure.t
      | PSig of Signature.t
      | PTyp of Core_type.t
      | PPat of Pattern.t * Expression.t option

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pstr :
      Structure.t
      -> t
    val create_psig :
      Signature.t
      -> t
    val create_ptyp :
      Core_type.t
      -> t
    val create_ppat :
      Pattern.t
      -> Expression.t option
      -> t
  end

  and Core_type : sig
    type t

    type concrete =
      { ptyp_desc : Core_type_desc.t
      ; ptyp_loc : Location.t
      ; ptyp_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      ptyp_desc:Core_type_desc.t
      -> ptyp_loc:Location.t
      -> ptyp_attributes:Attributes.t
      -> t
  end

  and Core_type_desc : sig
    type t

    type concrete =
      | Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of Arg_label.t * Core_type.t * Core_type.t
      | Ptyp_tuple of Core_type.t list
      | Ptyp_constr of Longident_loc.t * Core_type.t list
      | Ptyp_object of Object_field.t list * Closed_flag.t
      | Ptyp_class of Longident_loc.t * Core_type.t list
      | Ptyp_alias of Core_type.t * string
      | Ptyp_variant of Row_field.t list * Closed_flag.t * Label.t list option
      | Ptyp_poly of string Location.loc list * Core_type.t
      | Ptyp_package of Package_type.t
      | Ptyp_extension of Extension.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_ptyp_any : t
    val create_ptyp_var :
      string
      -> t
    val create_ptyp_arrow :
      Arg_label.t
      -> Core_type.t
      -> Core_type.t
      -> t
    val create_ptyp_tuple :
      Core_type.t list
      -> t
    val create_ptyp_constr :
      Longident_loc.t
      -> Core_type.t list
      -> t
    val create_ptyp_object :
      Object_field.t list
      -> Closed_flag.t
      -> t
    val create_ptyp_class :
      Longident_loc.t
      -> Core_type.t list
      -> t
    val create_ptyp_alias :
      Core_type.t
      -> string
      -> t
    val create_ptyp_variant :
      Row_field.t list
      -> Closed_flag.t
      -> Label.t list option
      -> t
    val create_ptyp_poly :
      string Location.loc list
      -> Core_type.t
      -> t
    val create_ptyp_package :
      Package_type.t
      -> t
    val create_ptyp_extension :
      Extension.t
      -> t
  end

  and Package_type : sig
    type t

    type concrete = (Longident_loc.t * (Longident_loc.t * Core_type.t) list)

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : (Longident_loc.t * (Longident_loc.t * Core_type.t) list) -> t
  end

  and Row_field : sig
    type t

    type concrete =
      | Rtag of Label.t Location.loc * Attributes.t * bool * Core_type.t list
      | Rinherit of Core_type.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_rtag :
      Label.t Location.loc
      -> Attributes.t
      -> bool
      -> Core_type.t list
      -> t
    val create_rinherit :
      Core_type.t
      -> t
  end

  and Object_field : sig
    type t

    type concrete =
      | Otag of Label.t Location.loc * Attributes.t * Core_type.t
      | Oinherit of Core_type.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_otag :
      Label.t Location.loc
      -> Attributes.t
      -> Core_type.t
      -> t
    val create_oinherit :
      Core_type.t
      -> t
  end

  and Pattern : sig
    type t

    type concrete =
      { ppat_desc : Pattern_desc.t
      ; ppat_loc : Location.t
      ; ppat_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      ppat_desc:Pattern_desc.t
      -> ppat_loc:Location.t
      -> ppat_attributes:Attributes.t
      -> t
  end

  and Pattern_desc : sig
    type t

    type concrete =
      | Ppat_any
      | Ppat_var of string Location.loc
      | Ppat_alias of Pattern.t * string Location.loc
      | Ppat_constant of Constant.t
      | Ppat_interval of Constant.t * Constant.t
      | Ppat_tuple of Pattern.t list
      | Ppat_construct of Longident_loc.t * Pattern.t option
      | Ppat_variant of Label.t * Pattern.t option
      | Ppat_record of (Longident_loc.t * Pattern.t) list * Closed_flag.t
      | Ppat_array of Pattern.t list
      | Ppat_or of Pattern.t * Pattern.t
      | Ppat_constraint of Pattern.t * Core_type.t
      | Ppat_type of Longident_loc.t
      | Ppat_lazy of Pattern.t
      | Ppat_unpack of string Location.loc
      | Ppat_exception of Pattern.t
      | Ppat_extension of Extension.t
      | Ppat_open of Longident_loc.t * Pattern.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_ppat_any : t
    val create_ppat_var :
      string Location.loc
      -> t
    val create_ppat_alias :
      Pattern.t
      -> string Location.loc
      -> t
    val create_ppat_constant :
      Constant.t
      -> t
    val create_ppat_interval :
      Constant.t
      -> Constant.t
      -> t
    val create_ppat_tuple :
      Pattern.t list
      -> t
    val create_ppat_construct :
      Longident_loc.t
      -> Pattern.t option
      -> t
    val create_ppat_variant :
      Label.t
      -> Pattern.t option
      -> t
    val create_ppat_record :
      (Longident_loc.t * Pattern.t) list
      -> Closed_flag.t
      -> t
    val create_ppat_array :
      Pattern.t list
      -> t
    val create_ppat_or :
      Pattern.t
      -> Pattern.t
      -> t
    val create_ppat_constraint :
      Pattern.t
      -> Core_type.t
      -> t
    val create_ppat_type :
      Longident_loc.t
      -> t
    val create_ppat_lazy :
      Pattern.t
      -> t
    val create_ppat_unpack :
      string Location.loc
      -> t
    val create_ppat_exception :
      Pattern.t
      -> t
    val create_ppat_extension :
      Extension.t
      -> t
    val create_ppat_open :
      Longident_loc.t
      -> Pattern.t
      -> t
  end

  and Expression : sig
    type t

    type concrete =
      { pexp_desc : Expression_desc.t
      ; pexp_loc : Location.t
      ; pexp_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pexp_desc:Expression_desc.t
      -> pexp_loc:Location.t
      -> pexp_attributes:Attributes.t
      -> t
  end

  and Expression_desc : sig
    type t

    type concrete =
      | Pexp_ident of Longident_loc.t
      | Pexp_constant of Constant.t
      | Pexp_let of Rec_flag.t * Value_binding.t list * Expression.t
      | Pexp_function of Case.t list
      | Pexp_fun of Arg_label.t * Expression.t option * Pattern.t * Expression.t
      | Pexp_apply of Expression.t * (Arg_label.t * Expression.t) list
      | Pexp_match of Expression.t * Case.t list
      | Pexp_try of Expression.t * Case.t list
      | Pexp_tuple of Expression.t list
      | Pexp_construct of Longident_loc.t * Expression.t option
      | Pexp_variant of Label.t * Expression.t option
      | Pexp_record of (Longident_loc.t * Expression.t) list * Expression.t option
      | Pexp_field of Expression.t * Longident_loc.t
      | Pexp_setfield of Expression.t * Longident_loc.t * Expression.t
      | Pexp_array of Expression.t list
      | Pexp_ifthenelse of Expression.t * Expression.t * Expression.t option
      | Pexp_sequence of Expression.t * Expression.t
      | Pexp_while of Expression.t * Expression.t
      | Pexp_for of Pattern.t * Expression.t * Expression.t * Direction_flag.t * Expression.t
      | Pexp_constraint of Expression.t * Core_type.t
      | Pexp_coerce of Expression.t * Core_type.t option * Core_type.t
      | Pexp_send of Expression.t * Label.t Location.loc
      | Pexp_new of Longident_loc.t
      | Pexp_setinstvar of Label.t Location.loc * Expression.t
      | Pexp_override of (Label.t Location.loc * Expression.t) list
      | Pexp_letmodule of string Location.loc * Module_expr.t * Expression.t
      | Pexp_letexception of Extension_constructor.t * Expression.t
      | Pexp_assert of Expression.t
      | Pexp_lazy of Expression.t
      | Pexp_poly of Expression.t * Core_type.t option
      | Pexp_object of Class_structure.t
      | Pexp_newtype of string Location.loc * Expression.t
      | Pexp_pack of Module_expr.t
      | Pexp_open of Override_flag.t * Longident_loc.t * Expression.t
      | Pexp_extension of Extension.t
      | Pexp_unreachable

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pexp_ident :
      Longident_loc.t
      -> t
    val create_pexp_constant :
      Constant.t
      -> t
    val create_pexp_let :
      Rec_flag.t
      -> Value_binding.t list
      -> Expression.t
      -> t
    val create_pexp_function :
      Case.t list
      -> t
    val create_pexp_fun :
      Arg_label.t
      -> Expression.t option
      -> Pattern.t
      -> Expression.t
      -> t
    val create_pexp_apply :
      Expression.t
      -> (Arg_label.t * Expression.t) list
      -> t
    val create_pexp_match :
      Expression.t
      -> Case.t list
      -> t
    val create_pexp_try :
      Expression.t
      -> Case.t list
      -> t
    val create_pexp_tuple :
      Expression.t list
      -> t
    val create_pexp_construct :
      Longident_loc.t
      -> Expression.t option
      -> t
    val create_pexp_variant :
      Label.t
      -> Expression.t option
      -> t
    val create_pexp_record :
      (Longident_loc.t * Expression.t) list
      -> Expression.t option
      -> t
    val create_pexp_field :
      Expression.t
      -> Longident_loc.t
      -> t
    val create_pexp_setfield :
      Expression.t
      -> Longident_loc.t
      -> Expression.t
      -> t
    val create_pexp_array :
      Expression.t list
      -> t
    val create_pexp_ifthenelse :
      Expression.t
      -> Expression.t
      -> Expression.t option
      -> t
    val create_pexp_sequence :
      Expression.t
      -> Expression.t
      -> t
    val create_pexp_while :
      Expression.t
      -> Expression.t
      -> t
    val create_pexp_for :
      Pattern.t
      -> Expression.t
      -> Expression.t
      -> Direction_flag.t
      -> Expression.t
      -> t
    val create_pexp_constraint :
      Expression.t
      -> Core_type.t
      -> t
    val create_pexp_coerce :
      Expression.t
      -> Core_type.t option
      -> Core_type.t
      -> t
    val create_pexp_send :
      Expression.t
      -> Label.t Location.loc
      -> t
    val create_pexp_new :
      Longident_loc.t
      -> t
    val create_pexp_setinstvar :
      Label.t Location.loc
      -> Expression.t
      -> t
    val create_pexp_override :
      (Label.t Location.loc * Expression.t) list
      -> t
    val create_pexp_letmodule :
      string Location.loc
      -> Module_expr.t
      -> Expression.t
      -> t
    val create_pexp_letexception :
      Extension_constructor.t
      -> Expression.t
      -> t
    val create_pexp_assert :
      Expression.t
      -> t
    val create_pexp_lazy :
      Expression.t
      -> t
    val create_pexp_poly :
      Expression.t
      -> Core_type.t option
      -> t
    val create_pexp_object :
      Class_structure.t
      -> t
    val create_pexp_newtype :
      string Location.loc
      -> Expression.t
      -> t
    val create_pexp_pack :
      Module_expr.t
      -> t
    val create_pexp_open :
      Override_flag.t
      -> Longident_loc.t
      -> Expression.t
      -> t
    val create_pexp_extension :
      Extension.t
      -> t
    val create_pexp_unreachable : t
  end

  and Case : sig
    type t

    type concrete =
      { pc_lhs : Pattern.t
      ; pc_guard : Expression.t option
      ; pc_rhs : Expression.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pc_lhs:Pattern.t
      -> pc_guard:Expression.t option
      -> pc_rhs:Expression.t
      -> t
  end

  and Value_description : sig
    type t

    type concrete =
      { pval_name : string Location.loc
      ; pval_type : Core_type.t
      ; pval_prim : string list
      ; pval_attributes : Attributes.t
      ; pval_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pval_name:string Location.loc
      -> pval_type:Core_type.t
      -> pval_prim:string list
      -> pval_attributes:Attributes.t
      -> pval_loc:Location.t
      -> t
  end

  and Type_declaration : sig
    type t

    type concrete =
      { ptype_name : string Location.loc
      ; ptype_params : (Core_type.t * Variance.t) list
      ; ptype_cstrs : (Core_type.t * Core_type.t * Location.t) list
      ; ptype_kind : Type_kind.t
      ; ptype_private : Private_flag.t
      ; ptype_manifest : Core_type.t option
      ; ptype_attributes : Attributes.t
      ; ptype_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      ptype_name:string Location.loc
      -> ptype_params:(Core_type.t * Variance.t) list
      -> ptype_cstrs:(Core_type.t * Core_type.t * Location.t) list
      -> ptype_kind:Type_kind.t
      -> ptype_private:Private_flag.t
      -> ptype_manifest:Core_type.t option
      -> ptype_attributes:Attributes.t
      -> ptype_loc:Location.t
      -> t
  end

  and Type_kind : sig
    type t

    type concrete =
      | Ptype_abstract
      | Ptype_variant of Constructor_declaration.t list
      | Ptype_record of Label_declaration.t list
      | Ptype_open

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_ptype_abstract : t
    val create_ptype_variant :
      Constructor_declaration.t list
      -> t
    val create_ptype_record :
      Label_declaration.t list
      -> t
    val create_ptype_open : t
  end

  and Label_declaration : sig
    type t

    type concrete =
      { pld_name : string Location.loc
      ; pld_mutable : Mutable_flag.t
      ; pld_type : Core_type.t
      ; pld_loc : Location.t
      ; pld_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pld_name:string Location.loc
      -> pld_mutable:Mutable_flag.t
      -> pld_type:Core_type.t
      -> pld_loc:Location.t
      -> pld_attributes:Attributes.t
      -> t
  end

  and Constructor_declaration : sig
    type t

    type concrete =
      { pcd_name : string Location.loc
      ; pcd_args : Constructor_arguments.t
      ; pcd_res : Core_type.t option
      ; pcd_loc : Location.t
      ; pcd_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pcd_name:string Location.loc
      -> pcd_args:Constructor_arguments.t
      -> pcd_res:Core_type.t option
      -> pcd_loc:Location.t
      -> pcd_attributes:Attributes.t
      -> t
  end

  and Constructor_arguments : sig
    type t

    type concrete =
      | Pcstr_tuple of Core_type.t list
      | Pcstr_record of Label_declaration.t list

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pcstr_tuple :
      Core_type.t list
      -> t
    val create_pcstr_record :
      Label_declaration.t list
      -> t
  end

  and Type_extension : sig
    type t

    type concrete =
      { ptyext_path : Longident_loc.t
      ; ptyext_params : (Core_type.t * Variance.t) list
      ; ptyext_constructors : Extension_constructor.t list
      ; ptyext_private : Private_flag.t
      ; ptyext_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      ptyext_path:Longident_loc.t
      -> ptyext_params:(Core_type.t * Variance.t) list
      -> ptyext_constructors:Extension_constructor.t list
      -> ptyext_private:Private_flag.t
      -> ptyext_attributes:Attributes.t
      -> t
  end

  and Extension_constructor : sig
    type t

    type concrete =
      { pext_name : string Location.loc
      ; pext_kind : Extension_constructor_kind.t
      ; pext_loc : Location.t
      ; pext_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pext_name:string Location.loc
      -> pext_kind:Extension_constructor_kind.t
      -> pext_loc:Location.t
      -> pext_attributes:Attributes.t
      -> t
  end

  and Extension_constructor_kind : sig
    type t

    type concrete =
      | Pext_decl of Constructor_arguments.t * Core_type.t option
      | Pext_rebind of Longident_loc.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pext_decl :
      Constructor_arguments.t
      -> Core_type.t option
      -> t
    val create_pext_rebind :
      Longident_loc.t
      -> t
  end

  and Class_type : sig
    type t

    type concrete =
      { pcty_desc : Class_type_desc.t
      ; pcty_loc : Location.t
      ; pcty_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pcty_desc:Class_type_desc.t
      -> pcty_loc:Location.t
      -> pcty_attributes:Attributes.t
      -> t
  end

  and Class_type_desc : sig
    type t

    type concrete =
      | Pcty_constr of Longident_loc.t * Core_type.t list
      | Pcty_signature of Class_signature.t
      | Pcty_arrow of Arg_label.t * Core_type.t * Class_type.t
      | Pcty_extension of Extension.t
      | Pcty_open of Override_flag.t * Longident_loc.t * Class_type.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pcty_constr :
      Longident_loc.t
      -> Core_type.t list
      -> t
    val create_pcty_signature :
      Class_signature.t
      -> t
    val create_pcty_arrow :
      Arg_label.t
      -> Core_type.t
      -> Class_type.t
      -> t
    val create_pcty_extension :
      Extension.t
      -> t
    val create_pcty_open :
      Override_flag.t
      -> Longident_loc.t
      -> Class_type.t
      -> t
  end

  and Class_signature : sig
    type t

    type concrete =
      { pcsig_self : Core_type.t
      ; pcsig_fields : Class_type_field.t list
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pcsig_self:Core_type.t
      -> pcsig_fields:Class_type_field.t list
      -> t
  end

  and Class_type_field : sig
    type t

    type concrete =
      { pctf_desc : Class_type_field_desc.t
      ; pctf_loc : Location.t
      ; pctf_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pctf_desc:Class_type_field_desc.t
      -> pctf_loc:Location.t
      -> pctf_attributes:Attributes.t
      -> t
  end

  and Class_type_field_desc : sig
    type t

    type concrete =
      | Pctf_inherit of Class_type.t
      | Pctf_val of (Label.t Location.loc * Mutable_flag.t * Virtual_flag.t * Core_type.t)
      | Pctf_method of (Label.t Location.loc * Private_flag.t * Virtual_flag.t * Core_type.t)
      | Pctf_constraint of (Core_type.t * Core_type.t)
      | Pctf_attribute of Attribute.t
      | Pctf_extension of Extension.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pctf_inherit :
      Class_type.t
      -> t
    val create_pctf_val :
      (Label.t Location.loc * Mutable_flag.t * Virtual_flag.t * Core_type.t)
      -> t
    val create_pctf_method :
      (Label.t Location.loc * Private_flag.t * Virtual_flag.t * Core_type.t)
      -> t
    val create_pctf_constraint :
      (Core_type.t * Core_type.t)
      -> t
    val create_pctf_attribute :
      Attribute.t
      -> t
    val create_pctf_extension :
      Extension.t
      -> t
  end

  and Class_infos : sig
    type 'a t

    type 'a concrete =
      { pci_virt : Virtual_flag.t
      ; pci_params : (Core_type.t * Variance.t) list
      ; pci_name : string Location.loc
      ; pci_expr : 'a
      ; pci_loc : Location.t
      ; pci_attributes : Attributes.t
      }

    val of_concrete_class_expr : Class_expr.t concrete -> Class_expr.t t
    val to_concrete_class_expr : Class_expr.t t -> Class_expr.t concrete option

    val create_class_expr :
      pci_virt:Virtual_flag.t
      -> pci_params:(Core_type.t * Variance.t) list
      -> pci_name:string Location.loc
      -> pci_expr:Class_expr.t
      -> pci_loc:Location.t
      -> pci_attributes:Attributes.t
      -> Class_expr.t t

    val of_concrete_class_type : Class_type.t concrete -> Class_type.t t
    val to_concrete_class_type : Class_type.t t -> Class_type.t concrete option

    val create_class_type :
      pci_virt:Virtual_flag.t
      -> pci_params:(Core_type.t * Variance.t) list
      -> pci_name:string Location.loc
      -> pci_expr:Class_type.t
      -> pci_loc:Location.t
      -> pci_attributes:Attributes.t
      -> Class_type.t t
  end

  and Class_description : sig
    type t

    type concrete = Class_type.t Class_infos.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Class_type.t Class_infos.t -> t
  end

  and Class_type_declaration : sig
    type t

    type concrete = Class_type.t Class_infos.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Class_type.t Class_infos.t -> t
  end

  and Class_expr : sig
    type t

    type concrete =
      { pcl_desc : Class_expr_desc.t
      ; pcl_loc : Location.t
      ; pcl_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pcl_desc:Class_expr_desc.t
      -> pcl_loc:Location.t
      -> pcl_attributes:Attributes.t
      -> t
  end

  and Class_expr_desc : sig
    type t

    type concrete =
      | Pcl_constr of Longident_loc.t * Core_type.t list
      | Pcl_structure of Class_structure.t
      | Pcl_fun of Arg_label.t * Expression.t option * Pattern.t * Class_expr.t
      | Pcl_apply of Class_expr.t * (Arg_label.t * Expression.t) list
      | Pcl_let of Rec_flag.t * Value_binding.t list * Class_expr.t
      | Pcl_constraint of Class_expr.t * Class_type.t
      | Pcl_extension of Extension.t
      | Pcl_open of Override_flag.t * Longident_loc.t * Class_expr.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pcl_constr :
      Longident_loc.t
      -> Core_type.t list
      -> t
    val create_pcl_structure :
      Class_structure.t
      -> t
    val create_pcl_fun :
      Arg_label.t
      -> Expression.t option
      -> Pattern.t
      -> Class_expr.t
      -> t
    val create_pcl_apply :
      Class_expr.t
      -> (Arg_label.t * Expression.t) list
      -> t
    val create_pcl_let :
      Rec_flag.t
      -> Value_binding.t list
      -> Class_expr.t
      -> t
    val create_pcl_constraint :
      Class_expr.t
      -> Class_type.t
      -> t
    val create_pcl_extension :
      Extension.t
      -> t
    val create_pcl_open :
      Override_flag.t
      -> Longident_loc.t
      -> Class_expr.t
      -> t
  end

  and Class_structure : sig
    type t

    type concrete =
      { pcstr_self : Pattern.t
      ; pcstr_fields : Class_field.t list
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pcstr_self:Pattern.t
      -> pcstr_fields:Class_field.t list
      -> t
  end

  and Class_field : sig
    type t

    type concrete =
      { pcf_desc : Class_field_desc.t
      ; pcf_loc : Location.t
      ; pcf_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pcf_desc:Class_field_desc.t
      -> pcf_loc:Location.t
      -> pcf_attributes:Attributes.t
      -> t
  end

  and Class_field_desc : sig
    type t

    type concrete =
      | Pcf_inherit of Override_flag.t * Class_expr.t * string Location.loc option
      | Pcf_val of (Label.t Location.loc * Mutable_flag.t * Class_field_kind.t)
      | Pcf_method of (Label.t Location.loc * Private_flag.t * Class_field_kind.t)
      | Pcf_constraint of (Core_type.t * Core_type.t)
      | Pcf_initializer of Expression.t
      | Pcf_attribute of Attribute.t
      | Pcf_extension of Extension.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pcf_inherit :
      Override_flag.t
      -> Class_expr.t
      -> string Location.loc option
      -> t
    val create_pcf_val :
      (Label.t Location.loc * Mutable_flag.t * Class_field_kind.t)
      -> t
    val create_pcf_method :
      (Label.t Location.loc * Private_flag.t * Class_field_kind.t)
      -> t
    val create_pcf_constraint :
      (Core_type.t * Core_type.t)
      -> t
    val create_pcf_initializer :
      Expression.t
      -> t
    val create_pcf_attribute :
      Attribute.t
      -> t
    val create_pcf_extension :
      Extension.t
      -> t
  end

  and Class_field_kind : sig
    type t

    type concrete =
      | Cfk_virtual of Core_type.t
      | Cfk_concrete of Override_flag.t * Expression.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_cfk_virtual :
      Core_type.t
      -> t
    val create_cfk_concrete :
      Override_flag.t
      -> Expression.t
      -> t
  end

  and Class_declaration : sig
    type t

    type concrete = Class_expr.t Class_infos.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Class_expr.t Class_infos.t -> t
  end

  and Module_type : sig
    type t

    type concrete =
      { pmty_desc : Module_type_desc.t
      ; pmty_loc : Location.t
      ; pmty_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pmty_desc:Module_type_desc.t
      -> pmty_loc:Location.t
      -> pmty_attributes:Attributes.t
      -> t
  end

  and Module_type_desc : sig
    type t

    type concrete =
      | Pmty_ident of Longident_loc.t
      | Pmty_signature of Signature.t
      | Pmty_functor of string Location.loc * Module_type.t option * Module_type.t
      | Pmty_with of Module_type.t * With_constraint.t list
      | Pmty_typeof of Module_expr.t
      | Pmty_extension of Extension.t
      | Pmty_alias of Longident_loc.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pmty_ident :
      Longident_loc.t
      -> t
    val create_pmty_signature :
      Signature.t
      -> t
    val create_pmty_functor :
      string Location.loc
      -> Module_type.t option
      -> Module_type.t
      -> t
    val create_pmty_with :
      Module_type.t
      -> With_constraint.t list
      -> t
    val create_pmty_typeof :
      Module_expr.t
      -> t
    val create_pmty_extension :
      Extension.t
      -> t
    val create_pmty_alias :
      Longident_loc.t
      -> t
  end

  and Signature : sig
    type t

    type concrete = Signature_item.t list

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Signature_item.t list -> t
  end

  and Signature_item : sig
    type t

    type concrete =
      { psig_desc : Signature_item_desc.t
      ; psig_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      psig_desc:Signature_item_desc.t
      -> psig_loc:Location.t
      -> t
  end

  and Signature_item_desc : sig
    type t

    type concrete =
      | Psig_value of Value_description.t
      | Psig_type of Rec_flag.t * Type_declaration.t list
      | Psig_typext of Type_extension.t
      | Psig_exception of Extension_constructor.t
      | Psig_module of Module_declaration.t
      | Psig_recmodule of Module_declaration.t list
      | Psig_modtype of Module_type_declaration.t
      | Psig_open of Open_description.t
      | Psig_include of Include_description.t
      | Psig_class of Class_description.t list
      | Psig_class_type of Class_type_declaration.t list
      | Psig_attribute of Attribute.t
      | Psig_extension of Extension.t * Attributes.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_psig_value :
      Value_description.t
      -> t
    val create_psig_type :
      Rec_flag.t
      -> Type_declaration.t list
      -> t
    val create_psig_typext :
      Type_extension.t
      -> t
    val create_psig_exception :
      Extension_constructor.t
      -> t
    val create_psig_module :
      Module_declaration.t
      -> t
    val create_psig_recmodule :
      Module_declaration.t list
      -> t
    val create_psig_modtype :
      Module_type_declaration.t
      -> t
    val create_psig_open :
      Open_description.t
      -> t
    val create_psig_include :
      Include_description.t
      -> t
    val create_psig_class :
      Class_description.t list
      -> t
    val create_psig_class_type :
      Class_type_declaration.t list
      -> t
    val create_psig_attribute :
      Attribute.t
      -> t
    val create_psig_extension :
      Extension.t
      -> Attributes.t
      -> t
  end

  and Module_declaration : sig
    type t

    type concrete =
      { pmd_name : string Location.loc
      ; pmd_type : Module_type.t
      ; pmd_attributes : Attributes.t
      ; pmd_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pmd_name:string Location.loc
      -> pmd_type:Module_type.t
      -> pmd_attributes:Attributes.t
      -> pmd_loc:Location.t
      -> t
  end

  and Module_type_declaration : sig
    type t

    type concrete =
      { pmtd_name : string Location.loc
      ; pmtd_type : Module_type.t option
      ; pmtd_attributes : Attributes.t
      ; pmtd_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pmtd_name:string Location.loc
      -> pmtd_type:Module_type.t option
      -> pmtd_attributes:Attributes.t
      -> pmtd_loc:Location.t
      -> t
  end

  and Open_description : sig
    type t

    type concrete =
      { popen_lid : Longident_loc.t
      ; popen_override : Override_flag.t
      ; popen_loc : Location.t
      ; popen_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      popen_lid:Longident_loc.t
      -> popen_override:Override_flag.t
      -> popen_loc:Location.t
      -> popen_attributes:Attributes.t
      -> t
  end

  and Include_infos : sig
    type 'a t

    type 'a concrete =
      { pincl_mod : 'a
      ; pincl_loc : Location.t
      ; pincl_attributes : Attributes.t
      }

    val of_concrete_module_expr : Module_expr.t concrete -> Module_expr.t t
    val to_concrete_module_expr : Module_expr.t t -> Module_expr.t concrete option

    val create_module_expr :
      pincl_mod:Module_expr.t
      -> pincl_loc:Location.t
      -> pincl_attributes:Attributes.t
      -> Module_expr.t t

    val of_concrete_module_type : Module_type.t concrete -> Module_type.t t
    val to_concrete_module_type : Module_type.t t -> Module_type.t concrete option

    val create_module_type :
      pincl_mod:Module_type.t
      -> pincl_loc:Location.t
      -> pincl_attributes:Attributes.t
      -> Module_type.t t
  end

  and Include_description : sig
    type t

    type concrete = Module_type.t Include_infos.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Module_type.t Include_infos.t -> t
  end

  and Include_declaration : sig
    type t

    type concrete = Module_expr.t Include_infos.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Module_expr.t Include_infos.t -> t
  end

  and With_constraint : sig
    type t

    type concrete =
      | Pwith_type of Longident_loc.t * Type_declaration.t
      | Pwith_module of Longident_loc.t * Longident_loc.t
      | Pwith_typesubst of Longident_loc.t * Type_declaration.t
      | Pwith_modsubst of Longident_loc.t * Longident_loc.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pwith_type :
      Longident_loc.t
      -> Type_declaration.t
      -> t
    val create_pwith_module :
      Longident_loc.t
      -> Longident_loc.t
      -> t
    val create_pwith_typesubst :
      Longident_loc.t
      -> Type_declaration.t
      -> t
    val create_pwith_modsubst :
      Longident_loc.t
      -> Longident_loc.t
      -> t
  end

  and Module_expr : sig
    type t

    type concrete =
      { pmod_desc : Module_expr_desc.t
      ; pmod_loc : Location.t
      ; pmod_attributes : Attributes.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pmod_desc:Module_expr_desc.t
      -> pmod_loc:Location.t
      -> pmod_attributes:Attributes.t
      -> t
  end

  and Module_expr_desc : sig
    type t

    type concrete =
      | Pmod_ident of Longident_loc.t
      | Pmod_structure of Structure.t
      | Pmod_functor of string Location.loc * Module_type.t option * Module_expr.t
      | Pmod_apply of Module_expr.t * Module_expr.t
      | Pmod_constraint of Module_expr.t * Module_type.t
      | Pmod_unpack of Expression.t
      | Pmod_extension of Extension.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pmod_ident :
      Longident_loc.t
      -> t
    val create_pmod_structure :
      Structure.t
      -> t
    val create_pmod_functor :
      string Location.loc
      -> Module_type.t option
      -> Module_expr.t
      -> t
    val create_pmod_apply :
      Module_expr.t
      -> Module_expr.t
      -> t
    val create_pmod_constraint :
      Module_expr.t
      -> Module_type.t
      -> t
    val create_pmod_unpack :
      Expression.t
      -> t
    val create_pmod_extension :
      Extension.t
      -> t
  end

  and Structure : sig
    type t

    type concrete = Structure_item.t list

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create : Structure_item.t list -> t
  end

  and Structure_item : sig
    type t

    type concrete =
      { pstr_desc : Structure_item_desc.t
      ; pstr_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pstr_desc:Structure_item_desc.t
      -> pstr_loc:Location.t
      -> t
  end

  and Structure_item_desc : sig
    type t

    type concrete =
      | Pstr_eval of Expression.t * Attributes.t
      | Pstr_value of Rec_flag.t * Value_binding.t list
      | Pstr_primitive of Value_description.t
      | Pstr_type of Rec_flag.t * Type_declaration.t list
      | Pstr_typext of Type_extension.t
      | Pstr_exception of Extension_constructor.t
      | Pstr_module of Module_binding.t
      | Pstr_recmodule of Module_binding.t list
      | Pstr_modtype of Module_type_declaration.t
      | Pstr_open of Open_description.t
      | Pstr_class of Class_declaration.t list
      | Pstr_class_type of Class_type_declaration.t list
      | Pstr_include of Include_declaration.t
      | Pstr_attribute of Attribute.t
      | Pstr_extension of Extension.t * Attributes.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pstr_eval :
      Expression.t
      -> Attributes.t
      -> t
    val create_pstr_value :
      Rec_flag.t
      -> Value_binding.t list
      -> t
    val create_pstr_primitive :
      Value_description.t
      -> t
    val create_pstr_type :
      Rec_flag.t
      -> Type_declaration.t list
      -> t
    val create_pstr_typext :
      Type_extension.t
      -> t
    val create_pstr_exception :
      Extension_constructor.t
      -> t
    val create_pstr_module :
      Module_binding.t
      -> t
    val create_pstr_recmodule :
      Module_binding.t list
      -> t
    val create_pstr_modtype :
      Module_type_declaration.t
      -> t
    val create_pstr_open :
      Open_description.t
      -> t
    val create_pstr_class :
      Class_declaration.t list
      -> t
    val create_pstr_class_type :
      Class_type_declaration.t list
      -> t
    val create_pstr_include :
      Include_declaration.t
      -> t
    val create_pstr_attribute :
      Attribute.t
      -> t
    val create_pstr_extension :
      Extension.t
      -> Attributes.t
      -> t
  end

  and Value_binding : sig
    type t

    type concrete =
      { pvb_pat : Pattern.t
      ; pvb_expr : Expression.t
      ; pvb_attributes : Attributes.t
      ; pvb_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pvb_pat:Pattern.t
      -> pvb_expr:Expression.t
      -> pvb_attributes:Attributes.t
      -> pvb_loc:Location.t
      -> t
  end

  and Module_binding : sig
    type t

    type concrete =
      { pmb_name : string Location.loc
      ; pmb_expr : Module_expr.t
      ; pmb_attributes : Attributes.t
      ; pmb_loc : Location.t
      }

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create :
      pmb_name:string Location.loc
      -> pmb_expr:Module_expr.t
      -> pmb_attributes:Attributes.t
      -> pmb_loc:Location.t
      -> t
  end

  and Toplevel_phrase : sig
    type t

    type concrete =
      | Ptop_def of Structure.t
      | Ptop_dir of string * Directive_argument.t

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_ptop_def :
      Structure.t
      -> t
    val create_ptop_dir :
      string
      -> Directive_argument.t
      -> t
  end

  and Directive_argument : sig
    type t

    type concrete =
      | Pdir_none
      | Pdir_string of string
      | Pdir_int of string * char option
      | Pdir_ident of Longident.t
      | Pdir_bool of bool

    val of_concrete : concrete -> t
    val to_concrete : t -> concrete option

    val create_pdir_none : t
    val create_pdir_string :
      string
      -> t
    val create_pdir_int :
      string
      -> char option
      -> t
    val create_pdir_ident :
      Longident.t
      -> t
    val create_pdir_bool :
      bool
      -> t
  end
end
(*$*)
