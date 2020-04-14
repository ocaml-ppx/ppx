open Unversioned.Types

(*$ Ppx_ast_cinaps.print_version_mli (Astlib.Version.of_string "v4_07") *)
module rec Longident : sig
  type t = longident

  type concrete =
    | Lident of string
    | Ldot of Longident.t * string
    | Lapply of Longident.t * Longident.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val lident :
    string
    -> t
  val ldot :
    Longident.t
    -> string
    -> t
  val lapply :
    Longident.t
    -> Longident.t
    -> t
end

and Longident_loc : sig
  type t = longident_loc

  type concrete = Longident.t Astlib.Loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : Longident.t Astlib.Loc.t -> t
end

and Rec_flag : sig
  type t = rec_flag

  type concrete =
    | Nonrecursive
    | Recursive

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val nonrecursive : t
  val recursive : t
end

and Direction_flag : sig
  type t = direction_flag

  type concrete =
    | Upto
    | Downto

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val upto : t
  val downto_ : t
end

and Private_flag : sig
  type t = private_flag

  type concrete =
    | Private
    | Public

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val private_ : t
  val public : t
end

and Mutable_flag : sig
  type t = mutable_flag

  type concrete =
    | Immutable
    | Mutable

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val immutable : t
  val mutable_ : t
end

and Virtual_flag : sig
  type t = virtual_flag

  type concrete =
    | Virtual
    | Concrete

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val virtual_ : t
  val concrete : t
end

and Override_flag : sig
  type t = override_flag

  type concrete =
    | Override
    | Fresh

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val override : t
  val fresh : t
end

and Closed_flag : sig
  type t = closed_flag

  type concrete =
    | Closed
    | Open

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val closed : t
  val open_ : t
end

and Arg_label : sig
  type t = arg_label

  type concrete =
    | Nolabel
    | Labelled of string
    | Optional of string

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val nolabel : t
  val labelled :
    string
    -> t
  val optional :
    string
    -> t
end

and Variance : sig
  type t = variance

  type concrete =
    | Covariant
    | Contravariant
    | Invariant

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val covariant : t
  val contravariant : t
  val invariant : t
end

and Constant : sig
  type t = constant

  type concrete =
    | Pconst_integer of string * char option
    | Pconst_char of char
    | Pconst_string of string * string option
    | Pconst_float of string * char option

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pconst_integer :
    string
    -> char option
    -> t
  val pconst_char :
    char
    -> t
  val pconst_string :
    string
    -> string option
    -> t
  val pconst_float :
    string
    -> char option
    -> t
end

and Attribute : sig
  type t = attribute

  type concrete = (string Astlib.Loc.t * Payload.t)

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : (string Astlib.Loc.t * Payload.t) -> t
end

and Extension : sig
  type t = extension

  type concrete = (string Astlib.Loc.t * Payload.t)

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : (string Astlib.Loc.t * Payload.t) -> t
end

and Attributes : sig
  type t = attributes

  type concrete = Attribute.t list

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : Attribute.t list -> t
end

and Payload : sig
  type t = payload

  type concrete =
    | PStr of Structure.t
    | PSig of Signature.t
    | PTyp of Core_type.t
    | PPat of Pattern.t * Expression.t option

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pstr :
    Structure.t
    -> t
  val psig :
    Signature.t
    -> t
  val ptyp :
    Core_type.t
    -> t
  val ppat :
    Pattern.t
    -> Expression.t option
    -> t
end

and Core_type : sig
  type t = core_type

  type concrete =
    { ptyp_desc : Core_type_desc.t
    ; ptyp_loc : Astlib.Location.t
    ; ptyp_attributes : Attributes.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ptyp_desc:Core_type_desc.t
    -> ptyp_loc:Astlib.Location.t
    -> ptyp_attributes:Attributes.t
    -> t
end

and Core_type_desc : sig
  type t = core_type_desc

  type concrete =
    | Ptyp_any
    | Ptyp_var of string
    | Ptyp_arrow of Arg_label.t * Core_type.t * Core_type.t
    | Ptyp_tuple of Core_type.t list
    | Ptyp_constr of Longident_loc.t * Core_type.t list
    | Ptyp_object of Object_field.t list * Closed_flag.t
    | Ptyp_class of Longident_loc.t * Core_type.t list
    | Ptyp_alias of Core_type.t * string
    | Ptyp_variant of Row_field.t list * Closed_flag.t * string list option
    | Ptyp_poly of string Astlib.Loc.t list * Core_type.t
    | Ptyp_package of Package_type.t
    | Ptyp_extension of Extension.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ptyp_any : t
  val ptyp_var :
    string
    -> t
  val ptyp_arrow :
    Arg_label.t
    -> Core_type.t
    -> Core_type.t
    -> t
  val ptyp_tuple :
    Core_type.t list
    -> t
  val ptyp_constr :
    Longident_loc.t
    -> Core_type.t list
    -> t
  val ptyp_object :
    Object_field.t list
    -> Closed_flag.t
    -> t
  val ptyp_class :
    Longident_loc.t
    -> Core_type.t list
    -> t
  val ptyp_alias :
    Core_type.t
    -> string
    -> t
  val ptyp_variant :
    Row_field.t list
    -> Closed_flag.t
    -> string list option
    -> t
  val ptyp_poly :
    string Astlib.Loc.t list
    -> Core_type.t
    -> t
  val ptyp_package :
    Package_type.t
    -> t
  val ptyp_extension :
    Extension.t
    -> t
end

and Package_type : sig
  type t = package_type

  type concrete = (Longident_loc.t * (Longident_loc.t * Core_type.t) list)

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : (Longident_loc.t * (Longident_loc.t * Core_type.t) list) -> t
end

and Row_field : sig
  type t = row_field

  type concrete =
    | Rtag of string Astlib.Loc.t * Attributes.t * bool * Core_type.t list
    | Rinherit of Core_type.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val rtag :
    string Astlib.Loc.t
    -> Attributes.t
    -> bool
    -> Core_type.t list
    -> t
  val rinherit :
    Core_type.t
    -> t
end

and Object_field : sig
  type t = object_field

  type concrete =
    | Otag of string Astlib.Loc.t * Attributes.t * Core_type.t
    | Oinherit of Core_type.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val otag :
    string Astlib.Loc.t
    -> Attributes.t
    -> Core_type.t
    -> t
  val oinherit :
    Core_type.t
    -> t
end

and Pattern : sig
  type t = pattern

  type concrete =
    { ppat_desc : Pattern_desc.t
    ; ppat_loc : Astlib.Location.t
    ; ppat_attributes : Attributes.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ppat_desc:Pattern_desc.t
    -> ppat_loc:Astlib.Location.t
    -> ppat_attributes:Attributes.t
    -> t
end

and Pattern_desc : sig
  type t = pattern_desc

  type concrete =
    | Ppat_any
    | Ppat_var of string Astlib.Loc.t
    | Ppat_alias of Pattern.t * string Astlib.Loc.t
    | Ppat_constant of Constant.t
    | Ppat_interval of Constant.t * Constant.t
    | Ppat_tuple of Pattern.t list
    | Ppat_construct of Longident_loc.t * Pattern.t option
    | Ppat_variant of string * Pattern.t option
    | Ppat_record of (Longident_loc.t * Pattern.t) list * Closed_flag.t
    | Ppat_array of Pattern.t list
    | Ppat_or of Pattern.t * Pattern.t
    | Ppat_constraint of Pattern.t * Core_type.t
    | Ppat_type of Longident_loc.t
    | Ppat_lazy of Pattern.t
    | Ppat_unpack of string Astlib.Loc.t
    | Ppat_exception of Pattern.t
    | Ppat_extension of Extension.t
    | Ppat_open of Longident_loc.t * Pattern.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ppat_any : t
  val ppat_var :
    string Astlib.Loc.t
    -> t
  val ppat_alias :
    Pattern.t
    -> string Astlib.Loc.t
    -> t
  val ppat_constant :
    Constant.t
    -> t
  val ppat_interval :
    Constant.t
    -> Constant.t
    -> t
  val ppat_tuple :
    Pattern.t list
    -> t
  val ppat_construct :
    Longident_loc.t
    -> Pattern.t option
    -> t
  val ppat_variant :
    string
    -> Pattern.t option
    -> t
  val ppat_record :
    (Longident_loc.t * Pattern.t) list
    -> Closed_flag.t
    -> t
  val ppat_array :
    Pattern.t list
    -> t
  val ppat_or :
    Pattern.t
    -> Pattern.t
    -> t
  val ppat_constraint :
    Pattern.t
    -> Core_type.t
    -> t
  val ppat_type :
    Longident_loc.t
    -> t
  val ppat_lazy :
    Pattern.t
    -> t
  val ppat_unpack :
    string Astlib.Loc.t
    -> t
  val ppat_exception :
    Pattern.t
    -> t
  val ppat_extension :
    Extension.t
    -> t
  val ppat_open :
    Longident_loc.t
    -> Pattern.t
    -> t
end

and Expression : sig
  type t = expression

  type concrete =
    { pexp_desc : Expression_desc.t
    ; pexp_loc : Astlib.Location.t
    ; pexp_attributes : Attributes.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pexp_desc:Expression_desc.t
    -> pexp_loc:Astlib.Location.t
    -> pexp_attributes:Attributes.t
    -> t
end

and Expression_desc : sig
  type t = expression_desc

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
    | Pexp_variant of string * Expression.t option
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
    | Pexp_send of Expression.t * string Astlib.Loc.t
    | Pexp_new of Longident_loc.t
    | Pexp_setinstvar of string Astlib.Loc.t * Expression.t
    | Pexp_override of (string Astlib.Loc.t * Expression.t) list
    | Pexp_letmodule of string Astlib.Loc.t * Module_expr.t * Expression.t
    | Pexp_letexception of Extension_constructor.t * Expression.t
    | Pexp_assert of Expression.t
    | Pexp_lazy of Expression.t
    | Pexp_poly of Expression.t * Core_type.t option
    | Pexp_object of Class_structure.t
    | Pexp_newtype of string Astlib.Loc.t * Expression.t
    | Pexp_pack of Module_expr.t
    | Pexp_open of Override_flag.t * Longident_loc.t * Expression.t
    | Pexp_extension of Extension.t
    | Pexp_unreachable

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pexp_ident :
    Longident_loc.t
    -> t
  val pexp_constant :
    Constant.t
    -> t
  val pexp_let :
    Rec_flag.t
    -> Value_binding.t list
    -> Expression.t
    -> t
  val pexp_function :
    Case.t list
    -> t
  val pexp_fun :
    Arg_label.t
    -> Expression.t option
    -> Pattern.t
    -> Expression.t
    -> t
  val pexp_apply :
    Expression.t
    -> (Arg_label.t * Expression.t) list
    -> t
  val pexp_match :
    Expression.t
    -> Case.t list
    -> t
  val pexp_try :
    Expression.t
    -> Case.t list
    -> t
  val pexp_tuple :
    Expression.t list
    -> t
  val pexp_construct :
    Longident_loc.t
    -> Expression.t option
    -> t
  val pexp_variant :
    string
    -> Expression.t option
    -> t
  val pexp_record :
    (Longident_loc.t * Expression.t) list
    -> Expression.t option
    -> t
  val pexp_field :
    Expression.t
    -> Longident_loc.t
    -> t
  val pexp_setfield :
    Expression.t
    -> Longident_loc.t
    -> Expression.t
    -> t
  val pexp_array :
    Expression.t list
    -> t
  val pexp_ifthenelse :
    Expression.t
    -> Expression.t
    -> Expression.t option
    -> t
  val pexp_sequence :
    Expression.t
    -> Expression.t
    -> t
  val pexp_while :
    Expression.t
    -> Expression.t
    -> t
  val pexp_for :
    Pattern.t
    -> Expression.t
    -> Expression.t
    -> Direction_flag.t
    -> Expression.t
    -> t
  val pexp_constraint :
    Expression.t
    -> Core_type.t
    -> t
  val pexp_coerce :
    Expression.t
    -> Core_type.t option
    -> Core_type.t
    -> t
  val pexp_send :
    Expression.t
    -> string Astlib.Loc.t
    -> t
  val pexp_new :
    Longident_loc.t
    -> t
  val pexp_setinstvar :
    string Astlib.Loc.t
    -> Expression.t
    -> t
  val pexp_override :
    (string Astlib.Loc.t * Expression.t) list
    -> t
  val pexp_letmodule :
    string Astlib.Loc.t
    -> Module_expr.t
    -> Expression.t
    -> t
  val pexp_letexception :
    Extension_constructor.t
    -> Expression.t
    -> t
  val pexp_assert :
    Expression.t
    -> t
  val pexp_lazy :
    Expression.t
    -> t
  val pexp_poly :
    Expression.t
    -> Core_type.t option
    -> t
  val pexp_object :
    Class_structure.t
    -> t
  val pexp_newtype :
    string Astlib.Loc.t
    -> Expression.t
    -> t
  val pexp_pack :
    Module_expr.t
    -> t
  val pexp_open :
    Override_flag.t
    -> Longident_loc.t
    -> Expression.t
    -> t
  val pexp_extension :
    Extension.t
    -> t
  val pexp_unreachable : t
end

and Case : sig
  type t = case

  type concrete =
    { pc_lhs : Pattern.t
    ; pc_guard : Expression.t option
    ; pc_rhs : Expression.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pc_lhs:Pattern.t
    -> pc_guard:Expression.t option
    -> pc_rhs:Expression.t
    -> t
end

and Value_description : sig
  type t = value_description

  type concrete =
    { pval_name : string Astlib.Loc.t
    ; pval_type : Core_type.t
    ; pval_prim : string list
    ; pval_attributes : Attributes.t
    ; pval_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pval_name:string Astlib.Loc.t
    -> pval_type:Core_type.t
    -> pval_prim:string list
    -> pval_attributes:Attributes.t
    -> pval_loc:Astlib.Location.t
    -> t
end

and Type_declaration : sig
  type t = type_declaration

  type concrete =
    { ptype_name : string Astlib.Loc.t
    ; ptype_params : (Core_type.t * Variance.t) list
    ; ptype_cstrs : (Core_type.t * Core_type.t * Astlib.Location.t) list
    ; ptype_kind : Type_kind.t
    ; ptype_private : Private_flag.t
    ; ptype_manifest : Core_type.t option
    ; ptype_attributes : Attributes.t
    ; ptype_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ptype_name:string Astlib.Loc.t
    -> ptype_params:(Core_type.t * Variance.t) list
    -> ptype_cstrs:(Core_type.t * Core_type.t * Astlib.Location.t) list
    -> ptype_kind:Type_kind.t
    -> ptype_private:Private_flag.t
    -> ptype_manifest:Core_type.t option
    -> ptype_attributes:Attributes.t
    -> ptype_loc:Astlib.Location.t
    -> t
end

and Type_kind : sig
  type t = type_kind

  type concrete =
    | Ptype_abstract
    | Ptype_variant of Constructor_declaration.t list
    | Ptype_record of Label_declaration.t list
    | Ptype_open

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ptype_abstract : t
  val ptype_variant :
    Constructor_declaration.t list
    -> t
  val ptype_record :
    Label_declaration.t list
    -> t
  val ptype_open : t
end

and Label_declaration : sig
  type t = label_declaration

  type concrete =
    { pld_name : string Astlib.Loc.t
    ; pld_mutable : Mutable_flag.t
    ; pld_type : Core_type.t
    ; pld_loc : Astlib.Location.t
    ; pld_attributes : Attributes.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pld_name:string Astlib.Loc.t
    -> pld_mutable:Mutable_flag.t
    -> pld_type:Core_type.t
    -> pld_loc:Astlib.Location.t
    -> pld_attributes:Attributes.t
    -> t
end

and Constructor_declaration : sig
  type t = constructor_declaration

  type concrete =
    { pcd_name : string Astlib.Loc.t
    ; pcd_args : Constructor_arguments.t
    ; pcd_res : Core_type.t option
    ; pcd_loc : Astlib.Location.t
    ; pcd_attributes : Attributes.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcd_name:string Astlib.Loc.t
    -> pcd_args:Constructor_arguments.t
    -> pcd_res:Core_type.t option
    -> pcd_loc:Astlib.Location.t
    -> pcd_attributes:Attributes.t
    -> t
end

and Constructor_arguments : sig
  type t = constructor_arguments

  type concrete =
    | Pcstr_tuple of Core_type.t list
    | Pcstr_record of Label_declaration.t list

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcstr_tuple :
    Core_type.t list
    -> t
  val pcstr_record :
    Label_declaration.t list
    -> t
end

and Type_extension : sig
  type t = type_extension

  type concrete =
    { ptyext_path : Longident_loc.t
    ; ptyext_params : (Core_type.t * Variance.t) list
    ; ptyext_constructors : Extension_constructor.t list
    ; ptyext_private : Private_flag.t
    ; ptyext_attributes : Attributes.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ptyext_path:Longident_loc.t
    -> ptyext_params:(Core_type.t * Variance.t) list
    -> ptyext_constructors:Extension_constructor.t list
    -> ptyext_private:Private_flag.t
    -> ptyext_attributes:Attributes.t
    -> t
end

and Extension_constructor : sig
  type t = extension_constructor

  type concrete =
    { pext_name : string Astlib.Loc.t
    ; pext_kind : Extension_constructor_kind.t
    ; pext_loc : Astlib.Location.t
    ; pext_attributes : Attributes.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pext_name:string Astlib.Loc.t
    -> pext_kind:Extension_constructor_kind.t
    -> pext_loc:Astlib.Location.t
    -> pext_attributes:Attributes.t
    -> t
end

and Extension_constructor_kind : sig
  type t = extension_constructor_kind

  type concrete =
    | Pext_decl of Constructor_arguments.t * Core_type.t option
    | Pext_rebind of Longident_loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pext_decl :
    Constructor_arguments.t
    -> Core_type.t option
    -> t
  val pext_rebind :
    Longident_loc.t
    -> t
end

and Class_type : sig
  type t = class_type

  type concrete =
    { pcty_desc : Class_type_desc.t
    ; pcty_loc : Astlib.Location.t
    ; pcty_attributes : Attributes.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcty_desc:Class_type_desc.t
    -> pcty_loc:Astlib.Location.t
    -> pcty_attributes:Attributes.t
    -> t
end

and Class_type_desc : sig
  type t = class_type_desc

  type concrete =
    | Pcty_constr of Longident_loc.t * Core_type.t list
    | Pcty_signature of Class_signature.t
    | Pcty_arrow of Arg_label.t * Core_type.t * Class_type.t
    | Pcty_extension of Extension.t
    | Pcty_open of Override_flag.t * Longident_loc.t * Class_type.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcty_constr :
    Longident_loc.t
    -> Core_type.t list
    -> t
  val pcty_signature :
    Class_signature.t
    -> t
  val pcty_arrow :
    Arg_label.t
    -> Core_type.t
    -> Class_type.t
    -> t
  val pcty_extension :
    Extension.t
    -> t
  val pcty_open :
    Override_flag.t
    -> Longident_loc.t
    -> Class_type.t
    -> t
end

and Class_signature : sig
  type t = class_signature

  type concrete =
    { pcsig_self : Core_type.t
    ; pcsig_fields : Class_type_field.t list
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcsig_self:Core_type.t
    -> pcsig_fields:Class_type_field.t list
    -> t
end

and Class_type_field : sig
  type t = class_type_field

  type concrete =
    { pctf_desc : Class_type_field_desc.t
    ; pctf_loc : Astlib.Location.t
    ; pctf_attributes : Attributes.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pctf_desc:Class_type_field_desc.t
    -> pctf_loc:Astlib.Location.t
    -> pctf_attributes:Attributes.t
    -> t
end

and Class_type_field_desc : sig
  type t = class_type_field_desc

  type concrete =
    | Pctf_inherit of Class_type.t
    | Pctf_val of (string Astlib.Loc.t * Mutable_flag.t * Virtual_flag.t * Core_type.t)
    | Pctf_method of (string Astlib.Loc.t * Private_flag.t * Virtual_flag.t * Core_type.t)
    | Pctf_constraint of (Core_type.t * Core_type.t)
    | Pctf_attribute of Attribute.t
    | Pctf_extension of Extension.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pctf_inherit :
    Class_type.t
    -> t
  val pctf_val :
    (string Astlib.Loc.t * Mutable_flag.t * Virtual_flag.t * Core_type.t)
    -> t
  val pctf_method :
    (string Astlib.Loc.t * Private_flag.t * Virtual_flag.t * Core_type.t)
    -> t
  val pctf_constraint :
    (Core_type.t * Core_type.t)
    -> t
  val pctf_attribute :
    Attribute.t
    -> t
  val pctf_extension :
    Extension.t
    -> t
end

and Class_infos : sig
  type 'a t = 'a class_infos

  type 'a concrete =
    { pci_virt : Virtual_flag.t
    ; pci_params : (Core_type.t * Variance.t) list
    ; pci_name : string Astlib.Loc.t
    ; pci_expr : 'a
    ; pci_loc : Astlib.Location.t
    ; pci_attributes : Attributes.t
    }

  val of_concrete : 'a node concrete -> 'a node t
  val to_concrete : 'a node t -> 'a node concrete
  val to_concrete_opt : 'a node t -> 'a node concrete option

  val create :
    pci_virt:Virtual_flag.t
    -> pci_params:(Core_type.t * Variance.t) list
    -> pci_name:string Astlib.Loc.t
    -> pci_expr:'a node
    -> pci_loc:Astlib.Location.t
    -> pci_attributes:Attributes.t
    -> 'a node t
end

and Class_description : sig
  type t = class_description

  type concrete = Class_type.t Class_infos.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : Class_type.t Class_infos.t -> t
end

and Class_type_declaration : sig
  type t = class_type_declaration

  type concrete = Class_type.t Class_infos.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : Class_type.t Class_infos.t -> t
end

and Class_expr : sig
  type t = class_expr

  type concrete =
    { pcl_desc : Class_expr_desc.t
    ; pcl_loc : Astlib.Location.t
    ; pcl_attributes : Attributes.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcl_desc:Class_expr_desc.t
    -> pcl_loc:Astlib.Location.t
    -> pcl_attributes:Attributes.t
    -> t
end

and Class_expr_desc : sig
  type t = class_expr_desc

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
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcl_constr :
    Longident_loc.t
    -> Core_type.t list
    -> t
  val pcl_structure :
    Class_structure.t
    -> t
  val pcl_fun :
    Arg_label.t
    -> Expression.t option
    -> Pattern.t
    -> Class_expr.t
    -> t
  val pcl_apply :
    Class_expr.t
    -> (Arg_label.t * Expression.t) list
    -> t
  val pcl_let :
    Rec_flag.t
    -> Value_binding.t list
    -> Class_expr.t
    -> t
  val pcl_constraint :
    Class_expr.t
    -> Class_type.t
    -> t
  val pcl_extension :
    Extension.t
    -> t
  val pcl_open :
    Override_flag.t
    -> Longident_loc.t
    -> Class_expr.t
    -> t
end

and Class_structure : sig
  type t = class_structure

  type concrete =
    { pcstr_self : Pattern.t
    ; pcstr_fields : Class_field.t list
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcstr_self:Pattern.t
    -> pcstr_fields:Class_field.t list
    -> t
end

and Class_field : sig
  type t = class_field

  type concrete =
    { pcf_desc : Class_field_desc.t
    ; pcf_loc : Astlib.Location.t
    ; pcf_attributes : Attributes.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcf_desc:Class_field_desc.t
    -> pcf_loc:Astlib.Location.t
    -> pcf_attributes:Attributes.t
    -> t
end

and Class_field_desc : sig
  type t = class_field_desc

  type concrete =
    | Pcf_inherit of Override_flag.t * Class_expr.t * string Astlib.Loc.t option
    | Pcf_val of (string Astlib.Loc.t * Mutable_flag.t * Class_field_kind.t)
    | Pcf_method of (string Astlib.Loc.t * Private_flag.t * Class_field_kind.t)
    | Pcf_constraint of (Core_type.t * Core_type.t)
    | Pcf_initializer of Expression.t
    | Pcf_attribute of Attribute.t
    | Pcf_extension of Extension.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcf_inherit :
    Override_flag.t
    -> Class_expr.t
    -> string Astlib.Loc.t option
    -> t
  val pcf_val :
    (string Astlib.Loc.t * Mutable_flag.t * Class_field_kind.t)
    -> t
  val pcf_method :
    (string Astlib.Loc.t * Private_flag.t * Class_field_kind.t)
    -> t
  val pcf_constraint :
    (Core_type.t * Core_type.t)
    -> t
  val pcf_initializer :
    Expression.t
    -> t
  val pcf_attribute :
    Attribute.t
    -> t
  val pcf_extension :
    Extension.t
    -> t
end

and Class_field_kind : sig
  type t = class_field_kind

  type concrete =
    | Cfk_virtual of Core_type.t
    | Cfk_concrete of Override_flag.t * Expression.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val cfk_virtual :
    Core_type.t
    -> t
  val cfk_concrete :
    Override_flag.t
    -> Expression.t
    -> t
end

and Class_declaration : sig
  type t = class_declaration

  type concrete = Class_expr.t Class_infos.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : Class_expr.t Class_infos.t -> t
end

and Module_type : sig
  type t = module_type

  type concrete =
    { pmty_desc : Module_type_desc.t
    ; pmty_loc : Astlib.Location.t
    ; pmty_attributes : Attributes.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmty_desc:Module_type_desc.t
    -> pmty_loc:Astlib.Location.t
    -> pmty_attributes:Attributes.t
    -> t
end

and Module_type_desc : sig
  type t = module_type_desc

  type concrete =
    | Pmty_ident of Longident_loc.t
    | Pmty_signature of Signature.t
    | Pmty_functor of string Astlib.Loc.t * Module_type.t option * Module_type.t
    | Pmty_with of Module_type.t * With_constraint.t list
    | Pmty_typeof of Module_expr.t
    | Pmty_extension of Extension.t
    | Pmty_alias of Longident_loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pmty_ident :
    Longident_loc.t
    -> t
  val pmty_signature :
    Signature.t
    -> t
  val pmty_functor :
    string Astlib.Loc.t
    -> Module_type.t option
    -> Module_type.t
    -> t
  val pmty_with :
    Module_type.t
    -> With_constraint.t list
    -> t
  val pmty_typeof :
    Module_expr.t
    -> t
  val pmty_extension :
    Extension.t
    -> t
  val pmty_alias :
    Longident_loc.t
    -> t
end

and Signature : sig
  type t = signature

  type concrete = Signature_item.t list

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : Signature_item.t list -> t
end

and Signature_item : sig
  type t = signature_item

  type concrete =
    { psig_desc : Signature_item_desc.t
    ; psig_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    psig_desc:Signature_item_desc.t
    -> psig_loc:Astlib.Location.t
    -> t
end

and Signature_item_desc : sig
  type t = signature_item_desc

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
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val psig_value :
    Value_description.t
    -> t
  val psig_type :
    Rec_flag.t
    -> Type_declaration.t list
    -> t
  val psig_typext :
    Type_extension.t
    -> t
  val psig_exception :
    Extension_constructor.t
    -> t
  val psig_module :
    Module_declaration.t
    -> t
  val psig_recmodule :
    Module_declaration.t list
    -> t
  val psig_modtype :
    Module_type_declaration.t
    -> t
  val psig_open :
    Open_description.t
    -> t
  val psig_include :
    Include_description.t
    -> t
  val psig_class :
    Class_description.t list
    -> t
  val psig_class_type :
    Class_type_declaration.t list
    -> t
  val psig_attribute :
    Attribute.t
    -> t
  val psig_extension :
    Extension.t
    -> Attributes.t
    -> t
end

and Module_declaration : sig
  type t = module_declaration

  type concrete =
    { pmd_name : string Astlib.Loc.t
    ; pmd_type : Module_type.t
    ; pmd_attributes : Attributes.t
    ; pmd_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmd_name:string Astlib.Loc.t
    -> pmd_type:Module_type.t
    -> pmd_attributes:Attributes.t
    -> pmd_loc:Astlib.Location.t
    -> t
end

and Module_type_declaration : sig
  type t = module_type_declaration

  type concrete =
    { pmtd_name : string Astlib.Loc.t
    ; pmtd_type : Module_type.t option
    ; pmtd_attributes : Attributes.t
    ; pmtd_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmtd_name:string Astlib.Loc.t
    -> pmtd_type:Module_type.t option
    -> pmtd_attributes:Attributes.t
    -> pmtd_loc:Astlib.Location.t
    -> t
end

and Open_description : sig
  type t = open_description

  type concrete =
    { popen_lid : Longident_loc.t
    ; popen_override : Override_flag.t
    ; popen_loc : Astlib.Location.t
    ; popen_attributes : Attributes.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    popen_lid:Longident_loc.t
    -> popen_override:Override_flag.t
    -> popen_loc:Astlib.Location.t
    -> popen_attributes:Attributes.t
    -> t
end

and Include_infos : sig
  type 'a t = 'a include_infos

  type 'a concrete =
    { pincl_mod : 'a
    ; pincl_loc : Astlib.Location.t
    ; pincl_attributes : Attributes.t
    }

  val of_concrete : 'a node concrete -> 'a node t
  val to_concrete : 'a node t -> 'a node concrete
  val to_concrete_opt : 'a node t -> 'a node concrete option

  val create :
    pincl_mod:'a node
    -> pincl_loc:Astlib.Location.t
    -> pincl_attributes:Attributes.t
    -> 'a node t
end

and Include_description : sig
  type t = include_description

  type concrete = Module_type.t Include_infos.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : Module_type.t Include_infos.t -> t
end

and Include_declaration : sig
  type t = include_declaration

  type concrete = Module_expr.t Include_infos.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : Module_expr.t Include_infos.t -> t
end

and With_constraint : sig
  type t = with_constraint

  type concrete =
    | Pwith_type of Longident_loc.t * Type_declaration.t
    | Pwith_module of Longident_loc.t * Longident_loc.t
    | Pwith_typesubst of Longident_loc.t * Type_declaration.t
    | Pwith_modsubst of Longident_loc.t * Longident_loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pwith_type :
    Longident_loc.t
    -> Type_declaration.t
    -> t
  val pwith_module :
    Longident_loc.t
    -> Longident_loc.t
    -> t
  val pwith_typesubst :
    Longident_loc.t
    -> Type_declaration.t
    -> t
  val pwith_modsubst :
    Longident_loc.t
    -> Longident_loc.t
    -> t
end

and Module_expr : sig
  type t = module_expr

  type concrete =
    { pmod_desc : Module_expr_desc.t
    ; pmod_loc : Astlib.Location.t
    ; pmod_attributes : Attributes.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmod_desc:Module_expr_desc.t
    -> pmod_loc:Astlib.Location.t
    -> pmod_attributes:Attributes.t
    -> t
end

and Module_expr_desc : sig
  type t = module_expr_desc

  type concrete =
    | Pmod_ident of Longident_loc.t
    | Pmod_structure of Structure.t
    | Pmod_functor of string Astlib.Loc.t * Module_type.t option * Module_expr.t
    | Pmod_apply of Module_expr.t * Module_expr.t
    | Pmod_constraint of Module_expr.t * Module_type.t
    | Pmod_unpack of Expression.t
    | Pmod_extension of Extension.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pmod_ident :
    Longident_loc.t
    -> t
  val pmod_structure :
    Structure.t
    -> t
  val pmod_functor :
    string Astlib.Loc.t
    -> Module_type.t option
    -> Module_expr.t
    -> t
  val pmod_apply :
    Module_expr.t
    -> Module_expr.t
    -> t
  val pmod_constraint :
    Module_expr.t
    -> Module_type.t
    -> t
  val pmod_unpack :
    Expression.t
    -> t
  val pmod_extension :
    Extension.t
    -> t
end

and Structure : sig
  type t = structure

  type concrete = Structure_item.t list

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : Structure_item.t list -> t
end

and Structure_item : sig
  type t = structure_item

  type concrete =
    { pstr_desc : Structure_item_desc.t
    ; pstr_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pstr_desc:Structure_item_desc.t
    -> pstr_loc:Astlib.Location.t
    -> t
end

and Structure_item_desc : sig
  type t = structure_item_desc

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
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pstr_eval :
    Expression.t
    -> Attributes.t
    -> t
  val pstr_value :
    Rec_flag.t
    -> Value_binding.t list
    -> t
  val pstr_primitive :
    Value_description.t
    -> t
  val pstr_type :
    Rec_flag.t
    -> Type_declaration.t list
    -> t
  val pstr_typext :
    Type_extension.t
    -> t
  val pstr_exception :
    Extension_constructor.t
    -> t
  val pstr_module :
    Module_binding.t
    -> t
  val pstr_recmodule :
    Module_binding.t list
    -> t
  val pstr_modtype :
    Module_type_declaration.t
    -> t
  val pstr_open :
    Open_description.t
    -> t
  val pstr_class :
    Class_declaration.t list
    -> t
  val pstr_class_type :
    Class_type_declaration.t list
    -> t
  val pstr_include :
    Include_declaration.t
    -> t
  val pstr_attribute :
    Attribute.t
    -> t
  val pstr_extension :
    Extension.t
    -> Attributes.t
    -> t
end

and Value_binding : sig
  type t = value_binding

  type concrete =
    { pvb_pat : Pattern.t
    ; pvb_expr : Expression.t
    ; pvb_attributes : Attributes.t
    ; pvb_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pvb_pat:Pattern.t
    -> pvb_expr:Expression.t
    -> pvb_attributes:Attributes.t
    -> pvb_loc:Astlib.Location.t
    -> t
end

and Module_binding : sig
  type t = module_binding

  type concrete =
    { pmb_name : string Astlib.Loc.t
    ; pmb_expr : Module_expr.t
    ; pmb_attributes : Attributes.t
    ; pmb_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmb_name:string Astlib.Loc.t
    -> pmb_expr:Module_expr.t
    -> pmb_attributes:Attributes.t
    -> pmb_loc:Astlib.Location.t
    -> t
end

and Toplevel_phrase : sig
  type t = toplevel_phrase

  type concrete =
    | Ptop_def of Structure.t
    | Ptop_dir of string * Directive_argument.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ptop_def :
    Structure.t
    -> t
  val ptop_dir :
    string
    -> Directive_argument.t
    -> t
end

and Directive_argument : sig
  type t = directive_argument

  type concrete =
    | Pdir_none
    | Pdir_string of string
    | Pdir_int of string * char option
    | Pdir_ident of Longident.t
    | Pdir_bool of bool

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pdir_none : t
  val pdir_string :
    string
    -> t
  val pdir_int :
    string
    -> char option
    -> t
  val pdir_ident :
    Longident.t
    -> t
  val pdir_bool :
    bool
    -> t
end
(*$*)
