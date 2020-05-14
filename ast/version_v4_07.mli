open Unversioned.Types

(*$ Ppx_ast_cinaps.print_version_mli (Astlib.Version.of_string "v4_07") *)
module rec Longident : sig
  type t = longident

  type concrete =
    | Lident of string
    | Ldot of longident * string
    | Lapply of longident * longident

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val lident :
    string
    -> t
  val ldot :
    longident
    -> string
    -> t
  val lapply :
    longident
    -> longident
    -> t
end

and Longident_loc : sig
  type t = longident_loc

  type concrete = longident Astlib.Loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : longident Astlib.Loc.t -> t
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

  type concrete = (string Astlib.Loc.t * payload)

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : (string Astlib.Loc.t * payload) -> t
end

and Extension : sig
  type t = extension

  type concrete = (string Astlib.Loc.t * payload)

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : (string Astlib.Loc.t * payload) -> t
end

and Attributes : sig
  type t = attributes

  type concrete = attribute list

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : attribute list -> t
end

and Payload : sig
  type t = payload

  type concrete =
    | PStr of structure
    | PSig of signature
    | PTyp of core_type
    | PPat of pattern * expression option

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pStr :
    structure
    -> t
  val pSig :
    signature
    -> t
  val pTyp :
    core_type
    -> t
  val pPat :
    pattern
    -> expression option
    -> t
end

and Core_type : sig
  type t = core_type

  type concrete =
    { ptyp_desc : core_type_desc
    ; ptyp_loc : Astlib.Location.t
    ; ptyp_attributes : attributes
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ptyp_desc:core_type_desc
    -> ptyp_loc:Astlib.Location.t
    -> ptyp_attributes:attributes
    -> t
  val update :
    ?ptyp_desc:core_type_desc
    -> ?ptyp_loc:Astlib.Location.t
    -> ?ptyp_attributes:attributes
    -> t -> t

  val ptyp_desc : t -> core_type_desc
  val ptyp_loc : t -> Astlib.Location.t
  val ptyp_attributes : t -> attributes
end

and Core_type_desc : sig
  type t = core_type_desc

  type concrete =
    | Ptyp_any
    | Ptyp_var of string
    | Ptyp_arrow of arg_label * core_type * core_type
    | Ptyp_tuple of core_type list
    | Ptyp_constr of longident_loc * core_type list
    | Ptyp_object of object_field list * closed_flag
    | Ptyp_class of longident_loc * core_type list
    | Ptyp_alias of core_type * string
    | Ptyp_variant of row_field list * closed_flag * string list option
    | Ptyp_poly of string Astlib.Loc.t list * core_type
    | Ptyp_package of package_type
    | Ptyp_extension of extension

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ptyp_any : t
  val ptyp_var :
    string
    -> t
  val ptyp_arrow :
    arg_label
    -> core_type
    -> core_type
    -> t
  val ptyp_tuple :
    core_type list
    -> t
  val ptyp_constr :
    longident_loc
    -> core_type list
    -> t
  val ptyp_object :
    object_field list
    -> closed_flag
    -> t
  val ptyp_class :
    longident_loc
    -> core_type list
    -> t
  val ptyp_alias :
    core_type
    -> string
    -> t
  val ptyp_variant :
    row_field list
    -> closed_flag
    -> string list option
    -> t
  val ptyp_poly :
    string Astlib.Loc.t list
    -> core_type
    -> t
  val ptyp_package :
    package_type
    -> t
  val ptyp_extension :
    extension
    -> t
end

and Package_type : sig
  type t = package_type

  type concrete = (longident_loc * (longident_loc * core_type) list)

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : (longident_loc * (longident_loc * core_type) list) -> t
end

and Row_field : sig
  type t = row_field

  type concrete =
    | Rtag of string Astlib.Loc.t * attributes * bool * core_type list
    | Rinherit of core_type

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val rtag :
    string Astlib.Loc.t
    -> attributes
    -> bool
    -> core_type list
    -> t
  val rinherit :
    core_type
    -> t
end

and Object_field : sig
  type t = object_field

  type concrete =
    | Otag of string Astlib.Loc.t * attributes * core_type
    | Oinherit of core_type

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val otag :
    string Astlib.Loc.t
    -> attributes
    -> core_type
    -> t
  val oinherit :
    core_type
    -> t
end

and Pattern : sig
  type t = pattern

  type concrete =
    { ppat_desc : pattern_desc
    ; ppat_loc : Astlib.Location.t
    ; ppat_attributes : attributes
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ppat_desc:pattern_desc
    -> ppat_loc:Astlib.Location.t
    -> ppat_attributes:attributes
    -> t
  val update :
    ?ppat_desc:pattern_desc
    -> ?ppat_loc:Astlib.Location.t
    -> ?ppat_attributes:attributes
    -> t -> t

  val ppat_desc : t -> pattern_desc
  val ppat_loc : t -> Astlib.Location.t
  val ppat_attributes : t -> attributes
end

and Pattern_desc : sig
  type t = pattern_desc

  type concrete =
    | Ppat_any
    | Ppat_var of string Astlib.Loc.t
    | Ppat_alias of pattern * string Astlib.Loc.t
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
    | Ppat_unpack of string Astlib.Loc.t
    | Ppat_exception of pattern
    | Ppat_extension of extension
    | Ppat_open of longident_loc * pattern

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ppat_any : t
  val ppat_var :
    string Astlib.Loc.t
    -> t
  val ppat_alias :
    pattern
    -> string Astlib.Loc.t
    -> t
  val ppat_constant :
    constant
    -> t
  val ppat_interval :
    constant
    -> constant
    -> t
  val ppat_tuple :
    pattern list
    -> t
  val ppat_construct :
    longident_loc
    -> pattern option
    -> t
  val ppat_variant :
    string
    -> pattern option
    -> t
  val ppat_record :
    (longident_loc * pattern) list
    -> closed_flag
    -> t
  val ppat_array :
    pattern list
    -> t
  val ppat_or :
    pattern
    -> pattern
    -> t
  val ppat_constraint :
    pattern
    -> core_type
    -> t
  val ppat_type :
    longident_loc
    -> t
  val ppat_lazy :
    pattern
    -> t
  val ppat_unpack :
    string Astlib.Loc.t
    -> t
  val ppat_exception :
    pattern
    -> t
  val ppat_extension :
    extension
    -> t
  val ppat_open :
    longident_loc
    -> pattern
    -> t
end

and Expression : sig
  type t = expression

  type concrete =
    { pexp_desc : expression_desc
    ; pexp_loc : Astlib.Location.t
    ; pexp_attributes : attributes
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pexp_desc:expression_desc
    -> pexp_loc:Astlib.Location.t
    -> pexp_attributes:attributes
    -> t
  val update :
    ?pexp_desc:expression_desc
    -> ?pexp_loc:Astlib.Location.t
    -> ?pexp_attributes:attributes
    -> t -> t

  val pexp_desc : t -> expression_desc
  val pexp_loc : t -> Astlib.Location.t
  val pexp_attributes : t -> attributes
end

and Expression_desc : sig
  type t = expression_desc

  type concrete =
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
    | Pexp_send of expression * string Astlib.Loc.t
    | Pexp_new of longident_loc
    | Pexp_setinstvar of string Astlib.Loc.t * expression
    | Pexp_override of (string Astlib.Loc.t * expression) list
    | Pexp_letmodule of string Astlib.Loc.t * module_expr * expression
    | Pexp_letexception of extension_constructor * expression
    | Pexp_assert of expression
    | Pexp_lazy of expression
    | Pexp_poly of expression * core_type option
    | Pexp_object of class_structure
    | Pexp_newtype of string Astlib.Loc.t * expression
    | Pexp_pack of module_expr
    | Pexp_open of override_flag * longident_loc * expression
    | Pexp_extension of extension
    | Pexp_unreachable

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pexp_ident :
    longident_loc
    -> t
  val pexp_constant :
    constant
    -> t
  val pexp_let :
    rec_flag
    -> value_binding list
    -> expression
    -> t
  val pexp_function :
    case list
    -> t
  val pexp_fun :
    arg_label
    -> expression option
    -> pattern
    -> expression
    -> t
  val pexp_apply :
    expression
    -> (arg_label * expression) list
    -> t
  val pexp_match :
    expression
    -> case list
    -> t
  val pexp_try :
    expression
    -> case list
    -> t
  val pexp_tuple :
    expression list
    -> t
  val pexp_construct :
    longident_loc
    -> expression option
    -> t
  val pexp_variant :
    string
    -> expression option
    -> t
  val pexp_record :
    (longident_loc * expression) list
    -> expression option
    -> t
  val pexp_field :
    expression
    -> longident_loc
    -> t
  val pexp_setfield :
    expression
    -> longident_loc
    -> expression
    -> t
  val pexp_array :
    expression list
    -> t
  val pexp_ifthenelse :
    expression
    -> expression
    -> expression option
    -> t
  val pexp_sequence :
    expression
    -> expression
    -> t
  val pexp_while :
    expression
    -> expression
    -> t
  val pexp_for :
    pattern
    -> expression
    -> expression
    -> direction_flag
    -> expression
    -> t
  val pexp_constraint :
    expression
    -> core_type
    -> t
  val pexp_coerce :
    expression
    -> core_type option
    -> core_type
    -> t
  val pexp_send :
    expression
    -> string Astlib.Loc.t
    -> t
  val pexp_new :
    longident_loc
    -> t
  val pexp_setinstvar :
    string Astlib.Loc.t
    -> expression
    -> t
  val pexp_override :
    (string Astlib.Loc.t * expression) list
    -> t
  val pexp_letmodule :
    string Astlib.Loc.t
    -> module_expr
    -> expression
    -> t
  val pexp_letexception :
    extension_constructor
    -> expression
    -> t
  val pexp_assert :
    expression
    -> t
  val pexp_lazy :
    expression
    -> t
  val pexp_poly :
    expression
    -> core_type option
    -> t
  val pexp_object :
    class_structure
    -> t
  val pexp_newtype :
    string Astlib.Loc.t
    -> expression
    -> t
  val pexp_pack :
    module_expr
    -> t
  val pexp_open :
    override_flag
    -> longident_loc
    -> expression
    -> t
  val pexp_extension :
    extension
    -> t
  val pexp_unreachable : t
end

and Case : sig
  type t = case

  type concrete =
    { pc_lhs : pattern
    ; pc_guard : expression option
    ; pc_rhs : expression
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pc_lhs:pattern
    -> pc_guard:expression option
    -> pc_rhs:expression
    -> t
  val update :
    ?pc_lhs:pattern
    -> ?pc_guard:expression option
    -> ?pc_rhs:expression
    -> t -> t

  val pc_lhs : t -> pattern
  val pc_guard : t -> expression option
  val pc_rhs : t -> expression
end

and Value_description : sig
  type t = value_description

  type concrete =
    { pval_name : string Astlib.Loc.t
    ; pval_type : core_type
    ; pval_prim : string list
    ; pval_attributes : attributes
    ; pval_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pval_name:string Astlib.Loc.t
    -> pval_type:core_type
    -> pval_prim:string list
    -> pval_attributes:attributes
    -> pval_loc:Astlib.Location.t
    -> t
  val update :
    ?pval_name:string Astlib.Loc.t
    -> ?pval_type:core_type
    -> ?pval_prim:string list
    -> ?pval_attributes:attributes
    -> ?pval_loc:Astlib.Location.t
    -> t -> t

  val pval_name : t -> string Astlib.Loc.t
  val pval_type : t -> core_type
  val pval_prim : t -> string list
  val pval_attributes : t -> attributes
  val pval_loc : t -> Astlib.Location.t
end

and Type_declaration : sig
  type t = type_declaration

  type concrete =
    { ptype_name : string Astlib.Loc.t
    ; ptype_params : (core_type * variance) list
    ; ptype_cstrs : (core_type * core_type * Astlib.Location.t) list
    ; ptype_kind : type_kind
    ; ptype_private : private_flag
    ; ptype_manifest : core_type option
    ; ptype_attributes : attributes
    ; ptype_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ptype_name:string Astlib.Loc.t
    -> ptype_params:(core_type * variance) list
    -> ptype_cstrs:(core_type * core_type * Astlib.Location.t) list
    -> ptype_kind:type_kind
    -> ptype_private:private_flag
    -> ptype_manifest:core_type option
    -> ptype_attributes:attributes
    -> ptype_loc:Astlib.Location.t
    -> t
  val update :
    ?ptype_name:string Astlib.Loc.t
    -> ?ptype_params:(core_type * variance) list
    -> ?ptype_cstrs:(core_type * core_type * Astlib.Location.t) list
    -> ?ptype_kind:type_kind
    -> ?ptype_private:private_flag
    -> ?ptype_manifest:core_type option
    -> ?ptype_attributes:attributes
    -> ?ptype_loc:Astlib.Location.t
    -> t -> t

  val ptype_name : t -> string Astlib.Loc.t
  val ptype_params : t -> (core_type * variance) list
  val ptype_cstrs : t -> (core_type * core_type * Astlib.Location.t) list
  val ptype_kind : t -> type_kind
  val ptype_private : t -> private_flag
  val ptype_manifest : t -> core_type option
  val ptype_attributes : t -> attributes
  val ptype_loc : t -> Astlib.Location.t
end

and Type_kind : sig
  type t = type_kind

  type concrete =
    | Ptype_abstract
    | Ptype_variant of constructor_declaration list
    | Ptype_record of label_declaration list
    | Ptype_open

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ptype_abstract : t
  val ptype_variant :
    constructor_declaration list
    -> t
  val ptype_record :
    label_declaration list
    -> t
  val ptype_open : t
end

and Label_declaration : sig
  type t = label_declaration

  type concrete =
    { pld_name : string Astlib.Loc.t
    ; pld_mutable : mutable_flag
    ; pld_type : core_type
    ; pld_loc : Astlib.Location.t
    ; pld_attributes : attributes
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pld_name:string Astlib.Loc.t
    -> pld_mutable:mutable_flag
    -> pld_type:core_type
    -> pld_loc:Astlib.Location.t
    -> pld_attributes:attributes
    -> t
  val update :
    ?pld_name:string Astlib.Loc.t
    -> ?pld_mutable:mutable_flag
    -> ?pld_type:core_type
    -> ?pld_loc:Astlib.Location.t
    -> ?pld_attributes:attributes
    -> t -> t

  val pld_name : t -> string Astlib.Loc.t
  val pld_mutable : t -> mutable_flag
  val pld_type : t -> core_type
  val pld_loc : t -> Astlib.Location.t
  val pld_attributes : t -> attributes
end

and Constructor_declaration : sig
  type t = constructor_declaration

  type concrete =
    { pcd_name : string Astlib.Loc.t
    ; pcd_args : constructor_arguments
    ; pcd_res : core_type option
    ; pcd_loc : Astlib.Location.t
    ; pcd_attributes : attributes
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcd_name:string Astlib.Loc.t
    -> pcd_args:constructor_arguments
    -> pcd_res:core_type option
    -> pcd_loc:Astlib.Location.t
    -> pcd_attributes:attributes
    -> t
  val update :
    ?pcd_name:string Astlib.Loc.t
    -> ?pcd_args:constructor_arguments
    -> ?pcd_res:core_type option
    -> ?pcd_loc:Astlib.Location.t
    -> ?pcd_attributes:attributes
    -> t -> t

  val pcd_name : t -> string Astlib.Loc.t
  val pcd_args : t -> constructor_arguments
  val pcd_res : t -> core_type option
  val pcd_loc : t -> Astlib.Location.t
  val pcd_attributes : t -> attributes
end

and Constructor_arguments : sig
  type t = constructor_arguments

  type concrete =
    | Pcstr_tuple of core_type list
    | Pcstr_record of label_declaration list

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcstr_tuple :
    core_type list
    -> t
  val pcstr_record :
    label_declaration list
    -> t
end

and Type_extension : sig
  type t = type_extension

  type concrete =
    { ptyext_path : longident_loc
    ; ptyext_params : (core_type * variance) list
    ; ptyext_constructors : extension_constructor list
    ; ptyext_private : private_flag
    ; ptyext_attributes : attributes
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ptyext_path:longident_loc
    -> ptyext_params:(core_type * variance) list
    -> ptyext_constructors:extension_constructor list
    -> ptyext_private:private_flag
    -> ptyext_attributes:attributes
    -> t
  val update :
    ?ptyext_path:longident_loc
    -> ?ptyext_params:(core_type * variance) list
    -> ?ptyext_constructors:extension_constructor list
    -> ?ptyext_private:private_flag
    -> ?ptyext_attributes:attributes
    -> t -> t

  val ptyext_path : t -> longident_loc
  val ptyext_params : t -> (core_type * variance) list
  val ptyext_constructors : t -> extension_constructor list
  val ptyext_private : t -> private_flag
  val ptyext_attributes : t -> attributes
end

and Extension_constructor : sig
  type t = extension_constructor

  type concrete =
    { pext_name : string Astlib.Loc.t
    ; pext_kind : extension_constructor_kind
    ; pext_loc : Astlib.Location.t
    ; pext_attributes : attributes
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pext_name:string Astlib.Loc.t
    -> pext_kind:extension_constructor_kind
    -> pext_loc:Astlib.Location.t
    -> pext_attributes:attributes
    -> t
  val update :
    ?pext_name:string Astlib.Loc.t
    -> ?pext_kind:extension_constructor_kind
    -> ?pext_loc:Astlib.Location.t
    -> ?pext_attributes:attributes
    -> t -> t

  val pext_name : t -> string Astlib.Loc.t
  val pext_kind : t -> extension_constructor_kind
  val pext_loc : t -> Astlib.Location.t
  val pext_attributes : t -> attributes
end

and Extension_constructor_kind : sig
  type t = extension_constructor_kind

  type concrete =
    | Pext_decl of constructor_arguments * core_type option
    | Pext_rebind of longident_loc

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pext_decl :
    constructor_arguments
    -> core_type option
    -> t
  val pext_rebind :
    longident_loc
    -> t
end

and Class_type : sig
  type t = class_type

  type concrete =
    { pcty_desc : class_type_desc
    ; pcty_loc : Astlib.Location.t
    ; pcty_attributes : attributes
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcty_desc:class_type_desc
    -> pcty_loc:Astlib.Location.t
    -> pcty_attributes:attributes
    -> t
  val update :
    ?pcty_desc:class_type_desc
    -> ?pcty_loc:Astlib.Location.t
    -> ?pcty_attributes:attributes
    -> t -> t

  val pcty_desc : t -> class_type_desc
  val pcty_loc : t -> Astlib.Location.t
  val pcty_attributes : t -> attributes
end

and Class_type_desc : sig
  type t = class_type_desc

  type concrete =
    | Pcty_constr of longident_loc * core_type list
    | Pcty_signature of class_signature
    | Pcty_arrow of arg_label * core_type * class_type
    | Pcty_extension of extension
    | Pcty_open of override_flag * longident_loc * class_type

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcty_constr :
    longident_loc
    -> core_type list
    -> t
  val pcty_signature :
    class_signature
    -> t
  val pcty_arrow :
    arg_label
    -> core_type
    -> class_type
    -> t
  val pcty_extension :
    extension
    -> t
  val pcty_open :
    override_flag
    -> longident_loc
    -> class_type
    -> t
end

and Class_signature : sig
  type t = class_signature

  type concrete =
    { pcsig_self : core_type
    ; pcsig_fields : class_type_field list
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcsig_self:core_type
    -> pcsig_fields:class_type_field list
    -> t
  val update :
    ?pcsig_self:core_type
    -> ?pcsig_fields:class_type_field list
    -> t -> t

  val pcsig_self : t -> core_type
  val pcsig_fields : t -> class_type_field list
end

and Class_type_field : sig
  type t = class_type_field

  type concrete =
    { pctf_desc : class_type_field_desc
    ; pctf_loc : Astlib.Location.t
    ; pctf_attributes : attributes
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pctf_desc:class_type_field_desc
    -> pctf_loc:Astlib.Location.t
    -> pctf_attributes:attributes
    -> t
  val update :
    ?pctf_desc:class_type_field_desc
    -> ?pctf_loc:Astlib.Location.t
    -> ?pctf_attributes:attributes
    -> t -> t

  val pctf_desc : t -> class_type_field_desc
  val pctf_loc : t -> Astlib.Location.t
  val pctf_attributes : t -> attributes
end

and Class_type_field_desc : sig
  type t = class_type_field_desc

  type concrete =
    | Pctf_inherit of class_type
    | Pctf_val of (string Astlib.Loc.t * mutable_flag * virtual_flag * core_type)
    | Pctf_method of (string Astlib.Loc.t * private_flag * virtual_flag * core_type)
    | Pctf_constraint of (core_type * core_type)
    | Pctf_attribute of attribute
    | Pctf_extension of extension

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pctf_inherit :
    class_type
    -> t
  val pctf_val :
    (string Astlib.Loc.t * mutable_flag * virtual_flag * core_type)
    -> t
  val pctf_method :
    (string Astlib.Loc.t * private_flag * virtual_flag * core_type)
    -> t
  val pctf_constraint :
    (core_type * core_type)
    -> t
  val pctf_attribute :
    attribute
    -> t
  val pctf_extension :
    extension
    -> t
end

and Class_infos : sig
  type 'a t = 'a class_infos

  type 'a concrete =
    { pci_virt : virtual_flag
    ; pci_params : (core_type * variance) list
    ; pci_name : string Astlib.Loc.t
    ; pci_expr : 'a
    ; pci_loc : Astlib.Location.t
    ; pci_attributes : attributes
    }

  val of_concrete : 'a node concrete -> 'a node t
  val to_concrete : 'a node t -> 'a node concrete
  val to_concrete_opt : 'a node t -> 'a node concrete option

  val create :
    pci_virt:virtual_flag
    -> pci_params:(core_type * variance) list
    -> pci_name:string Astlib.Loc.t
    -> pci_expr:'a node
    -> pci_loc:Astlib.Location.t
    -> pci_attributes:attributes
    -> 'a node t
  val update :
    ?pci_virt:virtual_flag
    -> ?pci_params:(core_type * variance) list
    -> ?pci_name:string Astlib.Loc.t
    -> ?pci_expr:'a node
    -> ?pci_loc:Astlib.Location.t
    -> ?pci_attributes:attributes
    -> 'a node t -> 'a node t

  val pci_virt : 'a node t -> virtual_flag
  val pci_params : 'a node t -> (core_type * variance) list
  val pci_name : 'a node t -> string Astlib.Loc.t
  val pci_expr : 'a node t -> 'a node
  val pci_loc : 'a node t -> Astlib.Location.t
  val pci_attributes : 'a node t -> attributes
end

and Class_description : sig
  type t = class_description

  type concrete = class_type class_infos

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : class_type class_infos -> t
end

and Class_type_declaration : sig
  type t = class_type_declaration

  type concrete = class_type class_infos

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : class_type class_infos -> t
end

and Class_expr : sig
  type t = class_expr

  type concrete =
    { pcl_desc : class_expr_desc
    ; pcl_loc : Astlib.Location.t
    ; pcl_attributes : attributes
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcl_desc:class_expr_desc
    -> pcl_loc:Astlib.Location.t
    -> pcl_attributes:attributes
    -> t
  val update :
    ?pcl_desc:class_expr_desc
    -> ?pcl_loc:Astlib.Location.t
    -> ?pcl_attributes:attributes
    -> t -> t

  val pcl_desc : t -> class_expr_desc
  val pcl_loc : t -> Astlib.Location.t
  val pcl_attributes : t -> attributes
end

and Class_expr_desc : sig
  type t = class_expr_desc

  type concrete =
    | Pcl_constr of longident_loc * core_type list
    | Pcl_structure of class_structure
    | Pcl_fun of arg_label * expression option * pattern * class_expr
    | Pcl_apply of class_expr * (arg_label * expression) list
    | Pcl_let of rec_flag * value_binding list * class_expr
    | Pcl_constraint of class_expr * class_type
    | Pcl_extension of extension
    | Pcl_open of override_flag * longident_loc * class_expr

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcl_constr :
    longident_loc
    -> core_type list
    -> t
  val pcl_structure :
    class_structure
    -> t
  val pcl_fun :
    arg_label
    -> expression option
    -> pattern
    -> class_expr
    -> t
  val pcl_apply :
    class_expr
    -> (arg_label * expression) list
    -> t
  val pcl_let :
    rec_flag
    -> value_binding list
    -> class_expr
    -> t
  val pcl_constraint :
    class_expr
    -> class_type
    -> t
  val pcl_extension :
    extension
    -> t
  val pcl_open :
    override_flag
    -> longident_loc
    -> class_expr
    -> t
end

and Class_structure : sig
  type t = class_structure

  type concrete =
    { pcstr_self : pattern
    ; pcstr_fields : class_field list
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcstr_self:pattern
    -> pcstr_fields:class_field list
    -> t
  val update :
    ?pcstr_self:pattern
    -> ?pcstr_fields:class_field list
    -> t -> t

  val pcstr_self : t -> pattern
  val pcstr_fields : t -> class_field list
end

and Class_field : sig
  type t = class_field

  type concrete =
    { pcf_desc : class_field_desc
    ; pcf_loc : Astlib.Location.t
    ; pcf_attributes : attributes
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcf_desc:class_field_desc
    -> pcf_loc:Astlib.Location.t
    -> pcf_attributes:attributes
    -> t
  val update :
    ?pcf_desc:class_field_desc
    -> ?pcf_loc:Astlib.Location.t
    -> ?pcf_attributes:attributes
    -> t -> t

  val pcf_desc : t -> class_field_desc
  val pcf_loc : t -> Astlib.Location.t
  val pcf_attributes : t -> attributes
end

and Class_field_desc : sig
  type t = class_field_desc

  type concrete =
    | Pcf_inherit of override_flag * class_expr * string Astlib.Loc.t option
    | Pcf_val of (string Astlib.Loc.t * mutable_flag * class_field_kind)
    | Pcf_method of (string Astlib.Loc.t * private_flag * class_field_kind)
    | Pcf_constraint of (core_type * core_type)
    | Pcf_initializer of expression
    | Pcf_attribute of attribute
    | Pcf_extension of extension

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcf_inherit :
    override_flag
    -> class_expr
    -> string Astlib.Loc.t option
    -> t
  val pcf_val :
    (string Astlib.Loc.t * mutable_flag * class_field_kind)
    -> t
  val pcf_method :
    (string Astlib.Loc.t * private_flag * class_field_kind)
    -> t
  val pcf_constraint :
    (core_type * core_type)
    -> t
  val pcf_initializer :
    expression
    -> t
  val pcf_attribute :
    attribute
    -> t
  val pcf_extension :
    extension
    -> t
end

and Class_field_kind : sig
  type t = class_field_kind

  type concrete =
    | Cfk_virtual of core_type
    | Cfk_concrete of override_flag * expression

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val cfk_virtual :
    core_type
    -> t
  val cfk_concrete :
    override_flag
    -> expression
    -> t
end

and Class_declaration : sig
  type t = class_declaration

  type concrete = class_expr class_infos

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : class_expr class_infos -> t
end

and Module_type : sig
  type t = module_type

  type concrete =
    { pmty_desc : module_type_desc
    ; pmty_loc : Astlib.Location.t
    ; pmty_attributes : attributes
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmty_desc:module_type_desc
    -> pmty_loc:Astlib.Location.t
    -> pmty_attributes:attributes
    -> t
  val update :
    ?pmty_desc:module_type_desc
    -> ?pmty_loc:Astlib.Location.t
    -> ?pmty_attributes:attributes
    -> t -> t

  val pmty_desc : t -> module_type_desc
  val pmty_loc : t -> Astlib.Location.t
  val pmty_attributes : t -> attributes
end

and Module_type_desc : sig
  type t = module_type_desc

  type concrete =
    | Pmty_ident of longident_loc
    | Pmty_signature of signature
    | Pmty_functor of string Astlib.Loc.t * module_type option * module_type
    | Pmty_with of module_type * with_constraint list
    | Pmty_typeof of module_expr
    | Pmty_extension of extension
    | Pmty_alias of longident_loc

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pmty_ident :
    longident_loc
    -> t
  val pmty_signature :
    signature
    -> t
  val pmty_functor :
    string Astlib.Loc.t
    -> module_type option
    -> module_type
    -> t
  val pmty_with :
    module_type
    -> with_constraint list
    -> t
  val pmty_typeof :
    module_expr
    -> t
  val pmty_extension :
    extension
    -> t
  val pmty_alias :
    longident_loc
    -> t
end

and Signature : sig
  type t = signature

  type concrete = signature_item list

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : signature_item list -> t
end

and Signature_item : sig
  type t = signature_item

  type concrete =
    { psig_desc : signature_item_desc
    ; psig_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    psig_desc:signature_item_desc
    -> psig_loc:Astlib.Location.t
    -> t
  val update :
    ?psig_desc:signature_item_desc
    -> ?psig_loc:Astlib.Location.t
    -> t -> t

  val psig_desc : t -> signature_item_desc
  val psig_loc : t -> Astlib.Location.t
end

and Signature_item_desc : sig
  type t = signature_item_desc

  type concrete =
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

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val psig_value :
    value_description
    -> t
  val psig_type :
    rec_flag
    -> type_declaration list
    -> t
  val psig_typext :
    type_extension
    -> t
  val psig_exception :
    extension_constructor
    -> t
  val psig_module :
    module_declaration
    -> t
  val psig_recmodule :
    module_declaration list
    -> t
  val psig_modtype :
    module_type_declaration
    -> t
  val psig_open :
    open_description
    -> t
  val psig_include :
    include_description
    -> t
  val psig_class :
    class_description list
    -> t
  val psig_class_type :
    class_type_declaration list
    -> t
  val psig_attribute :
    attribute
    -> t
  val psig_extension :
    extension
    -> attributes
    -> t
end

and Module_declaration : sig
  type t = module_declaration

  type concrete =
    { pmd_name : string Astlib.Loc.t
    ; pmd_type : module_type
    ; pmd_attributes : attributes
    ; pmd_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmd_name:string Astlib.Loc.t
    -> pmd_type:module_type
    -> pmd_attributes:attributes
    -> pmd_loc:Astlib.Location.t
    -> t
  val update :
    ?pmd_name:string Astlib.Loc.t
    -> ?pmd_type:module_type
    -> ?pmd_attributes:attributes
    -> ?pmd_loc:Astlib.Location.t
    -> t -> t

  val pmd_name : t -> string Astlib.Loc.t
  val pmd_type : t -> module_type
  val pmd_attributes : t -> attributes
  val pmd_loc : t -> Astlib.Location.t
end

and Module_type_declaration : sig
  type t = module_type_declaration

  type concrete =
    { pmtd_name : string Astlib.Loc.t
    ; pmtd_type : module_type option
    ; pmtd_attributes : attributes
    ; pmtd_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmtd_name:string Astlib.Loc.t
    -> pmtd_type:module_type option
    -> pmtd_attributes:attributes
    -> pmtd_loc:Astlib.Location.t
    -> t
  val update :
    ?pmtd_name:string Astlib.Loc.t
    -> ?pmtd_type:module_type option
    -> ?pmtd_attributes:attributes
    -> ?pmtd_loc:Astlib.Location.t
    -> t -> t

  val pmtd_name : t -> string Astlib.Loc.t
  val pmtd_type : t -> module_type option
  val pmtd_attributes : t -> attributes
  val pmtd_loc : t -> Astlib.Location.t
end

and Open_description : sig
  type t = open_description

  type concrete =
    { popen_lid : longident_loc
    ; popen_override : override_flag
    ; popen_loc : Astlib.Location.t
    ; popen_attributes : attributes
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    popen_lid:longident_loc
    -> popen_override:override_flag
    -> popen_loc:Astlib.Location.t
    -> popen_attributes:attributes
    -> t
  val update :
    ?popen_lid:longident_loc
    -> ?popen_override:override_flag
    -> ?popen_loc:Astlib.Location.t
    -> ?popen_attributes:attributes
    -> t -> t

  val popen_lid : t -> longident_loc
  val popen_override : t -> override_flag
  val popen_loc : t -> Astlib.Location.t
  val popen_attributes : t -> attributes
end

and Include_infos : sig
  type 'a t = 'a include_infos

  type 'a concrete =
    { pincl_mod : 'a
    ; pincl_loc : Astlib.Location.t
    ; pincl_attributes : attributes
    }

  val of_concrete : 'a node concrete -> 'a node t
  val to_concrete : 'a node t -> 'a node concrete
  val to_concrete_opt : 'a node t -> 'a node concrete option

  val create :
    pincl_mod:'a node
    -> pincl_loc:Astlib.Location.t
    -> pincl_attributes:attributes
    -> 'a node t
  val update :
    ?pincl_mod:'a node
    -> ?pincl_loc:Astlib.Location.t
    -> ?pincl_attributes:attributes
    -> 'a node t -> 'a node t

  val pincl_mod : 'a node t -> 'a node
  val pincl_loc : 'a node t -> Astlib.Location.t
  val pincl_attributes : 'a node t -> attributes
end

and Include_description : sig
  type t = include_description

  type concrete = module_type include_infos

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : module_type include_infos -> t
end

and Include_declaration : sig
  type t = include_declaration

  type concrete = module_expr include_infos

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : module_expr include_infos -> t
end

and With_constraint : sig
  type t = with_constraint

  type concrete =
    | Pwith_type of longident_loc * type_declaration
    | Pwith_module of longident_loc * longident_loc
    | Pwith_typesubst of longident_loc * type_declaration
    | Pwith_modsubst of longident_loc * longident_loc

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pwith_type :
    longident_loc
    -> type_declaration
    -> t
  val pwith_module :
    longident_loc
    -> longident_loc
    -> t
  val pwith_typesubst :
    longident_loc
    -> type_declaration
    -> t
  val pwith_modsubst :
    longident_loc
    -> longident_loc
    -> t
end

and Module_expr : sig
  type t = module_expr

  type concrete =
    { pmod_desc : module_expr_desc
    ; pmod_loc : Astlib.Location.t
    ; pmod_attributes : attributes
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmod_desc:module_expr_desc
    -> pmod_loc:Astlib.Location.t
    -> pmod_attributes:attributes
    -> t
  val update :
    ?pmod_desc:module_expr_desc
    -> ?pmod_loc:Astlib.Location.t
    -> ?pmod_attributes:attributes
    -> t -> t

  val pmod_desc : t -> module_expr_desc
  val pmod_loc : t -> Astlib.Location.t
  val pmod_attributes : t -> attributes
end

and Module_expr_desc : sig
  type t = module_expr_desc

  type concrete =
    | Pmod_ident of longident_loc
    | Pmod_structure of structure
    | Pmod_functor of string Astlib.Loc.t * module_type option * module_expr
    | Pmod_apply of module_expr * module_expr
    | Pmod_constraint of module_expr * module_type
    | Pmod_unpack of expression
    | Pmod_extension of extension

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pmod_ident :
    longident_loc
    -> t
  val pmod_structure :
    structure
    -> t
  val pmod_functor :
    string Astlib.Loc.t
    -> module_type option
    -> module_expr
    -> t
  val pmod_apply :
    module_expr
    -> module_expr
    -> t
  val pmod_constraint :
    module_expr
    -> module_type
    -> t
  val pmod_unpack :
    expression
    -> t
  val pmod_extension :
    extension
    -> t
end

and Structure : sig
  type t = structure

  type concrete = structure_item list

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : structure_item list -> t
end

and Structure_item : sig
  type t = structure_item

  type concrete =
    { pstr_desc : structure_item_desc
    ; pstr_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pstr_desc:structure_item_desc
    -> pstr_loc:Astlib.Location.t
    -> t
  val update :
    ?pstr_desc:structure_item_desc
    -> ?pstr_loc:Astlib.Location.t
    -> t -> t

  val pstr_desc : t -> structure_item_desc
  val pstr_loc : t -> Astlib.Location.t
end

and Structure_item_desc : sig
  type t = structure_item_desc

  type concrete =
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

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pstr_eval :
    expression
    -> attributes
    -> t
  val pstr_value :
    rec_flag
    -> value_binding list
    -> t
  val pstr_primitive :
    value_description
    -> t
  val pstr_type :
    rec_flag
    -> type_declaration list
    -> t
  val pstr_typext :
    type_extension
    -> t
  val pstr_exception :
    extension_constructor
    -> t
  val pstr_module :
    module_binding
    -> t
  val pstr_recmodule :
    module_binding list
    -> t
  val pstr_modtype :
    module_type_declaration
    -> t
  val pstr_open :
    open_description
    -> t
  val pstr_class :
    class_declaration list
    -> t
  val pstr_class_type :
    class_type_declaration list
    -> t
  val pstr_include :
    include_declaration
    -> t
  val pstr_attribute :
    attribute
    -> t
  val pstr_extension :
    extension
    -> attributes
    -> t
end

and Value_binding : sig
  type t = value_binding

  type concrete =
    { pvb_pat : pattern
    ; pvb_expr : expression
    ; pvb_attributes : attributes
    ; pvb_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pvb_pat:pattern
    -> pvb_expr:expression
    -> pvb_attributes:attributes
    -> pvb_loc:Astlib.Location.t
    -> t
  val update :
    ?pvb_pat:pattern
    -> ?pvb_expr:expression
    -> ?pvb_attributes:attributes
    -> ?pvb_loc:Astlib.Location.t
    -> t -> t

  val pvb_pat : t -> pattern
  val pvb_expr : t -> expression
  val pvb_attributes : t -> attributes
  val pvb_loc : t -> Astlib.Location.t
end

and Module_binding : sig
  type t = module_binding

  type concrete =
    { pmb_name : string Astlib.Loc.t
    ; pmb_expr : module_expr
    ; pmb_attributes : attributes
    ; pmb_loc : Astlib.Location.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmb_name:string Astlib.Loc.t
    -> pmb_expr:module_expr
    -> pmb_attributes:attributes
    -> pmb_loc:Astlib.Location.t
    -> t
  val update :
    ?pmb_name:string Astlib.Loc.t
    -> ?pmb_expr:module_expr
    -> ?pmb_attributes:attributes
    -> ?pmb_loc:Astlib.Location.t
    -> t -> t

  val pmb_name : t -> string Astlib.Loc.t
  val pmb_expr : t -> module_expr
  val pmb_attributes : t -> attributes
  val pmb_loc : t -> Astlib.Location.t
end

and Toplevel_phrase : sig
  type t = toplevel_phrase

  type concrete =
    | Ptop_def of structure
    | Ptop_dir of string * directive_argument

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ptop_def :
    structure
    -> t
  val ptop_dir :
    string
    -> directive_argument
    -> t
end

and Directive_argument : sig
  type t = directive_argument

  type concrete =
    | Pdir_none
    | Pdir_string of string
    | Pdir_int of string * char option
    | Pdir_ident of longident
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
    longident
    -> t
  val pdir_bool :
    bool
    -> t
end
(*$*)
