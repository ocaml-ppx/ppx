open Unversioned.Types

(*$ Ppx_ast_cinaps.print_version_mli (Astlib.Version.of_string "unstable_for_testing") *)
module rec Directive_argument : sig
  type t = directive_argument

  type concrete =
    | Pdir_bool of bool
    | Pdir_ident of longident
    | Pdir_int of char option * string
    | Pdir_string of string
    | Pdir_none

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pdir_bool :
    bool
    -> t
  val pdir_ident :
    longident
    -> t
  val pdir_int :
    char option
    -> string
    -> t
  val pdir_string :
    string
    -> t
  val pdir_none : t
end

and Toplevel_phrase : sig
  type t = toplevel_phrase

  type concrete =
    | Ptop_dir of directive_argument * string
    | Ptop_def of structure

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ptop_dir :
    directive_argument
    -> string
    -> t
  val ptop_def :
    structure
    -> t
end

and Module_binding : sig
  type t = module_binding

  type concrete =
    { pmb_loc : Astlib.Location.t
    ; pmb_attributes : attributes
    ; pmb_expr : module_expr
    ; pmb_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmb_loc:Astlib.Location.t
    -> pmb_attributes:attributes
    -> pmb_expr:module_expr
    -> pmb_name:string Astlib.Loc.t
    -> t
  val update :
    ?pmb_loc:Astlib.Location.t
    -> ?pmb_attributes:attributes
    -> ?pmb_expr:module_expr
    -> ?pmb_name:string Astlib.Loc.t
    -> t -> t

  val pmb_loc : t -> Astlib.Location.t
  val pmb_attributes : t -> attributes
  val pmb_expr : t -> module_expr
  val pmb_name : t -> string Astlib.Loc.t
end

and Value_binding : sig
  type t = value_binding

  type concrete =
    { pvb_loc : Astlib.Location.t
    ; pvb_attributes : attributes
    ; pvb_expr : expression
    ; pvb_pat : pattern
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pvb_loc:Astlib.Location.t
    -> pvb_attributes:attributes
    -> pvb_expr:expression
    -> pvb_pat:pattern
    -> t
  val update :
    ?pvb_loc:Astlib.Location.t
    -> ?pvb_attributes:attributes
    -> ?pvb_expr:expression
    -> ?pvb_pat:pattern
    -> t -> t

  val pvb_loc : t -> Astlib.Location.t
  val pvb_attributes : t -> attributes
  val pvb_expr : t -> expression
  val pvb_pat : t -> pattern
end

and Structure_item_desc : sig
  type t = structure_item_desc

  type concrete =
    | Pstr_extension of attributes * extension
    | Pstr_attribute of attribute
    | Pstr_include of include_declaration
    | Pstr_class_type of class_type_declaration list
    | Pstr_class of class_declaration list
    | Pstr_open of open_description
    | Pstr_modtype of module_type_declaration
    | Pstr_recmodule of module_binding list
    | Pstr_module of module_binding
    | Pstr_exception of extension_constructor
    | Pstr_typext of type_extension
    | Pstr_type of type_declaration list * rec_flag
    | Pstr_primitive of value_description
    | Pstr_value of value_binding list * rec_flag
    | Pstr_eval of attributes * expression

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pstr_extension :
    attributes
    -> extension
    -> t
  val pstr_attribute :
    attribute
    -> t
  val pstr_include :
    include_declaration
    -> t
  val pstr_class_type :
    class_type_declaration list
    -> t
  val pstr_class :
    class_declaration list
    -> t
  val pstr_open :
    open_description
    -> t
  val pstr_modtype :
    module_type_declaration
    -> t
  val pstr_recmodule :
    module_binding list
    -> t
  val pstr_module :
    module_binding
    -> t
  val pstr_exception :
    extension_constructor
    -> t
  val pstr_typext :
    type_extension
    -> t
  val pstr_type :
    type_declaration list
    -> rec_flag
    -> t
  val pstr_primitive :
    value_description
    -> t
  val pstr_value :
    value_binding list
    -> rec_flag
    -> t
  val pstr_eval :
    attributes
    -> expression
    -> t
end

and Structure_item : sig
  type t = structure_item

  type concrete =
    { pstr_loc : Astlib.Location.t
    ; pstr_desc : structure_item_desc
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pstr_loc:Astlib.Location.t
    -> pstr_desc:structure_item_desc
    -> t
  val update :
    ?pstr_loc:Astlib.Location.t
    -> ?pstr_desc:structure_item_desc
    -> t -> t

  val pstr_loc : t -> Astlib.Location.t
  val pstr_desc : t -> structure_item_desc
end

and Structure : sig
  type t = structure

  type concrete = structure_item list

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : structure_item list -> t
end

and Module_expr_desc : sig
  type t = module_expr_desc

  type concrete =
    | Pmod_extension of extension
    | Pmod_unpack of expression
    | Pmod_constraint of module_type * module_expr
    | Pmod_apply of module_expr * module_expr
    | Pmod_functor of module_expr * module_type option * string Astlib.Loc.t
    | Pmod_structure of structure
    | Pmod_ident of longident_loc

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pmod_extension :
    extension
    -> t
  val pmod_unpack :
    expression
    -> t
  val pmod_constraint :
    module_type
    -> module_expr
    -> t
  val pmod_apply :
    module_expr
    -> module_expr
    -> t
  val pmod_functor :
    module_expr
    -> module_type option
    -> string Astlib.Loc.t
    -> t
  val pmod_structure :
    structure
    -> t
  val pmod_ident :
    longident_loc
    -> t
end

and Module_expr : sig
  type t = module_expr

  type concrete =
    { pmod_attributes : attributes
    ; pmod_loc : Astlib.Location.t
    ; pmod_desc : module_expr_desc
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmod_attributes:attributes
    -> pmod_loc:Astlib.Location.t
    -> pmod_desc:module_expr_desc
    -> t
  val update :
    ?pmod_attributes:attributes
    -> ?pmod_loc:Astlib.Location.t
    -> ?pmod_desc:module_expr_desc
    -> t -> t

  val pmod_attributes : t -> attributes
  val pmod_loc : t -> Astlib.Location.t
  val pmod_desc : t -> module_expr_desc
end

and With_constraint : sig
  type t = with_constraint

  type concrete =
    | Pwith_modsubst of longident_loc * longident_loc
    | Pwith_typesubst of type_declaration * longident_loc
    | Pwith_module of longident_loc * longident_loc
    | Pwith_type of type_declaration * longident_loc

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pwith_modsubst :
    longident_loc
    -> longident_loc
    -> t
  val pwith_typesubst :
    type_declaration
    -> longident_loc
    -> t
  val pwith_module :
    longident_loc
    -> longident_loc
    -> t
  val pwith_type :
    type_declaration
    -> longident_loc
    -> t
end

and Include_declaration : sig
  type t = include_declaration

  type concrete = module_expr include_infos

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : module_expr include_infos -> t
end

and Include_description : sig
  type t = include_description

  type concrete = module_type include_infos

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : module_type include_infos -> t
end

and Include_infos : sig
  type 'a t = 'a include_infos

  type 'a concrete =
    { pincl_attributes : attributes
    ; pincl_loc : Astlib.Location.t
    ; pincl_mod : 'a
    }

  val of_concrete : 'a node concrete -> 'a node t
  val to_concrete : 'a node t -> 'a node concrete
  val to_concrete_opt : 'a node t -> 'a node concrete option

  val create :
    pincl_attributes:attributes
    -> pincl_loc:Astlib.Location.t
    -> pincl_mod:'a node
    -> 'a node t
  val update :
    ?pincl_attributes:attributes
    -> ?pincl_loc:Astlib.Location.t
    -> ?pincl_mod:'a node
    -> 'a node t -> 'a node t

  val pincl_attributes : 'a node t -> attributes
  val pincl_loc : 'a node t -> Astlib.Location.t
  val pincl_mod : 'a node t -> 'a node
end

and Open_description : sig
  type t = open_description

  type concrete =
    { popen_attributes : attributes
    ; popen_loc : Astlib.Location.t
    ; popen_override : override_flag
    ; popen_lid : longident_loc
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    popen_attributes:attributes
    -> popen_loc:Astlib.Location.t
    -> popen_override:override_flag
    -> popen_lid:longident_loc
    -> t
  val update :
    ?popen_attributes:attributes
    -> ?popen_loc:Astlib.Location.t
    -> ?popen_override:override_flag
    -> ?popen_lid:longident_loc
    -> t -> t

  val popen_attributes : t -> attributes
  val popen_loc : t -> Astlib.Location.t
  val popen_override : t -> override_flag
  val popen_lid : t -> longident_loc
end

and Module_type_declaration : sig
  type t = module_type_declaration

  type concrete =
    { pmtd_loc : Astlib.Location.t
    ; pmtd_attributes : attributes
    ; pmtd_type : module_type option
    ; pmtd_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmtd_loc:Astlib.Location.t
    -> pmtd_attributes:attributes
    -> pmtd_type:module_type option
    -> pmtd_name:string Astlib.Loc.t
    -> t
  val update :
    ?pmtd_loc:Astlib.Location.t
    -> ?pmtd_attributes:attributes
    -> ?pmtd_type:module_type option
    -> ?pmtd_name:string Astlib.Loc.t
    -> t -> t

  val pmtd_loc : t -> Astlib.Location.t
  val pmtd_attributes : t -> attributes
  val pmtd_type : t -> module_type option
  val pmtd_name : t -> string Astlib.Loc.t
end

and Module_declaration : sig
  type t = module_declaration

  type concrete =
    { pmd_loc : Astlib.Location.t
    ; pmd_attributes : attributes
    ; pmd_type : module_type
    ; pmd_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmd_loc:Astlib.Location.t
    -> pmd_attributes:attributes
    -> pmd_type:module_type
    -> pmd_name:string Astlib.Loc.t
    -> t
  val update :
    ?pmd_loc:Astlib.Location.t
    -> ?pmd_attributes:attributes
    -> ?pmd_type:module_type
    -> ?pmd_name:string Astlib.Loc.t
    -> t -> t

  val pmd_loc : t -> Astlib.Location.t
  val pmd_attributes : t -> attributes
  val pmd_type : t -> module_type
  val pmd_name : t -> string Astlib.Loc.t
end

and Signature_item_desc : sig
  type t = signature_item_desc

  type concrete =
    | Psig_extension of attributes * extension
    | Psig_attribute of attribute
    | Psig_class_type of class_type_declaration list
    | Psig_class of class_description list
    | Psig_include of include_description
    | Psig_open of open_description
    | Psig_modtype of module_type_declaration
    | Psig_recmodule of module_declaration list
    | Psig_module of module_declaration
    | Psig_exception of extension_constructor
    | Psig_typext of type_extension
    | Psig_type of type_declaration list * rec_flag
    | Psig_value of value_description

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val psig_extension :
    attributes
    -> extension
    -> t
  val psig_attribute :
    attribute
    -> t
  val psig_class_type :
    class_type_declaration list
    -> t
  val psig_class :
    class_description list
    -> t
  val psig_include :
    include_description
    -> t
  val psig_open :
    open_description
    -> t
  val psig_modtype :
    module_type_declaration
    -> t
  val psig_recmodule :
    module_declaration list
    -> t
  val psig_module :
    module_declaration
    -> t
  val psig_exception :
    extension_constructor
    -> t
  val psig_typext :
    type_extension
    -> t
  val psig_type :
    type_declaration list
    -> rec_flag
    -> t
  val psig_value :
    value_description
    -> t
end

and Signature_item : sig
  type t = signature_item

  type concrete =
    { psig_loc : Astlib.Location.t
    ; psig_desc : signature_item_desc
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    psig_loc:Astlib.Location.t
    -> psig_desc:signature_item_desc
    -> t
  val update :
    ?psig_loc:Astlib.Location.t
    -> ?psig_desc:signature_item_desc
    -> t -> t

  val psig_loc : t -> Astlib.Location.t
  val psig_desc : t -> signature_item_desc
end

and Signature : sig
  type t = signature

  type concrete = signature_item list

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : signature_item list -> t
end

and Module_type_desc : sig
  type t = module_type_desc

  type concrete =
    | Pmty_alias of longident_loc
    | Pmty_extension of extension
    | Pmty_typeof of module_expr
    | Pmty_with of with_constraint list * module_type
    | Pmty_functor of module_type * module_type option * string Astlib.Loc.t
    | Pmty_signature of signature
    | Pmty_ident of longident_loc

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pmty_alias :
    longident_loc
    -> t
  val pmty_extension :
    extension
    -> t
  val pmty_typeof :
    module_expr
    -> t
  val pmty_with :
    with_constraint list
    -> module_type
    -> t
  val pmty_functor :
    module_type
    -> module_type option
    -> string Astlib.Loc.t
    -> t
  val pmty_signature :
    signature
    -> t
  val pmty_ident :
    longident_loc
    -> t
end

and Module_type : sig
  type t = module_type

  type concrete =
    { pmty_attributes : attributes
    ; pmty_loc : Astlib.Location.t
    ; pmty_desc : module_type_desc
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmty_attributes:attributes
    -> pmty_loc:Astlib.Location.t
    -> pmty_desc:module_type_desc
    -> t
  val update :
    ?pmty_attributes:attributes
    -> ?pmty_loc:Astlib.Location.t
    -> ?pmty_desc:module_type_desc
    -> t -> t

  val pmty_attributes : t -> attributes
  val pmty_loc : t -> Astlib.Location.t
  val pmty_desc : t -> module_type_desc
end

and Class_declaration : sig
  type t = class_declaration

  type concrete = class_expr class_infos

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : class_expr class_infos -> t
end

and Class_field_kind : sig
  type t = class_field_kind

  type concrete =
    | Cfk_concrete of expression * override_flag
    | Cfk_virtual of core_type

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val cfk_concrete :
    expression
    -> override_flag
    -> t
  val cfk_virtual :
    core_type
    -> t
end

and Class_field_desc : sig
  type t = class_field_desc

  type concrete =
    | Pcf_extension of extension
    | Pcf_attribute of attribute
    | Pcf_initializer of expression
    | Pcf_constraint of (core_type * core_type)
    | Pcf_method of (class_field_kind * private_flag * string Astlib.Loc.t)
    | Pcf_val of (class_field_kind * mutable_flag * string Astlib.Loc.t)
    | Pcf_inherit of string Astlib.Loc.t option * class_expr * override_flag

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcf_extension :
    extension
    -> t
  val pcf_attribute :
    attribute
    -> t
  val pcf_initializer :
    expression
    -> t
  val pcf_constraint :
    (core_type * core_type)
    -> t
  val pcf_method :
    (class_field_kind * private_flag * string Astlib.Loc.t)
    -> t
  val pcf_val :
    (class_field_kind * mutable_flag * string Astlib.Loc.t)
    -> t
  val pcf_inherit :
    string Astlib.Loc.t option
    -> class_expr
    -> override_flag
    -> t
end

and Class_field : sig
  type t = class_field

  type concrete =
    { pcf_attributes : attributes
    ; pcf_loc : Astlib.Location.t
    ; pcf_desc : class_field_desc
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcf_attributes:attributes
    -> pcf_loc:Astlib.Location.t
    -> pcf_desc:class_field_desc
    -> t
  val update :
    ?pcf_attributes:attributes
    -> ?pcf_loc:Astlib.Location.t
    -> ?pcf_desc:class_field_desc
    -> t -> t

  val pcf_attributes : t -> attributes
  val pcf_loc : t -> Astlib.Location.t
  val pcf_desc : t -> class_field_desc
end

and Class_structure : sig
  type t = class_structure

  type concrete =
    { pcstr_fields : class_field list
    ; pcstr_self : pattern
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcstr_fields:class_field list
    -> pcstr_self:pattern
    -> t
  val update :
    ?pcstr_fields:class_field list
    -> ?pcstr_self:pattern
    -> t -> t

  val pcstr_fields : t -> class_field list
  val pcstr_self : t -> pattern
end

and Class_expr_desc : sig
  type t = class_expr_desc

  type concrete =
    | Pcl_open of class_expr * longident_loc * override_flag
    | Pcl_extension of extension
    | Pcl_constraint of class_type * class_expr
    | Pcl_let of class_expr * value_binding list * rec_flag
    | Pcl_apply of (expression * arg_label) list * class_expr
    | Pcl_fun of class_expr * pattern * expression option * arg_label
    | Pcl_structure of class_structure
    | Pcl_constr of core_type list * longident_loc

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcl_open :
    class_expr
    -> longident_loc
    -> override_flag
    -> t
  val pcl_extension :
    extension
    -> t
  val pcl_constraint :
    class_type
    -> class_expr
    -> t
  val pcl_let :
    class_expr
    -> value_binding list
    -> rec_flag
    -> t
  val pcl_apply :
    (expression * arg_label) list
    -> class_expr
    -> t
  val pcl_fun :
    class_expr
    -> pattern
    -> expression option
    -> arg_label
    -> t
  val pcl_structure :
    class_structure
    -> t
  val pcl_constr :
    core_type list
    -> longident_loc
    -> t
end

and Class_expr : sig
  type t = class_expr

  type concrete =
    { pcl_attributes : attributes
    ; pcl_loc : Astlib.Location.t
    ; pcl_desc : class_expr_desc
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcl_attributes:attributes
    -> pcl_loc:Astlib.Location.t
    -> pcl_desc:class_expr_desc
    -> t
  val update :
    ?pcl_attributes:attributes
    -> ?pcl_loc:Astlib.Location.t
    -> ?pcl_desc:class_expr_desc
    -> t -> t

  val pcl_attributes : t -> attributes
  val pcl_loc : t -> Astlib.Location.t
  val pcl_desc : t -> class_expr_desc
end

and Class_type_declaration : sig
  type t = class_type_declaration

  type concrete = class_type class_infos

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : class_type class_infos -> t
end

and Class_description : sig
  type t = class_description

  type concrete = class_type class_infos

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : class_type class_infos -> t
end

and Class_infos : sig
  type 'a t = 'a class_infos

  type 'a concrete =
    { pci_attributes : attributes
    ; pci_loc : Astlib.Location.t
    ; pci_expr : 'a
    ; pci_name : string Astlib.Loc.t
    ; pci_params : (variance * core_type) list
    ; pci_virt : virtual_flag
    }

  val of_concrete : 'a node concrete -> 'a node t
  val to_concrete : 'a node t -> 'a node concrete
  val to_concrete_opt : 'a node t -> 'a node concrete option

  val create :
    pci_attributes:attributes
    -> pci_loc:Astlib.Location.t
    -> pci_expr:'a node
    -> pci_name:string Astlib.Loc.t
    -> pci_params:(variance * core_type) list
    -> pci_virt:virtual_flag
    -> 'a node t
  val update :
    ?pci_attributes:attributes
    -> ?pci_loc:Astlib.Location.t
    -> ?pci_expr:'a node
    -> ?pci_name:string Astlib.Loc.t
    -> ?pci_params:(variance * core_type) list
    -> ?pci_virt:virtual_flag
    -> 'a node t -> 'a node t

  val pci_attributes : 'a node t -> attributes
  val pci_loc : 'a node t -> Astlib.Location.t
  val pci_expr : 'a node t -> 'a node
  val pci_name : 'a node t -> string Astlib.Loc.t
  val pci_params : 'a node t -> (variance * core_type) list
  val pci_virt : 'a node t -> virtual_flag
end

and Class_type_field_desc : sig
  type t = class_type_field_desc

  type concrete =
    | Pctf_extension of extension
    | Pctf_attribute of attribute
    | Pctf_constraint of (core_type * core_type)
    | Pctf_method of (core_type * virtual_flag * private_flag * string Astlib.Loc.t)
    | Pctf_val of (core_type * virtual_flag * mutable_flag * string Astlib.Loc.t)
    | Pctf_inherit of class_type

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pctf_extension :
    extension
    -> t
  val pctf_attribute :
    attribute
    -> t
  val pctf_constraint :
    (core_type * core_type)
    -> t
  val pctf_method :
    (core_type * virtual_flag * private_flag * string Astlib.Loc.t)
    -> t
  val pctf_val :
    (core_type * virtual_flag * mutable_flag * string Astlib.Loc.t)
    -> t
  val pctf_inherit :
    class_type
    -> t
end

and Class_type_field : sig
  type t = class_type_field

  type concrete =
    { pctf_attributes : attributes
    ; pctf_loc : Astlib.Location.t
    ; pctf_desc : class_type_field_desc
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pctf_attributes:attributes
    -> pctf_loc:Astlib.Location.t
    -> pctf_desc:class_type_field_desc
    -> t
  val update :
    ?pctf_attributes:attributes
    -> ?pctf_loc:Astlib.Location.t
    -> ?pctf_desc:class_type_field_desc
    -> t -> t

  val pctf_attributes : t -> attributes
  val pctf_loc : t -> Astlib.Location.t
  val pctf_desc : t -> class_type_field_desc
end

and Class_signature : sig
  type t = class_signature

  type concrete =
    { pcsig_fields : class_type_field list
    ; pcsig_self : core_type
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcsig_fields:class_type_field list
    -> pcsig_self:core_type
    -> t
  val update :
    ?pcsig_fields:class_type_field list
    -> ?pcsig_self:core_type
    -> t -> t

  val pcsig_fields : t -> class_type_field list
  val pcsig_self : t -> core_type
end

and Class_type_desc : sig
  type t = class_type_desc

  type concrete =
    | Pcty_open of class_type * longident_loc * override_flag
    | Pcty_extension of extension
    | Pcty_arrow of class_type * core_type * arg_label
    | Pcty_signature of class_signature
    | Pcty_constr of core_type list * longident_loc

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcty_open :
    class_type
    -> longident_loc
    -> override_flag
    -> t
  val pcty_extension :
    extension
    -> t
  val pcty_arrow :
    class_type
    -> core_type
    -> arg_label
    -> t
  val pcty_signature :
    class_signature
    -> t
  val pcty_constr :
    core_type list
    -> longident_loc
    -> t
end

and Class_type : sig
  type t = class_type

  type concrete =
    { pcty_attributes : attributes
    ; pcty_loc : Astlib.Location.t
    ; pcty_desc : class_type_desc
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcty_attributes:attributes
    -> pcty_loc:Astlib.Location.t
    -> pcty_desc:class_type_desc
    -> t
  val update :
    ?pcty_attributes:attributes
    -> ?pcty_loc:Astlib.Location.t
    -> ?pcty_desc:class_type_desc
    -> t -> t

  val pcty_attributes : t -> attributes
  val pcty_loc : t -> Astlib.Location.t
  val pcty_desc : t -> class_type_desc
end

and Extension_constructor_kind : sig
  type t = extension_constructor_kind

  type concrete =
    | Pext_rebind of longident_loc
    | Pext_decl of core_type option * constructor_arguments

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pext_rebind :
    longident_loc
    -> t
  val pext_decl :
    core_type option
    -> constructor_arguments
    -> t
end

and Extension_constructor : sig
  type t = extension_constructor

  type concrete =
    { pext_attributes : attributes
    ; pext_loc : Astlib.Location.t
    ; pext_kind : extension_constructor_kind
    ; pext_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pext_attributes:attributes
    -> pext_loc:Astlib.Location.t
    -> pext_kind:extension_constructor_kind
    -> pext_name:string Astlib.Loc.t
    -> t
  val update :
    ?pext_attributes:attributes
    -> ?pext_loc:Astlib.Location.t
    -> ?pext_kind:extension_constructor_kind
    -> ?pext_name:string Astlib.Loc.t
    -> t -> t

  val pext_attributes : t -> attributes
  val pext_loc : t -> Astlib.Location.t
  val pext_kind : t -> extension_constructor_kind
  val pext_name : t -> string Astlib.Loc.t
end

and Type_extension : sig
  type t = type_extension

  type concrete =
    { ptyext_attributes : attributes
    ; ptyext_private : private_flag
    ; ptyext_constructors : extension_constructor list
    ; ptyext_params : (variance * core_type) list
    ; ptyext_path : longident_loc
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ptyext_attributes:attributes
    -> ptyext_private:private_flag
    -> ptyext_constructors:extension_constructor list
    -> ptyext_params:(variance * core_type) list
    -> ptyext_path:longident_loc
    -> t
  val update :
    ?ptyext_attributes:attributes
    -> ?ptyext_private:private_flag
    -> ?ptyext_constructors:extension_constructor list
    -> ?ptyext_params:(variance * core_type) list
    -> ?ptyext_path:longident_loc
    -> t -> t

  val ptyext_attributes : t -> attributes
  val ptyext_private : t -> private_flag
  val ptyext_constructors : t -> extension_constructor list
  val ptyext_params : t -> (variance * core_type) list
  val ptyext_path : t -> longident_loc
end

and Constructor_arguments : sig
  type t = constructor_arguments

  type concrete =
    | Pcstr_record of label_declaration list
    | Pcstr_tuple of core_type list

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcstr_record :
    label_declaration list
    -> t
  val pcstr_tuple :
    core_type list
    -> t
end

and Constructor_declaration : sig
  type t = constructor_declaration

  type concrete =
    { pcd_attributes : attributes
    ; pcd_loc : Astlib.Location.t
    ; pcd_res : core_type option
    ; pcd_args : constructor_arguments
    ; pcd_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcd_attributes:attributes
    -> pcd_loc:Astlib.Location.t
    -> pcd_res:core_type option
    -> pcd_args:constructor_arguments
    -> pcd_name:string Astlib.Loc.t
    -> t
  val update :
    ?pcd_attributes:attributes
    -> ?pcd_loc:Astlib.Location.t
    -> ?pcd_res:core_type option
    -> ?pcd_args:constructor_arguments
    -> ?pcd_name:string Astlib.Loc.t
    -> t -> t

  val pcd_attributes : t -> attributes
  val pcd_loc : t -> Astlib.Location.t
  val pcd_res : t -> core_type option
  val pcd_args : t -> constructor_arguments
  val pcd_name : t -> string Astlib.Loc.t
end

and Label_declaration : sig
  type t = label_declaration

  type concrete =
    { pld_attributes : attributes
    ; pld_loc : Astlib.Location.t
    ; pld_type : core_type
    ; pld_mutable : mutable_flag
    ; pld_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pld_attributes:attributes
    -> pld_loc:Astlib.Location.t
    -> pld_type:core_type
    -> pld_mutable:mutable_flag
    -> pld_name:string Astlib.Loc.t
    -> t
  val update :
    ?pld_attributes:attributes
    -> ?pld_loc:Astlib.Location.t
    -> ?pld_type:core_type
    -> ?pld_mutable:mutable_flag
    -> ?pld_name:string Astlib.Loc.t
    -> t -> t

  val pld_attributes : t -> attributes
  val pld_loc : t -> Astlib.Location.t
  val pld_type : t -> core_type
  val pld_mutable : t -> mutable_flag
  val pld_name : t -> string Astlib.Loc.t
end

and Type_kind : sig
  type t = type_kind

  type concrete =
    | Ptype_open
    | Ptype_record of label_declaration list
    | Ptype_variant of constructor_declaration list
    | Ptype_abstract

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ptype_open : t
  val ptype_record :
    label_declaration list
    -> t
  val ptype_variant :
    constructor_declaration list
    -> t
  val ptype_abstract : t
end

and Type_declaration : sig
  type t = type_declaration

  type concrete =
    { ptype_loc : Astlib.Location.t
    ; ptype_attributes : attributes
    ; ptype_manifest : core_type option
    ; ptype_private : private_flag
    ; ptype_kind : type_kind
    ; ptype_cstrs : (Astlib.Location.t * core_type * core_type) list
    ; ptype_params : (variance * core_type) list
    ; ptype_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ptype_loc:Astlib.Location.t
    -> ptype_attributes:attributes
    -> ptype_manifest:core_type option
    -> ptype_private:private_flag
    -> ptype_kind:type_kind
    -> ptype_cstrs:(Astlib.Location.t * core_type * core_type) list
    -> ptype_params:(variance * core_type) list
    -> ptype_name:string Astlib.Loc.t
    -> t
  val update :
    ?ptype_loc:Astlib.Location.t
    -> ?ptype_attributes:attributes
    -> ?ptype_manifest:core_type option
    -> ?ptype_private:private_flag
    -> ?ptype_kind:type_kind
    -> ?ptype_cstrs:(Astlib.Location.t * core_type * core_type) list
    -> ?ptype_params:(variance * core_type) list
    -> ?ptype_name:string Astlib.Loc.t
    -> t -> t

  val ptype_loc : t -> Astlib.Location.t
  val ptype_attributes : t -> attributes
  val ptype_manifest : t -> core_type option
  val ptype_private : t -> private_flag
  val ptype_kind : t -> type_kind
  val ptype_cstrs : t -> (Astlib.Location.t * core_type * core_type) list
  val ptype_params : t -> (variance * core_type) list
  val ptype_name : t -> string Astlib.Loc.t
end

and Value_description : sig
  type t = value_description

  type concrete =
    { pval_loc : Astlib.Location.t
    ; pval_attributes : attributes
    ; pval_prim : string list
    ; pval_type : core_type
    ; pval_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pval_loc:Astlib.Location.t
    -> pval_attributes:attributes
    -> pval_prim:string list
    -> pval_type:core_type
    -> pval_name:string Astlib.Loc.t
    -> t
  val update :
    ?pval_loc:Astlib.Location.t
    -> ?pval_attributes:attributes
    -> ?pval_prim:string list
    -> ?pval_type:core_type
    -> ?pval_name:string Astlib.Loc.t
    -> t -> t

  val pval_loc : t -> Astlib.Location.t
  val pval_attributes : t -> attributes
  val pval_prim : t -> string list
  val pval_type : t -> core_type
  val pval_name : t -> string Astlib.Loc.t
end

and Case : sig
  type t = case

  type concrete =
    { pc_rhs : expression
    ; pc_guard : expression option
    ; pc_lhs : pattern
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pc_rhs:expression
    -> pc_guard:expression option
    -> pc_lhs:pattern
    -> t
  val update :
    ?pc_rhs:expression
    -> ?pc_guard:expression option
    -> ?pc_lhs:pattern
    -> t -> t

  val pc_rhs : t -> expression
  val pc_guard : t -> expression option
  val pc_lhs : t -> pattern
end

and Expression_desc : sig
  type t = expression_desc

  type concrete =
    | Pexp_unreachable
    | Pexp_extension of extension
    | Pexp_open of expression * longident_loc * override_flag
    | Pexp_pack of module_expr
    | Pexp_newtype of expression * string Astlib.Loc.t
    | Pexp_object of class_structure
    | Pexp_poly of core_type option * expression
    | Pexp_lazy of expression
    | Pexp_assert of expression
    | Pexp_letexception of expression * extension_constructor
    | Pexp_letmodule of expression * module_expr * string Astlib.Loc.t
    | Pexp_override of (expression * string Astlib.Loc.t) list
    | Pexp_setinstvar of expression * string Astlib.Loc.t
    | Pexp_new of longident_loc
    | Pexp_send of string Astlib.Loc.t * expression
    | Pexp_coerce of core_type * core_type option * expression
    | Pexp_constraint of core_type * expression
    | Pexp_for of expression * direction_flag * expression * expression * pattern
    | Pexp_while of expression * expression
    | Pexp_sequence of expression * expression
    | Pexp_ifthenelse of expression option * expression * expression
    | Pexp_array of expression list
    | Pexp_setfield of expression * longident_loc * expression
    | Pexp_field of longident_loc * expression
    | Pexp_record of expression option * (expression * longident_loc) list
    | Pexp_variant of expression option * string
    | Pexp_construct of expression option * longident_loc
    | Pexp_tuple of expression list
    | Pexp_try of case list * expression
    | Pexp_match of case list * expression
    | Pexp_apply of (expression * arg_label) list * expression
    | Pexp_fun of expression * pattern * expression option * arg_label
    | Pexp_function of case list
    | Pexp_let of expression * value_binding list * rec_flag
    | Pexp_constant of constant
    | Pexp_ident of longident_loc

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pexp_unreachable : t
  val pexp_extension :
    extension
    -> t
  val pexp_open :
    expression
    -> longident_loc
    -> override_flag
    -> t
  val pexp_pack :
    module_expr
    -> t
  val pexp_newtype :
    expression
    -> string Astlib.Loc.t
    -> t
  val pexp_object :
    class_structure
    -> t
  val pexp_poly :
    core_type option
    -> expression
    -> t
  val pexp_lazy :
    expression
    -> t
  val pexp_assert :
    expression
    -> t
  val pexp_letexception :
    expression
    -> extension_constructor
    -> t
  val pexp_letmodule :
    expression
    -> module_expr
    -> string Astlib.Loc.t
    -> t
  val pexp_override :
    (expression * string Astlib.Loc.t) list
    -> t
  val pexp_setinstvar :
    expression
    -> string Astlib.Loc.t
    -> t
  val pexp_new :
    longident_loc
    -> t
  val pexp_send :
    string Astlib.Loc.t
    -> expression
    -> t
  val pexp_coerce :
    core_type
    -> core_type option
    -> expression
    -> t
  val pexp_constraint :
    core_type
    -> expression
    -> t
  val pexp_for :
    expression
    -> direction_flag
    -> expression
    -> expression
    -> pattern
    -> t
  val pexp_while :
    expression
    -> expression
    -> t
  val pexp_sequence :
    expression
    -> expression
    -> t
  val pexp_ifthenelse :
    expression option
    -> expression
    -> expression
    -> t
  val pexp_array :
    expression list
    -> t
  val pexp_setfield :
    expression
    -> longident_loc
    -> expression
    -> t
  val pexp_field :
    longident_loc
    -> expression
    -> t
  val pexp_record :
    expression option
    -> (expression * longident_loc) list
    -> t
  val pexp_variant :
    expression option
    -> string
    -> t
  val pexp_construct :
    expression option
    -> longident_loc
    -> t
  val pexp_tuple :
    expression list
    -> t
  val pexp_try :
    case list
    -> expression
    -> t
  val pexp_match :
    case list
    -> expression
    -> t
  val pexp_apply :
    (expression * arg_label) list
    -> expression
    -> t
  val pexp_fun :
    expression
    -> pattern
    -> expression option
    -> arg_label
    -> t
  val pexp_function :
    case list
    -> t
  val pexp_let :
    expression
    -> value_binding list
    -> rec_flag
    -> t
  val pexp_constant :
    constant
    -> t
  val pexp_ident :
    longident_loc
    -> t
end

and Expression : sig
  type t = expression

  type concrete =
    { pexp_attributes : attributes
    ; pexp_loc : Astlib.Location.t
    ; pexp_desc : expression_desc
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pexp_attributes:attributes
    -> pexp_loc:Astlib.Location.t
    -> pexp_desc:expression_desc
    -> t
  val update :
    ?pexp_attributes:attributes
    -> ?pexp_loc:Astlib.Location.t
    -> ?pexp_desc:expression_desc
    -> t -> t

  val pexp_attributes : t -> attributes
  val pexp_loc : t -> Astlib.Location.t
  val pexp_desc : t -> expression_desc
end

and Pattern_desc : sig
  type t = pattern_desc

  type concrete =
    | Ppat_open of pattern * longident_loc
    | Ppat_extension of extension
    | Ppat_exception of pattern
    | Ppat_unpack of string Astlib.Loc.t
    | Ppat_lazy of pattern
    | Ppat_type of longident_loc
    | Ppat_constraint of core_type * pattern
    | Ppat_or of pattern * pattern
    | Ppat_array of pattern list
    | Ppat_record of closed_flag * (pattern * longident_loc) list
    | Ppat_variant of pattern option * string
    | Ppat_construct of pattern option * longident_loc
    | Ppat_tuple of pattern list
    | Ppat_interval of constant * constant
    | Ppat_constant of constant
    | Ppat_alias of string Astlib.Loc.t * pattern
    | Ppat_var of string Astlib.Loc.t
    | Ppat_any

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ppat_open :
    pattern
    -> longident_loc
    -> t
  val ppat_extension :
    extension
    -> t
  val ppat_exception :
    pattern
    -> t
  val ppat_unpack :
    string Astlib.Loc.t
    -> t
  val ppat_lazy :
    pattern
    -> t
  val ppat_type :
    longident_loc
    -> t
  val ppat_constraint :
    core_type
    -> pattern
    -> t
  val ppat_or :
    pattern
    -> pattern
    -> t
  val ppat_array :
    pattern list
    -> t
  val ppat_record :
    closed_flag
    -> (pattern * longident_loc) list
    -> t
  val ppat_variant :
    pattern option
    -> string
    -> t
  val ppat_construct :
    pattern option
    -> longident_loc
    -> t
  val ppat_tuple :
    pattern list
    -> t
  val ppat_interval :
    constant
    -> constant
    -> t
  val ppat_constant :
    constant
    -> t
  val ppat_alias :
    string Astlib.Loc.t
    -> pattern
    -> t
  val ppat_var :
    string Astlib.Loc.t
    -> t
  val ppat_any : t
end

and Pattern : sig
  type t = pattern

  type concrete =
    { ppat_attributes : attributes
    ; ppat_loc : Astlib.Location.t
    ; ppat_desc : pattern_desc
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ppat_attributes:attributes
    -> ppat_loc:Astlib.Location.t
    -> ppat_desc:pattern_desc
    -> t
  val update :
    ?ppat_attributes:attributes
    -> ?ppat_loc:Astlib.Location.t
    -> ?ppat_desc:pattern_desc
    -> t -> t

  val ppat_attributes : t -> attributes
  val ppat_loc : t -> Astlib.Location.t
  val ppat_desc : t -> pattern_desc
end

and Object_field : sig
  type t = object_field

  type concrete =
    | Oinherit of core_type
    | Otag of core_type * attributes * string Astlib.Loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val oinherit :
    core_type
    -> t
  val otag :
    core_type
    -> attributes
    -> string Astlib.Loc.t
    -> t
end

and Row_field : sig
  type t = row_field

  type concrete =
    | Rinherit of core_type
    | Rtag of core_type list * bool * attributes * string Astlib.Loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val rinherit :
    core_type
    -> t
  val rtag :
    core_type list
    -> bool
    -> attributes
    -> string Astlib.Loc.t
    -> t
end

and Package_type : sig
  type t = package_type

  type concrete = ((core_type * longident_loc) list * longident_loc)

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : ((core_type * longident_loc) list * longident_loc) -> t
end

and Core_type_desc : sig
  type t = core_type_desc

  type concrete =
    | Ptyp_extension of extension
    | Ptyp_package of package_type
    | Ptyp_poly of core_type * string Astlib.Loc.t list
    | Ptyp_variant of string list option * closed_flag * row_field list
    | Ptyp_alias of string * core_type
    | Ptyp_class of core_type list * longident_loc
    | Ptyp_object of closed_flag * object_field list
    | Ptyp_constr of core_type list * longident_loc
    | Ptyp_tuple of core_type list
    | Ptyp_arrow of core_type * core_type * arg_label
    | Ptyp_var of string
    | Ptyp_any

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ptyp_extension :
    extension
    -> t
  val ptyp_package :
    package_type
    -> t
  val ptyp_poly :
    core_type
    -> string Astlib.Loc.t list
    -> t
  val ptyp_variant :
    string list option
    -> closed_flag
    -> row_field list
    -> t
  val ptyp_alias :
    string
    -> core_type
    -> t
  val ptyp_class :
    core_type list
    -> longident_loc
    -> t
  val ptyp_object :
    closed_flag
    -> object_field list
    -> t
  val ptyp_constr :
    core_type list
    -> longident_loc
    -> t
  val ptyp_tuple :
    core_type list
    -> t
  val ptyp_arrow :
    core_type
    -> core_type
    -> arg_label
    -> t
  val ptyp_var :
    string
    -> t
  val ptyp_any : t
end

and Core_type : sig
  type t = core_type

  type concrete =
    { ptyp_attributes : attributes
    ; ptyp_loc : Astlib.Location.t
    ; ptyp_desc : core_type_desc
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ptyp_attributes:attributes
    -> ptyp_loc:Astlib.Location.t
    -> ptyp_desc:core_type_desc
    -> t
  val update :
    ?ptyp_attributes:attributes
    -> ?ptyp_loc:Astlib.Location.t
    -> ?ptyp_desc:core_type_desc
    -> t -> t

  val ptyp_attributes : t -> attributes
  val ptyp_loc : t -> Astlib.Location.t
  val ptyp_desc : t -> core_type_desc
end

and Payload : sig
  type t = payload

  type concrete =
    | PPat of expression option * pattern
    | PTyp of core_type
    | PSig of signature
    | PStr of structure

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pPat :
    expression option
    -> pattern
    -> t
  val pTyp :
    core_type
    -> t
  val pSig :
    signature
    -> t
  val pStr :
    structure
    -> t
end

and Attributes : sig
  type t = attributes

  type concrete = attribute list

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : attribute list -> t
end

and Extension : sig
  type t = extension

  type concrete = (payload * string Astlib.Loc.t)

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : (payload * string Astlib.Loc.t) -> t
end

and Attribute : sig
  type t = attribute

  type concrete = (payload * string Astlib.Loc.t)

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : (payload * string Astlib.Loc.t) -> t
end

and Constant : sig
  type t = constant

  type concrete =
    | Pconst_float of char option * string
    | Pconst_string of string option * string
    | Pconst_char of char
    | Pconst_integer of char option * string

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pconst_float :
    char option
    -> string
    -> t
  val pconst_string :
    string option
    -> string
    -> t
  val pconst_char :
    char
    -> t
  val pconst_integer :
    char option
    -> string
    -> t
end

and Variance : sig
  type t = variance

  type concrete =
    | Invariant
    | Contravariant
    | Covariant

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val invariant : t
  val contravariant : t
  val covariant : t
end

and Arg_label : sig
  type t = arg_label

  type concrete =
    | Optional of string
    | Labelled of string
    | Nolabel

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val optional :
    string
    -> t
  val labelled :
    string
    -> t
  val nolabel : t
end

and Closed_flag : sig
  type t = closed_flag

  type concrete =
    | Open
    | Closed

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val open_ : t
  val closed : t
end

and Override_flag : sig
  type t = override_flag

  type concrete =
    | Fresh
    | Override

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val fresh : t
  val override : t
end

and Virtual_flag : sig
  type t = virtual_flag

  type concrete =
    | Concrete
    | Virtual

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val concrete : t
  val virtual_ : t
end

and Mutable_flag : sig
  type t = mutable_flag

  type concrete =
    | Mutable
    | Immutable

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val mutable_ : t
  val immutable : t
end

and Private_flag : sig
  type t = private_flag

  type concrete =
    | Public
    | Private

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val public : t
  val private_ : t
end

and Direction_flag : sig
  type t = direction_flag

  type concrete =
    | Downto
    | Upto

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val downto_ : t
  val upto : t
end

and Rec_flag : sig
  type t = rec_flag

  type concrete =
    | Recursive
    | Nonrecursive

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val recursive : t
  val nonrecursive : t
end

and Longident_loc : sig
  type t = longident_loc

  type concrete = longident Astlib.Loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : longident Astlib.Loc.t -> t
end

and Longident : sig
  type t = longident

  type concrete =
    | Lapply of longident * longident
    | Ldot of string * longident
    | Lident of string

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val lapply :
    longident
    -> longident
    -> t
  val ldot :
    string
    -> longident
    -> t
  val lident :
    string
    -> t
end
(*$*)
