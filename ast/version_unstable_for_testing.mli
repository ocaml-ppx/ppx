open Unversioned.Types

(*$ Ppx_ast_cinaps.print_version_mli (Astlib.Version.of_string "unstable_for_testing") *)
module rec Directive_argument : sig
  type t = directive_argument

  type concrete =
    | Pdir_bool of bool
    | Pdir_ident of Longident.t
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
    Longident.t
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
    | Ptop_dir of Directive_argument.t * string
    | Ptop_def of Structure.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ptop_dir :
    Directive_argument.t
    -> string
    -> t
  val ptop_def :
    Structure.t
    -> t
end

and Module_binding : sig
  type t = module_binding

  type concrete =
    { pmb_loc : Astlib.Location.t
    ; pmb_attributes : Attributes.t
    ; pmb_expr : Module_expr.t
    ; pmb_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmb_loc:Astlib.Location.t
    -> pmb_attributes:Attributes.t
    -> pmb_expr:Module_expr.t
    -> pmb_name:string Astlib.Loc.t
    -> t
end

and Value_binding : sig
  type t = value_binding

  type concrete =
    { pvb_loc : Astlib.Location.t
    ; pvb_attributes : Attributes.t
    ; pvb_expr : Expression.t
    ; pvb_pat : Pattern.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pvb_loc:Astlib.Location.t
    -> pvb_attributes:Attributes.t
    -> pvb_expr:Expression.t
    -> pvb_pat:Pattern.t
    -> t
end

and Structure_item_desc : sig
  type t = structure_item_desc

  type concrete =
    | Pstr_extension of Attributes.t * Extension.t
    | Pstr_attribute of Attribute.t
    | Pstr_include of Include_declaration.t
    | Pstr_class_type of Class_type_declaration.t list
    | Pstr_class of Class_declaration.t list
    | Pstr_open of Open_description.t
    | Pstr_modtype of Module_type_declaration.t
    | Pstr_recmodule of Module_binding.t list
    | Pstr_module of Module_binding.t
    | Pstr_exception of Extension_constructor.t
    | Pstr_typext of Type_extension.t
    | Pstr_type of Type_declaration.t list * Rec_flag.t
    | Pstr_primitive of Value_description.t
    | Pstr_value of Value_binding.t list * Rec_flag.t
    | Pstr_eval of Attributes.t * Expression.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pstr_extension :
    Attributes.t
    -> Extension.t
    -> t
  val pstr_attribute :
    Attribute.t
    -> t
  val pstr_include :
    Include_declaration.t
    -> t
  val pstr_class_type :
    Class_type_declaration.t list
    -> t
  val pstr_class :
    Class_declaration.t list
    -> t
  val pstr_open :
    Open_description.t
    -> t
  val pstr_modtype :
    Module_type_declaration.t
    -> t
  val pstr_recmodule :
    Module_binding.t list
    -> t
  val pstr_module :
    Module_binding.t
    -> t
  val pstr_exception :
    Extension_constructor.t
    -> t
  val pstr_typext :
    Type_extension.t
    -> t
  val pstr_type :
    Type_declaration.t list
    -> Rec_flag.t
    -> t
  val pstr_primitive :
    Value_description.t
    -> t
  val pstr_value :
    Value_binding.t list
    -> Rec_flag.t
    -> t
  val pstr_eval :
    Attributes.t
    -> Expression.t
    -> t
end

and Structure_item : sig
  type t = structure_item

  type concrete =
    { pstr_loc : Astlib.Location.t
    ; pstr_desc : Structure_item_desc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pstr_loc:Astlib.Location.t
    -> pstr_desc:Structure_item_desc.t
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

and Module_expr_desc : sig
  type t = module_expr_desc

  type concrete =
    | Pmod_extension of Extension.t
    | Pmod_unpack of Expression.t
    | Pmod_constraint of Module_type.t * Module_expr.t
    | Pmod_apply of Module_expr.t * Module_expr.t
    | Pmod_functor of Module_expr.t * Module_type.t option * string Astlib.Loc.t
    | Pmod_structure of Structure.t
    | Pmod_ident of Longident_loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pmod_extension :
    Extension.t
    -> t
  val pmod_unpack :
    Expression.t
    -> t
  val pmod_constraint :
    Module_type.t
    -> Module_expr.t
    -> t
  val pmod_apply :
    Module_expr.t
    -> Module_expr.t
    -> t
  val pmod_functor :
    Module_expr.t
    -> Module_type.t option
    -> string Astlib.Loc.t
    -> t
  val pmod_structure :
    Structure.t
    -> t
  val pmod_ident :
    Longident_loc.t
    -> t
end

and Module_expr : sig
  type t = module_expr

  type concrete =
    { pmod_attributes : Attributes.t
    ; pmod_loc : Astlib.Location.t
    ; pmod_desc : Module_expr_desc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmod_attributes:Attributes.t
    -> pmod_loc:Astlib.Location.t
    -> pmod_desc:Module_expr_desc.t
    -> t
end

and With_constraint : sig
  type t = with_constraint

  type concrete =
    | Pwith_modsubst of Longident_loc.t * Longident_loc.t
    | Pwith_typesubst of Type_declaration.t * Longident_loc.t
    | Pwith_module of Longident_loc.t * Longident_loc.t
    | Pwith_type of Type_declaration.t * Longident_loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pwith_modsubst :
    Longident_loc.t
    -> Longident_loc.t
    -> t
  val pwith_typesubst :
    Type_declaration.t
    -> Longident_loc.t
    -> t
  val pwith_module :
    Longident_loc.t
    -> Longident_loc.t
    -> t
  val pwith_type :
    Type_declaration.t
    -> Longident_loc.t
    -> t
end

and Include_declaration : sig
  type t = include_declaration

  type concrete = Module_expr.t Include_infos.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : Module_expr.t Include_infos.t -> t
end

and Include_description : sig
  type t = include_description

  type concrete = Module_type.t Include_infos.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : Module_type.t Include_infos.t -> t
end

and Include_infos : sig
  type 'a t = 'a include_infos

  type 'a concrete =
    { pincl_attributes : Attributes.t
    ; pincl_loc : Astlib.Location.t
    ; pincl_mod : 'a
    }

  val of_concrete : 'a node concrete -> 'a node t
  val to_concrete : 'a node t -> 'a node concrete
  val to_concrete_opt : 'a node t -> 'a node concrete option

  val create :
    pincl_attributes:Attributes.t
    -> pincl_loc:Astlib.Location.t
    -> pincl_mod:'a node
    -> 'a node t
end

and Open_description : sig
  type t = open_description

  type concrete =
    { popen_attributes : Attributes.t
    ; popen_loc : Astlib.Location.t
    ; popen_override : Override_flag.t
    ; popen_lid : Longident_loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    popen_attributes:Attributes.t
    -> popen_loc:Astlib.Location.t
    -> popen_override:Override_flag.t
    -> popen_lid:Longident_loc.t
    -> t
end

and Module_type_declaration : sig
  type t = module_type_declaration

  type concrete =
    { pmtd_loc : Astlib.Location.t
    ; pmtd_attributes : Attributes.t
    ; pmtd_type : Module_type.t option
    ; pmtd_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmtd_loc:Astlib.Location.t
    -> pmtd_attributes:Attributes.t
    -> pmtd_type:Module_type.t option
    -> pmtd_name:string Astlib.Loc.t
    -> t
end

and Module_declaration : sig
  type t = module_declaration

  type concrete =
    { pmd_loc : Astlib.Location.t
    ; pmd_attributes : Attributes.t
    ; pmd_type : Module_type.t
    ; pmd_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmd_loc:Astlib.Location.t
    -> pmd_attributes:Attributes.t
    -> pmd_type:Module_type.t
    -> pmd_name:string Astlib.Loc.t
    -> t
end

and Signature_item_desc : sig
  type t = signature_item_desc

  type concrete =
    | Psig_extension of Attributes.t * Extension.t
    | Psig_attribute of Attribute.t
    | Psig_class_type of Class_type_declaration.t list
    | Psig_class of Class_description.t list
    | Psig_include of Include_description.t
    | Psig_open of Open_description.t
    | Psig_modtype of Module_type_declaration.t
    | Psig_recmodule of Module_declaration.t list
    | Psig_module of Module_declaration.t
    | Psig_exception of Extension_constructor.t
    | Psig_typext of Type_extension.t
    | Psig_type of Type_declaration.t list * Rec_flag.t
    | Psig_value of Value_description.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val psig_extension :
    Attributes.t
    -> Extension.t
    -> t
  val psig_attribute :
    Attribute.t
    -> t
  val psig_class_type :
    Class_type_declaration.t list
    -> t
  val psig_class :
    Class_description.t list
    -> t
  val psig_include :
    Include_description.t
    -> t
  val psig_open :
    Open_description.t
    -> t
  val psig_modtype :
    Module_type_declaration.t
    -> t
  val psig_recmodule :
    Module_declaration.t list
    -> t
  val psig_module :
    Module_declaration.t
    -> t
  val psig_exception :
    Extension_constructor.t
    -> t
  val psig_typext :
    Type_extension.t
    -> t
  val psig_type :
    Type_declaration.t list
    -> Rec_flag.t
    -> t
  val psig_value :
    Value_description.t
    -> t
end

and Signature_item : sig
  type t = signature_item

  type concrete =
    { psig_loc : Astlib.Location.t
    ; psig_desc : Signature_item_desc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    psig_loc:Astlib.Location.t
    -> psig_desc:Signature_item_desc.t
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

and Module_type_desc : sig
  type t = module_type_desc

  type concrete =
    | Pmty_alias of Longident_loc.t
    | Pmty_extension of Extension.t
    | Pmty_typeof of Module_expr.t
    | Pmty_with of With_constraint.t list * Module_type.t
    | Pmty_functor of Module_type.t * Module_type.t option * string Astlib.Loc.t
    | Pmty_signature of Signature.t
    | Pmty_ident of Longident_loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pmty_alias :
    Longident_loc.t
    -> t
  val pmty_extension :
    Extension.t
    -> t
  val pmty_typeof :
    Module_expr.t
    -> t
  val pmty_with :
    With_constraint.t list
    -> Module_type.t
    -> t
  val pmty_functor :
    Module_type.t
    -> Module_type.t option
    -> string Astlib.Loc.t
    -> t
  val pmty_signature :
    Signature.t
    -> t
  val pmty_ident :
    Longident_loc.t
    -> t
end

and Module_type : sig
  type t = module_type

  type concrete =
    { pmty_attributes : Attributes.t
    ; pmty_loc : Astlib.Location.t
    ; pmty_desc : Module_type_desc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pmty_attributes:Attributes.t
    -> pmty_loc:Astlib.Location.t
    -> pmty_desc:Module_type_desc.t
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

and Class_field_kind : sig
  type t = class_field_kind

  type concrete =
    | Cfk_concrete of Expression.t * Override_flag.t
    | Cfk_virtual of Core_type.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val cfk_concrete :
    Expression.t
    -> Override_flag.t
    -> t
  val cfk_virtual :
    Core_type.t
    -> t
end

and Class_field_desc : sig
  type t = class_field_desc

  type concrete =
    | Pcf_extension of Extension.t
    | Pcf_attribute of Attribute.t
    | Pcf_initializer of Expression.t
    | Pcf_constraint of (Core_type.t * Core_type.t)
    | Pcf_method of (Class_field_kind.t * Private_flag.t * string Astlib.Loc.t)
    | Pcf_val of (Class_field_kind.t * Mutable_flag.t * string Astlib.Loc.t)
    | Pcf_inherit of string Astlib.Loc.t option * Class_expr.t * Override_flag.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcf_extension :
    Extension.t
    -> t
  val pcf_attribute :
    Attribute.t
    -> t
  val pcf_initializer :
    Expression.t
    -> t
  val pcf_constraint :
    (Core_type.t * Core_type.t)
    -> t
  val pcf_method :
    (Class_field_kind.t * Private_flag.t * string Astlib.Loc.t)
    -> t
  val pcf_val :
    (Class_field_kind.t * Mutable_flag.t * string Astlib.Loc.t)
    -> t
  val pcf_inherit :
    string Astlib.Loc.t option
    -> Class_expr.t
    -> Override_flag.t
    -> t
end

and Class_field : sig
  type t = class_field

  type concrete =
    { pcf_attributes : Attributes.t
    ; pcf_loc : Astlib.Location.t
    ; pcf_desc : Class_field_desc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcf_attributes:Attributes.t
    -> pcf_loc:Astlib.Location.t
    -> pcf_desc:Class_field_desc.t
    -> t
end

and Class_structure : sig
  type t = class_structure

  type concrete =
    { pcstr_fields : Class_field.t list
    ; pcstr_self : Pattern.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcstr_fields:Class_field.t list
    -> pcstr_self:Pattern.t
    -> t
end

and Class_expr_desc : sig
  type t = class_expr_desc

  type concrete =
    | Pcl_open of Class_expr.t * Longident_loc.t * Override_flag.t
    | Pcl_extension of Extension.t
    | Pcl_constraint of Class_type.t * Class_expr.t
    | Pcl_let of Class_expr.t * Value_binding.t list * Rec_flag.t
    | Pcl_apply of (Expression.t * Arg_label.t) list * Class_expr.t
    | Pcl_fun of Class_expr.t * Pattern.t * Expression.t option * Arg_label.t
    | Pcl_structure of Class_structure.t
    | Pcl_constr of Core_type.t list * Longident_loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcl_open :
    Class_expr.t
    -> Longident_loc.t
    -> Override_flag.t
    -> t
  val pcl_extension :
    Extension.t
    -> t
  val pcl_constraint :
    Class_type.t
    -> Class_expr.t
    -> t
  val pcl_let :
    Class_expr.t
    -> Value_binding.t list
    -> Rec_flag.t
    -> t
  val pcl_apply :
    (Expression.t * Arg_label.t) list
    -> Class_expr.t
    -> t
  val pcl_fun :
    Class_expr.t
    -> Pattern.t
    -> Expression.t option
    -> Arg_label.t
    -> t
  val pcl_structure :
    Class_structure.t
    -> t
  val pcl_constr :
    Core_type.t list
    -> Longident_loc.t
    -> t
end

and Class_expr : sig
  type t = class_expr

  type concrete =
    { pcl_attributes : Attributes.t
    ; pcl_loc : Astlib.Location.t
    ; pcl_desc : Class_expr_desc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcl_attributes:Attributes.t
    -> pcl_loc:Astlib.Location.t
    -> pcl_desc:Class_expr_desc.t
    -> t
end

and Class_type_declaration : sig
  type t = class_type_declaration

  type concrete = Class_type.t Class_infos.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : Class_type.t Class_infos.t -> t
end

and Class_description : sig
  type t = class_description

  type concrete = Class_type.t Class_infos.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : Class_type.t Class_infos.t -> t
end

and Class_infos : sig
  type 'a t = 'a class_infos

  type 'a concrete =
    { pci_attributes : Attributes.t
    ; pci_loc : Astlib.Location.t
    ; pci_expr : 'a
    ; pci_name : string Astlib.Loc.t
    ; pci_params : (Variance.t * Core_type.t) list
    ; pci_virt : Virtual_flag.t
    }

  val of_concrete : 'a node concrete -> 'a node t
  val to_concrete : 'a node t -> 'a node concrete
  val to_concrete_opt : 'a node t -> 'a node concrete option

  val create :
    pci_attributes:Attributes.t
    -> pci_loc:Astlib.Location.t
    -> pci_expr:'a node
    -> pci_name:string Astlib.Loc.t
    -> pci_params:(Variance.t * Core_type.t) list
    -> pci_virt:Virtual_flag.t
    -> 'a node t
end

and Class_type_field_desc : sig
  type t = class_type_field_desc

  type concrete =
    | Pctf_extension of Extension.t
    | Pctf_attribute of Attribute.t
    | Pctf_constraint of (Core_type.t * Core_type.t)
    | Pctf_method of (Core_type.t * Virtual_flag.t * Private_flag.t * string Astlib.Loc.t)
    | Pctf_val of (Core_type.t * Virtual_flag.t * Mutable_flag.t * string Astlib.Loc.t)
    | Pctf_inherit of Class_type.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pctf_extension :
    Extension.t
    -> t
  val pctf_attribute :
    Attribute.t
    -> t
  val pctf_constraint :
    (Core_type.t * Core_type.t)
    -> t
  val pctf_method :
    (Core_type.t * Virtual_flag.t * Private_flag.t * string Astlib.Loc.t)
    -> t
  val pctf_val :
    (Core_type.t * Virtual_flag.t * Mutable_flag.t * string Astlib.Loc.t)
    -> t
  val pctf_inherit :
    Class_type.t
    -> t
end

and Class_type_field : sig
  type t = class_type_field

  type concrete =
    { pctf_attributes : Attributes.t
    ; pctf_loc : Astlib.Location.t
    ; pctf_desc : Class_type_field_desc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pctf_attributes:Attributes.t
    -> pctf_loc:Astlib.Location.t
    -> pctf_desc:Class_type_field_desc.t
    -> t
end

and Class_signature : sig
  type t = class_signature

  type concrete =
    { pcsig_fields : Class_type_field.t list
    ; pcsig_self : Core_type.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcsig_fields:Class_type_field.t list
    -> pcsig_self:Core_type.t
    -> t
end

and Class_type_desc : sig
  type t = class_type_desc

  type concrete =
    | Pcty_open of Class_type.t * Longident_loc.t * Override_flag.t
    | Pcty_extension of Extension.t
    | Pcty_arrow of Class_type.t * Core_type.t * Arg_label.t
    | Pcty_signature of Class_signature.t
    | Pcty_constr of Core_type.t list * Longident_loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcty_open :
    Class_type.t
    -> Longident_loc.t
    -> Override_flag.t
    -> t
  val pcty_extension :
    Extension.t
    -> t
  val pcty_arrow :
    Class_type.t
    -> Core_type.t
    -> Arg_label.t
    -> t
  val pcty_signature :
    Class_signature.t
    -> t
  val pcty_constr :
    Core_type.t list
    -> Longident_loc.t
    -> t
end

and Class_type : sig
  type t = class_type

  type concrete =
    { pcty_attributes : Attributes.t
    ; pcty_loc : Astlib.Location.t
    ; pcty_desc : Class_type_desc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcty_attributes:Attributes.t
    -> pcty_loc:Astlib.Location.t
    -> pcty_desc:Class_type_desc.t
    -> t
end

and Extension_constructor_kind : sig
  type t = extension_constructor_kind

  type concrete =
    | Pext_rebind of Longident_loc.t
    | Pext_decl of Core_type.t option * Constructor_arguments.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pext_rebind :
    Longident_loc.t
    -> t
  val pext_decl :
    Core_type.t option
    -> Constructor_arguments.t
    -> t
end

and Extension_constructor : sig
  type t = extension_constructor

  type concrete =
    { pext_attributes : Attributes.t
    ; pext_loc : Astlib.Location.t
    ; pext_kind : Extension_constructor_kind.t
    ; pext_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pext_attributes:Attributes.t
    -> pext_loc:Astlib.Location.t
    -> pext_kind:Extension_constructor_kind.t
    -> pext_name:string Astlib.Loc.t
    -> t
end

and Type_extension : sig
  type t = type_extension

  type concrete =
    { ptyext_attributes : Attributes.t
    ; ptyext_private : Private_flag.t
    ; ptyext_constructors : Extension_constructor.t list
    ; ptyext_params : (Variance.t * Core_type.t) list
    ; ptyext_path : Longident_loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ptyext_attributes:Attributes.t
    -> ptyext_private:Private_flag.t
    -> ptyext_constructors:Extension_constructor.t list
    -> ptyext_params:(Variance.t * Core_type.t) list
    -> ptyext_path:Longident_loc.t
    -> t
end

and Constructor_arguments : sig
  type t = constructor_arguments

  type concrete =
    | Pcstr_record of Label_declaration.t list
    | Pcstr_tuple of Core_type.t list

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pcstr_record :
    Label_declaration.t list
    -> t
  val pcstr_tuple :
    Core_type.t list
    -> t
end

and Constructor_declaration : sig
  type t = constructor_declaration

  type concrete =
    { pcd_attributes : Attributes.t
    ; pcd_loc : Astlib.Location.t
    ; pcd_res : Core_type.t option
    ; pcd_args : Constructor_arguments.t
    ; pcd_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pcd_attributes:Attributes.t
    -> pcd_loc:Astlib.Location.t
    -> pcd_res:Core_type.t option
    -> pcd_args:Constructor_arguments.t
    -> pcd_name:string Astlib.Loc.t
    -> t
end

and Label_declaration : sig
  type t = label_declaration

  type concrete =
    { pld_attributes : Attributes.t
    ; pld_loc : Astlib.Location.t
    ; pld_type : Core_type.t
    ; pld_mutable : Mutable_flag.t
    ; pld_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pld_attributes:Attributes.t
    -> pld_loc:Astlib.Location.t
    -> pld_type:Core_type.t
    -> pld_mutable:Mutable_flag.t
    -> pld_name:string Astlib.Loc.t
    -> t
end

and Type_kind : sig
  type t = type_kind

  type concrete =
    | Ptype_open
    | Ptype_record of Label_declaration.t list
    | Ptype_variant of Constructor_declaration.t list
    | Ptype_abstract

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ptype_open : t
  val ptype_record :
    Label_declaration.t list
    -> t
  val ptype_variant :
    Constructor_declaration.t list
    -> t
  val ptype_abstract : t
end

and Type_declaration : sig
  type t = type_declaration

  type concrete =
    { ptype_loc : Astlib.Location.t
    ; ptype_attributes : Attributes.t
    ; ptype_manifest : Core_type.t option
    ; ptype_private : Private_flag.t
    ; ptype_kind : Type_kind.t
    ; ptype_cstrs : (Astlib.Location.t * Core_type.t * Core_type.t) list
    ; ptype_params : (Variance.t * Core_type.t) list
    ; ptype_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ptype_loc:Astlib.Location.t
    -> ptype_attributes:Attributes.t
    -> ptype_manifest:Core_type.t option
    -> ptype_private:Private_flag.t
    -> ptype_kind:Type_kind.t
    -> ptype_cstrs:(Astlib.Location.t * Core_type.t * Core_type.t) list
    -> ptype_params:(Variance.t * Core_type.t) list
    -> ptype_name:string Astlib.Loc.t
    -> t
end

and Value_description : sig
  type t = value_description

  type concrete =
    { pval_loc : Astlib.Location.t
    ; pval_attributes : Attributes.t
    ; pval_prim : string list
    ; pval_type : Core_type.t
    ; pval_name : string Astlib.Loc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pval_loc:Astlib.Location.t
    -> pval_attributes:Attributes.t
    -> pval_prim:string list
    -> pval_type:Core_type.t
    -> pval_name:string Astlib.Loc.t
    -> t
end

and Case : sig
  type t = case

  type concrete =
    { pc_rhs : Expression.t
    ; pc_guard : Expression.t option
    ; pc_lhs : Pattern.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pc_rhs:Expression.t
    -> pc_guard:Expression.t option
    -> pc_lhs:Pattern.t
    -> t
end

and Expression_desc : sig
  type t = expression_desc

  type concrete =
    | Pexp_unreachable
    | Pexp_extension of Extension.t
    | Pexp_open of Expression.t * Longident_loc.t * Override_flag.t
    | Pexp_pack of Module_expr.t
    | Pexp_newtype of Expression.t * string Astlib.Loc.t
    | Pexp_object of Class_structure.t
    | Pexp_poly of Core_type.t option * Expression.t
    | Pexp_lazy of Expression.t
    | Pexp_assert of Expression.t
    | Pexp_letexception of Expression.t * Extension_constructor.t
    | Pexp_letmodule of Expression.t * Module_expr.t * string Astlib.Loc.t
    | Pexp_override of (Expression.t * string Astlib.Loc.t) list
    | Pexp_setinstvar of Expression.t * string Astlib.Loc.t
    | Pexp_new of Longident_loc.t
    | Pexp_send of string Astlib.Loc.t * Expression.t
    | Pexp_coerce of Core_type.t * Core_type.t option * Expression.t
    | Pexp_constraint of Core_type.t * Expression.t
    | Pexp_for of Expression.t * Direction_flag.t * Expression.t * Expression.t * Pattern.t
    | Pexp_while of Expression.t * Expression.t
    | Pexp_sequence of Expression.t * Expression.t
    | Pexp_ifthenelse of Expression.t option * Expression.t * Expression.t
    | Pexp_array of Expression.t list
    | Pexp_setfield of Expression.t * Longident_loc.t * Expression.t
    | Pexp_field of Longident_loc.t * Expression.t
    | Pexp_record of Expression.t option * (Expression.t * Longident_loc.t) list
    | Pexp_variant of Expression.t option * string
    | Pexp_construct of Expression.t option * Longident_loc.t
    | Pexp_tuple of Expression.t list
    | Pexp_try of Case.t list * Expression.t
    | Pexp_match of Case.t list * Expression.t
    | Pexp_apply of (Expression.t * Arg_label.t) list * Expression.t
    | Pexp_fun of Expression.t * Pattern.t * Expression.t option * Arg_label.t
    | Pexp_function of Case.t list
    | Pexp_let of Expression.t * Value_binding.t list * Rec_flag.t
    | Pexp_constant of Constant.t
    | Pexp_ident of Longident_loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val pexp_unreachable : t
  val pexp_extension :
    Extension.t
    -> t
  val pexp_open :
    Expression.t
    -> Longident_loc.t
    -> Override_flag.t
    -> t
  val pexp_pack :
    Module_expr.t
    -> t
  val pexp_newtype :
    Expression.t
    -> string Astlib.Loc.t
    -> t
  val pexp_object :
    Class_structure.t
    -> t
  val pexp_poly :
    Core_type.t option
    -> Expression.t
    -> t
  val pexp_lazy :
    Expression.t
    -> t
  val pexp_assert :
    Expression.t
    -> t
  val pexp_letexception :
    Expression.t
    -> Extension_constructor.t
    -> t
  val pexp_letmodule :
    Expression.t
    -> Module_expr.t
    -> string Astlib.Loc.t
    -> t
  val pexp_override :
    (Expression.t * string Astlib.Loc.t) list
    -> t
  val pexp_setinstvar :
    Expression.t
    -> string Astlib.Loc.t
    -> t
  val pexp_new :
    Longident_loc.t
    -> t
  val pexp_send :
    string Astlib.Loc.t
    -> Expression.t
    -> t
  val pexp_coerce :
    Core_type.t
    -> Core_type.t option
    -> Expression.t
    -> t
  val pexp_constraint :
    Core_type.t
    -> Expression.t
    -> t
  val pexp_for :
    Expression.t
    -> Direction_flag.t
    -> Expression.t
    -> Expression.t
    -> Pattern.t
    -> t
  val pexp_while :
    Expression.t
    -> Expression.t
    -> t
  val pexp_sequence :
    Expression.t
    -> Expression.t
    -> t
  val pexp_ifthenelse :
    Expression.t option
    -> Expression.t
    -> Expression.t
    -> t
  val pexp_array :
    Expression.t list
    -> t
  val pexp_setfield :
    Expression.t
    -> Longident_loc.t
    -> Expression.t
    -> t
  val pexp_field :
    Longident_loc.t
    -> Expression.t
    -> t
  val pexp_record :
    Expression.t option
    -> (Expression.t * Longident_loc.t) list
    -> t
  val pexp_variant :
    Expression.t option
    -> string
    -> t
  val pexp_construct :
    Expression.t option
    -> Longident_loc.t
    -> t
  val pexp_tuple :
    Expression.t list
    -> t
  val pexp_try :
    Case.t list
    -> Expression.t
    -> t
  val pexp_match :
    Case.t list
    -> Expression.t
    -> t
  val pexp_apply :
    (Expression.t * Arg_label.t) list
    -> Expression.t
    -> t
  val pexp_fun :
    Expression.t
    -> Pattern.t
    -> Expression.t option
    -> Arg_label.t
    -> t
  val pexp_function :
    Case.t list
    -> t
  val pexp_let :
    Expression.t
    -> Value_binding.t list
    -> Rec_flag.t
    -> t
  val pexp_constant :
    Constant.t
    -> t
  val pexp_ident :
    Longident_loc.t
    -> t
end

and Expression : sig
  type t = expression

  type concrete =
    { pexp_attributes : Attributes.t
    ; pexp_loc : Astlib.Location.t
    ; pexp_desc : Expression_desc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    pexp_attributes:Attributes.t
    -> pexp_loc:Astlib.Location.t
    -> pexp_desc:Expression_desc.t
    -> t
end

and Pattern_desc : sig
  type t = pattern_desc

  type concrete =
    | Ppat_open of Pattern.t * Longident_loc.t
    | Ppat_extension of Extension.t
    | Ppat_exception of Pattern.t
    | Ppat_unpack of string Astlib.Loc.t
    | Ppat_lazy of Pattern.t
    | Ppat_type of Longident_loc.t
    | Ppat_constraint of Core_type.t * Pattern.t
    | Ppat_or of Pattern.t * Pattern.t
    | Ppat_array of Pattern.t list
    | Ppat_record of Closed_flag.t * (Pattern.t * Longident_loc.t) list
    | Ppat_variant of Pattern.t option * string
    | Ppat_construct of Pattern.t option * Longident_loc.t
    | Ppat_tuple of Pattern.t list
    | Ppat_interval of Constant.t * Constant.t
    | Ppat_constant of Constant.t
    | Ppat_alias of string Astlib.Loc.t * Pattern.t
    | Ppat_var of string Astlib.Loc.t
    | Ppat_any

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ppat_open :
    Pattern.t
    -> Longident_loc.t
    -> t
  val ppat_extension :
    Extension.t
    -> t
  val ppat_exception :
    Pattern.t
    -> t
  val ppat_unpack :
    string Astlib.Loc.t
    -> t
  val ppat_lazy :
    Pattern.t
    -> t
  val ppat_type :
    Longident_loc.t
    -> t
  val ppat_constraint :
    Core_type.t
    -> Pattern.t
    -> t
  val ppat_or :
    Pattern.t
    -> Pattern.t
    -> t
  val ppat_array :
    Pattern.t list
    -> t
  val ppat_record :
    Closed_flag.t
    -> (Pattern.t * Longident_loc.t) list
    -> t
  val ppat_variant :
    Pattern.t option
    -> string
    -> t
  val ppat_construct :
    Pattern.t option
    -> Longident_loc.t
    -> t
  val ppat_tuple :
    Pattern.t list
    -> t
  val ppat_interval :
    Constant.t
    -> Constant.t
    -> t
  val ppat_constant :
    Constant.t
    -> t
  val ppat_alias :
    string Astlib.Loc.t
    -> Pattern.t
    -> t
  val ppat_var :
    string Astlib.Loc.t
    -> t
  val ppat_any : t
end

and Pattern : sig
  type t = pattern

  type concrete =
    { ppat_attributes : Attributes.t
    ; ppat_loc : Astlib.Location.t
    ; ppat_desc : Pattern_desc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ppat_attributes:Attributes.t
    -> ppat_loc:Astlib.Location.t
    -> ppat_desc:Pattern_desc.t
    -> t
end

and Object_field : sig
  type t = object_field

  type concrete =
    | Oinherit of Core_type.t
    | Otag of Core_type.t * Attributes.t * string Astlib.Loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val oinherit :
    Core_type.t
    -> t
  val otag :
    Core_type.t
    -> Attributes.t
    -> string Astlib.Loc.t
    -> t
end

and Row_field : sig
  type t = row_field

  type concrete =
    | Rinherit of Core_type.t
    | Rtag of Core_type.t list * bool * Attributes.t * string Astlib.Loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val rinherit :
    Core_type.t
    -> t
  val rtag :
    Core_type.t list
    -> bool
    -> Attributes.t
    -> string Astlib.Loc.t
    -> t
end

and Package_type : sig
  type t = package_type

  type concrete = ((Core_type.t * Longident_loc.t) list * Longident_loc.t)

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : ((Core_type.t * Longident_loc.t) list * Longident_loc.t) -> t
end

and Core_type_desc : sig
  type t = core_type_desc

  type concrete =
    | Ptyp_extension of Extension.t
    | Ptyp_package of Package_type.t
    | Ptyp_poly of Core_type.t * string Astlib.Loc.t list
    | Ptyp_variant of string list option * Closed_flag.t * Row_field.t list
    | Ptyp_alias of string * Core_type.t
    | Ptyp_class of Core_type.t list * Longident_loc.t
    | Ptyp_object of Closed_flag.t * Object_field.t list
    | Ptyp_constr of Core_type.t list * Longident_loc.t
    | Ptyp_tuple of Core_type.t list
    | Ptyp_arrow of Core_type.t * Core_type.t * Arg_label.t
    | Ptyp_var of string
    | Ptyp_any

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ptyp_extension :
    Extension.t
    -> t
  val ptyp_package :
    Package_type.t
    -> t
  val ptyp_poly :
    Core_type.t
    -> string Astlib.Loc.t list
    -> t
  val ptyp_variant :
    string list option
    -> Closed_flag.t
    -> Row_field.t list
    -> t
  val ptyp_alias :
    string
    -> Core_type.t
    -> t
  val ptyp_class :
    Core_type.t list
    -> Longident_loc.t
    -> t
  val ptyp_object :
    Closed_flag.t
    -> Object_field.t list
    -> t
  val ptyp_constr :
    Core_type.t list
    -> Longident_loc.t
    -> t
  val ptyp_tuple :
    Core_type.t list
    -> t
  val ptyp_arrow :
    Core_type.t
    -> Core_type.t
    -> Arg_label.t
    -> t
  val ptyp_var :
    string
    -> t
  val ptyp_any : t
end

and Core_type : sig
  type t = core_type

  type concrete =
    { ptyp_attributes : Attributes.t
    ; ptyp_loc : Astlib.Location.t
    ; ptyp_desc : Core_type_desc.t
    }

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create :
    ptyp_attributes:Attributes.t
    -> ptyp_loc:Astlib.Location.t
    -> ptyp_desc:Core_type_desc.t
    -> t
end

and Payload : sig
  type t = payload

  type concrete =
    | PPat of Expression.t option * Pattern.t
    | PTyp of Core_type.t
    | PSig of Signature.t
    | PStr of Structure.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val ppat :
    Expression.t option
    -> Pattern.t
    -> t
  val ptyp :
    Core_type.t
    -> t
  val psig :
    Signature.t
    -> t
  val pstr :
    Structure.t
    -> t
end

and Attributes : sig
  type t = attributes

  type concrete = Attribute.t list

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : Attribute.t list -> t
end

and Extension : sig
  type t = extension

  type concrete = (Payload.t * string Astlib.Loc.t)

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : (Payload.t * string Astlib.Loc.t) -> t
end

and Attribute : sig
  type t = attribute

  type concrete = (Payload.t * string Astlib.Loc.t)

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : (Payload.t * string Astlib.Loc.t) -> t
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

  type concrete = Longident.t Astlib.Loc.t

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val create : Longident.t Astlib.Loc.t -> t
end

and Longident : sig
  type t = longident

  type concrete =
    | Lapply of Longident.t * Longident.t
    | Ldot of string * Longident.t
    | Lident of string

  val of_concrete : concrete -> t
  val to_concrete : t -> concrete
  val to_concrete_opt : t -> concrete option

  val lapply :
    Longident.t
    -> Longident.t
    -> t
  val ldot :
    string
    -> Longident.t
    -> t
  val lident :
    string
    -> t
end
(*$*)
