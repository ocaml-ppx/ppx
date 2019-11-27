(*$ Ppx_ast_cinaps.print_virtual_traverse_mli () *)
module V4_07 : sig
  open Versions.V4_07

  class virtual map :
    object
      method virtual bool : bool -> bool
      method virtual char : char -> char
      method virtual int : int -> int
      method virtual list : 'a . ('a -> 'a) -> 'a list -> 'a list
      method virtual option : 'a . ('a -> 'a) -> 'a option -> 'a option
      method virtual string : string -> string
      method virtual location : Astlib.Location.t -> Astlib.Location.t
      method virtual loc : 'a . ('a -> 'a) -> 'a Astlib.Loc.t -> 'a Astlib.Loc.t
      method longident : Longident.t -> Longident.t
      method longident_loc : Longident_loc.t -> Longident_loc.t
      method rec_flag : Rec_flag.t -> Rec_flag.t
      method direction_flag : Direction_flag.t -> Direction_flag.t
      method private_flag : Private_flag.t -> Private_flag.t
      method mutable_flag : Mutable_flag.t -> Mutable_flag.t
      method virtual_flag : Virtual_flag.t -> Virtual_flag.t
      method override_flag : Override_flag.t -> Override_flag.t
      method closed_flag : Closed_flag.t -> Closed_flag.t
      method label : Label.t -> Label.t
      method arg_label : Arg_label.t -> Arg_label.t
      method variance : Variance.t -> Variance.t
      method constant : Constant.t -> Constant.t
      method attribute : Attribute.t -> Attribute.t
      method extension : Extension.t -> Extension.t
      method attributes : Attributes.t -> Attributes.t
      method payload : Payload.t -> Payload.t
      method core_type : Core_type.t -> Core_type.t
      method core_type_desc : Core_type_desc.t -> Core_type_desc.t
      method package_type : Package_type.t -> Package_type.t
      method row_field : Row_field.t -> Row_field.t
      method object_field : Object_field.t -> Object_field.t
      method pattern : Pattern.t -> Pattern.t
      method pattern_desc : Pattern_desc.t -> Pattern_desc.t
      method expression : Expression.t -> Expression.t
      method expression_desc : Expression_desc.t -> Expression_desc.t
      method case : Case.t -> Case.t
      method value_description : Value_description.t -> Value_description.t
      method type_declaration : Type_declaration.t -> Type_declaration.t
      method type_kind : Type_kind.t -> Type_kind.t
      method label_declaration : Label_declaration.t -> Label_declaration.t
      method constructor_declaration : Constructor_declaration.t -> Constructor_declaration.t
      method constructor_arguments : Constructor_arguments.t -> Constructor_arguments.t
      method type_extension : Type_extension.t -> Type_extension.t
      method extension_constructor : Extension_constructor.t -> Extension_constructor.t
      method extension_constructor_kind : Extension_constructor_kind.t -> Extension_constructor_kind.t
      method class_type : Class_type.t -> Class_type.t
      method class_type_desc : Class_type_desc.t -> Class_type_desc.t
      method class_signature : Class_signature.t -> Class_signature.t
      method class_type_field : Class_type_field.t -> Class_type_field.t
      method class_type_field_desc : Class_type_field_desc.t -> Class_type_field_desc.t
      method class_infos_class_expr : Class_expr.t Class_infos.t -> Class_expr.t Class_infos.t
      method class_infos_class_type : Class_type.t Class_infos.t -> Class_type.t Class_infos.t
      method class_description : Class_description.t -> Class_description.t
      method class_type_declaration : Class_type_declaration.t -> Class_type_declaration.t
      method class_expr : Class_expr.t -> Class_expr.t
      method class_expr_desc : Class_expr_desc.t -> Class_expr_desc.t
      method class_structure : Class_structure.t -> Class_structure.t
      method class_field : Class_field.t -> Class_field.t
      method class_field_desc : Class_field_desc.t -> Class_field_desc.t
      method class_field_kind : Class_field_kind.t -> Class_field_kind.t
      method class_declaration : Class_declaration.t -> Class_declaration.t
      method module_type : Module_type.t -> Module_type.t
      method module_type_desc : Module_type_desc.t -> Module_type_desc.t
      method signature : Signature.t -> Signature.t
      method signature_item : Signature_item.t -> Signature_item.t
      method signature_item_desc : Signature_item_desc.t -> Signature_item_desc.t
      method module_declaration : Module_declaration.t -> Module_declaration.t
      method module_type_declaration : Module_type_declaration.t -> Module_type_declaration.t
      method open_description : Open_description.t -> Open_description.t
      method include_infos_module_expr : Module_expr.t Include_infos.t -> Module_expr.t Include_infos.t
      method include_infos_module_type : Module_type.t Include_infos.t -> Module_type.t Include_infos.t
      method include_description : Include_description.t -> Include_description.t
      method include_declaration : Include_declaration.t -> Include_declaration.t
      method with_constraint : With_constraint.t -> With_constraint.t
      method module_expr : Module_expr.t -> Module_expr.t
      method module_expr_desc : Module_expr_desc.t -> Module_expr_desc.t
      method structure : Structure.t -> Structure.t
      method structure_item : Structure_item.t -> Structure_item.t
      method structure_item_desc : Structure_item_desc.t -> Structure_item_desc.t
      method value_binding : Value_binding.t -> Value_binding.t
      method module_binding : Module_binding.t -> Module_binding.t
      method toplevel_phrase : Toplevel_phrase.t -> Toplevel_phrase.t
      method directive_argument : Directive_argument.t -> Directive_argument.t
    end

  class virtual iter :
    object
      method virtual bool : bool -> unit
      method virtual char : char -> unit
      method virtual int : int -> unit
      method virtual list : 'a . ('a -> unit) -> 'a list -> unit
      method virtual option : 'a . ('a -> unit) -> 'a option -> unit
      method virtual string : string -> unit
      method virtual location : Astlib.Location.t -> unit
      method virtual loc : 'a . ('a -> unit) -> 'a Astlib.Loc.t -> unit
      method longident : Longident.t -> unit
      method longident_loc : Longident_loc.t -> unit
      method rec_flag : Rec_flag.t -> unit
      method direction_flag : Direction_flag.t -> unit
      method private_flag : Private_flag.t -> unit
      method mutable_flag : Mutable_flag.t -> unit
      method virtual_flag : Virtual_flag.t -> unit
      method override_flag : Override_flag.t -> unit
      method closed_flag : Closed_flag.t -> unit
      method label : Label.t -> unit
      method arg_label : Arg_label.t -> unit
      method variance : Variance.t -> unit
      method constant : Constant.t -> unit
      method attribute : Attribute.t -> unit
      method extension : Extension.t -> unit
      method attributes : Attributes.t -> unit
      method payload : Payload.t -> unit
      method core_type : Core_type.t -> unit
      method core_type_desc : Core_type_desc.t -> unit
      method package_type : Package_type.t -> unit
      method row_field : Row_field.t -> unit
      method object_field : Object_field.t -> unit
      method pattern : Pattern.t -> unit
      method pattern_desc : Pattern_desc.t -> unit
      method expression : Expression.t -> unit
      method expression_desc : Expression_desc.t -> unit
      method case : Case.t -> unit
      method value_description : Value_description.t -> unit
      method type_declaration : Type_declaration.t -> unit
      method type_kind : Type_kind.t -> unit
      method label_declaration : Label_declaration.t -> unit
      method constructor_declaration : Constructor_declaration.t -> unit
      method constructor_arguments : Constructor_arguments.t -> unit
      method type_extension : Type_extension.t -> unit
      method extension_constructor : Extension_constructor.t -> unit
      method extension_constructor_kind : Extension_constructor_kind.t -> unit
      method class_type : Class_type.t -> unit
      method class_type_desc : Class_type_desc.t -> unit
      method class_signature : Class_signature.t -> unit
      method class_type_field : Class_type_field.t -> unit
      method class_type_field_desc : Class_type_field_desc.t -> unit
      method class_infos_class_expr : Class_expr.t Class_infos.t -> unit
      method class_infos_class_type : Class_type.t Class_infos.t -> unit
      method class_description : Class_description.t -> unit
      method class_type_declaration : Class_type_declaration.t -> unit
      method class_expr : Class_expr.t -> unit
      method class_expr_desc : Class_expr_desc.t -> unit
      method class_structure : Class_structure.t -> unit
      method class_field : Class_field.t -> unit
      method class_field_desc : Class_field_desc.t -> unit
      method class_field_kind : Class_field_kind.t -> unit
      method class_declaration : Class_declaration.t -> unit
      method module_type : Module_type.t -> unit
      method module_type_desc : Module_type_desc.t -> unit
      method signature : Signature.t -> unit
      method signature_item : Signature_item.t -> unit
      method signature_item_desc : Signature_item_desc.t -> unit
      method module_declaration : Module_declaration.t -> unit
      method module_type_declaration : Module_type_declaration.t -> unit
      method open_description : Open_description.t -> unit
      method include_infos_module_expr : Module_expr.t Include_infos.t -> unit
      method include_infos_module_type : Module_type.t Include_infos.t -> unit
      method include_description : Include_description.t -> unit
      method include_declaration : Include_declaration.t -> unit
      method with_constraint : With_constraint.t -> unit
      method module_expr : Module_expr.t -> unit
      method module_expr_desc : Module_expr_desc.t -> unit
      method structure : Structure.t -> unit
      method structure_item : Structure_item.t -> unit
      method structure_item_desc : Structure_item_desc.t -> unit
      method value_binding : Value_binding.t -> unit
      method module_binding : Module_binding.t -> unit
      method toplevel_phrase : Toplevel_phrase.t -> unit
      method directive_argument : Directive_argument.t -> unit
    end

  class virtual ['acc] fold :
    object
      method virtual bool : bool -> 'acc -> 'acc
      method virtual char : char -> 'acc -> 'acc
      method virtual int : int -> 'acc -> 'acc
      method virtual list : 'a . ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
      method virtual option : 'a . ('a -> 'acc -> 'acc) -> 'a option -> 'acc -> 'acc
      method virtual string : string -> 'acc -> 'acc
      method virtual location : Astlib.Location.t -> 'acc -> 'acc
      method virtual loc : 'a . ('a -> 'acc -> 'acc) -> 'a Astlib.Loc.t -> 'acc -> 'acc
      method longident : Longident.t -> 'acc -> 'acc
      method longident_loc : Longident_loc.t -> 'acc -> 'acc
      method rec_flag : Rec_flag.t -> 'acc -> 'acc
      method direction_flag : Direction_flag.t -> 'acc -> 'acc
      method private_flag : Private_flag.t -> 'acc -> 'acc
      method mutable_flag : Mutable_flag.t -> 'acc -> 'acc
      method virtual_flag : Virtual_flag.t -> 'acc -> 'acc
      method override_flag : Override_flag.t -> 'acc -> 'acc
      method closed_flag : Closed_flag.t -> 'acc -> 'acc
      method label : Label.t -> 'acc -> 'acc
      method arg_label : Arg_label.t -> 'acc -> 'acc
      method variance : Variance.t -> 'acc -> 'acc
      method constant : Constant.t -> 'acc -> 'acc
      method attribute : Attribute.t -> 'acc -> 'acc
      method extension : Extension.t -> 'acc -> 'acc
      method attributes : Attributes.t -> 'acc -> 'acc
      method payload : Payload.t -> 'acc -> 'acc
      method core_type : Core_type.t -> 'acc -> 'acc
      method core_type_desc : Core_type_desc.t -> 'acc -> 'acc
      method package_type : Package_type.t -> 'acc -> 'acc
      method row_field : Row_field.t -> 'acc -> 'acc
      method object_field : Object_field.t -> 'acc -> 'acc
      method pattern : Pattern.t -> 'acc -> 'acc
      method pattern_desc : Pattern_desc.t -> 'acc -> 'acc
      method expression : Expression.t -> 'acc -> 'acc
      method expression_desc : Expression_desc.t -> 'acc -> 'acc
      method case : Case.t -> 'acc -> 'acc
      method value_description : Value_description.t -> 'acc -> 'acc
      method type_declaration : Type_declaration.t -> 'acc -> 'acc
      method type_kind : Type_kind.t -> 'acc -> 'acc
      method label_declaration : Label_declaration.t -> 'acc -> 'acc
      method constructor_declaration : Constructor_declaration.t -> 'acc -> 'acc
      method constructor_arguments : Constructor_arguments.t -> 'acc -> 'acc
      method type_extension : Type_extension.t -> 'acc -> 'acc
      method extension_constructor : Extension_constructor.t -> 'acc -> 'acc
      method extension_constructor_kind : Extension_constructor_kind.t -> 'acc -> 'acc
      method class_type : Class_type.t -> 'acc -> 'acc
      method class_type_desc : Class_type_desc.t -> 'acc -> 'acc
      method class_signature : Class_signature.t -> 'acc -> 'acc
      method class_type_field : Class_type_field.t -> 'acc -> 'acc
      method class_type_field_desc : Class_type_field_desc.t -> 'acc -> 'acc
      method class_infos_class_expr : Class_expr.t Class_infos.t -> 'acc -> 'acc
      method class_infos_class_type : Class_type.t Class_infos.t -> 'acc -> 'acc
      method class_description : Class_description.t -> 'acc -> 'acc
      method class_type_declaration : Class_type_declaration.t -> 'acc -> 'acc
      method class_expr : Class_expr.t -> 'acc -> 'acc
      method class_expr_desc : Class_expr_desc.t -> 'acc -> 'acc
      method class_structure : Class_structure.t -> 'acc -> 'acc
      method class_field : Class_field.t -> 'acc -> 'acc
      method class_field_desc : Class_field_desc.t -> 'acc -> 'acc
      method class_field_kind : Class_field_kind.t -> 'acc -> 'acc
      method class_declaration : Class_declaration.t -> 'acc -> 'acc
      method module_type : Module_type.t -> 'acc -> 'acc
      method module_type_desc : Module_type_desc.t -> 'acc -> 'acc
      method signature : Signature.t -> 'acc -> 'acc
      method signature_item : Signature_item.t -> 'acc -> 'acc
      method signature_item_desc : Signature_item_desc.t -> 'acc -> 'acc
      method module_declaration : Module_declaration.t -> 'acc -> 'acc
      method module_type_declaration : Module_type_declaration.t -> 'acc -> 'acc
      method open_description : Open_description.t -> 'acc -> 'acc
      method include_infos_module_expr : Module_expr.t Include_infos.t -> 'acc -> 'acc
      method include_infos_module_type : Module_type.t Include_infos.t -> 'acc -> 'acc
      method include_description : Include_description.t -> 'acc -> 'acc
      method include_declaration : Include_declaration.t -> 'acc -> 'acc
      method with_constraint : With_constraint.t -> 'acc -> 'acc
      method module_expr : Module_expr.t -> 'acc -> 'acc
      method module_expr_desc : Module_expr_desc.t -> 'acc -> 'acc
      method structure : Structure.t -> 'acc -> 'acc
      method structure_item : Structure_item.t -> 'acc -> 'acc
      method structure_item_desc : Structure_item_desc.t -> 'acc -> 'acc
      method value_binding : Value_binding.t -> 'acc -> 'acc
      method module_binding : Module_binding.t -> 'acc -> 'acc
      method toplevel_phrase : Toplevel_phrase.t -> 'acc -> 'acc
      method directive_argument : Directive_argument.t -> 'acc -> 'acc
    end

  class virtual ['acc] fold_map :
    object
      method virtual bool : bool -> 'acc -> (bool * 'acc)
      method virtual char : char -> 'acc -> (char * 'acc)
      method virtual int : int -> 'acc -> (int * 'acc)
      method virtual list : 'a . ('a -> 'acc -> ('a * 'acc)) -> 'a list -> 'acc -> ('a list * 'acc)
      method virtual option : 'a . ('a -> 'acc -> ('a * 'acc)) -> 'a option -> 'acc -> ('a option * 'acc)
      method virtual string : string -> 'acc -> (string * 'acc)
      method virtual location : Astlib.Location.t -> 'acc -> (Astlib.Location.t * 'acc)
      method virtual loc : 'a . ('a -> 'acc -> ('a * 'acc)) -> 'a Astlib.Loc.t -> 'acc -> ('a Astlib.Loc.t * 'acc)
      method longident : Longident.t -> 'acc -> (Longident.t * 'acc)
      method longident_loc : Longident_loc.t -> 'acc -> (Longident_loc.t * 'acc)
      method rec_flag : Rec_flag.t -> 'acc -> (Rec_flag.t * 'acc)
      method direction_flag : Direction_flag.t -> 'acc -> (Direction_flag.t * 'acc)
      method private_flag : Private_flag.t -> 'acc -> (Private_flag.t * 'acc)
      method mutable_flag : Mutable_flag.t -> 'acc -> (Mutable_flag.t * 'acc)
      method virtual_flag : Virtual_flag.t -> 'acc -> (Virtual_flag.t * 'acc)
      method override_flag : Override_flag.t -> 'acc -> (Override_flag.t * 'acc)
      method closed_flag : Closed_flag.t -> 'acc -> (Closed_flag.t * 'acc)
      method label : Label.t -> 'acc -> (Label.t * 'acc)
      method arg_label : Arg_label.t -> 'acc -> (Arg_label.t * 'acc)
      method variance : Variance.t -> 'acc -> (Variance.t * 'acc)
      method constant : Constant.t -> 'acc -> (Constant.t * 'acc)
      method attribute : Attribute.t -> 'acc -> (Attribute.t * 'acc)
      method extension : Extension.t -> 'acc -> (Extension.t * 'acc)
      method attributes : Attributes.t -> 'acc -> (Attributes.t * 'acc)
      method payload : Payload.t -> 'acc -> (Payload.t * 'acc)
      method core_type : Core_type.t -> 'acc -> (Core_type.t * 'acc)
      method core_type_desc : Core_type_desc.t -> 'acc -> (Core_type_desc.t * 'acc)
      method package_type : Package_type.t -> 'acc -> (Package_type.t * 'acc)
      method row_field : Row_field.t -> 'acc -> (Row_field.t * 'acc)
      method object_field : Object_field.t -> 'acc -> (Object_field.t * 'acc)
      method pattern : Pattern.t -> 'acc -> (Pattern.t * 'acc)
      method pattern_desc : Pattern_desc.t -> 'acc -> (Pattern_desc.t * 'acc)
      method expression : Expression.t -> 'acc -> (Expression.t * 'acc)
      method expression_desc : Expression_desc.t -> 'acc -> (Expression_desc.t * 'acc)
      method case : Case.t -> 'acc -> (Case.t * 'acc)
      method value_description : Value_description.t -> 'acc -> (Value_description.t * 'acc)
      method type_declaration : Type_declaration.t -> 'acc -> (Type_declaration.t * 'acc)
      method type_kind : Type_kind.t -> 'acc -> (Type_kind.t * 'acc)
      method label_declaration : Label_declaration.t -> 'acc -> (Label_declaration.t * 'acc)
      method constructor_declaration : Constructor_declaration.t -> 'acc -> (Constructor_declaration.t * 'acc)
      method constructor_arguments : Constructor_arguments.t -> 'acc -> (Constructor_arguments.t * 'acc)
      method type_extension : Type_extension.t -> 'acc -> (Type_extension.t * 'acc)
      method extension_constructor : Extension_constructor.t -> 'acc -> (Extension_constructor.t * 'acc)
      method extension_constructor_kind : Extension_constructor_kind.t -> 'acc -> (Extension_constructor_kind.t * 'acc)
      method class_type : Class_type.t -> 'acc -> (Class_type.t * 'acc)
      method class_type_desc : Class_type_desc.t -> 'acc -> (Class_type_desc.t * 'acc)
      method class_signature : Class_signature.t -> 'acc -> (Class_signature.t * 'acc)
      method class_type_field : Class_type_field.t -> 'acc -> (Class_type_field.t * 'acc)
      method class_type_field_desc : Class_type_field_desc.t -> 'acc -> (Class_type_field_desc.t * 'acc)
      method class_infos_class_expr : Class_expr.t Class_infos.t -> 'acc -> (Class_expr.t Class_infos.t * 'acc)
      method class_infos_class_type : Class_type.t Class_infos.t -> 'acc -> (Class_type.t Class_infos.t * 'acc)
      method class_description : Class_description.t -> 'acc -> (Class_description.t * 'acc)
      method class_type_declaration : Class_type_declaration.t -> 'acc -> (Class_type_declaration.t * 'acc)
      method class_expr : Class_expr.t -> 'acc -> (Class_expr.t * 'acc)
      method class_expr_desc : Class_expr_desc.t -> 'acc -> (Class_expr_desc.t * 'acc)
      method class_structure : Class_structure.t -> 'acc -> (Class_structure.t * 'acc)
      method class_field : Class_field.t -> 'acc -> (Class_field.t * 'acc)
      method class_field_desc : Class_field_desc.t -> 'acc -> (Class_field_desc.t * 'acc)
      method class_field_kind : Class_field_kind.t -> 'acc -> (Class_field_kind.t * 'acc)
      method class_declaration : Class_declaration.t -> 'acc -> (Class_declaration.t * 'acc)
      method module_type : Module_type.t -> 'acc -> (Module_type.t * 'acc)
      method module_type_desc : Module_type_desc.t -> 'acc -> (Module_type_desc.t * 'acc)
      method signature : Signature.t -> 'acc -> (Signature.t * 'acc)
      method signature_item : Signature_item.t -> 'acc -> (Signature_item.t * 'acc)
      method signature_item_desc : Signature_item_desc.t -> 'acc -> (Signature_item_desc.t * 'acc)
      method module_declaration : Module_declaration.t -> 'acc -> (Module_declaration.t * 'acc)
      method module_type_declaration : Module_type_declaration.t -> 'acc -> (Module_type_declaration.t * 'acc)
      method open_description : Open_description.t -> 'acc -> (Open_description.t * 'acc)
      method include_infos_module_expr : Module_expr.t Include_infos.t -> 'acc -> (Module_expr.t Include_infos.t * 'acc)
      method include_infos_module_type : Module_type.t Include_infos.t -> 'acc -> (Module_type.t Include_infos.t * 'acc)
      method include_description : Include_description.t -> 'acc -> (Include_description.t * 'acc)
      method include_declaration : Include_declaration.t -> 'acc -> (Include_declaration.t * 'acc)
      method with_constraint : With_constraint.t -> 'acc -> (With_constraint.t * 'acc)
      method module_expr : Module_expr.t -> 'acc -> (Module_expr.t * 'acc)
      method module_expr_desc : Module_expr_desc.t -> 'acc -> (Module_expr_desc.t * 'acc)
      method structure : Structure.t -> 'acc -> (Structure.t * 'acc)
      method structure_item : Structure_item.t -> 'acc -> (Structure_item.t * 'acc)
      method structure_item_desc : Structure_item_desc.t -> 'acc -> (Structure_item_desc.t * 'acc)
      method value_binding : Value_binding.t -> 'acc -> (Value_binding.t * 'acc)
      method module_binding : Module_binding.t -> 'acc -> (Module_binding.t * 'acc)
      method toplevel_phrase : Toplevel_phrase.t -> 'acc -> (Toplevel_phrase.t * 'acc)
      method directive_argument : Directive_argument.t -> 'acc -> (Directive_argument.t * 'acc)
    end

  class virtual ['ctx] map_with_context :
    object
      method virtual bool : 'ctx -> bool -> bool
      method virtual char : 'ctx -> char -> char
      method virtual int : 'ctx -> int -> int
      method virtual list : 'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a list -> 'a list
      method virtual option : 'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a option -> 'a option
      method virtual string : 'ctx -> string -> string
      method virtual location : 'ctx -> Astlib.Location.t -> Astlib.Location.t
      method virtual loc : 'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a Astlib.Loc.t -> 'a Astlib.Loc.t
      method longident : 'ctx -> Longident.t -> Longident.t
      method longident_loc : 'ctx -> Longident_loc.t -> Longident_loc.t
      method rec_flag : 'ctx -> Rec_flag.t -> Rec_flag.t
      method direction_flag : 'ctx -> Direction_flag.t -> Direction_flag.t
      method private_flag : 'ctx -> Private_flag.t -> Private_flag.t
      method mutable_flag : 'ctx -> Mutable_flag.t -> Mutable_flag.t
      method virtual_flag : 'ctx -> Virtual_flag.t -> Virtual_flag.t
      method override_flag : 'ctx -> Override_flag.t -> Override_flag.t
      method closed_flag : 'ctx -> Closed_flag.t -> Closed_flag.t
      method label : 'ctx -> Label.t -> Label.t
      method arg_label : 'ctx -> Arg_label.t -> Arg_label.t
      method variance : 'ctx -> Variance.t -> Variance.t
      method constant : 'ctx -> Constant.t -> Constant.t
      method attribute : 'ctx -> Attribute.t -> Attribute.t
      method extension : 'ctx -> Extension.t -> Extension.t
      method attributes : 'ctx -> Attributes.t -> Attributes.t
      method payload : 'ctx -> Payload.t -> Payload.t
      method core_type : 'ctx -> Core_type.t -> Core_type.t
      method core_type_desc : 'ctx -> Core_type_desc.t -> Core_type_desc.t
      method package_type : 'ctx -> Package_type.t -> Package_type.t
      method row_field : 'ctx -> Row_field.t -> Row_field.t
      method object_field : 'ctx -> Object_field.t -> Object_field.t
      method pattern : 'ctx -> Pattern.t -> Pattern.t
      method pattern_desc : 'ctx -> Pattern_desc.t -> Pattern_desc.t
      method expression : 'ctx -> Expression.t -> Expression.t
      method expression_desc : 'ctx -> Expression_desc.t -> Expression_desc.t
      method case : 'ctx -> Case.t -> Case.t
      method value_description : 'ctx -> Value_description.t -> Value_description.t
      method type_declaration : 'ctx -> Type_declaration.t -> Type_declaration.t
      method type_kind : 'ctx -> Type_kind.t -> Type_kind.t
      method label_declaration : 'ctx -> Label_declaration.t -> Label_declaration.t
      method constructor_declaration : 'ctx -> Constructor_declaration.t -> Constructor_declaration.t
      method constructor_arguments : 'ctx -> Constructor_arguments.t -> Constructor_arguments.t
      method type_extension : 'ctx -> Type_extension.t -> Type_extension.t
      method extension_constructor : 'ctx -> Extension_constructor.t -> Extension_constructor.t
      method extension_constructor_kind : 'ctx -> Extension_constructor_kind.t -> Extension_constructor_kind.t
      method class_type : 'ctx -> Class_type.t -> Class_type.t
      method class_type_desc : 'ctx -> Class_type_desc.t -> Class_type_desc.t
      method class_signature : 'ctx -> Class_signature.t -> Class_signature.t
      method class_type_field : 'ctx -> Class_type_field.t -> Class_type_field.t
      method class_type_field_desc : 'ctx -> Class_type_field_desc.t -> Class_type_field_desc.t
      method class_infos_class_expr : 'ctx -> Class_expr.t Class_infos.t -> Class_expr.t Class_infos.t
      method class_infos_class_type : 'ctx -> Class_type.t Class_infos.t -> Class_type.t Class_infos.t
      method class_description : 'ctx -> Class_description.t -> Class_description.t
      method class_type_declaration : 'ctx -> Class_type_declaration.t -> Class_type_declaration.t
      method class_expr : 'ctx -> Class_expr.t -> Class_expr.t
      method class_expr_desc : 'ctx -> Class_expr_desc.t -> Class_expr_desc.t
      method class_structure : 'ctx -> Class_structure.t -> Class_structure.t
      method class_field : 'ctx -> Class_field.t -> Class_field.t
      method class_field_desc : 'ctx -> Class_field_desc.t -> Class_field_desc.t
      method class_field_kind : 'ctx -> Class_field_kind.t -> Class_field_kind.t
      method class_declaration : 'ctx -> Class_declaration.t -> Class_declaration.t
      method module_type : 'ctx -> Module_type.t -> Module_type.t
      method module_type_desc : 'ctx -> Module_type_desc.t -> Module_type_desc.t
      method signature : 'ctx -> Signature.t -> Signature.t
      method signature_item : 'ctx -> Signature_item.t -> Signature_item.t
      method signature_item_desc : 'ctx -> Signature_item_desc.t -> Signature_item_desc.t
      method module_declaration : 'ctx -> Module_declaration.t -> Module_declaration.t
      method module_type_declaration : 'ctx -> Module_type_declaration.t -> Module_type_declaration.t
      method open_description : 'ctx -> Open_description.t -> Open_description.t
      method include_infos_module_expr : 'ctx -> Module_expr.t Include_infos.t -> Module_expr.t Include_infos.t
      method include_infos_module_type : 'ctx -> Module_type.t Include_infos.t -> Module_type.t Include_infos.t
      method include_description : 'ctx -> Include_description.t -> Include_description.t
      method include_declaration : 'ctx -> Include_declaration.t -> Include_declaration.t
      method with_constraint : 'ctx -> With_constraint.t -> With_constraint.t
      method module_expr : 'ctx -> Module_expr.t -> Module_expr.t
      method module_expr_desc : 'ctx -> Module_expr_desc.t -> Module_expr_desc.t
      method structure : 'ctx -> Structure.t -> Structure.t
      method structure_item : 'ctx -> Structure_item.t -> Structure_item.t
      method structure_item_desc : 'ctx -> Structure_item_desc.t -> Structure_item_desc.t
      method value_binding : 'ctx -> Value_binding.t -> Value_binding.t
      method module_binding : 'ctx -> Module_binding.t -> Module_binding.t
      method toplevel_phrase : 'ctx -> Toplevel_phrase.t -> Toplevel_phrase.t
      method directive_argument : 'ctx -> Directive_argument.t -> Directive_argument.t
    end

  class virtual ['res] lift :
    object
      method virtual record : (string * 'res) list -> 'res
      method virtual constr : string -> 'res list -> 'res
      method virtual tuple : 'res list -> 'res
      method virtual bool : bool -> 'res
      method virtual char : char -> 'res
      method virtual int : int -> 'res
      method virtual list : 'a . ('a -> 'res) -> 'a list -> 'res
      method virtual option : 'a . ('a -> 'res) -> 'a option -> 'res
      method virtual string : string -> 'res
      method virtual location : Astlib.Location.t -> 'res
      method virtual loc : 'a . ('a -> 'res) -> 'a Astlib.Loc.t -> 'res
      method longident : Longident.t -> 'res
      method longident_loc : Longident_loc.t -> 'res
      method rec_flag : Rec_flag.t -> 'res
      method direction_flag : Direction_flag.t -> 'res
      method private_flag : Private_flag.t -> 'res
      method mutable_flag : Mutable_flag.t -> 'res
      method virtual_flag : Virtual_flag.t -> 'res
      method override_flag : Override_flag.t -> 'res
      method closed_flag : Closed_flag.t -> 'res
      method label : Label.t -> 'res
      method arg_label : Arg_label.t -> 'res
      method variance : Variance.t -> 'res
      method constant : Constant.t -> 'res
      method attribute : Attribute.t -> 'res
      method extension : Extension.t -> 'res
      method attributes : Attributes.t -> 'res
      method payload : Payload.t -> 'res
      method core_type : Core_type.t -> 'res
      method core_type_desc : Core_type_desc.t -> 'res
      method package_type : Package_type.t -> 'res
      method row_field : Row_field.t -> 'res
      method object_field : Object_field.t -> 'res
      method pattern : Pattern.t -> 'res
      method pattern_desc : Pattern_desc.t -> 'res
      method expression : Expression.t -> 'res
      method expression_desc : Expression_desc.t -> 'res
      method case : Case.t -> 'res
      method value_description : Value_description.t -> 'res
      method type_declaration : Type_declaration.t -> 'res
      method type_kind : Type_kind.t -> 'res
      method label_declaration : Label_declaration.t -> 'res
      method constructor_declaration : Constructor_declaration.t -> 'res
      method constructor_arguments : Constructor_arguments.t -> 'res
      method type_extension : Type_extension.t -> 'res
      method extension_constructor : Extension_constructor.t -> 'res
      method extension_constructor_kind : Extension_constructor_kind.t -> 'res
      method class_type : Class_type.t -> 'res
      method class_type_desc : Class_type_desc.t -> 'res
      method class_signature : Class_signature.t -> 'res
      method class_type_field : Class_type_field.t -> 'res
      method class_type_field_desc : Class_type_field_desc.t -> 'res
      method class_infos_class_expr : Class_expr.t Class_infos.t -> 'res
      method class_infos_class_type : Class_type.t Class_infos.t -> 'res
      method class_description : Class_description.t -> 'res
      method class_type_declaration : Class_type_declaration.t -> 'res
      method class_expr : Class_expr.t -> 'res
      method class_expr_desc : Class_expr_desc.t -> 'res
      method class_structure : Class_structure.t -> 'res
      method class_field : Class_field.t -> 'res
      method class_field_desc : Class_field_desc.t -> 'res
      method class_field_kind : Class_field_kind.t -> 'res
      method class_declaration : Class_declaration.t -> 'res
      method module_type : Module_type.t -> 'res
      method module_type_desc : Module_type_desc.t -> 'res
      method signature : Signature.t -> 'res
      method signature_item : Signature_item.t -> 'res
      method signature_item_desc : Signature_item_desc.t -> 'res
      method module_declaration : Module_declaration.t -> 'res
      method module_type_declaration : Module_type_declaration.t -> 'res
      method open_description : Open_description.t -> 'res
      method include_infos_module_expr : Module_expr.t Include_infos.t -> 'res
      method include_infos_module_type : Module_type.t Include_infos.t -> 'res
      method include_description : Include_description.t -> 'res
      method include_declaration : Include_declaration.t -> 'res
      method with_constraint : With_constraint.t -> 'res
      method module_expr : Module_expr.t -> 'res
      method module_expr_desc : Module_expr_desc.t -> 'res
      method structure : Structure.t -> 'res
      method structure_item : Structure_item.t -> 'res
      method structure_item_desc : Structure_item_desc.t -> 'res
      method value_binding : Value_binding.t -> 'res
      method module_binding : Module_binding.t -> 'res
      method toplevel_phrase : Toplevel_phrase.t -> 'res
      method directive_argument : Directive_argument.t -> 'res
    end
end

module V4_06 : sig
  open Versions.V4_06

  class virtual map :
    object
      method virtual bool : bool -> bool
      method virtual char : char -> char
      method virtual int : int -> int
      method virtual list : 'a . ('a -> 'a) -> 'a list -> 'a list
      method virtual option : 'a . ('a -> 'a) -> 'a option -> 'a option
      method virtual string : string -> string
      method virtual location : Astlib.Location.t -> Astlib.Location.t
      method virtual loc : 'a . ('a -> 'a) -> 'a Astlib.Loc.t -> 'a Astlib.Loc.t
      method longident : Longident.t -> Longident.t
      method longident_loc : Longident_loc.t -> Longident_loc.t
      method rec_flag : Rec_flag.t -> Rec_flag.t
      method direction_flag : Direction_flag.t -> Direction_flag.t
      method private_flag : Private_flag.t -> Private_flag.t
      method mutable_flag : Mutable_flag.t -> Mutable_flag.t
      method virtual_flag : Virtual_flag.t -> Virtual_flag.t
      method override_flag : Override_flag.t -> Override_flag.t
      method closed_flag : Closed_flag.t -> Closed_flag.t
      method label : Label.t -> Label.t
      method arg_label : Arg_label.t -> Arg_label.t
      method variance : Variance.t -> Variance.t
      method constant : Constant.t -> Constant.t
      method attribute : Attribute.t -> Attribute.t
      method extension : Extension.t -> Extension.t
      method attributes : Attributes.t -> Attributes.t
      method payload : Payload.t -> Payload.t
      method core_type : Core_type.t -> Core_type.t
      method core_type_desc : Core_type_desc.t -> Core_type_desc.t
      method package_type : Package_type.t -> Package_type.t
      method row_field : Row_field.t -> Row_field.t
      method object_field : Object_field.t -> Object_field.t
      method pattern : Pattern.t -> Pattern.t
      method pattern_desc : Pattern_desc.t -> Pattern_desc.t
      method expression : Expression.t -> Expression.t
      method expression_desc : Expression_desc.t -> Expression_desc.t
      method case : Case.t -> Case.t
      method value_description : Value_description.t -> Value_description.t
      method type_declaration : Type_declaration.t -> Type_declaration.t
      method type_kind : Type_kind.t -> Type_kind.t
      method label_declaration : Label_declaration.t -> Label_declaration.t
      method constructor_declaration : Constructor_declaration.t -> Constructor_declaration.t
      method constructor_arguments : Constructor_arguments.t -> Constructor_arguments.t
      method type_extension : Type_extension.t -> Type_extension.t
      method extension_constructor : Extension_constructor.t -> Extension_constructor.t
      method extension_constructor_kind : Extension_constructor_kind.t -> Extension_constructor_kind.t
      method class_type : Class_type.t -> Class_type.t
      method class_type_desc : Class_type_desc.t -> Class_type_desc.t
      method class_signature : Class_signature.t -> Class_signature.t
      method class_type_field : Class_type_field.t -> Class_type_field.t
      method class_type_field_desc : Class_type_field_desc.t -> Class_type_field_desc.t
      method class_infos_class_expr : Class_expr.t Class_infos.t -> Class_expr.t Class_infos.t
      method class_infos_class_type : Class_type.t Class_infos.t -> Class_type.t Class_infos.t
      method class_description : Class_description.t -> Class_description.t
      method class_type_declaration : Class_type_declaration.t -> Class_type_declaration.t
      method class_expr : Class_expr.t -> Class_expr.t
      method class_expr_desc : Class_expr_desc.t -> Class_expr_desc.t
      method class_structure : Class_structure.t -> Class_structure.t
      method class_field : Class_field.t -> Class_field.t
      method class_field_desc : Class_field_desc.t -> Class_field_desc.t
      method class_field_kind : Class_field_kind.t -> Class_field_kind.t
      method class_declaration : Class_declaration.t -> Class_declaration.t
      method module_type : Module_type.t -> Module_type.t
      method module_type_desc : Module_type_desc.t -> Module_type_desc.t
      method signature : Signature.t -> Signature.t
      method signature_item : Signature_item.t -> Signature_item.t
      method signature_item_desc : Signature_item_desc.t -> Signature_item_desc.t
      method module_declaration : Module_declaration.t -> Module_declaration.t
      method module_type_declaration : Module_type_declaration.t -> Module_type_declaration.t
      method open_description : Open_description.t -> Open_description.t
      method include_infos_module_expr : Module_expr.t Include_infos.t -> Module_expr.t Include_infos.t
      method include_infos_module_type : Module_type.t Include_infos.t -> Module_type.t Include_infos.t
      method include_description : Include_description.t -> Include_description.t
      method include_declaration : Include_declaration.t -> Include_declaration.t
      method with_constraint : With_constraint.t -> With_constraint.t
      method module_expr : Module_expr.t -> Module_expr.t
      method module_expr_desc : Module_expr_desc.t -> Module_expr_desc.t
      method structure : Structure.t -> Structure.t
      method structure_item : Structure_item.t -> Structure_item.t
      method structure_item_desc : Structure_item_desc.t -> Structure_item_desc.t
      method value_binding : Value_binding.t -> Value_binding.t
      method module_binding : Module_binding.t -> Module_binding.t
      method toplevel_phrase : Toplevel_phrase.t -> Toplevel_phrase.t
      method directive_argument : Directive_argument.t -> Directive_argument.t
    end

  class virtual iter :
    object
      method virtual bool : bool -> unit
      method virtual char : char -> unit
      method virtual int : int -> unit
      method virtual list : 'a . ('a -> unit) -> 'a list -> unit
      method virtual option : 'a . ('a -> unit) -> 'a option -> unit
      method virtual string : string -> unit
      method virtual location : Astlib.Location.t -> unit
      method virtual loc : 'a . ('a -> unit) -> 'a Astlib.Loc.t -> unit
      method longident : Longident.t -> unit
      method longident_loc : Longident_loc.t -> unit
      method rec_flag : Rec_flag.t -> unit
      method direction_flag : Direction_flag.t -> unit
      method private_flag : Private_flag.t -> unit
      method mutable_flag : Mutable_flag.t -> unit
      method virtual_flag : Virtual_flag.t -> unit
      method override_flag : Override_flag.t -> unit
      method closed_flag : Closed_flag.t -> unit
      method label : Label.t -> unit
      method arg_label : Arg_label.t -> unit
      method variance : Variance.t -> unit
      method constant : Constant.t -> unit
      method attribute : Attribute.t -> unit
      method extension : Extension.t -> unit
      method attributes : Attributes.t -> unit
      method payload : Payload.t -> unit
      method core_type : Core_type.t -> unit
      method core_type_desc : Core_type_desc.t -> unit
      method package_type : Package_type.t -> unit
      method row_field : Row_field.t -> unit
      method object_field : Object_field.t -> unit
      method pattern : Pattern.t -> unit
      method pattern_desc : Pattern_desc.t -> unit
      method expression : Expression.t -> unit
      method expression_desc : Expression_desc.t -> unit
      method case : Case.t -> unit
      method value_description : Value_description.t -> unit
      method type_declaration : Type_declaration.t -> unit
      method type_kind : Type_kind.t -> unit
      method label_declaration : Label_declaration.t -> unit
      method constructor_declaration : Constructor_declaration.t -> unit
      method constructor_arguments : Constructor_arguments.t -> unit
      method type_extension : Type_extension.t -> unit
      method extension_constructor : Extension_constructor.t -> unit
      method extension_constructor_kind : Extension_constructor_kind.t -> unit
      method class_type : Class_type.t -> unit
      method class_type_desc : Class_type_desc.t -> unit
      method class_signature : Class_signature.t -> unit
      method class_type_field : Class_type_field.t -> unit
      method class_type_field_desc : Class_type_field_desc.t -> unit
      method class_infos_class_expr : Class_expr.t Class_infos.t -> unit
      method class_infos_class_type : Class_type.t Class_infos.t -> unit
      method class_description : Class_description.t -> unit
      method class_type_declaration : Class_type_declaration.t -> unit
      method class_expr : Class_expr.t -> unit
      method class_expr_desc : Class_expr_desc.t -> unit
      method class_structure : Class_structure.t -> unit
      method class_field : Class_field.t -> unit
      method class_field_desc : Class_field_desc.t -> unit
      method class_field_kind : Class_field_kind.t -> unit
      method class_declaration : Class_declaration.t -> unit
      method module_type : Module_type.t -> unit
      method module_type_desc : Module_type_desc.t -> unit
      method signature : Signature.t -> unit
      method signature_item : Signature_item.t -> unit
      method signature_item_desc : Signature_item_desc.t -> unit
      method module_declaration : Module_declaration.t -> unit
      method module_type_declaration : Module_type_declaration.t -> unit
      method open_description : Open_description.t -> unit
      method include_infos_module_expr : Module_expr.t Include_infos.t -> unit
      method include_infos_module_type : Module_type.t Include_infos.t -> unit
      method include_description : Include_description.t -> unit
      method include_declaration : Include_declaration.t -> unit
      method with_constraint : With_constraint.t -> unit
      method module_expr : Module_expr.t -> unit
      method module_expr_desc : Module_expr_desc.t -> unit
      method structure : Structure.t -> unit
      method structure_item : Structure_item.t -> unit
      method structure_item_desc : Structure_item_desc.t -> unit
      method value_binding : Value_binding.t -> unit
      method module_binding : Module_binding.t -> unit
      method toplevel_phrase : Toplevel_phrase.t -> unit
      method directive_argument : Directive_argument.t -> unit
    end

  class virtual ['acc] fold :
    object
      method virtual bool : bool -> 'acc -> 'acc
      method virtual char : char -> 'acc -> 'acc
      method virtual int : int -> 'acc -> 'acc
      method virtual list : 'a . ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
      method virtual option : 'a . ('a -> 'acc -> 'acc) -> 'a option -> 'acc -> 'acc
      method virtual string : string -> 'acc -> 'acc
      method virtual location : Astlib.Location.t -> 'acc -> 'acc
      method virtual loc : 'a . ('a -> 'acc -> 'acc) -> 'a Astlib.Loc.t -> 'acc -> 'acc
      method longident : Longident.t -> 'acc -> 'acc
      method longident_loc : Longident_loc.t -> 'acc -> 'acc
      method rec_flag : Rec_flag.t -> 'acc -> 'acc
      method direction_flag : Direction_flag.t -> 'acc -> 'acc
      method private_flag : Private_flag.t -> 'acc -> 'acc
      method mutable_flag : Mutable_flag.t -> 'acc -> 'acc
      method virtual_flag : Virtual_flag.t -> 'acc -> 'acc
      method override_flag : Override_flag.t -> 'acc -> 'acc
      method closed_flag : Closed_flag.t -> 'acc -> 'acc
      method label : Label.t -> 'acc -> 'acc
      method arg_label : Arg_label.t -> 'acc -> 'acc
      method variance : Variance.t -> 'acc -> 'acc
      method constant : Constant.t -> 'acc -> 'acc
      method attribute : Attribute.t -> 'acc -> 'acc
      method extension : Extension.t -> 'acc -> 'acc
      method attributes : Attributes.t -> 'acc -> 'acc
      method payload : Payload.t -> 'acc -> 'acc
      method core_type : Core_type.t -> 'acc -> 'acc
      method core_type_desc : Core_type_desc.t -> 'acc -> 'acc
      method package_type : Package_type.t -> 'acc -> 'acc
      method row_field : Row_field.t -> 'acc -> 'acc
      method object_field : Object_field.t -> 'acc -> 'acc
      method pattern : Pattern.t -> 'acc -> 'acc
      method pattern_desc : Pattern_desc.t -> 'acc -> 'acc
      method expression : Expression.t -> 'acc -> 'acc
      method expression_desc : Expression_desc.t -> 'acc -> 'acc
      method case : Case.t -> 'acc -> 'acc
      method value_description : Value_description.t -> 'acc -> 'acc
      method type_declaration : Type_declaration.t -> 'acc -> 'acc
      method type_kind : Type_kind.t -> 'acc -> 'acc
      method label_declaration : Label_declaration.t -> 'acc -> 'acc
      method constructor_declaration : Constructor_declaration.t -> 'acc -> 'acc
      method constructor_arguments : Constructor_arguments.t -> 'acc -> 'acc
      method type_extension : Type_extension.t -> 'acc -> 'acc
      method extension_constructor : Extension_constructor.t -> 'acc -> 'acc
      method extension_constructor_kind : Extension_constructor_kind.t -> 'acc -> 'acc
      method class_type : Class_type.t -> 'acc -> 'acc
      method class_type_desc : Class_type_desc.t -> 'acc -> 'acc
      method class_signature : Class_signature.t -> 'acc -> 'acc
      method class_type_field : Class_type_field.t -> 'acc -> 'acc
      method class_type_field_desc : Class_type_field_desc.t -> 'acc -> 'acc
      method class_infos_class_expr : Class_expr.t Class_infos.t -> 'acc -> 'acc
      method class_infos_class_type : Class_type.t Class_infos.t -> 'acc -> 'acc
      method class_description : Class_description.t -> 'acc -> 'acc
      method class_type_declaration : Class_type_declaration.t -> 'acc -> 'acc
      method class_expr : Class_expr.t -> 'acc -> 'acc
      method class_expr_desc : Class_expr_desc.t -> 'acc -> 'acc
      method class_structure : Class_structure.t -> 'acc -> 'acc
      method class_field : Class_field.t -> 'acc -> 'acc
      method class_field_desc : Class_field_desc.t -> 'acc -> 'acc
      method class_field_kind : Class_field_kind.t -> 'acc -> 'acc
      method class_declaration : Class_declaration.t -> 'acc -> 'acc
      method module_type : Module_type.t -> 'acc -> 'acc
      method module_type_desc : Module_type_desc.t -> 'acc -> 'acc
      method signature : Signature.t -> 'acc -> 'acc
      method signature_item : Signature_item.t -> 'acc -> 'acc
      method signature_item_desc : Signature_item_desc.t -> 'acc -> 'acc
      method module_declaration : Module_declaration.t -> 'acc -> 'acc
      method module_type_declaration : Module_type_declaration.t -> 'acc -> 'acc
      method open_description : Open_description.t -> 'acc -> 'acc
      method include_infos_module_expr : Module_expr.t Include_infos.t -> 'acc -> 'acc
      method include_infos_module_type : Module_type.t Include_infos.t -> 'acc -> 'acc
      method include_description : Include_description.t -> 'acc -> 'acc
      method include_declaration : Include_declaration.t -> 'acc -> 'acc
      method with_constraint : With_constraint.t -> 'acc -> 'acc
      method module_expr : Module_expr.t -> 'acc -> 'acc
      method module_expr_desc : Module_expr_desc.t -> 'acc -> 'acc
      method structure : Structure.t -> 'acc -> 'acc
      method structure_item : Structure_item.t -> 'acc -> 'acc
      method structure_item_desc : Structure_item_desc.t -> 'acc -> 'acc
      method value_binding : Value_binding.t -> 'acc -> 'acc
      method module_binding : Module_binding.t -> 'acc -> 'acc
      method toplevel_phrase : Toplevel_phrase.t -> 'acc -> 'acc
      method directive_argument : Directive_argument.t -> 'acc -> 'acc
    end

  class virtual ['acc] fold_map :
    object
      method virtual bool : bool -> 'acc -> (bool * 'acc)
      method virtual char : char -> 'acc -> (char * 'acc)
      method virtual int : int -> 'acc -> (int * 'acc)
      method virtual list : 'a . ('a -> 'acc -> ('a * 'acc)) -> 'a list -> 'acc -> ('a list * 'acc)
      method virtual option : 'a . ('a -> 'acc -> ('a * 'acc)) -> 'a option -> 'acc -> ('a option * 'acc)
      method virtual string : string -> 'acc -> (string * 'acc)
      method virtual location : Astlib.Location.t -> 'acc -> (Astlib.Location.t * 'acc)
      method virtual loc : 'a . ('a -> 'acc -> ('a * 'acc)) -> 'a Astlib.Loc.t -> 'acc -> ('a Astlib.Loc.t * 'acc)
      method longident : Longident.t -> 'acc -> (Longident.t * 'acc)
      method longident_loc : Longident_loc.t -> 'acc -> (Longident_loc.t * 'acc)
      method rec_flag : Rec_flag.t -> 'acc -> (Rec_flag.t * 'acc)
      method direction_flag : Direction_flag.t -> 'acc -> (Direction_flag.t * 'acc)
      method private_flag : Private_flag.t -> 'acc -> (Private_flag.t * 'acc)
      method mutable_flag : Mutable_flag.t -> 'acc -> (Mutable_flag.t * 'acc)
      method virtual_flag : Virtual_flag.t -> 'acc -> (Virtual_flag.t * 'acc)
      method override_flag : Override_flag.t -> 'acc -> (Override_flag.t * 'acc)
      method closed_flag : Closed_flag.t -> 'acc -> (Closed_flag.t * 'acc)
      method label : Label.t -> 'acc -> (Label.t * 'acc)
      method arg_label : Arg_label.t -> 'acc -> (Arg_label.t * 'acc)
      method variance : Variance.t -> 'acc -> (Variance.t * 'acc)
      method constant : Constant.t -> 'acc -> (Constant.t * 'acc)
      method attribute : Attribute.t -> 'acc -> (Attribute.t * 'acc)
      method extension : Extension.t -> 'acc -> (Extension.t * 'acc)
      method attributes : Attributes.t -> 'acc -> (Attributes.t * 'acc)
      method payload : Payload.t -> 'acc -> (Payload.t * 'acc)
      method core_type : Core_type.t -> 'acc -> (Core_type.t * 'acc)
      method core_type_desc : Core_type_desc.t -> 'acc -> (Core_type_desc.t * 'acc)
      method package_type : Package_type.t -> 'acc -> (Package_type.t * 'acc)
      method row_field : Row_field.t -> 'acc -> (Row_field.t * 'acc)
      method object_field : Object_field.t -> 'acc -> (Object_field.t * 'acc)
      method pattern : Pattern.t -> 'acc -> (Pattern.t * 'acc)
      method pattern_desc : Pattern_desc.t -> 'acc -> (Pattern_desc.t * 'acc)
      method expression : Expression.t -> 'acc -> (Expression.t * 'acc)
      method expression_desc : Expression_desc.t -> 'acc -> (Expression_desc.t * 'acc)
      method case : Case.t -> 'acc -> (Case.t * 'acc)
      method value_description : Value_description.t -> 'acc -> (Value_description.t * 'acc)
      method type_declaration : Type_declaration.t -> 'acc -> (Type_declaration.t * 'acc)
      method type_kind : Type_kind.t -> 'acc -> (Type_kind.t * 'acc)
      method label_declaration : Label_declaration.t -> 'acc -> (Label_declaration.t * 'acc)
      method constructor_declaration : Constructor_declaration.t -> 'acc -> (Constructor_declaration.t * 'acc)
      method constructor_arguments : Constructor_arguments.t -> 'acc -> (Constructor_arguments.t * 'acc)
      method type_extension : Type_extension.t -> 'acc -> (Type_extension.t * 'acc)
      method extension_constructor : Extension_constructor.t -> 'acc -> (Extension_constructor.t * 'acc)
      method extension_constructor_kind : Extension_constructor_kind.t -> 'acc -> (Extension_constructor_kind.t * 'acc)
      method class_type : Class_type.t -> 'acc -> (Class_type.t * 'acc)
      method class_type_desc : Class_type_desc.t -> 'acc -> (Class_type_desc.t * 'acc)
      method class_signature : Class_signature.t -> 'acc -> (Class_signature.t * 'acc)
      method class_type_field : Class_type_field.t -> 'acc -> (Class_type_field.t * 'acc)
      method class_type_field_desc : Class_type_field_desc.t -> 'acc -> (Class_type_field_desc.t * 'acc)
      method class_infos_class_expr : Class_expr.t Class_infos.t -> 'acc -> (Class_expr.t Class_infos.t * 'acc)
      method class_infos_class_type : Class_type.t Class_infos.t -> 'acc -> (Class_type.t Class_infos.t * 'acc)
      method class_description : Class_description.t -> 'acc -> (Class_description.t * 'acc)
      method class_type_declaration : Class_type_declaration.t -> 'acc -> (Class_type_declaration.t * 'acc)
      method class_expr : Class_expr.t -> 'acc -> (Class_expr.t * 'acc)
      method class_expr_desc : Class_expr_desc.t -> 'acc -> (Class_expr_desc.t * 'acc)
      method class_structure : Class_structure.t -> 'acc -> (Class_structure.t * 'acc)
      method class_field : Class_field.t -> 'acc -> (Class_field.t * 'acc)
      method class_field_desc : Class_field_desc.t -> 'acc -> (Class_field_desc.t * 'acc)
      method class_field_kind : Class_field_kind.t -> 'acc -> (Class_field_kind.t * 'acc)
      method class_declaration : Class_declaration.t -> 'acc -> (Class_declaration.t * 'acc)
      method module_type : Module_type.t -> 'acc -> (Module_type.t * 'acc)
      method module_type_desc : Module_type_desc.t -> 'acc -> (Module_type_desc.t * 'acc)
      method signature : Signature.t -> 'acc -> (Signature.t * 'acc)
      method signature_item : Signature_item.t -> 'acc -> (Signature_item.t * 'acc)
      method signature_item_desc : Signature_item_desc.t -> 'acc -> (Signature_item_desc.t * 'acc)
      method module_declaration : Module_declaration.t -> 'acc -> (Module_declaration.t * 'acc)
      method module_type_declaration : Module_type_declaration.t -> 'acc -> (Module_type_declaration.t * 'acc)
      method open_description : Open_description.t -> 'acc -> (Open_description.t * 'acc)
      method include_infos_module_expr : Module_expr.t Include_infos.t -> 'acc -> (Module_expr.t Include_infos.t * 'acc)
      method include_infos_module_type : Module_type.t Include_infos.t -> 'acc -> (Module_type.t Include_infos.t * 'acc)
      method include_description : Include_description.t -> 'acc -> (Include_description.t * 'acc)
      method include_declaration : Include_declaration.t -> 'acc -> (Include_declaration.t * 'acc)
      method with_constraint : With_constraint.t -> 'acc -> (With_constraint.t * 'acc)
      method module_expr : Module_expr.t -> 'acc -> (Module_expr.t * 'acc)
      method module_expr_desc : Module_expr_desc.t -> 'acc -> (Module_expr_desc.t * 'acc)
      method structure : Structure.t -> 'acc -> (Structure.t * 'acc)
      method structure_item : Structure_item.t -> 'acc -> (Structure_item.t * 'acc)
      method structure_item_desc : Structure_item_desc.t -> 'acc -> (Structure_item_desc.t * 'acc)
      method value_binding : Value_binding.t -> 'acc -> (Value_binding.t * 'acc)
      method module_binding : Module_binding.t -> 'acc -> (Module_binding.t * 'acc)
      method toplevel_phrase : Toplevel_phrase.t -> 'acc -> (Toplevel_phrase.t * 'acc)
      method directive_argument : Directive_argument.t -> 'acc -> (Directive_argument.t * 'acc)
    end

  class virtual ['ctx] map_with_context :
    object
      method virtual bool : 'ctx -> bool -> bool
      method virtual char : 'ctx -> char -> char
      method virtual int : 'ctx -> int -> int
      method virtual list : 'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a list -> 'a list
      method virtual option : 'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a option -> 'a option
      method virtual string : 'ctx -> string -> string
      method virtual location : 'ctx -> Astlib.Location.t -> Astlib.Location.t
      method virtual loc : 'a . ('ctx -> 'a -> 'a) -> 'ctx -> 'a Astlib.Loc.t -> 'a Astlib.Loc.t
      method longident : 'ctx -> Longident.t -> Longident.t
      method longident_loc : 'ctx -> Longident_loc.t -> Longident_loc.t
      method rec_flag : 'ctx -> Rec_flag.t -> Rec_flag.t
      method direction_flag : 'ctx -> Direction_flag.t -> Direction_flag.t
      method private_flag : 'ctx -> Private_flag.t -> Private_flag.t
      method mutable_flag : 'ctx -> Mutable_flag.t -> Mutable_flag.t
      method virtual_flag : 'ctx -> Virtual_flag.t -> Virtual_flag.t
      method override_flag : 'ctx -> Override_flag.t -> Override_flag.t
      method closed_flag : 'ctx -> Closed_flag.t -> Closed_flag.t
      method label : 'ctx -> Label.t -> Label.t
      method arg_label : 'ctx -> Arg_label.t -> Arg_label.t
      method variance : 'ctx -> Variance.t -> Variance.t
      method constant : 'ctx -> Constant.t -> Constant.t
      method attribute : 'ctx -> Attribute.t -> Attribute.t
      method extension : 'ctx -> Extension.t -> Extension.t
      method attributes : 'ctx -> Attributes.t -> Attributes.t
      method payload : 'ctx -> Payload.t -> Payload.t
      method core_type : 'ctx -> Core_type.t -> Core_type.t
      method core_type_desc : 'ctx -> Core_type_desc.t -> Core_type_desc.t
      method package_type : 'ctx -> Package_type.t -> Package_type.t
      method row_field : 'ctx -> Row_field.t -> Row_field.t
      method object_field : 'ctx -> Object_field.t -> Object_field.t
      method pattern : 'ctx -> Pattern.t -> Pattern.t
      method pattern_desc : 'ctx -> Pattern_desc.t -> Pattern_desc.t
      method expression : 'ctx -> Expression.t -> Expression.t
      method expression_desc : 'ctx -> Expression_desc.t -> Expression_desc.t
      method case : 'ctx -> Case.t -> Case.t
      method value_description : 'ctx -> Value_description.t -> Value_description.t
      method type_declaration : 'ctx -> Type_declaration.t -> Type_declaration.t
      method type_kind : 'ctx -> Type_kind.t -> Type_kind.t
      method label_declaration : 'ctx -> Label_declaration.t -> Label_declaration.t
      method constructor_declaration : 'ctx -> Constructor_declaration.t -> Constructor_declaration.t
      method constructor_arguments : 'ctx -> Constructor_arguments.t -> Constructor_arguments.t
      method type_extension : 'ctx -> Type_extension.t -> Type_extension.t
      method extension_constructor : 'ctx -> Extension_constructor.t -> Extension_constructor.t
      method extension_constructor_kind : 'ctx -> Extension_constructor_kind.t -> Extension_constructor_kind.t
      method class_type : 'ctx -> Class_type.t -> Class_type.t
      method class_type_desc : 'ctx -> Class_type_desc.t -> Class_type_desc.t
      method class_signature : 'ctx -> Class_signature.t -> Class_signature.t
      method class_type_field : 'ctx -> Class_type_field.t -> Class_type_field.t
      method class_type_field_desc : 'ctx -> Class_type_field_desc.t -> Class_type_field_desc.t
      method class_infos_class_expr : 'ctx -> Class_expr.t Class_infos.t -> Class_expr.t Class_infos.t
      method class_infos_class_type : 'ctx -> Class_type.t Class_infos.t -> Class_type.t Class_infos.t
      method class_description : 'ctx -> Class_description.t -> Class_description.t
      method class_type_declaration : 'ctx -> Class_type_declaration.t -> Class_type_declaration.t
      method class_expr : 'ctx -> Class_expr.t -> Class_expr.t
      method class_expr_desc : 'ctx -> Class_expr_desc.t -> Class_expr_desc.t
      method class_structure : 'ctx -> Class_structure.t -> Class_structure.t
      method class_field : 'ctx -> Class_field.t -> Class_field.t
      method class_field_desc : 'ctx -> Class_field_desc.t -> Class_field_desc.t
      method class_field_kind : 'ctx -> Class_field_kind.t -> Class_field_kind.t
      method class_declaration : 'ctx -> Class_declaration.t -> Class_declaration.t
      method module_type : 'ctx -> Module_type.t -> Module_type.t
      method module_type_desc : 'ctx -> Module_type_desc.t -> Module_type_desc.t
      method signature : 'ctx -> Signature.t -> Signature.t
      method signature_item : 'ctx -> Signature_item.t -> Signature_item.t
      method signature_item_desc : 'ctx -> Signature_item_desc.t -> Signature_item_desc.t
      method module_declaration : 'ctx -> Module_declaration.t -> Module_declaration.t
      method module_type_declaration : 'ctx -> Module_type_declaration.t -> Module_type_declaration.t
      method open_description : 'ctx -> Open_description.t -> Open_description.t
      method include_infos_module_expr : 'ctx -> Module_expr.t Include_infos.t -> Module_expr.t Include_infos.t
      method include_infos_module_type : 'ctx -> Module_type.t Include_infos.t -> Module_type.t Include_infos.t
      method include_description : 'ctx -> Include_description.t -> Include_description.t
      method include_declaration : 'ctx -> Include_declaration.t -> Include_declaration.t
      method with_constraint : 'ctx -> With_constraint.t -> With_constraint.t
      method module_expr : 'ctx -> Module_expr.t -> Module_expr.t
      method module_expr_desc : 'ctx -> Module_expr_desc.t -> Module_expr_desc.t
      method structure : 'ctx -> Structure.t -> Structure.t
      method structure_item : 'ctx -> Structure_item.t -> Structure_item.t
      method structure_item_desc : 'ctx -> Structure_item_desc.t -> Structure_item_desc.t
      method value_binding : 'ctx -> Value_binding.t -> Value_binding.t
      method module_binding : 'ctx -> Module_binding.t -> Module_binding.t
      method toplevel_phrase : 'ctx -> Toplevel_phrase.t -> Toplevel_phrase.t
      method directive_argument : 'ctx -> Directive_argument.t -> Directive_argument.t
    end

  class virtual ['res] lift :
    object
      method virtual record : (string * 'res) list -> 'res
      method virtual constr : string -> 'res list -> 'res
      method virtual tuple : 'res list -> 'res
      method virtual bool : bool -> 'res
      method virtual char : char -> 'res
      method virtual int : int -> 'res
      method virtual list : 'a . ('a -> 'res) -> 'a list -> 'res
      method virtual option : 'a . ('a -> 'res) -> 'a option -> 'res
      method virtual string : string -> 'res
      method virtual location : Astlib.Location.t -> 'res
      method virtual loc : 'a . ('a -> 'res) -> 'a Astlib.Loc.t -> 'res
      method longident : Longident.t -> 'res
      method longident_loc : Longident_loc.t -> 'res
      method rec_flag : Rec_flag.t -> 'res
      method direction_flag : Direction_flag.t -> 'res
      method private_flag : Private_flag.t -> 'res
      method mutable_flag : Mutable_flag.t -> 'res
      method virtual_flag : Virtual_flag.t -> 'res
      method override_flag : Override_flag.t -> 'res
      method closed_flag : Closed_flag.t -> 'res
      method label : Label.t -> 'res
      method arg_label : Arg_label.t -> 'res
      method variance : Variance.t -> 'res
      method constant : Constant.t -> 'res
      method attribute : Attribute.t -> 'res
      method extension : Extension.t -> 'res
      method attributes : Attributes.t -> 'res
      method payload : Payload.t -> 'res
      method core_type : Core_type.t -> 'res
      method core_type_desc : Core_type_desc.t -> 'res
      method package_type : Package_type.t -> 'res
      method row_field : Row_field.t -> 'res
      method object_field : Object_field.t -> 'res
      method pattern : Pattern.t -> 'res
      method pattern_desc : Pattern_desc.t -> 'res
      method expression : Expression.t -> 'res
      method expression_desc : Expression_desc.t -> 'res
      method case : Case.t -> 'res
      method value_description : Value_description.t -> 'res
      method type_declaration : Type_declaration.t -> 'res
      method type_kind : Type_kind.t -> 'res
      method label_declaration : Label_declaration.t -> 'res
      method constructor_declaration : Constructor_declaration.t -> 'res
      method constructor_arguments : Constructor_arguments.t -> 'res
      method type_extension : Type_extension.t -> 'res
      method extension_constructor : Extension_constructor.t -> 'res
      method extension_constructor_kind : Extension_constructor_kind.t -> 'res
      method class_type : Class_type.t -> 'res
      method class_type_desc : Class_type_desc.t -> 'res
      method class_signature : Class_signature.t -> 'res
      method class_type_field : Class_type_field.t -> 'res
      method class_type_field_desc : Class_type_field_desc.t -> 'res
      method class_infos_class_expr : Class_expr.t Class_infos.t -> 'res
      method class_infos_class_type : Class_type.t Class_infos.t -> 'res
      method class_description : Class_description.t -> 'res
      method class_type_declaration : Class_type_declaration.t -> 'res
      method class_expr : Class_expr.t -> 'res
      method class_expr_desc : Class_expr_desc.t -> 'res
      method class_structure : Class_structure.t -> 'res
      method class_field : Class_field.t -> 'res
      method class_field_desc : Class_field_desc.t -> 'res
      method class_field_kind : Class_field_kind.t -> 'res
      method class_declaration : Class_declaration.t -> 'res
      method module_type : Module_type.t -> 'res
      method module_type_desc : Module_type_desc.t -> 'res
      method signature : Signature.t -> 'res
      method signature_item : Signature_item.t -> 'res
      method signature_item_desc : Signature_item_desc.t -> 'res
      method module_declaration : Module_declaration.t -> 'res
      method module_type_declaration : Module_type_declaration.t -> 'res
      method open_description : Open_description.t -> 'res
      method include_infos_module_expr : Module_expr.t Include_infos.t -> 'res
      method include_infos_module_type : Module_type.t Include_infos.t -> 'res
      method include_description : Include_description.t -> 'res
      method include_declaration : Include_declaration.t -> 'res
      method with_constraint : With_constraint.t -> 'res
      method module_expr : Module_expr.t -> 'res
      method module_expr_desc : Module_expr_desc.t -> 'res
      method structure : Structure.t -> 'res
      method structure_item : Structure_item.t -> 'res
      method structure_item_desc : Structure_item_desc.t -> 'res
      method value_binding : Value_binding.t -> 'res
      method module_binding : Module_binding.t -> 'res
      method toplevel_phrase : Toplevel_phrase.t -> 'res
      method directive_argument : Directive_argument.t -> 'res
    end
end
(*$*)
