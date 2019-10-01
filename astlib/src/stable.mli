(*$ Astlib_src_cinaps.print_astlib_mli () *)
open! StdLabels
open! Ocaml_common

module Unversioned : sig
  type apply_arg
  type arg_label
  type attribute
  type attributes
  type case
  type class_declaration
  type class_description
  type class_expr
  type class_expr_desc
  type class_field
  type class_field_desc
  type class_field_kind
  type class_method_desc
  type class_signature
  type class_structure
  type class_type
  type class_type_constraint
  type class_type_declaration
  type class_type_desc
  type class_type_field
  type class_type_field_desc
  type class_type_method_desc
  type class_type_value_desc
  type class_value_desc
  type closed_flag
  type constant
  type constructor_arguments
  type constructor_declaration
  type core_type
  type core_type_desc
  type direction_flag
  type directive_argument
  type expression
  type expression_desc
  type extension
  type extension_constructor
  type extension_constructor_kind
  type include_declaration
  type include_description
  type label
  type label_declaration
  type label_loc
  type longident
  type longident_loc
  type module_binding
  type module_declaration
  type module_expr
  type module_expr_desc
  type module_type
  type module_type_declaration
  type module_type_desc
  type mutable_flag
  type object_field
  type open_description
  type override_expression
  type override_flag
  type package_type
  type package_type_constraint
  type pattern
  type pattern_desc
  type payload
  type private_flag
  type rec_flag
  type record_field_expression
  type record_field_pattern
  type row_field
  type signature
  type signature_item
  type signature_item_desc
  type string_loc
  type structure
  type structure_item
  type structure_item_desc
  type toplevel_phrase
  type type_constraint
  type type_declaration
  type type_extension
  type type_kind
  type type_param
  type value_binding
  type value_description
  type variance
  type virtual_flag
  type with_constraint
end

module V4_07 : sig
  module rec Longident : sig
    type t = Unversioned.longident

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Lident of { a : string }
        | Ldot of { a : Longident.t; b : string }
        | Lapply of { a : Longident.t; b : Longident.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val lident : a:string -> t
    val ldot : a:Longident.t -> b:string -> t
    val lapply : a:Longident.t -> b:Longident.t -> t
  end

  and Longident_loc : sig
    type t = Unversioned.longident_loc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Longident_loc of { txt : Longident.t; loc : Location.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val longident_loc : txt:Longident.t -> loc:Location.t -> t
  end

  and Rec_flag : sig
    type t = Unversioned.rec_flag

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Nonrecursive
        | Recursive
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val nonrecursive : t
    val recursive : t
  end

  and Direction_flag : sig
    type t = Unversioned.direction_flag

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Upto
        | Downto
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val upto : t
    val downto_ : t
  end

  and Private_flag : sig
    type t = Unversioned.private_flag

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Private
        | Public
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val private_ : t
    val public : t
  end

  and Mutable_flag : sig
    type t = Unversioned.mutable_flag

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Immutable
        | Mutable
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val immutable : t
    val mutable_ : t
  end

  and Virtual_flag : sig
    type t = Unversioned.virtual_flag

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Virtual
        | Concrete
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val virtual_ : t
    val concrete : t
  end

  and Override_flag : sig
    type t = Unversioned.override_flag

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Override
        | Fresh
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val override : t
    val fresh : t
  end

  and Closed_flag : sig
    type t = Unversioned.closed_flag

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Closed
        | Open
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val closed : t
    val open_ : t
  end

  and Label : sig
    type t = Unversioned.label

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Label of { a : string }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val label : a:string -> t
  end

  and Label_loc : sig
    type t = Unversioned.label_loc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Label_loc of { txt : Label.t; loc : Location.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val label_loc : txt:Label.t -> loc:Location.t -> t
  end

  and String_loc : sig
    type t = Unversioned.string_loc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | String_loc of { txt : string; loc : Location.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val string_loc : txt:string -> loc:Location.t -> t
  end

  and Arg_label : sig
    type t = Unversioned.arg_label

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Nolabel
        | Labelled of { a : string }
        | Optional of { a : string }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val nolabel : t
    val labelled : a:string -> t
    val optional : a:string -> t
  end

  and Variance : sig
    type t = Unversioned.variance

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Covariant
        | Contravariant
        | Invariant
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val covariant : t
    val contravariant : t
    val invariant : t
  end

  and Constant : sig
    type t = Unversioned.constant

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Pconst_integer of { a : string; b : char option }
        | Pconst_char of { a : char }
        | Pconst_string of { a : string; b : string option }
        | Pconst_float of { a : string; b : char option }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val pconst_integer : a:string -> b:char option -> t
    val pconst_char : a:char -> t
    val pconst_string : a:string -> b:string option -> t
    val pconst_float : a:string -> b:char option -> t
  end

  and Attribute : sig
    type t = Unversioned.attribute

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Attribute of { a : String_loc.t; b : Payload.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val attribute : a:String_loc.t -> b:Payload.t -> t
  end

  and Extension : sig
    type t = Unversioned.extension

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Extension of { a : String_loc.t; b : Payload.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val extension : a:String_loc.t -> b:Payload.t -> t
  end

  and Attributes : sig
    type t = Unversioned.attributes

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Attributes of { a : Attribute.t list }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val attributes : a:Attribute.t list -> t
  end

  and Payload : sig
    type t = Unversioned.payload

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | PStr of { a : Structure.t }
        | PSig of { a : Signature.t }
        | PTyp of { a : Core_type.t }
        | PPat of { a : Pattern.t; b : Expression.t option }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val pstr : a:Structure.t -> t
    val psig : a:Signature.t -> t
    val ptyp : a:Core_type.t -> t
    val ppat : a:Pattern.t -> b:Expression.t option -> t
  end

  and Core_type : sig
    type t = Unversioned.core_type

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Core_type of { ptyp_desc : Core_type_desc.t; ptyp_loc : Location.t; ptyp_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val core_type : ptyp_desc:Core_type_desc.t -> ptyp_loc:Location.t -> ptyp_attributes:Attributes.t -> t
  end

  and Core_type_desc : sig
    type t = Unversioned.core_type_desc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Ptyp_any
        | Ptyp_var of { a : string }
        | Ptyp_arrow of { a : Arg_label.t; b : Core_type.t; c : Core_type.t }
        | Ptyp_tuple of { a : Core_type.t list }
        | Ptyp_constr of { a : Longident_loc.t; b : Core_type.t list }
        | Ptyp_object of { a : Object_field.t list; b : Closed_flag.t }
        | Ptyp_class of { a : Longident_loc.t; b : Core_type.t list }
        | Ptyp_alias of { a : Core_type.t; b : string }
        | Ptyp_variant of { a : Row_field.t list; b : Closed_flag.t; c : Label.t list option }
        | Ptyp_poly of { a : String_loc.t list; b : Core_type.t }
        | Ptyp_package of { a : Package_type.t }
        | Ptyp_extension of { a : Extension.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val ptyp_any : t
    val ptyp_var : a:string -> t
    val ptyp_arrow : a:Arg_label.t -> b:Core_type.t -> c:Core_type.t -> t
    val ptyp_tuple : a:Core_type.t list -> t
    val ptyp_constr : a:Longident_loc.t -> b:Core_type.t list -> t
    val ptyp_object : a:Object_field.t list -> b:Closed_flag.t -> t
    val ptyp_class : a:Longident_loc.t -> b:Core_type.t list -> t
    val ptyp_alias : a:Core_type.t -> b:string -> t
    val ptyp_variant : a:Row_field.t list -> b:Closed_flag.t -> c:Label.t list option -> t
    val ptyp_poly : a:String_loc.t list -> b:Core_type.t -> t
    val ptyp_package : a:Package_type.t -> t
    val ptyp_extension : a:Extension.t -> t
  end

  and Package_type : sig
    type t = Unversioned.package_type

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Package_type of { a : Longident_loc.t; b : Package_type_constraint.t list }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val package_type : a:Longident_loc.t -> b:Package_type_constraint.t list -> t
  end

  and Package_type_constraint : sig
    type t = Unversioned.package_type_constraint

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Package_type_constraint of { a : Longident_loc.t; b : Core_type.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val package_type_constraint : a:Longident_loc.t -> b:Core_type.t -> t
  end

  and Row_field : sig
    type t = Unversioned.row_field

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Rtag of { a : Label_loc.t; b : Attributes.t; c : bool; d : Core_type.t list }
        | Rinherit of { a : Core_type.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val rtag : a:Label_loc.t -> b:Attributes.t -> c:bool -> d:Core_type.t list -> t
    val rinherit : a:Core_type.t -> t
  end

  and Object_field : sig
    type t = Unversioned.object_field

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Otag of { a : Label_loc.t; b : Attributes.t; c : Core_type.t }
        | Oinherit of { a : Core_type.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val otag : a:Label_loc.t -> b:Attributes.t -> c:Core_type.t -> t
    val oinherit : a:Core_type.t -> t
  end

  and Pattern : sig
    type t = Unversioned.pattern

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Pattern of { ppat_desc : Pattern_desc.t; ppat_loc : Location.t; ppat_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val pattern : ppat_desc:Pattern_desc.t -> ppat_loc:Location.t -> ppat_attributes:Attributes.t -> t
  end

  and Pattern_desc : sig
    type t = Unversioned.pattern_desc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Ppat_any
        | Ppat_var of { a : String_loc.t }
        | Ppat_alias of { a : Pattern.t; b : String_loc.t }
        | Ppat_constant of { a : Constant.t }
        | Ppat_interval of { a : Constant.t; b : Constant.t }
        | Ppat_tuple of { a : Pattern.t list }
        | Ppat_construct of { a : Longident_loc.t; b : Pattern.t option }
        | Ppat_variant of { a : Label.t; b : Pattern.t option }
        | Ppat_record of { a : Record_field_pattern.t list; b : Closed_flag.t }
        | Ppat_array of { a : Pattern.t list }
        | Ppat_or of { a : Pattern.t; b : Pattern.t }
        | Ppat_constraint of { a : Pattern.t; b : Core_type.t }
        | Ppat_type of { a : Longident_loc.t }
        | Ppat_lazy of { a : Pattern.t }
        | Ppat_unpack of { a : String_loc.t }
        | Ppat_exception of { a : Pattern.t }
        | Ppat_extension of { a : Extension.t }
        | Ppat_open of { a : Longident_loc.t; b : Pattern.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val ppat_any : t
    val ppat_var : a:String_loc.t -> t
    val ppat_alias : a:Pattern.t -> b:String_loc.t -> t
    val ppat_constant : a:Constant.t -> t
    val ppat_interval : a:Constant.t -> b:Constant.t -> t
    val ppat_tuple : a:Pattern.t list -> t
    val ppat_construct : a:Longident_loc.t -> b:Pattern.t option -> t
    val ppat_variant : a:Label.t -> b:Pattern.t option -> t
    val ppat_record : a:Record_field_pattern.t list -> b:Closed_flag.t -> t
    val ppat_array : a:Pattern.t list -> t
    val ppat_or : a:Pattern.t -> b:Pattern.t -> t
    val ppat_constraint : a:Pattern.t -> b:Core_type.t -> t
    val ppat_type : a:Longident_loc.t -> t
    val ppat_lazy : a:Pattern.t -> t
    val ppat_unpack : a:String_loc.t -> t
    val ppat_exception : a:Pattern.t -> t
    val ppat_extension : a:Extension.t -> t
    val ppat_open : a:Longident_loc.t -> b:Pattern.t -> t
  end

  and Record_field_pattern : sig
    type t = Unversioned.record_field_pattern

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Record_field_pattern of { a : Longident_loc.t; b : Pattern.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val record_field_pattern : a:Longident_loc.t -> b:Pattern.t -> t
  end

  and Expression : sig
    type t = Unversioned.expression

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Expression of { pexp_desc : Expression_desc.t; pexp_loc : Location.t; pexp_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val expression : pexp_desc:Expression_desc.t -> pexp_loc:Location.t -> pexp_attributes:Attributes.t -> t
  end

  and Expression_desc : sig
    type t = Unversioned.expression_desc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Pexp_ident of { a : Longident_loc.t }
        | Pexp_constant of { a : Constant.t }
        | Pexp_let of { a : Rec_flag.t; b : Value_binding.t list; c : Expression.t }
        | Pexp_function of { a : Case.t list }
        | Pexp_fun of { a : Arg_label.t; b : Expression.t option; c : Pattern.t; d : Expression.t }
        | Pexp_apply of { a : Expression.t; b : Apply_arg.t list }
        | Pexp_match of { a : Expression.t; b : Case.t list }
        | Pexp_try of { a : Expression.t; b : Case.t list }
        | Pexp_tuple of { a : Expression.t list }
        | Pexp_construct of { a : Longident_loc.t; b : Expression.t option }
        | Pexp_variant of { a : Label.t; b : Expression.t option }
        | Pexp_record of { a : Record_field_expression.t list; b : Expression.t option }
        | Pexp_field of { a : Expression.t; b : Longident_loc.t }
        | Pexp_setfield of { a : Expression.t; b : Longident_loc.t; c : Expression.t }
        | Pexp_array of { a : Expression.t list }
        | Pexp_ifthenelse of { a : Expression.t; b : Expression.t; c : Expression.t option }
        | Pexp_sequence of { a : Expression.t; b : Expression.t }
        | Pexp_while of { a : Expression.t; b : Expression.t }
        | Pexp_for of { a : Pattern.t; b : Expression.t; c : Expression.t; d : Direction_flag.t; e : Expression.t }
        | Pexp_constraint of { a : Expression.t; b : Core_type.t }
        | Pexp_coerce of { a : Expression.t; b : Core_type.t option; c : Core_type.t }
        | Pexp_send of { a : Expression.t; b : Label_loc.t }
        | Pexp_new of { a : Longident_loc.t }
        | Pexp_setinstvar of { a : Label_loc.t; b : Expression.t }
        | Pexp_override of { a : Override_expression.t list }
        | Pexp_letmodule of { a : String_loc.t; b : Module_expr.t; c : Expression.t }
        | Pexp_letexception of { a : Extension_constructor.t; b : Expression.t }
        | Pexp_assert of { a : Expression.t }
        | Pexp_lazy of { a : Expression.t }
        | Pexp_poly of { a : Expression.t; b : Core_type.t option }
        | Pexp_object of { a : Class_structure.t }
        | Pexp_newtype of { a : String_loc.t; b : Expression.t }
        | Pexp_pack of { a : Module_expr.t }
        | Pexp_open of { a : Override_flag.t; b : Longident_loc.t; c : Expression.t }
        | Pexp_extension of { a : Extension.t }
        | Pexp_unreachable
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val pexp_ident : a:Longident_loc.t -> t
    val pexp_constant : a:Constant.t -> t
    val pexp_let : a:Rec_flag.t -> b:Value_binding.t list -> c:Expression.t -> t
    val pexp_function : a:Case.t list -> t
    val pexp_fun : a:Arg_label.t -> b:Expression.t option -> c:Pattern.t -> d:Expression.t -> t
    val pexp_apply : a:Expression.t -> b:Apply_arg.t list -> t
    val pexp_match : a:Expression.t -> b:Case.t list -> t
    val pexp_try : a:Expression.t -> b:Case.t list -> t
    val pexp_tuple : a:Expression.t list -> t
    val pexp_construct : a:Longident_loc.t -> b:Expression.t option -> t
    val pexp_variant : a:Label.t -> b:Expression.t option -> t
    val pexp_record : a:Record_field_expression.t list -> b:Expression.t option -> t
    val pexp_field : a:Expression.t -> b:Longident_loc.t -> t
    val pexp_setfield : a:Expression.t -> b:Longident_loc.t -> c:Expression.t -> t
    val pexp_array : a:Expression.t list -> t
    val pexp_ifthenelse : a:Expression.t -> b:Expression.t -> c:Expression.t option -> t
    val pexp_sequence : a:Expression.t -> b:Expression.t -> t
    val pexp_while : a:Expression.t -> b:Expression.t -> t
    val pexp_for : a:Pattern.t -> b:Expression.t -> c:Expression.t -> d:Direction_flag.t -> e:Expression.t -> t
    val pexp_constraint : a:Expression.t -> b:Core_type.t -> t
    val pexp_coerce : a:Expression.t -> b:Core_type.t option -> c:Core_type.t -> t
    val pexp_send : a:Expression.t -> b:Label_loc.t -> t
    val pexp_new : a:Longident_loc.t -> t
    val pexp_setinstvar : a:Label_loc.t -> b:Expression.t -> t
    val pexp_override : a:Override_expression.t list -> t
    val pexp_letmodule : a:String_loc.t -> b:Module_expr.t -> c:Expression.t -> t
    val pexp_letexception : a:Extension_constructor.t -> b:Expression.t -> t
    val pexp_assert : a:Expression.t -> t
    val pexp_lazy : a:Expression.t -> t
    val pexp_poly : a:Expression.t -> b:Core_type.t option -> t
    val pexp_object : a:Class_structure.t -> t
    val pexp_newtype : a:String_loc.t -> b:Expression.t -> t
    val pexp_pack : a:Module_expr.t -> t
    val pexp_open : a:Override_flag.t -> b:Longident_loc.t -> c:Expression.t -> t
    val pexp_extension : a:Extension.t -> t
    val pexp_unreachable : t
  end

  and Override_expression : sig
    type t = Unversioned.override_expression

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Override_expression of { a : Label_loc.t; b : Expression.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val override_expression : a:Label_loc.t -> b:Expression.t -> t
  end

  and Record_field_expression : sig
    type t = Unversioned.record_field_expression

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Record_field_expression of { a : Longident_loc.t; b : Expression.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val record_field_expression : a:Longident_loc.t -> b:Expression.t -> t
  end

  and Apply_arg : sig
    type t = Unversioned.apply_arg

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Apply_arg of { a : Arg_label.t; b : Expression.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val apply_arg : a:Arg_label.t -> b:Expression.t -> t
  end

  and Case : sig
    type t = Unversioned.case

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Case of { pc_lhs : Pattern.t; pc_guard : Expression.t option; pc_rhs : Expression.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val case : pc_lhs:Pattern.t -> pc_guard:Expression.t option -> pc_rhs:Expression.t -> t
  end

  and Value_description : sig
    type t = Unversioned.value_description

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Value_description of { pval_name : String_loc.t; pval_type : Core_type.t; pval_prim : string list; pval_attributes : Attributes.t; pval_loc : Location.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val value_description : pval_name:String_loc.t -> pval_type:Core_type.t -> pval_prim:string list -> pval_attributes:Attributes.t -> pval_loc:Location.t -> t
  end

  and Type_declaration : sig
    type t = Unversioned.type_declaration

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Type_declaration of { ptype_name : String_loc.t; ptype_params : Type_param.t list; ptype_cstrs : Type_constraint.t list; ptype_kind : Type_kind.t; ptype_private : Private_flag.t; ptype_manifest : Core_type.t option; ptype_attributes : Attributes.t; ptype_loc : Location.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val type_declaration : ptype_name:String_loc.t -> ptype_params:Type_param.t list -> ptype_cstrs:Type_constraint.t list -> ptype_kind:Type_kind.t -> ptype_private:Private_flag.t -> ptype_manifest:Core_type.t option -> ptype_attributes:Attributes.t -> ptype_loc:Location.t -> t
  end

  and Type_param : sig
    type t = Unversioned.type_param

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Type_param of { a : Core_type.t; b : Variance.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val type_param : a:Core_type.t -> b:Variance.t -> t
  end

  and Type_constraint : sig
    type t = Unversioned.type_constraint

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Type_constraint of { a : Core_type.t; b : Core_type.t; c : Location.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val type_constraint : a:Core_type.t -> b:Core_type.t -> c:Location.t -> t
  end

  and Type_kind : sig
    type t = Unversioned.type_kind

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Ptype_abstract
        | Ptype_variant of { a : Constructor_declaration.t list }
        | Ptype_record of { a : Label_declaration.t list }
        | Ptype_open
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val ptype_abstract : t
    val ptype_variant : a:Constructor_declaration.t list -> t
    val ptype_record : a:Label_declaration.t list -> t
    val ptype_open : t
  end

  and Label_declaration : sig
    type t = Unversioned.label_declaration

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Label_declaration of { pld_name : String_loc.t; pld_mutable : Mutable_flag.t; pld_type : Core_type.t; pld_loc : Location.t; pld_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val label_declaration : pld_name:String_loc.t -> pld_mutable:Mutable_flag.t -> pld_type:Core_type.t -> pld_loc:Location.t -> pld_attributes:Attributes.t -> t
  end

  and Constructor_declaration : sig
    type t = Unversioned.constructor_declaration

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Constructor_declaration of { pcd_name : String_loc.t; pcd_args : Constructor_arguments.t; pcd_res : Core_type.t option; pcd_loc : Location.t; pcd_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val constructor_declaration : pcd_name:String_loc.t -> pcd_args:Constructor_arguments.t -> pcd_res:Core_type.t option -> pcd_loc:Location.t -> pcd_attributes:Attributes.t -> t
  end

  and Constructor_arguments : sig
    type t = Unversioned.constructor_arguments

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Pcstr_tuple of { a : Core_type.t list }
        | Pcstr_record of { a : Label_declaration.t list }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val pcstr_tuple : a:Core_type.t list -> t
    val pcstr_record : a:Label_declaration.t list -> t
  end

  and Type_extension : sig
    type t = Unversioned.type_extension

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Type_extension of { ptyext_path : Longident_loc.t; ptyext_params : Type_param.t list; ptyext_constructors : Extension_constructor.t list; ptyext_private : Private_flag.t; ptyext_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val type_extension : ptyext_path:Longident_loc.t -> ptyext_params:Type_param.t list -> ptyext_constructors:Extension_constructor.t list -> ptyext_private:Private_flag.t -> ptyext_attributes:Attributes.t -> t
  end

  and Extension_constructor : sig
    type t = Unversioned.extension_constructor

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Extension_constructor of { pext_name : String_loc.t; pext_kind : Extension_constructor_kind.t; pext_loc : Location.t; pext_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val extension_constructor : pext_name:String_loc.t -> pext_kind:Extension_constructor_kind.t -> pext_loc:Location.t -> pext_attributes:Attributes.t -> t
  end

  and Extension_constructor_kind : sig
    type t = Unversioned.extension_constructor_kind

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Pext_decl of { a : Constructor_arguments.t; b : Core_type.t option }
        | Pext_rebind of { a : Longident_loc.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val pext_decl : a:Constructor_arguments.t -> b:Core_type.t option -> t
    val pext_rebind : a:Longident_loc.t -> t
  end

  and Class_type : sig
    type t = Unversioned.class_type

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Class_type of { pcty_desc : Class_type_desc.t; pcty_loc : Location.t; pcty_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val class_type : pcty_desc:Class_type_desc.t -> pcty_loc:Location.t -> pcty_attributes:Attributes.t -> t
  end

  and Class_type_desc : sig
    type t = Unversioned.class_type_desc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Pcty_constr of { a : Longident_loc.t; b : Core_type.t list }
        | Pcty_signature of { a : Class_signature.t }
        | Pcty_arrow of { a : Arg_label.t; b : Core_type.t; c : Class_type.t }
        | Pcty_extension of { a : Extension.t }
        | Pcty_open of { a : Override_flag.t; b : Longident_loc.t; c : Class_type.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val pcty_constr : a:Longident_loc.t -> b:Core_type.t list -> t
    val pcty_signature : a:Class_signature.t -> t
    val pcty_arrow : a:Arg_label.t -> b:Core_type.t -> c:Class_type.t -> t
    val pcty_extension : a:Extension.t -> t
    val pcty_open : a:Override_flag.t -> b:Longident_loc.t -> c:Class_type.t -> t
  end

  and Class_signature : sig
    type t = Unversioned.class_signature

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Class_signature of { pcsig_self : Core_type.t; pcsig_fields : Class_type_field.t list }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val class_signature : pcsig_self:Core_type.t -> pcsig_fields:Class_type_field.t list -> t
  end

  and Class_type_field : sig
    type t = Unversioned.class_type_field

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Class_type_field of { pctf_desc : Class_type_field_desc.t; pctf_loc : Location.t; pctf_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val class_type_field : pctf_desc:Class_type_field_desc.t -> pctf_loc:Location.t -> pctf_attributes:Attributes.t -> t
  end

  and Class_type_field_desc : sig
    type t = Unversioned.class_type_field_desc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Pctf_inherit of { a : Class_type.t }
        | Pctf_val of { a : Class_type_value_desc.t }
        | Pctf_method of { a : Class_type_method_desc.t }
        | Pctf_constraint of { a : Class_type_constraint.t }
        | Pctf_attribute of { a : Attribute.t }
        | Pctf_extension of { a : Extension.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val pctf_inherit : a:Class_type.t -> t
    val pctf_val : a:Class_type_value_desc.t -> t
    val pctf_method : a:Class_type_method_desc.t -> t
    val pctf_constraint : a:Class_type_constraint.t -> t
    val pctf_attribute : a:Attribute.t -> t
    val pctf_extension : a:Extension.t -> t
  end

  and Class_type_value_desc : sig
    type t = Unversioned.class_type_value_desc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Class_type_value_desc of { a : Label_loc.t; b : Mutable_flag.t; c : Virtual_flag.t; d : Core_type.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val class_type_value_desc : a:Label_loc.t -> b:Mutable_flag.t -> c:Virtual_flag.t -> d:Core_type.t -> t
  end

  and Class_type_method_desc : sig
    type t = Unversioned.class_type_method_desc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Class_type_method_desc of { a : Label_loc.t; b : Private_flag.t; c : Virtual_flag.t; d : Core_type.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val class_type_method_desc : a:Label_loc.t -> b:Private_flag.t -> c:Virtual_flag.t -> d:Core_type.t -> t
  end

  and Class_type_constraint : sig
    type t = Unversioned.class_type_constraint

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Class_type_constraint of { a : Core_type.t; b : Core_type.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val class_type_constraint : a:Core_type.t -> b:Core_type.t -> t
  end

  and Class_description : sig
    type t = Unversioned.class_description

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Class_description of { pci_virt : Virtual_flag.t; pci_params : Type_param.t list; pci_name : String_loc.t; pci_expr : Class_type.t; pci_loc : Location.t; pci_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val class_description : pci_virt:Virtual_flag.t -> pci_params:Type_param.t list -> pci_name:String_loc.t -> pci_expr:Class_type.t -> pci_loc:Location.t -> pci_attributes:Attributes.t -> t
  end

  and Class_type_declaration : sig
    type t = Unversioned.class_type_declaration

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Class_type_declaration of { pci_virt : Virtual_flag.t; pci_params : Type_param.t list; pci_name : String_loc.t; pci_expr : Class_type.t; pci_loc : Location.t; pci_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val class_type_declaration : pci_virt:Virtual_flag.t -> pci_params:Type_param.t list -> pci_name:String_loc.t -> pci_expr:Class_type.t -> pci_loc:Location.t -> pci_attributes:Attributes.t -> t
  end

  and Class_expr : sig
    type t = Unversioned.class_expr

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Class_expr of { pcl_desc : Class_expr_desc.t; pcl_loc : Location.t; pcl_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val class_expr : pcl_desc:Class_expr_desc.t -> pcl_loc:Location.t -> pcl_attributes:Attributes.t -> t
  end

  and Class_expr_desc : sig
    type t = Unversioned.class_expr_desc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Pcl_constr of { a : Longident_loc.t; b : Core_type.t list }
        | Pcl_structure of { a : Class_structure.t }
        | Pcl_fun of { a : Arg_label.t; b : Expression.t option; c : Pattern.t; d : Class_expr.t }
        | Pcl_apply of { a : Class_expr.t; b : Apply_arg.t list }
        | Pcl_let of { a : Rec_flag.t; b : Value_binding.t list; c : Class_expr.t }
        | Pcl_constraint of { a : Class_expr.t; b : Class_type.t }
        | Pcl_extension of { a : Extension.t }
        | Pcl_open of { a : Override_flag.t; b : Longident_loc.t; c : Class_expr.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val pcl_constr : a:Longident_loc.t -> b:Core_type.t list -> t
    val pcl_structure : a:Class_structure.t -> t
    val pcl_fun : a:Arg_label.t -> b:Expression.t option -> c:Pattern.t -> d:Class_expr.t -> t
    val pcl_apply : a:Class_expr.t -> b:Apply_arg.t list -> t
    val pcl_let : a:Rec_flag.t -> b:Value_binding.t list -> c:Class_expr.t -> t
    val pcl_constraint : a:Class_expr.t -> b:Class_type.t -> t
    val pcl_extension : a:Extension.t -> t
    val pcl_open : a:Override_flag.t -> b:Longident_loc.t -> c:Class_expr.t -> t
  end

  and Class_structure : sig
    type t = Unversioned.class_structure

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Class_structure of { pcstr_self : Pattern.t; pcstr_fields : Class_field.t list }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val class_structure : pcstr_self:Pattern.t -> pcstr_fields:Class_field.t list -> t
  end

  and Class_field : sig
    type t = Unversioned.class_field

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Class_field of { pcf_desc : Class_field_desc.t; pcf_loc : Location.t; pcf_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val class_field : pcf_desc:Class_field_desc.t -> pcf_loc:Location.t -> pcf_attributes:Attributes.t -> t
  end

  and Class_field_desc : sig
    type t = Unversioned.class_field_desc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Pcf_inherit of { a : Override_flag.t; b : Class_expr.t; c : String_loc.t option }
        | Pcf_val of { a : Class_value_desc.t }
        | Pcf_method of { a : Class_method_desc.t }
        | Pcf_constraint of { a : Class_type_constraint.t }
        | Pcf_initializer of { a : Expression.t }
        | Pcf_attribute of { a : Attribute.t }
        | Pcf_extension of { a : Extension.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val pcf_inherit : a:Override_flag.t -> b:Class_expr.t -> c:String_loc.t option -> t
    val pcf_val : a:Class_value_desc.t -> t
    val pcf_method : a:Class_method_desc.t -> t
    val pcf_constraint : a:Class_type_constraint.t -> t
    val pcf_initializer : a:Expression.t -> t
    val pcf_attribute : a:Attribute.t -> t
    val pcf_extension : a:Extension.t -> t
  end

  and Class_value_desc : sig
    type t = Unversioned.class_value_desc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Class_value_desc of { a : Label_loc.t; b : Mutable_flag.t; c : Class_field_kind.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val class_value_desc : a:Label_loc.t -> b:Mutable_flag.t -> c:Class_field_kind.t -> t
  end

  and Class_method_desc : sig
    type t = Unversioned.class_method_desc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Class_method_desc of { a : Label_loc.t; b : Private_flag.t; c : Class_field_kind.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val class_method_desc : a:Label_loc.t -> b:Private_flag.t -> c:Class_field_kind.t -> t
  end

  and Class_field_kind : sig
    type t = Unversioned.class_field_kind

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Cfk_virtual of { a : Core_type.t }
        | Cfk_concrete of { a : Override_flag.t; b : Expression.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val cfk_virtual : a:Core_type.t -> t
    val cfk_concrete : a:Override_flag.t -> b:Expression.t -> t
  end

  and Class_declaration : sig
    type t = Unversioned.class_declaration

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Class_declaration of { pci_virt : Virtual_flag.t; pci_params : Type_param.t list; pci_name : String_loc.t; pci_expr : Class_expr.t; pci_loc : Location.t; pci_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val class_declaration : pci_virt:Virtual_flag.t -> pci_params:Type_param.t list -> pci_name:String_loc.t -> pci_expr:Class_expr.t -> pci_loc:Location.t -> pci_attributes:Attributes.t -> t
  end

  and Module_type : sig
    type t = Unversioned.module_type

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Module_type of { pmty_desc : Module_type_desc.t; pmty_loc : Location.t; pmty_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val module_type : pmty_desc:Module_type_desc.t -> pmty_loc:Location.t -> pmty_attributes:Attributes.t -> t
  end

  and Module_type_desc : sig
    type t = Unversioned.module_type_desc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Pmty_ident of { a : Longident_loc.t }
        | Pmty_signature of { a : Signature.t }
        | Pmty_functor of { a : String_loc.t; b : Module_type.t option; c : Module_type.t }
        | Pmty_with of { a : Module_type.t; b : With_constraint.t list }
        | Pmty_typeof of { a : Module_expr.t }
        | Pmty_extension of { a : Extension.t }
        | Pmty_alias of { a : Longident_loc.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val pmty_ident : a:Longident_loc.t -> t
    val pmty_signature : a:Signature.t -> t
    val pmty_functor : a:String_loc.t -> b:Module_type.t option -> c:Module_type.t -> t
    val pmty_with : a:Module_type.t -> b:With_constraint.t list -> t
    val pmty_typeof : a:Module_expr.t -> t
    val pmty_extension : a:Extension.t -> t
    val pmty_alias : a:Longident_loc.t -> t
  end

  and Signature : sig
    type t = Unversioned.signature

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Signature of { a : Signature_item.t list }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val signature : a:Signature_item.t list -> t
  end

  and Signature_item : sig
    type t = Unversioned.signature_item

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Signature_item of { psig_desc : Signature_item_desc.t; psig_loc : Location.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val signature_item : psig_desc:Signature_item_desc.t -> psig_loc:Location.t -> t
  end

  and Signature_item_desc : sig
    type t = Unversioned.signature_item_desc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Psig_value of { a : Value_description.t }
        | Psig_type of { a : Rec_flag.t; b : Type_declaration.t list }
        | Psig_typext of { a : Type_extension.t }
        | Psig_exception of { a : Extension_constructor.t }
        | Psig_module of { a : Module_declaration.t }
        | Psig_recmodule of { a : Module_declaration.t list }
        | Psig_modtype of { a : Module_type_declaration.t }
        | Psig_open of { a : Open_description.t }
        | Psig_include of { a : Include_description.t }
        | Psig_class of { a : Class_description.t list }
        | Psig_class_type of { a : Class_type_declaration.t list }
        | Psig_attribute of { a : Attribute.t }
        | Psig_extension of { a : Extension.t; b : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val psig_value : a:Value_description.t -> t
    val psig_type : a:Rec_flag.t -> b:Type_declaration.t list -> t
    val psig_typext : a:Type_extension.t -> t
    val psig_exception : a:Extension_constructor.t -> t
    val psig_module : a:Module_declaration.t -> t
    val psig_recmodule : a:Module_declaration.t list -> t
    val psig_modtype : a:Module_type_declaration.t -> t
    val psig_open : a:Open_description.t -> t
    val psig_include : a:Include_description.t -> t
    val psig_class : a:Class_description.t list -> t
    val psig_class_type : a:Class_type_declaration.t list -> t
    val psig_attribute : a:Attribute.t -> t
    val psig_extension : a:Extension.t -> b:Attributes.t -> t
  end

  and Module_declaration : sig
    type t = Unversioned.module_declaration

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Module_declaration of { pmd_name : String_loc.t; pmd_type : Module_type.t; pmd_attributes : Attributes.t; pmd_loc : Location.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val module_declaration : pmd_name:String_loc.t -> pmd_type:Module_type.t -> pmd_attributes:Attributes.t -> pmd_loc:Location.t -> t
  end

  and Module_type_declaration : sig
    type t = Unversioned.module_type_declaration

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Module_type_declaration of { pmtd_name : String_loc.t; pmtd_type : Module_type.t option; pmtd_attributes : Attributes.t; pmtd_loc : Location.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val module_type_declaration : pmtd_name:String_loc.t -> pmtd_type:Module_type.t option -> pmtd_attributes:Attributes.t -> pmtd_loc:Location.t -> t
  end

  and Open_description : sig
    type t = Unversioned.open_description

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Open_description of { popen_lid : Longident_loc.t; popen_override : Override_flag.t; popen_loc : Location.t; popen_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val open_description : popen_lid:Longident_loc.t -> popen_override:Override_flag.t -> popen_loc:Location.t -> popen_attributes:Attributes.t -> t
  end

  and Include_description : sig
    type t = Unversioned.include_description

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Include_description of { pincl_mod : Module_type.t; pincl_loc : Location.t; pincl_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val include_description : pincl_mod:Module_type.t -> pincl_loc:Location.t -> pincl_attributes:Attributes.t -> t
  end

  and Include_declaration : sig
    type t = Unversioned.include_declaration

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Include_declaration of { pincl_mod : Module_expr.t; pincl_loc : Location.t; pincl_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val include_declaration : pincl_mod:Module_expr.t -> pincl_loc:Location.t -> pincl_attributes:Attributes.t -> t
  end

  and With_constraint : sig
    type t = Unversioned.with_constraint

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Pwith_type of { a : Longident_loc.t; b : Type_declaration.t }
        | Pwith_module of { a : Longident_loc.t; b : Longident_loc.t }
        | Pwith_typesubst of { a : Longident_loc.t; b : Type_declaration.t }
        | Pwith_modsubst of { a : Longident_loc.t; b : Longident_loc.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val pwith_type : a:Longident_loc.t -> b:Type_declaration.t -> t
    val pwith_module : a:Longident_loc.t -> b:Longident_loc.t -> t
    val pwith_typesubst : a:Longident_loc.t -> b:Type_declaration.t -> t
    val pwith_modsubst : a:Longident_loc.t -> b:Longident_loc.t -> t
  end

  and Module_expr : sig
    type t = Unversioned.module_expr

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Module_expr of { pmod_desc : Module_expr_desc.t; pmod_loc : Location.t; pmod_attributes : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val module_expr : pmod_desc:Module_expr_desc.t -> pmod_loc:Location.t -> pmod_attributes:Attributes.t -> t
  end

  and Module_expr_desc : sig
    type t = Unversioned.module_expr_desc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Pmod_ident of { a : Longident_loc.t }
        | Pmod_structure of { a : Structure.t }
        | Pmod_functor of { a : String_loc.t; b : Module_type.t option; c : Module_expr.t }
        | Pmod_apply of { a : Module_expr.t; b : Module_expr.t }
        | Pmod_constraint of { a : Module_expr.t; b : Module_type.t }
        | Pmod_unpack of { a : Expression.t }
        | Pmod_extension of { a : Extension.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val pmod_ident : a:Longident_loc.t -> t
    val pmod_structure : a:Structure.t -> t
    val pmod_functor : a:String_loc.t -> b:Module_type.t option -> c:Module_expr.t -> t
    val pmod_apply : a:Module_expr.t -> b:Module_expr.t -> t
    val pmod_constraint : a:Module_expr.t -> b:Module_type.t -> t
    val pmod_unpack : a:Expression.t -> t
    val pmod_extension : a:Extension.t -> t
  end

  and Structure : sig
    type t = Unversioned.structure

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Structure of { a : Structure_item.t list }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val structure : a:Structure_item.t list -> t
  end

  and Structure_item : sig
    type t = Unversioned.structure_item

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Structure_item of { pstr_desc : Structure_item_desc.t; pstr_loc : Location.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val structure_item : pstr_desc:Structure_item_desc.t -> pstr_loc:Location.t -> t
  end

  and Structure_item_desc : sig
    type t = Unversioned.structure_item_desc

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Pstr_eval of { a : Expression.t; b : Attributes.t }
        | Pstr_value of { a : Rec_flag.t; b : Value_binding.t list }
        | Pstr_primitive of { a : Value_description.t }
        | Pstr_type of { a : Rec_flag.t; b : Type_declaration.t list }
        | Pstr_typext of { a : Type_extension.t }
        | Pstr_exception of { a : Extension_constructor.t }
        | Pstr_module of { a : Module_binding.t }
        | Pstr_recmodule of { a : Module_binding.t list }
        | Pstr_modtype of { a : Module_type_declaration.t }
        | Pstr_open of { a : Open_description.t }
        | Pstr_class of { a : Class_declaration.t list }
        | Pstr_class_type of { a : Class_type_declaration.t list }
        | Pstr_include of { a : Include_declaration.t }
        | Pstr_attribute of { a : Attribute.t }
        | Pstr_extension of { a : Extension.t; b : Attributes.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val pstr_eval : a:Expression.t -> b:Attributes.t -> t
    val pstr_value : a:Rec_flag.t -> b:Value_binding.t list -> t
    val pstr_primitive : a:Value_description.t -> t
    val pstr_type : a:Rec_flag.t -> b:Type_declaration.t list -> t
    val pstr_typext : a:Type_extension.t -> t
    val pstr_exception : a:Extension_constructor.t -> t
    val pstr_module : a:Module_binding.t -> t
    val pstr_recmodule : a:Module_binding.t list -> t
    val pstr_modtype : a:Module_type_declaration.t -> t
    val pstr_open : a:Open_description.t -> t
    val pstr_class : a:Class_declaration.t list -> t
    val pstr_class_type : a:Class_type_declaration.t list -> t
    val pstr_include : a:Include_declaration.t -> t
    val pstr_attribute : a:Attribute.t -> t
    val pstr_extension : a:Extension.t -> b:Attributes.t -> t
  end

  and Value_binding : sig
    type t = Unversioned.value_binding

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Value_binding of { pvb_pat : Pattern.t; pvb_expr : Expression.t; pvb_attributes : Attributes.t; pvb_loc : Location.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val value_binding : pvb_pat:Pattern.t -> pvb_expr:Expression.t -> pvb_attributes:Attributes.t -> pvb_loc:Location.t -> t
  end

  and Module_binding : sig
    type t = Unversioned.module_binding

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Module_binding of { pmb_name : String_loc.t; pmb_expr : Module_expr.t; pmb_attributes : Attributes.t; pmb_loc : Location.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val module_binding : pmb_name:String_loc.t -> pmb_expr:Module_expr.t -> pmb_attributes:Attributes.t -> pmb_loc:Location.t -> t
  end

  and Toplevel_phrase : sig
    type t = Unversioned.toplevel_phrase

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Ptop_def of { a : Structure.t }
        | Ptop_dir of { a : string; b : Directive_argument.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val ptop_def : a:Structure.t -> t
    val ptop_dir : a:string -> b:Directive_argument.t -> t
  end

  and Directive_argument : sig
    type t = Unversioned.directive_argument

    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Pdir_none
        | Pdir_string of { a : string }
        | Pdir_int of { a : string; b : char option }
        | Pdir_ident of { a : Longident.t }
        | Pdir_bool of { a : bool }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
    val pdir_none : t
    val pdir_string : a:string -> t
    val pdir_int : a:string -> b:char option -> t
    val pdir_ident : a:Longident.t -> t
    val pdir_bool : a:bool -> t
  end
end
(*$*)
