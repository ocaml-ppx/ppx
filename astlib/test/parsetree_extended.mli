(*$ Astlib_test_cinaps.print_parsetree_extended_mli () *)
open! Base

module Loc : sig
  type 'a t = 'a Astlib_parsetree.loc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Longident : sig
  type t = Astlib_parsetree.longident
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Longident_loc : sig
  type t = Astlib_parsetree.longident_loc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Rec_flag : sig
  type t = Astlib_parsetree.rec_flag
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Direction_flag : sig
  type t = Astlib_parsetree.direction_flag
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Private_flag : sig
  type t = Astlib_parsetree.private_flag
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Mutable_flag : sig
  type t = Astlib_parsetree.mutable_flag
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Virtual_flag : sig
  type t = Astlib_parsetree.virtual_flag
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Override_flag : sig
  type t = Astlib_parsetree.override_flag
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Closed_flag : sig
  type t = Astlib_parsetree.closed_flag
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Label : sig
  type t = Astlib_parsetree.label
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Label_loc : sig
  type t = Astlib_parsetree.label_loc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module String_loc : sig
  type t = Astlib_parsetree.string_loc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Arg_label : sig
  type t = Astlib_parsetree.arg_label
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Variance : sig
  type t = Astlib_parsetree.variance
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Constant : sig
  type t = Astlib_parsetree.constant
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Attribute : sig
  type t = Astlib_parsetree.attribute
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Extension : sig
  type t = Astlib_parsetree.extension
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Attributes : sig
  type t = Astlib_parsetree.attributes
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Payload : sig
  type t = Astlib_parsetree.payload
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Core_type : sig
  type t = Astlib_parsetree.core_type
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Core_type_desc : sig
  type t = Astlib_parsetree.core_type_desc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Package_type : sig
  type t = Astlib_parsetree.package_type
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Package_type_constraint : sig
  type t = Astlib_parsetree.package_type_constraint
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Row_field : sig
  type t = Astlib_parsetree.row_field
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Object_field : sig
  type t = Astlib_parsetree.object_field
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Pattern : sig
  type t = Astlib_parsetree.pattern
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Pattern_desc : sig
  type t = Astlib_parsetree.pattern_desc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Record_field_pattern : sig
  type t = Astlib_parsetree.record_field_pattern
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Expression : sig
  type t = Astlib_parsetree.expression
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Expression_desc : sig
  type t = Astlib_parsetree.expression_desc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Override_expression : sig
  type t = Astlib_parsetree.override_expression
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Record_field_expression : sig
  type t = Astlib_parsetree.record_field_expression
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Apply_arg : sig
  type t = Astlib_parsetree.apply_arg
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Case : sig
  type t = Astlib_parsetree.case
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Value_description : sig
  type t = Astlib_parsetree.value_description
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Type_declaration : sig
  type t = Astlib_parsetree.type_declaration
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Type_param : sig
  type t = Astlib_parsetree.type_param
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Type_constraint : sig
  type t = Astlib_parsetree.type_constraint
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Type_kind : sig
  type t = Astlib_parsetree.type_kind
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Label_declaration : sig
  type t = Astlib_parsetree.label_declaration
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Constructor_declaration : sig
  type t = Astlib_parsetree.constructor_declaration
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Constructor_arguments : sig
  type t = Astlib_parsetree.constructor_arguments
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Type_extension : sig
  type t = Astlib_parsetree.type_extension
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Extension_constructor : sig
  type t = Astlib_parsetree.extension_constructor
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Extension_constructor_kind : sig
  type t = Astlib_parsetree.extension_constructor_kind
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_type : sig
  type t = Astlib_parsetree.class_type
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_type_desc : sig
  type t = Astlib_parsetree.class_type_desc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_signature : sig
  type t = Astlib_parsetree.class_signature
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_type_field : sig
  type t = Astlib_parsetree.class_type_field
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_type_field_desc : sig
  type t = Astlib_parsetree.class_type_field_desc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_type_value_desc : sig
  type t = Astlib_parsetree.class_type_value_desc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_type_method_desc : sig
  type t = Astlib_parsetree.class_type_method_desc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_type_constraint : sig
  type t = Astlib_parsetree.class_type_constraint
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_infos : sig
  type 'a t = 'a Astlib_parsetree.class_infos
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_description : sig
  type t = Astlib_parsetree.class_description
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_type_declaration : sig
  type t = Astlib_parsetree.class_type_declaration
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_expr : sig
  type t = Astlib_parsetree.class_expr
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_expr_desc : sig
  type t = Astlib_parsetree.class_expr_desc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_structure : sig
  type t = Astlib_parsetree.class_structure
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_field : sig
  type t = Astlib_parsetree.class_field
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_field_desc : sig
  type t = Astlib_parsetree.class_field_desc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_value_desc : sig
  type t = Astlib_parsetree.class_value_desc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_method_desc : sig
  type t = Astlib_parsetree.class_method_desc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_field_kind : sig
  type t = Astlib_parsetree.class_field_kind
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Class_declaration : sig
  type t = Astlib_parsetree.class_declaration
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Module_type : sig
  type t = Astlib_parsetree.module_type
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Module_type_desc : sig
  type t = Astlib_parsetree.module_type_desc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Signature : sig
  type t = Astlib_parsetree.signature
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Signature_item : sig
  type t = Astlib_parsetree.signature_item
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Signature_item_desc : sig
  type t = Astlib_parsetree.signature_item_desc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Module_declaration : sig
  type t = Astlib_parsetree.module_declaration
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Module_type_declaration : sig
  type t = Astlib_parsetree.module_type_declaration
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Open_description : sig
  type t = Astlib_parsetree.open_description
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Include_infos : sig
  type 'a t = 'a Astlib_parsetree.include_infos
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Include_description : sig
  type t = Astlib_parsetree.include_description
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Include_declaration : sig
  type t = Astlib_parsetree.include_declaration
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module With_constraint : sig
  type t = Astlib_parsetree.with_constraint
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Module_expr : sig
  type t = Astlib_parsetree.module_expr
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Module_expr_desc : sig
  type t = Astlib_parsetree.module_expr_desc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Structure : sig
  type t = Astlib_parsetree.structure
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Structure_item : sig
  type t = Astlib_parsetree.structure_item
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Structure_item_desc : sig
  type t = Astlib_parsetree.structure_item_desc
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Value_binding : sig
  type t = Astlib_parsetree.value_binding
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Module_binding : sig
  type t = Astlib_parsetree.module_binding
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Toplevel_phrase : sig
  type t = Astlib_parsetree.toplevel_phrase
  [@@deriving compare, equal, quickcheck, sexp_of]
end

module Directive_argument : sig
  type t = Astlib_parsetree.directive_argument
  [@@deriving compare, equal, quickcheck, sexp_of]
end
(*$*)
