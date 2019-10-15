open Ppx_ast_versioned

(*$ Ppx_ast_versioned_tests_cinaps.print_deriving_mli () *)
module Longident : sig
  type t = Compiler_types.longident
  [@@deriving equal, quickcheck, sexp_of]
end

module Longident_loc : sig
  type t = Compiler_types.longident_loc
  [@@deriving equal, quickcheck, sexp_of]
end

module Rec_flag : sig
  type t = Compiler_types.rec_flag
  [@@deriving equal, quickcheck, sexp_of]
end

module Direction_flag : sig
  type t = Compiler_types.direction_flag
  [@@deriving equal, quickcheck, sexp_of]
end

module Private_flag : sig
  type t = Compiler_types.private_flag
  [@@deriving equal, quickcheck, sexp_of]
end

module Mutable_flag : sig
  type t = Compiler_types.mutable_flag
  [@@deriving equal, quickcheck, sexp_of]
end

module Virtual_flag : sig
  type t = Compiler_types.virtual_flag
  [@@deriving equal, quickcheck, sexp_of]
end

module Override_flag : sig
  type t = Compiler_types.override_flag
  [@@deriving equal, quickcheck, sexp_of]
end

module Closed_flag : sig
  type t = Compiler_types.closed_flag
  [@@deriving equal, quickcheck, sexp_of]
end

module Label : sig
  type t = Compiler_types.label
  [@@deriving equal, quickcheck, sexp_of]
end

module Arg_label : sig
  type t = Compiler_types.arg_label
  [@@deriving equal, quickcheck, sexp_of]
end

module Variance : sig
  type t = Compiler_types.variance
  [@@deriving equal, quickcheck, sexp_of]
end

module Constant : sig
  type t = Compiler_types.constant
  [@@deriving equal, quickcheck, sexp_of]
end

module Attribute : sig
  type t = Compiler_types.attribute
  [@@deriving equal, quickcheck, sexp_of]
end

module Extension : sig
  type t = Compiler_types.extension
  [@@deriving equal, quickcheck, sexp_of]
end

module Attributes : sig
  type t = Compiler_types.attributes
  [@@deriving equal, quickcheck, sexp_of]
end

module Payload : sig
  type t = Compiler_types.payload
  [@@deriving equal, quickcheck, sexp_of]
end

module Core_type : sig
  type t = Compiler_types.core_type
  [@@deriving equal, quickcheck, sexp_of]
end

module Core_type_desc : sig
  type t = Compiler_types.core_type_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Package_type : sig
  type t = Compiler_types.package_type
  [@@deriving equal, quickcheck, sexp_of]
end

module Row_field : sig
  type t = Compiler_types.row_field
  [@@deriving equal, quickcheck, sexp_of]
end

module Object_field : sig
  type t = Compiler_types.object_field
  [@@deriving equal, quickcheck, sexp_of]
end

module Pattern : sig
  type t = Compiler_types.pattern
  [@@deriving equal, quickcheck, sexp_of]
end

module Pattern_desc : sig
  type t = Compiler_types.pattern_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Expression : sig
  type t = Compiler_types.expression
  [@@deriving equal, quickcheck, sexp_of]
end

module Expression_desc : sig
  type t = Compiler_types.expression_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Case : sig
  type t = Compiler_types.case
  [@@deriving equal, quickcheck, sexp_of]
end

module Value_description : sig
  type t = Compiler_types.value_description
  [@@deriving equal, quickcheck, sexp_of]
end

module Type_declaration : sig
  type t = Compiler_types.type_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module Type_kind : sig
  type t = Compiler_types.type_kind
  [@@deriving equal, quickcheck, sexp_of]
end

module Label_declaration : sig
  type t = Compiler_types.label_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module Constructor_declaration : sig
  type t = Compiler_types.constructor_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module Constructor_arguments : sig
  type t = Compiler_types.constructor_arguments
  [@@deriving equal, quickcheck, sexp_of]
end

module Type_extension : sig
  type t = Compiler_types.type_extension
  [@@deriving equal, quickcheck, sexp_of]
end

module Extension_constructor : sig
  type t = Compiler_types.extension_constructor
  [@@deriving equal, quickcheck, sexp_of]
end

module Extension_constructor_kind : sig
  type t = Compiler_types.extension_constructor_kind
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_type : sig
  type t = Compiler_types.class_type
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_type_desc : sig
  type t = Compiler_types.class_type_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_signature : sig
  type t = Compiler_types.class_signature
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_type_field : sig
  type t = Compiler_types.class_type_field
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_type_field_desc : sig
  type t = Compiler_types.class_type_field_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_infos : sig
  type 'a t = 'a Compiler_types.class_infos
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_description : sig
  type t = Compiler_types.class_description
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_type_declaration : sig
  type t = Compiler_types.class_type_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_expr : sig
  type t = Compiler_types.class_expr
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_expr_desc : sig
  type t = Compiler_types.class_expr_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_structure : sig
  type t = Compiler_types.class_structure
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_field : sig
  type t = Compiler_types.class_field
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_field_desc : sig
  type t = Compiler_types.class_field_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_field_kind : sig
  type t = Compiler_types.class_field_kind
  [@@deriving equal, quickcheck, sexp_of]
end

module Class_declaration : sig
  type t = Compiler_types.class_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module Module_type : sig
  type t = Compiler_types.module_type
  [@@deriving equal, quickcheck, sexp_of]
end

module Module_type_desc : sig
  type t = Compiler_types.module_type_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Signature : sig
  type t = Compiler_types.signature
  [@@deriving equal, quickcheck, sexp_of]
end

module Signature_item : sig
  type t = Compiler_types.signature_item
  [@@deriving equal, quickcheck, sexp_of]
end

module Signature_item_desc : sig
  type t = Compiler_types.signature_item_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Module_declaration : sig
  type t = Compiler_types.module_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module Module_type_declaration : sig
  type t = Compiler_types.module_type_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module Open_description : sig
  type t = Compiler_types.open_description
  [@@deriving equal, quickcheck, sexp_of]
end

module Include_infos : sig
  type 'a t = 'a Compiler_types.include_infos
  [@@deriving equal, quickcheck, sexp_of]
end

module Include_description : sig
  type t = Compiler_types.include_description
  [@@deriving equal, quickcheck, sexp_of]
end

module Include_declaration : sig
  type t = Compiler_types.include_declaration
  [@@deriving equal, quickcheck, sexp_of]
end

module With_constraint : sig
  type t = Compiler_types.with_constraint
  [@@deriving equal, quickcheck, sexp_of]
end

module Module_expr : sig
  type t = Compiler_types.module_expr
  [@@deriving equal, quickcheck, sexp_of]
end

module Module_expr_desc : sig
  type t = Compiler_types.module_expr_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Structure : sig
  type t = Compiler_types.structure
  [@@deriving equal, quickcheck, sexp_of]
end

module Structure_item : sig
  type t = Compiler_types.structure_item
  [@@deriving equal, quickcheck, sexp_of]
end

module Structure_item_desc : sig
  type t = Compiler_types.structure_item_desc
  [@@deriving equal, quickcheck, sexp_of]
end

module Value_binding : sig
  type t = Compiler_types.value_binding
  [@@deriving equal, quickcheck, sexp_of]
end

module Module_binding : sig
  type t = Compiler_types.module_binding
  [@@deriving equal, quickcheck, sexp_of]
end

module Toplevel_phrase : sig
  type t = Compiler_types.toplevel_phrase
  [@@deriving equal, quickcheck, sexp_of]
end

module Directive_argument : sig
  type t = Compiler_types.directive_argument
  [@@deriving equal, quickcheck, sexp_of]
end
(*$*)
