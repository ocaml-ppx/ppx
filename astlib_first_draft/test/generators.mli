(*$ Astlib_first_draft_test_cinaps.print_generators_mli () *)
module V4_07 : sig
  module Longident : sig
    type t = Astlib_first_draft.V4_07.Longident.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Longident_loc : sig
    type t = Astlib_first_draft.V4_07.Longident_loc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Rec_flag : sig
    type t = Astlib_first_draft.V4_07.Rec_flag.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Direction_flag : sig
    type t = Astlib_first_draft.V4_07.Direction_flag.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Private_flag : sig
    type t = Astlib_first_draft.V4_07.Private_flag.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Mutable_flag : sig
    type t = Astlib_first_draft.V4_07.Mutable_flag.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Virtual_flag : sig
    type t = Astlib_first_draft.V4_07.Virtual_flag.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Override_flag : sig
    type t = Astlib_first_draft.V4_07.Override_flag.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Closed_flag : sig
    type t = Astlib_first_draft.V4_07.Closed_flag.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Label : sig
    type t = Astlib_first_draft.V4_07.Label.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Label_loc : sig
    type t = Astlib_first_draft.V4_07.Label_loc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module String_loc : sig
    type t = Astlib_first_draft.V4_07.String_loc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Arg_label : sig
    type t = Astlib_first_draft.V4_07.Arg_label.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Variance : sig
    type t = Astlib_first_draft.V4_07.Variance.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Constant : sig
    type t = Astlib_first_draft.V4_07.Constant.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Attribute : sig
    type t = Astlib_first_draft.V4_07.Attribute.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Extension : sig
    type t = Astlib_first_draft.V4_07.Extension.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Attributes : sig
    type t = Astlib_first_draft.V4_07.Attributes.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Payload : sig
    type t = Astlib_first_draft.V4_07.Payload.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Core_type : sig
    type t = Astlib_first_draft.V4_07.Core_type.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Core_type_desc : sig
    type t = Astlib_first_draft.V4_07.Core_type_desc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Package_type : sig
    type t = Astlib_first_draft.V4_07.Package_type.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Package_type_constraint : sig
    type t = Astlib_first_draft.V4_07.Package_type_constraint.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Row_field : sig
    type t = Astlib_first_draft.V4_07.Row_field.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Object_field : sig
    type t = Astlib_first_draft.V4_07.Object_field.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Pattern : sig
    type t = Astlib_first_draft.V4_07.Pattern.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Pattern_desc : sig
    type t = Astlib_first_draft.V4_07.Pattern_desc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Record_field_pattern : sig
    type t = Astlib_first_draft.V4_07.Record_field_pattern.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Expression : sig
    type t = Astlib_first_draft.V4_07.Expression.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Expression_desc : sig
    type t = Astlib_first_draft.V4_07.Expression_desc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Override_expression : sig
    type t = Astlib_first_draft.V4_07.Override_expression.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Record_field_expression : sig
    type t = Astlib_first_draft.V4_07.Record_field_expression.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Apply_arg : sig
    type t = Astlib_first_draft.V4_07.Apply_arg.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Case : sig
    type t = Astlib_first_draft.V4_07.Case.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Value_description : sig
    type t = Astlib_first_draft.V4_07.Value_description.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Type_declaration : sig
    type t = Astlib_first_draft.V4_07.Type_declaration.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Type_param : sig
    type t = Astlib_first_draft.V4_07.Type_param.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Type_constraint : sig
    type t = Astlib_first_draft.V4_07.Type_constraint.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Type_kind : sig
    type t = Astlib_first_draft.V4_07.Type_kind.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Label_declaration : sig
    type t = Astlib_first_draft.V4_07.Label_declaration.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Constructor_declaration : sig
    type t = Astlib_first_draft.V4_07.Constructor_declaration.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Constructor_arguments : sig
    type t = Astlib_first_draft.V4_07.Constructor_arguments.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Type_extension : sig
    type t = Astlib_first_draft.V4_07.Type_extension.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Extension_constructor : sig
    type t = Astlib_first_draft.V4_07.Extension_constructor.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Extension_constructor_kind : sig
    type t = Astlib_first_draft.V4_07.Extension_constructor_kind.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_type : sig
    type t = Astlib_first_draft.V4_07.Class_type.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_type_desc : sig
    type t = Astlib_first_draft.V4_07.Class_type_desc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_signature : sig
    type t = Astlib_first_draft.V4_07.Class_signature.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_type_field : sig
    type t = Astlib_first_draft.V4_07.Class_type_field.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_type_field_desc : sig
    type t = Astlib_first_draft.V4_07.Class_type_field_desc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_type_value_desc : sig
    type t = Astlib_first_draft.V4_07.Class_type_value_desc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_type_method_desc : sig
    type t = Astlib_first_draft.V4_07.Class_type_method_desc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_type_constraint : sig
    type t = Astlib_first_draft.V4_07.Class_type_constraint.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_description : sig
    type t = Astlib_first_draft.V4_07.Class_description.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_type_declaration : sig
    type t = Astlib_first_draft.V4_07.Class_type_declaration.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_expr : sig
    type t = Astlib_first_draft.V4_07.Class_expr.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_expr_desc : sig
    type t = Astlib_first_draft.V4_07.Class_expr_desc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_structure : sig
    type t = Astlib_first_draft.V4_07.Class_structure.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_field : sig
    type t = Astlib_first_draft.V4_07.Class_field.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_field_desc : sig
    type t = Astlib_first_draft.V4_07.Class_field_desc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_value_desc : sig
    type t = Astlib_first_draft.V4_07.Class_value_desc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_method_desc : sig
    type t = Astlib_first_draft.V4_07.Class_method_desc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_field_kind : sig
    type t = Astlib_first_draft.V4_07.Class_field_kind.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Class_declaration : sig
    type t = Astlib_first_draft.V4_07.Class_declaration.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Module_type : sig
    type t = Astlib_first_draft.V4_07.Module_type.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Module_type_desc : sig
    type t = Astlib_first_draft.V4_07.Module_type_desc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Signature : sig
    type t = Astlib_first_draft.V4_07.Signature.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Signature_item : sig
    type t = Astlib_first_draft.V4_07.Signature_item.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Signature_item_desc : sig
    type t = Astlib_first_draft.V4_07.Signature_item_desc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Module_declaration : sig
    type t = Astlib_first_draft.V4_07.Module_declaration.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Module_type_declaration : sig
    type t = Astlib_first_draft.V4_07.Module_type_declaration.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Open_description : sig
    type t = Astlib_first_draft.V4_07.Open_description.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Include_description : sig
    type t = Astlib_first_draft.V4_07.Include_description.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Include_declaration : sig
    type t = Astlib_first_draft.V4_07.Include_declaration.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module With_constraint : sig
    type t = Astlib_first_draft.V4_07.With_constraint.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Module_expr : sig
    type t = Astlib_first_draft.V4_07.Module_expr.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Module_expr_desc : sig
    type t = Astlib_first_draft.V4_07.Module_expr_desc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Structure : sig
    type t = Astlib_first_draft.V4_07.Structure.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Structure_item : sig
    type t = Astlib_first_draft.V4_07.Structure_item.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Structure_item_desc : sig
    type t = Astlib_first_draft.V4_07.Structure_item_desc.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Value_binding : sig
    type t = Astlib_first_draft.V4_07.Value_binding.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Module_binding : sig
    type t = Astlib_first_draft.V4_07.Module_binding.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Toplevel_phrase : sig
    type t = Astlib_first_draft.V4_07.Toplevel_phrase.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Directive_argument : sig
    type t = Astlib_first_draft.V4_07.Directive_argument.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end
end
(*$*)
