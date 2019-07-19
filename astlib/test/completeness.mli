(*$ Astlib_test_cinaps.print_completeness_mli () *)
module V4_07 : sig
  module Longident : sig
    val check_exn : Astlib.V4_07.Longident.t -> unit
  end

  module Longident_loc : sig
    val check_exn : Astlib.V4_07.Longident_loc.t -> unit
  end

  module Rec_flag : sig
    val check_exn : Astlib.V4_07.Rec_flag.t -> unit
  end

  module Direction_flag : sig
    val check_exn : Astlib.V4_07.Direction_flag.t -> unit
  end

  module Private_flag : sig
    val check_exn : Astlib.V4_07.Private_flag.t -> unit
  end

  module Mutable_flag : sig
    val check_exn : Astlib.V4_07.Mutable_flag.t -> unit
  end

  module Virtual_flag : sig
    val check_exn : Astlib.V4_07.Virtual_flag.t -> unit
  end

  module Override_flag : sig
    val check_exn : Astlib.V4_07.Override_flag.t -> unit
  end

  module Closed_flag : sig
    val check_exn : Astlib.V4_07.Closed_flag.t -> unit
  end

  module Label : sig
    val check_exn : Astlib.V4_07.Label.t -> unit
  end

  module Label_loc : sig
    val check_exn : Astlib.V4_07.Label_loc.t -> unit
  end

  module String_loc : sig
    val check_exn : Astlib.V4_07.String_loc.t -> unit
  end

  module Arg_label : sig
    val check_exn : Astlib.V4_07.Arg_label.t -> unit
  end

  module Variance : sig
    val check_exn : Astlib.V4_07.Variance.t -> unit
  end

  module Constant : sig
    val check_exn : Astlib.V4_07.Constant.t -> unit
  end

  module Attribute : sig
    val check_exn : Astlib.V4_07.Attribute.t -> unit
  end

  module Extension : sig
    val check_exn : Astlib.V4_07.Extension.t -> unit
  end

  module Attributes : sig
    val check_exn : Astlib.V4_07.Attributes.t -> unit
  end

  module Payload : sig
    val check_exn : Astlib.V4_07.Payload.t -> unit
  end

  module Core_type : sig
    val check_exn : Astlib.V4_07.Core_type.t -> unit
  end

  module Core_type_desc : sig
    val check_exn : Astlib.V4_07.Core_type_desc.t -> unit
  end

  module Package_type : sig
    val check_exn : Astlib.V4_07.Package_type.t -> unit
  end

  module Package_type_constraint : sig
    val check_exn : Astlib.V4_07.Package_type_constraint.t -> unit
  end

  module Row_field : sig
    val check_exn : Astlib.V4_07.Row_field.t -> unit
  end

  module Object_field : sig
    val check_exn : Astlib.V4_07.Object_field.t -> unit
  end

  module Pattern : sig
    val check_exn : Astlib.V4_07.Pattern.t -> unit
  end

  module Pattern_desc : sig
    val check_exn : Astlib.V4_07.Pattern_desc.t -> unit
  end

  module Record_field_pattern : sig
    val check_exn : Astlib.V4_07.Record_field_pattern.t -> unit
  end

  module Expression : sig
    val check_exn : Astlib.V4_07.Expression.t -> unit
  end

  module Expression_desc : sig
    val check_exn : Astlib.V4_07.Expression_desc.t -> unit
  end

  module Override_expression : sig
    val check_exn : Astlib.V4_07.Override_expression.t -> unit
  end

  module Record_field_expression : sig
    val check_exn : Astlib.V4_07.Record_field_expression.t -> unit
  end

  module Apply_arg : sig
    val check_exn : Astlib.V4_07.Apply_arg.t -> unit
  end

  module Case : sig
    val check_exn : Astlib.V4_07.Case.t -> unit
  end

  module Value_description : sig
    val check_exn : Astlib.V4_07.Value_description.t -> unit
  end

  module Type_declaration : sig
    val check_exn : Astlib.V4_07.Type_declaration.t -> unit
  end

  module Type_param : sig
    val check_exn : Astlib.V4_07.Type_param.t -> unit
  end

  module Type_constraint : sig
    val check_exn : Astlib.V4_07.Type_constraint.t -> unit
  end

  module Type_kind : sig
    val check_exn : Astlib.V4_07.Type_kind.t -> unit
  end

  module Label_declaration : sig
    val check_exn : Astlib.V4_07.Label_declaration.t -> unit
  end

  module Constructor_declaration : sig
    val check_exn : Astlib.V4_07.Constructor_declaration.t -> unit
  end

  module Constructor_arguments : sig
    val check_exn : Astlib.V4_07.Constructor_arguments.t -> unit
  end

  module Type_extension : sig
    val check_exn : Astlib.V4_07.Type_extension.t -> unit
  end

  module Extension_constructor : sig
    val check_exn : Astlib.V4_07.Extension_constructor.t -> unit
  end

  module Extension_constructor_kind : sig
    val check_exn : Astlib.V4_07.Extension_constructor_kind.t -> unit
  end

  module Class_type : sig
    val check_exn : Astlib.V4_07.Class_type.t -> unit
  end

  module Class_type_desc : sig
    val check_exn : Astlib.V4_07.Class_type_desc.t -> unit
  end

  module Class_signature : sig
    val check_exn : Astlib.V4_07.Class_signature.t -> unit
  end

  module Class_type_field : sig
    val check_exn : Astlib.V4_07.Class_type_field.t -> unit
  end

  module Class_type_field_desc : sig
    val check_exn : Astlib.V4_07.Class_type_field_desc.t -> unit
  end

  module Class_type_value_desc : sig
    val check_exn : Astlib.V4_07.Class_type_value_desc.t -> unit
  end

  module Class_type_method_desc : sig
    val check_exn : Astlib.V4_07.Class_type_method_desc.t -> unit
  end

  module Class_type_constraint : sig
    val check_exn : Astlib.V4_07.Class_type_constraint.t -> unit
  end

  module Class_description : sig
    val check_exn : Astlib.V4_07.Class_description.t -> unit
  end

  module Class_type_declaration : sig
    val check_exn : Astlib.V4_07.Class_type_declaration.t -> unit
  end

  module Class_expr : sig
    val check_exn : Astlib.V4_07.Class_expr.t -> unit
  end

  module Class_expr_desc : sig
    val check_exn : Astlib.V4_07.Class_expr_desc.t -> unit
  end

  module Class_structure : sig
    val check_exn : Astlib.V4_07.Class_structure.t -> unit
  end

  module Class_field : sig
    val check_exn : Astlib.V4_07.Class_field.t -> unit
  end

  module Class_field_desc : sig
    val check_exn : Astlib.V4_07.Class_field_desc.t -> unit
  end

  module Class_value_desc : sig
    val check_exn : Astlib.V4_07.Class_value_desc.t -> unit
  end

  module Class_method_desc : sig
    val check_exn : Astlib.V4_07.Class_method_desc.t -> unit
  end

  module Class_field_kind : sig
    val check_exn : Astlib.V4_07.Class_field_kind.t -> unit
  end

  module Class_declaration : sig
    val check_exn : Astlib.V4_07.Class_declaration.t -> unit
  end

  module Module_type : sig
    val check_exn : Astlib.V4_07.Module_type.t -> unit
  end

  module Module_type_desc : sig
    val check_exn : Astlib.V4_07.Module_type_desc.t -> unit
  end

  module Signature : sig
    val check_exn : Astlib.V4_07.Signature.t -> unit
  end

  module Signature_item : sig
    val check_exn : Astlib.V4_07.Signature_item.t -> unit
  end

  module Signature_item_desc : sig
    val check_exn : Astlib.V4_07.Signature_item_desc.t -> unit
  end

  module Module_declaration : sig
    val check_exn : Astlib.V4_07.Module_declaration.t -> unit
  end

  module Module_type_declaration : sig
    val check_exn : Astlib.V4_07.Module_type_declaration.t -> unit
  end

  module Open_description : sig
    val check_exn : Astlib.V4_07.Open_description.t -> unit
  end

  module Include_description : sig
    val check_exn : Astlib.V4_07.Include_description.t -> unit
  end

  module Include_declaration : sig
    val check_exn : Astlib.V4_07.Include_declaration.t -> unit
  end

  module With_constraint : sig
    val check_exn : Astlib.V4_07.With_constraint.t -> unit
  end

  module Module_expr : sig
    val check_exn : Astlib.V4_07.Module_expr.t -> unit
  end

  module Module_expr_desc : sig
    val check_exn : Astlib.V4_07.Module_expr_desc.t -> unit
  end

  module Structure : sig
    val check_exn : Astlib.V4_07.Structure.t -> unit
  end

  module Structure_item : sig
    val check_exn : Astlib.V4_07.Structure_item.t -> unit
  end

  module Structure_item_desc : sig
    val check_exn : Astlib.V4_07.Structure_item_desc.t -> unit
  end

  module Value_binding : sig
    val check_exn : Astlib.V4_07.Value_binding.t -> unit
  end

  module Module_binding : sig
    val check_exn : Astlib.V4_07.Module_binding.t -> unit
  end

  module Toplevel_phrase : sig
    val check_exn : Astlib.V4_07.Toplevel_phrase.t -> unit
  end

  module Directive_argument : sig
    val check_exn : Astlib.V4_07.Directive_argument.t -> unit
  end
end
(*$*)
