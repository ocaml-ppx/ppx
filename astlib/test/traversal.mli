(*$ Astlib_test_cinaps.print_traversal_mli () *)
module V4_07 : sig
  module Longident : sig
    val copy : Astlib.V4_07.Longident.t -> Astlib.V4_07.Longident.t
  end

  module Longident_loc : sig
    val copy : Astlib.V4_07.Longident_loc.t -> Astlib.V4_07.Longident_loc.t
  end

  module Rec_flag : sig
    val copy : Astlib.V4_07.Rec_flag.t -> Astlib.V4_07.Rec_flag.t
  end

  module Direction_flag : sig
    val copy : Astlib.V4_07.Direction_flag.t -> Astlib.V4_07.Direction_flag.t
  end

  module Private_flag : sig
    val copy : Astlib.V4_07.Private_flag.t -> Astlib.V4_07.Private_flag.t
  end

  module Mutable_flag : sig
    val copy : Astlib.V4_07.Mutable_flag.t -> Astlib.V4_07.Mutable_flag.t
  end

  module Virtual_flag : sig
    val copy : Astlib.V4_07.Virtual_flag.t -> Astlib.V4_07.Virtual_flag.t
  end

  module Override_flag : sig
    val copy : Astlib.V4_07.Override_flag.t -> Astlib.V4_07.Override_flag.t
  end

  module Closed_flag : sig
    val copy : Astlib.V4_07.Closed_flag.t -> Astlib.V4_07.Closed_flag.t
  end

  module Label : sig
    val copy : Astlib.V4_07.Label.t -> Astlib.V4_07.Label.t
  end

  module Label_loc : sig
    val copy : Astlib.V4_07.Label_loc.t -> Astlib.V4_07.Label_loc.t
  end

  module String_loc : sig
    val copy : Astlib.V4_07.String_loc.t -> Astlib.V4_07.String_loc.t
  end

  module Arg_label : sig
    val copy : Astlib.V4_07.Arg_label.t -> Astlib.V4_07.Arg_label.t
  end

  module Variance : sig
    val copy : Astlib.V4_07.Variance.t -> Astlib.V4_07.Variance.t
  end

  module Constant : sig
    val copy : Astlib.V4_07.Constant.t -> Astlib.V4_07.Constant.t
  end

  module Attribute : sig
    val copy : Astlib.V4_07.Attribute.t -> Astlib.V4_07.Attribute.t
  end

  module Extension : sig
    val copy : Astlib.V4_07.Extension.t -> Astlib.V4_07.Extension.t
  end

  module Attributes : sig
    val copy : Astlib.V4_07.Attributes.t -> Astlib.V4_07.Attributes.t
  end

  module Payload : sig
    val copy : Astlib.V4_07.Payload.t -> Astlib.V4_07.Payload.t
  end

  module Core_type : sig
    val copy : Astlib.V4_07.Core_type.t -> Astlib.V4_07.Core_type.t
  end

  module Core_type_desc : sig
    val copy : Astlib.V4_07.Core_type_desc.t -> Astlib.V4_07.Core_type_desc.t
  end

  module Package_type : sig
    val copy : Astlib.V4_07.Package_type.t -> Astlib.V4_07.Package_type.t
  end

  module Package_type_constraint : sig
    val copy : Astlib.V4_07.Package_type_constraint.t -> Astlib.V4_07.Package_type_constraint.t
  end

  module Row_field : sig
    val copy : Astlib.V4_07.Row_field.t -> Astlib.V4_07.Row_field.t
  end

  module Object_field : sig
    val copy : Astlib.V4_07.Object_field.t -> Astlib.V4_07.Object_field.t
  end

  module Pattern : sig
    val copy : Astlib.V4_07.Pattern.t -> Astlib.V4_07.Pattern.t
  end

  module Pattern_desc : sig
    val copy : Astlib.V4_07.Pattern_desc.t -> Astlib.V4_07.Pattern_desc.t
  end

  module Record_field_pattern : sig
    val copy : Astlib.V4_07.Record_field_pattern.t -> Astlib.V4_07.Record_field_pattern.t
  end

  module Expression : sig
    val copy : Astlib.V4_07.Expression.t -> Astlib.V4_07.Expression.t
  end

  module Expression_desc : sig
    val copy : Astlib.V4_07.Expression_desc.t -> Astlib.V4_07.Expression_desc.t
  end

  module Override_expression : sig
    val copy : Astlib.V4_07.Override_expression.t -> Astlib.V4_07.Override_expression.t
  end

  module Record_field_expression : sig
    val copy : Astlib.V4_07.Record_field_expression.t -> Astlib.V4_07.Record_field_expression.t
  end

  module Apply_arg : sig
    val copy : Astlib.V4_07.Apply_arg.t -> Astlib.V4_07.Apply_arg.t
  end

  module Case : sig
    val copy : Astlib.V4_07.Case.t -> Astlib.V4_07.Case.t
  end

  module Value_description : sig
    val copy : Astlib.V4_07.Value_description.t -> Astlib.V4_07.Value_description.t
  end

  module Type_declaration : sig
    val copy : Astlib.V4_07.Type_declaration.t -> Astlib.V4_07.Type_declaration.t
  end

  module Type_param : sig
    val copy : Astlib.V4_07.Type_param.t -> Astlib.V4_07.Type_param.t
  end

  module Type_constraint : sig
    val copy : Astlib.V4_07.Type_constraint.t -> Astlib.V4_07.Type_constraint.t
  end

  module Type_kind : sig
    val copy : Astlib.V4_07.Type_kind.t -> Astlib.V4_07.Type_kind.t
  end

  module Label_declaration : sig
    val copy : Astlib.V4_07.Label_declaration.t -> Astlib.V4_07.Label_declaration.t
  end

  module Constructor_declaration : sig
    val copy : Astlib.V4_07.Constructor_declaration.t -> Astlib.V4_07.Constructor_declaration.t
  end

  module Constructor_arguments : sig
    val copy : Astlib.V4_07.Constructor_arguments.t -> Astlib.V4_07.Constructor_arguments.t
  end

  module Type_extension : sig
    val copy : Astlib.V4_07.Type_extension.t -> Astlib.V4_07.Type_extension.t
  end

  module Extension_constructor : sig
    val copy : Astlib.V4_07.Extension_constructor.t -> Astlib.V4_07.Extension_constructor.t
  end

  module Extension_constructor_kind : sig
    val copy : Astlib.V4_07.Extension_constructor_kind.t -> Astlib.V4_07.Extension_constructor_kind.t
  end

  module Class_type : sig
    val copy : Astlib.V4_07.Class_type.t -> Astlib.V4_07.Class_type.t
  end

  module Class_type_desc : sig
    val copy : Astlib.V4_07.Class_type_desc.t -> Astlib.V4_07.Class_type_desc.t
  end

  module Class_signature : sig
    val copy : Astlib.V4_07.Class_signature.t -> Astlib.V4_07.Class_signature.t
  end

  module Class_type_field : sig
    val copy : Astlib.V4_07.Class_type_field.t -> Astlib.V4_07.Class_type_field.t
  end

  module Class_type_field_desc : sig
    val copy : Astlib.V4_07.Class_type_field_desc.t -> Astlib.V4_07.Class_type_field_desc.t
  end

  module Class_type_value_desc : sig
    val copy : Astlib.V4_07.Class_type_value_desc.t -> Astlib.V4_07.Class_type_value_desc.t
  end

  module Class_type_method_desc : sig
    val copy : Astlib.V4_07.Class_type_method_desc.t -> Astlib.V4_07.Class_type_method_desc.t
  end

  module Class_type_constraint : sig
    val copy : Astlib.V4_07.Class_type_constraint.t -> Astlib.V4_07.Class_type_constraint.t
  end

  module Class_description : sig
    val copy : Astlib.V4_07.Class_description.t -> Astlib.V4_07.Class_description.t
  end

  module Class_type_declaration : sig
    val copy : Astlib.V4_07.Class_type_declaration.t -> Astlib.V4_07.Class_type_declaration.t
  end

  module Class_expr : sig
    val copy : Astlib.V4_07.Class_expr.t -> Astlib.V4_07.Class_expr.t
  end

  module Class_expr_desc : sig
    val copy : Astlib.V4_07.Class_expr_desc.t -> Astlib.V4_07.Class_expr_desc.t
  end

  module Class_structure : sig
    val copy : Astlib.V4_07.Class_structure.t -> Astlib.V4_07.Class_structure.t
  end

  module Class_field : sig
    val copy : Astlib.V4_07.Class_field.t -> Astlib.V4_07.Class_field.t
  end

  module Class_field_desc : sig
    val copy : Astlib.V4_07.Class_field_desc.t -> Astlib.V4_07.Class_field_desc.t
  end

  module Class_value_desc : sig
    val copy : Astlib.V4_07.Class_value_desc.t -> Astlib.V4_07.Class_value_desc.t
  end

  module Class_method_desc : sig
    val copy : Astlib.V4_07.Class_method_desc.t -> Astlib.V4_07.Class_method_desc.t
  end

  module Class_field_kind : sig
    val copy : Astlib.V4_07.Class_field_kind.t -> Astlib.V4_07.Class_field_kind.t
  end

  module Class_declaration : sig
    val copy : Astlib.V4_07.Class_declaration.t -> Astlib.V4_07.Class_declaration.t
  end

  module Module_type : sig
    val copy : Astlib.V4_07.Module_type.t -> Astlib.V4_07.Module_type.t
  end

  module Module_type_desc : sig
    val copy : Astlib.V4_07.Module_type_desc.t -> Astlib.V4_07.Module_type_desc.t
  end

  module Signature : sig
    val copy : Astlib.V4_07.Signature.t -> Astlib.V4_07.Signature.t
  end

  module Signature_item : sig
    val copy : Astlib.V4_07.Signature_item.t -> Astlib.V4_07.Signature_item.t
  end

  module Signature_item_desc : sig
    val copy : Astlib.V4_07.Signature_item_desc.t -> Astlib.V4_07.Signature_item_desc.t
  end

  module Module_declaration : sig
    val copy : Astlib.V4_07.Module_declaration.t -> Astlib.V4_07.Module_declaration.t
  end

  module Module_type_declaration : sig
    val copy : Astlib.V4_07.Module_type_declaration.t -> Astlib.V4_07.Module_type_declaration.t
  end

  module Open_description : sig
    val copy : Astlib.V4_07.Open_description.t -> Astlib.V4_07.Open_description.t
  end

  module Include_description : sig
    val copy : Astlib.V4_07.Include_description.t -> Astlib.V4_07.Include_description.t
  end

  module Include_declaration : sig
    val copy : Astlib.V4_07.Include_declaration.t -> Astlib.V4_07.Include_declaration.t
  end

  module With_constraint : sig
    val copy : Astlib.V4_07.With_constraint.t -> Astlib.V4_07.With_constraint.t
  end

  module Module_expr : sig
    val copy : Astlib.V4_07.Module_expr.t -> Astlib.V4_07.Module_expr.t
  end

  module Module_expr_desc : sig
    val copy : Astlib.V4_07.Module_expr_desc.t -> Astlib.V4_07.Module_expr_desc.t
  end

  module Structure : sig
    val copy : Astlib.V4_07.Structure.t -> Astlib.V4_07.Structure.t
  end

  module Structure_item : sig
    val copy : Astlib.V4_07.Structure_item.t -> Astlib.V4_07.Structure_item.t
  end

  module Structure_item_desc : sig
    val copy : Astlib.V4_07.Structure_item_desc.t -> Astlib.V4_07.Structure_item_desc.t
  end

  module Value_binding : sig
    val copy : Astlib.V4_07.Value_binding.t -> Astlib.V4_07.Value_binding.t
  end

  module Module_binding : sig
    val copy : Astlib.V4_07.Module_binding.t -> Astlib.V4_07.Module_binding.t
  end

  module Toplevel_phrase : sig
    val copy : Astlib.V4_07.Toplevel_phrase.t -> Astlib.V4_07.Toplevel_phrase.t
  end

  module Directive_argument : sig
    val copy : Astlib.V4_07.Directive_argument.t -> Astlib.V4_07.Directive_argument.t
  end
end
(*$*)
