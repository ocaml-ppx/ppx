open Stdppx

(*$ Ppx_ast_cinaps.print_conversion_ml () *)
let rec ast_of_longident x =
  Versions.V4_07.Longident.of_concrete (concrete_of_longident x)

and concrete_of_longident x : Versions.V4_07.Longident.concrete =
  match (x : Compiler_types.longident) with
  | Lident (x1) ->
    Lident (x1)
  | Ldot (x1, x2) ->
    let x1 = ast_of_longident x1 in
    Ldot (x1, x2)
  | Lapply (x1, x2) ->
    let x1 = ast_of_longident x1 in
    let x2 = ast_of_longident x2 in
    Lapply (x1, x2)

and ast_to_longident x =
  let concrete = Versions.V4_07.Longident.to_concrete x in
  concrete_to_longident concrete

and concrete_to_longident x : Compiler_types.longident =
  match (x : Versions.V4_07.Longident.concrete) with
  | Lident (x1) ->
    Lident (x1)
  | Ldot (x1, x2) ->
    let x1 = ast_to_longident x1 in
    Ldot (x1, x2)
  | Lapply (x1, x2) ->
    let x1 = ast_to_longident x1 in
    let x2 = ast_to_longident x2 in
    Lapply (x1, x2)

and ast_of_longident_loc x =
  Versions.V4_07.Longident_loc.of_concrete (concrete_of_longident_loc x)

and concrete_of_longident_loc x =
  (Astlib.Loc.map ~f:ast_of_longident) x

and ast_to_longident_loc x =
  let concrete = Versions.V4_07.Longident_loc.to_concrete x in
  concrete_to_longident_loc concrete

and concrete_to_longident_loc x =
  (Astlib.Loc.map ~f:ast_to_longident) x

and ast_of_rec_flag x =
  Versions.V4_07.Rec_flag.of_concrete (concrete_of_rec_flag x)

and concrete_of_rec_flag x : Versions.V4_07.Rec_flag.concrete =
  match (x : Compiler_types.rec_flag) with
  | Nonrecursive -> Nonrecursive
  | Recursive -> Recursive

and ast_to_rec_flag x =
  let concrete = Versions.V4_07.Rec_flag.to_concrete x in
  concrete_to_rec_flag concrete

and concrete_to_rec_flag x : Compiler_types.rec_flag =
  match (x : Versions.V4_07.Rec_flag.concrete) with
  | Nonrecursive -> Nonrecursive
  | Recursive -> Recursive

and ast_of_direction_flag x =
  Versions.V4_07.Direction_flag.of_concrete (concrete_of_direction_flag x)

and concrete_of_direction_flag x : Versions.V4_07.Direction_flag.concrete =
  match (x : Compiler_types.direction_flag) with
  | Upto -> Upto
  | Downto -> Downto

and ast_to_direction_flag x =
  let concrete = Versions.V4_07.Direction_flag.to_concrete x in
  concrete_to_direction_flag concrete

and concrete_to_direction_flag x : Compiler_types.direction_flag =
  match (x : Versions.V4_07.Direction_flag.concrete) with
  | Upto -> Upto
  | Downto -> Downto

and ast_of_private_flag x =
  Versions.V4_07.Private_flag.of_concrete (concrete_of_private_flag x)

and concrete_of_private_flag x : Versions.V4_07.Private_flag.concrete =
  match (x : Compiler_types.private_flag) with
  | Private -> Private
  | Public -> Public

and ast_to_private_flag x =
  let concrete = Versions.V4_07.Private_flag.to_concrete x in
  concrete_to_private_flag concrete

and concrete_to_private_flag x : Compiler_types.private_flag =
  match (x : Versions.V4_07.Private_flag.concrete) with
  | Private -> Private
  | Public -> Public

and ast_of_mutable_flag x =
  Versions.V4_07.Mutable_flag.of_concrete (concrete_of_mutable_flag x)

and concrete_of_mutable_flag x : Versions.V4_07.Mutable_flag.concrete =
  match (x : Compiler_types.mutable_flag) with
  | Immutable -> Immutable
  | Mutable -> Mutable

and ast_to_mutable_flag x =
  let concrete = Versions.V4_07.Mutable_flag.to_concrete x in
  concrete_to_mutable_flag concrete

and concrete_to_mutable_flag x : Compiler_types.mutable_flag =
  match (x : Versions.V4_07.Mutable_flag.concrete) with
  | Immutable -> Immutable
  | Mutable -> Mutable

and ast_of_virtual_flag x =
  Versions.V4_07.Virtual_flag.of_concrete (concrete_of_virtual_flag x)

and concrete_of_virtual_flag x : Versions.V4_07.Virtual_flag.concrete =
  match (x : Compiler_types.virtual_flag) with
  | Virtual -> Virtual
  | Concrete -> Concrete

and ast_to_virtual_flag x =
  let concrete = Versions.V4_07.Virtual_flag.to_concrete x in
  concrete_to_virtual_flag concrete

and concrete_to_virtual_flag x : Compiler_types.virtual_flag =
  match (x : Versions.V4_07.Virtual_flag.concrete) with
  | Virtual -> Virtual
  | Concrete -> Concrete

and ast_of_override_flag x =
  Versions.V4_07.Override_flag.of_concrete (concrete_of_override_flag x)

and concrete_of_override_flag x : Versions.V4_07.Override_flag.concrete =
  match (x : Compiler_types.override_flag) with
  | Override -> Override
  | Fresh -> Fresh

and ast_to_override_flag x =
  let concrete = Versions.V4_07.Override_flag.to_concrete x in
  concrete_to_override_flag concrete

and concrete_to_override_flag x : Compiler_types.override_flag =
  match (x : Versions.V4_07.Override_flag.concrete) with
  | Override -> Override
  | Fresh -> Fresh

and ast_of_closed_flag x =
  Versions.V4_07.Closed_flag.of_concrete (concrete_of_closed_flag x)

and concrete_of_closed_flag x : Versions.V4_07.Closed_flag.concrete =
  match (x : Compiler_types.closed_flag) with
  | Closed -> Closed
  | Open -> Open

and ast_to_closed_flag x =
  let concrete = Versions.V4_07.Closed_flag.to_concrete x in
  concrete_to_closed_flag concrete

and concrete_to_closed_flag x : Compiler_types.closed_flag =
  match (x : Versions.V4_07.Closed_flag.concrete) with
  | Closed -> Closed
  | Open -> Open

and ast_of_arg_label x =
  Versions.V4_07.Arg_label.of_concrete (concrete_of_arg_label x)

and concrete_of_arg_label x : Versions.V4_07.Arg_label.concrete =
  match (x : Compiler_types.arg_label) with
  | Nolabel -> Nolabel
  | Labelled (x1) ->
    Labelled (x1)
  | Optional (x1) ->
    Optional (x1)

and ast_to_arg_label x =
  let concrete = Versions.V4_07.Arg_label.to_concrete x in
  concrete_to_arg_label concrete

and concrete_to_arg_label x : Compiler_types.arg_label =
  match (x : Versions.V4_07.Arg_label.concrete) with
  | Nolabel -> Nolabel
  | Labelled (x1) ->
    Labelled (x1)
  | Optional (x1) ->
    Optional (x1)

and ast_of_variance x =
  Versions.V4_07.Variance.of_concrete (concrete_of_variance x)

and concrete_of_variance x : Versions.V4_07.Variance.concrete =
  match (x : Compiler_types.variance) with
  | Covariant -> Covariant
  | Contravariant -> Contravariant
  | Invariant -> Invariant

and ast_to_variance x =
  let concrete = Versions.V4_07.Variance.to_concrete x in
  concrete_to_variance concrete

and concrete_to_variance x : Compiler_types.variance =
  match (x : Versions.V4_07.Variance.concrete) with
  | Covariant -> Covariant
  | Contravariant -> Contravariant
  | Invariant -> Invariant

and ast_of_constant x =
  Versions.V4_07.Constant.of_concrete (concrete_of_constant x)

and concrete_of_constant x : Versions.V4_07.Constant.concrete =
  match (x : Compiler_types.constant) with
  | Pconst_integer (x1, x2) ->
    Pconst_integer (x1, x2)
  | Pconst_char (x1) ->
    Pconst_char (x1)
  | Pconst_string (x1, x2) ->
    Pconst_string (x1, x2)
  | Pconst_float (x1, x2) ->
    Pconst_float (x1, x2)

and ast_to_constant x =
  let concrete = Versions.V4_07.Constant.to_concrete x in
  concrete_to_constant concrete

and concrete_to_constant x : Compiler_types.constant =
  match (x : Versions.V4_07.Constant.concrete) with
  | Pconst_integer (x1, x2) ->
    Pconst_integer (x1, x2)
  | Pconst_char (x1) ->
    Pconst_char (x1)
  | Pconst_string (x1, x2) ->
    Pconst_string (x1, x2)
  | Pconst_float (x1, x2) ->
    Pconst_float (x1, x2)

and ast_of_attribute x =
  Versions.V4_07.Attribute.of_concrete (concrete_of_attribute x)

and concrete_of_attribute x =
  (Tuple.map2 ~f1:Fn.id ~f2:ast_of_payload) x

and ast_to_attribute x =
  let concrete = Versions.V4_07.Attribute.to_concrete x in
  concrete_to_attribute concrete

and concrete_to_attribute x =
  (Tuple.map2 ~f1:Fn.id ~f2:ast_to_payload) x

and ast_of_extension x =
  Versions.V4_07.Extension.of_concrete (concrete_of_extension x)

and concrete_of_extension x =
  (Tuple.map2 ~f1:Fn.id ~f2:ast_of_payload) x

and ast_to_extension x =
  let concrete = Versions.V4_07.Extension.to_concrete x in
  concrete_to_extension concrete

and concrete_to_extension x =
  (Tuple.map2 ~f1:Fn.id ~f2:ast_to_payload) x

and ast_of_attributes x =
  Versions.V4_07.Attributes.of_concrete (concrete_of_attributes x)

and concrete_of_attributes x =
  (List.map ~f:ast_of_attribute) x

and ast_to_attributes x =
  let concrete = Versions.V4_07.Attributes.to_concrete x in
  concrete_to_attributes concrete

and concrete_to_attributes x =
  (List.map ~f:ast_to_attribute) x

and ast_of_payload x =
  Versions.V4_07.Payload.of_concrete (concrete_of_payload x)

and concrete_of_payload x : Versions.V4_07.Payload.concrete =
  match (x : Compiler_types.payload) with
  | PStr (x1) ->
    let x1 = ast_of_structure x1 in
    PStr (x1)
  | PSig (x1) ->
    let x1 = ast_of_signature x1 in
    PSig (x1)
  | PTyp (x1) ->
    let x1 = ast_of_core_type x1 in
    PTyp (x1)
  | PPat (x1, x2) ->
    let x1 = ast_of_pattern x1 in
    let x2 = (Option.map ~f:ast_of_expression) x2 in
    PPat (x1, x2)

and ast_to_payload x =
  let concrete = Versions.V4_07.Payload.to_concrete x in
  concrete_to_payload concrete

and concrete_to_payload x : Compiler_types.payload =
  match (x : Versions.V4_07.Payload.concrete) with
  | PStr (x1) ->
    let x1 = ast_to_structure x1 in
    PStr (x1)
  | PSig (x1) ->
    let x1 = ast_to_signature x1 in
    PSig (x1)
  | PTyp (x1) ->
    let x1 = ast_to_core_type x1 in
    PTyp (x1)
  | PPat (x1, x2) ->
    let x1 = ast_to_pattern x1 in
    let x2 = (Option.map ~f:ast_to_expression) x2 in
    PPat (x1, x2)

and ast_of_core_type x =
  Versions.V4_07.Core_type.of_concrete (concrete_of_core_type x)

and concrete_of_core_type
  ({ ptyp_desc; ptyp_loc; ptyp_attributes } : Compiler_types.core_type)
=
  let ptyp_desc = ast_of_core_type_desc ptyp_desc in
  let ptyp_attributes = ast_of_attributes ptyp_attributes in
  ({ ptyp_desc; ptyp_loc; ptyp_attributes } : Versions.V4_07.Core_type.concrete)

and ast_to_core_type x =
  let concrete = Versions.V4_07.Core_type.to_concrete x in
  concrete_to_core_type concrete

and concrete_to_core_type
  ({ ptyp_desc; ptyp_loc; ptyp_attributes } : Versions.V4_07.Core_type.concrete)
=
  let ptyp_desc = ast_to_core_type_desc ptyp_desc in
  let ptyp_attributes = ast_to_attributes ptyp_attributes in
  ({ ptyp_desc; ptyp_loc; ptyp_attributes } : Compiler_types.core_type)

and ast_of_core_type_desc x =
  Versions.V4_07.Core_type_desc.of_concrete (concrete_of_core_type_desc x)

and concrete_of_core_type_desc x : Versions.V4_07.Core_type_desc.concrete =
  match (x : Compiler_types.core_type_desc) with
  | Ptyp_any -> Ptyp_any
  | Ptyp_var (x1) ->
    Ptyp_var (x1)
  | Ptyp_arrow (x1, x2, x3) ->
    let x1 = ast_of_arg_label x1 in
    let x2 = ast_of_core_type x2 in
    let x3 = ast_of_core_type x3 in
    Ptyp_arrow (x1, x2, x3)
  | Ptyp_tuple (x1) ->
    let x1 = (List.map ~f:ast_of_core_type) x1 in
    Ptyp_tuple (x1)
  | Ptyp_constr (x1, x2) ->
    let x1 = ast_of_longident_loc x1 in
    let x2 = (List.map ~f:ast_of_core_type) x2 in
    Ptyp_constr (x1, x2)
  | Ptyp_object (x1, x2) ->
    let x1 = (List.map ~f:ast_of_object_field) x1 in
    let x2 = ast_of_closed_flag x2 in
    Ptyp_object (x1, x2)
  | Ptyp_class (x1, x2) ->
    let x1 = ast_of_longident_loc x1 in
    let x2 = (List.map ~f:ast_of_core_type) x2 in
    Ptyp_class (x1, x2)
  | Ptyp_alias (x1, x2) ->
    let x1 = ast_of_core_type x1 in
    Ptyp_alias (x1, x2)
  | Ptyp_variant (x1, x2, x3) ->
    let x1 = (List.map ~f:ast_of_row_field) x1 in
    let x2 = ast_of_closed_flag x2 in
    Ptyp_variant (x1, x2, x3)
  | Ptyp_poly (x1, x2) ->
    let x2 = ast_of_core_type x2 in
    Ptyp_poly (x1, x2)
  | Ptyp_package (x1) ->
    let x1 = ast_of_package_type x1 in
    Ptyp_package (x1)
  | Ptyp_extension (x1) ->
    let x1 = ast_of_extension x1 in
    Ptyp_extension (x1)

and ast_to_core_type_desc x =
  let concrete = Versions.V4_07.Core_type_desc.to_concrete x in
  concrete_to_core_type_desc concrete

and concrete_to_core_type_desc x : Compiler_types.core_type_desc =
  match (x : Versions.V4_07.Core_type_desc.concrete) with
  | Ptyp_any -> Ptyp_any
  | Ptyp_var (x1) ->
    Ptyp_var (x1)
  | Ptyp_arrow (x1, x2, x3) ->
    let x1 = ast_to_arg_label x1 in
    let x2 = ast_to_core_type x2 in
    let x3 = ast_to_core_type x3 in
    Ptyp_arrow (x1, x2, x3)
  | Ptyp_tuple (x1) ->
    let x1 = (List.map ~f:ast_to_core_type) x1 in
    Ptyp_tuple (x1)
  | Ptyp_constr (x1, x2) ->
    let x1 = ast_to_longident_loc x1 in
    let x2 = (List.map ~f:ast_to_core_type) x2 in
    Ptyp_constr (x1, x2)
  | Ptyp_object (x1, x2) ->
    let x1 = (List.map ~f:ast_to_object_field) x1 in
    let x2 = ast_to_closed_flag x2 in
    Ptyp_object (x1, x2)
  | Ptyp_class (x1, x2) ->
    let x1 = ast_to_longident_loc x1 in
    let x2 = (List.map ~f:ast_to_core_type) x2 in
    Ptyp_class (x1, x2)
  | Ptyp_alias (x1, x2) ->
    let x1 = ast_to_core_type x1 in
    Ptyp_alias (x1, x2)
  | Ptyp_variant (x1, x2, x3) ->
    let x1 = (List.map ~f:ast_to_row_field) x1 in
    let x2 = ast_to_closed_flag x2 in
    Ptyp_variant (x1, x2, x3)
  | Ptyp_poly (x1, x2) ->
    let x2 = ast_to_core_type x2 in
    Ptyp_poly (x1, x2)
  | Ptyp_package (x1) ->
    let x1 = ast_to_package_type x1 in
    Ptyp_package (x1)
  | Ptyp_extension (x1) ->
    let x1 = ast_to_extension x1 in
    Ptyp_extension (x1)

and ast_of_package_type x =
  Versions.V4_07.Package_type.of_concrete (concrete_of_package_type x)

and concrete_of_package_type x =
  (Tuple.map2 ~f1:ast_of_longident_loc ~f2:(List.map ~f:(Tuple.map2 ~f1:ast_of_longident_loc ~f2:ast_of_core_type))) x

and ast_to_package_type x =
  let concrete = Versions.V4_07.Package_type.to_concrete x in
  concrete_to_package_type concrete

and concrete_to_package_type x =
  (Tuple.map2 ~f1:ast_to_longident_loc ~f2:(List.map ~f:(Tuple.map2 ~f1:ast_to_longident_loc ~f2:ast_to_core_type))) x

and ast_of_row_field x =
  Versions.V4_07.Row_field.of_concrete (concrete_of_row_field x)

and concrete_of_row_field x : Versions.V4_07.Row_field.concrete =
  match (x : Compiler_types.row_field) with
  | Rtag (x1, x2, x3, x4) ->
    let x2 = ast_of_attributes x2 in
    let x4 = (List.map ~f:ast_of_core_type) x4 in
    Rtag (x1, x2, x3, x4)
  | Rinherit (x1) ->
    let x1 = ast_of_core_type x1 in
    Rinherit (x1)

and ast_to_row_field x =
  let concrete = Versions.V4_07.Row_field.to_concrete x in
  concrete_to_row_field concrete

and concrete_to_row_field x : Compiler_types.row_field =
  match (x : Versions.V4_07.Row_field.concrete) with
  | Rtag (x1, x2, x3, x4) ->
    let x2 = ast_to_attributes x2 in
    let x4 = (List.map ~f:ast_to_core_type) x4 in
    Rtag (x1, x2, x3, x4)
  | Rinherit (x1) ->
    let x1 = ast_to_core_type x1 in
    Rinherit (x1)

and ast_of_object_field x =
  Versions.V4_07.Object_field.of_concrete (concrete_of_object_field x)

and concrete_of_object_field x : Versions.V4_07.Object_field.concrete =
  match (x : Compiler_types.object_field) with
  | Otag (x1, x2, x3) ->
    let x2 = ast_of_attributes x2 in
    let x3 = ast_of_core_type x3 in
    Otag (x1, x2, x3)
  | Oinherit (x1) ->
    let x1 = ast_of_core_type x1 in
    Oinherit (x1)

and ast_to_object_field x =
  let concrete = Versions.V4_07.Object_field.to_concrete x in
  concrete_to_object_field concrete

and concrete_to_object_field x : Compiler_types.object_field =
  match (x : Versions.V4_07.Object_field.concrete) with
  | Otag (x1, x2, x3) ->
    let x2 = ast_to_attributes x2 in
    let x3 = ast_to_core_type x3 in
    Otag (x1, x2, x3)
  | Oinherit (x1) ->
    let x1 = ast_to_core_type x1 in
    Oinherit (x1)

and ast_of_pattern x =
  Versions.V4_07.Pattern.of_concrete (concrete_of_pattern x)

and concrete_of_pattern
  ({ ppat_desc; ppat_loc; ppat_attributes } : Compiler_types.pattern)
=
  let ppat_desc = ast_of_pattern_desc ppat_desc in
  let ppat_attributes = ast_of_attributes ppat_attributes in
  ({ ppat_desc; ppat_loc; ppat_attributes } : Versions.V4_07.Pattern.concrete)

and ast_to_pattern x =
  let concrete = Versions.V4_07.Pattern.to_concrete x in
  concrete_to_pattern concrete

and concrete_to_pattern
  ({ ppat_desc; ppat_loc; ppat_attributes } : Versions.V4_07.Pattern.concrete)
=
  let ppat_desc = ast_to_pattern_desc ppat_desc in
  let ppat_attributes = ast_to_attributes ppat_attributes in
  ({ ppat_desc; ppat_loc; ppat_attributes } : Compiler_types.pattern)

and ast_of_pattern_desc x =
  Versions.V4_07.Pattern_desc.of_concrete (concrete_of_pattern_desc x)

and concrete_of_pattern_desc x : Versions.V4_07.Pattern_desc.concrete =
  match (x : Compiler_types.pattern_desc) with
  | Ppat_any -> Ppat_any
  | Ppat_var (x1) ->
    Ppat_var (x1)
  | Ppat_alias (x1, x2) ->
    let x1 = ast_of_pattern x1 in
    Ppat_alias (x1, x2)
  | Ppat_constant (x1) ->
    let x1 = ast_of_constant x1 in
    Ppat_constant (x1)
  | Ppat_interval (x1, x2) ->
    let x1 = ast_of_constant x1 in
    let x2 = ast_of_constant x2 in
    Ppat_interval (x1, x2)
  | Ppat_tuple (x1) ->
    let x1 = (List.map ~f:ast_of_pattern) x1 in
    Ppat_tuple (x1)
  | Ppat_construct (x1, x2) ->
    let x1 = ast_of_longident_loc x1 in
    let x2 = (Option.map ~f:ast_of_pattern) x2 in
    Ppat_construct (x1, x2)
  | Ppat_variant (x1, x2) ->
    let x2 = (Option.map ~f:ast_of_pattern) x2 in
    Ppat_variant (x1, x2)
  | Ppat_record (x1, x2) ->
    let x1 = (List.map ~f:(Tuple.map2 ~f1:ast_of_longident_loc ~f2:ast_of_pattern)) x1 in
    let x2 = ast_of_closed_flag x2 in
    Ppat_record (x1, x2)
  | Ppat_array (x1) ->
    let x1 = (List.map ~f:ast_of_pattern) x1 in
    Ppat_array (x1)
  | Ppat_or (x1, x2) ->
    let x1 = ast_of_pattern x1 in
    let x2 = ast_of_pattern x2 in
    Ppat_or (x1, x2)
  | Ppat_constraint (x1, x2) ->
    let x1 = ast_of_pattern x1 in
    let x2 = ast_of_core_type x2 in
    Ppat_constraint (x1, x2)
  | Ppat_type (x1) ->
    let x1 = ast_of_longident_loc x1 in
    Ppat_type (x1)
  | Ppat_lazy (x1) ->
    let x1 = ast_of_pattern x1 in
    Ppat_lazy (x1)
  | Ppat_unpack (x1) ->
    Ppat_unpack (x1)
  | Ppat_exception (x1) ->
    let x1 = ast_of_pattern x1 in
    Ppat_exception (x1)
  | Ppat_extension (x1) ->
    let x1 = ast_of_extension x1 in
    Ppat_extension (x1)
  | Ppat_open (x1, x2) ->
    let x1 = ast_of_longident_loc x1 in
    let x2 = ast_of_pattern x2 in
    Ppat_open (x1, x2)

and ast_to_pattern_desc x =
  let concrete = Versions.V4_07.Pattern_desc.to_concrete x in
  concrete_to_pattern_desc concrete

and concrete_to_pattern_desc x : Compiler_types.pattern_desc =
  match (x : Versions.V4_07.Pattern_desc.concrete) with
  | Ppat_any -> Ppat_any
  | Ppat_var (x1) ->
    Ppat_var (x1)
  | Ppat_alias (x1, x2) ->
    let x1 = ast_to_pattern x1 in
    Ppat_alias (x1, x2)
  | Ppat_constant (x1) ->
    let x1 = ast_to_constant x1 in
    Ppat_constant (x1)
  | Ppat_interval (x1, x2) ->
    let x1 = ast_to_constant x1 in
    let x2 = ast_to_constant x2 in
    Ppat_interval (x1, x2)
  | Ppat_tuple (x1) ->
    let x1 = (List.map ~f:ast_to_pattern) x1 in
    Ppat_tuple (x1)
  | Ppat_construct (x1, x2) ->
    let x1 = ast_to_longident_loc x1 in
    let x2 = (Option.map ~f:ast_to_pattern) x2 in
    Ppat_construct (x1, x2)
  | Ppat_variant (x1, x2) ->
    let x2 = (Option.map ~f:ast_to_pattern) x2 in
    Ppat_variant (x1, x2)
  | Ppat_record (x1, x2) ->
    let x1 = (List.map ~f:(Tuple.map2 ~f1:ast_to_longident_loc ~f2:ast_to_pattern)) x1 in
    let x2 = ast_to_closed_flag x2 in
    Ppat_record (x1, x2)
  | Ppat_array (x1) ->
    let x1 = (List.map ~f:ast_to_pattern) x1 in
    Ppat_array (x1)
  | Ppat_or (x1, x2) ->
    let x1 = ast_to_pattern x1 in
    let x2 = ast_to_pattern x2 in
    Ppat_or (x1, x2)
  | Ppat_constraint (x1, x2) ->
    let x1 = ast_to_pattern x1 in
    let x2 = ast_to_core_type x2 in
    Ppat_constraint (x1, x2)
  | Ppat_type (x1) ->
    let x1 = ast_to_longident_loc x1 in
    Ppat_type (x1)
  | Ppat_lazy (x1) ->
    let x1 = ast_to_pattern x1 in
    Ppat_lazy (x1)
  | Ppat_unpack (x1) ->
    Ppat_unpack (x1)
  | Ppat_exception (x1) ->
    let x1 = ast_to_pattern x1 in
    Ppat_exception (x1)
  | Ppat_extension (x1) ->
    let x1 = ast_to_extension x1 in
    Ppat_extension (x1)
  | Ppat_open (x1, x2) ->
    let x1 = ast_to_longident_loc x1 in
    let x2 = ast_to_pattern x2 in
    Ppat_open (x1, x2)

and ast_of_expression x =
  Versions.V4_07.Expression.of_concrete (concrete_of_expression x)

and concrete_of_expression
  ({ pexp_desc; pexp_loc; pexp_attributes } : Compiler_types.expression)
=
  let pexp_desc = ast_of_expression_desc pexp_desc in
  let pexp_attributes = ast_of_attributes pexp_attributes in
  ({ pexp_desc; pexp_loc; pexp_attributes } : Versions.V4_07.Expression.concrete)

and ast_to_expression x =
  let concrete = Versions.V4_07.Expression.to_concrete x in
  concrete_to_expression concrete

and concrete_to_expression
  ({ pexp_desc; pexp_loc; pexp_attributes } : Versions.V4_07.Expression.concrete)
=
  let pexp_desc = ast_to_expression_desc pexp_desc in
  let pexp_attributes = ast_to_attributes pexp_attributes in
  ({ pexp_desc; pexp_loc; pexp_attributes } : Compiler_types.expression)

and ast_of_expression_desc x =
  Versions.V4_07.Expression_desc.of_concrete (concrete_of_expression_desc x)

and concrete_of_expression_desc x : Versions.V4_07.Expression_desc.concrete =
  match (x : Compiler_types.expression_desc) with
  | Pexp_ident (x1) ->
    let x1 = ast_of_longident_loc x1 in
    Pexp_ident (x1)
  | Pexp_constant (x1) ->
    let x1 = ast_of_constant x1 in
    Pexp_constant (x1)
  | Pexp_let (x1, x2, x3) ->
    let x1 = ast_of_rec_flag x1 in
    let x2 = (List.map ~f:ast_of_value_binding) x2 in
    let x3 = ast_of_expression x3 in
    Pexp_let (x1, x2, x3)
  | Pexp_function (x1) ->
    let x1 = (List.map ~f:ast_of_case) x1 in
    Pexp_function (x1)
  | Pexp_fun (x1, x2, x3, x4) ->
    let x1 = ast_of_arg_label x1 in
    let x2 = (Option.map ~f:ast_of_expression) x2 in
    let x3 = ast_of_pattern x3 in
    let x4 = ast_of_expression x4 in
    Pexp_fun (x1, x2, x3, x4)
  | Pexp_apply (x1, x2) ->
    let x1 = ast_of_expression x1 in
    let x2 = (List.map ~f:(Tuple.map2 ~f1:ast_of_arg_label ~f2:ast_of_expression)) x2 in
    Pexp_apply (x1, x2)
  | Pexp_match (x1, x2) ->
    let x1 = ast_of_expression x1 in
    let x2 = (List.map ~f:ast_of_case) x2 in
    Pexp_match (x1, x2)
  | Pexp_try (x1, x2) ->
    let x1 = ast_of_expression x1 in
    let x2 = (List.map ~f:ast_of_case) x2 in
    Pexp_try (x1, x2)
  | Pexp_tuple (x1) ->
    let x1 = (List.map ~f:ast_of_expression) x1 in
    Pexp_tuple (x1)
  | Pexp_construct (x1, x2) ->
    let x1 = ast_of_longident_loc x1 in
    let x2 = (Option.map ~f:ast_of_expression) x2 in
    Pexp_construct (x1, x2)
  | Pexp_variant (x1, x2) ->
    let x2 = (Option.map ~f:ast_of_expression) x2 in
    Pexp_variant (x1, x2)
  | Pexp_record (x1, x2) ->
    let x1 = (List.map ~f:(Tuple.map2 ~f1:ast_of_longident_loc ~f2:ast_of_expression)) x1 in
    let x2 = (Option.map ~f:ast_of_expression) x2 in
    Pexp_record (x1, x2)
  | Pexp_field (x1, x2) ->
    let x1 = ast_of_expression x1 in
    let x2 = ast_of_longident_loc x2 in
    Pexp_field (x1, x2)
  | Pexp_setfield (x1, x2, x3) ->
    let x1 = ast_of_expression x1 in
    let x2 = ast_of_longident_loc x2 in
    let x3 = ast_of_expression x3 in
    Pexp_setfield (x1, x2, x3)
  | Pexp_array (x1) ->
    let x1 = (List.map ~f:ast_of_expression) x1 in
    Pexp_array (x1)
  | Pexp_ifthenelse (x1, x2, x3) ->
    let x1 = ast_of_expression x1 in
    let x2 = ast_of_expression x2 in
    let x3 = (Option.map ~f:ast_of_expression) x3 in
    Pexp_ifthenelse (x1, x2, x3)
  | Pexp_sequence (x1, x2) ->
    let x1 = ast_of_expression x1 in
    let x2 = ast_of_expression x2 in
    Pexp_sequence (x1, x2)
  | Pexp_while (x1, x2) ->
    let x1 = ast_of_expression x1 in
    let x2 = ast_of_expression x2 in
    Pexp_while (x1, x2)
  | Pexp_for (x1, x2, x3, x4, x5) ->
    let x1 = ast_of_pattern x1 in
    let x2 = ast_of_expression x2 in
    let x3 = ast_of_expression x3 in
    let x4 = ast_of_direction_flag x4 in
    let x5 = ast_of_expression x5 in
    Pexp_for (x1, x2, x3, x4, x5)
  | Pexp_constraint (x1, x2) ->
    let x1 = ast_of_expression x1 in
    let x2 = ast_of_core_type x2 in
    Pexp_constraint (x1, x2)
  | Pexp_coerce (x1, x2, x3) ->
    let x1 = ast_of_expression x1 in
    let x2 = (Option.map ~f:ast_of_core_type) x2 in
    let x3 = ast_of_core_type x3 in
    Pexp_coerce (x1, x2, x3)
  | Pexp_send (x1, x2) ->
    let x1 = ast_of_expression x1 in
    Pexp_send (x1, x2)
  | Pexp_new (x1) ->
    let x1 = ast_of_longident_loc x1 in
    Pexp_new (x1)
  | Pexp_setinstvar (x1, x2) ->
    let x2 = ast_of_expression x2 in
    Pexp_setinstvar (x1, x2)
  | Pexp_override (x1) ->
    let x1 = (List.map ~f:(Tuple.map2 ~f1:Fn.id ~f2:ast_of_expression)) x1 in
    Pexp_override (x1)
  | Pexp_letmodule (x1, x2, x3) ->
    let x2 = ast_of_module_expr x2 in
    let x3 = ast_of_expression x3 in
    Pexp_letmodule (x1, x2, x3)
  | Pexp_letexception (x1, x2) ->
    let x1 = ast_of_extension_constructor x1 in
    let x2 = ast_of_expression x2 in
    Pexp_letexception (x1, x2)
  | Pexp_assert (x1) ->
    let x1 = ast_of_expression x1 in
    Pexp_assert (x1)
  | Pexp_lazy (x1) ->
    let x1 = ast_of_expression x1 in
    Pexp_lazy (x1)
  | Pexp_poly (x1, x2) ->
    let x1 = ast_of_expression x1 in
    let x2 = (Option.map ~f:ast_of_core_type) x2 in
    Pexp_poly (x1, x2)
  | Pexp_object (x1) ->
    let x1 = ast_of_class_structure x1 in
    Pexp_object (x1)
  | Pexp_newtype (x1, x2) ->
    let x2 = ast_of_expression x2 in
    Pexp_newtype (x1, x2)
  | Pexp_pack (x1) ->
    let x1 = ast_of_module_expr x1 in
    Pexp_pack (x1)
  | Pexp_open (x1, x2, x3) ->
    let x1 = ast_of_override_flag x1 in
    let x2 = ast_of_longident_loc x2 in
    let x3 = ast_of_expression x3 in
    Pexp_open (x1, x2, x3)
  | Pexp_extension (x1) ->
    let x1 = ast_of_extension x1 in
    Pexp_extension (x1)
  | Pexp_unreachable -> Pexp_unreachable

and ast_to_expression_desc x =
  let concrete = Versions.V4_07.Expression_desc.to_concrete x in
  concrete_to_expression_desc concrete

and concrete_to_expression_desc x : Compiler_types.expression_desc =
  match (x : Versions.V4_07.Expression_desc.concrete) with
  | Pexp_ident (x1) ->
    let x1 = ast_to_longident_loc x1 in
    Pexp_ident (x1)
  | Pexp_constant (x1) ->
    let x1 = ast_to_constant x1 in
    Pexp_constant (x1)
  | Pexp_let (x1, x2, x3) ->
    let x1 = ast_to_rec_flag x1 in
    let x2 = (List.map ~f:ast_to_value_binding) x2 in
    let x3 = ast_to_expression x3 in
    Pexp_let (x1, x2, x3)
  | Pexp_function (x1) ->
    let x1 = (List.map ~f:ast_to_case) x1 in
    Pexp_function (x1)
  | Pexp_fun (x1, x2, x3, x4) ->
    let x1 = ast_to_arg_label x1 in
    let x2 = (Option.map ~f:ast_to_expression) x2 in
    let x3 = ast_to_pattern x3 in
    let x4 = ast_to_expression x4 in
    Pexp_fun (x1, x2, x3, x4)
  | Pexp_apply (x1, x2) ->
    let x1 = ast_to_expression x1 in
    let x2 = (List.map ~f:(Tuple.map2 ~f1:ast_to_arg_label ~f2:ast_to_expression)) x2 in
    Pexp_apply (x1, x2)
  | Pexp_match (x1, x2) ->
    let x1 = ast_to_expression x1 in
    let x2 = (List.map ~f:ast_to_case) x2 in
    Pexp_match (x1, x2)
  | Pexp_try (x1, x2) ->
    let x1 = ast_to_expression x1 in
    let x2 = (List.map ~f:ast_to_case) x2 in
    Pexp_try (x1, x2)
  | Pexp_tuple (x1) ->
    let x1 = (List.map ~f:ast_to_expression) x1 in
    Pexp_tuple (x1)
  | Pexp_construct (x1, x2) ->
    let x1 = ast_to_longident_loc x1 in
    let x2 = (Option.map ~f:ast_to_expression) x2 in
    Pexp_construct (x1, x2)
  | Pexp_variant (x1, x2) ->
    let x2 = (Option.map ~f:ast_to_expression) x2 in
    Pexp_variant (x1, x2)
  | Pexp_record (x1, x2) ->
    let x1 = (List.map ~f:(Tuple.map2 ~f1:ast_to_longident_loc ~f2:ast_to_expression)) x1 in
    let x2 = (Option.map ~f:ast_to_expression) x2 in
    Pexp_record (x1, x2)
  | Pexp_field (x1, x2) ->
    let x1 = ast_to_expression x1 in
    let x2 = ast_to_longident_loc x2 in
    Pexp_field (x1, x2)
  | Pexp_setfield (x1, x2, x3) ->
    let x1 = ast_to_expression x1 in
    let x2 = ast_to_longident_loc x2 in
    let x3 = ast_to_expression x3 in
    Pexp_setfield (x1, x2, x3)
  | Pexp_array (x1) ->
    let x1 = (List.map ~f:ast_to_expression) x1 in
    Pexp_array (x1)
  | Pexp_ifthenelse (x1, x2, x3) ->
    let x1 = ast_to_expression x1 in
    let x2 = ast_to_expression x2 in
    let x3 = (Option.map ~f:ast_to_expression) x3 in
    Pexp_ifthenelse (x1, x2, x3)
  | Pexp_sequence (x1, x2) ->
    let x1 = ast_to_expression x1 in
    let x2 = ast_to_expression x2 in
    Pexp_sequence (x1, x2)
  | Pexp_while (x1, x2) ->
    let x1 = ast_to_expression x1 in
    let x2 = ast_to_expression x2 in
    Pexp_while (x1, x2)
  | Pexp_for (x1, x2, x3, x4, x5) ->
    let x1 = ast_to_pattern x1 in
    let x2 = ast_to_expression x2 in
    let x3 = ast_to_expression x3 in
    let x4 = ast_to_direction_flag x4 in
    let x5 = ast_to_expression x5 in
    Pexp_for (x1, x2, x3, x4, x5)
  | Pexp_constraint (x1, x2) ->
    let x1 = ast_to_expression x1 in
    let x2 = ast_to_core_type x2 in
    Pexp_constraint (x1, x2)
  | Pexp_coerce (x1, x2, x3) ->
    let x1 = ast_to_expression x1 in
    let x2 = (Option.map ~f:ast_to_core_type) x2 in
    let x3 = ast_to_core_type x3 in
    Pexp_coerce (x1, x2, x3)
  | Pexp_send (x1, x2) ->
    let x1 = ast_to_expression x1 in
    Pexp_send (x1, x2)
  | Pexp_new (x1) ->
    let x1 = ast_to_longident_loc x1 in
    Pexp_new (x1)
  | Pexp_setinstvar (x1, x2) ->
    let x2 = ast_to_expression x2 in
    Pexp_setinstvar (x1, x2)
  | Pexp_override (x1) ->
    let x1 = (List.map ~f:(Tuple.map2 ~f1:Fn.id ~f2:ast_to_expression)) x1 in
    Pexp_override (x1)
  | Pexp_letmodule (x1, x2, x3) ->
    let x2 = ast_to_module_expr x2 in
    let x3 = ast_to_expression x3 in
    Pexp_letmodule (x1, x2, x3)
  | Pexp_letexception (x1, x2) ->
    let x1 = ast_to_extension_constructor x1 in
    let x2 = ast_to_expression x2 in
    Pexp_letexception (x1, x2)
  | Pexp_assert (x1) ->
    let x1 = ast_to_expression x1 in
    Pexp_assert (x1)
  | Pexp_lazy (x1) ->
    let x1 = ast_to_expression x1 in
    Pexp_lazy (x1)
  | Pexp_poly (x1, x2) ->
    let x1 = ast_to_expression x1 in
    let x2 = (Option.map ~f:ast_to_core_type) x2 in
    Pexp_poly (x1, x2)
  | Pexp_object (x1) ->
    let x1 = ast_to_class_structure x1 in
    Pexp_object (x1)
  | Pexp_newtype (x1, x2) ->
    let x2 = ast_to_expression x2 in
    Pexp_newtype (x1, x2)
  | Pexp_pack (x1) ->
    let x1 = ast_to_module_expr x1 in
    Pexp_pack (x1)
  | Pexp_open (x1, x2, x3) ->
    let x1 = ast_to_override_flag x1 in
    let x2 = ast_to_longident_loc x2 in
    let x3 = ast_to_expression x3 in
    Pexp_open (x1, x2, x3)
  | Pexp_extension (x1) ->
    let x1 = ast_to_extension x1 in
    Pexp_extension (x1)
  | Pexp_unreachable -> Pexp_unreachable

and ast_of_case x =
  Versions.V4_07.Case.of_concrete (concrete_of_case x)

and concrete_of_case
  ({ pc_lhs; pc_guard; pc_rhs } : Compiler_types.case)
=
  let pc_lhs = ast_of_pattern pc_lhs in
  let pc_guard = (Option.map ~f:ast_of_expression) pc_guard in
  let pc_rhs = ast_of_expression pc_rhs in
  ({ pc_lhs; pc_guard; pc_rhs } : Versions.V4_07.Case.concrete)

and ast_to_case x =
  let concrete = Versions.V4_07.Case.to_concrete x in
  concrete_to_case concrete

and concrete_to_case
  ({ pc_lhs; pc_guard; pc_rhs } : Versions.V4_07.Case.concrete)
=
  let pc_lhs = ast_to_pattern pc_lhs in
  let pc_guard = (Option.map ~f:ast_to_expression) pc_guard in
  let pc_rhs = ast_to_expression pc_rhs in
  ({ pc_lhs; pc_guard; pc_rhs } : Compiler_types.case)

and ast_of_value_description x =
  Versions.V4_07.Value_description.of_concrete (concrete_of_value_description x)

and concrete_of_value_description
  ({ pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Compiler_types.value_description)
=
  let pval_type = ast_of_core_type pval_type in
  let pval_attributes = ast_of_attributes pval_attributes in
  ({ pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Versions.V4_07.Value_description.concrete)

and ast_to_value_description x =
  let concrete = Versions.V4_07.Value_description.to_concrete x in
  concrete_to_value_description concrete

and concrete_to_value_description
  ({ pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Versions.V4_07.Value_description.concrete)
=
  let pval_type = ast_to_core_type pval_type in
  let pval_attributes = ast_to_attributes pval_attributes in
  ({ pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Compiler_types.value_description)

and ast_of_type_declaration x =
  Versions.V4_07.Type_declaration.of_concrete (concrete_of_type_declaration x)

and concrete_of_type_declaration
  ({ ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Compiler_types.type_declaration)
=
  let ptype_params = (List.map ~f:(Tuple.map2 ~f1:ast_of_core_type ~f2:ast_of_variance)) ptype_params in
  let ptype_cstrs = (List.map ~f:(Tuple.map3 ~f1:ast_of_core_type ~f2:ast_of_core_type ~f3:Fn.id)) ptype_cstrs in
  let ptype_kind = ast_of_type_kind ptype_kind in
  let ptype_private = ast_of_private_flag ptype_private in
  let ptype_manifest = (Option.map ~f:ast_of_core_type) ptype_manifest in
  let ptype_attributes = ast_of_attributes ptype_attributes in
  ({ ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Versions.V4_07.Type_declaration.concrete)

and ast_to_type_declaration x =
  let concrete = Versions.V4_07.Type_declaration.to_concrete x in
  concrete_to_type_declaration concrete

and concrete_to_type_declaration
  ({ ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Versions.V4_07.Type_declaration.concrete)
=
  let ptype_params = (List.map ~f:(Tuple.map2 ~f1:ast_to_core_type ~f2:ast_to_variance)) ptype_params in
  let ptype_cstrs = (List.map ~f:(Tuple.map3 ~f1:ast_to_core_type ~f2:ast_to_core_type ~f3:Fn.id)) ptype_cstrs in
  let ptype_kind = ast_to_type_kind ptype_kind in
  let ptype_private = ast_to_private_flag ptype_private in
  let ptype_manifest = (Option.map ~f:ast_to_core_type) ptype_manifest in
  let ptype_attributes = ast_to_attributes ptype_attributes in
  ({ ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Compiler_types.type_declaration)

and ast_of_type_kind x =
  Versions.V4_07.Type_kind.of_concrete (concrete_of_type_kind x)

and concrete_of_type_kind x : Versions.V4_07.Type_kind.concrete =
  match (x : Compiler_types.type_kind) with
  | Ptype_abstract -> Ptype_abstract
  | Ptype_variant (x1) ->
    let x1 = (List.map ~f:ast_of_constructor_declaration) x1 in
    Ptype_variant (x1)
  | Ptype_record (x1) ->
    let x1 = (List.map ~f:ast_of_label_declaration) x1 in
    Ptype_record (x1)
  | Ptype_open -> Ptype_open

and ast_to_type_kind x =
  let concrete = Versions.V4_07.Type_kind.to_concrete x in
  concrete_to_type_kind concrete

and concrete_to_type_kind x : Compiler_types.type_kind =
  match (x : Versions.V4_07.Type_kind.concrete) with
  | Ptype_abstract -> Ptype_abstract
  | Ptype_variant (x1) ->
    let x1 = (List.map ~f:ast_to_constructor_declaration) x1 in
    Ptype_variant (x1)
  | Ptype_record (x1) ->
    let x1 = (List.map ~f:ast_to_label_declaration) x1 in
    Ptype_record (x1)
  | Ptype_open -> Ptype_open

and ast_of_label_declaration x =
  Versions.V4_07.Label_declaration.of_concrete (concrete_of_label_declaration x)

and concrete_of_label_declaration
  ({ pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Compiler_types.label_declaration)
=
  let pld_mutable = ast_of_mutable_flag pld_mutable in
  let pld_type = ast_of_core_type pld_type in
  let pld_attributes = ast_of_attributes pld_attributes in
  ({ pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Versions.V4_07.Label_declaration.concrete)

and ast_to_label_declaration x =
  let concrete = Versions.V4_07.Label_declaration.to_concrete x in
  concrete_to_label_declaration concrete

and concrete_to_label_declaration
  ({ pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Versions.V4_07.Label_declaration.concrete)
=
  let pld_mutable = ast_to_mutable_flag pld_mutable in
  let pld_type = ast_to_core_type pld_type in
  let pld_attributes = ast_to_attributes pld_attributes in
  ({ pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Compiler_types.label_declaration)

and ast_of_constructor_declaration x =
  Versions.V4_07.Constructor_declaration.of_concrete (concrete_of_constructor_declaration x)

and concrete_of_constructor_declaration
  ({ pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Compiler_types.constructor_declaration)
=
  let pcd_args = ast_of_constructor_arguments pcd_args in
  let pcd_res = (Option.map ~f:ast_of_core_type) pcd_res in
  let pcd_attributes = ast_of_attributes pcd_attributes in
  ({ pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Versions.V4_07.Constructor_declaration.concrete)

and ast_to_constructor_declaration x =
  let concrete = Versions.V4_07.Constructor_declaration.to_concrete x in
  concrete_to_constructor_declaration concrete

and concrete_to_constructor_declaration
  ({ pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Versions.V4_07.Constructor_declaration.concrete)
=
  let pcd_args = ast_to_constructor_arguments pcd_args in
  let pcd_res = (Option.map ~f:ast_to_core_type) pcd_res in
  let pcd_attributes = ast_to_attributes pcd_attributes in
  ({ pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Compiler_types.constructor_declaration)

and ast_of_constructor_arguments x =
  Versions.V4_07.Constructor_arguments.of_concrete (concrete_of_constructor_arguments x)

and concrete_of_constructor_arguments x : Versions.V4_07.Constructor_arguments.concrete =
  match (x : Compiler_types.constructor_arguments) with
  | Pcstr_tuple (x1) ->
    let x1 = (List.map ~f:ast_of_core_type) x1 in
    Pcstr_tuple (x1)
  | Pcstr_record (x1) ->
    let x1 = (List.map ~f:ast_of_label_declaration) x1 in
    Pcstr_record (x1)

and ast_to_constructor_arguments x =
  let concrete = Versions.V4_07.Constructor_arguments.to_concrete x in
  concrete_to_constructor_arguments concrete

and concrete_to_constructor_arguments x : Compiler_types.constructor_arguments =
  match (x : Versions.V4_07.Constructor_arguments.concrete) with
  | Pcstr_tuple (x1) ->
    let x1 = (List.map ~f:ast_to_core_type) x1 in
    Pcstr_tuple (x1)
  | Pcstr_record (x1) ->
    let x1 = (List.map ~f:ast_to_label_declaration) x1 in
    Pcstr_record (x1)

and ast_of_type_extension x =
  Versions.V4_07.Type_extension.of_concrete (concrete_of_type_extension x)

and concrete_of_type_extension
  ({ ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Compiler_types.type_extension)
=
  let ptyext_path = ast_of_longident_loc ptyext_path in
  let ptyext_params = (List.map ~f:(Tuple.map2 ~f1:ast_of_core_type ~f2:ast_of_variance)) ptyext_params in
  let ptyext_constructors = (List.map ~f:ast_of_extension_constructor) ptyext_constructors in
  let ptyext_private = ast_of_private_flag ptyext_private in
  let ptyext_attributes = ast_of_attributes ptyext_attributes in
  ({ ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Versions.V4_07.Type_extension.concrete)

and ast_to_type_extension x =
  let concrete = Versions.V4_07.Type_extension.to_concrete x in
  concrete_to_type_extension concrete

and concrete_to_type_extension
  ({ ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Versions.V4_07.Type_extension.concrete)
=
  let ptyext_path = ast_to_longident_loc ptyext_path in
  let ptyext_params = (List.map ~f:(Tuple.map2 ~f1:ast_to_core_type ~f2:ast_to_variance)) ptyext_params in
  let ptyext_constructors = (List.map ~f:ast_to_extension_constructor) ptyext_constructors in
  let ptyext_private = ast_to_private_flag ptyext_private in
  let ptyext_attributes = ast_to_attributes ptyext_attributes in
  ({ ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Compiler_types.type_extension)

and ast_of_extension_constructor x =
  Versions.V4_07.Extension_constructor.of_concrete (concrete_of_extension_constructor x)

and concrete_of_extension_constructor
  ({ pext_name; pext_kind; pext_loc; pext_attributes } : Compiler_types.extension_constructor)
=
  let pext_kind = ast_of_extension_constructor_kind pext_kind in
  let pext_attributes = ast_of_attributes pext_attributes in
  ({ pext_name; pext_kind; pext_loc; pext_attributes } : Versions.V4_07.Extension_constructor.concrete)

and ast_to_extension_constructor x =
  let concrete = Versions.V4_07.Extension_constructor.to_concrete x in
  concrete_to_extension_constructor concrete

and concrete_to_extension_constructor
  ({ pext_name; pext_kind; pext_loc; pext_attributes } : Versions.V4_07.Extension_constructor.concrete)
=
  let pext_kind = ast_to_extension_constructor_kind pext_kind in
  let pext_attributes = ast_to_attributes pext_attributes in
  ({ pext_name; pext_kind; pext_loc; pext_attributes } : Compiler_types.extension_constructor)

and ast_of_extension_constructor_kind x =
  Versions.V4_07.Extension_constructor_kind.of_concrete (concrete_of_extension_constructor_kind x)

and concrete_of_extension_constructor_kind x : Versions.V4_07.Extension_constructor_kind.concrete =
  match (x : Compiler_types.extension_constructor_kind) with
  | Pext_decl (x1, x2) ->
    let x1 = ast_of_constructor_arguments x1 in
    let x2 = (Option.map ~f:ast_of_core_type) x2 in
    Pext_decl (x1, x2)
  | Pext_rebind (x1) ->
    let x1 = ast_of_longident_loc x1 in
    Pext_rebind (x1)

and ast_to_extension_constructor_kind x =
  let concrete = Versions.V4_07.Extension_constructor_kind.to_concrete x in
  concrete_to_extension_constructor_kind concrete

and concrete_to_extension_constructor_kind x : Compiler_types.extension_constructor_kind =
  match (x : Versions.V4_07.Extension_constructor_kind.concrete) with
  | Pext_decl (x1, x2) ->
    let x1 = ast_to_constructor_arguments x1 in
    let x2 = (Option.map ~f:ast_to_core_type) x2 in
    Pext_decl (x1, x2)
  | Pext_rebind (x1) ->
    let x1 = ast_to_longident_loc x1 in
    Pext_rebind (x1)

and ast_of_class_type x =
  Versions.V4_07.Class_type.of_concrete (concrete_of_class_type x)

and concrete_of_class_type
  ({ pcty_desc; pcty_loc; pcty_attributes } : Compiler_types.class_type)
=
  let pcty_desc = ast_of_class_type_desc pcty_desc in
  let pcty_attributes = ast_of_attributes pcty_attributes in
  ({ pcty_desc; pcty_loc; pcty_attributes } : Versions.V4_07.Class_type.concrete)

and ast_to_class_type x =
  let concrete = Versions.V4_07.Class_type.to_concrete x in
  concrete_to_class_type concrete

and concrete_to_class_type
  ({ pcty_desc; pcty_loc; pcty_attributes } : Versions.V4_07.Class_type.concrete)
=
  let pcty_desc = ast_to_class_type_desc pcty_desc in
  let pcty_attributes = ast_to_attributes pcty_attributes in
  ({ pcty_desc; pcty_loc; pcty_attributes } : Compiler_types.class_type)

and ast_of_class_type_desc x =
  Versions.V4_07.Class_type_desc.of_concrete (concrete_of_class_type_desc x)

and concrete_of_class_type_desc x : Versions.V4_07.Class_type_desc.concrete =
  match (x : Compiler_types.class_type_desc) with
  | Pcty_constr (x1, x2) ->
    let x1 = ast_of_longident_loc x1 in
    let x2 = (List.map ~f:ast_of_core_type) x2 in
    Pcty_constr (x1, x2)
  | Pcty_signature (x1) ->
    let x1 = ast_of_class_signature x1 in
    Pcty_signature (x1)
  | Pcty_arrow (x1, x2, x3) ->
    let x1 = ast_of_arg_label x1 in
    let x2 = ast_of_core_type x2 in
    let x3 = ast_of_class_type x3 in
    Pcty_arrow (x1, x2, x3)
  | Pcty_extension (x1) ->
    let x1 = ast_of_extension x1 in
    Pcty_extension (x1)
  | Pcty_open (x1, x2, x3) ->
    let x1 = ast_of_override_flag x1 in
    let x2 = ast_of_longident_loc x2 in
    let x3 = ast_of_class_type x3 in
    Pcty_open (x1, x2, x3)

and ast_to_class_type_desc x =
  let concrete = Versions.V4_07.Class_type_desc.to_concrete x in
  concrete_to_class_type_desc concrete

and concrete_to_class_type_desc x : Compiler_types.class_type_desc =
  match (x : Versions.V4_07.Class_type_desc.concrete) with
  | Pcty_constr (x1, x2) ->
    let x1 = ast_to_longident_loc x1 in
    let x2 = (List.map ~f:ast_to_core_type) x2 in
    Pcty_constr (x1, x2)
  | Pcty_signature (x1) ->
    let x1 = ast_to_class_signature x1 in
    Pcty_signature (x1)
  | Pcty_arrow (x1, x2, x3) ->
    let x1 = ast_to_arg_label x1 in
    let x2 = ast_to_core_type x2 in
    let x3 = ast_to_class_type x3 in
    Pcty_arrow (x1, x2, x3)
  | Pcty_extension (x1) ->
    let x1 = ast_to_extension x1 in
    Pcty_extension (x1)
  | Pcty_open (x1, x2, x3) ->
    let x1 = ast_to_override_flag x1 in
    let x2 = ast_to_longident_loc x2 in
    let x3 = ast_to_class_type x3 in
    Pcty_open (x1, x2, x3)

and ast_of_class_signature x =
  Versions.V4_07.Class_signature.of_concrete (concrete_of_class_signature x)

and concrete_of_class_signature
  ({ pcsig_self; pcsig_fields } : Compiler_types.class_signature)
=
  let pcsig_self = ast_of_core_type pcsig_self in
  let pcsig_fields = (List.map ~f:ast_of_class_type_field) pcsig_fields in
  ({ pcsig_self; pcsig_fields } : Versions.V4_07.Class_signature.concrete)

and ast_to_class_signature x =
  let concrete = Versions.V4_07.Class_signature.to_concrete x in
  concrete_to_class_signature concrete

and concrete_to_class_signature
  ({ pcsig_self; pcsig_fields } : Versions.V4_07.Class_signature.concrete)
=
  let pcsig_self = ast_to_core_type pcsig_self in
  let pcsig_fields = (List.map ~f:ast_to_class_type_field) pcsig_fields in
  ({ pcsig_self; pcsig_fields } : Compiler_types.class_signature)

and ast_of_class_type_field x =
  Versions.V4_07.Class_type_field.of_concrete (concrete_of_class_type_field x)

and concrete_of_class_type_field
  ({ pctf_desc; pctf_loc; pctf_attributes } : Compiler_types.class_type_field)
=
  let pctf_desc = ast_of_class_type_field_desc pctf_desc in
  let pctf_attributes = ast_of_attributes pctf_attributes in
  ({ pctf_desc; pctf_loc; pctf_attributes } : Versions.V4_07.Class_type_field.concrete)

and ast_to_class_type_field x =
  let concrete = Versions.V4_07.Class_type_field.to_concrete x in
  concrete_to_class_type_field concrete

and concrete_to_class_type_field
  ({ pctf_desc; pctf_loc; pctf_attributes } : Versions.V4_07.Class_type_field.concrete)
=
  let pctf_desc = ast_to_class_type_field_desc pctf_desc in
  let pctf_attributes = ast_to_attributes pctf_attributes in
  ({ pctf_desc; pctf_loc; pctf_attributes } : Compiler_types.class_type_field)

and ast_of_class_type_field_desc x =
  Versions.V4_07.Class_type_field_desc.of_concrete (concrete_of_class_type_field_desc x)

and concrete_of_class_type_field_desc x : Versions.V4_07.Class_type_field_desc.concrete =
  match (x : Compiler_types.class_type_field_desc) with
  | Pctf_inherit (x1) ->
    let x1 = ast_of_class_type x1 in
    Pctf_inherit (x1)
  | Pctf_val (x1) ->
    let x1 = (Tuple.map4 ~f1:Fn.id ~f2:ast_of_mutable_flag ~f3:ast_of_virtual_flag ~f4:ast_of_core_type) x1 in
    Pctf_val (x1)
  | Pctf_method (x1) ->
    let x1 = (Tuple.map4 ~f1:Fn.id ~f2:ast_of_private_flag ~f3:ast_of_virtual_flag ~f4:ast_of_core_type) x1 in
    Pctf_method (x1)
  | Pctf_constraint (x1) ->
    let x1 = (Tuple.map2 ~f1:ast_of_core_type ~f2:ast_of_core_type) x1 in
    Pctf_constraint (x1)
  | Pctf_attribute (x1) ->
    let x1 = ast_of_attribute x1 in
    Pctf_attribute (x1)
  | Pctf_extension (x1) ->
    let x1 = ast_of_extension x1 in
    Pctf_extension (x1)

and ast_to_class_type_field_desc x =
  let concrete = Versions.V4_07.Class_type_field_desc.to_concrete x in
  concrete_to_class_type_field_desc concrete

and concrete_to_class_type_field_desc x : Compiler_types.class_type_field_desc =
  match (x : Versions.V4_07.Class_type_field_desc.concrete) with
  | Pctf_inherit (x1) ->
    let x1 = ast_to_class_type x1 in
    Pctf_inherit (x1)
  | Pctf_val (x1) ->
    let x1 = (Tuple.map4 ~f1:Fn.id ~f2:ast_to_mutable_flag ~f3:ast_to_virtual_flag ~f4:ast_to_core_type) x1 in
    Pctf_val (x1)
  | Pctf_method (x1) ->
    let x1 = (Tuple.map4 ~f1:Fn.id ~f2:ast_to_private_flag ~f3:ast_to_virtual_flag ~f4:ast_to_core_type) x1 in
    Pctf_method (x1)
  | Pctf_constraint (x1) ->
    let x1 = (Tuple.map2 ~f1:ast_to_core_type ~f2:ast_to_core_type) x1 in
    Pctf_constraint (x1)
  | Pctf_attribute (x1) ->
    let x1 = ast_to_attribute x1 in
    Pctf_attribute (x1)
  | Pctf_extension (x1) ->
    let x1 = ast_to_extension x1 in
    Pctf_extension (x1)

and ast_of_class_infos_class_expr x =
  Versions.V4_07.Class_infos.of_concrete (concrete_of_class_infos_class_expr x)

and concrete_of_class_infos_class_expr
  ({ pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Compiler_types.class_expr Compiler_types.class_infos)
=
  let pci_virt = ast_of_virtual_flag pci_virt in
  let pci_params = (List.map ~f:(Tuple.map2 ~f1:ast_of_core_type ~f2:ast_of_variance)) pci_params in
  let pci_expr = ast_of_class_expr pci_expr in
  let pci_attributes = ast_of_attributes pci_attributes in
  ({ pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Versions.V4_07.Class_expr.t Versions.V4_07.Class_infos.concrete)

and ast_to_class_infos_class_expr x =
  let concrete = Versions.V4_07.Class_infos.to_concrete x in
  concrete_to_class_infos_class_expr concrete

and concrete_to_class_infos_class_expr
  ({ pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Versions.V4_07.Class_expr.t Versions.V4_07.Class_infos.concrete)
=
  let pci_virt = ast_to_virtual_flag pci_virt in
  let pci_params = (List.map ~f:(Tuple.map2 ~f1:ast_to_core_type ~f2:ast_to_variance)) pci_params in
  let pci_expr = ast_to_class_expr pci_expr in
  let pci_attributes = ast_to_attributes pci_attributes in
  ({ pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Compiler_types.class_expr Compiler_types.class_infos)

and ast_of_class_infos_class_type x =
  Versions.V4_07.Class_infos.of_concrete (concrete_of_class_infos_class_type x)

and concrete_of_class_infos_class_type
  ({ pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Compiler_types.class_type Compiler_types.class_infos)
=
  let pci_virt = ast_of_virtual_flag pci_virt in
  let pci_params = (List.map ~f:(Tuple.map2 ~f1:ast_of_core_type ~f2:ast_of_variance)) pci_params in
  let pci_expr = ast_of_class_type pci_expr in
  let pci_attributes = ast_of_attributes pci_attributes in
  ({ pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Versions.V4_07.Class_type.t Versions.V4_07.Class_infos.concrete)

and ast_to_class_infos_class_type x =
  let concrete = Versions.V4_07.Class_infos.to_concrete x in
  concrete_to_class_infos_class_type concrete

and concrete_to_class_infos_class_type
  ({ pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Versions.V4_07.Class_type.t Versions.V4_07.Class_infos.concrete)
=
  let pci_virt = ast_to_virtual_flag pci_virt in
  let pci_params = (List.map ~f:(Tuple.map2 ~f1:ast_to_core_type ~f2:ast_to_variance)) pci_params in
  let pci_expr = ast_to_class_type pci_expr in
  let pci_attributes = ast_to_attributes pci_attributes in
  ({ pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Compiler_types.class_type Compiler_types.class_infos)

and ast_of_class_description x =
  Versions.V4_07.Class_description.of_concrete (concrete_of_class_description x)

and concrete_of_class_description x =
  ast_of_class_infos_class_type x

and ast_to_class_description x =
  let concrete = Versions.V4_07.Class_description.to_concrete x in
  concrete_to_class_description concrete

and concrete_to_class_description x =
  ast_to_class_infos_class_type x

and ast_of_class_type_declaration x =
  Versions.V4_07.Class_type_declaration.of_concrete (concrete_of_class_type_declaration x)

and concrete_of_class_type_declaration x =
  ast_of_class_infos_class_type x

and ast_to_class_type_declaration x =
  let concrete = Versions.V4_07.Class_type_declaration.to_concrete x in
  concrete_to_class_type_declaration concrete

and concrete_to_class_type_declaration x =
  ast_to_class_infos_class_type x

and ast_of_class_expr x =
  Versions.V4_07.Class_expr.of_concrete (concrete_of_class_expr x)

and concrete_of_class_expr
  ({ pcl_desc; pcl_loc; pcl_attributes } : Compiler_types.class_expr)
=
  let pcl_desc = ast_of_class_expr_desc pcl_desc in
  let pcl_attributes = ast_of_attributes pcl_attributes in
  ({ pcl_desc; pcl_loc; pcl_attributes } : Versions.V4_07.Class_expr.concrete)

and ast_to_class_expr x =
  let concrete = Versions.V4_07.Class_expr.to_concrete x in
  concrete_to_class_expr concrete

and concrete_to_class_expr
  ({ pcl_desc; pcl_loc; pcl_attributes } : Versions.V4_07.Class_expr.concrete)
=
  let pcl_desc = ast_to_class_expr_desc pcl_desc in
  let pcl_attributes = ast_to_attributes pcl_attributes in
  ({ pcl_desc; pcl_loc; pcl_attributes } : Compiler_types.class_expr)

and ast_of_class_expr_desc x =
  Versions.V4_07.Class_expr_desc.of_concrete (concrete_of_class_expr_desc x)

and concrete_of_class_expr_desc x : Versions.V4_07.Class_expr_desc.concrete =
  match (x : Compiler_types.class_expr_desc) with
  | Pcl_constr (x1, x2) ->
    let x1 = ast_of_longident_loc x1 in
    let x2 = (List.map ~f:ast_of_core_type) x2 in
    Pcl_constr (x1, x2)
  | Pcl_structure (x1) ->
    let x1 = ast_of_class_structure x1 in
    Pcl_structure (x1)
  | Pcl_fun (x1, x2, x3, x4) ->
    let x1 = ast_of_arg_label x1 in
    let x2 = (Option.map ~f:ast_of_expression) x2 in
    let x3 = ast_of_pattern x3 in
    let x4 = ast_of_class_expr x4 in
    Pcl_fun (x1, x2, x3, x4)
  | Pcl_apply (x1, x2) ->
    let x1 = ast_of_class_expr x1 in
    let x2 = (List.map ~f:(Tuple.map2 ~f1:ast_of_arg_label ~f2:ast_of_expression)) x2 in
    Pcl_apply (x1, x2)
  | Pcl_let (x1, x2, x3) ->
    let x1 = ast_of_rec_flag x1 in
    let x2 = (List.map ~f:ast_of_value_binding) x2 in
    let x3 = ast_of_class_expr x3 in
    Pcl_let (x1, x2, x3)
  | Pcl_constraint (x1, x2) ->
    let x1 = ast_of_class_expr x1 in
    let x2 = ast_of_class_type x2 in
    Pcl_constraint (x1, x2)
  | Pcl_extension (x1) ->
    let x1 = ast_of_extension x1 in
    Pcl_extension (x1)
  | Pcl_open (x1, x2, x3) ->
    let x1 = ast_of_override_flag x1 in
    let x2 = ast_of_longident_loc x2 in
    let x3 = ast_of_class_expr x3 in
    Pcl_open (x1, x2, x3)

and ast_to_class_expr_desc x =
  let concrete = Versions.V4_07.Class_expr_desc.to_concrete x in
  concrete_to_class_expr_desc concrete

and concrete_to_class_expr_desc x : Compiler_types.class_expr_desc =
  match (x : Versions.V4_07.Class_expr_desc.concrete) with
  | Pcl_constr (x1, x2) ->
    let x1 = ast_to_longident_loc x1 in
    let x2 = (List.map ~f:ast_to_core_type) x2 in
    Pcl_constr (x1, x2)
  | Pcl_structure (x1) ->
    let x1 = ast_to_class_structure x1 in
    Pcl_structure (x1)
  | Pcl_fun (x1, x2, x3, x4) ->
    let x1 = ast_to_arg_label x1 in
    let x2 = (Option.map ~f:ast_to_expression) x2 in
    let x3 = ast_to_pattern x3 in
    let x4 = ast_to_class_expr x4 in
    Pcl_fun (x1, x2, x3, x4)
  | Pcl_apply (x1, x2) ->
    let x1 = ast_to_class_expr x1 in
    let x2 = (List.map ~f:(Tuple.map2 ~f1:ast_to_arg_label ~f2:ast_to_expression)) x2 in
    Pcl_apply (x1, x2)
  | Pcl_let (x1, x2, x3) ->
    let x1 = ast_to_rec_flag x1 in
    let x2 = (List.map ~f:ast_to_value_binding) x2 in
    let x3 = ast_to_class_expr x3 in
    Pcl_let (x1, x2, x3)
  | Pcl_constraint (x1, x2) ->
    let x1 = ast_to_class_expr x1 in
    let x2 = ast_to_class_type x2 in
    Pcl_constraint (x1, x2)
  | Pcl_extension (x1) ->
    let x1 = ast_to_extension x1 in
    Pcl_extension (x1)
  | Pcl_open (x1, x2, x3) ->
    let x1 = ast_to_override_flag x1 in
    let x2 = ast_to_longident_loc x2 in
    let x3 = ast_to_class_expr x3 in
    Pcl_open (x1, x2, x3)

and ast_of_class_structure x =
  Versions.V4_07.Class_structure.of_concrete (concrete_of_class_structure x)

and concrete_of_class_structure
  ({ pcstr_self; pcstr_fields } : Compiler_types.class_structure)
=
  let pcstr_self = ast_of_pattern pcstr_self in
  let pcstr_fields = (List.map ~f:ast_of_class_field) pcstr_fields in
  ({ pcstr_self; pcstr_fields } : Versions.V4_07.Class_structure.concrete)

and ast_to_class_structure x =
  let concrete = Versions.V4_07.Class_structure.to_concrete x in
  concrete_to_class_structure concrete

and concrete_to_class_structure
  ({ pcstr_self; pcstr_fields } : Versions.V4_07.Class_structure.concrete)
=
  let pcstr_self = ast_to_pattern pcstr_self in
  let pcstr_fields = (List.map ~f:ast_to_class_field) pcstr_fields in
  ({ pcstr_self; pcstr_fields } : Compiler_types.class_structure)

and ast_of_class_field x =
  Versions.V4_07.Class_field.of_concrete (concrete_of_class_field x)

and concrete_of_class_field
  ({ pcf_desc; pcf_loc; pcf_attributes } : Compiler_types.class_field)
=
  let pcf_desc = ast_of_class_field_desc pcf_desc in
  let pcf_attributes = ast_of_attributes pcf_attributes in
  ({ pcf_desc; pcf_loc; pcf_attributes } : Versions.V4_07.Class_field.concrete)

and ast_to_class_field x =
  let concrete = Versions.V4_07.Class_field.to_concrete x in
  concrete_to_class_field concrete

and concrete_to_class_field
  ({ pcf_desc; pcf_loc; pcf_attributes } : Versions.V4_07.Class_field.concrete)
=
  let pcf_desc = ast_to_class_field_desc pcf_desc in
  let pcf_attributes = ast_to_attributes pcf_attributes in
  ({ pcf_desc; pcf_loc; pcf_attributes } : Compiler_types.class_field)

and ast_of_class_field_desc x =
  Versions.V4_07.Class_field_desc.of_concrete (concrete_of_class_field_desc x)

and concrete_of_class_field_desc x : Versions.V4_07.Class_field_desc.concrete =
  match (x : Compiler_types.class_field_desc) with
  | Pcf_inherit (x1, x2, x3) ->
    let x1 = ast_of_override_flag x1 in
    let x2 = ast_of_class_expr x2 in
    Pcf_inherit (x1, x2, x3)
  | Pcf_val (x1) ->
    let x1 = (Tuple.map3 ~f1:Fn.id ~f2:ast_of_mutable_flag ~f3:ast_of_class_field_kind) x1 in
    Pcf_val (x1)
  | Pcf_method (x1) ->
    let x1 = (Tuple.map3 ~f1:Fn.id ~f2:ast_of_private_flag ~f3:ast_of_class_field_kind) x1 in
    Pcf_method (x1)
  | Pcf_constraint (x1) ->
    let x1 = (Tuple.map2 ~f1:ast_of_core_type ~f2:ast_of_core_type) x1 in
    Pcf_constraint (x1)
  | Pcf_initializer (x1) ->
    let x1 = ast_of_expression x1 in
    Pcf_initializer (x1)
  | Pcf_attribute (x1) ->
    let x1 = ast_of_attribute x1 in
    Pcf_attribute (x1)
  | Pcf_extension (x1) ->
    let x1 = ast_of_extension x1 in
    Pcf_extension (x1)

and ast_to_class_field_desc x =
  let concrete = Versions.V4_07.Class_field_desc.to_concrete x in
  concrete_to_class_field_desc concrete

and concrete_to_class_field_desc x : Compiler_types.class_field_desc =
  match (x : Versions.V4_07.Class_field_desc.concrete) with
  | Pcf_inherit (x1, x2, x3) ->
    let x1 = ast_to_override_flag x1 in
    let x2 = ast_to_class_expr x2 in
    Pcf_inherit (x1, x2, x3)
  | Pcf_val (x1) ->
    let x1 = (Tuple.map3 ~f1:Fn.id ~f2:ast_to_mutable_flag ~f3:ast_to_class_field_kind) x1 in
    Pcf_val (x1)
  | Pcf_method (x1) ->
    let x1 = (Tuple.map3 ~f1:Fn.id ~f2:ast_to_private_flag ~f3:ast_to_class_field_kind) x1 in
    Pcf_method (x1)
  | Pcf_constraint (x1) ->
    let x1 = (Tuple.map2 ~f1:ast_to_core_type ~f2:ast_to_core_type) x1 in
    Pcf_constraint (x1)
  | Pcf_initializer (x1) ->
    let x1 = ast_to_expression x1 in
    Pcf_initializer (x1)
  | Pcf_attribute (x1) ->
    let x1 = ast_to_attribute x1 in
    Pcf_attribute (x1)
  | Pcf_extension (x1) ->
    let x1 = ast_to_extension x1 in
    Pcf_extension (x1)

and ast_of_class_field_kind x =
  Versions.V4_07.Class_field_kind.of_concrete (concrete_of_class_field_kind x)

and concrete_of_class_field_kind x : Versions.V4_07.Class_field_kind.concrete =
  match (x : Compiler_types.class_field_kind) with
  | Cfk_virtual (x1) ->
    let x1 = ast_of_core_type x1 in
    Cfk_virtual (x1)
  | Cfk_concrete (x1, x2) ->
    let x1 = ast_of_override_flag x1 in
    let x2 = ast_of_expression x2 in
    Cfk_concrete (x1, x2)

and ast_to_class_field_kind x =
  let concrete = Versions.V4_07.Class_field_kind.to_concrete x in
  concrete_to_class_field_kind concrete

and concrete_to_class_field_kind x : Compiler_types.class_field_kind =
  match (x : Versions.V4_07.Class_field_kind.concrete) with
  | Cfk_virtual (x1) ->
    let x1 = ast_to_core_type x1 in
    Cfk_virtual (x1)
  | Cfk_concrete (x1, x2) ->
    let x1 = ast_to_override_flag x1 in
    let x2 = ast_to_expression x2 in
    Cfk_concrete (x1, x2)

and ast_of_class_declaration x =
  Versions.V4_07.Class_declaration.of_concrete (concrete_of_class_declaration x)

and concrete_of_class_declaration x =
  ast_of_class_infos_class_expr x

and ast_to_class_declaration x =
  let concrete = Versions.V4_07.Class_declaration.to_concrete x in
  concrete_to_class_declaration concrete

and concrete_to_class_declaration x =
  ast_to_class_infos_class_expr x

and ast_of_module_type x =
  Versions.V4_07.Module_type.of_concrete (concrete_of_module_type x)

and concrete_of_module_type
  ({ pmty_desc; pmty_loc; pmty_attributes } : Compiler_types.module_type)
=
  let pmty_desc = ast_of_module_type_desc pmty_desc in
  let pmty_attributes = ast_of_attributes pmty_attributes in
  ({ pmty_desc; pmty_loc; pmty_attributes } : Versions.V4_07.Module_type.concrete)

and ast_to_module_type x =
  let concrete = Versions.V4_07.Module_type.to_concrete x in
  concrete_to_module_type concrete

and concrete_to_module_type
  ({ pmty_desc; pmty_loc; pmty_attributes } : Versions.V4_07.Module_type.concrete)
=
  let pmty_desc = ast_to_module_type_desc pmty_desc in
  let pmty_attributes = ast_to_attributes pmty_attributes in
  ({ pmty_desc; pmty_loc; pmty_attributes } : Compiler_types.module_type)

and ast_of_module_type_desc x =
  Versions.V4_07.Module_type_desc.of_concrete (concrete_of_module_type_desc x)

and concrete_of_module_type_desc x : Versions.V4_07.Module_type_desc.concrete =
  match (x : Compiler_types.module_type_desc) with
  | Pmty_ident (x1) ->
    let x1 = ast_of_longident_loc x1 in
    Pmty_ident (x1)
  | Pmty_signature (x1) ->
    let x1 = ast_of_signature x1 in
    Pmty_signature (x1)
  | Pmty_functor (x1, x2, x3) ->
    let x2 = (Option.map ~f:ast_of_module_type) x2 in
    let x3 = ast_of_module_type x3 in
    Pmty_functor (x1, x2, x3)
  | Pmty_with (x1, x2) ->
    let x1 = ast_of_module_type x1 in
    let x2 = (List.map ~f:ast_of_with_constraint) x2 in
    Pmty_with (x1, x2)
  | Pmty_typeof (x1) ->
    let x1 = ast_of_module_expr x1 in
    Pmty_typeof (x1)
  | Pmty_extension (x1) ->
    let x1 = ast_of_extension x1 in
    Pmty_extension (x1)
  | Pmty_alias (x1) ->
    let x1 = ast_of_longident_loc x1 in
    Pmty_alias (x1)

and ast_to_module_type_desc x =
  let concrete = Versions.V4_07.Module_type_desc.to_concrete x in
  concrete_to_module_type_desc concrete

and concrete_to_module_type_desc x : Compiler_types.module_type_desc =
  match (x : Versions.V4_07.Module_type_desc.concrete) with
  | Pmty_ident (x1) ->
    let x1 = ast_to_longident_loc x1 in
    Pmty_ident (x1)
  | Pmty_signature (x1) ->
    let x1 = ast_to_signature x1 in
    Pmty_signature (x1)
  | Pmty_functor (x1, x2, x3) ->
    let x2 = (Option.map ~f:ast_to_module_type) x2 in
    let x3 = ast_to_module_type x3 in
    Pmty_functor (x1, x2, x3)
  | Pmty_with (x1, x2) ->
    let x1 = ast_to_module_type x1 in
    let x2 = (List.map ~f:ast_to_with_constraint) x2 in
    Pmty_with (x1, x2)
  | Pmty_typeof (x1) ->
    let x1 = ast_to_module_expr x1 in
    Pmty_typeof (x1)
  | Pmty_extension (x1) ->
    let x1 = ast_to_extension x1 in
    Pmty_extension (x1)
  | Pmty_alias (x1) ->
    let x1 = ast_to_longident_loc x1 in
    Pmty_alias (x1)

and ast_of_signature x =
  Versions.V4_07.Signature.of_concrete (concrete_of_signature x)

and concrete_of_signature x =
  (List.map ~f:ast_of_signature_item) x

and ast_to_signature x =
  let concrete = Versions.V4_07.Signature.to_concrete x in
  concrete_to_signature concrete

and concrete_to_signature x =
  (List.map ~f:ast_to_signature_item) x

and ast_of_signature_item x =
  Versions.V4_07.Signature_item.of_concrete (concrete_of_signature_item x)

and concrete_of_signature_item
  ({ psig_desc; psig_loc } : Compiler_types.signature_item)
=
  let psig_desc = ast_of_signature_item_desc psig_desc in
  ({ psig_desc; psig_loc } : Versions.V4_07.Signature_item.concrete)

and ast_to_signature_item x =
  let concrete = Versions.V4_07.Signature_item.to_concrete x in
  concrete_to_signature_item concrete

and concrete_to_signature_item
  ({ psig_desc; psig_loc } : Versions.V4_07.Signature_item.concrete)
=
  let psig_desc = ast_to_signature_item_desc psig_desc in
  ({ psig_desc; psig_loc } : Compiler_types.signature_item)

and ast_of_signature_item_desc x =
  Versions.V4_07.Signature_item_desc.of_concrete (concrete_of_signature_item_desc x)

and concrete_of_signature_item_desc x : Versions.V4_07.Signature_item_desc.concrete =
  match (x : Compiler_types.signature_item_desc) with
  | Psig_value (x1) ->
    let x1 = ast_of_value_description x1 in
    Psig_value (x1)
  | Psig_type (x1, x2) ->
    let x1 = ast_of_rec_flag x1 in
    let x2 = (List.map ~f:ast_of_type_declaration) x2 in
    Psig_type (x1, x2)
  | Psig_typext (x1) ->
    let x1 = ast_of_type_extension x1 in
    Psig_typext (x1)
  | Psig_exception (x1) ->
    let x1 = ast_of_extension_constructor x1 in
    Psig_exception (x1)
  | Psig_module (x1) ->
    let x1 = ast_of_module_declaration x1 in
    Psig_module (x1)
  | Psig_recmodule (x1) ->
    let x1 = (List.map ~f:ast_of_module_declaration) x1 in
    Psig_recmodule (x1)
  | Psig_modtype (x1) ->
    let x1 = ast_of_module_type_declaration x1 in
    Psig_modtype (x1)
  | Psig_open (x1) ->
    let x1 = ast_of_open_description x1 in
    Psig_open (x1)
  | Psig_include (x1) ->
    let x1 = ast_of_include_description x1 in
    Psig_include (x1)
  | Psig_class (x1) ->
    let x1 = (List.map ~f:ast_of_class_description) x1 in
    Psig_class (x1)
  | Psig_class_type (x1) ->
    let x1 = (List.map ~f:ast_of_class_type_declaration) x1 in
    Psig_class_type (x1)
  | Psig_attribute (x1) ->
    let x1 = ast_of_attribute x1 in
    Psig_attribute (x1)
  | Psig_extension (x1, x2) ->
    let x1 = ast_of_extension x1 in
    let x2 = ast_of_attributes x2 in
    Psig_extension (x1, x2)

and ast_to_signature_item_desc x =
  let concrete = Versions.V4_07.Signature_item_desc.to_concrete x in
  concrete_to_signature_item_desc concrete

and concrete_to_signature_item_desc x : Compiler_types.signature_item_desc =
  match (x : Versions.V4_07.Signature_item_desc.concrete) with
  | Psig_value (x1) ->
    let x1 = ast_to_value_description x1 in
    Psig_value (x1)
  | Psig_type (x1, x2) ->
    let x1 = ast_to_rec_flag x1 in
    let x2 = (List.map ~f:ast_to_type_declaration) x2 in
    Psig_type (x1, x2)
  | Psig_typext (x1) ->
    let x1 = ast_to_type_extension x1 in
    Psig_typext (x1)
  | Psig_exception (x1) ->
    let x1 = ast_to_extension_constructor x1 in
    Psig_exception (x1)
  | Psig_module (x1) ->
    let x1 = ast_to_module_declaration x1 in
    Psig_module (x1)
  | Psig_recmodule (x1) ->
    let x1 = (List.map ~f:ast_to_module_declaration) x1 in
    Psig_recmodule (x1)
  | Psig_modtype (x1) ->
    let x1 = ast_to_module_type_declaration x1 in
    Psig_modtype (x1)
  | Psig_open (x1) ->
    let x1 = ast_to_open_description x1 in
    Psig_open (x1)
  | Psig_include (x1) ->
    let x1 = ast_to_include_description x1 in
    Psig_include (x1)
  | Psig_class (x1) ->
    let x1 = (List.map ~f:ast_to_class_description) x1 in
    Psig_class (x1)
  | Psig_class_type (x1) ->
    let x1 = (List.map ~f:ast_to_class_type_declaration) x1 in
    Psig_class_type (x1)
  | Psig_attribute (x1) ->
    let x1 = ast_to_attribute x1 in
    Psig_attribute (x1)
  | Psig_extension (x1, x2) ->
    let x1 = ast_to_extension x1 in
    let x2 = ast_to_attributes x2 in
    Psig_extension (x1, x2)

and ast_of_module_declaration x =
  Versions.V4_07.Module_declaration.of_concrete (concrete_of_module_declaration x)

and concrete_of_module_declaration
  ({ pmd_name; pmd_type; pmd_attributes; pmd_loc } : Compiler_types.module_declaration)
=
  let pmd_type = ast_of_module_type pmd_type in
  let pmd_attributes = ast_of_attributes pmd_attributes in
  ({ pmd_name; pmd_type; pmd_attributes; pmd_loc } : Versions.V4_07.Module_declaration.concrete)

and ast_to_module_declaration x =
  let concrete = Versions.V4_07.Module_declaration.to_concrete x in
  concrete_to_module_declaration concrete

and concrete_to_module_declaration
  ({ pmd_name; pmd_type; pmd_attributes; pmd_loc } : Versions.V4_07.Module_declaration.concrete)
=
  let pmd_type = ast_to_module_type pmd_type in
  let pmd_attributes = ast_to_attributes pmd_attributes in
  ({ pmd_name; pmd_type; pmd_attributes; pmd_loc } : Compiler_types.module_declaration)

and ast_of_module_type_declaration x =
  Versions.V4_07.Module_type_declaration.of_concrete (concrete_of_module_type_declaration x)

and concrete_of_module_type_declaration
  ({ pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Compiler_types.module_type_declaration)
=
  let pmtd_type = (Option.map ~f:ast_of_module_type) pmtd_type in
  let pmtd_attributes = ast_of_attributes pmtd_attributes in
  ({ pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Versions.V4_07.Module_type_declaration.concrete)

and ast_to_module_type_declaration x =
  let concrete = Versions.V4_07.Module_type_declaration.to_concrete x in
  concrete_to_module_type_declaration concrete

and concrete_to_module_type_declaration
  ({ pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Versions.V4_07.Module_type_declaration.concrete)
=
  let pmtd_type = (Option.map ~f:ast_to_module_type) pmtd_type in
  let pmtd_attributes = ast_to_attributes pmtd_attributes in
  ({ pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Compiler_types.module_type_declaration)

and ast_of_open_description x =
  Versions.V4_07.Open_description.of_concrete (concrete_of_open_description x)

and concrete_of_open_description
  ({ popen_lid; popen_override; popen_loc; popen_attributes } : Compiler_types.open_description)
=
  let popen_lid = ast_of_longident_loc popen_lid in
  let popen_override = ast_of_override_flag popen_override in
  let popen_attributes = ast_of_attributes popen_attributes in
  ({ popen_lid; popen_override; popen_loc; popen_attributes } : Versions.V4_07.Open_description.concrete)

and ast_to_open_description x =
  let concrete = Versions.V4_07.Open_description.to_concrete x in
  concrete_to_open_description concrete

and concrete_to_open_description
  ({ popen_lid; popen_override; popen_loc; popen_attributes } : Versions.V4_07.Open_description.concrete)
=
  let popen_lid = ast_to_longident_loc popen_lid in
  let popen_override = ast_to_override_flag popen_override in
  let popen_attributes = ast_to_attributes popen_attributes in
  ({ popen_lid; popen_override; popen_loc; popen_attributes } : Compiler_types.open_description)

and ast_of_include_infos_module_expr x =
  Versions.V4_07.Include_infos.of_concrete (concrete_of_include_infos_module_expr x)

and concrete_of_include_infos_module_expr
  ({ pincl_mod; pincl_loc; pincl_attributes } : Compiler_types.module_expr Compiler_types.include_infos)
=
  let pincl_mod = ast_of_module_expr pincl_mod in
  let pincl_attributes = ast_of_attributes pincl_attributes in
  ({ pincl_mod; pincl_loc; pincl_attributes } : Versions.V4_07.Module_expr.t Versions.V4_07.Include_infos.concrete)

and ast_to_include_infos_module_expr x =
  let concrete = Versions.V4_07.Include_infos.to_concrete x in
  concrete_to_include_infos_module_expr concrete

and concrete_to_include_infos_module_expr
  ({ pincl_mod; pincl_loc; pincl_attributes } : Versions.V4_07.Module_expr.t Versions.V4_07.Include_infos.concrete)
=
  let pincl_mod = ast_to_module_expr pincl_mod in
  let pincl_attributes = ast_to_attributes pincl_attributes in
  ({ pincl_mod; pincl_loc; pincl_attributes } : Compiler_types.module_expr Compiler_types.include_infos)

and ast_of_include_infos_module_type x =
  Versions.V4_07.Include_infos.of_concrete (concrete_of_include_infos_module_type x)

and concrete_of_include_infos_module_type
  ({ pincl_mod; pincl_loc; pincl_attributes } : Compiler_types.module_type Compiler_types.include_infos)
=
  let pincl_mod = ast_of_module_type pincl_mod in
  let pincl_attributes = ast_of_attributes pincl_attributes in
  ({ pincl_mod; pincl_loc; pincl_attributes } : Versions.V4_07.Module_type.t Versions.V4_07.Include_infos.concrete)

and ast_to_include_infos_module_type x =
  let concrete = Versions.V4_07.Include_infos.to_concrete x in
  concrete_to_include_infos_module_type concrete

and concrete_to_include_infos_module_type
  ({ pincl_mod; pincl_loc; pincl_attributes } : Versions.V4_07.Module_type.t Versions.V4_07.Include_infos.concrete)
=
  let pincl_mod = ast_to_module_type pincl_mod in
  let pincl_attributes = ast_to_attributes pincl_attributes in
  ({ pincl_mod; pincl_loc; pincl_attributes } : Compiler_types.module_type Compiler_types.include_infos)

and ast_of_include_description x =
  Versions.V4_07.Include_description.of_concrete (concrete_of_include_description x)

and concrete_of_include_description x =
  ast_of_include_infos_module_type x

and ast_to_include_description x =
  let concrete = Versions.V4_07.Include_description.to_concrete x in
  concrete_to_include_description concrete

and concrete_to_include_description x =
  ast_to_include_infos_module_type x

and ast_of_include_declaration x =
  Versions.V4_07.Include_declaration.of_concrete (concrete_of_include_declaration x)

and concrete_of_include_declaration x =
  ast_of_include_infos_module_expr x

and ast_to_include_declaration x =
  let concrete = Versions.V4_07.Include_declaration.to_concrete x in
  concrete_to_include_declaration concrete

and concrete_to_include_declaration x =
  ast_to_include_infos_module_expr x

and ast_of_with_constraint x =
  Versions.V4_07.With_constraint.of_concrete (concrete_of_with_constraint x)

and concrete_of_with_constraint x : Versions.V4_07.With_constraint.concrete =
  match (x : Compiler_types.with_constraint) with
  | Pwith_type (x1, x2) ->
    let x1 = ast_of_longident_loc x1 in
    let x2 = ast_of_type_declaration x2 in
    Pwith_type (x1, x2)
  | Pwith_module (x1, x2) ->
    let x1 = ast_of_longident_loc x1 in
    let x2 = ast_of_longident_loc x2 in
    Pwith_module (x1, x2)
  | Pwith_typesubst (x1, x2) ->
    let x1 = ast_of_longident_loc x1 in
    let x2 = ast_of_type_declaration x2 in
    Pwith_typesubst (x1, x2)
  | Pwith_modsubst (x1, x2) ->
    let x1 = ast_of_longident_loc x1 in
    let x2 = ast_of_longident_loc x2 in
    Pwith_modsubst (x1, x2)

and ast_to_with_constraint x =
  let concrete = Versions.V4_07.With_constraint.to_concrete x in
  concrete_to_with_constraint concrete

and concrete_to_with_constraint x : Compiler_types.with_constraint =
  match (x : Versions.V4_07.With_constraint.concrete) with
  | Pwith_type (x1, x2) ->
    let x1 = ast_to_longident_loc x1 in
    let x2 = ast_to_type_declaration x2 in
    Pwith_type (x1, x2)
  | Pwith_module (x1, x2) ->
    let x1 = ast_to_longident_loc x1 in
    let x2 = ast_to_longident_loc x2 in
    Pwith_module (x1, x2)
  | Pwith_typesubst (x1, x2) ->
    let x1 = ast_to_longident_loc x1 in
    let x2 = ast_to_type_declaration x2 in
    Pwith_typesubst (x1, x2)
  | Pwith_modsubst (x1, x2) ->
    let x1 = ast_to_longident_loc x1 in
    let x2 = ast_to_longident_loc x2 in
    Pwith_modsubst (x1, x2)

and ast_of_module_expr x =
  Versions.V4_07.Module_expr.of_concrete (concrete_of_module_expr x)

and concrete_of_module_expr
  ({ pmod_desc; pmod_loc; pmod_attributes } : Compiler_types.module_expr)
=
  let pmod_desc = ast_of_module_expr_desc pmod_desc in
  let pmod_attributes = ast_of_attributes pmod_attributes in
  ({ pmod_desc; pmod_loc; pmod_attributes } : Versions.V4_07.Module_expr.concrete)

and ast_to_module_expr x =
  let concrete = Versions.V4_07.Module_expr.to_concrete x in
  concrete_to_module_expr concrete

and concrete_to_module_expr
  ({ pmod_desc; pmod_loc; pmod_attributes } : Versions.V4_07.Module_expr.concrete)
=
  let pmod_desc = ast_to_module_expr_desc pmod_desc in
  let pmod_attributes = ast_to_attributes pmod_attributes in
  ({ pmod_desc; pmod_loc; pmod_attributes } : Compiler_types.module_expr)

and ast_of_module_expr_desc x =
  Versions.V4_07.Module_expr_desc.of_concrete (concrete_of_module_expr_desc x)

and concrete_of_module_expr_desc x : Versions.V4_07.Module_expr_desc.concrete =
  match (x : Compiler_types.module_expr_desc) with
  | Pmod_ident (x1) ->
    let x1 = ast_of_longident_loc x1 in
    Pmod_ident (x1)
  | Pmod_structure (x1) ->
    let x1 = ast_of_structure x1 in
    Pmod_structure (x1)
  | Pmod_functor (x1, x2, x3) ->
    let x2 = (Option.map ~f:ast_of_module_type) x2 in
    let x3 = ast_of_module_expr x3 in
    Pmod_functor (x1, x2, x3)
  | Pmod_apply (x1, x2) ->
    let x1 = ast_of_module_expr x1 in
    let x2 = ast_of_module_expr x2 in
    Pmod_apply (x1, x2)
  | Pmod_constraint (x1, x2) ->
    let x1 = ast_of_module_expr x1 in
    let x2 = ast_of_module_type x2 in
    Pmod_constraint (x1, x2)
  | Pmod_unpack (x1) ->
    let x1 = ast_of_expression x1 in
    Pmod_unpack (x1)
  | Pmod_extension (x1) ->
    let x1 = ast_of_extension x1 in
    Pmod_extension (x1)

and ast_to_module_expr_desc x =
  let concrete = Versions.V4_07.Module_expr_desc.to_concrete x in
  concrete_to_module_expr_desc concrete

and concrete_to_module_expr_desc x : Compiler_types.module_expr_desc =
  match (x : Versions.V4_07.Module_expr_desc.concrete) with
  | Pmod_ident (x1) ->
    let x1 = ast_to_longident_loc x1 in
    Pmod_ident (x1)
  | Pmod_structure (x1) ->
    let x1 = ast_to_structure x1 in
    Pmod_structure (x1)
  | Pmod_functor (x1, x2, x3) ->
    let x2 = (Option.map ~f:ast_to_module_type) x2 in
    let x3 = ast_to_module_expr x3 in
    Pmod_functor (x1, x2, x3)
  | Pmod_apply (x1, x2) ->
    let x1 = ast_to_module_expr x1 in
    let x2 = ast_to_module_expr x2 in
    Pmod_apply (x1, x2)
  | Pmod_constraint (x1, x2) ->
    let x1 = ast_to_module_expr x1 in
    let x2 = ast_to_module_type x2 in
    Pmod_constraint (x1, x2)
  | Pmod_unpack (x1) ->
    let x1 = ast_to_expression x1 in
    Pmod_unpack (x1)
  | Pmod_extension (x1) ->
    let x1 = ast_to_extension x1 in
    Pmod_extension (x1)

and ast_of_structure x =
  Versions.V4_07.Structure.of_concrete (concrete_of_structure x)

and concrete_of_structure x =
  (List.map ~f:ast_of_structure_item) x

and ast_to_structure x =
  let concrete = Versions.V4_07.Structure.to_concrete x in
  concrete_to_structure concrete

and concrete_to_structure x =
  (List.map ~f:ast_to_structure_item) x

and ast_of_structure_item x =
  Versions.V4_07.Structure_item.of_concrete (concrete_of_structure_item x)

and concrete_of_structure_item
  ({ pstr_desc; pstr_loc } : Compiler_types.structure_item)
=
  let pstr_desc = ast_of_structure_item_desc pstr_desc in
  ({ pstr_desc; pstr_loc } : Versions.V4_07.Structure_item.concrete)

and ast_to_structure_item x =
  let concrete = Versions.V4_07.Structure_item.to_concrete x in
  concrete_to_structure_item concrete

and concrete_to_structure_item
  ({ pstr_desc; pstr_loc } : Versions.V4_07.Structure_item.concrete)
=
  let pstr_desc = ast_to_structure_item_desc pstr_desc in
  ({ pstr_desc; pstr_loc } : Compiler_types.structure_item)

and ast_of_structure_item_desc x =
  Versions.V4_07.Structure_item_desc.of_concrete (concrete_of_structure_item_desc x)

and concrete_of_structure_item_desc x : Versions.V4_07.Structure_item_desc.concrete =
  match (x : Compiler_types.structure_item_desc) with
  | Pstr_eval (x1, x2) ->
    let x1 = ast_of_expression x1 in
    let x2 = ast_of_attributes x2 in
    Pstr_eval (x1, x2)
  | Pstr_value (x1, x2) ->
    let x1 = ast_of_rec_flag x1 in
    let x2 = (List.map ~f:ast_of_value_binding) x2 in
    Pstr_value (x1, x2)
  | Pstr_primitive (x1) ->
    let x1 = ast_of_value_description x1 in
    Pstr_primitive (x1)
  | Pstr_type (x1, x2) ->
    let x1 = ast_of_rec_flag x1 in
    let x2 = (List.map ~f:ast_of_type_declaration) x2 in
    Pstr_type (x1, x2)
  | Pstr_typext (x1) ->
    let x1 = ast_of_type_extension x1 in
    Pstr_typext (x1)
  | Pstr_exception (x1) ->
    let x1 = ast_of_extension_constructor x1 in
    Pstr_exception (x1)
  | Pstr_module (x1) ->
    let x1 = ast_of_module_binding x1 in
    Pstr_module (x1)
  | Pstr_recmodule (x1) ->
    let x1 = (List.map ~f:ast_of_module_binding) x1 in
    Pstr_recmodule (x1)
  | Pstr_modtype (x1) ->
    let x1 = ast_of_module_type_declaration x1 in
    Pstr_modtype (x1)
  | Pstr_open (x1) ->
    let x1 = ast_of_open_description x1 in
    Pstr_open (x1)
  | Pstr_class (x1) ->
    let x1 = (List.map ~f:ast_of_class_declaration) x1 in
    Pstr_class (x1)
  | Pstr_class_type (x1) ->
    let x1 = (List.map ~f:ast_of_class_type_declaration) x1 in
    Pstr_class_type (x1)
  | Pstr_include (x1) ->
    let x1 = ast_of_include_declaration x1 in
    Pstr_include (x1)
  | Pstr_attribute (x1) ->
    let x1 = ast_of_attribute x1 in
    Pstr_attribute (x1)
  | Pstr_extension (x1, x2) ->
    let x1 = ast_of_extension x1 in
    let x2 = ast_of_attributes x2 in
    Pstr_extension (x1, x2)

and ast_to_structure_item_desc x =
  let concrete = Versions.V4_07.Structure_item_desc.to_concrete x in
  concrete_to_structure_item_desc concrete

and concrete_to_structure_item_desc x : Compiler_types.structure_item_desc =
  match (x : Versions.V4_07.Structure_item_desc.concrete) with
  | Pstr_eval (x1, x2) ->
    let x1 = ast_to_expression x1 in
    let x2 = ast_to_attributes x2 in
    Pstr_eval (x1, x2)
  | Pstr_value (x1, x2) ->
    let x1 = ast_to_rec_flag x1 in
    let x2 = (List.map ~f:ast_to_value_binding) x2 in
    Pstr_value (x1, x2)
  | Pstr_primitive (x1) ->
    let x1 = ast_to_value_description x1 in
    Pstr_primitive (x1)
  | Pstr_type (x1, x2) ->
    let x1 = ast_to_rec_flag x1 in
    let x2 = (List.map ~f:ast_to_type_declaration) x2 in
    Pstr_type (x1, x2)
  | Pstr_typext (x1) ->
    let x1 = ast_to_type_extension x1 in
    Pstr_typext (x1)
  | Pstr_exception (x1) ->
    let x1 = ast_to_extension_constructor x1 in
    Pstr_exception (x1)
  | Pstr_module (x1) ->
    let x1 = ast_to_module_binding x1 in
    Pstr_module (x1)
  | Pstr_recmodule (x1) ->
    let x1 = (List.map ~f:ast_to_module_binding) x1 in
    Pstr_recmodule (x1)
  | Pstr_modtype (x1) ->
    let x1 = ast_to_module_type_declaration x1 in
    Pstr_modtype (x1)
  | Pstr_open (x1) ->
    let x1 = ast_to_open_description x1 in
    Pstr_open (x1)
  | Pstr_class (x1) ->
    let x1 = (List.map ~f:ast_to_class_declaration) x1 in
    Pstr_class (x1)
  | Pstr_class_type (x1) ->
    let x1 = (List.map ~f:ast_to_class_type_declaration) x1 in
    Pstr_class_type (x1)
  | Pstr_include (x1) ->
    let x1 = ast_to_include_declaration x1 in
    Pstr_include (x1)
  | Pstr_attribute (x1) ->
    let x1 = ast_to_attribute x1 in
    Pstr_attribute (x1)
  | Pstr_extension (x1, x2) ->
    let x1 = ast_to_extension x1 in
    let x2 = ast_to_attributes x2 in
    Pstr_extension (x1, x2)

and ast_of_value_binding x =
  Versions.V4_07.Value_binding.of_concrete (concrete_of_value_binding x)

and concrete_of_value_binding
  ({ pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Compiler_types.value_binding)
=
  let pvb_pat = ast_of_pattern pvb_pat in
  let pvb_expr = ast_of_expression pvb_expr in
  let pvb_attributes = ast_of_attributes pvb_attributes in
  ({ pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Versions.V4_07.Value_binding.concrete)

and ast_to_value_binding x =
  let concrete = Versions.V4_07.Value_binding.to_concrete x in
  concrete_to_value_binding concrete

and concrete_to_value_binding
  ({ pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Versions.V4_07.Value_binding.concrete)
=
  let pvb_pat = ast_to_pattern pvb_pat in
  let pvb_expr = ast_to_expression pvb_expr in
  let pvb_attributes = ast_to_attributes pvb_attributes in
  ({ pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Compiler_types.value_binding)

and ast_of_module_binding x =
  Versions.V4_07.Module_binding.of_concrete (concrete_of_module_binding x)

and concrete_of_module_binding
  ({ pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Compiler_types.module_binding)
=
  let pmb_expr = ast_of_module_expr pmb_expr in
  let pmb_attributes = ast_of_attributes pmb_attributes in
  ({ pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Versions.V4_07.Module_binding.concrete)

and ast_to_module_binding x =
  let concrete = Versions.V4_07.Module_binding.to_concrete x in
  concrete_to_module_binding concrete

and concrete_to_module_binding
  ({ pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Versions.V4_07.Module_binding.concrete)
=
  let pmb_expr = ast_to_module_expr pmb_expr in
  let pmb_attributes = ast_to_attributes pmb_attributes in
  ({ pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Compiler_types.module_binding)

and ast_of_toplevel_phrase x =
  Versions.V4_07.Toplevel_phrase.of_concrete (concrete_of_toplevel_phrase x)

and concrete_of_toplevel_phrase x : Versions.V4_07.Toplevel_phrase.concrete =
  match (x : Compiler_types.toplevel_phrase) with
  | Ptop_def (x1) ->
    let x1 = ast_of_structure x1 in
    Ptop_def (x1)
  | Ptop_dir (x1, x2) ->
    let x2 = ast_of_directive_argument x2 in
    Ptop_dir (x1, x2)

and ast_to_toplevel_phrase x =
  let concrete = Versions.V4_07.Toplevel_phrase.to_concrete x in
  concrete_to_toplevel_phrase concrete

and concrete_to_toplevel_phrase x : Compiler_types.toplevel_phrase =
  match (x : Versions.V4_07.Toplevel_phrase.concrete) with
  | Ptop_def (x1) ->
    let x1 = ast_to_structure x1 in
    Ptop_def (x1)
  | Ptop_dir (x1, x2) ->
    let x2 = ast_to_directive_argument x2 in
    Ptop_dir (x1, x2)

and ast_of_directive_argument x =
  Versions.V4_07.Directive_argument.of_concrete (concrete_of_directive_argument x)

and concrete_of_directive_argument x : Versions.V4_07.Directive_argument.concrete =
  match (x : Compiler_types.directive_argument) with
  | Pdir_none -> Pdir_none
  | Pdir_string (x1) ->
    Pdir_string (x1)
  | Pdir_int (x1, x2) ->
    Pdir_int (x1, x2)
  | Pdir_ident (x1) ->
    let x1 = ast_of_longident x1 in
    Pdir_ident (x1)
  | Pdir_bool (x1) ->
    Pdir_bool (x1)

and ast_to_directive_argument x =
  let concrete = Versions.V4_07.Directive_argument.to_concrete x in
  concrete_to_directive_argument concrete

and concrete_to_directive_argument x : Compiler_types.directive_argument =
  match (x : Versions.V4_07.Directive_argument.concrete) with
  | Pdir_none -> Pdir_none
  | Pdir_string (x1) ->
    Pdir_string (x1)
  | Pdir_int (x1, x2) ->
    Pdir_int (x1, x2)
  | Pdir_ident (x1) ->
    let x1 = ast_to_longident x1 in
    Pdir_ident (x1)
  | Pdir_bool (x1) ->
    Pdir_bool (x1)
(*$*)
