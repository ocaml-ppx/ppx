open Stdppx

(*$ Ppx_ast_cinaps.print_conversion_ml () *)
let rec ast_of_longident
  : Compiler_types.longident -> Versions.V4_08.Longident.t
  = fun x ->
    Versions.V4_08.Longident.of_concrete (concrete_of_longident x)

and concrete_of_longident
  : Compiler_types.longident -> Versions.V4_08.Longident.concrete
  = fun x ->
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

and ast_to_longident
  : Versions.V4_08.Longident.t -> Compiler_types.longident
  = fun x ->
    let concrete = Versions.V4_08.Longident.to_concrete x in
    concrete_to_longident concrete

and concrete_to_longident
  : Versions.V4_08.Longident.concrete -> Compiler_types.longident
  = fun x ->
    match (x : Versions.V4_08.Longident.concrete) with
    | Lident (x1) ->
      Lident (x1)
    | Ldot (x1, x2) ->
      let x1 = ast_to_longident x1 in
      Ldot (x1, x2)
    | Lapply (x1, x2) ->
      let x1 = ast_to_longident x1 in
      let x2 = ast_to_longident x2 in
      Lapply (x1, x2)

and ast_of_longident_loc
  : Compiler_types.longident_loc -> Versions.V4_08.Longident_loc.t
  = fun x ->
    Versions.V4_08.Longident_loc.of_concrete (concrete_of_longident_loc x)

and concrete_of_longident_loc
  : Compiler_types.longident_loc -> Versions.V4_08.Longident_loc.concrete
  = fun x ->
    (Astlib.Loc.map ~f:ast_of_longident) x

and ast_to_longident_loc
  : Versions.V4_08.Longident_loc.t -> Compiler_types.longident_loc
  = fun x ->
    let concrete = Versions.V4_08.Longident_loc.to_concrete x in
    concrete_to_longident_loc concrete

and concrete_to_longident_loc
  : Versions.V4_08.Longident_loc.concrete -> Compiler_types.longident_loc
  = fun x ->
    (Astlib.Loc.map ~f:ast_to_longident) x

and ast_of_rec_flag
  : Compiler_types.rec_flag -> Versions.V4_08.Rec_flag.t
  = fun x ->
    Versions.V4_08.Rec_flag.of_concrete (concrete_of_rec_flag x)

and concrete_of_rec_flag
  : Compiler_types.rec_flag -> Versions.V4_08.Rec_flag.concrete
  = fun x ->
    match (x : Compiler_types.rec_flag) with
    | Nonrecursive -> Nonrecursive
    | Recursive -> Recursive

and ast_to_rec_flag
  : Versions.V4_08.Rec_flag.t -> Compiler_types.rec_flag
  = fun x ->
    let concrete = Versions.V4_08.Rec_flag.to_concrete x in
    concrete_to_rec_flag concrete

and concrete_to_rec_flag
  : Versions.V4_08.Rec_flag.concrete -> Compiler_types.rec_flag
  = fun x ->
    match (x : Versions.V4_08.Rec_flag.concrete) with
    | Nonrecursive -> Nonrecursive
    | Recursive -> Recursive

and ast_of_direction_flag
  : Compiler_types.direction_flag -> Versions.V4_08.Direction_flag.t
  = fun x ->
    Versions.V4_08.Direction_flag.of_concrete (concrete_of_direction_flag x)

and concrete_of_direction_flag
  : Compiler_types.direction_flag -> Versions.V4_08.Direction_flag.concrete
  = fun x ->
    match (x : Compiler_types.direction_flag) with
    | Upto -> Upto
    | Downto -> Downto

and ast_to_direction_flag
  : Versions.V4_08.Direction_flag.t -> Compiler_types.direction_flag
  = fun x ->
    let concrete = Versions.V4_08.Direction_flag.to_concrete x in
    concrete_to_direction_flag concrete

and concrete_to_direction_flag
  : Versions.V4_08.Direction_flag.concrete -> Compiler_types.direction_flag
  = fun x ->
    match (x : Versions.V4_08.Direction_flag.concrete) with
    | Upto -> Upto
    | Downto -> Downto

and ast_of_private_flag
  : Compiler_types.private_flag -> Versions.V4_08.Private_flag.t
  = fun x ->
    Versions.V4_08.Private_flag.of_concrete (concrete_of_private_flag x)

and concrete_of_private_flag
  : Compiler_types.private_flag -> Versions.V4_08.Private_flag.concrete
  = fun x ->
    match (x : Compiler_types.private_flag) with
    | Private -> Private
    | Public -> Public

and ast_to_private_flag
  : Versions.V4_08.Private_flag.t -> Compiler_types.private_flag
  = fun x ->
    let concrete = Versions.V4_08.Private_flag.to_concrete x in
    concrete_to_private_flag concrete

and concrete_to_private_flag
  : Versions.V4_08.Private_flag.concrete -> Compiler_types.private_flag
  = fun x ->
    match (x : Versions.V4_08.Private_flag.concrete) with
    | Private -> Private
    | Public -> Public

and ast_of_mutable_flag
  : Compiler_types.mutable_flag -> Versions.V4_08.Mutable_flag.t
  = fun x ->
    Versions.V4_08.Mutable_flag.of_concrete (concrete_of_mutable_flag x)

and concrete_of_mutable_flag
  : Compiler_types.mutable_flag -> Versions.V4_08.Mutable_flag.concrete
  = fun x ->
    match (x : Compiler_types.mutable_flag) with
    | Immutable -> Immutable
    | Mutable -> Mutable

and ast_to_mutable_flag
  : Versions.V4_08.Mutable_flag.t -> Compiler_types.mutable_flag
  = fun x ->
    let concrete = Versions.V4_08.Mutable_flag.to_concrete x in
    concrete_to_mutable_flag concrete

and concrete_to_mutable_flag
  : Versions.V4_08.Mutable_flag.concrete -> Compiler_types.mutable_flag
  = fun x ->
    match (x : Versions.V4_08.Mutable_flag.concrete) with
    | Immutable -> Immutable
    | Mutable -> Mutable

and ast_of_virtual_flag
  : Compiler_types.virtual_flag -> Versions.V4_08.Virtual_flag.t
  = fun x ->
    Versions.V4_08.Virtual_flag.of_concrete (concrete_of_virtual_flag x)

and concrete_of_virtual_flag
  : Compiler_types.virtual_flag -> Versions.V4_08.Virtual_flag.concrete
  = fun x ->
    match (x : Compiler_types.virtual_flag) with
    | Virtual -> Virtual
    | Concrete -> Concrete

and ast_to_virtual_flag
  : Versions.V4_08.Virtual_flag.t -> Compiler_types.virtual_flag
  = fun x ->
    let concrete = Versions.V4_08.Virtual_flag.to_concrete x in
    concrete_to_virtual_flag concrete

and concrete_to_virtual_flag
  : Versions.V4_08.Virtual_flag.concrete -> Compiler_types.virtual_flag
  = fun x ->
    match (x : Versions.V4_08.Virtual_flag.concrete) with
    | Virtual -> Virtual
    | Concrete -> Concrete

and ast_of_override_flag
  : Compiler_types.override_flag -> Versions.V4_08.Override_flag.t
  = fun x ->
    Versions.V4_08.Override_flag.of_concrete (concrete_of_override_flag x)

and concrete_of_override_flag
  : Compiler_types.override_flag -> Versions.V4_08.Override_flag.concrete
  = fun x ->
    match (x : Compiler_types.override_flag) with
    | Override -> Override
    | Fresh -> Fresh

and ast_to_override_flag
  : Versions.V4_08.Override_flag.t -> Compiler_types.override_flag
  = fun x ->
    let concrete = Versions.V4_08.Override_flag.to_concrete x in
    concrete_to_override_flag concrete

and concrete_to_override_flag
  : Versions.V4_08.Override_flag.concrete -> Compiler_types.override_flag
  = fun x ->
    match (x : Versions.V4_08.Override_flag.concrete) with
    | Override -> Override
    | Fresh -> Fresh

and ast_of_closed_flag
  : Compiler_types.closed_flag -> Versions.V4_08.Closed_flag.t
  = fun x ->
    Versions.V4_08.Closed_flag.of_concrete (concrete_of_closed_flag x)

and concrete_of_closed_flag
  : Compiler_types.closed_flag -> Versions.V4_08.Closed_flag.concrete
  = fun x ->
    match (x : Compiler_types.closed_flag) with
    | Closed -> Closed
    | Open -> Open

and ast_to_closed_flag
  : Versions.V4_08.Closed_flag.t -> Compiler_types.closed_flag
  = fun x ->
    let concrete = Versions.V4_08.Closed_flag.to_concrete x in
    concrete_to_closed_flag concrete

and concrete_to_closed_flag
  : Versions.V4_08.Closed_flag.concrete -> Compiler_types.closed_flag
  = fun x ->
    match (x : Versions.V4_08.Closed_flag.concrete) with
    | Closed -> Closed
    | Open -> Open

and ast_of_arg_label
  : Compiler_types.arg_label -> Versions.V4_08.Arg_label.t
  = fun x ->
    Versions.V4_08.Arg_label.of_concrete (concrete_of_arg_label x)

and concrete_of_arg_label
  : Compiler_types.arg_label -> Versions.V4_08.Arg_label.concrete
  = fun x ->
    match (x : Compiler_types.arg_label) with
    | Nolabel -> Nolabel
    | Labelled (x1) ->
      Labelled (x1)
    | Optional (x1) ->
      Optional (x1)

and ast_to_arg_label
  : Versions.V4_08.Arg_label.t -> Compiler_types.arg_label
  = fun x ->
    let concrete = Versions.V4_08.Arg_label.to_concrete x in
    concrete_to_arg_label concrete

and concrete_to_arg_label
  : Versions.V4_08.Arg_label.concrete -> Compiler_types.arg_label
  = fun x ->
    match (x : Versions.V4_08.Arg_label.concrete) with
    | Nolabel -> Nolabel
    | Labelled (x1) ->
      Labelled (x1)
    | Optional (x1) ->
      Optional (x1)

and ast_of_variance
  : Compiler_types.variance -> Versions.V4_08.Variance.t
  = fun x ->
    Versions.V4_08.Variance.of_concrete (concrete_of_variance x)

and concrete_of_variance
  : Compiler_types.variance -> Versions.V4_08.Variance.concrete
  = fun x ->
    match (x : Compiler_types.variance) with
    | Covariant -> Covariant
    | Contravariant -> Contravariant
    | Invariant -> Invariant

and ast_to_variance
  : Versions.V4_08.Variance.t -> Compiler_types.variance
  = fun x ->
    let concrete = Versions.V4_08.Variance.to_concrete x in
    concrete_to_variance concrete

and concrete_to_variance
  : Versions.V4_08.Variance.concrete -> Compiler_types.variance
  = fun x ->
    match (x : Versions.V4_08.Variance.concrete) with
    | Covariant -> Covariant
    | Contravariant -> Contravariant
    | Invariant -> Invariant

and ast_of_constant
  : Compiler_types.constant -> Versions.V4_08.Constant.t
  = fun x ->
    Versions.V4_08.Constant.of_concrete (concrete_of_constant x)

and concrete_of_constant
  : Compiler_types.constant -> Versions.V4_08.Constant.concrete
  = fun x ->
    match (x : Compiler_types.constant) with
    | Pconst_integer (x1, x2) ->
      Pconst_integer (x1, x2)
    | Pconst_char (x1) ->
      Pconst_char (x1)
    | Pconst_string (x1, x2) ->
      Pconst_string (x1, x2)
    | Pconst_float (x1, x2) ->
      Pconst_float (x1, x2)

and ast_to_constant
  : Versions.V4_08.Constant.t -> Compiler_types.constant
  = fun x ->
    let concrete = Versions.V4_08.Constant.to_concrete x in
    concrete_to_constant concrete

and concrete_to_constant
  : Versions.V4_08.Constant.concrete -> Compiler_types.constant
  = fun x ->
    match (x : Versions.V4_08.Constant.concrete) with
    | Pconst_integer (x1, x2) ->
      Pconst_integer (x1, x2)
    | Pconst_char (x1) ->
      Pconst_char (x1)
    | Pconst_string (x1, x2) ->
      Pconst_string (x1, x2)
    | Pconst_float (x1, x2) ->
      Pconst_float (x1, x2)

and ast_of_attribute
  : Compiler_types.attribute -> Versions.V4_08.Attribute.t
  = fun x ->
    Versions.V4_08.Attribute.of_concrete (concrete_of_attribute x)

and concrete_of_attribute
  : Compiler_types.attribute -> Versions.V4_08.Attribute.concrete
  = fun { attr_name; attr_payload; attr_loc } ->
      let attr_payload = ast_of_payload attr_payload in
      { attr_name; attr_payload; attr_loc }

and ast_to_attribute
  : Versions.V4_08.Attribute.t -> Compiler_types.attribute
  = fun x ->
    let concrete = Versions.V4_08.Attribute.to_concrete x in
    concrete_to_attribute concrete

and concrete_to_attribute
  : Versions.V4_08.Attribute.concrete -> Compiler_types.attribute
  = fun { attr_name; attr_payload; attr_loc } ->
      let attr_payload = ast_to_payload attr_payload in
      { attr_name; attr_payload; attr_loc }

and ast_of_extension
  : Compiler_types.extension -> Versions.V4_08.Extension.t
  = fun x ->
    Versions.V4_08.Extension.of_concrete (concrete_of_extension x)

and concrete_of_extension
  : Compiler_types.extension -> Versions.V4_08.Extension.concrete
  = fun x ->
    (Tuple.map2 ~f1:Fn.id ~f2:ast_of_payload) x

and ast_to_extension
  : Versions.V4_08.Extension.t -> Compiler_types.extension
  = fun x ->
    let concrete = Versions.V4_08.Extension.to_concrete x in
    concrete_to_extension concrete

and concrete_to_extension
  : Versions.V4_08.Extension.concrete -> Compiler_types.extension
  = fun x ->
    (Tuple.map2 ~f1:Fn.id ~f2:ast_to_payload) x

and ast_of_attributes
  : Compiler_types.attributes -> Versions.V4_08.Attributes.t
  = fun x ->
    Versions.V4_08.Attributes.of_concrete (concrete_of_attributes x)

and concrete_of_attributes
  : Compiler_types.attributes -> Versions.V4_08.Attributes.concrete
  = fun x ->
    (List.map ~f:ast_of_attribute) x

and ast_to_attributes
  : Versions.V4_08.Attributes.t -> Compiler_types.attributes
  = fun x ->
    let concrete = Versions.V4_08.Attributes.to_concrete x in
    concrete_to_attributes concrete

and concrete_to_attributes
  : Versions.V4_08.Attributes.concrete -> Compiler_types.attributes
  = fun x ->
    (List.map ~f:ast_to_attribute) x

and ast_of_payload
  : Compiler_types.payload -> Versions.V4_08.Payload.t
  = fun x ->
    Versions.V4_08.Payload.of_concrete (concrete_of_payload x)

and concrete_of_payload
  : Compiler_types.payload -> Versions.V4_08.Payload.concrete
  = fun x ->
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

and ast_to_payload
  : Versions.V4_08.Payload.t -> Compiler_types.payload
  = fun x ->
    let concrete = Versions.V4_08.Payload.to_concrete x in
    concrete_to_payload concrete

and concrete_to_payload
  : Versions.V4_08.Payload.concrete -> Compiler_types.payload
  = fun x ->
    match (x : Versions.V4_08.Payload.concrete) with
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

and ast_of_core_type
  : Compiler_types.core_type -> Versions.V4_08.Core_type.t
  = fun x ->
    Versions.V4_08.Core_type.of_concrete (concrete_of_core_type x)

and concrete_of_core_type
  : Compiler_types.core_type -> Versions.V4_08.Core_type.concrete
  = fun { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes } ->
      let ptyp_desc = ast_of_core_type_desc ptyp_desc in
      let ptyp_attributes = ast_of_attributes ptyp_attributes in
      { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes }

and ast_to_core_type
  : Versions.V4_08.Core_type.t -> Compiler_types.core_type
  = fun x ->
    let concrete = Versions.V4_08.Core_type.to_concrete x in
    concrete_to_core_type concrete

and concrete_to_core_type
  : Versions.V4_08.Core_type.concrete -> Compiler_types.core_type
  = fun { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes } ->
      let ptyp_desc = ast_to_core_type_desc ptyp_desc in
      let ptyp_attributes = ast_to_attributes ptyp_attributes in
      { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes }

and ast_of_core_type_desc
  : Compiler_types.core_type_desc -> Versions.V4_08.Core_type_desc.t
  = fun x ->
    Versions.V4_08.Core_type_desc.of_concrete (concrete_of_core_type_desc x)

and concrete_of_core_type_desc
  : Compiler_types.core_type_desc -> Versions.V4_08.Core_type_desc.concrete
  = fun x ->
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

and ast_to_core_type_desc
  : Versions.V4_08.Core_type_desc.t -> Compiler_types.core_type_desc
  = fun x ->
    let concrete = Versions.V4_08.Core_type_desc.to_concrete x in
    concrete_to_core_type_desc concrete

and concrete_to_core_type_desc
  : Versions.V4_08.Core_type_desc.concrete -> Compiler_types.core_type_desc
  = fun x ->
    match (x : Versions.V4_08.Core_type_desc.concrete) with
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

and ast_of_package_type
  : Compiler_types.package_type -> Versions.V4_08.Package_type.t
  = fun x ->
    Versions.V4_08.Package_type.of_concrete (concrete_of_package_type x)

and concrete_of_package_type
  : Compiler_types.package_type -> Versions.V4_08.Package_type.concrete
  = fun x ->
    (Tuple.map2 ~f1:ast_of_longident_loc ~f2:(List.map ~f:(Tuple.map2 ~f1:ast_of_longident_loc ~f2:ast_of_core_type))) x

and ast_to_package_type
  : Versions.V4_08.Package_type.t -> Compiler_types.package_type
  = fun x ->
    let concrete = Versions.V4_08.Package_type.to_concrete x in
    concrete_to_package_type concrete

and concrete_to_package_type
  : Versions.V4_08.Package_type.concrete -> Compiler_types.package_type
  = fun x ->
    (Tuple.map2 ~f1:ast_to_longident_loc ~f2:(List.map ~f:(Tuple.map2 ~f1:ast_to_longident_loc ~f2:ast_to_core_type))) x

and ast_of_row_field
  : Compiler_types.row_field -> Versions.V4_08.Row_field.t
  = fun x ->
    Versions.V4_08.Row_field.of_concrete (concrete_of_row_field x)

and concrete_of_row_field
  : Compiler_types.row_field -> Versions.V4_08.Row_field.concrete
  = fun { prf_desc; prf_loc; prf_attributes } ->
      let prf_desc = ast_of_row_field_desc prf_desc in
      let prf_attributes = ast_of_attributes prf_attributes in
      { prf_desc; prf_loc; prf_attributes }

and ast_to_row_field
  : Versions.V4_08.Row_field.t -> Compiler_types.row_field
  = fun x ->
    let concrete = Versions.V4_08.Row_field.to_concrete x in
    concrete_to_row_field concrete

and concrete_to_row_field
  : Versions.V4_08.Row_field.concrete -> Compiler_types.row_field
  = fun { prf_desc; prf_loc; prf_attributes } ->
      let prf_desc = ast_to_row_field_desc prf_desc in
      let prf_attributes = ast_to_attributes prf_attributes in
      { prf_desc; prf_loc; prf_attributes }

and ast_of_row_field_desc
  : Compiler_types.row_field_desc -> Versions.V4_08.Row_field_desc.t
  = fun x ->
    Versions.V4_08.Row_field_desc.of_concrete (concrete_of_row_field_desc x)

and concrete_of_row_field_desc
  : Compiler_types.row_field_desc -> Versions.V4_08.Row_field_desc.concrete
  = fun x ->
    match (x : Compiler_types.row_field_desc) with
    | Rtag (x1, x2, x3) ->
      let x3 = (List.map ~f:ast_of_core_type) x3 in
      Rtag (x1, x2, x3)
    | Rinherit (x1) ->
      let x1 = ast_of_core_type x1 in
      Rinherit (x1)

and ast_to_row_field_desc
  : Versions.V4_08.Row_field_desc.t -> Compiler_types.row_field_desc
  = fun x ->
    let concrete = Versions.V4_08.Row_field_desc.to_concrete x in
    concrete_to_row_field_desc concrete

and concrete_to_row_field_desc
  : Versions.V4_08.Row_field_desc.concrete -> Compiler_types.row_field_desc
  = fun x ->
    match (x : Versions.V4_08.Row_field_desc.concrete) with
    | Rtag (x1, x2, x3) ->
      let x3 = (List.map ~f:ast_to_core_type) x3 in
      Rtag (x1, x2, x3)
    | Rinherit (x1) ->
      let x1 = ast_to_core_type x1 in
      Rinherit (x1)

and ast_of_object_field
  : Compiler_types.object_field -> Versions.V4_08.Object_field.t
  = fun x ->
    Versions.V4_08.Object_field.of_concrete (concrete_of_object_field x)

and concrete_of_object_field
  : Compiler_types.object_field -> Versions.V4_08.Object_field.concrete
  = fun { pof_desc; pof_loc; pof_attributes } ->
      let pof_desc = ast_of_object_field_desc pof_desc in
      let pof_attributes = ast_of_attributes pof_attributes in
      { pof_desc; pof_loc; pof_attributes }

and ast_to_object_field
  : Versions.V4_08.Object_field.t -> Compiler_types.object_field
  = fun x ->
    let concrete = Versions.V4_08.Object_field.to_concrete x in
    concrete_to_object_field concrete

and concrete_to_object_field
  : Versions.V4_08.Object_field.concrete -> Compiler_types.object_field
  = fun { pof_desc; pof_loc; pof_attributes } ->
      let pof_desc = ast_to_object_field_desc pof_desc in
      let pof_attributes = ast_to_attributes pof_attributes in
      { pof_desc; pof_loc; pof_attributes }

and ast_of_object_field_desc
  : Compiler_types.object_field_desc -> Versions.V4_08.Object_field_desc.t
  = fun x ->
    Versions.V4_08.Object_field_desc.of_concrete (concrete_of_object_field_desc x)

and concrete_of_object_field_desc
  : Compiler_types.object_field_desc -> Versions.V4_08.Object_field_desc.concrete
  = fun x ->
    match (x : Compiler_types.object_field_desc) with
    | Otag (x1, x2) ->
      let x2 = ast_of_core_type x2 in
      Otag (x1, x2)
    | Oinherit (x1) ->
      let x1 = ast_of_core_type x1 in
      Oinherit (x1)

and ast_to_object_field_desc
  : Versions.V4_08.Object_field_desc.t -> Compiler_types.object_field_desc
  = fun x ->
    let concrete = Versions.V4_08.Object_field_desc.to_concrete x in
    concrete_to_object_field_desc concrete

and concrete_to_object_field_desc
  : Versions.V4_08.Object_field_desc.concrete -> Compiler_types.object_field_desc
  = fun x ->
    match (x : Versions.V4_08.Object_field_desc.concrete) with
    | Otag (x1, x2) ->
      let x2 = ast_to_core_type x2 in
      Otag (x1, x2)
    | Oinherit (x1) ->
      let x1 = ast_to_core_type x1 in
      Oinherit (x1)

and ast_of_pattern
  : Compiler_types.pattern -> Versions.V4_08.Pattern.t
  = fun x ->
    Versions.V4_08.Pattern.of_concrete (concrete_of_pattern x)

and concrete_of_pattern
  : Compiler_types.pattern -> Versions.V4_08.Pattern.concrete
  = fun { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes } ->
      let ppat_desc = ast_of_pattern_desc ppat_desc in
      let ppat_attributes = ast_of_attributes ppat_attributes in
      { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes }

and ast_to_pattern
  : Versions.V4_08.Pattern.t -> Compiler_types.pattern
  = fun x ->
    let concrete = Versions.V4_08.Pattern.to_concrete x in
    concrete_to_pattern concrete

and concrete_to_pattern
  : Versions.V4_08.Pattern.concrete -> Compiler_types.pattern
  = fun { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes } ->
      let ppat_desc = ast_to_pattern_desc ppat_desc in
      let ppat_attributes = ast_to_attributes ppat_attributes in
      { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes }

and ast_of_pattern_desc
  : Compiler_types.pattern_desc -> Versions.V4_08.Pattern_desc.t
  = fun x ->
    Versions.V4_08.Pattern_desc.of_concrete (concrete_of_pattern_desc x)

and concrete_of_pattern_desc
  : Compiler_types.pattern_desc -> Versions.V4_08.Pattern_desc.concrete
  = fun x ->
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

and ast_to_pattern_desc
  : Versions.V4_08.Pattern_desc.t -> Compiler_types.pattern_desc
  = fun x ->
    let concrete = Versions.V4_08.Pattern_desc.to_concrete x in
    concrete_to_pattern_desc concrete

and concrete_to_pattern_desc
  : Versions.V4_08.Pattern_desc.concrete -> Compiler_types.pattern_desc
  = fun x ->
    match (x : Versions.V4_08.Pattern_desc.concrete) with
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

and ast_of_expression
  : Compiler_types.expression -> Versions.V4_08.Expression.t
  = fun x ->
    Versions.V4_08.Expression.of_concrete (concrete_of_expression x)

and concrete_of_expression
  : Compiler_types.expression -> Versions.V4_08.Expression.concrete
  = fun { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } ->
      let pexp_desc = ast_of_expression_desc pexp_desc in
      let pexp_attributes = ast_of_attributes pexp_attributes in
      { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes }

and ast_to_expression
  : Versions.V4_08.Expression.t -> Compiler_types.expression
  = fun x ->
    let concrete = Versions.V4_08.Expression.to_concrete x in
    concrete_to_expression concrete

and concrete_to_expression
  : Versions.V4_08.Expression.concrete -> Compiler_types.expression
  = fun { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } ->
      let pexp_desc = ast_to_expression_desc pexp_desc in
      let pexp_attributes = ast_to_attributes pexp_attributes in
      { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes }

and ast_of_expression_desc
  : Compiler_types.expression_desc -> Versions.V4_08.Expression_desc.t
  = fun x ->
    Versions.V4_08.Expression_desc.of_concrete (concrete_of_expression_desc x)

and concrete_of_expression_desc
  : Compiler_types.expression_desc -> Versions.V4_08.Expression_desc.concrete
  = fun x ->
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
    | Pexp_open (x1, x2) ->
      let x1 = ast_of_open_declaration x1 in
      let x2 = ast_of_expression x2 in
      Pexp_open (x1, x2)
    | Pexp_letop (x1) ->
      let x1 = ast_of_letop x1 in
      Pexp_letop (x1)
    | Pexp_extension (x1) ->
      let x1 = ast_of_extension x1 in
      Pexp_extension (x1)
    | Pexp_unreachable -> Pexp_unreachable

and ast_to_expression_desc
  : Versions.V4_08.Expression_desc.t -> Compiler_types.expression_desc
  = fun x ->
    let concrete = Versions.V4_08.Expression_desc.to_concrete x in
    concrete_to_expression_desc concrete

and concrete_to_expression_desc
  : Versions.V4_08.Expression_desc.concrete -> Compiler_types.expression_desc
  = fun x ->
    match (x : Versions.V4_08.Expression_desc.concrete) with
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
    | Pexp_open (x1, x2) ->
      let x1 = ast_to_open_declaration x1 in
      let x2 = ast_to_expression x2 in
      Pexp_open (x1, x2)
    | Pexp_letop (x1) ->
      let x1 = ast_to_letop x1 in
      Pexp_letop (x1)
    | Pexp_extension (x1) ->
      let x1 = ast_to_extension x1 in
      Pexp_extension (x1)
    | Pexp_unreachable -> Pexp_unreachable

and ast_of_case
  : Compiler_types.case -> Versions.V4_08.Case.t
  = fun x ->
    Versions.V4_08.Case.of_concrete (concrete_of_case x)

and concrete_of_case
  : Compiler_types.case -> Versions.V4_08.Case.concrete
  = fun { pc_lhs; pc_guard; pc_rhs } ->
      let pc_lhs = ast_of_pattern pc_lhs in
      let pc_guard = (Option.map ~f:ast_of_expression) pc_guard in
      let pc_rhs = ast_of_expression pc_rhs in
      { pc_lhs; pc_guard; pc_rhs }

and ast_to_case
  : Versions.V4_08.Case.t -> Compiler_types.case
  = fun x ->
    let concrete = Versions.V4_08.Case.to_concrete x in
    concrete_to_case concrete

and concrete_to_case
  : Versions.V4_08.Case.concrete -> Compiler_types.case
  = fun { pc_lhs; pc_guard; pc_rhs } ->
      let pc_lhs = ast_to_pattern pc_lhs in
      let pc_guard = (Option.map ~f:ast_to_expression) pc_guard in
      let pc_rhs = ast_to_expression pc_rhs in
      { pc_lhs; pc_guard; pc_rhs }

and ast_of_letop
  : Compiler_types.letop -> Versions.V4_08.Letop.t
  = fun x ->
    Versions.V4_08.Letop.of_concrete (concrete_of_letop x)

and concrete_of_letop
  : Compiler_types.letop -> Versions.V4_08.Letop.concrete
  = fun { let_; ands; body } ->
      let let_ = ast_of_binding_op let_ in
      let ands = (List.map ~f:ast_of_binding_op) ands in
      let body = ast_of_expression body in
      { let_; ands; body }

and ast_to_letop
  : Versions.V4_08.Letop.t -> Compiler_types.letop
  = fun x ->
    let concrete = Versions.V4_08.Letop.to_concrete x in
    concrete_to_letop concrete

and concrete_to_letop
  : Versions.V4_08.Letop.concrete -> Compiler_types.letop
  = fun { let_; ands; body } ->
      let let_ = ast_to_binding_op let_ in
      let ands = (List.map ~f:ast_to_binding_op) ands in
      let body = ast_to_expression body in
      { let_; ands; body }

and ast_of_binding_op
  : Compiler_types.binding_op -> Versions.V4_08.Binding_op.t
  = fun x ->
    Versions.V4_08.Binding_op.of_concrete (concrete_of_binding_op x)

and concrete_of_binding_op
  : Compiler_types.binding_op -> Versions.V4_08.Binding_op.concrete
  = fun { pbop_op; pbop_pat; pbop_exp; pbop_loc } ->
      let pbop_pat = ast_of_pattern pbop_pat in
      let pbop_exp = ast_of_expression pbop_exp in
      { pbop_op; pbop_pat; pbop_exp; pbop_loc }

and ast_to_binding_op
  : Versions.V4_08.Binding_op.t -> Compiler_types.binding_op
  = fun x ->
    let concrete = Versions.V4_08.Binding_op.to_concrete x in
    concrete_to_binding_op concrete

and concrete_to_binding_op
  : Versions.V4_08.Binding_op.concrete -> Compiler_types.binding_op
  = fun { pbop_op; pbop_pat; pbop_exp; pbop_loc } ->
      let pbop_pat = ast_to_pattern pbop_pat in
      let pbop_exp = ast_to_expression pbop_exp in
      { pbop_op; pbop_pat; pbop_exp; pbop_loc }

and ast_of_value_description
  : Compiler_types.value_description -> Versions.V4_08.Value_description.t
  = fun x ->
    Versions.V4_08.Value_description.of_concrete (concrete_of_value_description x)

and concrete_of_value_description
  : Compiler_types.value_description -> Versions.V4_08.Value_description.concrete
  = fun { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } ->
      let pval_type = ast_of_core_type pval_type in
      let pval_attributes = ast_of_attributes pval_attributes in
      { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }

and ast_to_value_description
  : Versions.V4_08.Value_description.t -> Compiler_types.value_description
  = fun x ->
    let concrete = Versions.V4_08.Value_description.to_concrete x in
    concrete_to_value_description concrete

and concrete_to_value_description
  : Versions.V4_08.Value_description.concrete -> Compiler_types.value_description
  = fun { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } ->
      let pval_type = ast_to_core_type pval_type in
      let pval_attributes = ast_to_attributes pval_attributes in
      { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }

and ast_of_type_declaration
  : Compiler_types.type_declaration -> Versions.V4_08.Type_declaration.t
  = fun x ->
    Versions.V4_08.Type_declaration.of_concrete (concrete_of_type_declaration x)

and concrete_of_type_declaration
  : Compiler_types.type_declaration -> Versions.V4_08.Type_declaration.concrete
  = fun { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } ->
      let ptype_params = (List.map ~f:(Tuple.map2 ~f1:ast_of_core_type ~f2:ast_of_variance)) ptype_params in
      let ptype_cstrs = (List.map ~f:(Tuple.map3 ~f1:ast_of_core_type ~f2:ast_of_core_type ~f3:Fn.id)) ptype_cstrs in
      let ptype_kind = ast_of_type_kind ptype_kind in
      let ptype_private = ast_of_private_flag ptype_private in
      let ptype_manifest = (Option.map ~f:ast_of_core_type) ptype_manifest in
      let ptype_attributes = ast_of_attributes ptype_attributes in
      { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc }

and ast_to_type_declaration
  : Versions.V4_08.Type_declaration.t -> Compiler_types.type_declaration
  = fun x ->
    let concrete = Versions.V4_08.Type_declaration.to_concrete x in
    concrete_to_type_declaration concrete

and concrete_to_type_declaration
  : Versions.V4_08.Type_declaration.concrete -> Compiler_types.type_declaration
  = fun { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } ->
      let ptype_params = (List.map ~f:(Tuple.map2 ~f1:ast_to_core_type ~f2:ast_to_variance)) ptype_params in
      let ptype_cstrs = (List.map ~f:(Tuple.map3 ~f1:ast_to_core_type ~f2:ast_to_core_type ~f3:Fn.id)) ptype_cstrs in
      let ptype_kind = ast_to_type_kind ptype_kind in
      let ptype_private = ast_to_private_flag ptype_private in
      let ptype_manifest = (Option.map ~f:ast_to_core_type) ptype_manifest in
      let ptype_attributes = ast_to_attributes ptype_attributes in
      { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc }

and ast_of_type_kind
  : Compiler_types.type_kind -> Versions.V4_08.Type_kind.t
  = fun x ->
    Versions.V4_08.Type_kind.of_concrete (concrete_of_type_kind x)

and concrete_of_type_kind
  : Compiler_types.type_kind -> Versions.V4_08.Type_kind.concrete
  = fun x ->
    match (x : Compiler_types.type_kind) with
    | Ptype_abstract -> Ptype_abstract
    | Ptype_variant (x1) ->
      let x1 = (List.map ~f:ast_of_constructor_declaration) x1 in
      Ptype_variant (x1)
    | Ptype_record (x1) ->
      let x1 = (List.map ~f:ast_of_label_declaration) x1 in
      Ptype_record (x1)
    | Ptype_open -> Ptype_open

and ast_to_type_kind
  : Versions.V4_08.Type_kind.t -> Compiler_types.type_kind
  = fun x ->
    let concrete = Versions.V4_08.Type_kind.to_concrete x in
    concrete_to_type_kind concrete

and concrete_to_type_kind
  : Versions.V4_08.Type_kind.concrete -> Compiler_types.type_kind
  = fun x ->
    match (x : Versions.V4_08.Type_kind.concrete) with
    | Ptype_abstract -> Ptype_abstract
    | Ptype_variant (x1) ->
      let x1 = (List.map ~f:ast_to_constructor_declaration) x1 in
      Ptype_variant (x1)
    | Ptype_record (x1) ->
      let x1 = (List.map ~f:ast_to_label_declaration) x1 in
      Ptype_record (x1)
    | Ptype_open -> Ptype_open

and ast_of_label_declaration
  : Compiler_types.label_declaration -> Versions.V4_08.Label_declaration.t
  = fun x ->
    Versions.V4_08.Label_declaration.of_concrete (concrete_of_label_declaration x)

and concrete_of_label_declaration
  : Compiler_types.label_declaration -> Versions.V4_08.Label_declaration.concrete
  = fun { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } ->
      let pld_mutable = ast_of_mutable_flag pld_mutable in
      let pld_type = ast_of_core_type pld_type in
      let pld_attributes = ast_of_attributes pld_attributes in
      { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }

and ast_to_label_declaration
  : Versions.V4_08.Label_declaration.t -> Compiler_types.label_declaration
  = fun x ->
    let concrete = Versions.V4_08.Label_declaration.to_concrete x in
    concrete_to_label_declaration concrete

and concrete_to_label_declaration
  : Versions.V4_08.Label_declaration.concrete -> Compiler_types.label_declaration
  = fun { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } ->
      let pld_mutable = ast_to_mutable_flag pld_mutable in
      let pld_type = ast_to_core_type pld_type in
      let pld_attributes = ast_to_attributes pld_attributes in
      { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }

and ast_of_constructor_declaration
  : Compiler_types.constructor_declaration -> Versions.V4_08.Constructor_declaration.t
  = fun x ->
    Versions.V4_08.Constructor_declaration.of_concrete (concrete_of_constructor_declaration x)

and concrete_of_constructor_declaration
  : Compiler_types.constructor_declaration -> Versions.V4_08.Constructor_declaration.concrete
  = fun { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } ->
      let pcd_args = ast_of_constructor_arguments pcd_args in
      let pcd_res = (Option.map ~f:ast_of_core_type) pcd_res in
      let pcd_attributes = ast_of_attributes pcd_attributes in
      { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }

and ast_to_constructor_declaration
  : Versions.V4_08.Constructor_declaration.t -> Compiler_types.constructor_declaration
  = fun x ->
    let concrete = Versions.V4_08.Constructor_declaration.to_concrete x in
    concrete_to_constructor_declaration concrete

and concrete_to_constructor_declaration
  : Versions.V4_08.Constructor_declaration.concrete -> Compiler_types.constructor_declaration
  = fun { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } ->
      let pcd_args = ast_to_constructor_arguments pcd_args in
      let pcd_res = (Option.map ~f:ast_to_core_type) pcd_res in
      let pcd_attributes = ast_to_attributes pcd_attributes in
      { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }

and ast_of_constructor_arguments
  : Compiler_types.constructor_arguments -> Versions.V4_08.Constructor_arguments.t
  = fun x ->
    Versions.V4_08.Constructor_arguments.of_concrete (concrete_of_constructor_arguments x)

and concrete_of_constructor_arguments
  : Compiler_types.constructor_arguments -> Versions.V4_08.Constructor_arguments.concrete
  = fun x ->
    match (x : Compiler_types.constructor_arguments) with
    | Pcstr_tuple (x1) ->
      let x1 = (List.map ~f:ast_of_core_type) x1 in
      Pcstr_tuple (x1)
    | Pcstr_record (x1) ->
      let x1 = (List.map ~f:ast_of_label_declaration) x1 in
      Pcstr_record (x1)

and ast_to_constructor_arguments
  : Versions.V4_08.Constructor_arguments.t -> Compiler_types.constructor_arguments
  = fun x ->
    let concrete = Versions.V4_08.Constructor_arguments.to_concrete x in
    concrete_to_constructor_arguments concrete

and concrete_to_constructor_arguments
  : Versions.V4_08.Constructor_arguments.concrete -> Compiler_types.constructor_arguments
  = fun x ->
    match (x : Versions.V4_08.Constructor_arguments.concrete) with
    | Pcstr_tuple (x1) ->
      let x1 = (List.map ~f:ast_to_core_type) x1 in
      Pcstr_tuple (x1)
    | Pcstr_record (x1) ->
      let x1 = (List.map ~f:ast_to_label_declaration) x1 in
      Pcstr_record (x1)

and ast_of_type_extension
  : Compiler_types.type_extension -> Versions.V4_08.Type_extension.t
  = fun x ->
    Versions.V4_08.Type_extension.of_concrete (concrete_of_type_extension x)

and concrete_of_type_extension
  : Compiler_types.type_extension -> Versions.V4_08.Type_extension.concrete
  = fun { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_loc; ptyext_attributes } ->
      let ptyext_path = ast_of_longident_loc ptyext_path in
      let ptyext_params = (List.map ~f:(Tuple.map2 ~f1:ast_of_core_type ~f2:ast_of_variance)) ptyext_params in
      let ptyext_constructors = (List.map ~f:ast_of_extension_constructor) ptyext_constructors in
      let ptyext_private = ast_of_private_flag ptyext_private in
      let ptyext_attributes = ast_of_attributes ptyext_attributes in
      { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_loc; ptyext_attributes }

and ast_to_type_extension
  : Versions.V4_08.Type_extension.t -> Compiler_types.type_extension
  = fun x ->
    let concrete = Versions.V4_08.Type_extension.to_concrete x in
    concrete_to_type_extension concrete

and concrete_to_type_extension
  : Versions.V4_08.Type_extension.concrete -> Compiler_types.type_extension
  = fun { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_loc; ptyext_attributes } ->
      let ptyext_path = ast_to_longident_loc ptyext_path in
      let ptyext_params = (List.map ~f:(Tuple.map2 ~f1:ast_to_core_type ~f2:ast_to_variance)) ptyext_params in
      let ptyext_constructors = (List.map ~f:ast_to_extension_constructor) ptyext_constructors in
      let ptyext_private = ast_to_private_flag ptyext_private in
      let ptyext_attributes = ast_to_attributes ptyext_attributes in
      { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_loc; ptyext_attributes }

and ast_of_extension_constructor
  : Compiler_types.extension_constructor -> Versions.V4_08.Extension_constructor.t
  = fun x ->
    Versions.V4_08.Extension_constructor.of_concrete (concrete_of_extension_constructor x)

and concrete_of_extension_constructor
  : Compiler_types.extension_constructor -> Versions.V4_08.Extension_constructor.concrete
  = fun { pext_name; pext_kind; pext_loc; pext_attributes } ->
      let pext_kind = ast_of_extension_constructor_kind pext_kind in
      let pext_attributes = ast_of_attributes pext_attributes in
      { pext_name; pext_kind; pext_loc; pext_attributes }

and ast_to_extension_constructor
  : Versions.V4_08.Extension_constructor.t -> Compiler_types.extension_constructor
  = fun x ->
    let concrete = Versions.V4_08.Extension_constructor.to_concrete x in
    concrete_to_extension_constructor concrete

and concrete_to_extension_constructor
  : Versions.V4_08.Extension_constructor.concrete -> Compiler_types.extension_constructor
  = fun { pext_name; pext_kind; pext_loc; pext_attributes } ->
      let pext_kind = ast_to_extension_constructor_kind pext_kind in
      let pext_attributes = ast_to_attributes pext_attributes in
      { pext_name; pext_kind; pext_loc; pext_attributes }

and ast_of_type_exception
  : Compiler_types.type_exception -> Versions.V4_08.Type_exception.t
  = fun x ->
    Versions.V4_08.Type_exception.of_concrete (concrete_of_type_exception x)

and concrete_of_type_exception
  : Compiler_types.type_exception -> Versions.V4_08.Type_exception.concrete
  = fun { ptyexn_constructor; ptyexn_loc; ptyexn_attributes } ->
      let ptyexn_constructor = ast_of_extension_constructor ptyexn_constructor in
      let ptyexn_attributes = ast_of_attributes ptyexn_attributes in
      { ptyexn_constructor; ptyexn_loc; ptyexn_attributes }

and ast_to_type_exception
  : Versions.V4_08.Type_exception.t -> Compiler_types.type_exception
  = fun x ->
    let concrete = Versions.V4_08.Type_exception.to_concrete x in
    concrete_to_type_exception concrete

and concrete_to_type_exception
  : Versions.V4_08.Type_exception.concrete -> Compiler_types.type_exception
  = fun { ptyexn_constructor; ptyexn_loc; ptyexn_attributes } ->
      let ptyexn_constructor = ast_to_extension_constructor ptyexn_constructor in
      let ptyexn_attributes = ast_to_attributes ptyexn_attributes in
      { ptyexn_constructor; ptyexn_loc; ptyexn_attributes }

and ast_of_extension_constructor_kind
  : Compiler_types.extension_constructor_kind -> Versions.V4_08.Extension_constructor_kind.t
  = fun x ->
    Versions.V4_08.Extension_constructor_kind.of_concrete (concrete_of_extension_constructor_kind x)

and concrete_of_extension_constructor_kind
  : Compiler_types.extension_constructor_kind -> Versions.V4_08.Extension_constructor_kind.concrete
  = fun x ->
    match (x : Compiler_types.extension_constructor_kind) with
    | Pext_decl (x1, x2) ->
      let x1 = ast_of_constructor_arguments x1 in
      let x2 = (Option.map ~f:ast_of_core_type) x2 in
      Pext_decl (x1, x2)
    | Pext_rebind (x1) ->
      let x1 = ast_of_longident_loc x1 in
      Pext_rebind (x1)

and ast_to_extension_constructor_kind
  : Versions.V4_08.Extension_constructor_kind.t -> Compiler_types.extension_constructor_kind
  = fun x ->
    let concrete = Versions.V4_08.Extension_constructor_kind.to_concrete x in
    concrete_to_extension_constructor_kind concrete

and concrete_to_extension_constructor_kind
  : Versions.V4_08.Extension_constructor_kind.concrete -> Compiler_types.extension_constructor_kind
  = fun x ->
    match (x : Versions.V4_08.Extension_constructor_kind.concrete) with
    | Pext_decl (x1, x2) ->
      let x1 = ast_to_constructor_arguments x1 in
      let x2 = (Option.map ~f:ast_to_core_type) x2 in
      Pext_decl (x1, x2)
    | Pext_rebind (x1) ->
      let x1 = ast_to_longident_loc x1 in
      Pext_rebind (x1)

and ast_of_class_type
  : Compiler_types.class_type -> Versions.V4_08.Class_type.t
  = fun x ->
    Versions.V4_08.Class_type.of_concrete (concrete_of_class_type x)

and concrete_of_class_type
  : Compiler_types.class_type -> Versions.V4_08.Class_type.concrete
  = fun { pcty_desc; pcty_loc; pcty_attributes } ->
      let pcty_desc = ast_of_class_type_desc pcty_desc in
      let pcty_attributes = ast_of_attributes pcty_attributes in
      { pcty_desc; pcty_loc; pcty_attributes }

and ast_to_class_type
  : Versions.V4_08.Class_type.t -> Compiler_types.class_type
  = fun x ->
    let concrete = Versions.V4_08.Class_type.to_concrete x in
    concrete_to_class_type concrete

and concrete_to_class_type
  : Versions.V4_08.Class_type.concrete -> Compiler_types.class_type
  = fun { pcty_desc; pcty_loc; pcty_attributes } ->
      let pcty_desc = ast_to_class_type_desc pcty_desc in
      let pcty_attributes = ast_to_attributes pcty_attributes in
      { pcty_desc; pcty_loc; pcty_attributes }

and ast_of_class_type_desc
  : Compiler_types.class_type_desc -> Versions.V4_08.Class_type_desc.t
  = fun x ->
    Versions.V4_08.Class_type_desc.of_concrete (concrete_of_class_type_desc x)

and concrete_of_class_type_desc
  : Compiler_types.class_type_desc -> Versions.V4_08.Class_type_desc.concrete
  = fun x ->
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
    | Pcty_open (x1, x2) ->
      let x1 = ast_of_open_description x1 in
      let x2 = ast_of_class_type x2 in
      Pcty_open (x1, x2)

and ast_to_class_type_desc
  : Versions.V4_08.Class_type_desc.t -> Compiler_types.class_type_desc
  = fun x ->
    let concrete = Versions.V4_08.Class_type_desc.to_concrete x in
    concrete_to_class_type_desc concrete

and concrete_to_class_type_desc
  : Versions.V4_08.Class_type_desc.concrete -> Compiler_types.class_type_desc
  = fun x ->
    match (x : Versions.V4_08.Class_type_desc.concrete) with
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
    | Pcty_open (x1, x2) ->
      let x1 = ast_to_open_description x1 in
      let x2 = ast_to_class_type x2 in
      Pcty_open (x1, x2)

and ast_of_class_signature
  : Compiler_types.class_signature -> Versions.V4_08.Class_signature.t
  = fun x ->
    Versions.V4_08.Class_signature.of_concrete (concrete_of_class_signature x)

and concrete_of_class_signature
  : Compiler_types.class_signature -> Versions.V4_08.Class_signature.concrete
  = fun { pcsig_self; pcsig_fields } ->
      let pcsig_self = ast_of_core_type pcsig_self in
      let pcsig_fields = (List.map ~f:ast_of_class_type_field) pcsig_fields in
      { pcsig_self; pcsig_fields }

and ast_to_class_signature
  : Versions.V4_08.Class_signature.t -> Compiler_types.class_signature
  = fun x ->
    let concrete = Versions.V4_08.Class_signature.to_concrete x in
    concrete_to_class_signature concrete

and concrete_to_class_signature
  : Versions.V4_08.Class_signature.concrete -> Compiler_types.class_signature
  = fun { pcsig_self; pcsig_fields } ->
      let pcsig_self = ast_to_core_type pcsig_self in
      let pcsig_fields = (List.map ~f:ast_to_class_type_field) pcsig_fields in
      { pcsig_self; pcsig_fields }

and ast_of_class_type_field
  : Compiler_types.class_type_field -> Versions.V4_08.Class_type_field.t
  = fun x ->
    Versions.V4_08.Class_type_field.of_concrete (concrete_of_class_type_field x)

and concrete_of_class_type_field
  : Compiler_types.class_type_field -> Versions.V4_08.Class_type_field.concrete
  = fun { pctf_desc; pctf_loc; pctf_attributes } ->
      let pctf_desc = ast_of_class_type_field_desc pctf_desc in
      let pctf_attributes = ast_of_attributes pctf_attributes in
      { pctf_desc; pctf_loc; pctf_attributes }

and ast_to_class_type_field
  : Versions.V4_08.Class_type_field.t -> Compiler_types.class_type_field
  = fun x ->
    let concrete = Versions.V4_08.Class_type_field.to_concrete x in
    concrete_to_class_type_field concrete

and concrete_to_class_type_field
  : Versions.V4_08.Class_type_field.concrete -> Compiler_types.class_type_field
  = fun { pctf_desc; pctf_loc; pctf_attributes } ->
      let pctf_desc = ast_to_class_type_field_desc pctf_desc in
      let pctf_attributes = ast_to_attributes pctf_attributes in
      { pctf_desc; pctf_loc; pctf_attributes }

and ast_of_class_type_field_desc
  : Compiler_types.class_type_field_desc -> Versions.V4_08.Class_type_field_desc.t
  = fun x ->
    Versions.V4_08.Class_type_field_desc.of_concrete (concrete_of_class_type_field_desc x)

and concrete_of_class_type_field_desc
  : Compiler_types.class_type_field_desc -> Versions.V4_08.Class_type_field_desc.concrete
  = fun x ->
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

and ast_to_class_type_field_desc
  : Versions.V4_08.Class_type_field_desc.t -> Compiler_types.class_type_field_desc
  = fun x ->
    let concrete = Versions.V4_08.Class_type_field_desc.to_concrete x in
    concrete_to_class_type_field_desc concrete

and concrete_to_class_type_field_desc
  : Versions.V4_08.Class_type_field_desc.concrete -> Compiler_types.class_type_field_desc
  = fun x ->
    match (x : Versions.V4_08.Class_type_field_desc.concrete) with
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

and ast_of_class_infos
  : type a a_ .(a_ -> a Unversioned.Types.node) -> a_ Compiler_types.class_infos -> a Unversioned.Types.node Versions.V4_08.Class_infos.t
  = fun ast_of_a x ->
    Versions.V4_08.Class_infos.of_concrete (concrete_of_class_infos ast_of_a x)

and concrete_of_class_infos
  : type a a_ .(a_ -> a Unversioned.Types.node) -> a_ Compiler_types.class_infos -> a Unversioned.Types.node Versions.V4_08.Class_infos.concrete
  = fun ast_of_a { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } ->
      let pci_virt = ast_of_virtual_flag pci_virt in
      let pci_params = (List.map ~f:(Tuple.map2 ~f1:ast_of_core_type ~f2:ast_of_variance)) pci_params in
      let pci_expr = ast_of_a pci_expr in
      let pci_attributes = ast_of_attributes pci_attributes in
      { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }

and ast_to_class_infos
  : type a a_ .(a Unversioned.Types.node -> a_) -> a Unversioned.Types.node Versions.V4_08.Class_infos.t -> a_ Compiler_types.class_infos
  = fun ast_to_a x ->
    let concrete = Versions.V4_08.Class_infos.to_concrete x in
    concrete_to_class_infos ast_to_a concrete

and concrete_to_class_infos
  : type a a_ .(a Unversioned.Types.node -> a_) -> a Unversioned.Types.node Versions.V4_08.Class_infos.concrete -> a_ Compiler_types.class_infos
  = fun ast_to_a { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } ->
      let pci_virt = ast_to_virtual_flag pci_virt in
      let pci_params = (List.map ~f:(Tuple.map2 ~f1:ast_to_core_type ~f2:ast_to_variance)) pci_params in
      let pci_expr = ast_to_a pci_expr in
      let pci_attributes = ast_to_attributes pci_attributes in
      { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }

and ast_of_class_description
  : Compiler_types.class_description -> Versions.V4_08.Class_description.t
  = fun x ->
    Versions.V4_08.Class_description.of_concrete (concrete_of_class_description x)

and concrete_of_class_description
  : Compiler_types.class_description -> Versions.V4_08.Class_description.concrete
  = fun x ->
    (ast_of_class_infos ast_of_class_type) x

and ast_to_class_description
  : Versions.V4_08.Class_description.t -> Compiler_types.class_description
  = fun x ->
    let concrete = Versions.V4_08.Class_description.to_concrete x in
    concrete_to_class_description concrete

and concrete_to_class_description
  : Versions.V4_08.Class_description.concrete -> Compiler_types.class_description
  = fun x ->
    (ast_to_class_infos ast_to_class_type) x

and ast_of_class_type_declaration
  : Compiler_types.class_type_declaration -> Versions.V4_08.Class_type_declaration.t
  = fun x ->
    Versions.V4_08.Class_type_declaration.of_concrete (concrete_of_class_type_declaration x)

and concrete_of_class_type_declaration
  : Compiler_types.class_type_declaration -> Versions.V4_08.Class_type_declaration.concrete
  = fun x ->
    (ast_of_class_infos ast_of_class_type) x

and ast_to_class_type_declaration
  : Versions.V4_08.Class_type_declaration.t -> Compiler_types.class_type_declaration
  = fun x ->
    let concrete = Versions.V4_08.Class_type_declaration.to_concrete x in
    concrete_to_class_type_declaration concrete

and concrete_to_class_type_declaration
  : Versions.V4_08.Class_type_declaration.concrete -> Compiler_types.class_type_declaration
  = fun x ->
    (ast_to_class_infos ast_to_class_type) x

and ast_of_class_expr
  : Compiler_types.class_expr -> Versions.V4_08.Class_expr.t
  = fun x ->
    Versions.V4_08.Class_expr.of_concrete (concrete_of_class_expr x)

and concrete_of_class_expr
  : Compiler_types.class_expr -> Versions.V4_08.Class_expr.concrete
  = fun { pcl_desc; pcl_loc; pcl_attributes } ->
      let pcl_desc = ast_of_class_expr_desc pcl_desc in
      let pcl_attributes = ast_of_attributes pcl_attributes in
      { pcl_desc; pcl_loc; pcl_attributes }

and ast_to_class_expr
  : Versions.V4_08.Class_expr.t -> Compiler_types.class_expr
  = fun x ->
    let concrete = Versions.V4_08.Class_expr.to_concrete x in
    concrete_to_class_expr concrete

and concrete_to_class_expr
  : Versions.V4_08.Class_expr.concrete -> Compiler_types.class_expr
  = fun { pcl_desc; pcl_loc; pcl_attributes } ->
      let pcl_desc = ast_to_class_expr_desc pcl_desc in
      let pcl_attributes = ast_to_attributes pcl_attributes in
      { pcl_desc; pcl_loc; pcl_attributes }

and ast_of_class_expr_desc
  : Compiler_types.class_expr_desc -> Versions.V4_08.Class_expr_desc.t
  = fun x ->
    Versions.V4_08.Class_expr_desc.of_concrete (concrete_of_class_expr_desc x)

and concrete_of_class_expr_desc
  : Compiler_types.class_expr_desc -> Versions.V4_08.Class_expr_desc.concrete
  = fun x ->
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
    | Pcl_open (x1, x2) ->
      let x1 = ast_of_open_description x1 in
      let x2 = ast_of_class_expr x2 in
      Pcl_open (x1, x2)

and ast_to_class_expr_desc
  : Versions.V4_08.Class_expr_desc.t -> Compiler_types.class_expr_desc
  = fun x ->
    let concrete = Versions.V4_08.Class_expr_desc.to_concrete x in
    concrete_to_class_expr_desc concrete

and concrete_to_class_expr_desc
  : Versions.V4_08.Class_expr_desc.concrete -> Compiler_types.class_expr_desc
  = fun x ->
    match (x : Versions.V4_08.Class_expr_desc.concrete) with
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
    | Pcl_open (x1, x2) ->
      let x1 = ast_to_open_description x1 in
      let x2 = ast_to_class_expr x2 in
      Pcl_open (x1, x2)

and ast_of_class_structure
  : Compiler_types.class_structure -> Versions.V4_08.Class_structure.t
  = fun x ->
    Versions.V4_08.Class_structure.of_concrete (concrete_of_class_structure x)

and concrete_of_class_structure
  : Compiler_types.class_structure -> Versions.V4_08.Class_structure.concrete
  = fun { pcstr_self; pcstr_fields } ->
      let pcstr_self = ast_of_pattern pcstr_self in
      let pcstr_fields = (List.map ~f:ast_of_class_field) pcstr_fields in
      { pcstr_self; pcstr_fields }

and ast_to_class_structure
  : Versions.V4_08.Class_structure.t -> Compiler_types.class_structure
  = fun x ->
    let concrete = Versions.V4_08.Class_structure.to_concrete x in
    concrete_to_class_structure concrete

and concrete_to_class_structure
  : Versions.V4_08.Class_structure.concrete -> Compiler_types.class_structure
  = fun { pcstr_self; pcstr_fields } ->
      let pcstr_self = ast_to_pattern pcstr_self in
      let pcstr_fields = (List.map ~f:ast_to_class_field) pcstr_fields in
      { pcstr_self; pcstr_fields }

and ast_of_class_field
  : Compiler_types.class_field -> Versions.V4_08.Class_field.t
  = fun x ->
    Versions.V4_08.Class_field.of_concrete (concrete_of_class_field x)

and concrete_of_class_field
  : Compiler_types.class_field -> Versions.V4_08.Class_field.concrete
  = fun { pcf_desc; pcf_loc; pcf_attributes } ->
      let pcf_desc = ast_of_class_field_desc pcf_desc in
      let pcf_attributes = ast_of_attributes pcf_attributes in
      { pcf_desc; pcf_loc; pcf_attributes }

and ast_to_class_field
  : Versions.V4_08.Class_field.t -> Compiler_types.class_field
  = fun x ->
    let concrete = Versions.V4_08.Class_field.to_concrete x in
    concrete_to_class_field concrete

and concrete_to_class_field
  : Versions.V4_08.Class_field.concrete -> Compiler_types.class_field
  = fun { pcf_desc; pcf_loc; pcf_attributes } ->
      let pcf_desc = ast_to_class_field_desc pcf_desc in
      let pcf_attributes = ast_to_attributes pcf_attributes in
      { pcf_desc; pcf_loc; pcf_attributes }

and ast_of_class_field_desc
  : Compiler_types.class_field_desc -> Versions.V4_08.Class_field_desc.t
  = fun x ->
    Versions.V4_08.Class_field_desc.of_concrete (concrete_of_class_field_desc x)

and concrete_of_class_field_desc
  : Compiler_types.class_field_desc -> Versions.V4_08.Class_field_desc.concrete
  = fun x ->
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

and ast_to_class_field_desc
  : Versions.V4_08.Class_field_desc.t -> Compiler_types.class_field_desc
  = fun x ->
    let concrete = Versions.V4_08.Class_field_desc.to_concrete x in
    concrete_to_class_field_desc concrete

and concrete_to_class_field_desc
  : Versions.V4_08.Class_field_desc.concrete -> Compiler_types.class_field_desc
  = fun x ->
    match (x : Versions.V4_08.Class_field_desc.concrete) with
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

and ast_of_class_field_kind
  : Compiler_types.class_field_kind -> Versions.V4_08.Class_field_kind.t
  = fun x ->
    Versions.V4_08.Class_field_kind.of_concrete (concrete_of_class_field_kind x)

and concrete_of_class_field_kind
  : Compiler_types.class_field_kind -> Versions.V4_08.Class_field_kind.concrete
  = fun x ->
    match (x : Compiler_types.class_field_kind) with
    | Cfk_virtual (x1) ->
      let x1 = ast_of_core_type x1 in
      Cfk_virtual (x1)
    | Cfk_concrete (x1, x2) ->
      let x1 = ast_of_override_flag x1 in
      let x2 = ast_of_expression x2 in
      Cfk_concrete (x1, x2)

and ast_to_class_field_kind
  : Versions.V4_08.Class_field_kind.t -> Compiler_types.class_field_kind
  = fun x ->
    let concrete = Versions.V4_08.Class_field_kind.to_concrete x in
    concrete_to_class_field_kind concrete

and concrete_to_class_field_kind
  : Versions.V4_08.Class_field_kind.concrete -> Compiler_types.class_field_kind
  = fun x ->
    match (x : Versions.V4_08.Class_field_kind.concrete) with
    | Cfk_virtual (x1) ->
      let x1 = ast_to_core_type x1 in
      Cfk_virtual (x1)
    | Cfk_concrete (x1, x2) ->
      let x1 = ast_to_override_flag x1 in
      let x2 = ast_to_expression x2 in
      Cfk_concrete (x1, x2)

and ast_of_class_declaration
  : Compiler_types.class_declaration -> Versions.V4_08.Class_declaration.t
  = fun x ->
    Versions.V4_08.Class_declaration.of_concrete (concrete_of_class_declaration x)

and concrete_of_class_declaration
  : Compiler_types.class_declaration -> Versions.V4_08.Class_declaration.concrete
  = fun x ->
    (ast_of_class_infos ast_of_class_expr) x

and ast_to_class_declaration
  : Versions.V4_08.Class_declaration.t -> Compiler_types.class_declaration
  = fun x ->
    let concrete = Versions.V4_08.Class_declaration.to_concrete x in
    concrete_to_class_declaration concrete

and concrete_to_class_declaration
  : Versions.V4_08.Class_declaration.concrete -> Compiler_types.class_declaration
  = fun x ->
    (ast_to_class_infos ast_to_class_expr) x

and ast_of_module_type
  : Compiler_types.module_type -> Versions.V4_08.Module_type.t
  = fun x ->
    Versions.V4_08.Module_type.of_concrete (concrete_of_module_type x)

and concrete_of_module_type
  : Compiler_types.module_type -> Versions.V4_08.Module_type.concrete
  = fun { pmty_desc; pmty_loc; pmty_attributes } ->
      let pmty_desc = ast_of_module_type_desc pmty_desc in
      let pmty_attributes = ast_of_attributes pmty_attributes in
      { pmty_desc; pmty_loc; pmty_attributes }

and ast_to_module_type
  : Versions.V4_08.Module_type.t -> Compiler_types.module_type
  = fun x ->
    let concrete = Versions.V4_08.Module_type.to_concrete x in
    concrete_to_module_type concrete

and concrete_to_module_type
  : Versions.V4_08.Module_type.concrete -> Compiler_types.module_type
  = fun { pmty_desc; pmty_loc; pmty_attributes } ->
      let pmty_desc = ast_to_module_type_desc pmty_desc in
      let pmty_attributes = ast_to_attributes pmty_attributes in
      { pmty_desc; pmty_loc; pmty_attributes }

and ast_of_module_type_desc
  : Compiler_types.module_type_desc -> Versions.V4_08.Module_type_desc.t
  = fun x ->
    Versions.V4_08.Module_type_desc.of_concrete (concrete_of_module_type_desc x)

and concrete_of_module_type_desc
  : Compiler_types.module_type_desc -> Versions.V4_08.Module_type_desc.concrete
  = fun x ->
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

and ast_to_module_type_desc
  : Versions.V4_08.Module_type_desc.t -> Compiler_types.module_type_desc
  = fun x ->
    let concrete = Versions.V4_08.Module_type_desc.to_concrete x in
    concrete_to_module_type_desc concrete

and concrete_to_module_type_desc
  : Versions.V4_08.Module_type_desc.concrete -> Compiler_types.module_type_desc
  = fun x ->
    match (x : Versions.V4_08.Module_type_desc.concrete) with
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

and ast_of_signature
  : Compiler_types.signature -> Versions.V4_08.Signature.t
  = fun x ->
    Versions.V4_08.Signature.of_concrete (concrete_of_signature x)

and concrete_of_signature
  : Compiler_types.signature -> Versions.V4_08.Signature.concrete
  = fun x ->
    (List.map ~f:ast_of_signature_item) x

and ast_to_signature
  : Versions.V4_08.Signature.t -> Compiler_types.signature
  = fun x ->
    let concrete = Versions.V4_08.Signature.to_concrete x in
    concrete_to_signature concrete

and concrete_to_signature
  : Versions.V4_08.Signature.concrete -> Compiler_types.signature
  = fun x ->
    (List.map ~f:ast_to_signature_item) x

and ast_of_signature_item
  : Compiler_types.signature_item -> Versions.V4_08.Signature_item.t
  = fun x ->
    Versions.V4_08.Signature_item.of_concrete (concrete_of_signature_item x)

and concrete_of_signature_item
  : Compiler_types.signature_item -> Versions.V4_08.Signature_item.concrete
  = fun { psig_desc; psig_loc } ->
      let psig_desc = ast_of_signature_item_desc psig_desc in
      { psig_desc; psig_loc }

and ast_to_signature_item
  : Versions.V4_08.Signature_item.t -> Compiler_types.signature_item
  = fun x ->
    let concrete = Versions.V4_08.Signature_item.to_concrete x in
    concrete_to_signature_item concrete

and concrete_to_signature_item
  : Versions.V4_08.Signature_item.concrete -> Compiler_types.signature_item
  = fun { psig_desc; psig_loc } ->
      let psig_desc = ast_to_signature_item_desc psig_desc in
      { psig_desc; psig_loc }

and ast_of_signature_item_desc
  : Compiler_types.signature_item_desc -> Versions.V4_08.Signature_item_desc.t
  = fun x ->
    Versions.V4_08.Signature_item_desc.of_concrete (concrete_of_signature_item_desc x)

and concrete_of_signature_item_desc
  : Compiler_types.signature_item_desc -> Versions.V4_08.Signature_item_desc.concrete
  = fun x ->
    match (x : Compiler_types.signature_item_desc) with
    | Psig_value (x1) ->
      let x1 = ast_of_value_description x1 in
      Psig_value (x1)
    | Psig_type (x1, x2) ->
      let x1 = ast_of_rec_flag x1 in
      let x2 = (List.map ~f:ast_of_type_declaration) x2 in
      Psig_type (x1, x2)
    | Psig_typesubst (x1) ->
      let x1 = (List.map ~f:ast_of_type_declaration) x1 in
      Psig_typesubst (x1)
    | Psig_typext (x1) ->
      let x1 = ast_of_type_extension x1 in
      Psig_typext (x1)
    | Psig_exception (x1) ->
      let x1 = ast_of_type_exception x1 in
      Psig_exception (x1)
    | Psig_module (x1) ->
      let x1 = ast_of_module_declaration x1 in
      Psig_module (x1)
    | Psig_modsubst (x1) ->
      let x1 = ast_of_module_substitution x1 in
      Psig_modsubst (x1)
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

and ast_to_signature_item_desc
  : Versions.V4_08.Signature_item_desc.t -> Compiler_types.signature_item_desc
  = fun x ->
    let concrete = Versions.V4_08.Signature_item_desc.to_concrete x in
    concrete_to_signature_item_desc concrete

and concrete_to_signature_item_desc
  : Versions.V4_08.Signature_item_desc.concrete -> Compiler_types.signature_item_desc
  = fun x ->
    match (x : Versions.V4_08.Signature_item_desc.concrete) with
    | Psig_value (x1) ->
      let x1 = ast_to_value_description x1 in
      Psig_value (x1)
    | Psig_type (x1, x2) ->
      let x1 = ast_to_rec_flag x1 in
      let x2 = (List.map ~f:ast_to_type_declaration) x2 in
      Psig_type (x1, x2)
    | Psig_typesubst (x1) ->
      let x1 = (List.map ~f:ast_to_type_declaration) x1 in
      Psig_typesubst (x1)
    | Psig_typext (x1) ->
      let x1 = ast_to_type_extension x1 in
      Psig_typext (x1)
    | Psig_exception (x1) ->
      let x1 = ast_to_type_exception x1 in
      Psig_exception (x1)
    | Psig_module (x1) ->
      let x1 = ast_to_module_declaration x1 in
      Psig_module (x1)
    | Psig_modsubst (x1) ->
      let x1 = ast_to_module_substitution x1 in
      Psig_modsubst (x1)
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

and ast_of_module_declaration
  : Compiler_types.module_declaration -> Versions.V4_08.Module_declaration.t
  = fun x ->
    Versions.V4_08.Module_declaration.of_concrete (concrete_of_module_declaration x)

and concrete_of_module_declaration
  : Compiler_types.module_declaration -> Versions.V4_08.Module_declaration.concrete
  = fun { pmd_name; pmd_type; pmd_attributes; pmd_loc } ->
      let pmd_type = ast_of_module_type pmd_type in
      let pmd_attributes = ast_of_attributes pmd_attributes in
      { pmd_name; pmd_type; pmd_attributes; pmd_loc }

and ast_to_module_declaration
  : Versions.V4_08.Module_declaration.t -> Compiler_types.module_declaration
  = fun x ->
    let concrete = Versions.V4_08.Module_declaration.to_concrete x in
    concrete_to_module_declaration concrete

and concrete_to_module_declaration
  : Versions.V4_08.Module_declaration.concrete -> Compiler_types.module_declaration
  = fun { pmd_name; pmd_type; pmd_attributes; pmd_loc } ->
      let pmd_type = ast_to_module_type pmd_type in
      let pmd_attributes = ast_to_attributes pmd_attributes in
      { pmd_name; pmd_type; pmd_attributes; pmd_loc }

and ast_of_module_substitution
  : Compiler_types.module_substitution -> Versions.V4_08.Module_substitution.t
  = fun x ->
    Versions.V4_08.Module_substitution.of_concrete (concrete_of_module_substitution x)

and concrete_of_module_substitution
  : Compiler_types.module_substitution -> Versions.V4_08.Module_substitution.concrete
  = fun { pms_name; pms_manifest; pms_attributes; pms_loc } ->
      let pms_manifest = ast_of_longident_loc pms_manifest in
      let pms_attributes = ast_of_attributes pms_attributes in
      { pms_name; pms_manifest; pms_attributes; pms_loc }

and ast_to_module_substitution
  : Versions.V4_08.Module_substitution.t -> Compiler_types.module_substitution
  = fun x ->
    let concrete = Versions.V4_08.Module_substitution.to_concrete x in
    concrete_to_module_substitution concrete

and concrete_to_module_substitution
  : Versions.V4_08.Module_substitution.concrete -> Compiler_types.module_substitution
  = fun { pms_name; pms_manifest; pms_attributes; pms_loc } ->
      let pms_manifest = ast_to_longident_loc pms_manifest in
      let pms_attributes = ast_to_attributes pms_attributes in
      { pms_name; pms_manifest; pms_attributes; pms_loc }

and ast_of_module_type_declaration
  : Compiler_types.module_type_declaration -> Versions.V4_08.Module_type_declaration.t
  = fun x ->
    Versions.V4_08.Module_type_declaration.of_concrete (concrete_of_module_type_declaration x)

and concrete_of_module_type_declaration
  : Compiler_types.module_type_declaration -> Versions.V4_08.Module_type_declaration.concrete
  = fun { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } ->
      let pmtd_type = (Option.map ~f:ast_of_module_type) pmtd_type in
      let pmtd_attributes = ast_of_attributes pmtd_attributes in
      { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }

and ast_to_module_type_declaration
  : Versions.V4_08.Module_type_declaration.t -> Compiler_types.module_type_declaration
  = fun x ->
    let concrete = Versions.V4_08.Module_type_declaration.to_concrete x in
    concrete_to_module_type_declaration concrete

and concrete_to_module_type_declaration
  : Versions.V4_08.Module_type_declaration.concrete -> Compiler_types.module_type_declaration
  = fun { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } ->
      let pmtd_type = (Option.map ~f:ast_to_module_type) pmtd_type in
      let pmtd_attributes = ast_to_attributes pmtd_attributes in
      { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }

and ast_of_open_infos
  : type a a_ .(a_ -> a Unversioned.Types.node) -> a_ Compiler_types.open_infos -> a Unversioned.Types.node Versions.V4_08.Open_infos.t
  = fun ast_of_a x ->
    Versions.V4_08.Open_infos.of_concrete (concrete_of_open_infos ast_of_a x)

and concrete_of_open_infos
  : type a a_ .(a_ -> a Unversioned.Types.node) -> a_ Compiler_types.open_infos -> a Unversioned.Types.node Versions.V4_08.Open_infos.concrete
  = fun ast_of_a { popen_expr; popen_override; popen_loc; popen_attributes } ->
      let popen_expr = ast_of_a popen_expr in
      let popen_override = ast_of_override_flag popen_override in
      let popen_attributes = ast_of_attributes popen_attributes in
      { popen_expr; popen_override; popen_loc; popen_attributes }

and ast_to_open_infos
  : type a a_ .(a Unversioned.Types.node -> a_) -> a Unversioned.Types.node Versions.V4_08.Open_infos.t -> a_ Compiler_types.open_infos
  = fun ast_to_a x ->
    let concrete = Versions.V4_08.Open_infos.to_concrete x in
    concrete_to_open_infos ast_to_a concrete

and concrete_to_open_infos
  : type a a_ .(a Unversioned.Types.node -> a_) -> a Unversioned.Types.node Versions.V4_08.Open_infos.concrete -> a_ Compiler_types.open_infos
  = fun ast_to_a { popen_expr; popen_override; popen_loc; popen_attributes } ->
      let popen_expr = ast_to_a popen_expr in
      let popen_override = ast_to_override_flag popen_override in
      let popen_attributes = ast_to_attributes popen_attributes in
      { popen_expr; popen_override; popen_loc; popen_attributes }

and ast_of_open_description
  : Compiler_types.open_description -> Versions.V4_08.Open_description.t
  = fun x ->
    Versions.V4_08.Open_description.of_concrete (concrete_of_open_description x)

and concrete_of_open_description
  : Compiler_types.open_description -> Versions.V4_08.Open_description.concrete
  = fun x ->
    (ast_of_open_infos ast_of_longident_loc) x

and ast_to_open_description
  : Versions.V4_08.Open_description.t -> Compiler_types.open_description
  = fun x ->
    let concrete = Versions.V4_08.Open_description.to_concrete x in
    concrete_to_open_description concrete

and concrete_to_open_description
  : Versions.V4_08.Open_description.concrete -> Compiler_types.open_description
  = fun x ->
    (ast_to_open_infos ast_to_longident_loc) x

and ast_of_open_declaration
  : Compiler_types.open_declaration -> Versions.V4_08.Open_declaration.t
  = fun x ->
    Versions.V4_08.Open_declaration.of_concrete (concrete_of_open_declaration x)

and concrete_of_open_declaration
  : Compiler_types.open_declaration -> Versions.V4_08.Open_declaration.concrete
  = fun x ->
    (ast_of_open_infos ast_of_module_expr) x

and ast_to_open_declaration
  : Versions.V4_08.Open_declaration.t -> Compiler_types.open_declaration
  = fun x ->
    let concrete = Versions.V4_08.Open_declaration.to_concrete x in
    concrete_to_open_declaration concrete

and concrete_to_open_declaration
  : Versions.V4_08.Open_declaration.concrete -> Compiler_types.open_declaration
  = fun x ->
    (ast_to_open_infos ast_to_module_expr) x

and ast_of_include_infos
  : type a a_ .(a_ -> a Unversioned.Types.node) -> a_ Compiler_types.include_infos -> a Unversioned.Types.node Versions.V4_08.Include_infos.t
  = fun ast_of_a x ->
    Versions.V4_08.Include_infos.of_concrete (concrete_of_include_infos ast_of_a x)

and concrete_of_include_infos
  : type a a_ .(a_ -> a Unversioned.Types.node) -> a_ Compiler_types.include_infos -> a Unversioned.Types.node Versions.V4_08.Include_infos.concrete
  = fun ast_of_a { pincl_mod; pincl_loc; pincl_attributes } ->
      let pincl_mod = ast_of_a pincl_mod in
      let pincl_attributes = ast_of_attributes pincl_attributes in
      { pincl_mod; pincl_loc; pincl_attributes }

and ast_to_include_infos
  : type a a_ .(a Unversioned.Types.node -> a_) -> a Unversioned.Types.node Versions.V4_08.Include_infos.t -> a_ Compiler_types.include_infos
  = fun ast_to_a x ->
    let concrete = Versions.V4_08.Include_infos.to_concrete x in
    concrete_to_include_infos ast_to_a concrete

and concrete_to_include_infos
  : type a a_ .(a Unversioned.Types.node -> a_) -> a Unversioned.Types.node Versions.V4_08.Include_infos.concrete -> a_ Compiler_types.include_infos
  = fun ast_to_a { pincl_mod; pincl_loc; pincl_attributes } ->
      let pincl_mod = ast_to_a pincl_mod in
      let pincl_attributes = ast_to_attributes pincl_attributes in
      { pincl_mod; pincl_loc; pincl_attributes }

and ast_of_include_description
  : Compiler_types.include_description -> Versions.V4_08.Include_description.t
  = fun x ->
    Versions.V4_08.Include_description.of_concrete (concrete_of_include_description x)

and concrete_of_include_description
  : Compiler_types.include_description -> Versions.V4_08.Include_description.concrete
  = fun x ->
    (ast_of_include_infos ast_of_module_type) x

and ast_to_include_description
  : Versions.V4_08.Include_description.t -> Compiler_types.include_description
  = fun x ->
    let concrete = Versions.V4_08.Include_description.to_concrete x in
    concrete_to_include_description concrete

and concrete_to_include_description
  : Versions.V4_08.Include_description.concrete -> Compiler_types.include_description
  = fun x ->
    (ast_to_include_infos ast_to_module_type) x

and ast_of_include_declaration
  : Compiler_types.include_declaration -> Versions.V4_08.Include_declaration.t
  = fun x ->
    Versions.V4_08.Include_declaration.of_concrete (concrete_of_include_declaration x)

and concrete_of_include_declaration
  : Compiler_types.include_declaration -> Versions.V4_08.Include_declaration.concrete
  = fun x ->
    (ast_of_include_infos ast_of_module_expr) x

and ast_to_include_declaration
  : Versions.V4_08.Include_declaration.t -> Compiler_types.include_declaration
  = fun x ->
    let concrete = Versions.V4_08.Include_declaration.to_concrete x in
    concrete_to_include_declaration concrete

and concrete_to_include_declaration
  : Versions.V4_08.Include_declaration.concrete -> Compiler_types.include_declaration
  = fun x ->
    (ast_to_include_infos ast_to_module_expr) x

and ast_of_with_constraint
  : Compiler_types.with_constraint -> Versions.V4_08.With_constraint.t
  = fun x ->
    Versions.V4_08.With_constraint.of_concrete (concrete_of_with_constraint x)

and concrete_of_with_constraint
  : Compiler_types.with_constraint -> Versions.V4_08.With_constraint.concrete
  = fun x ->
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

and ast_to_with_constraint
  : Versions.V4_08.With_constraint.t -> Compiler_types.with_constraint
  = fun x ->
    let concrete = Versions.V4_08.With_constraint.to_concrete x in
    concrete_to_with_constraint concrete

and concrete_to_with_constraint
  : Versions.V4_08.With_constraint.concrete -> Compiler_types.with_constraint
  = fun x ->
    match (x : Versions.V4_08.With_constraint.concrete) with
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

and ast_of_module_expr
  : Compiler_types.module_expr -> Versions.V4_08.Module_expr.t
  = fun x ->
    Versions.V4_08.Module_expr.of_concrete (concrete_of_module_expr x)

and concrete_of_module_expr
  : Compiler_types.module_expr -> Versions.V4_08.Module_expr.concrete
  = fun { pmod_desc; pmod_loc; pmod_attributes } ->
      let pmod_desc = ast_of_module_expr_desc pmod_desc in
      let pmod_attributes = ast_of_attributes pmod_attributes in
      { pmod_desc; pmod_loc; pmod_attributes }

and ast_to_module_expr
  : Versions.V4_08.Module_expr.t -> Compiler_types.module_expr
  = fun x ->
    let concrete = Versions.V4_08.Module_expr.to_concrete x in
    concrete_to_module_expr concrete

and concrete_to_module_expr
  : Versions.V4_08.Module_expr.concrete -> Compiler_types.module_expr
  = fun { pmod_desc; pmod_loc; pmod_attributes } ->
      let pmod_desc = ast_to_module_expr_desc pmod_desc in
      let pmod_attributes = ast_to_attributes pmod_attributes in
      { pmod_desc; pmod_loc; pmod_attributes }

and ast_of_module_expr_desc
  : Compiler_types.module_expr_desc -> Versions.V4_08.Module_expr_desc.t
  = fun x ->
    Versions.V4_08.Module_expr_desc.of_concrete (concrete_of_module_expr_desc x)

and concrete_of_module_expr_desc
  : Compiler_types.module_expr_desc -> Versions.V4_08.Module_expr_desc.concrete
  = fun x ->
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

and ast_to_module_expr_desc
  : Versions.V4_08.Module_expr_desc.t -> Compiler_types.module_expr_desc
  = fun x ->
    let concrete = Versions.V4_08.Module_expr_desc.to_concrete x in
    concrete_to_module_expr_desc concrete

and concrete_to_module_expr_desc
  : Versions.V4_08.Module_expr_desc.concrete -> Compiler_types.module_expr_desc
  = fun x ->
    match (x : Versions.V4_08.Module_expr_desc.concrete) with
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

and ast_of_structure
  : Compiler_types.structure -> Versions.V4_08.Structure.t
  = fun x ->
    Versions.V4_08.Structure.of_concrete (concrete_of_structure x)

and concrete_of_structure
  : Compiler_types.structure -> Versions.V4_08.Structure.concrete
  = fun x ->
    (List.map ~f:ast_of_structure_item) x

and ast_to_structure
  : Versions.V4_08.Structure.t -> Compiler_types.structure
  = fun x ->
    let concrete = Versions.V4_08.Structure.to_concrete x in
    concrete_to_structure concrete

and concrete_to_structure
  : Versions.V4_08.Structure.concrete -> Compiler_types.structure
  = fun x ->
    (List.map ~f:ast_to_structure_item) x

and ast_of_structure_item
  : Compiler_types.structure_item -> Versions.V4_08.Structure_item.t
  = fun x ->
    Versions.V4_08.Structure_item.of_concrete (concrete_of_structure_item x)

and concrete_of_structure_item
  : Compiler_types.structure_item -> Versions.V4_08.Structure_item.concrete
  = fun { pstr_desc; pstr_loc } ->
      let pstr_desc = ast_of_structure_item_desc pstr_desc in
      { pstr_desc; pstr_loc }

and ast_to_structure_item
  : Versions.V4_08.Structure_item.t -> Compiler_types.structure_item
  = fun x ->
    let concrete = Versions.V4_08.Structure_item.to_concrete x in
    concrete_to_structure_item concrete

and concrete_to_structure_item
  : Versions.V4_08.Structure_item.concrete -> Compiler_types.structure_item
  = fun { pstr_desc; pstr_loc } ->
      let pstr_desc = ast_to_structure_item_desc pstr_desc in
      { pstr_desc; pstr_loc }

and ast_of_structure_item_desc
  : Compiler_types.structure_item_desc -> Versions.V4_08.Structure_item_desc.t
  = fun x ->
    Versions.V4_08.Structure_item_desc.of_concrete (concrete_of_structure_item_desc x)

and concrete_of_structure_item_desc
  : Compiler_types.structure_item_desc -> Versions.V4_08.Structure_item_desc.concrete
  = fun x ->
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
      let x1 = ast_of_type_exception x1 in
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
      let x1 = ast_of_open_declaration x1 in
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

and ast_to_structure_item_desc
  : Versions.V4_08.Structure_item_desc.t -> Compiler_types.structure_item_desc
  = fun x ->
    let concrete = Versions.V4_08.Structure_item_desc.to_concrete x in
    concrete_to_structure_item_desc concrete

and concrete_to_structure_item_desc
  : Versions.V4_08.Structure_item_desc.concrete -> Compiler_types.structure_item_desc
  = fun x ->
    match (x : Versions.V4_08.Structure_item_desc.concrete) with
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
      let x1 = ast_to_type_exception x1 in
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
      let x1 = ast_to_open_declaration x1 in
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

and ast_of_value_binding
  : Compiler_types.value_binding -> Versions.V4_08.Value_binding.t
  = fun x ->
    Versions.V4_08.Value_binding.of_concrete (concrete_of_value_binding x)

and concrete_of_value_binding
  : Compiler_types.value_binding -> Versions.V4_08.Value_binding.concrete
  = fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } ->
      let pvb_pat = ast_of_pattern pvb_pat in
      let pvb_expr = ast_of_expression pvb_expr in
      let pvb_attributes = ast_of_attributes pvb_attributes in
      { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }

and ast_to_value_binding
  : Versions.V4_08.Value_binding.t -> Compiler_types.value_binding
  = fun x ->
    let concrete = Versions.V4_08.Value_binding.to_concrete x in
    concrete_to_value_binding concrete

and concrete_to_value_binding
  : Versions.V4_08.Value_binding.concrete -> Compiler_types.value_binding
  = fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } ->
      let pvb_pat = ast_to_pattern pvb_pat in
      let pvb_expr = ast_to_expression pvb_expr in
      let pvb_attributes = ast_to_attributes pvb_attributes in
      { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }

and ast_of_module_binding
  : Compiler_types.module_binding -> Versions.V4_08.Module_binding.t
  = fun x ->
    Versions.V4_08.Module_binding.of_concrete (concrete_of_module_binding x)

and concrete_of_module_binding
  : Compiler_types.module_binding -> Versions.V4_08.Module_binding.concrete
  = fun { pmb_name; pmb_expr; pmb_attributes; pmb_loc } ->
      let pmb_expr = ast_of_module_expr pmb_expr in
      let pmb_attributes = ast_of_attributes pmb_attributes in
      { pmb_name; pmb_expr; pmb_attributes; pmb_loc }

and ast_to_module_binding
  : Versions.V4_08.Module_binding.t -> Compiler_types.module_binding
  = fun x ->
    let concrete = Versions.V4_08.Module_binding.to_concrete x in
    concrete_to_module_binding concrete

and concrete_to_module_binding
  : Versions.V4_08.Module_binding.concrete -> Compiler_types.module_binding
  = fun { pmb_name; pmb_expr; pmb_attributes; pmb_loc } ->
      let pmb_expr = ast_to_module_expr pmb_expr in
      let pmb_attributes = ast_to_attributes pmb_attributes in
      { pmb_name; pmb_expr; pmb_attributes; pmb_loc }

and ast_of_toplevel_phrase
  : Compiler_types.toplevel_phrase -> Versions.V4_08.Toplevel_phrase.t
  = fun x ->
    Versions.V4_08.Toplevel_phrase.of_concrete (concrete_of_toplevel_phrase x)

and concrete_of_toplevel_phrase
  : Compiler_types.toplevel_phrase -> Versions.V4_08.Toplevel_phrase.concrete
  = fun x ->
    match (x : Compiler_types.toplevel_phrase) with
    | Ptop_def (x1) ->
      let x1 = ast_of_structure x1 in
      Ptop_def (x1)
    | Ptop_dir (x1) ->
      let x1 = ast_of_toplevel_directive x1 in
      Ptop_dir (x1)

and ast_to_toplevel_phrase
  : Versions.V4_08.Toplevel_phrase.t -> Compiler_types.toplevel_phrase
  = fun x ->
    let concrete = Versions.V4_08.Toplevel_phrase.to_concrete x in
    concrete_to_toplevel_phrase concrete

and concrete_to_toplevel_phrase
  : Versions.V4_08.Toplevel_phrase.concrete -> Compiler_types.toplevel_phrase
  = fun x ->
    match (x : Versions.V4_08.Toplevel_phrase.concrete) with
    | Ptop_def (x1) ->
      let x1 = ast_to_structure x1 in
      Ptop_def (x1)
    | Ptop_dir (x1) ->
      let x1 = ast_to_toplevel_directive x1 in
      Ptop_dir (x1)

and ast_of_toplevel_directive
  : Compiler_types.toplevel_directive -> Versions.V4_08.Toplevel_directive.t
  = fun x ->
    Versions.V4_08.Toplevel_directive.of_concrete (concrete_of_toplevel_directive x)

and concrete_of_toplevel_directive
  : Compiler_types.toplevel_directive -> Versions.V4_08.Toplevel_directive.concrete
  = fun { pdir_name; pdir_arg; pdir_loc } ->
      let pdir_arg = (Option.map ~f:ast_of_directive_argument) pdir_arg in
      { pdir_name; pdir_arg; pdir_loc }

and ast_to_toplevel_directive
  : Versions.V4_08.Toplevel_directive.t -> Compiler_types.toplevel_directive
  = fun x ->
    let concrete = Versions.V4_08.Toplevel_directive.to_concrete x in
    concrete_to_toplevel_directive concrete

and concrete_to_toplevel_directive
  : Versions.V4_08.Toplevel_directive.concrete -> Compiler_types.toplevel_directive
  = fun { pdir_name; pdir_arg; pdir_loc } ->
      let pdir_arg = (Option.map ~f:ast_to_directive_argument) pdir_arg in
      { pdir_name; pdir_arg; pdir_loc }

and ast_of_directive_argument
  : Compiler_types.directive_argument -> Versions.V4_08.Directive_argument.t
  = fun x ->
    Versions.V4_08.Directive_argument.of_concrete (concrete_of_directive_argument x)

and concrete_of_directive_argument
  : Compiler_types.directive_argument -> Versions.V4_08.Directive_argument.concrete
  = fun { pdira_desc; pdira_loc } ->
      let pdira_desc = ast_of_directive_argument_desc pdira_desc in
      { pdira_desc; pdira_loc }

and ast_to_directive_argument
  : Versions.V4_08.Directive_argument.t -> Compiler_types.directive_argument
  = fun x ->
    let concrete = Versions.V4_08.Directive_argument.to_concrete x in
    concrete_to_directive_argument concrete

and concrete_to_directive_argument
  : Versions.V4_08.Directive_argument.concrete -> Compiler_types.directive_argument
  = fun { pdira_desc; pdira_loc } ->
      let pdira_desc = ast_to_directive_argument_desc pdira_desc in
      { pdira_desc; pdira_loc }

and ast_of_directive_argument_desc
  : Compiler_types.directive_argument_desc -> Versions.V4_08.Directive_argument_desc.t
  = fun x ->
    Versions.V4_08.Directive_argument_desc.of_concrete (concrete_of_directive_argument_desc x)

and concrete_of_directive_argument_desc
  : Compiler_types.directive_argument_desc -> Versions.V4_08.Directive_argument_desc.concrete
  = fun x ->
    match (x : Compiler_types.directive_argument_desc) with
    | Pdir_string (x1) ->
      Pdir_string (x1)
    | Pdir_int (x1, x2) ->
      Pdir_int (x1, x2)
    | Pdir_ident (x1) ->
      let x1 = ast_of_longident x1 in
      Pdir_ident (x1)
    | Pdir_bool (x1) ->
      Pdir_bool (x1)

and ast_to_directive_argument_desc
  : Versions.V4_08.Directive_argument_desc.t -> Compiler_types.directive_argument_desc
  = fun x ->
    let concrete = Versions.V4_08.Directive_argument_desc.to_concrete x in
    concrete_to_directive_argument_desc concrete

and concrete_to_directive_argument_desc
  : Versions.V4_08.Directive_argument_desc.concrete -> Compiler_types.directive_argument_desc
  = fun x ->
    match (x : Versions.V4_08.Directive_argument_desc.concrete) with
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
