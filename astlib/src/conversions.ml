(*$ Astlib_src_cinaps.print_conversions_ml () *)
open! StdLabels

let rec longident_of_ast x =
  let of_concrete : Stable.V4_07.Longident.Concrete.t -> Astlib_parsetree.longident option =
    function
    | Lident { a } ->
      Some (Lident (a) : Astlib_parsetree.longident)
    | Ldot { a; b } ->
      Optional.bind (longident_of_ast a) ~f:(fun a ->
        Some (Ldot (a, b) : Astlib_parsetree.longident))
    | Lapply { a; b } ->
      Optional.bind (longident_of_ast a) ~f:(fun a ->
        Optional.bind (longident_of_ast b) ~f:(fun b ->
          Some (Lapply (a, b) : Astlib_parsetree.longident)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Longident.to_concrete x)

and longident_loc_of_ast x =
  let of_concrete : Stable.V4_07.Longident_loc.Concrete.t -> Astlib_parsetree.longident_loc option =
    fun (Longident_loc { txt; loc }) ->
      Optional.bind (longident_of_ast txt) ~f:(fun txt ->
        Some ({ txt; loc } : Astlib_parsetree.longident_loc))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Longident_loc.to_concrete x)

and rec_flag_of_ast x =
  let of_concrete : Stable.V4_07.Rec_flag.Concrete.t -> Astlib_parsetree.rec_flag option =
    function
    | Nonrecursive ->
      Some (Nonrecursive : Astlib_parsetree.rec_flag)
    | Recursive ->
      Some (Recursive : Astlib_parsetree.rec_flag)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Rec_flag.to_concrete x)

and direction_flag_of_ast x =
  let of_concrete : Stable.V4_07.Direction_flag.Concrete.t -> Astlib_parsetree.direction_flag option =
    function
    | Upto ->
      Some (Upto : Astlib_parsetree.direction_flag)
    | Downto ->
      Some (Downto : Astlib_parsetree.direction_flag)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Direction_flag.to_concrete x)

and private_flag_of_ast x =
  let of_concrete : Stable.V4_07.Private_flag.Concrete.t -> Astlib_parsetree.private_flag option =
    function
    | Private ->
      Some (Private : Astlib_parsetree.private_flag)
    | Public ->
      Some (Public : Astlib_parsetree.private_flag)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Private_flag.to_concrete x)

and mutable_flag_of_ast x =
  let of_concrete : Stable.V4_07.Mutable_flag.Concrete.t -> Astlib_parsetree.mutable_flag option =
    function
    | Immutable ->
      Some (Immutable : Astlib_parsetree.mutable_flag)
    | Mutable ->
      Some (Mutable : Astlib_parsetree.mutable_flag)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Mutable_flag.to_concrete x)

and virtual_flag_of_ast x =
  let of_concrete : Stable.V4_07.Virtual_flag.Concrete.t -> Astlib_parsetree.virtual_flag option =
    function
    | Virtual ->
      Some (Virtual : Astlib_parsetree.virtual_flag)
    | Concrete ->
      Some (Concrete : Astlib_parsetree.virtual_flag)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Virtual_flag.to_concrete x)

and override_flag_of_ast x =
  let of_concrete : Stable.V4_07.Override_flag.Concrete.t -> Astlib_parsetree.override_flag option =
    function
    | Override ->
      Some (Override : Astlib_parsetree.override_flag)
    | Fresh ->
      Some (Fresh : Astlib_parsetree.override_flag)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Override_flag.to_concrete x)

and closed_flag_of_ast x =
  let of_concrete : Stable.V4_07.Closed_flag.Concrete.t -> Astlib_parsetree.closed_flag option =
    function
    | Closed ->
      Some (Closed : Astlib_parsetree.closed_flag)
    | Open ->
      Some (Open : Astlib_parsetree.closed_flag)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Closed_flag.to_concrete x)

and label_of_ast x =
  let of_concrete : Stable.V4_07.Label.Concrete.t -> Astlib_parsetree.label option =
    fun (Label { a }) ->
      Some a
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Label.to_concrete x)

and label_loc_of_ast x =
  let of_concrete : Stable.V4_07.Label_loc.Concrete.t -> Astlib_parsetree.label_loc option =
    fun (Label_loc { txt; loc }) ->
      Optional.bind (label_of_ast txt) ~f:(fun txt ->
        Some ({ txt; loc } : Astlib_parsetree.label_loc))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Label_loc.to_concrete x)

and string_loc_of_ast x =
  let of_concrete : Stable.V4_07.String_loc.Concrete.t -> Astlib_parsetree.string_loc option =
    fun (String_loc { txt; loc }) ->
      Some ({ txt; loc } : Astlib_parsetree.string_loc)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.String_loc.to_concrete x)

and arg_label_of_ast x =
  let of_concrete : Stable.V4_07.Arg_label.Concrete.t -> Astlib_parsetree.arg_label option =
    function
    | Nolabel ->
      Some (Nolabel : Astlib_parsetree.arg_label)
    | Labelled { a } ->
      Some (Labelled (a) : Astlib_parsetree.arg_label)
    | Optional { a } ->
      Some (Optional (a) : Astlib_parsetree.arg_label)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Arg_label.to_concrete x)

and variance_of_ast x =
  let of_concrete : Stable.V4_07.Variance.Concrete.t -> Astlib_parsetree.variance option =
    function
    | Covariant ->
      Some (Covariant : Astlib_parsetree.variance)
    | Contravariant ->
      Some (Contravariant : Astlib_parsetree.variance)
    | Invariant ->
      Some (Invariant : Astlib_parsetree.variance)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Variance.to_concrete x)

and constant_of_ast x =
  let of_concrete : Stable.V4_07.Constant.Concrete.t -> Astlib_parsetree.constant option =
    function
    | Pconst_integer { a; b } ->
      Some (Pconst_integer (a, b) : Astlib_parsetree.constant)
    | Pconst_char { a } ->
      Some (Pconst_char (a) : Astlib_parsetree.constant)
    | Pconst_string { a; b } ->
      Some (Pconst_string (a, b) : Astlib_parsetree.constant)
    | Pconst_float { a; b } ->
      Some (Pconst_float (a, b) : Astlib_parsetree.constant)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Constant.to_concrete x)

and attribute_of_ast x =
  let of_concrete : Stable.V4_07.Attribute.Concrete.t -> Astlib_parsetree.attribute option =
    fun (Attribute { a; b }) ->
      Optional.bind (string_loc_of_ast a) ~f:(fun a ->
        Optional.bind (payload_of_ast b) ~f:(fun b ->
          Some (a, b)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Attribute.to_concrete x)

and extension_of_ast x =
  let of_concrete : Stable.V4_07.Extension.Concrete.t -> Astlib_parsetree.extension option =
    fun (Extension { a; b }) ->
      Optional.bind (string_loc_of_ast a) ~f:(fun a ->
        Optional.bind (payload_of_ast b) ~f:(fun b ->
          Some (a, b)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Extension.to_concrete x)

and attributes_of_ast x =
  let of_concrete : Stable.V4_07.Attributes.Concrete.t -> Astlib_parsetree.attributes option =
    fun (Attributes { a }) ->
      Optional.bind (Optional.List.map ~f:(attribute_of_ast) a) ~f:(fun a ->
        Some a)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Attributes.to_concrete x)

and payload_of_ast x =
  let of_concrete : Stable.V4_07.Payload.Concrete.t -> Astlib_parsetree.payload option =
    function
    | PStr { a } ->
      Optional.bind (structure_of_ast a) ~f:(fun a ->
        Some (PStr (a) : Astlib_parsetree.payload))
    | PSig { a } ->
      Optional.bind (signature_of_ast a) ~f:(fun a ->
        Some (PSig (a) : Astlib_parsetree.payload))
    | PTyp { a } ->
      Optional.bind (core_type_of_ast a) ~f:(fun a ->
        Some (PTyp (a) : Astlib_parsetree.payload))
    | PPat { a; b } ->
      Optional.bind (pattern_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.Option.map ~f:(expression_of_ast) b) ~f:(fun b ->
          Some (PPat (a, b) : Astlib_parsetree.payload)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Payload.to_concrete x)

and core_type_of_ast x =
  let of_concrete : Stable.V4_07.Core_type.Concrete.t -> Astlib_parsetree.core_type option =
    fun (Core_type { ptyp_desc; ptyp_loc; ptyp_attributes }) ->
      Optional.bind (core_type_desc_of_ast ptyp_desc) ~f:(fun ptyp_desc ->
        Optional.bind (attributes_of_ast ptyp_attributes) ~f:(fun ptyp_attributes ->
          Some ({ ptyp_desc; ptyp_loc; ptyp_attributes } : Astlib_parsetree.core_type)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Core_type.to_concrete x)

and core_type_desc_of_ast x =
  let of_concrete : Stable.V4_07.Core_type_desc.Concrete.t -> Astlib_parsetree.core_type_desc option =
    function
    | Ptyp_any ->
      Some (Ptyp_any : Astlib_parsetree.core_type_desc)
    | Ptyp_var { a } ->
      Some (Ptyp_var (a) : Astlib_parsetree.core_type_desc)
    | Ptyp_arrow { a; b; c } ->
      Optional.bind (arg_label_of_ast a) ~f:(fun a ->
        Optional.bind (core_type_of_ast b) ~f:(fun b ->
          Optional.bind (core_type_of_ast c) ~f:(fun c ->
            Some (Ptyp_arrow (a, b, c) : Astlib_parsetree.core_type_desc))))
    | Ptyp_tuple { a } ->
      Optional.bind (Optional.List.map ~f:(core_type_of_ast) a) ~f:(fun a ->
        Some (Ptyp_tuple (a) : Astlib_parsetree.core_type_desc))
    | Ptyp_constr { a; b } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.List.map ~f:(core_type_of_ast) b) ~f:(fun b ->
          Some (Ptyp_constr (a, b) : Astlib_parsetree.core_type_desc)))
    | Ptyp_object { a; b } ->
      Optional.bind (Optional.List.map ~f:(object_field_of_ast) a) ~f:(fun a ->
        Optional.bind (closed_flag_of_ast b) ~f:(fun b ->
          Some (Ptyp_object (a, b) : Astlib_parsetree.core_type_desc)))
    | Ptyp_class { a; b } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.List.map ~f:(core_type_of_ast) b) ~f:(fun b ->
          Some (Ptyp_class (a, b) : Astlib_parsetree.core_type_desc)))
    | Ptyp_alias { a; b } ->
      Optional.bind (core_type_of_ast a) ~f:(fun a ->
        Some (Ptyp_alias (a, b) : Astlib_parsetree.core_type_desc))
    | Ptyp_variant { a; b; c } ->
      Optional.bind (Optional.List.map ~f:(row_field_of_ast) a) ~f:(fun a ->
        Optional.bind (closed_flag_of_ast b) ~f:(fun b ->
          Optional.bind (Optional.Option.map ~f:(Optional.List.map ~f:(label_of_ast)) c) ~f:(fun c ->
            Some (Ptyp_variant (a, b, c) : Astlib_parsetree.core_type_desc))))
    | Ptyp_poly { a; b } ->
      Optional.bind (Optional.List.map ~f:(string_loc_of_ast) a) ~f:(fun a ->
        Optional.bind (core_type_of_ast b) ~f:(fun b ->
          Some (Ptyp_poly (a, b) : Astlib_parsetree.core_type_desc)))
    | Ptyp_package { a } ->
      Optional.bind (package_type_of_ast a) ~f:(fun a ->
        Some (Ptyp_package (a) : Astlib_parsetree.core_type_desc))
    | Ptyp_extension { a } ->
      Optional.bind (extension_of_ast a) ~f:(fun a ->
        Some (Ptyp_extension (a) : Astlib_parsetree.core_type_desc))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Core_type_desc.to_concrete x)

and package_type_of_ast x =
  let of_concrete : Stable.V4_07.Package_type.Concrete.t -> Astlib_parsetree.package_type option =
    fun (Package_type { a; b }) ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.List.map ~f:(package_type_constraint_of_ast) b) ~f:(fun b ->
          Some (a, b)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Package_type.to_concrete x)

and package_type_constraint_of_ast x =
  let of_concrete : Stable.V4_07.Package_type_constraint.Concrete.t -> Astlib_parsetree.package_type_constraint option =
    fun (Package_type_constraint { a; b }) ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Optional.bind (core_type_of_ast b) ~f:(fun b ->
          Some (a, b)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Package_type_constraint.to_concrete x)

and row_field_of_ast x =
  let of_concrete : Stable.V4_07.Row_field.Concrete.t -> Astlib_parsetree.row_field option =
    function
    | Rtag { a; b; c; d } ->
      Optional.bind (label_loc_of_ast a) ~f:(fun a ->
        Optional.bind (attributes_of_ast b) ~f:(fun b ->
          Optional.bind (Optional.List.map ~f:(core_type_of_ast) d) ~f:(fun d ->
            Some (Rtag (a, b, c, d) : Astlib_parsetree.row_field))))
    | Rinherit { a } ->
      Optional.bind (core_type_of_ast a) ~f:(fun a ->
        Some (Rinherit (a) : Astlib_parsetree.row_field))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Row_field.to_concrete x)

and object_field_of_ast x =
  let of_concrete : Stable.V4_07.Object_field.Concrete.t -> Astlib_parsetree.object_field option =
    function
    | Otag { a; b; c } ->
      Optional.bind (label_loc_of_ast a) ~f:(fun a ->
        Optional.bind (attributes_of_ast b) ~f:(fun b ->
          Optional.bind (core_type_of_ast c) ~f:(fun c ->
            Some (Otag (a, b, c) : Astlib_parsetree.object_field))))
    | Oinherit { a } ->
      Optional.bind (core_type_of_ast a) ~f:(fun a ->
        Some (Oinherit (a) : Astlib_parsetree.object_field))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Object_field.to_concrete x)

and pattern_of_ast x =
  let of_concrete : Stable.V4_07.Pattern.Concrete.t -> Astlib_parsetree.pattern option =
    fun (Pattern { ppat_desc; ppat_loc; ppat_attributes }) ->
      Optional.bind (pattern_desc_of_ast ppat_desc) ~f:(fun ppat_desc ->
        Optional.bind (attributes_of_ast ppat_attributes) ~f:(fun ppat_attributes ->
          Some ({ ppat_desc; ppat_loc; ppat_attributes } : Astlib_parsetree.pattern)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Pattern.to_concrete x)

and pattern_desc_of_ast x =
  let of_concrete : Stable.V4_07.Pattern_desc.Concrete.t -> Astlib_parsetree.pattern_desc option =
    function
    | Ppat_any ->
      Some (Ppat_any : Astlib_parsetree.pattern_desc)
    | Ppat_var { a } ->
      Optional.bind (string_loc_of_ast a) ~f:(fun a ->
        Some (Ppat_var (a) : Astlib_parsetree.pattern_desc))
    | Ppat_alias { a; b } ->
      Optional.bind (pattern_of_ast a) ~f:(fun a ->
        Optional.bind (string_loc_of_ast b) ~f:(fun b ->
          Some (Ppat_alias (a, b) : Astlib_parsetree.pattern_desc)))
    | Ppat_constant { a } ->
      Optional.bind (constant_of_ast a) ~f:(fun a ->
        Some (Ppat_constant (a) : Astlib_parsetree.pattern_desc))
    | Ppat_interval { a; b } ->
      Optional.bind (constant_of_ast a) ~f:(fun a ->
        Optional.bind (constant_of_ast b) ~f:(fun b ->
          Some (Ppat_interval (a, b) : Astlib_parsetree.pattern_desc)))
    | Ppat_tuple { a } ->
      Optional.bind (Optional.List.map ~f:(pattern_of_ast) a) ~f:(fun a ->
        Some (Ppat_tuple (a) : Astlib_parsetree.pattern_desc))
    | Ppat_construct { a; b } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.Option.map ~f:(pattern_of_ast) b) ~f:(fun b ->
          Some (Ppat_construct (a, b) : Astlib_parsetree.pattern_desc)))
    | Ppat_variant { a; b } ->
      Optional.bind (label_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.Option.map ~f:(pattern_of_ast) b) ~f:(fun b ->
          Some (Ppat_variant (a, b) : Astlib_parsetree.pattern_desc)))
    | Ppat_record { a; b } ->
      Optional.bind (Optional.List.map ~f:(record_field_pattern_of_ast) a) ~f:(fun a ->
        Optional.bind (closed_flag_of_ast b) ~f:(fun b ->
          Some (Ppat_record (a, b) : Astlib_parsetree.pattern_desc)))
    | Ppat_array { a } ->
      Optional.bind (Optional.List.map ~f:(pattern_of_ast) a) ~f:(fun a ->
        Some (Ppat_array (a) : Astlib_parsetree.pattern_desc))
    | Ppat_or { a; b } ->
      Optional.bind (pattern_of_ast a) ~f:(fun a ->
        Optional.bind (pattern_of_ast b) ~f:(fun b ->
          Some (Ppat_or (a, b) : Astlib_parsetree.pattern_desc)))
    | Ppat_constraint { a; b } ->
      Optional.bind (pattern_of_ast a) ~f:(fun a ->
        Optional.bind (core_type_of_ast b) ~f:(fun b ->
          Some (Ppat_constraint (a, b) : Astlib_parsetree.pattern_desc)))
    | Ppat_type { a } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Some (Ppat_type (a) : Astlib_parsetree.pattern_desc))
    | Ppat_lazy { a } ->
      Optional.bind (pattern_of_ast a) ~f:(fun a ->
        Some (Ppat_lazy (a) : Astlib_parsetree.pattern_desc))
    | Ppat_unpack { a } ->
      Optional.bind (string_loc_of_ast a) ~f:(fun a ->
        Some (Ppat_unpack (a) : Astlib_parsetree.pattern_desc))
    | Ppat_exception { a } ->
      Optional.bind (pattern_of_ast a) ~f:(fun a ->
        Some (Ppat_exception (a) : Astlib_parsetree.pattern_desc))
    | Ppat_extension { a } ->
      Optional.bind (extension_of_ast a) ~f:(fun a ->
        Some (Ppat_extension (a) : Astlib_parsetree.pattern_desc))
    | Ppat_open { a; b } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Optional.bind (pattern_of_ast b) ~f:(fun b ->
          Some (Ppat_open (a, b) : Astlib_parsetree.pattern_desc)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Pattern_desc.to_concrete x)

and record_field_pattern_of_ast x =
  let of_concrete : Stable.V4_07.Record_field_pattern.Concrete.t -> Astlib_parsetree.record_field_pattern option =
    fun (Record_field_pattern { a; b }) ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Optional.bind (pattern_of_ast b) ~f:(fun b ->
          Some (a, b)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Record_field_pattern.to_concrete x)

and expression_of_ast x =
  let of_concrete : Stable.V4_07.Expression.Concrete.t -> Astlib_parsetree.expression option =
    fun (Expression { pexp_desc; pexp_loc; pexp_attributes }) ->
      Optional.bind (expression_desc_of_ast pexp_desc) ~f:(fun pexp_desc ->
        Optional.bind (attributes_of_ast pexp_attributes) ~f:(fun pexp_attributes ->
          Some ({ pexp_desc; pexp_loc; pexp_attributes } : Astlib_parsetree.expression)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Expression.to_concrete x)

and expression_desc_of_ast x =
  let of_concrete : Stable.V4_07.Expression_desc.Concrete.t -> Astlib_parsetree.expression_desc option =
    function
    | Pexp_ident { a } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Some (Pexp_ident (a) : Astlib_parsetree.expression_desc))
    | Pexp_constant { a } ->
      Optional.bind (constant_of_ast a) ~f:(fun a ->
        Some (Pexp_constant (a) : Astlib_parsetree.expression_desc))
    | Pexp_let { a; b; c } ->
      Optional.bind (rec_flag_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.List.map ~f:(value_binding_of_ast) b) ~f:(fun b ->
          Optional.bind (expression_of_ast c) ~f:(fun c ->
            Some (Pexp_let (a, b, c) : Astlib_parsetree.expression_desc))))
    | Pexp_function { a } ->
      Optional.bind (Optional.List.map ~f:(case_of_ast) a) ~f:(fun a ->
        Some (Pexp_function (a) : Astlib_parsetree.expression_desc))
    | Pexp_fun { a; b; c; d } ->
      Optional.bind (arg_label_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.Option.map ~f:(expression_of_ast) b) ~f:(fun b ->
          Optional.bind (pattern_of_ast c) ~f:(fun c ->
            Optional.bind (expression_of_ast d) ~f:(fun d ->
              Some (Pexp_fun (a, b, c, d) : Astlib_parsetree.expression_desc)))))
    | Pexp_apply { a; b } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.List.map ~f:(apply_arg_of_ast) b) ~f:(fun b ->
          Some (Pexp_apply (a, b) : Astlib_parsetree.expression_desc)))
    | Pexp_match { a; b } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.List.map ~f:(case_of_ast) b) ~f:(fun b ->
          Some (Pexp_match (a, b) : Astlib_parsetree.expression_desc)))
    | Pexp_try { a; b } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.List.map ~f:(case_of_ast) b) ~f:(fun b ->
          Some (Pexp_try (a, b) : Astlib_parsetree.expression_desc)))
    | Pexp_tuple { a } ->
      Optional.bind (Optional.List.map ~f:(expression_of_ast) a) ~f:(fun a ->
        Some (Pexp_tuple (a) : Astlib_parsetree.expression_desc))
    | Pexp_construct { a; b } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.Option.map ~f:(expression_of_ast) b) ~f:(fun b ->
          Some (Pexp_construct (a, b) : Astlib_parsetree.expression_desc)))
    | Pexp_variant { a; b } ->
      Optional.bind (label_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.Option.map ~f:(expression_of_ast) b) ~f:(fun b ->
          Some (Pexp_variant (a, b) : Astlib_parsetree.expression_desc)))
    | Pexp_record { a; b } ->
      Optional.bind (Optional.List.map ~f:(record_field_expression_of_ast) a) ~f:(fun a ->
        Optional.bind (Optional.Option.map ~f:(expression_of_ast) b) ~f:(fun b ->
          Some (Pexp_record (a, b) : Astlib_parsetree.expression_desc)))
    | Pexp_field { a; b } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Optional.bind (longident_loc_of_ast b) ~f:(fun b ->
          Some (Pexp_field (a, b) : Astlib_parsetree.expression_desc)))
    | Pexp_setfield { a; b; c } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Optional.bind (longident_loc_of_ast b) ~f:(fun b ->
          Optional.bind (expression_of_ast c) ~f:(fun c ->
            Some (Pexp_setfield (a, b, c) : Astlib_parsetree.expression_desc))))
    | Pexp_array { a } ->
      Optional.bind (Optional.List.map ~f:(expression_of_ast) a) ~f:(fun a ->
        Some (Pexp_array (a) : Astlib_parsetree.expression_desc))
    | Pexp_ifthenelse { a; b; c } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Optional.bind (expression_of_ast b) ~f:(fun b ->
          Optional.bind (Optional.Option.map ~f:(expression_of_ast) c) ~f:(fun c ->
            Some (Pexp_ifthenelse (a, b, c) : Astlib_parsetree.expression_desc))))
    | Pexp_sequence { a; b } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Optional.bind (expression_of_ast b) ~f:(fun b ->
          Some (Pexp_sequence (a, b) : Astlib_parsetree.expression_desc)))
    | Pexp_while { a; b } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Optional.bind (expression_of_ast b) ~f:(fun b ->
          Some (Pexp_while (a, b) : Astlib_parsetree.expression_desc)))
    | Pexp_for { a; b; c; d; e } ->
      Optional.bind (pattern_of_ast a) ~f:(fun a ->
        Optional.bind (expression_of_ast b) ~f:(fun b ->
          Optional.bind (expression_of_ast c) ~f:(fun c ->
            Optional.bind (direction_flag_of_ast d) ~f:(fun d ->
              Optional.bind (expression_of_ast e) ~f:(fun e ->
                Some (Pexp_for (a, b, c, d, e) : Astlib_parsetree.expression_desc))))))
    | Pexp_constraint { a; b } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Optional.bind (core_type_of_ast b) ~f:(fun b ->
          Some (Pexp_constraint (a, b) : Astlib_parsetree.expression_desc)))
    | Pexp_coerce { a; b; c } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.Option.map ~f:(core_type_of_ast) b) ~f:(fun b ->
          Optional.bind (core_type_of_ast c) ~f:(fun c ->
            Some (Pexp_coerce (a, b, c) : Astlib_parsetree.expression_desc))))
    | Pexp_send { a; b } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Optional.bind (label_loc_of_ast b) ~f:(fun b ->
          Some (Pexp_send (a, b) : Astlib_parsetree.expression_desc)))
    | Pexp_new { a } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Some (Pexp_new (a) : Astlib_parsetree.expression_desc))
    | Pexp_setinstvar { a; b } ->
      Optional.bind (label_loc_of_ast a) ~f:(fun a ->
        Optional.bind (expression_of_ast b) ~f:(fun b ->
          Some (Pexp_setinstvar (a, b) : Astlib_parsetree.expression_desc)))
    | Pexp_override { a } ->
      Optional.bind (Optional.List.map ~f:(override_expression_of_ast) a) ~f:(fun a ->
        Some (Pexp_override (a) : Astlib_parsetree.expression_desc))
    | Pexp_letmodule { a; b; c } ->
      Optional.bind (string_loc_of_ast a) ~f:(fun a ->
        Optional.bind (module_expr_of_ast b) ~f:(fun b ->
          Optional.bind (expression_of_ast c) ~f:(fun c ->
            Some (Pexp_letmodule (a, b, c) : Astlib_parsetree.expression_desc))))
    | Pexp_letexception { a; b } ->
      Optional.bind (extension_constructor_of_ast a) ~f:(fun a ->
        Optional.bind (expression_of_ast b) ~f:(fun b ->
          Some (Pexp_letexception (a, b) : Astlib_parsetree.expression_desc)))
    | Pexp_assert { a } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Some (Pexp_assert (a) : Astlib_parsetree.expression_desc))
    | Pexp_lazy { a } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Some (Pexp_lazy (a) : Astlib_parsetree.expression_desc))
    | Pexp_poly { a; b } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.Option.map ~f:(core_type_of_ast) b) ~f:(fun b ->
          Some (Pexp_poly (a, b) : Astlib_parsetree.expression_desc)))
    | Pexp_object { a } ->
      Optional.bind (class_structure_of_ast a) ~f:(fun a ->
        Some (Pexp_object (a) : Astlib_parsetree.expression_desc))
    | Pexp_newtype { a; b } ->
      Optional.bind (string_loc_of_ast a) ~f:(fun a ->
        Optional.bind (expression_of_ast b) ~f:(fun b ->
          Some (Pexp_newtype (a, b) : Astlib_parsetree.expression_desc)))
    | Pexp_pack { a } ->
      Optional.bind (module_expr_of_ast a) ~f:(fun a ->
        Some (Pexp_pack (a) : Astlib_parsetree.expression_desc))
    | Pexp_open { a; b; c } ->
      Optional.bind (override_flag_of_ast a) ~f:(fun a ->
        Optional.bind (longident_loc_of_ast b) ~f:(fun b ->
          Optional.bind (expression_of_ast c) ~f:(fun c ->
            Some (Pexp_open (a, b, c) : Astlib_parsetree.expression_desc))))
    | Pexp_extension { a } ->
      Optional.bind (extension_of_ast a) ~f:(fun a ->
        Some (Pexp_extension (a) : Astlib_parsetree.expression_desc))
    | Pexp_unreachable ->
      Some (Pexp_unreachable : Astlib_parsetree.expression_desc)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Expression_desc.to_concrete x)

and override_expression_of_ast x =
  let of_concrete : Stable.V4_07.Override_expression.Concrete.t -> Astlib_parsetree.override_expression option =
    fun (Override_expression { a; b }) ->
      Optional.bind (label_loc_of_ast a) ~f:(fun a ->
        Optional.bind (expression_of_ast b) ~f:(fun b ->
          Some (a, b)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Override_expression.to_concrete x)

and record_field_expression_of_ast x =
  let of_concrete : Stable.V4_07.Record_field_expression.Concrete.t -> Astlib_parsetree.record_field_expression option =
    fun (Record_field_expression { a; b }) ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Optional.bind (expression_of_ast b) ~f:(fun b ->
          Some (a, b)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Record_field_expression.to_concrete x)

and apply_arg_of_ast x =
  let of_concrete : Stable.V4_07.Apply_arg.Concrete.t -> Astlib_parsetree.apply_arg option =
    fun (Apply_arg { a; b }) ->
      Optional.bind (arg_label_of_ast a) ~f:(fun a ->
        Optional.bind (expression_of_ast b) ~f:(fun b ->
          Some (a, b)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Apply_arg.to_concrete x)

and case_of_ast x =
  let of_concrete : Stable.V4_07.Case.Concrete.t -> Astlib_parsetree.case option =
    fun (Case { pc_lhs; pc_guard; pc_rhs }) ->
      Optional.bind (pattern_of_ast pc_lhs) ~f:(fun pc_lhs ->
        Optional.bind (Optional.Option.map ~f:(expression_of_ast) pc_guard) ~f:(fun pc_guard ->
          Optional.bind (expression_of_ast pc_rhs) ~f:(fun pc_rhs ->
            Some ({ pc_lhs; pc_guard; pc_rhs } : Astlib_parsetree.case))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Case.to_concrete x)

and value_description_of_ast x =
  let of_concrete : Stable.V4_07.Value_description.Concrete.t -> Astlib_parsetree.value_description option =
    fun (Value_description { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }) ->
      Optional.bind (string_loc_of_ast pval_name) ~f:(fun pval_name ->
        Optional.bind (core_type_of_ast pval_type) ~f:(fun pval_type ->
          Optional.bind (attributes_of_ast pval_attributes) ~f:(fun pval_attributes ->
            Some ({ pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Astlib_parsetree.value_description))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Value_description.to_concrete x)

and type_declaration_of_ast x =
  let of_concrete : Stable.V4_07.Type_declaration.Concrete.t -> Astlib_parsetree.type_declaration option =
    fun (Type_declaration { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc }) ->
      Optional.bind (string_loc_of_ast ptype_name) ~f:(fun ptype_name ->
        Optional.bind (Optional.List.map ~f:(type_param_of_ast) ptype_params) ~f:(fun ptype_params ->
          Optional.bind (Optional.List.map ~f:(type_constraint_of_ast) ptype_cstrs) ~f:(fun ptype_cstrs ->
            Optional.bind (type_kind_of_ast ptype_kind) ~f:(fun ptype_kind ->
              Optional.bind (private_flag_of_ast ptype_private) ~f:(fun ptype_private ->
                Optional.bind (Optional.Option.map ~f:(core_type_of_ast) ptype_manifest) ~f:(fun ptype_manifest ->
                  Optional.bind (attributes_of_ast ptype_attributes) ~f:(fun ptype_attributes ->
                    Some ({ ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Astlib_parsetree.type_declaration))))))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Type_declaration.to_concrete x)

and type_param_of_ast x =
  let of_concrete : Stable.V4_07.Type_param.Concrete.t -> Astlib_parsetree.type_param option =
    fun (Type_param { a; b }) ->
      Optional.bind (core_type_of_ast a) ~f:(fun a ->
        Optional.bind (variance_of_ast b) ~f:(fun b ->
          Some (a, b)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Type_param.to_concrete x)

and type_constraint_of_ast x =
  let of_concrete : Stable.V4_07.Type_constraint.Concrete.t -> Astlib_parsetree.type_constraint option =
    fun (Type_constraint { a; b; c }) ->
      Optional.bind (core_type_of_ast a) ~f:(fun a ->
        Optional.bind (core_type_of_ast b) ~f:(fun b ->
          Some (a, b, c)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Type_constraint.to_concrete x)

and type_kind_of_ast x =
  let of_concrete : Stable.V4_07.Type_kind.Concrete.t -> Astlib_parsetree.type_kind option =
    function
    | Ptype_abstract ->
      Some (Ptype_abstract : Astlib_parsetree.type_kind)
    | Ptype_variant { a } ->
      Optional.bind (Optional.List.map ~f:(constructor_declaration_of_ast) a) ~f:(fun a ->
        Some (Ptype_variant (a) : Astlib_parsetree.type_kind))
    | Ptype_record { a } ->
      Optional.bind (Optional.List.map ~f:(label_declaration_of_ast) a) ~f:(fun a ->
        Some (Ptype_record (a) : Astlib_parsetree.type_kind))
    | Ptype_open ->
      Some (Ptype_open : Astlib_parsetree.type_kind)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Type_kind.to_concrete x)

and label_declaration_of_ast x =
  let of_concrete : Stable.V4_07.Label_declaration.Concrete.t -> Astlib_parsetree.label_declaration option =
    fun (Label_declaration { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }) ->
      Optional.bind (string_loc_of_ast pld_name) ~f:(fun pld_name ->
        Optional.bind (mutable_flag_of_ast pld_mutable) ~f:(fun pld_mutable ->
          Optional.bind (core_type_of_ast pld_type) ~f:(fun pld_type ->
            Optional.bind (attributes_of_ast pld_attributes) ~f:(fun pld_attributes ->
              Some ({ pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Astlib_parsetree.label_declaration)))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Label_declaration.to_concrete x)

and constructor_declaration_of_ast x =
  let of_concrete : Stable.V4_07.Constructor_declaration.Concrete.t -> Astlib_parsetree.constructor_declaration option =
    fun (Constructor_declaration { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }) ->
      Optional.bind (string_loc_of_ast pcd_name) ~f:(fun pcd_name ->
        Optional.bind (constructor_arguments_of_ast pcd_args) ~f:(fun pcd_args ->
          Optional.bind (Optional.Option.map ~f:(core_type_of_ast) pcd_res) ~f:(fun pcd_res ->
            Optional.bind (attributes_of_ast pcd_attributes) ~f:(fun pcd_attributes ->
              Some ({ pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Astlib_parsetree.constructor_declaration)))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Constructor_declaration.to_concrete x)

and constructor_arguments_of_ast x =
  let of_concrete : Stable.V4_07.Constructor_arguments.Concrete.t -> Astlib_parsetree.constructor_arguments option =
    function
    | Pcstr_tuple { a } ->
      Optional.bind (Optional.List.map ~f:(core_type_of_ast) a) ~f:(fun a ->
        Some (Pcstr_tuple (a) : Astlib_parsetree.constructor_arguments))
    | Pcstr_record { a } ->
      Optional.bind (Optional.List.map ~f:(label_declaration_of_ast) a) ~f:(fun a ->
        Some (Pcstr_record (a) : Astlib_parsetree.constructor_arguments))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Constructor_arguments.to_concrete x)

and type_extension_of_ast x =
  let of_concrete : Stable.V4_07.Type_extension.Concrete.t -> Astlib_parsetree.type_extension option =
    fun (Type_extension { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes }) ->
      Optional.bind (longident_loc_of_ast ptyext_path) ~f:(fun ptyext_path ->
        Optional.bind (Optional.List.map ~f:(type_param_of_ast) ptyext_params) ~f:(fun ptyext_params ->
          Optional.bind (Optional.List.map ~f:(extension_constructor_of_ast) ptyext_constructors) ~f:(fun ptyext_constructors ->
            Optional.bind (private_flag_of_ast ptyext_private) ~f:(fun ptyext_private ->
              Optional.bind (attributes_of_ast ptyext_attributes) ~f:(fun ptyext_attributes ->
                Some ({ ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Astlib_parsetree.type_extension))))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Type_extension.to_concrete x)

and extension_constructor_of_ast x =
  let of_concrete : Stable.V4_07.Extension_constructor.Concrete.t -> Astlib_parsetree.extension_constructor option =
    fun (Extension_constructor { pext_name; pext_kind; pext_loc; pext_attributes }) ->
      Optional.bind (string_loc_of_ast pext_name) ~f:(fun pext_name ->
        Optional.bind (extension_constructor_kind_of_ast pext_kind) ~f:(fun pext_kind ->
          Optional.bind (attributes_of_ast pext_attributes) ~f:(fun pext_attributes ->
            Some ({ pext_name; pext_kind; pext_loc; pext_attributes } : Astlib_parsetree.extension_constructor))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Extension_constructor.to_concrete x)

and extension_constructor_kind_of_ast x =
  let of_concrete : Stable.V4_07.Extension_constructor_kind.Concrete.t -> Astlib_parsetree.extension_constructor_kind option =
    function
    | Pext_decl { a; b } ->
      Optional.bind (constructor_arguments_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.Option.map ~f:(core_type_of_ast) b) ~f:(fun b ->
          Some (Pext_decl (a, b) : Astlib_parsetree.extension_constructor_kind)))
    | Pext_rebind { a } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Some (Pext_rebind (a) : Astlib_parsetree.extension_constructor_kind))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Extension_constructor_kind.to_concrete x)

and class_type_of_ast x =
  let of_concrete : Stable.V4_07.Class_type.Concrete.t -> Astlib_parsetree.class_type option =
    fun (Class_type { pcty_desc; pcty_loc; pcty_attributes }) ->
      Optional.bind (class_type_desc_of_ast pcty_desc) ~f:(fun pcty_desc ->
        Optional.bind (attributes_of_ast pcty_attributes) ~f:(fun pcty_attributes ->
          Some ({ pcty_desc; pcty_loc; pcty_attributes } : Astlib_parsetree.class_type)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_type.to_concrete x)

and class_type_desc_of_ast x =
  let of_concrete : Stable.V4_07.Class_type_desc.Concrete.t -> Astlib_parsetree.class_type_desc option =
    function
    | Pcty_constr { a; b } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.List.map ~f:(core_type_of_ast) b) ~f:(fun b ->
          Some (Pcty_constr (a, b) : Astlib_parsetree.class_type_desc)))
    | Pcty_signature { a } ->
      Optional.bind (class_signature_of_ast a) ~f:(fun a ->
        Some (Pcty_signature (a) : Astlib_parsetree.class_type_desc))
    | Pcty_arrow { a; b; c } ->
      Optional.bind (arg_label_of_ast a) ~f:(fun a ->
        Optional.bind (core_type_of_ast b) ~f:(fun b ->
          Optional.bind (class_type_of_ast c) ~f:(fun c ->
            Some (Pcty_arrow (a, b, c) : Astlib_parsetree.class_type_desc))))
    | Pcty_extension { a } ->
      Optional.bind (extension_of_ast a) ~f:(fun a ->
        Some (Pcty_extension (a) : Astlib_parsetree.class_type_desc))
    | Pcty_open { a; b; c } ->
      Optional.bind (override_flag_of_ast a) ~f:(fun a ->
        Optional.bind (longident_loc_of_ast b) ~f:(fun b ->
          Optional.bind (class_type_of_ast c) ~f:(fun c ->
            Some (Pcty_open (a, b, c) : Astlib_parsetree.class_type_desc))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_type_desc.to_concrete x)

and class_signature_of_ast x =
  let of_concrete : Stable.V4_07.Class_signature.Concrete.t -> Astlib_parsetree.class_signature option =
    fun (Class_signature { pcsig_self; pcsig_fields }) ->
      Optional.bind (core_type_of_ast pcsig_self) ~f:(fun pcsig_self ->
        Optional.bind (Optional.List.map ~f:(class_type_field_of_ast) pcsig_fields) ~f:(fun pcsig_fields ->
          Some ({ pcsig_self; pcsig_fields } : Astlib_parsetree.class_signature)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_signature.to_concrete x)

and class_type_field_of_ast x =
  let of_concrete : Stable.V4_07.Class_type_field.Concrete.t -> Astlib_parsetree.class_type_field option =
    fun (Class_type_field { pctf_desc; pctf_loc; pctf_attributes }) ->
      Optional.bind (class_type_field_desc_of_ast pctf_desc) ~f:(fun pctf_desc ->
        Optional.bind (attributes_of_ast pctf_attributes) ~f:(fun pctf_attributes ->
          Some ({ pctf_desc; pctf_loc; pctf_attributes } : Astlib_parsetree.class_type_field)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_type_field.to_concrete x)

and class_type_field_desc_of_ast x =
  let of_concrete : Stable.V4_07.Class_type_field_desc.Concrete.t -> Astlib_parsetree.class_type_field_desc option =
    function
    | Pctf_inherit { a } ->
      Optional.bind (class_type_of_ast a) ~f:(fun a ->
        Some (Pctf_inherit (a) : Astlib_parsetree.class_type_field_desc))
    | Pctf_val { a } ->
      Optional.bind (class_type_value_desc_of_ast a) ~f:(fun a ->
        Some (Pctf_val (a) : Astlib_parsetree.class_type_field_desc))
    | Pctf_method { a } ->
      Optional.bind (class_type_method_desc_of_ast a) ~f:(fun a ->
        Some (Pctf_method (a) : Astlib_parsetree.class_type_field_desc))
    | Pctf_constraint { a } ->
      Optional.bind (class_type_constraint_of_ast a) ~f:(fun a ->
        Some (Pctf_constraint (a) : Astlib_parsetree.class_type_field_desc))
    | Pctf_attribute { a } ->
      Optional.bind (attribute_of_ast a) ~f:(fun a ->
        Some (Pctf_attribute (a) : Astlib_parsetree.class_type_field_desc))
    | Pctf_extension { a } ->
      Optional.bind (extension_of_ast a) ~f:(fun a ->
        Some (Pctf_extension (a) : Astlib_parsetree.class_type_field_desc))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_type_field_desc.to_concrete x)

and class_type_value_desc_of_ast x =
  let of_concrete : Stable.V4_07.Class_type_value_desc.Concrete.t -> Astlib_parsetree.class_type_value_desc option =
    fun (Class_type_value_desc { a; b; c; d }) ->
      Optional.bind (label_loc_of_ast a) ~f:(fun a ->
        Optional.bind (mutable_flag_of_ast b) ~f:(fun b ->
          Optional.bind (virtual_flag_of_ast c) ~f:(fun c ->
            Optional.bind (core_type_of_ast d) ~f:(fun d ->
              Some (a, b, c, d)))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_type_value_desc.to_concrete x)

and class_type_method_desc_of_ast x =
  let of_concrete : Stable.V4_07.Class_type_method_desc.Concrete.t -> Astlib_parsetree.class_type_method_desc option =
    fun (Class_type_method_desc { a; b; c; d }) ->
      Optional.bind (label_loc_of_ast a) ~f:(fun a ->
        Optional.bind (private_flag_of_ast b) ~f:(fun b ->
          Optional.bind (virtual_flag_of_ast c) ~f:(fun c ->
            Optional.bind (core_type_of_ast d) ~f:(fun d ->
              Some (a, b, c, d)))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_type_method_desc.to_concrete x)

and class_type_constraint_of_ast x =
  let of_concrete : Stable.V4_07.Class_type_constraint.Concrete.t -> Astlib_parsetree.class_type_constraint option =
    fun (Class_type_constraint { a; b }) ->
      Optional.bind (core_type_of_ast a) ~f:(fun a ->
        Optional.bind (core_type_of_ast b) ~f:(fun b ->
          Some (a, b)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_type_constraint.to_concrete x)

and class_description_of_ast x =
  let of_concrete : Stable.V4_07.Class_description.Concrete.t -> Astlib_parsetree.class_description option =
    fun (Class_description { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }) ->
      Optional.bind (virtual_flag_of_ast pci_virt) ~f:(fun pci_virt ->
        Optional.bind (Optional.List.map ~f:(type_param_of_ast) pci_params) ~f:(fun pci_params ->
          Optional.bind (string_loc_of_ast pci_name) ~f:(fun pci_name ->
            Optional.bind (class_type_of_ast pci_expr) ~f:(fun pci_expr ->
              Optional.bind (attributes_of_ast pci_attributes) ~f:(fun pci_attributes ->
                Some ({ pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Astlib_parsetree.class_description))))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_description.to_concrete x)

and class_type_declaration_of_ast x =
  let of_concrete : Stable.V4_07.Class_type_declaration.Concrete.t -> Astlib_parsetree.class_type_declaration option =
    fun (Class_type_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }) ->
      Optional.bind (virtual_flag_of_ast pci_virt) ~f:(fun pci_virt ->
        Optional.bind (Optional.List.map ~f:(type_param_of_ast) pci_params) ~f:(fun pci_params ->
          Optional.bind (string_loc_of_ast pci_name) ~f:(fun pci_name ->
            Optional.bind (class_type_of_ast pci_expr) ~f:(fun pci_expr ->
              Optional.bind (attributes_of_ast pci_attributes) ~f:(fun pci_attributes ->
                Some ({ pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Astlib_parsetree.class_type_declaration))))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_type_declaration.to_concrete x)

and class_expr_of_ast x =
  let of_concrete : Stable.V4_07.Class_expr.Concrete.t -> Astlib_parsetree.class_expr option =
    fun (Class_expr { pcl_desc; pcl_loc; pcl_attributes }) ->
      Optional.bind (class_expr_desc_of_ast pcl_desc) ~f:(fun pcl_desc ->
        Optional.bind (attributes_of_ast pcl_attributes) ~f:(fun pcl_attributes ->
          Some ({ pcl_desc; pcl_loc; pcl_attributes } : Astlib_parsetree.class_expr)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_expr.to_concrete x)

and class_expr_desc_of_ast x =
  let of_concrete : Stable.V4_07.Class_expr_desc.Concrete.t -> Astlib_parsetree.class_expr_desc option =
    function
    | Pcl_constr { a; b } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.List.map ~f:(core_type_of_ast) b) ~f:(fun b ->
          Some (Pcl_constr (a, b) : Astlib_parsetree.class_expr_desc)))
    | Pcl_structure { a } ->
      Optional.bind (class_structure_of_ast a) ~f:(fun a ->
        Some (Pcl_structure (a) : Astlib_parsetree.class_expr_desc))
    | Pcl_fun { a; b; c; d } ->
      Optional.bind (arg_label_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.Option.map ~f:(expression_of_ast) b) ~f:(fun b ->
          Optional.bind (pattern_of_ast c) ~f:(fun c ->
            Optional.bind (class_expr_of_ast d) ~f:(fun d ->
              Some (Pcl_fun (a, b, c, d) : Astlib_parsetree.class_expr_desc)))))
    | Pcl_apply { a; b } ->
      Optional.bind (class_expr_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.List.map ~f:(apply_arg_of_ast) b) ~f:(fun b ->
          Some (Pcl_apply (a, b) : Astlib_parsetree.class_expr_desc)))
    | Pcl_let { a; b; c } ->
      Optional.bind (rec_flag_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.List.map ~f:(value_binding_of_ast) b) ~f:(fun b ->
          Optional.bind (class_expr_of_ast c) ~f:(fun c ->
            Some (Pcl_let (a, b, c) : Astlib_parsetree.class_expr_desc))))
    | Pcl_constraint { a; b } ->
      Optional.bind (class_expr_of_ast a) ~f:(fun a ->
        Optional.bind (class_type_of_ast b) ~f:(fun b ->
          Some (Pcl_constraint (a, b) : Astlib_parsetree.class_expr_desc)))
    | Pcl_extension { a } ->
      Optional.bind (extension_of_ast a) ~f:(fun a ->
        Some (Pcl_extension (a) : Astlib_parsetree.class_expr_desc))
    | Pcl_open { a; b; c } ->
      Optional.bind (override_flag_of_ast a) ~f:(fun a ->
        Optional.bind (longident_loc_of_ast b) ~f:(fun b ->
          Optional.bind (class_expr_of_ast c) ~f:(fun c ->
            Some (Pcl_open (a, b, c) : Astlib_parsetree.class_expr_desc))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_expr_desc.to_concrete x)

and class_structure_of_ast x =
  let of_concrete : Stable.V4_07.Class_structure.Concrete.t -> Astlib_parsetree.class_structure option =
    fun (Class_structure { pcstr_self; pcstr_fields }) ->
      Optional.bind (pattern_of_ast pcstr_self) ~f:(fun pcstr_self ->
        Optional.bind (Optional.List.map ~f:(class_field_of_ast) pcstr_fields) ~f:(fun pcstr_fields ->
          Some ({ pcstr_self; pcstr_fields } : Astlib_parsetree.class_structure)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_structure.to_concrete x)

and class_field_of_ast x =
  let of_concrete : Stable.V4_07.Class_field.Concrete.t -> Astlib_parsetree.class_field option =
    fun (Class_field { pcf_desc; pcf_loc; pcf_attributes }) ->
      Optional.bind (class_field_desc_of_ast pcf_desc) ~f:(fun pcf_desc ->
        Optional.bind (attributes_of_ast pcf_attributes) ~f:(fun pcf_attributes ->
          Some ({ pcf_desc; pcf_loc; pcf_attributes } : Astlib_parsetree.class_field)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_field.to_concrete x)

and class_field_desc_of_ast x =
  let of_concrete : Stable.V4_07.Class_field_desc.Concrete.t -> Astlib_parsetree.class_field_desc option =
    function
    | Pcf_inherit { a; b; c } ->
      Optional.bind (override_flag_of_ast a) ~f:(fun a ->
        Optional.bind (class_expr_of_ast b) ~f:(fun b ->
          Optional.bind (Optional.Option.map ~f:(string_loc_of_ast) c) ~f:(fun c ->
            Some (Pcf_inherit (a, b, c) : Astlib_parsetree.class_field_desc))))
    | Pcf_val { a } ->
      Optional.bind (class_value_desc_of_ast a) ~f:(fun a ->
        Some (Pcf_val (a) : Astlib_parsetree.class_field_desc))
    | Pcf_method { a } ->
      Optional.bind (class_method_desc_of_ast a) ~f:(fun a ->
        Some (Pcf_method (a) : Astlib_parsetree.class_field_desc))
    | Pcf_constraint { a } ->
      Optional.bind (class_type_constraint_of_ast a) ~f:(fun a ->
        Some (Pcf_constraint (a) : Astlib_parsetree.class_field_desc))
    | Pcf_initializer { a } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Some (Pcf_initializer (a) : Astlib_parsetree.class_field_desc))
    | Pcf_attribute { a } ->
      Optional.bind (attribute_of_ast a) ~f:(fun a ->
        Some (Pcf_attribute (a) : Astlib_parsetree.class_field_desc))
    | Pcf_extension { a } ->
      Optional.bind (extension_of_ast a) ~f:(fun a ->
        Some (Pcf_extension (a) : Astlib_parsetree.class_field_desc))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_field_desc.to_concrete x)

and class_value_desc_of_ast x =
  let of_concrete : Stable.V4_07.Class_value_desc.Concrete.t -> Astlib_parsetree.class_value_desc option =
    fun (Class_value_desc { a; b; c }) ->
      Optional.bind (label_loc_of_ast a) ~f:(fun a ->
        Optional.bind (mutable_flag_of_ast b) ~f:(fun b ->
          Optional.bind (class_field_kind_of_ast c) ~f:(fun c ->
            Some (a, b, c))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_value_desc.to_concrete x)

and class_method_desc_of_ast x =
  let of_concrete : Stable.V4_07.Class_method_desc.Concrete.t -> Astlib_parsetree.class_method_desc option =
    fun (Class_method_desc { a; b; c }) ->
      Optional.bind (label_loc_of_ast a) ~f:(fun a ->
        Optional.bind (private_flag_of_ast b) ~f:(fun b ->
          Optional.bind (class_field_kind_of_ast c) ~f:(fun c ->
            Some (a, b, c))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_method_desc.to_concrete x)

and class_field_kind_of_ast x =
  let of_concrete : Stable.V4_07.Class_field_kind.Concrete.t -> Astlib_parsetree.class_field_kind option =
    function
    | Cfk_virtual { a } ->
      Optional.bind (core_type_of_ast a) ~f:(fun a ->
        Some (Cfk_virtual (a) : Astlib_parsetree.class_field_kind))
    | Cfk_concrete { a; b } ->
      Optional.bind (override_flag_of_ast a) ~f:(fun a ->
        Optional.bind (expression_of_ast b) ~f:(fun b ->
          Some (Cfk_concrete (a, b) : Astlib_parsetree.class_field_kind)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_field_kind.to_concrete x)

and class_declaration_of_ast x =
  let of_concrete : Stable.V4_07.Class_declaration.Concrete.t -> Astlib_parsetree.class_declaration option =
    fun (Class_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }) ->
      Optional.bind (virtual_flag_of_ast pci_virt) ~f:(fun pci_virt ->
        Optional.bind (Optional.List.map ~f:(type_param_of_ast) pci_params) ~f:(fun pci_params ->
          Optional.bind (string_loc_of_ast pci_name) ~f:(fun pci_name ->
            Optional.bind (class_expr_of_ast pci_expr) ~f:(fun pci_expr ->
              Optional.bind (attributes_of_ast pci_attributes) ~f:(fun pci_attributes ->
                Some ({ pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : Astlib_parsetree.class_declaration))))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Class_declaration.to_concrete x)

and module_type_of_ast x =
  let of_concrete : Stable.V4_07.Module_type.Concrete.t -> Astlib_parsetree.module_type option =
    fun (Module_type { pmty_desc; pmty_loc; pmty_attributes }) ->
      Optional.bind (module_type_desc_of_ast pmty_desc) ~f:(fun pmty_desc ->
        Optional.bind (attributes_of_ast pmty_attributes) ~f:(fun pmty_attributes ->
          Some ({ pmty_desc; pmty_loc; pmty_attributes } : Astlib_parsetree.module_type)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Module_type.to_concrete x)

and module_type_desc_of_ast x =
  let of_concrete : Stable.V4_07.Module_type_desc.Concrete.t -> Astlib_parsetree.module_type_desc option =
    function
    | Pmty_ident { a } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Some (Pmty_ident (a) : Astlib_parsetree.module_type_desc))
    | Pmty_signature { a } ->
      Optional.bind (signature_of_ast a) ~f:(fun a ->
        Some (Pmty_signature (a) : Astlib_parsetree.module_type_desc))
    | Pmty_functor { a; b; c } ->
      Optional.bind (string_loc_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.Option.map ~f:(module_type_of_ast) b) ~f:(fun b ->
          Optional.bind (module_type_of_ast c) ~f:(fun c ->
            Some (Pmty_functor (a, b, c) : Astlib_parsetree.module_type_desc))))
    | Pmty_with { a; b } ->
      Optional.bind (module_type_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.List.map ~f:(with_constraint_of_ast) b) ~f:(fun b ->
          Some (Pmty_with (a, b) : Astlib_parsetree.module_type_desc)))
    | Pmty_typeof { a } ->
      Optional.bind (module_expr_of_ast a) ~f:(fun a ->
        Some (Pmty_typeof (a) : Astlib_parsetree.module_type_desc))
    | Pmty_extension { a } ->
      Optional.bind (extension_of_ast a) ~f:(fun a ->
        Some (Pmty_extension (a) : Astlib_parsetree.module_type_desc))
    | Pmty_alias { a } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Some (Pmty_alias (a) : Astlib_parsetree.module_type_desc))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Module_type_desc.to_concrete x)

and signature_of_ast x =
  let of_concrete : Stable.V4_07.Signature.Concrete.t -> Astlib_parsetree.signature option =
    fun (Signature { a }) ->
      Optional.bind (Optional.List.map ~f:(signature_item_of_ast) a) ~f:(fun a ->
        Some a)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Signature.to_concrete x)

and signature_item_of_ast x =
  let of_concrete : Stable.V4_07.Signature_item.Concrete.t -> Astlib_parsetree.signature_item option =
    fun (Signature_item { psig_desc; psig_loc }) ->
      Optional.bind (signature_item_desc_of_ast psig_desc) ~f:(fun psig_desc ->
        Some ({ psig_desc; psig_loc } : Astlib_parsetree.signature_item))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Signature_item.to_concrete x)

and signature_item_desc_of_ast x =
  let of_concrete : Stable.V4_07.Signature_item_desc.Concrete.t -> Astlib_parsetree.signature_item_desc option =
    function
    | Psig_value { a } ->
      Optional.bind (value_description_of_ast a) ~f:(fun a ->
        Some (Psig_value (a) : Astlib_parsetree.signature_item_desc))
    | Psig_type { a; b } ->
      Optional.bind (rec_flag_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.List.map ~f:(type_declaration_of_ast) b) ~f:(fun b ->
          Some (Psig_type (a, b) : Astlib_parsetree.signature_item_desc)))
    | Psig_typext { a } ->
      Optional.bind (type_extension_of_ast a) ~f:(fun a ->
        Some (Psig_typext (a) : Astlib_parsetree.signature_item_desc))
    | Psig_exception { a } ->
      Optional.bind (extension_constructor_of_ast a) ~f:(fun a ->
        Some (Psig_exception (a) : Astlib_parsetree.signature_item_desc))
    | Psig_module { a } ->
      Optional.bind (module_declaration_of_ast a) ~f:(fun a ->
        Some (Psig_module (a) : Astlib_parsetree.signature_item_desc))
    | Psig_recmodule { a } ->
      Optional.bind (Optional.List.map ~f:(module_declaration_of_ast) a) ~f:(fun a ->
        Some (Psig_recmodule (a) : Astlib_parsetree.signature_item_desc))
    | Psig_modtype { a } ->
      Optional.bind (module_type_declaration_of_ast a) ~f:(fun a ->
        Some (Psig_modtype (a) : Astlib_parsetree.signature_item_desc))
    | Psig_open { a } ->
      Optional.bind (open_description_of_ast a) ~f:(fun a ->
        Some (Psig_open (a) : Astlib_parsetree.signature_item_desc))
    | Psig_include { a } ->
      Optional.bind (include_description_of_ast a) ~f:(fun a ->
        Some (Psig_include (a) : Astlib_parsetree.signature_item_desc))
    | Psig_class { a } ->
      Optional.bind (Optional.List.map ~f:(class_description_of_ast) a) ~f:(fun a ->
        Some (Psig_class (a) : Astlib_parsetree.signature_item_desc))
    | Psig_class_type { a } ->
      Optional.bind (Optional.List.map ~f:(class_type_declaration_of_ast) a) ~f:(fun a ->
        Some (Psig_class_type (a) : Astlib_parsetree.signature_item_desc))
    | Psig_attribute { a } ->
      Optional.bind (attribute_of_ast a) ~f:(fun a ->
        Some (Psig_attribute (a) : Astlib_parsetree.signature_item_desc))
    | Psig_extension { a; b } ->
      Optional.bind (extension_of_ast a) ~f:(fun a ->
        Optional.bind (attributes_of_ast b) ~f:(fun b ->
          Some (Psig_extension (a, b) : Astlib_parsetree.signature_item_desc)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Signature_item_desc.to_concrete x)

and module_declaration_of_ast x =
  let of_concrete : Stable.V4_07.Module_declaration.Concrete.t -> Astlib_parsetree.module_declaration option =
    fun (Module_declaration { pmd_name; pmd_type; pmd_attributes; pmd_loc }) ->
      Optional.bind (string_loc_of_ast pmd_name) ~f:(fun pmd_name ->
        Optional.bind (module_type_of_ast pmd_type) ~f:(fun pmd_type ->
          Optional.bind (attributes_of_ast pmd_attributes) ~f:(fun pmd_attributes ->
            Some ({ pmd_name; pmd_type; pmd_attributes; pmd_loc } : Astlib_parsetree.module_declaration))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Module_declaration.to_concrete x)

and module_type_declaration_of_ast x =
  let of_concrete : Stable.V4_07.Module_type_declaration.Concrete.t -> Astlib_parsetree.module_type_declaration option =
    fun (Module_type_declaration { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }) ->
      Optional.bind (string_loc_of_ast pmtd_name) ~f:(fun pmtd_name ->
        Optional.bind (Optional.Option.map ~f:(module_type_of_ast) pmtd_type) ~f:(fun pmtd_type ->
          Optional.bind (attributes_of_ast pmtd_attributes) ~f:(fun pmtd_attributes ->
            Some ({ pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Astlib_parsetree.module_type_declaration))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Module_type_declaration.to_concrete x)

and open_description_of_ast x =
  let of_concrete : Stable.V4_07.Open_description.Concrete.t -> Astlib_parsetree.open_description option =
    fun (Open_description { popen_lid; popen_override; popen_loc; popen_attributes }) ->
      Optional.bind (longident_loc_of_ast popen_lid) ~f:(fun popen_lid ->
        Optional.bind (override_flag_of_ast popen_override) ~f:(fun popen_override ->
          Optional.bind (attributes_of_ast popen_attributes) ~f:(fun popen_attributes ->
            Some ({ popen_lid; popen_override; popen_loc; popen_attributes } : Astlib_parsetree.open_description))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Open_description.to_concrete x)

and include_description_of_ast x =
  let of_concrete : Stable.V4_07.Include_description.Concrete.t -> Astlib_parsetree.include_description option =
    fun (Include_description { pincl_mod; pincl_loc; pincl_attributes }) ->
      Optional.bind (module_type_of_ast pincl_mod) ~f:(fun pincl_mod ->
        Optional.bind (attributes_of_ast pincl_attributes) ~f:(fun pincl_attributes ->
          Some ({ pincl_mod; pincl_loc; pincl_attributes } : Astlib_parsetree.include_description)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Include_description.to_concrete x)

and include_declaration_of_ast x =
  let of_concrete : Stable.V4_07.Include_declaration.Concrete.t -> Astlib_parsetree.include_declaration option =
    fun (Include_declaration { pincl_mod; pincl_loc; pincl_attributes }) ->
      Optional.bind (module_expr_of_ast pincl_mod) ~f:(fun pincl_mod ->
        Optional.bind (attributes_of_ast pincl_attributes) ~f:(fun pincl_attributes ->
          Some ({ pincl_mod; pincl_loc; pincl_attributes } : Astlib_parsetree.include_declaration)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Include_declaration.to_concrete x)

and with_constraint_of_ast x =
  let of_concrete : Stable.V4_07.With_constraint.Concrete.t -> Astlib_parsetree.with_constraint option =
    function
    | Pwith_type { a; b } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Optional.bind (type_declaration_of_ast b) ~f:(fun b ->
          Some (Pwith_type (a, b) : Astlib_parsetree.with_constraint)))
    | Pwith_module { a; b } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Optional.bind (longident_loc_of_ast b) ~f:(fun b ->
          Some (Pwith_module (a, b) : Astlib_parsetree.with_constraint)))
    | Pwith_typesubst { a; b } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Optional.bind (type_declaration_of_ast b) ~f:(fun b ->
          Some (Pwith_typesubst (a, b) : Astlib_parsetree.with_constraint)))
    | Pwith_modsubst { a; b } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Optional.bind (longident_loc_of_ast b) ~f:(fun b ->
          Some (Pwith_modsubst (a, b) : Astlib_parsetree.with_constraint)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.With_constraint.to_concrete x)

and module_expr_of_ast x =
  let of_concrete : Stable.V4_07.Module_expr.Concrete.t -> Astlib_parsetree.module_expr option =
    fun (Module_expr { pmod_desc; pmod_loc; pmod_attributes }) ->
      Optional.bind (module_expr_desc_of_ast pmod_desc) ~f:(fun pmod_desc ->
        Optional.bind (attributes_of_ast pmod_attributes) ~f:(fun pmod_attributes ->
          Some ({ pmod_desc; pmod_loc; pmod_attributes } : Astlib_parsetree.module_expr)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Module_expr.to_concrete x)

and module_expr_desc_of_ast x =
  let of_concrete : Stable.V4_07.Module_expr_desc.Concrete.t -> Astlib_parsetree.module_expr_desc option =
    function
    | Pmod_ident { a } ->
      Optional.bind (longident_loc_of_ast a) ~f:(fun a ->
        Some (Pmod_ident (a) : Astlib_parsetree.module_expr_desc))
    | Pmod_structure { a } ->
      Optional.bind (structure_of_ast a) ~f:(fun a ->
        Some (Pmod_structure (a) : Astlib_parsetree.module_expr_desc))
    | Pmod_functor { a; b; c } ->
      Optional.bind (string_loc_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.Option.map ~f:(module_type_of_ast) b) ~f:(fun b ->
          Optional.bind (module_expr_of_ast c) ~f:(fun c ->
            Some (Pmod_functor (a, b, c) : Astlib_parsetree.module_expr_desc))))
    | Pmod_apply { a; b } ->
      Optional.bind (module_expr_of_ast a) ~f:(fun a ->
        Optional.bind (module_expr_of_ast b) ~f:(fun b ->
          Some (Pmod_apply (a, b) : Astlib_parsetree.module_expr_desc)))
    | Pmod_constraint { a; b } ->
      Optional.bind (module_expr_of_ast a) ~f:(fun a ->
        Optional.bind (module_type_of_ast b) ~f:(fun b ->
          Some (Pmod_constraint (a, b) : Astlib_parsetree.module_expr_desc)))
    | Pmod_unpack { a } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Some (Pmod_unpack (a) : Astlib_parsetree.module_expr_desc))
    | Pmod_extension { a } ->
      Optional.bind (extension_of_ast a) ~f:(fun a ->
        Some (Pmod_extension (a) : Astlib_parsetree.module_expr_desc))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Module_expr_desc.to_concrete x)

and structure_of_ast x =
  let of_concrete : Stable.V4_07.Structure.Concrete.t -> Astlib_parsetree.structure option =
    fun (Structure { a }) ->
      Optional.bind (Optional.List.map ~f:(structure_item_of_ast) a) ~f:(fun a ->
        Some a)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Structure.to_concrete x)

and structure_item_of_ast x =
  let of_concrete : Stable.V4_07.Structure_item.Concrete.t -> Astlib_parsetree.structure_item option =
    fun (Structure_item { pstr_desc; pstr_loc }) ->
      Optional.bind (structure_item_desc_of_ast pstr_desc) ~f:(fun pstr_desc ->
        Some ({ pstr_desc; pstr_loc } : Astlib_parsetree.structure_item))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Structure_item.to_concrete x)

and structure_item_desc_of_ast x =
  let of_concrete : Stable.V4_07.Structure_item_desc.Concrete.t -> Astlib_parsetree.structure_item_desc option =
    function
    | Pstr_eval { a; b } ->
      Optional.bind (expression_of_ast a) ~f:(fun a ->
        Optional.bind (attributes_of_ast b) ~f:(fun b ->
          Some (Pstr_eval (a, b) : Astlib_parsetree.structure_item_desc)))
    | Pstr_value { a; b } ->
      Optional.bind (rec_flag_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.List.map ~f:(value_binding_of_ast) b) ~f:(fun b ->
          Some (Pstr_value (a, b) : Astlib_parsetree.structure_item_desc)))
    | Pstr_primitive { a } ->
      Optional.bind (value_description_of_ast a) ~f:(fun a ->
        Some (Pstr_primitive (a) : Astlib_parsetree.structure_item_desc))
    | Pstr_type { a; b } ->
      Optional.bind (rec_flag_of_ast a) ~f:(fun a ->
        Optional.bind (Optional.List.map ~f:(type_declaration_of_ast) b) ~f:(fun b ->
          Some (Pstr_type (a, b) : Astlib_parsetree.structure_item_desc)))
    | Pstr_typext { a } ->
      Optional.bind (type_extension_of_ast a) ~f:(fun a ->
        Some (Pstr_typext (a) : Astlib_parsetree.structure_item_desc))
    | Pstr_exception { a } ->
      Optional.bind (extension_constructor_of_ast a) ~f:(fun a ->
        Some (Pstr_exception (a) : Astlib_parsetree.structure_item_desc))
    | Pstr_module { a } ->
      Optional.bind (module_binding_of_ast a) ~f:(fun a ->
        Some (Pstr_module (a) : Astlib_parsetree.structure_item_desc))
    | Pstr_recmodule { a } ->
      Optional.bind (Optional.List.map ~f:(module_binding_of_ast) a) ~f:(fun a ->
        Some (Pstr_recmodule (a) : Astlib_parsetree.structure_item_desc))
    | Pstr_modtype { a } ->
      Optional.bind (module_type_declaration_of_ast a) ~f:(fun a ->
        Some (Pstr_modtype (a) : Astlib_parsetree.structure_item_desc))
    | Pstr_open { a } ->
      Optional.bind (open_description_of_ast a) ~f:(fun a ->
        Some (Pstr_open (a) : Astlib_parsetree.structure_item_desc))
    | Pstr_class { a } ->
      Optional.bind (Optional.List.map ~f:(class_declaration_of_ast) a) ~f:(fun a ->
        Some (Pstr_class (a) : Astlib_parsetree.structure_item_desc))
    | Pstr_class_type { a } ->
      Optional.bind (Optional.List.map ~f:(class_type_declaration_of_ast) a) ~f:(fun a ->
        Some (Pstr_class_type (a) : Astlib_parsetree.structure_item_desc))
    | Pstr_include { a } ->
      Optional.bind (include_declaration_of_ast a) ~f:(fun a ->
        Some (Pstr_include (a) : Astlib_parsetree.structure_item_desc))
    | Pstr_attribute { a } ->
      Optional.bind (attribute_of_ast a) ~f:(fun a ->
        Some (Pstr_attribute (a) : Astlib_parsetree.structure_item_desc))
    | Pstr_extension { a; b } ->
      Optional.bind (extension_of_ast a) ~f:(fun a ->
        Optional.bind (attributes_of_ast b) ~f:(fun b ->
          Some (Pstr_extension (a, b) : Astlib_parsetree.structure_item_desc)))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Structure_item_desc.to_concrete x)

and value_binding_of_ast x =
  let of_concrete : Stable.V4_07.Value_binding.Concrete.t -> Astlib_parsetree.value_binding option =
    fun (Value_binding { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }) ->
      Optional.bind (pattern_of_ast pvb_pat) ~f:(fun pvb_pat ->
        Optional.bind (expression_of_ast pvb_expr) ~f:(fun pvb_expr ->
          Optional.bind (attributes_of_ast pvb_attributes) ~f:(fun pvb_attributes ->
            Some ({ pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Astlib_parsetree.value_binding))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Value_binding.to_concrete x)

and module_binding_of_ast x =
  let of_concrete : Stable.V4_07.Module_binding.Concrete.t -> Astlib_parsetree.module_binding option =
    fun (Module_binding { pmb_name; pmb_expr; pmb_attributes; pmb_loc }) ->
      Optional.bind (string_loc_of_ast pmb_name) ~f:(fun pmb_name ->
        Optional.bind (module_expr_of_ast pmb_expr) ~f:(fun pmb_expr ->
          Optional.bind (attributes_of_ast pmb_attributes) ~f:(fun pmb_attributes ->
            Some ({ pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Astlib_parsetree.module_binding))))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Module_binding.to_concrete x)

and toplevel_phrase_of_ast x =
  let of_concrete : Stable.V4_07.Toplevel_phrase.Concrete.t -> Astlib_parsetree.toplevel_phrase option =
    function
    | Ptop_def { a } ->
      Optional.bind (structure_of_ast a) ~f:(fun a ->
        Some (Ptop_def (a) : Astlib_parsetree.toplevel_phrase))
    | Ptop_dir { a; b } ->
      Optional.bind (directive_argument_of_ast b) ~f:(fun b ->
        Some (Ptop_dir (a, b) : Astlib_parsetree.toplevel_phrase))
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Toplevel_phrase.to_concrete x)

and directive_argument_of_ast x =
  let of_concrete : Stable.V4_07.Directive_argument.Concrete.t -> Astlib_parsetree.directive_argument option =
    function
    | Pdir_none ->
      Some (Pdir_none : Astlib_parsetree.directive_argument)
    | Pdir_string { a } ->
      Some (Pdir_string (a) : Astlib_parsetree.directive_argument)
    | Pdir_int { a; b } ->
      Some (Pdir_int (a, b) : Astlib_parsetree.directive_argument)
    | Pdir_ident { a } ->
      Optional.bind (longident_of_ast a) ~f:(fun a ->
        Some (Pdir_ident (a) : Astlib_parsetree.directive_argument))
    | Pdir_bool { a } ->
      Some (Pdir_bool (a) : Astlib_parsetree.directive_argument)
  in
  Optional.bind ~f:of_concrete (Stable.V4_07.Directive_argument.to_concrete x)

let rec longident_to_ast : Astlib_parsetree.longident -> Stable.V4_07.Longident.t =
  function
  | Lident (a) ->
    Stable.V4_07.Longident.of_concrete (Lident { a })
  | Ldot (a, b) ->
    let a = longident_to_ast a in
    Stable.V4_07.Longident.of_concrete (Ldot { a; b })
  | Lapply (a, b) ->
    let a = longident_to_ast a in
    let b = longident_to_ast b in
    Stable.V4_07.Longident.of_concrete (Lapply { a; b })

and longident_loc_to_ast : Astlib_parsetree.longident_loc -> Stable.V4_07.Longident_loc.t =
  fun { txt; loc } ->
    let txt = longident_to_ast txt in
    Stable.V4_07.Longident_loc.of_concrete (Longident_loc { txt; loc })

and rec_flag_to_ast : Astlib_parsetree.rec_flag -> Stable.V4_07.Rec_flag.t =
  function
  | Nonrecursive ->
    Stable.V4_07.Rec_flag.of_concrete (Nonrecursive)
  | Recursive ->
    Stable.V4_07.Rec_flag.of_concrete (Recursive)

and direction_flag_to_ast : Astlib_parsetree.direction_flag -> Stable.V4_07.Direction_flag.t =
  function
  | Upto ->
    Stable.V4_07.Direction_flag.of_concrete (Upto)
  | Downto ->
    Stable.V4_07.Direction_flag.of_concrete (Downto)

and private_flag_to_ast : Astlib_parsetree.private_flag -> Stable.V4_07.Private_flag.t =
  function
  | Private ->
    Stable.V4_07.Private_flag.of_concrete (Private)
  | Public ->
    Stable.V4_07.Private_flag.of_concrete (Public)

and mutable_flag_to_ast : Astlib_parsetree.mutable_flag -> Stable.V4_07.Mutable_flag.t =
  function
  | Immutable ->
    Stable.V4_07.Mutable_flag.of_concrete (Immutable)
  | Mutable ->
    Stable.V4_07.Mutable_flag.of_concrete (Mutable)

and virtual_flag_to_ast : Astlib_parsetree.virtual_flag -> Stable.V4_07.Virtual_flag.t =
  function
  | Virtual ->
    Stable.V4_07.Virtual_flag.of_concrete (Virtual)
  | Concrete ->
    Stable.V4_07.Virtual_flag.of_concrete (Concrete)

and override_flag_to_ast : Astlib_parsetree.override_flag -> Stable.V4_07.Override_flag.t =
  function
  | Override ->
    Stable.V4_07.Override_flag.of_concrete (Override)
  | Fresh ->
    Stable.V4_07.Override_flag.of_concrete (Fresh)

and closed_flag_to_ast : Astlib_parsetree.closed_flag -> Stable.V4_07.Closed_flag.t =
  function
  | Closed ->
    Stable.V4_07.Closed_flag.of_concrete (Closed)
  | Open ->
    Stable.V4_07.Closed_flag.of_concrete (Open)

and label_to_ast : Astlib_parsetree.label -> Stable.V4_07.Label.t =
  fun a ->
    Stable.V4_07.Label.of_concrete (Label { a })

and label_loc_to_ast : Astlib_parsetree.label_loc -> Stable.V4_07.Label_loc.t =
  fun { txt; loc } ->
    let txt = label_to_ast txt in
    Stable.V4_07.Label_loc.of_concrete (Label_loc { txt; loc })

and string_loc_to_ast : Astlib_parsetree.string_loc -> Stable.V4_07.String_loc.t =
  fun { txt; loc } ->
    Stable.V4_07.String_loc.of_concrete (String_loc { txt; loc })

and arg_label_to_ast : Astlib_parsetree.arg_label -> Stable.V4_07.Arg_label.t =
  function
  | Nolabel ->
    Stable.V4_07.Arg_label.of_concrete (Nolabel)
  | Labelled (a) ->
    Stable.V4_07.Arg_label.of_concrete (Labelled { a })
  | Optional (a) ->
    Stable.V4_07.Arg_label.of_concrete (Optional { a })

and variance_to_ast : Astlib_parsetree.variance -> Stable.V4_07.Variance.t =
  function
  | Covariant ->
    Stable.V4_07.Variance.of_concrete (Covariant)
  | Contravariant ->
    Stable.V4_07.Variance.of_concrete (Contravariant)
  | Invariant ->
    Stable.V4_07.Variance.of_concrete (Invariant)

and constant_to_ast : Astlib_parsetree.constant -> Stable.V4_07.Constant.t =
  function
  | Pconst_integer (a, b) ->
    Stable.V4_07.Constant.of_concrete (Pconst_integer { a; b })
  | Pconst_char (a) ->
    Stable.V4_07.Constant.of_concrete (Pconst_char { a })
  | Pconst_string (a, b) ->
    Stable.V4_07.Constant.of_concrete (Pconst_string { a; b })
  | Pconst_float (a, b) ->
    Stable.V4_07.Constant.of_concrete (Pconst_float { a; b })

and attribute_to_ast : Astlib_parsetree.attribute -> Stable.V4_07.Attribute.t =
  fun (a, b) ->
    let a = string_loc_to_ast a in
    let b = payload_to_ast b in
    Stable.V4_07.Attribute.of_concrete (Attribute { a; b })

and extension_to_ast : Astlib_parsetree.extension -> Stable.V4_07.Extension.t =
  fun (a, b) ->
    let a = string_loc_to_ast a in
    let b = payload_to_ast b in
    Stable.V4_07.Extension.of_concrete (Extension { a; b })

and attributes_to_ast : Astlib_parsetree.attributes -> Stable.V4_07.Attributes.t =
  fun a ->
    let a = List.map ~f:(attribute_to_ast) a in
    Stable.V4_07.Attributes.of_concrete (Attributes { a })

and payload_to_ast : Astlib_parsetree.payload -> Stable.V4_07.Payload.t =
  function
  | PStr (a) ->
    let a = structure_to_ast a in
    Stable.V4_07.Payload.of_concrete (PStr { a })
  | PSig (a) ->
    let a = signature_to_ast a in
    Stable.V4_07.Payload.of_concrete (PSig { a })
  | PTyp (a) ->
    let a = core_type_to_ast a in
    Stable.V4_07.Payload.of_concrete (PTyp { a })
  | PPat (a, b) ->
    let a = pattern_to_ast a in
    let b = Optional.map ~f:(expression_to_ast) b in
    Stable.V4_07.Payload.of_concrete (PPat { a; b })

and core_type_to_ast : Astlib_parsetree.core_type -> Stable.V4_07.Core_type.t =
  fun { ptyp_desc; ptyp_loc; ptyp_attributes } ->
    let ptyp_desc = core_type_desc_to_ast ptyp_desc in
    let ptyp_attributes = attributes_to_ast ptyp_attributes in
    Stable.V4_07.Core_type.of_concrete (Core_type { ptyp_desc; ptyp_loc; ptyp_attributes })

and core_type_desc_to_ast : Astlib_parsetree.core_type_desc -> Stable.V4_07.Core_type_desc.t =
  function
  | Ptyp_any ->
    Stable.V4_07.Core_type_desc.of_concrete (Ptyp_any)
  | Ptyp_var (a) ->
    Stable.V4_07.Core_type_desc.of_concrete (Ptyp_var { a })
  | Ptyp_arrow (a, b, c) ->
    let a = arg_label_to_ast a in
    let b = core_type_to_ast b in
    let c = core_type_to_ast c in
    Stable.V4_07.Core_type_desc.of_concrete (Ptyp_arrow { a; b; c })
  | Ptyp_tuple (a) ->
    let a = List.map ~f:(core_type_to_ast) a in
    Stable.V4_07.Core_type_desc.of_concrete (Ptyp_tuple { a })
  | Ptyp_constr (a, b) ->
    let a = longident_loc_to_ast a in
    let b = List.map ~f:(core_type_to_ast) b in
    Stable.V4_07.Core_type_desc.of_concrete (Ptyp_constr { a; b })
  | Ptyp_object (a, b) ->
    let a = List.map ~f:(object_field_to_ast) a in
    let b = closed_flag_to_ast b in
    Stable.V4_07.Core_type_desc.of_concrete (Ptyp_object { a; b })
  | Ptyp_class (a, b) ->
    let a = longident_loc_to_ast a in
    let b = List.map ~f:(core_type_to_ast) b in
    Stable.V4_07.Core_type_desc.of_concrete (Ptyp_class { a; b })
  | Ptyp_alias (a, b) ->
    let a = core_type_to_ast a in
    Stable.V4_07.Core_type_desc.of_concrete (Ptyp_alias { a; b })
  | Ptyp_variant (a, b, c) ->
    let a = List.map ~f:(row_field_to_ast) a in
    let b = closed_flag_to_ast b in
    let c = Optional.map ~f:(List.map ~f:(label_to_ast)) c in
    Stable.V4_07.Core_type_desc.of_concrete (Ptyp_variant { a; b; c })
  | Ptyp_poly (a, b) ->
    let a = List.map ~f:(string_loc_to_ast) a in
    let b = core_type_to_ast b in
    Stable.V4_07.Core_type_desc.of_concrete (Ptyp_poly { a; b })
  | Ptyp_package (a) ->
    let a = package_type_to_ast a in
    Stable.V4_07.Core_type_desc.of_concrete (Ptyp_package { a })
  | Ptyp_extension (a) ->
    let a = extension_to_ast a in
    Stable.V4_07.Core_type_desc.of_concrete (Ptyp_extension { a })

and package_type_to_ast : Astlib_parsetree.package_type -> Stable.V4_07.Package_type.t =
  fun (a, b) ->
    let a = longident_loc_to_ast a in
    let b = List.map ~f:(package_type_constraint_to_ast) b in
    Stable.V4_07.Package_type.of_concrete (Package_type { a; b })

and package_type_constraint_to_ast : Astlib_parsetree.package_type_constraint -> Stable.V4_07.Package_type_constraint.t =
  fun (a, b) ->
    let a = longident_loc_to_ast a in
    let b = core_type_to_ast b in
    Stable.V4_07.Package_type_constraint.of_concrete (Package_type_constraint { a; b })

and row_field_to_ast : Astlib_parsetree.row_field -> Stable.V4_07.Row_field.t =
  function
  | Rtag (a, b, c, d) ->
    let a = label_loc_to_ast a in
    let b = attributes_to_ast b in
    let d = List.map ~f:(core_type_to_ast) d in
    Stable.V4_07.Row_field.of_concrete (Rtag { a; b; c; d })
  | Rinherit (a) ->
    let a = core_type_to_ast a in
    Stable.V4_07.Row_field.of_concrete (Rinherit { a })

and object_field_to_ast : Astlib_parsetree.object_field -> Stable.V4_07.Object_field.t =
  function
  | Otag (a, b, c) ->
    let a = label_loc_to_ast a in
    let b = attributes_to_ast b in
    let c = core_type_to_ast c in
    Stable.V4_07.Object_field.of_concrete (Otag { a; b; c })
  | Oinherit (a) ->
    let a = core_type_to_ast a in
    Stable.V4_07.Object_field.of_concrete (Oinherit { a })

and pattern_to_ast : Astlib_parsetree.pattern -> Stable.V4_07.Pattern.t =
  fun { ppat_desc; ppat_loc; ppat_attributes } ->
    let ppat_desc = pattern_desc_to_ast ppat_desc in
    let ppat_attributes = attributes_to_ast ppat_attributes in
    Stable.V4_07.Pattern.of_concrete (Pattern { ppat_desc; ppat_loc; ppat_attributes })

and pattern_desc_to_ast : Astlib_parsetree.pattern_desc -> Stable.V4_07.Pattern_desc.t =
  function
  | Ppat_any ->
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_any)
  | Ppat_var (a) ->
    let a = string_loc_to_ast a in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_var { a })
  | Ppat_alias (a, b) ->
    let a = pattern_to_ast a in
    let b = string_loc_to_ast b in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_alias { a; b })
  | Ppat_constant (a) ->
    let a = constant_to_ast a in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_constant { a })
  | Ppat_interval (a, b) ->
    let a = constant_to_ast a in
    let b = constant_to_ast b in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_interval { a; b })
  | Ppat_tuple (a) ->
    let a = List.map ~f:(pattern_to_ast) a in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_tuple { a })
  | Ppat_construct (a, b) ->
    let a = longident_loc_to_ast a in
    let b = Optional.map ~f:(pattern_to_ast) b in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_construct { a; b })
  | Ppat_variant (a, b) ->
    let a = label_to_ast a in
    let b = Optional.map ~f:(pattern_to_ast) b in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_variant { a; b })
  | Ppat_record (a, b) ->
    let a = List.map ~f:(record_field_pattern_to_ast) a in
    let b = closed_flag_to_ast b in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_record { a; b })
  | Ppat_array (a) ->
    let a = List.map ~f:(pattern_to_ast) a in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_array { a })
  | Ppat_or (a, b) ->
    let a = pattern_to_ast a in
    let b = pattern_to_ast b in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_or { a; b })
  | Ppat_constraint (a, b) ->
    let a = pattern_to_ast a in
    let b = core_type_to_ast b in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_constraint { a; b })
  | Ppat_type (a) ->
    let a = longident_loc_to_ast a in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_type { a })
  | Ppat_lazy (a) ->
    let a = pattern_to_ast a in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_lazy { a })
  | Ppat_unpack (a) ->
    let a = string_loc_to_ast a in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_unpack { a })
  | Ppat_exception (a) ->
    let a = pattern_to_ast a in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_exception { a })
  | Ppat_extension (a) ->
    let a = extension_to_ast a in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_extension { a })
  | Ppat_open (a, b) ->
    let a = longident_loc_to_ast a in
    let b = pattern_to_ast b in
    Stable.V4_07.Pattern_desc.of_concrete (Ppat_open { a; b })

and record_field_pattern_to_ast : Astlib_parsetree.record_field_pattern -> Stable.V4_07.Record_field_pattern.t =
  fun (a, b) ->
    let a = longident_loc_to_ast a in
    let b = pattern_to_ast b in
    Stable.V4_07.Record_field_pattern.of_concrete (Record_field_pattern { a; b })

and expression_to_ast : Astlib_parsetree.expression -> Stable.V4_07.Expression.t =
  fun { pexp_desc; pexp_loc; pexp_attributes } ->
    let pexp_desc = expression_desc_to_ast pexp_desc in
    let pexp_attributes = attributes_to_ast pexp_attributes in
    Stable.V4_07.Expression.of_concrete (Expression { pexp_desc; pexp_loc; pexp_attributes })

and expression_desc_to_ast : Astlib_parsetree.expression_desc -> Stable.V4_07.Expression_desc.t =
  function
  | Pexp_ident (a) ->
    let a = longident_loc_to_ast a in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_ident { a })
  | Pexp_constant (a) ->
    let a = constant_to_ast a in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_constant { a })
  | Pexp_let (a, b, c) ->
    let a = rec_flag_to_ast a in
    let b = List.map ~f:(value_binding_to_ast) b in
    let c = expression_to_ast c in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_let { a; b; c })
  | Pexp_function (a) ->
    let a = List.map ~f:(case_to_ast) a in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_function { a })
  | Pexp_fun (a, b, c, d) ->
    let a = arg_label_to_ast a in
    let b = Optional.map ~f:(expression_to_ast) b in
    let c = pattern_to_ast c in
    let d = expression_to_ast d in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_fun { a; b; c; d })
  | Pexp_apply (a, b) ->
    let a = expression_to_ast a in
    let b = List.map ~f:(apply_arg_to_ast) b in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_apply { a; b })
  | Pexp_match (a, b) ->
    let a = expression_to_ast a in
    let b = List.map ~f:(case_to_ast) b in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_match { a; b })
  | Pexp_try (a, b) ->
    let a = expression_to_ast a in
    let b = List.map ~f:(case_to_ast) b in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_try { a; b })
  | Pexp_tuple (a) ->
    let a = List.map ~f:(expression_to_ast) a in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_tuple { a })
  | Pexp_construct (a, b) ->
    let a = longident_loc_to_ast a in
    let b = Optional.map ~f:(expression_to_ast) b in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_construct { a; b })
  | Pexp_variant (a, b) ->
    let a = label_to_ast a in
    let b = Optional.map ~f:(expression_to_ast) b in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_variant { a; b })
  | Pexp_record (a, b) ->
    let a = List.map ~f:(record_field_expression_to_ast) a in
    let b = Optional.map ~f:(expression_to_ast) b in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_record { a; b })
  | Pexp_field (a, b) ->
    let a = expression_to_ast a in
    let b = longident_loc_to_ast b in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_field { a; b })
  | Pexp_setfield (a, b, c) ->
    let a = expression_to_ast a in
    let b = longident_loc_to_ast b in
    let c = expression_to_ast c in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_setfield { a; b; c })
  | Pexp_array (a) ->
    let a = List.map ~f:(expression_to_ast) a in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_array { a })
  | Pexp_ifthenelse (a, b, c) ->
    let a = expression_to_ast a in
    let b = expression_to_ast b in
    let c = Optional.map ~f:(expression_to_ast) c in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_ifthenelse { a; b; c })
  | Pexp_sequence (a, b) ->
    let a = expression_to_ast a in
    let b = expression_to_ast b in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_sequence { a; b })
  | Pexp_while (a, b) ->
    let a = expression_to_ast a in
    let b = expression_to_ast b in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_while { a; b })
  | Pexp_for (a, b, c, d, e) ->
    let a = pattern_to_ast a in
    let b = expression_to_ast b in
    let c = expression_to_ast c in
    let d = direction_flag_to_ast d in
    let e = expression_to_ast e in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_for { a; b; c; d; e })
  | Pexp_constraint (a, b) ->
    let a = expression_to_ast a in
    let b = core_type_to_ast b in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_constraint { a; b })
  | Pexp_coerce (a, b, c) ->
    let a = expression_to_ast a in
    let b = Optional.map ~f:(core_type_to_ast) b in
    let c = core_type_to_ast c in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_coerce { a; b; c })
  | Pexp_send (a, b) ->
    let a = expression_to_ast a in
    let b = label_loc_to_ast b in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_send { a; b })
  | Pexp_new (a) ->
    let a = longident_loc_to_ast a in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_new { a })
  | Pexp_setinstvar (a, b) ->
    let a = label_loc_to_ast a in
    let b = expression_to_ast b in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_setinstvar { a; b })
  | Pexp_override (a) ->
    let a = List.map ~f:(override_expression_to_ast) a in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_override { a })
  | Pexp_letmodule (a, b, c) ->
    let a = string_loc_to_ast a in
    let b = module_expr_to_ast b in
    let c = expression_to_ast c in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_letmodule { a; b; c })
  | Pexp_letexception (a, b) ->
    let a = extension_constructor_to_ast a in
    let b = expression_to_ast b in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_letexception { a; b })
  | Pexp_assert (a) ->
    let a = expression_to_ast a in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_assert { a })
  | Pexp_lazy (a) ->
    let a = expression_to_ast a in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_lazy { a })
  | Pexp_poly (a, b) ->
    let a = expression_to_ast a in
    let b = Optional.map ~f:(core_type_to_ast) b in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_poly { a; b })
  | Pexp_object (a) ->
    let a = class_structure_to_ast a in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_object { a })
  | Pexp_newtype (a, b) ->
    let a = string_loc_to_ast a in
    let b = expression_to_ast b in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_newtype { a; b })
  | Pexp_pack (a) ->
    let a = module_expr_to_ast a in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_pack { a })
  | Pexp_open (a, b, c) ->
    let a = override_flag_to_ast a in
    let b = longident_loc_to_ast b in
    let c = expression_to_ast c in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_open { a; b; c })
  | Pexp_extension (a) ->
    let a = extension_to_ast a in
    Stable.V4_07.Expression_desc.of_concrete (Pexp_extension { a })
  | Pexp_unreachable ->
    Stable.V4_07.Expression_desc.of_concrete (Pexp_unreachable)

and override_expression_to_ast : Astlib_parsetree.override_expression -> Stable.V4_07.Override_expression.t =
  fun (a, b) ->
    let a = label_loc_to_ast a in
    let b = expression_to_ast b in
    Stable.V4_07.Override_expression.of_concrete (Override_expression { a; b })

and record_field_expression_to_ast : Astlib_parsetree.record_field_expression -> Stable.V4_07.Record_field_expression.t =
  fun (a, b) ->
    let a = longident_loc_to_ast a in
    let b = expression_to_ast b in
    Stable.V4_07.Record_field_expression.of_concrete (Record_field_expression { a; b })

and apply_arg_to_ast : Astlib_parsetree.apply_arg -> Stable.V4_07.Apply_arg.t =
  fun (a, b) ->
    let a = arg_label_to_ast a in
    let b = expression_to_ast b in
    Stable.V4_07.Apply_arg.of_concrete (Apply_arg { a; b })

and case_to_ast : Astlib_parsetree.case -> Stable.V4_07.Case.t =
  fun { pc_lhs; pc_guard; pc_rhs } ->
    let pc_lhs = pattern_to_ast pc_lhs in
    let pc_guard = Optional.map ~f:(expression_to_ast) pc_guard in
    let pc_rhs = expression_to_ast pc_rhs in
    Stable.V4_07.Case.of_concrete (Case { pc_lhs; pc_guard; pc_rhs })

and value_description_to_ast : Astlib_parsetree.value_description -> Stable.V4_07.Value_description.t =
  fun { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } ->
    let pval_name = string_loc_to_ast pval_name in
    let pval_type = core_type_to_ast pval_type in
    let pval_attributes = attributes_to_ast pval_attributes in
    Stable.V4_07.Value_description.of_concrete (Value_description { pval_name; pval_type; pval_prim; pval_attributes; pval_loc })

and type_declaration_to_ast : Astlib_parsetree.type_declaration -> Stable.V4_07.Type_declaration.t =
  fun { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } ->
    let ptype_name = string_loc_to_ast ptype_name in
    let ptype_params = List.map ~f:(type_param_to_ast) ptype_params in
    let ptype_cstrs = List.map ~f:(type_constraint_to_ast) ptype_cstrs in
    let ptype_kind = type_kind_to_ast ptype_kind in
    let ptype_private = private_flag_to_ast ptype_private in
    let ptype_manifest = Optional.map ~f:(core_type_to_ast) ptype_manifest in
    let ptype_attributes = attributes_to_ast ptype_attributes in
    Stable.V4_07.Type_declaration.of_concrete (Type_declaration { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc })

and type_param_to_ast : Astlib_parsetree.type_param -> Stable.V4_07.Type_param.t =
  fun (a, b) ->
    let a = core_type_to_ast a in
    let b = variance_to_ast b in
    Stable.V4_07.Type_param.of_concrete (Type_param { a; b })

and type_constraint_to_ast : Astlib_parsetree.type_constraint -> Stable.V4_07.Type_constraint.t =
  fun (a, b, c) ->
    let a = core_type_to_ast a in
    let b = core_type_to_ast b in
    Stable.V4_07.Type_constraint.of_concrete (Type_constraint { a; b; c })

and type_kind_to_ast : Astlib_parsetree.type_kind -> Stable.V4_07.Type_kind.t =
  function
  | Ptype_abstract ->
    Stable.V4_07.Type_kind.of_concrete (Ptype_abstract)
  | Ptype_variant (a) ->
    let a = List.map ~f:(constructor_declaration_to_ast) a in
    Stable.V4_07.Type_kind.of_concrete (Ptype_variant { a })
  | Ptype_record (a) ->
    let a = List.map ~f:(label_declaration_to_ast) a in
    Stable.V4_07.Type_kind.of_concrete (Ptype_record { a })
  | Ptype_open ->
    Stable.V4_07.Type_kind.of_concrete (Ptype_open)

and label_declaration_to_ast : Astlib_parsetree.label_declaration -> Stable.V4_07.Label_declaration.t =
  fun { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } ->
    let pld_name = string_loc_to_ast pld_name in
    let pld_mutable = mutable_flag_to_ast pld_mutable in
    let pld_type = core_type_to_ast pld_type in
    let pld_attributes = attributes_to_ast pld_attributes in
    Stable.V4_07.Label_declaration.of_concrete (Label_declaration { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes })

and constructor_declaration_to_ast : Astlib_parsetree.constructor_declaration -> Stable.V4_07.Constructor_declaration.t =
  fun { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } ->
    let pcd_name = string_loc_to_ast pcd_name in
    let pcd_args = constructor_arguments_to_ast pcd_args in
    let pcd_res = Optional.map ~f:(core_type_to_ast) pcd_res in
    let pcd_attributes = attributes_to_ast pcd_attributes in
    Stable.V4_07.Constructor_declaration.of_concrete (Constructor_declaration { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes })

and constructor_arguments_to_ast : Astlib_parsetree.constructor_arguments -> Stable.V4_07.Constructor_arguments.t =
  function
  | Pcstr_tuple (a) ->
    let a = List.map ~f:(core_type_to_ast) a in
    Stable.V4_07.Constructor_arguments.of_concrete (Pcstr_tuple { a })
  | Pcstr_record (a) ->
    let a = List.map ~f:(label_declaration_to_ast) a in
    Stable.V4_07.Constructor_arguments.of_concrete (Pcstr_record { a })

and type_extension_to_ast : Astlib_parsetree.type_extension -> Stable.V4_07.Type_extension.t =
  fun { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } ->
    let ptyext_path = longident_loc_to_ast ptyext_path in
    let ptyext_params = List.map ~f:(type_param_to_ast) ptyext_params in
    let ptyext_constructors = List.map ~f:(extension_constructor_to_ast) ptyext_constructors in
    let ptyext_private = private_flag_to_ast ptyext_private in
    let ptyext_attributes = attributes_to_ast ptyext_attributes in
    Stable.V4_07.Type_extension.of_concrete (Type_extension { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes })

and extension_constructor_to_ast : Astlib_parsetree.extension_constructor -> Stable.V4_07.Extension_constructor.t =
  fun { pext_name; pext_kind; pext_loc; pext_attributes } ->
    let pext_name = string_loc_to_ast pext_name in
    let pext_kind = extension_constructor_kind_to_ast pext_kind in
    let pext_attributes = attributes_to_ast pext_attributes in
    Stable.V4_07.Extension_constructor.of_concrete (Extension_constructor { pext_name; pext_kind; pext_loc; pext_attributes })

and extension_constructor_kind_to_ast : Astlib_parsetree.extension_constructor_kind -> Stable.V4_07.Extension_constructor_kind.t =
  function
  | Pext_decl (a, b) ->
    let a = constructor_arguments_to_ast a in
    let b = Optional.map ~f:(core_type_to_ast) b in
    Stable.V4_07.Extension_constructor_kind.of_concrete (Pext_decl { a; b })
  | Pext_rebind (a) ->
    let a = longident_loc_to_ast a in
    Stable.V4_07.Extension_constructor_kind.of_concrete (Pext_rebind { a })

and class_type_to_ast : Astlib_parsetree.class_type -> Stable.V4_07.Class_type.t =
  fun { pcty_desc; pcty_loc; pcty_attributes } ->
    let pcty_desc = class_type_desc_to_ast pcty_desc in
    let pcty_attributes = attributes_to_ast pcty_attributes in
    Stable.V4_07.Class_type.of_concrete (Class_type { pcty_desc; pcty_loc; pcty_attributes })

and class_type_desc_to_ast : Astlib_parsetree.class_type_desc -> Stable.V4_07.Class_type_desc.t =
  function
  | Pcty_constr (a, b) ->
    let a = longident_loc_to_ast a in
    let b = List.map ~f:(core_type_to_ast) b in
    Stable.V4_07.Class_type_desc.of_concrete (Pcty_constr { a; b })
  | Pcty_signature (a) ->
    let a = class_signature_to_ast a in
    Stable.V4_07.Class_type_desc.of_concrete (Pcty_signature { a })
  | Pcty_arrow (a, b, c) ->
    let a = arg_label_to_ast a in
    let b = core_type_to_ast b in
    let c = class_type_to_ast c in
    Stable.V4_07.Class_type_desc.of_concrete (Pcty_arrow { a; b; c })
  | Pcty_extension (a) ->
    let a = extension_to_ast a in
    Stable.V4_07.Class_type_desc.of_concrete (Pcty_extension { a })
  | Pcty_open (a, b, c) ->
    let a = override_flag_to_ast a in
    let b = longident_loc_to_ast b in
    let c = class_type_to_ast c in
    Stable.V4_07.Class_type_desc.of_concrete (Pcty_open { a; b; c })

and class_signature_to_ast : Astlib_parsetree.class_signature -> Stable.V4_07.Class_signature.t =
  fun { pcsig_self; pcsig_fields } ->
    let pcsig_self = core_type_to_ast pcsig_self in
    let pcsig_fields = List.map ~f:(class_type_field_to_ast) pcsig_fields in
    Stable.V4_07.Class_signature.of_concrete (Class_signature { pcsig_self; pcsig_fields })

and class_type_field_to_ast : Astlib_parsetree.class_type_field -> Stable.V4_07.Class_type_field.t =
  fun { pctf_desc; pctf_loc; pctf_attributes } ->
    let pctf_desc = class_type_field_desc_to_ast pctf_desc in
    let pctf_attributes = attributes_to_ast pctf_attributes in
    Stable.V4_07.Class_type_field.of_concrete (Class_type_field { pctf_desc; pctf_loc; pctf_attributes })

and class_type_field_desc_to_ast : Astlib_parsetree.class_type_field_desc -> Stable.V4_07.Class_type_field_desc.t =
  function
  | Pctf_inherit (a) ->
    let a = class_type_to_ast a in
    Stable.V4_07.Class_type_field_desc.of_concrete (Pctf_inherit { a })
  | Pctf_val (a) ->
    let a = class_type_value_desc_to_ast a in
    Stable.V4_07.Class_type_field_desc.of_concrete (Pctf_val { a })
  | Pctf_method (a) ->
    let a = class_type_method_desc_to_ast a in
    Stable.V4_07.Class_type_field_desc.of_concrete (Pctf_method { a })
  | Pctf_constraint (a) ->
    let a = class_type_constraint_to_ast a in
    Stable.V4_07.Class_type_field_desc.of_concrete (Pctf_constraint { a })
  | Pctf_attribute (a) ->
    let a = attribute_to_ast a in
    Stable.V4_07.Class_type_field_desc.of_concrete (Pctf_attribute { a })
  | Pctf_extension (a) ->
    let a = extension_to_ast a in
    Stable.V4_07.Class_type_field_desc.of_concrete (Pctf_extension { a })

and class_type_value_desc_to_ast : Astlib_parsetree.class_type_value_desc -> Stable.V4_07.Class_type_value_desc.t =
  fun (a, b, c, d) ->
    let a = label_loc_to_ast a in
    let b = mutable_flag_to_ast b in
    let c = virtual_flag_to_ast c in
    let d = core_type_to_ast d in
    Stable.V4_07.Class_type_value_desc.of_concrete (Class_type_value_desc { a; b; c; d })

and class_type_method_desc_to_ast : Astlib_parsetree.class_type_method_desc -> Stable.V4_07.Class_type_method_desc.t =
  fun (a, b, c, d) ->
    let a = label_loc_to_ast a in
    let b = private_flag_to_ast b in
    let c = virtual_flag_to_ast c in
    let d = core_type_to_ast d in
    Stable.V4_07.Class_type_method_desc.of_concrete (Class_type_method_desc { a; b; c; d })

and class_type_constraint_to_ast : Astlib_parsetree.class_type_constraint -> Stable.V4_07.Class_type_constraint.t =
  fun (a, b) ->
    let a = core_type_to_ast a in
    let b = core_type_to_ast b in
    Stable.V4_07.Class_type_constraint.of_concrete (Class_type_constraint { a; b })

and class_description_to_ast : Astlib_parsetree.class_description -> Stable.V4_07.Class_description.t =
  fun { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } ->
    let pci_virt = virtual_flag_to_ast pci_virt in
    let pci_params = List.map ~f:(type_param_to_ast) pci_params in
    let pci_name = string_loc_to_ast pci_name in
    let pci_expr = class_type_to_ast pci_expr in
    let pci_attributes = attributes_to_ast pci_attributes in
    Stable.V4_07.Class_description.of_concrete (Class_description { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes })

and class_type_declaration_to_ast : Astlib_parsetree.class_type_declaration -> Stable.V4_07.Class_type_declaration.t =
  fun { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } ->
    let pci_virt = virtual_flag_to_ast pci_virt in
    let pci_params = List.map ~f:(type_param_to_ast) pci_params in
    let pci_name = string_loc_to_ast pci_name in
    let pci_expr = class_type_to_ast pci_expr in
    let pci_attributes = attributes_to_ast pci_attributes in
    Stable.V4_07.Class_type_declaration.of_concrete (Class_type_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes })

and class_expr_to_ast : Astlib_parsetree.class_expr -> Stable.V4_07.Class_expr.t =
  fun { pcl_desc; pcl_loc; pcl_attributes } ->
    let pcl_desc = class_expr_desc_to_ast pcl_desc in
    let pcl_attributes = attributes_to_ast pcl_attributes in
    Stable.V4_07.Class_expr.of_concrete (Class_expr { pcl_desc; pcl_loc; pcl_attributes })

and class_expr_desc_to_ast : Astlib_parsetree.class_expr_desc -> Stable.V4_07.Class_expr_desc.t =
  function
  | Pcl_constr (a, b) ->
    let a = longident_loc_to_ast a in
    let b = List.map ~f:(core_type_to_ast) b in
    Stable.V4_07.Class_expr_desc.of_concrete (Pcl_constr { a; b })
  | Pcl_structure (a) ->
    let a = class_structure_to_ast a in
    Stable.V4_07.Class_expr_desc.of_concrete (Pcl_structure { a })
  | Pcl_fun (a, b, c, d) ->
    let a = arg_label_to_ast a in
    let b = Optional.map ~f:(expression_to_ast) b in
    let c = pattern_to_ast c in
    let d = class_expr_to_ast d in
    Stable.V4_07.Class_expr_desc.of_concrete (Pcl_fun { a; b; c; d })
  | Pcl_apply (a, b) ->
    let a = class_expr_to_ast a in
    let b = List.map ~f:(apply_arg_to_ast) b in
    Stable.V4_07.Class_expr_desc.of_concrete (Pcl_apply { a; b })
  | Pcl_let (a, b, c) ->
    let a = rec_flag_to_ast a in
    let b = List.map ~f:(value_binding_to_ast) b in
    let c = class_expr_to_ast c in
    Stable.V4_07.Class_expr_desc.of_concrete (Pcl_let { a; b; c })
  | Pcl_constraint (a, b) ->
    let a = class_expr_to_ast a in
    let b = class_type_to_ast b in
    Stable.V4_07.Class_expr_desc.of_concrete (Pcl_constraint { a; b })
  | Pcl_extension (a) ->
    let a = extension_to_ast a in
    Stable.V4_07.Class_expr_desc.of_concrete (Pcl_extension { a })
  | Pcl_open (a, b, c) ->
    let a = override_flag_to_ast a in
    let b = longident_loc_to_ast b in
    let c = class_expr_to_ast c in
    Stable.V4_07.Class_expr_desc.of_concrete (Pcl_open { a; b; c })

and class_structure_to_ast : Astlib_parsetree.class_structure -> Stable.V4_07.Class_structure.t =
  fun { pcstr_self; pcstr_fields } ->
    let pcstr_self = pattern_to_ast pcstr_self in
    let pcstr_fields = List.map ~f:(class_field_to_ast) pcstr_fields in
    Stable.V4_07.Class_structure.of_concrete (Class_structure { pcstr_self; pcstr_fields })

and class_field_to_ast : Astlib_parsetree.class_field -> Stable.V4_07.Class_field.t =
  fun { pcf_desc; pcf_loc; pcf_attributes } ->
    let pcf_desc = class_field_desc_to_ast pcf_desc in
    let pcf_attributes = attributes_to_ast pcf_attributes in
    Stable.V4_07.Class_field.of_concrete (Class_field { pcf_desc; pcf_loc; pcf_attributes })

and class_field_desc_to_ast : Astlib_parsetree.class_field_desc -> Stable.V4_07.Class_field_desc.t =
  function
  | Pcf_inherit (a, b, c) ->
    let a = override_flag_to_ast a in
    let b = class_expr_to_ast b in
    let c = Optional.map ~f:(string_loc_to_ast) c in
    Stable.V4_07.Class_field_desc.of_concrete (Pcf_inherit { a; b; c })
  | Pcf_val (a) ->
    let a = class_value_desc_to_ast a in
    Stable.V4_07.Class_field_desc.of_concrete (Pcf_val { a })
  | Pcf_method (a) ->
    let a = class_method_desc_to_ast a in
    Stable.V4_07.Class_field_desc.of_concrete (Pcf_method { a })
  | Pcf_constraint (a) ->
    let a = class_type_constraint_to_ast a in
    Stable.V4_07.Class_field_desc.of_concrete (Pcf_constraint { a })
  | Pcf_initializer (a) ->
    let a = expression_to_ast a in
    Stable.V4_07.Class_field_desc.of_concrete (Pcf_initializer { a })
  | Pcf_attribute (a) ->
    let a = attribute_to_ast a in
    Stable.V4_07.Class_field_desc.of_concrete (Pcf_attribute { a })
  | Pcf_extension (a) ->
    let a = extension_to_ast a in
    Stable.V4_07.Class_field_desc.of_concrete (Pcf_extension { a })

and class_value_desc_to_ast : Astlib_parsetree.class_value_desc -> Stable.V4_07.Class_value_desc.t =
  fun (a, b, c) ->
    let a = label_loc_to_ast a in
    let b = mutable_flag_to_ast b in
    let c = class_field_kind_to_ast c in
    Stable.V4_07.Class_value_desc.of_concrete (Class_value_desc { a; b; c })

and class_method_desc_to_ast : Astlib_parsetree.class_method_desc -> Stable.V4_07.Class_method_desc.t =
  fun (a, b, c) ->
    let a = label_loc_to_ast a in
    let b = private_flag_to_ast b in
    let c = class_field_kind_to_ast c in
    Stable.V4_07.Class_method_desc.of_concrete (Class_method_desc { a; b; c })

and class_field_kind_to_ast : Astlib_parsetree.class_field_kind -> Stable.V4_07.Class_field_kind.t =
  function
  | Cfk_virtual (a) ->
    let a = core_type_to_ast a in
    Stable.V4_07.Class_field_kind.of_concrete (Cfk_virtual { a })
  | Cfk_concrete (a, b) ->
    let a = override_flag_to_ast a in
    let b = expression_to_ast b in
    Stable.V4_07.Class_field_kind.of_concrete (Cfk_concrete { a; b })

and class_declaration_to_ast : Astlib_parsetree.class_declaration -> Stable.V4_07.Class_declaration.t =
  fun { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } ->
    let pci_virt = virtual_flag_to_ast pci_virt in
    let pci_params = List.map ~f:(type_param_to_ast) pci_params in
    let pci_name = string_loc_to_ast pci_name in
    let pci_expr = class_expr_to_ast pci_expr in
    let pci_attributes = attributes_to_ast pci_attributes in
    Stable.V4_07.Class_declaration.of_concrete (Class_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes })

and module_type_to_ast : Astlib_parsetree.module_type -> Stable.V4_07.Module_type.t =
  fun { pmty_desc; pmty_loc; pmty_attributes } ->
    let pmty_desc = module_type_desc_to_ast pmty_desc in
    let pmty_attributes = attributes_to_ast pmty_attributes in
    Stable.V4_07.Module_type.of_concrete (Module_type { pmty_desc; pmty_loc; pmty_attributes })

and module_type_desc_to_ast : Astlib_parsetree.module_type_desc -> Stable.V4_07.Module_type_desc.t =
  function
  | Pmty_ident (a) ->
    let a = longident_loc_to_ast a in
    Stable.V4_07.Module_type_desc.of_concrete (Pmty_ident { a })
  | Pmty_signature (a) ->
    let a = signature_to_ast a in
    Stable.V4_07.Module_type_desc.of_concrete (Pmty_signature { a })
  | Pmty_functor (a, b, c) ->
    let a = string_loc_to_ast a in
    let b = Optional.map ~f:(module_type_to_ast) b in
    let c = module_type_to_ast c in
    Stable.V4_07.Module_type_desc.of_concrete (Pmty_functor { a; b; c })
  | Pmty_with (a, b) ->
    let a = module_type_to_ast a in
    let b = List.map ~f:(with_constraint_to_ast) b in
    Stable.V4_07.Module_type_desc.of_concrete (Pmty_with { a; b })
  | Pmty_typeof (a) ->
    let a = module_expr_to_ast a in
    Stable.V4_07.Module_type_desc.of_concrete (Pmty_typeof { a })
  | Pmty_extension (a) ->
    let a = extension_to_ast a in
    Stable.V4_07.Module_type_desc.of_concrete (Pmty_extension { a })
  | Pmty_alias (a) ->
    let a = longident_loc_to_ast a in
    Stable.V4_07.Module_type_desc.of_concrete (Pmty_alias { a })

and signature_to_ast : Astlib_parsetree.signature -> Stable.V4_07.Signature.t =
  fun a ->
    let a = List.map ~f:(signature_item_to_ast) a in
    Stable.V4_07.Signature.of_concrete (Signature { a })

and signature_item_to_ast : Astlib_parsetree.signature_item -> Stable.V4_07.Signature_item.t =
  fun { psig_desc; psig_loc } ->
    let psig_desc = signature_item_desc_to_ast psig_desc in
    Stable.V4_07.Signature_item.of_concrete (Signature_item { psig_desc; psig_loc })

and signature_item_desc_to_ast : Astlib_parsetree.signature_item_desc -> Stable.V4_07.Signature_item_desc.t =
  function
  | Psig_value (a) ->
    let a = value_description_to_ast a in
    Stable.V4_07.Signature_item_desc.of_concrete (Psig_value { a })
  | Psig_type (a, b) ->
    let a = rec_flag_to_ast a in
    let b = List.map ~f:(type_declaration_to_ast) b in
    Stable.V4_07.Signature_item_desc.of_concrete (Psig_type { a; b })
  | Psig_typext (a) ->
    let a = type_extension_to_ast a in
    Stable.V4_07.Signature_item_desc.of_concrete (Psig_typext { a })
  | Psig_exception (a) ->
    let a = extension_constructor_to_ast a in
    Stable.V4_07.Signature_item_desc.of_concrete (Psig_exception { a })
  | Psig_module (a) ->
    let a = module_declaration_to_ast a in
    Stable.V4_07.Signature_item_desc.of_concrete (Psig_module { a })
  | Psig_recmodule (a) ->
    let a = List.map ~f:(module_declaration_to_ast) a in
    Stable.V4_07.Signature_item_desc.of_concrete (Psig_recmodule { a })
  | Psig_modtype (a) ->
    let a = module_type_declaration_to_ast a in
    Stable.V4_07.Signature_item_desc.of_concrete (Psig_modtype { a })
  | Psig_open (a) ->
    let a = open_description_to_ast a in
    Stable.V4_07.Signature_item_desc.of_concrete (Psig_open { a })
  | Psig_include (a) ->
    let a = include_description_to_ast a in
    Stable.V4_07.Signature_item_desc.of_concrete (Psig_include { a })
  | Psig_class (a) ->
    let a = List.map ~f:(class_description_to_ast) a in
    Stable.V4_07.Signature_item_desc.of_concrete (Psig_class { a })
  | Psig_class_type (a) ->
    let a = List.map ~f:(class_type_declaration_to_ast) a in
    Stable.V4_07.Signature_item_desc.of_concrete (Psig_class_type { a })
  | Psig_attribute (a) ->
    let a = attribute_to_ast a in
    Stable.V4_07.Signature_item_desc.of_concrete (Psig_attribute { a })
  | Psig_extension (a, b) ->
    let a = extension_to_ast a in
    let b = attributes_to_ast b in
    Stable.V4_07.Signature_item_desc.of_concrete (Psig_extension { a; b })

and module_declaration_to_ast : Astlib_parsetree.module_declaration -> Stable.V4_07.Module_declaration.t =
  fun { pmd_name; pmd_type; pmd_attributes; pmd_loc } ->
    let pmd_name = string_loc_to_ast pmd_name in
    let pmd_type = module_type_to_ast pmd_type in
    let pmd_attributes = attributes_to_ast pmd_attributes in
    Stable.V4_07.Module_declaration.of_concrete (Module_declaration { pmd_name; pmd_type; pmd_attributes; pmd_loc })

and module_type_declaration_to_ast : Astlib_parsetree.module_type_declaration -> Stable.V4_07.Module_type_declaration.t =
  fun { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } ->
    let pmtd_name = string_loc_to_ast pmtd_name in
    let pmtd_type = Optional.map ~f:(module_type_to_ast) pmtd_type in
    let pmtd_attributes = attributes_to_ast pmtd_attributes in
    Stable.V4_07.Module_type_declaration.of_concrete (Module_type_declaration { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc })

and open_description_to_ast : Astlib_parsetree.open_description -> Stable.V4_07.Open_description.t =
  fun { popen_lid; popen_override; popen_loc; popen_attributes } ->
    let popen_lid = longident_loc_to_ast popen_lid in
    let popen_override = override_flag_to_ast popen_override in
    let popen_attributes = attributes_to_ast popen_attributes in
    Stable.V4_07.Open_description.of_concrete (Open_description { popen_lid; popen_override; popen_loc; popen_attributes })

and include_description_to_ast : Astlib_parsetree.include_description -> Stable.V4_07.Include_description.t =
  fun { pincl_mod; pincl_loc; pincl_attributes } ->
    let pincl_mod = module_type_to_ast pincl_mod in
    let pincl_attributes = attributes_to_ast pincl_attributes in
    Stable.V4_07.Include_description.of_concrete (Include_description { pincl_mod; pincl_loc; pincl_attributes })

and include_declaration_to_ast : Astlib_parsetree.include_declaration -> Stable.V4_07.Include_declaration.t =
  fun { pincl_mod; pincl_loc; pincl_attributes } ->
    let pincl_mod = module_expr_to_ast pincl_mod in
    let pincl_attributes = attributes_to_ast pincl_attributes in
    Stable.V4_07.Include_declaration.of_concrete (Include_declaration { pincl_mod; pincl_loc; pincl_attributes })

and with_constraint_to_ast : Astlib_parsetree.with_constraint -> Stable.V4_07.With_constraint.t =
  function
  | Pwith_type (a, b) ->
    let a = longident_loc_to_ast a in
    let b = type_declaration_to_ast b in
    Stable.V4_07.With_constraint.of_concrete (Pwith_type { a; b })
  | Pwith_module (a, b) ->
    let a = longident_loc_to_ast a in
    let b = longident_loc_to_ast b in
    Stable.V4_07.With_constraint.of_concrete (Pwith_module { a; b })
  | Pwith_typesubst (a, b) ->
    let a = longident_loc_to_ast a in
    let b = type_declaration_to_ast b in
    Stable.V4_07.With_constraint.of_concrete (Pwith_typesubst { a; b })
  | Pwith_modsubst (a, b) ->
    let a = longident_loc_to_ast a in
    let b = longident_loc_to_ast b in
    Stable.V4_07.With_constraint.of_concrete (Pwith_modsubst { a; b })

and module_expr_to_ast : Astlib_parsetree.module_expr -> Stable.V4_07.Module_expr.t =
  fun { pmod_desc; pmod_loc; pmod_attributes } ->
    let pmod_desc = module_expr_desc_to_ast pmod_desc in
    let pmod_attributes = attributes_to_ast pmod_attributes in
    Stable.V4_07.Module_expr.of_concrete (Module_expr { pmod_desc; pmod_loc; pmod_attributes })

and module_expr_desc_to_ast : Astlib_parsetree.module_expr_desc -> Stable.V4_07.Module_expr_desc.t =
  function
  | Pmod_ident (a) ->
    let a = longident_loc_to_ast a in
    Stable.V4_07.Module_expr_desc.of_concrete (Pmod_ident { a })
  | Pmod_structure (a) ->
    let a = structure_to_ast a in
    Stable.V4_07.Module_expr_desc.of_concrete (Pmod_structure { a })
  | Pmod_functor (a, b, c) ->
    let a = string_loc_to_ast a in
    let b = Optional.map ~f:(module_type_to_ast) b in
    let c = module_expr_to_ast c in
    Stable.V4_07.Module_expr_desc.of_concrete (Pmod_functor { a; b; c })
  | Pmod_apply (a, b) ->
    let a = module_expr_to_ast a in
    let b = module_expr_to_ast b in
    Stable.V4_07.Module_expr_desc.of_concrete (Pmod_apply { a; b })
  | Pmod_constraint (a, b) ->
    let a = module_expr_to_ast a in
    let b = module_type_to_ast b in
    Stable.V4_07.Module_expr_desc.of_concrete (Pmod_constraint { a; b })
  | Pmod_unpack (a) ->
    let a = expression_to_ast a in
    Stable.V4_07.Module_expr_desc.of_concrete (Pmod_unpack { a })
  | Pmod_extension (a) ->
    let a = extension_to_ast a in
    Stable.V4_07.Module_expr_desc.of_concrete (Pmod_extension { a })

and structure_to_ast : Astlib_parsetree.structure -> Stable.V4_07.Structure.t =
  fun a ->
    let a = List.map ~f:(structure_item_to_ast) a in
    Stable.V4_07.Structure.of_concrete (Structure { a })

and structure_item_to_ast : Astlib_parsetree.structure_item -> Stable.V4_07.Structure_item.t =
  fun { pstr_desc; pstr_loc } ->
    let pstr_desc = structure_item_desc_to_ast pstr_desc in
    Stable.V4_07.Structure_item.of_concrete (Structure_item { pstr_desc; pstr_loc })

and structure_item_desc_to_ast : Astlib_parsetree.structure_item_desc -> Stable.V4_07.Structure_item_desc.t =
  function
  | Pstr_eval (a, b) ->
    let a = expression_to_ast a in
    let b = attributes_to_ast b in
    Stable.V4_07.Structure_item_desc.of_concrete (Pstr_eval { a; b })
  | Pstr_value (a, b) ->
    let a = rec_flag_to_ast a in
    let b = List.map ~f:(value_binding_to_ast) b in
    Stable.V4_07.Structure_item_desc.of_concrete (Pstr_value { a; b })
  | Pstr_primitive (a) ->
    let a = value_description_to_ast a in
    Stable.V4_07.Structure_item_desc.of_concrete (Pstr_primitive { a })
  | Pstr_type (a, b) ->
    let a = rec_flag_to_ast a in
    let b = List.map ~f:(type_declaration_to_ast) b in
    Stable.V4_07.Structure_item_desc.of_concrete (Pstr_type { a; b })
  | Pstr_typext (a) ->
    let a = type_extension_to_ast a in
    Stable.V4_07.Structure_item_desc.of_concrete (Pstr_typext { a })
  | Pstr_exception (a) ->
    let a = extension_constructor_to_ast a in
    Stable.V4_07.Structure_item_desc.of_concrete (Pstr_exception { a })
  | Pstr_module (a) ->
    let a = module_binding_to_ast a in
    Stable.V4_07.Structure_item_desc.of_concrete (Pstr_module { a })
  | Pstr_recmodule (a) ->
    let a = List.map ~f:(module_binding_to_ast) a in
    Stable.V4_07.Structure_item_desc.of_concrete (Pstr_recmodule { a })
  | Pstr_modtype (a) ->
    let a = module_type_declaration_to_ast a in
    Stable.V4_07.Structure_item_desc.of_concrete (Pstr_modtype { a })
  | Pstr_open (a) ->
    let a = open_description_to_ast a in
    Stable.V4_07.Structure_item_desc.of_concrete (Pstr_open { a })
  | Pstr_class (a) ->
    let a = List.map ~f:(class_declaration_to_ast) a in
    Stable.V4_07.Structure_item_desc.of_concrete (Pstr_class { a })
  | Pstr_class_type (a) ->
    let a = List.map ~f:(class_type_declaration_to_ast) a in
    Stable.V4_07.Structure_item_desc.of_concrete (Pstr_class_type { a })
  | Pstr_include (a) ->
    let a = include_declaration_to_ast a in
    Stable.V4_07.Structure_item_desc.of_concrete (Pstr_include { a })
  | Pstr_attribute (a) ->
    let a = attribute_to_ast a in
    Stable.V4_07.Structure_item_desc.of_concrete (Pstr_attribute { a })
  | Pstr_extension (a, b) ->
    let a = extension_to_ast a in
    let b = attributes_to_ast b in
    Stable.V4_07.Structure_item_desc.of_concrete (Pstr_extension { a; b })

and value_binding_to_ast : Astlib_parsetree.value_binding -> Stable.V4_07.Value_binding.t =
  fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } ->
    let pvb_pat = pattern_to_ast pvb_pat in
    let pvb_expr = expression_to_ast pvb_expr in
    let pvb_attributes = attributes_to_ast pvb_attributes in
    Stable.V4_07.Value_binding.of_concrete (Value_binding { pvb_pat; pvb_expr; pvb_attributes; pvb_loc })

and module_binding_to_ast : Astlib_parsetree.module_binding -> Stable.V4_07.Module_binding.t =
  fun { pmb_name; pmb_expr; pmb_attributes; pmb_loc } ->
    let pmb_name = string_loc_to_ast pmb_name in
    let pmb_expr = module_expr_to_ast pmb_expr in
    let pmb_attributes = attributes_to_ast pmb_attributes in
    Stable.V4_07.Module_binding.of_concrete (Module_binding { pmb_name; pmb_expr; pmb_attributes; pmb_loc })

and toplevel_phrase_to_ast : Astlib_parsetree.toplevel_phrase -> Stable.V4_07.Toplevel_phrase.t =
  function
  | Ptop_def (a) ->
    let a = structure_to_ast a in
    Stable.V4_07.Toplevel_phrase.of_concrete (Ptop_def { a })
  | Ptop_dir (a, b) ->
    let b = directive_argument_to_ast b in
    Stable.V4_07.Toplevel_phrase.of_concrete (Ptop_dir { a; b })

and directive_argument_to_ast : Astlib_parsetree.directive_argument -> Stable.V4_07.Directive_argument.t =
  function
  | Pdir_none ->
    Stable.V4_07.Directive_argument.of_concrete (Pdir_none)
  | Pdir_string (a) ->
    Stable.V4_07.Directive_argument.of_concrete (Pdir_string { a })
  | Pdir_int (a, b) ->
    Stable.V4_07.Directive_argument.of_concrete (Pdir_int { a; b })
  | Pdir_ident (a) ->
    let a = longident_to_ast a in
    Stable.V4_07.Directive_argument.of_concrete (Pdir_ident { a })
  | Pdir_bool (a) ->
    Stable.V4_07.Directive_argument.of_concrete (Pdir_bool { a })
(*$*)
