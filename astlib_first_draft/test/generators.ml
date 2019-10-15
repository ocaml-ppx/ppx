module Base_quickcheck = struct
  module Generator = struct
    include Base_quickcheck.Generator

    (* available in [Base_quickcheck] snapshot *)
    let of_lazy lazy_t =
      create (fun ~size ~random ->
        generate (Lazy.force lazy_t) ~size ~random)
  end

  module Shrinker = Base_quickcheck.Shrinker
end

(*$ Astlib_first_draft_test_cinaps.print_generators_ml () *)
open! Base
open! Ocaml_common
open Base_quickcheck.Generator.Let_syntax

let with_decremented_size generator =
  let%bind size = Base_quickcheck.Generator.size in
  Base_quickcheck.Generator.with_size generator ~size:(size - 1)

module V4_07 = struct
  let rec longident_generator =
    lazy begin
      let%map concrete =
        let lident_gen =
          let%bind a =
            Base_quickcheck.Generator.string_non_empty
          in
          return (Astlib_first_draft.V4_07.Longident.Concrete.Lident { a })
        in
        let ldot_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_generator)
          and b =
            Base_quickcheck.Generator.string_non_empty
          in
          return (Astlib_first_draft.V4_07.Longident.Concrete.Ldot { a; b })
        in
        let lapply_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy longident_generator)
          in
          return (Astlib_first_draft.V4_07.Longident.Concrete.Lapply { a; b })
        in
        let non_recursive_alist =
          [ 1. /. 1., lident_gen
          ]
        and recursive_alist =
          [ 1. /. 2., ldot_gen
          ; 1. /. 3., lapply_gen
          ]
          |> List.map ~f:(fun (weight, gen) -> (weight, with_decremented_size gen))
        in
        let non_recursive_gen =
          Base_quickcheck.Generator.weighted_union non_recursive_alist
        and recursive_gen =
          Base_quickcheck.Generator.weighted_union
            (non_recursive_alist @ recursive_alist)
        in
        match%bind Base_quickcheck.Generator.size with
        | 0 -> non_recursive_gen
        | _ -> recursive_gen
      in
      Astlib_first_draft.V4_07.Longident.of_concrete concrete
    end

  and longident_loc_generator =
    lazy begin
      let%map concrete =
        let longident_loc_gen =
          let%bind txt =
            (Base_quickcheck.Generator.of_lazy longident_generator)
          and loc =
            (Base_quickcheck.Generator.return Location.none)
          in
          return (Astlib_first_draft.V4_07.Longident_loc.Concrete.Longident_loc { txt; loc })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 2., longident_loc_gen
          ]
      in
      Astlib_first_draft.V4_07.Longident_loc.of_concrete concrete
    end

  and rec_flag_generator =
    lazy begin
      let%map concrete =
        let nonrecursive_gen =
          return (Astlib_first_draft.V4_07.Rec_flag.Concrete.Nonrecursive)
        in
        let recursive_gen =
          return (Astlib_first_draft.V4_07.Rec_flag.Concrete.Recursive)
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 1., nonrecursive_gen
          ; 1. /. 1., recursive_gen
          ]
      in
      Astlib_first_draft.V4_07.Rec_flag.of_concrete concrete
    end

  and direction_flag_generator =
    lazy begin
      let%map concrete =
        let upto_gen =
          return (Astlib_first_draft.V4_07.Direction_flag.Concrete.Upto)
        in
        let downto_gen =
          return (Astlib_first_draft.V4_07.Direction_flag.Concrete.Downto)
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 1., upto_gen
          ; 1. /. 1., downto_gen
          ]
      in
      Astlib_first_draft.V4_07.Direction_flag.of_concrete concrete
    end

  and private_flag_generator =
    lazy begin
      let%map concrete =
        let private_gen =
          return (Astlib_first_draft.V4_07.Private_flag.Concrete.Private)
        in
        let public_gen =
          return (Astlib_first_draft.V4_07.Private_flag.Concrete.Public)
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 1., private_gen
          ; 1. /. 1., public_gen
          ]
      in
      Astlib_first_draft.V4_07.Private_flag.of_concrete concrete
    end

  and mutable_flag_generator =
    lazy begin
      let%map concrete =
        let immutable_gen =
          return (Astlib_first_draft.V4_07.Mutable_flag.Concrete.Immutable)
        in
        let mutable_gen =
          return (Astlib_first_draft.V4_07.Mutable_flag.Concrete.Mutable)
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 1., immutable_gen
          ; 1. /. 1., mutable_gen
          ]
      in
      Astlib_first_draft.V4_07.Mutable_flag.of_concrete concrete
    end

  and virtual_flag_generator =
    lazy begin
      let%map concrete =
        let virtual_gen =
          return (Astlib_first_draft.V4_07.Virtual_flag.Concrete.Virtual)
        in
        let concrete_gen =
          return (Astlib_first_draft.V4_07.Virtual_flag.Concrete.Concrete)
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 1., virtual_gen
          ; 1. /. 1., concrete_gen
          ]
      in
      Astlib_first_draft.V4_07.Virtual_flag.of_concrete concrete
    end

  and override_flag_generator =
    lazy begin
      let%map concrete =
        let override_gen =
          return (Astlib_first_draft.V4_07.Override_flag.Concrete.Override)
        in
        let fresh_gen =
          return (Astlib_first_draft.V4_07.Override_flag.Concrete.Fresh)
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 1., override_gen
          ; 1. /. 1., fresh_gen
          ]
      in
      Astlib_first_draft.V4_07.Override_flag.of_concrete concrete
    end

  and closed_flag_generator =
    lazy begin
      let%map concrete =
        let closed_gen =
          return (Astlib_first_draft.V4_07.Closed_flag.Concrete.Closed)
        in
        let open_gen =
          return (Astlib_first_draft.V4_07.Closed_flag.Concrete.Open)
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 1., closed_gen
          ; 1. /. 1., open_gen
          ]
      in
      Astlib_first_draft.V4_07.Closed_flag.of_concrete concrete
    end

  and label_generator =
    lazy begin
      let%map concrete =
        let label_gen =
          let%bind a =
            Base_quickcheck.Generator.string_non_empty
          in
          return (Astlib_first_draft.V4_07.Label.Concrete.Label { a })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 1., label_gen
          ]
      in
      Astlib_first_draft.V4_07.Label.of_concrete concrete
    end

  and label_loc_generator =
    lazy begin
      let%map concrete =
        let label_loc_gen =
          let%bind txt =
            (Base_quickcheck.Generator.of_lazy label_generator)
          and loc =
            (Base_quickcheck.Generator.return Location.none)
          in
          return (Astlib_first_draft.V4_07.Label_loc.Concrete.Label_loc { txt; loc })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 2., label_loc_gen
          ]
      in
      Astlib_first_draft.V4_07.Label_loc.of_concrete concrete
    end

  and string_loc_generator =
    lazy begin
      let%map concrete =
        let string_loc_gen =
          let%bind txt =
            Base_quickcheck.Generator.string_non_empty
          and loc =
            (Base_quickcheck.Generator.return Location.none)
          in
          return (Astlib_first_draft.V4_07.String_loc.Concrete.String_loc { txt; loc })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 1., string_loc_gen
          ]
      in
      Astlib_first_draft.V4_07.String_loc.of_concrete concrete
    end

  and arg_label_generator =
    lazy begin
      let%map concrete =
        let nolabel_gen =
          return (Astlib_first_draft.V4_07.Arg_label.Concrete.Nolabel)
        in
        let labelled_gen =
          let%bind a =
            Base_quickcheck.Generator.string_non_empty
          in
          return (Astlib_first_draft.V4_07.Arg_label.Concrete.Labelled { a })
        in
        let optional_gen =
          let%bind a =
            Base_quickcheck.Generator.string_non_empty
          in
          return (Astlib_first_draft.V4_07.Arg_label.Concrete.Optional { a })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 1., nolabel_gen
          ; 1. /. 1., labelled_gen
          ; 1. /. 1., optional_gen
          ]
      in
      Astlib_first_draft.V4_07.Arg_label.of_concrete concrete
    end

  and variance_generator =
    lazy begin
      let%map concrete =
        let covariant_gen =
          return (Astlib_first_draft.V4_07.Variance.Concrete.Covariant)
        in
        let contravariant_gen =
          return (Astlib_first_draft.V4_07.Variance.Concrete.Contravariant)
        in
        let invariant_gen =
          return (Astlib_first_draft.V4_07.Variance.Concrete.Invariant)
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 1., covariant_gen
          ; 1. /. 1., contravariant_gen
          ; 1. /. 1., invariant_gen
          ]
      in
      Astlib_first_draft.V4_07.Variance.of_concrete concrete
    end

  and constant_generator =
    lazy begin
      let%map concrete =
        let pconst_integer_gen =
          let%bind a =
            Base_quickcheck.Generator.string_non_empty
          and b =
            (Base_quickcheck.Generator.option Base_quickcheck.Generator.char)
          in
          return (Astlib_first_draft.V4_07.Constant.Concrete.Pconst_integer { a; b })
        in
        let pconst_char_gen =
          let%bind a =
            Base_quickcheck.Generator.char
          in
          return (Astlib_first_draft.V4_07.Constant.Concrete.Pconst_char { a })
        in
        let pconst_string_gen =
          let%bind a =
            Base_quickcheck.Generator.string_non_empty
          and b =
            (Base_quickcheck.Generator.option Base_quickcheck.Generator.string_non_empty)
          in
          return (Astlib_first_draft.V4_07.Constant.Concrete.Pconst_string { a; b })
        in
        let pconst_float_gen =
          let%bind a =
            Base_quickcheck.Generator.string_non_empty
          and b =
            (Base_quickcheck.Generator.option Base_quickcheck.Generator.char)
          in
          return (Astlib_first_draft.V4_07.Constant.Concrete.Pconst_float { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 1., pconst_integer_gen
          ; 1. /. 1., pconst_char_gen
          ; 1. /. 1., pconst_string_gen
          ; 1. /. 1., pconst_float_gen
          ]
      in
      Astlib_first_draft.V4_07.Constant.of_concrete concrete
    end

  and attribute_generator =
    lazy begin
      let%map concrete =
        let attribute_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy payload_generator)
          in
          return (Astlib_first_draft.V4_07.Attribute.Concrete.Attribute { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., attribute_gen
          ]
      in
      Astlib_first_draft.V4_07.Attribute.of_concrete concrete
    end

  and extension_generator =
    lazy begin
      let%map concrete =
        let extension_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy payload_generator)
          in
          return (Astlib_first_draft.V4_07.Extension.Concrete.Extension { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., extension_gen
          ]
      in
      Astlib_first_draft.V4_07.Extension.of_concrete concrete
    end

  and attributes_generator =
    lazy begin
      let%map concrete =
        let attributes_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy attribute_generator))
          in
          return (Astlib_first_draft.V4_07.Attributes.Concrete.Attributes { a })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 2., attributes_gen
          ]
      in
      Astlib_first_draft.V4_07.Attributes.of_concrete concrete
    end

  and payload_generator =
    lazy begin
      let%map concrete =
        let pstr_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy structure_generator)
          in
          return (Astlib_first_draft.V4_07.Payload.Concrete.PStr { a })
        in
        let psig_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy signature_generator)
          in
          return (Astlib_first_draft.V4_07.Payload.Concrete.PSig { a })
        in
        let ptyp_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          in
          return (Astlib_first_draft.V4_07.Payload.Concrete.PTyp { a })
        in
        let ppat_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy pattern_generator)
          and b =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy expression_generator))
          in
          return (Astlib_first_draft.V4_07.Payload.Concrete.PPat { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 2., pstr_gen
          ; 1. /. 2., psig_gen
          ; 1. /. 2., ptyp_gen
          ; 1. /. 3., ppat_gen
          ]
      in
      Astlib_first_draft.V4_07.Payload.of_concrete concrete
    end

  and core_type_generator =
    lazy begin
      let%map concrete =
        let core_type_gen =
          let%bind ptyp_desc =
            (Base_quickcheck.Generator.of_lazy core_type_desc_generator)
          and ptyp_loc =
            (Base_quickcheck.Generator.return Location.none)
          and ptyp_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Core_type.Concrete.Core_type { ptyp_desc; ptyp_loc; ptyp_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., core_type_gen
          ]
      in
      Astlib_first_draft.V4_07.Core_type.of_concrete concrete
    end

  and core_type_desc_generator =
    lazy begin
      let%map concrete =
        let ptyp_any_gen =
          return (Astlib_first_draft.V4_07.Core_type_desc.Concrete.Ptyp_any)
        in
        let ptyp_var_gen =
          let%bind a =
            Base_quickcheck.Generator.string_non_empty
          in
          return (Astlib_first_draft.V4_07.Core_type_desc.Concrete.Ptyp_var { a })
        in
        let ptyp_arrow_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy arg_label_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          and c =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          in
          return (Astlib_first_draft.V4_07.Core_type_desc.Concrete.Ptyp_arrow { a; b; c })
        in
        let ptyp_tuple_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy core_type_generator))
          in
          return (Astlib_first_draft.V4_07.Core_type_desc.Concrete.Ptyp_tuple { a })
        in
        let ptyp_constr_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and b =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy core_type_generator))
          in
          return (Astlib_first_draft.V4_07.Core_type_desc.Concrete.Ptyp_constr { a; b })
        in
        let ptyp_object_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy object_field_generator))
          and b =
            (Base_quickcheck.Generator.of_lazy closed_flag_generator)
          in
          return (Astlib_first_draft.V4_07.Core_type_desc.Concrete.Ptyp_object { a; b })
        in
        let ptyp_class_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and b =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy core_type_generator))
          in
          return (Astlib_first_draft.V4_07.Core_type_desc.Concrete.Ptyp_class { a; b })
        in
        let ptyp_alias_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          and b =
            Base_quickcheck.Generator.string_non_empty
          in
          return (Astlib_first_draft.V4_07.Core_type_desc.Concrete.Ptyp_alias { a; b })
        in
        let ptyp_variant_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy row_field_generator))
          and b =
            (Base_quickcheck.Generator.of_lazy closed_flag_generator)
          and c =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy label_generator)))
          in
          return (Astlib_first_draft.V4_07.Core_type_desc.Concrete.Ptyp_variant { a; b; c })
        in
        let ptyp_poly_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy string_loc_generator))
          and b =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          in
          return (Astlib_first_draft.V4_07.Core_type_desc.Concrete.Ptyp_poly { a; b })
        in
        let ptyp_package_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy package_type_generator)
          in
          return (Astlib_first_draft.V4_07.Core_type_desc.Concrete.Ptyp_package { a })
        in
        let ptyp_extension_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy extension_generator)
          in
          return (Astlib_first_draft.V4_07.Core_type_desc.Concrete.Ptyp_extension { a })
        in
        let non_recursive_alist =
          [ 1. /. 1., ptyp_any_gen
          ; 1. /. 1., ptyp_var_gen
          ]
        and recursive_alist =
          [ 1. /. 4., ptyp_arrow_gen
          ; 1. /. 2., ptyp_tuple_gen
          ; 1. /. 3., ptyp_constr_gen
          ; 1. /. 3., ptyp_object_gen
          ; 1. /. 3., ptyp_class_gen
          ; 1. /. 2., ptyp_alias_gen
          ; 1. /. 4., ptyp_variant_gen
          ; 1. /. 3., ptyp_poly_gen
          ; 1. /. 2., ptyp_package_gen
          ; 1. /. 2., ptyp_extension_gen
          ]
          |> List.map ~f:(fun (weight, gen) -> (weight, with_decremented_size gen))
        in
        let non_recursive_gen =
          Base_quickcheck.Generator.weighted_union non_recursive_alist
        and recursive_gen =
          Base_quickcheck.Generator.weighted_union
            (non_recursive_alist @ recursive_alist)
        in
        match%bind Base_quickcheck.Generator.size with
        | 0 -> non_recursive_gen
        | _ -> recursive_gen
      in
      Astlib_first_draft.V4_07.Core_type_desc.of_concrete concrete
    end

  and package_type_generator =
    lazy begin
      let%map concrete =
        let package_type_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and b =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy package_type_constraint_generator))
          in
          return (Astlib_first_draft.V4_07.Package_type.Concrete.Package_type { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., package_type_gen
          ]
      in
      Astlib_first_draft.V4_07.Package_type.of_concrete concrete
    end

  and package_type_constraint_generator =
    lazy begin
      let%map concrete =
        let package_type_constraint_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          in
          return (Astlib_first_draft.V4_07.Package_type_constraint.Concrete.Package_type_constraint { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., package_type_constraint_gen
          ]
      in
      Astlib_first_draft.V4_07.Package_type_constraint.of_concrete concrete
    end

  and row_field_generator =
    lazy begin
      let%map concrete =
        let rtag_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy label_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          and c =
            Base_quickcheck.Generator.bool
          and d =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy core_type_generator))
          in
          return (Astlib_first_draft.V4_07.Row_field.Concrete.Rtag { a; b; c; d })
        in
        let rinherit_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          in
          return (Astlib_first_draft.V4_07.Row_field.Concrete.Rinherit { a })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 4., rtag_gen
          ; 1. /. 2., rinherit_gen
          ]
      in
      Astlib_first_draft.V4_07.Row_field.of_concrete concrete
    end

  and object_field_generator =
    lazy begin
      let%map concrete =
        let otag_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy label_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          and c =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          in
          return (Astlib_first_draft.V4_07.Object_field.Concrete.Otag { a; b; c })
        in
        let oinherit_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          in
          return (Astlib_first_draft.V4_07.Object_field.Concrete.Oinherit { a })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 4., otag_gen
          ; 1. /. 2., oinherit_gen
          ]
      in
      Astlib_first_draft.V4_07.Object_field.of_concrete concrete
    end

  and pattern_generator =
    lazy begin
      let%map concrete =
        let pattern_gen =
          let%bind ppat_desc =
            (Base_quickcheck.Generator.of_lazy pattern_desc_generator)
          and ppat_loc =
            (Base_quickcheck.Generator.return Location.none)
          and ppat_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Pattern.Concrete.Pattern { ppat_desc; ppat_loc; ppat_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., pattern_gen
          ]
      in
      Astlib_first_draft.V4_07.Pattern.of_concrete concrete
    end

  and pattern_desc_generator =
    lazy begin
      let%map concrete =
        let ppat_any_gen =
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_any)
        in
        let ppat_var_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_var { a })
        in
        let ppat_alias_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy pattern_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_alias { a; b })
        in
        let ppat_constant_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy constant_generator)
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_constant { a })
        in
        let ppat_interval_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy constant_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy constant_generator)
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_interval { a; b })
        in
        let ppat_tuple_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy pattern_generator))
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_tuple { a })
        in
        let ppat_construct_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and b =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy pattern_generator))
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_construct { a; b })
        in
        let ppat_variant_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy label_generator)
          and b =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy pattern_generator))
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_variant { a; b })
        in
        let ppat_record_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy record_field_pattern_generator))
          and b =
            (Base_quickcheck.Generator.of_lazy closed_flag_generator)
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_record { a; b })
        in
        let ppat_array_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy pattern_generator))
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_array { a })
        in
        let ppat_or_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy pattern_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy pattern_generator)
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_or { a; b })
        in
        let ppat_constraint_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy pattern_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_constraint { a; b })
        in
        let ppat_type_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_type { a })
        in
        let ppat_lazy_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy pattern_generator)
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_lazy { a })
        in
        let ppat_unpack_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_unpack { a })
        in
        let ppat_exception_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy pattern_generator)
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_exception { a })
        in
        let ppat_extension_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy extension_generator)
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_extension { a })
        in
        let ppat_open_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy pattern_generator)
          in
          return (Astlib_first_draft.V4_07.Pattern_desc.Concrete.Ppat_open { a; b })
        in
        let non_recursive_alist =
          [ 1. /. 1., ppat_any_gen
          ]
        and recursive_alist =
          [ 1. /. 2., ppat_var_gen
          ; 1. /. 3., ppat_alias_gen
          ; 1. /. 2., ppat_constant_gen
          ; 1. /. 3., ppat_interval_gen
          ; 1. /. 2., ppat_tuple_gen
          ; 1. /. 3., ppat_construct_gen
          ; 1. /. 3., ppat_variant_gen
          ; 1. /. 3., ppat_record_gen
          ; 1. /. 2., ppat_array_gen
          ; 1. /. 3., ppat_or_gen
          ; 1. /. 3., ppat_constraint_gen
          ; 1. /. 2., ppat_type_gen
          ; 1. /. 2., ppat_lazy_gen
          ; 1. /. 2., ppat_unpack_gen
          ; 1. /. 2., ppat_exception_gen
          ; 1. /. 2., ppat_extension_gen
          ; 1. /. 3., ppat_open_gen
          ]
          |> List.map ~f:(fun (weight, gen) -> (weight, with_decremented_size gen))
        in
        let non_recursive_gen =
          Base_quickcheck.Generator.weighted_union non_recursive_alist
        and recursive_gen =
          Base_quickcheck.Generator.weighted_union
            (non_recursive_alist @ recursive_alist)
        in
        match%bind Base_quickcheck.Generator.size with
        | 0 -> non_recursive_gen
        | _ -> recursive_gen
      in
      Astlib_first_draft.V4_07.Pattern_desc.of_concrete concrete
    end

  and record_field_pattern_generator =
    lazy begin
      let%map concrete =
        let record_field_pattern_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy pattern_generator)
          in
          return (Astlib_first_draft.V4_07.Record_field_pattern.Concrete.Record_field_pattern { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., record_field_pattern_gen
          ]
      in
      Astlib_first_draft.V4_07.Record_field_pattern.of_concrete concrete
    end

  and expression_generator =
    lazy begin
      let%map concrete =
        let expression_gen =
          let%bind pexp_desc =
            (Base_quickcheck.Generator.of_lazy expression_desc_generator)
          and pexp_loc =
            (Base_quickcheck.Generator.return Location.none)
          and pexp_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Expression.Concrete.Expression { pexp_desc; pexp_loc; pexp_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., expression_gen
          ]
      in
      Astlib_first_draft.V4_07.Expression.of_concrete concrete
    end

  and expression_desc_generator =
    lazy begin
      let%map concrete =
        let pexp_ident_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_ident { a })
        in
        let pexp_constant_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy constant_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_constant { a })
        in
        let pexp_let_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy rec_flag_generator)
          and b =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy value_binding_generator))
          and c =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_let { a; b; c })
        in
        let pexp_function_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy case_generator))
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_function { a })
        in
        let pexp_fun_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy arg_label_generator)
          and b =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy expression_generator))
          and c =
            (Base_quickcheck.Generator.of_lazy pattern_generator)
          and d =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_fun { a; b; c; d })
        in
        let pexp_apply_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and b =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy apply_arg_generator))
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_apply { a; b })
        in
        let pexp_match_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and b =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy case_generator))
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_match { a; b })
        in
        let pexp_try_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and b =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy case_generator))
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_try { a; b })
        in
        let pexp_tuple_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy expression_generator))
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_tuple { a })
        in
        let pexp_construct_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and b =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy expression_generator))
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_construct { a; b })
        in
        let pexp_variant_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy label_generator)
          and b =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy expression_generator))
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_variant { a; b })
        in
        let pexp_record_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy record_field_expression_generator))
          and b =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy expression_generator))
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_record { a; b })
        in
        let pexp_field_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_field { a; b })
        in
        let pexp_setfield_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and c =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_setfield { a; b; c })
        in
        let pexp_array_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy expression_generator))
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_array { a })
        in
        let pexp_ifthenelse_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and c =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy expression_generator))
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_ifthenelse { a; b; c })
        in
        let pexp_sequence_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_sequence { a; b })
        in
        let pexp_while_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_while { a; b })
        in
        let pexp_for_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy pattern_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and c =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and d =
            (Base_quickcheck.Generator.of_lazy direction_flag_generator)
          and e =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_for { a; b; c; d; e })
        in
        let pexp_constraint_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_constraint { a; b })
        in
        let pexp_coerce_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and b =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy core_type_generator))
          and c =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_coerce { a; b; c })
        in
        let pexp_send_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy label_loc_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_send { a; b })
        in
        let pexp_new_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_new { a })
        in
        let pexp_setinstvar_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy label_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_setinstvar { a; b })
        in
        let pexp_override_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy override_expression_generator))
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_override { a })
        in
        let pexp_letmodule_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy module_expr_generator)
          and c =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_letmodule { a; b; c })
        in
        let pexp_letexception_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy extension_constructor_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_letexception { a; b })
        in
        let pexp_assert_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_assert { a })
        in
        let pexp_lazy_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_lazy { a })
        in
        let pexp_poly_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and b =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy core_type_generator))
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_poly { a; b })
        in
        let pexp_object_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy class_structure_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_object { a })
        in
        let pexp_newtype_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_newtype { a; b })
        in
        let pexp_pack_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy module_expr_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_pack { a })
        in
        let pexp_open_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy override_flag_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and c =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_open { a; b; c })
        in
        let pexp_extension_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy extension_generator)
          in
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_extension { a })
        in
        let pexp_unreachable_gen =
          return (Astlib_first_draft.V4_07.Expression_desc.Concrete.Pexp_unreachable)
        in
        let non_recursive_alist =
          [ 1. /. 1., pexp_unreachable_gen
          ]
        and recursive_alist =
          [ 1. /. 2., pexp_ident_gen
          ; 1. /. 2., pexp_constant_gen
          ; 1. /. 4., pexp_let_gen
          ; 1. /. 2., pexp_function_gen
          ; 1. /. 5., pexp_fun_gen
          ; 1. /. 3., pexp_apply_gen
          ; 1. /. 3., pexp_match_gen
          ; 1. /. 3., pexp_try_gen
          ; 1. /. 2., pexp_tuple_gen
          ; 1. /. 3., pexp_construct_gen
          ; 1. /. 3., pexp_variant_gen
          ; 1. /. 3., pexp_record_gen
          ; 1. /. 3., pexp_field_gen
          ; 1. /. 4., pexp_setfield_gen
          ; 1. /. 2., pexp_array_gen
          ; 1. /. 4., pexp_ifthenelse_gen
          ; 1. /. 3., pexp_sequence_gen
          ; 1. /. 3., pexp_while_gen
          ; 1. /. 6., pexp_for_gen
          ; 1. /. 3., pexp_constraint_gen
          ; 1. /. 4., pexp_coerce_gen
          ; 1. /. 3., pexp_send_gen
          ; 1. /. 2., pexp_new_gen
          ; 1. /. 3., pexp_setinstvar_gen
          ; 1. /. 2., pexp_override_gen
          ; 1. /. 4., pexp_letmodule_gen
          ; 1. /. 3., pexp_letexception_gen
          ; 1. /. 2., pexp_assert_gen
          ; 1. /. 2., pexp_lazy_gen
          ; 1. /. 3., pexp_poly_gen
          ; 1. /. 2., pexp_object_gen
          ; 1. /. 3., pexp_newtype_gen
          ; 1. /. 2., pexp_pack_gen
          ; 1. /. 4., pexp_open_gen
          ; 1. /. 2., pexp_extension_gen
          ]
          |> List.map ~f:(fun (weight, gen) -> (weight, with_decremented_size gen))
        in
        let non_recursive_gen =
          Base_quickcheck.Generator.weighted_union non_recursive_alist
        and recursive_gen =
          Base_quickcheck.Generator.weighted_union
            (non_recursive_alist @ recursive_alist)
        in
        match%bind Base_quickcheck.Generator.size with
        | 0 -> non_recursive_gen
        | _ -> recursive_gen
      in
      Astlib_first_draft.V4_07.Expression_desc.of_concrete concrete
    end

  and override_expression_generator =
    lazy begin
      let%map concrete =
        let override_expression_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy label_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Override_expression.Concrete.Override_expression { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., override_expression_gen
          ]
      in
      Astlib_first_draft.V4_07.Override_expression.of_concrete concrete
    end

  and record_field_expression_generator =
    lazy begin
      let%map concrete =
        let record_field_expression_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Record_field_expression.Concrete.Record_field_expression { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., record_field_expression_gen
          ]
      in
      Astlib_first_draft.V4_07.Record_field_expression.of_concrete concrete
    end

  and apply_arg_generator =
    lazy begin
      let%map concrete =
        let apply_arg_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy arg_label_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Apply_arg.Concrete.Apply_arg { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., apply_arg_gen
          ]
      in
      Astlib_first_draft.V4_07.Apply_arg.of_concrete concrete
    end

  and case_generator =
    lazy begin
      let%map concrete =
        let case_gen =
          let%bind pc_lhs =
            (Base_quickcheck.Generator.of_lazy pattern_generator)
          and pc_guard =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy expression_generator))
          and pc_rhs =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Case.Concrete.Case { pc_lhs; pc_guard; pc_rhs })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 4., case_gen
          ]
      in
      Astlib_first_draft.V4_07.Case.of_concrete concrete
    end

  and value_description_generator =
    lazy begin
      let%map concrete =
        let value_description_gen =
          let%bind pval_name =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and pval_type =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          and pval_prim =
            (Base_quickcheck.Generator.list Base_quickcheck.Generator.string_non_empty)
          and pval_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          and pval_loc =
            (Base_quickcheck.Generator.return Location.none)
          in
          return (Astlib_first_draft.V4_07.Value_description.Concrete.Value_description { pval_name; pval_type; pval_prim; pval_attributes; pval_loc })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 4., value_description_gen
          ]
      in
      Astlib_first_draft.V4_07.Value_description.of_concrete concrete
    end

  and type_declaration_generator =
    lazy begin
      let%map concrete =
        let type_declaration_gen =
          let%bind ptype_name =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and ptype_params =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy type_param_generator))
          and ptype_cstrs =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy type_constraint_generator))
          and ptype_kind =
            (Base_quickcheck.Generator.of_lazy type_kind_generator)
          and ptype_private =
            (Base_quickcheck.Generator.of_lazy private_flag_generator)
          and ptype_manifest =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy core_type_generator))
          and ptype_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          and ptype_loc =
            (Base_quickcheck.Generator.return Location.none)
          in
          return (Astlib_first_draft.V4_07.Type_declaration.Concrete.Type_declaration { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 8., type_declaration_gen
          ]
      in
      Astlib_first_draft.V4_07.Type_declaration.of_concrete concrete
    end

  and type_param_generator =
    lazy begin
      let%map concrete =
        let type_param_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy variance_generator)
          in
          return (Astlib_first_draft.V4_07.Type_param.Concrete.Type_param { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., type_param_gen
          ]
      in
      Astlib_first_draft.V4_07.Type_param.of_concrete concrete
    end

  and type_constraint_generator =
    lazy begin
      let%map concrete =
        let type_constraint_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          and c =
            (Base_quickcheck.Generator.return Location.none)
          in
          return (Astlib_first_draft.V4_07.Type_constraint.Concrete.Type_constraint { a; b; c })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., type_constraint_gen
          ]
      in
      Astlib_first_draft.V4_07.Type_constraint.of_concrete concrete
    end

  and type_kind_generator =
    lazy begin
      let%map concrete =
        let ptype_abstract_gen =
          return (Astlib_first_draft.V4_07.Type_kind.Concrete.Ptype_abstract)
        in
        let ptype_variant_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy constructor_declaration_generator))
          in
          return (Astlib_first_draft.V4_07.Type_kind.Concrete.Ptype_variant { a })
        in
        let ptype_record_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy label_declaration_generator))
          in
          return (Astlib_first_draft.V4_07.Type_kind.Concrete.Ptype_record { a })
        in
        let ptype_open_gen =
          return (Astlib_first_draft.V4_07.Type_kind.Concrete.Ptype_open)
        in
        let non_recursive_alist =
          [ 1. /. 1., ptype_abstract_gen
          ; 1. /. 1., ptype_open_gen
          ]
        and recursive_alist =
          [ 1. /. 2., ptype_variant_gen
          ; 1. /. 2., ptype_record_gen
          ]
          |> List.map ~f:(fun (weight, gen) -> (weight, with_decremented_size gen))
        in
        let non_recursive_gen =
          Base_quickcheck.Generator.weighted_union non_recursive_alist
        and recursive_gen =
          Base_quickcheck.Generator.weighted_union
            (non_recursive_alist @ recursive_alist)
        in
        match%bind Base_quickcheck.Generator.size with
        | 0 -> non_recursive_gen
        | _ -> recursive_gen
      in
      Astlib_first_draft.V4_07.Type_kind.of_concrete concrete
    end

  and label_declaration_generator =
    lazy begin
      let%map concrete =
        let label_declaration_gen =
          let%bind pld_name =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and pld_mutable =
            (Base_quickcheck.Generator.of_lazy mutable_flag_generator)
          and pld_type =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          and pld_loc =
            (Base_quickcheck.Generator.return Location.none)
          and pld_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Label_declaration.Concrete.Label_declaration { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 5., label_declaration_gen
          ]
      in
      Astlib_first_draft.V4_07.Label_declaration.of_concrete concrete
    end

  and constructor_declaration_generator =
    lazy begin
      let%map concrete =
        let constructor_declaration_gen =
          let%bind pcd_name =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and pcd_args =
            (Base_quickcheck.Generator.of_lazy constructor_arguments_generator)
          and pcd_res =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy core_type_generator))
          and pcd_loc =
            (Base_quickcheck.Generator.return Location.none)
          and pcd_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Constructor_declaration.Concrete.Constructor_declaration { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 5., constructor_declaration_gen
          ]
      in
      Astlib_first_draft.V4_07.Constructor_declaration.of_concrete concrete
    end

  and constructor_arguments_generator =
    lazy begin
      let%map concrete =
        let pcstr_tuple_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy core_type_generator))
          in
          return (Astlib_first_draft.V4_07.Constructor_arguments.Concrete.Pcstr_tuple { a })
        in
        let pcstr_record_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy label_declaration_generator))
          in
          return (Astlib_first_draft.V4_07.Constructor_arguments.Concrete.Pcstr_record { a })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 2., pcstr_tuple_gen
          ; 1. /. 2., pcstr_record_gen
          ]
      in
      Astlib_first_draft.V4_07.Constructor_arguments.of_concrete concrete
    end

  and type_extension_generator =
    lazy begin
      let%map concrete =
        let type_extension_gen =
          let%bind ptyext_path =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and ptyext_params =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy type_param_generator))
          and ptyext_constructors =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy extension_constructor_generator))
          and ptyext_private =
            (Base_quickcheck.Generator.of_lazy private_flag_generator)
          and ptyext_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Type_extension.Concrete.Type_extension { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 6., type_extension_gen
          ]
      in
      Astlib_first_draft.V4_07.Type_extension.of_concrete concrete
    end

  and extension_constructor_generator =
    lazy begin
      let%map concrete =
        let extension_constructor_gen =
          let%bind pext_name =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and pext_kind =
            (Base_quickcheck.Generator.of_lazy extension_constructor_kind_generator)
          and pext_loc =
            (Base_quickcheck.Generator.return Location.none)
          and pext_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Extension_constructor.Concrete.Extension_constructor { pext_name; pext_kind; pext_loc; pext_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 4., extension_constructor_gen
          ]
      in
      Astlib_first_draft.V4_07.Extension_constructor.of_concrete concrete
    end

  and extension_constructor_kind_generator =
    lazy begin
      let%map concrete =
        let pext_decl_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy constructor_arguments_generator)
          and b =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy core_type_generator))
          in
          return (Astlib_first_draft.V4_07.Extension_constructor_kind.Concrete.Pext_decl { a; b })
        in
        let pext_rebind_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          in
          return (Astlib_first_draft.V4_07.Extension_constructor_kind.Concrete.Pext_rebind { a })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., pext_decl_gen
          ; 1. /. 2., pext_rebind_gen
          ]
      in
      Astlib_first_draft.V4_07.Extension_constructor_kind.of_concrete concrete
    end

  and class_type_generator =
    lazy begin
      let%map concrete =
        let class_type_gen =
          let%bind pcty_desc =
            (Base_quickcheck.Generator.of_lazy class_type_desc_generator)
          and pcty_loc =
            (Base_quickcheck.Generator.return Location.none)
          and pcty_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type.Concrete.Class_type { pcty_desc; pcty_loc; pcty_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., class_type_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_type.of_concrete concrete
    end

  and class_type_desc_generator =
    lazy begin
      let%map concrete =
        let pcty_constr_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and b =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy core_type_generator))
          in
          return (Astlib_first_draft.V4_07.Class_type_desc.Concrete.Pcty_constr { a; b })
        in
        let pcty_signature_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy class_signature_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type_desc.Concrete.Pcty_signature { a })
        in
        let pcty_arrow_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy arg_label_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          and c =
            (Base_quickcheck.Generator.of_lazy class_type_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type_desc.Concrete.Pcty_arrow { a; b; c })
        in
        let pcty_extension_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy extension_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type_desc.Concrete.Pcty_extension { a })
        in
        let pcty_open_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy override_flag_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and c =
            (Base_quickcheck.Generator.of_lazy class_type_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type_desc.Concrete.Pcty_open { a; b; c })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., pcty_constr_gen
          ; 1. /. 2., pcty_signature_gen
          ; 1. /. 4., pcty_arrow_gen
          ; 1. /. 2., pcty_extension_gen
          ; 1. /. 4., pcty_open_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_type_desc.of_concrete concrete
    end

  and class_signature_generator =
    lazy begin
      let%map concrete =
        let class_signature_gen =
          let%bind pcsig_self =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          and pcsig_fields =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy class_type_field_generator))
          in
          return (Astlib_first_draft.V4_07.Class_signature.Concrete.Class_signature { pcsig_self; pcsig_fields })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., class_signature_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_signature.of_concrete concrete
    end

  and class_type_field_generator =
    lazy begin
      let%map concrete =
        let class_type_field_gen =
          let%bind pctf_desc =
            (Base_quickcheck.Generator.of_lazy class_type_field_desc_generator)
          and pctf_loc =
            (Base_quickcheck.Generator.return Location.none)
          and pctf_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type_field.Concrete.Class_type_field { pctf_desc; pctf_loc; pctf_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., class_type_field_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_type_field.of_concrete concrete
    end

  and class_type_field_desc_generator =
    lazy begin
      let%map concrete =
        let pctf_inherit_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy class_type_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type_field_desc.Concrete.Pctf_inherit { a })
        in
        let pctf_val_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy class_type_value_desc_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type_field_desc.Concrete.Pctf_val { a })
        in
        let pctf_method_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy class_type_method_desc_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type_field_desc.Concrete.Pctf_method { a })
        in
        let pctf_constraint_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy class_type_constraint_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type_field_desc.Concrete.Pctf_constraint { a })
        in
        let pctf_attribute_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy attribute_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type_field_desc.Concrete.Pctf_attribute { a })
        in
        let pctf_extension_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy extension_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type_field_desc.Concrete.Pctf_extension { a })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 2., pctf_inherit_gen
          ; 1. /. 2., pctf_val_gen
          ; 1. /. 2., pctf_method_gen
          ; 1. /. 2., pctf_constraint_gen
          ; 1. /. 2., pctf_attribute_gen
          ; 1. /. 2., pctf_extension_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_type_field_desc.of_concrete concrete
    end

  and class_type_value_desc_generator =
    lazy begin
      let%map concrete =
        let class_type_value_desc_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy label_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy mutable_flag_generator)
          and c =
            (Base_quickcheck.Generator.of_lazy virtual_flag_generator)
          and d =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type_value_desc.Concrete.Class_type_value_desc { a; b; c; d })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 5., class_type_value_desc_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_type_value_desc.of_concrete concrete
    end

  and class_type_method_desc_generator =
    lazy begin
      let%map concrete =
        let class_type_method_desc_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy label_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy private_flag_generator)
          and c =
            (Base_quickcheck.Generator.of_lazy virtual_flag_generator)
          and d =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type_method_desc.Concrete.Class_type_method_desc { a; b; c; d })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 5., class_type_method_desc_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_type_method_desc.of_concrete concrete
    end

  and class_type_constraint_generator =
    lazy begin
      let%map concrete =
        let class_type_constraint_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type_constraint.Concrete.Class_type_constraint { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., class_type_constraint_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_type_constraint.of_concrete concrete
    end

  and class_description_generator =
    lazy begin
      let%map concrete =
        let class_description_gen =
          let%bind pci_virt =
            (Base_quickcheck.Generator.of_lazy virtual_flag_generator)
          and pci_params =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy type_param_generator))
          and pci_name =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and pci_expr =
            (Base_quickcheck.Generator.of_lazy class_type_generator)
          and pci_loc =
            (Base_quickcheck.Generator.return Location.none)
          and pci_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Class_description.Concrete.Class_description { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 6., class_description_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_description.of_concrete concrete
    end

  and class_type_declaration_generator =
    lazy begin
      let%map concrete =
        let class_type_declaration_gen =
          let%bind pci_virt =
            (Base_quickcheck.Generator.of_lazy virtual_flag_generator)
          and pci_params =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy type_param_generator))
          and pci_name =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and pci_expr =
            (Base_quickcheck.Generator.of_lazy class_type_generator)
          and pci_loc =
            (Base_quickcheck.Generator.return Location.none)
          and pci_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Class_type_declaration.Concrete.Class_type_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 6., class_type_declaration_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_type_declaration.of_concrete concrete
    end

  and class_expr_generator =
    lazy begin
      let%map concrete =
        let class_expr_gen =
          let%bind pcl_desc =
            (Base_quickcheck.Generator.of_lazy class_expr_desc_generator)
          and pcl_loc =
            (Base_quickcheck.Generator.return Location.none)
          and pcl_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Class_expr.Concrete.Class_expr { pcl_desc; pcl_loc; pcl_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., class_expr_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_expr.of_concrete concrete
    end

  and class_expr_desc_generator =
    lazy begin
      let%map concrete =
        let pcl_constr_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and b =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy core_type_generator))
          in
          return (Astlib_first_draft.V4_07.Class_expr_desc.Concrete.Pcl_constr { a; b })
        in
        let pcl_structure_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy class_structure_generator)
          in
          return (Astlib_first_draft.V4_07.Class_expr_desc.Concrete.Pcl_structure { a })
        in
        let pcl_fun_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy arg_label_generator)
          and b =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy expression_generator))
          and c =
            (Base_quickcheck.Generator.of_lazy pattern_generator)
          and d =
            (Base_quickcheck.Generator.of_lazy class_expr_generator)
          in
          return (Astlib_first_draft.V4_07.Class_expr_desc.Concrete.Pcl_fun { a; b; c; d })
        in
        let pcl_apply_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy class_expr_generator)
          and b =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy apply_arg_generator))
          in
          return (Astlib_first_draft.V4_07.Class_expr_desc.Concrete.Pcl_apply { a; b })
        in
        let pcl_let_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy rec_flag_generator)
          and b =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy value_binding_generator))
          and c =
            (Base_quickcheck.Generator.of_lazy class_expr_generator)
          in
          return (Astlib_first_draft.V4_07.Class_expr_desc.Concrete.Pcl_let { a; b; c })
        in
        let pcl_constraint_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy class_expr_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy class_type_generator)
          in
          return (Astlib_first_draft.V4_07.Class_expr_desc.Concrete.Pcl_constraint { a; b })
        in
        let pcl_extension_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy extension_generator)
          in
          return (Astlib_first_draft.V4_07.Class_expr_desc.Concrete.Pcl_extension { a })
        in
        let pcl_open_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy override_flag_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and c =
            (Base_quickcheck.Generator.of_lazy class_expr_generator)
          in
          return (Astlib_first_draft.V4_07.Class_expr_desc.Concrete.Pcl_open { a; b; c })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., pcl_constr_gen
          ; 1. /. 2., pcl_structure_gen
          ; 1. /. 5., pcl_fun_gen
          ; 1. /. 3., pcl_apply_gen
          ; 1. /. 4., pcl_let_gen
          ; 1. /. 3., pcl_constraint_gen
          ; 1. /. 2., pcl_extension_gen
          ; 1. /. 4., pcl_open_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_expr_desc.of_concrete concrete
    end

  and class_structure_generator =
    lazy begin
      let%map concrete =
        let class_structure_gen =
          let%bind pcstr_self =
            (Base_quickcheck.Generator.of_lazy pattern_generator)
          and pcstr_fields =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy class_field_generator))
          in
          return (Astlib_first_draft.V4_07.Class_structure.Concrete.Class_structure { pcstr_self; pcstr_fields })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., class_structure_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_structure.of_concrete concrete
    end

  and class_field_generator =
    lazy begin
      let%map concrete =
        let class_field_gen =
          let%bind pcf_desc =
            (Base_quickcheck.Generator.of_lazy class_field_desc_generator)
          and pcf_loc =
            (Base_quickcheck.Generator.return Location.none)
          and pcf_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Class_field.Concrete.Class_field { pcf_desc; pcf_loc; pcf_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., class_field_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_field.of_concrete concrete
    end

  and class_field_desc_generator =
    lazy begin
      let%map concrete =
        let pcf_inherit_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy override_flag_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy class_expr_generator)
          and c =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy string_loc_generator))
          in
          return (Astlib_first_draft.V4_07.Class_field_desc.Concrete.Pcf_inherit { a; b; c })
        in
        let pcf_val_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy class_value_desc_generator)
          in
          return (Astlib_first_draft.V4_07.Class_field_desc.Concrete.Pcf_val { a })
        in
        let pcf_method_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy class_method_desc_generator)
          in
          return (Astlib_first_draft.V4_07.Class_field_desc.Concrete.Pcf_method { a })
        in
        let pcf_constraint_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy class_type_constraint_generator)
          in
          return (Astlib_first_draft.V4_07.Class_field_desc.Concrete.Pcf_constraint { a })
        in
        let pcf_initializer_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Class_field_desc.Concrete.Pcf_initializer { a })
        in
        let pcf_attribute_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy attribute_generator)
          in
          return (Astlib_first_draft.V4_07.Class_field_desc.Concrete.Pcf_attribute { a })
        in
        let pcf_extension_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy extension_generator)
          in
          return (Astlib_first_draft.V4_07.Class_field_desc.Concrete.Pcf_extension { a })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 4., pcf_inherit_gen
          ; 1. /. 2., pcf_val_gen
          ; 1. /. 2., pcf_method_gen
          ; 1. /. 2., pcf_constraint_gen
          ; 1. /. 2., pcf_initializer_gen
          ; 1. /. 2., pcf_attribute_gen
          ; 1. /. 2., pcf_extension_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_field_desc.of_concrete concrete
    end

  and class_value_desc_generator =
    lazy begin
      let%map concrete =
        let class_value_desc_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy label_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy mutable_flag_generator)
          and c =
            (Base_quickcheck.Generator.of_lazy class_field_kind_generator)
          in
          return (Astlib_first_draft.V4_07.Class_value_desc.Concrete.Class_value_desc { a; b; c })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 4., class_value_desc_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_value_desc.of_concrete concrete
    end

  and class_method_desc_generator =
    lazy begin
      let%map concrete =
        let class_method_desc_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy label_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy private_flag_generator)
          and c =
            (Base_quickcheck.Generator.of_lazy class_field_kind_generator)
          in
          return (Astlib_first_draft.V4_07.Class_method_desc.Concrete.Class_method_desc { a; b; c })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 4., class_method_desc_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_method_desc.of_concrete concrete
    end

  and class_field_kind_generator =
    lazy begin
      let%map concrete =
        let cfk_virtual_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy core_type_generator)
          in
          return (Astlib_first_draft.V4_07.Class_field_kind.Concrete.Cfk_virtual { a })
        in
        let cfk_concrete_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy override_flag_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Class_field_kind.Concrete.Cfk_concrete { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 2., cfk_virtual_gen
          ; 1. /. 3., cfk_concrete_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_field_kind.of_concrete concrete
    end

  and class_declaration_generator =
    lazy begin
      let%map concrete =
        let class_declaration_gen =
          let%bind pci_virt =
            (Base_quickcheck.Generator.of_lazy virtual_flag_generator)
          and pci_params =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy type_param_generator))
          and pci_name =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and pci_expr =
            (Base_quickcheck.Generator.of_lazy class_expr_generator)
          and pci_loc =
            (Base_quickcheck.Generator.return Location.none)
          and pci_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Class_declaration.Concrete.Class_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 6., class_declaration_gen
          ]
      in
      Astlib_first_draft.V4_07.Class_declaration.of_concrete concrete
    end

  and module_type_generator =
    lazy begin
      let%map concrete =
        let module_type_gen =
          let%bind pmty_desc =
            (Base_quickcheck.Generator.of_lazy module_type_desc_generator)
          and pmty_loc =
            (Base_quickcheck.Generator.return Location.none)
          and pmty_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Module_type.Concrete.Module_type { pmty_desc; pmty_loc; pmty_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., module_type_gen
          ]
      in
      Astlib_first_draft.V4_07.Module_type.of_concrete concrete
    end

  and module_type_desc_generator =
    lazy begin
      let%map concrete =
        let pmty_ident_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          in
          return (Astlib_first_draft.V4_07.Module_type_desc.Concrete.Pmty_ident { a })
        in
        let pmty_signature_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy signature_generator)
          in
          return (Astlib_first_draft.V4_07.Module_type_desc.Concrete.Pmty_signature { a })
        in
        let pmty_functor_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and b =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy module_type_generator))
          and c =
            (Base_quickcheck.Generator.of_lazy module_type_generator)
          in
          return (Astlib_first_draft.V4_07.Module_type_desc.Concrete.Pmty_functor { a; b; c })
        in
        let pmty_with_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy module_type_generator)
          and b =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy with_constraint_generator))
          in
          return (Astlib_first_draft.V4_07.Module_type_desc.Concrete.Pmty_with { a; b })
        in
        let pmty_typeof_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy module_expr_generator)
          in
          return (Astlib_first_draft.V4_07.Module_type_desc.Concrete.Pmty_typeof { a })
        in
        let pmty_extension_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy extension_generator)
          in
          return (Astlib_first_draft.V4_07.Module_type_desc.Concrete.Pmty_extension { a })
        in
        let pmty_alias_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          in
          return (Astlib_first_draft.V4_07.Module_type_desc.Concrete.Pmty_alias { a })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 2., pmty_ident_gen
          ; 1. /. 2., pmty_signature_gen
          ; 1. /. 4., pmty_functor_gen
          ; 1. /. 3., pmty_with_gen
          ; 1. /. 2., pmty_typeof_gen
          ; 1. /. 2., pmty_extension_gen
          ; 1. /. 2., pmty_alias_gen
          ]
      in
      Astlib_first_draft.V4_07.Module_type_desc.of_concrete concrete
    end

  and signature_generator =
    lazy begin
      let%map concrete =
        let signature_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy signature_item_generator))
          in
          return (Astlib_first_draft.V4_07.Signature.Concrete.Signature { a })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 2., signature_gen
          ]
      in
      Astlib_first_draft.V4_07.Signature.of_concrete concrete
    end

  and signature_item_generator =
    lazy begin
      let%map concrete =
        let signature_item_gen =
          let%bind psig_desc =
            (Base_quickcheck.Generator.of_lazy signature_item_desc_generator)
          and psig_loc =
            (Base_quickcheck.Generator.return Location.none)
          in
          return (Astlib_first_draft.V4_07.Signature_item.Concrete.Signature_item { psig_desc; psig_loc })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 2., signature_item_gen
          ]
      in
      Astlib_first_draft.V4_07.Signature_item.of_concrete concrete
    end

  and signature_item_desc_generator =
    lazy begin
      let%map concrete =
        let psig_value_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy value_description_generator)
          in
          return (Astlib_first_draft.V4_07.Signature_item_desc.Concrete.Psig_value { a })
        in
        let psig_type_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy rec_flag_generator)
          and b =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy type_declaration_generator))
          in
          return (Astlib_first_draft.V4_07.Signature_item_desc.Concrete.Psig_type { a; b })
        in
        let psig_typext_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy type_extension_generator)
          in
          return (Astlib_first_draft.V4_07.Signature_item_desc.Concrete.Psig_typext { a })
        in
        let psig_exception_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy extension_constructor_generator)
          in
          return (Astlib_first_draft.V4_07.Signature_item_desc.Concrete.Psig_exception { a })
        in
        let psig_module_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy module_declaration_generator)
          in
          return (Astlib_first_draft.V4_07.Signature_item_desc.Concrete.Psig_module { a })
        in
        let psig_recmodule_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy module_declaration_generator))
          in
          return (Astlib_first_draft.V4_07.Signature_item_desc.Concrete.Psig_recmodule { a })
        in
        let psig_modtype_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy module_type_declaration_generator)
          in
          return (Astlib_first_draft.V4_07.Signature_item_desc.Concrete.Psig_modtype { a })
        in
        let psig_open_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy open_description_generator)
          in
          return (Astlib_first_draft.V4_07.Signature_item_desc.Concrete.Psig_open { a })
        in
        let psig_include_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy include_description_generator)
          in
          return (Astlib_first_draft.V4_07.Signature_item_desc.Concrete.Psig_include { a })
        in
        let psig_class_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy class_description_generator))
          in
          return (Astlib_first_draft.V4_07.Signature_item_desc.Concrete.Psig_class { a })
        in
        let psig_class_type_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy class_type_declaration_generator))
          in
          return (Astlib_first_draft.V4_07.Signature_item_desc.Concrete.Psig_class_type { a })
        in
        let psig_attribute_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy attribute_generator)
          in
          return (Astlib_first_draft.V4_07.Signature_item_desc.Concrete.Psig_attribute { a })
        in
        let psig_extension_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy extension_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Signature_item_desc.Concrete.Psig_extension { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 2., psig_value_gen
          ; 1. /. 3., psig_type_gen
          ; 1. /. 2., psig_typext_gen
          ; 1. /. 2., psig_exception_gen
          ; 1. /. 2., psig_module_gen
          ; 1. /. 2., psig_recmodule_gen
          ; 1. /. 2., psig_modtype_gen
          ; 1. /. 2., psig_open_gen
          ; 1. /. 2., psig_include_gen
          ; 1. /. 2., psig_class_gen
          ; 1. /. 2., psig_class_type_gen
          ; 1. /. 2., psig_attribute_gen
          ; 1. /. 3., psig_extension_gen
          ]
      in
      Astlib_first_draft.V4_07.Signature_item_desc.of_concrete concrete
    end

  and module_declaration_generator =
    lazy begin
      let%map concrete =
        let module_declaration_gen =
          let%bind pmd_name =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and pmd_type =
            (Base_quickcheck.Generator.of_lazy module_type_generator)
          and pmd_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          and pmd_loc =
            (Base_quickcheck.Generator.return Location.none)
          in
          return (Astlib_first_draft.V4_07.Module_declaration.Concrete.Module_declaration { pmd_name; pmd_type; pmd_attributes; pmd_loc })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 4., module_declaration_gen
          ]
      in
      Astlib_first_draft.V4_07.Module_declaration.of_concrete concrete
    end

  and module_type_declaration_generator =
    lazy begin
      let%map concrete =
        let module_type_declaration_gen =
          let%bind pmtd_name =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and pmtd_type =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy module_type_generator))
          and pmtd_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          and pmtd_loc =
            (Base_quickcheck.Generator.return Location.none)
          in
          return (Astlib_first_draft.V4_07.Module_type_declaration.Concrete.Module_type_declaration { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 4., module_type_declaration_gen
          ]
      in
      Astlib_first_draft.V4_07.Module_type_declaration.of_concrete concrete
    end

  and open_description_generator =
    lazy begin
      let%map concrete =
        let open_description_gen =
          let%bind popen_lid =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and popen_override =
            (Base_quickcheck.Generator.of_lazy override_flag_generator)
          and popen_loc =
            (Base_quickcheck.Generator.return Location.none)
          and popen_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Open_description.Concrete.Open_description { popen_lid; popen_override; popen_loc; popen_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 4., open_description_gen
          ]
      in
      Astlib_first_draft.V4_07.Open_description.of_concrete concrete
    end

  and include_description_generator =
    lazy begin
      let%map concrete =
        let include_description_gen =
          let%bind pincl_mod =
            (Base_quickcheck.Generator.of_lazy module_type_generator)
          and pincl_loc =
            (Base_quickcheck.Generator.return Location.none)
          and pincl_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Include_description.Concrete.Include_description { pincl_mod; pincl_loc; pincl_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., include_description_gen
          ]
      in
      Astlib_first_draft.V4_07.Include_description.of_concrete concrete
    end

  and include_declaration_generator =
    lazy begin
      let%map concrete =
        let include_declaration_gen =
          let%bind pincl_mod =
            (Base_quickcheck.Generator.of_lazy module_expr_generator)
          and pincl_loc =
            (Base_quickcheck.Generator.return Location.none)
          and pincl_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Include_declaration.Concrete.Include_declaration { pincl_mod; pincl_loc; pincl_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., include_declaration_gen
          ]
      in
      Astlib_first_draft.V4_07.Include_declaration.of_concrete concrete
    end

  and with_constraint_generator =
    lazy begin
      let%map concrete =
        let pwith_type_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy type_declaration_generator)
          in
          return (Astlib_first_draft.V4_07.With_constraint.Concrete.Pwith_type { a; b })
        in
        let pwith_module_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          in
          return (Astlib_first_draft.V4_07.With_constraint.Concrete.Pwith_module { a; b })
        in
        let pwith_typesubst_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy type_declaration_generator)
          in
          return (Astlib_first_draft.V4_07.With_constraint.Concrete.Pwith_typesubst { a; b })
        in
        let pwith_modsubst_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          in
          return (Astlib_first_draft.V4_07.With_constraint.Concrete.Pwith_modsubst { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., pwith_type_gen
          ; 1. /. 3., pwith_module_gen
          ; 1. /. 3., pwith_typesubst_gen
          ; 1. /. 3., pwith_modsubst_gen
          ]
      in
      Astlib_first_draft.V4_07.With_constraint.of_concrete concrete
    end

  and module_expr_generator =
    lazy begin
      let%map concrete =
        let module_expr_gen =
          let%bind pmod_desc =
            (Base_quickcheck.Generator.of_lazy module_expr_desc_generator)
          and pmod_loc =
            (Base_quickcheck.Generator.return Location.none)
          and pmod_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Module_expr.Concrete.Module_expr { pmod_desc; pmod_loc; pmod_attributes })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., module_expr_gen
          ]
      in
      Astlib_first_draft.V4_07.Module_expr.of_concrete concrete
    end

  and module_expr_desc_generator =
    lazy begin
      let%map concrete =
        let pmod_ident_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_loc_generator)
          in
          return (Astlib_first_draft.V4_07.Module_expr_desc.Concrete.Pmod_ident { a })
        in
        let pmod_structure_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy structure_generator)
          in
          return (Astlib_first_draft.V4_07.Module_expr_desc.Concrete.Pmod_structure { a })
        in
        let pmod_functor_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and b =
            (Base_quickcheck.Generator.option (Base_quickcheck.Generator.of_lazy module_type_generator))
          and c =
            (Base_quickcheck.Generator.of_lazy module_expr_generator)
          in
          return (Astlib_first_draft.V4_07.Module_expr_desc.Concrete.Pmod_functor { a; b; c })
        in
        let pmod_apply_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy module_expr_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy module_expr_generator)
          in
          return (Astlib_first_draft.V4_07.Module_expr_desc.Concrete.Pmod_apply { a; b })
        in
        let pmod_constraint_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy module_expr_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy module_type_generator)
          in
          return (Astlib_first_draft.V4_07.Module_expr_desc.Concrete.Pmod_constraint { a; b })
        in
        let pmod_unpack_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          in
          return (Astlib_first_draft.V4_07.Module_expr_desc.Concrete.Pmod_unpack { a })
        in
        let pmod_extension_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy extension_generator)
          in
          return (Astlib_first_draft.V4_07.Module_expr_desc.Concrete.Pmod_extension { a })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 2., pmod_ident_gen
          ; 1. /. 2., pmod_structure_gen
          ; 1. /. 4., pmod_functor_gen
          ; 1. /. 3., pmod_apply_gen
          ; 1. /. 3., pmod_constraint_gen
          ; 1. /. 2., pmod_unpack_gen
          ; 1. /. 2., pmod_extension_gen
          ]
      in
      Astlib_first_draft.V4_07.Module_expr_desc.of_concrete concrete
    end

  and structure_generator =
    lazy begin
      let%map concrete =
        let structure_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy structure_item_generator))
          in
          return (Astlib_first_draft.V4_07.Structure.Concrete.Structure { a })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 2., structure_gen
          ]
      in
      Astlib_first_draft.V4_07.Structure.of_concrete concrete
    end

  and structure_item_generator =
    lazy begin
      let%map concrete =
        let structure_item_gen =
          let%bind pstr_desc =
            (Base_quickcheck.Generator.of_lazy structure_item_desc_generator)
          and pstr_loc =
            (Base_quickcheck.Generator.return Location.none)
          in
          return (Astlib_first_draft.V4_07.Structure_item.Concrete.Structure_item { pstr_desc; pstr_loc })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 2., structure_item_gen
          ]
      in
      Astlib_first_draft.V4_07.Structure_item.of_concrete concrete
    end

  and structure_item_desc_generator =
    lazy begin
      let%map concrete =
        let pstr_eval_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Structure_item_desc.Concrete.Pstr_eval { a; b })
        in
        let pstr_value_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy rec_flag_generator)
          and b =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy value_binding_generator))
          in
          return (Astlib_first_draft.V4_07.Structure_item_desc.Concrete.Pstr_value { a; b })
        in
        let pstr_primitive_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy value_description_generator)
          in
          return (Astlib_first_draft.V4_07.Structure_item_desc.Concrete.Pstr_primitive { a })
        in
        let pstr_type_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy rec_flag_generator)
          and b =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy type_declaration_generator))
          in
          return (Astlib_first_draft.V4_07.Structure_item_desc.Concrete.Pstr_type { a; b })
        in
        let pstr_typext_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy type_extension_generator)
          in
          return (Astlib_first_draft.V4_07.Structure_item_desc.Concrete.Pstr_typext { a })
        in
        let pstr_exception_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy extension_constructor_generator)
          in
          return (Astlib_first_draft.V4_07.Structure_item_desc.Concrete.Pstr_exception { a })
        in
        let pstr_module_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy module_binding_generator)
          in
          return (Astlib_first_draft.V4_07.Structure_item_desc.Concrete.Pstr_module { a })
        in
        let pstr_recmodule_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy module_binding_generator))
          in
          return (Astlib_first_draft.V4_07.Structure_item_desc.Concrete.Pstr_recmodule { a })
        in
        let pstr_modtype_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy module_type_declaration_generator)
          in
          return (Astlib_first_draft.V4_07.Structure_item_desc.Concrete.Pstr_modtype { a })
        in
        let pstr_open_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy open_description_generator)
          in
          return (Astlib_first_draft.V4_07.Structure_item_desc.Concrete.Pstr_open { a })
        in
        let pstr_class_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy class_declaration_generator))
          in
          return (Astlib_first_draft.V4_07.Structure_item_desc.Concrete.Pstr_class { a })
        in
        let pstr_class_type_gen =
          let%bind a =
            (Base_quickcheck.Generator.list (Base_quickcheck.Generator.of_lazy class_type_declaration_generator))
          in
          return (Astlib_first_draft.V4_07.Structure_item_desc.Concrete.Pstr_class_type { a })
        in
        let pstr_include_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy include_declaration_generator)
          in
          return (Astlib_first_draft.V4_07.Structure_item_desc.Concrete.Pstr_include { a })
        in
        let pstr_attribute_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy attribute_generator)
          in
          return (Astlib_first_draft.V4_07.Structure_item_desc.Concrete.Pstr_attribute { a })
        in
        let pstr_extension_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy extension_generator)
          and b =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          in
          return (Astlib_first_draft.V4_07.Structure_item_desc.Concrete.Pstr_extension { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 3., pstr_eval_gen
          ; 1. /. 3., pstr_value_gen
          ; 1. /. 2., pstr_primitive_gen
          ; 1. /. 3., pstr_type_gen
          ; 1. /. 2., pstr_typext_gen
          ; 1. /. 2., pstr_exception_gen
          ; 1. /. 2., pstr_module_gen
          ; 1. /. 2., pstr_recmodule_gen
          ; 1. /. 2., pstr_modtype_gen
          ; 1. /. 2., pstr_open_gen
          ; 1. /. 2., pstr_class_gen
          ; 1. /. 2., pstr_class_type_gen
          ; 1. /. 2., pstr_include_gen
          ; 1. /. 2., pstr_attribute_gen
          ; 1. /. 3., pstr_extension_gen
          ]
      in
      Astlib_first_draft.V4_07.Structure_item_desc.of_concrete concrete
    end

  and value_binding_generator =
    lazy begin
      let%map concrete =
        let value_binding_gen =
          let%bind pvb_pat =
            (Base_quickcheck.Generator.of_lazy pattern_generator)
          and pvb_expr =
            (Base_quickcheck.Generator.of_lazy expression_generator)
          and pvb_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          and pvb_loc =
            (Base_quickcheck.Generator.return Location.none)
          in
          return (Astlib_first_draft.V4_07.Value_binding.Concrete.Value_binding { pvb_pat; pvb_expr; pvb_attributes; pvb_loc })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 4., value_binding_gen
          ]
      in
      Astlib_first_draft.V4_07.Value_binding.of_concrete concrete
    end

  and module_binding_generator =
    lazy begin
      let%map concrete =
        let module_binding_gen =
          let%bind pmb_name =
            (Base_quickcheck.Generator.of_lazy string_loc_generator)
          and pmb_expr =
            (Base_quickcheck.Generator.of_lazy module_expr_generator)
          and pmb_attributes =
            (Base_quickcheck.Generator.of_lazy attributes_generator)
          and pmb_loc =
            (Base_quickcheck.Generator.return Location.none)
          in
          return (Astlib_first_draft.V4_07.Module_binding.Concrete.Module_binding { pmb_name; pmb_expr; pmb_attributes; pmb_loc })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 4., module_binding_gen
          ]
      in
      Astlib_first_draft.V4_07.Module_binding.of_concrete concrete
    end

  and toplevel_phrase_generator =
    lazy begin
      let%map concrete =
        let ptop_def_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy structure_generator)
          in
          return (Astlib_first_draft.V4_07.Toplevel_phrase.Concrete.Ptop_def { a })
        in
        let ptop_dir_gen =
          let%bind a =
            Base_quickcheck.Generator.string_non_empty
          and b =
            (Base_quickcheck.Generator.of_lazy directive_argument_generator)
          in
          return (Astlib_first_draft.V4_07.Toplevel_phrase.Concrete.Ptop_dir { a; b })
        in
        Base_quickcheck.Generator.weighted_union
          [ 1. /. 2., ptop_def_gen
          ; 1. /. 2., ptop_dir_gen
          ]
      in
      Astlib_first_draft.V4_07.Toplevel_phrase.of_concrete concrete
    end

  and directive_argument_generator =
    lazy begin
      let%map concrete =
        let pdir_none_gen =
          return (Astlib_first_draft.V4_07.Directive_argument.Concrete.Pdir_none)
        in
        let pdir_string_gen =
          let%bind a =
            Base_quickcheck.Generator.string_non_empty
          in
          return (Astlib_first_draft.V4_07.Directive_argument.Concrete.Pdir_string { a })
        in
        let pdir_int_gen =
          let%bind a =
            Base_quickcheck.Generator.string_non_empty
          and b =
            (Base_quickcheck.Generator.option Base_quickcheck.Generator.char)
          in
          return (Astlib_first_draft.V4_07.Directive_argument.Concrete.Pdir_int { a; b })
        in
        let pdir_ident_gen =
          let%bind a =
            (Base_quickcheck.Generator.of_lazy longident_generator)
          in
          return (Astlib_first_draft.V4_07.Directive_argument.Concrete.Pdir_ident { a })
        in
        let pdir_bool_gen =
          let%bind a =
            Base_quickcheck.Generator.bool
          in
          return (Astlib_first_draft.V4_07.Directive_argument.Concrete.Pdir_bool { a })
        in
        let non_recursive_alist =
          [ 1. /. 1., pdir_none_gen
          ; 1. /. 1., pdir_string_gen
          ; 1. /. 1., pdir_int_gen
          ; 1. /. 1., pdir_bool_gen
          ]
        and recursive_alist =
          [ 1. /. 2., pdir_ident_gen
          ]
          |> List.map ~f:(fun (weight, gen) -> (weight, with_decremented_size gen))
        in
        let non_recursive_gen =
          Base_quickcheck.Generator.weighted_union non_recursive_alist
        and recursive_gen =
          Base_quickcheck.Generator.weighted_union
            (non_recursive_alist @ recursive_alist)
        in
        match%bind Base_quickcheck.Generator.size with
        | 0 -> non_recursive_gen
        | _ -> recursive_gen
      in
      Astlib_first_draft.V4_07.Directive_argument.of_concrete concrete
    end

  module Longident = struct
    type t = Astlib_first_draft.V4_07.Longident.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Longident.to_ast t)
    let quickcheck_generator = Lazy.force longident_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Longident_loc = struct
    type t = Astlib_first_draft.V4_07.Longident_loc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Longident_loc.to_ast t)
    let quickcheck_generator = Lazy.force longident_loc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Rec_flag = struct
    type t = Astlib_first_draft.V4_07.Rec_flag.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Rec_flag.to_ast t)
    let quickcheck_generator = Lazy.force rec_flag_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Direction_flag = struct
    type t = Astlib_first_draft.V4_07.Direction_flag.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Direction_flag.to_ast t)
    let quickcheck_generator = Lazy.force direction_flag_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Private_flag = struct
    type t = Astlib_first_draft.V4_07.Private_flag.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Private_flag.to_ast t)
    let quickcheck_generator = Lazy.force private_flag_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Mutable_flag = struct
    type t = Astlib_first_draft.V4_07.Mutable_flag.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Mutable_flag.to_ast t)
    let quickcheck_generator = Lazy.force mutable_flag_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Virtual_flag = struct
    type t = Astlib_first_draft.V4_07.Virtual_flag.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Virtual_flag.to_ast t)
    let quickcheck_generator = Lazy.force virtual_flag_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Override_flag = struct
    type t = Astlib_first_draft.V4_07.Override_flag.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Override_flag.to_ast t)
    let quickcheck_generator = Lazy.force override_flag_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Closed_flag = struct
    type t = Astlib_first_draft.V4_07.Closed_flag.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Closed_flag.to_ast t)
    let quickcheck_generator = Lazy.force closed_flag_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Label = struct
    type t = Astlib_first_draft.V4_07.Label.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Label.to_ast t)
    let quickcheck_generator = Lazy.force label_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Label_loc = struct
    type t = Astlib_first_draft.V4_07.Label_loc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Label_loc.to_ast t)
    let quickcheck_generator = Lazy.force label_loc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module String_loc = struct
    type t = Astlib_first_draft.V4_07.String_loc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.String_loc.to_ast t)
    let quickcheck_generator = Lazy.force string_loc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Arg_label = struct
    type t = Astlib_first_draft.V4_07.Arg_label.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Arg_label.to_ast t)
    let quickcheck_generator = Lazy.force arg_label_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Variance = struct
    type t = Astlib_first_draft.V4_07.Variance.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Variance.to_ast t)
    let quickcheck_generator = Lazy.force variance_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Constant = struct
    type t = Astlib_first_draft.V4_07.Constant.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Constant.to_ast t)
    let quickcheck_generator = Lazy.force constant_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Attribute = struct
    type t = Astlib_first_draft.V4_07.Attribute.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Attribute.to_ast t)
    let quickcheck_generator = Lazy.force attribute_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Extension = struct
    type t = Astlib_first_draft.V4_07.Extension.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Extension.to_ast t)
    let quickcheck_generator = Lazy.force extension_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Attributes = struct
    type t = Astlib_first_draft.V4_07.Attributes.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Attributes.to_ast t)
    let quickcheck_generator = Lazy.force attributes_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Payload = struct
    type t = Astlib_first_draft.V4_07.Payload.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Payload.to_ast t)
    let quickcheck_generator = Lazy.force payload_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Core_type = struct
    type t = Astlib_first_draft.V4_07.Core_type.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Core_type.to_ast t)
    let quickcheck_generator = Lazy.force core_type_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Core_type_desc = struct
    type t = Astlib_first_draft.V4_07.Core_type_desc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Core_type_desc.to_ast t)
    let quickcheck_generator = Lazy.force core_type_desc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Package_type = struct
    type t = Astlib_first_draft.V4_07.Package_type.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Package_type.to_ast t)
    let quickcheck_generator = Lazy.force package_type_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Package_type_constraint = struct
    type t = Astlib_first_draft.V4_07.Package_type_constraint.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Package_type_constraint.to_ast t)
    let quickcheck_generator = Lazy.force package_type_constraint_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Row_field = struct
    type t = Astlib_first_draft.V4_07.Row_field.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Row_field.to_ast t)
    let quickcheck_generator = Lazy.force row_field_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Object_field = struct
    type t = Astlib_first_draft.V4_07.Object_field.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Object_field.to_ast t)
    let quickcheck_generator = Lazy.force object_field_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Pattern = struct
    type t = Astlib_first_draft.V4_07.Pattern.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Pattern.to_ast t)
    let quickcheck_generator = Lazy.force pattern_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Pattern_desc = struct
    type t = Astlib_first_draft.V4_07.Pattern_desc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Pattern_desc.to_ast t)
    let quickcheck_generator = Lazy.force pattern_desc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Record_field_pattern = struct
    type t = Astlib_first_draft.V4_07.Record_field_pattern.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Record_field_pattern.to_ast t)
    let quickcheck_generator = Lazy.force record_field_pattern_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Expression = struct
    type t = Astlib_first_draft.V4_07.Expression.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Expression.to_ast t)
    let quickcheck_generator = Lazy.force expression_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Expression_desc = struct
    type t = Astlib_first_draft.V4_07.Expression_desc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Expression_desc.to_ast t)
    let quickcheck_generator = Lazy.force expression_desc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Override_expression = struct
    type t = Astlib_first_draft.V4_07.Override_expression.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Override_expression.to_ast t)
    let quickcheck_generator = Lazy.force override_expression_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Record_field_expression = struct
    type t = Astlib_first_draft.V4_07.Record_field_expression.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Record_field_expression.to_ast t)
    let quickcheck_generator = Lazy.force record_field_expression_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Apply_arg = struct
    type t = Astlib_first_draft.V4_07.Apply_arg.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Apply_arg.to_ast t)
    let quickcheck_generator = Lazy.force apply_arg_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Case = struct
    type t = Astlib_first_draft.V4_07.Case.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Case.to_ast t)
    let quickcheck_generator = Lazy.force case_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Value_description = struct
    type t = Astlib_first_draft.V4_07.Value_description.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Value_description.to_ast t)
    let quickcheck_generator = Lazy.force value_description_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Type_declaration = struct
    type t = Astlib_first_draft.V4_07.Type_declaration.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Type_declaration.to_ast t)
    let quickcheck_generator = Lazy.force type_declaration_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Type_param = struct
    type t = Astlib_first_draft.V4_07.Type_param.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Type_param.to_ast t)
    let quickcheck_generator = Lazy.force type_param_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Type_constraint = struct
    type t = Astlib_first_draft.V4_07.Type_constraint.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Type_constraint.to_ast t)
    let quickcheck_generator = Lazy.force type_constraint_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Type_kind = struct
    type t = Astlib_first_draft.V4_07.Type_kind.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Type_kind.to_ast t)
    let quickcheck_generator = Lazy.force type_kind_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Label_declaration = struct
    type t = Astlib_first_draft.V4_07.Label_declaration.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Label_declaration.to_ast t)
    let quickcheck_generator = Lazy.force label_declaration_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Constructor_declaration = struct
    type t = Astlib_first_draft.V4_07.Constructor_declaration.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Constructor_declaration.to_ast t)
    let quickcheck_generator = Lazy.force constructor_declaration_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Constructor_arguments = struct
    type t = Astlib_first_draft.V4_07.Constructor_arguments.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Constructor_arguments.to_ast t)
    let quickcheck_generator = Lazy.force constructor_arguments_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Type_extension = struct
    type t = Astlib_first_draft.V4_07.Type_extension.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Type_extension.to_ast t)
    let quickcheck_generator = Lazy.force type_extension_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Extension_constructor = struct
    type t = Astlib_first_draft.V4_07.Extension_constructor.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Extension_constructor.to_ast t)
    let quickcheck_generator = Lazy.force extension_constructor_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Extension_constructor_kind = struct
    type t = Astlib_first_draft.V4_07.Extension_constructor_kind.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Extension_constructor_kind.to_ast t)
    let quickcheck_generator = Lazy.force extension_constructor_kind_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_type = struct
    type t = Astlib_first_draft.V4_07.Class_type.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type.to_ast t)
    let quickcheck_generator = Lazy.force class_type_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_type_desc = struct
    type t = Astlib_first_draft.V4_07.Class_type_desc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type_desc.to_ast t)
    let quickcheck_generator = Lazy.force class_type_desc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_signature = struct
    type t = Astlib_first_draft.V4_07.Class_signature.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_signature.to_ast t)
    let quickcheck_generator = Lazy.force class_signature_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_type_field = struct
    type t = Astlib_first_draft.V4_07.Class_type_field.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type_field.to_ast t)
    let quickcheck_generator = Lazy.force class_type_field_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_type_field_desc = struct
    type t = Astlib_first_draft.V4_07.Class_type_field_desc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type_field_desc.to_ast t)
    let quickcheck_generator = Lazy.force class_type_field_desc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_type_value_desc = struct
    type t = Astlib_first_draft.V4_07.Class_type_value_desc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type_value_desc.to_ast t)
    let quickcheck_generator = Lazy.force class_type_value_desc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_type_method_desc = struct
    type t = Astlib_first_draft.V4_07.Class_type_method_desc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type_method_desc.to_ast t)
    let quickcheck_generator = Lazy.force class_type_method_desc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_type_constraint = struct
    type t = Astlib_first_draft.V4_07.Class_type_constraint.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type_constraint.to_ast t)
    let quickcheck_generator = Lazy.force class_type_constraint_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_description = struct
    type t = Astlib_first_draft.V4_07.Class_description.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_description.to_ast t)
    let quickcheck_generator = Lazy.force class_description_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_type_declaration = struct
    type t = Astlib_first_draft.V4_07.Class_type_declaration.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_type_declaration.to_ast t)
    let quickcheck_generator = Lazy.force class_type_declaration_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_expr = struct
    type t = Astlib_first_draft.V4_07.Class_expr.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_expr.to_ast t)
    let quickcheck_generator = Lazy.force class_expr_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_expr_desc = struct
    type t = Astlib_first_draft.V4_07.Class_expr_desc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_expr_desc.to_ast t)
    let quickcheck_generator = Lazy.force class_expr_desc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_structure = struct
    type t = Astlib_first_draft.V4_07.Class_structure.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_structure.to_ast t)
    let quickcheck_generator = Lazy.force class_structure_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_field = struct
    type t = Astlib_first_draft.V4_07.Class_field.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_field.to_ast t)
    let quickcheck_generator = Lazy.force class_field_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_field_desc = struct
    type t = Astlib_first_draft.V4_07.Class_field_desc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_field_desc.to_ast t)
    let quickcheck_generator = Lazy.force class_field_desc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_value_desc = struct
    type t = Astlib_first_draft.V4_07.Class_value_desc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_value_desc.to_ast t)
    let quickcheck_generator = Lazy.force class_value_desc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_method_desc = struct
    type t = Astlib_first_draft.V4_07.Class_method_desc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_method_desc.to_ast t)
    let quickcheck_generator = Lazy.force class_method_desc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_field_kind = struct
    type t = Astlib_first_draft.V4_07.Class_field_kind.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_field_kind.to_ast t)
    let quickcheck_generator = Lazy.force class_field_kind_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Class_declaration = struct
    type t = Astlib_first_draft.V4_07.Class_declaration.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Class_declaration.to_ast t)
    let quickcheck_generator = Lazy.force class_declaration_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Module_type = struct
    type t = Astlib_first_draft.V4_07.Module_type.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Module_type.to_ast t)
    let quickcheck_generator = Lazy.force module_type_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Module_type_desc = struct
    type t = Astlib_first_draft.V4_07.Module_type_desc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Module_type_desc.to_ast t)
    let quickcheck_generator = Lazy.force module_type_desc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Signature = struct
    type t = Astlib_first_draft.V4_07.Signature.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Signature.to_ast t)
    let quickcheck_generator = Lazy.force signature_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Signature_item = struct
    type t = Astlib_first_draft.V4_07.Signature_item.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Signature_item.to_ast t)
    let quickcheck_generator = Lazy.force signature_item_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Signature_item_desc = struct
    type t = Astlib_first_draft.V4_07.Signature_item_desc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Signature_item_desc.to_ast t)
    let quickcheck_generator = Lazy.force signature_item_desc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Module_declaration = struct
    type t = Astlib_first_draft.V4_07.Module_declaration.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Module_declaration.to_ast t)
    let quickcheck_generator = Lazy.force module_declaration_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Module_type_declaration = struct
    type t = Astlib_first_draft.V4_07.Module_type_declaration.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Module_type_declaration.to_ast t)
    let quickcheck_generator = Lazy.force module_type_declaration_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Open_description = struct
    type t = Astlib_first_draft.V4_07.Open_description.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Open_description.to_ast t)
    let quickcheck_generator = Lazy.force open_description_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Include_description = struct
    type t = Astlib_first_draft.V4_07.Include_description.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Include_description.to_ast t)
    let quickcheck_generator = Lazy.force include_description_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Include_declaration = struct
    type t = Astlib_first_draft.V4_07.Include_declaration.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Include_declaration.to_ast t)
    let quickcheck_generator = Lazy.force include_declaration_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module With_constraint = struct
    type t = Astlib_first_draft.V4_07.With_constraint.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.With_constraint.to_ast t)
    let quickcheck_generator = Lazy.force with_constraint_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Module_expr = struct
    type t = Astlib_first_draft.V4_07.Module_expr.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Module_expr.to_ast t)
    let quickcheck_generator = Lazy.force module_expr_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Module_expr_desc = struct
    type t = Astlib_first_draft.V4_07.Module_expr_desc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Module_expr_desc.to_ast t)
    let quickcheck_generator = Lazy.force module_expr_desc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Structure = struct
    type t = Astlib_first_draft.V4_07.Structure.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Structure.to_ast t)
    let quickcheck_generator = Lazy.force structure_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Structure_item = struct
    type t = Astlib_first_draft.V4_07.Structure_item.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Structure_item.to_ast t)
    let quickcheck_generator = Lazy.force structure_item_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Structure_item_desc = struct
    type t = Astlib_first_draft.V4_07.Structure_item_desc.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Structure_item_desc.to_ast t)
    let quickcheck_generator = Lazy.force structure_item_desc_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Value_binding = struct
    type t = Astlib_first_draft.V4_07.Value_binding.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Value_binding.to_ast t)
    let quickcheck_generator = Lazy.force value_binding_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Module_binding = struct
    type t = Astlib_first_draft.V4_07.Module_binding.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Module_binding.to_ast t)
    let quickcheck_generator = Lazy.force module_binding_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Toplevel_phrase = struct
    type t = Astlib_first_draft.V4_07.Toplevel_phrase.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Toplevel_phrase.to_ast t)
    let quickcheck_generator = Lazy.force toplevel_phrase_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Directive_argument = struct
    type t = Astlib_first_draft.V4_07.Directive_argument.t

    let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.V4_07.Directive_argument.to_ast t)
    let quickcheck_generator = Lazy.force directive_argument_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end
end
(*$*)
