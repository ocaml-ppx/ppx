(*$ Astlib_src_cinaps.print_astlib_ml () *)
open! StdLabels
open! Ocaml_common

module Unversioned = struct
  type apply_arg = Versioned_ast.t
  type arg_label = Versioned_ast.t
  type attribute = Versioned_ast.t
  type attributes = Versioned_ast.t
  type case = Versioned_ast.t
  type class_declaration = Versioned_ast.t
  type class_description = Versioned_ast.t
  type class_expr = Versioned_ast.t
  type class_expr_desc = Versioned_ast.t
  type class_field = Versioned_ast.t
  type class_field_desc = Versioned_ast.t
  type class_field_kind = Versioned_ast.t
  type 'a class_infos = Versioned_ast.t
  type class_method_desc = Versioned_ast.t
  type class_signature = Versioned_ast.t
  type class_structure = Versioned_ast.t
  type class_type = Versioned_ast.t
  type class_type_constraint = Versioned_ast.t
  type class_type_declaration = Versioned_ast.t
  type class_type_desc = Versioned_ast.t
  type class_type_field = Versioned_ast.t
  type class_type_field_desc = Versioned_ast.t
  type class_type_method_desc = Versioned_ast.t
  type class_type_value_desc = Versioned_ast.t
  type class_value_desc = Versioned_ast.t
  type closed_flag = Versioned_ast.t
  type constant = Versioned_ast.t
  type constructor_arguments = Versioned_ast.t
  type constructor_declaration = Versioned_ast.t
  type core_type = Versioned_ast.t
  type core_type_desc = Versioned_ast.t
  type direction_flag = Versioned_ast.t
  type directive_argument = Versioned_ast.t
  type expression = Versioned_ast.t
  type expression_desc = Versioned_ast.t
  type extension = Versioned_ast.t
  type extension_constructor = Versioned_ast.t
  type extension_constructor_kind = Versioned_ast.t
  type include_declaration = Versioned_ast.t
  type include_description = Versioned_ast.t
  type 'a include_infos = Versioned_ast.t
  type label = Versioned_ast.t
  type label_declaration = Versioned_ast.t
  type label_loc = Versioned_ast.t
  type 'a loc = Versioned_ast.t
  type longident = Versioned_ast.t
  type longident_loc = Versioned_ast.t
  type module_binding = Versioned_ast.t
  type module_declaration = Versioned_ast.t
  type module_expr = Versioned_ast.t
  type module_expr_desc = Versioned_ast.t
  type module_type = Versioned_ast.t
  type module_type_declaration = Versioned_ast.t
  type module_type_desc = Versioned_ast.t
  type mutable_flag = Versioned_ast.t
  type object_field = Versioned_ast.t
  type open_description = Versioned_ast.t
  type override_expression = Versioned_ast.t
  type override_flag = Versioned_ast.t
  type package_type = Versioned_ast.t
  type package_type_constraint = Versioned_ast.t
  type pattern = Versioned_ast.t
  type pattern_desc = Versioned_ast.t
  type payload = Versioned_ast.t
  type private_flag = Versioned_ast.t
  type rec_flag = Versioned_ast.t
  type record_field_expression = Versioned_ast.t
  type record_field_pattern = Versioned_ast.t
  type row_field = Versioned_ast.t
  type signature = Versioned_ast.t
  type signature_item = Versioned_ast.t
  type signature_item_desc = Versioned_ast.t
  type string_loc = Versioned_ast.t
  type structure = Versioned_ast.t
  type structure_item = Versioned_ast.t
  type structure_item_desc = Versioned_ast.t
  type toplevel_phrase = Versioned_ast.t
  type type_constraint = Versioned_ast.t
  type type_declaration = Versioned_ast.t
  type type_extension = Versioned_ast.t
  type type_kind = Versioned_ast.t
  type type_param = Versioned_ast.t
  type value_binding = Versioned_ast.t
  type value_description = Versioned_ast.t
  type variance = Versioned_ast.t
  type virtual_flag = Versioned_ast.t
  type with_constraint = Versioned_ast.t
end

module V4_07 = struct
  let version = "V4_07"

  module Loc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type 'a t = { txt : 'a; loc : Location.t }
    end

    let create ~txt ~loc =
      Versioned_ast.create ~version
        { name = "loc"
        ; data =
            Record [("txt", a_of_concrete txt); ("loc", Versioned_value.of_location loc)]
        }

    let of_concrete ({ txt; loc } : 'a Concrete.t) : 'a t =
      { name = "loc"; data = Record [("txt", a_of_concrete txt); ("loc", Versioned_value.of_location loc)] }

    let to_concrete t : 'a Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "loc"; data = Record ["txt", txt; "loc", loc] } ->
        Optional.bind (a_to_concrete txt) ~f:(fun txt ->
          Optional.bind (Versioned_value.to_location loc) ~f:(fun loc ->
            Some { txt; loc }
        ))
      | _ -> None
  end

  module Longident = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Lident of string
        | Ldot of Versioned_ast.t * string
        | Lapply of Versioned_ast.t * Versioned_ast.t
    end

    let create_lident x1 =
      Versioned_ast.create ~version
        { name = "longident"
        ; data =
            Variant ("Lident", Tuple [Versioned_value.of_string x1])
        }
    let create_ldot x1 x2 =
      Versioned_ast.create ~version
        { name = "longident"
        ; data =
            Variant ("Ldot", Tuple [Versioned_value.of_ast x1; Versioned_value.of_string x2])
        }
    let create_lapply x1 x2 =
      Versioned_ast.create ~version
        { name = "longident"
        ; data =
            Variant ("Lapply", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Lident (x1) ->
        create_lident x1
      | Ldot (x1, x2) ->
        create_ldot x1 x2
      | Lapply (x1, x2) ->
        create_lapply x1 x2

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "longident"; data = data } ->
        ( match data with
        | Variant ("Lident", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_string x1) ~f:(fun x1 ->
            Some (Lident (x1))
          )
        | Variant ("Ldot", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_string x2) ~f:(fun x2 ->
              Some (Ldot (x1, x2))
          ))
        | Variant ("Lapply", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Lapply (x1, x2))
          ))
        )
      | _ -> None
  end

  module Longident_loc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t) Loc.t
    end

    let create x =
      Versioned_ast.create ~version
        { name = "longident_loc"
        ; data =
            (loc_of_concrete Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "longident_loc"; data = (loc_of_concrete Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "longident_loc"; data = data } ->
        (loc_to_concrete Versioned_value.to_ast) data
      | _ -> None
  end

  module Rec_flag = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Nonrecursive
        | Recursive
    end

    let create_nonrecursive =
      Versioned_ast.create ~version
        { name = "rec_flag"
        ; data =
            Variant ("Nonrecursive", Empty)
        }
    let create_recursive =
      Versioned_ast.create ~version
        { name = "rec_flag"
        ; data =
            Variant ("Recursive", Empty)
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Nonrecursive -> create_nonrecursive
      | Recursive -> create_recursive

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "rec_flag"; data = data } ->
        ( match data with
        | Variant ("Nonrecursive", Empty) -> Some Nonrecursive
        | Variant ("Recursive", Empty) -> Some Recursive
        )
      | _ -> None
  end

  module Direction_flag = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Upto
        | Downto
    end

    let create_upto =
      Versioned_ast.create ~version
        { name = "direction_flag"
        ; data =
            Variant ("Upto", Empty)
        }
    let create_downto =
      Versioned_ast.create ~version
        { name = "direction_flag"
        ; data =
            Variant ("Downto", Empty)
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Upto -> create_upto
      | Downto -> create_downto

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "direction_flag"; data = data } ->
        ( match data with
        | Variant ("Upto", Empty) -> Some Upto
        | Variant ("Downto", Empty) -> Some Downto
        )
      | _ -> None
  end

  module Private_flag = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Private
        | Public
    end

    let create_private =
      Versioned_ast.create ~version
        { name = "private_flag"
        ; data =
            Variant ("Private", Empty)
        }
    let create_public =
      Versioned_ast.create ~version
        { name = "private_flag"
        ; data =
            Variant ("Public", Empty)
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Private -> create_private
      | Public -> create_public

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "private_flag"; data = data } ->
        ( match data with
        | Variant ("Private", Empty) -> Some Private
        | Variant ("Public", Empty) -> Some Public
        )
      | _ -> None
  end

  module Mutable_flag = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Immutable
        | Mutable
    end

    let create_immutable =
      Versioned_ast.create ~version
        { name = "mutable_flag"
        ; data =
            Variant ("Immutable", Empty)
        }
    let create_mutable =
      Versioned_ast.create ~version
        { name = "mutable_flag"
        ; data =
            Variant ("Mutable", Empty)
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Immutable -> create_immutable
      | Mutable -> create_mutable

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "mutable_flag"; data = data } ->
        ( match data with
        | Variant ("Immutable", Empty) -> Some Immutable
        | Variant ("Mutable", Empty) -> Some Mutable
        )
      | _ -> None
  end

  module Virtual_flag = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Virtual
        | Concrete
    end

    let create_virtual =
      Versioned_ast.create ~version
        { name = "virtual_flag"
        ; data =
            Variant ("Virtual", Empty)
        }
    let create_concrete =
      Versioned_ast.create ~version
        { name = "virtual_flag"
        ; data =
            Variant ("Concrete", Empty)
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Virtual -> create_virtual
      | Concrete -> create_concrete

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "virtual_flag"; data = data } ->
        ( match data with
        | Variant ("Virtual", Empty) -> Some Virtual
        | Variant ("Concrete", Empty) -> Some Concrete
        )
      | _ -> None
  end

  module Override_flag = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Override
        | Fresh
    end

    let create_override =
      Versioned_ast.create ~version
        { name = "override_flag"
        ; data =
            Variant ("Override", Empty)
        }
    let create_fresh =
      Versioned_ast.create ~version
        { name = "override_flag"
        ; data =
            Variant ("Fresh", Empty)
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Override -> create_override
      | Fresh -> create_fresh

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "override_flag"; data = data } ->
        ( match data with
        | Variant ("Override", Empty) -> Some Override
        | Variant ("Fresh", Empty) -> Some Fresh
        )
      | _ -> None
  end

  module Closed_flag = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Closed
        | Open
    end

    let create_closed =
      Versioned_ast.create ~version
        { name = "closed_flag"
        ; data =
            Variant ("Closed", Empty)
        }
    let create_open =
      Versioned_ast.create ~version
        { name = "closed_flag"
        ; data =
            Variant ("Open", Empty)
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Closed -> create_closed
      | Open -> create_open

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "closed_flag"; data = data } ->
        ( match data with
        | Variant ("Closed", Empty) -> Some Closed
        | Variant ("Open", Empty) -> Some Open
        )
      | _ -> None
  end

  module Label = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = string
    end

    let create x =
      Versioned_ast.create ~version
        { name = "label"
        ; data =
            Versioned_value.of_string x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "label"; data = Versioned_value.of_string concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "label"; data = data } ->
        Versioned_value.to_string data
      | _ -> None
  end

  module Label_loc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t) Loc.t
    end

    let create x =
      Versioned_ast.create ~version
        { name = "label_loc"
        ; data =
            (loc_of_concrete Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "label_loc"; data = (loc_of_concrete Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "label_loc"; data = data } ->
        (loc_to_concrete Versioned_value.to_ast) data
      | _ -> None
  end

  module String_loc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (string) Loc.t
    end

    let create x =
      Versioned_ast.create ~version
        { name = "string_loc"
        ; data =
            (loc_of_concrete Versioned_value.of_string) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "string_loc"; data = (loc_of_concrete Versioned_value.of_string) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "string_loc"; data = data } ->
        (loc_to_concrete Versioned_value.to_string) data
      | _ -> None
  end

  module Arg_label = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Nolabel
        | Labelled of string
        | Optional of string
    end

    let create_nolabel =
      Versioned_ast.create ~version
        { name = "arg_label"
        ; data =
            Variant ("Nolabel", Empty)
        }
    let create_labelled x1 =
      Versioned_ast.create ~version
        { name = "arg_label"
        ; data =
            Variant ("Labelled", Tuple [Versioned_value.of_string x1])
        }
    let create_optional x1 =
      Versioned_ast.create ~version
        { name = "arg_label"
        ; data =
            Variant ("Optional", Tuple [Versioned_value.of_string x1])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Nolabel -> create_nolabel
      | Labelled (x1) ->
        create_labelled x1
      | Optional (x1) ->
        create_optional x1

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "arg_label"; data = data } ->
        ( match data with
        | Variant ("Nolabel", Empty) -> Some Nolabel
        | Variant ("Labelled", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_string x1) ~f:(fun x1 ->
            Some (Labelled (x1))
          )
        | Variant ("Optional", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_string x1) ~f:(fun x1 ->
            Some (Optional (x1))
          )
        )
      | _ -> None
  end

  module Variance = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Covariant
        | Contravariant
        | Invariant
    end

    let create_covariant =
      Versioned_ast.create ~version
        { name = "variance"
        ; data =
            Variant ("Covariant", Empty)
        }
    let create_contravariant =
      Versioned_ast.create ~version
        { name = "variance"
        ; data =
            Variant ("Contravariant", Empty)
        }
    let create_invariant =
      Versioned_ast.create ~version
        { name = "variance"
        ; data =
            Variant ("Invariant", Empty)
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Covariant -> create_covariant
      | Contravariant -> create_contravariant
      | Invariant -> create_invariant

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "variance"; data = data } ->
        ( match data with
        | Variant ("Covariant", Empty) -> Some Covariant
        | Variant ("Contravariant", Empty) -> Some Contravariant
        | Variant ("Invariant", Empty) -> Some Invariant
        )
      | _ -> None
  end

  module Constant = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pconst_integer of string * char option
        | Pconst_char of char
        | Pconst_string of string * string option
        | Pconst_float of string * char option
    end

    let create_pconst_integer x1 x2 =
      Versioned_ast.create ~version
        { name = "constant"
        ; data =
            Variant ("Pconst_integer", Tuple [Versioned_value.of_string x1; (Versioned_value.of_option ~f:Versioned_value.of_char) x2])
        }
    let create_pconst_char x1 =
      Versioned_ast.create ~version
        { name = "constant"
        ; data =
            Variant ("Pconst_char", Tuple [Versioned_value.of_char x1])
        }
    let create_pconst_string x1 x2 =
      Versioned_ast.create ~version
        { name = "constant"
        ; data =
            Variant ("Pconst_string", Tuple [Versioned_value.of_string x1; (Versioned_value.of_option ~f:Versioned_value.of_string) x2])
        }
    let create_pconst_float x1 x2 =
      Versioned_ast.create ~version
        { name = "constant"
        ; data =
            Variant ("Pconst_float", Tuple [Versioned_value.of_string x1; (Versioned_value.of_option ~f:Versioned_value.of_char) x2])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Pconst_integer (x1, x2) ->
        create_pconst_integer x1 x2
      | Pconst_char (x1) ->
        create_pconst_char x1
      | Pconst_string (x1, x2) ->
        create_pconst_string x1 x2
      | Pconst_float (x1, x2) ->
        create_pconst_float x1 x2

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "constant"; data = data } ->
        ( match data with
        | Variant ("Pconst_integer", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_string x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_char) x2) ~f:(fun x2 ->
              Some (Pconst_integer (x1, x2))
          ))
        | Variant ("Pconst_char", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_char x1) ~f:(fun x1 ->
            Some (Pconst_char (x1))
          )
        | Variant ("Pconst_string", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_string x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_string) x2) ~f:(fun x2 ->
              Some (Pconst_string (x1, x2))
          ))
        | Variant ("Pconst_float", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_string x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_char) x2) ~f:(fun x2 ->
              Some (Pconst_float (x1, x2))
          ))
        )
      | _ -> None
  end

  module Attribute = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t * Versioned_ast.t)
    end

    let create x =
      Versioned_ast.create ~version
        { name = "attribute"
        ; data =
            (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "attribute"; data = (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "attribute"; data = data } ->
        (Versioned_value.to_tuple2 ~f1:Versioned_value.to_ast ~f2:Versioned_value.to_ast) data
      | _ -> None
  end

  module Extension = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t * Versioned_ast.t)
    end

    let create x =
      Versioned_ast.create ~version
        { name = "extension"
        ; data =
            (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "extension"; data = (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "extension"; data = data } ->
        (Versioned_value.to_tuple2 ~f1:Versioned_value.to_ast ~f2:Versioned_value.to_ast) data
      | _ -> None
  end

  module Attributes = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = Versioned_ast.t list
    end

    let create x =
      Versioned_ast.create ~version
        { name = "attributes"
        ; data =
            (Versioned_value.of_list ~f:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "attributes"; data = (Versioned_value.of_list ~f:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "attributes"; data = data } ->
        (Versioned_value.to_list ~f:Versioned_value.to_ast) data
      | _ -> None
  end

  module Payload = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | PStr of Versioned_ast.t
        | PSig of Versioned_ast.t
        | PTyp of Versioned_ast.t
        | PPat of Versioned_ast.t * Versioned_ast.t option
    end

    let create_pstr x1 =
      Versioned_ast.create ~version
        { name = "payload"
        ; data =
            Variant ("PStr", Tuple [Versioned_value.of_ast x1])
        }
    let create_psig x1 =
      Versioned_ast.create ~version
        { name = "payload"
        ; data =
            Variant ("PSig", Tuple [Versioned_value.of_ast x1])
        }
    let create_ptyp x1 =
      Versioned_ast.create ~version
        { name = "payload"
        ; data =
            Variant ("PTyp", Tuple [Versioned_value.of_ast x1])
        }
    let create_ppat x1 x2 =
      Versioned_ast.create ~version
        { name = "payload"
        ; data =
            Variant ("PPat", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_option ~f:Versioned_value.of_ast) x2])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | PStr (x1) ->
        create_pstr x1
      | PSig (x1) ->
        create_psig x1
      | PTyp (x1) ->
        create_ptyp x1
      | PPat (x1, x2) ->
        create_ppat x1 x2

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "payload"; data = data } ->
        ( match data with
        | Variant ("PStr", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (PStr (x1))
          )
        | Variant ("PSig", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (PSig (x1))
          )
        | Variant ("PTyp", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (PTyp (x1))
          )
        | Variant ("PPat", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (PPat (x1, x2))
          ))
        )
      | _ -> None
  end

  module Core_type = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { ptyp_desc : Versioned_ast.t; ptyp_loc : Location.t; ptyp_attributes : Versioned_ast.t }
    end

    let create ~ptyp_desc ~ptyp_loc ~ptyp_attributes =
      Versioned_ast.create ~version
        { name = "core_type"
        ; data =
            Record [("ptyp_desc", Versioned_value.of_ast ptyp_desc); ("ptyp_loc", Versioned_value.of_location ptyp_loc); ("ptyp_attributes", Versioned_value.of_ast ptyp_attributes)]
        }

    let of_concrete ({ ptyp_desc; ptyp_loc; ptyp_attributes } : Concrete.t) : t =
      { name = "core_type"; data = Record [("ptyp_desc", Versioned_value.of_ast ptyp_desc); ("ptyp_loc", Versioned_value.of_location ptyp_loc); ("ptyp_attributes", Versioned_value.of_ast ptyp_attributes)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "core_type"; data = Record ["ptyp_desc", ptyp_desc; "ptyp_loc", ptyp_loc; "ptyp_attributes", ptyp_attributes] } ->
        Optional.bind (Versioned_value.to_ast ptyp_desc) ~f:(fun ptyp_desc ->
          Optional.bind (Versioned_value.to_location ptyp_loc) ~f:(fun ptyp_loc ->
            Optional.bind (Versioned_value.to_ast ptyp_attributes) ~f:(fun ptyp_attributes ->
              Some { ptyp_desc; ptyp_loc; ptyp_attributes }
        )))
      | _ -> None
  end

  module Core_type_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Ptyp_any
        | Ptyp_var of string
        | Ptyp_arrow of Versioned_ast.t * Versioned_ast.t * Versioned_ast.t
        | Ptyp_tuple of Versioned_ast.t list
        | Ptyp_constr of Versioned_ast.t * Versioned_ast.t list
        | Ptyp_object of Versioned_ast.t list * Versioned_ast.t
        | Ptyp_class of Versioned_ast.t * Versioned_ast.t list
        | Ptyp_alias of Versioned_ast.t * string
        | Ptyp_variant of Versioned_ast.t list * Versioned_ast.t * Versioned_ast.t list option
        | Ptyp_poly of Versioned_ast.t list * Versioned_ast.t
        | Ptyp_package of Versioned_ast.t
        | Ptyp_extension of Versioned_ast.t
    end

    let create_ptyp_any =
      Versioned_ast.create ~version
        { name = "core_type_desc"
        ; data =
            Variant ("Ptyp_any", Empty)
        }
    let create_ptyp_var x1 =
      Versioned_ast.create ~version
        { name = "core_type_desc"
        ; data =
            Variant ("Ptyp_var", Tuple [Versioned_value.of_string x1])
        }
    let create_ptyp_arrow x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "core_type_desc"
        ; data =
            Variant ("Ptyp_arrow", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2; Versioned_value.of_ast x3])
        }
    let create_ptyp_tuple x1 =
      Versioned_ast.create ~version
        { name = "core_type_desc"
        ; data =
            Variant ("Ptyp_tuple", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_ptyp_constr x1 x2 =
      Versioned_ast.create ~version
        { name = "core_type_desc"
        ; data =
            Variant ("Ptyp_constr", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_list ~f:Versioned_value.of_ast) x2])
        }
    let create_ptyp_object x1 x2 =
      Versioned_ast.create ~version
        { name = "core_type_desc"
        ; data =
            Variant ("Ptyp_object", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1; Versioned_value.of_ast x2])
        }
    let create_ptyp_class x1 x2 =
      Versioned_ast.create ~version
        { name = "core_type_desc"
        ; data =
            Variant ("Ptyp_class", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_list ~f:Versioned_value.of_ast) x2])
        }
    let create_ptyp_alias x1 x2 =
      Versioned_ast.create ~version
        { name = "core_type_desc"
        ; data =
            Variant ("Ptyp_alias", Tuple [Versioned_value.of_ast x1; Versioned_value.of_string x2])
        }
    let create_ptyp_variant x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "core_type_desc"
        ; data =
            Variant ("Ptyp_variant", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1; Versioned_value.of_ast x2; (Versioned_value.of_option ~f:(Versioned_value.of_list ~f:Versioned_value.of_ast)) x3])
        }
    let create_ptyp_poly x1 x2 =
      Versioned_ast.create ~version
        { name = "core_type_desc"
        ; data =
            Variant ("Ptyp_poly", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1; Versioned_value.of_ast x2])
        }
    let create_ptyp_package x1 =
      Versioned_ast.create ~version
        { name = "core_type_desc"
        ; data =
            Variant ("Ptyp_package", Tuple [Versioned_value.of_ast x1])
        }
    let create_ptyp_extension x1 =
      Versioned_ast.create ~version
        { name = "core_type_desc"
        ; data =
            Variant ("Ptyp_extension", Tuple [Versioned_value.of_ast x1])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Ptyp_any -> create_ptyp_any
      | Ptyp_var (x1) ->
        create_ptyp_var x1
      | Ptyp_arrow (x1, x2, x3) ->
        create_ptyp_arrow x1 x2 x3
      | Ptyp_tuple (x1) ->
        create_ptyp_tuple x1
      | Ptyp_constr (x1, x2) ->
        create_ptyp_constr x1 x2
      | Ptyp_object (x1, x2) ->
        create_ptyp_object x1 x2
      | Ptyp_class (x1, x2) ->
        create_ptyp_class x1 x2
      | Ptyp_alias (x1, x2) ->
        create_ptyp_alias x1 x2
      | Ptyp_variant (x1, x2, x3) ->
        create_ptyp_variant x1 x2 x3
      | Ptyp_poly (x1, x2) ->
        create_ptyp_poly x1 x2
      | Ptyp_package (x1) ->
        create_ptyp_package x1
      | Ptyp_extension (x1) ->
        create_ptyp_extension x1

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "core_type_desc"; data = data } ->
        ( match data with
        | Variant ("Ptyp_any", Empty) -> Some Ptyp_any
        | Variant ("Ptyp_var", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_string x1) ~f:(fun x1 ->
            Some (Ptyp_var (x1))
          )
        | Variant ("Ptyp_arrow", Tuple [x1; x2; x3]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Some (Ptyp_arrow (x1, x2, x3))
          )))
        | Variant ("Ptyp_tuple", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Ptyp_tuple (x1))
          )
        | Variant ("Ptyp_constr", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Ptyp_constr (x1, x2))
          ))
        | Variant ("Ptyp_object", Tuple [x1; x2]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Ptyp_object (x1, x2))
          ))
        | Variant ("Ptyp_class", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Ptyp_class (x1, x2))
          ))
        | Variant ("Ptyp_alias", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_string x2) ~f:(fun x2 ->
              Some (Ptyp_alias (x1, x2))
          ))
        | Variant ("Ptyp_variant", Tuple [x1; x2; x3]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Optional.bind ((Versioned_value.to_option ~f:(Versioned_value.to_list ~f:Versioned_value.to_ast)) x3) ~f:(fun x3 ->
                Some (Ptyp_variant (x1, x2, x3))
          )))
        | Variant ("Ptyp_poly", Tuple [x1; x2]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Ptyp_poly (x1, x2))
          ))
        | Variant ("Ptyp_package", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Ptyp_package (x1))
          )
        | Variant ("Ptyp_extension", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Ptyp_extension (x1))
          )
        )
      | _ -> None
  end

  module Package_type = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t * Versioned_ast.t list)
    end

    let create x =
      Versioned_ast.create ~version
        { name = "package_type"
        ; data =
            (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:(Versioned_value.of_list ~f:Versioned_value.of_ast)) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "package_type"; data = (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:(Versioned_value.of_list ~f:Versioned_value.of_ast)) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "package_type"; data = data } ->
        (Versioned_value.to_tuple2 ~f1:Versioned_value.to_ast ~f2:(Versioned_value.to_list ~f:Versioned_value.to_ast)) data
      | _ -> None
  end

  module Package_type_constraint = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t * Versioned_ast.t)
    end

    let create x =
      Versioned_ast.create ~version
        { name = "package_type_constraint"
        ; data =
            (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "package_type_constraint"; data = (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "package_type_constraint"; data = data } ->
        (Versioned_value.to_tuple2 ~f1:Versioned_value.to_ast ~f2:Versioned_value.to_ast) data
      | _ -> None
  end

  module Row_field = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Rtag of Versioned_ast.t * Versioned_ast.t * bool * Versioned_ast.t list
        | Rinherit of Versioned_ast.t
    end

    let create_rtag x1 x2 x3 x4 =
      Versioned_ast.create ~version
        { name = "row_field"
        ; data =
            Variant ("Rtag", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2; Versioned_value.of_bool x3; (Versioned_value.of_list ~f:Versioned_value.of_ast) x4])
        }
    let create_rinherit x1 =
      Versioned_ast.create ~version
        { name = "row_field"
        ; data =
            Variant ("Rinherit", Tuple [Versioned_value.of_ast x1])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Rtag (x1, x2, x3, x4) ->
        create_rtag x1 x2 x3 x4
      | Rinherit (x1) ->
        create_rinherit x1

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "row_field"; data = data } ->
        ( match data with
        | Variant ("Rtag", Tuple [x1; x2; x3; x4]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_bool x3) ~f:(fun x3 ->
                Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x4) ~f:(fun x4 ->
                  Some (Rtag (x1, x2, x3, x4))
          ))))
        | Variant ("Rinherit", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Rinherit (x1))
          )
        )
      | _ -> None
  end

  module Object_field = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Otag of Versioned_ast.t * Versioned_ast.t * Versioned_ast.t
        | Oinherit of Versioned_ast.t
    end

    let create_otag x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "object_field"
        ; data =
            Variant ("Otag", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2; Versioned_value.of_ast x3])
        }
    let create_oinherit x1 =
      Versioned_ast.create ~version
        { name = "object_field"
        ; data =
            Variant ("Oinherit", Tuple [Versioned_value.of_ast x1])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Otag (x1, x2, x3) ->
        create_otag x1 x2 x3
      | Oinherit (x1) ->
        create_oinherit x1

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "object_field"; data = data } ->
        ( match data with
        | Variant ("Otag", Tuple [x1; x2; x3]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Some (Otag (x1, x2, x3))
          )))
        | Variant ("Oinherit", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Oinherit (x1))
          )
        )
      | _ -> None
  end

  module Pattern = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { ppat_desc : Versioned_ast.t; ppat_loc : Location.t; ppat_attributes : Versioned_ast.t }
    end

    let create ~ppat_desc ~ppat_loc ~ppat_attributes =
      Versioned_ast.create ~version
        { name = "pattern"
        ; data =
            Record [("ppat_desc", Versioned_value.of_ast ppat_desc); ("ppat_loc", Versioned_value.of_location ppat_loc); ("ppat_attributes", Versioned_value.of_ast ppat_attributes)]
        }

    let of_concrete ({ ppat_desc; ppat_loc; ppat_attributes } : Concrete.t) : t =
      { name = "pattern"; data = Record [("ppat_desc", Versioned_value.of_ast ppat_desc); ("ppat_loc", Versioned_value.of_location ppat_loc); ("ppat_attributes", Versioned_value.of_ast ppat_attributes)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "pattern"; data = Record ["ppat_desc", ppat_desc; "ppat_loc", ppat_loc; "ppat_attributes", ppat_attributes] } ->
        Optional.bind (Versioned_value.to_ast ppat_desc) ~f:(fun ppat_desc ->
          Optional.bind (Versioned_value.to_location ppat_loc) ~f:(fun ppat_loc ->
            Optional.bind (Versioned_value.to_ast ppat_attributes) ~f:(fun ppat_attributes ->
              Some { ppat_desc; ppat_loc; ppat_attributes }
        )))
      | _ -> None
  end

  module Pattern_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Ppat_any
        | Ppat_var of Versioned_ast.t
        | Ppat_alias of Versioned_ast.t * Versioned_ast.t
        | Ppat_constant of Versioned_ast.t
        | Ppat_interval of Versioned_ast.t * Versioned_ast.t
        | Ppat_tuple of Versioned_ast.t list
        | Ppat_construct of Versioned_ast.t * Versioned_ast.t option
        | Ppat_variant of Versioned_ast.t * Versioned_ast.t option
        | Ppat_record of Versioned_ast.t list * Versioned_ast.t
        | Ppat_array of Versioned_ast.t list
        | Ppat_or of Versioned_ast.t * Versioned_ast.t
        | Ppat_constraint of Versioned_ast.t * Versioned_ast.t
        | Ppat_type of Versioned_ast.t
        | Ppat_lazy of Versioned_ast.t
        | Ppat_unpack of Versioned_ast.t
        | Ppat_exception of Versioned_ast.t
        | Ppat_extension of Versioned_ast.t
        | Ppat_open of Versioned_ast.t * Versioned_ast.t
    end

    let create_ppat_any =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_any", Empty)
        }
    let create_ppat_var x1 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_var", Tuple [Versioned_value.of_ast x1])
        }
    let create_ppat_alias x1 x2 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_alias", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_ppat_constant x1 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_constant", Tuple [Versioned_value.of_ast x1])
        }
    let create_ppat_interval x1 x2 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_interval", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_ppat_tuple x1 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_tuple", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_ppat_construct x1 x2 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_construct", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_option ~f:Versioned_value.of_ast) x2])
        }
    let create_ppat_variant x1 x2 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_variant", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_option ~f:Versioned_value.of_ast) x2])
        }
    let create_ppat_record x1 x2 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_record", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1; Versioned_value.of_ast x2])
        }
    let create_ppat_array x1 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_array", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_ppat_or x1 x2 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_or", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_ppat_constraint x1 x2 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_constraint", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_ppat_type x1 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_type", Tuple [Versioned_value.of_ast x1])
        }
    let create_ppat_lazy x1 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_lazy", Tuple [Versioned_value.of_ast x1])
        }
    let create_ppat_unpack x1 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_unpack", Tuple [Versioned_value.of_ast x1])
        }
    let create_ppat_exception x1 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_exception", Tuple [Versioned_value.of_ast x1])
        }
    let create_ppat_extension x1 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_extension", Tuple [Versioned_value.of_ast x1])
        }
    let create_ppat_open x1 x2 =
      Versioned_ast.create ~version
        { name = "pattern_desc"
        ; data =
            Variant ("Ppat_open", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Ppat_any -> create_ppat_any
      | Ppat_var (x1) ->
        create_ppat_var x1
      | Ppat_alias (x1, x2) ->
        create_ppat_alias x1 x2
      | Ppat_constant (x1) ->
        create_ppat_constant x1
      | Ppat_interval (x1, x2) ->
        create_ppat_interval x1 x2
      | Ppat_tuple (x1) ->
        create_ppat_tuple x1
      | Ppat_construct (x1, x2) ->
        create_ppat_construct x1 x2
      | Ppat_variant (x1, x2) ->
        create_ppat_variant x1 x2
      | Ppat_record (x1, x2) ->
        create_ppat_record x1 x2
      | Ppat_array (x1) ->
        create_ppat_array x1
      | Ppat_or (x1, x2) ->
        create_ppat_or x1 x2
      | Ppat_constraint (x1, x2) ->
        create_ppat_constraint x1 x2
      | Ppat_type (x1) ->
        create_ppat_type x1
      | Ppat_lazy (x1) ->
        create_ppat_lazy x1
      | Ppat_unpack (x1) ->
        create_ppat_unpack x1
      | Ppat_exception (x1) ->
        create_ppat_exception x1
      | Ppat_extension (x1) ->
        create_ppat_extension x1
      | Ppat_open (x1, x2) ->
        create_ppat_open x1 x2

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "pattern_desc"; data = data } ->
        ( match data with
        | Variant ("Ppat_any", Empty) -> Some Ppat_any
        | Variant ("Ppat_var", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Ppat_var (x1))
          )
        | Variant ("Ppat_alias", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Ppat_alias (x1, x2))
          ))
        | Variant ("Ppat_constant", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Ppat_constant (x1))
          )
        | Variant ("Ppat_interval", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Ppat_interval (x1, x2))
          ))
        | Variant ("Ppat_tuple", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Ppat_tuple (x1))
          )
        | Variant ("Ppat_construct", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Ppat_construct (x1, x2))
          ))
        | Variant ("Ppat_variant", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Ppat_variant (x1, x2))
          ))
        | Variant ("Ppat_record", Tuple [x1; x2]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Ppat_record (x1, x2))
          ))
        | Variant ("Ppat_array", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Ppat_array (x1))
          )
        | Variant ("Ppat_or", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Ppat_or (x1, x2))
          ))
        | Variant ("Ppat_constraint", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Ppat_constraint (x1, x2))
          ))
        | Variant ("Ppat_type", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Ppat_type (x1))
          )
        | Variant ("Ppat_lazy", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Ppat_lazy (x1))
          )
        | Variant ("Ppat_unpack", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Ppat_unpack (x1))
          )
        | Variant ("Ppat_exception", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Ppat_exception (x1))
          )
        | Variant ("Ppat_extension", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Ppat_extension (x1))
          )
        | Variant ("Ppat_open", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Ppat_open (x1, x2))
          ))
        )
      | _ -> None
  end

  module Record_field_pattern = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t * Versioned_ast.t)
    end

    let create x =
      Versioned_ast.create ~version
        { name = "record_field_pattern"
        ; data =
            (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "record_field_pattern"; data = (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "record_field_pattern"; data = data } ->
        (Versioned_value.to_tuple2 ~f1:Versioned_value.to_ast ~f2:Versioned_value.to_ast) data
      | _ -> None
  end

  module Expression = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pexp_desc : Versioned_ast.t; pexp_loc : Location.t; pexp_attributes : Versioned_ast.t }
    end

    let create ~pexp_desc ~pexp_loc ~pexp_attributes =
      Versioned_ast.create ~version
        { name = "expression"
        ; data =
            Record [("pexp_desc", Versioned_value.of_ast pexp_desc); ("pexp_loc", Versioned_value.of_location pexp_loc); ("pexp_attributes", Versioned_value.of_ast pexp_attributes)]
        }

    let of_concrete ({ pexp_desc; pexp_loc; pexp_attributes } : Concrete.t) : t =
      { name = "expression"; data = Record [("pexp_desc", Versioned_value.of_ast pexp_desc); ("pexp_loc", Versioned_value.of_location pexp_loc); ("pexp_attributes", Versioned_value.of_ast pexp_attributes)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "expression"; data = Record ["pexp_desc", pexp_desc; "pexp_loc", pexp_loc; "pexp_attributes", pexp_attributes] } ->
        Optional.bind (Versioned_value.to_ast pexp_desc) ~f:(fun pexp_desc ->
          Optional.bind (Versioned_value.to_location pexp_loc) ~f:(fun pexp_loc ->
            Optional.bind (Versioned_value.to_ast pexp_attributes) ~f:(fun pexp_attributes ->
              Some { pexp_desc; pexp_loc; pexp_attributes }
        )))
      | _ -> None
  end

  module Expression_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pexp_ident of Versioned_ast.t
        | Pexp_constant of Versioned_ast.t
        | Pexp_let of Versioned_ast.t * Versioned_ast.t list * Versioned_ast.t
        | Pexp_function of Versioned_ast.t list
        | Pexp_fun of Versioned_ast.t * Versioned_ast.t option * Versioned_ast.t * Versioned_ast.t
        | Pexp_apply of Versioned_ast.t * Versioned_ast.t list
        | Pexp_match of Versioned_ast.t * Versioned_ast.t list
        | Pexp_try of Versioned_ast.t * Versioned_ast.t list
        | Pexp_tuple of Versioned_ast.t list
        | Pexp_construct of Versioned_ast.t * Versioned_ast.t option
        | Pexp_variant of Versioned_ast.t * Versioned_ast.t option
        | Pexp_record of Versioned_ast.t list * Versioned_ast.t option
        | Pexp_field of Versioned_ast.t * Versioned_ast.t
        | Pexp_setfield of Versioned_ast.t * Versioned_ast.t * Versioned_ast.t
        | Pexp_array of Versioned_ast.t list
        | Pexp_ifthenelse of Versioned_ast.t * Versioned_ast.t * Versioned_ast.t option
        | Pexp_sequence of Versioned_ast.t * Versioned_ast.t
        | Pexp_while of Versioned_ast.t * Versioned_ast.t
        | Pexp_for of Versioned_ast.t * Versioned_ast.t * Versioned_ast.t * Versioned_ast.t * Versioned_ast.t
        | Pexp_constraint of Versioned_ast.t * Versioned_ast.t
        | Pexp_coerce of Versioned_ast.t * Versioned_ast.t option * Versioned_ast.t
        | Pexp_send of Versioned_ast.t * Versioned_ast.t
        | Pexp_new of Versioned_ast.t
        | Pexp_setinstvar of Versioned_ast.t * Versioned_ast.t
        | Pexp_override of Versioned_ast.t list
        | Pexp_letmodule of Versioned_ast.t * Versioned_ast.t * Versioned_ast.t
        | Pexp_letexception of Versioned_ast.t * Versioned_ast.t
        | Pexp_assert of Versioned_ast.t
        | Pexp_lazy of Versioned_ast.t
        | Pexp_poly of Versioned_ast.t * Versioned_ast.t option
        | Pexp_object of Versioned_ast.t
        | Pexp_newtype of Versioned_ast.t * Versioned_ast.t
        | Pexp_pack of Versioned_ast.t
        | Pexp_open of Versioned_ast.t * Versioned_ast.t * Versioned_ast.t
        | Pexp_extension of Versioned_ast.t
        | Pexp_unreachable
    end

    let create_pexp_ident x1 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_ident", Tuple [Versioned_value.of_ast x1])
        }
    let create_pexp_constant x1 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_constant", Tuple [Versioned_value.of_ast x1])
        }
    let create_pexp_let x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_let", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_list ~f:Versioned_value.of_ast) x2; Versioned_value.of_ast x3])
        }
    let create_pexp_function x1 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_function", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_pexp_fun x1 x2 x3 x4 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_fun", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_option ~f:Versioned_value.of_ast) x2; Versioned_value.of_ast x3; Versioned_value.of_ast x4])
        }
    let create_pexp_apply x1 x2 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_apply", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_list ~f:Versioned_value.of_ast) x2])
        }
    let create_pexp_match x1 x2 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_match", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_list ~f:Versioned_value.of_ast) x2])
        }
    let create_pexp_try x1 x2 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_try", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_list ~f:Versioned_value.of_ast) x2])
        }
    let create_pexp_tuple x1 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_tuple", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_pexp_construct x1 x2 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_construct", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_option ~f:Versioned_value.of_ast) x2])
        }
    let create_pexp_variant x1 x2 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_variant", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_option ~f:Versioned_value.of_ast) x2])
        }
    let create_pexp_record x1 x2 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_record", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1; (Versioned_value.of_option ~f:Versioned_value.of_ast) x2])
        }
    let create_pexp_field x1 x2 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_field", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_pexp_setfield x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_setfield", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2; Versioned_value.of_ast x3])
        }
    let create_pexp_array x1 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_array", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_pexp_ifthenelse x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_ifthenelse", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2; (Versioned_value.of_option ~f:Versioned_value.of_ast) x3])
        }
    let create_pexp_sequence x1 x2 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_sequence", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_pexp_while x1 x2 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_while", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_pexp_for x1 x2 x3 x4 x5 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_for", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2; Versioned_value.of_ast x3; Versioned_value.of_ast x4; Versioned_value.of_ast x5])
        }
    let create_pexp_constraint x1 x2 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_constraint", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_pexp_coerce x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_coerce", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_option ~f:Versioned_value.of_ast) x2; Versioned_value.of_ast x3])
        }
    let create_pexp_send x1 x2 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_send", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_pexp_new x1 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_new", Tuple [Versioned_value.of_ast x1])
        }
    let create_pexp_setinstvar x1 x2 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_setinstvar", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_pexp_override x1 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_override", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_pexp_letmodule x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_letmodule", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2; Versioned_value.of_ast x3])
        }
    let create_pexp_letexception x1 x2 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_letexception", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_pexp_assert x1 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_assert", Tuple [Versioned_value.of_ast x1])
        }
    let create_pexp_lazy x1 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_lazy", Tuple [Versioned_value.of_ast x1])
        }
    let create_pexp_poly x1 x2 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_poly", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_option ~f:Versioned_value.of_ast) x2])
        }
    let create_pexp_object x1 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_object", Tuple [Versioned_value.of_ast x1])
        }
    let create_pexp_newtype x1 x2 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_newtype", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_pexp_pack x1 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_pack", Tuple [Versioned_value.of_ast x1])
        }
    let create_pexp_open x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_open", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2; Versioned_value.of_ast x3])
        }
    let create_pexp_extension x1 =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_extension", Tuple [Versioned_value.of_ast x1])
        }
    let create_pexp_unreachable =
      Versioned_ast.create ~version
        { name = "expression_desc"
        ; data =
            Variant ("Pexp_unreachable", Empty)
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Pexp_ident (x1) ->
        create_pexp_ident x1
      | Pexp_constant (x1) ->
        create_pexp_constant x1
      | Pexp_let (x1, x2, x3) ->
        create_pexp_let x1 x2 x3
      | Pexp_function (x1) ->
        create_pexp_function x1
      | Pexp_fun (x1, x2, x3, x4) ->
        create_pexp_fun x1 x2 x3 x4
      | Pexp_apply (x1, x2) ->
        create_pexp_apply x1 x2
      | Pexp_match (x1, x2) ->
        create_pexp_match x1 x2
      | Pexp_try (x1, x2) ->
        create_pexp_try x1 x2
      | Pexp_tuple (x1) ->
        create_pexp_tuple x1
      | Pexp_construct (x1, x2) ->
        create_pexp_construct x1 x2
      | Pexp_variant (x1, x2) ->
        create_pexp_variant x1 x2
      | Pexp_record (x1, x2) ->
        create_pexp_record x1 x2
      | Pexp_field (x1, x2) ->
        create_pexp_field x1 x2
      | Pexp_setfield (x1, x2, x3) ->
        create_pexp_setfield x1 x2 x3
      | Pexp_array (x1) ->
        create_pexp_array x1
      | Pexp_ifthenelse (x1, x2, x3) ->
        create_pexp_ifthenelse x1 x2 x3
      | Pexp_sequence (x1, x2) ->
        create_pexp_sequence x1 x2
      | Pexp_while (x1, x2) ->
        create_pexp_while x1 x2
      | Pexp_for (x1, x2, x3, x4, x5) ->
        create_pexp_for x1 x2 x3 x4 x5
      | Pexp_constraint (x1, x2) ->
        create_pexp_constraint x1 x2
      | Pexp_coerce (x1, x2, x3) ->
        create_pexp_coerce x1 x2 x3
      | Pexp_send (x1, x2) ->
        create_pexp_send x1 x2
      | Pexp_new (x1) ->
        create_pexp_new x1
      | Pexp_setinstvar (x1, x2) ->
        create_pexp_setinstvar x1 x2
      | Pexp_override (x1) ->
        create_pexp_override x1
      | Pexp_letmodule (x1, x2, x3) ->
        create_pexp_letmodule x1 x2 x3
      | Pexp_letexception (x1, x2) ->
        create_pexp_letexception x1 x2
      | Pexp_assert (x1) ->
        create_pexp_assert x1
      | Pexp_lazy (x1) ->
        create_pexp_lazy x1
      | Pexp_poly (x1, x2) ->
        create_pexp_poly x1 x2
      | Pexp_object (x1) ->
        create_pexp_object x1
      | Pexp_newtype (x1, x2) ->
        create_pexp_newtype x1 x2
      | Pexp_pack (x1) ->
        create_pexp_pack x1
      | Pexp_open (x1, x2, x3) ->
        create_pexp_open x1 x2 x3
      | Pexp_extension (x1) ->
        create_pexp_extension x1
      | Pexp_unreachable -> create_pexp_unreachable

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "expression_desc"; data = data } ->
        ( match data with
        | Variant ("Pexp_ident", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pexp_ident (x1))
          )
        | Variant ("Pexp_constant", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pexp_constant (x1))
          )
        | Variant ("Pexp_let", Tuple [x1; x2; x3]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Some (Pexp_let (x1, x2, x3))
          )))
        | Variant ("Pexp_function", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Pexp_function (x1))
          )
        | Variant ("Pexp_fun", Tuple [x1; x2; x3; x4]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Optional.bind (Versioned_value.to_ast x4) ~f:(fun x4 ->
                  Some (Pexp_fun (x1, x2, x3, x4))
          ))))
        | Variant ("Pexp_apply", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Pexp_apply (x1, x2))
          ))
        | Variant ("Pexp_match", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Pexp_match (x1, x2))
          ))
        | Variant ("Pexp_try", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Pexp_try (x1, x2))
          ))
        | Variant ("Pexp_tuple", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Pexp_tuple (x1))
          )
        | Variant ("Pexp_construct", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Pexp_construct (x1, x2))
          ))
        | Variant ("Pexp_variant", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Pexp_variant (x1, x2))
          ))
        | Variant ("Pexp_record", Tuple [x1; x2]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Pexp_record (x1, x2))
          ))
        | Variant ("Pexp_field", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pexp_field (x1, x2))
          ))
        | Variant ("Pexp_setfield", Tuple [x1; x2; x3]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Some (Pexp_setfield (x1, x2, x3))
          )))
        | Variant ("Pexp_array", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Pexp_array (x1))
          )
        | Variant ("Pexp_ifthenelse", Tuple [x1; x2; x3]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) x3) ~f:(fun x3 ->
                Some (Pexp_ifthenelse (x1, x2, x3))
          )))
        | Variant ("Pexp_sequence", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pexp_sequence (x1, x2))
          ))
        | Variant ("Pexp_while", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pexp_while (x1, x2))
          ))
        | Variant ("Pexp_for", Tuple [x1; x2; x3; x4; x5]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Optional.bind (Versioned_value.to_ast x4) ~f:(fun x4 ->
                  Optional.bind (Versioned_value.to_ast x5) ~f:(fun x5 ->
                    Some (Pexp_for (x1, x2, x3, x4, x5))
          )))))
        | Variant ("Pexp_constraint", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pexp_constraint (x1, x2))
          ))
        | Variant ("Pexp_coerce", Tuple [x1; x2; x3]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Some (Pexp_coerce (x1, x2, x3))
          )))
        | Variant ("Pexp_send", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pexp_send (x1, x2))
          ))
        | Variant ("Pexp_new", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pexp_new (x1))
          )
        | Variant ("Pexp_setinstvar", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pexp_setinstvar (x1, x2))
          ))
        | Variant ("Pexp_override", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Pexp_override (x1))
          )
        | Variant ("Pexp_letmodule", Tuple [x1; x2; x3]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Some (Pexp_letmodule (x1, x2, x3))
          )))
        | Variant ("Pexp_letexception", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pexp_letexception (x1, x2))
          ))
        | Variant ("Pexp_assert", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pexp_assert (x1))
          )
        | Variant ("Pexp_lazy", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pexp_lazy (x1))
          )
        | Variant ("Pexp_poly", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Pexp_poly (x1, x2))
          ))
        | Variant ("Pexp_object", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pexp_object (x1))
          )
        | Variant ("Pexp_newtype", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pexp_newtype (x1, x2))
          ))
        | Variant ("Pexp_pack", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pexp_pack (x1))
          )
        | Variant ("Pexp_open", Tuple [x1; x2; x3]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Some (Pexp_open (x1, x2, x3))
          )))
        | Variant ("Pexp_extension", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pexp_extension (x1))
          )
        | Variant ("Pexp_unreachable", Empty) -> Some Pexp_unreachable
        )
      | _ -> None
  end

  module Override_expression = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t * Versioned_ast.t)
    end

    let create x =
      Versioned_ast.create ~version
        { name = "override_expression"
        ; data =
            (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "override_expression"; data = (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "override_expression"; data = data } ->
        (Versioned_value.to_tuple2 ~f1:Versioned_value.to_ast ~f2:Versioned_value.to_ast) data
      | _ -> None
  end

  module Record_field_expression = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t * Versioned_ast.t)
    end

    let create x =
      Versioned_ast.create ~version
        { name = "record_field_expression"
        ; data =
            (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "record_field_expression"; data = (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "record_field_expression"; data = data } ->
        (Versioned_value.to_tuple2 ~f1:Versioned_value.to_ast ~f2:Versioned_value.to_ast) data
      | _ -> None
  end

  module Apply_arg = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t * Versioned_ast.t)
    end

    let create x =
      Versioned_ast.create ~version
        { name = "apply_arg"
        ; data =
            (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "apply_arg"; data = (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "apply_arg"; data = data } ->
        (Versioned_value.to_tuple2 ~f1:Versioned_value.to_ast ~f2:Versioned_value.to_ast) data
      | _ -> None
  end

  module Case = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pc_lhs : Versioned_ast.t; pc_guard : Versioned_ast.t option; pc_rhs : Versioned_ast.t }
    end

    let create ~pc_lhs ~pc_guard ~pc_rhs =
      Versioned_ast.create ~version
        { name = "case"
        ; data =
            Record [("pc_lhs", Versioned_value.of_ast pc_lhs); ("pc_guard", (Versioned_value.of_option ~f:Versioned_value.of_ast) pc_guard); ("pc_rhs", Versioned_value.of_ast pc_rhs)]
        }

    let of_concrete ({ pc_lhs; pc_guard; pc_rhs } : Concrete.t) : t =
      { name = "case"; data = Record [("pc_lhs", Versioned_value.of_ast pc_lhs); ("pc_guard", (Versioned_value.of_option ~f:Versioned_value.of_ast) pc_guard); ("pc_rhs", Versioned_value.of_ast pc_rhs)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "case"; data = Record ["pc_lhs", pc_lhs; "pc_guard", pc_guard; "pc_rhs", pc_rhs] } ->
        Optional.bind (Versioned_value.to_ast pc_lhs) ~f:(fun pc_lhs ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) pc_guard) ~f:(fun pc_guard ->
            Optional.bind (Versioned_value.to_ast pc_rhs) ~f:(fun pc_rhs ->
              Some { pc_lhs; pc_guard; pc_rhs }
        )))
      | _ -> None
  end

  module Value_description = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pval_name : Versioned_ast.t; pval_type : Versioned_ast.t; pval_prim : string list; pval_attributes : Versioned_ast.t; pval_loc : Location.t }
    end

    let create ~pval_name ~pval_type ~pval_prim ~pval_attributes ~pval_loc =
      Versioned_ast.create ~version
        { name = "value_description"
        ; data =
            Record [("pval_name", Versioned_value.of_ast pval_name); ("pval_type", Versioned_value.of_ast pval_type); ("pval_prim", (Versioned_value.of_list ~f:Versioned_value.of_string) pval_prim); ("pval_attributes", Versioned_value.of_ast pval_attributes); ("pval_loc", Versioned_value.of_location pval_loc)]
        }

    let of_concrete ({ pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : Concrete.t) : t =
      { name = "value_description"; data = Record [("pval_name", Versioned_value.of_ast pval_name); ("pval_type", Versioned_value.of_ast pval_type); ("pval_prim", (Versioned_value.of_list ~f:Versioned_value.of_string) pval_prim); ("pval_attributes", Versioned_value.of_ast pval_attributes); ("pval_loc", Versioned_value.of_location pval_loc)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "value_description"; data = Record ["pval_name", pval_name; "pval_type", pval_type; "pval_prim", pval_prim; "pval_attributes", pval_attributes; "pval_loc", pval_loc] } ->
        Optional.bind (Versioned_value.to_ast pval_name) ~f:(fun pval_name ->
          Optional.bind (Versioned_value.to_ast pval_type) ~f:(fun pval_type ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_string) pval_prim) ~f:(fun pval_prim ->
              Optional.bind (Versioned_value.to_ast pval_attributes) ~f:(fun pval_attributes ->
                Optional.bind (Versioned_value.to_location pval_loc) ~f:(fun pval_loc ->
                  Some { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }
        )))))
      | _ -> None
  end

  module Type_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { ptype_name : Versioned_ast.t; ptype_params : Versioned_ast.t list; ptype_cstrs : Versioned_ast.t list; ptype_kind : Versioned_ast.t; ptype_private : Versioned_ast.t; ptype_manifest : Versioned_ast.t option; ptype_attributes : Versioned_ast.t; ptype_loc : Location.t }
    end

    let create ~ptype_name ~ptype_params ~ptype_cstrs ~ptype_kind ~ptype_private ~ptype_manifest ~ptype_attributes ~ptype_loc =
      Versioned_ast.create ~version
        { name = "type_declaration"
        ; data =
            Record [("ptype_name", Versioned_value.of_ast ptype_name); ("ptype_params", (Versioned_value.of_list ~f:Versioned_value.of_ast) ptype_params); ("ptype_cstrs", (Versioned_value.of_list ~f:Versioned_value.of_ast) ptype_cstrs); ("ptype_kind", Versioned_value.of_ast ptype_kind); ("ptype_private", Versioned_value.of_ast ptype_private); ("ptype_manifest", (Versioned_value.of_option ~f:Versioned_value.of_ast) ptype_manifest); ("ptype_attributes", Versioned_value.of_ast ptype_attributes); ("ptype_loc", Versioned_value.of_location ptype_loc)]
        }

    let of_concrete ({ ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : Concrete.t) : t =
      { name = "type_declaration"; data = Record [("ptype_name", Versioned_value.of_ast ptype_name); ("ptype_params", (Versioned_value.of_list ~f:Versioned_value.of_ast) ptype_params); ("ptype_cstrs", (Versioned_value.of_list ~f:Versioned_value.of_ast) ptype_cstrs); ("ptype_kind", Versioned_value.of_ast ptype_kind); ("ptype_private", Versioned_value.of_ast ptype_private); ("ptype_manifest", (Versioned_value.of_option ~f:Versioned_value.of_ast) ptype_manifest); ("ptype_attributes", Versioned_value.of_ast ptype_attributes); ("ptype_loc", Versioned_value.of_location ptype_loc)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "type_declaration"; data = Record ["ptype_name", ptype_name; "ptype_params", ptype_params; "ptype_cstrs", ptype_cstrs; "ptype_kind", ptype_kind; "ptype_private", ptype_private; "ptype_manifest", ptype_manifest; "ptype_attributes", ptype_attributes; "ptype_loc", ptype_loc] } ->
        Optional.bind (Versioned_value.to_ast ptype_name) ~f:(fun ptype_name ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) ptype_params) ~f:(fun ptype_params ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) ptype_cstrs) ~f:(fun ptype_cstrs ->
              Optional.bind (Versioned_value.to_ast ptype_kind) ~f:(fun ptype_kind ->
                Optional.bind (Versioned_value.to_ast ptype_private) ~f:(fun ptype_private ->
                  Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) ptype_manifest) ~f:(fun ptype_manifest ->
                    Optional.bind (Versioned_value.to_ast ptype_attributes) ~f:(fun ptype_attributes ->
                      Optional.bind (Versioned_value.to_location ptype_loc) ~f:(fun ptype_loc ->
                        Some { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc }
        ))))))))
      | _ -> None
  end

  module Type_param = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t * Versioned_ast.t)
    end

    let create x =
      Versioned_ast.create ~version
        { name = "type_param"
        ; data =
            (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "type_param"; data = (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "type_param"; data = data } ->
        (Versioned_value.to_tuple2 ~f1:Versioned_value.to_ast ~f2:Versioned_value.to_ast) data
      | _ -> None
  end

  module Type_constraint = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t * Versioned_ast.t * Location.t)
    end

    let create x =
      Versioned_ast.create ~version
        { name = "type_constraint"
        ; data =
            (Versioned_value.of_tuple3 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast ~f3:Versioned_value.of_location) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "type_constraint"; data = (Versioned_value.of_tuple3 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast ~f3:Versioned_value.of_location) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "type_constraint"; data = data } ->
        (Versioned_value.to_tuple3 ~f1:Versioned_value.to_ast ~f2:Versioned_value.to_ast ~f3:Versioned_value.to_location) data
      | _ -> None
  end

  module Type_kind = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Ptype_abstract
        | Ptype_variant of Versioned_ast.t list
        | Ptype_record of Versioned_ast.t list
        | Ptype_open
    end

    let create_ptype_abstract =
      Versioned_ast.create ~version
        { name = "type_kind"
        ; data =
            Variant ("Ptype_abstract", Empty)
        }
    let create_ptype_variant x1 =
      Versioned_ast.create ~version
        { name = "type_kind"
        ; data =
            Variant ("Ptype_variant", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_ptype_record x1 =
      Versioned_ast.create ~version
        { name = "type_kind"
        ; data =
            Variant ("Ptype_record", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_ptype_open =
      Versioned_ast.create ~version
        { name = "type_kind"
        ; data =
            Variant ("Ptype_open", Empty)
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Ptype_abstract -> create_ptype_abstract
      | Ptype_variant (x1) ->
        create_ptype_variant x1
      | Ptype_record (x1) ->
        create_ptype_record x1
      | Ptype_open -> create_ptype_open

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "type_kind"; data = data } ->
        ( match data with
        | Variant ("Ptype_abstract", Empty) -> Some Ptype_abstract
        | Variant ("Ptype_variant", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Ptype_variant (x1))
          )
        | Variant ("Ptype_record", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Ptype_record (x1))
          )
        | Variant ("Ptype_open", Empty) -> Some Ptype_open
        )
      | _ -> None
  end

  module Label_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pld_name : Versioned_ast.t; pld_mutable : Versioned_ast.t; pld_type : Versioned_ast.t; pld_loc : Location.t; pld_attributes : Versioned_ast.t }
    end

    let create ~pld_name ~pld_mutable ~pld_type ~pld_loc ~pld_attributes =
      Versioned_ast.create ~version
        { name = "label_declaration"
        ; data =
            Record [("pld_name", Versioned_value.of_ast pld_name); ("pld_mutable", Versioned_value.of_ast pld_mutable); ("pld_type", Versioned_value.of_ast pld_type); ("pld_loc", Versioned_value.of_location pld_loc); ("pld_attributes", Versioned_value.of_ast pld_attributes)]
        }

    let of_concrete ({ pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : Concrete.t) : t =
      { name = "label_declaration"; data = Record [("pld_name", Versioned_value.of_ast pld_name); ("pld_mutable", Versioned_value.of_ast pld_mutable); ("pld_type", Versioned_value.of_ast pld_type); ("pld_loc", Versioned_value.of_location pld_loc); ("pld_attributes", Versioned_value.of_ast pld_attributes)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "label_declaration"; data = Record ["pld_name", pld_name; "pld_mutable", pld_mutable; "pld_type", pld_type; "pld_loc", pld_loc; "pld_attributes", pld_attributes] } ->
        Optional.bind (Versioned_value.to_ast pld_name) ~f:(fun pld_name ->
          Optional.bind (Versioned_value.to_ast pld_mutable) ~f:(fun pld_mutable ->
            Optional.bind (Versioned_value.to_ast pld_type) ~f:(fun pld_type ->
              Optional.bind (Versioned_value.to_location pld_loc) ~f:(fun pld_loc ->
                Optional.bind (Versioned_value.to_ast pld_attributes) ~f:(fun pld_attributes ->
                  Some { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }
        )))))
      | _ -> None
  end

  module Constructor_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pcd_name : Versioned_ast.t; pcd_args : Versioned_ast.t; pcd_res : Versioned_ast.t option; pcd_loc : Location.t; pcd_attributes : Versioned_ast.t }
    end

    let create ~pcd_name ~pcd_args ~pcd_res ~pcd_loc ~pcd_attributes =
      Versioned_ast.create ~version
        { name = "constructor_declaration"
        ; data =
            Record [("pcd_name", Versioned_value.of_ast pcd_name); ("pcd_args", Versioned_value.of_ast pcd_args); ("pcd_res", (Versioned_value.of_option ~f:Versioned_value.of_ast) pcd_res); ("pcd_loc", Versioned_value.of_location pcd_loc); ("pcd_attributes", Versioned_value.of_ast pcd_attributes)]
        }

    let of_concrete ({ pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : Concrete.t) : t =
      { name = "constructor_declaration"; data = Record [("pcd_name", Versioned_value.of_ast pcd_name); ("pcd_args", Versioned_value.of_ast pcd_args); ("pcd_res", (Versioned_value.of_option ~f:Versioned_value.of_ast) pcd_res); ("pcd_loc", Versioned_value.of_location pcd_loc); ("pcd_attributes", Versioned_value.of_ast pcd_attributes)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "constructor_declaration"; data = Record ["pcd_name", pcd_name; "pcd_args", pcd_args; "pcd_res", pcd_res; "pcd_loc", pcd_loc; "pcd_attributes", pcd_attributes] } ->
        Optional.bind (Versioned_value.to_ast pcd_name) ~f:(fun pcd_name ->
          Optional.bind (Versioned_value.to_ast pcd_args) ~f:(fun pcd_args ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) pcd_res) ~f:(fun pcd_res ->
              Optional.bind (Versioned_value.to_location pcd_loc) ~f:(fun pcd_loc ->
                Optional.bind (Versioned_value.to_ast pcd_attributes) ~f:(fun pcd_attributes ->
                  Some { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }
        )))))
      | _ -> None
  end

  module Constructor_arguments = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pcstr_tuple of Versioned_ast.t list
        | Pcstr_record of Versioned_ast.t list
    end

    let create_pcstr_tuple x1 =
      Versioned_ast.create ~version
        { name = "constructor_arguments"
        ; data =
            Variant ("Pcstr_tuple", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_pcstr_record x1 =
      Versioned_ast.create ~version
        { name = "constructor_arguments"
        ; data =
            Variant ("Pcstr_record", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Pcstr_tuple (x1) ->
        create_pcstr_tuple x1
      | Pcstr_record (x1) ->
        create_pcstr_record x1

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "constructor_arguments"; data = data } ->
        ( match data with
        | Variant ("Pcstr_tuple", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Pcstr_tuple (x1))
          )
        | Variant ("Pcstr_record", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Pcstr_record (x1))
          )
        )
      | _ -> None
  end

  module Type_extension = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { ptyext_path : Versioned_ast.t; ptyext_params : Versioned_ast.t list; ptyext_constructors : Versioned_ast.t list; ptyext_private : Versioned_ast.t; ptyext_attributes : Versioned_ast.t }
    end

    let create ~ptyext_path ~ptyext_params ~ptyext_constructors ~ptyext_private ~ptyext_attributes =
      Versioned_ast.create ~version
        { name = "type_extension"
        ; data =
            Record [("ptyext_path", Versioned_value.of_ast ptyext_path); ("ptyext_params", (Versioned_value.of_list ~f:Versioned_value.of_ast) ptyext_params); ("ptyext_constructors", (Versioned_value.of_list ~f:Versioned_value.of_ast) ptyext_constructors); ("ptyext_private", Versioned_value.of_ast ptyext_private); ("ptyext_attributes", Versioned_value.of_ast ptyext_attributes)]
        }

    let of_concrete ({ ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : Concrete.t) : t =
      { name = "type_extension"; data = Record [("ptyext_path", Versioned_value.of_ast ptyext_path); ("ptyext_params", (Versioned_value.of_list ~f:Versioned_value.of_ast) ptyext_params); ("ptyext_constructors", (Versioned_value.of_list ~f:Versioned_value.of_ast) ptyext_constructors); ("ptyext_private", Versioned_value.of_ast ptyext_private); ("ptyext_attributes", Versioned_value.of_ast ptyext_attributes)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "type_extension"; data = Record ["ptyext_path", ptyext_path; "ptyext_params", ptyext_params; "ptyext_constructors", ptyext_constructors; "ptyext_private", ptyext_private; "ptyext_attributes", ptyext_attributes] } ->
        Optional.bind (Versioned_value.to_ast ptyext_path) ~f:(fun ptyext_path ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) ptyext_params) ~f:(fun ptyext_params ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) ptyext_constructors) ~f:(fun ptyext_constructors ->
              Optional.bind (Versioned_value.to_ast ptyext_private) ~f:(fun ptyext_private ->
                Optional.bind (Versioned_value.to_ast ptyext_attributes) ~f:(fun ptyext_attributes ->
                  Some { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes }
        )))))
      | _ -> None
  end

  module Extension_constructor = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pext_name : Versioned_ast.t; pext_kind : Versioned_ast.t; pext_loc : Location.t; pext_attributes : Versioned_ast.t }
    end

    let create ~pext_name ~pext_kind ~pext_loc ~pext_attributes =
      Versioned_ast.create ~version
        { name = "extension_constructor"
        ; data =
            Record [("pext_name", Versioned_value.of_ast pext_name); ("pext_kind", Versioned_value.of_ast pext_kind); ("pext_loc", Versioned_value.of_location pext_loc); ("pext_attributes", Versioned_value.of_ast pext_attributes)]
        }

    let of_concrete ({ pext_name; pext_kind; pext_loc; pext_attributes } : Concrete.t) : t =
      { name = "extension_constructor"; data = Record [("pext_name", Versioned_value.of_ast pext_name); ("pext_kind", Versioned_value.of_ast pext_kind); ("pext_loc", Versioned_value.of_location pext_loc); ("pext_attributes", Versioned_value.of_ast pext_attributes)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "extension_constructor"; data = Record ["pext_name", pext_name; "pext_kind", pext_kind; "pext_loc", pext_loc; "pext_attributes", pext_attributes] } ->
        Optional.bind (Versioned_value.to_ast pext_name) ~f:(fun pext_name ->
          Optional.bind (Versioned_value.to_ast pext_kind) ~f:(fun pext_kind ->
            Optional.bind (Versioned_value.to_location pext_loc) ~f:(fun pext_loc ->
              Optional.bind (Versioned_value.to_ast pext_attributes) ~f:(fun pext_attributes ->
                Some { pext_name; pext_kind; pext_loc; pext_attributes }
        ))))
      | _ -> None
  end

  module Extension_constructor_kind = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pext_decl of Versioned_ast.t * Versioned_ast.t option
        | Pext_rebind of Versioned_ast.t
    end

    let create_pext_decl x1 x2 =
      Versioned_ast.create ~version
        { name = "extension_constructor_kind"
        ; data =
            Variant ("Pext_decl", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_option ~f:Versioned_value.of_ast) x2])
        }
    let create_pext_rebind x1 =
      Versioned_ast.create ~version
        { name = "extension_constructor_kind"
        ; data =
            Variant ("Pext_rebind", Tuple [Versioned_value.of_ast x1])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Pext_decl (x1, x2) ->
        create_pext_decl x1 x2
      | Pext_rebind (x1) ->
        create_pext_rebind x1

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "extension_constructor_kind"; data = data } ->
        ( match data with
        | Variant ("Pext_decl", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Pext_decl (x1, x2))
          ))
        | Variant ("Pext_rebind", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pext_rebind (x1))
          )
        )
      | _ -> None
  end

  module Class_type = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pcty_desc : Versioned_ast.t; pcty_loc : Location.t; pcty_attributes : Versioned_ast.t }
    end

    let create ~pcty_desc ~pcty_loc ~pcty_attributes =
      Versioned_ast.create ~version
        { name = "class_type"
        ; data =
            Record [("pcty_desc", Versioned_value.of_ast pcty_desc); ("pcty_loc", Versioned_value.of_location pcty_loc); ("pcty_attributes", Versioned_value.of_ast pcty_attributes)]
        }

    let of_concrete ({ pcty_desc; pcty_loc; pcty_attributes } : Concrete.t) : t =
      { name = "class_type"; data = Record [("pcty_desc", Versioned_value.of_ast pcty_desc); ("pcty_loc", Versioned_value.of_location pcty_loc); ("pcty_attributes", Versioned_value.of_ast pcty_attributes)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_type"; data = Record ["pcty_desc", pcty_desc; "pcty_loc", pcty_loc; "pcty_attributes", pcty_attributes] } ->
        Optional.bind (Versioned_value.to_ast pcty_desc) ~f:(fun pcty_desc ->
          Optional.bind (Versioned_value.to_location pcty_loc) ~f:(fun pcty_loc ->
            Optional.bind (Versioned_value.to_ast pcty_attributes) ~f:(fun pcty_attributes ->
              Some { pcty_desc; pcty_loc; pcty_attributes }
        )))
      | _ -> None
  end

  module Class_type_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pcty_constr of Versioned_ast.t * Versioned_ast.t list
        | Pcty_signature of Versioned_ast.t
        | Pcty_arrow of Versioned_ast.t * Versioned_ast.t * Versioned_ast.t
        | Pcty_extension of Versioned_ast.t
        | Pcty_open of Versioned_ast.t * Versioned_ast.t * Versioned_ast.t
    end

    let create_pcty_constr x1 x2 =
      Versioned_ast.create ~version
        { name = "class_type_desc"
        ; data =
            Variant ("Pcty_constr", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_list ~f:Versioned_value.of_ast) x2])
        }
    let create_pcty_signature x1 =
      Versioned_ast.create ~version
        { name = "class_type_desc"
        ; data =
            Variant ("Pcty_signature", Tuple [Versioned_value.of_ast x1])
        }
    let create_pcty_arrow x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "class_type_desc"
        ; data =
            Variant ("Pcty_arrow", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2; Versioned_value.of_ast x3])
        }
    let create_pcty_extension x1 =
      Versioned_ast.create ~version
        { name = "class_type_desc"
        ; data =
            Variant ("Pcty_extension", Tuple [Versioned_value.of_ast x1])
        }
    let create_pcty_open x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "class_type_desc"
        ; data =
            Variant ("Pcty_open", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2; Versioned_value.of_ast x3])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Pcty_constr (x1, x2) ->
        create_pcty_constr x1 x2
      | Pcty_signature (x1) ->
        create_pcty_signature x1
      | Pcty_arrow (x1, x2, x3) ->
        create_pcty_arrow x1 x2 x3
      | Pcty_extension (x1) ->
        create_pcty_extension x1
      | Pcty_open (x1, x2, x3) ->
        create_pcty_open x1 x2 x3

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_type_desc"; data = data } ->
        ( match data with
        | Variant ("Pcty_constr", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Pcty_constr (x1, x2))
          ))
        | Variant ("Pcty_signature", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pcty_signature (x1))
          )
        | Variant ("Pcty_arrow", Tuple [x1; x2; x3]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Some (Pcty_arrow (x1, x2, x3))
          )))
        | Variant ("Pcty_extension", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pcty_extension (x1))
          )
        | Variant ("Pcty_open", Tuple [x1; x2; x3]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Some (Pcty_open (x1, x2, x3))
          )))
        )
      | _ -> None
  end

  module Class_signature = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pcsig_self : Versioned_ast.t; pcsig_fields : Versioned_ast.t list }
    end

    let create ~pcsig_self ~pcsig_fields =
      Versioned_ast.create ~version
        { name = "class_signature"
        ; data =
            Record [("pcsig_self", Versioned_value.of_ast pcsig_self); ("pcsig_fields", (Versioned_value.of_list ~f:Versioned_value.of_ast) pcsig_fields)]
        }

    let of_concrete ({ pcsig_self; pcsig_fields } : Concrete.t) : t =
      { name = "class_signature"; data = Record [("pcsig_self", Versioned_value.of_ast pcsig_self); ("pcsig_fields", (Versioned_value.of_list ~f:Versioned_value.of_ast) pcsig_fields)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_signature"; data = Record ["pcsig_self", pcsig_self; "pcsig_fields", pcsig_fields] } ->
        Optional.bind (Versioned_value.to_ast pcsig_self) ~f:(fun pcsig_self ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) pcsig_fields) ~f:(fun pcsig_fields ->
            Some { pcsig_self; pcsig_fields }
        ))
      | _ -> None
  end

  module Class_type_field = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pctf_desc : Versioned_ast.t; pctf_loc : Location.t; pctf_attributes : Versioned_ast.t }
    end

    let create ~pctf_desc ~pctf_loc ~pctf_attributes =
      Versioned_ast.create ~version
        { name = "class_type_field"
        ; data =
            Record [("pctf_desc", Versioned_value.of_ast pctf_desc); ("pctf_loc", Versioned_value.of_location pctf_loc); ("pctf_attributes", Versioned_value.of_ast pctf_attributes)]
        }

    let of_concrete ({ pctf_desc; pctf_loc; pctf_attributes } : Concrete.t) : t =
      { name = "class_type_field"; data = Record [("pctf_desc", Versioned_value.of_ast pctf_desc); ("pctf_loc", Versioned_value.of_location pctf_loc); ("pctf_attributes", Versioned_value.of_ast pctf_attributes)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_type_field"; data = Record ["pctf_desc", pctf_desc; "pctf_loc", pctf_loc; "pctf_attributes", pctf_attributes] } ->
        Optional.bind (Versioned_value.to_ast pctf_desc) ~f:(fun pctf_desc ->
          Optional.bind (Versioned_value.to_location pctf_loc) ~f:(fun pctf_loc ->
            Optional.bind (Versioned_value.to_ast pctf_attributes) ~f:(fun pctf_attributes ->
              Some { pctf_desc; pctf_loc; pctf_attributes }
        )))
      | _ -> None
  end

  module Class_type_field_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pctf_inherit of Versioned_ast.t
        | Pctf_val of Versioned_ast.t
        | Pctf_method of Versioned_ast.t
        | Pctf_constraint of Versioned_ast.t
        | Pctf_attribute of Versioned_ast.t
        | Pctf_extension of Versioned_ast.t
    end

    let create_pctf_inherit x1 =
      Versioned_ast.create ~version
        { name = "class_type_field_desc"
        ; data =
            Variant ("Pctf_inherit", Tuple [Versioned_value.of_ast x1])
        }
    let create_pctf_val x1 =
      Versioned_ast.create ~version
        { name = "class_type_field_desc"
        ; data =
            Variant ("Pctf_val", Tuple [Versioned_value.of_ast x1])
        }
    let create_pctf_method x1 =
      Versioned_ast.create ~version
        { name = "class_type_field_desc"
        ; data =
            Variant ("Pctf_method", Tuple [Versioned_value.of_ast x1])
        }
    let create_pctf_constraint x1 =
      Versioned_ast.create ~version
        { name = "class_type_field_desc"
        ; data =
            Variant ("Pctf_constraint", Tuple [Versioned_value.of_ast x1])
        }
    let create_pctf_attribute x1 =
      Versioned_ast.create ~version
        { name = "class_type_field_desc"
        ; data =
            Variant ("Pctf_attribute", Tuple [Versioned_value.of_ast x1])
        }
    let create_pctf_extension x1 =
      Versioned_ast.create ~version
        { name = "class_type_field_desc"
        ; data =
            Variant ("Pctf_extension", Tuple [Versioned_value.of_ast x1])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Pctf_inherit (x1) ->
        create_pctf_inherit x1
      | Pctf_val (x1) ->
        create_pctf_val x1
      | Pctf_method (x1) ->
        create_pctf_method x1
      | Pctf_constraint (x1) ->
        create_pctf_constraint x1
      | Pctf_attribute (x1) ->
        create_pctf_attribute x1
      | Pctf_extension (x1) ->
        create_pctf_extension x1

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_type_field_desc"; data = data } ->
        ( match data with
        | Variant ("Pctf_inherit", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pctf_inherit (x1))
          )
        | Variant ("Pctf_val", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pctf_val (x1))
          )
        | Variant ("Pctf_method", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pctf_method (x1))
          )
        | Variant ("Pctf_constraint", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pctf_constraint (x1))
          )
        | Variant ("Pctf_attribute", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pctf_attribute (x1))
          )
        | Variant ("Pctf_extension", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pctf_extension (x1))
          )
        )
      | _ -> None
  end

  module Class_type_value_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t * Versioned_ast.t * Versioned_ast.t * Versioned_ast.t)
    end

    let create x =
      Versioned_ast.create ~version
        { name = "class_type_value_desc"
        ; data =
            (Versioned_value.of_tuple4 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast ~f3:Versioned_value.of_ast ~f4:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "class_type_value_desc"; data = (Versioned_value.of_tuple4 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast ~f3:Versioned_value.of_ast ~f4:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_type_value_desc"; data = data } ->
        (Versioned_value.to_tuple4 ~f1:Versioned_value.to_ast ~f2:Versioned_value.to_ast ~f3:Versioned_value.to_ast ~f4:Versioned_value.to_ast) data
      | _ -> None
  end

  module Class_type_method_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t * Versioned_ast.t * Versioned_ast.t * Versioned_ast.t)
    end

    let create x =
      Versioned_ast.create ~version
        { name = "class_type_method_desc"
        ; data =
            (Versioned_value.of_tuple4 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast ~f3:Versioned_value.of_ast ~f4:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "class_type_method_desc"; data = (Versioned_value.of_tuple4 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast ~f3:Versioned_value.of_ast ~f4:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_type_method_desc"; data = data } ->
        (Versioned_value.to_tuple4 ~f1:Versioned_value.to_ast ~f2:Versioned_value.to_ast ~f3:Versioned_value.to_ast ~f4:Versioned_value.to_ast) data
      | _ -> None
  end

  module Class_type_constraint = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t * Versioned_ast.t)
    end

    let create x =
      Versioned_ast.create ~version
        { name = "class_type_constraint"
        ; data =
            (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "class_type_constraint"; data = (Versioned_value.of_tuple2 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_type_constraint"; data = data } ->
        (Versioned_value.to_tuple2 ~f1:Versioned_value.to_ast ~f2:Versioned_value.to_ast) data
      | _ -> None
  end

  module Class_infos = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type 'a t = { pci_virt : Versioned_ast.t; pci_params : Versioned_ast.t list; pci_name : Versioned_ast.t; pci_expr : 'a; pci_loc : Location.t; pci_attributes : Versioned_ast.t }
    end

    let create ~pci_virt ~pci_params ~pci_name ~pci_expr ~pci_loc ~pci_attributes =
      Versioned_ast.create ~version
        { name = "class_infos"
        ; data =
            Record [("pci_virt", Versioned_value.of_ast pci_virt); ("pci_params", (Versioned_value.of_list ~f:Versioned_value.of_ast) pci_params); ("pci_name", Versioned_value.of_ast pci_name); ("pci_expr", a_of_concrete pci_expr); ("pci_loc", Versioned_value.of_location pci_loc); ("pci_attributes", Versioned_value.of_ast pci_attributes)]
        }

    let of_concrete ({ pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : 'a Concrete.t) : 'a t =
      { name = "class_infos"; data = Record [("pci_virt", Versioned_value.of_ast pci_virt); ("pci_params", (Versioned_value.of_list ~f:Versioned_value.of_ast) pci_params); ("pci_name", Versioned_value.of_ast pci_name); ("pci_expr", a_of_concrete pci_expr); ("pci_loc", Versioned_value.of_location pci_loc); ("pci_attributes", Versioned_value.of_ast pci_attributes)] }

    let to_concrete t : 'a Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_infos"; data = Record ["pci_virt", pci_virt; "pci_params", pci_params; "pci_name", pci_name; "pci_expr", pci_expr; "pci_loc", pci_loc; "pci_attributes", pci_attributes] } ->
        Optional.bind (Versioned_value.to_ast pci_virt) ~f:(fun pci_virt ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) pci_params) ~f:(fun pci_params ->
            Optional.bind (Versioned_value.to_ast pci_name) ~f:(fun pci_name ->
              Optional.bind (a_to_concrete pci_expr) ~f:(fun pci_expr ->
                Optional.bind (Versioned_value.to_location pci_loc) ~f:(fun pci_loc ->
                  Optional.bind (Versioned_value.to_ast pci_attributes) ~f:(fun pci_attributes ->
                    Some { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
        ))))))
      | _ -> None
  end

  module Class_description = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t) Class_infos.t
    end

    let create x =
      Versioned_ast.create ~version
        { name = "class_description"
        ; data =
            (class_infos_of_concrete Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "class_description"; data = (class_infos_of_concrete Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_description"; data = data } ->
        (class_infos_to_concrete Versioned_value.to_ast) data
      | _ -> None
  end

  module Class_type_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t) Class_infos.t
    end

    let create x =
      Versioned_ast.create ~version
        { name = "class_type_declaration"
        ; data =
            (class_infos_of_concrete Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "class_type_declaration"; data = (class_infos_of_concrete Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_type_declaration"; data = data } ->
        (class_infos_to_concrete Versioned_value.to_ast) data
      | _ -> None
  end

  module Class_expr = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pcl_desc : Versioned_ast.t; pcl_loc : Location.t; pcl_attributes : Versioned_ast.t }
    end

    let create ~pcl_desc ~pcl_loc ~pcl_attributes =
      Versioned_ast.create ~version
        { name = "class_expr"
        ; data =
            Record [("pcl_desc", Versioned_value.of_ast pcl_desc); ("pcl_loc", Versioned_value.of_location pcl_loc); ("pcl_attributes", Versioned_value.of_ast pcl_attributes)]
        }

    let of_concrete ({ pcl_desc; pcl_loc; pcl_attributes } : Concrete.t) : t =
      { name = "class_expr"; data = Record [("pcl_desc", Versioned_value.of_ast pcl_desc); ("pcl_loc", Versioned_value.of_location pcl_loc); ("pcl_attributes", Versioned_value.of_ast pcl_attributes)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_expr"; data = Record ["pcl_desc", pcl_desc; "pcl_loc", pcl_loc; "pcl_attributes", pcl_attributes] } ->
        Optional.bind (Versioned_value.to_ast pcl_desc) ~f:(fun pcl_desc ->
          Optional.bind (Versioned_value.to_location pcl_loc) ~f:(fun pcl_loc ->
            Optional.bind (Versioned_value.to_ast pcl_attributes) ~f:(fun pcl_attributes ->
              Some { pcl_desc; pcl_loc; pcl_attributes }
        )))
      | _ -> None
  end

  module Class_expr_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pcl_constr of Versioned_ast.t * Versioned_ast.t list
        | Pcl_structure of Versioned_ast.t
        | Pcl_fun of Versioned_ast.t * Versioned_ast.t option * Versioned_ast.t * Versioned_ast.t
        | Pcl_apply of Versioned_ast.t * Versioned_ast.t list
        | Pcl_let of Versioned_ast.t * Versioned_ast.t list * Versioned_ast.t
        | Pcl_constraint of Versioned_ast.t * Versioned_ast.t
        | Pcl_extension of Versioned_ast.t
        | Pcl_open of Versioned_ast.t * Versioned_ast.t * Versioned_ast.t
    end

    let create_pcl_constr x1 x2 =
      Versioned_ast.create ~version
        { name = "class_expr_desc"
        ; data =
            Variant ("Pcl_constr", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_list ~f:Versioned_value.of_ast) x2])
        }
    let create_pcl_structure x1 =
      Versioned_ast.create ~version
        { name = "class_expr_desc"
        ; data =
            Variant ("Pcl_structure", Tuple [Versioned_value.of_ast x1])
        }
    let create_pcl_fun x1 x2 x3 x4 =
      Versioned_ast.create ~version
        { name = "class_expr_desc"
        ; data =
            Variant ("Pcl_fun", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_option ~f:Versioned_value.of_ast) x2; Versioned_value.of_ast x3; Versioned_value.of_ast x4])
        }
    let create_pcl_apply x1 x2 =
      Versioned_ast.create ~version
        { name = "class_expr_desc"
        ; data =
            Variant ("Pcl_apply", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_list ~f:Versioned_value.of_ast) x2])
        }
    let create_pcl_let x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "class_expr_desc"
        ; data =
            Variant ("Pcl_let", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_list ~f:Versioned_value.of_ast) x2; Versioned_value.of_ast x3])
        }
    let create_pcl_constraint x1 x2 =
      Versioned_ast.create ~version
        { name = "class_expr_desc"
        ; data =
            Variant ("Pcl_constraint", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_pcl_extension x1 =
      Versioned_ast.create ~version
        { name = "class_expr_desc"
        ; data =
            Variant ("Pcl_extension", Tuple [Versioned_value.of_ast x1])
        }
    let create_pcl_open x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "class_expr_desc"
        ; data =
            Variant ("Pcl_open", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2; Versioned_value.of_ast x3])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Pcl_constr (x1, x2) ->
        create_pcl_constr x1 x2
      | Pcl_structure (x1) ->
        create_pcl_structure x1
      | Pcl_fun (x1, x2, x3, x4) ->
        create_pcl_fun x1 x2 x3 x4
      | Pcl_apply (x1, x2) ->
        create_pcl_apply x1 x2
      | Pcl_let (x1, x2, x3) ->
        create_pcl_let x1 x2 x3
      | Pcl_constraint (x1, x2) ->
        create_pcl_constraint x1 x2
      | Pcl_extension (x1) ->
        create_pcl_extension x1
      | Pcl_open (x1, x2, x3) ->
        create_pcl_open x1 x2 x3

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_expr_desc"; data = data } ->
        ( match data with
        | Variant ("Pcl_constr", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Pcl_constr (x1, x2))
          ))
        | Variant ("Pcl_structure", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pcl_structure (x1))
          )
        | Variant ("Pcl_fun", Tuple [x1; x2; x3; x4]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Optional.bind (Versioned_value.to_ast x4) ~f:(fun x4 ->
                  Some (Pcl_fun (x1, x2, x3, x4))
          ))))
        | Variant ("Pcl_apply", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Pcl_apply (x1, x2))
          ))
        | Variant ("Pcl_let", Tuple [x1; x2; x3]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Some (Pcl_let (x1, x2, x3))
          )))
        | Variant ("Pcl_constraint", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pcl_constraint (x1, x2))
          ))
        | Variant ("Pcl_extension", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pcl_extension (x1))
          )
        | Variant ("Pcl_open", Tuple [x1; x2; x3]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Some (Pcl_open (x1, x2, x3))
          )))
        )
      | _ -> None
  end

  module Class_structure = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pcstr_self : Versioned_ast.t; pcstr_fields : Versioned_ast.t list }
    end

    let create ~pcstr_self ~pcstr_fields =
      Versioned_ast.create ~version
        { name = "class_structure"
        ; data =
            Record [("pcstr_self", Versioned_value.of_ast pcstr_self); ("pcstr_fields", (Versioned_value.of_list ~f:Versioned_value.of_ast) pcstr_fields)]
        }

    let of_concrete ({ pcstr_self; pcstr_fields } : Concrete.t) : t =
      { name = "class_structure"; data = Record [("pcstr_self", Versioned_value.of_ast pcstr_self); ("pcstr_fields", (Versioned_value.of_list ~f:Versioned_value.of_ast) pcstr_fields)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_structure"; data = Record ["pcstr_self", pcstr_self; "pcstr_fields", pcstr_fields] } ->
        Optional.bind (Versioned_value.to_ast pcstr_self) ~f:(fun pcstr_self ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) pcstr_fields) ~f:(fun pcstr_fields ->
            Some { pcstr_self; pcstr_fields }
        ))
      | _ -> None
  end

  module Class_field = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pcf_desc : Versioned_ast.t; pcf_loc : Location.t; pcf_attributes : Versioned_ast.t }
    end

    let create ~pcf_desc ~pcf_loc ~pcf_attributes =
      Versioned_ast.create ~version
        { name = "class_field"
        ; data =
            Record [("pcf_desc", Versioned_value.of_ast pcf_desc); ("pcf_loc", Versioned_value.of_location pcf_loc); ("pcf_attributes", Versioned_value.of_ast pcf_attributes)]
        }

    let of_concrete ({ pcf_desc; pcf_loc; pcf_attributes } : Concrete.t) : t =
      { name = "class_field"; data = Record [("pcf_desc", Versioned_value.of_ast pcf_desc); ("pcf_loc", Versioned_value.of_location pcf_loc); ("pcf_attributes", Versioned_value.of_ast pcf_attributes)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_field"; data = Record ["pcf_desc", pcf_desc; "pcf_loc", pcf_loc; "pcf_attributes", pcf_attributes] } ->
        Optional.bind (Versioned_value.to_ast pcf_desc) ~f:(fun pcf_desc ->
          Optional.bind (Versioned_value.to_location pcf_loc) ~f:(fun pcf_loc ->
            Optional.bind (Versioned_value.to_ast pcf_attributes) ~f:(fun pcf_attributes ->
              Some { pcf_desc; pcf_loc; pcf_attributes }
        )))
      | _ -> None
  end

  module Class_field_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pcf_inherit of Versioned_ast.t * Versioned_ast.t * Versioned_ast.t option
        | Pcf_val of Versioned_ast.t
        | Pcf_method of Versioned_ast.t
        | Pcf_constraint of Versioned_ast.t
        | Pcf_initializer of Versioned_ast.t
        | Pcf_attribute of Versioned_ast.t
        | Pcf_extension of Versioned_ast.t
    end

    let create_pcf_inherit x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "class_field_desc"
        ; data =
            Variant ("Pcf_inherit", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2; (Versioned_value.of_option ~f:Versioned_value.of_ast) x3])
        }
    let create_pcf_val x1 =
      Versioned_ast.create ~version
        { name = "class_field_desc"
        ; data =
            Variant ("Pcf_val", Tuple [Versioned_value.of_ast x1])
        }
    let create_pcf_method x1 =
      Versioned_ast.create ~version
        { name = "class_field_desc"
        ; data =
            Variant ("Pcf_method", Tuple [Versioned_value.of_ast x1])
        }
    let create_pcf_constraint x1 =
      Versioned_ast.create ~version
        { name = "class_field_desc"
        ; data =
            Variant ("Pcf_constraint", Tuple [Versioned_value.of_ast x1])
        }
    let create_pcf_initializer x1 =
      Versioned_ast.create ~version
        { name = "class_field_desc"
        ; data =
            Variant ("Pcf_initializer", Tuple [Versioned_value.of_ast x1])
        }
    let create_pcf_attribute x1 =
      Versioned_ast.create ~version
        { name = "class_field_desc"
        ; data =
            Variant ("Pcf_attribute", Tuple [Versioned_value.of_ast x1])
        }
    let create_pcf_extension x1 =
      Versioned_ast.create ~version
        { name = "class_field_desc"
        ; data =
            Variant ("Pcf_extension", Tuple [Versioned_value.of_ast x1])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Pcf_inherit (x1, x2, x3) ->
        create_pcf_inherit x1 x2 x3
      | Pcf_val (x1) ->
        create_pcf_val x1
      | Pcf_method (x1) ->
        create_pcf_method x1
      | Pcf_constraint (x1) ->
        create_pcf_constraint x1
      | Pcf_initializer (x1) ->
        create_pcf_initializer x1
      | Pcf_attribute (x1) ->
        create_pcf_attribute x1
      | Pcf_extension (x1) ->
        create_pcf_extension x1

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_field_desc"; data = data } ->
        ( match data with
        | Variant ("Pcf_inherit", Tuple [x1; x2; x3]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) x3) ~f:(fun x3 ->
                Some (Pcf_inherit (x1, x2, x3))
          )))
        | Variant ("Pcf_val", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pcf_val (x1))
          )
        | Variant ("Pcf_method", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pcf_method (x1))
          )
        | Variant ("Pcf_constraint", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pcf_constraint (x1))
          )
        | Variant ("Pcf_initializer", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pcf_initializer (x1))
          )
        | Variant ("Pcf_attribute", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pcf_attribute (x1))
          )
        | Variant ("Pcf_extension", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pcf_extension (x1))
          )
        )
      | _ -> None
  end

  module Class_value_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t * Versioned_ast.t * Versioned_ast.t)
    end

    let create x =
      Versioned_ast.create ~version
        { name = "class_value_desc"
        ; data =
            (Versioned_value.of_tuple3 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast ~f3:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "class_value_desc"; data = (Versioned_value.of_tuple3 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast ~f3:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_value_desc"; data = data } ->
        (Versioned_value.to_tuple3 ~f1:Versioned_value.to_ast ~f2:Versioned_value.to_ast ~f3:Versioned_value.to_ast) data
      | _ -> None
  end

  module Class_method_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t * Versioned_ast.t * Versioned_ast.t)
    end

    let create x =
      Versioned_ast.create ~version
        { name = "class_method_desc"
        ; data =
            (Versioned_value.of_tuple3 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast ~f3:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "class_method_desc"; data = (Versioned_value.of_tuple3 ~f1:Versioned_value.of_ast ~f2:Versioned_value.of_ast ~f3:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_method_desc"; data = data } ->
        (Versioned_value.to_tuple3 ~f1:Versioned_value.to_ast ~f2:Versioned_value.to_ast ~f3:Versioned_value.to_ast) data
      | _ -> None
  end

  module Class_field_kind = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Cfk_virtual of Versioned_ast.t
        | Cfk_concrete of Versioned_ast.t * Versioned_ast.t
    end

    let create_cfk_virtual x1 =
      Versioned_ast.create ~version
        { name = "class_field_kind"
        ; data =
            Variant ("Cfk_virtual", Tuple [Versioned_value.of_ast x1])
        }
    let create_cfk_concrete x1 x2 =
      Versioned_ast.create ~version
        { name = "class_field_kind"
        ; data =
            Variant ("Cfk_concrete", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Cfk_virtual (x1) ->
        create_cfk_virtual x1
      | Cfk_concrete (x1, x2) ->
        create_cfk_concrete x1 x2

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_field_kind"; data = data } ->
        ( match data with
        | Variant ("Cfk_virtual", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Cfk_virtual (x1))
          )
        | Variant ("Cfk_concrete", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Cfk_concrete (x1, x2))
          ))
        )
      | _ -> None
  end

  module Class_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t) Class_infos.t
    end

    let create x =
      Versioned_ast.create ~version
        { name = "class_declaration"
        ; data =
            (class_infos_of_concrete Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "class_declaration"; data = (class_infos_of_concrete Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "class_declaration"; data = data } ->
        (class_infos_to_concrete Versioned_value.to_ast) data
      | _ -> None
  end

  module Module_type = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pmty_desc : Versioned_ast.t; pmty_loc : Location.t; pmty_attributes : Versioned_ast.t }
    end

    let create ~pmty_desc ~pmty_loc ~pmty_attributes =
      Versioned_ast.create ~version
        { name = "module_type"
        ; data =
            Record [("pmty_desc", Versioned_value.of_ast pmty_desc); ("pmty_loc", Versioned_value.of_location pmty_loc); ("pmty_attributes", Versioned_value.of_ast pmty_attributes)]
        }

    let of_concrete ({ pmty_desc; pmty_loc; pmty_attributes } : Concrete.t) : t =
      { name = "module_type"; data = Record [("pmty_desc", Versioned_value.of_ast pmty_desc); ("pmty_loc", Versioned_value.of_location pmty_loc); ("pmty_attributes", Versioned_value.of_ast pmty_attributes)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "module_type"; data = Record ["pmty_desc", pmty_desc; "pmty_loc", pmty_loc; "pmty_attributes", pmty_attributes] } ->
        Optional.bind (Versioned_value.to_ast pmty_desc) ~f:(fun pmty_desc ->
          Optional.bind (Versioned_value.to_location pmty_loc) ~f:(fun pmty_loc ->
            Optional.bind (Versioned_value.to_ast pmty_attributes) ~f:(fun pmty_attributes ->
              Some { pmty_desc; pmty_loc; pmty_attributes }
        )))
      | _ -> None
  end

  module Module_type_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pmty_ident of Versioned_ast.t
        | Pmty_signature of Versioned_ast.t
        | Pmty_functor of Versioned_ast.t * Versioned_ast.t option * Versioned_ast.t
        | Pmty_with of Versioned_ast.t * Versioned_ast.t list
        | Pmty_typeof of Versioned_ast.t
        | Pmty_extension of Versioned_ast.t
        | Pmty_alias of Versioned_ast.t
    end

    let create_pmty_ident x1 =
      Versioned_ast.create ~version
        { name = "module_type_desc"
        ; data =
            Variant ("Pmty_ident", Tuple [Versioned_value.of_ast x1])
        }
    let create_pmty_signature x1 =
      Versioned_ast.create ~version
        { name = "module_type_desc"
        ; data =
            Variant ("Pmty_signature", Tuple [Versioned_value.of_ast x1])
        }
    let create_pmty_functor x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "module_type_desc"
        ; data =
            Variant ("Pmty_functor", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_option ~f:Versioned_value.of_ast) x2; Versioned_value.of_ast x3])
        }
    let create_pmty_with x1 x2 =
      Versioned_ast.create ~version
        { name = "module_type_desc"
        ; data =
            Variant ("Pmty_with", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_list ~f:Versioned_value.of_ast) x2])
        }
    let create_pmty_typeof x1 =
      Versioned_ast.create ~version
        { name = "module_type_desc"
        ; data =
            Variant ("Pmty_typeof", Tuple [Versioned_value.of_ast x1])
        }
    let create_pmty_extension x1 =
      Versioned_ast.create ~version
        { name = "module_type_desc"
        ; data =
            Variant ("Pmty_extension", Tuple [Versioned_value.of_ast x1])
        }
    let create_pmty_alias x1 =
      Versioned_ast.create ~version
        { name = "module_type_desc"
        ; data =
            Variant ("Pmty_alias", Tuple [Versioned_value.of_ast x1])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Pmty_ident (x1) ->
        create_pmty_ident x1
      | Pmty_signature (x1) ->
        create_pmty_signature x1
      | Pmty_functor (x1, x2, x3) ->
        create_pmty_functor x1 x2 x3
      | Pmty_with (x1, x2) ->
        create_pmty_with x1 x2
      | Pmty_typeof (x1) ->
        create_pmty_typeof x1
      | Pmty_extension (x1) ->
        create_pmty_extension x1
      | Pmty_alias (x1) ->
        create_pmty_alias x1

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "module_type_desc"; data = data } ->
        ( match data with
        | Variant ("Pmty_ident", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pmty_ident (x1))
          )
        | Variant ("Pmty_signature", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pmty_signature (x1))
          )
        | Variant ("Pmty_functor", Tuple [x1; x2; x3]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Some (Pmty_functor (x1, x2, x3))
          )))
        | Variant ("Pmty_with", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Pmty_with (x1, x2))
          ))
        | Variant ("Pmty_typeof", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pmty_typeof (x1))
          )
        | Variant ("Pmty_extension", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pmty_extension (x1))
          )
        | Variant ("Pmty_alias", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pmty_alias (x1))
          )
        )
      | _ -> None
  end

  module Signature = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = Versioned_ast.t list
    end

    let create x =
      Versioned_ast.create ~version
        { name = "signature"
        ; data =
            (Versioned_value.of_list ~f:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "signature"; data = (Versioned_value.of_list ~f:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "signature"; data = data } ->
        (Versioned_value.to_list ~f:Versioned_value.to_ast) data
      | _ -> None
  end

  module Signature_item = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { psig_desc : Versioned_ast.t; psig_loc : Location.t }
    end

    let create ~psig_desc ~psig_loc =
      Versioned_ast.create ~version
        { name = "signature_item"
        ; data =
            Record [("psig_desc", Versioned_value.of_ast psig_desc); ("psig_loc", Versioned_value.of_location psig_loc)]
        }

    let of_concrete ({ psig_desc; psig_loc } : Concrete.t) : t =
      { name = "signature_item"; data = Record [("psig_desc", Versioned_value.of_ast psig_desc); ("psig_loc", Versioned_value.of_location psig_loc)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "signature_item"; data = Record ["psig_desc", psig_desc; "psig_loc", psig_loc] } ->
        Optional.bind (Versioned_value.to_ast psig_desc) ~f:(fun psig_desc ->
          Optional.bind (Versioned_value.to_location psig_loc) ~f:(fun psig_loc ->
            Some { psig_desc; psig_loc }
        ))
      | _ -> None
  end

  module Signature_item_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Psig_value of Versioned_ast.t
        | Psig_type of Versioned_ast.t * Versioned_ast.t list
        | Psig_typext of Versioned_ast.t
        | Psig_exception of Versioned_ast.t
        | Psig_module of Versioned_ast.t
        | Psig_recmodule of Versioned_ast.t list
        | Psig_modtype of Versioned_ast.t
        | Psig_open of Versioned_ast.t
        | Psig_include of Versioned_ast.t
        | Psig_class of Versioned_ast.t list
        | Psig_class_type of Versioned_ast.t list
        | Psig_attribute of Versioned_ast.t
        | Psig_extension of Versioned_ast.t * Versioned_ast.t
    end

    let create_psig_value x1 =
      Versioned_ast.create ~version
        { name = "signature_item_desc"
        ; data =
            Variant ("Psig_value", Tuple [Versioned_value.of_ast x1])
        }
    let create_psig_type x1 x2 =
      Versioned_ast.create ~version
        { name = "signature_item_desc"
        ; data =
            Variant ("Psig_type", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_list ~f:Versioned_value.of_ast) x2])
        }
    let create_psig_typext x1 =
      Versioned_ast.create ~version
        { name = "signature_item_desc"
        ; data =
            Variant ("Psig_typext", Tuple [Versioned_value.of_ast x1])
        }
    let create_psig_exception x1 =
      Versioned_ast.create ~version
        { name = "signature_item_desc"
        ; data =
            Variant ("Psig_exception", Tuple [Versioned_value.of_ast x1])
        }
    let create_psig_module x1 =
      Versioned_ast.create ~version
        { name = "signature_item_desc"
        ; data =
            Variant ("Psig_module", Tuple [Versioned_value.of_ast x1])
        }
    let create_psig_recmodule x1 =
      Versioned_ast.create ~version
        { name = "signature_item_desc"
        ; data =
            Variant ("Psig_recmodule", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_psig_modtype x1 =
      Versioned_ast.create ~version
        { name = "signature_item_desc"
        ; data =
            Variant ("Psig_modtype", Tuple [Versioned_value.of_ast x1])
        }
    let create_psig_open x1 =
      Versioned_ast.create ~version
        { name = "signature_item_desc"
        ; data =
            Variant ("Psig_open", Tuple [Versioned_value.of_ast x1])
        }
    let create_psig_include x1 =
      Versioned_ast.create ~version
        { name = "signature_item_desc"
        ; data =
            Variant ("Psig_include", Tuple [Versioned_value.of_ast x1])
        }
    let create_psig_class x1 =
      Versioned_ast.create ~version
        { name = "signature_item_desc"
        ; data =
            Variant ("Psig_class", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_psig_class_type x1 =
      Versioned_ast.create ~version
        { name = "signature_item_desc"
        ; data =
            Variant ("Psig_class_type", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_psig_attribute x1 =
      Versioned_ast.create ~version
        { name = "signature_item_desc"
        ; data =
            Variant ("Psig_attribute", Tuple [Versioned_value.of_ast x1])
        }
    let create_psig_extension x1 x2 =
      Versioned_ast.create ~version
        { name = "signature_item_desc"
        ; data =
            Variant ("Psig_extension", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Psig_value (x1) ->
        create_psig_value x1
      | Psig_type (x1, x2) ->
        create_psig_type x1 x2
      | Psig_typext (x1) ->
        create_psig_typext x1
      | Psig_exception (x1) ->
        create_psig_exception x1
      | Psig_module (x1) ->
        create_psig_module x1
      | Psig_recmodule (x1) ->
        create_psig_recmodule x1
      | Psig_modtype (x1) ->
        create_psig_modtype x1
      | Psig_open (x1) ->
        create_psig_open x1
      | Psig_include (x1) ->
        create_psig_include x1
      | Psig_class (x1) ->
        create_psig_class x1
      | Psig_class_type (x1) ->
        create_psig_class_type x1
      | Psig_attribute (x1) ->
        create_psig_attribute x1
      | Psig_extension (x1, x2) ->
        create_psig_extension x1 x2

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "signature_item_desc"; data = data } ->
        ( match data with
        | Variant ("Psig_value", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Psig_value (x1))
          )
        | Variant ("Psig_type", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Psig_type (x1, x2))
          ))
        | Variant ("Psig_typext", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Psig_typext (x1))
          )
        | Variant ("Psig_exception", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Psig_exception (x1))
          )
        | Variant ("Psig_module", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Psig_module (x1))
          )
        | Variant ("Psig_recmodule", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Psig_recmodule (x1))
          )
        | Variant ("Psig_modtype", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Psig_modtype (x1))
          )
        | Variant ("Psig_open", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Psig_open (x1))
          )
        | Variant ("Psig_include", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Psig_include (x1))
          )
        | Variant ("Psig_class", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Psig_class (x1))
          )
        | Variant ("Psig_class_type", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Psig_class_type (x1))
          )
        | Variant ("Psig_attribute", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Psig_attribute (x1))
          )
        | Variant ("Psig_extension", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Psig_extension (x1, x2))
          ))
        )
      | _ -> None
  end

  module Module_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pmd_name : Versioned_ast.t; pmd_type : Versioned_ast.t; pmd_attributes : Versioned_ast.t; pmd_loc : Location.t }
    end

    let create ~pmd_name ~pmd_type ~pmd_attributes ~pmd_loc =
      Versioned_ast.create ~version
        { name = "module_declaration"
        ; data =
            Record [("pmd_name", Versioned_value.of_ast pmd_name); ("pmd_type", Versioned_value.of_ast pmd_type); ("pmd_attributes", Versioned_value.of_ast pmd_attributes); ("pmd_loc", Versioned_value.of_location pmd_loc)]
        }

    let of_concrete ({ pmd_name; pmd_type; pmd_attributes; pmd_loc } : Concrete.t) : t =
      { name = "module_declaration"; data = Record [("pmd_name", Versioned_value.of_ast pmd_name); ("pmd_type", Versioned_value.of_ast pmd_type); ("pmd_attributes", Versioned_value.of_ast pmd_attributes); ("pmd_loc", Versioned_value.of_location pmd_loc)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "module_declaration"; data = Record ["pmd_name", pmd_name; "pmd_type", pmd_type; "pmd_attributes", pmd_attributes; "pmd_loc", pmd_loc] } ->
        Optional.bind (Versioned_value.to_ast pmd_name) ~f:(fun pmd_name ->
          Optional.bind (Versioned_value.to_ast pmd_type) ~f:(fun pmd_type ->
            Optional.bind (Versioned_value.to_ast pmd_attributes) ~f:(fun pmd_attributes ->
              Optional.bind (Versioned_value.to_location pmd_loc) ~f:(fun pmd_loc ->
                Some { pmd_name; pmd_type; pmd_attributes; pmd_loc }
        ))))
      | _ -> None
  end

  module Module_type_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pmtd_name : Versioned_ast.t; pmtd_type : Versioned_ast.t option; pmtd_attributes : Versioned_ast.t; pmtd_loc : Location.t }
    end

    let create ~pmtd_name ~pmtd_type ~pmtd_attributes ~pmtd_loc =
      Versioned_ast.create ~version
        { name = "module_type_declaration"
        ; data =
            Record [("pmtd_name", Versioned_value.of_ast pmtd_name); ("pmtd_type", (Versioned_value.of_option ~f:Versioned_value.of_ast) pmtd_type); ("pmtd_attributes", Versioned_value.of_ast pmtd_attributes); ("pmtd_loc", Versioned_value.of_location pmtd_loc)]
        }

    let of_concrete ({ pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : Concrete.t) : t =
      { name = "module_type_declaration"; data = Record [("pmtd_name", Versioned_value.of_ast pmtd_name); ("pmtd_type", (Versioned_value.of_option ~f:Versioned_value.of_ast) pmtd_type); ("pmtd_attributes", Versioned_value.of_ast pmtd_attributes); ("pmtd_loc", Versioned_value.of_location pmtd_loc)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "module_type_declaration"; data = Record ["pmtd_name", pmtd_name; "pmtd_type", pmtd_type; "pmtd_attributes", pmtd_attributes; "pmtd_loc", pmtd_loc] } ->
        Optional.bind (Versioned_value.to_ast pmtd_name) ~f:(fun pmtd_name ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) pmtd_type) ~f:(fun pmtd_type ->
            Optional.bind (Versioned_value.to_ast pmtd_attributes) ~f:(fun pmtd_attributes ->
              Optional.bind (Versioned_value.to_location pmtd_loc) ~f:(fun pmtd_loc ->
                Some { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }
        ))))
      | _ -> None
  end

  module Open_description = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { popen_lid : Versioned_ast.t; popen_override : Versioned_ast.t; popen_loc : Location.t; popen_attributes : Versioned_ast.t }
    end

    let create ~popen_lid ~popen_override ~popen_loc ~popen_attributes =
      Versioned_ast.create ~version
        { name = "open_description"
        ; data =
            Record [("popen_lid", Versioned_value.of_ast popen_lid); ("popen_override", Versioned_value.of_ast popen_override); ("popen_loc", Versioned_value.of_location popen_loc); ("popen_attributes", Versioned_value.of_ast popen_attributes)]
        }

    let of_concrete ({ popen_lid; popen_override; popen_loc; popen_attributes } : Concrete.t) : t =
      { name = "open_description"; data = Record [("popen_lid", Versioned_value.of_ast popen_lid); ("popen_override", Versioned_value.of_ast popen_override); ("popen_loc", Versioned_value.of_location popen_loc); ("popen_attributes", Versioned_value.of_ast popen_attributes)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "open_description"; data = Record ["popen_lid", popen_lid; "popen_override", popen_override; "popen_loc", popen_loc; "popen_attributes", popen_attributes] } ->
        Optional.bind (Versioned_value.to_ast popen_lid) ~f:(fun popen_lid ->
          Optional.bind (Versioned_value.to_ast popen_override) ~f:(fun popen_override ->
            Optional.bind (Versioned_value.to_location popen_loc) ~f:(fun popen_loc ->
              Optional.bind (Versioned_value.to_ast popen_attributes) ~f:(fun popen_attributes ->
                Some { popen_lid; popen_override; popen_loc; popen_attributes }
        ))))
      | _ -> None
  end

  module Include_infos = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type 'a t = { pincl_mod : 'a; pincl_loc : Location.t; pincl_attributes : Versioned_ast.t }
    end

    let create ~pincl_mod ~pincl_loc ~pincl_attributes =
      Versioned_ast.create ~version
        { name = "include_infos"
        ; data =
            Record [("pincl_mod", a_of_concrete pincl_mod); ("pincl_loc", Versioned_value.of_location pincl_loc); ("pincl_attributes", Versioned_value.of_ast pincl_attributes)]
        }

    let of_concrete ({ pincl_mod; pincl_loc; pincl_attributes } : 'a Concrete.t) : 'a t =
      { name = "include_infos"; data = Record [("pincl_mod", a_of_concrete pincl_mod); ("pincl_loc", Versioned_value.of_location pincl_loc); ("pincl_attributes", Versioned_value.of_ast pincl_attributes)] }

    let to_concrete t : 'a Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "include_infos"; data = Record ["pincl_mod", pincl_mod; "pincl_loc", pincl_loc; "pincl_attributes", pincl_attributes] } ->
        Optional.bind (a_to_concrete pincl_mod) ~f:(fun pincl_mod ->
          Optional.bind (Versioned_value.to_location pincl_loc) ~f:(fun pincl_loc ->
            Optional.bind (Versioned_value.to_ast pincl_attributes) ~f:(fun pincl_attributes ->
              Some { pincl_mod; pincl_loc; pincl_attributes }
        )))
      | _ -> None
  end

  module Include_description = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t) Include_infos.t
    end

    let create x =
      Versioned_ast.create ~version
        { name = "include_description"
        ; data =
            (include_infos_of_concrete Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "include_description"; data = (include_infos_of_concrete Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "include_description"; data = data } ->
        (include_infos_to_concrete Versioned_value.to_ast) data
      | _ -> None
  end

  module Include_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = (Versioned_ast.t) Include_infos.t
    end

    let create x =
      Versioned_ast.create ~version
        { name = "include_declaration"
        ; data =
            (include_infos_of_concrete Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "include_declaration"; data = (include_infos_of_concrete Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "include_declaration"; data = data } ->
        (include_infos_to_concrete Versioned_value.to_ast) data
      | _ -> None
  end

  module With_constraint = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pwith_type of Versioned_ast.t * Versioned_ast.t
        | Pwith_module of Versioned_ast.t * Versioned_ast.t
        | Pwith_typesubst of Versioned_ast.t * Versioned_ast.t
        | Pwith_modsubst of Versioned_ast.t * Versioned_ast.t
    end

    let create_pwith_type x1 x2 =
      Versioned_ast.create ~version
        { name = "with_constraint"
        ; data =
            Variant ("Pwith_type", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_pwith_module x1 x2 =
      Versioned_ast.create ~version
        { name = "with_constraint"
        ; data =
            Variant ("Pwith_module", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_pwith_typesubst x1 x2 =
      Versioned_ast.create ~version
        { name = "with_constraint"
        ; data =
            Variant ("Pwith_typesubst", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_pwith_modsubst x1 x2 =
      Versioned_ast.create ~version
        { name = "with_constraint"
        ; data =
            Variant ("Pwith_modsubst", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Pwith_type (x1, x2) ->
        create_pwith_type x1 x2
      | Pwith_module (x1, x2) ->
        create_pwith_module x1 x2
      | Pwith_typesubst (x1, x2) ->
        create_pwith_typesubst x1 x2
      | Pwith_modsubst (x1, x2) ->
        create_pwith_modsubst x1 x2

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "with_constraint"; data = data } ->
        ( match data with
        | Variant ("Pwith_type", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pwith_type (x1, x2))
          ))
        | Variant ("Pwith_module", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pwith_module (x1, x2))
          ))
        | Variant ("Pwith_typesubst", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pwith_typesubst (x1, x2))
          ))
        | Variant ("Pwith_modsubst", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pwith_modsubst (x1, x2))
          ))
        )
      | _ -> None
  end

  module Module_expr = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pmod_desc : Versioned_ast.t; pmod_loc : Location.t; pmod_attributes : Versioned_ast.t }
    end

    let create ~pmod_desc ~pmod_loc ~pmod_attributes =
      Versioned_ast.create ~version
        { name = "module_expr"
        ; data =
            Record [("pmod_desc", Versioned_value.of_ast pmod_desc); ("pmod_loc", Versioned_value.of_location pmod_loc); ("pmod_attributes", Versioned_value.of_ast pmod_attributes)]
        }

    let of_concrete ({ pmod_desc; pmod_loc; pmod_attributes } : Concrete.t) : t =
      { name = "module_expr"; data = Record [("pmod_desc", Versioned_value.of_ast pmod_desc); ("pmod_loc", Versioned_value.of_location pmod_loc); ("pmod_attributes", Versioned_value.of_ast pmod_attributes)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "module_expr"; data = Record ["pmod_desc", pmod_desc; "pmod_loc", pmod_loc; "pmod_attributes", pmod_attributes] } ->
        Optional.bind (Versioned_value.to_ast pmod_desc) ~f:(fun pmod_desc ->
          Optional.bind (Versioned_value.to_location pmod_loc) ~f:(fun pmod_loc ->
            Optional.bind (Versioned_value.to_ast pmod_attributes) ~f:(fun pmod_attributes ->
              Some { pmod_desc; pmod_loc; pmod_attributes }
        )))
      | _ -> None
  end

  module Module_expr_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pmod_ident of Versioned_ast.t
        | Pmod_structure of Versioned_ast.t
        | Pmod_functor of Versioned_ast.t * Versioned_ast.t option * Versioned_ast.t
        | Pmod_apply of Versioned_ast.t * Versioned_ast.t
        | Pmod_constraint of Versioned_ast.t * Versioned_ast.t
        | Pmod_unpack of Versioned_ast.t
        | Pmod_extension of Versioned_ast.t
    end

    let create_pmod_ident x1 =
      Versioned_ast.create ~version
        { name = "module_expr_desc"
        ; data =
            Variant ("Pmod_ident", Tuple [Versioned_value.of_ast x1])
        }
    let create_pmod_structure x1 =
      Versioned_ast.create ~version
        { name = "module_expr_desc"
        ; data =
            Variant ("Pmod_structure", Tuple [Versioned_value.of_ast x1])
        }
    let create_pmod_functor x1 x2 x3 =
      Versioned_ast.create ~version
        { name = "module_expr_desc"
        ; data =
            Variant ("Pmod_functor", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_option ~f:Versioned_value.of_ast) x2; Versioned_value.of_ast x3])
        }
    let create_pmod_apply x1 x2 =
      Versioned_ast.create ~version
        { name = "module_expr_desc"
        ; data =
            Variant ("Pmod_apply", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_pmod_constraint x1 x2 =
      Versioned_ast.create ~version
        { name = "module_expr_desc"
        ; data =
            Variant ("Pmod_constraint", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_pmod_unpack x1 =
      Versioned_ast.create ~version
        { name = "module_expr_desc"
        ; data =
            Variant ("Pmod_unpack", Tuple [Versioned_value.of_ast x1])
        }
    let create_pmod_extension x1 =
      Versioned_ast.create ~version
        { name = "module_expr_desc"
        ; data =
            Variant ("Pmod_extension", Tuple [Versioned_value.of_ast x1])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Pmod_ident (x1) ->
        create_pmod_ident x1
      | Pmod_structure (x1) ->
        create_pmod_structure x1
      | Pmod_functor (x1, x2, x3) ->
        create_pmod_functor x1 x2 x3
      | Pmod_apply (x1, x2) ->
        create_pmod_apply x1 x2
      | Pmod_constraint (x1, x2) ->
        create_pmod_constraint x1 x2
      | Pmod_unpack (x1) ->
        create_pmod_unpack x1
      | Pmod_extension (x1) ->
        create_pmod_extension x1

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "module_expr_desc"; data = data } ->
        ( match data with
        | Variant ("Pmod_ident", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pmod_ident (x1))
          )
        | Variant ("Pmod_structure", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pmod_structure (x1))
          )
        | Variant ("Pmod_functor", Tuple [x1; x2; x3]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Optional.bind (Versioned_value.to_ast x3) ~f:(fun x3 ->
                Some (Pmod_functor (x1, x2, x3))
          )))
        | Variant ("Pmod_apply", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pmod_apply (x1, x2))
          ))
        | Variant ("Pmod_constraint", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pmod_constraint (x1, x2))
          ))
        | Variant ("Pmod_unpack", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pmod_unpack (x1))
          )
        | Variant ("Pmod_extension", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pmod_extension (x1))
          )
        )
      | _ -> None
  end

  module Structure = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = Versioned_ast.t list
    end

    let create x =
      Versioned_ast.create ~version
        { name = "structure"
        ; data =
            (Versioned_value.of_list ~f:Versioned_value.of_ast) x
        }

    let of_concrete (concrete : Concrete.t) : t =
      { name = "structure"; data = (Versioned_value.of_list ~f:Versioned_value.of_ast) concrete }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "structure"; data = data } ->
        (Versioned_value.to_list ~f:Versioned_value.to_ast) data
      | _ -> None
  end

  module Structure_item = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pstr_desc : Versioned_ast.t; pstr_loc : Location.t }
    end

    let create ~pstr_desc ~pstr_loc =
      Versioned_ast.create ~version
        { name = "structure_item"
        ; data =
            Record [("pstr_desc", Versioned_value.of_ast pstr_desc); ("pstr_loc", Versioned_value.of_location pstr_loc)]
        }

    let of_concrete ({ pstr_desc; pstr_loc } : Concrete.t) : t =
      { name = "structure_item"; data = Record [("pstr_desc", Versioned_value.of_ast pstr_desc); ("pstr_loc", Versioned_value.of_location pstr_loc)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "structure_item"; data = Record ["pstr_desc", pstr_desc; "pstr_loc", pstr_loc] } ->
        Optional.bind (Versioned_value.to_ast pstr_desc) ~f:(fun pstr_desc ->
          Optional.bind (Versioned_value.to_location pstr_loc) ~f:(fun pstr_loc ->
            Some { pstr_desc; pstr_loc }
        ))
      | _ -> None
  end

  module Structure_item_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pstr_eval of Versioned_ast.t * Versioned_ast.t
        | Pstr_value of Versioned_ast.t * Versioned_ast.t list
        | Pstr_primitive of Versioned_ast.t
        | Pstr_type of Versioned_ast.t * Versioned_ast.t list
        | Pstr_typext of Versioned_ast.t
        | Pstr_exception of Versioned_ast.t
        | Pstr_module of Versioned_ast.t
        | Pstr_recmodule of Versioned_ast.t list
        | Pstr_modtype of Versioned_ast.t
        | Pstr_open of Versioned_ast.t
        | Pstr_class of Versioned_ast.t list
        | Pstr_class_type of Versioned_ast.t list
        | Pstr_include of Versioned_ast.t
        | Pstr_attribute of Versioned_ast.t
        | Pstr_extension of Versioned_ast.t * Versioned_ast.t
    end

    let create_pstr_eval x1 x2 =
      Versioned_ast.create ~version
        { name = "structure_item_desc"
        ; data =
            Variant ("Pstr_eval", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }
    let create_pstr_value x1 x2 =
      Versioned_ast.create ~version
        { name = "structure_item_desc"
        ; data =
            Variant ("Pstr_value", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_list ~f:Versioned_value.of_ast) x2])
        }
    let create_pstr_primitive x1 =
      Versioned_ast.create ~version
        { name = "structure_item_desc"
        ; data =
            Variant ("Pstr_primitive", Tuple [Versioned_value.of_ast x1])
        }
    let create_pstr_type x1 x2 =
      Versioned_ast.create ~version
        { name = "structure_item_desc"
        ; data =
            Variant ("Pstr_type", Tuple [Versioned_value.of_ast x1; (Versioned_value.of_list ~f:Versioned_value.of_ast) x2])
        }
    let create_pstr_typext x1 =
      Versioned_ast.create ~version
        { name = "structure_item_desc"
        ; data =
            Variant ("Pstr_typext", Tuple [Versioned_value.of_ast x1])
        }
    let create_pstr_exception x1 =
      Versioned_ast.create ~version
        { name = "structure_item_desc"
        ; data =
            Variant ("Pstr_exception", Tuple [Versioned_value.of_ast x1])
        }
    let create_pstr_module x1 =
      Versioned_ast.create ~version
        { name = "structure_item_desc"
        ; data =
            Variant ("Pstr_module", Tuple [Versioned_value.of_ast x1])
        }
    let create_pstr_recmodule x1 =
      Versioned_ast.create ~version
        { name = "structure_item_desc"
        ; data =
            Variant ("Pstr_recmodule", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_pstr_modtype x1 =
      Versioned_ast.create ~version
        { name = "structure_item_desc"
        ; data =
            Variant ("Pstr_modtype", Tuple [Versioned_value.of_ast x1])
        }
    let create_pstr_open x1 =
      Versioned_ast.create ~version
        { name = "structure_item_desc"
        ; data =
            Variant ("Pstr_open", Tuple [Versioned_value.of_ast x1])
        }
    let create_pstr_class x1 =
      Versioned_ast.create ~version
        { name = "structure_item_desc"
        ; data =
            Variant ("Pstr_class", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_pstr_class_type x1 =
      Versioned_ast.create ~version
        { name = "structure_item_desc"
        ; data =
            Variant ("Pstr_class_type", Tuple [(Versioned_value.of_list ~f:Versioned_value.of_ast) x1])
        }
    let create_pstr_include x1 =
      Versioned_ast.create ~version
        { name = "structure_item_desc"
        ; data =
            Variant ("Pstr_include", Tuple [Versioned_value.of_ast x1])
        }
    let create_pstr_attribute x1 =
      Versioned_ast.create ~version
        { name = "structure_item_desc"
        ; data =
            Variant ("Pstr_attribute", Tuple [Versioned_value.of_ast x1])
        }
    let create_pstr_extension x1 x2 =
      Versioned_ast.create ~version
        { name = "structure_item_desc"
        ; data =
            Variant ("Pstr_extension", Tuple [Versioned_value.of_ast x1; Versioned_value.of_ast x2])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Pstr_eval (x1, x2) ->
        create_pstr_eval x1 x2
      | Pstr_value (x1, x2) ->
        create_pstr_value x1 x2
      | Pstr_primitive (x1) ->
        create_pstr_primitive x1
      | Pstr_type (x1, x2) ->
        create_pstr_type x1 x2
      | Pstr_typext (x1) ->
        create_pstr_typext x1
      | Pstr_exception (x1) ->
        create_pstr_exception x1
      | Pstr_module (x1) ->
        create_pstr_module x1
      | Pstr_recmodule (x1) ->
        create_pstr_recmodule x1
      | Pstr_modtype (x1) ->
        create_pstr_modtype x1
      | Pstr_open (x1) ->
        create_pstr_open x1
      | Pstr_class (x1) ->
        create_pstr_class x1
      | Pstr_class_type (x1) ->
        create_pstr_class_type x1
      | Pstr_include (x1) ->
        create_pstr_include x1
      | Pstr_attribute (x1) ->
        create_pstr_attribute x1
      | Pstr_extension (x1, x2) ->
        create_pstr_extension x1 x2

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "structure_item_desc"; data = data } ->
        ( match data with
        | Variant ("Pstr_eval", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pstr_eval (x1, x2))
          ))
        | Variant ("Pstr_value", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Pstr_value (x1, x2))
          ))
        | Variant ("Pstr_primitive", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pstr_primitive (x1))
          )
        | Variant ("Pstr_type", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x2) ~f:(fun x2 ->
              Some (Pstr_type (x1, x2))
          ))
        | Variant ("Pstr_typext", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pstr_typext (x1))
          )
        | Variant ("Pstr_exception", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pstr_exception (x1))
          )
        | Variant ("Pstr_module", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pstr_module (x1))
          )
        | Variant ("Pstr_recmodule", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Pstr_recmodule (x1))
          )
        | Variant ("Pstr_modtype", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pstr_modtype (x1))
          )
        | Variant ("Pstr_open", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pstr_open (x1))
          )
        | Variant ("Pstr_class", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Pstr_class (x1))
          )
        | Variant ("Pstr_class_type", Tuple [x1]) ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) x1) ~f:(fun x1 ->
            Some (Pstr_class_type (x1))
          )
        | Variant ("Pstr_include", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pstr_include (x1))
          )
        | Variant ("Pstr_attribute", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pstr_attribute (x1))
          )
        | Variant ("Pstr_extension", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Pstr_extension (x1, x2))
          ))
        )
      | _ -> None
  end

  module Value_binding = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pvb_pat : Versioned_ast.t; pvb_expr : Versioned_ast.t; pvb_attributes : Versioned_ast.t; pvb_loc : Location.t }
    end

    let create ~pvb_pat ~pvb_expr ~pvb_attributes ~pvb_loc =
      Versioned_ast.create ~version
        { name = "value_binding"
        ; data =
            Record [("pvb_pat", Versioned_value.of_ast pvb_pat); ("pvb_expr", Versioned_value.of_ast pvb_expr); ("pvb_attributes", Versioned_value.of_ast pvb_attributes); ("pvb_loc", Versioned_value.of_location pvb_loc)]
        }

    let of_concrete ({ pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : Concrete.t) : t =
      { name = "value_binding"; data = Record [("pvb_pat", Versioned_value.of_ast pvb_pat); ("pvb_expr", Versioned_value.of_ast pvb_expr); ("pvb_attributes", Versioned_value.of_ast pvb_attributes); ("pvb_loc", Versioned_value.of_location pvb_loc)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "value_binding"; data = Record ["pvb_pat", pvb_pat; "pvb_expr", pvb_expr; "pvb_attributes", pvb_attributes; "pvb_loc", pvb_loc] } ->
        Optional.bind (Versioned_value.to_ast pvb_pat) ~f:(fun pvb_pat ->
          Optional.bind (Versioned_value.to_ast pvb_expr) ~f:(fun pvb_expr ->
            Optional.bind (Versioned_value.to_ast pvb_attributes) ~f:(fun pvb_attributes ->
              Optional.bind (Versioned_value.to_location pvb_loc) ~f:(fun pvb_loc ->
                Some { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }
        ))))
      | _ -> None
  end

  module Module_binding = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t = { pmb_name : Versioned_ast.t; pmb_expr : Versioned_ast.t; pmb_attributes : Versioned_ast.t; pmb_loc : Location.t }
    end

    let create ~pmb_name ~pmb_expr ~pmb_attributes ~pmb_loc =
      Versioned_ast.create ~version
        { name = "module_binding"
        ; data =
            Record [("pmb_name", Versioned_value.of_ast pmb_name); ("pmb_expr", Versioned_value.of_ast pmb_expr); ("pmb_attributes", Versioned_value.of_ast pmb_attributes); ("pmb_loc", Versioned_value.of_location pmb_loc)]
        }

    let of_concrete ({ pmb_name; pmb_expr; pmb_attributes; pmb_loc } : Concrete.t) : t =
      { name = "module_binding"; data = Record [("pmb_name", Versioned_value.of_ast pmb_name); ("pmb_expr", Versioned_value.of_ast pmb_expr); ("pmb_attributes", Versioned_value.of_ast pmb_attributes); ("pmb_loc", Versioned_value.of_location pmb_loc)] }

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "module_binding"; data = Record ["pmb_name", pmb_name; "pmb_expr", pmb_expr; "pmb_attributes", pmb_attributes; "pmb_loc", pmb_loc] } ->
        Optional.bind (Versioned_value.to_ast pmb_name) ~f:(fun pmb_name ->
          Optional.bind (Versioned_value.to_ast pmb_expr) ~f:(fun pmb_expr ->
            Optional.bind (Versioned_value.to_ast pmb_attributes) ~f:(fun pmb_attributes ->
              Optional.bind (Versioned_value.to_location pmb_loc) ~f:(fun pmb_loc ->
                Some { pmb_name; pmb_expr; pmb_attributes; pmb_loc }
        ))))
      | _ -> None
  end

  module Toplevel_phrase = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Ptop_def of Versioned_ast.t
        | Ptop_dir of string * Versioned_ast.t
    end

    let create_ptop_def x1 =
      Versioned_ast.create ~version
        { name = "toplevel_phrase"
        ; data =
            Variant ("Ptop_def", Tuple [Versioned_value.of_ast x1])
        }
    let create_ptop_dir x1 x2 =
      Versioned_ast.create ~version
        { name = "toplevel_phrase"
        ; data =
            Variant ("Ptop_dir", Tuple [Versioned_value.of_string x1; Versioned_value.of_ast x2])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Ptop_def (x1) ->
        create_ptop_def x1
      | Ptop_dir (x1, x2) ->
        create_ptop_dir x1 x2

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "toplevel_phrase"; data = data } ->
        ( match data with
        | Variant ("Ptop_def", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Ptop_def (x1))
          )
        | Variant ("Ptop_dir", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_string x1) ~f:(fun x1 ->
            Optional.bind (Versioned_value.to_ast x2) ~f:(fun x2 ->
              Some (Ptop_dir (x1, x2))
          ))
        )
      | _ -> None
  end

  module Directive_argument = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pdir_none
        | Pdir_string of string
        | Pdir_int of string * char option
        | Pdir_ident of Versioned_ast.t
        | Pdir_bool of bool
    end

    let create_pdir_none =
      Versioned_ast.create ~version
        { name = "directive_argument"
        ; data =
            Variant ("Pdir_none", Empty)
        }
    let create_pdir_string x1 =
      Versioned_ast.create ~version
        { name = "directive_argument"
        ; data =
            Variant ("Pdir_string", Tuple [Versioned_value.of_string x1])
        }
    let create_pdir_int x1 x2 =
      Versioned_ast.create ~version
        { name = "directive_argument"
        ; data =
            Variant ("Pdir_int", Tuple [Versioned_value.of_string x1; (Versioned_value.of_option ~f:Versioned_value.of_char) x2])
        }
    let create_pdir_ident x1 =
      Versioned_ast.create ~version
        { name = "directive_argument"
        ; data =
            Variant ("Pdir_ident", Tuple [Versioned_value.of_ast x1])
        }
    let create_pdir_bool x1 =
      Versioned_ast.create ~version
        { name = "directive_argument"
        ; data =
            Variant ("Pdir_bool", Tuple [Versioned_value.of_bool x1])
        }

    let of_concrete (concrete : Concrete.t) : t =
      match concrete with
      | Pdir_none -> create_pdir_none
      | Pdir_string (x1) ->
        create_pdir_string x1
      | Pdir_int (x1, x2) ->
        create_pdir_int x1 x2
      | Pdir_ident (x1) ->
        create_pdir_ident x1
      | Pdir_bool (x1) ->
        create_pdir_bool x1

    let to_concrete t : Concrete.t =
      match Versioned_ast.convert t ~version with
      | { name = "directive_argument"; data = data } ->
        ( match data with
        | Variant ("Pdir_none", Empty) -> Some Pdir_none
        | Variant ("Pdir_string", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_string x1) ~f:(fun x1 ->
            Some (Pdir_string (x1))
          )
        | Variant ("Pdir_int", Tuple [x1; x2]) ->
          Optional.bind (Versioned_value.to_string x1) ~f:(fun x1 ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_char) x2) ~f:(fun x2 ->
              Some (Pdir_int (x1, x2))
          ))
        | Variant ("Pdir_ident", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_ast x1) ~f:(fun x1 ->
            Some (Pdir_ident (x1))
          )
        | Variant ("Pdir_bool", Tuple [x1]) ->
          Optional.bind (Versioned_value.to_bool x1) ~f:(fun x1 ->
            Some (Pdir_bool (x1))
          )
        )
      | _ -> None
  end
end
(*$*)
