open Stdppx

module Helpers = struct
  module Option = struct
    let bind = Option.bind
  end
end

type 'a node = Node.t

(*$ Ppx_ast_cinaps.print_versions_ml () *)
type arg_label_
type attribute_
type attributes_
type case_
type class_expr_
type class_expr_desc_
type class_field_
type class_field_desc_
type class_field_kind_
type 'a class_infos_
type class_signature_
type class_structure_
type class_type_
type class_type_desc_
type class_type_field_
type class_type_field_desc_
type closed_flag_
type constant_
type constructor_arguments_
type constructor_declaration_
type core_type_
type core_type_desc_
type direction_flag_
type directive_argument_
type expression_
type expression_desc_
type extension_
type extension_constructor_
type extension_constructor_kind_
type 'a include_infos_
type label_declaration_
type longident_
type module_binding_
type module_declaration_
type module_expr_
type module_expr_desc_
type module_type_
type module_type_declaration_
type module_type_desc_
type mutable_flag_
type object_field_
type open_description_
type override_flag_
type package_type_
type pattern_
type pattern_desc_
type payload_
type private_flag_
type rec_flag_
type row_field_
type signature_
type signature_item_
type signature_item_desc_
type structure_
type structure_item_
type structure_item_desc_
type toplevel_phrase_
type type_declaration_
type type_extension_
type type_kind_
type value_binding_
type value_description_
type variance_
type virtual_flag_
type with_constraint_

type arg_label = arg_label_ node
type attribute = attribute_ node
type attributes = attributes_ node
type case = case_ node
type class_expr = class_expr_ node
type class_expr_desc = class_expr_desc_ node
type class_field = class_field_ node
type class_field_desc = class_field_desc_ node
type class_field_kind = class_field_kind_ node
type 'a class_infos = 'a class_infos_ node
type class_signature = class_signature_ node
type class_structure = class_structure_ node
type class_type = class_type_ node
type class_type_desc = class_type_desc_ node
type class_type_field = class_type_field_ node
type class_type_field_desc = class_type_field_desc_ node
type closed_flag = closed_flag_ node
type constant = constant_ node
type constructor_arguments = constructor_arguments_ node
type constructor_declaration = constructor_declaration_ node
type core_type = core_type_ node
type core_type_desc = core_type_desc_ node
type direction_flag = direction_flag_ node
type directive_argument = directive_argument_ node
type expression = expression_ node
type expression_desc = expression_desc_ node
type extension = extension_ node
type extension_constructor = extension_constructor_ node
type extension_constructor_kind = extension_constructor_kind_ node
type 'a include_infos = 'a include_infos_ node
type label_declaration = label_declaration_ node
type longident = longident_ node
type module_binding = module_binding_ node
type module_declaration = module_declaration_ node
type module_expr = module_expr_ node
type module_expr_desc = module_expr_desc_ node
type module_type = module_type_ node
type module_type_declaration = module_type_declaration_ node
type module_type_desc = module_type_desc_ node
type mutable_flag = mutable_flag_ node
type object_field = object_field_ node
type open_description = open_description_ node
type override_flag = override_flag_ node
type package_type = package_type_ node
type pattern = pattern_ node
type pattern_desc = pattern_desc_ node
type payload = payload_ node
type private_flag = private_flag_ node
type rec_flag = rec_flag_ node
type row_field = row_field_ node
type signature = signature_ node
type signature_item = signature_item_ node
type signature_item_desc = signature_item_desc_ node
type structure = structure_ node
type structure_item = structure_item_ node
type structure_item_desc = structure_item_desc_ node
type toplevel_phrase = toplevel_phrase_ node
type type_declaration = type_declaration_ node
type type_extension = type_extension_ node
type type_kind = type_kind_ node
type value_binding = value_binding_ node
type value_description = value_description_ node
type variance = variance_ node
type virtual_flag = virtual_flag_ node
type with_constraint = with_constraint_ node

type class_declaration = class_expr class_infos
type class_description = class_type class_infos
type class_type_declaration = class_type class_infos
type include_declaration = module_expr include_infos
type include_description = module_type include_infos
type label = string
type longident_loc = longident Astlib.Loc.t

module Unstable_for_testing = struct
  let version = "unstable-for-testing"
  let node name data = Node.of_node ~version { name; data }

  module Directive_argument = struct
    type t = directive_argument

    type concrete =
      | Pdir_bool of bool
      | Pdir_ident of longident
      | Pdir_int of char option * string
      | Pdir_string of string
      | Pdir_none

    let pdir_bool x1 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_bool"
          ; args =
            [| Data.of_bool x1
            |]
          })
    let pdir_ident x1 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_ident"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pdir_int x1 x2 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_int"
          ; args =
            [| (Data.of_option ~f:Data.of_char) x1
             ; Data.of_string x2
            |]
          })
    let pdir_string x1 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_string"
          ; args =
            [| Data.of_string x1
            |]
          })
    let pdir_none =
      node "directive_argument" (Variant { tag = "Pdir_none"; args = [||] })

    let of_concrete c =
      match c with
      | Pdir_bool (x1) ->
        pdir_bool x1
      | Pdir_ident (x1) ->
        pdir_ident x1
      | Pdir_int (x1, x2) ->
        pdir_int x1 x2
      | Pdir_string (x1) ->
        pdir_string x1
      | Pdir_none -> pdir_none

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "directive_argument"; data } ->
        begin
          match data with
          | Variant { tag = "Pdir_bool"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_bool x1) ~f:(fun x1 ->
              Some (Pdir_bool (x1))
            )
          | Variant { tag = "Pdir_ident"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pdir_ident (x1))
            )
          | Variant { tag = "Pdir_int"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_char) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_string x2) ~f:(fun x2 ->
                Some (Pdir_int (x1, x2))
            ))
          | Variant { tag = "Pdir_string"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Some (Pdir_string (x1))
            )
          | Variant { tag = "Pdir_none"; args = [||] } -> Some Pdir_none
        | _ -> None
        end
      | _ -> None
  end

  module Toplevel_phrase = struct
    type t = toplevel_phrase

    type concrete =
      | Ptop_dir of directive_argument * string
      | Ptop_def of structure

    let ptop_dir x1 x2 =
      node "toplevel_phrase"
        (Variant
          { tag = "Ptop_dir"
          ; args =
            [| Data.of_node x1
             ; Data.of_string x2
            |]
          })
    let ptop_def x1 =
      node "toplevel_phrase"
        (Variant
          { tag = "Ptop_def"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete c =
      match c with
      | Ptop_dir (x1, x2) ->
        ptop_dir x1 x2
      | Ptop_def (x1) ->
        ptop_def x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "toplevel_phrase"; data } ->
        begin
          match data with
          | Variant { tag = "Ptop_dir"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_string x2) ~f:(fun x2 ->
                Some (Ptop_dir (x1, x2))
            ))
          | Variant { tag = "Ptop_def"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Ptop_def (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Module_binding = struct
    type t = module_binding

    type concrete =
      { pmb_loc : Astlib.Location.t
      ; pmb_attributes : attributes
      ; pmb_expr : module_expr
      ; pmb_name : string Astlib.Loc.t
      }

    let create ~pmb_loc ~pmb_attributes ~pmb_expr ~pmb_name =
      let fields =
        [| Data.of_location pmb_loc
         ; Data.of_node pmb_attributes
         ; Data.of_node pmb_expr
         ; (Data.of_loc ~f:Data.of_string) pmb_name
        |]
      in
      node "module_binding" (Record fields)

    let of_concrete { pmb_loc; pmb_attributes; pmb_expr; pmb_name } =
      create ~pmb_loc ~pmb_attributes ~pmb_expr ~pmb_name

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "module_binding"
        ; data = Record [| pmb_loc; pmb_attributes; pmb_expr; pmb_name |]
        } ->
          Helpers.Option.bind (Data.to_location pmb_loc) ~f:(fun pmb_loc ->
            Helpers.Option.bind (Data.to_node pmb_attributes) ~f:(fun pmb_attributes ->
              Helpers.Option.bind (Data.to_node pmb_expr) ~f:(fun pmb_expr ->
                Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pmb_name) ~f:(fun pmb_name ->
                  Some { pmb_loc; pmb_attributes; pmb_expr; pmb_name }
          ))))
      | _ -> None
  end

  module Value_binding = struct
    type t = value_binding

    type concrete =
      { pvb_loc : Astlib.Location.t
      ; pvb_attributes : attributes
      ; pvb_expr : expression
      ; pvb_pat : pattern
      }

    let create ~pvb_loc ~pvb_attributes ~pvb_expr ~pvb_pat =
      let fields =
        [| Data.of_location pvb_loc
         ; Data.of_node pvb_attributes
         ; Data.of_node pvb_expr
         ; Data.of_node pvb_pat
        |]
      in
      node "value_binding" (Record fields)

    let of_concrete { pvb_loc; pvb_attributes; pvb_expr; pvb_pat } =
      create ~pvb_loc ~pvb_attributes ~pvb_expr ~pvb_pat

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "value_binding"
        ; data = Record [| pvb_loc; pvb_attributes; pvb_expr; pvb_pat |]
        } ->
          Helpers.Option.bind (Data.to_location pvb_loc) ~f:(fun pvb_loc ->
            Helpers.Option.bind (Data.to_node pvb_attributes) ~f:(fun pvb_attributes ->
              Helpers.Option.bind (Data.to_node pvb_expr) ~f:(fun pvb_expr ->
                Helpers.Option.bind (Data.to_node pvb_pat) ~f:(fun pvb_pat ->
                  Some { pvb_loc; pvb_attributes; pvb_expr; pvb_pat }
          ))))
      | _ -> None
  end

  module Structure_item_desc = struct
    type t = structure_item_desc

    type concrete =
      | Pstr_extension of attributes * extension
      | Pstr_attribute of attribute
      | Pstr_include of include_declaration
      | Pstr_class_type of class_type_declaration list
      | Pstr_class of class_declaration list
      | Pstr_open of open_description
      | Pstr_modtype of module_type_declaration
      | Pstr_recmodule of module_binding list
      | Pstr_module of module_binding
      | Pstr_exception of extension_constructor
      | Pstr_typext of type_extension
      | Pstr_type of type_declaration list * rec_flag
      | Pstr_primitive of value_description
      | Pstr_value of value_binding list * rec_flag
      | Pstr_eval of attributes * expression

    let pstr_extension x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_extension"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pstr_attribute x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_include x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_include"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_class_type x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_class_type"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let pstr_class x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_class"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let pstr_open x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_open"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_modtype x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_modtype"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_recmodule x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_recmodule"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let pstr_module x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_module"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_exception x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_exception"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_typext x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_typext"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_type x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_type"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let pstr_primitive x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_primitive"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_value x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_value"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let pstr_eval x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_eval"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete c =
      match c with
      | Pstr_extension (x1, x2) ->
        pstr_extension x1 x2
      | Pstr_attribute (x1) ->
        pstr_attribute x1
      | Pstr_include (x1) ->
        pstr_include x1
      | Pstr_class_type (x1) ->
        pstr_class_type x1
      | Pstr_class (x1) ->
        pstr_class x1
      | Pstr_open (x1) ->
        pstr_open x1
      | Pstr_modtype (x1) ->
        pstr_modtype x1
      | Pstr_recmodule (x1) ->
        pstr_recmodule x1
      | Pstr_module (x1) ->
        pstr_module x1
      | Pstr_exception (x1) ->
        pstr_exception x1
      | Pstr_typext (x1) ->
        pstr_typext x1
      | Pstr_type (x1, x2) ->
        pstr_type x1 x2
      | Pstr_primitive (x1) ->
        pstr_primitive x1
      | Pstr_value (x1, x2) ->
        pstr_value x1 x2
      | Pstr_eval (x1, x2) ->
        pstr_eval x1 x2

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "structure_item_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pstr_extension"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pstr_extension (x1, x2))
            ))
          | Variant { tag = "Pstr_attribute"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_attribute (x1))
            )
          | Variant { tag = "Pstr_include"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_include (x1))
            )
          | Variant { tag = "Pstr_class_type"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pstr_class_type (x1))
            )
          | Variant { tag = "Pstr_class"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pstr_class (x1))
            )
          | Variant { tag = "Pstr_open"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_open (x1))
            )
          | Variant { tag = "Pstr_modtype"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_modtype (x1))
            )
          | Variant { tag = "Pstr_recmodule"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pstr_recmodule (x1))
            )
          | Variant { tag = "Pstr_module"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_module (x1))
            )
          | Variant { tag = "Pstr_exception"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_exception (x1))
            )
          | Variant { tag = "Pstr_typext"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_typext (x1))
            )
          | Variant { tag = "Pstr_type"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pstr_type (x1, x2))
            ))
          | Variant { tag = "Pstr_primitive"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_primitive (x1))
            )
          | Variant { tag = "Pstr_value"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pstr_value (x1, x2))
            ))
          | Variant { tag = "Pstr_eval"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pstr_eval (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Structure_item = struct
    type t = structure_item

    type concrete =
      { pstr_loc : Astlib.Location.t
      ; pstr_desc : structure_item_desc
      }

    let create ~pstr_loc ~pstr_desc =
      let fields =
        [| Data.of_location pstr_loc
         ; Data.of_node pstr_desc
        |]
      in
      node "structure_item" (Record fields)

    let of_concrete { pstr_loc; pstr_desc } =
      create ~pstr_loc ~pstr_desc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "structure_item"
        ; data = Record [| pstr_loc; pstr_desc |]
        } ->
          Helpers.Option.bind (Data.to_location pstr_loc) ~f:(fun pstr_loc ->
            Helpers.Option.bind (Data.to_node pstr_desc) ~f:(fun pstr_desc ->
              Some { pstr_loc; pstr_desc }
          ))
      | _ -> None
  end

  module Structure = struct
    type t = structure

    type concrete = structure_item list

    let create =
      let data = (Data.of_list ~f:Data.of_node) in
      fun x -> node "structure" (data x)

    let of_concrete = create

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "structure"; data } -> (Data.to_list ~f:Data.to_node) data
      | _ -> None
  end

  module Module_expr_desc = struct
    type t = module_expr_desc

    type concrete =
      | Pmod_extension of extension
      | Pmod_unpack of expression
      | Pmod_constraint of module_type * module_expr
      | Pmod_apply of module_expr * module_expr
      | Pmod_functor of module_expr * module_type option * string Astlib.Loc.t
      | Pmod_structure of structure
      | Pmod_ident of longident_loc

    let pmod_extension x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pmod_unpack x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_unpack"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pmod_constraint x1 x2 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pmod_apply x1 x2 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_apply"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pmod_functor x1 x2 x3 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_functor"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
             ; (Data.of_loc ~f:Data.of_string) x3
            |]
          })
    let pmod_structure x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_structure"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pmod_ident x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_ident"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
            |]
          })

    let of_concrete c =
      match c with
      | Pmod_extension (x1) ->
        pmod_extension x1
      | Pmod_unpack (x1) ->
        pmod_unpack x1
      | Pmod_constraint (x1, x2) ->
        pmod_constraint x1 x2
      | Pmod_apply (x1, x2) ->
        pmod_apply x1 x2
      | Pmod_functor (x1, x2, x3) ->
        pmod_functor x1 x2 x3
      | Pmod_structure (x1) ->
        pmod_structure x1
      | Pmod_ident (x1) ->
        pmod_ident x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "module_expr_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pmod_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pmod_extension (x1))
            )
          | Variant { tag = "Pmod_unpack"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pmod_unpack (x1))
            )
          | Variant { tag = "Pmod_constraint"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pmod_constraint (x1, x2))
            ))
          | Variant { tag = "Pmod_apply"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pmod_apply (x1, x2))
            ))
          | Variant { tag = "Pmod_functor"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x3) ~f:(fun x3 ->
                  Some (Pmod_functor (x1, x2, x3))
            )))
          | Variant { tag = "Pmod_structure"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pmod_structure (x1))
            )
          | Variant { tag = "Pmod_ident"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pmod_ident (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Module_expr = struct
    type t = module_expr

    type concrete =
      { pmod_attributes : attributes
      ; pmod_loc : Astlib.Location.t
      ; pmod_desc : module_expr_desc
      }

    let create ~pmod_attributes ~pmod_loc ~pmod_desc =
      let fields =
        [| Data.of_node pmod_attributes
         ; Data.of_location pmod_loc
         ; Data.of_node pmod_desc
        |]
      in
      node "module_expr" (Record fields)

    let of_concrete { pmod_attributes; pmod_loc; pmod_desc } =
      create ~pmod_attributes ~pmod_loc ~pmod_desc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "module_expr"
        ; data = Record [| pmod_attributes; pmod_loc; pmod_desc |]
        } ->
          Helpers.Option.bind (Data.to_node pmod_attributes) ~f:(fun pmod_attributes ->
            Helpers.Option.bind (Data.to_location pmod_loc) ~f:(fun pmod_loc ->
              Helpers.Option.bind (Data.to_node pmod_desc) ~f:(fun pmod_desc ->
                Some { pmod_attributes; pmod_loc; pmod_desc }
          )))
      | _ -> None
  end

  module With_constraint = struct
    type t = with_constraint

    type concrete =
      | Pwith_modsubst of longident_loc * longident_loc
      | Pwith_typesubst of type_declaration * longident_loc
      | Pwith_module of longident_loc * longident_loc
      | Pwith_type of type_declaration * longident_loc

    let pwith_modsubst x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_modsubst"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })
    let pwith_typesubst x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_typesubst"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })
    let pwith_module x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_module"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })
    let pwith_type x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_type"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })

    let of_concrete c =
      match c with
      | Pwith_modsubst (x1, x2) ->
        pwith_modsubst x1 x2
      | Pwith_typesubst (x1, x2) ->
        pwith_typesubst x1 x2
      | Pwith_module (x1, x2) ->
        pwith_module x1 x2
      | Pwith_type (x1, x2) ->
        pwith_type x1 x2

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "with_constraint"; data } ->
        begin
          match data with
          | Variant { tag = "Pwith_modsubst"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pwith_modsubst (x1, x2))
            ))
          | Variant { tag = "Pwith_typesubst"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pwith_typesubst (x1, x2))
            ))
          | Variant { tag = "Pwith_module"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pwith_module (x1, x2))
            ))
          | Variant { tag = "Pwith_type"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pwith_type (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Include_declaration = struct
    type t = module_expr include_infos
  end

  module Include_description = struct
    type t = module_type include_infos
  end

  module Include_infos = struct
    type 'a t = 'a include_infos

    type 'a concrete =
      { pincl_attributes : attributes
      ; pincl_loc : Astlib.Location.t
      ; pincl_mod : 'a
      }

    let create ~pincl_attributes ~pincl_loc ~pincl_mod =
      let fields =
        [| Data.of_node pincl_attributes
         ; Data.of_location pincl_loc
         ; Data.of_node pincl_mod
        |]
      in
      node "include_infos" (Record fields)

    let of_concrete { pincl_attributes; pincl_loc; pincl_mod } =
      create ~pincl_attributes ~pincl_loc ~pincl_mod

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "include_infos"
        ; data = Record [| pincl_attributes; pincl_loc; pincl_mod |]
        } ->
          Helpers.Option.bind (Data.to_node pincl_attributes) ~f:(fun pincl_attributes ->
            Helpers.Option.bind (Data.to_location pincl_loc) ~f:(fun pincl_loc ->
              Helpers.Option.bind (Data.to_node pincl_mod) ~f:(fun pincl_mod ->
                Some { pincl_attributes; pincl_loc; pincl_mod }
          )))
      | _ -> None
  end

  module Open_description = struct
    type t = open_description

    type concrete =
      { popen_attributes : attributes
      ; popen_loc : Astlib.Location.t
      ; popen_override : override_flag
      ; popen_lid : longident_loc
      }

    let create ~popen_attributes ~popen_loc ~popen_override ~popen_lid =
      let fields =
        [| Data.of_node popen_attributes
         ; Data.of_location popen_loc
         ; Data.of_node popen_override
         ; (Data.of_loc ~f:Data.of_node) popen_lid
        |]
      in
      node "open_description" (Record fields)

    let of_concrete { popen_attributes; popen_loc; popen_override; popen_lid } =
      create ~popen_attributes ~popen_loc ~popen_override ~popen_lid

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "open_description"
        ; data = Record [| popen_attributes; popen_loc; popen_override; popen_lid |]
        } ->
          Helpers.Option.bind (Data.to_node popen_attributes) ~f:(fun popen_attributes ->
            Helpers.Option.bind (Data.to_location popen_loc) ~f:(fun popen_loc ->
              Helpers.Option.bind (Data.to_node popen_override) ~f:(fun popen_override ->
                Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) popen_lid) ~f:(fun popen_lid ->
                  Some { popen_attributes; popen_loc; popen_override; popen_lid }
          ))))
      | _ -> None
  end

  module Module_type_declaration = struct
    type t = module_type_declaration

    type concrete =
      { pmtd_loc : Astlib.Location.t
      ; pmtd_attributes : attributes
      ; pmtd_type : module_type option
      ; pmtd_name : string Astlib.Loc.t
      }

    let create ~pmtd_loc ~pmtd_attributes ~pmtd_type ~pmtd_name =
      let fields =
        [| Data.of_location pmtd_loc
         ; Data.of_node pmtd_attributes
         ; (Data.of_option ~f:Data.of_node) pmtd_type
         ; (Data.of_loc ~f:Data.of_string) pmtd_name
        |]
      in
      node "module_type_declaration" (Record fields)

    let of_concrete { pmtd_loc; pmtd_attributes; pmtd_type; pmtd_name } =
      create ~pmtd_loc ~pmtd_attributes ~pmtd_type ~pmtd_name

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "module_type_declaration"
        ; data = Record [| pmtd_loc; pmtd_attributes; pmtd_type; pmtd_name |]
        } ->
          Helpers.Option.bind (Data.to_location pmtd_loc) ~f:(fun pmtd_loc ->
            Helpers.Option.bind (Data.to_node pmtd_attributes) ~f:(fun pmtd_attributes ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) pmtd_type) ~f:(fun pmtd_type ->
                Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pmtd_name) ~f:(fun pmtd_name ->
                  Some { pmtd_loc; pmtd_attributes; pmtd_type; pmtd_name }
          ))))
      | _ -> None
  end

  module Module_declaration = struct
    type t = module_declaration

    type concrete =
      { pmd_loc : Astlib.Location.t
      ; pmd_attributes : attributes
      ; pmd_type : module_type
      ; pmd_name : string Astlib.Loc.t
      }

    let create ~pmd_loc ~pmd_attributes ~pmd_type ~pmd_name =
      let fields =
        [| Data.of_location pmd_loc
         ; Data.of_node pmd_attributes
         ; Data.of_node pmd_type
         ; (Data.of_loc ~f:Data.of_string) pmd_name
        |]
      in
      node "module_declaration" (Record fields)

    let of_concrete { pmd_loc; pmd_attributes; pmd_type; pmd_name } =
      create ~pmd_loc ~pmd_attributes ~pmd_type ~pmd_name

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "module_declaration"
        ; data = Record [| pmd_loc; pmd_attributes; pmd_type; pmd_name |]
        } ->
          Helpers.Option.bind (Data.to_location pmd_loc) ~f:(fun pmd_loc ->
            Helpers.Option.bind (Data.to_node pmd_attributes) ~f:(fun pmd_attributes ->
              Helpers.Option.bind (Data.to_node pmd_type) ~f:(fun pmd_type ->
                Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pmd_name) ~f:(fun pmd_name ->
                  Some { pmd_loc; pmd_attributes; pmd_type; pmd_name }
          ))))
      | _ -> None
  end

  module Signature_item_desc = struct
    type t = signature_item_desc

    type concrete =
      | Psig_extension of attributes * extension
      | Psig_attribute of attribute
      | Psig_class_type of class_type_declaration list
      | Psig_class of class_description list
      | Psig_include of include_description
      | Psig_open of open_description
      | Psig_modtype of module_type_declaration
      | Psig_recmodule of module_declaration list
      | Psig_module of module_declaration
      | Psig_exception of extension_constructor
      | Psig_typext of type_extension
      | Psig_type of type_declaration list * rec_flag
      | Psig_value of value_description

    let psig_extension x1 x2 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_extension"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let psig_attribute x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig_class_type x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_class_type"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let psig_class x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_class"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let psig_include x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_include"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig_open x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_open"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig_modtype x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_modtype"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig_recmodule x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_recmodule"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let psig_module x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_module"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig_exception x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_exception"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig_typext x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_typext"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig_type x1 x2 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_type"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let psig_value x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_value"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete c =
      match c with
      | Psig_extension (x1, x2) ->
        psig_extension x1 x2
      | Psig_attribute (x1) ->
        psig_attribute x1
      | Psig_class_type (x1) ->
        psig_class_type x1
      | Psig_class (x1) ->
        psig_class x1
      | Psig_include (x1) ->
        psig_include x1
      | Psig_open (x1) ->
        psig_open x1
      | Psig_modtype (x1) ->
        psig_modtype x1
      | Psig_recmodule (x1) ->
        psig_recmodule x1
      | Psig_module (x1) ->
        psig_module x1
      | Psig_exception (x1) ->
        psig_exception x1
      | Psig_typext (x1) ->
        psig_typext x1
      | Psig_type (x1, x2) ->
        psig_type x1 x2
      | Psig_value (x1) ->
        psig_value x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "signature_item_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Psig_extension"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Psig_extension (x1, x2))
            ))
          | Variant { tag = "Psig_attribute"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_attribute (x1))
            )
          | Variant { tag = "Psig_class_type"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Psig_class_type (x1))
            )
          | Variant { tag = "Psig_class"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Psig_class (x1))
            )
          | Variant { tag = "Psig_include"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_include (x1))
            )
          | Variant { tag = "Psig_open"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_open (x1))
            )
          | Variant { tag = "Psig_modtype"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_modtype (x1))
            )
          | Variant { tag = "Psig_recmodule"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Psig_recmodule (x1))
            )
          | Variant { tag = "Psig_module"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_module (x1))
            )
          | Variant { tag = "Psig_exception"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_exception (x1))
            )
          | Variant { tag = "Psig_typext"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_typext (x1))
            )
          | Variant { tag = "Psig_type"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Psig_type (x1, x2))
            ))
          | Variant { tag = "Psig_value"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_value (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Signature_item = struct
    type t = signature_item

    type concrete =
      { psig_loc : Astlib.Location.t
      ; psig_desc : signature_item_desc
      }

    let create ~psig_loc ~psig_desc =
      let fields =
        [| Data.of_location psig_loc
         ; Data.of_node psig_desc
        |]
      in
      node "signature_item" (Record fields)

    let of_concrete { psig_loc; psig_desc } =
      create ~psig_loc ~psig_desc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "signature_item"
        ; data = Record [| psig_loc; psig_desc |]
        } ->
          Helpers.Option.bind (Data.to_location psig_loc) ~f:(fun psig_loc ->
            Helpers.Option.bind (Data.to_node psig_desc) ~f:(fun psig_desc ->
              Some { psig_loc; psig_desc }
          ))
      | _ -> None
  end

  module Signature = struct
    type t = signature

    type concrete = signature_item list

    let create =
      let data = (Data.of_list ~f:Data.of_node) in
      fun x -> node "signature" (data x)

    let of_concrete = create

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "signature"; data } -> (Data.to_list ~f:Data.to_node) data
      | _ -> None
  end

  module Module_type_desc = struct
    type t = module_type_desc

    type concrete =
      | Pmty_alias of longident_loc
      | Pmty_extension of extension
      | Pmty_typeof of module_expr
      | Pmty_with of with_constraint list * module_type
      | Pmty_functor of module_type * module_type option * string Astlib.Loc.t
      | Pmty_signature of signature
      | Pmty_ident of longident_loc

    let pmty_alias x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_alias"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
            |]
          })
    let pmty_extension x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pmty_typeof x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_typeof"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pmty_with x1 x2 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_with"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let pmty_functor x1 x2 x3 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_functor"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
             ; (Data.of_loc ~f:Data.of_string) x3
            |]
          })
    let pmty_signature x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_signature"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pmty_ident x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_ident"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
            |]
          })

    let of_concrete c =
      match c with
      | Pmty_alias (x1) ->
        pmty_alias x1
      | Pmty_extension (x1) ->
        pmty_extension x1
      | Pmty_typeof (x1) ->
        pmty_typeof x1
      | Pmty_with (x1, x2) ->
        pmty_with x1 x2
      | Pmty_functor (x1, x2, x3) ->
        pmty_functor x1 x2 x3
      | Pmty_signature (x1) ->
        pmty_signature x1
      | Pmty_ident (x1) ->
        pmty_ident x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "module_type_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pmty_alias"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pmty_alias (x1))
            )
          | Variant { tag = "Pmty_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pmty_extension (x1))
            )
          | Variant { tag = "Pmty_typeof"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pmty_typeof (x1))
            )
          | Variant { tag = "Pmty_with"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pmty_with (x1, x2))
            ))
          | Variant { tag = "Pmty_functor"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x3) ~f:(fun x3 ->
                  Some (Pmty_functor (x1, x2, x3))
            )))
          | Variant { tag = "Pmty_signature"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pmty_signature (x1))
            )
          | Variant { tag = "Pmty_ident"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pmty_ident (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Module_type = struct
    type t = module_type

    type concrete =
      { pmty_attributes : attributes
      ; pmty_loc : Astlib.Location.t
      ; pmty_desc : module_type_desc
      }

    let create ~pmty_attributes ~pmty_loc ~pmty_desc =
      let fields =
        [| Data.of_node pmty_attributes
         ; Data.of_location pmty_loc
         ; Data.of_node pmty_desc
        |]
      in
      node "module_type" (Record fields)

    let of_concrete { pmty_attributes; pmty_loc; pmty_desc } =
      create ~pmty_attributes ~pmty_loc ~pmty_desc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "module_type"
        ; data = Record [| pmty_attributes; pmty_loc; pmty_desc |]
        } ->
          Helpers.Option.bind (Data.to_node pmty_attributes) ~f:(fun pmty_attributes ->
            Helpers.Option.bind (Data.to_location pmty_loc) ~f:(fun pmty_loc ->
              Helpers.Option.bind (Data.to_node pmty_desc) ~f:(fun pmty_desc ->
                Some { pmty_attributes; pmty_loc; pmty_desc }
          )))
      | _ -> None
  end

  module Class_declaration = struct
    type t = class_expr class_infos
  end

  module Class_field_kind = struct
    type t = class_field_kind

    type concrete =
      | Cfk_concrete of expression * override_flag
      | Cfk_virtual of core_type

    let cfk_concrete x1 x2 =
      node "class_field_kind"
        (Variant
          { tag = "Cfk_concrete"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let cfk_virtual x1 =
      node "class_field_kind"
        (Variant
          { tag = "Cfk_virtual"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete c =
      match c with
      | Cfk_concrete (x1, x2) ->
        cfk_concrete x1 x2
      | Cfk_virtual (x1) ->
        cfk_virtual x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_field_kind"; data } ->
        begin
          match data with
          | Variant { tag = "Cfk_concrete"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Cfk_concrete (x1, x2))
            ))
          | Variant { tag = "Cfk_virtual"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Cfk_virtual (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Class_field_desc = struct
    type t = class_field_desc

    type concrete =
      | Pcf_extension of extension
      | Pcf_attribute of attribute
      | Pcf_initializer of expression
      | Pcf_constraint of (core_type * core_type)
      | Pcf_method of (class_field_kind * private_flag * label Astlib.Loc.t)
      | Pcf_val of (class_field_kind * mutable_flag * label Astlib.Loc.t)
      | Pcf_inherit of string Astlib.Loc.t option * class_expr * override_flag

    let pcf_extension x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pcf_attribute x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pcf_initializer x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_initializer"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pcf_constraint x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_constraint"
          ; args =
            [| (Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node) x1
            |]
          })
    let pcf_method x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_method"
          ; args =
            [| (Data.of_tuple3 ~f1:Data.of_node ~f2:Data.of_node ~f3:(Data.of_loc ~f:Data.of_string)) x1
            |]
          })
    let pcf_val x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_val"
          ; args =
            [| (Data.of_tuple3 ~f1:Data.of_node ~f2:Data.of_node ~f3:(Data.of_loc ~f:Data.of_string)) x1
            |]
          })
    let pcf_inherit x1 x2 x3 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_inherit"
          ; args =
            [| (Data.of_option ~f:(Data.of_loc ~f:Data.of_string)) x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })

    let of_concrete c =
      match c with
      | Pcf_extension (x1) ->
        pcf_extension x1
      | Pcf_attribute (x1) ->
        pcf_attribute x1
      | Pcf_initializer (x1) ->
        pcf_initializer x1
      | Pcf_constraint (x1) ->
        pcf_constraint x1
      | Pcf_method (x1) ->
        pcf_method x1
      | Pcf_val (x1) ->
        pcf_val x1
      | Pcf_inherit (x1, x2, x3) ->
        pcf_inherit x1 x2 x3

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_field_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pcf_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pcf_extension (x1))
            )
          | Variant { tag = "Pcf_attribute"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pcf_attribute (x1))
            )
          | Variant { tag = "Pcf_initializer"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pcf_initializer (x1))
            )
          | Variant { tag = "Pcf_constraint"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pcf_constraint (x1))
            )
          | Variant { tag = "Pcf_method"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple3 ~f1:Data.to_node ~f2:Data.to_node ~f3:(Data.to_loc ~f:Data.to_string)) x1) ~f:(fun x1 ->
              Some (Pcf_method (x1))
            )
          | Variant { tag = "Pcf_val"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple3 ~f1:Data.to_node ~f2:Data.to_node ~f3:(Data.to_loc ~f:Data.to_string)) x1) ~f:(fun x1 ->
              Some (Pcf_val (x1))
            )
          | Variant { tag = "Pcf_inherit"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind ((Data.to_option ~f:(Data.to_loc ~f:Data.to_string)) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pcf_inherit (x1, x2, x3))
            )))
        | _ -> None
        end
      | _ -> None
  end

  module Class_field = struct
    type t = class_field

    type concrete =
      { pcf_attributes : attributes
      ; pcf_loc : Astlib.Location.t
      ; pcf_desc : class_field_desc
      }

    let create ~pcf_attributes ~pcf_loc ~pcf_desc =
      let fields =
        [| Data.of_node pcf_attributes
         ; Data.of_location pcf_loc
         ; Data.of_node pcf_desc
        |]
      in
      node "class_field" (Record fields)

    let of_concrete { pcf_attributes; pcf_loc; pcf_desc } =
      create ~pcf_attributes ~pcf_loc ~pcf_desc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_field"
        ; data = Record [| pcf_attributes; pcf_loc; pcf_desc |]
        } ->
          Helpers.Option.bind (Data.to_node pcf_attributes) ~f:(fun pcf_attributes ->
            Helpers.Option.bind (Data.to_location pcf_loc) ~f:(fun pcf_loc ->
              Helpers.Option.bind (Data.to_node pcf_desc) ~f:(fun pcf_desc ->
                Some { pcf_attributes; pcf_loc; pcf_desc }
          )))
      | _ -> None
  end

  module Class_structure = struct
    type t = class_structure

    type concrete =
      { pcstr_fields : class_field list
      ; pcstr_self : pattern
      }

    let create ~pcstr_fields ~pcstr_self =
      let fields =
        [| (Data.of_list ~f:Data.of_node) pcstr_fields
         ; Data.of_node pcstr_self
        |]
      in
      node "class_structure" (Record fields)

    let of_concrete { pcstr_fields; pcstr_self } =
      create ~pcstr_fields ~pcstr_self

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_structure"
        ; data = Record [| pcstr_fields; pcstr_self |]
        } ->
          Helpers.Option.bind ((Data.to_list ~f:Data.to_node) pcstr_fields) ~f:(fun pcstr_fields ->
            Helpers.Option.bind (Data.to_node pcstr_self) ~f:(fun pcstr_self ->
              Some { pcstr_fields; pcstr_self }
          ))
      | _ -> None
  end

  module Class_expr_desc = struct
    type t = class_expr_desc

    type concrete =
      | Pcl_open of class_expr * longident_loc * override_flag
      | Pcl_extension of extension
      | Pcl_constraint of class_type * class_expr
      | Pcl_let of class_expr * value_binding list * rec_flag
      | Pcl_apply of (expression * arg_label) list * class_expr
      | Pcl_fun of class_expr * pattern * expression option * arg_label
      | Pcl_structure of class_structure
      | Pcl_constr of core_type list * longident_loc

    let pcl_open x1 x2 x3 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_open"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let pcl_extension x1 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pcl_constraint x1 x2 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pcl_let x1 x2 x3 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_let"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let pcl_apply x1 x2 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_apply"
          ; args =
            [| (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) x1
             ; Data.of_node x2
            |]
          })
    let pcl_fun x1 x2 x3 x4 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_fun"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; (Data.of_option ~f:Data.of_node) x3
             ; Data.of_node x4
            |]
          })
    let pcl_structure x1 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_structure"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pcl_constr x1 x2 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_constr"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })

    let of_concrete c =
      match c with
      | Pcl_open (x1, x2, x3) ->
        pcl_open x1 x2 x3
      | Pcl_extension (x1) ->
        pcl_extension x1
      | Pcl_constraint (x1, x2) ->
        pcl_constraint x1 x2
      | Pcl_let (x1, x2, x3) ->
        pcl_let x1 x2 x3
      | Pcl_apply (x1, x2) ->
        pcl_apply x1 x2
      | Pcl_fun (x1, x2, x3, x4) ->
        pcl_fun x1 x2 x3 x4
      | Pcl_structure (x1) ->
        pcl_structure x1
      | Pcl_constr (x1, x2) ->
        pcl_constr x1 x2

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_expr_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pcl_open"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pcl_open (x1, x2, x3))
            )))
          | Variant { tag = "Pcl_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pcl_extension (x1))
            )
          | Variant { tag = "Pcl_constraint"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pcl_constraint (x1, x2))
            ))
          | Variant { tag = "Pcl_let"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pcl_let (x1, x2, x3))
            )))
          | Variant { tag = "Pcl_apply"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pcl_apply (x1, x2))
            ))
          | Variant { tag = "Pcl_fun"; args = [| x1; x2; x3; x4 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x3) ~f:(fun x3 ->
                  Helpers.Option.bind (Data.to_node x4) ~f:(fun x4 ->
                    Some (Pcl_fun (x1, x2, x3, x4))
            ))))
          | Variant { tag = "Pcl_structure"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pcl_structure (x1))
            )
          | Variant { tag = "Pcl_constr"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pcl_constr (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Class_expr = struct
    type t = class_expr

    type concrete =
      { pcl_attributes : attributes
      ; pcl_loc : Astlib.Location.t
      ; pcl_desc : class_expr_desc
      }

    let create ~pcl_attributes ~pcl_loc ~pcl_desc =
      let fields =
        [| Data.of_node pcl_attributes
         ; Data.of_location pcl_loc
         ; Data.of_node pcl_desc
        |]
      in
      node "class_expr" (Record fields)

    let of_concrete { pcl_attributes; pcl_loc; pcl_desc } =
      create ~pcl_attributes ~pcl_loc ~pcl_desc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_expr"
        ; data = Record [| pcl_attributes; pcl_loc; pcl_desc |]
        } ->
          Helpers.Option.bind (Data.to_node pcl_attributes) ~f:(fun pcl_attributes ->
            Helpers.Option.bind (Data.to_location pcl_loc) ~f:(fun pcl_loc ->
              Helpers.Option.bind (Data.to_node pcl_desc) ~f:(fun pcl_desc ->
                Some { pcl_attributes; pcl_loc; pcl_desc }
          )))
      | _ -> None
  end

  module Class_type_declaration = struct
    type t = class_type class_infos
  end

  module Class_description = struct
    type t = class_type class_infos
  end

  module Class_infos = struct
    type 'a t = 'a class_infos

    type 'a concrete =
      { pci_attributes : attributes
      ; pci_loc : Astlib.Location.t
      ; pci_expr : 'a
      ; pci_name : string Astlib.Loc.t
      ; pci_params : (variance * core_type) list
      ; pci_virt : virtual_flag
      }

    let create ~pci_attributes ~pci_loc ~pci_expr ~pci_name ~pci_params ~pci_virt =
      let fields =
        [| Data.of_node pci_attributes
         ; Data.of_location pci_loc
         ; Data.of_node pci_expr
         ; (Data.of_loc ~f:Data.of_string) pci_name
         ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) pci_params
         ; Data.of_node pci_virt
        |]
      in
      node "class_infos" (Record fields)

    let of_concrete { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt } =
      create ~pci_attributes ~pci_loc ~pci_expr ~pci_name ~pci_params ~pci_virt

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_infos"
        ; data = Record [| pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt |]
        } ->
          Helpers.Option.bind (Data.to_node pci_attributes) ~f:(fun pci_attributes ->
            Helpers.Option.bind (Data.to_location pci_loc) ~f:(fun pci_loc ->
              Helpers.Option.bind (Data.to_node pci_expr) ~f:(fun pci_expr ->
                Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pci_name) ~f:(fun pci_name ->
                  Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) pci_params) ~f:(fun pci_params ->
                    Helpers.Option.bind (Data.to_node pci_virt) ~f:(fun pci_virt ->
                      Some { pci_attributes; pci_loc; pci_expr; pci_name; pci_params; pci_virt }
          ))))))
      | _ -> None
  end

  module Class_type_field_desc = struct
    type t = class_type_field_desc

    type concrete =
      | Pctf_extension of extension
      | Pctf_attribute of attribute
      | Pctf_constraint of (core_type * core_type)
      | Pctf_method of (core_type * virtual_flag * private_flag * label Astlib.Loc.t)
      | Pctf_val of (core_type * virtual_flag * mutable_flag * label Astlib.Loc.t)
      | Pctf_inherit of class_type

    let pctf_extension x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pctf_attribute x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pctf_constraint x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_constraint"
          ; args =
            [| (Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node) x1
            |]
          })
    let pctf_method x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_method"
          ; args =
            [| (Data.of_tuple4 ~f1:Data.of_node ~f2:Data.of_node ~f3:Data.of_node ~f4:(Data.of_loc ~f:Data.of_string)) x1
            |]
          })
    let pctf_val x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_val"
          ; args =
            [| (Data.of_tuple4 ~f1:Data.of_node ~f2:Data.of_node ~f3:Data.of_node ~f4:(Data.of_loc ~f:Data.of_string)) x1
            |]
          })
    let pctf_inherit x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_inherit"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete c =
      match c with
      | Pctf_extension (x1) ->
        pctf_extension x1
      | Pctf_attribute (x1) ->
        pctf_attribute x1
      | Pctf_constraint (x1) ->
        pctf_constraint x1
      | Pctf_method (x1) ->
        pctf_method x1
      | Pctf_val (x1) ->
        pctf_val x1
      | Pctf_inherit (x1) ->
        pctf_inherit x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_type_field_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pctf_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pctf_extension (x1))
            )
          | Variant { tag = "Pctf_attribute"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pctf_attribute (x1))
            )
          | Variant { tag = "Pctf_constraint"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pctf_constraint (x1))
            )
          | Variant { tag = "Pctf_method"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple4 ~f1:Data.to_node ~f2:Data.to_node ~f3:Data.to_node ~f4:(Data.to_loc ~f:Data.to_string)) x1) ~f:(fun x1 ->
              Some (Pctf_method (x1))
            )
          | Variant { tag = "Pctf_val"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple4 ~f1:Data.to_node ~f2:Data.to_node ~f3:Data.to_node ~f4:(Data.to_loc ~f:Data.to_string)) x1) ~f:(fun x1 ->
              Some (Pctf_val (x1))
            )
          | Variant { tag = "Pctf_inherit"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pctf_inherit (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Class_type_field = struct
    type t = class_type_field

    type concrete =
      { pctf_attributes : attributes
      ; pctf_loc : Astlib.Location.t
      ; pctf_desc : class_type_field_desc
      }

    let create ~pctf_attributes ~pctf_loc ~pctf_desc =
      let fields =
        [| Data.of_node pctf_attributes
         ; Data.of_location pctf_loc
         ; Data.of_node pctf_desc
        |]
      in
      node "class_type_field" (Record fields)

    let of_concrete { pctf_attributes; pctf_loc; pctf_desc } =
      create ~pctf_attributes ~pctf_loc ~pctf_desc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_type_field"
        ; data = Record [| pctf_attributes; pctf_loc; pctf_desc |]
        } ->
          Helpers.Option.bind (Data.to_node pctf_attributes) ~f:(fun pctf_attributes ->
            Helpers.Option.bind (Data.to_location pctf_loc) ~f:(fun pctf_loc ->
              Helpers.Option.bind (Data.to_node pctf_desc) ~f:(fun pctf_desc ->
                Some { pctf_attributes; pctf_loc; pctf_desc }
          )))
      | _ -> None
  end

  module Class_signature = struct
    type t = class_signature

    type concrete =
      { pcsig_fields : class_type_field list
      ; pcsig_self : core_type
      }

    let create ~pcsig_fields ~pcsig_self =
      let fields =
        [| (Data.of_list ~f:Data.of_node) pcsig_fields
         ; Data.of_node pcsig_self
        |]
      in
      node "class_signature" (Record fields)

    let of_concrete { pcsig_fields; pcsig_self } =
      create ~pcsig_fields ~pcsig_self

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_signature"
        ; data = Record [| pcsig_fields; pcsig_self |]
        } ->
          Helpers.Option.bind ((Data.to_list ~f:Data.to_node) pcsig_fields) ~f:(fun pcsig_fields ->
            Helpers.Option.bind (Data.to_node pcsig_self) ~f:(fun pcsig_self ->
              Some { pcsig_fields; pcsig_self }
          ))
      | _ -> None
  end

  module Class_type_desc = struct
    type t = class_type_desc

    type concrete =
      | Pcty_open of class_type * longident_loc * override_flag
      | Pcty_extension of extension
      | Pcty_arrow of class_type * core_type * arg_label
      | Pcty_signature of class_signature
      | Pcty_constr of core_type list * longident_loc

    let pcty_open x1 x2 x3 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_open"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let pcty_extension x1 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pcty_arrow x1 x2 x3 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_arrow"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let pcty_signature x1 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_signature"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pcty_constr x1 x2 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_constr"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })

    let of_concrete c =
      match c with
      | Pcty_open (x1, x2, x3) ->
        pcty_open x1 x2 x3
      | Pcty_extension (x1) ->
        pcty_extension x1
      | Pcty_arrow (x1, x2, x3) ->
        pcty_arrow x1 x2 x3
      | Pcty_signature (x1) ->
        pcty_signature x1
      | Pcty_constr (x1, x2) ->
        pcty_constr x1 x2

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_type_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pcty_open"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pcty_open (x1, x2, x3))
            )))
          | Variant { tag = "Pcty_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pcty_extension (x1))
            )
          | Variant { tag = "Pcty_arrow"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pcty_arrow (x1, x2, x3))
            )))
          | Variant { tag = "Pcty_signature"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pcty_signature (x1))
            )
          | Variant { tag = "Pcty_constr"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pcty_constr (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Class_type = struct
    type t = class_type

    type concrete =
      { pcty_attributes : attributes
      ; pcty_loc : Astlib.Location.t
      ; pcty_desc : class_type_desc
      }

    let create ~pcty_attributes ~pcty_loc ~pcty_desc =
      let fields =
        [| Data.of_node pcty_attributes
         ; Data.of_location pcty_loc
         ; Data.of_node pcty_desc
        |]
      in
      node "class_type" (Record fields)

    let of_concrete { pcty_attributes; pcty_loc; pcty_desc } =
      create ~pcty_attributes ~pcty_loc ~pcty_desc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_type"
        ; data = Record [| pcty_attributes; pcty_loc; pcty_desc |]
        } ->
          Helpers.Option.bind (Data.to_node pcty_attributes) ~f:(fun pcty_attributes ->
            Helpers.Option.bind (Data.to_location pcty_loc) ~f:(fun pcty_loc ->
              Helpers.Option.bind (Data.to_node pcty_desc) ~f:(fun pcty_desc ->
                Some { pcty_attributes; pcty_loc; pcty_desc }
          )))
      | _ -> None
  end

  module Extension_constructor_kind = struct
    type t = extension_constructor_kind

    type concrete =
      | Pext_rebind of longident_loc
      | Pext_decl of core_type option * constructor_arguments

    let pext_rebind x1 =
      node "extension_constructor_kind"
        (Variant
          { tag = "Pext_rebind"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
            |]
          })
    let pext_decl x1 x2 =
      node "extension_constructor_kind"
        (Variant
          { tag = "Pext_decl"
          ; args =
            [| (Data.of_option ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })

    let of_concrete c =
      match c with
      | Pext_rebind (x1) ->
        pext_rebind x1
      | Pext_decl (x1, x2) ->
        pext_decl x1 x2

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "extension_constructor_kind"; data } ->
        begin
          match data with
          | Variant { tag = "Pext_rebind"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pext_rebind (x1))
            )
          | Variant { tag = "Pext_decl"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pext_decl (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Extension_constructor = struct
    type t = extension_constructor

    type concrete =
      { pext_attributes : attributes
      ; pext_loc : Astlib.Location.t
      ; pext_kind : extension_constructor_kind
      ; pext_name : string Astlib.Loc.t
      }

    let create ~pext_attributes ~pext_loc ~pext_kind ~pext_name =
      let fields =
        [| Data.of_node pext_attributes
         ; Data.of_location pext_loc
         ; Data.of_node pext_kind
         ; (Data.of_loc ~f:Data.of_string) pext_name
        |]
      in
      node "extension_constructor" (Record fields)

    let of_concrete { pext_attributes; pext_loc; pext_kind; pext_name } =
      create ~pext_attributes ~pext_loc ~pext_kind ~pext_name

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "extension_constructor"
        ; data = Record [| pext_attributes; pext_loc; pext_kind; pext_name |]
        } ->
          Helpers.Option.bind (Data.to_node pext_attributes) ~f:(fun pext_attributes ->
            Helpers.Option.bind (Data.to_location pext_loc) ~f:(fun pext_loc ->
              Helpers.Option.bind (Data.to_node pext_kind) ~f:(fun pext_kind ->
                Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pext_name) ~f:(fun pext_name ->
                  Some { pext_attributes; pext_loc; pext_kind; pext_name }
          ))))
      | _ -> None
  end

  module Type_extension = struct
    type t = type_extension

    type concrete =
      { ptyext_attributes : attributes
      ; ptyext_private : private_flag
      ; ptyext_constructors : extension_constructor list
      ; ptyext_params : (variance * core_type) list
      ; ptyext_path : longident_loc
      }

    let create ~ptyext_attributes ~ptyext_private ~ptyext_constructors ~ptyext_params ~ptyext_path =
      let fields =
        [| Data.of_node ptyext_attributes
         ; Data.of_node ptyext_private
         ; (Data.of_list ~f:Data.of_node) ptyext_constructors
         ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) ptyext_params
         ; (Data.of_loc ~f:Data.of_node) ptyext_path
        |]
      in
      node "type_extension" (Record fields)

    let of_concrete { ptyext_attributes; ptyext_private; ptyext_constructors; ptyext_params; ptyext_path } =
      create ~ptyext_attributes ~ptyext_private ~ptyext_constructors ~ptyext_params ~ptyext_path

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "type_extension"
        ; data = Record [| ptyext_attributes; ptyext_private; ptyext_constructors; ptyext_params; ptyext_path |]
        } ->
          Helpers.Option.bind (Data.to_node ptyext_attributes) ~f:(fun ptyext_attributes ->
            Helpers.Option.bind (Data.to_node ptyext_private) ~f:(fun ptyext_private ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) ptyext_constructors) ~f:(fun ptyext_constructors ->
                Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) ptyext_params) ~f:(fun ptyext_params ->
                  Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) ptyext_path) ~f:(fun ptyext_path ->
                    Some { ptyext_attributes; ptyext_private; ptyext_constructors; ptyext_params; ptyext_path }
          )))))
      | _ -> None
  end

  module Constructor_arguments = struct
    type t = constructor_arguments

    type concrete =
      | Pcstr_record of label_declaration list
      | Pcstr_tuple of core_type list

    let pcstr_record x1 =
      node "constructor_arguments"
        (Variant
          { tag = "Pcstr_record"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let pcstr_tuple x1 =
      node "constructor_arguments"
        (Variant
          { tag = "Pcstr_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })

    let of_concrete c =
      match c with
      | Pcstr_record (x1) ->
        pcstr_record x1
      | Pcstr_tuple (x1) ->
        pcstr_tuple x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "constructor_arguments"; data } ->
        begin
          match data with
          | Variant { tag = "Pcstr_record"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pcstr_record (x1))
            )
          | Variant { tag = "Pcstr_tuple"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pcstr_tuple (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Constructor_declaration = struct
    type t = constructor_declaration

    type concrete =
      { pcd_attributes : attributes
      ; pcd_loc : Astlib.Location.t
      ; pcd_res : core_type option
      ; pcd_args : constructor_arguments
      ; pcd_name : string Astlib.Loc.t
      }

    let create ~pcd_attributes ~pcd_loc ~pcd_res ~pcd_args ~pcd_name =
      let fields =
        [| Data.of_node pcd_attributes
         ; Data.of_location pcd_loc
         ; (Data.of_option ~f:Data.of_node) pcd_res
         ; Data.of_node pcd_args
         ; (Data.of_loc ~f:Data.of_string) pcd_name
        |]
      in
      node "constructor_declaration" (Record fields)

    let of_concrete { pcd_attributes; pcd_loc; pcd_res; pcd_args; pcd_name } =
      create ~pcd_attributes ~pcd_loc ~pcd_res ~pcd_args ~pcd_name

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "constructor_declaration"
        ; data = Record [| pcd_attributes; pcd_loc; pcd_res; pcd_args; pcd_name |]
        } ->
          Helpers.Option.bind (Data.to_node pcd_attributes) ~f:(fun pcd_attributes ->
            Helpers.Option.bind (Data.to_location pcd_loc) ~f:(fun pcd_loc ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) pcd_res) ~f:(fun pcd_res ->
                Helpers.Option.bind (Data.to_node pcd_args) ~f:(fun pcd_args ->
                  Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pcd_name) ~f:(fun pcd_name ->
                    Some { pcd_attributes; pcd_loc; pcd_res; pcd_args; pcd_name }
          )))))
      | _ -> None
  end

  module Label_declaration = struct
    type t = label_declaration

    type concrete =
      { pld_attributes : attributes
      ; pld_loc : Astlib.Location.t
      ; pld_type : core_type
      ; pld_mutable : mutable_flag
      ; pld_name : string Astlib.Loc.t
      }

    let create ~pld_attributes ~pld_loc ~pld_type ~pld_mutable ~pld_name =
      let fields =
        [| Data.of_node pld_attributes
         ; Data.of_location pld_loc
         ; Data.of_node pld_type
         ; Data.of_node pld_mutable
         ; (Data.of_loc ~f:Data.of_string) pld_name
        |]
      in
      node "label_declaration" (Record fields)

    let of_concrete { pld_attributes; pld_loc; pld_type; pld_mutable; pld_name } =
      create ~pld_attributes ~pld_loc ~pld_type ~pld_mutable ~pld_name

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "label_declaration"
        ; data = Record [| pld_attributes; pld_loc; pld_type; pld_mutable; pld_name |]
        } ->
          Helpers.Option.bind (Data.to_node pld_attributes) ~f:(fun pld_attributes ->
            Helpers.Option.bind (Data.to_location pld_loc) ~f:(fun pld_loc ->
              Helpers.Option.bind (Data.to_node pld_type) ~f:(fun pld_type ->
                Helpers.Option.bind (Data.to_node pld_mutable) ~f:(fun pld_mutable ->
                  Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pld_name) ~f:(fun pld_name ->
                    Some { pld_attributes; pld_loc; pld_type; pld_mutable; pld_name }
          )))))
      | _ -> None
  end

  module Type_kind = struct
    type t = type_kind

    type concrete =
      | Ptype_open
      | Ptype_record of label_declaration list
      | Ptype_variant of constructor_declaration list
      | Ptype_abstract

    let ptype_open =
      node "type_kind" (Variant { tag = "Ptype_open"; args = [||] })
    let ptype_record x1 =
      node "type_kind"
        (Variant
          { tag = "Ptype_record"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let ptype_variant x1 =
      node "type_kind"
        (Variant
          { tag = "Ptype_variant"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let ptype_abstract =
      node "type_kind" (Variant { tag = "Ptype_abstract"; args = [||] })

    let of_concrete c =
      match c with
      | Ptype_open -> ptype_open
      | Ptype_record (x1) ->
        ptype_record x1
      | Ptype_variant (x1) ->
        ptype_variant x1
      | Ptype_abstract -> ptype_abstract

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "type_kind"; data } ->
        begin
          match data with
          | Variant { tag = "Ptype_open"; args = [||] } -> Some Ptype_open
          | Variant { tag = "Ptype_record"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Ptype_record (x1))
            )
          | Variant { tag = "Ptype_variant"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Ptype_variant (x1))
            )
          | Variant { tag = "Ptype_abstract"; args = [||] } -> Some Ptype_abstract
        | _ -> None
        end
      | _ -> None
  end

  module Type_declaration = struct
    type t = type_declaration

    type concrete =
      { ptype_loc : Astlib.Location.t
      ; ptype_attributes : attributes
      ; ptype_manifest : core_type option
      ; ptype_private : private_flag
      ; ptype_kind : type_kind
      ; ptype_cstrs : (Astlib.Location.t * core_type * core_type) list
      ; ptype_params : (variance * core_type) list
      ; ptype_name : string Astlib.Loc.t
      }

    let create ~ptype_loc ~ptype_attributes ~ptype_manifest ~ptype_private ~ptype_kind ~ptype_cstrs ~ptype_params ~ptype_name =
      let fields =
        [| Data.of_location ptype_loc
         ; Data.of_node ptype_attributes
         ; (Data.of_option ~f:Data.of_node) ptype_manifest
         ; Data.of_node ptype_private
         ; Data.of_node ptype_kind
         ; (Data.of_list ~f:(Data.of_tuple3 ~f1:Data.of_location ~f2:Data.of_node ~f3:Data.of_node)) ptype_cstrs
         ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) ptype_params
         ; (Data.of_loc ~f:Data.of_string) ptype_name
        |]
      in
      node "type_declaration" (Record fields)

    let of_concrete { ptype_loc; ptype_attributes; ptype_manifest; ptype_private; ptype_kind; ptype_cstrs; ptype_params; ptype_name } =
      create ~ptype_loc ~ptype_attributes ~ptype_manifest ~ptype_private ~ptype_kind ~ptype_cstrs ~ptype_params ~ptype_name

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "type_declaration"
        ; data = Record [| ptype_loc; ptype_attributes; ptype_manifest; ptype_private; ptype_kind; ptype_cstrs; ptype_params; ptype_name |]
        } ->
          Helpers.Option.bind (Data.to_location ptype_loc) ~f:(fun ptype_loc ->
            Helpers.Option.bind (Data.to_node ptype_attributes) ~f:(fun ptype_attributes ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) ptype_manifest) ~f:(fun ptype_manifest ->
                Helpers.Option.bind (Data.to_node ptype_private) ~f:(fun ptype_private ->
                  Helpers.Option.bind (Data.to_node ptype_kind) ~f:(fun ptype_kind ->
                    Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple3 ~f1:Data.to_location ~f2:Data.to_node ~f3:Data.to_node)) ptype_cstrs) ~f:(fun ptype_cstrs ->
                      Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) ptype_params) ~f:(fun ptype_params ->
                        Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) ptype_name) ~f:(fun ptype_name ->
                          Some { ptype_loc; ptype_attributes; ptype_manifest; ptype_private; ptype_kind; ptype_cstrs; ptype_params; ptype_name }
          ))))))))
      | _ -> None
  end

  module Value_description = struct
    type t = value_description

    type concrete =
      { pval_loc : Astlib.Location.t
      ; pval_attributes : attributes
      ; pval_prim : string list
      ; pval_type : core_type
      ; pval_name : string Astlib.Loc.t
      }

    let create ~pval_loc ~pval_attributes ~pval_prim ~pval_type ~pval_name =
      let fields =
        [| Data.of_location pval_loc
         ; Data.of_node pval_attributes
         ; (Data.of_list ~f:Data.of_string) pval_prim
         ; Data.of_node pval_type
         ; (Data.of_loc ~f:Data.of_string) pval_name
        |]
      in
      node "value_description" (Record fields)

    let of_concrete { pval_loc; pval_attributes; pval_prim; pval_type; pval_name } =
      create ~pval_loc ~pval_attributes ~pval_prim ~pval_type ~pval_name

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "value_description"
        ; data = Record [| pval_loc; pval_attributes; pval_prim; pval_type; pval_name |]
        } ->
          Helpers.Option.bind (Data.to_location pval_loc) ~f:(fun pval_loc ->
            Helpers.Option.bind (Data.to_node pval_attributes) ~f:(fun pval_attributes ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_string) pval_prim) ~f:(fun pval_prim ->
                Helpers.Option.bind (Data.to_node pval_type) ~f:(fun pval_type ->
                  Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pval_name) ~f:(fun pval_name ->
                    Some { pval_loc; pval_attributes; pval_prim; pval_type; pval_name }
          )))))
      | _ -> None
  end

  module Case = struct
    type t = case

    type concrete =
      { pc_rhs : expression
      ; pc_guard : expression option
      ; pc_lhs : pattern
      }

    let create ~pc_rhs ~pc_guard ~pc_lhs =
      let fields =
        [| Data.of_node pc_rhs
         ; (Data.of_option ~f:Data.of_node) pc_guard
         ; Data.of_node pc_lhs
        |]
      in
      node "case" (Record fields)

    let of_concrete { pc_rhs; pc_guard; pc_lhs } =
      create ~pc_rhs ~pc_guard ~pc_lhs

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "case"
        ; data = Record [| pc_rhs; pc_guard; pc_lhs |]
        } ->
          Helpers.Option.bind (Data.to_node pc_rhs) ~f:(fun pc_rhs ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_node) pc_guard) ~f:(fun pc_guard ->
              Helpers.Option.bind (Data.to_node pc_lhs) ~f:(fun pc_lhs ->
                Some { pc_rhs; pc_guard; pc_lhs }
          )))
      | _ -> None
  end

  module Expression_desc = struct
    type t = expression_desc

    type concrete =
      | Pexp_unreachable
      | Pexp_extension of extension
      | Pexp_open of expression * longident_loc * override_flag
      | Pexp_pack of module_expr
      | Pexp_newtype of expression * string Astlib.Loc.t
      | Pexp_object of class_structure
      | Pexp_poly of core_type option * expression
      | Pexp_lazy of expression
      | Pexp_assert of expression
      | Pexp_letexception of expression * extension_constructor
      | Pexp_letmodule of expression * module_expr * string Astlib.Loc.t
      | Pexp_override of (expression * label Astlib.Loc.t) list
      | Pexp_setinstvar of expression * label Astlib.Loc.t
      | Pexp_new of longident_loc
      | Pexp_send of label Astlib.Loc.t * expression
      | Pexp_coerce of core_type * core_type option * expression
      | Pexp_constraint of core_type * expression
      | Pexp_for of expression * direction_flag * expression * expression * pattern
      | Pexp_while of expression * expression
      | Pexp_sequence of expression * expression
      | Pexp_ifthenelse of expression option * expression * expression
      | Pexp_array of expression list
      | Pexp_setfield of expression * longident_loc * expression
      | Pexp_field of longident_loc * expression
      | Pexp_record of expression option * (expression * longident_loc) list
      | Pexp_variant of expression option * label
      | Pexp_construct of expression option * longident_loc
      | Pexp_tuple of expression list
      | Pexp_try of case list * expression
      | Pexp_match of case list * expression
      | Pexp_apply of (expression * arg_label) list * expression
      | Pexp_fun of expression * pattern * expression option * arg_label
      | Pexp_function of case list
      | Pexp_let of expression * value_binding list * rec_flag
      | Pexp_constant of constant
      | Pexp_ident of longident_loc

    let pexp_unreachable =
      node "expression_desc" (Variant { tag = "Pexp_unreachable"; args = [||] })
    let pexp_extension x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pexp_open x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_open"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let pexp_pack x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_pack"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pexp_newtype x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_newtype"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_string) x2
            |]
          })
    let pexp_object x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_object"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pexp_poly x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_poly"
          ; args =
            [| (Data.of_option ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let pexp_lazy x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_lazy"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pexp_assert x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_assert"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pexp_letexception x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_letexception"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pexp_letmodule x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_letmodule"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; (Data.of_loc ~f:Data.of_string) x3
            |]
          })
    let pexp_override x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_override"
          ; args =
            [| (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:(Data.of_loc ~f:Data.of_string))) x1
            |]
          })
    let pexp_setinstvar x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_setinstvar"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_string) x2
            |]
          })
    let pexp_new x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_new"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
            |]
          })
    let pexp_send x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_send"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; Data.of_node x2
            |]
          })
    let pexp_coerce x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_coerce"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let pexp_constraint x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pexp_for x1 x2 x3 x4 x5 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_for"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
             ; Data.of_node x4
             ; Data.of_node x5
            |]
          })
    let pexp_while x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_while"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pexp_sequence x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_sequence"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pexp_ifthenelse x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_ifthenelse"
          ; args =
            [| (Data.of_option ~f:Data.of_node) x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let pexp_array x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_array"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let pexp_setfield x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_setfield"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let pexp_field x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_field"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let pexp_record x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_record"
          ; args =
            [| (Data.of_option ~f:Data.of_node) x1
             ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:(Data.of_loc ~f:Data.of_node))) x2
            |]
          })
    let pexp_variant x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_variant"
          ; args =
            [| (Data.of_option ~f:Data.of_node) x1
             ; Data.of_string x2
            |]
          })
    let pexp_construct x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_construct"
          ; args =
            [| (Data.of_option ~f:Data.of_node) x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })
    let pexp_tuple x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let pexp_try x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_try"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let pexp_match x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_match"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let pexp_apply x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_apply"
          ; args =
            [| (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) x1
             ; Data.of_node x2
            |]
          })
    let pexp_fun x1 x2 x3 x4 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_fun"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; (Data.of_option ~f:Data.of_node) x3
             ; Data.of_node x4
            |]
          })
    let pexp_function x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_function"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let pexp_let x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_let"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let pexp_constant x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_constant"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pexp_ident x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_ident"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
            |]
          })

    let of_concrete c =
      match c with
      | Pexp_unreachable -> pexp_unreachable
      | Pexp_extension (x1) ->
        pexp_extension x1
      | Pexp_open (x1, x2, x3) ->
        pexp_open x1 x2 x3
      | Pexp_pack (x1) ->
        pexp_pack x1
      | Pexp_newtype (x1, x2) ->
        pexp_newtype x1 x2
      | Pexp_object (x1) ->
        pexp_object x1
      | Pexp_poly (x1, x2) ->
        pexp_poly x1 x2
      | Pexp_lazy (x1) ->
        pexp_lazy x1
      | Pexp_assert (x1) ->
        pexp_assert x1
      | Pexp_letexception (x1, x2) ->
        pexp_letexception x1 x2
      | Pexp_letmodule (x1, x2, x3) ->
        pexp_letmodule x1 x2 x3
      | Pexp_override (x1) ->
        pexp_override x1
      | Pexp_setinstvar (x1, x2) ->
        pexp_setinstvar x1 x2
      | Pexp_new (x1) ->
        pexp_new x1
      | Pexp_send (x1, x2) ->
        pexp_send x1 x2
      | Pexp_coerce (x1, x2, x3) ->
        pexp_coerce x1 x2 x3
      | Pexp_constraint (x1, x2) ->
        pexp_constraint x1 x2
      | Pexp_for (x1, x2, x3, x4, x5) ->
        pexp_for x1 x2 x3 x4 x5
      | Pexp_while (x1, x2) ->
        pexp_while x1 x2
      | Pexp_sequence (x1, x2) ->
        pexp_sequence x1 x2
      | Pexp_ifthenelse (x1, x2, x3) ->
        pexp_ifthenelse x1 x2 x3
      | Pexp_array (x1) ->
        pexp_array x1
      | Pexp_setfield (x1, x2, x3) ->
        pexp_setfield x1 x2 x3
      | Pexp_field (x1, x2) ->
        pexp_field x1 x2
      | Pexp_record (x1, x2) ->
        pexp_record x1 x2
      | Pexp_variant (x1, x2) ->
        pexp_variant x1 x2
      | Pexp_construct (x1, x2) ->
        pexp_construct x1 x2
      | Pexp_tuple (x1) ->
        pexp_tuple x1
      | Pexp_try (x1, x2) ->
        pexp_try x1 x2
      | Pexp_match (x1, x2) ->
        pexp_match x1 x2
      | Pexp_apply (x1, x2) ->
        pexp_apply x1 x2
      | Pexp_fun (x1, x2, x3, x4) ->
        pexp_fun x1 x2 x3 x4
      | Pexp_function (x1) ->
        pexp_function x1
      | Pexp_let (x1, x2, x3) ->
        pexp_let x1 x2 x3
      | Pexp_constant (x1) ->
        pexp_constant x1
      | Pexp_ident (x1) ->
        pexp_ident x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "expression_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pexp_unreachable"; args = [||] } -> Some Pexp_unreachable
          | Variant { tag = "Pexp_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pexp_extension (x1))
            )
          | Variant { tag = "Pexp_open"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pexp_open (x1, x2, x3))
            )))
          | Variant { tag = "Pexp_pack"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pexp_pack (x1))
            )
          | Variant { tag = "Pexp_newtype"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x2) ~f:(fun x2 ->
                Some (Pexp_newtype (x1, x2))
            ))
          | Variant { tag = "Pexp_object"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pexp_object (x1))
            )
          | Variant { tag = "Pexp_poly"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_poly (x1, x2))
            ))
          | Variant { tag = "Pexp_lazy"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pexp_lazy (x1))
            )
          | Variant { tag = "Pexp_assert"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pexp_assert (x1))
            )
          | Variant { tag = "Pexp_letexception"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_letexception (x1, x2))
            ))
          | Variant { tag = "Pexp_letmodule"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x3) ~f:(fun x3 ->
                  Some (Pexp_letmodule (x1, x2, x3))
            )))
          | Variant { tag = "Pexp_override"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:(Data.to_loc ~f:Data.to_string))) x1) ~f:(fun x1 ->
              Some (Pexp_override (x1))
            )
          | Variant { tag = "Pexp_setinstvar"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x2) ~f:(fun x2 ->
                Some (Pexp_setinstvar (x1, x2))
            ))
          | Variant { tag = "Pexp_new"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pexp_new (x1))
            )
          | Variant { tag = "Pexp_send"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_send (x1, x2))
            ))
          | Variant { tag = "Pexp_coerce"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pexp_coerce (x1, x2, x3))
            )))
          | Variant { tag = "Pexp_constraint"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_constraint (x1, x2))
            ))
          | Variant { tag = "Pexp_for"; args = [| x1; x2; x3; x4; x5 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Helpers.Option.bind (Data.to_node x4) ~f:(fun x4 ->
                    Helpers.Option.bind (Data.to_node x5) ~f:(fun x5 ->
                      Some (Pexp_for (x1, x2, x3, x4, x5))
            )))))
          | Variant { tag = "Pexp_while"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_while (x1, x2))
            ))
          | Variant { tag = "Pexp_sequence"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_sequence (x1, x2))
            ))
          | Variant { tag = "Pexp_ifthenelse"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pexp_ifthenelse (x1, x2, x3))
            )))
          | Variant { tag = "Pexp_array"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pexp_array (x1))
            )
          | Variant { tag = "Pexp_setfield"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pexp_setfield (x1, x2, x3))
            )))
          | Variant { tag = "Pexp_field"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_field (x1, x2))
            ))
          | Variant { tag = "Pexp_record"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:(Data.to_loc ~f:Data.to_node))) x2) ~f:(fun x2 ->
                Some (Pexp_record (x1, x2))
            ))
          | Variant { tag = "Pexp_variant"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_string x2) ~f:(fun x2 ->
                Some (Pexp_variant (x1, x2))
            ))
          | Variant { tag = "Pexp_construct"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_construct (x1, x2))
            ))
          | Variant { tag = "Pexp_tuple"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pexp_tuple (x1))
            )
          | Variant { tag = "Pexp_try"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_try (x1, x2))
            ))
          | Variant { tag = "Pexp_match"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_match (x1, x2))
            ))
          | Variant { tag = "Pexp_apply"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_apply (x1, x2))
            ))
          | Variant { tag = "Pexp_fun"; args = [| x1; x2; x3; x4 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x3) ~f:(fun x3 ->
                  Helpers.Option.bind (Data.to_node x4) ~f:(fun x4 ->
                    Some (Pexp_fun (x1, x2, x3, x4))
            ))))
          | Variant { tag = "Pexp_function"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pexp_function (x1))
            )
          | Variant { tag = "Pexp_let"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pexp_let (x1, x2, x3))
            )))
          | Variant { tag = "Pexp_constant"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pexp_constant (x1))
            )
          | Variant { tag = "Pexp_ident"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pexp_ident (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Expression = struct
    type t = expression

    type concrete =
      { pexp_attributes : attributes
      ; pexp_loc : Astlib.Location.t
      ; pexp_desc : expression_desc
      }

    let create ~pexp_attributes ~pexp_loc ~pexp_desc =
      let fields =
        [| Data.of_node pexp_attributes
         ; Data.of_location pexp_loc
         ; Data.of_node pexp_desc
        |]
      in
      node "expression" (Record fields)

    let of_concrete { pexp_attributes; pexp_loc; pexp_desc } =
      create ~pexp_attributes ~pexp_loc ~pexp_desc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "expression"
        ; data = Record [| pexp_attributes; pexp_loc; pexp_desc |]
        } ->
          Helpers.Option.bind (Data.to_node pexp_attributes) ~f:(fun pexp_attributes ->
            Helpers.Option.bind (Data.to_location pexp_loc) ~f:(fun pexp_loc ->
              Helpers.Option.bind (Data.to_node pexp_desc) ~f:(fun pexp_desc ->
                Some { pexp_attributes; pexp_loc; pexp_desc }
          )))
      | _ -> None
  end

  module Pattern_desc = struct
    type t = pattern_desc

    type concrete =
      | Ppat_open of pattern * longident_loc
      | Ppat_extension of extension
      | Ppat_exception of pattern
      | Ppat_unpack of string Astlib.Loc.t
      | Ppat_lazy of pattern
      | Ppat_type of longident_loc
      | Ppat_constraint of core_type * pattern
      | Ppat_or of pattern * pattern
      | Ppat_array of pattern list
      | Ppat_record of closed_flag * (pattern * longident_loc) list
      | Ppat_variant of pattern option * label
      | Ppat_construct of pattern option * longident_loc
      | Ppat_tuple of pattern list
      | Ppat_interval of constant * constant
      | Ppat_constant of constant
      | Ppat_alias of string Astlib.Loc.t * pattern
      | Ppat_var of string Astlib.Loc.t
      | Ppat_any

    let ppat_open x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_open"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })
    let ppat_extension x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let ppat_exception x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_exception"
          ; args =
            [| Data.of_node x1
            |]
          })
    let ppat_unpack x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_unpack"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
            |]
          })
    let ppat_lazy x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_lazy"
          ; args =
            [| Data.of_node x1
            |]
          })
    let ppat_type x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_type"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
            |]
          })
    let ppat_constraint x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let ppat_or x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_or"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let ppat_array x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_array"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let ppat_record x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_record"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:(Data.of_loc ~f:Data.of_node))) x2
            |]
          })
    let ppat_variant x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_variant"
          ; args =
            [| (Data.of_option ~f:Data.of_node) x1
             ; Data.of_string x2
            |]
          })
    let ppat_construct x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_construct"
          ; args =
            [| (Data.of_option ~f:Data.of_node) x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })
    let ppat_tuple x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let ppat_interval x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_interval"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let ppat_constant x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_constant"
          ; args =
            [| Data.of_node x1
            |]
          })
    let ppat_alias x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_alias"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; Data.of_node x2
            |]
          })
    let ppat_var x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_var"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
            |]
          })
    let ppat_any =
      node "pattern_desc" (Variant { tag = "Ppat_any"; args = [||] })

    let of_concrete c =
      match c with
      | Ppat_open (x1, x2) ->
        ppat_open x1 x2
      | Ppat_extension (x1) ->
        ppat_extension x1
      | Ppat_exception (x1) ->
        ppat_exception x1
      | Ppat_unpack (x1) ->
        ppat_unpack x1
      | Ppat_lazy (x1) ->
        ppat_lazy x1
      | Ppat_type (x1) ->
        ppat_type x1
      | Ppat_constraint (x1, x2) ->
        ppat_constraint x1 x2
      | Ppat_or (x1, x2) ->
        ppat_or x1 x2
      | Ppat_array (x1) ->
        ppat_array x1
      | Ppat_record (x1, x2) ->
        ppat_record x1 x2
      | Ppat_variant (x1, x2) ->
        ppat_variant x1 x2
      | Ppat_construct (x1, x2) ->
        ppat_construct x1 x2
      | Ppat_tuple (x1) ->
        ppat_tuple x1
      | Ppat_interval (x1, x2) ->
        ppat_interval x1 x2
      | Ppat_constant (x1) ->
        ppat_constant x1
      | Ppat_alias (x1, x2) ->
        ppat_alias x1 x2
      | Ppat_var (x1) ->
        ppat_var x1
      | Ppat_any -> ppat_any

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "pattern_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Ppat_open"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Ppat_open (x1, x2))
            ))
          | Variant { tag = "Ppat_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Ppat_extension (x1))
            )
          | Variant { tag = "Ppat_exception"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Ppat_exception (x1))
            )
          | Variant { tag = "Ppat_unpack"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x1) ~f:(fun x1 ->
              Some (Ppat_unpack (x1))
            )
          | Variant { tag = "Ppat_lazy"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Ppat_lazy (x1))
            )
          | Variant { tag = "Ppat_type"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Ppat_type (x1))
            )
          | Variant { tag = "Ppat_constraint"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ppat_constraint (x1, x2))
            ))
          | Variant { tag = "Ppat_or"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ppat_or (x1, x2))
            ))
          | Variant { tag = "Ppat_array"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Ppat_array (x1))
            )
          | Variant { tag = "Ppat_record"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:(Data.to_loc ~f:Data.to_node))) x2) ~f:(fun x2 ->
                Some (Ppat_record (x1, x2))
            ))
          | Variant { tag = "Ppat_variant"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_string x2) ~f:(fun x2 ->
                Some (Ppat_variant (x1, x2))
            ))
          | Variant { tag = "Ppat_construct"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Ppat_construct (x1, x2))
            ))
          | Variant { tag = "Ppat_tuple"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Ppat_tuple (x1))
            )
          | Variant { tag = "Ppat_interval"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ppat_interval (x1, x2))
            ))
          | Variant { tag = "Ppat_constant"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Ppat_constant (x1))
            )
          | Variant { tag = "Ppat_alias"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ppat_alias (x1, x2))
            ))
          | Variant { tag = "Ppat_var"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x1) ~f:(fun x1 ->
              Some (Ppat_var (x1))
            )
          | Variant { tag = "Ppat_any"; args = [||] } -> Some Ppat_any
        | _ -> None
        end
      | _ -> None
  end

  module Pattern = struct
    type t = pattern

    type concrete =
      { ppat_attributes : attributes
      ; ppat_loc : Astlib.Location.t
      ; ppat_desc : pattern_desc
      }

    let create ~ppat_attributes ~ppat_loc ~ppat_desc =
      let fields =
        [| Data.of_node ppat_attributes
         ; Data.of_location ppat_loc
         ; Data.of_node ppat_desc
        |]
      in
      node "pattern" (Record fields)

    let of_concrete { ppat_attributes; ppat_loc; ppat_desc } =
      create ~ppat_attributes ~ppat_loc ~ppat_desc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "pattern"
        ; data = Record [| ppat_attributes; ppat_loc; ppat_desc |]
        } ->
          Helpers.Option.bind (Data.to_node ppat_attributes) ~f:(fun ppat_attributes ->
            Helpers.Option.bind (Data.to_location ppat_loc) ~f:(fun ppat_loc ->
              Helpers.Option.bind (Data.to_node ppat_desc) ~f:(fun ppat_desc ->
                Some { ppat_attributes; ppat_loc; ppat_desc }
          )))
      | _ -> None
  end

  module Object_field = struct
    type t = object_field

    type concrete =
      | Oinherit of core_type
      | Otag of core_type * attributes * label Astlib.Loc.t

    let oinherit x1 =
      node "object_field"
        (Variant
          { tag = "Oinherit"
          ; args =
            [| Data.of_node x1
            |]
          })
    let otag x1 x2 x3 =
      node "object_field"
        (Variant
          { tag = "Otag"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; (Data.of_loc ~f:Data.of_string) x3
            |]
          })

    let of_concrete c =
      match c with
      | Oinherit (x1) ->
        oinherit x1
      | Otag (x1, x2, x3) ->
        otag x1 x2 x3

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "object_field"; data } ->
        begin
          match data with
          | Variant { tag = "Oinherit"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Oinherit (x1))
            )
          | Variant { tag = "Otag"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x3) ~f:(fun x3 ->
                  Some (Otag (x1, x2, x3))
            )))
        | _ -> None
        end
      | _ -> None
  end

  module Row_field = struct
    type t = row_field

    type concrete =
      | Rinherit of core_type
      | Rtag of core_type list * bool * attributes * label Astlib.Loc.t

    let rinherit x1 =
      node "row_field"
        (Variant
          { tag = "Rinherit"
          ; args =
            [| Data.of_node x1
            |]
          })
    let rtag x1 x2 x3 x4 =
      node "row_field"
        (Variant
          { tag = "Rtag"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; Data.of_bool x2
             ; Data.of_node x3
             ; (Data.of_loc ~f:Data.of_string) x4
            |]
          })

    let of_concrete c =
      match c with
      | Rinherit (x1) ->
        rinherit x1
      | Rtag (x1, x2, x3, x4) ->
        rtag x1 x2 x3 x4

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "row_field"; data } ->
        begin
          match data with
          | Variant { tag = "Rinherit"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Rinherit (x1))
            )
          | Variant { tag = "Rtag"; args = [| x1; x2; x3; x4 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_bool x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x4) ~f:(fun x4 ->
                    Some (Rtag (x1, x2, x3, x4))
            ))))
        | _ -> None
        end
      | _ -> None
  end

  module Package_type = struct
    type t = package_type

    type concrete = ((core_type * longident_loc) list * longident_loc)

    let create =
      let data = (Data.of_tuple2 ~f1:(Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:(Data.of_loc ~f:Data.of_node))) ~f2:(Data.of_loc ~f:Data.of_node)) in
      fun x -> node "package_type" (data x)

    let of_concrete = create

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "package_type"; data } -> (Data.to_tuple2 ~f1:(Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:(Data.to_loc ~f:Data.to_node))) ~f2:(Data.to_loc ~f:Data.to_node)) data
      | _ -> None
  end

  module Core_type_desc = struct
    type t = core_type_desc

    type concrete =
      | Ptyp_extension of extension
      | Ptyp_package of package_type
      | Ptyp_poly of core_type * string Astlib.Loc.t list
      | Ptyp_variant of label list option * closed_flag * row_field list
      | Ptyp_alias of string * core_type
      | Ptyp_class of core_type list * longident_loc
      | Ptyp_object of closed_flag * object_field list
      | Ptyp_constr of core_type list * longident_loc
      | Ptyp_tuple of core_type list
      | Ptyp_arrow of core_type * core_type * arg_label
      | Ptyp_var of string
      | Ptyp_any

    let ptyp_extension x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let ptyp_package x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_package"
          ; args =
            [| Data.of_node x1
            |]
          })
    let ptyp_poly x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_poly"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:(Data.of_loc ~f:Data.of_string)) x2
            |]
          })
    let ptyp_variant x1 x2 x3 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_variant"
          ; args =
            [| (Data.of_option ~f:(Data.of_list ~f:Data.of_string)) x1
             ; Data.of_node x2
             ; (Data.of_list ~f:Data.of_node) x3
            |]
          })
    let ptyp_alias x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_alias"
          ; args =
            [| Data.of_string x1
             ; Data.of_node x2
            |]
          })
    let ptyp_class x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_class"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })
    let ptyp_object x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_object"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let ptyp_constr x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_constr"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })
    let ptyp_tuple x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let ptyp_arrow x1 x2 x3 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_arrow"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let ptyp_var x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_var"
          ; args =
            [| Data.of_string x1
            |]
          })
    let ptyp_any =
      node "core_type_desc" (Variant { tag = "Ptyp_any"; args = [||] })

    let of_concrete c =
      match c with
      | Ptyp_extension (x1) ->
        ptyp_extension x1
      | Ptyp_package (x1) ->
        ptyp_package x1
      | Ptyp_poly (x1, x2) ->
        ptyp_poly x1 x2
      | Ptyp_variant (x1, x2, x3) ->
        ptyp_variant x1 x2 x3
      | Ptyp_alias (x1, x2) ->
        ptyp_alias x1 x2
      | Ptyp_class (x1, x2) ->
        ptyp_class x1 x2
      | Ptyp_object (x1, x2) ->
        ptyp_object x1 x2
      | Ptyp_constr (x1, x2) ->
        ptyp_constr x1 x2
      | Ptyp_tuple (x1) ->
        ptyp_tuple x1
      | Ptyp_arrow (x1, x2, x3) ->
        ptyp_arrow x1 x2 x3
      | Ptyp_var (x1) ->
        ptyp_var x1
      | Ptyp_any -> ptyp_any

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "core_type_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Ptyp_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Ptyp_extension (x1))
            )
          | Variant { tag = "Ptyp_package"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Ptyp_package (x1))
            )
          | Variant { tag = "Ptyp_poly"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:(Data.to_loc ~f:Data.to_string)) x2) ~f:(fun x2 ->
                Some (Ptyp_poly (x1, x2))
            ))
          | Variant { tag = "Ptyp_variant"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind ((Data.to_option ~f:(Data.to_list ~f:Data.to_string)) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x3) ~f:(fun x3 ->
                  Some (Ptyp_variant (x1, x2, x3))
            )))
          | Variant { tag = "Ptyp_alias"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ptyp_alias (x1, x2))
            ))
          | Variant { tag = "Ptyp_class"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Ptyp_class (x1, x2))
            ))
          | Variant { tag = "Ptyp_object"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Ptyp_object (x1, x2))
            ))
          | Variant { tag = "Ptyp_constr"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Ptyp_constr (x1, x2))
            ))
          | Variant { tag = "Ptyp_tuple"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Ptyp_tuple (x1))
            )
          | Variant { tag = "Ptyp_arrow"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Ptyp_arrow (x1, x2, x3))
            )))
          | Variant { tag = "Ptyp_var"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Some (Ptyp_var (x1))
            )
          | Variant { tag = "Ptyp_any"; args = [||] } -> Some Ptyp_any
        | _ -> None
        end
      | _ -> None
  end

  module Core_type = struct
    type t = core_type

    type concrete =
      { ptyp_attributes : attributes
      ; ptyp_loc : Astlib.Location.t
      ; ptyp_desc : core_type_desc
      }

    let create ~ptyp_attributes ~ptyp_loc ~ptyp_desc =
      let fields =
        [| Data.of_node ptyp_attributes
         ; Data.of_location ptyp_loc
         ; Data.of_node ptyp_desc
        |]
      in
      node "core_type" (Record fields)

    let of_concrete { ptyp_attributes; ptyp_loc; ptyp_desc } =
      create ~ptyp_attributes ~ptyp_loc ~ptyp_desc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "core_type"
        ; data = Record [| ptyp_attributes; ptyp_loc; ptyp_desc |]
        } ->
          Helpers.Option.bind (Data.to_node ptyp_attributes) ~f:(fun ptyp_attributes ->
            Helpers.Option.bind (Data.to_location ptyp_loc) ~f:(fun ptyp_loc ->
              Helpers.Option.bind (Data.to_node ptyp_desc) ~f:(fun ptyp_desc ->
                Some { ptyp_attributes; ptyp_loc; ptyp_desc }
          )))
      | _ -> None
  end

  module Payload = struct
    type t = payload

    type concrete =
      | PPat of expression option * pattern
      | PTyp of core_type
      | PSig of signature
      | PStr of structure

    let ppat x1 x2 =
      node "payload"
        (Variant
          { tag = "PPat"
          ; args =
            [| (Data.of_option ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let ptyp x1 =
      node "payload"
        (Variant
          { tag = "PTyp"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig x1 =
      node "payload"
        (Variant
          { tag = "PSig"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr x1 =
      node "payload"
        (Variant
          { tag = "PStr"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete c =
      match c with
      | PPat (x1, x2) ->
        ppat x1 x2
      | PTyp (x1) ->
        ptyp x1
      | PSig (x1) ->
        psig x1
      | PStr (x1) ->
        pstr x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "payload"; data } ->
        begin
          match data with
          | Variant { tag = "PPat"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (PPat (x1, x2))
            ))
          | Variant { tag = "PTyp"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (PTyp (x1))
            )
          | Variant { tag = "PSig"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (PSig (x1))
            )
          | Variant { tag = "PStr"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (PStr (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Attributes = struct
    type t = attributes

    type concrete = attribute list

    let create =
      let data = (Data.of_list ~f:Data.of_node) in
      fun x -> node "attributes" (data x)

    let of_concrete = create

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "attributes"; data } -> (Data.to_list ~f:Data.to_node) data
      | _ -> None
  end

  module Extension = struct
    type t = extension

    type concrete = (payload * string Astlib.Loc.t)

    let create =
      let data = (Data.of_tuple2 ~f1:Data.of_node ~f2:(Data.of_loc ~f:Data.of_string)) in
      fun x -> node "extension" (data x)

    let of_concrete = create

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "extension"; data } -> (Data.to_tuple2 ~f1:Data.to_node ~f2:(Data.to_loc ~f:Data.to_string)) data
      | _ -> None
  end

  module Attribute = struct
    type t = attribute

    type concrete = (payload * string Astlib.Loc.t)

    let create =
      let data = (Data.of_tuple2 ~f1:Data.of_node ~f2:(Data.of_loc ~f:Data.of_string)) in
      fun x -> node "attribute" (data x)

    let of_concrete = create

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "attribute"; data } -> (Data.to_tuple2 ~f1:Data.to_node ~f2:(Data.to_loc ~f:Data.to_string)) data
      | _ -> None
  end

  module Constant = struct
    type t = constant

    type concrete =
      | Pconst_float of char option * string
      | Pconst_string of string option * string
      | Pconst_char of char
      | Pconst_integer of char option * string

    let pconst_float x1 x2 =
      node "constant"
        (Variant
          { tag = "Pconst_float"
          ; args =
            [| (Data.of_option ~f:Data.of_char) x1
             ; Data.of_string x2
            |]
          })
    let pconst_string x1 x2 =
      node "constant"
        (Variant
          { tag = "Pconst_string"
          ; args =
            [| (Data.of_option ~f:Data.of_string) x1
             ; Data.of_string x2
            |]
          })
    let pconst_char x1 =
      node "constant"
        (Variant
          { tag = "Pconst_char"
          ; args =
            [| Data.of_char x1
            |]
          })
    let pconst_integer x1 x2 =
      node "constant"
        (Variant
          { tag = "Pconst_integer"
          ; args =
            [| (Data.of_option ~f:Data.of_char) x1
             ; Data.of_string x2
            |]
          })

    let of_concrete c =
      match c with
      | Pconst_float (x1, x2) ->
        pconst_float x1 x2
      | Pconst_string (x1, x2) ->
        pconst_string x1 x2
      | Pconst_char (x1) ->
        pconst_char x1
      | Pconst_integer (x1, x2) ->
        pconst_integer x1 x2

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "constant"; data } ->
        begin
          match data with
          | Variant { tag = "Pconst_float"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_char) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_string x2) ~f:(fun x2 ->
                Some (Pconst_float (x1, x2))
            ))
          | Variant { tag = "Pconst_string"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_string) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_string x2) ~f:(fun x2 ->
                Some (Pconst_string (x1, x2))
            ))
          | Variant { tag = "Pconst_char"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_char x1) ~f:(fun x1 ->
              Some (Pconst_char (x1))
            )
          | Variant { tag = "Pconst_integer"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_char) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_string x2) ~f:(fun x2 ->
                Some (Pconst_integer (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Variance = struct
    type t = variance

    type concrete =
      | Invariant
      | Contravariant
      | Covariant

    let invariant =
      node "variance" (Variant { tag = "Invariant"; args = [||] })
    let contravariant =
      node "variance" (Variant { tag = "Contravariant"; args = [||] })
    let covariant =
      node "variance" (Variant { tag = "Covariant"; args = [||] })

    let of_concrete c =
      match c with
      | Invariant -> invariant
      | Contravariant -> contravariant
      | Covariant -> covariant

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "variance"; data } ->
        begin
          match data with
          | Variant { tag = "Invariant"; args = [||] } -> Some Invariant
          | Variant { tag = "Contravariant"; args = [||] } -> Some Contravariant
          | Variant { tag = "Covariant"; args = [||] } -> Some Covariant
        | _ -> None
        end
      | _ -> None
  end

  module Arg_label = struct
    type t = arg_label

    type concrete =
      | Optional of string
      | Labelled of string
      | Nolabel

    let optional x1 =
      node "arg_label"
        (Variant
          { tag = "Optional"
          ; args =
            [| Data.of_string x1
            |]
          })
    let labelled x1 =
      node "arg_label"
        (Variant
          { tag = "Labelled"
          ; args =
            [| Data.of_string x1
            |]
          })
    let nolabel =
      node "arg_label" (Variant { tag = "Nolabel"; args = [||] })

    let of_concrete c =
      match c with
      | Optional (x1) ->
        optional x1
      | Labelled (x1) ->
        labelled x1
      | Nolabel -> nolabel

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "arg_label"; data } ->
        begin
          match data with
          | Variant { tag = "Optional"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Some (Optional (x1))
            )
          | Variant { tag = "Labelled"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Some (Labelled (x1))
            )
          | Variant { tag = "Nolabel"; args = [||] } -> Some Nolabel
        | _ -> None
        end
      | _ -> None
  end

  module Label = struct
    type t = string
  end

  module Closed_flag = struct
    type t = closed_flag

    type concrete =
      | Open
      | Closed

    let open_ =
      node "closed_flag" (Variant { tag = "Open"; args = [||] })
    let closed =
      node "closed_flag" (Variant { tag = "Closed"; args = [||] })

    let of_concrete c =
      match c with
      | Open -> open_
      | Closed -> closed

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "closed_flag"; data } ->
        begin
          match data with
          | Variant { tag = "Open"; args = [||] } -> Some Open
          | Variant { tag = "Closed"; args = [||] } -> Some Closed
        | _ -> None
        end
      | _ -> None
  end

  module Override_flag = struct
    type t = override_flag

    type concrete =
      | Fresh
      | Override

    let fresh =
      node "override_flag" (Variant { tag = "Fresh"; args = [||] })
    let override =
      node "override_flag" (Variant { tag = "Override"; args = [||] })

    let of_concrete c =
      match c with
      | Fresh -> fresh
      | Override -> override

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "override_flag"; data } ->
        begin
          match data with
          | Variant { tag = "Fresh"; args = [||] } -> Some Fresh
          | Variant { tag = "Override"; args = [||] } -> Some Override
        | _ -> None
        end
      | _ -> None
  end

  module Virtual_flag = struct
    type t = virtual_flag

    type concrete =
      | Concrete
      | Virtual

    let concrete =
      node "virtual_flag" (Variant { tag = "Concrete"; args = [||] })
    let virtual_ =
      node "virtual_flag" (Variant { tag = "Virtual"; args = [||] })

    let of_concrete c =
      match c with
      | Concrete -> concrete
      | Virtual -> virtual_

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "virtual_flag"; data } ->
        begin
          match data with
          | Variant { tag = "Concrete"; args = [||] } -> Some Concrete
          | Variant { tag = "Virtual"; args = [||] } -> Some Virtual
        | _ -> None
        end
      | _ -> None
  end

  module Mutable_flag = struct
    type t = mutable_flag

    type concrete =
      | Mutable
      | Immutable

    let mutable_ =
      node "mutable_flag" (Variant { tag = "Mutable"; args = [||] })
    let immutable =
      node "mutable_flag" (Variant { tag = "Immutable"; args = [||] })

    let of_concrete c =
      match c with
      | Mutable -> mutable_
      | Immutable -> immutable

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "mutable_flag"; data } ->
        begin
          match data with
          | Variant { tag = "Mutable"; args = [||] } -> Some Mutable
          | Variant { tag = "Immutable"; args = [||] } -> Some Immutable
        | _ -> None
        end
      | _ -> None
  end

  module Private_flag = struct
    type t = private_flag

    type concrete =
      | Public
      | Private

    let public =
      node "private_flag" (Variant { tag = "Public"; args = [||] })
    let private_ =
      node "private_flag" (Variant { tag = "Private"; args = [||] })

    let of_concrete c =
      match c with
      | Public -> public
      | Private -> private_

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "private_flag"; data } ->
        begin
          match data with
          | Variant { tag = "Public"; args = [||] } -> Some Public
          | Variant { tag = "Private"; args = [||] } -> Some Private
        | _ -> None
        end
      | _ -> None
  end

  module Direction_flag = struct
    type t = direction_flag

    type concrete =
      | Downto
      | Upto

    let downto_ =
      node "direction_flag" (Variant { tag = "Downto"; args = [||] })
    let upto =
      node "direction_flag" (Variant { tag = "Upto"; args = [||] })

    let of_concrete c =
      match c with
      | Downto -> downto_
      | Upto -> upto

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "direction_flag"; data } ->
        begin
          match data with
          | Variant { tag = "Downto"; args = [||] } -> Some Downto
          | Variant { tag = "Upto"; args = [||] } -> Some Upto
        | _ -> None
        end
      | _ -> None
  end

  module Rec_flag = struct
    type t = rec_flag

    type concrete =
      | Recursive
      | Nonrecursive

    let recursive =
      node "rec_flag" (Variant { tag = "Recursive"; args = [||] })
    let nonrecursive =
      node "rec_flag" (Variant { tag = "Nonrecursive"; args = [||] })

    let of_concrete c =
      match c with
      | Recursive -> recursive
      | Nonrecursive -> nonrecursive

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "rec_flag"; data } ->
        begin
          match data with
          | Variant { tag = "Recursive"; args = [||] } -> Some Recursive
          | Variant { tag = "Nonrecursive"; args = [||] } -> Some Nonrecursive
        | _ -> None
        end
      | _ -> None
  end

  module Longident_loc = struct
    type t = longident Astlib.Loc.t
  end

  module Longident = struct
    type t = longident

    type concrete =
      | Lapply of longident * longident
      | Ldot of string * longident
      | Lident of string

    let lapply x1 x2 =
      node "longident"
        (Variant
          { tag = "Lapply"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let ldot x1 x2 =
      node "longident"
        (Variant
          { tag = "Ldot"
          ; args =
            [| Data.of_string x1
             ; Data.of_node x2
            |]
          })
    let lident x1 =
      node "longident"
        (Variant
          { tag = "Lident"
          ; args =
            [| Data.of_string x1
            |]
          })

    let of_concrete c =
      match c with
      | Lapply (x1, x2) ->
        lapply x1 x2
      | Ldot (x1, x2) ->
        ldot x1 x2
      | Lident (x1) ->
        lident x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "longident"; data } ->
        begin
          match data with
          | Variant { tag = "Lapply"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Lapply (x1, x2))
            ))
          | Variant { tag = "Ldot"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ldot (x1, x2))
            ))
          | Variant { tag = "Lident"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Some (Lident (x1))
            )
        | _ -> None
        end
      | _ -> None
  end
end

module V4_07 = struct
  let version = "v4.07"
  let node name data = Node.of_node ~version { name; data }

  module Longident = struct
    type t = longident

    type concrete =
      | Lident of string
      | Ldot of longident * string
      | Lapply of longident * longident

    let lident x1 =
      node "longident"
        (Variant
          { tag = "Lident"
          ; args =
            [| Data.of_string x1
            |]
          })
    let ldot x1 x2 =
      node "longident"
        (Variant
          { tag = "Ldot"
          ; args =
            [| Data.of_node x1
             ; Data.of_string x2
            |]
          })
    let lapply x1 x2 =
      node "longident"
        (Variant
          { tag = "Lapply"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete c =
      match c with
      | Lident (x1) ->
        lident x1
      | Ldot (x1, x2) ->
        ldot x1 x2
      | Lapply (x1, x2) ->
        lapply x1 x2

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "longident"; data } ->
        begin
          match data with
          | Variant { tag = "Lident"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Some (Lident (x1))
            )
          | Variant { tag = "Ldot"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_string x2) ~f:(fun x2 ->
                Some (Ldot (x1, x2))
            ))
          | Variant { tag = "Lapply"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Lapply (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Longident_loc = struct
    type t = longident Astlib.Loc.t
  end

  module Rec_flag = struct
    type t = rec_flag

    type concrete =
      | Nonrecursive
      | Recursive

    let nonrecursive =
      node "rec_flag" (Variant { tag = "Nonrecursive"; args = [||] })
    let recursive =
      node "rec_flag" (Variant { tag = "Recursive"; args = [||] })

    let of_concrete c =
      match c with
      | Nonrecursive -> nonrecursive
      | Recursive -> recursive

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "rec_flag"; data } ->
        begin
          match data with
          | Variant { tag = "Nonrecursive"; args = [||] } -> Some Nonrecursive
          | Variant { tag = "Recursive"; args = [||] } -> Some Recursive
        | _ -> None
        end
      | _ -> None
  end

  module Direction_flag = struct
    type t = direction_flag

    type concrete =
      | Upto
      | Downto

    let upto =
      node "direction_flag" (Variant { tag = "Upto"; args = [||] })
    let downto_ =
      node "direction_flag" (Variant { tag = "Downto"; args = [||] })

    let of_concrete c =
      match c with
      | Upto -> upto
      | Downto -> downto_

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "direction_flag"; data } ->
        begin
          match data with
          | Variant { tag = "Upto"; args = [||] } -> Some Upto
          | Variant { tag = "Downto"; args = [||] } -> Some Downto
        | _ -> None
        end
      | _ -> None
  end

  module Private_flag = struct
    type t = private_flag

    type concrete =
      | Private
      | Public

    let private_ =
      node "private_flag" (Variant { tag = "Private"; args = [||] })
    let public =
      node "private_flag" (Variant { tag = "Public"; args = [||] })

    let of_concrete c =
      match c with
      | Private -> private_
      | Public -> public

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "private_flag"; data } ->
        begin
          match data with
          | Variant { tag = "Private"; args = [||] } -> Some Private
          | Variant { tag = "Public"; args = [||] } -> Some Public
        | _ -> None
        end
      | _ -> None
  end

  module Mutable_flag = struct
    type t = mutable_flag

    type concrete =
      | Immutable
      | Mutable

    let immutable =
      node "mutable_flag" (Variant { tag = "Immutable"; args = [||] })
    let mutable_ =
      node "mutable_flag" (Variant { tag = "Mutable"; args = [||] })

    let of_concrete c =
      match c with
      | Immutable -> immutable
      | Mutable -> mutable_

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "mutable_flag"; data } ->
        begin
          match data with
          | Variant { tag = "Immutable"; args = [||] } -> Some Immutable
          | Variant { tag = "Mutable"; args = [||] } -> Some Mutable
        | _ -> None
        end
      | _ -> None
  end

  module Virtual_flag = struct
    type t = virtual_flag

    type concrete =
      | Virtual
      | Concrete

    let virtual_ =
      node "virtual_flag" (Variant { tag = "Virtual"; args = [||] })
    let concrete =
      node "virtual_flag" (Variant { tag = "Concrete"; args = [||] })

    let of_concrete c =
      match c with
      | Virtual -> virtual_
      | Concrete -> concrete

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "virtual_flag"; data } ->
        begin
          match data with
          | Variant { tag = "Virtual"; args = [||] } -> Some Virtual
          | Variant { tag = "Concrete"; args = [||] } -> Some Concrete
        | _ -> None
        end
      | _ -> None
  end

  module Override_flag = struct
    type t = override_flag

    type concrete =
      | Override
      | Fresh

    let override =
      node "override_flag" (Variant { tag = "Override"; args = [||] })
    let fresh =
      node "override_flag" (Variant { tag = "Fresh"; args = [||] })

    let of_concrete c =
      match c with
      | Override -> override
      | Fresh -> fresh

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "override_flag"; data } ->
        begin
          match data with
          | Variant { tag = "Override"; args = [||] } -> Some Override
          | Variant { tag = "Fresh"; args = [||] } -> Some Fresh
        | _ -> None
        end
      | _ -> None
  end

  module Closed_flag = struct
    type t = closed_flag

    type concrete =
      | Closed
      | Open

    let closed =
      node "closed_flag" (Variant { tag = "Closed"; args = [||] })
    let open_ =
      node "closed_flag" (Variant { tag = "Open"; args = [||] })

    let of_concrete c =
      match c with
      | Closed -> closed
      | Open -> open_

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "closed_flag"; data } ->
        begin
          match data with
          | Variant { tag = "Closed"; args = [||] } -> Some Closed
          | Variant { tag = "Open"; args = [||] } -> Some Open
        | _ -> None
        end
      | _ -> None
  end

  module Label = struct
    type t = string
  end

  module Arg_label = struct
    type t = arg_label

    type concrete =
      | Nolabel
      | Labelled of string
      | Optional of string

    let nolabel =
      node "arg_label" (Variant { tag = "Nolabel"; args = [||] })
    let labelled x1 =
      node "arg_label"
        (Variant
          { tag = "Labelled"
          ; args =
            [| Data.of_string x1
            |]
          })
    let optional x1 =
      node "arg_label"
        (Variant
          { tag = "Optional"
          ; args =
            [| Data.of_string x1
            |]
          })

    let of_concrete c =
      match c with
      | Nolabel -> nolabel
      | Labelled (x1) ->
        labelled x1
      | Optional (x1) ->
        optional x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "arg_label"; data } ->
        begin
          match data with
          | Variant { tag = "Nolabel"; args = [||] } -> Some Nolabel
          | Variant { tag = "Labelled"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Some (Labelled (x1))
            )
          | Variant { tag = "Optional"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Some (Optional (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Variance = struct
    type t = variance

    type concrete =
      | Covariant
      | Contravariant
      | Invariant

    let covariant =
      node "variance" (Variant { tag = "Covariant"; args = [||] })
    let contravariant =
      node "variance" (Variant { tag = "Contravariant"; args = [||] })
    let invariant =
      node "variance" (Variant { tag = "Invariant"; args = [||] })

    let of_concrete c =
      match c with
      | Covariant -> covariant
      | Contravariant -> contravariant
      | Invariant -> invariant

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "variance"; data } ->
        begin
          match data with
          | Variant { tag = "Covariant"; args = [||] } -> Some Covariant
          | Variant { tag = "Contravariant"; args = [||] } -> Some Contravariant
          | Variant { tag = "Invariant"; args = [||] } -> Some Invariant
        | _ -> None
        end
      | _ -> None
  end

  module Constant = struct
    type t = constant

    type concrete =
      | Pconst_integer of string * char option
      | Pconst_char of char
      | Pconst_string of string * string option
      | Pconst_float of string * char option

    let pconst_integer x1 x2 =
      node "constant"
        (Variant
          { tag = "Pconst_integer"
          ; args =
            [| Data.of_string x1
             ; (Data.of_option ~f:Data.of_char) x2
            |]
          })
    let pconst_char x1 =
      node "constant"
        (Variant
          { tag = "Pconst_char"
          ; args =
            [| Data.of_char x1
            |]
          })
    let pconst_string x1 x2 =
      node "constant"
        (Variant
          { tag = "Pconst_string"
          ; args =
            [| Data.of_string x1
             ; (Data.of_option ~f:Data.of_string) x2
            |]
          })
    let pconst_float x1 x2 =
      node "constant"
        (Variant
          { tag = "Pconst_float"
          ; args =
            [| Data.of_string x1
             ; (Data.of_option ~f:Data.of_char) x2
            |]
          })

    let of_concrete c =
      match c with
      | Pconst_integer (x1, x2) ->
        pconst_integer x1 x2
      | Pconst_char (x1) ->
        pconst_char x1
      | Pconst_string (x1, x2) ->
        pconst_string x1 x2
      | Pconst_float (x1, x2) ->
        pconst_float x1 x2

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "constant"; data } ->
        begin
          match data with
          | Variant { tag = "Pconst_integer"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_char) x2) ~f:(fun x2 ->
                Some (Pconst_integer (x1, x2))
            ))
          | Variant { tag = "Pconst_char"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_char x1) ~f:(fun x1 ->
              Some (Pconst_char (x1))
            )
          | Variant { tag = "Pconst_string"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_string) x2) ~f:(fun x2 ->
                Some (Pconst_string (x1, x2))
            ))
          | Variant { tag = "Pconst_float"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_char) x2) ~f:(fun x2 ->
                Some (Pconst_float (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Attribute = struct
    type t = attribute

    type concrete = (string Astlib.Loc.t * payload)

    let create =
      let data = (Data.of_tuple2 ~f1:(Data.of_loc ~f:Data.of_string) ~f2:Data.of_node) in
      fun x -> node "attribute" (data x)

    let of_concrete = create

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "attribute"; data } -> (Data.to_tuple2 ~f1:(Data.to_loc ~f:Data.to_string) ~f2:Data.to_node) data
      | _ -> None
  end

  module Extension = struct
    type t = extension

    type concrete = (string Astlib.Loc.t * payload)

    let create =
      let data = (Data.of_tuple2 ~f1:(Data.of_loc ~f:Data.of_string) ~f2:Data.of_node) in
      fun x -> node "extension" (data x)

    let of_concrete = create

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "extension"; data } -> (Data.to_tuple2 ~f1:(Data.to_loc ~f:Data.to_string) ~f2:Data.to_node) data
      | _ -> None
  end

  module Attributes = struct
    type t = attributes

    type concrete = attribute list

    let create =
      let data = (Data.of_list ~f:Data.of_node) in
      fun x -> node "attributes" (data x)

    let of_concrete = create

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "attributes"; data } -> (Data.to_list ~f:Data.to_node) data
      | _ -> None
  end

  module Payload = struct
    type t = payload

    type concrete =
      | PStr of structure
      | PSig of signature
      | PTyp of core_type
      | PPat of pattern * expression option

    let pstr x1 =
      node "payload"
        (Variant
          { tag = "PStr"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig x1 =
      node "payload"
        (Variant
          { tag = "PSig"
          ; args =
            [| Data.of_node x1
            |]
          })
    let ptyp x1 =
      node "payload"
        (Variant
          { tag = "PTyp"
          ; args =
            [| Data.of_node x1
            |]
          })
    let ppat x1 x2 =
      node "payload"
        (Variant
          { tag = "PPat"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })

    let of_concrete c =
      match c with
      | PStr (x1) ->
        pstr x1
      | PSig (x1) ->
        psig x1
      | PTyp (x1) ->
        ptyp x1
      | PPat (x1, x2) ->
        ppat x1 x2

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "payload"; data } ->
        begin
          match data with
          | Variant { tag = "PStr"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (PStr (x1))
            )
          | Variant { tag = "PSig"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (PSig (x1))
            )
          | Variant { tag = "PTyp"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (PTyp (x1))
            )
          | Variant { tag = "PPat"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (PPat (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Core_type = struct
    type t = core_type

    type concrete =
      { ptyp_desc : core_type_desc
      ; ptyp_loc : Astlib.Location.t
      ; ptyp_attributes : attributes
      }

    let create ~ptyp_desc ~ptyp_loc ~ptyp_attributes =
      let fields =
        [| Data.of_node ptyp_desc
         ; Data.of_location ptyp_loc
         ; Data.of_node ptyp_attributes
        |]
      in
      node "core_type" (Record fields)

    let of_concrete { ptyp_desc; ptyp_loc; ptyp_attributes } =
      create ~ptyp_desc ~ptyp_loc ~ptyp_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "core_type"
        ; data = Record [| ptyp_desc; ptyp_loc; ptyp_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node ptyp_desc) ~f:(fun ptyp_desc ->
            Helpers.Option.bind (Data.to_location ptyp_loc) ~f:(fun ptyp_loc ->
              Helpers.Option.bind (Data.to_node ptyp_attributes) ~f:(fun ptyp_attributes ->
                Some { ptyp_desc; ptyp_loc; ptyp_attributes }
          )))
      | _ -> None
  end

  module Core_type_desc = struct
    type t = core_type_desc

    type concrete =
      | Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of arg_label * core_type * core_type
      | Ptyp_tuple of core_type list
      | Ptyp_constr of longident_loc * core_type list
      | Ptyp_object of object_field list * closed_flag
      | Ptyp_class of longident_loc * core_type list
      | Ptyp_alias of core_type * string
      | Ptyp_variant of row_field list * closed_flag * label list option
      | Ptyp_poly of string Astlib.Loc.t list * core_type
      | Ptyp_package of package_type
      | Ptyp_extension of extension

    let ptyp_any =
      node "core_type_desc" (Variant { tag = "Ptyp_any"; args = [||] })
    let ptyp_var x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_var"
          ; args =
            [| Data.of_string x1
            |]
          })
    let ptyp_arrow x1 x2 x3 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_arrow"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let ptyp_tuple x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let ptyp_constr x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_constr"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let ptyp_object x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_object"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let ptyp_class x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_class"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let ptyp_alias x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_alias"
          ; args =
            [| Data.of_node x1
             ; Data.of_string x2
            |]
          })
    let ptyp_variant x1 x2 x3 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_variant"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; Data.of_node x2
             ; (Data.of_option ~f:(Data.of_list ~f:Data.of_string)) x3
            |]
          })
    let ptyp_poly x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_poly"
          ; args =
            [| (Data.of_list ~f:(Data.of_loc ~f:Data.of_string)) x1
             ; Data.of_node x2
            |]
          })
    let ptyp_package x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_package"
          ; args =
            [| Data.of_node x1
            |]
          })
    let ptyp_extension x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_extension"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete c =
      match c with
      | Ptyp_any -> ptyp_any
      | Ptyp_var (x1) ->
        ptyp_var x1
      | Ptyp_arrow (x1, x2, x3) ->
        ptyp_arrow x1 x2 x3
      | Ptyp_tuple (x1) ->
        ptyp_tuple x1
      | Ptyp_constr (x1, x2) ->
        ptyp_constr x1 x2
      | Ptyp_object (x1, x2) ->
        ptyp_object x1 x2
      | Ptyp_class (x1, x2) ->
        ptyp_class x1 x2
      | Ptyp_alias (x1, x2) ->
        ptyp_alias x1 x2
      | Ptyp_variant (x1, x2, x3) ->
        ptyp_variant x1 x2 x3
      | Ptyp_poly (x1, x2) ->
        ptyp_poly x1 x2
      | Ptyp_package (x1) ->
        ptyp_package x1
      | Ptyp_extension (x1) ->
        ptyp_extension x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "core_type_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Ptyp_any"; args = [||] } -> Some Ptyp_any
          | Variant { tag = "Ptyp_var"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Some (Ptyp_var (x1))
            )
          | Variant { tag = "Ptyp_arrow"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Ptyp_arrow (x1, x2, x3))
            )))
          | Variant { tag = "Ptyp_tuple"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Ptyp_tuple (x1))
            )
          | Variant { tag = "Ptyp_constr"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Ptyp_constr (x1, x2))
            ))
          | Variant { tag = "Ptyp_object"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ptyp_object (x1, x2))
            ))
          | Variant { tag = "Ptyp_class"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Ptyp_class (x1, x2))
            ))
          | Variant { tag = "Ptyp_alias"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_string x2) ~f:(fun x2 ->
                Some (Ptyp_alias (x1, x2))
            ))
          | Variant { tag = "Ptyp_variant"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind ((Data.to_option ~f:(Data.to_list ~f:Data.to_string)) x3) ~f:(fun x3 ->
                  Some (Ptyp_variant (x1, x2, x3))
            )))
          | Variant { tag = "Ptyp_poly"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_loc ~f:Data.to_string)) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ptyp_poly (x1, x2))
            ))
          | Variant { tag = "Ptyp_package"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Ptyp_package (x1))
            )
          | Variant { tag = "Ptyp_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Ptyp_extension (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Package_type = struct
    type t = package_type

    type concrete = (longident_loc * (longident_loc * core_type) list)

    let create =
      let data = (Data.of_tuple2 ~f1:(Data.of_loc ~f:Data.of_node) ~f2:(Data.of_list ~f:(Data.of_tuple2 ~f1:(Data.of_loc ~f:Data.of_node) ~f2:Data.of_node))) in
      fun x -> node "package_type" (data x)

    let of_concrete = create

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "package_type"; data } -> (Data.to_tuple2 ~f1:(Data.to_loc ~f:Data.to_node) ~f2:(Data.to_list ~f:(Data.to_tuple2 ~f1:(Data.to_loc ~f:Data.to_node) ~f2:Data.to_node))) data
      | _ -> None
  end

  module Row_field = struct
    type t = row_field

    type concrete =
      | Rtag of label Astlib.Loc.t * attributes * bool * core_type list
      | Rinherit of core_type

    let rtag x1 x2 x3 x4 =
      node "row_field"
        (Variant
          { tag = "Rtag"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; Data.of_node x2
             ; Data.of_bool x3
             ; (Data.of_list ~f:Data.of_node) x4
            |]
          })
    let rinherit x1 =
      node "row_field"
        (Variant
          { tag = "Rinherit"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete c =
      match c with
      | Rtag (x1, x2, x3, x4) ->
        rtag x1 x2 x3 x4
      | Rinherit (x1) ->
        rinherit x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "row_field"; data } ->
        begin
          match data with
          | Variant { tag = "Rtag"; args = [| x1; x2; x3; x4 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_bool x3) ~f:(fun x3 ->
                  Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x4) ~f:(fun x4 ->
                    Some (Rtag (x1, x2, x3, x4))
            ))))
          | Variant { tag = "Rinherit"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Rinherit (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Object_field = struct
    type t = object_field

    type concrete =
      | Otag of label Astlib.Loc.t * attributes * core_type
      | Oinherit of core_type

    let otag x1 x2 x3 =
      node "object_field"
        (Variant
          { tag = "Otag"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let oinherit x1 =
      node "object_field"
        (Variant
          { tag = "Oinherit"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete c =
      match c with
      | Otag (x1, x2, x3) ->
        otag x1 x2 x3
      | Oinherit (x1) ->
        oinherit x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "object_field"; data } ->
        begin
          match data with
          | Variant { tag = "Otag"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Otag (x1, x2, x3))
            )))
          | Variant { tag = "Oinherit"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Oinherit (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Pattern = struct
    type t = pattern

    type concrete =
      { ppat_desc : pattern_desc
      ; ppat_loc : Astlib.Location.t
      ; ppat_attributes : attributes
      }

    let create ~ppat_desc ~ppat_loc ~ppat_attributes =
      let fields =
        [| Data.of_node ppat_desc
         ; Data.of_location ppat_loc
         ; Data.of_node ppat_attributes
        |]
      in
      node "pattern" (Record fields)

    let of_concrete { ppat_desc; ppat_loc; ppat_attributes } =
      create ~ppat_desc ~ppat_loc ~ppat_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "pattern"
        ; data = Record [| ppat_desc; ppat_loc; ppat_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node ppat_desc) ~f:(fun ppat_desc ->
            Helpers.Option.bind (Data.to_location ppat_loc) ~f:(fun ppat_loc ->
              Helpers.Option.bind (Data.to_node ppat_attributes) ~f:(fun ppat_attributes ->
                Some { ppat_desc; ppat_loc; ppat_attributes }
          )))
      | _ -> None
  end

  module Pattern_desc = struct
    type t = pattern_desc

    type concrete =
      | Ppat_any
      | Ppat_var of string Astlib.Loc.t
      | Ppat_alias of pattern * string Astlib.Loc.t
      | Ppat_constant of constant
      | Ppat_interval of constant * constant
      | Ppat_tuple of pattern list
      | Ppat_construct of longident_loc * pattern option
      | Ppat_variant of label * pattern option
      | Ppat_record of (longident_loc * pattern) list * closed_flag
      | Ppat_array of pattern list
      | Ppat_or of pattern * pattern
      | Ppat_constraint of pattern * core_type
      | Ppat_type of longident_loc
      | Ppat_lazy of pattern
      | Ppat_unpack of string Astlib.Loc.t
      | Ppat_exception of pattern
      | Ppat_extension of extension
      | Ppat_open of longident_loc * pattern

    let ppat_any =
      node "pattern_desc" (Variant { tag = "Ppat_any"; args = [||] })
    let ppat_var x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_var"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
            |]
          })
    let ppat_alias x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_alias"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_string) x2
            |]
          })
    let ppat_constant x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_constant"
          ; args =
            [| Data.of_node x1
            |]
          })
    let ppat_interval x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_interval"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let ppat_tuple x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let ppat_construct x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_construct"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let ppat_variant x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_variant"
          ; args =
            [| Data.of_string x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let ppat_record x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_record"
          ; args =
            [| (Data.of_list ~f:(Data.of_tuple2 ~f1:(Data.of_loc ~f:Data.of_node) ~f2:Data.of_node)) x1
             ; Data.of_node x2
            |]
          })
    let ppat_array x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_array"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let ppat_or x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_or"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let ppat_constraint x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let ppat_type x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_type"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
            |]
          })
    let ppat_lazy x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_lazy"
          ; args =
            [| Data.of_node x1
            |]
          })
    let ppat_unpack x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_unpack"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
            |]
          })
    let ppat_exception x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_exception"
          ; args =
            [| Data.of_node x1
            |]
          })
    let ppat_extension x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let ppat_open x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_open"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })

    let of_concrete c =
      match c with
      | Ppat_any -> ppat_any
      | Ppat_var (x1) ->
        ppat_var x1
      | Ppat_alias (x1, x2) ->
        ppat_alias x1 x2
      | Ppat_constant (x1) ->
        ppat_constant x1
      | Ppat_interval (x1, x2) ->
        ppat_interval x1 x2
      | Ppat_tuple (x1) ->
        ppat_tuple x1
      | Ppat_construct (x1, x2) ->
        ppat_construct x1 x2
      | Ppat_variant (x1, x2) ->
        ppat_variant x1 x2
      | Ppat_record (x1, x2) ->
        ppat_record x1 x2
      | Ppat_array (x1) ->
        ppat_array x1
      | Ppat_or (x1, x2) ->
        ppat_or x1 x2
      | Ppat_constraint (x1, x2) ->
        ppat_constraint x1 x2
      | Ppat_type (x1) ->
        ppat_type x1
      | Ppat_lazy (x1) ->
        ppat_lazy x1
      | Ppat_unpack (x1) ->
        ppat_unpack x1
      | Ppat_exception (x1) ->
        ppat_exception x1
      | Ppat_extension (x1) ->
        ppat_extension x1
      | Ppat_open (x1, x2) ->
        ppat_open x1 x2

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "pattern_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Ppat_any"; args = [||] } -> Some Ppat_any
          | Variant { tag = "Ppat_var"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x1) ~f:(fun x1 ->
              Some (Ppat_var (x1))
            )
          | Variant { tag = "Ppat_alias"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x2) ~f:(fun x2 ->
                Some (Ppat_alias (x1, x2))
            ))
          | Variant { tag = "Ppat_constant"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Ppat_constant (x1))
            )
          | Variant { tag = "Ppat_interval"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ppat_interval (x1, x2))
            ))
          | Variant { tag = "Ppat_tuple"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Ppat_tuple (x1))
            )
          | Variant { tag = "Ppat_construct"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Ppat_construct (x1, x2))
            ))
          | Variant { tag = "Ppat_variant"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Ppat_variant (x1, x2))
            ))
          | Variant { tag = "Ppat_record"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:(Data.to_loc ~f:Data.to_node) ~f2:Data.to_node)) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ppat_record (x1, x2))
            ))
          | Variant { tag = "Ppat_array"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Ppat_array (x1))
            )
          | Variant { tag = "Ppat_or"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ppat_or (x1, x2))
            ))
          | Variant { tag = "Ppat_constraint"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ppat_constraint (x1, x2))
            ))
          | Variant { tag = "Ppat_type"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Ppat_type (x1))
            )
          | Variant { tag = "Ppat_lazy"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Ppat_lazy (x1))
            )
          | Variant { tag = "Ppat_unpack"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x1) ~f:(fun x1 ->
              Some (Ppat_unpack (x1))
            )
          | Variant { tag = "Ppat_exception"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Ppat_exception (x1))
            )
          | Variant { tag = "Ppat_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Ppat_extension (x1))
            )
          | Variant { tag = "Ppat_open"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ppat_open (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Expression = struct
    type t = expression

    type concrete =
      { pexp_desc : expression_desc
      ; pexp_loc : Astlib.Location.t
      ; pexp_attributes : attributes
      }

    let create ~pexp_desc ~pexp_loc ~pexp_attributes =
      let fields =
        [| Data.of_node pexp_desc
         ; Data.of_location pexp_loc
         ; Data.of_node pexp_attributes
        |]
      in
      node "expression" (Record fields)

    let of_concrete { pexp_desc; pexp_loc; pexp_attributes } =
      create ~pexp_desc ~pexp_loc ~pexp_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "expression"
        ; data = Record [| pexp_desc; pexp_loc; pexp_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node pexp_desc) ~f:(fun pexp_desc ->
            Helpers.Option.bind (Data.to_location pexp_loc) ~f:(fun pexp_loc ->
              Helpers.Option.bind (Data.to_node pexp_attributes) ~f:(fun pexp_attributes ->
                Some { pexp_desc; pexp_loc; pexp_attributes }
          )))
      | _ -> None
  end

  module Expression_desc = struct
    type t = expression_desc

    type concrete =
      | Pexp_ident of longident_loc
      | Pexp_constant of constant
      | Pexp_let of rec_flag * value_binding list * expression
      | Pexp_function of case list
      | Pexp_fun of arg_label * expression option * pattern * expression
      | Pexp_apply of expression * (arg_label * expression) list
      | Pexp_match of expression * case list
      | Pexp_try of expression * case list
      | Pexp_tuple of expression list
      | Pexp_construct of longident_loc * expression option
      | Pexp_variant of label * expression option
      | Pexp_record of (longident_loc * expression) list * expression option
      | Pexp_field of expression * longident_loc
      | Pexp_setfield of expression * longident_loc * expression
      | Pexp_array of expression list
      | Pexp_ifthenelse of expression * expression * expression option
      | Pexp_sequence of expression * expression
      | Pexp_while of expression * expression
      | Pexp_for of pattern * expression * expression * direction_flag * expression
      | Pexp_constraint of expression * core_type
      | Pexp_coerce of expression * core_type option * core_type
      | Pexp_send of expression * label Astlib.Loc.t
      | Pexp_new of longident_loc
      | Pexp_setinstvar of label Astlib.Loc.t * expression
      | Pexp_override of (label Astlib.Loc.t * expression) list
      | Pexp_letmodule of string Astlib.Loc.t * module_expr * expression
      | Pexp_letexception of extension_constructor * expression
      | Pexp_assert of expression
      | Pexp_lazy of expression
      | Pexp_poly of expression * core_type option
      | Pexp_object of class_structure
      | Pexp_newtype of string Astlib.Loc.t * expression
      | Pexp_pack of module_expr
      | Pexp_open of override_flag * longident_loc * expression
      | Pexp_extension of extension
      | Pexp_unreachable

    let pexp_ident x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_ident"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
            |]
          })
    let pexp_constant x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_constant"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pexp_let x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_let"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let pexp_function x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_function"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let pexp_fun x1 x2 x3 x4 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_fun"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
             ; Data.of_node x3
             ; Data.of_node x4
            |]
          })
    let pexp_apply x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_apply"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) x2
            |]
          })
    let pexp_match x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_match"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let pexp_try x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_try"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let pexp_tuple x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let pexp_construct x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_construct"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let pexp_variant x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_variant"
          ; args =
            [| Data.of_string x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let pexp_record x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_record"
          ; args =
            [| (Data.of_list ~f:(Data.of_tuple2 ~f1:(Data.of_loc ~f:Data.of_node) ~f2:Data.of_node)) x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let pexp_field x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_field"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })
    let pexp_setfield x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_setfield"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let pexp_array x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_array"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let pexp_ifthenelse x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_ifthenelse"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; (Data.of_option ~f:Data.of_node) x3
            |]
          })
    let pexp_sequence x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_sequence"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pexp_while x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_while"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pexp_for x1 x2 x3 x4 x5 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_for"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
             ; Data.of_node x4
             ; Data.of_node x5
            |]
          })
    let pexp_constraint x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pexp_coerce x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_coerce"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let pexp_send x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_send"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_string) x2
            |]
          })
    let pexp_new x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_new"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
            |]
          })
    let pexp_setinstvar x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_setinstvar"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; Data.of_node x2
            |]
          })
    let pexp_override x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_override"
          ; args =
            [| (Data.of_list ~f:(Data.of_tuple2 ~f1:(Data.of_loc ~f:Data.of_string) ~f2:Data.of_node)) x1
            |]
          })
    let pexp_letmodule x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_letmodule"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let pexp_letexception x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_letexception"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pexp_assert x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_assert"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pexp_lazy x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_lazy"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pexp_poly x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_poly"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let pexp_object x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_object"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pexp_newtype x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_newtype"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; Data.of_node x2
            |]
          })
    let pexp_pack x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_pack"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pexp_open x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_open"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let pexp_extension x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pexp_unreachable =
      node "expression_desc" (Variant { tag = "Pexp_unreachable"; args = [||] })

    let of_concrete c =
      match c with
      | Pexp_ident (x1) ->
        pexp_ident x1
      | Pexp_constant (x1) ->
        pexp_constant x1
      | Pexp_let (x1, x2, x3) ->
        pexp_let x1 x2 x3
      | Pexp_function (x1) ->
        pexp_function x1
      | Pexp_fun (x1, x2, x3, x4) ->
        pexp_fun x1 x2 x3 x4
      | Pexp_apply (x1, x2) ->
        pexp_apply x1 x2
      | Pexp_match (x1, x2) ->
        pexp_match x1 x2
      | Pexp_try (x1, x2) ->
        pexp_try x1 x2
      | Pexp_tuple (x1) ->
        pexp_tuple x1
      | Pexp_construct (x1, x2) ->
        pexp_construct x1 x2
      | Pexp_variant (x1, x2) ->
        pexp_variant x1 x2
      | Pexp_record (x1, x2) ->
        pexp_record x1 x2
      | Pexp_field (x1, x2) ->
        pexp_field x1 x2
      | Pexp_setfield (x1, x2, x3) ->
        pexp_setfield x1 x2 x3
      | Pexp_array (x1) ->
        pexp_array x1
      | Pexp_ifthenelse (x1, x2, x3) ->
        pexp_ifthenelse x1 x2 x3
      | Pexp_sequence (x1, x2) ->
        pexp_sequence x1 x2
      | Pexp_while (x1, x2) ->
        pexp_while x1 x2
      | Pexp_for (x1, x2, x3, x4, x5) ->
        pexp_for x1 x2 x3 x4 x5
      | Pexp_constraint (x1, x2) ->
        pexp_constraint x1 x2
      | Pexp_coerce (x1, x2, x3) ->
        pexp_coerce x1 x2 x3
      | Pexp_send (x1, x2) ->
        pexp_send x1 x2
      | Pexp_new (x1) ->
        pexp_new x1
      | Pexp_setinstvar (x1, x2) ->
        pexp_setinstvar x1 x2
      | Pexp_override (x1) ->
        pexp_override x1
      | Pexp_letmodule (x1, x2, x3) ->
        pexp_letmodule x1 x2 x3
      | Pexp_letexception (x1, x2) ->
        pexp_letexception x1 x2
      | Pexp_assert (x1) ->
        pexp_assert x1
      | Pexp_lazy (x1) ->
        pexp_lazy x1
      | Pexp_poly (x1, x2) ->
        pexp_poly x1 x2
      | Pexp_object (x1) ->
        pexp_object x1
      | Pexp_newtype (x1, x2) ->
        pexp_newtype x1 x2
      | Pexp_pack (x1) ->
        pexp_pack x1
      | Pexp_open (x1, x2, x3) ->
        pexp_open x1 x2 x3
      | Pexp_extension (x1) ->
        pexp_extension x1
      | Pexp_unreachable -> pexp_unreachable

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "expression_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pexp_ident"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pexp_ident (x1))
            )
          | Variant { tag = "Pexp_constant"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pexp_constant (x1))
            )
          | Variant { tag = "Pexp_let"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pexp_let (x1, x2, x3))
            )))
          | Variant { tag = "Pexp_function"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pexp_function (x1))
            )
          | Variant { tag = "Pexp_fun"; args = [| x1; x2; x3; x4 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Helpers.Option.bind (Data.to_node x4) ~f:(fun x4 ->
                    Some (Pexp_fun (x1, x2, x3, x4))
            ))))
          | Variant { tag = "Pexp_apply"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) x2) ~f:(fun x2 ->
                Some (Pexp_apply (x1, x2))
            ))
          | Variant { tag = "Pexp_match"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_match (x1, x2))
            ))
          | Variant { tag = "Pexp_try"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_try (x1, x2))
            ))
          | Variant { tag = "Pexp_tuple"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pexp_tuple (x1))
            )
          | Variant { tag = "Pexp_construct"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_construct (x1, x2))
            ))
          | Variant { tag = "Pexp_variant"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_variant (x1, x2))
            ))
          | Variant { tag = "Pexp_record"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:(Data.to_loc ~f:Data.to_node) ~f2:Data.to_node)) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_record (x1, x2))
            ))
          | Variant { tag = "Pexp_field"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_field (x1, x2))
            ))
          | Variant { tag = "Pexp_setfield"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pexp_setfield (x1, x2, x3))
            )))
          | Variant { tag = "Pexp_array"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pexp_array (x1))
            )
          | Variant { tag = "Pexp_ifthenelse"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x3) ~f:(fun x3 ->
                  Some (Pexp_ifthenelse (x1, x2, x3))
            )))
          | Variant { tag = "Pexp_sequence"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_sequence (x1, x2))
            ))
          | Variant { tag = "Pexp_while"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_while (x1, x2))
            ))
          | Variant { tag = "Pexp_for"; args = [| x1; x2; x3; x4; x5 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Helpers.Option.bind (Data.to_node x4) ~f:(fun x4 ->
                    Helpers.Option.bind (Data.to_node x5) ~f:(fun x5 ->
                      Some (Pexp_for (x1, x2, x3, x4, x5))
            )))))
          | Variant { tag = "Pexp_constraint"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_constraint (x1, x2))
            ))
          | Variant { tag = "Pexp_coerce"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pexp_coerce (x1, x2, x3))
            )))
          | Variant { tag = "Pexp_send"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x2) ~f:(fun x2 ->
                Some (Pexp_send (x1, x2))
            ))
          | Variant { tag = "Pexp_new"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pexp_new (x1))
            )
          | Variant { tag = "Pexp_setinstvar"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_setinstvar (x1, x2))
            ))
          | Variant { tag = "Pexp_override"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:(Data.to_loc ~f:Data.to_string) ~f2:Data.to_node)) x1) ~f:(fun x1 ->
              Some (Pexp_override (x1))
            )
          | Variant { tag = "Pexp_letmodule"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pexp_letmodule (x1, x2, x3))
            )))
          | Variant { tag = "Pexp_letexception"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_letexception (x1, x2))
            ))
          | Variant { tag = "Pexp_assert"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pexp_assert (x1))
            )
          | Variant { tag = "Pexp_lazy"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pexp_lazy (x1))
            )
          | Variant { tag = "Pexp_poly"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_poly (x1, x2))
            ))
          | Variant { tag = "Pexp_object"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pexp_object (x1))
            )
          | Variant { tag = "Pexp_newtype"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_newtype (x1, x2))
            ))
          | Variant { tag = "Pexp_pack"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pexp_pack (x1))
            )
          | Variant { tag = "Pexp_open"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pexp_open (x1, x2, x3))
            )))
          | Variant { tag = "Pexp_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pexp_extension (x1))
            )
          | Variant { tag = "Pexp_unreachable"; args = [||] } -> Some Pexp_unreachable
        | _ -> None
        end
      | _ -> None
  end

  module Case = struct
    type t = case

    type concrete =
      { pc_lhs : pattern
      ; pc_guard : expression option
      ; pc_rhs : expression
      }

    let create ~pc_lhs ~pc_guard ~pc_rhs =
      let fields =
        [| Data.of_node pc_lhs
         ; (Data.of_option ~f:Data.of_node) pc_guard
         ; Data.of_node pc_rhs
        |]
      in
      node "case" (Record fields)

    let of_concrete { pc_lhs; pc_guard; pc_rhs } =
      create ~pc_lhs ~pc_guard ~pc_rhs

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "case"
        ; data = Record [| pc_lhs; pc_guard; pc_rhs |]
        } ->
          Helpers.Option.bind (Data.to_node pc_lhs) ~f:(fun pc_lhs ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_node) pc_guard) ~f:(fun pc_guard ->
              Helpers.Option.bind (Data.to_node pc_rhs) ~f:(fun pc_rhs ->
                Some { pc_lhs; pc_guard; pc_rhs }
          )))
      | _ -> None
  end

  module Value_description = struct
    type t = value_description

    type concrete =
      { pval_name : string Astlib.Loc.t
      ; pval_type : core_type
      ; pval_prim : string list
      ; pval_attributes : attributes
      ; pval_loc : Astlib.Location.t
      }

    let create ~pval_name ~pval_type ~pval_prim ~pval_attributes ~pval_loc =
      let fields =
        [| (Data.of_loc ~f:Data.of_string) pval_name
         ; Data.of_node pval_type
         ; (Data.of_list ~f:Data.of_string) pval_prim
         ; Data.of_node pval_attributes
         ; Data.of_location pval_loc
        |]
      in
      node "value_description" (Record fields)

    let of_concrete { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } =
      create ~pval_name ~pval_type ~pval_prim ~pval_attributes ~pval_loc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "value_description"
        ; data = Record [| pval_name; pval_type; pval_prim; pval_attributes; pval_loc |]
        } ->
          Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pval_name) ~f:(fun pval_name ->
            Helpers.Option.bind (Data.to_node pval_type) ~f:(fun pval_type ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_string) pval_prim) ~f:(fun pval_prim ->
                Helpers.Option.bind (Data.to_node pval_attributes) ~f:(fun pval_attributes ->
                  Helpers.Option.bind (Data.to_location pval_loc) ~f:(fun pval_loc ->
                    Some { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }
          )))))
      | _ -> None
  end

  module Type_declaration = struct
    type t = type_declaration

    type concrete =
      { ptype_name : string Astlib.Loc.t
      ; ptype_params : (core_type * variance) list
      ; ptype_cstrs : (core_type * core_type * Astlib.Location.t) list
      ; ptype_kind : type_kind
      ; ptype_private : private_flag
      ; ptype_manifest : core_type option
      ; ptype_attributes : attributes
      ; ptype_loc : Astlib.Location.t
      }

    let create ~ptype_name ~ptype_params ~ptype_cstrs ~ptype_kind ~ptype_private ~ptype_manifest ~ptype_attributes ~ptype_loc =
      let fields =
        [| (Data.of_loc ~f:Data.of_string) ptype_name
         ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) ptype_params
         ; (Data.of_list ~f:(Data.of_tuple3 ~f1:Data.of_node ~f2:Data.of_node ~f3:Data.of_location)) ptype_cstrs
         ; Data.of_node ptype_kind
         ; Data.of_node ptype_private
         ; (Data.of_option ~f:Data.of_node) ptype_manifest
         ; Data.of_node ptype_attributes
         ; Data.of_location ptype_loc
        |]
      in
      node "type_declaration" (Record fields)

    let of_concrete { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } =
      create ~ptype_name ~ptype_params ~ptype_cstrs ~ptype_kind ~ptype_private ~ptype_manifest ~ptype_attributes ~ptype_loc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "type_declaration"
        ; data = Record [| ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc |]
        } ->
          Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) ptype_name) ~f:(fun ptype_name ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) ptype_params) ~f:(fun ptype_params ->
              Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple3 ~f1:Data.to_node ~f2:Data.to_node ~f3:Data.to_location)) ptype_cstrs) ~f:(fun ptype_cstrs ->
                Helpers.Option.bind (Data.to_node ptype_kind) ~f:(fun ptype_kind ->
                  Helpers.Option.bind (Data.to_node ptype_private) ~f:(fun ptype_private ->
                    Helpers.Option.bind ((Data.to_option ~f:Data.to_node) ptype_manifest) ~f:(fun ptype_manifest ->
                      Helpers.Option.bind (Data.to_node ptype_attributes) ~f:(fun ptype_attributes ->
                        Helpers.Option.bind (Data.to_location ptype_loc) ~f:(fun ptype_loc ->
                          Some { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc }
          ))))))))
      | _ -> None
  end

  module Type_kind = struct
    type t = type_kind

    type concrete =
      | Ptype_abstract
      | Ptype_variant of constructor_declaration list
      | Ptype_record of label_declaration list
      | Ptype_open

    let ptype_abstract =
      node "type_kind" (Variant { tag = "Ptype_abstract"; args = [||] })
    let ptype_variant x1 =
      node "type_kind"
        (Variant
          { tag = "Ptype_variant"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let ptype_record x1 =
      node "type_kind"
        (Variant
          { tag = "Ptype_record"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let ptype_open =
      node "type_kind" (Variant { tag = "Ptype_open"; args = [||] })

    let of_concrete c =
      match c with
      | Ptype_abstract -> ptype_abstract
      | Ptype_variant (x1) ->
        ptype_variant x1
      | Ptype_record (x1) ->
        ptype_record x1
      | Ptype_open -> ptype_open

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "type_kind"; data } ->
        begin
          match data with
          | Variant { tag = "Ptype_abstract"; args = [||] } -> Some Ptype_abstract
          | Variant { tag = "Ptype_variant"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Ptype_variant (x1))
            )
          | Variant { tag = "Ptype_record"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Ptype_record (x1))
            )
          | Variant { tag = "Ptype_open"; args = [||] } -> Some Ptype_open
        | _ -> None
        end
      | _ -> None
  end

  module Label_declaration = struct
    type t = label_declaration

    type concrete =
      { pld_name : string Astlib.Loc.t
      ; pld_mutable : mutable_flag
      ; pld_type : core_type
      ; pld_loc : Astlib.Location.t
      ; pld_attributes : attributes
      }

    let create ~pld_name ~pld_mutable ~pld_type ~pld_loc ~pld_attributes =
      let fields =
        [| (Data.of_loc ~f:Data.of_string) pld_name
         ; Data.of_node pld_mutable
         ; Data.of_node pld_type
         ; Data.of_location pld_loc
         ; Data.of_node pld_attributes
        |]
      in
      node "label_declaration" (Record fields)

    let of_concrete { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } =
      create ~pld_name ~pld_mutable ~pld_type ~pld_loc ~pld_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "label_declaration"
        ; data = Record [| pld_name; pld_mutable; pld_type; pld_loc; pld_attributes |]
        } ->
          Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pld_name) ~f:(fun pld_name ->
            Helpers.Option.bind (Data.to_node pld_mutable) ~f:(fun pld_mutable ->
              Helpers.Option.bind (Data.to_node pld_type) ~f:(fun pld_type ->
                Helpers.Option.bind (Data.to_location pld_loc) ~f:(fun pld_loc ->
                  Helpers.Option.bind (Data.to_node pld_attributes) ~f:(fun pld_attributes ->
                    Some { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }
          )))))
      | _ -> None
  end

  module Constructor_declaration = struct
    type t = constructor_declaration

    type concrete =
      { pcd_name : string Astlib.Loc.t
      ; pcd_args : constructor_arguments
      ; pcd_res : core_type option
      ; pcd_loc : Astlib.Location.t
      ; pcd_attributes : attributes
      }

    let create ~pcd_name ~pcd_args ~pcd_res ~pcd_loc ~pcd_attributes =
      let fields =
        [| (Data.of_loc ~f:Data.of_string) pcd_name
         ; Data.of_node pcd_args
         ; (Data.of_option ~f:Data.of_node) pcd_res
         ; Data.of_location pcd_loc
         ; Data.of_node pcd_attributes
        |]
      in
      node "constructor_declaration" (Record fields)

    let of_concrete { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } =
      create ~pcd_name ~pcd_args ~pcd_res ~pcd_loc ~pcd_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "constructor_declaration"
        ; data = Record [| pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes |]
        } ->
          Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pcd_name) ~f:(fun pcd_name ->
            Helpers.Option.bind (Data.to_node pcd_args) ~f:(fun pcd_args ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) pcd_res) ~f:(fun pcd_res ->
                Helpers.Option.bind (Data.to_location pcd_loc) ~f:(fun pcd_loc ->
                  Helpers.Option.bind (Data.to_node pcd_attributes) ~f:(fun pcd_attributes ->
                    Some { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }
          )))))
      | _ -> None
  end

  module Constructor_arguments = struct
    type t = constructor_arguments

    type concrete =
      | Pcstr_tuple of core_type list
      | Pcstr_record of label_declaration list

    let pcstr_tuple x1 =
      node "constructor_arguments"
        (Variant
          { tag = "Pcstr_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let pcstr_record x1 =
      node "constructor_arguments"
        (Variant
          { tag = "Pcstr_record"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })

    let of_concrete c =
      match c with
      | Pcstr_tuple (x1) ->
        pcstr_tuple x1
      | Pcstr_record (x1) ->
        pcstr_record x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "constructor_arguments"; data } ->
        begin
          match data with
          | Variant { tag = "Pcstr_tuple"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pcstr_tuple (x1))
            )
          | Variant { tag = "Pcstr_record"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pcstr_record (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Type_extension = struct
    type t = type_extension

    type concrete =
      { ptyext_path : longident_loc
      ; ptyext_params : (core_type * variance) list
      ; ptyext_constructors : extension_constructor list
      ; ptyext_private : private_flag
      ; ptyext_attributes : attributes
      }

    let create ~ptyext_path ~ptyext_params ~ptyext_constructors ~ptyext_private ~ptyext_attributes =
      let fields =
        [| (Data.of_loc ~f:Data.of_node) ptyext_path
         ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) ptyext_params
         ; (Data.of_list ~f:Data.of_node) ptyext_constructors
         ; Data.of_node ptyext_private
         ; Data.of_node ptyext_attributes
        |]
      in
      node "type_extension" (Record fields)

    let of_concrete { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } =
      create ~ptyext_path ~ptyext_params ~ptyext_constructors ~ptyext_private ~ptyext_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "type_extension"
        ; data = Record [| ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes |]
        } ->
          Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) ptyext_path) ~f:(fun ptyext_path ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) ptyext_params) ~f:(fun ptyext_params ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) ptyext_constructors) ~f:(fun ptyext_constructors ->
                Helpers.Option.bind (Data.to_node ptyext_private) ~f:(fun ptyext_private ->
                  Helpers.Option.bind (Data.to_node ptyext_attributes) ~f:(fun ptyext_attributes ->
                    Some { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes }
          )))))
      | _ -> None
  end

  module Extension_constructor = struct
    type t = extension_constructor

    type concrete =
      { pext_name : string Astlib.Loc.t
      ; pext_kind : extension_constructor_kind
      ; pext_loc : Astlib.Location.t
      ; pext_attributes : attributes
      }

    let create ~pext_name ~pext_kind ~pext_loc ~pext_attributes =
      let fields =
        [| (Data.of_loc ~f:Data.of_string) pext_name
         ; Data.of_node pext_kind
         ; Data.of_location pext_loc
         ; Data.of_node pext_attributes
        |]
      in
      node "extension_constructor" (Record fields)

    let of_concrete { pext_name; pext_kind; pext_loc; pext_attributes } =
      create ~pext_name ~pext_kind ~pext_loc ~pext_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "extension_constructor"
        ; data = Record [| pext_name; pext_kind; pext_loc; pext_attributes |]
        } ->
          Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pext_name) ~f:(fun pext_name ->
            Helpers.Option.bind (Data.to_node pext_kind) ~f:(fun pext_kind ->
              Helpers.Option.bind (Data.to_location pext_loc) ~f:(fun pext_loc ->
                Helpers.Option.bind (Data.to_node pext_attributes) ~f:(fun pext_attributes ->
                  Some { pext_name; pext_kind; pext_loc; pext_attributes }
          ))))
      | _ -> None
  end

  module Extension_constructor_kind = struct
    type t = extension_constructor_kind

    type concrete =
      | Pext_decl of constructor_arguments * core_type option
      | Pext_rebind of longident_loc

    let pext_decl x1 x2 =
      node "extension_constructor_kind"
        (Variant
          { tag = "Pext_decl"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let pext_rebind x1 =
      node "extension_constructor_kind"
        (Variant
          { tag = "Pext_rebind"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
            |]
          })

    let of_concrete c =
      match c with
      | Pext_decl (x1, x2) ->
        pext_decl x1 x2
      | Pext_rebind (x1) ->
        pext_rebind x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "extension_constructor_kind"; data } ->
        begin
          match data with
          | Variant { tag = "Pext_decl"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pext_decl (x1, x2))
            ))
          | Variant { tag = "Pext_rebind"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pext_rebind (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Class_type = struct
    type t = class_type

    type concrete =
      { pcty_desc : class_type_desc
      ; pcty_loc : Astlib.Location.t
      ; pcty_attributes : attributes
      }

    let create ~pcty_desc ~pcty_loc ~pcty_attributes =
      let fields =
        [| Data.of_node pcty_desc
         ; Data.of_location pcty_loc
         ; Data.of_node pcty_attributes
        |]
      in
      node "class_type" (Record fields)

    let of_concrete { pcty_desc; pcty_loc; pcty_attributes } =
      create ~pcty_desc ~pcty_loc ~pcty_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_type"
        ; data = Record [| pcty_desc; pcty_loc; pcty_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node pcty_desc) ~f:(fun pcty_desc ->
            Helpers.Option.bind (Data.to_location pcty_loc) ~f:(fun pcty_loc ->
              Helpers.Option.bind (Data.to_node pcty_attributes) ~f:(fun pcty_attributes ->
                Some { pcty_desc; pcty_loc; pcty_attributes }
          )))
      | _ -> None
  end

  module Class_type_desc = struct
    type t = class_type_desc

    type concrete =
      | Pcty_constr of longident_loc * core_type list
      | Pcty_signature of class_signature
      | Pcty_arrow of arg_label * core_type * class_type
      | Pcty_extension of extension
      | Pcty_open of override_flag * longident_loc * class_type

    let pcty_constr x1 x2 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_constr"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let pcty_signature x1 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_signature"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pcty_arrow x1 x2 x3 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_arrow"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let pcty_extension x1 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pcty_open x1 x2 x3 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_open"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })

    let of_concrete c =
      match c with
      | Pcty_constr (x1, x2) ->
        pcty_constr x1 x2
      | Pcty_signature (x1) ->
        pcty_signature x1
      | Pcty_arrow (x1, x2, x3) ->
        pcty_arrow x1 x2 x3
      | Pcty_extension (x1) ->
        pcty_extension x1
      | Pcty_open (x1, x2, x3) ->
        pcty_open x1 x2 x3

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_type_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pcty_constr"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pcty_constr (x1, x2))
            ))
          | Variant { tag = "Pcty_signature"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pcty_signature (x1))
            )
          | Variant { tag = "Pcty_arrow"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pcty_arrow (x1, x2, x3))
            )))
          | Variant { tag = "Pcty_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pcty_extension (x1))
            )
          | Variant { tag = "Pcty_open"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pcty_open (x1, x2, x3))
            )))
        | _ -> None
        end
      | _ -> None
  end

  module Class_signature = struct
    type t = class_signature

    type concrete =
      { pcsig_self : core_type
      ; pcsig_fields : class_type_field list
      }

    let create ~pcsig_self ~pcsig_fields =
      let fields =
        [| Data.of_node pcsig_self
         ; (Data.of_list ~f:Data.of_node) pcsig_fields
        |]
      in
      node "class_signature" (Record fields)

    let of_concrete { pcsig_self; pcsig_fields } =
      create ~pcsig_self ~pcsig_fields

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_signature"
        ; data = Record [| pcsig_self; pcsig_fields |]
        } ->
          Helpers.Option.bind (Data.to_node pcsig_self) ~f:(fun pcsig_self ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) pcsig_fields) ~f:(fun pcsig_fields ->
              Some { pcsig_self; pcsig_fields }
          ))
      | _ -> None
  end

  module Class_type_field = struct
    type t = class_type_field

    type concrete =
      { pctf_desc : class_type_field_desc
      ; pctf_loc : Astlib.Location.t
      ; pctf_attributes : attributes
      }

    let create ~pctf_desc ~pctf_loc ~pctf_attributes =
      let fields =
        [| Data.of_node pctf_desc
         ; Data.of_location pctf_loc
         ; Data.of_node pctf_attributes
        |]
      in
      node "class_type_field" (Record fields)

    let of_concrete { pctf_desc; pctf_loc; pctf_attributes } =
      create ~pctf_desc ~pctf_loc ~pctf_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_type_field"
        ; data = Record [| pctf_desc; pctf_loc; pctf_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node pctf_desc) ~f:(fun pctf_desc ->
            Helpers.Option.bind (Data.to_location pctf_loc) ~f:(fun pctf_loc ->
              Helpers.Option.bind (Data.to_node pctf_attributes) ~f:(fun pctf_attributes ->
                Some { pctf_desc; pctf_loc; pctf_attributes }
          )))
      | _ -> None
  end

  module Class_type_field_desc = struct
    type t = class_type_field_desc

    type concrete =
      | Pctf_inherit of class_type
      | Pctf_val of (label Astlib.Loc.t * mutable_flag * virtual_flag * core_type)
      | Pctf_method of (label Astlib.Loc.t * private_flag * virtual_flag * core_type)
      | Pctf_constraint of (core_type * core_type)
      | Pctf_attribute of attribute
      | Pctf_extension of extension

    let pctf_inherit x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_inherit"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pctf_val x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_val"
          ; args =
            [| (Data.of_tuple4 ~f1:(Data.of_loc ~f:Data.of_string) ~f2:Data.of_node ~f3:Data.of_node ~f4:Data.of_node) x1
            |]
          })
    let pctf_method x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_method"
          ; args =
            [| (Data.of_tuple4 ~f1:(Data.of_loc ~f:Data.of_string) ~f2:Data.of_node ~f3:Data.of_node ~f4:Data.of_node) x1
            |]
          })
    let pctf_constraint x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_constraint"
          ; args =
            [| (Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node) x1
            |]
          })
    let pctf_attribute x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pctf_extension x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_extension"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete c =
      match c with
      | Pctf_inherit (x1) ->
        pctf_inherit x1
      | Pctf_val (x1) ->
        pctf_val x1
      | Pctf_method (x1) ->
        pctf_method x1
      | Pctf_constraint (x1) ->
        pctf_constraint x1
      | Pctf_attribute (x1) ->
        pctf_attribute x1
      | Pctf_extension (x1) ->
        pctf_extension x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_type_field_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pctf_inherit"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pctf_inherit (x1))
            )
          | Variant { tag = "Pctf_val"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple4 ~f1:(Data.to_loc ~f:Data.to_string) ~f2:Data.to_node ~f3:Data.to_node ~f4:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pctf_val (x1))
            )
          | Variant { tag = "Pctf_method"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple4 ~f1:(Data.to_loc ~f:Data.to_string) ~f2:Data.to_node ~f3:Data.to_node ~f4:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pctf_method (x1))
            )
          | Variant { tag = "Pctf_constraint"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pctf_constraint (x1))
            )
          | Variant { tag = "Pctf_attribute"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pctf_attribute (x1))
            )
          | Variant { tag = "Pctf_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pctf_extension (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Class_infos = struct
    type 'a t = 'a class_infos

    type 'a concrete =
      { pci_virt : virtual_flag
      ; pci_params : (core_type * variance) list
      ; pci_name : string Astlib.Loc.t
      ; pci_expr : 'a
      ; pci_loc : Astlib.Location.t
      ; pci_attributes : attributes
      }

    let create ~pci_virt ~pci_params ~pci_name ~pci_expr ~pci_loc ~pci_attributes =
      let fields =
        [| Data.of_node pci_virt
         ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) pci_params
         ; (Data.of_loc ~f:Data.of_string) pci_name
         ; Data.of_node pci_expr
         ; Data.of_location pci_loc
         ; Data.of_node pci_attributes
        |]
      in
      node "class_infos" (Record fields)

    let of_concrete { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } =
      create ~pci_virt ~pci_params ~pci_name ~pci_expr ~pci_loc ~pci_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_infos"
        ; data = Record [| pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node pci_virt) ~f:(fun pci_virt ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) pci_params) ~f:(fun pci_params ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pci_name) ~f:(fun pci_name ->
                Helpers.Option.bind (Data.to_node pci_expr) ~f:(fun pci_expr ->
                  Helpers.Option.bind (Data.to_location pci_loc) ~f:(fun pci_loc ->
                    Helpers.Option.bind (Data.to_node pci_attributes) ~f:(fun pci_attributes ->
                      Some { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
          ))))))
      | _ -> None
  end

  module Class_description = struct
    type t = class_type class_infos
  end

  module Class_type_declaration = struct
    type t = class_type class_infos
  end

  module Class_expr = struct
    type t = class_expr

    type concrete =
      { pcl_desc : class_expr_desc
      ; pcl_loc : Astlib.Location.t
      ; pcl_attributes : attributes
      }

    let create ~pcl_desc ~pcl_loc ~pcl_attributes =
      let fields =
        [| Data.of_node pcl_desc
         ; Data.of_location pcl_loc
         ; Data.of_node pcl_attributes
        |]
      in
      node "class_expr" (Record fields)

    let of_concrete { pcl_desc; pcl_loc; pcl_attributes } =
      create ~pcl_desc ~pcl_loc ~pcl_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_expr"
        ; data = Record [| pcl_desc; pcl_loc; pcl_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node pcl_desc) ~f:(fun pcl_desc ->
            Helpers.Option.bind (Data.to_location pcl_loc) ~f:(fun pcl_loc ->
              Helpers.Option.bind (Data.to_node pcl_attributes) ~f:(fun pcl_attributes ->
                Some { pcl_desc; pcl_loc; pcl_attributes }
          )))
      | _ -> None
  end

  module Class_expr_desc = struct
    type t = class_expr_desc

    type concrete =
      | Pcl_constr of longident_loc * core_type list
      | Pcl_structure of class_structure
      | Pcl_fun of arg_label * expression option * pattern * class_expr
      | Pcl_apply of class_expr * (arg_label * expression) list
      | Pcl_let of rec_flag * value_binding list * class_expr
      | Pcl_constraint of class_expr * class_type
      | Pcl_extension of extension
      | Pcl_open of override_flag * longident_loc * class_expr

    let pcl_constr x1 x2 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_constr"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let pcl_structure x1 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_structure"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pcl_fun x1 x2 x3 x4 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_fun"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
             ; Data.of_node x3
             ; Data.of_node x4
            |]
          })
    let pcl_apply x1 x2 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_apply"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) x2
            |]
          })
    let pcl_let x1 x2 x3 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_let"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let pcl_constraint x1 x2 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pcl_extension x1 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pcl_open x1 x2 x3 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_open"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })

    let of_concrete c =
      match c with
      | Pcl_constr (x1, x2) ->
        pcl_constr x1 x2
      | Pcl_structure (x1) ->
        pcl_structure x1
      | Pcl_fun (x1, x2, x3, x4) ->
        pcl_fun x1 x2 x3 x4
      | Pcl_apply (x1, x2) ->
        pcl_apply x1 x2
      | Pcl_let (x1, x2, x3) ->
        pcl_let x1 x2 x3
      | Pcl_constraint (x1, x2) ->
        pcl_constraint x1 x2
      | Pcl_extension (x1) ->
        pcl_extension x1
      | Pcl_open (x1, x2, x3) ->
        pcl_open x1 x2 x3

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_expr_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pcl_constr"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pcl_constr (x1, x2))
            ))
          | Variant { tag = "Pcl_structure"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pcl_structure (x1))
            )
          | Variant { tag = "Pcl_fun"; args = [| x1; x2; x3; x4 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Helpers.Option.bind (Data.to_node x4) ~f:(fun x4 ->
                    Some (Pcl_fun (x1, x2, x3, x4))
            ))))
          | Variant { tag = "Pcl_apply"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) x2) ~f:(fun x2 ->
                Some (Pcl_apply (x1, x2))
            ))
          | Variant { tag = "Pcl_let"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pcl_let (x1, x2, x3))
            )))
          | Variant { tag = "Pcl_constraint"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pcl_constraint (x1, x2))
            ))
          | Variant { tag = "Pcl_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pcl_extension (x1))
            )
          | Variant { tag = "Pcl_open"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pcl_open (x1, x2, x3))
            )))
        | _ -> None
        end
      | _ -> None
  end

  module Class_structure = struct
    type t = class_structure

    type concrete =
      { pcstr_self : pattern
      ; pcstr_fields : class_field list
      }

    let create ~pcstr_self ~pcstr_fields =
      let fields =
        [| Data.of_node pcstr_self
         ; (Data.of_list ~f:Data.of_node) pcstr_fields
        |]
      in
      node "class_structure" (Record fields)

    let of_concrete { pcstr_self; pcstr_fields } =
      create ~pcstr_self ~pcstr_fields

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_structure"
        ; data = Record [| pcstr_self; pcstr_fields |]
        } ->
          Helpers.Option.bind (Data.to_node pcstr_self) ~f:(fun pcstr_self ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) pcstr_fields) ~f:(fun pcstr_fields ->
              Some { pcstr_self; pcstr_fields }
          ))
      | _ -> None
  end

  module Class_field = struct
    type t = class_field

    type concrete =
      { pcf_desc : class_field_desc
      ; pcf_loc : Astlib.Location.t
      ; pcf_attributes : attributes
      }

    let create ~pcf_desc ~pcf_loc ~pcf_attributes =
      let fields =
        [| Data.of_node pcf_desc
         ; Data.of_location pcf_loc
         ; Data.of_node pcf_attributes
        |]
      in
      node "class_field" (Record fields)

    let of_concrete { pcf_desc; pcf_loc; pcf_attributes } =
      create ~pcf_desc ~pcf_loc ~pcf_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_field"
        ; data = Record [| pcf_desc; pcf_loc; pcf_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node pcf_desc) ~f:(fun pcf_desc ->
            Helpers.Option.bind (Data.to_location pcf_loc) ~f:(fun pcf_loc ->
              Helpers.Option.bind (Data.to_node pcf_attributes) ~f:(fun pcf_attributes ->
                Some { pcf_desc; pcf_loc; pcf_attributes }
          )))
      | _ -> None
  end

  module Class_field_desc = struct
    type t = class_field_desc

    type concrete =
      | Pcf_inherit of override_flag * class_expr * string Astlib.Loc.t option
      | Pcf_val of (label Astlib.Loc.t * mutable_flag * class_field_kind)
      | Pcf_method of (label Astlib.Loc.t * private_flag * class_field_kind)
      | Pcf_constraint of (core_type * core_type)
      | Pcf_initializer of expression
      | Pcf_attribute of attribute
      | Pcf_extension of extension

    let pcf_inherit x1 x2 x3 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_inherit"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; (Data.of_option ~f:(Data.of_loc ~f:Data.of_string)) x3
            |]
          })
    let pcf_val x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_val"
          ; args =
            [| (Data.of_tuple3 ~f1:(Data.of_loc ~f:Data.of_string) ~f2:Data.of_node ~f3:Data.of_node) x1
            |]
          })
    let pcf_method x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_method"
          ; args =
            [| (Data.of_tuple3 ~f1:(Data.of_loc ~f:Data.of_string) ~f2:Data.of_node ~f3:Data.of_node) x1
            |]
          })
    let pcf_constraint x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_constraint"
          ; args =
            [| (Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node) x1
            |]
          })
    let pcf_initializer x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_initializer"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pcf_attribute x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pcf_extension x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_extension"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete c =
      match c with
      | Pcf_inherit (x1, x2, x3) ->
        pcf_inherit x1 x2 x3
      | Pcf_val (x1) ->
        pcf_val x1
      | Pcf_method (x1) ->
        pcf_method x1
      | Pcf_constraint (x1) ->
        pcf_constraint x1
      | Pcf_initializer (x1) ->
        pcf_initializer x1
      | Pcf_attribute (x1) ->
        pcf_attribute x1
      | Pcf_extension (x1) ->
        pcf_extension x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_field_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pcf_inherit"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind ((Data.to_option ~f:(Data.to_loc ~f:Data.to_string)) x3) ~f:(fun x3 ->
                  Some (Pcf_inherit (x1, x2, x3))
            )))
          | Variant { tag = "Pcf_val"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple3 ~f1:(Data.to_loc ~f:Data.to_string) ~f2:Data.to_node ~f3:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pcf_val (x1))
            )
          | Variant { tag = "Pcf_method"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple3 ~f1:(Data.to_loc ~f:Data.to_string) ~f2:Data.to_node ~f3:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pcf_method (x1))
            )
          | Variant { tag = "Pcf_constraint"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pcf_constraint (x1))
            )
          | Variant { tag = "Pcf_initializer"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pcf_initializer (x1))
            )
          | Variant { tag = "Pcf_attribute"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pcf_attribute (x1))
            )
          | Variant { tag = "Pcf_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pcf_extension (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Class_field_kind = struct
    type t = class_field_kind

    type concrete =
      | Cfk_virtual of core_type
      | Cfk_concrete of override_flag * expression

    let cfk_virtual x1 =
      node "class_field_kind"
        (Variant
          { tag = "Cfk_virtual"
          ; args =
            [| Data.of_node x1
            |]
          })
    let cfk_concrete x1 x2 =
      node "class_field_kind"
        (Variant
          { tag = "Cfk_concrete"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete c =
      match c with
      | Cfk_virtual (x1) ->
        cfk_virtual x1
      | Cfk_concrete (x1, x2) ->
        cfk_concrete x1 x2

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "class_field_kind"; data } ->
        begin
          match data with
          | Variant { tag = "Cfk_virtual"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Cfk_virtual (x1))
            )
          | Variant { tag = "Cfk_concrete"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Cfk_concrete (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Class_declaration = struct
    type t = class_expr class_infos
  end

  module Module_type = struct
    type t = module_type

    type concrete =
      { pmty_desc : module_type_desc
      ; pmty_loc : Astlib.Location.t
      ; pmty_attributes : attributes
      }

    let create ~pmty_desc ~pmty_loc ~pmty_attributes =
      let fields =
        [| Data.of_node pmty_desc
         ; Data.of_location pmty_loc
         ; Data.of_node pmty_attributes
        |]
      in
      node "module_type" (Record fields)

    let of_concrete { pmty_desc; pmty_loc; pmty_attributes } =
      create ~pmty_desc ~pmty_loc ~pmty_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "module_type"
        ; data = Record [| pmty_desc; pmty_loc; pmty_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node pmty_desc) ~f:(fun pmty_desc ->
            Helpers.Option.bind (Data.to_location pmty_loc) ~f:(fun pmty_loc ->
              Helpers.Option.bind (Data.to_node pmty_attributes) ~f:(fun pmty_attributes ->
                Some { pmty_desc; pmty_loc; pmty_attributes }
          )))
      | _ -> None
  end

  module Module_type_desc = struct
    type t = module_type_desc

    type concrete =
      | Pmty_ident of longident_loc
      | Pmty_signature of signature
      | Pmty_functor of string Astlib.Loc.t * module_type option * module_type
      | Pmty_with of module_type * with_constraint list
      | Pmty_typeof of module_expr
      | Pmty_extension of extension
      | Pmty_alias of longident_loc

    let pmty_ident x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_ident"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
            |]
          })
    let pmty_signature x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_signature"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pmty_functor x1 x2 x3 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_functor"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; (Data.of_option ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let pmty_with x1 x2 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_with"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let pmty_typeof x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_typeof"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pmty_extension x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pmty_alias x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_alias"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
            |]
          })

    let of_concrete c =
      match c with
      | Pmty_ident (x1) ->
        pmty_ident x1
      | Pmty_signature (x1) ->
        pmty_signature x1
      | Pmty_functor (x1, x2, x3) ->
        pmty_functor x1 x2 x3
      | Pmty_with (x1, x2) ->
        pmty_with x1 x2
      | Pmty_typeof (x1) ->
        pmty_typeof x1
      | Pmty_extension (x1) ->
        pmty_extension x1
      | Pmty_alias (x1) ->
        pmty_alias x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "module_type_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pmty_ident"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pmty_ident (x1))
            )
          | Variant { tag = "Pmty_signature"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pmty_signature (x1))
            )
          | Variant { tag = "Pmty_functor"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pmty_functor (x1, x2, x3))
            )))
          | Variant { tag = "Pmty_with"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pmty_with (x1, x2))
            ))
          | Variant { tag = "Pmty_typeof"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pmty_typeof (x1))
            )
          | Variant { tag = "Pmty_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pmty_extension (x1))
            )
          | Variant { tag = "Pmty_alias"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pmty_alias (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Signature = struct
    type t = signature

    type concrete = signature_item list

    let create =
      let data = (Data.of_list ~f:Data.of_node) in
      fun x -> node "signature" (data x)

    let of_concrete = create

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "signature"; data } -> (Data.to_list ~f:Data.to_node) data
      | _ -> None
  end

  module Signature_item = struct
    type t = signature_item

    type concrete =
      { psig_desc : signature_item_desc
      ; psig_loc : Astlib.Location.t
      }

    let create ~psig_desc ~psig_loc =
      let fields =
        [| Data.of_node psig_desc
         ; Data.of_location psig_loc
        |]
      in
      node "signature_item" (Record fields)

    let of_concrete { psig_desc; psig_loc } =
      create ~psig_desc ~psig_loc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "signature_item"
        ; data = Record [| psig_desc; psig_loc |]
        } ->
          Helpers.Option.bind (Data.to_node psig_desc) ~f:(fun psig_desc ->
            Helpers.Option.bind (Data.to_location psig_loc) ~f:(fun psig_loc ->
              Some { psig_desc; psig_loc }
          ))
      | _ -> None
  end

  module Signature_item_desc = struct
    type t = signature_item_desc

    type concrete =
      | Psig_value of value_description
      | Psig_type of rec_flag * type_declaration list
      | Psig_typext of type_extension
      | Psig_exception of extension_constructor
      | Psig_module of module_declaration
      | Psig_recmodule of module_declaration list
      | Psig_modtype of module_type_declaration
      | Psig_open of open_description
      | Psig_include of include_description
      | Psig_class of class_description list
      | Psig_class_type of class_type_declaration list
      | Psig_attribute of attribute
      | Psig_extension of extension * attributes

    let psig_value x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_value"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig_type x1 x2 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_type"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let psig_typext x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_typext"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig_exception x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_exception"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig_module x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_module"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig_recmodule x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_recmodule"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let psig_modtype x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_modtype"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig_open x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_open"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig_include x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_include"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig_class x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_class"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let psig_class_type x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_class_type"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let psig_attribute x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let psig_extension x1 x2 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_extension"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete c =
      match c with
      | Psig_value (x1) ->
        psig_value x1
      | Psig_type (x1, x2) ->
        psig_type x1 x2
      | Psig_typext (x1) ->
        psig_typext x1
      | Psig_exception (x1) ->
        psig_exception x1
      | Psig_module (x1) ->
        psig_module x1
      | Psig_recmodule (x1) ->
        psig_recmodule x1
      | Psig_modtype (x1) ->
        psig_modtype x1
      | Psig_open (x1) ->
        psig_open x1
      | Psig_include (x1) ->
        psig_include x1
      | Psig_class (x1) ->
        psig_class x1
      | Psig_class_type (x1) ->
        psig_class_type x1
      | Psig_attribute (x1) ->
        psig_attribute x1
      | Psig_extension (x1, x2) ->
        psig_extension x1 x2

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "signature_item_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Psig_value"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_value (x1))
            )
          | Variant { tag = "Psig_type"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Psig_type (x1, x2))
            ))
          | Variant { tag = "Psig_typext"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_typext (x1))
            )
          | Variant { tag = "Psig_exception"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_exception (x1))
            )
          | Variant { tag = "Psig_module"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_module (x1))
            )
          | Variant { tag = "Psig_recmodule"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Psig_recmodule (x1))
            )
          | Variant { tag = "Psig_modtype"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_modtype (x1))
            )
          | Variant { tag = "Psig_open"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_open (x1))
            )
          | Variant { tag = "Psig_include"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_include (x1))
            )
          | Variant { tag = "Psig_class"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Psig_class (x1))
            )
          | Variant { tag = "Psig_class_type"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Psig_class_type (x1))
            )
          | Variant { tag = "Psig_attribute"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Psig_attribute (x1))
            )
          | Variant { tag = "Psig_extension"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Psig_extension (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Module_declaration = struct
    type t = module_declaration

    type concrete =
      { pmd_name : string Astlib.Loc.t
      ; pmd_type : module_type
      ; pmd_attributes : attributes
      ; pmd_loc : Astlib.Location.t
      }

    let create ~pmd_name ~pmd_type ~pmd_attributes ~pmd_loc =
      let fields =
        [| (Data.of_loc ~f:Data.of_string) pmd_name
         ; Data.of_node pmd_type
         ; Data.of_node pmd_attributes
         ; Data.of_location pmd_loc
        |]
      in
      node "module_declaration" (Record fields)

    let of_concrete { pmd_name; pmd_type; pmd_attributes; pmd_loc } =
      create ~pmd_name ~pmd_type ~pmd_attributes ~pmd_loc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "module_declaration"
        ; data = Record [| pmd_name; pmd_type; pmd_attributes; pmd_loc |]
        } ->
          Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pmd_name) ~f:(fun pmd_name ->
            Helpers.Option.bind (Data.to_node pmd_type) ~f:(fun pmd_type ->
              Helpers.Option.bind (Data.to_node pmd_attributes) ~f:(fun pmd_attributes ->
                Helpers.Option.bind (Data.to_location pmd_loc) ~f:(fun pmd_loc ->
                  Some { pmd_name; pmd_type; pmd_attributes; pmd_loc }
          ))))
      | _ -> None
  end

  module Module_type_declaration = struct
    type t = module_type_declaration

    type concrete =
      { pmtd_name : string Astlib.Loc.t
      ; pmtd_type : module_type option
      ; pmtd_attributes : attributes
      ; pmtd_loc : Astlib.Location.t
      }

    let create ~pmtd_name ~pmtd_type ~pmtd_attributes ~pmtd_loc =
      let fields =
        [| (Data.of_loc ~f:Data.of_string) pmtd_name
         ; (Data.of_option ~f:Data.of_node) pmtd_type
         ; Data.of_node pmtd_attributes
         ; Data.of_location pmtd_loc
        |]
      in
      node "module_type_declaration" (Record fields)

    let of_concrete { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } =
      create ~pmtd_name ~pmtd_type ~pmtd_attributes ~pmtd_loc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "module_type_declaration"
        ; data = Record [| pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc |]
        } ->
          Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pmtd_name) ~f:(fun pmtd_name ->
            Helpers.Option.bind ((Data.to_option ~f:Data.to_node) pmtd_type) ~f:(fun pmtd_type ->
              Helpers.Option.bind (Data.to_node pmtd_attributes) ~f:(fun pmtd_attributes ->
                Helpers.Option.bind (Data.to_location pmtd_loc) ~f:(fun pmtd_loc ->
                  Some { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }
          ))))
      | _ -> None
  end

  module Open_description = struct
    type t = open_description

    type concrete =
      { popen_lid : longident_loc
      ; popen_override : override_flag
      ; popen_loc : Astlib.Location.t
      ; popen_attributes : attributes
      }

    let create ~popen_lid ~popen_override ~popen_loc ~popen_attributes =
      let fields =
        [| (Data.of_loc ~f:Data.of_node) popen_lid
         ; Data.of_node popen_override
         ; Data.of_location popen_loc
         ; Data.of_node popen_attributes
        |]
      in
      node "open_description" (Record fields)

    let of_concrete { popen_lid; popen_override; popen_loc; popen_attributes } =
      create ~popen_lid ~popen_override ~popen_loc ~popen_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "open_description"
        ; data = Record [| popen_lid; popen_override; popen_loc; popen_attributes |]
        } ->
          Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) popen_lid) ~f:(fun popen_lid ->
            Helpers.Option.bind (Data.to_node popen_override) ~f:(fun popen_override ->
              Helpers.Option.bind (Data.to_location popen_loc) ~f:(fun popen_loc ->
                Helpers.Option.bind (Data.to_node popen_attributes) ~f:(fun popen_attributes ->
                  Some { popen_lid; popen_override; popen_loc; popen_attributes }
          ))))
      | _ -> None
  end

  module Include_infos = struct
    type 'a t = 'a include_infos

    type 'a concrete =
      { pincl_mod : 'a
      ; pincl_loc : Astlib.Location.t
      ; pincl_attributes : attributes
      }

    let create ~pincl_mod ~pincl_loc ~pincl_attributes =
      let fields =
        [| Data.of_node pincl_mod
         ; Data.of_location pincl_loc
         ; Data.of_node pincl_attributes
        |]
      in
      node "include_infos" (Record fields)

    let of_concrete { pincl_mod; pincl_loc; pincl_attributes } =
      create ~pincl_mod ~pincl_loc ~pincl_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "include_infos"
        ; data = Record [| pincl_mod; pincl_loc; pincl_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node pincl_mod) ~f:(fun pincl_mod ->
            Helpers.Option.bind (Data.to_location pincl_loc) ~f:(fun pincl_loc ->
              Helpers.Option.bind (Data.to_node pincl_attributes) ~f:(fun pincl_attributes ->
                Some { pincl_mod; pincl_loc; pincl_attributes }
          )))
      | _ -> None
  end

  module Include_description = struct
    type t = module_type include_infos
  end

  module Include_declaration = struct
    type t = module_expr include_infos
  end

  module With_constraint = struct
    type t = with_constraint

    type concrete =
      | Pwith_type of longident_loc * type_declaration
      | Pwith_module of longident_loc * longident_loc
      | Pwith_typesubst of longident_loc * type_declaration
      | Pwith_modsubst of longident_loc * longident_loc

    let pwith_type x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_type"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let pwith_module x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_module"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })
    let pwith_typesubst x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_typesubst"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let pwith_modsubst x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_modsubst"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })

    let of_concrete c =
      match c with
      | Pwith_type (x1, x2) ->
        pwith_type x1 x2
      | Pwith_module (x1, x2) ->
        pwith_module x1 x2
      | Pwith_typesubst (x1, x2) ->
        pwith_typesubst x1 x2
      | Pwith_modsubst (x1, x2) ->
        pwith_modsubst x1 x2

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "with_constraint"; data } ->
        begin
          match data with
          | Variant { tag = "Pwith_type"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pwith_type (x1, x2))
            ))
          | Variant { tag = "Pwith_module"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pwith_module (x1, x2))
            ))
          | Variant { tag = "Pwith_typesubst"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pwith_typesubst (x1, x2))
            ))
          | Variant { tag = "Pwith_modsubst"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pwith_modsubst (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Module_expr = struct
    type t = module_expr

    type concrete =
      { pmod_desc : module_expr_desc
      ; pmod_loc : Astlib.Location.t
      ; pmod_attributes : attributes
      }

    let create ~pmod_desc ~pmod_loc ~pmod_attributes =
      let fields =
        [| Data.of_node pmod_desc
         ; Data.of_location pmod_loc
         ; Data.of_node pmod_attributes
        |]
      in
      node "module_expr" (Record fields)

    let of_concrete { pmod_desc; pmod_loc; pmod_attributes } =
      create ~pmod_desc ~pmod_loc ~pmod_attributes

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "module_expr"
        ; data = Record [| pmod_desc; pmod_loc; pmod_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node pmod_desc) ~f:(fun pmod_desc ->
            Helpers.Option.bind (Data.to_location pmod_loc) ~f:(fun pmod_loc ->
              Helpers.Option.bind (Data.to_node pmod_attributes) ~f:(fun pmod_attributes ->
                Some { pmod_desc; pmod_loc; pmod_attributes }
          )))
      | _ -> None
  end

  module Module_expr_desc = struct
    type t = module_expr_desc

    type concrete =
      | Pmod_ident of longident_loc
      | Pmod_structure of structure
      | Pmod_functor of string Astlib.Loc.t * module_type option * module_expr
      | Pmod_apply of module_expr * module_expr
      | Pmod_constraint of module_expr * module_type
      | Pmod_unpack of expression
      | Pmod_extension of extension

    let pmod_ident x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_ident"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
            |]
          })
    let pmod_structure x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_structure"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pmod_functor x1 x2 x3 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_functor"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; (Data.of_option ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let pmod_apply x1 x2 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_apply"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pmod_constraint x1 x2 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pmod_unpack x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_unpack"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pmod_extension x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_extension"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete c =
      match c with
      | Pmod_ident (x1) ->
        pmod_ident x1
      | Pmod_structure (x1) ->
        pmod_structure x1
      | Pmod_functor (x1, x2, x3) ->
        pmod_functor x1 x2 x3
      | Pmod_apply (x1, x2) ->
        pmod_apply x1 x2
      | Pmod_constraint (x1, x2) ->
        pmod_constraint x1 x2
      | Pmod_unpack (x1) ->
        pmod_unpack x1
      | Pmod_extension (x1) ->
        pmod_extension x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "module_expr_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pmod_ident"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pmod_ident (x1))
            )
          | Variant { tag = "Pmod_structure"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pmod_structure (x1))
            )
          | Variant { tag = "Pmod_functor"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pmod_functor (x1, x2, x3))
            )))
          | Variant { tag = "Pmod_apply"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pmod_apply (x1, x2))
            ))
          | Variant { tag = "Pmod_constraint"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pmod_constraint (x1, x2))
            ))
          | Variant { tag = "Pmod_unpack"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pmod_unpack (x1))
            )
          | Variant { tag = "Pmod_extension"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pmod_extension (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Structure = struct
    type t = structure

    type concrete = structure_item list

    let create =
      let data = (Data.of_list ~f:Data.of_node) in
      fun x -> node "structure" (data x)

    let of_concrete = create

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "structure"; data } -> (Data.to_list ~f:Data.to_node) data
      | _ -> None
  end

  module Structure_item = struct
    type t = structure_item

    type concrete =
      { pstr_desc : structure_item_desc
      ; pstr_loc : Astlib.Location.t
      }

    let create ~pstr_desc ~pstr_loc =
      let fields =
        [| Data.of_node pstr_desc
         ; Data.of_location pstr_loc
        |]
      in
      node "structure_item" (Record fields)

    let of_concrete { pstr_desc; pstr_loc } =
      create ~pstr_desc ~pstr_loc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "structure_item"
        ; data = Record [| pstr_desc; pstr_loc |]
        } ->
          Helpers.Option.bind (Data.to_node pstr_desc) ~f:(fun pstr_desc ->
            Helpers.Option.bind (Data.to_location pstr_loc) ~f:(fun pstr_loc ->
              Some { pstr_desc; pstr_loc }
          ))
      | _ -> None
  end

  module Structure_item_desc = struct
    type t = structure_item_desc

    type concrete =
      | Pstr_eval of expression * attributes
      | Pstr_value of rec_flag * value_binding list
      | Pstr_primitive of value_description
      | Pstr_type of rec_flag * type_declaration list
      | Pstr_typext of type_extension
      | Pstr_exception of extension_constructor
      | Pstr_module of module_binding
      | Pstr_recmodule of module_binding list
      | Pstr_modtype of module_type_declaration
      | Pstr_open of open_description
      | Pstr_class of class_declaration list
      | Pstr_class_type of class_type_declaration list
      | Pstr_include of include_declaration
      | Pstr_attribute of attribute
      | Pstr_extension of extension * attributes

    let pstr_eval x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_eval"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let pstr_value x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_value"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let pstr_primitive x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_primitive"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_type x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_type"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let pstr_typext x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_typext"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_exception x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_exception"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_module x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_module"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_recmodule x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_recmodule"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let pstr_modtype x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_modtype"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_open x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_open"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_class x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_class"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let pstr_class_type x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_class_type"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let pstr_include x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_include"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_attribute x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pstr_extension x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_extension"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete c =
      match c with
      | Pstr_eval (x1, x2) ->
        pstr_eval x1 x2
      | Pstr_value (x1, x2) ->
        pstr_value x1 x2
      | Pstr_primitive (x1) ->
        pstr_primitive x1
      | Pstr_type (x1, x2) ->
        pstr_type x1 x2
      | Pstr_typext (x1) ->
        pstr_typext x1
      | Pstr_exception (x1) ->
        pstr_exception x1
      | Pstr_module (x1) ->
        pstr_module x1
      | Pstr_recmodule (x1) ->
        pstr_recmodule x1
      | Pstr_modtype (x1) ->
        pstr_modtype x1
      | Pstr_open (x1) ->
        pstr_open x1
      | Pstr_class (x1) ->
        pstr_class x1
      | Pstr_class_type (x1) ->
        pstr_class_type x1
      | Pstr_include (x1) ->
        pstr_include x1
      | Pstr_attribute (x1) ->
        pstr_attribute x1
      | Pstr_extension (x1, x2) ->
        pstr_extension x1 x2

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "structure_item_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pstr_eval"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pstr_eval (x1, x2))
            ))
          | Variant { tag = "Pstr_value"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pstr_value (x1, x2))
            ))
          | Variant { tag = "Pstr_primitive"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_primitive (x1))
            )
          | Variant { tag = "Pstr_type"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pstr_type (x1, x2))
            ))
          | Variant { tag = "Pstr_typext"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_typext (x1))
            )
          | Variant { tag = "Pstr_exception"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_exception (x1))
            )
          | Variant { tag = "Pstr_module"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_module (x1))
            )
          | Variant { tag = "Pstr_recmodule"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pstr_recmodule (x1))
            )
          | Variant { tag = "Pstr_modtype"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_modtype (x1))
            )
          | Variant { tag = "Pstr_open"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_open (x1))
            )
          | Variant { tag = "Pstr_class"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pstr_class (x1))
            )
          | Variant { tag = "Pstr_class_type"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pstr_class_type (x1))
            )
          | Variant { tag = "Pstr_include"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_include (x1))
            )
          | Variant { tag = "Pstr_attribute"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pstr_attribute (x1))
            )
          | Variant { tag = "Pstr_extension"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pstr_extension (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Value_binding = struct
    type t = value_binding

    type concrete =
      { pvb_pat : pattern
      ; pvb_expr : expression
      ; pvb_attributes : attributes
      ; pvb_loc : Astlib.Location.t
      }

    let create ~pvb_pat ~pvb_expr ~pvb_attributes ~pvb_loc =
      let fields =
        [| Data.of_node pvb_pat
         ; Data.of_node pvb_expr
         ; Data.of_node pvb_attributes
         ; Data.of_location pvb_loc
        |]
      in
      node "value_binding" (Record fields)

    let of_concrete { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } =
      create ~pvb_pat ~pvb_expr ~pvb_attributes ~pvb_loc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "value_binding"
        ; data = Record [| pvb_pat; pvb_expr; pvb_attributes; pvb_loc |]
        } ->
          Helpers.Option.bind (Data.to_node pvb_pat) ~f:(fun pvb_pat ->
            Helpers.Option.bind (Data.to_node pvb_expr) ~f:(fun pvb_expr ->
              Helpers.Option.bind (Data.to_node pvb_attributes) ~f:(fun pvb_attributes ->
                Helpers.Option.bind (Data.to_location pvb_loc) ~f:(fun pvb_loc ->
                  Some { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }
          ))))
      | _ -> None
  end

  module Module_binding = struct
    type t = module_binding

    type concrete =
      { pmb_name : string Astlib.Loc.t
      ; pmb_expr : module_expr
      ; pmb_attributes : attributes
      ; pmb_loc : Astlib.Location.t
      }

    let create ~pmb_name ~pmb_expr ~pmb_attributes ~pmb_loc =
      let fields =
        [| (Data.of_loc ~f:Data.of_string) pmb_name
         ; Data.of_node pmb_expr
         ; Data.of_node pmb_attributes
         ; Data.of_location pmb_loc
        |]
      in
      node "module_binding" (Record fields)

    let of_concrete { pmb_name; pmb_expr; pmb_attributes; pmb_loc } =
      create ~pmb_name ~pmb_expr ~pmb_attributes ~pmb_loc

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "module_binding"
        ; data = Record [| pmb_name; pmb_expr; pmb_attributes; pmb_loc |]
        } ->
          Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pmb_name) ~f:(fun pmb_name ->
            Helpers.Option.bind (Data.to_node pmb_expr) ~f:(fun pmb_expr ->
              Helpers.Option.bind (Data.to_node pmb_attributes) ~f:(fun pmb_attributes ->
                Helpers.Option.bind (Data.to_location pmb_loc) ~f:(fun pmb_loc ->
                  Some { pmb_name; pmb_expr; pmb_attributes; pmb_loc }
          ))))
      | _ -> None
  end

  module Toplevel_phrase = struct
    type t = toplevel_phrase

    type concrete =
      | Ptop_def of structure
      | Ptop_dir of string * directive_argument

    let ptop_def x1 =
      node "toplevel_phrase"
        (Variant
          { tag = "Ptop_def"
          ; args =
            [| Data.of_node x1
            |]
          })
    let ptop_dir x1 x2 =
      node "toplevel_phrase"
        (Variant
          { tag = "Ptop_dir"
          ; args =
            [| Data.of_string x1
             ; Data.of_node x2
            |]
          })

    let of_concrete c =
      match c with
      | Ptop_def (x1) ->
        ptop_def x1
      | Ptop_dir (x1, x2) ->
        ptop_dir x1 x2

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "toplevel_phrase"; data } ->
        begin
          match data with
          | Variant { tag = "Ptop_def"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Ptop_def (x1))
            )
          | Variant { tag = "Ptop_dir"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ptop_dir (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Directive_argument = struct
    type t = directive_argument

    type concrete =
      | Pdir_none
      | Pdir_string of string
      | Pdir_int of string * char option
      | Pdir_ident of longident
      | Pdir_bool of bool

    let pdir_none =
      node "directive_argument" (Variant { tag = "Pdir_none"; args = [||] })
    let pdir_string x1 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_string"
          ; args =
            [| Data.of_string x1
            |]
          })
    let pdir_int x1 x2 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_int"
          ; args =
            [| Data.of_string x1
             ; (Data.of_option ~f:Data.of_char) x2
            |]
          })
    let pdir_ident x1 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_ident"
          ; args =
            [| Data.of_node x1
            |]
          })
    let pdir_bool x1 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_bool"
          ; args =
            [| Data.of_bool x1
            |]
          })

    let of_concrete c =
      match c with
      | Pdir_none -> pdir_none
      | Pdir_string (x1) ->
        pdir_string x1
      | Pdir_int (x1, x2) ->
        pdir_int x1 x2
      | Pdir_ident (x1) ->
        pdir_ident x1
      | Pdir_bool (x1) ->
        pdir_bool x1

    let to_concrete t =
      match Node.to_node t ~version with
      | { name = "directive_argument"; data } ->
        begin
          match data with
          | Variant { tag = "Pdir_none"; args = [||] } -> Some Pdir_none
          | Variant { tag = "Pdir_string"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Some (Pdir_string (x1))
            )
          | Variant { tag = "Pdir_int"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_string x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_char) x2) ~f:(fun x2 ->
                Some (Pdir_int (x1, x2))
            ))
          | Variant { tag = "Pdir_ident"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pdir_ident (x1))
            )
          | Variant { tag = "Pdir_bool"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_bool x1) ~f:(fun x1 ->
              Some (Pdir_bool (x1))
            )
        | _ -> None
        end
      | _ -> None
  end
end
(*$*)
