open Ocaml_common

module Helpers = struct
  module Option = struct
    let bind option ~f =
      match option with
      | None -> None
      | Some x -> f x
  end
end

(*$ Ppx_ast_versioned_cinaps.print_versions_ml () *)
module V4_07 = struct
  let version = "v4.07"
  let node name data = Node.of_node ~version { name; data }

  module Longident = struct
    type t = Node.t

    type concrete =
      | Lident of string
      | Ldot of Node.t * string
      | Lapply of Node.t * Node.t

    let create_lident x1 =
      node "longident"
        (Variant
          { tag = "Lident"
          ; args =
            [| Data.of_string x1
            |]
          })
    let create_ldot x1 x2 =
      node "longident"
        (Variant
          { tag = "Ldot"
          ; args =
            [| Data.of_node x1
             ; Data.of_string x2
            |]
          })
    let create_lapply x1 x2 =
      node "longident"
        (Variant
          { tag = "Lapply"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Lident (x1) ->
        create_lident x1
      | Ldot (x1, x2) ->
        create_ldot x1 x2
      | Lapply (x1, x2) ->
        create_lapply x1 x2

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete = Node.t Location.loc

    let create =
      let data = (Data.of_loc ~f:Data.of_node) in
      fun x -> node "longident_loc" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "longident_loc"; data } -> (Data.to_loc ~f:Data.to_node) data
      | _ -> None
  end

  module Rec_flag = struct
    type t = Node.t

    type concrete =
      | Nonrecursive
      | Recursive

    let create_nonrecursive =
      node "rec_flag" (Variant { tag = "Nonrecursive"; args = [||] })
    let create_recursive =
      node "rec_flag" (Variant { tag = "Recursive"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Nonrecursive -> create_nonrecursive
      | Recursive -> create_recursive

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Upto
      | Downto

    let create_upto =
      node "direction_flag" (Variant { tag = "Upto"; args = [||] })
    let create_downto =
      node "direction_flag" (Variant { tag = "Downto"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Upto -> create_upto
      | Downto -> create_downto

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Private
      | Public

    let create_private =
      node "private_flag" (Variant { tag = "Private"; args = [||] })
    let create_public =
      node "private_flag" (Variant { tag = "Public"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Private -> create_private
      | Public -> create_public

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Immutable
      | Mutable

    let create_immutable =
      node "mutable_flag" (Variant { tag = "Immutable"; args = [||] })
    let create_mutable =
      node "mutable_flag" (Variant { tag = "Mutable"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Immutable -> create_immutable
      | Mutable -> create_mutable

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Virtual
      | Concrete

    let create_virtual =
      node "virtual_flag" (Variant { tag = "Virtual"; args = [||] })
    let create_concrete =
      node "virtual_flag" (Variant { tag = "Concrete"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Virtual -> create_virtual
      | Concrete -> create_concrete

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Override
      | Fresh

    let create_override =
      node "override_flag" (Variant { tag = "Override"; args = [||] })
    let create_fresh =
      node "override_flag" (Variant { tag = "Fresh"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Override -> create_override
      | Fresh -> create_fresh

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Closed
      | Open

    let create_closed =
      node "closed_flag" (Variant { tag = "Closed"; args = [||] })
    let create_open =
      node "closed_flag" (Variant { tag = "Open"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Closed -> create_closed
      | Open -> create_open

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete = string

    let create =
      let data = Data.of_string in
      fun x -> node "label" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "label"; data } -> Data.to_string data
      | _ -> None
  end

  module Arg_label = struct
    type t = Node.t

    type concrete =
      | Nolabel
      | Labelled of string
      | Optional of string

    let create_nolabel =
      node "arg_label" (Variant { tag = "Nolabel"; args = [||] })
    let create_labelled x1 =
      node "arg_label"
        (Variant
          { tag = "Labelled"
          ; args =
            [| Data.of_string x1
            |]
          })
    let create_optional x1 =
      node "arg_label"
        (Variant
          { tag = "Optional"
          ; args =
            [| Data.of_string x1
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Nolabel -> create_nolabel
      | Labelled (x1) ->
        create_labelled x1
      | Optional (x1) ->
        create_optional x1

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Covariant
      | Contravariant
      | Invariant

    let create_covariant =
      node "variance" (Variant { tag = "Covariant"; args = [||] })
    let create_contravariant =
      node "variance" (Variant { tag = "Contravariant"; args = [||] })
    let create_invariant =
      node "variance" (Variant { tag = "Invariant"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Covariant -> create_covariant
      | Contravariant -> create_contravariant
      | Invariant -> create_invariant

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pconst_integer of string * char option
      | Pconst_char of char
      | Pconst_string of string * string option
      | Pconst_float of string * char option

    let create_pconst_integer x1 x2 =
      node "constant"
        (Variant
          { tag = "Pconst_integer"
          ; args =
            [| Data.of_string x1
             ; (Data.of_option ~f:Data.of_char) x2
            |]
          })
    let create_pconst_char x1 =
      node "constant"
        (Variant
          { tag = "Pconst_char"
          ; args =
            [| Data.of_char x1
            |]
          })
    let create_pconst_string x1 x2 =
      node "constant"
        (Variant
          { tag = "Pconst_string"
          ; args =
            [| Data.of_string x1
             ; (Data.of_option ~f:Data.of_string) x2
            |]
          })
    let create_pconst_float x1 x2 =
      node "constant"
        (Variant
          { tag = "Pconst_float"
          ; args =
            [| Data.of_string x1
             ; (Data.of_option ~f:Data.of_char) x2
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Pconst_integer (x1, x2) ->
        create_pconst_integer x1 x2
      | Pconst_char (x1) ->
        create_pconst_char x1
      | Pconst_string (x1, x2) ->
        create_pconst_string x1 x2
      | Pconst_float (x1, x2) ->
        create_pconst_float x1 x2

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete = (string Location.loc * Node.t)

    let create =
      let data = (Data.of_tuple2 ~f1:(Data.of_loc ~f:Data.of_string) ~f2:Data.of_node) in
      fun x -> node "attribute" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "attribute"; data } -> (Data.to_tuple2 ~f1:(Data.to_loc ~f:Data.to_string) ~f2:Data.to_node) data
      | _ -> None
  end

  module Extension = struct
    type t = Node.t

    type concrete = (string Location.loc * Node.t)

    let create =
      let data = (Data.of_tuple2 ~f1:(Data.of_loc ~f:Data.of_string) ~f2:Data.of_node) in
      fun x -> node "extension" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "extension"; data } -> (Data.to_tuple2 ~f1:(Data.to_loc ~f:Data.to_string) ~f2:Data.to_node) data
      | _ -> None
  end

  module Attributes = struct
    type t = Node.t

    type concrete = Node.t list

    let create =
      let data = (Data.of_list ~f:Data.of_node) in
      fun x -> node "attributes" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "attributes"; data } -> (Data.to_list ~f:Data.to_node) data
      | _ -> None
  end

  module Payload = struct
    type t = Node.t

    type concrete =
      | PStr of Node.t
      | PSig of Node.t
      | PTyp of Node.t
      | PPat of Node.t * Node.t option

    let create_pstr x1 =
      node "payload"
        (Variant
          { tag = "PStr"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig x1 =
      node "payload"
        (Variant
          { tag = "PSig"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ptyp x1 =
      node "payload"
        (Variant
          { tag = "PTyp"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ppat x1 x2 =
      node "payload"
        (Variant
          { tag = "PPat"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | PStr (x1) ->
        create_pstr x1
      | PSig (x1) ->
        create_psig x1
      | PTyp (x1) ->
        create_ptyp x1
      | PPat (x1, x2) ->
        create_ppat x1 x2

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { ptyp_desc : Node.t
      ; ptyp_loc : Location.t
      ; ptyp_attributes : Node.t
      }

    let create ~ptyp_desc ~ptyp_loc ~ptyp_attributes =
      let fields =
        [| Data.of_node ptyp_desc
         ; Data.of_location ptyp_loc
         ; Data.of_node ptyp_attributes
        |]
      in
      node "core_type" (Record fields)

    let of_concrete ({ ptyp_desc; ptyp_loc; ptyp_attributes } : concrete) =
      create ~ptyp_desc ~ptyp_loc ~ptyp_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of Node.t * Node.t * Node.t
      | Ptyp_tuple of Node.t list
      | Ptyp_constr of Node.t * Node.t list
      | Ptyp_object of Node.t list * Node.t
      | Ptyp_class of Node.t * Node.t list
      | Ptyp_alias of Node.t * string
      | Ptyp_variant of Node.t list * Node.t * Node.t list option
      | Ptyp_poly of string Location.loc list * Node.t
      | Ptyp_package of Node.t
      | Ptyp_extension of Node.t

    let create_ptyp_any =
      node "core_type_desc" (Variant { tag = "Ptyp_any"; args = [||] })
    let create_ptyp_var x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_var"
          ; args =
            [| Data.of_string x1
            |]
          })
    let create_ptyp_arrow x1 x2 x3 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_arrow"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let create_ptyp_tuple x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_ptyp_constr x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_constr"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_ptyp_object x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_object"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let create_ptyp_class x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_class"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_ptyp_alias x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_alias"
          ; args =
            [| Data.of_node x1
             ; Data.of_string x2
            |]
          })
    let create_ptyp_variant x1 x2 x3 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_variant"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; Data.of_node x2
             ; (Data.of_option ~f:(Data.of_list ~f:Data.of_node)) x3
            |]
          })
    let create_ptyp_poly x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_poly"
          ; args =
            [| (Data.of_list ~f:(Data.of_loc ~f:Data.of_string)) x1
             ; Data.of_node x2
            |]
          })
    let create_ptyp_package x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_package"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ptyp_extension x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_extension"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
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
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Ptyp_constr (x1, x2))
            ))
          | Variant { tag = "Ptyp_object"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ptyp_object (x1, x2))
            ))
          | Variant { tag = "Ptyp_class"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
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
                Helpers.Option.bind ((Data.to_option ~f:(Data.to_list ~f:Data.to_node)) x3) ~f:(fun x3 ->
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
    type t = Node.t

    type concrete = (Node.t * (Node.t * Node.t) list)

    let create =
      let data = (Data.of_tuple2 ~f1:Data.of_node ~f2:(Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node))) in
      fun x -> node "package_type" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "package_type"; data } -> (Data.to_tuple2 ~f1:Data.to_node ~f2:(Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node))) data
      | _ -> None
  end

  module Row_field = struct
    type t = Node.t

    type concrete =
      | Rtag of Node.t Location.loc * Node.t * bool * Node.t list
      | Rinherit of Node.t

    let create_rtag x1 x2 x3 x4 =
      node "row_field"
        (Variant
          { tag = "Rtag"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; Data.of_node x2
             ; Data.of_bool x3
             ; (Data.of_list ~f:Data.of_node) x4
            |]
          })
    let create_rinherit x1 =
      node "row_field"
        (Variant
          { tag = "Rinherit"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Rtag (x1, x2, x3, x4) ->
        create_rtag x1 x2 x3 x4
      | Rinherit (x1) ->
        create_rinherit x1

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "row_field"; data } ->
        begin
          match data with
          | Variant { tag = "Rtag"; args = [| x1; x2; x3; x4 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
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
    type t = Node.t

    type concrete =
      | Otag of Node.t Location.loc * Node.t * Node.t
      | Oinherit of Node.t

    let create_otag x1 x2 x3 =
      node "object_field"
        (Variant
          { tag = "Otag"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let create_oinherit x1 =
      node "object_field"
        (Variant
          { tag = "Oinherit"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Otag (x1, x2, x3) ->
        create_otag x1 x2 x3
      | Oinherit (x1) ->
        create_oinherit x1

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "object_field"; data } ->
        begin
          match data with
          | Variant { tag = "Otag"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
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
    type t = Node.t

    type concrete =
      { ppat_desc : Node.t
      ; ppat_loc : Location.t
      ; ppat_attributes : Node.t
      }

    let create ~ppat_desc ~ppat_loc ~ppat_attributes =
      let fields =
        [| Data.of_node ppat_desc
         ; Data.of_location ppat_loc
         ; Data.of_node ppat_attributes
        |]
      in
      node "pattern" (Record fields)

    let of_concrete ({ ppat_desc; ppat_loc; ppat_attributes } : concrete) =
      create ~ppat_desc ~ppat_loc ~ppat_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Ppat_any
      | Ppat_var of string Location.loc
      | Ppat_alias of Node.t * string Location.loc
      | Ppat_constant of Node.t
      | Ppat_interval of Node.t * Node.t
      | Ppat_tuple of Node.t list
      | Ppat_construct of Node.t * Node.t option
      | Ppat_variant of Node.t * Node.t option
      | Ppat_record of (Node.t * Node.t) list * Node.t
      | Ppat_array of Node.t list
      | Ppat_or of Node.t * Node.t
      | Ppat_constraint of Node.t * Node.t
      | Ppat_type of Node.t
      | Ppat_lazy of Node.t
      | Ppat_unpack of string Location.loc
      | Ppat_exception of Node.t
      | Ppat_extension of Node.t
      | Ppat_open of Node.t * Node.t

    let create_ppat_any =
      node "pattern_desc" (Variant { tag = "Ppat_any"; args = [||] })
    let create_ppat_var x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_var"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
            |]
          })
    let create_ppat_alias x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_alias"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_string) x2
            |]
          })
    let create_ppat_constant x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_constant"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ppat_interval x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_interval"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_ppat_tuple x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_ppat_construct x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_construct"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let create_ppat_variant x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_variant"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let create_ppat_record x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_record"
          ; args =
            [| (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) x1
             ; Data.of_node x2
            |]
          })
    let create_ppat_array x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_array"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_ppat_or x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_or"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_ppat_constraint x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_ppat_type x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_type"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ppat_lazy x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_lazy"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ppat_unpack x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_unpack"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
            |]
          })
    let create_ppat_exception x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_exception"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ppat_extension x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ppat_open x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_open"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
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
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Ppat_construct (x1, x2))
            ))
          | Variant { tag = "Ppat_variant"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Ppat_variant (x1, x2))
            ))
          | Variant { tag = "Ppat_record"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) x1) ~f:(fun x1 ->
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
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
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
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ppat_open (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Expression = struct
    type t = Node.t

    type concrete =
      { pexp_desc : Node.t
      ; pexp_loc : Location.t
      ; pexp_attributes : Node.t
      }

    let create ~pexp_desc ~pexp_loc ~pexp_attributes =
      let fields =
        [| Data.of_node pexp_desc
         ; Data.of_location pexp_loc
         ; Data.of_node pexp_attributes
        |]
      in
      node "expression" (Record fields)

    let of_concrete ({ pexp_desc; pexp_loc; pexp_attributes } : concrete) =
      create ~pexp_desc ~pexp_loc ~pexp_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pexp_ident of Node.t
      | Pexp_constant of Node.t
      | Pexp_let of Node.t * Node.t list * Node.t
      | Pexp_function of Node.t list
      | Pexp_fun of Node.t * Node.t option * Node.t * Node.t
      | Pexp_apply of Node.t * (Node.t * Node.t) list
      | Pexp_match of Node.t * Node.t list
      | Pexp_try of Node.t * Node.t list
      | Pexp_tuple of Node.t list
      | Pexp_construct of Node.t * Node.t option
      | Pexp_variant of Node.t * Node.t option
      | Pexp_record of (Node.t * Node.t) list * Node.t option
      | Pexp_field of Node.t * Node.t
      | Pexp_setfield of Node.t * Node.t * Node.t
      | Pexp_array of Node.t list
      | Pexp_ifthenelse of Node.t * Node.t * Node.t option
      | Pexp_sequence of Node.t * Node.t
      | Pexp_while of Node.t * Node.t
      | Pexp_for of Node.t * Node.t * Node.t * Node.t * Node.t
      | Pexp_constraint of Node.t * Node.t
      | Pexp_coerce of Node.t * Node.t option * Node.t
      | Pexp_send of Node.t * Node.t Location.loc
      | Pexp_new of Node.t
      | Pexp_setinstvar of Node.t Location.loc * Node.t
      | Pexp_override of (Node.t Location.loc * Node.t) list
      | Pexp_letmodule of string Location.loc * Node.t * Node.t
      | Pexp_letexception of Node.t * Node.t
      | Pexp_assert of Node.t
      | Pexp_lazy of Node.t
      | Pexp_poly of Node.t * Node.t option
      | Pexp_object of Node.t
      | Pexp_newtype of string Location.loc * Node.t
      | Pexp_pack of Node.t
      | Pexp_open of Node.t * Node.t * Node.t
      | Pexp_extension of Node.t
      | Pexp_unreachable

    let create_pexp_ident x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_ident"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_constant x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_constant"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_let x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_let"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let create_pexp_function x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_function"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_pexp_fun x1 x2 x3 x4 =
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
    let create_pexp_apply x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_apply"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) x2
            |]
          })
    let create_pexp_match x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_match"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_pexp_try x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_try"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_pexp_tuple x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_pexp_construct x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_construct"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let create_pexp_variant x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_variant"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let create_pexp_record x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_record"
          ; args =
            [| (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let create_pexp_field x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_field"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pexp_setfield x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_setfield"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let create_pexp_array x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_array"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_pexp_ifthenelse x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_ifthenelse"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; (Data.of_option ~f:Data.of_node) x3
            |]
          })
    let create_pexp_sequence x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_sequence"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pexp_while x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_while"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pexp_for x1 x2 x3 x4 x5 =
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
    let create_pexp_constraint x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pexp_coerce x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_coerce"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let create_pexp_send x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_send"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })
    let create_pexp_new x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_new"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_setinstvar x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_setinstvar"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let create_pexp_override x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_override"
          ; args =
            [| (Data.of_list ~f:(Data.of_tuple2 ~f1:(Data.of_loc ~f:Data.of_node) ~f2:Data.of_node)) x1
            |]
          })
    let create_pexp_letmodule x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_letmodule"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let create_pexp_letexception x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_letexception"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pexp_assert x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_assert"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_lazy x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_lazy"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_poly x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_poly"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let create_pexp_object x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_object"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_newtype x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_newtype"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; Data.of_node x2
            |]
          })
    let create_pexp_pack x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_pack"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_open x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_open"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let create_pexp_extension x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_unreachable =
      node "expression_desc" (Variant { tag = "Pexp_unreachable"; args = [||] })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "expression_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pexp_ident"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
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
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_construct (x1, x2))
            ))
          | Variant { tag = "Pexp_variant"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_variant (x1, x2))
            ))
          | Variant { tag = "Pexp_record"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_record (x1, x2))
            ))
          | Variant { tag = "Pexp_field"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_field (x1, x2))
            ))
          | Variant { tag = "Pexp_setfield"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
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
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_send (x1, x2))
            ))
          | Variant { tag = "Pexp_new"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pexp_new (x1))
            )
          | Variant { tag = "Pexp_setinstvar"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_setinstvar (x1, x2))
            ))
          | Variant { tag = "Pexp_override"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:(Data.to_loc ~f:Data.to_node) ~f2:Data.to_node)) x1) ~f:(fun x1 ->
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
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
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
    type t = Node.t

    type concrete =
      { pc_lhs : Node.t
      ; pc_guard : Node.t option
      ; pc_rhs : Node.t
      }

    let create ~pc_lhs ~pc_guard ~pc_rhs =
      let fields =
        [| Data.of_node pc_lhs
         ; (Data.of_option ~f:Data.of_node) pc_guard
         ; Data.of_node pc_rhs
        |]
      in
      node "case" (Record fields)

    let of_concrete ({ pc_lhs; pc_guard; pc_rhs } : concrete) =
      create ~pc_lhs ~pc_guard ~pc_rhs

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pval_name : string Location.loc
      ; pval_type : Node.t
      ; pval_prim : string list
      ; pval_attributes : Node.t
      ; pval_loc : Location.t
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

    let of_concrete ({ pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : concrete) =
      create ~pval_name ~pval_type ~pval_prim ~pval_attributes ~pval_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { ptype_name : string Location.loc
      ; ptype_params : (Node.t * Node.t) list
      ; ptype_cstrs : (Node.t * Node.t * Location.t) list
      ; ptype_kind : Node.t
      ; ptype_private : Node.t
      ; ptype_manifest : Node.t option
      ; ptype_attributes : Node.t
      ; ptype_loc : Location.t
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

    let of_concrete ({ ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : concrete) =
      create ~ptype_name ~ptype_params ~ptype_cstrs ~ptype_kind ~ptype_private ~ptype_manifest ~ptype_attributes ~ptype_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Ptype_abstract
      | Ptype_variant of Node.t list
      | Ptype_record of Node.t list
      | Ptype_open

    let create_ptype_abstract =
      node "type_kind" (Variant { tag = "Ptype_abstract"; args = [||] })
    let create_ptype_variant x1 =
      node "type_kind"
        (Variant
          { tag = "Ptype_variant"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_ptype_record x1 =
      node "type_kind"
        (Variant
          { tag = "Ptype_record"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_ptype_open =
      node "type_kind" (Variant { tag = "Ptype_open"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Ptype_abstract -> create_ptype_abstract
      | Ptype_variant (x1) ->
        create_ptype_variant x1
      | Ptype_record (x1) ->
        create_ptype_record x1
      | Ptype_open -> create_ptype_open

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pld_name : string Location.loc
      ; pld_mutable : Node.t
      ; pld_type : Node.t
      ; pld_loc : Location.t
      ; pld_attributes : Node.t
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

    let of_concrete ({ pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : concrete) =
      create ~pld_name ~pld_mutable ~pld_type ~pld_loc ~pld_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pcd_name : string Location.loc
      ; pcd_args : Node.t
      ; pcd_res : Node.t option
      ; pcd_loc : Location.t
      ; pcd_attributes : Node.t
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

    let of_concrete ({ pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : concrete) =
      create ~pcd_name ~pcd_args ~pcd_res ~pcd_loc ~pcd_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pcstr_tuple of Node.t list
      | Pcstr_record of Node.t list

    let create_pcstr_tuple x1 =
      node "constructor_arguments"
        (Variant
          { tag = "Pcstr_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_pcstr_record x1 =
      node "constructor_arguments"
        (Variant
          { tag = "Pcstr_record"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Pcstr_tuple (x1) ->
        create_pcstr_tuple x1
      | Pcstr_record (x1) ->
        create_pcstr_record x1

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { ptyext_path : Node.t
      ; ptyext_params : (Node.t * Node.t) list
      ; ptyext_constructors : Node.t list
      ; ptyext_private : Node.t
      ; ptyext_attributes : Node.t
      }

    let create ~ptyext_path ~ptyext_params ~ptyext_constructors ~ptyext_private ~ptyext_attributes =
      let fields =
        [| Data.of_node ptyext_path
         ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) ptyext_params
         ; (Data.of_list ~f:Data.of_node) ptyext_constructors
         ; Data.of_node ptyext_private
         ; Data.of_node ptyext_attributes
        |]
      in
      node "type_extension" (Record fields)

    let of_concrete ({ ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : concrete) =
      create ~ptyext_path ~ptyext_params ~ptyext_constructors ~ptyext_private ~ptyext_attributes

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "type_extension"
        ; data = Record [| ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node ptyext_path) ~f:(fun ptyext_path ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) ptyext_params) ~f:(fun ptyext_params ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) ptyext_constructors) ~f:(fun ptyext_constructors ->
                Helpers.Option.bind (Data.to_node ptyext_private) ~f:(fun ptyext_private ->
                  Helpers.Option.bind (Data.to_node ptyext_attributes) ~f:(fun ptyext_attributes ->
                    Some { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes }
          )))))
      | _ -> None
  end

  module Extension_constructor = struct
    type t = Node.t

    type concrete =
      { pext_name : string Location.loc
      ; pext_kind : Node.t
      ; pext_loc : Location.t
      ; pext_attributes : Node.t
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

    let of_concrete ({ pext_name; pext_kind; pext_loc; pext_attributes } : concrete) =
      create ~pext_name ~pext_kind ~pext_loc ~pext_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pext_decl of Node.t * Node.t option
      | Pext_rebind of Node.t

    let create_pext_decl x1 x2 =
      node "extension_constructor_kind"
        (Variant
          { tag = "Pext_decl"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let create_pext_rebind x1 =
      node "extension_constructor_kind"
        (Variant
          { tag = "Pext_rebind"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Pext_decl (x1, x2) ->
        create_pext_decl x1 x2
      | Pext_rebind (x1) ->
        create_pext_rebind x1

    let to_concrete (t : t) : concrete option =
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
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pext_rebind (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Class_type = struct
    type t = Node.t

    type concrete =
      { pcty_desc : Node.t
      ; pcty_loc : Location.t
      ; pcty_attributes : Node.t
      }

    let create ~pcty_desc ~pcty_loc ~pcty_attributes =
      let fields =
        [| Data.of_node pcty_desc
         ; Data.of_location pcty_loc
         ; Data.of_node pcty_attributes
        |]
      in
      node "class_type" (Record fields)

    let of_concrete ({ pcty_desc; pcty_loc; pcty_attributes } : concrete) =
      create ~pcty_desc ~pcty_loc ~pcty_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pcty_constr of Node.t * Node.t list
      | Pcty_signature of Node.t
      | Pcty_arrow of Node.t * Node.t * Node.t
      | Pcty_extension of Node.t
      | Pcty_open of Node.t * Node.t * Node.t

    let create_pcty_constr x1 x2 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_constr"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_pcty_signature x1 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_signature"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pcty_arrow x1 x2 x3 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_arrow"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let create_pcty_extension x1 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pcty_open x1 x2 x3 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_open"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "class_type_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pcty_constr"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
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
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pcty_open (x1, x2, x3))
            )))
        | _ -> None
        end
      | _ -> None
  end

  module Class_signature = struct
    type t = Node.t

    type concrete =
      { pcsig_self : Node.t
      ; pcsig_fields : Node.t list
      }

    let create ~pcsig_self ~pcsig_fields =
      let fields =
        [| Data.of_node pcsig_self
         ; (Data.of_list ~f:Data.of_node) pcsig_fields
        |]
      in
      node "class_signature" (Record fields)

    let of_concrete ({ pcsig_self; pcsig_fields } : concrete) =
      create ~pcsig_self ~pcsig_fields

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pctf_desc : Node.t
      ; pctf_loc : Location.t
      ; pctf_attributes : Node.t
      }

    let create ~pctf_desc ~pctf_loc ~pctf_attributes =
      let fields =
        [| Data.of_node pctf_desc
         ; Data.of_location pctf_loc
         ; Data.of_node pctf_attributes
        |]
      in
      node "class_type_field" (Record fields)

    let of_concrete ({ pctf_desc; pctf_loc; pctf_attributes } : concrete) =
      create ~pctf_desc ~pctf_loc ~pctf_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pctf_inherit of Node.t
      | Pctf_val of (Node.t Location.loc * Node.t * Node.t * Node.t)
      | Pctf_method of (Node.t Location.loc * Node.t * Node.t * Node.t)
      | Pctf_constraint of (Node.t * Node.t)
      | Pctf_attribute of Node.t
      | Pctf_extension of Node.t

    let create_pctf_inherit x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_inherit"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pctf_val x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_val"
          ; args =
            [| (Data.of_tuple4 ~f1:(Data.of_loc ~f:Data.of_node) ~f2:Data.of_node ~f3:Data.of_node ~f4:Data.of_node) x1
            |]
          })
    let create_pctf_method x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_method"
          ; args =
            [| (Data.of_tuple4 ~f1:(Data.of_loc ~f:Data.of_node) ~f2:Data.of_node ~f3:Data.of_node ~f4:Data.of_node) x1
            |]
          })
    let create_pctf_constraint x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_constraint"
          ; args =
            [| (Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node) x1
            |]
          })
    let create_pctf_attribute x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pctf_extension x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_extension"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "class_type_field_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pctf_inherit"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pctf_inherit (x1))
            )
          | Variant { tag = "Pctf_val"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple4 ~f1:(Data.to_loc ~f:Data.to_node) ~f2:Data.to_node ~f3:Data.to_node ~f4:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pctf_val (x1))
            )
          | Variant { tag = "Pctf_method"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple4 ~f1:(Data.to_loc ~f:Data.to_node) ~f2:Data.to_node ~f3:Data.to_node ~f4:Data.to_node) x1) ~f:(fun x1 ->
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
    type 'a t = Node.t

    type 'a concrete =
      { pci_virt : Node.t
      ; pci_params : (Node.t * Node.t) list
      ; pci_name : string Location.loc
      ; pci_expr : 'a
      ; pci_loc : Location.t
      ; pci_attributes : Node.t
      }

    let create ast_of_a ~pci_virt ~pci_params ~pci_name ~pci_expr ~pci_loc ~pci_attributes =
      let fields =
        [| Data.of_node pci_virt
         ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) pci_params
         ; (Data.of_loc ~f:Data.of_string) pci_name
         ; ast_of_a pci_expr
         ; Data.of_location pci_loc
         ; Data.of_node pci_attributes
        |]
      in
      node "class_infos" (Record fields)

    let of_concrete ast_of_a ({ pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : _ concrete) =
      create ast_of_a ~pci_virt ~pci_params ~pci_name ~pci_expr ~pci_loc ~pci_attributes

    let to_concrete ast_to_a (t : _ t) : _ concrete option =
      match Node.to_node t ~version with
      | { name = "class_infos"
        ; data = Record [| pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node pci_virt) ~f:(fun pci_virt ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) pci_params) ~f:(fun pci_params ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pci_name) ~f:(fun pci_name ->
                Helpers.Option.bind (ast_to_a pci_expr) ~f:(fun pci_expr ->
                  Helpers.Option.bind (Data.to_location pci_loc) ~f:(fun pci_loc ->
                    Helpers.Option.bind (Data.to_node pci_attributes) ~f:(fun pci_attributes ->
                      Some { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
          ))))))
      | _ -> None

    let of_concrete_class_expr =
      of_concrete
        Data.of_node

    let to_concrete_class_expr =
      to_concrete
        Data.to_node

    let create_class_expr =
      create
        Data.of_node

    let of_concrete_class_type =
      of_concrete
        Data.of_node

    let to_concrete_class_type =
      to_concrete
        Data.to_node

    let create_class_type =
      create
        Data.of_node
  end

  module Class_description = struct
    type t = Node.t

    type concrete = Node.t Class_infos.t

    let create =
      let data = Data.of_node in
      fun x -> node "class_description" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "class_description"; data } -> Data.to_node data
      | _ -> None
  end

  module Class_type_declaration = struct
    type t = Node.t

    type concrete = Node.t Class_infos.t

    let create =
      let data = Data.of_node in
      fun x -> node "class_type_declaration" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "class_type_declaration"; data } -> Data.to_node data
      | _ -> None
  end

  module Class_expr = struct
    type t = Node.t

    type concrete =
      { pcl_desc : Node.t
      ; pcl_loc : Location.t
      ; pcl_attributes : Node.t
      }

    let create ~pcl_desc ~pcl_loc ~pcl_attributes =
      let fields =
        [| Data.of_node pcl_desc
         ; Data.of_location pcl_loc
         ; Data.of_node pcl_attributes
        |]
      in
      node "class_expr" (Record fields)

    let of_concrete ({ pcl_desc; pcl_loc; pcl_attributes } : concrete) =
      create ~pcl_desc ~pcl_loc ~pcl_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pcl_constr of Node.t * Node.t list
      | Pcl_structure of Node.t
      | Pcl_fun of Node.t * Node.t option * Node.t * Node.t
      | Pcl_apply of Node.t * (Node.t * Node.t) list
      | Pcl_let of Node.t * Node.t list * Node.t
      | Pcl_constraint of Node.t * Node.t
      | Pcl_extension of Node.t
      | Pcl_open of Node.t * Node.t * Node.t

    let create_pcl_constr x1 x2 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_constr"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_pcl_structure x1 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_structure"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pcl_fun x1 x2 x3 x4 =
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
    let create_pcl_apply x1 x2 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_apply"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) x2
            |]
          })
    let create_pcl_let x1 x2 x3 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_let"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let create_pcl_constraint x1 x2 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pcl_extension x1 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pcl_open x1 x2 x3 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_open"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "class_expr_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pcl_constr"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
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
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pcl_open (x1, x2, x3))
            )))
        | _ -> None
        end
      | _ -> None
  end

  module Class_structure = struct
    type t = Node.t

    type concrete =
      { pcstr_self : Node.t
      ; pcstr_fields : Node.t list
      }

    let create ~pcstr_self ~pcstr_fields =
      let fields =
        [| Data.of_node pcstr_self
         ; (Data.of_list ~f:Data.of_node) pcstr_fields
        |]
      in
      node "class_structure" (Record fields)

    let of_concrete ({ pcstr_self; pcstr_fields } : concrete) =
      create ~pcstr_self ~pcstr_fields

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pcf_desc : Node.t
      ; pcf_loc : Location.t
      ; pcf_attributes : Node.t
      }

    let create ~pcf_desc ~pcf_loc ~pcf_attributes =
      let fields =
        [| Data.of_node pcf_desc
         ; Data.of_location pcf_loc
         ; Data.of_node pcf_attributes
        |]
      in
      node "class_field" (Record fields)

    let of_concrete ({ pcf_desc; pcf_loc; pcf_attributes } : concrete) =
      create ~pcf_desc ~pcf_loc ~pcf_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pcf_inherit of Node.t * Node.t * string Location.loc option
      | Pcf_val of (Node.t Location.loc * Node.t * Node.t)
      | Pcf_method of (Node.t Location.loc * Node.t * Node.t)
      | Pcf_constraint of (Node.t * Node.t)
      | Pcf_initializer of Node.t
      | Pcf_attribute of Node.t
      | Pcf_extension of Node.t

    let create_pcf_inherit x1 x2 x3 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_inherit"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; (Data.of_option ~f:(Data.of_loc ~f:Data.of_string)) x3
            |]
          })
    let create_pcf_val x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_val"
          ; args =
            [| (Data.of_tuple3 ~f1:(Data.of_loc ~f:Data.of_node) ~f2:Data.of_node ~f3:Data.of_node) x1
            |]
          })
    let create_pcf_method x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_method"
          ; args =
            [| (Data.of_tuple3 ~f1:(Data.of_loc ~f:Data.of_node) ~f2:Data.of_node ~f3:Data.of_node) x1
            |]
          })
    let create_pcf_constraint x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_constraint"
          ; args =
            [| (Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node) x1
            |]
          })
    let create_pcf_initializer x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_initializer"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pcf_attribute x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pcf_extension x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_extension"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
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
            Helpers.Option.bind ((Data.to_tuple3 ~f1:(Data.to_loc ~f:Data.to_node) ~f2:Data.to_node ~f3:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pcf_val (x1))
            )
          | Variant { tag = "Pcf_method"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple3 ~f1:(Data.to_loc ~f:Data.to_node) ~f2:Data.to_node ~f3:Data.to_node) x1) ~f:(fun x1 ->
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
    type t = Node.t

    type concrete =
      | Cfk_virtual of Node.t
      | Cfk_concrete of Node.t * Node.t

    let create_cfk_virtual x1 =
      node "class_field_kind"
        (Variant
          { tag = "Cfk_virtual"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_cfk_concrete x1 x2 =
      node "class_field_kind"
        (Variant
          { tag = "Cfk_concrete"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Cfk_virtual (x1) ->
        create_cfk_virtual x1
      | Cfk_concrete (x1, x2) ->
        create_cfk_concrete x1 x2

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete = Node.t Class_infos.t

    let create =
      let data = Data.of_node in
      fun x -> node "class_declaration" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "class_declaration"; data } -> Data.to_node data
      | _ -> None
  end

  module Module_type = struct
    type t = Node.t

    type concrete =
      { pmty_desc : Node.t
      ; pmty_loc : Location.t
      ; pmty_attributes : Node.t
      }

    let create ~pmty_desc ~pmty_loc ~pmty_attributes =
      let fields =
        [| Data.of_node pmty_desc
         ; Data.of_location pmty_loc
         ; Data.of_node pmty_attributes
        |]
      in
      node "module_type" (Record fields)

    let of_concrete ({ pmty_desc; pmty_loc; pmty_attributes } : concrete) =
      create ~pmty_desc ~pmty_loc ~pmty_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pmty_ident of Node.t
      | Pmty_signature of Node.t
      | Pmty_functor of string Location.loc * Node.t option * Node.t
      | Pmty_with of Node.t * Node.t list
      | Pmty_typeof of Node.t
      | Pmty_extension of Node.t
      | Pmty_alias of Node.t

    let create_pmty_ident x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_ident"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pmty_signature x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_signature"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pmty_functor x1 x2 x3 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_functor"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; (Data.of_option ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let create_pmty_with x1 x2 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_with"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_pmty_typeof x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_typeof"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pmty_extension x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pmty_alias x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_alias"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "module_type_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pmty_ident"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
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
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pmty_alias (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Signature = struct
    type t = Node.t

    type concrete = Node.t list

    let create =
      let data = (Data.of_list ~f:Data.of_node) in
      fun x -> node "signature" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "signature"; data } -> (Data.to_list ~f:Data.to_node) data
      | _ -> None
  end

  module Signature_item = struct
    type t = Node.t

    type concrete =
      { psig_desc : Node.t
      ; psig_loc : Location.t
      }

    let create ~psig_desc ~psig_loc =
      let fields =
        [| Data.of_node psig_desc
         ; Data.of_location psig_loc
        |]
      in
      node "signature_item" (Record fields)

    let of_concrete ({ psig_desc; psig_loc } : concrete) =
      create ~psig_desc ~psig_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Psig_value of Node.t
      | Psig_type of Node.t * Node.t list
      | Psig_typext of Node.t
      | Psig_exception of Node.t
      | Psig_module of Node.t
      | Psig_recmodule of Node.t list
      | Psig_modtype of Node.t
      | Psig_open of Node.t
      | Psig_include of Node.t
      | Psig_class of Node.t list
      | Psig_class_type of Node.t list
      | Psig_attribute of Node.t
      | Psig_extension of Node.t * Node.t

    let create_psig_value x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_value"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_type x1 x2 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_type"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_psig_typext x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_typext"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_exception x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_exception"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_module x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_module"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_recmodule x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_recmodule"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_psig_modtype x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_modtype"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_open x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_open"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_include x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_include"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_class x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_class"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_psig_class_type x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_class_type"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_psig_attribute x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_extension x1 x2 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_extension"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pmd_name : string Location.loc
      ; pmd_type : Node.t
      ; pmd_attributes : Node.t
      ; pmd_loc : Location.t
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

    let of_concrete ({ pmd_name; pmd_type; pmd_attributes; pmd_loc } : concrete) =
      create ~pmd_name ~pmd_type ~pmd_attributes ~pmd_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pmtd_name : string Location.loc
      ; pmtd_type : Node.t option
      ; pmtd_attributes : Node.t
      ; pmtd_loc : Location.t
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

    let of_concrete ({ pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : concrete) =
      create ~pmtd_name ~pmtd_type ~pmtd_attributes ~pmtd_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { popen_lid : Node.t
      ; popen_override : Node.t
      ; popen_loc : Location.t
      ; popen_attributes : Node.t
      }

    let create ~popen_lid ~popen_override ~popen_loc ~popen_attributes =
      let fields =
        [| Data.of_node popen_lid
         ; Data.of_node popen_override
         ; Data.of_location popen_loc
         ; Data.of_node popen_attributes
        |]
      in
      node "open_description" (Record fields)

    let of_concrete ({ popen_lid; popen_override; popen_loc; popen_attributes } : concrete) =
      create ~popen_lid ~popen_override ~popen_loc ~popen_attributes

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "open_description"
        ; data = Record [| popen_lid; popen_override; popen_loc; popen_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node popen_lid) ~f:(fun popen_lid ->
            Helpers.Option.bind (Data.to_node popen_override) ~f:(fun popen_override ->
              Helpers.Option.bind (Data.to_location popen_loc) ~f:(fun popen_loc ->
                Helpers.Option.bind (Data.to_node popen_attributes) ~f:(fun popen_attributes ->
                  Some { popen_lid; popen_override; popen_loc; popen_attributes }
          ))))
      | _ -> None
  end

  module Include_infos = struct
    type 'a t = Node.t

    type 'a concrete =
      { pincl_mod : 'a
      ; pincl_loc : Location.t
      ; pincl_attributes : Node.t
      }

    let create ast_of_a ~pincl_mod ~pincl_loc ~pincl_attributes =
      let fields =
        [| ast_of_a pincl_mod
         ; Data.of_location pincl_loc
         ; Data.of_node pincl_attributes
        |]
      in
      node "include_infos" (Record fields)

    let of_concrete ast_of_a ({ pincl_mod; pincl_loc; pincl_attributes } : _ concrete) =
      create ast_of_a ~pincl_mod ~pincl_loc ~pincl_attributes

    let to_concrete ast_to_a (t : _ t) : _ concrete option =
      match Node.to_node t ~version with
      | { name = "include_infos"
        ; data = Record [| pincl_mod; pincl_loc; pincl_attributes |]
        } ->
          Helpers.Option.bind (ast_to_a pincl_mod) ~f:(fun pincl_mod ->
            Helpers.Option.bind (Data.to_location pincl_loc) ~f:(fun pincl_loc ->
              Helpers.Option.bind (Data.to_node pincl_attributes) ~f:(fun pincl_attributes ->
                Some { pincl_mod; pincl_loc; pincl_attributes }
          )))
      | _ -> None

    let of_concrete_module_expr =
      of_concrete
        Data.of_node

    let to_concrete_module_expr =
      to_concrete
        Data.to_node

    let create_module_expr =
      create
        Data.of_node

    let of_concrete_module_type =
      of_concrete
        Data.of_node

    let to_concrete_module_type =
      to_concrete
        Data.to_node

    let create_module_type =
      create
        Data.of_node
  end

  module Include_description = struct
    type t = Node.t

    type concrete = Node.t Include_infos.t

    let create =
      let data = Data.of_node in
      fun x -> node "include_description" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "include_description"; data } -> Data.to_node data
      | _ -> None
  end

  module Include_declaration = struct
    type t = Node.t

    type concrete = Node.t Include_infos.t

    let create =
      let data = Data.of_node in
      fun x -> node "include_declaration" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "include_declaration"; data } -> Data.to_node data
      | _ -> None
  end

  module With_constraint = struct
    type t = Node.t

    type concrete =
      | Pwith_type of Node.t * Node.t
      | Pwith_module of Node.t * Node.t
      | Pwith_typesubst of Node.t * Node.t
      | Pwith_modsubst of Node.t * Node.t

    let create_pwith_type x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_type"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pwith_module x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_module"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pwith_typesubst x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_typesubst"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pwith_modsubst x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_modsubst"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Pwith_type (x1, x2) ->
        create_pwith_type x1 x2
      | Pwith_module (x1, x2) ->
        create_pwith_module x1 x2
      | Pwith_typesubst (x1, x2) ->
        create_pwith_typesubst x1 x2
      | Pwith_modsubst (x1, x2) ->
        create_pwith_modsubst x1 x2

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "with_constraint"; data } ->
        begin
          match data with
          | Variant { tag = "Pwith_type"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pwith_type (x1, x2))
            ))
          | Variant { tag = "Pwith_module"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pwith_module (x1, x2))
            ))
          | Variant { tag = "Pwith_typesubst"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pwith_typesubst (x1, x2))
            ))
          | Variant { tag = "Pwith_modsubst"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pwith_modsubst (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Module_expr = struct
    type t = Node.t

    type concrete =
      { pmod_desc : Node.t
      ; pmod_loc : Location.t
      ; pmod_attributes : Node.t
      }

    let create ~pmod_desc ~pmod_loc ~pmod_attributes =
      let fields =
        [| Data.of_node pmod_desc
         ; Data.of_location pmod_loc
         ; Data.of_node pmod_attributes
        |]
      in
      node "module_expr" (Record fields)

    let of_concrete ({ pmod_desc; pmod_loc; pmod_attributes } : concrete) =
      create ~pmod_desc ~pmod_loc ~pmod_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pmod_ident of Node.t
      | Pmod_structure of Node.t
      | Pmod_functor of string Location.loc * Node.t option * Node.t
      | Pmod_apply of Node.t * Node.t
      | Pmod_constraint of Node.t * Node.t
      | Pmod_unpack of Node.t
      | Pmod_extension of Node.t

    let create_pmod_ident x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_ident"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pmod_structure x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_structure"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pmod_functor x1 x2 x3 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_functor"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; (Data.of_option ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let create_pmod_apply x1 x2 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_apply"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pmod_constraint x1 x2 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pmod_unpack x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_unpack"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pmod_extension x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_extension"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "module_expr_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pmod_ident"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
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
    type t = Node.t

    type concrete = Node.t list

    let create =
      let data = (Data.of_list ~f:Data.of_node) in
      fun x -> node "structure" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "structure"; data } -> (Data.to_list ~f:Data.to_node) data
      | _ -> None
  end

  module Structure_item = struct
    type t = Node.t

    type concrete =
      { pstr_desc : Node.t
      ; pstr_loc : Location.t
      }

    let create ~pstr_desc ~pstr_loc =
      let fields =
        [| Data.of_node pstr_desc
         ; Data.of_location pstr_loc
        |]
      in
      node "structure_item" (Record fields)

    let of_concrete ({ pstr_desc; pstr_loc } : concrete) =
      create ~pstr_desc ~pstr_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pstr_eval of Node.t * Node.t
      | Pstr_value of Node.t * Node.t list
      | Pstr_primitive of Node.t
      | Pstr_type of Node.t * Node.t list
      | Pstr_typext of Node.t
      | Pstr_exception of Node.t
      | Pstr_module of Node.t
      | Pstr_recmodule of Node.t list
      | Pstr_modtype of Node.t
      | Pstr_open of Node.t
      | Pstr_class of Node.t list
      | Pstr_class_type of Node.t list
      | Pstr_include of Node.t
      | Pstr_attribute of Node.t
      | Pstr_extension of Node.t * Node.t

    let create_pstr_eval x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_eval"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pstr_value x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_value"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_pstr_primitive x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_primitive"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_type x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_type"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_pstr_typext x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_typext"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_exception x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_exception"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_module x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_module"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_recmodule x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_recmodule"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_pstr_modtype x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_modtype"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_open x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_open"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_class x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_class"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_pstr_class_type x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_class_type"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_pstr_include x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_include"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_attribute x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_extension x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_extension"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pvb_pat : Node.t
      ; pvb_expr : Node.t
      ; pvb_attributes : Node.t
      ; pvb_loc : Location.t
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

    let of_concrete ({ pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : concrete) =
      create ~pvb_pat ~pvb_expr ~pvb_attributes ~pvb_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pmb_name : string Location.loc
      ; pmb_expr : Node.t
      ; pmb_attributes : Node.t
      ; pmb_loc : Location.t
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

    let of_concrete ({ pmb_name; pmb_expr; pmb_attributes; pmb_loc } : concrete) =
      create ~pmb_name ~pmb_expr ~pmb_attributes ~pmb_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Ptop_def of Node.t
      | Ptop_dir of string * Node.t

    let create_ptop_def x1 =
      node "toplevel_phrase"
        (Variant
          { tag = "Ptop_def"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ptop_dir x1 x2 =
      node "toplevel_phrase"
        (Variant
          { tag = "Ptop_dir"
          ; args =
            [| Data.of_string x1
             ; Data.of_node x2
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Ptop_def (x1) ->
        create_ptop_def x1
      | Ptop_dir (x1, x2) ->
        create_ptop_dir x1 x2

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pdir_none
      | Pdir_string of string
      | Pdir_int of string * char option
      | Pdir_ident of Node.t
      | Pdir_bool of bool

    let create_pdir_none =
      node "directive_argument" (Variant { tag = "Pdir_none"; args = [||] })
    let create_pdir_string x1 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_string"
          ; args =
            [| Data.of_string x1
            |]
          })
    let create_pdir_int x1 x2 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_int"
          ; args =
            [| Data.of_string x1
             ; (Data.of_option ~f:Data.of_char) x2
            |]
          })
    let create_pdir_ident x1 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_ident"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pdir_bool x1 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_bool"
          ; args =
            [| Data.of_bool x1
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
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

module V4_06 = struct
  let version = "v4.06"
  let node name data = Node.of_node ~version { name; data }

  module Longident = struct
    type t = Node.t

    type concrete =
      | Lident of string
      | Ldot of Node.t * string
      | Lapply of Node.t * Node.t

    let create_lident x1 =
      node "longident"
        (Variant
          { tag = "Lident"
          ; args =
            [| Data.of_string x1
            |]
          })
    let create_ldot x1 x2 =
      node "longident"
        (Variant
          { tag = "Ldot"
          ; args =
            [| Data.of_node x1
             ; Data.of_string x2
            |]
          })
    let create_lapply x1 x2 =
      node "longident"
        (Variant
          { tag = "Lapply"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Lident (x1) ->
        create_lident x1
      | Ldot (x1, x2) ->
        create_ldot x1 x2
      | Lapply (x1, x2) ->
        create_lapply x1 x2

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete = Node.t Location.loc

    let create =
      let data = (Data.of_loc ~f:Data.of_node) in
      fun x -> node "longident_loc" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "longident_loc"; data } -> (Data.to_loc ~f:Data.to_node) data
      | _ -> None
  end

  module Rec_flag = struct
    type t = Node.t

    type concrete =
      | Nonrecursive
      | Recursive

    let create_nonrecursive =
      node "rec_flag" (Variant { tag = "Nonrecursive"; args = [||] })
    let create_recursive =
      node "rec_flag" (Variant { tag = "Recursive"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Nonrecursive -> create_nonrecursive
      | Recursive -> create_recursive

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Upto
      | Downto

    let create_upto =
      node "direction_flag" (Variant { tag = "Upto"; args = [||] })
    let create_downto =
      node "direction_flag" (Variant { tag = "Downto"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Upto -> create_upto
      | Downto -> create_downto

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Private
      | Public

    let create_private =
      node "private_flag" (Variant { tag = "Private"; args = [||] })
    let create_public =
      node "private_flag" (Variant { tag = "Public"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Private -> create_private
      | Public -> create_public

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Immutable
      | Mutable

    let create_immutable =
      node "mutable_flag" (Variant { tag = "Immutable"; args = [||] })
    let create_mutable =
      node "mutable_flag" (Variant { tag = "Mutable"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Immutable -> create_immutable
      | Mutable -> create_mutable

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Virtual
      | Concrete

    let create_virtual =
      node "virtual_flag" (Variant { tag = "Virtual"; args = [||] })
    let create_concrete =
      node "virtual_flag" (Variant { tag = "Concrete"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Virtual -> create_virtual
      | Concrete -> create_concrete

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Override
      | Fresh

    let create_override =
      node "override_flag" (Variant { tag = "Override"; args = [||] })
    let create_fresh =
      node "override_flag" (Variant { tag = "Fresh"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Override -> create_override
      | Fresh -> create_fresh

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Closed
      | Open

    let create_closed =
      node "closed_flag" (Variant { tag = "Closed"; args = [||] })
    let create_open =
      node "closed_flag" (Variant { tag = "Open"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Closed -> create_closed
      | Open -> create_open

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete = string

    let create =
      let data = Data.of_string in
      fun x -> node "label" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "label"; data } -> Data.to_string data
      | _ -> None
  end

  module Arg_label = struct
    type t = Node.t

    type concrete =
      | Nolabel
      | Labelled of string
      | Optional of string

    let create_nolabel =
      node "arg_label" (Variant { tag = "Nolabel"; args = [||] })
    let create_labelled x1 =
      node "arg_label"
        (Variant
          { tag = "Labelled"
          ; args =
            [| Data.of_string x1
            |]
          })
    let create_optional x1 =
      node "arg_label"
        (Variant
          { tag = "Optional"
          ; args =
            [| Data.of_string x1
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Nolabel -> create_nolabel
      | Labelled (x1) ->
        create_labelled x1
      | Optional (x1) ->
        create_optional x1

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Covariant
      | Contravariant
      | Invariant

    let create_covariant =
      node "variance" (Variant { tag = "Covariant"; args = [||] })
    let create_contravariant =
      node "variance" (Variant { tag = "Contravariant"; args = [||] })
    let create_invariant =
      node "variance" (Variant { tag = "Invariant"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Covariant -> create_covariant
      | Contravariant -> create_contravariant
      | Invariant -> create_invariant

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pconst_integer of string * char option
      | Pconst_char of char
      | Pconst_string of string * string option
      | Pconst_float of string * char option

    let create_pconst_integer x1 x2 =
      node "constant"
        (Variant
          { tag = "Pconst_integer"
          ; args =
            [| Data.of_string x1
             ; (Data.of_option ~f:Data.of_char) x2
            |]
          })
    let create_pconst_char x1 =
      node "constant"
        (Variant
          { tag = "Pconst_char"
          ; args =
            [| Data.of_char x1
            |]
          })
    let create_pconst_string x1 x2 =
      node "constant"
        (Variant
          { tag = "Pconst_string"
          ; args =
            [| Data.of_string x1
             ; (Data.of_option ~f:Data.of_string) x2
            |]
          })
    let create_pconst_float x1 x2 =
      node "constant"
        (Variant
          { tag = "Pconst_float"
          ; args =
            [| Data.of_string x1
             ; (Data.of_option ~f:Data.of_char) x2
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Pconst_integer (x1, x2) ->
        create_pconst_integer x1 x2
      | Pconst_char (x1) ->
        create_pconst_char x1
      | Pconst_string (x1, x2) ->
        create_pconst_string x1 x2
      | Pconst_float (x1, x2) ->
        create_pconst_float x1 x2

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete = (string Location.loc * Node.t)

    let create =
      let data = (Data.of_tuple2 ~f1:(Data.of_loc ~f:Data.of_string) ~f2:Data.of_node) in
      fun x -> node "attribute" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "attribute"; data } -> (Data.to_tuple2 ~f1:(Data.to_loc ~f:Data.to_string) ~f2:Data.to_node) data
      | _ -> None
  end

  module Extension = struct
    type t = Node.t

    type concrete = (string Location.loc * Node.t)

    let create =
      let data = (Data.of_tuple2 ~f1:(Data.of_loc ~f:Data.of_string) ~f2:Data.of_node) in
      fun x -> node "extension" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "extension"; data } -> (Data.to_tuple2 ~f1:(Data.to_loc ~f:Data.to_string) ~f2:Data.to_node) data
      | _ -> None
  end

  module Attributes = struct
    type t = Node.t

    type concrete = Node.t list

    let create =
      let data = (Data.of_list ~f:Data.of_node) in
      fun x -> node "attributes" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "attributes"; data } -> (Data.to_list ~f:Data.to_node) data
      | _ -> None
  end

  module Payload = struct
    type t = Node.t

    type concrete =
      | PStr of Node.t
      | PSig of Node.t
      | PTyp of Node.t
      | PPat of Node.t * Node.t option

    let create_pstr x1 =
      node "payload"
        (Variant
          { tag = "PStr"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig x1 =
      node "payload"
        (Variant
          { tag = "PSig"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ptyp x1 =
      node "payload"
        (Variant
          { tag = "PTyp"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ppat x1 x2 =
      node "payload"
        (Variant
          { tag = "PPat"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | PStr (x1) ->
        create_pstr x1
      | PSig (x1) ->
        create_psig x1
      | PTyp (x1) ->
        create_ptyp x1
      | PPat (x1, x2) ->
        create_ppat x1 x2

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { ptyp_desc : Node.t
      ; ptyp_loc : Location.t
      ; ptyp_attributes : Node.t
      }

    let create ~ptyp_desc ~ptyp_loc ~ptyp_attributes =
      let fields =
        [| Data.of_node ptyp_desc
         ; Data.of_location ptyp_loc
         ; Data.of_node ptyp_attributes
        |]
      in
      node "core_type" (Record fields)

    let of_concrete ({ ptyp_desc; ptyp_loc; ptyp_attributes } : concrete) =
      create ~ptyp_desc ~ptyp_loc ~ptyp_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of Node.t * Node.t * Node.t
      | Ptyp_tuple of Node.t list
      | Ptyp_constr of Node.t * Node.t list
      | Ptyp_object of Node.t list * Node.t
      | Ptyp_class of Node.t * Node.t list
      | Ptyp_alias of Node.t * string
      | Ptyp_variant of Node.t list * Node.t * Node.t list option
      | Ptyp_poly of string Location.loc list * Node.t
      | Ptyp_package of Node.t
      | Ptyp_extension of Node.t

    let create_ptyp_any =
      node "core_type_desc" (Variant { tag = "Ptyp_any"; args = [||] })
    let create_ptyp_var x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_var"
          ; args =
            [| Data.of_string x1
            |]
          })
    let create_ptyp_arrow x1 x2 x3 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_arrow"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let create_ptyp_tuple x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_ptyp_constr x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_constr"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_ptyp_object x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_object"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let create_ptyp_class x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_class"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_ptyp_alias x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_alias"
          ; args =
            [| Data.of_node x1
             ; Data.of_string x2
            |]
          })
    let create_ptyp_variant x1 x2 x3 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_variant"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
             ; Data.of_node x2
             ; (Data.of_option ~f:(Data.of_list ~f:Data.of_node)) x3
            |]
          })
    let create_ptyp_poly x1 x2 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_poly"
          ; args =
            [| (Data.of_list ~f:(Data.of_loc ~f:Data.of_string)) x1
             ; Data.of_node x2
            |]
          })
    let create_ptyp_package x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_package"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ptyp_extension x1 =
      node "core_type_desc"
        (Variant
          { tag = "Ptyp_extension"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
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
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Ptyp_constr (x1, x2))
            ))
          | Variant { tag = "Ptyp_object"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ptyp_object (x1, x2))
            ))
          | Variant { tag = "Ptyp_class"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
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
                Helpers.Option.bind ((Data.to_option ~f:(Data.to_list ~f:Data.to_node)) x3) ~f:(fun x3 ->
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
    type t = Node.t

    type concrete = (Node.t * (Node.t * Node.t) list)

    let create =
      let data = (Data.of_tuple2 ~f1:Data.of_node ~f2:(Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node))) in
      fun x -> node "package_type" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "package_type"; data } -> (Data.to_tuple2 ~f1:Data.to_node ~f2:(Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node))) data
      | _ -> None
  end

  module Row_field = struct
    type t = Node.t

    type concrete =
      | Rtag of Node.t Location.loc * Node.t * bool * Node.t list
      | Rinherit of Node.t

    let create_rtag x1 x2 x3 x4 =
      node "row_field"
        (Variant
          { tag = "Rtag"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; Data.of_node x2
             ; Data.of_bool x3
             ; (Data.of_list ~f:Data.of_node) x4
            |]
          })
    let create_rinherit x1 =
      node "row_field"
        (Variant
          { tag = "Rinherit"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Rtag (x1, x2, x3, x4) ->
        create_rtag x1 x2 x3 x4
      | Rinherit (x1) ->
        create_rinherit x1

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "row_field"; data } ->
        begin
          match data with
          | Variant { tag = "Rtag"; args = [| x1; x2; x3; x4 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
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
    type t = Node.t

    type concrete =
      | Otag of Node.t Location.loc * Node.t * Node.t
      | Oinherit of Node.t

    let create_otag x1 x2 x3 =
      node "object_field"
        (Variant
          { tag = "Otag"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let create_oinherit x1 =
      node "object_field"
        (Variant
          { tag = "Oinherit"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Otag (x1, x2, x3) ->
        create_otag x1 x2 x3
      | Oinherit (x1) ->
        create_oinherit x1

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "object_field"; data } ->
        begin
          match data with
          | Variant { tag = "Otag"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
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
    type t = Node.t

    type concrete =
      { ppat_desc : Node.t
      ; ppat_loc : Location.t
      ; ppat_attributes : Node.t
      }

    let create ~ppat_desc ~ppat_loc ~ppat_attributes =
      let fields =
        [| Data.of_node ppat_desc
         ; Data.of_location ppat_loc
         ; Data.of_node ppat_attributes
        |]
      in
      node "pattern" (Record fields)

    let of_concrete ({ ppat_desc; ppat_loc; ppat_attributes } : concrete) =
      create ~ppat_desc ~ppat_loc ~ppat_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Ppat_any
      | Ppat_var of string Location.loc
      | Ppat_alias of Node.t * string Location.loc
      | Ppat_constant of Node.t
      | Ppat_interval of Node.t * Node.t
      | Ppat_tuple of Node.t list
      | Ppat_construct of Node.t * Node.t option
      | Ppat_variant of Node.t * Node.t option
      | Ppat_record of (Node.t * Node.t) list * Node.t
      | Ppat_array of Node.t list
      | Ppat_or of Node.t * Node.t
      | Ppat_constraint of Node.t * Node.t
      | Ppat_type of Node.t
      | Ppat_lazy of Node.t
      | Ppat_unpack of string Location.loc
      | Ppat_exception of Node.t
      | Ppat_extension of Node.t
      | Ppat_open of Node.t * Node.t

    let create_ppat_any =
      node "pattern_desc" (Variant { tag = "Ppat_any"; args = [||] })
    let create_ppat_var x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_var"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
            |]
          })
    let create_ppat_alias x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_alias"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_string) x2
            |]
          })
    let create_ppat_constant x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_constant"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ppat_interval x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_interval"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_ppat_tuple x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_ppat_construct x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_construct"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let create_ppat_variant x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_variant"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let create_ppat_record x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_record"
          ; args =
            [| (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) x1
             ; Data.of_node x2
            |]
          })
    let create_ppat_array x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_array"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_ppat_or x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_or"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_ppat_constraint x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_ppat_type x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_type"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ppat_lazy x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_lazy"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ppat_unpack x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_unpack"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
            |]
          })
    let create_ppat_exception x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_exception"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ppat_extension x1 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ppat_open x1 x2 =
      node "pattern_desc"
        (Variant
          { tag = "Ppat_open"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
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
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Ppat_construct (x1, x2))
            ))
          | Variant { tag = "Ppat_variant"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Ppat_variant (x1, x2))
            ))
          | Variant { tag = "Ppat_record"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) x1) ~f:(fun x1 ->
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
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
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
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Ppat_open (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Expression = struct
    type t = Node.t

    type concrete =
      { pexp_desc : Node.t
      ; pexp_loc : Location.t
      ; pexp_attributes : Node.t
      }

    let create ~pexp_desc ~pexp_loc ~pexp_attributes =
      let fields =
        [| Data.of_node pexp_desc
         ; Data.of_location pexp_loc
         ; Data.of_node pexp_attributes
        |]
      in
      node "expression" (Record fields)

    let of_concrete ({ pexp_desc; pexp_loc; pexp_attributes } : concrete) =
      create ~pexp_desc ~pexp_loc ~pexp_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pexp_ident of Node.t
      | Pexp_constant of Node.t
      | Pexp_let of Node.t * Node.t list * Node.t
      | Pexp_function of Node.t list
      | Pexp_fun of Node.t * Node.t option * Node.t * Node.t
      | Pexp_apply of Node.t * (Node.t * Node.t) list
      | Pexp_match of Node.t * Node.t list
      | Pexp_try of Node.t * Node.t list
      | Pexp_tuple of Node.t list
      | Pexp_construct of Node.t * Node.t option
      | Pexp_variant of Node.t * Node.t option
      | Pexp_record of (Node.t * Node.t) list * Node.t option
      | Pexp_field of Node.t * Node.t
      | Pexp_setfield of Node.t * Node.t * Node.t
      | Pexp_array of Node.t list
      | Pexp_ifthenelse of Node.t * Node.t * Node.t option
      | Pexp_sequence of Node.t * Node.t
      | Pexp_while of Node.t * Node.t
      | Pexp_for of Node.t * Node.t * Node.t * Node.t * Node.t
      | Pexp_constraint of Node.t * Node.t
      | Pexp_coerce of Node.t * Node.t option * Node.t
      | Pexp_send of Node.t * Node.t Location.loc
      | Pexp_new of Node.t
      | Pexp_setinstvar of Node.t Location.loc * Node.t
      | Pexp_override of (Node.t Location.loc * Node.t) list
      | Pexp_letmodule of string Location.loc * Node.t * Node.t
      | Pexp_letexception of Node.t * Node.t
      | Pexp_assert of Node.t
      | Pexp_lazy of Node.t
      | Pexp_poly of Node.t * Node.t option
      | Pexp_object of Node.t
      | Pexp_newtype of string Location.loc * Node.t
      | Pexp_pack of Node.t
      | Pexp_open of Node.t * Node.t * Node.t
      | Pexp_extension of Node.t
      | Pexp_unreachable

    let create_pexp_ident x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_ident"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_constant x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_constant"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_let x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_let"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let create_pexp_function x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_function"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_pexp_fun x1 x2 x3 x4 =
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
    let create_pexp_apply x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_apply"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) x2
            |]
          })
    let create_pexp_match x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_match"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_pexp_try x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_try"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_pexp_tuple x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_pexp_construct x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_construct"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let create_pexp_variant x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_variant"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let create_pexp_record x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_record"
          ; args =
            [| (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let create_pexp_field x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_field"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pexp_setfield x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_setfield"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let create_pexp_array x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_array"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_pexp_ifthenelse x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_ifthenelse"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; (Data.of_option ~f:Data.of_node) x3
            |]
          })
    let create_pexp_sequence x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_sequence"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pexp_while x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_while"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pexp_for x1 x2 x3 x4 x5 =
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
    let create_pexp_constraint x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pexp_coerce x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_coerce"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let create_pexp_send x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_send"
          ; args =
            [| Data.of_node x1
             ; (Data.of_loc ~f:Data.of_node) x2
            |]
          })
    let create_pexp_new x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_new"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_setinstvar x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_setinstvar"
          ; args =
            [| (Data.of_loc ~f:Data.of_node) x1
             ; Data.of_node x2
            |]
          })
    let create_pexp_override x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_override"
          ; args =
            [| (Data.of_list ~f:(Data.of_tuple2 ~f1:(Data.of_loc ~f:Data.of_node) ~f2:Data.of_node)) x1
            |]
          })
    let create_pexp_letmodule x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_letmodule"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let create_pexp_letexception x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_letexception"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pexp_assert x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_assert"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_lazy x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_lazy"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_poly x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_poly"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let create_pexp_object x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_object"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_newtype x1 x2 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_newtype"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; Data.of_node x2
            |]
          })
    let create_pexp_pack x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_pack"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_open x1 x2 x3 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_open"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let create_pexp_extension x1 =
      node "expression_desc"
        (Variant
          { tag = "Pexp_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pexp_unreachable =
      node "expression_desc" (Variant { tag = "Pexp_unreachable"; args = [||] })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "expression_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pexp_ident"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
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
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_construct (x1, x2))
            ))
          | Variant { tag = "Pexp_variant"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_variant (x1, x2))
            ))
          | Variant { tag = "Pexp_record"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) x1) ~f:(fun x1 ->
              Helpers.Option.bind ((Data.to_option ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_record (x1, x2))
            ))
          | Variant { tag = "Pexp_field"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_field (x1, x2))
            ))
          | Variant { tag = "Pexp_setfield"; args = [| x1; x2; x3 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
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
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x2) ~f:(fun x2 ->
                Some (Pexp_send (x1, x2))
            ))
          | Variant { tag = "Pexp_new"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pexp_new (x1))
            )
          | Variant { tag = "Pexp_setinstvar"; args = [| x1; x2 |] } ->
            Helpers.Option.bind ((Data.to_loc ~f:Data.to_node) x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pexp_setinstvar (x1, x2))
            ))
          | Variant { tag = "Pexp_override"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:(Data.to_loc ~f:Data.to_node) ~f2:Data.to_node)) x1) ~f:(fun x1 ->
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
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
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
    type t = Node.t

    type concrete =
      { pc_lhs : Node.t
      ; pc_guard : Node.t option
      ; pc_rhs : Node.t
      }

    let create ~pc_lhs ~pc_guard ~pc_rhs =
      let fields =
        [| Data.of_node pc_lhs
         ; (Data.of_option ~f:Data.of_node) pc_guard
         ; Data.of_node pc_rhs
        |]
      in
      node "case" (Record fields)

    let of_concrete ({ pc_lhs; pc_guard; pc_rhs } : concrete) =
      create ~pc_lhs ~pc_guard ~pc_rhs

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pval_name : string Location.loc
      ; pval_type : Node.t
      ; pval_prim : string list
      ; pval_attributes : Node.t
      ; pval_loc : Location.t
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

    let of_concrete ({ pval_name; pval_type; pval_prim; pval_attributes; pval_loc } : concrete) =
      create ~pval_name ~pval_type ~pval_prim ~pval_attributes ~pval_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { ptype_name : string Location.loc
      ; ptype_params : (Node.t * Node.t) list
      ; ptype_cstrs : (Node.t * Node.t * Location.t) list
      ; ptype_kind : Node.t
      ; ptype_private : Node.t
      ; ptype_manifest : Node.t option
      ; ptype_attributes : Node.t
      ; ptype_loc : Location.t
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

    let of_concrete ({ ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } : concrete) =
      create ~ptype_name ~ptype_params ~ptype_cstrs ~ptype_kind ~ptype_private ~ptype_manifest ~ptype_attributes ~ptype_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Ptype_abstract
      | Ptype_variant of Node.t list
      | Ptype_record of Node.t list
      | Ptype_open

    let create_ptype_abstract =
      node "type_kind" (Variant { tag = "Ptype_abstract"; args = [||] })
    let create_ptype_variant x1 =
      node "type_kind"
        (Variant
          { tag = "Ptype_variant"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_ptype_record x1 =
      node "type_kind"
        (Variant
          { tag = "Ptype_record"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_ptype_open =
      node "type_kind" (Variant { tag = "Ptype_open"; args = [||] })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Ptype_abstract -> create_ptype_abstract
      | Ptype_variant (x1) ->
        create_ptype_variant x1
      | Ptype_record (x1) ->
        create_ptype_record x1
      | Ptype_open -> create_ptype_open

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pld_name : string Location.loc
      ; pld_mutable : Node.t
      ; pld_type : Node.t
      ; pld_loc : Location.t
      ; pld_attributes : Node.t
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

    let of_concrete ({ pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } : concrete) =
      create ~pld_name ~pld_mutable ~pld_type ~pld_loc ~pld_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pcd_name : string Location.loc
      ; pcd_args : Node.t
      ; pcd_res : Node.t option
      ; pcd_loc : Location.t
      ; pcd_attributes : Node.t
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

    let of_concrete ({ pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } : concrete) =
      create ~pcd_name ~pcd_args ~pcd_res ~pcd_loc ~pcd_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pcstr_tuple of Node.t list
      | Pcstr_record of Node.t list

    let create_pcstr_tuple x1 =
      node "constructor_arguments"
        (Variant
          { tag = "Pcstr_tuple"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_pcstr_record x1 =
      node "constructor_arguments"
        (Variant
          { tag = "Pcstr_record"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Pcstr_tuple (x1) ->
        create_pcstr_tuple x1
      | Pcstr_record (x1) ->
        create_pcstr_record x1

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { ptyext_path : Node.t
      ; ptyext_params : (Node.t * Node.t) list
      ; ptyext_constructors : Node.t list
      ; ptyext_private : Node.t
      ; ptyext_attributes : Node.t
      }

    let create ~ptyext_path ~ptyext_params ~ptyext_constructors ~ptyext_private ~ptyext_attributes =
      let fields =
        [| Data.of_node ptyext_path
         ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) ptyext_params
         ; (Data.of_list ~f:Data.of_node) ptyext_constructors
         ; Data.of_node ptyext_private
         ; Data.of_node ptyext_attributes
        |]
      in
      node "type_extension" (Record fields)

    let of_concrete ({ ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } : concrete) =
      create ~ptyext_path ~ptyext_params ~ptyext_constructors ~ptyext_private ~ptyext_attributes

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "type_extension"
        ; data = Record [| ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node ptyext_path) ~f:(fun ptyext_path ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) ptyext_params) ~f:(fun ptyext_params ->
              Helpers.Option.bind ((Data.to_list ~f:Data.to_node) ptyext_constructors) ~f:(fun ptyext_constructors ->
                Helpers.Option.bind (Data.to_node ptyext_private) ~f:(fun ptyext_private ->
                  Helpers.Option.bind (Data.to_node ptyext_attributes) ~f:(fun ptyext_attributes ->
                    Some { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes }
          )))))
      | _ -> None
  end

  module Extension_constructor = struct
    type t = Node.t

    type concrete =
      { pext_name : string Location.loc
      ; pext_kind : Node.t
      ; pext_loc : Location.t
      ; pext_attributes : Node.t
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

    let of_concrete ({ pext_name; pext_kind; pext_loc; pext_attributes } : concrete) =
      create ~pext_name ~pext_kind ~pext_loc ~pext_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pext_decl of Node.t * Node.t option
      | Pext_rebind of Node.t

    let create_pext_decl x1 x2 =
      node "extension_constructor_kind"
        (Variant
          { tag = "Pext_decl"
          ; args =
            [| Data.of_node x1
             ; (Data.of_option ~f:Data.of_node) x2
            |]
          })
    let create_pext_rebind x1 =
      node "extension_constructor_kind"
        (Variant
          { tag = "Pext_rebind"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Pext_decl (x1, x2) ->
        create_pext_decl x1 x2
      | Pext_rebind (x1) ->
        create_pext_rebind x1

    let to_concrete (t : t) : concrete option =
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
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pext_rebind (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Class_type = struct
    type t = Node.t

    type concrete =
      { pcty_desc : Node.t
      ; pcty_loc : Location.t
      ; pcty_attributes : Node.t
      }

    let create ~pcty_desc ~pcty_loc ~pcty_attributes =
      let fields =
        [| Data.of_node pcty_desc
         ; Data.of_location pcty_loc
         ; Data.of_node pcty_attributes
        |]
      in
      node "class_type" (Record fields)

    let of_concrete ({ pcty_desc; pcty_loc; pcty_attributes } : concrete) =
      create ~pcty_desc ~pcty_loc ~pcty_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pcty_constr of Node.t * Node.t list
      | Pcty_signature of Node.t
      | Pcty_arrow of Node.t * Node.t * Node.t
      | Pcty_extension of Node.t
      | Pcty_open of Node.t * Node.t * Node.t

    let create_pcty_constr x1 x2 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_constr"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_pcty_signature x1 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_signature"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pcty_arrow x1 x2 x3 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_arrow"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })
    let create_pcty_extension x1 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pcty_open x1 x2 x3 =
      node "class_type_desc"
        (Variant
          { tag = "Pcty_open"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "class_type_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pcty_constr"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
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
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pcty_open (x1, x2, x3))
            )))
        | _ -> None
        end
      | _ -> None
  end

  module Class_signature = struct
    type t = Node.t

    type concrete =
      { pcsig_self : Node.t
      ; pcsig_fields : Node.t list
      }

    let create ~pcsig_self ~pcsig_fields =
      let fields =
        [| Data.of_node pcsig_self
         ; (Data.of_list ~f:Data.of_node) pcsig_fields
        |]
      in
      node "class_signature" (Record fields)

    let of_concrete ({ pcsig_self; pcsig_fields } : concrete) =
      create ~pcsig_self ~pcsig_fields

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pctf_desc : Node.t
      ; pctf_loc : Location.t
      ; pctf_attributes : Node.t
      }

    let create ~pctf_desc ~pctf_loc ~pctf_attributes =
      let fields =
        [| Data.of_node pctf_desc
         ; Data.of_location pctf_loc
         ; Data.of_node pctf_attributes
        |]
      in
      node "class_type_field" (Record fields)

    let of_concrete ({ pctf_desc; pctf_loc; pctf_attributes } : concrete) =
      create ~pctf_desc ~pctf_loc ~pctf_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pctf_inherit of Node.t
      | Pctf_val of (Node.t Location.loc * Node.t * Node.t * Node.t)
      | Pctf_method of (Node.t Location.loc * Node.t * Node.t * Node.t)
      | Pctf_constraint of (Node.t * Node.t)
      | Pctf_attribute of Node.t
      | Pctf_extension of Node.t

    let create_pctf_inherit x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_inherit"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pctf_val x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_val"
          ; args =
            [| (Data.of_tuple4 ~f1:(Data.of_loc ~f:Data.of_node) ~f2:Data.of_node ~f3:Data.of_node ~f4:Data.of_node) x1
            |]
          })
    let create_pctf_method x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_method"
          ; args =
            [| (Data.of_tuple4 ~f1:(Data.of_loc ~f:Data.of_node) ~f2:Data.of_node ~f3:Data.of_node ~f4:Data.of_node) x1
            |]
          })
    let create_pctf_constraint x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_constraint"
          ; args =
            [| (Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node) x1
            |]
          })
    let create_pctf_attribute x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pctf_extension x1 =
      node "class_type_field_desc"
        (Variant
          { tag = "Pctf_extension"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "class_type_field_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pctf_inherit"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pctf_inherit (x1))
            )
          | Variant { tag = "Pctf_val"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple4 ~f1:(Data.to_loc ~f:Data.to_node) ~f2:Data.to_node ~f3:Data.to_node ~f4:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pctf_val (x1))
            )
          | Variant { tag = "Pctf_method"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple4 ~f1:(Data.to_loc ~f:Data.to_node) ~f2:Data.to_node ~f3:Data.to_node ~f4:Data.to_node) x1) ~f:(fun x1 ->
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
    type 'a t = Node.t

    type 'a concrete =
      { pci_virt : Node.t
      ; pci_params : (Node.t * Node.t) list
      ; pci_name : string Location.loc
      ; pci_expr : 'a
      ; pci_loc : Location.t
      ; pci_attributes : Node.t
      }

    let create ast_of_a ~pci_virt ~pci_params ~pci_name ~pci_expr ~pci_loc ~pci_attributes =
      let fields =
        [| Data.of_node pci_virt
         ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) pci_params
         ; (Data.of_loc ~f:Data.of_string) pci_name
         ; ast_of_a pci_expr
         ; Data.of_location pci_loc
         ; Data.of_node pci_attributes
        |]
      in
      node "class_infos" (Record fields)

    let of_concrete ast_of_a ({ pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } : _ concrete) =
      create ast_of_a ~pci_virt ~pci_params ~pci_name ~pci_expr ~pci_loc ~pci_attributes

    let to_concrete ast_to_a (t : _ t) : _ concrete option =
      match Node.to_node t ~version with
      | { name = "class_infos"
        ; data = Record [| pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node pci_virt) ~f:(fun pci_virt ->
            Helpers.Option.bind ((Data.to_list ~f:(Data.to_tuple2 ~f1:Data.to_node ~f2:Data.to_node)) pci_params) ~f:(fun pci_params ->
              Helpers.Option.bind ((Data.to_loc ~f:Data.to_string) pci_name) ~f:(fun pci_name ->
                Helpers.Option.bind (ast_to_a pci_expr) ~f:(fun pci_expr ->
                  Helpers.Option.bind (Data.to_location pci_loc) ~f:(fun pci_loc ->
                    Helpers.Option.bind (Data.to_node pci_attributes) ~f:(fun pci_attributes ->
                      Some { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
          ))))))
      | _ -> None

    let of_concrete_class_expr =
      of_concrete
        Data.of_node

    let to_concrete_class_expr =
      to_concrete
        Data.to_node

    let create_class_expr =
      create
        Data.of_node

    let of_concrete_class_type =
      of_concrete
        Data.of_node

    let to_concrete_class_type =
      to_concrete
        Data.to_node

    let create_class_type =
      create
        Data.of_node
  end

  module Class_description = struct
    type t = Node.t

    type concrete = Node.t Class_infos.t

    let create =
      let data = Data.of_node in
      fun x -> node "class_description" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "class_description"; data } -> Data.to_node data
      | _ -> None
  end

  module Class_type_declaration = struct
    type t = Node.t

    type concrete = Node.t Class_infos.t

    let create =
      let data = Data.of_node in
      fun x -> node "class_type_declaration" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "class_type_declaration"; data } -> Data.to_node data
      | _ -> None
  end

  module Class_expr = struct
    type t = Node.t

    type concrete =
      { pcl_desc : Node.t
      ; pcl_loc : Location.t
      ; pcl_attributes : Node.t
      }

    let create ~pcl_desc ~pcl_loc ~pcl_attributes =
      let fields =
        [| Data.of_node pcl_desc
         ; Data.of_location pcl_loc
         ; Data.of_node pcl_attributes
        |]
      in
      node "class_expr" (Record fields)

    let of_concrete ({ pcl_desc; pcl_loc; pcl_attributes } : concrete) =
      create ~pcl_desc ~pcl_loc ~pcl_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pcl_constr of Node.t * Node.t list
      | Pcl_structure of Node.t
      | Pcl_fun of Node.t * Node.t option * Node.t * Node.t
      | Pcl_apply of Node.t * (Node.t * Node.t) list
      | Pcl_let of Node.t * Node.t list * Node.t
      | Pcl_constraint of Node.t * Node.t
      | Pcl_extension of Node.t
      | Pcl_open of Node.t * Node.t * Node.t

    let create_pcl_constr x1 x2 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_constr"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_pcl_structure x1 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_structure"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pcl_fun x1 x2 x3 x4 =
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
    let create_pcl_apply x1 x2 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_apply"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:(Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node)) x2
            |]
          })
    let create_pcl_let x1 x2 x3 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_let"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let create_pcl_constraint x1 x2 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pcl_extension x1 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pcl_open x1 x2 x3 =
      node "class_expr_desc"
        (Variant
          { tag = "Pcl_open"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; Data.of_node x3
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "class_expr_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pcl_constr"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
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
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Helpers.Option.bind (Data.to_node x3) ~f:(fun x3 ->
                  Some (Pcl_open (x1, x2, x3))
            )))
        | _ -> None
        end
      | _ -> None
  end

  module Class_structure = struct
    type t = Node.t

    type concrete =
      { pcstr_self : Node.t
      ; pcstr_fields : Node.t list
      }

    let create ~pcstr_self ~pcstr_fields =
      let fields =
        [| Data.of_node pcstr_self
         ; (Data.of_list ~f:Data.of_node) pcstr_fields
        |]
      in
      node "class_structure" (Record fields)

    let of_concrete ({ pcstr_self; pcstr_fields } : concrete) =
      create ~pcstr_self ~pcstr_fields

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pcf_desc : Node.t
      ; pcf_loc : Location.t
      ; pcf_attributes : Node.t
      }

    let create ~pcf_desc ~pcf_loc ~pcf_attributes =
      let fields =
        [| Data.of_node pcf_desc
         ; Data.of_location pcf_loc
         ; Data.of_node pcf_attributes
        |]
      in
      node "class_field" (Record fields)

    let of_concrete ({ pcf_desc; pcf_loc; pcf_attributes } : concrete) =
      create ~pcf_desc ~pcf_loc ~pcf_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pcf_inherit of Node.t * Node.t * string Location.loc option
      | Pcf_val of (Node.t Location.loc * Node.t * Node.t)
      | Pcf_method of (Node.t Location.loc * Node.t * Node.t)
      | Pcf_constraint of (Node.t * Node.t)
      | Pcf_initializer of Node.t
      | Pcf_attribute of Node.t
      | Pcf_extension of Node.t

    let create_pcf_inherit x1 x2 x3 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_inherit"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
             ; (Data.of_option ~f:(Data.of_loc ~f:Data.of_string)) x3
            |]
          })
    let create_pcf_val x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_val"
          ; args =
            [| (Data.of_tuple3 ~f1:(Data.of_loc ~f:Data.of_node) ~f2:Data.of_node ~f3:Data.of_node) x1
            |]
          })
    let create_pcf_method x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_method"
          ; args =
            [| (Data.of_tuple3 ~f1:(Data.of_loc ~f:Data.of_node) ~f2:Data.of_node ~f3:Data.of_node) x1
            |]
          })
    let create_pcf_constraint x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_constraint"
          ; args =
            [| (Data.of_tuple2 ~f1:Data.of_node ~f2:Data.of_node) x1
            |]
          })
    let create_pcf_initializer x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_initializer"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pcf_attribute x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pcf_extension x1 =
      node "class_field_desc"
        (Variant
          { tag = "Pcf_extension"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
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
            Helpers.Option.bind ((Data.to_tuple3 ~f1:(Data.to_loc ~f:Data.to_node) ~f2:Data.to_node ~f3:Data.to_node) x1) ~f:(fun x1 ->
              Some (Pcf_val (x1))
            )
          | Variant { tag = "Pcf_method"; args = [| x1 |] } ->
            Helpers.Option.bind ((Data.to_tuple3 ~f1:(Data.to_loc ~f:Data.to_node) ~f2:Data.to_node ~f3:Data.to_node) x1) ~f:(fun x1 ->
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
    type t = Node.t

    type concrete =
      | Cfk_virtual of Node.t
      | Cfk_concrete of Node.t * Node.t

    let create_cfk_virtual x1 =
      node "class_field_kind"
        (Variant
          { tag = "Cfk_virtual"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_cfk_concrete x1 x2 =
      node "class_field_kind"
        (Variant
          { tag = "Cfk_concrete"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Cfk_virtual (x1) ->
        create_cfk_virtual x1
      | Cfk_concrete (x1, x2) ->
        create_cfk_concrete x1 x2

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete = Node.t Class_infos.t

    let create =
      let data = Data.of_node in
      fun x -> node "class_declaration" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "class_declaration"; data } -> Data.to_node data
      | _ -> None
  end

  module Module_type = struct
    type t = Node.t

    type concrete =
      { pmty_desc : Node.t
      ; pmty_loc : Location.t
      ; pmty_attributes : Node.t
      }

    let create ~pmty_desc ~pmty_loc ~pmty_attributes =
      let fields =
        [| Data.of_node pmty_desc
         ; Data.of_location pmty_loc
         ; Data.of_node pmty_attributes
        |]
      in
      node "module_type" (Record fields)

    let of_concrete ({ pmty_desc; pmty_loc; pmty_attributes } : concrete) =
      create ~pmty_desc ~pmty_loc ~pmty_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pmty_ident of Node.t
      | Pmty_signature of Node.t
      | Pmty_functor of string Location.loc * Node.t option * Node.t
      | Pmty_with of Node.t * Node.t list
      | Pmty_typeof of Node.t
      | Pmty_extension of Node.t
      | Pmty_alias of Node.t

    let create_pmty_ident x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_ident"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pmty_signature x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_signature"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pmty_functor x1 x2 x3 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_functor"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; (Data.of_option ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let create_pmty_with x1 x2 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_with"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_pmty_typeof x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_typeof"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pmty_extension x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_extension"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pmty_alias x1 =
      node "module_type_desc"
        (Variant
          { tag = "Pmty_alias"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "module_type_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pmty_ident"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
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
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Some (Pmty_alias (x1))
            )
        | _ -> None
        end
      | _ -> None
  end

  module Signature = struct
    type t = Node.t

    type concrete = Node.t list

    let create =
      let data = (Data.of_list ~f:Data.of_node) in
      fun x -> node "signature" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "signature"; data } -> (Data.to_list ~f:Data.to_node) data
      | _ -> None
  end

  module Signature_item = struct
    type t = Node.t

    type concrete =
      { psig_desc : Node.t
      ; psig_loc : Location.t
      }

    let create ~psig_desc ~psig_loc =
      let fields =
        [| Data.of_node psig_desc
         ; Data.of_location psig_loc
        |]
      in
      node "signature_item" (Record fields)

    let of_concrete ({ psig_desc; psig_loc } : concrete) =
      create ~psig_desc ~psig_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Psig_value of Node.t
      | Psig_type of Node.t * Node.t list
      | Psig_typext of Node.t
      | Psig_exception of Node.t
      | Psig_module of Node.t
      | Psig_recmodule of Node.t list
      | Psig_modtype of Node.t
      | Psig_open of Node.t
      | Psig_include of Node.t
      | Psig_class of Node.t list
      | Psig_class_type of Node.t list
      | Psig_attribute of Node.t
      | Psig_extension of Node.t * Node.t

    let create_psig_value x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_value"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_type x1 x2 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_type"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_psig_typext x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_typext"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_exception x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_exception"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_module x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_module"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_recmodule x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_recmodule"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_psig_modtype x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_modtype"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_open x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_open"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_include x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_include"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_class x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_class"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_psig_class_type x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_class_type"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_psig_attribute x1 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_psig_extension x1 x2 =
      node "signature_item_desc"
        (Variant
          { tag = "Psig_extension"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pmd_name : string Location.loc
      ; pmd_type : Node.t
      ; pmd_attributes : Node.t
      ; pmd_loc : Location.t
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

    let of_concrete ({ pmd_name; pmd_type; pmd_attributes; pmd_loc } : concrete) =
      create ~pmd_name ~pmd_type ~pmd_attributes ~pmd_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pmtd_name : string Location.loc
      ; pmtd_type : Node.t option
      ; pmtd_attributes : Node.t
      ; pmtd_loc : Location.t
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

    let of_concrete ({ pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } : concrete) =
      create ~pmtd_name ~pmtd_type ~pmtd_attributes ~pmtd_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { popen_lid : Node.t
      ; popen_override : Node.t
      ; popen_loc : Location.t
      ; popen_attributes : Node.t
      }

    let create ~popen_lid ~popen_override ~popen_loc ~popen_attributes =
      let fields =
        [| Data.of_node popen_lid
         ; Data.of_node popen_override
         ; Data.of_location popen_loc
         ; Data.of_node popen_attributes
        |]
      in
      node "open_description" (Record fields)

    let of_concrete ({ popen_lid; popen_override; popen_loc; popen_attributes } : concrete) =
      create ~popen_lid ~popen_override ~popen_loc ~popen_attributes

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "open_description"
        ; data = Record [| popen_lid; popen_override; popen_loc; popen_attributes |]
        } ->
          Helpers.Option.bind (Data.to_node popen_lid) ~f:(fun popen_lid ->
            Helpers.Option.bind (Data.to_node popen_override) ~f:(fun popen_override ->
              Helpers.Option.bind (Data.to_location popen_loc) ~f:(fun popen_loc ->
                Helpers.Option.bind (Data.to_node popen_attributes) ~f:(fun popen_attributes ->
                  Some { popen_lid; popen_override; popen_loc; popen_attributes }
          ))))
      | _ -> None
  end

  module Include_infos = struct
    type 'a t = Node.t

    type 'a concrete =
      { pincl_mod : 'a
      ; pincl_loc : Location.t
      ; pincl_attributes : Node.t
      }

    let create ast_of_a ~pincl_mod ~pincl_loc ~pincl_attributes =
      let fields =
        [| ast_of_a pincl_mod
         ; Data.of_location pincl_loc
         ; Data.of_node pincl_attributes
        |]
      in
      node "include_infos" (Record fields)

    let of_concrete ast_of_a ({ pincl_mod; pincl_loc; pincl_attributes } : _ concrete) =
      create ast_of_a ~pincl_mod ~pincl_loc ~pincl_attributes

    let to_concrete ast_to_a (t : _ t) : _ concrete option =
      match Node.to_node t ~version with
      | { name = "include_infos"
        ; data = Record [| pincl_mod; pincl_loc; pincl_attributes |]
        } ->
          Helpers.Option.bind (ast_to_a pincl_mod) ~f:(fun pincl_mod ->
            Helpers.Option.bind (Data.to_location pincl_loc) ~f:(fun pincl_loc ->
              Helpers.Option.bind (Data.to_node pincl_attributes) ~f:(fun pincl_attributes ->
                Some { pincl_mod; pincl_loc; pincl_attributes }
          )))
      | _ -> None

    let of_concrete_module_expr =
      of_concrete
        Data.of_node

    let to_concrete_module_expr =
      to_concrete
        Data.to_node

    let create_module_expr =
      create
        Data.of_node

    let of_concrete_module_type =
      of_concrete
        Data.of_node

    let to_concrete_module_type =
      to_concrete
        Data.to_node

    let create_module_type =
      create
        Data.of_node
  end

  module Include_description = struct
    type t = Node.t

    type concrete = Node.t Include_infos.t

    let create =
      let data = Data.of_node in
      fun x -> node "include_description" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "include_description"; data } -> Data.to_node data
      | _ -> None
  end

  module Include_declaration = struct
    type t = Node.t

    type concrete = Node.t Include_infos.t

    let create =
      let data = Data.of_node in
      fun x -> node "include_declaration" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "include_declaration"; data } -> Data.to_node data
      | _ -> None
  end

  module With_constraint = struct
    type t = Node.t

    type concrete =
      | Pwith_type of Node.t * Node.t
      | Pwith_module of Node.t * Node.t
      | Pwith_typesubst of Node.t * Node.t
      | Pwith_modsubst of Node.t * Node.t

    let create_pwith_type x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_type"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pwith_module x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_module"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pwith_typesubst x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_typesubst"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pwith_modsubst x1 x2 =
      node "with_constraint"
        (Variant
          { tag = "Pwith_modsubst"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Pwith_type (x1, x2) ->
        create_pwith_type x1 x2
      | Pwith_module (x1, x2) ->
        create_pwith_module x1 x2
      | Pwith_typesubst (x1, x2) ->
        create_pwith_typesubst x1 x2
      | Pwith_modsubst (x1, x2) ->
        create_pwith_modsubst x1 x2

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "with_constraint"; data } ->
        begin
          match data with
          | Variant { tag = "Pwith_type"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pwith_type (x1, x2))
            ))
          | Variant { tag = "Pwith_module"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pwith_module (x1, x2))
            ))
          | Variant { tag = "Pwith_typesubst"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pwith_typesubst (x1, x2))
            ))
          | Variant { tag = "Pwith_modsubst"; args = [| x1; x2 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
              Helpers.Option.bind (Data.to_node x2) ~f:(fun x2 ->
                Some (Pwith_modsubst (x1, x2))
            ))
        | _ -> None
        end
      | _ -> None
  end

  module Module_expr = struct
    type t = Node.t

    type concrete =
      { pmod_desc : Node.t
      ; pmod_loc : Location.t
      ; pmod_attributes : Node.t
      }

    let create ~pmod_desc ~pmod_loc ~pmod_attributes =
      let fields =
        [| Data.of_node pmod_desc
         ; Data.of_location pmod_loc
         ; Data.of_node pmod_attributes
        |]
      in
      node "module_expr" (Record fields)

    let of_concrete ({ pmod_desc; pmod_loc; pmod_attributes } : concrete) =
      create ~pmod_desc ~pmod_loc ~pmod_attributes

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pmod_ident of Node.t
      | Pmod_structure of Node.t
      | Pmod_functor of string Location.loc * Node.t option * Node.t
      | Pmod_apply of Node.t * Node.t
      | Pmod_constraint of Node.t * Node.t
      | Pmod_unpack of Node.t
      | Pmod_extension of Node.t

    let create_pmod_ident x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_ident"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pmod_structure x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_structure"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pmod_functor x1 x2 x3 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_functor"
          ; args =
            [| (Data.of_loc ~f:Data.of_string) x1
             ; (Data.of_option ~f:Data.of_node) x2
             ; Data.of_node x3
            |]
          })
    let create_pmod_apply x1 x2 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_apply"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pmod_constraint x1 x2 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_constraint"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pmod_unpack x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_unpack"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pmod_extension x1 =
      node "module_expr_desc"
        (Variant
          { tag = "Pmod_extension"
          ; args =
            [| Data.of_node x1
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
      match Node.to_node t ~version with
      | { name = "module_expr_desc"; data } ->
        begin
          match data with
          | Variant { tag = "Pmod_ident"; args = [| x1 |] } ->
            Helpers.Option.bind (Data.to_node x1) ~f:(fun x1 ->
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
    type t = Node.t

    type concrete = Node.t list

    let create =
      let data = (Data.of_list ~f:Data.of_node) in
      fun x -> node "structure" (data x)

    let of_concrete = create

    let to_concrete (t : t) =
      match Node.to_node t ~version with
      | { name = "structure"; data } -> (Data.to_list ~f:Data.to_node) data
      | _ -> None
  end

  module Structure_item = struct
    type t = Node.t

    type concrete =
      { pstr_desc : Node.t
      ; pstr_loc : Location.t
      }

    let create ~pstr_desc ~pstr_loc =
      let fields =
        [| Data.of_node pstr_desc
         ; Data.of_location pstr_loc
        |]
      in
      node "structure_item" (Record fields)

    let of_concrete ({ pstr_desc; pstr_loc } : concrete) =
      create ~pstr_desc ~pstr_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pstr_eval of Node.t * Node.t
      | Pstr_value of Node.t * Node.t list
      | Pstr_primitive of Node.t
      | Pstr_type of Node.t * Node.t list
      | Pstr_typext of Node.t
      | Pstr_exception of Node.t
      | Pstr_module of Node.t
      | Pstr_recmodule of Node.t list
      | Pstr_modtype of Node.t
      | Pstr_open of Node.t
      | Pstr_class of Node.t list
      | Pstr_class_type of Node.t list
      | Pstr_include of Node.t
      | Pstr_attribute of Node.t
      | Pstr_extension of Node.t * Node.t

    let create_pstr_eval x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_eval"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })
    let create_pstr_value x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_value"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_pstr_primitive x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_primitive"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_type x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_type"
          ; args =
            [| Data.of_node x1
             ; (Data.of_list ~f:Data.of_node) x2
            |]
          })
    let create_pstr_typext x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_typext"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_exception x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_exception"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_module x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_module"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_recmodule x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_recmodule"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_pstr_modtype x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_modtype"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_open x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_open"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_class x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_class"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_pstr_class_type x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_class_type"
          ; args =
            [| (Data.of_list ~f:Data.of_node) x1
            |]
          })
    let create_pstr_include x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_include"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_attribute x1 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_attribute"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pstr_extension x1 x2 =
      node "structure_item_desc"
        (Variant
          { tag = "Pstr_extension"
          ; args =
            [| Data.of_node x1
             ; Data.of_node x2
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pvb_pat : Node.t
      ; pvb_expr : Node.t
      ; pvb_attributes : Node.t
      ; pvb_loc : Location.t
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

    let of_concrete ({ pvb_pat; pvb_expr; pvb_attributes; pvb_loc } : concrete) =
      create ~pvb_pat ~pvb_expr ~pvb_attributes ~pvb_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      { pmb_name : string Location.loc
      ; pmb_expr : Node.t
      ; pmb_attributes : Node.t
      ; pmb_loc : Location.t
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

    let of_concrete ({ pmb_name; pmb_expr; pmb_attributes; pmb_loc } : concrete) =
      create ~pmb_name ~pmb_expr ~pmb_attributes ~pmb_loc

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Ptop_def of Node.t
      | Ptop_dir of string * Node.t

    let create_ptop_def x1 =
      node "toplevel_phrase"
        (Variant
          { tag = "Ptop_def"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_ptop_dir x1 x2 =
      node "toplevel_phrase"
        (Variant
          { tag = "Ptop_dir"
          ; args =
            [| Data.of_string x1
             ; Data.of_node x2
            |]
          })

    let of_concrete (concrete : concrete) =
      match concrete with
      | Ptop_def (x1) ->
        create_ptop_def x1
      | Ptop_dir (x1, x2) ->
        create_ptop_dir x1 x2

    let to_concrete (t : t) : concrete option =
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
    type t = Node.t

    type concrete =
      | Pdir_none
      | Pdir_string of string
      | Pdir_int of string * char option
      | Pdir_ident of Node.t
      | Pdir_bool of bool

    let create_pdir_none =
      node "directive_argument" (Variant { tag = "Pdir_none"; args = [||] })
    let create_pdir_string x1 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_string"
          ; args =
            [| Data.of_string x1
            |]
          })
    let create_pdir_int x1 x2 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_int"
          ; args =
            [| Data.of_string x1
             ; (Data.of_option ~f:Data.of_char) x2
            |]
          })
    let create_pdir_ident x1 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_ident"
          ; args =
            [| Data.of_node x1
            |]
          })
    let create_pdir_bool x1 =
      node "directive_argument"
        (Variant
          { tag = "Pdir_bool"
          ; args =
            [| Data.of_bool x1
            |]
          })

    let of_concrete (concrete : concrete) =
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

    let to_concrete (t : t) : concrete option =
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
