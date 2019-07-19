(*$ Astlib_src_cinaps.print_astlib_ml () *)
open! StdLabels
open! Ocaml_common

module V4_07 = struct
  let version = "V4_07"

  module Longident = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Lident of { a : string }
        | Ldot of { a : Versioned_ast.t; b : string }
        | Lapply of { a : Versioned_ast.t; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Lident { a } ->
        Versioned_ast.create ~version
          { kind = "Longident"
          ; clause = "Lident"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_string a }
              ]
          }
      | Ldot { a; b } ->
        Versioned_ast.create ~version
          { kind = "Longident"
          ; clause = "Ldot"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_string b }
              ]
          }
      | Lapply { a; b } ->
        Versioned_ast.create ~version
          { kind = "Longident"
          ; clause = "Lapply"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Longident"
        ; clause = "Lident"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_string a) ~f:(fun a ->
          Some (Concrete.Lident { a }))
      | { kind = "Longident"
        ; clause = "Ldot"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_string b) ~f:(fun b ->
            Some (Concrete.Ldot { a; b })))
      | { kind = "Longident"
        ; clause = "Lapply"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Lapply { a; b })))
      | _ -> None
  end

  module Longident_loc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Longident_loc of { txt : Versioned_ast.t; loc : Location.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Longident_loc { txt; loc } ->
        Versioned_ast.create ~version
          { kind = "Longident_loc"
          ; clause = "Longident_loc"
          ; fields =
              [ { name = "txt"; value = Versioned_value.of_ast txt }
              ; { name = "loc"; value = Versioned_value.of_location loc }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Longident_loc"
        ; clause = "Longident_loc"
        ; fields =
            [ { name = "txt"; value = txt }
            ; { name = "loc"; value = loc }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast txt) ~f:(fun txt ->
          Optional.bind (Versioned_value.to_location loc) ~f:(fun loc ->
            Some (Concrete.Longident_loc { txt; loc })))
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

    let of_concrete : Concrete.t -> t = function
      | Nonrecursive ->
        Versioned_ast.create ~version
          { kind = "Rec_flag"
          ; clause = "Nonrecursive"
          ; fields = []
          }
      | Recursive ->
        Versioned_ast.create ~version
          { kind = "Rec_flag"
          ; clause = "Recursive"
          ; fields = []
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Rec_flag"
        ; clause = "Nonrecursive"
        ; fields = []
        } ->
        Some (Concrete.Nonrecursive)
      | { kind = "Rec_flag"
        ; clause = "Recursive"
        ; fields = []
        } ->
        Some (Concrete.Recursive)
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

    let of_concrete : Concrete.t -> t = function
      | Upto ->
        Versioned_ast.create ~version
          { kind = "Direction_flag"
          ; clause = "Upto"
          ; fields = []
          }
      | Downto ->
        Versioned_ast.create ~version
          { kind = "Direction_flag"
          ; clause = "Downto"
          ; fields = []
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Direction_flag"
        ; clause = "Upto"
        ; fields = []
        } ->
        Some (Concrete.Upto)
      | { kind = "Direction_flag"
        ; clause = "Downto"
        ; fields = []
        } ->
        Some (Concrete.Downto)
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

    let of_concrete : Concrete.t -> t = function
      | Private ->
        Versioned_ast.create ~version
          { kind = "Private_flag"
          ; clause = "Private"
          ; fields = []
          }
      | Public ->
        Versioned_ast.create ~version
          { kind = "Private_flag"
          ; clause = "Public"
          ; fields = []
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Private_flag"
        ; clause = "Private"
        ; fields = []
        } ->
        Some (Concrete.Private)
      | { kind = "Private_flag"
        ; clause = "Public"
        ; fields = []
        } ->
        Some (Concrete.Public)
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

    let of_concrete : Concrete.t -> t = function
      | Immutable ->
        Versioned_ast.create ~version
          { kind = "Mutable_flag"
          ; clause = "Immutable"
          ; fields = []
          }
      | Mutable ->
        Versioned_ast.create ~version
          { kind = "Mutable_flag"
          ; clause = "Mutable"
          ; fields = []
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Mutable_flag"
        ; clause = "Immutable"
        ; fields = []
        } ->
        Some (Concrete.Immutable)
      | { kind = "Mutable_flag"
        ; clause = "Mutable"
        ; fields = []
        } ->
        Some (Concrete.Mutable)
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

    let of_concrete : Concrete.t -> t = function
      | Virtual ->
        Versioned_ast.create ~version
          { kind = "Virtual_flag"
          ; clause = "Virtual"
          ; fields = []
          }
      | Concrete ->
        Versioned_ast.create ~version
          { kind = "Virtual_flag"
          ; clause = "Concrete"
          ; fields = []
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Virtual_flag"
        ; clause = "Virtual"
        ; fields = []
        } ->
        Some (Concrete.Virtual)
      | { kind = "Virtual_flag"
        ; clause = "Concrete"
        ; fields = []
        } ->
        Some (Concrete.Concrete)
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

    let of_concrete : Concrete.t -> t = function
      | Override ->
        Versioned_ast.create ~version
          { kind = "Override_flag"
          ; clause = "Override"
          ; fields = []
          }
      | Fresh ->
        Versioned_ast.create ~version
          { kind = "Override_flag"
          ; clause = "Fresh"
          ; fields = []
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Override_flag"
        ; clause = "Override"
        ; fields = []
        } ->
        Some (Concrete.Override)
      | { kind = "Override_flag"
        ; clause = "Fresh"
        ; fields = []
        } ->
        Some (Concrete.Fresh)
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

    let of_concrete : Concrete.t -> t = function
      | Closed ->
        Versioned_ast.create ~version
          { kind = "Closed_flag"
          ; clause = "Closed"
          ; fields = []
          }
      | Open ->
        Versioned_ast.create ~version
          { kind = "Closed_flag"
          ; clause = "Open"
          ; fields = []
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Closed_flag"
        ; clause = "Closed"
        ; fields = []
        } ->
        Some (Concrete.Closed)
      | { kind = "Closed_flag"
        ; clause = "Open"
        ; fields = []
        } ->
        Some (Concrete.Open)
      | _ -> None
  end

  module Label = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Label of { a : string }
    end

    let of_concrete : Concrete.t -> t = function
      | Label { a } ->
        Versioned_ast.create ~version
          { kind = "Label"
          ; clause = "Label"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_string a }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Label"
        ; clause = "Label"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_string a) ~f:(fun a ->
          Some (Concrete.Label { a }))
      | _ -> None
  end

  module Label_loc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Label_loc of { txt : Versioned_ast.t; loc : Location.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Label_loc { txt; loc } ->
        Versioned_ast.create ~version
          { kind = "Label_loc"
          ; clause = "Label_loc"
          ; fields =
              [ { name = "txt"; value = Versioned_value.of_ast txt }
              ; { name = "loc"; value = Versioned_value.of_location loc }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Label_loc"
        ; clause = "Label_loc"
        ; fields =
            [ { name = "txt"; value = txt }
            ; { name = "loc"; value = loc }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast txt) ~f:(fun txt ->
          Optional.bind (Versioned_value.to_location loc) ~f:(fun loc ->
            Some (Concrete.Label_loc { txt; loc })))
      | _ -> None
  end

  module String_loc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | String_loc of { txt : string; loc : Location.t }
    end

    let of_concrete : Concrete.t -> t = function
      | String_loc { txt; loc } ->
        Versioned_ast.create ~version
          { kind = "String_loc"
          ; clause = "String_loc"
          ; fields =
              [ { name = "txt"; value = Versioned_value.of_string txt }
              ; { name = "loc"; value = Versioned_value.of_location loc }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "String_loc"
        ; clause = "String_loc"
        ; fields =
            [ { name = "txt"; value = txt }
            ; { name = "loc"; value = loc }
            ]
        } ->
        Optional.bind (Versioned_value.to_string txt) ~f:(fun txt ->
          Optional.bind (Versioned_value.to_location loc) ~f:(fun loc ->
            Some (Concrete.String_loc { txt; loc })))
      | _ -> None
  end

  module Arg_label = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Nolabel
        | Labelled of { a : string }
        | Optional of { a : string }
    end

    let of_concrete : Concrete.t -> t = function
      | Nolabel ->
        Versioned_ast.create ~version
          { kind = "Arg_label"
          ; clause = "Nolabel"
          ; fields = []
          }
      | Labelled { a } ->
        Versioned_ast.create ~version
          { kind = "Arg_label"
          ; clause = "Labelled"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_string a }
              ]
          }
      | Optional { a } ->
        Versioned_ast.create ~version
          { kind = "Arg_label"
          ; clause = "Optional"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_string a }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Arg_label"
        ; clause = "Nolabel"
        ; fields = []
        } ->
        Some (Concrete.Nolabel)
      | { kind = "Arg_label"
        ; clause = "Labelled"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_string a) ~f:(fun a ->
          Some (Concrete.Labelled { a }))
      | { kind = "Arg_label"
        ; clause = "Optional"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_string a) ~f:(fun a ->
          Some (Concrete.Optional { a }))
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

    let of_concrete : Concrete.t -> t = function
      | Covariant ->
        Versioned_ast.create ~version
          { kind = "Variance"
          ; clause = "Covariant"
          ; fields = []
          }
      | Contravariant ->
        Versioned_ast.create ~version
          { kind = "Variance"
          ; clause = "Contravariant"
          ; fields = []
          }
      | Invariant ->
        Versioned_ast.create ~version
          { kind = "Variance"
          ; clause = "Invariant"
          ; fields = []
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Variance"
        ; clause = "Covariant"
        ; fields = []
        } ->
        Some (Concrete.Covariant)
      | { kind = "Variance"
        ; clause = "Contravariant"
        ; fields = []
        } ->
        Some (Concrete.Contravariant)
      | { kind = "Variance"
        ; clause = "Invariant"
        ; fields = []
        } ->
        Some (Concrete.Invariant)
      | _ -> None
  end

  module Constant = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pconst_integer of { a : string; b : char option }
        | Pconst_char of { a : char }
        | Pconst_string of { a : string; b : string option }
        | Pconst_float of { a : string; b : char option }
    end

    let of_concrete : Concrete.t -> t = function
      | Pconst_integer { a; b } ->
        Versioned_ast.create ~version
          { kind = "Constant"
          ; clause = "Pconst_integer"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_string a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_char) b }
              ]
          }
      | Pconst_char { a } ->
        Versioned_ast.create ~version
          { kind = "Constant"
          ; clause = "Pconst_char"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_char a }
              ]
          }
      | Pconst_string { a; b } ->
        Versioned_ast.create ~version
          { kind = "Constant"
          ; clause = "Pconst_string"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_string a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_string) b }
              ]
          }
      | Pconst_float { a; b } ->
        Versioned_ast.create ~version
          { kind = "Constant"
          ; clause = "Pconst_float"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_string a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_char) b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Constant"
        ; clause = "Pconst_integer"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_string a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_char) b) ~f:(fun b ->
            Some (Concrete.Pconst_integer { a; b })))
      | { kind = "Constant"
        ; clause = "Pconst_char"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_char a) ~f:(fun a ->
          Some (Concrete.Pconst_char { a }))
      | { kind = "Constant"
        ; clause = "Pconst_string"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_string a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_string) b) ~f:(fun b ->
            Some (Concrete.Pconst_string { a; b })))
      | { kind = "Constant"
        ; clause = "Pconst_float"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_string a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_char) b) ~f:(fun b ->
            Some (Concrete.Pconst_float { a; b })))
      | _ -> None
  end

  module Attribute = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Attribute of { a : Versioned_ast.t; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Attribute { a; b } ->
        Versioned_ast.create ~version
          { kind = "Attribute"
          ; clause = "Attribute"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Attribute"
        ; clause = "Attribute"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Attribute { a; b })))
      | _ -> None
  end

  module Extension = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Extension of { a : Versioned_ast.t; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Extension { a; b } ->
        Versioned_ast.create ~version
          { kind = "Extension"
          ; clause = "Extension"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Extension"
        ; clause = "Extension"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Extension { a; b })))
      | _ -> None
  end

  module Attributes = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Attributes of { a : Versioned_ast.t list }
    end

    let of_concrete : Concrete.t -> t = function
      | Attributes { a } ->
        Versioned_ast.create ~version
          { kind = "Attributes"
          ; clause = "Attributes"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Attributes"
        ; clause = "Attributes"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Attributes { a }))
      | _ -> None
  end

  module Payload = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | PStr of { a : Versioned_ast.t }
        | PSig of { a : Versioned_ast.t }
        | PTyp of { a : Versioned_ast.t }
        | PPat of { a : Versioned_ast.t; b : Versioned_ast.t option }
    end

    let of_concrete : Concrete.t -> t = function
      | PStr { a } ->
        Versioned_ast.create ~version
          { kind = "Payload"
          ; clause = "PStr"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | PSig { a } ->
        Versioned_ast.create ~version
          { kind = "Payload"
          ; clause = "PSig"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | PTyp { a } ->
        Versioned_ast.create ~version
          { kind = "Payload"
          ; clause = "PTyp"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | PPat { a; b } ->
        Versioned_ast.create ~version
          { kind = "Payload"
          ; clause = "PPat"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Payload"
        ; clause = "PStr"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.PStr { a }))
      | { kind = "Payload"
        ; clause = "PSig"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.PSig { a }))
      | { kind = "Payload"
        ; clause = "PTyp"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.PTyp { a }))
      | { kind = "Payload"
        ; clause = "PPat"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.PPat { a; b })))
      | _ -> None
  end

  module Core_type = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Core_type of { ptyp_desc : Versioned_ast.t; ptyp_loc : Location.t; ptyp_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Core_type { ptyp_desc; ptyp_loc; ptyp_attributes } ->
        Versioned_ast.create ~version
          { kind = "Core_type"
          ; clause = "Core_type"
          ; fields =
              [ { name = "ptyp_desc"; value = Versioned_value.of_ast ptyp_desc }
              ; { name = "ptyp_loc"; value = Versioned_value.of_location ptyp_loc }
              ; { name = "ptyp_attributes"; value = Versioned_value.of_ast ptyp_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Core_type"
        ; clause = "Core_type"
        ; fields =
            [ { name = "ptyp_desc"; value = ptyp_desc }
            ; { name = "ptyp_loc"; value = ptyp_loc }
            ; { name = "ptyp_attributes"; value = ptyp_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast ptyp_desc) ~f:(fun ptyp_desc ->
          Optional.bind (Versioned_value.to_location ptyp_loc) ~f:(fun ptyp_loc ->
            Optional.bind (Versioned_value.to_ast ptyp_attributes) ~f:(fun ptyp_attributes ->
              Some (Concrete.Core_type { ptyp_desc; ptyp_loc; ptyp_attributes }))))
      | _ -> None
  end

  module Core_type_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Ptyp_any
        | Ptyp_var of { a : string }
        | Ptyp_arrow of { a : Versioned_ast.t; b : Versioned_ast.t; c : Versioned_ast.t }
        | Ptyp_tuple of { a : Versioned_ast.t list }
        | Ptyp_constr of { a : Versioned_ast.t; b : Versioned_ast.t list }
        | Ptyp_object of { a : Versioned_ast.t list; b : Versioned_ast.t }
        | Ptyp_class of { a : Versioned_ast.t; b : Versioned_ast.t list }
        | Ptyp_alias of { a : Versioned_ast.t; b : string }
        | Ptyp_variant of { a : Versioned_ast.t list; b : Versioned_ast.t; c : Versioned_ast.t list option }
        | Ptyp_poly of { a : Versioned_ast.t list; b : Versioned_ast.t }
        | Ptyp_package of { a : Versioned_ast.t }
        | Ptyp_extension of { a : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Ptyp_any ->
        Versioned_ast.create ~version
          { kind = "Core_type_desc"
          ; clause = "Ptyp_any"
          ; fields = []
          }
      | Ptyp_var { a } ->
        Versioned_ast.create ~version
          { kind = "Core_type_desc"
          ; clause = "Ptyp_var"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_string a }
              ]
          }
      | Ptyp_arrow { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Core_type_desc"
          ; clause = "Ptyp_arrow"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ]
          }
      | Ptyp_tuple { a } ->
        Versioned_ast.create ~version
          { kind = "Core_type_desc"
          ; clause = "Ptyp_tuple"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Ptyp_constr { a; b } ->
        Versioned_ast.create ~version
          { kind = "Core_type_desc"
          ; clause = "Ptyp_constr"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) b }
              ]
          }
      | Ptyp_object { a; b } ->
        Versioned_ast.create ~version
          { kind = "Core_type_desc"
          ; clause = "Ptyp_object"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Ptyp_class { a; b } ->
        Versioned_ast.create ~version
          { kind = "Core_type_desc"
          ; clause = "Ptyp_class"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) b }
              ]
          }
      | Ptyp_alias { a; b } ->
        Versioned_ast.create ~version
          { kind = "Core_type_desc"
          ; clause = "Ptyp_alias"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_string b }
              ]
          }
      | Ptyp_variant { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Core_type_desc"
          ; clause = "Ptyp_variant"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = (Versioned_value.of_option ~f:(Versioned_value.of_list ~f:Versioned_value.of_ast)) c }
              ]
          }
      | Ptyp_poly { a; b } ->
        Versioned_ast.create ~version
          { kind = "Core_type_desc"
          ; clause = "Ptyp_poly"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Ptyp_package { a } ->
        Versioned_ast.create ~version
          { kind = "Core_type_desc"
          ; clause = "Ptyp_package"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Ptyp_extension { a } ->
        Versioned_ast.create ~version
          { kind = "Core_type_desc"
          ; clause = "Ptyp_extension"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Core_type_desc"
        ; clause = "Ptyp_any"
        ; fields = []
        } ->
        Some (Concrete.Ptyp_any)
      | { kind = "Core_type_desc"
        ; clause = "Ptyp_var"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_string a) ~f:(fun a ->
          Some (Concrete.Ptyp_var { a }))
      | { kind = "Core_type_desc"
        ; clause = "Ptyp_arrow"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Some (Concrete.Ptyp_arrow { a; b; c }))))
      | { kind = "Core_type_desc"
        ; clause = "Ptyp_tuple"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Ptyp_tuple { a }))
      | { kind = "Core_type_desc"
        ; clause = "Ptyp_constr"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Ptyp_constr { a; b })))
      | { kind = "Core_type_desc"
        ; clause = "Ptyp_object"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Ptyp_object { a; b })))
      | { kind = "Core_type_desc"
        ; clause = "Ptyp_class"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Ptyp_class { a; b })))
      | { kind = "Core_type_desc"
        ; clause = "Ptyp_alias"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_string b) ~f:(fun b ->
            Some (Concrete.Ptyp_alias { a; b })))
      | { kind = "Core_type_desc"
        ; clause = "Ptyp_variant"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind ((Versioned_value.to_option ~f:(Versioned_value.to_list ~f:Versioned_value.to_ast)) c) ~f:(fun c ->
              Some (Concrete.Ptyp_variant { a; b; c }))))
      | { kind = "Core_type_desc"
        ; clause = "Ptyp_poly"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Ptyp_poly { a; b })))
      | { kind = "Core_type_desc"
        ; clause = "Ptyp_package"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Ptyp_package { a }))
      | { kind = "Core_type_desc"
        ; clause = "Ptyp_extension"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Ptyp_extension { a }))
      | _ -> None
  end

  module Package_type = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Package_type of { a : Versioned_ast.t; b : Versioned_ast.t list }
    end

    let of_concrete : Concrete.t -> t = function
      | Package_type { a; b } ->
        Versioned_ast.create ~version
          { kind = "Package_type"
          ; clause = "Package_type"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Package_type"
        ; clause = "Package_type"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Package_type { a; b })))
      | _ -> None
  end

  module Package_type_constraint = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Package_type_constraint of { a : Versioned_ast.t; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Package_type_constraint { a; b } ->
        Versioned_ast.create ~version
          { kind = "Package_type_constraint"
          ; clause = "Package_type_constraint"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Package_type_constraint"
        ; clause = "Package_type_constraint"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Package_type_constraint { a; b })))
      | _ -> None
  end

  module Row_field = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Rtag of { a : Versioned_ast.t; b : Versioned_ast.t; c : bool; d : Versioned_ast.t list }
        | Rinherit of { a : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Rtag { a; b; c; d } ->
        Versioned_ast.create ~version
          { kind = "Row_field"
          ; clause = "Rtag"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = Versioned_value.of_bool c }
              ; { name = "d"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) d }
              ]
          }
      | Rinherit { a } ->
        Versioned_ast.create ~version
          { kind = "Row_field"
          ; clause = "Rinherit"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Row_field"
        ; clause = "Rtag"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ; { name = "d"; value = d }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_bool c) ~f:(fun c ->
              Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) d) ~f:(fun d ->
                Some (Concrete.Rtag { a; b; c; d })))))
      | { kind = "Row_field"
        ; clause = "Rinherit"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Rinherit { a }))
      | _ -> None
  end

  module Object_field = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Otag of { a : Versioned_ast.t; b : Versioned_ast.t; c : Versioned_ast.t }
        | Oinherit of { a : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Otag { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Object_field"
          ; clause = "Otag"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ]
          }
      | Oinherit { a } ->
        Versioned_ast.create ~version
          { kind = "Object_field"
          ; clause = "Oinherit"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Object_field"
        ; clause = "Otag"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Some (Concrete.Otag { a; b; c }))))
      | { kind = "Object_field"
        ; clause = "Oinherit"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Oinherit { a }))
      | _ -> None
  end

  module Pattern = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pattern of { ppat_desc : Versioned_ast.t; ppat_loc : Location.t; ppat_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Pattern { ppat_desc; ppat_loc; ppat_attributes } ->
        Versioned_ast.create ~version
          { kind = "Pattern"
          ; clause = "Pattern"
          ; fields =
              [ { name = "ppat_desc"; value = Versioned_value.of_ast ppat_desc }
              ; { name = "ppat_loc"; value = Versioned_value.of_location ppat_loc }
              ; { name = "ppat_attributes"; value = Versioned_value.of_ast ppat_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Pattern"
        ; clause = "Pattern"
        ; fields =
            [ { name = "ppat_desc"; value = ppat_desc }
            ; { name = "ppat_loc"; value = ppat_loc }
            ; { name = "ppat_attributes"; value = ppat_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast ppat_desc) ~f:(fun ppat_desc ->
          Optional.bind (Versioned_value.to_location ppat_loc) ~f:(fun ppat_loc ->
            Optional.bind (Versioned_value.to_ast ppat_attributes) ~f:(fun ppat_attributes ->
              Some (Concrete.Pattern { ppat_desc; ppat_loc; ppat_attributes }))))
      | _ -> None
  end

  module Pattern_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Ppat_any
        | Ppat_var of { a : Versioned_ast.t }
        | Ppat_alias of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Ppat_constant of { a : Versioned_ast.t }
        | Ppat_interval of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Ppat_tuple of { a : Versioned_ast.t list }
        | Ppat_construct of { a : Versioned_ast.t; b : Versioned_ast.t option }
        | Ppat_variant of { a : Versioned_ast.t; b : Versioned_ast.t option }
        | Ppat_record of { a : Versioned_ast.t list; b : Versioned_ast.t }
        | Ppat_array of { a : Versioned_ast.t list }
        | Ppat_or of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Ppat_constraint of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Ppat_type of { a : Versioned_ast.t }
        | Ppat_lazy of { a : Versioned_ast.t }
        | Ppat_unpack of { a : Versioned_ast.t }
        | Ppat_exception of { a : Versioned_ast.t }
        | Ppat_extension of { a : Versioned_ast.t }
        | Ppat_open of { a : Versioned_ast.t; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Ppat_any ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_any"
          ; fields = []
          }
      | Ppat_var { a } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_var"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Ppat_alias { a; b } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_alias"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Ppat_constant { a } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_constant"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Ppat_interval { a; b } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_interval"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Ppat_tuple { a } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_tuple"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Ppat_construct { a; b } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_construct"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) b }
              ]
          }
      | Ppat_variant { a; b } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_variant"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) b }
              ]
          }
      | Ppat_record { a; b } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_record"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Ppat_array { a } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_array"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Ppat_or { a; b } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_or"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Ppat_constraint { a; b } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_constraint"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Ppat_type { a } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_type"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Ppat_lazy { a } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_lazy"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Ppat_unpack { a } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_unpack"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Ppat_exception { a } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_exception"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Ppat_extension { a } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_extension"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Ppat_open { a; b } ->
        Versioned_ast.create ~version
          { kind = "Pattern_desc"
          ; clause = "Ppat_open"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Pattern_desc"
        ; clause = "Ppat_any"
        ; fields = []
        } ->
        Some (Concrete.Ppat_any)
      | { kind = "Pattern_desc"
        ; clause = "Ppat_var"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Ppat_var { a }))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_alias"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Ppat_alias { a; b })))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_constant"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Ppat_constant { a }))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_interval"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Ppat_interval { a; b })))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_tuple"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Ppat_tuple { a }))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_construct"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Ppat_construct { a; b })))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_variant"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Ppat_variant { a; b })))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_record"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Ppat_record { a; b })))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_array"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Ppat_array { a }))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_or"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Ppat_or { a; b })))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_constraint"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Ppat_constraint { a; b })))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_type"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Ppat_type { a }))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_lazy"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Ppat_lazy { a }))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_unpack"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Ppat_unpack { a }))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_exception"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Ppat_exception { a }))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_extension"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Ppat_extension { a }))
      | { kind = "Pattern_desc"
        ; clause = "Ppat_open"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Ppat_open { a; b })))
      | _ -> None
  end

  module Record_field_pattern = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Record_field_pattern of { a : Versioned_ast.t; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Record_field_pattern { a; b } ->
        Versioned_ast.create ~version
          { kind = "Record_field_pattern"
          ; clause = "Record_field_pattern"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Record_field_pattern"
        ; clause = "Record_field_pattern"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Record_field_pattern { a; b })))
      | _ -> None
  end

  module Expression = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Expression of { pexp_desc : Versioned_ast.t; pexp_loc : Location.t; pexp_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Expression { pexp_desc; pexp_loc; pexp_attributes } ->
        Versioned_ast.create ~version
          { kind = "Expression"
          ; clause = "Expression"
          ; fields =
              [ { name = "pexp_desc"; value = Versioned_value.of_ast pexp_desc }
              ; { name = "pexp_loc"; value = Versioned_value.of_location pexp_loc }
              ; { name = "pexp_attributes"; value = Versioned_value.of_ast pexp_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Expression"
        ; clause = "Expression"
        ; fields =
            [ { name = "pexp_desc"; value = pexp_desc }
            ; { name = "pexp_loc"; value = pexp_loc }
            ; { name = "pexp_attributes"; value = pexp_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pexp_desc) ~f:(fun pexp_desc ->
          Optional.bind (Versioned_value.to_location pexp_loc) ~f:(fun pexp_loc ->
            Optional.bind (Versioned_value.to_ast pexp_attributes) ~f:(fun pexp_attributes ->
              Some (Concrete.Expression { pexp_desc; pexp_loc; pexp_attributes }))))
      | _ -> None
  end

  module Expression_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pexp_ident of { a : Versioned_ast.t }
        | Pexp_constant of { a : Versioned_ast.t }
        | Pexp_let of { a : Versioned_ast.t; b : Versioned_ast.t list; c : Versioned_ast.t }
        | Pexp_function of { a : Versioned_ast.t list }
        | Pexp_fun of { a : Versioned_ast.t; b : Versioned_ast.t option; c : Versioned_ast.t; d : Versioned_ast.t }
        | Pexp_apply of { a : Versioned_ast.t; b : Versioned_ast.t list }
        | Pexp_match of { a : Versioned_ast.t; b : Versioned_ast.t list }
        | Pexp_try of { a : Versioned_ast.t; b : Versioned_ast.t list }
        | Pexp_tuple of { a : Versioned_ast.t list }
        | Pexp_construct of { a : Versioned_ast.t; b : Versioned_ast.t option }
        | Pexp_variant of { a : Versioned_ast.t; b : Versioned_ast.t option }
        | Pexp_record of { a : Versioned_ast.t list; b : Versioned_ast.t option }
        | Pexp_field of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Pexp_setfield of { a : Versioned_ast.t; b : Versioned_ast.t; c : Versioned_ast.t }
        | Pexp_array of { a : Versioned_ast.t list }
        | Pexp_ifthenelse of { a : Versioned_ast.t; b : Versioned_ast.t; c : Versioned_ast.t option }
        | Pexp_sequence of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Pexp_while of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Pexp_for of { a : Versioned_ast.t; b : Versioned_ast.t; c : Versioned_ast.t; d : Versioned_ast.t; e : Versioned_ast.t }
        | Pexp_constraint of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Pexp_coerce of { a : Versioned_ast.t; b : Versioned_ast.t option; c : Versioned_ast.t }
        | Pexp_send of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Pexp_new of { a : Versioned_ast.t }
        | Pexp_setinstvar of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Pexp_override of { a : Versioned_ast.t list }
        | Pexp_letmodule of { a : Versioned_ast.t; b : Versioned_ast.t; c : Versioned_ast.t }
        | Pexp_letexception of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Pexp_assert of { a : Versioned_ast.t }
        | Pexp_lazy of { a : Versioned_ast.t }
        | Pexp_poly of { a : Versioned_ast.t; b : Versioned_ast.t option }
        | Pexp_object of { a : Versioned_ast.t }
        | Pexp_newtype of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Pexp_pack of { a : Versioned_ast.t }
        | Pexp_open of { a : Versioned_ast.t; b : Versioned_ast.t; c : Versioned_ast.t }
        | Pexp_extension of { a : Versioned_ast.t }
        | Pexp_unreachable
    end

    let of_concrete : Concrete.t -> t = function
      | Pexp_ident { a } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_ident"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pexp_constant { a } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_constant"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pexp_let { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_let"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ]
          }
      | Pexp_function { a } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_function"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Pexp_fun { a; b; c; d } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_fun"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ; { name = "d"; value = Versioned_value.of_ast d }
              ]
          }
      | Pexp_apply { a; b } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_apply"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) b }
              ]
          }
      | Pexp_match { a; b } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_match"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) b }
              ]
          }
      | Pexp_try { a; b } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_try"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) b }
              ]
          }
      | Pexp_tuple { a } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_tuple"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Pexp_construct { a; b } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_construct"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) b }
              ]
          }
      | Pexp_variant { a; b } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_variant"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) b }
              ]
          }
      | Pexp_record { a; b } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_record"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) b }
              ]
          }
      | Pexp_field { a; b } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_field"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Pexp_setfield { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_setfield"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ]
          }
      | Pexp_array { a } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_array"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Pexp_ifthenelse { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_ifthenelse"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) c }
              ]
          }
      | Pexp_sequence { a; b } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_sequence"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Pexp_while { a; b } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_while"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Pexp_for { a; b; c; d; e } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_for"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ; { name = "d"; value = Versioned_value.of_ast d }
              ; { name = "e"; value = Versioned_value.of_ast e }
              ]
          }
      | Pexp_constraint { a; b } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_constraint"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Pexp_coerce { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_coerce"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ]
          }
      | Pexp_send { a; b } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_send"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Pexp_new { a } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_new"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pexp_setinstvar { a; b } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_setinstvar"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Pexp_override { a } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_override"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Pexp_letmodule { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_letmodule"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ]
          }
      | Pexp_letexception { a; b } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_letexception"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Pexp_assert { a } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_assert"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pexp_lazy { a } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_lazy"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pexp_poly { a; b } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_poly"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) b }
              ]
          }
      | Pexp_object { a } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_object"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pexp_newtype { a; b } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_newtype"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Pexp_pack { a } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_pack"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pexp_open { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_open"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ]
          }
      | Pexp_extension { a } ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_extension"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pexp_unreachable ->
        Versioned_ast.create ~version
          { kind = "Expression_desc"
          ; clause = "Pexp_unreachable"
          ; fields = []
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Expression_desc"
        ; clause = "Pexp_ident"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pexp_ident { a }))
      | { kind = "Expression_desc"
        ; clause = "Pexp_constant"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pexp_constant { a }))
      | { kind = "Expression_desc"
        ; clause = "Pexp_let"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Some (Concrete.Pexp_let { a; b; c }))))
      | { kind = "Expression_desc"
        ; clause = "Pexp_function"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Pexp_function { a }))
      | { kind = "Expression_desc"
        ; clause = "Pexp_fun"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ; { name = "d"; value = d }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Optional.bind (Versioned_value.to_ast d) ~f:(fun d ->
                Some (Concrete.Pexp_fun { a; b; c; d })))))
      | { kind = "Expression_desc"
        ; clause = "Pexp_apply"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Pexp_apply { a; b })))
      | { kind = "Expression_desc"
        ; clause = "Pexp_match"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Pexp_match { a; b })))
      | { kind = "Expression_desc"
        ; clause = "Pexp_try"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Pexp_try { a; b })))
      | { kind = "Expression_desc"
        ; clause = "Pexp_tuple"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Pexp_tuple { a }))
      | { kind = "Expression_desc"
        ; clause = "Pexp_construct"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Pexp_construct { a; b })))
      | { kind = "Expression_desc"
        ; clause = "Pexp_variant"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Pexp_variant { a; b })))
      | { kind = "Expression_desc"
        ; clause = "Pexp_record"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Pexp_record { a; b })))
      | { kind = "Expression_desc"
        ; clause = "Pexp_field"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pexp_field { a; b })))
      | { kind = "Expression_desc"
        ; clause = "Pexp_setfield"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Some (Concrete.Pexp_setfield { a; b; c }))))
      | { kind = "Expression_desc"
        ; clause = "Pexp_array"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Pexp_array { a }))
      | { kind = "Expression_desc"
        ; clause = "Pexp_ifthenelse"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) c) ~f:(fun c ->
              Some (Concrete.Pexp_ifthenelse { a; b; c }))))
      | { kind = "Expression_desc"
        ; clause = "Pexp_sequence"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pexp_sequence { a; b })))
      | { kind = "Expression_desc"
        ; clause = "Pexp_while"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pexp_while { a; b })))
      | { kind = "Expression_desc"
        ; clause = "Pexp_for"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ; { name = "d"; value = d }
            ; { name = "e"; value = e }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Optional.bind (Versioned_value.to_ast d) ~f:(fun d ->
                Optional.bind (Versioned_value.to_ast e) ~f:(fun e ->
                  Some (Concrete.Pexp_for { a; b; c; d; e }))))))
      | { kind = "Expression_desc"
        ; clause = "Pexp_constraint"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pexp_constraint { a; b })))
      | { kind = "Expression_desc"
        ; clause = "Pexp_coerce"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Some (Concrete.Pexp_coerce { a; b; c }))))
      | { kind = "Expression_desc"
        ; clause = "Pexp_send"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pexp_send { a; b })))
      | { kind = "Expression_desc"
        ; clause = "Pexp_new"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pexp_new { a }))
      | { kind = "Expression_desc"
        ; clause = "Pexp_setinstvar"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pexp_setinstvar { a; b })))
      | { kind = "Expression_desc"
        ; clause = "Pexp_override"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Pexp_override { a }))
      | { kind = "Expression_desc"
        ; clause = "Pexp_letmodule"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Some (Concrete.Pexp_letmodule { a; b; c }))))
      | { kind = "Expression_desc"
        ; clause = "Pexp_letexception"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pexp_letexception { a; b })))
      | { kind = "Expression_desc"
        ; clause = "Pexp_assert"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pexp_assert { a }))
      | { kind = "Expression_desc"
        ; clause = "Pexp_lazy"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pexp_lazy { a }))
      | { kind = "Expression_desc"
        ; clause = "Pexp_poly"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Pexp_poly { a; b })))
      | { kind = "Expression_desc"
        ; clause = "Pexp_object"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pexp_object { a }))
      | { kind = "Expression_desc"
        ; clause = "Pexp_newtype"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pexp_newtype { a; b })))
      | { kind = "Expression_desc"
        ; clause = "Pexp_pack"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pexp_pack { a }))
      | { kind = "Expression_desc"
        ; clause = "Pexp_open"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Some (Concrete.Pexp_open { a; b; c }))))
      | { kind = "Expression_desc"
        ; clause = "Pexp_extension"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pexp_extension { a }))
      | { kind = "Expression_desc"
        ; clause = "Pexp_unreachable"
        ; fields = []
        } ->
        Some (Concrete.Pexp_unreachable)
      | _ -> None
  end

  module Override_expression = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Override_expression of { a : Versioned_ast.t; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Override_expression { a; b } ->
        Versioned_ast.create ~version
          { kind = "Override_expression"
          ; clause = "Override_expression"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Override_expression"
        ; clause = "Override_expression"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Override_expression { a; b })))
      | _ -> None
  end

  module Record_field_expression = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Record_field_expression of { a : Versioned_ast.t; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Record_field_expression { a; b } ->
        Versioned_ast.create ~version
          { kind = "Record_field_expression"
          ; clause = "Record_field_expression"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Record_field_expression"
        ; clause = "Record_field_expression"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Record_field_expression { a; b })))
      | _ -> None
  end

  module Apply_arg = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Apply_arg of { a : Versioned_ast.t; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Apply_arg { a; b } ->
        Versioned_ast.create ~version
          { kind = "Apply_arg"
          ; clause = "Apply_arg"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Apply_arg"
        ; clause = "Apply_arg"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Apply_arg { a; b })))
      | _ -> None
  end

  module Case = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Case of { pc_lhs : Versioned_ast.t; pc_guard : Versioned_ast.t option; pc_rhs : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Case { pc_lhs; pc_guard; pc_rhs } ->
        Versioned_ast.create ~version
          { kind = "Case"
          ; clause = "Case"
          ; fields =
              [ { name = "pc_lhs"; value = Versioned_value.of_ast pc_lhs }
              ; { name = "pc_guard"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) pc_guard }
              ; { name = "pc_rhs"; value = Versioned_value.of_ast pc_rhs }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Case"
        ; clause = "Case"
        ; fields =
            [ { name = "pc_lhs"; value = pc_lhs }
            ; { name = "pc_guard"; value = pc_guard }
            ; { name = "pc_rhs"; value = pc_rhs }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pc_lhs) ~f:(fun pc_lhs ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) pc_guard) ~f:(fun pc_guard ->
            Optional.bind (Versioned_value.to_ast pc_rhs) ~f:(fun pc_rhs ->
              Some (Concrete.Case { pc_lhs; pc_guard; pc_rhs }))))
      | _ -> None
  end

  module Value_description = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Value_description of { pval_name : Versioned_ast.t; pval_type : Versioned_ast.t; pval_prim : string list; pval_attributes : Versioned_ast.t; pval_loc : Location.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Value_description { pval_name; pval_type; pval_prim; pval_attributes; pval_loc } ->
        Versioned_ast.create ~version
          { kind = "Value_description"
          ; clause = "Value_description"
          ; fields =
              [ { name = "pval_name"; value = Versioned_value.of_ast pval_name }
              ; { name = "pval_type"; value = Versioned_value.of_ast pval_type }
              ; { name = "pval_prim"; value = (Versioned_value.of_list ~f:Versioned_value.of_string) pval_prim }
              ; { name = "pval_attributes"; value = Versioned_value.of_ast pval_attributes }
              ; { name = "pval_loc"; value = Versioned_value.of_location pval_loc }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Value_description"
        ; clause = "Value_description"
        ; fields =
            [ { name = "pval_name"; value = pval_name }
            ; { name = "pval_type"; value = pval_type }
            ; { name = "pval_prim"; value = pval_prim }
            ; { name = "pval_attributes"; value = pval_attributes }
            ; { name = "pval_loc"; value = pval_loc }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pval_name) ~f:(fun pval_name ->
          Optional.bind (Versioned_value.to_ast pval_type) ~f:(fun pval_type ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_string) pval_prim) ~f:(fun pval_prim ->
              Optional.bind (Versioned_value.to_ast pval_attributes) ~f:(fun pval_attributes ->
                Optional.bind (Versioned_value.to_location pval_loc) ~f:(fun pval_loc ->
                  Some (Concrete.Value_description { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }))))))
      | _ -> None
  end

  module Type_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Type_declaration of { ptype_name : Versioned_ast.t; ptype_params : Versioned_ast.t list; ptype_cstrs : Versioned_ast.t list; ptype_kind : Versioned_ast.t; ptype_private : Versioned_ast.t; ptype_manifest : Versioned_ast.t option; ptype_attributes : Versioned_ast.t; ptype_loc : Location.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Type_declaration { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc } ->
        Versioned_ast.create ~version
          { kind = "Type_declaration"
          ; clause = "Type_declaration"
          ; fields =
              [ { name = "ptype_name"; value = Versioned_value.of_ast ptype_name }
              ; { name = "ptype_params"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) ptype_params }
              ; { name = "ptype_cstrs"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) ptype_cstrs }
              ; { name = "ptype_kind"; value = Versioned_value.of_ast ptype_kind }
              ; { name = "ptype_private"; value = Versioned_value.of_ast ptype_private }
              ; { name = "ptype_manifest"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) ptype_manifest }
              ; { name = "ptype_attributes"; value = Versioned_value.of_ast ptype_attributes }
              ; { name = "ptype_loc"; value = Versioned_value.of_location ptype_loc }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Type_declaration"
        ; clause = "Type_declaration"
        ; fields =
            [ { name = "ptype_name"; value = ptype_name }
            ; { name = "ptype_params"; value = ptype_params }
            ; { name = "ptype_cstrs"; value = ptype_cstrs }
            ; { name = "ptype_kind"; value = ptype_kind }
            ; { name = "ptype_private"; value = ptype_private }
            ; { name = "ptype_manifest"; value = ptype_manifest }
            ; { name = "ptype_attributes"; value = ptype_attributes }
            ; { name = "ptype_loc"; value = ptype_loc }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast ptype_name) ~f:(fun ptype_name ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) ptype_params) ~f:(fun ptype_params ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) ptype_cstrs) ~f:(fun ptype_cstrs ->
              Optional.bind (Versioned_value.to_ast ptype_kind) ~f:(fun ptype_kind ->
                Optional.bind (Versioned_value.to_ast ptype_private) ~f:(fun ptype_private ->
                  Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) ptype_manifest) ~f:(fun ptype_manifest ->
                    Optional.bind (Versioned_value.to_ast ptype_attributes) ~f:(fun ptype_attributes ->
                      Optional.bind (Versioned_value.to_location ptype_loc) ~f:(fun ptype_loc ->
                        Some (Concrete.Type_declaration { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private; ptype_manifest; ptype_attributes; ptype_loc })))))))))
      | _ -> None
  end

  module Type_param = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Type_param of { a : Versioned_ast.t; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Type_param { a; b } ->
        Versioned_ast.create ~version
          { kind = "Type_param"
          ; clause = "Type_param"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Type_param"
        ; clause = "Type_param"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Type_param { a; b })))
      | _ -> None
  end

  module Type_constraint = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Type_constraint of { a : Versioned_ast.t; b : Versioned_ast.t; c : Location.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Type_constraint { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Type_constraint"
          ; clause = "Type_constraint"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = Versioned_value.of_location c }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Type_constraint"
        ; clause = "Type_constraint"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_location c) ~f:(fun c ->
              Some (Concrete.Type_constraint { a; b; c }))))
      | _ -> None
  end

  module Type_kind = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Ptype_abstract
        | Ptype_variant of { a : Versioned_ast.t list }
        | Ptype_record of { a : Versioned_ast.t list }
        | Ptype_open
    end

    let of_concrete : Concrete.t -> t = function
      | Ptype_abstract ->
        Versioned_ast.create ~version
          { kind = "Type_kind"
          ; clause = "Ptype_abstract"
          ; fields = []
          }
      | Ptype_variant { a } ->
        Versioned_ast.create ~version
          { kind = "Type_kind"
          ; clause = "Ptype_variant"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Ptype_record { a } ->
        Versioned_ast.create ~version
          { kind = "Type_kind"
          ; clause = "Ptype_record"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Ptype_open ->
        Versioned_ast.create ~version
          { kind = "Type_kind"
          ; clause = "Ptype_open"
          ; fields = []
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Type_kind"
        ; clause = "Ptype_abstract"
        ; fields = []
        } ->
        Some (Concrete.Ptype_abstract)
      | { kind = "Type_kind"
        ; clause = "Ptype_variant"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Ptype_variant { a }))
      | { kind = "Type_kind"
        ; clause = "Ptype_record"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Ptype_record { a }))
      | { kind = "Type_kind"
        ; clause = "Ptype_open"
        ; fields = []
        } ->
        Some (Concrete.Ptype_open)
      | _ -> None
  end

  module Label_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Label_declaration of { pld_name : Versioned_ast.t; pld_mutable : Versioned_ast.t; pld_type : Versioned_ast.t; pld_loc : Location.t; pld_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Label_declaration { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes } ->
        Versioned_ast.create ~version
          { kind = "Label_declaration"
          ; clause = "Label_declaration"
          ; fields =
              [ { name = "pld_name"; value = Versioned_value.of_ast pld_name }
              ; { name = "pld_mutable"; value = Versioned_value.of_ast pld_mutable }
              ; { name = "pld_type"; value = Versioned_value.of_ast pld_type }
              ; { name = "pld_loc"; value = Versioned_value.of_location pld_loc }
              ; { name = "pld_attributes"; value = Versioned_value.of_ast pld_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Label_declaration"
        ; clause = "Label_declaration"
        ; fields =
            [ { name = "pld_name"; value = pld_name }
            ; { name = "pld_mutable"; value = pld_mutable }
            ; { name = "pld_type"; value = pld_type }
            ; { name = "pld_loc"; value = pld_loc }
            ; { name = "pld_attributes"; value = pld_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pld_name) ~f:(fun pld_name ->
          Optional.bind (Versioned_value.to_ast pld_mutable) ~f:(fun pld_mutable ->
            Optional.bind (Versioned_value.to_ast pld_type) ~f:(fun pld_type ->
              Optional.bind (Versioned_value.to_location pld_loc) ~f:(fun pld_loc ->
                Optional.bind (Versioned_value.to_ast pld_attributes) ~f:(fun pld_attributes ->
                  Some (Concrete.Label_declaration { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }))))))
      | _ -> None
  end

  module Constructor_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Constructor_declaration of { pcd_name : Versioned_ast.t; pcd_args : Versioned_ast.t; pcd_res : Versioned_ast.t option; pcd_loc : Location.t; pcd_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Constructor_declaration { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes } ->
        Versioned_ast.create ~version
          { kind = "Constructor_declaration"
          ; clause = "Constructor_declaration"
          ; fields =
              [ { name = "pcd_name"; value = Versioned_value.of_ast pcd_name }
              ; { name = "pcd_args"; value = Versioned_value.of_ast pcd_args }
              ; { name = "pcd_res"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) pcd_res }
              ; { name = "pcd_loc"; value = Versioned_value.of_location pcd_loc }
              ; { name = "pcd_attributes"; value = Versioned_value.of_ast pcd_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Constructor_declaration"
        ; clause = "Constructor_declaration"
        ; fields =
            [ { name = "pcd_name"; value = pcd_name }
            ; { name = "pcd_args"; value = pcd_args }
            ; { name = "pcd_res"; value = pcd_res }
            ; { name = "pcd_loc"; value = pcd_loc }
            ; { name = "pcd_attributes"; value = pcd_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pcd_name) ~f:(fun pcd_name ->
          Optional.bind (Versioned_value.to_ast pcd_args) ~f:(fun pcd_args ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) pcd_res) ~f:(fun pcd_res ->
              Optional.bind (Versioned_value.to_location pcd_loc) ~f:(fun pcd_loc ->
                Optional.bind (Versioned_value.to_ast pcd_attributes) ~f:(fun pcd_attributes ->
                  Some (Concrete.Constructor_declaration { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }))))))
      | _ -> None
  end

  module Constructor_arguments = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pcstr_tuple of { a : Versioned_ast.t list }
        | Pcstr_record of { a : Versioned_ast.t list }
    end

    let of_concrete : Concrete.t -> t = function
      | Pcstr_tuple { a } ->
        Versioned_ast.create ~version
          { kind = "Constructor_arguments"
          ; clause = "Pcstr_tuple"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Pcstr_record { a } ->
        Versioned_ast.create ~version
          { kind = "Constructor_arguments"
          ; clause = "Pcstr_record"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Constructor_arguments"
        ; clause = "Pcstr_tuple"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Pcstr_tuple { a }))
      | { kind = "Constructor_arguments"
        ; clause = "Pcstr_record"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Pcstr_record { a }))
      | _ -> None
  end

  module Type_extension = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Type_extension of { ptyext_path : Versioned_ast.t; ptyext_params : Versioned_ast.t list; ptyext_constructors : Versioned_ast.t list; ptyext_private : Versioned_ast.t; ptyext_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Type_extension { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes } ->
        Versioned_ast.create ~version
          { kind = "Type_extension"
          ; clause = "Type_extension"
          ; fields =
              [ { name = "ptyext_path"; value = Versioned_value.of_ast ptyext_path }
              ; { name = "ptyext_params"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) ptyext_params }
              ; { name = "ptyext_constructors"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) ptyext_constructors }
              ; { name = "ptyext_private"; value = Versioned_value.of_ast ptyext_private }
              ; { name = "ptyext_attributes"; value = Versioned_value.of_ast ptyext_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Type_extension"
        ; clause = "Type_extension"
        ; fields =
            [ { name = "ptyext_path"; value = ptyext_path }
            ; { name = "ptyext_params"; value = ptyext_params }
            ; { name = "ptyext_constructors"; value = ptyext_constructors }
            ; { name = "ptyext_private"; value = ptyext_private }
            ; { name = "ptyext_attributes"; value = ptyext_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast ptyext_path) ~f:(fun ptyext_path ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) ptyext_params) ~f:(fun ptyext_params ->
            Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) ptyext_constructors) ~f:(fun ptyext_constructors ->
              Optional.bind (Versioned_value.to_ast ptyext_private) ~f:(fun ptyext_private ->
                Optional.bind (Versioned_value.to_ast ptyext_attributes) ~f:(fun ptyext_attributes ->
                  Some (Concrete.Type_extension { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private; ptyext_attributes }))))))
      | _ -> None
  end

  module Extension_constructor = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Extension_constructor of { pext_name : Versioned_ast.t; pext_kind : Versioned_ast.t; pext_loc : Location.t; pext_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Extension_constructor { pext_name; pext_kind; pext_loc; pext_attributes } ->
        Versioned_ast.create ~version
          { kind = "Extension_constructor"
          ; clause = "Extension_constructor"
          ; fields =
              [ { name = "pext_name"; value = Versioned_value.of_ast pext_name }
              ; { name = "pext_kind"; value = Versioned_value.of_ast pext_kind }
              ; { name = "pext_loc"; value = Versioned_value.of_location pext_loc }
              ; { name = "pext_attributes"; value = Versioned_value.of_ast pext_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Extension_constructor"
        ; clause = "Extension_constructor"
        ; fields =
            [ { name = "pext_name"; value = pext_name }
            ; { name = "pext_kind"; value = pext_kind }
            ; { name = "pext_loc"; value = pext_loc }
            ; { name = "pext_attributes"; value = pext_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pext_name) ~f:(fun pext_name ->
          Optional.bind (Versioned_value.to_ast pext_kind) ~f:(fun pext_kind ->
            Optional.bind (Versioned_value.to_location pext_loc) ~f:(fun pext_loc ->
              Optional.bind (Versioned_value.to_ast pext_attributes) ~f:(fun pext_attributes ->
                Some (Concrete.Extension_constructor { pext_name; pext_kind; pext_loc; pext_attributes })))))
      | _ -> None
  end

  module Extension_constructor_kind = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pext_decl of { a : Versioned_ast.t; b : Versioned_ast.t option }
        | Pext_rebind of { a : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Pext_decl { a; b } ->
        Versioned_ast.create ~version
          { kind = "Extension_constructor_kind"
          ; clause = "Pext_decl"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) b }
              ]
          }
      | Pext_rebind { a } ->
        Versioned_ast.create ~version
          { kind = "Extension_constructor_kind"
          ; clause = "Pext_rebind"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Extension_constructor_kind"
        ; clause = "Pext_decl"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Pext_decl { a; b })))
      | { kind = "Extension_constructor_kind"
        ; clause = "Pext_rebind"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pext_rebind { a }))
      | _ -> None
  end

  module Class_type = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Class_type of { pcty_desc : Versioned_ast.t; pcty_loc : Location.t; pcty_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Class_type { pcty_desc; pcty_loc; pcty_attributes } ->
        Versioned_ast.create ~version
          { kind = "Class_type"
          ; clause = "Class_type"
          ; fields =
              [ { name = "pcty_desc"; value = Versioned_value.of_ast pcty_desc }
              ; { name = "pcty_loc"; value = Versioned_value.of_location pcty_loc }
              ; { name = "pcty_attributes"; value = Versioned_value.of_ast pcty_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_type"
        ; clause = "Class_type"
        ; fields =
            [ { name = "pcty_desc"; value = pcty_desc }
            ; { name = "pcty_loc"; value = pcty_loc }
            ; { name = "pcty_attributes"; value = pcty_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pcty_desc) ~f:(fun pcty_desc ->
          Optional.bind (Versioned_value.to_location pcty_loc) ~f:(fun pcty_loc ->
            Optional.bind (Versioned_value.to_ast pcty_attributes) ~f:(fun pcty_attributes ->
              Some (Concrete.Class_type { pcty_desc; pcty_loc; pcty_attributes }))))
      | _ -> None
  end

  module Class_type_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pcty_constr of { a : Versioned_ast.t; b : Versioned_ast.t list }
        | Pcty_signature of { a : Versioned_ast.t }
        | Pcty_arrow of { a : Versioned_ast.t; b : Versioned_ast.t; c : Versioned_ast.t }
        | Pcty_extension of { a : Versioned_ast.t }
        | Pcty_open of { a : Versioned_ast.t; b : Versioned_ast.t; c : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Pcty_constr { a; b } ->
        Versioned_ast.create ~version
          { kind = "Class_type_desc"
          ; clause = "Pcty_constr"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) b }
              ]
          }
      | Pcty_signature { a } ->
        Versioned_ast.create ~version
          { kind = "Class_type_desc"
          ; clause = "Pcty_signature"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pcty_arrow { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Class_type_desc"
          ; clause = "Pcty_arrow"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ]
          }
      | Pcty_extension { a } ->
        Versioned_ast.create ~version
          { kind = "Class_type_desc"
          ; clause = "Pcty_extension"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pcty_open { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Class_type_desc"
          ; clause = "Pcty_open"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_type_desc"
        ; clause = "Pcty_constr"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Pcty_constr { a; b })))
      | { kind = "Class_type_desc"
        ; clause = "Pcty_signature"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pcty_signature { a }))
      | { kind = "Class_type_desc"
        ; clause = "Pcty_arrow"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Some (Concrete.Pcty_arrow { a; b; c }))))
      | { kind = "Class_type_desc"
        ; clause = "Pcty_extension"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pcty_extension { a }))
      | { kind = "Class_type_desc"
        ; clause = "Pcty_open"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Some (Concrete.Pcty_open { a; b; c }))))
      | _ -> None
  end

  module Class_signature = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Class_signature of { pcsig_self : Versioned_ast.t; pcsig_fields : Versioned_ast.t list }
    end

    let of_concrete : Concrete.t -> t = function
      | Class_signature { pcsig_self; pcsig_fields } ->
        Versioned_ast.create ~version
          { kind = "Class_signature"
          ; clause = "Class_signature"
          ; fields =
              [ { name = "pcsig_self"; value = Versioned_value.of_ast pcsig_self }
              ; { name = "pcsig_fields"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) pcsig_fields }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_signature"
        ; clause = "Class_signature"
        ; fields =
            [ { name = "pcsig_self"; value = pcsig_self }
            ; { name = "pcsig_fields"; value = pcsig_fields }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pcsig_self) ~f:(fun pcsig_self ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) pcsig_fields) ~f:(fun pcsig_fields ->
            Some (Concrete.Class_signature { pcsig_self; pcsig_fields })))
      | _ -> None
  end

  module Class_type_field = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Class_type_field of { pctf_desc : Versioned_ast.t; pctf_loc : Location.t; pctf_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Class_type_field { pctf_desc; pctf_loc; pctf_attributes } ->
        Versioned_ast.create ~version
          { kind = "Class_type_field"
          ; clause = "Class_type_field"
          ; fields =
              [ { name = "pctf_desc"; value = Versioned_value.of_ast pctf_desc }
              ; { name = "pctf_loc"; value = Versioned_value.of_location pctf_loc }
              ; { name = "pctf_attributes"; value = Versioned_value.of_ast pctf_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_type_field"
        ; clause = "Class_type_field"
        ; fields =
            [ { name = "pctf_desc"; value = pctf_desc }
            ; { name = "pctf_loc"; value = pctf_loc }
            ; { name = "pctf_attributes"; value = pctf_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pctf_desc) ~f:(fun pctf_desc ->
          Optional.bind (Versioned_value.to_location pctf_loc) ~f:(fun pctf_loc ->
            Optional.bind (Versioned_value.to_ast pctf_attributes) ~f:(fun pctf_attributes ->
              Some (Concrete.Class_type_field { pctf_desc; pctf_loc; pctf_attributes }))))
      | _ -> None
  end

  module Class_type_field_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pctf_inherit of { a : Versioned_ast.t }
        | Pctf_val of { a : Versioned_ast.t }
        | Pctf_method of { a : Versioned_ast.t }
        | Pctf_constraint of { a : Versioned_ast.t }
        | Pctf_attribute of { a : Versioned_ast.t }
        | Pctf_extension of { a : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Pctf_inherit { a } ->
        Versioned_ast.create ~version
          { kind = "Class_type_field_desc"
          ; clause = "Pctf_inherit"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pctf_val { a } ->
        Versioned_ast.create ~version
          { kind = "Class_type_field_desc"
          ; clause = "Pctf_val"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pctf_method { a } ->
        Versioned_ast.create ~version
          { kind = "Class_type_field_desc"
          ; clause = "Pctf_method"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pctf_constraint { a } ->
        Versioned_ast.create ~version
          { kind = "Class_type_field_desc"
          ; clause = "Pctf_constraint"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pctf_attribute { a } ->
        Versioned_ast.create ~version
          { kind = "Class_type_field_desc"
          ; clause = "Pctf_attribute"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pctf_extension { a } ->
        Versioned_ast.create ~version
          { kind = "Class_type_field_desc"
          ; clause = "Pctf_extension"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_type_field_desc"
        ; clause = "Pctf_inherit"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pctf_inherit { a }))
      | { kind = "Class_type_field_desc"
        ; clause = "Pctf_val"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pctf_val { a }))
      | { kind = "Class_type_field_desc"
        ; clause = "Pctf_method"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pctf_method { a }))
      | { kind = "Class_type_field_desc"
        ; clause = "Pctf_constraint"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pctf_constraint { a }))
      | { kind = "Class_type_field_desc"
        ; clause = "Pctf_attribute"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pctf_attribute { a }))
      | { kind = "Class_type_field_desc"
        ; clause = "Pctf_extension"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pctf_extension { a }))
      | _ -> None
  end

  module Class_type_value_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Class_type_value_desc of { a : Versioned_ast.t; b : Versioned_ast.t; c : Versioned_ast.t; d : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Class_type_value_desc { a; b; c; d } ->
        Versioned_ast.create ~version
          { kind = "Class_type_value_desc"
          ; clause = "Class_type_value_desc"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ; { name = "d"; value = Versioned_value.of_ast d }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_type_value_desc"
        ; clause = "Class_type_value_desc"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ; { name = "d"; value = d }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Optional.bind (Versioned_value.to_ast d) ~f:(fun d ->
                Some (Concrete.Class_type_value_desc { a; b; c; d })))))
      | _ -> None
  end

  module Class_type_method_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Class_type_method_desc of { a : Versioned_ast.t; b : Versioned_ast.t; c : Versioned_ast.t; d : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Class_type_method_desc { a; b; c; d } ->
        Versioned_ast.create ~version
          { kind = "Class_type_method_desc"
          ; clause = "Class_type_method_desc"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ; { name = "d"; value = Versioned_value.of_ast d }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_type_method_desc"
        ; clause = "Class_type_method_desc"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ; { name = "d"; value = d }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Optional.bind (Versioned_value.to_ast d) ~f:(fun d ->
                Some (Concrete.Class_type_method_desc { a; b; c; d })))))
      | _ -> None
  end

  module Class_type_constraint = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Class_type_constraint of { a : Versioned_ast.t; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Class_type_constraint { a; b } ->
        Versioned_ast.create ~version
          { kind = "Class_type_constraint"
          ; clause = "Class_type_constraint"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_type_constraint"
        ; clause = "Class_type_constraint"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Class_type_constraint { a; b })))
      | _ -> None
  end

  module Class_description = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Class_description of { pci_virt : Versioned_ast.t; pci_params : Versioned_ast.t list; pci_name : Versioned_ast.t; pci_expr : Versioned_ast.t; pci_loc : Location.t; pci_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Class_description { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } ->
        Versioned_ast.create ~version
          { kind = "Class_description"
          ; clause = "Class_description"
          ; fields =
              [ { name = "pci_virt"; value = Versioned_value.of_ast pci_virt }
              ; { name = "pci_params"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) pci_params }
              ; { name = "pci_name"; value = Versioned_value.of_ast pci_name }
              ; { name = "pci_expr"; value = Versioned_value.of_ast pci_expr }
              ; { name = "pci_loc"; value = Versioned_value.of_location pci_loc }
              ; { name = "pci_attributes"; value = Versioned_value.of_ast pci_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_description"
        ; clause = "Class_description"
        ; fields =
            [ { name = "pci_virt"; value = pci_virt }
            ; { name = "pci_params"; value = pci_params }
            ; { name = "pci_name"; value = pci_name }
            ; { name = "pci_expr"; value = pci_expr }
            ; { name = "pci_loc"; value = pci_loc }
            ; { name = "pci_attributes"; value = pci_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pci_virt) ~f:(fun pci_virt ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) pci_params) ~f:(fun pci_params ->
            Optional.bind (Versioned_value.to_ast pci_name) ~f:(fun pci_name ->
              Optional.bind (Versioned_value.to_ast pci_expr) ~f:(fun pci_expr ->
                Optional.bind (Versioned_value.to_location pci_loc) ~f:(fun pci_loc ->
                  Optional.bind (Versioned_value.to_ast pci_attributes) ~f:(fun pci_attributes ->
                    Some (Concrete.Class_description { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes })))))))
      | _ -> None
  end

  module Class_type_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Class_type_declaration of { pci_virt : Versioned_ast.t; pci_params : Versioned_ast.t list; pci_name : Versioned_ast.t; pci_expr : Versioned_ast.t; pci_loc : Location.t; pci_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Class_type_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } ->
        Versioned_ast.create ~version
          { kind = "Class_type_declaration"
          ; clause = "Class_type_declaration"
          ; fields =
              [ { name = "pci_virt"; value = Versioned_value.of_ast pci_virt }
              ; { name = "pci_params"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) pci_params }
              ; { name = "pci_name"; value = Versioned_value.of_ast pci_name }
              ; { name = "pci_expr"; value = Versioned_value.of_ast pci_expr }
              ; { name = "pci_loc"; value = Versioned_value.of_location pci_loc }
              ; { name = "pci_attributes"; value = Versioned_value.of_ast pci_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_type_declaration"
        ; clause = "Class_type_declaration"
        ; fields =
            [ { name = "pci_virt"; value = pci_virt }
            ; { name = "pci_params"; value = pci_params }
            ; { name = "pci_name"; value = pci_name }
            ; { name = "pci_expr"; value = pci_expr }
            ; { name = "pci_loc"; value = pci_loc }
            ; { name = "pci_attributes"; value = pci_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pci_virt) ~f:(fun pci_virt ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) pci_params) ~f:(fun pci_params ->
            Optional.bind (Versioned_value.to_ast pci_name) ~f:(fun pci_name ->
              Optional.bind (Versioned_value.to_ast pci_expr) ~f:(fun pci_expr ->
                Optional.bind (Versioned_value.to_location pci_loc) ~f:(fun pci_loc ->
                  Optional.bind (Versioned_value.to_ast pci_attributes) ~f:(fun pci_attributes ->
                    Some (Concrete.Class_type_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes })))))))
      | _ -> None
  end

  module Class_expr = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Class_expr of { pcl_desc : Versioned_ast.t; pcl_loc : Location.t; pcl_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Class_expr { pcl_desc; pcl_loc; pcl_attributes } ->
        Versioned_ast.create ~version
          { kind = "Class_expr"
          ; clause = "Class_expr"
          ; fields =
              [ { name = "pcl_desc"; value = Versioned_value.of_ast pcl_desc }
              ; { name = "pcl_loc"; value = Versioned_value.of_location pcl_loc }
              ; { name = "pcl_attributes"; value = Versioned_value.of_ast pcl_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_expr"
        ; clause = "Class_expr"
        ; fields =
            [ { name = "pcl_desc"; value = pcl_desc }
            ; { name = "pcl_loc"; value = pcl_loc }
            ; { name = "pcl_attributes"; value = pcl_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pcl_desc) ~f:(fun pcl_desc ->
          Optional.bind (Versioned_value.to_location pcl_loc) ~f:(fun pcl_loc ->
            Optional.bind (Versioned_value.to_ast pcl_attributes) ~f:(fun pcl_attributes ->
              Some (Concrete.Class_expr { pcl_desc; pcl_loc; pcl_attributes }))))
      | _ -> None
  end

  module Class_expr_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pcl_constr of { a : Versioned_ast.t; b : Versioned_ast.t list }
        | Pcl_structure of { a : Versioned_ast.t }
        | Pcl_fun of { a : Versioned_ast.t; b : Versioned_ast.t option; c : Versioned_ast.t; d : Versioned_ast.t }
        | Pcl_apply of { a : Versioned_ast.t; b : Versioned_ast.t list }
        | Pcl_let of { a : Versioned_ast.t; b : Versioned_ast.t list; c : Versioned_ast.t }
        | Pcl_constraint of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Pcl_extension of { a : Versioned_ast.t }
        | Pcl_open of { a : Versioned_ast.t; b : Versioned_ast.t; c : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Pcl_constr { a; b } ->
        Versioned_ast.create ~version
          { kind = "Class_expr_desc"
          ; clause = "Pcl_constr"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) b }
              ]
          }
      | Pcl_structure { a } ->
        Versioned_ast.create ~version
          { kind = "Class_expr_desc"
          ; clause = "Pcl_structure"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pcl_fun { a; b; c; d } ->
        Versioned_ast.create ~version
          { kind = "Class_expr_desc"
          ; clause = "Pcl_fun"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ; { name = "d"; value = Versioned_value.of_ast d }
              ]
          }
      | Pcl_apply { a; b } ->
        Versioned_ast.create ~version
          { kind = "Class_expr_desc"
          ; clause = "Pcl_apply"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) b }
              ]
          }
      | Pcl_let { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Class_expr_desc"
          ; clause = "Pcl_let"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ]
          }
      | Pcl_constraint { a; b } ->
        Versioned_ast.create ~version
          { kind = "Class_expr_desc"
          ; clause = "Pcl_constraint"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Pcl_extension { a } ->
        Versioned_ast.create ~version
          { kind = "Class_expr_desc"
          ; clause = "Pcl_extension"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pcl_open { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Class_expr_desc"
          ; clause = "Pcl_open"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_expr_desc"
        ; clause = "Pcl_constr"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Pcl_constr { a; b })))
      | { kind = "Class_expr_desc"
        ; clause = "Pcl_structure"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pcl_structure { a }))
      | { kind = "Class_expr_desc"
        ; clause = "Pcl_fun"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ; { name = "d"; value = d }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Optional.bind (Versioned_value.to_ast d) ~f:(fun d ->
                Some (Concrete.Pcl_fun { a; b; c; d })))))
      | { kind = "Class_expr_desc"
        ; clause = "Pcl_apply"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Pcl_apply { a; b })))
      | { kind = "Class_expr_desc"
        ; clause = "Pcl_let"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Some (Concrete.Pcl_let { a; b; c }))))
      | { kind = "Class_expr_desc"
        ; clause = "Pcl_constraint"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pcl_constraint { a; b })))
      | { kind = "Class_expr_desc"
        ; clause = "Pcl_extension"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pcl_extension { a }))
      | { kind = "Class_expr_desc"
        ; clause = "Pcl_open"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Some (Concrete.Pcl_open { a; b; c }))))
      | _ -> None
  end

  module Class_structure = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Class_structure of { pcstr_self : Versioned_ast.t; pcstr_fields : Versioned_ast.t list }
    end

    let of_concrete : Concrete.t -> t = function
      | Class_structure { pcstr_self; pcstr_fields } ->
        Versioned_ast.create ~version
          { kind = "Class_structure"
          ; clause = "Class_structure"
          ; fields =
              [ { name = "pcstr_self"; value = Versioned_value.of_ast pcstr_self }
              ; { name = "pcstr_fields"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) pcstr_fields }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_structure"
        ; clause = "Class_structure"
        ; fields =
            [ { name = "pcstr_self"; value = pcstr_self }
            ; { name = "pcstr_fields"; value = pcstr_fields }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pcstr_self) ~f:(fun pcstr_self ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) pcstr_fields) ~f:(fun pcstr_fields ->
            Some (Concrete.Class_structure { pcstr_self; pcstr_fields })))
      | _ -> None
  end

  module Class_field = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Class_field of { pcf_desc : Versioned_ast.t; pcf_loc : Location.t; pcf_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Class_field { pcf_desc; pcf_loc; pcf_attributes } ->
        Versioned_ast.create ~version
          { kind = "Class_field"
          ; clause = "Class_field"
          ; fields =
              [ { name = "pcf_desc"; value = Versioned_value.of_ast pcf_desc }
              ; { name = "pcf_loc"; value = Versioned_value.of_location pcf_loc }
              ; { name = "pcf_attributes"; value = Versioned_value.of_ast pcf_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_field"
        ; clause = "Class_field"
        ; fields =
            [ { name = "pcf_desc"; value = pcf_desc }
            ; { name = "pcf_loc"; value = pcf_loc }
            ; { name = "pcf_attributes"; value = pcf_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pcf_desc) ~f:(fun pcf_desc ->
          Optional.bind (Versioned_value.to_location pcf_loc) ~f:(fun pcf_loc ->
            Optional.bind (Versioned_value.to_ast pcf_attributes) ~f:(fun pcf_attributes ->
              Some (Concrete.Class_field { pcf_desc; pcf_loc; pcf_attributes }))))
      | _ -> None
  end

  module Class_field_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pcf_inherit of { a : Versioned_ast.t; b : Versioned_ast.t; c : Versioned_ast.t option }
        | Pcf_val of { a : Versioned_ast.t }
        | Pcf_method of { a : Versioned_ast.t }
        | Pcf_constraint of { a : Versioned_ast.t }
        | Pcf_initializer of { a : Versioned_ast.t }
        | Pcf_attribute of { a : Versioned_ast.t }
        | Pcf_extension of { a : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Pcf_inherit { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Class_field_desc"
          ; clause = "Pcf_inherit"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) c }
              ]
          }
      | Pcf_val { a } ->
        Versioned_ast.create ~version
          { kind = "Class_field_desc"
          ; clause = "Pcf_val"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pcf_method { a } ->
        Versioned_ast.create ~version
          { kind = "Class_field_desc"
          ; clause = "Pcf_method"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pcf_constraint { a } ->
        Versioned_ast.create ~version
          { kind = "Class_field_desc"
          ; clause = "Pcf_constraint"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pcf_initializer { a } ->
        Versioned_ast.create ~version
          { kind = "Class_field_desc"
          ; clause = "Pcf_initializer"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pcf_attribute { a } ->
        Versioned_ast.create ~version
          { kind = "Class_field_desc"
          ; clause = "Pcf_attribute"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pcf_extension { a } ->
        Versioned_ast.create ~version
          { kind = "Class_field_desc"
          ; clause = "Pcf_extension"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_field_desc"
        ; clause = "Pcf_inherit"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) c) ~f:(fun c ->
              Some (Concrete.Pcf_inherit { a; b; c }))))
      | { kind = "Class_field_desc"
        ; clause = "Pcf_val"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pcf_val { a }))
      | { kind = "Class_field_desc"
        ; clause = "Pcf_method"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pcf_method { a }))
      | { kind = "Class_field_desc"
        ; clause = "Pcf_constraint"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pcf_constraint { a }))
      | { kind = "Class_field_desc"
        ; clause = "Pcf_initializer"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pcf_initializer { a }))
      | { kind = "Class_field_desc"
        ; clause = "Pcf_attribute"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pcf_attribute { a }))
      | { kind = "Class_field_desc"
        ; clause = "Pcf_extension"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pcf_extension { a }))
      | _ -> None
  end

  module Class_value_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Class_value_desc of { a : Versioned_ast.t; b : Versioned_ast.t; c : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Class_value_desc { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Class_value_desc"
          ; clause = "Class_value_desc"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_value_desc"
        ; clause = "Class_value_desc"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Some (Concrete.Class_value_desc { a; b; c }))))
      | _ -> None
  end

  module Class_method_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Class_method_desc of { a : Versioned_ast.t; b : Versioned_ast.t; c : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Class_method_desc { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Class_method_desc"
          ; clause = "Class_method_desc"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_method_desc"
        ; clause = "Class_method_desc"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Some (Concrete.Class_method_desc { a; b; c }))))
      | _ -> None
  end

  module Class_field_kind = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Cfk_virtual of { a : Versioned_ast.t }
        | Cfk_concrete of { a : Versioned_ast.t; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Cfk_virtual { a } ->
        Versioned_ast.create ~version
          { kind = "Class_field_kind"
          ; clause = "Cfk_virtual"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Cfk_concrete { a; b } ->
        Versioned_ast.create ~version
          { kind = "Class_field_kind"
          ; clause = "Cfk_concrete"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_field_kind"
        ; clause = "Cfk_virtual"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Cfk_virtual { a }))
      | { kind = "Class_field_kind"
        ; clause = "Cfk_concrete"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Cfk_concrete { a; b })))
      | _ -> None
  end

  module Class_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Class_declaration of { pci_virt : Versioned_ast.t; pci_params : Versioned_ast.t list; pci_name : Versioned_ast.t; pci_expr : Versioned_ast.t; pci_loc : Location.t; pci_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Class_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } ->
        Versioned_ast.create ~version
          { kind = "Class_declaration"
          ; clause = "Class_declaration"
          ; fields =
              [ { name = "pci_virt"; value = Versioned_value.of_ast pci_virt }
              ; { name = "pci_params"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) pci_params }
              ; { name = "pci_name"; value = Versioned_value.of_ast pci_name }
              ; { name = "pci_expr"; value = Versioned_value.of_ast pci_expr }
              ; { name = "pci_loc"; value = Versioned_value.of_location pci_loc }
              ; { name = "pci_attributes"; value = Versioned_value.of_ast pci_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Class_declaration"
        ; clause = "Class_declaration"
        ; fields =
            [ { name = "pci_virt"; value = pci_virt }
            ; { name = "pci_params"; value = pci_params }
            ; { name = "pci_name"; value = pci_name }
            ; { name = "pci_expr"; value = pci_expr }
            ; { name = "pci_loc"; value = pci_loc }
            ; { name = "pci_attributes"; value = pci_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pci_virt) ~f:(fun pci_virt ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) pci_params) ~f:(fun pci_params ->
            Optional.bind (Versioned_value.to_ast pci_name) ~f:(fun pci_name ->
              Optional.bind (Versioned_value.to_ast pci_expr) ~f:(fun pci_expr ->
                Optional.bind (Versioned_value.to_location pci_loc) ~f:(fun pci_loc ->
                  Optional.bind (Versioned_value.to_ast pci_attributes) ~f:(fun pci_attributes ->
                    Some (Concrete.Class_declaration { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes })))))))
      | _ -> None
  end

  module Module_type = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Module_type of { pmty_desc : Versioned_ast.t; pmty_loc : Location.t; pmty_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Module_type { pmty_desc; pmty_loc; pmty_attributes } ->
        Versioned_ast.create ~version
          { kind = "Module_type"
          ; clause = "Module_type"
          ; fields =
              [ { name = "pmty_desc"; value = Versioned_value.of_ast pmty_desc }
              ; { name = "pmty_loc"; value = Versioned_value.of_location pmty_loc }
              ; { name = "pmty_attributes"; value = Versioned_value.of_ast pmty_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Module_type"
        ; clause = "Module_type"
        ; fields =
            [ { name = "pmty_desc"; value = pmty_desc }
            ; { name = "pmty_loc"; value = pmty_loc }
            ; { name = "pmty_attributes"; value = pmty_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pmty_desc) ~f:(fun pmty_desc ->
          Optional.bind (Versioned_value.to_location pmty_loc) ~f:(fun pmty_loc ->
            Optional.bind (Versioned_value.to_ast pmty_attributes) ~f:(fun pmty_attributes ->
              Some (Concrete.Module_type { pmty_desc; pmty_loc; pmty_attributes }))))
      | _ -> None
  end

  module Module_type_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pmty_ident of { a : Versioned_ast.t }
        | Pmty_signature of { a : Versioned_ast.t }
        | Pmty_functor of { a : Versioned_ast.t; b : Versioned_ast.t option; c : Versioned_ast.t }
        | Pmty_with of { a : Versioned_ast.t; b : Versioned_ast.t list }
        | Pmty_typeof of { a : Versioned_ast.t }
        | Pmty_extension of { a : Versioned_ast.t }
        | Pmty_alias of { a : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Pmty_ident { a } ->
        Versioned_ast.create ~version
          { kind = "Module_type_desc"
          ; clause = "Pmty_ident"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pmty_signature { a } ->
        Versioned_ast.create ~version
          { kind = "Module_type_desc"
          ; clause = "Pmty_signature"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pmty_functor { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Module_type_desc"
          ; clause = "Pmty_functor"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ]
          }
      | Pmty_with { a; b } ->
        Versioned_ast.create ~version
          { kind = "Module_type_desc"
          ; clause = "Pmty_with"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) b }
              ]
          }
      | Pmty_typeof { a } ->
        Versioned_ast.create ~version
          { kind = "Module_type_desc"
          ; clause = "Pmty_typeof"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pmty_extension { a } ->
        Versioned_ast.create ~version
          { kind = "Module_type_desc"
          ; clause = "Pmty_extension"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pmty_alias { a } ->
        Versioned_ast.create ~version
          { kind = "Module_type_desc"
          ; clause = "Pmty_alias"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Module_type_desc"
        ; clause = "Pmty_ident"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pmty_ident { a }))
      | { kind = "Module_type_desc"
        ; clause = "Pmty_signature"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pmty_signature { a }))
      | { kind = "Module_type_desc"
        ; clause = "Pmty_functor"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Some (Concrete.Pmty_functor { a; b; c }))))
      | { kind = "Module_type_desc"
        ; clause = "Pmty_with"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Pmty_with { a; b })))
      | { kind = "Module_type_desc"
        ; clause = "Pmty_typeof"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pmty_typeof { a }))
      | { kind = "Module_type_desc"
        ; clause = "Pmty_extension"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pmty_extension { a }))
      | { kind = "Module_type_desc"
        ; clause = "Pmty_alias"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pmty_alias { a }))
      | _ -> None
  end

  module Signature = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Signature of { a : Versioned_ast.t list }
    end

    let of_concrete : Concrete.t -> t = function
      | Signature { a } ->
        Versioned_ast.create ~version
          { kind = "Signature"
          ; clause = "Signature"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Signature"
        ; clause = "Signature"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Signature { a }))
      | _ -> None
  end

  module Signature_item = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Signature_item of { psig_desc : Versioned_ast.t; psig_loc : Location.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Signature_item { psig_desc; psig_loc } ->
        Versioned_ast.create ~version
          { kind = "Signature_item"
          ; clause = "Signature_item"
          ; fields =
              [ { name = "psig_desc"; value = Versioned_value.of_ast psig_desc }
              ; { name = "psig_loc"; value = Versioned_value.of_location psig_loc }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Signature_item"
        ; clause = "Signature_item"
        ; fields =
            [ { name = "psig_desc"; value = psig_desc }
            ; { name = "psig_loc"; value = psig_loc }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast psig_desc) ~f:(fun psig_desc ->
          Optional.bind (Versioned_value.to_location psig_loc) ~f:(fun psig_loc ->
            Some (Concrete.Signature_item { psig_desc; psig_loc })))
      | _ -> None
  end

  module Signature_item_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Psig_value of { a : Versioned_ast.t }
        | Psig_type of { a : Versioned_ast.t; b : Versioned_ast.t list }
        | Psig_typext of { a : Versioned_ast.t }
        | Psig_exception of { a : Versioned_ast.t }
        | Psig_module of { a : Versioned_ast.t }
        | Psig_recmodule of { a : Versioned_ast.t list }
        | Psig_modtype of { a : Versioned_ast.t }
        | Psig_open of { a : Versioned_ast.t }
        | Psig_include of { a : Versioned_ast.t }
        | Psig_class of { a : Versioned_ast.t list }
        | Psig_class_type of { a : Versioned_ast.t list }
        | Psig_attribute of { a : Versioned_ast.t }
        | Psig_extension of { a : Versioned_ast.t; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Psig_value { a } ->
        Versioned_ast.create ~version
          { kind = "Signature_item_desc"
          ; clause = "Psig_value"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Psig_type { a; b } ->
        Versioned_ast.create ~version
          { kind = "Signature_item_desc"
          ; clause = "Psig_type"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) b }
              ]
          }
      | Psig_typext { a } ->
        Versioned_ast.create ~version
          { kind = "Signature_item_desc"
          ; clause = "Psig_typext"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Psig_exception { a } ->
        Versioned_ast.create ~version
          { kind = "Signature_item_desc"
          ; clause = "Psig_exception"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Psig_module { a } ->
        Versioned_ast.create ~version
          { kind = "Signature_item_desc"
          ; clause = "Psig_module"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Psig_recmodule { a } ->
        Versioned_ast.create ~version
          { kind = "Signature_item_desc"
          ; clause = "Psig_recmodule"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Psig_modtype { a } ->
        Versioned_ast.create ~version
          { kind = "Signature_item_desc"
          ; clause = "Psig_modtype"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Psig_open { a } ->
        Versioned_ast.create ~version
          { kind = "Signature_item_desc"
          ; clause = "Psig_open"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Psig_include { a } ->
        Versioned_ast.create ~version
          { kind = "Signature_item_desc"
          ; clause = "Psig_include"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Psig_class { a } ->
        Versioned_ast.create ~version
          { kind = "Signature_item_desc"
          ; clause = "Psig_class"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Psig_class_type { a } ->
        Versioned_ast.create ~version
          { kind = "Signature_item_desc"
          ; clause = "Psig_class_type"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Psig_attribute { a } ->
        Versioned_ast.create ~version
          { kind = "Signature_item_desc"
          ; clause = "Psig_attribute"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Psig_extension { a; b } ->
        Versioned_ast.create ~version
          { kind = "Signature_item_desc"
          ; clause = "Psig_extension"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Signature_item_desc"
        ; clause = "Psig_value"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Psig_value { a }))
      | { kind = "Signature_item_desc"
        ; clause = "Psig_type"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Psig_type { a; b })))
      | { kind = "Signature_item_desc"
        ; clause = "Psig_typext"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Psig_typext { a }))
      | { kind = "Signature_item_desc"
        ; clause = "Psig_exception"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Psig_exception { a }))
      | { kind = "Signature_item_desc"
        ; clause = "Psig_module"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Psig_module { a }))
      | { kind = "Signature_item_desc"
        ; clause = "Psig_recmodule"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Psig_recmodule { a }))
      | { kind = "Signature_item_desc"
        ; clause = "Psig_modtype"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Psig_modtype { a }))
      | { kind = "Signature_item_desc"
        ; clause = "Psig_open"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Psig_open { a }))
      | { kind = "Signature_item_desc"
        ; clause = "Psig_include"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Psig_include { a }))
      | { kind = "Signature_item_desc"
        ; clause = "Psig_class"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Psig_class { a }))
      | { kind = "Signature_item_desc"
        ; clause = "Psig_class_type"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Psig_class_type { a }))
      | { kind = "Signature_item_desc"
        ; clause = "Psig_attribute"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Psig_attribute { a }))
      | { kind = "Signature_item_desc"
        ; clause = "Psig_extension"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Psig_extension { a; b })))
      | _ -> None
  end

  module Module_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Module_declaration of { pmd_name : Versioned_ast.t; pmd_type : Versioned_ast.t; pmd_attributes : Versioned_ast.t; pmd_loc : Location.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Module_declaration { pmd_name; pmd_type; pmd_attributes; pmd_loc } ->
        Versioned_ast.create ~version
          { kind = "Module_declaration"
          ; clause = "Module_declaration"
          ; fields =
              [ { name = "pmd_name"; value = Versioned_value.of_ast pmd_name }
              ; { name = "pmd_type"; value = Versioned_value.of_ast pmd_type }
              ; { name = "pmd_attributes"; value = Versioned_value.of_ast pmd_attributes }
              ; { name = "pmd_loc"; value = Versioned_value.of_location pmd_loc }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Module_declaration"
        ; clause = "Module_declaration"
        ; fields =
            [ { name = "pmd_name"; value = pmd_name }
            ; { name = "pmd_type"; value = pmd_type }
            ; { name = "pmd_attributes"; value = pmd_attributes }
            ; { name = "pmd_loc"; value = pmd_loc }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pmd_name) ~f:(fun pmd_name ->
          Optional.bind (Versioned_value.to_ast pmd_type) ~f:(fun pmd_type ->
            Optional.bind (Versioned_value.to_ast pmd_attributes) ~f:(fun pmd_attributes ->
              Optional.bind (Versioned_value.to_location pmd_loc) ~f:(fun pmd_loc ->
                Some (Concrete.Module_declaration { pmd_name; pmd_type; pmd_attributes; pmd_loc })))))
      | _ -> None
  end

  module Module_type_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Module_type_declaration of { pmtd_name : Versioned_ast.t; pmtd_type : Versioned_ast.t option; pmtd_attributes : Versioned_ast.t; pmtd_loc : Location.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Module_type_declaration { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } ->
        Versioned_ast.create ~version
          { kind = "Module_type_declaration"
          ; clause = "Module_type_declaration"
          ; fields =
              [ { name = "pmtd_name"; value = Versioned_value.of_ast pmtd_name }
              ; { name = "pmtd_type"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) pmtd_type }
              ; { name = "pmtd_attributes"; value = Versioned_value.of_ast pmtd_attributes }
              ; { name = "pmtd_loc"; value = Versioned_value.of_location pmtd_loc }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Module_type_declaration"
        ; clause = "Module_type_declaration"
        ; fields =
            [ { name = "pmtd_name"; value = pmtd_name }
            ; { name = "pmtd_type"; value = pmtd_type }
            ; { name = "pmtd_attributes"; value = pmtd_attributes }
            ; { name = "pmtd_loc"; value = pmtd_loc }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pmtd_name) ~f:(fun pmtd_name ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) pmtd_type) ~f:(fun pmtd_type ->
            Optional.bind (Versioned_value.to_ast pmtd_attributes) ~f:(fun pmtd_attributes ->
              Optional.bind (Versioned_value.to_location pmtd_loc) ~f:(fun pmtd_loc ->
                Some (Concrete.Module_type_declaration { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc })))))
      | _ -> None
  end

  module Open_description = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Open_description of { popen_lid : Versioned_ast.t; popen_override : Versioned_ast.t; popen_loc : Location.t; popen_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Open_description { popen_lid; popen_override; popen_loc; popen_attributes } ->
        Versioned_ast.create ~version
          { kind = "Open_description"
          ; clause = "Open_description"
          ; fields =
              [ { name = "popen_lid"; value = Versioned_value.of_ast popen_lid }
              ; { name = "popen_override"; value = Versioned_value.of_ast popen_override }
              ; { name = "popen_loc"; value = Versioned_value.of_location popen_loc }
              ; { name = "popen_attributes"; value = Versioned_value.of_ast popen_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Open_description"
        ; clause = "Open_description"
        ; fields =
            [ { name = "popen_lid"; value = popen_lid }
            ; { name = "popen_override"; value = popen_override }
            ; { name = "popen_loc"; value = popen_loc }
            ; { name = "popen_attributes"; value = popen_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast popen_lid) ~f:(fun popen_lid ->
          Optional.bind (Versioned_value.to_ast popen_override) ~f:(fun popen_override ->
            Optional.bind (Versioned_value.to_location popen_loc) ~f:(fun popen_loc ->
              Optional.bind (Versioned_value.to_ast popen_attributes) ~f:(fun popen_attributes ->
                Some (Concrete.Open_description { popen_lid; popen_override; popen_loc; popen_attributes })))))
      | _ -> None
  end

  module Include_description = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Include_description of { pincl_mod : Versioned_ast.t; pincl_loc : Location.t; pincl_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Include_description { pincl_mod; pincl_loc; pincl_attributes } ->
        Versioned_ast.create ~version
          { kind = "Include_description"
          ; clause = "Include_description"
          ; fields =
              [ { name = "pincl_mod"; value = Versioned_value.of_ast pincl_mod }
              ; { name = "pincl_loc"; value = Versioned_value.of_location pincl_loc }
              ; { name = "pincl_attributes"; value = Versioned_value.of_ast pincl_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Include_description"
        ; clause = "Include_description"
        ; fields =
            [ { name = "pincl_mod"; value = pincl_mod }
            ; { name = "pincl_loc"; value = pincl_loc }
            ; { name = "pincl_attributes"; value = pincl_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pincl_mod) ~f:(fun pincl_mod ->
          Optional.bind (Versioned_value.to_location pincl_loc) ~f:(fun pincl_loc ->
            Optional.bind (Versioned_value.to_ast pincl_attributes) ~f:(fun pincl_attributes ->
              Some (Concrete.Include_description { pincl_mod; pincl_loc; pincl_attributes }))))
      | _ -> None
  end

  module Include_declaration = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Include_declaration of { pincl_mod : Versioned_ast.t; pincl_loc : Location.t; pincl_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Include_declaration { pincl_mod; pincl_loc; pincl_attributes } ->
        Versioned_ast.create ~version
          { kind = "Include_declaration"
          ; clause = "Include_declaration"
          ; fields =
              [ { name = "pincl_mod"; value = Versioned_value.of_ast pincl_mod }
              ; { name = "pincl_loc"; value = Versioned_value.of_location pincl_loc }
              ; { name = "pincl_attributes"; value = Versioned_value.of_ast pincl_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Include_declaration"
        ; clause = "Include_declaration"
        ; fields =
            [ { name = "pincl_mod"; value = pincl_mod }
            ; { name = "pincl_loc"; value = pincl_loc }
            ; { name = "pincl_attributes"; value = pincl_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pincl_mod) ~f:(fun pincl_mod ->
          Optional.bind (Versioned_value.to_location pincl_loc) ~f:(fun pincl_loc ->
            Optional.bind (Versioned_value.to_ast pincl_attributes) ~f:(fun pincl_attributes ->
              Some (Concrete.Include_declaration { pincl_mod; pincl_loc; pincl_attributes }))))
      | _ -> None
  end

  module With_constraint = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pwith_type of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Pwith_module of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Pwith_typesubst of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Pwith_modsubst of { a : Versioned_ast.t; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Pwith_type { a; b } ->
        Versioned_ast.create ~version
          { kind = "With_constraint"
          ; clause = "Pwith_type"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Pwith_module { a; b } ->
        Versioned_ast.create ~version
          { kind = "With_constraint"
          ; clause = "Pwith_module"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Pwith_typesubst { a; b } ->
        Versioned_ast.create ~version
          { kind = "With_constraint"
          ; clause = "Pwith_typesubst"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Pwith_modsubst { a; b } ->
        Versioned_ast.create ~version
          { kind = "With_constraint"
          ; clause = "Pwith_modsubst"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "With_constraint"
        ; clause = "Pwith_type"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pwith_type { a; b })))
      | { kind = "With_constraint"
        ; clause = "Pwith_module"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pwith_module { a; b })))
      | { kind = "With_constraint"
        ; clause = "Pwith_typesubst"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pwith_typesubst { a; b })))
      | { kind = "With_constraint"
        ; clause = "Pwith_modsubst"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pwith_modsubst { a; b })))
      | _ -> None
  end

  module Module_expr = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Module_expr of { pmod_desc : Versioned_ast.t; pmod_loc : Location.t; pmod_attributes : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Module_expr { pmod_desc; pmod_loc; pmod_attributes } ->
        Versioned_ast.create ~version
          { kind = "Module_expr"
          ; clause = "Module_expr"
          ; fields =
              [ { name = "pmod_desc"; value = Versioned_value.of_ast pmod_desc }
              ; { name = "pmod_loc"; value = Versioned_value.of_location pmod_loc }
              ; { name = "pmod_attributes"; value = Versioned_value.of_ast pmod_attributes }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Module_expr"
        ; clause = "Module_expr"
        ; fields =
            [ { name = "pmod_desc"; value = pmod_desc }
            ; { name = "pmod_loc"; value = pmod_loc }
            ; { name = "pmod_attributes"; value = pmod_attributes }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pmod_desc) ~f:(fun pmod_desc ->
          Optional.bind (Versioned_value.to_location pmod_loc) ~f:(fun pmod_loc ->
            Optional.bind (Versioned_value.to_ast pmod_attributes) ~f:(fun pmod_attributes ->
              Some (Concrete.Module_expr { pmod_desc; pmod_loc; pmod_attributes }))))
      | _ -> None
  end

  module Module_expr_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pmod_ident of { a : Versioned_ast.t }
        | Pmod_structure of { a : Versioned_ast.t }
        | Pmod_functor of { a : Versioned_ast.t; b : Versioned_ast.t option; c : Versioned_ast.t }
        | Pmod_apply of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Pmod_constraint of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Pmod_unpack of { a : Versioned_ast.t }
        | Pmod_extension of { a : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Pmod_ident { a } ->
        Versioned_ast.create ~version
          { kind = "Module_expr_desc"
          ; clause = "Pmod_ident"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pmod_structure { a } ->
        Versioned_ast.create ~version
          { kind = "Module_expr_desc"
          ; clause = "Pmod_structure"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pmod_functor { a; b; c } ->
        Versioned_ast.create ~version
          { kind = "Module_expr_desc"
          ; clause = "Pmod_functor"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) b }
              ; { name = "c"; value = Versioned_value.of_ast c }
              ]
          }
      | Pmod_apply { a; b } ->
        Versioned_ast.create ~version
          { kind = "Module_expr_desc"
          ; clause = "Pmod_apply"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Pmod_constraint { a; b } ->
        Versioned_ast.create ~version
          { kind = "Module_expr_desc"
          ; clause = "Pmod_constraint"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Pmod_unpack { a } ->
        Versioned_ast.create ~version
          { kind = "Module_expr_desc"
          ; clause = "Pmod_unpack"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pmod_extension { a } ->
        Versioned_ast.create ~version
          { kind = "Module_expr_desc"
          ; clause = "Pmod_extension"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Module_expr_desc"
        ; clause = "Pmod_ident"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pmod_ident { a }))
      | { kind = "Module_expr_desc"
        ; clause = "Pmod_structure"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pmod_structure { a }))
      | { kind = "Module_expr_desc"
        ; clause = "Pmod_functor"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ; { name = "c"; value = c }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Optional.bind (Versioned_value.to_ast c) ~f:(fun c ->
              Some (Concrete.Pmod_functor { a; b; c }))))
      | { kind = "Module_expr_desc"
        ; clause = "Pmod_apply"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pmod_apply { a; b })))
      | { kind = "Module_expr_desc"
        ; clause = "Pmod_constraint"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pmod_constraint { a; b })))
      | { kind = "Module_expr_desc"
        ; clause = "Pmod_unpack"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pmod_unpack { a }))
      | { kind = "Module_expr_desc"
        ; clause = "Pmod_extension"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pmod_extension { a }))
      | _ -> None
  end

  module Structure = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Structure of { a : Versioned_ast.t list }
    end

    let of_concrete : Concrete.t -> t = function
      | Structure { a } ->
        Versioned_ast.create ~version
          { kind = "Structure"
          ; clause = "Structure"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Structure"
        ; clause = "Structure"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Structure { a }))
      | _ -> None
  end

  module Structure_item = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Structure_item of { pstr_desc : Versioned_ast.t; pstr_loc : Location.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Structure_item { pstr_desc; pstr_loc } ->
        Versioned_ast.create ~version
          { kind = "Structure_item"
          ; clause = "Structure_item"
          ; fields =
              [ { name = "pstr_desc"; value = Versioned_value.of_ast pstr_desc }
              ; { name = "pstr_loc"; value = Versioned_value.of_location pstr_loc }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Structure_item"
        ; clause = "Structure_item"
        ; fields =
            [ { name = "pstr_desc"; value = pstr_desc }
            ; { name = "pstr_loc"; value = pstr_loc }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pstr_desc) ~f:(fun pstr_desc ->
          Optional.bind (Versioned_value.to_location pstr_loc) ~f:(fun pstr_loc ->
            Some (Concrete.Structure_item { pstr_desc; pstr_loc })))
      | _ -> None
  end

  module Structure_item_desc = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pstr_eval of { a : Versioned_ast.t; b : Versioned_ast.t }
        | Pstr_value of { a : Versioned_ast.t; b : Versioned_ast.t list }
        | Pstr_primitive of { a : Versioned_ast.t }
        | Pstr_type of { a : Versioned_ast.t; b : Versioned_ast.t list }
        | Pstr_typext of { a : Versioned_ast.t }
        | Pstr_exception of { a : Versioned_ast.t }
        | Pstr_module of { a : Versioned_ast.t }
        | Pstr_recmodule of { a : Versioned_ast.t list }
        | Pstr_modtype of { a : Versioned_ast.t }
        | Pstr_open of { a : Versioned_ast.t }
        | Pstr_class of { a : Versioned_ast.t list }
        | Pstr_class_type of { a : Versioned_ast.t list }
        | Pstr_include of { a : Versioned_ast.t }
        | Pstr_attribute of { a : Versioned_ast.t }
        | Pstr_extension of { a : Versioned_ast.t; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Pstr_eval { a; b } ->
        Versioned_ast.create ~version
          { kind = "Structure_item_desc"
          ; clause = "Pstr_eval"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }
      | Pstr_value { a; b } ->
        Versioned_ast.create ~version
          { kind = "Structure_item_desc"
          ; clause = "Pstr_value"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) b }
              ]
          }
      | Pstr_primitive { a } ->
        Versioned_ast.create ~version
          { kind = "Structure_item_desc"
          ; clause = "Pstr_primitive"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pstr_type { a; b } ->
        Versioned_ast.create ~version
          { kind = "Structure_item_desc"
          ; clause = "Pstr_type"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) b }
              ]
          }
      | Pstr_typext { a } ->
        Versioned_ast.create ~version
          { kind = "Structure_item_desc"
          ; clause = "Pstr_typext"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pstr_exception { a } ->
        Versioned_ast.create ~version
          { kind = "Structure_item_desc"
          ; clause = "Pstr_exception"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pstr_module { a } ->
        Versioned_ast.create ~version
          { kind = "Structure_item_desc"
          ; clause = "Pstr_module"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pstr_recmodule { a } ->
        Versioned_ast.create ~version
          { kind = "Structure_item_desc"
          ; clause = "Pstr_recmodule"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Pstr_modtype { a } ->
        Versioned_ast.create ~version
          { kind = "Structure_item_desc"
          ; clause = "Pstr_modtype"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pstr_open { a } ->
        Versioned_ast.create ~version
          { kind = "Structure_item_desc"
          ; clause = "Pstr_open"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pstr_class { a } ->
        Versioned_ast.create ~version
          { kind = "Structure_item_desc"
          ; clause = "Pstr_class"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Pstr_class_type { a } ->
        Versioned_ast.create ~version
          { kind = "Structure_item_desc"
          ; clause = "Pstr_class_type"
          ; fields =
              [ { name = "a"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) a }
              ]
          }
      | Pstr_include { a } ->
        Versioned_ast.create ~version
          { kind = "Structure_item_desc"
          ; clause = "Pstr_include"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pstr_attribute { a } ->
        Versioned_ast.create ~version
          { kind = "Structure_item_desc"
          ; clause = "Pstr_attribute"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pstr_extension { a; b } ->
        Versioned_ast.create ~version
          { kind = "Structure_item_desc"
          ; clause = "Pstr_extension"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Structure_item_desc"
        ; clause = "Pstr_eval"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pstr_eval { a; b })))
      | { kind = "Structure_item_desc"
        ; clause = "Pstr_value"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Pstr_value { a; b })))
      | { kind = "Structure_item_desc"
        ; clause = "Pstr_primitive"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pstr_primitive { a }))
      | { kind = "Structure_item_desc"
        ; clause = "Pstr_type"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) b) ~f:(fun b ->
            Some (Concrete.Pstr_type { a; b })))
      | { kind = "Structure_item_desc"
        ; clause = "Pstr_typext"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pstr_typext { a }))
      | { kind = "Structure_item_desc"
        ; clause = "Pstr_exception"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pstr_exception { a }))
      | { kind = "Structure_item_desc"
        ; clause = "Pstr_module"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pstr_module { a }))
      | { kind = "Structure_item_desc"
        ; clause = "Pstr_recmodule"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Pstr_recmodule { a }))
      | { kind = "Structure_item_desc"
        ; clause = "Pstr_modtype"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pstr_modtype { a }))
      | { kind = "Structure_item_desc"
        ; clause = "Pstr_open"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pstr_open { a }))
      | { kind = "Structure_item_desc"
        ; clause = "Pstr_class"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Pstr_class { a }))
      | { kind = "Structure_item_desc"
        ; clause = "Pstr_class_type"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) a) ~f:(fun a ->
          Some (Concrete.Pstr_class_type { a }))
      | { kind = "Structure_item_desc"
        ; clause = "Pstr_include"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pstr_include { a }))
      | { kind = "Structure_item_desc"
        ; clause = "Pstr_attribute"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pstr_attribute { a }))
      | { kind = "Structure_item_desc"
        ; clause = "Pstr_extension"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Pstr_extension { a; b })))
      | _ -> None
  end

  module Value_binding = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Value_binding of { pvb_pat : Versioned_ast.t; pvb_expr : Versioned_ast.t; pvb_attributes : Versioned_ast.t; pvb_loc : Location.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Value_binding { pvb_pat; pvb_expr; pvb_attributes; pvb_loc } ->
        Versioned_ast.create ~version
          { kind = "Value_binding"
          ; clause = "Value_binding"
          ; fields =
              [ { name = "pvb_pat"; value = Versioned_value.of_ast pvb_pat }
              ; { name = "pvb_expr"; value = Versioned_value.of_ast pvb_expr }
              ; { name = "pvb_attributes"; value = Versioned_value.of_ast pvb_attributes }
              ; { name = "pvb_loc"; value = Versioned_value.of_location pvb_loc }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Value_binding"
        ; clause = "Value_binding"
        ; fields =
            [ { name = "pvb_pat"; value = pvb_pat }
            ; { name = "pvb_expr"; value = pvb_expr }
            ; { name = "pvb_attributes"; value = pvb_attributes }
            ; { name = "pvb_loc"; value = pvb_loc }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pvb_pat) ~f:(fun pvb_pat ->
          Optional.bind (Versioned_value.to_ast pvb_expr) ~f:(fun pvb_expr ->
            Optional.bind (Versioned_value.to_ast pvb_attributes) ~f:(fun pvb_attributes ->
              Optional.bind (Versioned_value.to_location pvb_loc) ~f:(fun pvb_loc ->
                Some (Concrete.Value_binding { pvb_pat; pvb_expr; pvb_attributes; pvb_loc })))))
      | _ -> None
  end

  module Module_binding = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Module_binding of { pmb_name : Versioned_ast.t; pmb_expr : Versioned_ast.t; pmb_attributes : Versioned_ast.t; pmb_loc : Location.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Module_binding { pmb_name; pmb_expr; pmb_attributes; pmb_loc } ->
        Versioned_ast.create ~version
          { kind = "Module_binding"
          ; clause = "Module_binding"
          ; fields =
              [ { name = "pmb_name"; value = Versioned_value.of_ast pmb_name }
              ; { name = "pmb_expr"; value = Versioned_value.of_ast pmb_expr }
              ; { name = "pmb_attributes"; value = Versioned_value.of_ast pmb_attributes }
              ; { name = "pmb_loc"; value = Versioned_value.of_location pmb_loc }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Module_binding"
        ; clause = "Module_binding"
        ; fields =
            [ { name = "pmb_name"; value = pmb_name }
            ; { name = "pmb_expr"; value = pmb_expr }
            ; { name = "pmb_attributes"; value = pmb_attributes }
            ; { name = "pmb_loc"; value = pmb_loc }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast pmb_name) ~f:(fun pmb_name ->
          Optional.bind (Versioned_value.to_ast pmb_expr) ~f:(fun pmb_expr ->
            Optional.bind (Versioned_value.to_ast pmb_attributes) ~f:(fun pmb_attributes ->
              Optional.bind (Versioned_value.to_location pmb_loc) ~f:(fun pmb_loc ->
                Some (Concrete.Module_binding { pmb_name; pmb_expr; pmb_attributes; pmb_loc })))))
      | _ -> None
  end

  module Toplevel_phrase = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Ptop_def of { a : Versioned_ast.t }
        | Ptop_dir of { a : string; b : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Ptop_def { a } ->
        Versioned_ast.create ~version
          { kind = "Toplevel_phrase"
          ; clause = "Ptop_def"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Ptop_dir { a; b } ->
        Versioned_ast.create ~version
          { kind = "Toplevel_phrase"
          ; clause = "Ptop_dir"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_string a }
              ; { name = "b"; value = Versioned_value.of_ast b }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Toplevel_phrase"
        ; clause = "Ptop_def"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Ptop_def { a }))
      | { kind = "Toplevel_phrase"
        ; clause = "Ptop_dir"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_string a) ~f:(fun a ->
          Optional.bind (Versioned_value.to_ast b) ~f:(fun b ->
            Some (Concrete.Ptop_dir { a; b })))
      | _ -> None
  end

  module Directive_argument = struct
    type t = Versioned_ast.t

    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Pdir_none
        | Pdir_string of { a : string }
        | Pdir_int of { a : string; b : char option }
        | Pdir_ident of { a : Versioned_ast.t }
        | Pdir_bool of { a : bool }
    end

    let of_concrete : Concrete.t -> t = function
      | Pdir_none ->
        Versioned_ast.create ~version
          { kind = "Directive_argument"
          ; clause = "Pdir_none"
          ; fields = []
          }
      | Pdir_string { a } ->
        Versioned_ast.create ~version
          { kind = "Directive_argument"
          ; clause = "Pdir_string"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_string a }
              ]
          }
      | Pdir_int { a; b } ->
        Versioned_ast.create ~version
          { kind = "Directive_argument"
          ; clause = "Pdir_int"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_string a }
              ; { name = "b"; value = (Versioned_value.of_option ~f:Versioned_value.of_char) b }
              ]
          }
      | Pdir_ident { a } ->
        Versioned_ast.create ~version
          { kind = "Directive_argument"
          ; clause = "Pdir_ident"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_ast a }
              ]
          }
      | Pdir_bool { a } ->
        Versioned_ast.create ~version
          { kind = "Directive_argument"
          ; clause = "Pdir_bool"
          ; fields =
              [ { name = "a"; value = Versioned_value.of_bool a }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Directive_argument"
        ; clause = "Pdir_none"
        ; fields = []
        } ->
        Some (Concrete.Pdir_none)
      | { kind = "Directive_argument"
        ; clause = "Pdir_string"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_string a) ~f:(fun a ->
          Some (Concrete.Pdir_string { a }))
      | { kind = "Directive_argument"
        ; clause = "Pdir_int"
        ; fields =
            [ { name = "a"; value = a }
            ; { name = "b"; value = b }
            ]
        } ->
        Optional.bind (Versioned_value.to_string a) ~f:(fun a ->
          Optional.bind ((Versioned_value.to_option ~f:Versioned_value.to_char) b) ~f:(fun b ->
            Some (Concrete.Pdir_int { a; b })))
      | { kind = "Directive_argument"
        ; clause = "Pdir_ident"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_ast a) ~f:(fun a ->
          Some (Concrete.Pdir_ident { a }))
      | { kind = "Directive_argument"
        ; clause = "Pdir_bool"
        ; fields =
            [ { name = "a"; value = a }
            ]
        } ->
        Optional.bind (Versioned_value.to_bool a) ~f:(fun a ->
          Some (Concrete.Pdir_bool { a }))
      | _ -> None
  end
end
(*$*)
