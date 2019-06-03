(*$ Astlib_cinaps.print_astlib_ml () *)
open Base
open Ocaml_common

module V1 = struct
  let version = "V1"

  module Expression = struct
    type t = Versioned_ast.t

    let sexp_of_t = Versioned_ast.sexp_of_t
    let equal = Versioned_ast.equal
    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Ident of { loc : Location.t; name : string }
        | Match of { loc : Location.t; arg : Versioned_ast.t; cases : Versioned_ast.t list }
    end

    let of_concrete : Concrete.t -> t = function
      | Ident { loc; name } ->
        Versioned_ast.create ~version
          { kind = "Expression"
          ; clause = "Ident"
          ; loc
          ; fields =
              [ { name = "name"; value = Versioned_value.of_string name }
              ]
          }
      | Match { loc; arg; cases } ->
        Versioned_ast.create ~version
          { kind = "Expression"
          ; clause = "Match"
          ; loc
          ; fields =
              [ { name = "arg"; value = Versioned_value.of_ast arg }
              ; { name = "cases"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) cases }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Expression"
        ; clause = "Ident"
        ; loc
        ; fields =
            [ { name = "name"; value = name }
            ]
        } ->
        Option.bind (Versioned_value.to_string name) ~f:(fun name ->
          Some (Concrete.Ident { loc; name }))
      | { kind = "Expression"
        ; clause = "Match"
        ; loc
        ; fields =
            [ { name = "arg"; value = arg }
            ; { name = "cases"; value = cases }
            ]
        } ->
        Option.bind (Versioned_value.to_ast arg) ~f:(fun arg ->
          Option.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) cases) ~f:(fun cases ->
            Some (Concrete.Match { loc; arg; cases })))
      | _ -> None
  end

  module Case = struct
    type t = Versioned_ast.t

    let sexp_of_t = Versioned_ast.sexp_of_t
    let equal = Versioned_ast.equal
    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Case of { loc : Location.t; lhs : Versioned_ast.t; guard : Versioned_ast.t option; rhs : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Case { loc; lhs; guard; rhs } ->
        Versioned_ast.create ~version
          { kind = "Case"
          ; clause = "Case"
          ; loc
          ; fields =
              [ { name = "lhs"; value = Versioned_value.of_ast lhs }
              ; { name = "guard"; value = (Versioned_value.of_option ~f:Versioned_value.of_ast) guard }
              ; { name = "rhs"; value = Versioned_value.of_ast rhs }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Case"
        ; clause = "Case"
        ; loc
        ; fields =
            [ { name = "lhs"; value = lhs }
            ; { name = "guard"; value = guard }
            ; { name = "rhs"; value = rhs }
            ]
        } ->
        Option.bind (Versioned_value.to_ast lhs) ~f:(fun lhs ->
          Option.bind ((Versioned_value.to_option ~f:Versioned_value.to_ast) guard) ~f:(fun guard ->
            Option.bind (Versioned_value.to_ast rhs) ~f:(fun rhs ->
              Some (Concrete.Case { loc; lhs; guard; rhs }))))
      | _ -> None
  end

  module Pattern = struct
    type t = Versioned_ast.t

    let sexp_of_t = Versioned_ast.sexp_of_t
    let equal = Versioned_ast.equal
    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Ident of { loc : Location.t; name : string }
    end

    let of_concrete : Concrete.t -> t = function
      | Ident { loc; name } ->
        Versioned_ast.create ~version
          { kind = "Pattern"
          ; clause = "Ident"
          ; loc
          ; fields =
              [ { name = "name"; value = Versioned_value.of_string name }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Pattern"
        ; clause = "Ident"
        ; loc
        ; fields =
            [ { name = "name"; value = name }
            ]
        } ->
        Option.bind (Versioned_value.to_string name) ~f:(fun name ->
          Some (Concrete.Ident { loc; name }))
      | _ -> None
  end
end

module V2 = struct
  let version = "V2"

  module Expression = struct
    type t = Versioned_ast.t

    let sexp_of_t = Versioned_ast.sexp_of_t
    let equal = Versioned_ast.equal
    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Ident of { loc : Location.t; name : string }
        | Match of { loc : Location.t; arg : Versioned_ast.t; cases : Versioned_ast.t list }
    end

    let of_concrete : Concrete.t -> t = function
      | Ident { loc; name } ->
        Versioned_ast.create ~version
          { kind = "Expression"
          ; clause = "Ident"
          ; loc
          ; fields =
              [ { name = "name"; value = Versioned_value.of_string name }
              ]
          }
      | Match { loc; arg; cases } ->
        Versioned_ast.create ~version
          { kind = "Expression"
          ; clause = "Match"
          ; loc
          ; fields =
              [ { name = "arg"; value = Versioned_value.of_ast arg }
              ; { name = "cases"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) cases }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Expression"
        ; clause = "Ident"
        ; loc
        ; fields =
            [ { name = "name"; value = name }
            ]
        } ->
        Option.bind (Versioned_value.to_string name) ~f:(fun name ->
          Some (Concrete.Ident { loc; name }))
      | { kind = "Expression"
        ; clause = "Match"
        ; loc
        ; fields =
            [ { name = "arg"; value = arg }
            ; { name = "cases"; value = cases }
            ]
        } ->
        Option.bind (Versioned_value.to_ast arg) ~f:(fun arg ->
          Option.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) cases) ~f:(fun cases ->
            Some (Concrete.Match { loc; arg; cases })))
      | _ -> None
  end

  module Case = struct
    type t = Versioned_ast.t

    let sexp_of_t = Versioned_ast.sexp_of_t
    let equal = Versioned_ast.equal
    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Case of { loc : Location.t; lhs : Versioned_ast.t; rhs : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Case { loc; lhs; rhs } ->
        Versioned_ast.create ~version
          { kind = "Case"
          ; clause = "Case"
          ; loc
          ; fields =
              [ { name = "lhs"; value = Versioned_value.of_ast lhs }
              ; { name = "rhs"; value = Versioned_value.of_ast rhs }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Case"
        ; clause = "Case"
        ; loc
        ; fields =
            [ { name = "lhs"; value = lhs }
            ; { name = "rhs"; value = rhs }
            ]
        } ->
        Option.bind (Versioned_value.to_ast lhs) ~f:(fun lhs ->
          Option.bind (Versioned_value.to_ast rhs) ~f:(fun rhs ->
            Some (Concrete.Case { loc; lhs; rhs })))
      | _ -> None
  end

  module Pattern = struct
    type t = Versioned_ast.t

    let sexp_of_t = Versioned_ast.sexp_of_t
    let equal = Versioned_ast.equal
    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Ident of { loc : Location.t; name : string }
        | Guard of { loc : Location.t; body : Versioned_ast.t; guard : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Ident { loc; name } ->
        Versioned_ast.create ~version
          { kind = "Pattern"
          ; clause = "Ident"
          ; loc
          ; fields =
              [ { name = "name"; value = Versioned_value.of_string name }
              ]
          }
      | Guard { loc; body; guard } ->
        Versioned_ast.create ~version
          { kind = "Pattern"
          ; clause = "Guard"
          ; loc
          ; fields =
              [ { name = "body"; value = Versioned_value.of_ast body }
              ; { name = "guard"; value = Versioned_value.of_ast guard }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Pattern"
        ; clause = "Ident"
        ; loc
        ; fields =
            [ { name = "name"; value = name }
            ]
        } ->
        Option.bind (Versioned_value.to_string name) ~f:(fun name ->
          Some (Concrete.Ident { loc; name }))
      | { kind = "Pattern"
        ; clause = "Guard"
        ; loc
        ; fields =
            [ { name = "body"; value = body }
            ; { name = "guard"; value = guard }
            ]
        } ->
        Option.bind (Versioned_value.to_ast body) ~f:(fun body ->
          Option.bind (Versioned_value.to_ast guard) ~f:(fun guard ->
            Some (Concrete.Guard { loc; body; guard })))
      | _ -> None
  end
end

module V3 = struct
  let version = "V3"

  module Identifier = struct
    type t = Versioned_ast.t

    let sexp_of_t = Versioned_ast.sexp_of_t
    let equal = Versioned_ast.equal
    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Var of { loc : Location.t; name : string }
        | Dot of { loc : Location.t; base : Versioned_ast.t; label : string }
    end

    let of_concrete : Concrete.t -> t = function
      | Var { loc; name } ->
        Versioned_ast.create ~version
          { kind = "Identifier"
          ; clause = "Var"
          ; loc
          ; fields =
              [ { name = "name"; value = Versioned_value.of_string name }
              ]
          }
      | Dot { loc; base; label } ->
        Versioned_ast.create ~version
          { kind = "Identifier"
          ; clause = "Dot"
          ; loc
          ; fields =
              [ { name = "base"; value = Versioned_value.of_ast base }
              ; { name = "label"; value = Versioned_value.of_string label }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Identifier"
        ; clause = "Var"
        ; loc
        ; fields =
            [ { name = "name"; value = name }
            ]
        } ->
        Option.bind (Versioned_value.to_string name) ~f:(fun name ->
          Some (Concrete.Var { loc; name }))
      | { kind = "Identifier"
        ; clause = "Dot"
        ; loc
        ; fields =
            [ { name = "base"; value = base }
            ; { name = "label"; value = label }
            ]
        } ->
        Option.bind (Versioned_value.to_ast base) ~f:(fun base ->
          Option.bind (Versioned_value.to_string label) ~f:(fun label ->
            Some (Concrete.Dot { loc; base; label })))
      | _ -> None
  end

  module Expression = struct
    type t = Versioned_ast.t

    let sexp_of_t = Versioned_ast.sexp_of_t
    let equal = Versioned_ast.equal
    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Ident of { loc : Location.t; id : Versioned_ast.t }
        | Match of { loc : Location.t; arg : Versioned_ast.t; cases : Versioned_ast.t list }
    end

    let of_concrete : Concrete.t -> t = function
      | Ident { loc; id } ->
        Versioned_ast.create ~version
          { kind = "Expression"
          ; clause = "Ident"
          ; loc
          ; fields =
              [ { name = "id"; value = Versioned_value.of_ast id }
              ]
          }
      | Match { loc; arg; cases } ->
        Versioned_ast.create ~version
          { kind = "Expression"
          ; clause = "Match"
          ; loc
          ; fields =
              [ { name = "arg"; value = Versioned_value.of_ast arg }
              ; { name = "cases"; value = (Versioned_value.of_list ~f:Versioned_value.of_ast) cases }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Expression"
        ; clause = "Ident"
        ; loc
        ; fields =
            [ { name = "id"; value = id }
            ]
        } ->
        Option.bind (Versioned_value.to_ast id) ~f:(fun id ->
          Some (Concrete.Ident { loc; id }))
      | { kind = "Expression"
        ; clause = "Match"
        ; loc
        ; fields =
            [ { name = "arg"; value = arg }
            ; { name = "cases"; value = cases }
            ]
        } ->
        Option.bind (Versioned_value.to_ast arg) ~f:(fun arg ->
          Option.bind ((Versioned_value.to_list ~f:Versioned_value.to_ast) cases) ~f:(fun cases ->
            Some (Concrete.Match { loc; arg; cases })))
      | _ -> None
  end

  module Case = struct
    type t = Versioned_ast.t

    let sexp_of_t = Versioned_ast.sexp_of_t
    let equal = Versioned_ast.equal
    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Case of { loc : Location.t; lhs : Versioned_ast.t; rhs : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Case { loc; lhs; rhs } ->
        Versioned_ast.create ~version
          { kind = "Case"
          ; clause = "Case"
          ; loc
          ; fields =
              [ { name = "lhs"; value = Versioned_value.of_ast lhs }
              ; { name = "rhs"; value = Versioned_value.of_ast rhs }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Case"
        ; clause = "Case"
        ; loc
        ; fields =
            [ { name = "lhs"; value = lhs }
            ; { name = "rhs"; value = rhs }
            ]
        } ->
        Option.bind (Versioned_value.to_ast lhs) ~f:(fun lhs ->
          Option.bind (Versioned_value.to_ast rhs) ~f:(fun rhs ->
            Some (Concrete.Case { loc; lhs; rhs })))
      | _ -> None
  end

  module Pattern = struct
    type t = Versioned_ast.t

    let sexp_of_t = Versioned_ast.sexp_of_t
    let equal = Versioned_ast.equal
    let of_ast t = t
    let to_ast t = t

    module Concrete = struct
      type t =
        | Ident of { loc : Location.t; name : string }
        | Guard of { loc : Location.t; body : Versioned_ast.t; guard : Versioned_ast.t }
    end

    let of_concrete : Concrete.t -> t = function
      | Ident { loc; name } ->
        Versioned_ast.create ~version
          { kind = "Pattern"
          ; clause = "Ident"
          ; loc
          ; fields =
              [ { name = "name"; value = Versioned_value.of_string name }
              ]
          }
      | Guard { loc; body; guard } ->
        Versioned_ast.create ~version
          { kind = "Pattern"
          ; clause = "Guard"
          ; loc
          ; fields =
              [ { name = "body"; value = Versioned_value.of_ast body }
              ; { name = "guard"; value = Versioned_value.of_ast guard }
              ]
          }

    let to_concrete t =
      match Versioned_ast.convert t ~version with
      | { kind = "Pattern"
        ; clause = "Ident"
        ; loc
        ; fields =
            [ { name = "name"; value = name }
            ]
        } ->
        Option.bind (Versioned_value.to_string name) ~f:(fun name ->
          Some (Concrete.Ident { loc; name }))
      | { kind = "Pattern"
        ; clause = "Guard"
        ; loc
        ; fields =
            [ { name = "body"; value = body }
            ; { name = "guard"; value = guard }
            ]
        } ->
        Option.bind (Versioned_value.to_ast body) ~f:(fun body ->
          Option.bind (Versioned_value.to_ast guard) ~f:(fun guard ->
            Some (Concrete.Guard { loc; body; guard })))
      | _ -> None
  end
end
(*$*)
