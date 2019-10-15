open Base
open Astlib_first_draft.Versioned_ast

type nonrec t = t

let rec sexp_of_t t =
  let version = version t in
  Sexp.List [Atom version; sexp_of_node (convert t ~version)]

and sexp_of_node { kind; clause; fields } =
  Sexp.List
    [ Atom kind
    ; Atom clause
    ; (sexp_of_list sexp_of_field fields)
    ]

and sexp_of_field { name; value } =
  Sexp.List [Atom name; sexp_of_value value]

and sexp_of_value = function
  | Tree t -> sexp_of_t t
  | Bool _ -> Atom "bool"
  | Char _ -> Atom "char"
  | String _ -> Atom "string"
  | Location _ -> Atom "Location.t"
  | List value -> List [sexp_of_list sexp_of_value value; Atom "list"]
  | Option value -> List [sexp_of_option sexp_of_value value; Atom "option"]

let equal x y = Poly.equal x y
