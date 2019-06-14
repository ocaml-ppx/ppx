type t = Stdppxlib.Sexp.t =
  | Atom of string
  | List of t list
