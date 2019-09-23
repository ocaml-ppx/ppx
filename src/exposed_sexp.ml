type t = Stdppx.Sexp.t =
  | Atom of string
  | List of t list
