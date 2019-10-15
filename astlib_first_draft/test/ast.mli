open Base

type t = Astlib_first_draft.Versioned_ast.t

val equal : t -> t -> bool
val sexp_of_t : t -> Sexp.t
