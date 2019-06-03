open Base

type t

val sexp_of_t : t -> Sexp.t
val equal : t -> t -> bool
val version : t -> string
val create : t Astlib_ast.Ast.node -> version:string -> t
val convert : t -> version:string -> t Astlib_ast.Ast.node
