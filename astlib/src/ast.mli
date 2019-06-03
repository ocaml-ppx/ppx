open Base

val sexp_of_node  : ('a -> Sexp.t) -> 'a Astlib_ast.Ast.node  -> Sexp.t
val sexp_of_field : ('a -> Sexp.t) -> 'a Astlib_ast.Ast.field -> Sexp.t
val sexp_of_value : ('a -> Sexp.t) -> 'a Astlib_ast.Ast.value -> Sexp.t

type 'a equal = 'a -> 'a -> bool

val equal_node  : 'a equal -> 'a Astlib_ast.Ast.node  -> 'a Astlib_ast.Ast.node  -> bool
val equal_field : 'a equal -> 'a Astlib_ast.Ast.field -> 'a Astlib_ast.Ast.field -> bool
val equal_value : 'a equal -> 'a Astlib_ast.Ast.value -> 'a Astlib_ast.Ast.value -> bool
