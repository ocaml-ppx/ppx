type t

val version : t -> string
val create : t Astlib_ast.Ast.node -> version:string -> t
val convert : t -> version:string -> t Astlib_ast.Ast.node
