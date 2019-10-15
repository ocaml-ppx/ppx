type t

val version : t -> string
val create : t Astlib_first_draft_ast.Ast.node -> version:string -> t
val convert : t -> version:string -> t Astlib_first_draft_ast.Ast.node
