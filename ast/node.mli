type t

val version : t -> string
val of_node : t Astlib.Ast.node -> version:string -> t
val to_node : t -> version:string -> t Astlib.Ast.node
