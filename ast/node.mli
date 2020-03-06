type t

val version : t -> Astlib.Version.t
val of_node : t Astlib.Ast.node -> version:Astlib.Version.t -> t
val to_node : t -> version:Astlib.Version.t -> t Astlib.Ast.node
