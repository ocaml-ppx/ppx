type t

val version : t -> Astlib.Version.t
val of_node : version:Astlib.Version.t -> t Astlib.Ast.node -> t
val to_node : version:Astlib.Version.t -> t -> t Astlib.Ast.node
