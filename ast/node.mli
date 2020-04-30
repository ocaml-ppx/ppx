type t

val version : t -> Astlib.Version.t
val of_ast : t Astlib.Ast.t -> version:Astlib.Version.t -> t
val to_ast : t -> version:Astlib.Version.t -> t Astlib.Ast.t
