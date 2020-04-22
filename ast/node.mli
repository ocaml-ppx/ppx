type t

val version : t -> Astlib.Version.t
val of_ast : version:Astlib.Version.t -> t Astlib.Ast.t -> t
val to_ast : version:Astlib.Version.t -> t -> t Astlib.Ast.t
