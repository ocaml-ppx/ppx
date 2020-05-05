type t

val version : t -> Astlib.Version.t
val wrap : version:Astlib.Version.t -> t Astlib.Ast.t -> t
val unwrap : version:Astlib.Version.t -> t -> t Astlib.Ast.t option
