type env
type env_table

val env_table : Astlib.Grammar.t -> env_table
val find : env_table -> string -> env list

val empty_env : env
val env_is_empty : env -> bool

val args : env -> Astlib.Grammar.ty list

val subst_ty : Astlib.Grammar.ty -> env:env -> Astlib.Grammar.ty
val subst_decl : Astlib.Grammar.decl -> env:env -> Astlib.Grammar.decl
