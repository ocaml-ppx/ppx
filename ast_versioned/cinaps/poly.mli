type env
type env_table

val env_table : Astlib.Grammar.t -> env_table
val find : env_table -> string -> env list

val empty_env : env
val env_is_empty : env -> bool

val args : env -> Astlib.Grammar.ty list
val suffix : env -> string
val suffix_of_args : Astlib.Grammar.ty list -> string

val subst_ty : Astlib.Grammar.ty -> env:env -> Astlib.Grammar.ty
