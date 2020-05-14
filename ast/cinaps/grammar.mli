(** Helper functions to print [Astlib.Grammar] values as the OCaml types they
    represent.

    The [nodify] optional argument is used to replace type variables by
    instances of the [node] type, e.g. it will replace any ['a] by ['a node]
    when printing the type. This is used to enforce that some functions in
    [Versions] only accept polymorphic types instantiated with other AST
    types. *)

val string_of_targ : nodify: bool -> Astlib.Grammar.targ -> string

val string_of_ty : nodify: bool -> Astlib.Grammar.ty -> string

val string_of_tuple_type
  :  nodify: bool
  -> parens: bool
  -> Astlib.Grammar.ty list
  -> string
