val string_of_ty : internal: bool -> Astlib.Grammar.ty -> string
val string_of_tuple_type :
  internal: bool ->
  ?parens: bool ->
  Astlib.Grammar.ty list ->
  string
