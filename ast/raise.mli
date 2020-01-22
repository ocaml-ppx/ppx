(** [conversion_failed ~version "x"] raises an exception describing a failure
    to convert a node of type x to the [version] AST representation. *)
val conversion_failed : version: string -> string -> 'a
