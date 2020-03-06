(** Represents a version of the OCaml compiler corresponding to a new AST. *)
type t

val to_string : t -> string
val of_string : string -> t
