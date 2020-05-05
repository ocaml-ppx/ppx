(** Represents a version of the OCaml compiler corresponding to a new AST. *)
type t

val compare : t -> t -> int
val equal : t -> t -> bool

val to_string : t -> string
val of_string : string -> t
