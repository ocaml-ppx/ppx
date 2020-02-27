(** Element ordering *)

type t =
  | Lt (** Lesser than  *)
  | Eq (** Equal        *)
  | Gt (** Greater than *)

val of_int : int -> t
val to_int : t -> int

(** returns the string representation. one of: "<", "=", ">" *)
val to_string : t -> string

val neq : t -> bool

val leq : t -> bool

val geq : t -> bool

val is_eq : t -> bool

val rev : t -> t
