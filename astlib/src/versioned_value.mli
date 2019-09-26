open Ocaml_common

type t = Versioned_ast.t Astlib_ast.Ast.value

val of_ast : Versioned_ast.t -> t
val to_ast : t -> Versioned_ast.t option

val of_list : 'a list -> f:('a -> t) -> t
val to_list : t -> f:(t -> 'a option) -> 'a list option

val of_option : 'a option -> f:('a -> t) -> t
val to_option : t -> f:(t -> 'a option) -> 'a option option

val of_location : Location.t -> t
val to_location : t -> Location.t option

val of_string : string -> t
val to_string : t -> string option

val of_char : char -> t
val to_char : t -> char option

val of_bool : bool -> t
val to_bool : t -> bool option