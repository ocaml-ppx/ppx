type t = Node.t Astlib.Ast.data

val of_node : _ Unversioned.node -> t
val of_bool : bool -> t
val of_char : char -> t
val of_int : int -> t
val of_string : string -> t
val of_location : Astlib.Location.t -> t
val of_loc : 'a Astlib.Loc.t -> f:('a -> t) -> t
val of_list : 'a list -> f:('a -> t) -> t
val of_option : 'a option -> f:('a -> t) -> t

val of_tuple2 : ('a * 'b) -> f1:('a -> t) -> f2:('b -> t) -> t
val of_tuple3 : ('a * 'b * 'c) -> f1:('a -> t) -> f2:('b -> t) -> f3:('c -> t) -> t

val of_tuple4
  :  ('a * 'b * 'c * 'd)
  -> f1:('a -> t)
  -> f2:('b -> t)
  -> f3:('c -> t)
  -> f4:('d -> t)
  -> t

val to_node : t -> _ Unversioned.node option
val to_bool : t -> bool option
val to_char : t -> char option
val to_int : t -> int option
val to_string : t -> string option
val to_location : t -> Astlib.Location.t option
val to_loc : t -> f:(t -> 'a option) -> 'a Astlib.Loc.t option
val to_list : t -> f:(t -> 'a option) -> 'a list option
val to_option : t -> f:(t -> 'a option) -> 'a option option

val to_tuple2 : t -> f1:(t -> 'a option) -> f2:(t -> 'b option) -> ('a * 'b) option

val to_tuple3
  :  t
  -> f1:(t -> 'a option)
  -> f2:(t -> 'b option)
  -> f3:(t -> 'c option)
  -> ('a * 'b * 'c) option

val to_tuple4
  :  t
  -> f1:(t -> 'a option)
  -> f2:(t -> 'b option)
  -> f3:(t -> 'c option)
  -> f4:(t -> 'd option)
  -> ('a * 'b * 'c * 'd) option
