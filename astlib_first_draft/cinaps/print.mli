val newline : unit -> unit

val format : ('a, unit, string, unit) format4 -> 'a

val declare_module : string -> (unit -> unit) -> unit
val define_module : string -> (unit -> unit) -> unit

val declare_recursive_modules : (string * (unit -> unit)) list -> unit
val define_recursive_values : (string * (unit -> unit)) list -> unit

val indented : ?levels:int -> (unit -> unit) -> unit
