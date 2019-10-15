val id : string -> string
val tvar : string -> string
val tag : string -> string
val module_name : string -> string

val poly_type : string -> tvars:string list -> string
val poly_inst : string -> args:string list -> string

val define_module : string -> (unit -> unit) -> unit
val declare_module : string -> (unit -> unit) -> unit

val define_modules
  :  ?recursive:bool
  -> (string * 'a) list
  -> f:(string -> 'a -> unit)
  -> unit

val declare_modules
  :  ?recursive:bool
  -> (string * 'a) list
  -> f:(string -> 'a -> unit)
  -> unit

val print_record_type : (string * 'a) list -> f:('a -> string) -> unit

val print_array : 'a list -> f:(int -> 'a -> string) -> unit

val print_arrow : 'a list -> f:('a -> string) -> string -> unit

val print_labelled_arrow : (string * 'a) list -> f:('a -> string) -> string -> unit

type element =
  | Empty
  | Line of string
  | Block of (unit -> unit)

val declare_type : ?tvars:string list -> string -> element -> unit

val declare_val : string -> element -> unit

val print_variant_type : (string * 'a) list -> f:('a -> element) -> unit
