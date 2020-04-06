module Located : sig
  val longident
    : loc: Astlib.Location.t -> Versions.longident -> Versions.longident Astlib.Loc.t
  val lident : loc: Astlib.Location.t -> string -> Versions.longident Astlib.Loc.t
  val dotted : loc: Astlib.Location.t -> string list -> Versions.longident Astlib.Loc.t
end

val echar : loc: Astlib.Location.t -> char -> Versions.expression
val estring : loc: Astlib.Location.t -> string -> Versions.expression
val eint : loc: Astlib.Location.t -> int -> Versions.expression
val eint32 : loc: Astlib.Location.t -> int32 -> Versions.expression
val eint64 : loc: Astlib.Location.t -> int64 -> Versions.expression
val enativeint : loc: Astlib.Location.t -> nativeint -> Versions.expression
val efloat : loc: Astlib.Location.t -> float -> Versions.expression
val evar : loc: Astlib.Location.t -> string -> Versions.expression
val eunit : loc: Astlib.Location.t -> Versions.expression
val ebool : loc: Astlib.Location.t -> bool -> Versions.expression
val enil : loc: Astlib.Location.t -> Versions.expression
val elist : loc: Astlib.Location.t -> Versions.expression list -> Versions.expression
val etuple : loc: Astlib.Location.t -> Versions.expression list -> Versions.expression
val eabstract
  :  loc:Astlib.Location.t
  -> Versions.pattern list
  -> Versions.expression
  -> Versions.expression
val eapply :
  loc : Astlib.Location.t ->
  Versions.expression ->
  Versions.expression list ->
  Versions.expression

val pchar : loc: Astlib.Location.t -> char -> Versions.pattern
val pstring : loc: Astlib.Location.t -> string -> Versions.pattern
val pint : loc: Astlib.Location.t -> int -> Versions.pattern
val pint32 : loc: Astlib.Location.t -> int32 -> Versions.pattern
val pint64 : loc: Astlib.Location.t -> int64 -> Versions.pattern
val pnativeint : loc: Astlib.Location.t -> nativeint -> Versions.pattern
val pfloat : loc: Astlib.Location.t -> float -> Versions.pattern
val pvar : loc: Astlib.Location.t -> string -> Versions.pattern
val punit : loc: Astlib.Location.t -> Versions.pattern
val pbool : loc: Astlib.Location.t -> bool -> Versions.pattern
val pnil : loc: Astlib.Location.t -> Versions.pattern
val ptuple : loc: Astlib.Location.t -> Versions.pattern list -> Versions.pattern
val plist : loc: Astlib.Location.t -> Versions.pattern list -> Versions.pattern

module Error_ext : sig
  (** Functions to build error as extension points. Each of these functions
      formats the given error message and returns the extension point for
      the required context, [exprf] for [expression], [patf] for [pattern],
      etc. *)

  val extension_of_error : Astlib.Location.Error.t -> Versions.extension

  val exprf :
    loc:Astlib.Location.t ->
    ('a, unit, string, Versions.expression) format4 ->
    'a

  val patf :
    loc:Astlib.Location.t ->
    ('a, unit, string, Versions.pattern) format4 ->
    'a

  val typf :
    loc:Astlib.Location.t ->
    ('a, unit, string, Versions.core_type) format4 ->
    'a

  val strif :
    loc:Astlib.Location.t ->
    ('a, unit, string, Versions.structure_item) format4 ->
    'a

  val sigif :
    loc:Astlib.Location.t ->
    ('a, unit, string, Versions.signature_item) format4 ->
    'a
end
