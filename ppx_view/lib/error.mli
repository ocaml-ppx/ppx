open Import
(** Error reporting functions *)

type t = string Astlib.Loc.t

(** Reports that the [[%view ...]] payload is neither a [match] nor a [function]
    expression. *)
val invalid_payload : loc: Astlib.Location.t -> t

(** Reports that the [[@view ...]] payload is not properly formed, i.e. it's not
    a record of the form [{a; b}] or [{a = <var1>; b = <var2>}]. *)
val invalid_attribute_payload : loc: Astlib.Location.t -> t

(** [unsupported_pattern ~loc "x"] reports a pattern that can't be translated
    by ppx_view as: ["ppx_view doesn't support pattern matching over x"]. *)
val unsupported_pattern : loc: Astlib.Location.t -> string -> t

(** Reports that two branches of an or-pattern binds a different set of
    variables. *)
val or_pattern_variables_differ : loc: Astlib.Location.t -> t

(** Reports that a record pattern contains a qualified field name. *)
val invalid_record_field : loc: Astlib.Location.t -> t

(** Reports a constant numeric pattern with an unknown suffix. *)
val unsupported_num_const :
  loc: Astlib.Location.t ->
  kind: string ->
  suffix: char ->
  t

val to_expr : t -> expression
val to_pat : t -> pattern
