(** Stable interface for arbitrary values annotated with Astlib locations.

    Astlib location annotations use the same representation as location annotations in the
    compiler; however, we allow for the possibility that new fields will be added to the
    type over time. We expose the type as [private] to discourage explicit construction,
    and provide a [create] function that can be extended with optional arguments in a
    backward-compatible way. *)

(** {1 Type} *)

(** The type equivalence with [Ocaml_common.Location.loc] is expected to stay stable. *)
type 'a t = private 'a Ocaml_common.Location.loc

(** {1 Conversions} It should always be possible to convert to/from
    [Ocaml_common.Location.loc]. *)

val of_loc : 'a Ocaml_common.Location.loc -> 'a t
val to_loc : 'a t -> 'a Ocaml_common.Location.loc

(** {1 Accessors} These should be stable. *)

val txt : 'a t -> 'a
val loc : 'a t -> Location.t

(** {1 Constructors} These may have additional optional arguments added over time. Where
    necessary, we add an unnamed [unit] argument so that optional arguments can be
    dropped, even if there currently are none. *)

val create
  :  txt : 'a
  -> loc : Location.t
  -> unit
  -> 'a t

val update
  :  ?txt : 'a
  -> ?loc : Location.t
  -> 'a t
  -> 'a t

val update_txt
  :  txt : 'a
  -> ?loc : Location.t
  -> _ t
  -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t
