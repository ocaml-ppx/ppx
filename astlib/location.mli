(** Stable interface for Astlib locations.

    Astlib locations use the same representation as locations in the compiler; however, we
    allow for the possibility that new fields will be added to the type over time. We
    expose the type as [private] to discourage explicit construction, and provide a
    [create] function that can be extended with optional arguments in a
    backward-compatible way. *)

(** {1 Type} *)

(** The type equivalence with [Ocaml_common.Location] is expected to stay stable. *)
type t = Ocaml_common.Location.t =
  { loc_start : Lexing.position
  ; loc_end : Lexing.position
  ; loc_ghost : bool
  }

(** {1 Conversions} It should always be possible to convert to/from
    [Ocaml_common.Location.t]. *)

val of_location : Ocaml_common.Location.t -> t
val to_location : t -> Ocaml_common.Location.t

(** {1 Accessors} These should be stable. *)

val start : t -> Position.t
val end_ : t -> Position.t
val ghost : t -> bool

(** {1 Constructors} These may have additional optional arguments added over time. Where
    necessary, we add an unnamed [unit] argument so that optional arguments can be
    dropped, even if there currently are none. *)

val create
  :  start : Position.t
  -> end_ : Position.t
  -> ?ghost : bool (** default: [false] *)
  -> unit
  -> t

val update
  :  ?start : Position.t
  -> ?end_ : Position.t
  -> ?ghost : bool
  -> t
  -> t
