(** Stable interface for Astlib locations.

    Astlib locations use the same representation as locations in the compiler; however, we
    allow for the possibility that new fields will be added to the type over time. We
    provide a [create] function that can be extended with optional arguments in a
    backward-compatible way, and we may add new accessors as well. *)

(** {1 Type} *)

type t

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

val none : t

(** {1 Errors} *)

type location = t

module Error : sig
  (** The type for located errors *)
  type t

  (** {2 Conversions} It should always be possible to convert to/from
      [Ocaml_common.Location.error]. *)

  val of_error : Ocaml_common.Location.error -> t
  val to_error : t -> Ocaml_common.Location.error

  (** {2 Constructors} *)

  (** [make ~loc pp_msg] returns the error located at [loc] with the message
      formatted by [pp_msg] *)
  val make :
    loc: location ->
    (Format.formatter -> unit) ->
    t

  (** {2 Accessors} *)

  val location : t -> location
  val report : Format.formatter -> t -> unit

  (** {2 Exceptions} *)

  (** Add a conversion for a new exception constructor. *)
  val register_of_exn : (exn -> t option) -> unit

  (** Extract a [t] from an exception using registered conversions. *)
  val of_exn : exn -> t option
end
