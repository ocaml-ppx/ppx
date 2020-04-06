(** Stable interface for Astlib locations. *)

(** {1 Type} *)

type t = Ocaml_common.Location.t =
  { loc_start : Position.t
  ; loc_end : Position.t
  ; loc_ghost : bool
  }

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

  (** [create ~loc pp_msg] returns the error located at [loc] with the message
      formatted by [pp_msg] *)
  val create :
    loc: location ->
    (Format.formatter -> unit) ->
    t

  (** Like [create], using a [Format.printf]-style format string. *)
  val createf :
    loc: location ->
    ('a, Format.formatter, unit, t) format4
    -> 'a

  (** {2 Accessors} *)

  val location : t -> location
  val report : Format.formatter -> t -> unit

  (** {2 Exceptions}

      TODO: we probably want to cut the entire exceptions section before upstreaming. *)

  (** Add a conversion for a new exception constructor. *)
  val register_of_exn : (exn -> t option) -> unit

  (** Extract a [t] from an exception using registered conversions. *)
  val of_exn : exn -> t option

  (** Used to raise/catch [t] as an exception. *)
  exception Error of t

  (** Like [createf]. Raises [Error t] *)
  val raisef :
    loc: location ->
    ('a, Format.formatter, unit, _) format4
    -> 'a
end
