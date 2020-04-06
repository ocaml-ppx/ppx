(** Overrides the Location module of OCaml *)

(** There are less functions in this module. However the API should be more stable than
    the Location module of OCaml. *)

open! Import

type t = Astlib.Location.t = {
  loc_start : Astlib.Position.t;
  loc_end : Astlib.Position.t;
  loc_ghost : bool;
}

(** Return an empty ghost range located in a given file. *)
val in_file : string -> t

(** An arbitrary value of type [t]; describes an empty ghost range. *)
val none : t

(** Raise a located error. The exception is caught by driver and handled
    appropriately *)
val raise_errorf : ?loc:t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** Return the location corresponding to the last matched regular expression *)
val of_lexbuf : Lexing.lexbuf -> t

(** Report an exception on the given formatter *)
val report_exception : Format.formatter -> exn -> unit

(** Prints [File "...", line ..., characters ...-...:] *)
val print : Format.formatter -> t -> unit

type 'a loc = 'a Astlib.Loc.t

module Error : sig
  type location = t
  type t = Astlib.Location.Error.t

  val createf : loc:location -> ('a, Format.formatter, unit, t) format4 -> 'a

  (** Register an exception handler. Exception registered this way will be properly
      displayed by [report_exception]. *)
  val register_of_exn: (exn -> t option) -> unit

  val of_exn : exn -> t option

  val to_extension : t -> extension
end with type location := t

exception Error of Error.t
