(** Overrides the Longident module of OCaml *)

open! Import

type t = longident

include Comparable.S with type t := t

val equal : t -> t -> bool

val lident : string -> t
val ldot : t -> string -> t
val lapply : t -> t -> t

val flatten_exn : t -> string list
val last_exn : t -> string

(** Parses the given string as a longident, properly handling infix operators
    which may contain '.'.
    Note that it does not parse [Lapply _] longidents and will raise
    [Invalid_argument _] if passed values such as ["A(B)"]. *)
val parse : string -> t

val name : t -> string
