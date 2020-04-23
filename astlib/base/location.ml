open! Base

type t = Astlib.Location.t =
  { loc_start : Position.t
  ; loc_end : Position.t
  ; loc_ghost : bool
  }
[@@deriving compare, equal, hash, sexp_of]
