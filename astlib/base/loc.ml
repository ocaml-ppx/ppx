open! Base

type 'a t = 'a Astlib.Loc.t =
  { txt : 'a
  ; loc : Location.t
  }
[@@deriving compare, equal, hash, sexp_of]
