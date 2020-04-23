open! Base

type t = Astlib.Position.t =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
[@@deriving compare, equal, hash, sexp_of]
