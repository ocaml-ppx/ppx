open! Base
open Base_quickcheck.Export
open Ocaml_common

type position = Lexing.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
[@@deriving compare, equal, hash, quickcheck, sexp_of]

type t = Location.t =
  { loc_start : position
  ; loc_end : position
  ; loc_ghost : bool
  }
[@@deriving compare, equal, hash, quickcheck, sexp_of]
