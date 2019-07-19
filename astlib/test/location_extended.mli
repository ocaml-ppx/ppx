open! Base
open Ocaml_common

type t = Location.t
[@@deriving compare, equal, hash, quickcheck, sexp_of]
