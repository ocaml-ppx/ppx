open Ocaml_common

type 'a node =
  { kind    : string
  ; clause  : string
  ; fields  : 'a field list
  }

and 'a field =
  { name  : string
  ; value : 'a value
  }

and 'a value =
  | Tree     of 'a
  | Bool     of bool
  | Char     of char
  | String   of string
  | Location of Location.t
  | List     of 'a value list
  | Option   of 'a value option
