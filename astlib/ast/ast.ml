open Ocaml_common

type 'a data =
  | Node of 'a
  | Bool of bool
  | Int of int
  | Char of char
  | String of string
  | Location of Location.t
  | List of 'a data list
  | Option of 'a data option
  | Tuple of 'a tuple

and 'a tuple = 'a data list

type 'a record = (string * 'a data) list

type 'a clause =
  | Tuple of 'a tuple
  | Record of 'a record

type 'a variant = string * 'a clause

type 'a constructor =
  | Data of 'a data
  | Record of 'a record
  | Variant of 'a variant

type 'a node =
  { name : string
  ; data : 'a constructor
  }
