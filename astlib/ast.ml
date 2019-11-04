type 'a data =
  | Node of 'a
  | Bool of bool
  | Char of char
  | Int of int
  | String of string
  | Location of Location.t
  | Loc of 'a data Loc.t
  | List of 'a data list
  | Option of 'a data option
  | Tuple of 'a data array
  | Record of 'a data array
  | Variant of { tag : string; args : 'a data array }

type 'a node = { name : string; data : 'a data }
