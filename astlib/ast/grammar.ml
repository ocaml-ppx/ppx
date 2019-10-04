type structural =
  | Var of string
  | Inst of inst
  | Name of string
  | Bool
  | Int
  | Char
  | String
  | Location
  | List of structural
  | Option of structural
  | Tuple of tuple

and tuple = structural list

and inst =
  { poly : string
  ; args : structural list
  }

type record = (string * structural) list

type clause =
  | Empty
  | Tuple of tuple
  | Record of record

type variant = (string * clause) list

type nominal =
  | Alias of structural
  | Record of record
  | Variant of variant

type decl =
  { vars : string list
  ; body : nominal
  }

type t = (string * decl) list
