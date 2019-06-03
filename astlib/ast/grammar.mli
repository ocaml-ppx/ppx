type kind =
  { kind_name : string
  ; clauses   : clause list
  }

and clause =
  { clause_name : string
  ; fields      : field list
  }

and field =
  { field_name : string
  ; data       : data
  }

and data =
  | Kind of string
  | Bool
  | Char
  | String
  | List   of data
  | Option of data

type t = kind list
