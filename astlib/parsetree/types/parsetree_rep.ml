type 'a ty =
  | Bool
  | Char
  | String
  | Location
  | A of 'a
  | T of string
  | List of 'a ty
  | Option of 'a ty

type 'a rep =
  | Alias of 'a ty
  | Tuple of 'a ty list
  | Record of { source : string; fields : (string * 'a ty) list }
  | Variant of { source : string; clauses : (string * 'a ty list) list }

type nothing = |

type decl =
  | Mono of nothing rep
  | Poly of unit rep
  | Inst of { poly : string; arg : nothing ty }

type t = (string * decl) list
