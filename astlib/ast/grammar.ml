type 'var structural =
  | Var of 'var
  | Inst of 'var inst
  | Name of string
  | Bool
  | Int
  | Char
  | String
  | Location
  | List of 'var structural
  | Option of 'var structural
  | Tuple of 'var tuple

and 'var tuple = 'var structural list

and 'var inst =
  { poly : string
  ; args : 'var structural list
  }

type 'var record = (string * 'var structural) list

type 'var clause =
  | Tuple of 'var tuple
  | Record of 'var record

type 'var variant = (string * 'var clause) list

type 'var nominal =
  | Alias of 'var structural
  | Record of 'var record
  | Variant of 'var variant

type nothing = |

type mono = nothing nominal

type poly =
  { vars : string list
  ; body : string nominal
  }

type decl =
  | Poly of poly
  | Mono of mono

type t = (string * decl) list
