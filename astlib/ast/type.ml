type 'var structural =
  | Var of 'var
  | Inst of 'var inst
  | Named of string
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
  { inst_poly : string
  ; inst_args : 'var structural list
  }

type 'var field =
  { field_name : string
  ; field_type : 'var structural
  }

type 'var record = 'var field list

type 'var clause =
  | Inline_tuple of 'var tuple
  | Inline_record of 'var record

type 'var variant = 'var clause list

type 'var nominal =
  | Alias of 'var structural
  | Record of 'var record
  | Variant of 'var variant

type 'var named =
  { decl_name : string
  ; decl_vars : 'var list
  ; decl_body : 'var nominal
  }

type nothing = |

type decl =
  | Poly of string named
  | Mono of nothing named

type t = decl list
