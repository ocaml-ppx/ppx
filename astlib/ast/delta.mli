type ('edit, 'self) t =
  | Remove of string
  | Insert of int * 'self
  | Modify of string * 'edit

type nothing = |

type field = (Type.data, Type.field) t
type clause = (field list, Type.clause) t
type kind = (clause list, Type.kind) t
type grammar = kind list

val apply_to_grammar : grammar -> Type.t -> Type.t
