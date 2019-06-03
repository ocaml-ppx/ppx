type ('edit, 'self) t =
  | Remove of string
  | Insert of int * 'self
  | Modify of string * 'edit

type nothing = |

type field = (Grammar.data, Grammar.field) t
type clause = (field list, Grammar.clause) t
type kind = (clause list, Grammar.kind) t
type grammar = kind list

val apply_to_grammar : grammar -> Grammar.t -> Grammar.t
