(** Type for shortcuts from description types to their parent record types.
    E.g. from [expression_desc] to [expression]. *)
type t

(** Returns true if the given record has a [XXX_desc] field. *)
val has_desc_field : Astlib.Grammar.record -> bool

(** Returns the shortcuts for the given grammar. *)
val from_grammar : Astlib.Grammar.t -> t

(** Returns the shortcut for the given type, if any.
    E.g. [shortcut t "expression_desc"] should return
    [("expression", [("pexp_desc", ...); ("pexp_loc", ...); ...])] *)
val shortcut : t -> string -> (string * Astlib.Grammar.record) option
