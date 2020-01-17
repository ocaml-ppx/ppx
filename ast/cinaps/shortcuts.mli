(** Type for shortcuts from description types to their parent record types.
    E.g. from [expression_desc] to [expression]. *)
type t

(** Returns true if the given record has a `_desc` field, ie if it is at the
    end of a shortcut. *)
val has_desc_field : Astlib.Grammar.record -> bool

(** Returns the shortcuts for the given grammar. *)
val from_grammar : Astlib.Grammar.t -> t

(** Returns the shortcut for the given type, if any. *)
val shortcut : t -> string -> (string * Astlib.Grammar.record) option
