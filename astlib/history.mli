(** Represents the history of AST grammars in the OCaml compiler. *)
type t

(** Produces an association list of grammars, paired with their version numbers. *)
val versioned_grammars : t -> (Version.t * Grammar.t) list

(** Produces the grammar corresponding to a given version number. Raises if there is no
    such version in the history. *)
val find_grammar : t -> version:Version.t -> Grammar.t

(** Converts the given AST node from [src_version]'s grammar to [dst_version]'s grammar,
    using the conversion functions stored in [t]. Converts ['a] to traverse and/or
    construct subnodes using [to_node] and [of_node], which may themselves call
    [convert] as appropriate. *)
val convert
  :  t
  -> 'a Ast.node
  -> src_version:Version.t
  -> dst_version:Version.t
  -> to_node:('a -> version:Version.t -> 'a Ast.node)
  -> of_node:('a Ast.node -> version:Version.t -> 'a)
  -> 'a Ast.node

type 'a conversion_function
  = 'a Ast.node
  -> to_node:('a -> version:Version.t -> 'a Ast.node)
  -> of_node:('a Ast.node -> version:Version.t -> 'a)
  -> 'a Ast.node

type conversion =
  { src_version : Version.t
  ; dst_version : Version.t
  ; f : 'a . 'a conversion_function
  }

(** It is expected that [versioned_grammars] are provided consecutively in order, and that
    [conversions] contains exactly one up-conversion and one down-conversion for each
    consecutive pair of versions. Raises otherwise. *)
val create
  :  versioned_grammars : (Version.t * Grammar.t) list
  -> conversions : conversion list
  -> t
