type t

val versioned_grammars : t -> (Version.t * Grammar.t) list

val find_grammar : t -> version:Version.t -> Grammar.t

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
