(** Represents the history of AST grammars in the OCaml compiler. *)
type t

(** Produces an association list of grammars, paired with their version numbers. *)
val versioned_grammars : t -> (Version.t * Grammar.t) list

(** Produces the grammar corresponding to a given version number. Raises if there is no
    such version in the history. *)
val find_grammar : t -> version:Version.t -> Grammar.t

(** Converts the given AST from [src_version]'s grammar to [dst_version]'s grammar, using
    the conversion functions stored in [t]. Converts ['node] to traverse and/or construct
    subtrees using [to_ast] and [of_ast], which may themselves call [convert] as
    appropriate. *)
val convert
  :  t
  -> 'node Ast.t
  -> src_version:Version.t
  -> dst_version:Version.t
  -> unwrap:(version:Version.t -> 'node -> 'node Ast.t)
  -> wrap:(version:Version.t -> 'node Ast.t -> 'node)
  -> 'node Ast.t

(**/**)

(** Constructors for internal use and for testing. *)

type 'node conversion_function
  = 'node Ast.t
  -> unwrap:('node -> 'node Ast.t)
  -> wrap:('node Ast.t -> 'node)
  -> 'node Ast.t

type conversion =
  { src_version : Version.t
  ; dst_version : Version.t
  ; f : 'node . 'node conversion_function
  }

(** It is expected that [versioned_grammars] are provided consecutively in order, and that
    [conversions] contains exactly one up-conversion and one down-conversion for each
    consecutive pair of versions. Raises otherwise. *)
val create
  :  versioned_grammars : (Version.t * Grammar.t) list
  -> conversions : conversion list
  -> t
