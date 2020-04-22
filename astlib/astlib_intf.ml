module type History = sig
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
    -> to_node:(version:Version.t -> 'a -> 'a Ast.node)
    -> of_node:(version:Version.t -> 'a Ast.node -> 'a)
    -> 'a Ast.node
end

module type Astlib = sig
  module Ast = Ast
  module Grammar = Grammar
  module History : History
  module Loc = Loc
  module Location = Location
  module Position = Position
  module Syntax = Syntax
  module Version = Version

  val current_version : Version.t
  val history : History.t
end
