module type History = sig
  (** Represents the history of AST grammars in the OCaml compiler. *)
  type t

  (** Produces an association list of grammars, paired with their version numbers. *)
  val versioned_grammars : t -> (string * Grammar.t) list

  (** Produces the grammar corresponding to a given version number. Raises if there is no
      such version in the history. *)
  val find_grammar : t -> version:string -> Grammar.t

  (** Converts the given AST node from [src_version]'s grammar to [dst_version]'s grammar,
      using the conversion functions stored in [t]. Converts ['a] to traverse and/or
      construct subnodes using [to_node] and [of_node], which may themselves call
      [convert] as appropriate. *)
  val convert
    :  t
    -> 'a Ast.node
    -> src_version:string
    -> dst_version:string
    -> to_node:('a -> version:string -> 'a Ast.node)
    -> of_node:('a Ast.node -> version:string -> 'a)
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

  val current_version : string
  val history : History.t
end
