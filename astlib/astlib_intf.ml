module type History = sig
  type t

  val versioned_grammars : t -> (string * Grammar.t) list

  val find_grammar : t -> version:string -> Grammar.t

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

  val current_version : string
  val history : History.t
end
