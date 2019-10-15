include Astlib_intf.History

type 'a conversion_function
  = 'a Ast.node
  -> to_node:('a -> version:string -> 'a Ast.node)
  -> of_node:('a Ast.node -> version:string -> 'a)
  -> 'a Ast.node

type conversion =
  { src_version : string
  ; dst_version : string
  ; f : 'a . 'a conversion_function
  }

val create
  :  versioned_grammars : (string * Grammar.t) list
  -> conversions : conversion list
  -> t
