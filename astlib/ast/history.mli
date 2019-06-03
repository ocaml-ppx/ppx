type t

type conversion =
  { name : string
  ; f :
      'a . 'a Ast.node
      -> to_node:('a -> 'a Ast.node)
      -> of_node:('a Ast.node -> 'a)
      -> 'a Ast.node
  }

type previous_version =
  { version : string
  ; next_version : string
  ; delta_from_next : Delta.grammar
  ; to_next : conversion
  ; of_next : conversion
  }

val create
  :  current_version:string
  -> current_grammar:Grammar.t
  -> previous_versions:previous_version list
  -> t

val to_versioned_grammars : t -> (string * Grammar.t) list

type conversion_step =
  { src_version : string
  ; dst_version : string
  ; conversion : conversion
  }

val conversion_steps
  :  t
  -> from_version:string
  -> to_version:string
  -> conversion_step list option
