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

(** It is expected that [versioned_grammars] are provided consecutively in order, and that
    [conversions] contains exactly one up-conversion and one down-conversion for each
    consecutive pair of versions. Raises otherwise. *)
val create
  :  versioned_grammars : (string * Grammar.t) list
  -> conversions : conversion list
  -> t
