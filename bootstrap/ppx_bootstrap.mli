open! Stdppx

module Expected : sig
  exception Expected of Astlib.Location.t * string

  val raise_ : loc: Astlib.Location.t -> string -> 'a
end

module Extension : sig
  type 'a entry =
    { name : string
    ; callback : (loc:Astlib.Location.t -> Ppx_ast.payload -> 'a)
    }

  type t =
    | Patt of Ppx_ast.pattern    entry
    | Expr of Ppx_ast.expression entry
end

val single_expression_payload :
  Ppx_ast.payload -> (Ppx_ast.expression * Ppx_ast.attributes) option

val single_structure_item_payload :
  Ppx_ast.payload -> Ppx_ast.structure_item option

val single_signature_item_payload :
  Ppx_ast.payload -> Ppx_ast.signature_item option
