module Expand : sig
  val parsetree_payload :
    loc: Ocaml_common.Location.t ->
    Ppx_ast_deprecated.Ast.expression ->
    Ppx_ast_deprecated.Ast.expression
end
