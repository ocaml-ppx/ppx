val payload :
  loc: Astlib.Location.t ->
  Ppx_ast.V4_07.Expression.t ->
  Ppx_ast.V4_07.Expression.t

val parsetree_payload :
  loc: Ocaml_common.Location.t ->
  Ppx_ast_deprecated.Ast.expression ->
  Ppx_ast_deprecated.Ast.expression
