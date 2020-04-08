module Expand = struct
  let parsetree_payload ~loc payload_expr =
    let astlib_payload_expr = Ppx_ast.Conversion.ast_of_expression payload_expr in
    let expanded = Ppx_view_lib.Expand.payload ~loc astlib_payload_expr in
    Ppx_ast.Conversion.ast_to_expression expanded
end
