open Ppxlib

let name = "view"

let extension =
  Extension.V3.declare
    name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~ctxt payload ->
       let loc = Expansion_context.Extension.extension_point_loc ctxt in
       Ppx_ast.Conversion.ast_of_expression payload
       |> Ppx_view_lib.Expand.payload ~loc
       |> Ppx_ast.Conversion.ast_to_expression)

let rule = Context_free.Rule.extension extension

let () =
  Driver.register_transformation
    ~rules:[rule]
    name
