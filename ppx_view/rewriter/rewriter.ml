open Ppx

let name = "view"

let extension =
  Extension.V3.declare
    name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~ctxt payload ->
       let loc = Expansion_context.Extension.extension_point_loc ctxt in
       Ppx_view_lib_deprecated.Expand.parsetree_payload ~loc payload)

let rule = Context_free.Rule.extension extension

let () =
  Driver.register_transformation
    ~rules:[rule]
    name
