open Ppx_ast.V4_07

let expression ~loc expr =
  match Expression.to_concrete expr with
  | None -> Error.conversion_failed ~loc "expression"
  | Some {pexp_loc; pexp_desc; _} ->
    match Expression_desc.to_concrete pexp_desc with
    | None -> Error.conversion_failed ~loc:pexp_loc "expression_desc"
    | Some desc -> (pexp_loc, desc)

let pattern ~loc pat =
  match Pattern.to_concrete pat with
  | None -> Error.conversion_failed ~loc "pattern"
  | Some {ppat_loc; ppat_desc; _} ->
    match Pattern_desc.to_concrete ppat_desc with
    | None -> Error.conversion_failed ~loc "pattern_desc"
    | Some desc -> (ppat_loc, desc)

let longident_loc ~loc longident_loc =
  match Longident_loc.to_concrete longident_loc with
  | None -> Error.conversion_failed ~loc "longident_loc"
  | Some { loc; txt = longident } ->
    match Longident.to_concrete longident with
    | None -> Error.conversion_failed ~loc "longident"
    | Some longident -> (loc, longident)
