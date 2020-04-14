open Ppx_ast.V4_07

let expression expr =
  let ({pexp_loc; pexp_desc; _} : Expression.concrete) = Expression.to_concrete expr in
  (pexp_loc, Expression_desc.to_concrete pexp_desc)

let pattern pat =
  let ({ppat_loc; ppat_desc; _} : Pattern.concrete) = Pattern.to_concrete pat in
  (ppat_loc, Pattern_desc.to_concrete ppat_desc)

let longident_loc longident_loc =
  let ({ loc; txt = longident } : _ Astlib.Loc.t) =
    Longident_loc.to_concrete longident_loc
  in
  (loc, Longident.to_concrete longident)
