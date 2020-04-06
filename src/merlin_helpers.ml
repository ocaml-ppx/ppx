open! Import

let mknoloc txt : _ Loc.t = { txt; loc = Location.none }

let hide_attribute =
  Ast.Attribute.create (mknoloc "merlin.hide", Payload.pStr (Structure.create []))

let focus_attribute =
  Ast.Attribute.create (mknoloc "merlin.focus", Payload.pStr (Structure.create []))

let cons attr attributes =
  Attributes.create (attr :: Attributes.to_concrete attributes)

let hide_pattern p =
  Pattern.update p ~ppat_attributes:(cons hide_attribute (Pattern.ppat_attributes p))
let focus_pattern p =
  Pattern.update p ~ppat_attributes:(cons focus_attribute (Pattern.ppat_attributes p))

let hide_expression e =
  Expression.update e ~pexp_attributes:(cons hide_attribute (Expression.pexp_attributes e))
let focus_expression e =
  Expression.update e ~pexp_attributes:(cons focus_attribute (Expression.pexp_attributes e))
