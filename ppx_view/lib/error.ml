let ast_version = "4.07"

let errorf ~loc fmt =
  let loc = Astlib.Location.to_location loc in
  Ocaml_common.Location.raise_errorf ~loc
    ("ppx_view: " ^^ fmt)

let conversion_failed ~loc node_name =
  errorf ~loc "Couldn't convert %s AST node to %s" node_name ast_version

let invalid_payload ~loc =
  errorf ~loc "Invalid payload, should be a 'match' or a 'function'"

let invalid_attribute_payload ~loc =
  errorf ~loc
    "Invalid @view attribute payload, should be a single record expression"

let unsupported_pattern ~loc pattern =
  errorf ~loc "ppx_view doesn't support pattern matching over %s" pattern

let or_pattern_variables_differ ~loc =
  errorf ~loc
    "Branches of an or-pattern should define the same variables \
     in the same order"

let invalid_record_field ~loc =
  errorf ~loc "Invalid record field, should be unqualified"

let unsupported_int_const ~loc suffix =
  errorf ~loc "Unsupported int litteral suffix %C" suffix
