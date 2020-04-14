let errorf ~loc fmt =
  Ocaml_common.Location.raise_errorf ~loc
    ("ppx_view: " ^^ fmt)

let invalid_payload ~loc =
  errorf ~loc "Invalid payload, should be a 'match' or a 'function'"

let invalid_attribute_payload ~loc =
  errorf ~loc
    "Invalid @view attribute payload, should be a record pattern"

let unsupported_pattern ~loc pattern =
  errorf ~loc "ppx_view doesn't support pattern matching over %s" pattern

let or_pattern_variables_differ ~loc =
  errorf ~loc
    "Branches of an or-pattern should define the same variables \
     in the same order"

let invalid_record_field ~loc =
  errorf ~loc "Invalid record field, should be unqualified"

let unsupported_num_const ~loc ~kind ~suffix =
  errorf ~loc "Unsupported %s litteral suffix %C" kind suffix
