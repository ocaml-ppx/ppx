open Stdppx
open Ppx_ast.V4_07

type field = Longident_loc.t * Pattern.t

let fields_from_pattern pattern =
  let ppat_loc, ppat_desc = Deconstructor.pattern pattern in
  match ppat_desc with
  | Ppat_record (fields, _closed) -> fields
  | _ -> Error.invalid_attribute_payload ~loc:ppat_loc

let fields_from_payload ~loc payload =
  match Payload.to_concrete payload with
  | PSig _ | PTyp _ | PStr _ | PPat (_, Some _) ->
    Error.invalid_attribute_payload ~loc
  | PPat (pattern, None) ->
    fields_from_pattern pattern

let fields_from_attribute attribute =
  match Attribute.to_concrete attribute with
  | { loc; txt = name }, payload ->
    match name with
    | "view" -> Some (fields_from_payload ~loc payload)
    | _ -> None

let extract_fields attributes =
  let attributes = Attributes.to_concrete attributes in
  List.find_map attributes ~f:fields_from_attribute
