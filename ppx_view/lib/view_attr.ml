open Stdppx
open Ppx_ast.V4_07

type field = Longident_loc.t * Pattern.t

let fields_from_pattern ~loc pattern =
  let ppat_loc, ppat_desc = Deconstructor.pattern ~loc pattern in
  match ppat_desc with
  | Ppat_record (fields, _closed) -> (ppat_loc, fields)
  | _ -> Error.invalid_attribute_payload ~loc:ppat_loc

let fields_from_payload ~loc payload =
  match Payload.to_concrete payload with
  | None -> Error.conversion_failed ~loc "payload"
  | Some (PSig _ | PTyp _ | PStr _ | PPat (_, Some _)) ->
    Error.invalid_attribute_payload ~loc
  | Some (PPat (pattern, None)) ->
    fields_from_pattern ~loc pattern

let fields_from_attribute ~loc attribute =
  match Attribute.to_concrete attribute with
  | None -> Error.conversion_failed ~loc "attribute"
  | Some (name, payload) ->
    let loc = Astlib.Loc.loc name in
    let name = Astlib.Loc.txt name in
    match name with
    | "view" -> Some (fields_from_payload ~loc payload)
    | _ -> None

let extract_fields ~err_loc:loc attributes =
  match Attributes.to_concrete attributes with
  | None -> Error.conversion_failed ~loc "attributes"
  | Some attributes ->
    List.find_map attributes ~f:(fields_from_attribute ~loc)
