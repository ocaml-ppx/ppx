open Stdppx
open Ppx_ast.V4_07

type field = Longident_loc.t * Pattern.t

let fields_from_pattern pattern =
  let ppat_loc, ppat_desc = Deconstructor.pattern pattern in
  match ppat_desc with
  | Ppat_record (fields, _closed) -> Ok fields
  | _ -> Error (Error.invalid_attribute_payload ~loc:ppat_loc)

let fields_from_payload ~loc payload =
  match Payload.to_concrete payload with
  | PSig _ | PTyp _ | PStr _ | PPat (_, Some _) ->
    Error (Error.invalid_attribute_payload ~loc)
  | PPat (pattern, None) ->
    fields_from_pattern pattern

let fields_from_attribute attribute =
  let open Result.O in
  match Attribute.to_concrete attribute with
  | { loc; txt = name }, payload ->
    match name with
    | "view" -> fields_from_payload ~loc payload >>| Option.some
    | _ -> Ok None

let extract_fields attributes =
  let attributes = Attributes.to_concrete attributes in
  List.find_map attributes
    ~f:( fun attr ->
      match fields_from_attribute attr with
      | Error _ as r -> Some r
      | Ok (Some _) as r -> Some r
      | Ok None -> None )
  |> Option.value ~default:(Ok None)
