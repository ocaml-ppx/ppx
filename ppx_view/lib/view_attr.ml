open Stdppx
open Ppx_ast.V4_07

type field =
  { label : string
  ; label_loc : Astlib.Location.t
  ; var : string
  ; var_loc : Astlib.Location.t
  }

let to_field ~loc (label, value) =
  let label_loc, ident = Deconstructor.longident_loc ~loc label in
  let pexp_loc, pexp_desc = Deconstructor.expression ~loc value in
  match ident, pexp_desc with
  | Lident label, Pexp_ident var_ident ->
    let var_loc, ident =
      Deconstructor.longident_loc ~loc:pexp_loc var_ident
    in
    (match ident with
     | Lident var -> {label; label_loc; var; var_loc}
     | _ -> Error.invalid_attribute_payload ~loc:var_loc)
  | (Ldot _ | Lapply _), _ -> Error.invalid_attribute_payload ~loc:label_loc
  | _, _ -> Error.invalid_attribute_payload ~loc:pexp_loc

let fields_from_expr ~loc expr =
  let pexp_loc, pexp_desc = Deconstructor.expression ~loc expr in
  match pexp_desc with
  | Pexp_record (fields, None) ->
    let fields = List.map ~f:(to_field ~loc:pexp_loc) fields in
    Some fields
  | _ -> Error.invalid_attribute_payload ~loc:pexp_loc

let fields_from_stri ~loc stri =
  match Structure_item.to_concrete stri with
  | None -> Error.conversion_failed ~loc "structure_item"
  | Some {pstr_loc; pstr_desc} ->
    match Structure_item_desc.to_concrete pstr_desc with
    | None -> Error.conversion_failed ~loc "structure_item_desc"
    | Some (Pstr_eval (expr, _attributes)) ->
      fields_from_expr ~loc:pstr_loc expr
    | _ -> Error.invalid_attribute_payload ~loc:pstr_loc

let fields_from_payload ~loc payload =
  match Payload.to_concrete payload with
  | None -> Error.conversion_failed ~loc "payload"
  | Some (PSig _ | PTyp _ | PPat _) -> None
  (* Shouldn't the paylaod be a pattern? so that one could write
     [[@view? {pexp_attributes = []; _}]] for instance? Even
     if we don't allow further deconstruction, I still feel like
     a pattern would be better suited. *)
  | Some (PStr structure) ->
    match Structure.to_concrete structure with
    | None -> Error.conversion_failed ~loc "structure"
    | Some [stri] -> fields_from_stri ~loc stri
    | Some _ -> Error.invalid_attribute_payload ~loc

let fields_from_attribute ~loc attribute =
  match Attribute.to_concrete attribute with
  | None -> Error.conversion_failed ~loc "attribute"
  | Some (name, payload) ->
    let loc = Astlib.Loc.loc name in
    let name = Astlib.Loc.txt name in
    match name with
    | "view" -> fields_from_payload ~loc payload
    | _ -> None

let extract_fields ~err_loc:loc attributes =
  match Attributes.to_concrete attributes with
  | None -> Error.conversion_failed ~loc "attributes"
  | Some attributes ->
    List.find_map attributes ~f:(fields_from_attribute ~loc)
