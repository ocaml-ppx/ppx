open Stdppx

type t =
  { outer_record : string
  ; inner_variant : string
  ; desc_field : string
  ; attr_field : string option
  ; loc_field : string option
  ; loc_stack_field : string option
  ; other_fields : (string * Astlib.Grammar.ty) list
  }

type shortcut = t

let find_field ~suffix record : (string * Astlib.Grammar.ty) option =
  List.find record
    ~f:(fun (field_name, _) -> String.is_suffix ~suffix field_name)

let loc_suffix = "_loc"
let desc_suffix = "_desc"
let attr_suffix = "_attributes"
let loc_stack_suffix = "_loc_stack"

let desc_field record = find_field ~suffix:desc_suffix record

let attr_field record =
  match find_field ~suffix:attr_suffix record with
  | None -> None
  | Some (attr_field, Name "attributes") -> Some attr_field
  | Some (_, _) -> assert false

let loc_field record =
  match find_field ~suffix:loc_suffix record with
  | None -> None
  | Some (loc_field, Location) -> Some loc_field
  | Some (_, _) -> assert false

let loc_stack_field record =
  match find_field ~suffix:loc_stack_suffix record with
  | None -> None
  | Some (loc_stack_field, List Location) -> Some loc_stack_field
  | Some (_, _) -> assert false

let other_fields record =
  List.filter record
    ~f:(fun (field_name, _) ->
      not
        ( String.is_suffix ~suffix:loc_suffix field_name
          || String.is_suffix ~suffix:desc_suffix field_name
          || String.is_suffix ~suffix:attr_suffix field_name
          || String.is_suffix ~suffix:loc_stack_suffix field_name))

let from_record ~name record =
  match desc_field record with
  | None -> None
  | Some (desc_field, Name inner_variant) ->
    let loc_field = loc_field record in
    let attr_field = attr_field record in
    let loc_stack_field = loc_stack_field record in
    let other_fields = other_fields record in
    Some
      { outer_record = name; inner_variant
      ; desc_field; loc_field; attr_field; loc_stack_field; other_fields }
  | Some (_, _) ->
    assert false

module Map = struct
  type t = shortcut String.Map.t

  let from_grammar grammar =
    List.fold_left grammar
      ~init:String.Map.empty
      ~f:(fun acc (name, kind) ->
        match (kind : Astlib.Grammar.kind) with
        | Poly (_, _) -> acc
        | Mono decl ->
          match decl with
          | Ty _ | Variant _ -> acc
          | Record record ->
            match from_record ~name record with
            | None -> acc
            | Some ({outer_record; inner_variant; _} as shortcut) ->
              let acc = String.Map.add acc outer_record shortcut in
              String.Map.add acc inner_variant shortcut)

  let find t type_name =
    String.Map.find t type_name
end
