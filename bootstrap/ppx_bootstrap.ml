open! Stdppx
open Ppx_ast.V4_07

module Expected = struct
  exception Expected of Astlib.Location.t * string

  let raise_ ~loc msg = raise (Expected (loc, msg))
end

module Extension = struct
  type 'a entry =
    { name : string
    ; callback : (loc:Astlib.Location.t -> Ppx_ast.payload -> 'a)
    }

  type t =
    | Patt of Ppx_ast.pattern    entry
    | Expr of Ppx_ast.expression entry
end

let single_signature_item_payload payload =
  match Payload.to_concrete_opt payload with
  | None | Some (PStr _ | PTyp _ | PPat _) -> None
  | Some (PSig signature) ->
    match Signature.to_concrete_opt signature with
    | None | Some [] | Some (_ :: _ :: _) -> None
    | Some [item] -> Some item

let single_structure_item_payload payload =
  match Payload.to_concrete_opt payload with
  | None | Some (PSig _ | PTyp _ | PPat _) -> None
  | Some (PStr structure) ->
    match Structure.to_concrete_opt structure with
    | None | Some [] | Some (_ :: _ :: _) -> None
    | Some [item] -> Some item

let single_expression_payload payload =
  match single_structure_item_payload payload with
  | None -> None
  | Some item ->
    match Structure_item.to_concrete_opt item with
    | None -> None
    | Some item ->
      match Structure_item_desc.to_concrete_opt item.pstr_desc with
      | Some Pstr_eval (e, attr) -> Some (e, attr)
      | Some _ | None -> None
