open StdLabels

module Input = Astlib_parsetree_types

let version = Input.version

module List = struct
  include List

  let concat_map ~f t =
    concat (map t ~f)

  let filter_map ~f t =
    concat_map t ~f:(fun x ->
      match f x with
      | None -> []
      | Some y -> [y])
end

let field_names =
  List.init ~len:256 ~f:Char.chr
  |> List.filter ~f:(function 'a' .. 'z' -> true | _ -> false)
  |> List.map ~f:(String.make 1)
  |> Array.of_list

let field_name i = Array.get field_names i

let rec substitute_ty ty ~arg =
  match (ty : _ Input.ty) with
  | (Bool | Char | String | Location | T _) as ty -> ty
  | A () -> arg
  | List ty -> List (substitute_ty ty ~arg)
  | Option ty -> Option (substitute_ty ty ~arg)

let substitute_rep rep ~arg : _ Input.rep =
  match (rep : _ Input.rep) with
  | Alias ty -> Alias (substitute_ty ty ~arg)
  | Record { source; fields } ->
    let fields =
      List.map fields ~f:(fun (name, ty) ->
        (name, substitute_ty ty ~arg))
    in
    Record { source; fields }
  | Variant { source; clauses } ->
    let clauses =
      List.map clauses ~f:(fun (name, tys) ->
        (name, List.map tys ~f:(substitute_ty ~arg)))
    in
    Variant { source; clauses }
  | Tuple tys -> Tuple (List.map tys ~f:(substitute_ty ~arg))

let simplify_poly_parsetree parsetree =
  let env =
    List.filter_map parsetree ~f:(fun (name, decl) ->
      match (decl : Input.decl) with
      | Poly rep -> Some (name, rep)
      | Inst _ | Mono _ -> None)
  in
  List.filter_map parsetree ~f:(fun (name, decl) ->
    match (decl : Input.decl) with
    | Mono rep -> Some (name, rep)
    | Poly _ -> None
    | Inst { poly; arg } ->
      match List.assoc_opt poly env with
      | None -> failwith ("unknown polymorphic type " ^ poly ^ " in " ^ name)
      | Some rep -> Some (name, substitute_rep rep ~arg))

let rec data_of_ty ty : Grammar.data =
  match (ty : _ Input.ty) with
  | Bool -> Bool
  | Char -> Char
  | String -> String
  | Location -> Location
  | T name -> Kind (String.capitalize_ascii name)
  | A (_ : Input.nothing) -> .
  | List ty -> List (data_of_ty ty)
  | Option ty -> Option (data_of_ty ty)

let grammar_fields_of_alist alist =
  List.map alist ~f:(fun (field_name, ty) ->
    ({ field_name; data = data_of_ty ty } : Grammar.field))

let grammar_fields_of_list list =
  grammar_fields_of_alist
    (List.mapi list ~f:(fun index ty ->
       (field_name index, ty)))

let record_kind ~name ~fields =
  ({ kind_name = String.capitalize_ascii name
   ; clauses =
       [ { clause_name = String.capitalize_ascii name
         ; fields = grammar_fields_of_alist fields
         }
       ]
   } : Grammar.kind)

let alias_kind ~name ~ty =
  record_kind ~name ~fields:[(field_name 0, ty)]

let tuple_kind ~name ~values =
  let fields =
    List.mapi values ~f:(fun index ty ->
      (field_name index, ty))
  in
  record_kind ~name ~fields

let grammar_clauses_of_alist alist =
  List.map alist ~f:(fun (clause_name, tys) ->
    ({ clause_name; fields = grammar_fields_of_list tys } : Grammar.clause))

let variant_kind ~name ~clauses =
  ({ kind_name = String.capitalize_ascii name
   ; clauses = grammar_clauses_of_alist clauses
   } : Grammar.kind)

let grammar_of_simplified_parsetree parsetree =
  List.map parsetree ~f:(fun (name, rep) ->
    match (rep : _ Input.rep) with
    | Alias ty -> alias_kind ~name ~ty
    | Tuple values -> tuple_kind ~name ~values
    | Record { fields; _ } -> record_kind ~name ~fields
    | Variant { clauses; _ } -> variant_kind ~name ~clauses)

let grammar_of_parsetree parsetree =
  parsetree
  |> simplify_poly_parsetree
  |> grammar_of_simplified_parsetree

let grammar =
  grammar_of_parsetree Input.t
