open Stdppx

let string_of_name ~internal name =
  if internal then Ml.id name else Ml.module_name name ^ ".t"

let string_of_var ~nodify var =
  if nodify then
    Ml.poly_inst "node" ~args:[Ml.tvar var]
  else
    Ml.tvar var

let string_of_targ ?(nodify=false) ~internal targ =
  match (targ : Astlib.Grammar.targ) with
  | Tname name -> string_of_name ~internal name
  | Tvar var -> string_of_var ~nodify var

let rec string_of_ty ?(nodify=false) ~internal ty =
  match (ty : Astlib.Grammar.ty) with
  | Var var -> string_of_var ~nodify var
  | Name name -> string_of_name ~internal name
  | Bool -> "bool"
  | Char -> "char"
  | Int -> "int"
  | String -> "string"
  | Location -> "Astlib.Location.t"
  | Loc ty -> string_of_ty ~nodify ~internal ty ^ " Astlib.Loc.t"
  | List ty -> string_of_ty ~nodify ~internal ty ^ " list"
  | Option ty -> string_of_ty ~nodify ~internal ty ^ " option"
  | Tuple tuple -> string_of_tuple_type ~nodify ~internal tuple
  | Instance (poly, args) ->
    let name = if poly = "node" || internal then poly else poly ^ ".t" in
    Ml.poly_inst name ~args:(List.map args ~f:(string_of_targ ~nodify ~internal))

and string_of_tuple_type ?nodify ~internal ?(parens = true) tuple =
  Printf.sprintf "%s%s%s"
    (if parens then "(" else "")
    (Ml.tuple_type (List.map tuple ~f:(string_of_ty ?nodify ~internal)))
    (if parens then ")" else "")
