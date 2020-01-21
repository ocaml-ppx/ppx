open Stdppx

let rec string_of_ty ~internal ty =
  match (ty : Astlib.Grammar.ty) with
  | Var var -> Ml.tvar var
  | Name name -> if internal then Ml.id name else Ml.module_name name ^ ".t"
  | Bool -> "bool"
  | Char -> "char"
  | Int -> "int"
  | String -> "string"
  | Location -> "Astlib.Location.t"
  | Loc ty -> string_of_ty ~internal ty ^ " Astlib.Loc.t"
  | List ty -> string_of_ty ~internal ty ^ " list"
  | Option ty -> string_of_ty ~internal ty ^ " option"
  | Tuple tuple -> string_of_tuple_type ~internal tuple
  | Instance (poly, args) ->
    let name = if poly = "node" || internal then poly else poly ^ ".t" in
    Ml.poly_inst name ~args:(List.map args ~f:(string_of_ty ~internal))

and string_of_tuple_type ~internal ?(parens = true) tuple =
  Printf.sprintf "%s%s%s"
    (if parens then "(" else "")
    (Ml.tuple_type (List.map tuple ~f:(string_of_ty ~internal)))
    (if parens then ")" else "")
