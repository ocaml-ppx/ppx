open Stdppx

let string_of_name name = Ml.id name

let string_of_var ~nodify var =
  if nodify then
    Ml.poly_inst "node" ~args:[Ml.tvar var]
  else
    Ml.tvar var

let string_of_targ ~nodify targ =
  match (targ : Astlib.Grammar.targ) with
  | Tname name -> string_of_name name
  | Tvar var -> string_of_var ~nodify var

let rec string_of_ty ~nodify ty =
  match (ty : Astlib.Grammar.ty) with
  | Var var -> string_of_var ~nodify var
  | Name name -> string_of_name name
  | Bool -> "bool"
  | Char -> "char"
  | Int -> "int"
  | String -> "string"
  | Location -> "Astlib.Location.t"
  | Loc ty -> string_of_ty ~nodify ty ^ " Astlib.Loc.t"
  | List ty -> string_of_ty ~nodify ty ^ " list"
  | Option ty -> string_of_ty ~nodify ty ^ " option"
  | Tuple tuple -> string_of_tuple_type ~nodify ~parens:true tuple
  | Instance (poly, args) ->
    Ml.poly_inst (string_of_name poly) ~args:(List.map args ~f:(string_of_targ ~nodify))

and string_of_tuple_type ~nodify ~parens tuple =
  let inner = Ml.tuple_type (List.map tuple ~f:(string_of_ty ~nodify)) in
  if parens
  then Printf.sprintf "(%s)" inner
  else inner
