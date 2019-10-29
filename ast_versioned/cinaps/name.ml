open StdLabels

let rec add_ty_suffix ty acc =
  match (ty : Astlib.Grammar.ty) with
  | Var var -> var :: acc
  | Name name -> name :: acc
  | Instance (poly, args) -> add_tuple_suffix args (poly :: acc)
  | Bool -> "bool" :: acc
  | Char -> "char" :: acc
  | Int -> "int" :: acc
  | String -> "string" :: acc
  | Location -> "location" :: acc
  | Loc ty -> add_ty_suffix ty ("loc" :: acc)
  | List ty -> add_ty_suffix ty ("list" :: acc)
  | Option ty -> add_ty_suffix ty ("option" :: acc)
  | Tuple tuple -> add_tuple_suffix tuple acc

and add_tuple_suffix tuple acc = List.fold_right tuple ~init:acc ~f:add_ty_suffix

let builtins =
  [ "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done"; "downto"
  ; "else"; "end"; "exception"; "external"; "false"; "for"; "fun"; "function"; "functor"
  ; "if"; "in"; "include"; "inherit"; "initializer"; "land"; "lazy"; "let"; "lor"; "lsl"
  ; "lsr"; "lxor"; "match"; "method"; "mod"; "module"; "mutable"; "new"; "nonrec"
  ; "object"; "of"; "open"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"
  ; "try"; "type"; "val"; "virtual"; "when"; "while"; "with"
  ]

let fix_builtin name =
  if List.mem name ~set:builtins
  then name ^ "_"
  else name

let make prefix tys =
  fix_builtin
    (String.concat ~sep:"_"
       (List.map ~f:Ml.id
          (prefix @ add_tuple_suffix tys [])))
