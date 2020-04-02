open StdLabels

let name_of_targ targ =
  match (targ : Astlib.Grammar.targ) with
  | Tname name -> name
  | Tvar var -> var

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

let make prefix targs =
  fix_builtin
    (String.concat ~sep:"_"
       (List.map ~f:Ml.id
          (prefix @ List.map targs ~f:name_of_targ)))
