(* When upstreaming this, we need to make sure it's built from the lexer's
   keyword_table rather than duplicated. *)
let keywords =
  [ "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done"
  ; "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun"
  ; "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer"
  ; "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"
  ; "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "or"
  ; "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"
  ; "val"; "virtual"; "when"; "while"; "with" ]
