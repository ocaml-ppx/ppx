let ocaml_keywords = [
  "and"; "as"; "assert"; "asr";
  "begin";
  "class"; "constraint";
  "do"; "done"; "downto";
  "else"; "end"; "exception"; "external";
  "false"; "for"; "fun"; "function"; "functor";
  "if"; "in"; "include"; "inherit"; "initializer";
  "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor";
  "match"; "method"; "mod"; "module"; "mutable";
  "new"; "nonrec";
  "object"; "of"; "open"; "or";
  "private";
  "rec";
  "sig"; "struct";
  "then"; "to"; "true"; "try"; "type";
  "val"; "virtual";
  "when"; "while"; "with";
]

let safe_ident str =
  if List.mem str ocaml_keywords then
    str ^ "_"
  else
    str

let safe_uncapitalize str =
  safe_ident (String.uncapitalize_ascii str)

let starts_with ~prefix str =
  let len_prefix = String.length prefix in
  let len_str    = String.length str in
  if len_str >= len_prefix then begin
    let idx = ref 0 in
    while (!idx < len_prefix) && (prefix.[!idx] = str.[!idx]) do
      incr idx
    done;
    !idx = len_prefix
  end else
    false

let ends_with ~suffix str =
  let len_suffix = String.length suffix in
  let len_str    = String.length str in
  if len_str >= len_suffix then begin
    let idx = ref (pred len_suffix) in
    while (!idx >= 0) && (suffix.[!idx] = str.[!idx + (len_str - len_suffix)]) do
      decr idx
    done;
    !idx < 0
  end else
    false
