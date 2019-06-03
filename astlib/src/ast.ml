open Base

let rec sexp_of_node
          sexp_of_eval
          ({ kind; clause; loc = _; fields } : _ Astlib_ast.Ast.node)
  =
  Sexp.List [Atom kind; Atom clause; sexp_of_list (sexp_of_field sexp_of_eval) fields]

and sexp_of_field sexp_of_eval { name; value } =
  Sexp.List [Atom name; sexp_of_value sexp_of_eval value]

and sexp_of_value sexp_of_eval value =
  match value with
  | Tree x -> sexp_of_eval x
  | Bool x -> sexp_of_bool x
  | Char x -> sexp_of_char x
  | String x -> sexp_of_string x
  | List x -> sexp_of_list (sexp_of_value sexp_of_eval) x
  | Option x -> sexp_of_option (sexp_of_value sexp_of_eval) x

type 'a equal = 'a -> 'a -> bool

let rec equal_node equal_eval (x : _ Astlib_ast.Ast.node) (y : _ Astlib_ast.Ast.node) =
  String.equal x.kind y.kind
  && String.equal x.clause y.clause
  && equal_list (equal_field equal_eval) x.fields y.fields

and equal_field equal_eval x y =
  String.equal x.name y.name
  && equal_value equal_eval x.value y.value

and equal_value equal_eval x y =
  match x, y with
  | Bool x, Bool y -> equal_bool x y
  | Bool _, _ | _, Bool _ -> false
  | Char x, Char y -> equal_char x y
  | Char _, _ | _, Char _ -> false
  | String x, String y -> equal_string x y
  | String _, _ | _, String _ -> false
  | List x, List y -> equal_list (equal_value equal_eval) x y
  | List _, _ | _, List _ -> false
  | Option x, Option y -> equal_option (equal_value equal_eval) x y
  | Option _, _ | _, Option _ -> false
  | Tree x, Tree y -> equal_eval x y
