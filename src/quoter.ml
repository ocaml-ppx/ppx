open Import

type t =
  { mutable next_id : int
  ; mutable bindings : Parsetree.value_binding list
  }

let create () =
  { next_id = 0
  ; bindings = []
  }

let sanitize t e =
  match t.bindings with
  | [] -> e
  | bindings ->
    Ast_builder.pexp_let ~loc:e.pexp_loc Recursive bindings e

let quote t (e : expression) =
  let loc = e.pexp_loc in
  let name = "__" ^ Int.to_string t.next_id in
  let binding =
    let pat = Ast_builder.pvar ~loc name in
    let expr =
      Ast_builder.pexp_fun ~loc Nolabel None
        (let unit = Ast_builder.Located.lident ~loc "()" in
         Ast_builder.ppat_construct ~loc unit None)
        e
    in
    Ast_builder.value_binding ~loc ~pat ~expr
  in
  t.bindings <- binding :: t.bindings;
  t.next_id <- t.next_id + 1;
  Ast_builder.evar ~loc name
