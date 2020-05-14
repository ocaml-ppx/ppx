open Import
open Current_ast

type t =
  { mutable next_id : int
  ; mutable bindings : value_binding list
  }

let create () =
  { next_id = 0
  ; bindings = []
  }

let sanitize t e =
  match t.bindings with
  | [] -> e
  | bindings ->
    pexp_let ~loc:(Expression.pexp_loc e) Rec_flag.recursive bindings e

let quote t (e : expression) =
  let loc = (Expression.pexp_loc e) in
  let name = "__" ^ Int.to_string t.next_id in
  let binding =
    let pat = Ast_builder.pvar ~loc name in
    let expr =
      pexp_fun ~loc Arg_label.nolabel None
        (let unit = Ast_builder.Located.lident ~loc "()" in
         ppat_construct ~loc unit None)
        e
    in
    value_binding ~loc ~pat ~expr
  in
  t.bindings <- binding :: t.bindings;
  t.next_id <- t.next_id + 1;
  Ast_builder.evar ~loc name
