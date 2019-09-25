open Migrate_parsetree.Ast_407
open Viewast


let twice_mapper =
  let module H = Ast_helper in
  let module M = Ast_mapper in
  let module V = Ast_viewer in
  let super = M.default_mapper in
  let expr self e =
    match%view super.expr self e with
    | V.Exp.Constant (V.Const.String str) ->
      H.Exp.constant (H.Const.string (str ^ str))
    | other ->
      other
  and pat self p =
    match%view super.pat self p with
    | V.Pat.Constant (V.Const.String str) ->
      H.Pat.constant (H.Const.string (str ^ str))
    | other ->
      other
  in
  { super with expr; pat; }

let rec print_string_list expr =
  let open Parseview in
  match%view expr with
  | Pexp_construct ({ txt = Lident "::"; _ },
                    Some (Pexp_tuple [Pexp_constant (Pconst_string (hd, _));
                                      tl])) ->
    print_endline hd;
    print_string_list tl
  | _ ->
    ()

let run () =
  let open Location in
  let open Longident in
  let nil =
    Ast_helper.Exp.construct
      { txt = Lident "[]"; loc = Location.none; }
      None
  in
  let cons hd tl =
    Ast_helper.Exp.construct
      { txt = Lident "::"; loc = Location.none; }
      (Some (Ast_helper.Exp.tuple [hd; tl]))
  in
  let abc =
    Ast_helper.Exp.constant
      (Ast_helper.Const.string "abc")
  in
  let def =
    Ast_helper.Exp.constant
      (Ast_helper.Const.string "def")
  in
  let abc_def =
    cons abc (cons def nil)
  in
  print_string_list (twice_mapper.expr twice_mapper abc_def)
