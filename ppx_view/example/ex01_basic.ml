open Migrate_parsetree.Ast_407


let rec desc : Parsetree.expression -> string = fun expr ->
  let open Viewast.Parseview in
  match%view expr with
  | Pexp_constant (Pconst_integer ("3", Some 'l')) ->
    "3 : int32"
  | Pexp_constant (Pconst_integer ("3", None)) ->
    "3 : int"
  | Pexp_constant (Pconst_integer (_, _)) ->
    "? : integer"
  | (Pexp_constant (Pconst_string ("def" as str, _)))[@view { pexp_attributes; }] ->
    let open Location in
    begin match pexp_attributes with
    | (attr, _) :: _ ->
      Printf.sprintf "%S[@%s ...] : string"
        str
        attr.txt
    | [] ->
      Printf.sprintf "%S : string"
        str
    end
  | (Pexp_constant (Pconst_string _))[@view { pexp_attributes; pexp_loc; }] ->
    ignore (pexp_attributes : Parsetree.attributes);
    ignore (pexp_loc : Location.t);
    "? : string"
  | Pexp_constant (Pconst_float (("3.14" | "3.1415"), None)) ->
    "pi : float"
  | Pexp_constant (Pconst_float (value, None)) when value.[0] = '-' ->
    "? : negative float"
  | Pexp_constant (Pconst_float (_, None)) ->
    "? : positive float"
  | (Pexp_ident { txt = Lident ident; _ }) ->
    ident
  | Pexp_construct ({ txt; _ }, None) ->
    (Longident.last txt) ^ " : <<constructor>>"
  | Pexp_construct ({ txt; _ }, Some (Pexp_tuple _)) ->
    (Longident.last txt) ^ " (,,,) : <<constructor>>"
  | Pexp_construct ({ txt; _ }, Some x) ->
    (Longident.last txt) ^ " (" ^ (desc x) ^ ") : <<constructor>>"
  | _ ->
    "???"


let run () =
  let one =
    Ast_helper.Exp.constant
      (Ast_helper.Const.int 1)
  in
  let three_int32 =
    Ast_helper.Exp.constant
      (Ast_helper.Const.int32 3l)
  in
  let three_int =
    Ast_helper.Exp.constant
      (Ast_helper.Const.int 3)
  in
  let pos = { Lexing.pos_fname = "filename.ml";
              pos_lnum = 11;
              pos_bol = 22;
              pos_cnum = 33; } in
  let abc =
    Ast_helper.Exp.constant
      ~loc:{ Location.loc_start = pos; loc_end = pos; loc_ghost = false; }
      (Ast_helper.Const.string "abc")
  in
  let def =
    Ast_helper.Exp.constant
      ~attrs:[{ txt = "my-attribute"; loc = Location.none; },
              PStr []]
      (Ast_helper.Const.string "def")
  in
  let pi =
    Ast_helper.Exp.constant
      (Ast_helper.Const.float "3.14")
  in
  let pi' =
    Ast_helper.Exp.constant
      (Ast_helper.Const.float "3.1415")
  in
  let minus_1 =
    Ast_helper.Exp.constant
      (Ast_helper.Const.float "-1")
  in
  let plus_1 =
    Ast_helper.Exp.constant
      (Ast_helper.Const.float "+1")
  in
  let x =
    Ast_helper.Exp.ident
      { txt = Lident "x"; loc = Location.none; }
  in
  let none =
    Ast_helper.Exp.construct
      { txt = Lident "None"; loc = Location.none; }
      None
  in
  let some_one =
    Ast_helper.Exp.construct
      { txt = Lident "Some"; loc = Location.none; }
      (Some one)
  in
  let one_three =
    Ast_helper.Exp.tuple [one; three_int]
  in
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
  let abc_def =
    cons abc (cons def nil)
  in
  Printf.printf "desc 1          ~> %S\n%!"   (desc one);
  Printf.printf "desc 3          ~> %S\n%!"   (desc three_int);
  Printf.printf "desc 3l         ~> %S\n%!"   (desc three_int32);
  Printf.printf "desc 3.14       ~> %S\n%!"   (desc pi);
  Printf.printf "desc 3.1415     ~> %S\n%!"   (desc pi');
  Printf.printf "desc -1         ~> %S\n%!"   (desc minus_1);
  Printf.printf "desc +1         ~> %S\n%!"   (desc plus_1);
  Printf.printf "desc \"abc\"      ~> %S\n%!" (desc abc);
  Printf.printf "desc \"def\"      ~> %S\n%!" (desc def);
  Printf.printf "desc x          ~> %S\n%!"   (desc x);
  Printf.printf "desc None       ~> %S\n%!"   (desc none);
  Printf.printf "desc Some 1     ~> %S\n%!"   (desc some_one);
  Printf.printf "desc 1, 3       ~> %S\n%!"   (desc one_three);
  Printf.printf "desc [abc; def] ~> %S\n%!"   (desc abc_def);
  ()
