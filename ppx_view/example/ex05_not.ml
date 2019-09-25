open Migrate_parsetree.Ast_407


let print_desc : string -> Parsetree.expression -> unit = fun value expr ->
  let open Viewast.Parseview in
  match%view expr with
  | Pexp_constant (Not (Pconst_integer _)) ->
    Printf.printf "%S is not an integer\n" value
  | Pexp_constant (Pconst_integer _) ->
    Printf.printf "%S is an integer\n" value
  | _ ->
    Printf.printf "%S is ???\n" value

let print_desc' : string -> Parsetree.expression -> unit = fun value expr ->
  let open Viewast.Parseview in
  match%view expr with
  | Pexp_constant (Not (Pconst_integer _ | Pconst_float _)) ->
    Printf.printf "%S is not a number\n" value
  | Pexp_constant (Pconst_integer _) ->
    Printf.printf "%S is an integer\n" value
  | Pexp_constant (Pconst_float _) ->
    Printf.printf "%S is a float\n" value
  | _ ->
    Printf.printf "%S is ???\n" value

let print_descs value expr =
  print_desc  value expr;
  print_desc' value expr


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
  let abc =
    Ast_helper.Exp.constant
      (Ast_helper.Const.string "abc")
  in
  let pi =
    Ast_helper.Exp.constant
      (Ast_helper.Const.float "3.14")
  in
  print_descs "1"    one;
  print_descs "3"    three_int;
  print_descs "3l"   three_int32;
  print_descs "3.14" pi;
  print_descs "abc"  abc;
  ()
