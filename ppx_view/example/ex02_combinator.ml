open Migrate_parsetree.Ast_407
open Viewlib
open Viewast.Parseview

module Matchers : sig

  val exp_string : (Parsetree.expression, string) View.matcher

  val exp_number : (Parsetree.expression, string) View.matcher

end = struct

  open View

  let exp_string = function%view
    | Pexp_constant (Pconst_string (x, _)) -> ok x
    | _ -> error ()

  let exp_number = function%view
    | Pexp_constant (Pconst_integer (x, _)) -> ok x
    | Pexp_constant (Pconst_float   (x, _)) -> ok x
    | _ -> error ()

end


module Combinators : sig

  val exp_list
     : (Parsetree.expression, 'a) View.matcher
    -> ('a list, 'i, 'o) View.t
    -> (Parsetree.expression, 'i, 'o) View.t

end = struct

  open View

  let exp_list m view value =
    let rec aux acc = function%view
      | Pexp_construct ({ txt = Lident "::"; _ },
                        Some (Pexp_tuple [hd; tl])) ->
        m hd >>= fun hd -> aux (hd :: acc) tl
      | Pexp_construct ({ txt = Lident "[]"; _ },
                        None) ->
        ok (List.rev acc)
      | _ ->
        error ()
    in
    aux [] value >>++ view

end


module Custom : sig

  val pexp_string_list
     : (string list, 'i, 'o) View.t
    -> (Parsetree.expression, 'i, 'o) View.t

  val pexp_number_list
     : (string list, 'i, 'o) View.t
    -> (Parsetree.expression, 'i, 'o) View.t

end = struct

  let pexp_string_list value =
    Combinators.exp_list Matchers.exp_string value

  let pexp_number_list value =
    Combinators.exp_list Matchers.exp_number value

end


let desc : Parsetree.expression -> string = function%view
  | Custom.Pexp_string_list l ->
    let escape s = Printf.sprintf "%S" s in
    "[" ^ (String.concat "; " (List.map escape l)) ^ "] : string list"
  | Custom.Pexp_number_list l ->
    "[" ^ (String.concat "; " l) ^ "] : number list"
  | _ ->
    "???"


let run () =
  let abc =
    Ast_helper.Exp.constant
      (Ast_helper.Const.string "abc")
  in
  let def =
    Ast_helper.Exp.constant
      (Ast_helper.Const.string "def")
  in
  let three =
    Ast_helper.Exp.constant
      (Ast_helper.Const.int 3)
  in
  let pi =
    Ast_helper.Exp.constant
      (Ast_helper.Const.float "3.14")
  in
  let nil =
    Ast_helper.Exp.construct
      { txt = Longident.Lident "[]"; loc = Location.none; }
      None
  in
  let cons hd tl =
    Ast_helper.Exp.construct
      { txt = Longident.Lident "::"; loc = Location.none; }
      (Some (Ast_helper.Exp.tuple [hd; tl]))
  in
  let abc_def = cons abc (cons def nil) in
  let three_pi = cons three (cons pi nil) in
  Printf.printf "desc [\"abc\"; \"def\"] ~> %S\n%!" (desc abc_def);
  Printf.printf "desc [3; 3.14]      ~> %S\n%!"     (desc three_pi);
  ()
