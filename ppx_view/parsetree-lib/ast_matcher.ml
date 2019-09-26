open Migrate_parsetree.Ast_407
open Viewlib

type 'a expression = (Parsetree.expression, 'a) View.matcher

open View
open Parseview

let exp_unit = function%view
  | Pexp_construct (lid, None) when lid.txt = Longident.Lident "()" ->
    ok ()
  | _ -> error ()

let exp_bool = function%view
  | Pexp_construct (lid, None) ->
    begin match lid.txt with
    | Longident.Lident "false" -> ok false
    | Longident.Lident "true"  -> ok true
    | _ -> error ()
    end
  | _ -> error ()

let exp_false = function%view
  | Pexp_construct (lid, None) ->
    begin match lid.txt with
    | Longident.Lident "false" -> ok ()
    | Longident.Lident "true"  -> error ()
    | _ -> error ()
    end
  | _ -> error ()

let exp_true = function%view
  | Pexp_construct (lid, None) ->
    begin match lid.txt with
    | Longident.Lident "false" -> error ()
    | Longident.Lident "true"  -> ok ()
    | _ -> error ()
    end
  | _ -> error ()

let exp_integer = function%view
  | Pexp_constant (Pconst_integer (str, suffix)) -> ok (str, suffix)
  | _ -> error ()

let exp_int = function%view
  | Pexp_constant (Pconst_integer (str, None)) -> ok str
  | _ -> error ()

let exp_int32 = function%view
  | Pexp_constant (Pconst_integer (str, Some 'l')) -> ok str
  | _ -> error ()

let exp_int64 = function%view
  | Pexp_constant (Pconst_integer (str, Some 'L')) -> ok str
  | _ -> error ()

let exp_nativeint = function%view
  | Pexp_constant (Pconst_integer (str, Some 'L')) -> ok str
  | _ -> error ()

let exp_char = function%view
  | Pexp_constant (Pconst_char ch) -> ok ch
  | _ -> error ()

let exp_string = function%view
  | Pexp_constant (Pconst_string (str, _)) -> ok str
  | _ -> error ()

let exp_string_delim = function%view
  | Pexp_constant (Pconst_string (str, delim)) -> ok (str, delim)
  | _ -> error ()

let exp_float = function%view
  | Pexp_constant (Pconst_float (str, suffix)) -> ok (str, suffix)
  | _ -> error ()
