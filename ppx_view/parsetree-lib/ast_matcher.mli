open Migrate_parsetree.Ast_407
open Viewlib

type 'a expression = (Parsetree.expression, 'a) View.matcher

val exp_unit : unit expression

val exp_bool : bool expression

val exp_false : unit expression

val exp_true : unit expression

val exp_integer : (string * char option) expression

val exp_int : string expression

val exp_int32 : string expression

val exp_int64 : string expression

val exp_nativeint : string expression

val exp_char : char expression

val exp_string : string expression

val exp_string_delim : (string * string option) expression

val exp_float : (string * char option) expression
