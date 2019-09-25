(* source file parsing/printing *)

val parse_signature
   : string
  -> Ppx_view_common.Ast_utils.Fixed_ast.Parsetree.signature

val parse_structure
   : string
  -> Ppx_view_common.Ast_utils.Fixed_ast.Parsetree.structure

val print_signature
   : Format.formatter
  -> Ppx_view_common.Ast_utils.Fixed_ast.Parsetree.signature
  -> unit

val print_structure
   : Format.formatter
  -> Ppx_view_common.Ast_utils.Fixed_ast.Parsetree.structure
  -> unit
