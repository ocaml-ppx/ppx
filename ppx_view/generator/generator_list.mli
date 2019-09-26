(* containers for generated code, with i/o functions *)

type 'a t = {
    add           : 'a -> unit;
    add_from_file : string -> unit;
    write         : string -> unit;
  }

val make_signature
   : unit
  -> Ppx_view_common.Ast_utils.Fixed_ast.Parsetree.signature_item t

val make_structure
   : unit
  -> Ppx_view_common.Ast_utils.Fixed_ast.Parsetree.structure_item t
