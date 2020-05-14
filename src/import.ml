include Stdppx
include Ppx_ast

(*$ Ppxlib_cinaps_helpers.define_current_ast () *)
module Current_ast = Ppx_ast.V4_07
(*$*)

(* This is not re-exported by Base and we can't use [%here] in ppx *)
external __FILE__ : string = "%loc_FILE"
