include Stdppx
include Ppx_ast_deprecated

(* This is not re-exported by Base and we can't use [%here] in ppx *)
external __FILE__ : string = "%loc_FILE"

include Ast
