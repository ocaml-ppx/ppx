include Stdppx
include Ppx_ast
module Ast = V4_07
include Ast

(* This is not re-exported by Base and we can't use [%here] in ppx *)
external __FILE__ : string = "%loc_FILE"
