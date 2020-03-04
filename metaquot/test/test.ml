#require "astlib"
#require "ppx.ast"

let loc = Astlib.Location.none
let () = Clflags.dump_source := true
[%%expect{|
val loc : Astlib.Location.t = <abstr>
|}]


[%expr 42]
[%%expect{|

;;Ppx_ast.V4_07.Expression.create
    ~pexp_desc:(Ppx_ast.V4_07.Expression_desc.pexp_constant
                  (Ppx_ast.V4_07.Constant.pconst_integer "42" None))
    ~pexp_loc:loc ~pexp_attributes:(Ppx_ast.V4_07.Attributes.create []);;
- : Ppx_ast.V4_07.Expression.t = <abstr>
|}]
