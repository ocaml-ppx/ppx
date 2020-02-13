open! Ppx

val expr  : Ppx.expression
val pat   : Ppx.pattern
val type_ : Ppx.core_type
val stri  : Ppx.structure_item
val str   : Ppx.structure
val sigi  : Ppx.signature_item
val sig_  : Ppx.signature

val f_expr : Ppx.expression     -> bool
val f_pat  : Ppx.pattern        -> bool
val f_type : Ppx.core_type      -> bool
val f_stri : Ppx.structure_item -> bool
val f_str  : Ppx.structure      -> bool
val f_sigi : Ppx.signature_item -> bool
val f_sig  : Ppx.signature      -> bool

val f_view : Ppx_ast.expression -> (Ppx_ast.expression * Ppx_ast.core_type) option
