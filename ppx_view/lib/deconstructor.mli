(** Helpers to deconstruct several layers of an AST node in one go, with
    proper error reporting. Note that the [loc] argument of the following
    functions is only used to report errors and should therefore be as
    precise as possible. *)

open Ppx_ast.V4_07

val expression :
  loc: Astlib.Location.t ->
  Expression.t ->
  Astlib.Location.t * Expression_desc.concrete

val pattern :
  loc: Astlib.Location.t ->
  Pattern.t ->
  Astlib.Location.t * Pattern_desc.concrete

val longident_loc :
  loc:Astlib.Location.t ->
  Longident_loc.t ->
  Astlib.Location.t * Longident.concrete
