(** Helpers to deconstruct several layers of an AST node in one go, with
    proper error reporting. *)

open Ppx_ast.V4_07

val expression :
  Expression.t ->
  Astlib.Location.t * Expression_desc.concrete

val pattern :
  Pattern.t ->
  Astlib.Location.t * Pattern_desc.concrete

val longident_loc :
  Longident_loc.t ->
  Astlib.Location.t * Longident.concrete
