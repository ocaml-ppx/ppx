(** AST traversal classes *)

(* TODO: remove in favor of Ppx_ast's versioned traversals *)

open! Import
open Current_ast

(** To use these classes, inherit from them and override the methods corresponding to the
    types from [Parsetree] you want to process. For instance to collect all the string
    constants in a structure:

    {[
      let string_constants_of = object
        inherit [string list] Ast_traverse.fold as super

        method! expression e acc =
          let acc = super#expression e acc in
          match e.pexp_desc with
          | Pexp_constant (Const_string (s, _)) -> s :: acc
          | _ -> acc

        method! pattern p acc =
          let acc = super#pattern p acc in
          match p.ppat_desc with
          | Ppat_constant (Const_string (s, _)) -> s :: acc
          | _ -> acc
      end

      let string_constants_of_structure = string_constants_of#structure
    ]}
*)

class map : object
  inherit Traverse_builtins.map
  inherit Virtual.map
end

class iter : object
  inherit Traverse_builtins.iter
  inherit Virtual.iter
end

class ['acc] fold : object
  inherit ['acc] Traverse_builtins.fold
  inherit ['acc] Virtual.fold
end

class ['acc] fold_map : object
  inherit ['acc] Traverse_builtins.fold_map
  inherit ['acc] Virtual.fold_map
end

class ['ctx] map_with_context : object
  inherit ['ctx] Traverse_builtins.map_with_context
  inherit ['ctx] Virtual.map_with_context
end

class map_with_path : [string] map_with_context

class map_with_expansion_context : [Expansion_context.Base.t] map_with_context

class virtual ['res] lift : object
  inherit ['res] Traverse_builtins.lift
  inherit ['res] Virtual.lift
end

class sexp_of : object
  inherit [Sexp.t] Traverse_builtins.std_lifters
  inherit [Sexp.t] Virtual.lift
end

val sexp_of : sexp_of
