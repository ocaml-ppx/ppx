open! Stdppx
open Ppx_ast

class expression_lifters : Astlib.Location.t -> [expression] Traverse_builtins.std_lifters
