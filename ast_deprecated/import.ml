(* This file is used to control what we use from the current compiler and what is embed in
   this library.

   It must be opened in all modules, especially the ones coming from the compiler.
*)

module Js    = Migrate_parsetree.OCaml_407
module Ocaml = Migrate_parsetree.Versions.OCaml_current

module Select_ast(Ocaml : Migrate_parsetree.Versions.OCaml_version) = struct
  open Migrate_parsetree

  include Js

  module Type = struct
    type ('js, 'ocaml) t =
      | Signature
        : (Js   .Ast.Parsetree.signature,
           Ocaml.Ast.Parsetree.signature) t
      | Structure
        : (Js   .Ast.Parsetree.structure,
           Ocaml.Ast.Parsetree.structure) t
      | Toplevel_phrase
        : (Js   .Ast.Parsetree.toplevel_phrase,
           Ocaml.Ast.Parsetree.toplevel_phrase) t
      | Out_phrase
        : (Js   .Ast.Outcometree.out_phrase,
           Ocaml.Ast.Outcometree.out_phrase) t
      | Expression
        : (Js   .Ast.Parsetree.expression,
           Ocaml.Ast.Parsetree.expression) t
      | Core_type
        : (Js   .Ast.Parsetree.core_type,
           Ocaml.Ast.Parsetree.core_type) t
      | Type_declaration
        : (Js   .Ast.Parsetree.type_declaration,
           Ocaml.Ast.Parsetree.type_declaration) t
      | Type_extension
        : (Js   .Ast.Parsetree.type_extension,
           Ocaml.Ast.Parsetree.type_extension) t
      | Extension_constructor
        : (Js   .Ast.Parsetree.extension_constructor,
           Ocaml.Ast.Parsetree.extension_constructor) t
      | List
        : ('a, 'b) t -> ('a list, 'b list) t
      | Pair
        : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
  end
  open Type

  module Of_ocaml = Versions.Convert(Ocaml)(Js)
  module To_ocaml = Versions.Convert(Js)(Ocaml)

  let rec of_ocaml : type ocaml js. (js, ocaml) Type.t -> ocaml -> js =
    let open Of_ocaml in
    fun node ->
      match node with
      | Signature             -> copy_signature
      | Structure             -> copy_structure
      | Toplevel_phrase       -> copy_toplevel_phrase
      | Out_phrase            -> copy_out_phrase
      | Expression            -> copy_expression
      | Core_type             -> copy_core_type
      | Type_declaration      -> copy_type_declaration
      | Type_extension        -> copy_type_extension
      | Extension_constructor -> copy_extension_constructor
      | List t                -> List.map (of_ocaml t)
      | Pair (a, b)           ->
        let f = of_ocaml a in
        let g = of_ocaml b in
        fun (x, y) -> (f x, g y)

  let rec to_ocaml : type ocaml js. (js, ocaml) Type.t -> js -> ocaml =
    let open To_ocaml in
    fun node ->
      match node with
      | Signature             -> copy_signature
      | Structure             -> copy_structure
      | Toplevel_phrase       -> copy_toplevel_phrase
      | Out_phrase            -> copy_out_phrase
      | Expression            -> copy_expression
      | Core_type             -> copy_core_type
      | Type_declaration      -> copy_type_declaration
      | Type_extension        -> copy_type_extension
      | Extension_constructor -> copy_extension_constructor
      | List t                -> List.map (to_ocaml t)
      | Pair (a, b)           ->
        let f = to_ocaml a in
        let g = to_ocaml b in
        fun (x, y) -> (f x, g y)

  let of_ocaml_mapper item f x =
    to_ocaml item x |> f |> of_ocaml item

  let to_ocaml_mapper item f x =
    of_ocaml item x |> f |> to_ocaml item
end

module Selected_ast = Select_ast(Ocaml)

(* Modules from migrate_parsetree *)
module Parsetree  = Selected_ast.Ast.Parsetree
module Asttypes   = Selected_ast.Ast.Asttypes

module Location   = struct
  include Ocaml_common.Location
  include Location_helper
end

(* Modules imported directly from the compiler *)
module Clflags    = Ocaml_common.Clflags
module Longident  = Ocaml_common.Longident
module Misc       = Ocaml_common.Misc
module Warnings   = Ocaml_common.Warnings
