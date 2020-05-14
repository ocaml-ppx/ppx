#require "base";;
#require "ppx.ast";;

open Base
open Ppx
open V4_07

(* Linters *)

let lint = object
  inherit [Driver.Lint_error.t list] Ast_traverse.fold as super

  method! type_declaration td acc =
    let acc = super#type_declaration td acc in
    match Type_kind.to_concrete (Type_declaration.ptype_kind td) with
    | Ptype_record lds ->
      if Poly.(<>)
           (List.sort lds ~compare:(fun a b ->
              String.compare
                (Label_declaration.pld_name a).txt
                (Label_declaration.pld_name b).txt))
           lds
      then
        Driver.Lint_error.of_string (Type_declaration.ptype_loc td)
          "Fields are not sorted!"
        :: acc
      else
        acc
    | _ -> acc
end
let () =
  Driver.register_transformation "lint" ~lint_impl:(fun st -> lint#structure st [])
[%%expect{|
val lint : Driver.Lint_error.t list Ast_traverse.fold = <obj>
|}]

type t =
  { b : int
  ; a : int
  }
[%%expect{|
Line _, characters 0-36:
Error (Warning 22): Fields are not sorted!
|}]


(* Extension with a path argument *)

let () =
  Driver.register_transformation "plop"
    ~rules:[Context_free.Rule.extension
              (Ext.declare_with_path_arg "plop"
                 Expression
                 Ast_pattern.(pstr nil)
                 (fun ~loc ~path:_ ~arg ->
                    let open Ast_builder in
                    match arg with
                    | None -> estring ~loc "-"
                    | Some { loc; txt } -> estring ~loc (Longid.name txt)))]
[%%expect{|
|}]

let _ = Caml.Printf.sprintf "%s\n" [%plop]
[%%expect{|
- : string = "-\n"
|}]

let _ = Caml.Printf.sprintf "%s\n" [%plop.Truc]
[%%expect{|
- : string = "Truc\n"
|}]

let _ = Caml.Printf.sprintf "%s\n" [%plop.Truc.Bidule]
[%%expect{|
- : string = "Truc.Bidule\n"
|}]
