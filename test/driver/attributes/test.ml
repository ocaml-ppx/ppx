#require "base";;
#require "ppx.ast";;

open Base
open Ppx
open V4_07

let () = Driver.enable_checks ()

let x = 1 [@@foo]
[%%expect{|
Line _, characters 13-16:
Error: Attribute `foo' was not used
|}]

let f x = 1 [@@deprecatd "..."]
[%%expect{|
Line _, characters 15-24:
Error: Attribute `deprecatd' was not used.
Hint: Did you mean deprecated?
|}]

let attr : _ Attr.t =
  Attr.declare "blah"
    Attr.Context.type_declaration
    Ast_pattern.(__)
    ignore
[%%expect{|
val attr : (Ppx__.Import.type_declaration, unit) Attr.t = <abstr>
|}]

type t = int [@blah]
[%%expect{|
Line _, characters 15-19:
Error: Attribute `blah' was not used.
Hint: `blah' is available for type declarations but is used here in the
context of a core type.
Did you put it at the wrong level?
|}]

let attr : _ Attr.t =
  Attr.declare "blah"
    Attr.Context.expression
    Ast_pattern.(__)
    ignore
[%%expect{|
val attr : (Ppx__.Import.expression, unit) Attr.t = <abstr>
|}]

type t = int [@blah]
[%%expect{|
Line _, characters 15-19:
Error: Attribute `blah' was not used.
Hint: `blah' is available for expressions and type declarations but is used
here in the context of a core type.
Did you put it at the wrong level?
|}]

(* Attribute drops *)

let faulty_transformation = object
  inherit Ast_traverse.map as super

  method! expression e =
    match Expression_desc.to_concrete (Expression.pexp_desc e) with
    | Pexp_constant c ->
      pexp_constant ~loc:(Expression.pexp_loc e) c
    | _ -> super#expression e
end
[%%expect{|
val faulty_transformation : Ast_traverse.map = <obj>
|}]

let () =
  Driver.register_transformation "faulty" ~impl:faulty_transformation#structure

let x = (42 [@foo])
[%%expect{|
Line _, characters 14-17:
Error: Attribute `foo' was silently dropped
|}]

type t1 = < >
type t2 = < t1 >
type t3 = < (t1[@foo]) >
[%%expect{|
type t1 = <  >
type t2 = <  >
Line _, characters 17-20:
Error: Attribute `foo' was not used
|}]
