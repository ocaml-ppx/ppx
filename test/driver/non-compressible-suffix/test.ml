#require "base";;
#require "stdio";;

open Ppx;;
open Ast_builder;;

Driver.register_transformation "blah"
  ~rules:[ Context_free.Rule.extension
             (Extension.declare "foo"
                Expression
                Ast_pattern.(pstr nil)
                (fun ~loc ~path:_ -> eint ~loc 42))
         ; Context_free.Rule.extension
             (Extension.declare "@foo.bar"
                Expression
                Ast_pattern.(pstr nil)
                (fun ~loc ~path:_ -> eint ~loc 42))
         ]
;;
[%%expect{|
- : unit = ()
|}]

[%foo];;
[%%expect{|
- : int = 42
|}]

[%foo.bar];;
[%%expect{|
- : int = 42
|}]

[%bar];;
[%%expect{|
Line _, characters 2-5:
Error: Uninterpreted extension 'bar'.
|}]
