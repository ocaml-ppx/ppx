open Stdppx
open Ppx_ast.V4_07
open Ppx_ast.Builder.V4_07
open Ppx_ast.Builder.Common
module Traverse = Ppx_ast.Traverse.V4_07
module Traverse_builtins = Ppx_ast.Traverse_builtins
module Expr = Expression_desc
module Pat = Pattern_desc
module Ml = Ppx_ast_cinaps.Ml

let version = "V4_07"

let constructor ~loc type_name name =
  pexp_ident ~loc
    (Located.dotted ~loc
       [ "Ppx_ast"
       ; Ml.module_name version
       ; Ml.module_name type_name
       ; Ml.id name
       ])

class expression_lifters loc =
  object
    inherit [Expression.t] Traverse_builtins.lift
    method node typ expr =
      match typ with
      | Some (type_name, _) ->
        pexp_apply ~loc
          (constructor ~loc type_name "create")
          [Arg_label.nolabel, expr]
      | None -> expr
    method record typ flds =
      match typ with
      | Some (type_name, _) ->
        pexp_apply ~loc
          (constructor ~loc type_name "create")
          (List.map flds ~f:(fun (lab, e) -> Arg_label.labelled lab, e))
      | None ->
        pexp_record ~loc
          (List.map flds ~f:(fun (lab, e) ->
             (Astlib.Loc.create ~loc ~txt:(Longident.lident (Ml.id lab)) ()), e))
          None
    method constr typ id args =
      match typ with
      | Some (type_name, _) ->
        let constructor = constructor ~loc type_name id in
        if List.is_empty args
        then constructor
        else pexp_apply ~loc constructor
               (List.map args ~f:(fun e -> Arg_label.nolabel, e))
      | None ->
        pexp_construct ~loc
          (Astlib.Loc.create ~loc ~txt:(Longident.lident (Ml.tag id)) ())
          (match args with
           | [] -> None
           | l  -> Some (etuple ~loc l))
    method tuple     l = etuple ~loc l
    method int       i = eint ~loc i
    method int32     i = eint32 ~loc i
    method int64     i = eint64 ~loc i
    method nativeint i = enativeint ~loc i
    method float     f = efloat ~loc f
    method string    s = estring ~loc s
    method char      c = echar ~loc c
    method bool      b = ebool ~loc b
    method array : 'a. ('a -> Expression.t) -> 'a array -> Expression.t =
      fun f a -> pexp_array ~loc (List.map (Array.to_list a) ~f)
    method unit () = eunit ~loc
    method other : 'a. 'a -> Expression.t = fun _ -> failwith "not supported"
  end
