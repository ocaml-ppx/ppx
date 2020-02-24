open Stdppx
open Ppx_ast.V4_07
module Traverse = Ppx_ast.Traverse.V4_07
module Traverse_builtins = Ppx_ast.Traverse_builtins
module Expr = Expression_desc
module Pat = Pattern_desc
open Ppx_ast.Builder.Common
open Ppx_ast.Builder.V4_07

class expression_lifters loc =
  object
    inherit [Expression.t] Traverse_builtins.lift
    method record flds =
      pexp_record ~loc
        (List.map flds ~f:(fun (lab, e) ->
           (Longident_loc.create
              (Astlib.Loc.create ~loc ~txt:(Longident.lident lab) ()), e)))
        None
    method constr id args =
      pexp_construct ~loc
        (Longident_loc.create (Astlib.Loc.create ~loc ~txt:(Longident.lident id) ()))
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

class pattern_lifters loc =
  object
    inherit [Pattern.t] Traverse_builtins.lift
    method record flds =
      ppat_record ~loc
        (List.map flds ~f:(fun (lab, e) ->
           (Longident_loc.create
              (Astlib.Loc.create ~loc ~txt:(Longident.lident lab) ()), e)))
        Closed_flag.closed
    method constr id args =
      ppat_construct ~loc
        (Longident_loc.create (Astlib.Loc.create ~loc ~txt:(Longident.lident id) ()))
        (match args with
         | [] -> None
         | l  -> Some (ptuple ~loc l))
    method tuple     l = ptuple ~loc l
    method int       i = pint ~loc i
    method int32     i = pint32 ~loc i
    method int64     i = pint64 ~loc i
    method nativeint i = pnativeint ~loc i
    method float     f = pfloat ~loc f
    method string    s = pstring ~loc s
    method char      c = pchar ~loc c
    method bool      b = pbool ~loc b
    method array : 'a. ('a -> Pattern.t) -> 'a array -> Pattern.t =
      fun f a -> ppat_array ~loc (List.map (Array.to_list a) ~f)
    method unit () = punit ~loc
    method other : 'a. 'a -> Pattern.t = fun _ -> failwith "not supported"
  end
