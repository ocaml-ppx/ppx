open Stdppx
open Ppx_ast.V4_07
module Traverse = Ppx_ast.Traverse.V4_07
module Traverse_builtins = Ppx_ast.Traverse_builtins
module Expr = Expression_desc
module Pat = Pattern_desc

let empty_attributes =
  Attributes.create []

let expr ~loc desc =
  Expression.create
    ~pexp_loc:loc
    ~pexp_desc:desc
    ~pexp_attributes:empty_attributes

let pattern ~loc desc =
  Pattern.create
    ~ppat_loc:loc
    ~ppat_desc:desc
    ~ppat_attributes:empty_attributes

let econst ~loc const = expr ~loc (Expr.pexp_constant const)
let pconst ~loc const = pattern ~loc (Pat.ppat_constant const)

let einteger ~loc string copt = econst ~loc (Constant.pconst_integer string copt)
let pinteger ~loc string copt = pconst ~loc (Constant.pconst_integer string copt)

let lid ~loc name = Longident_loc.create { loc; txt = Longident.lident name }

let bool_lid ~loc b = lid ~loc (Bool.to_string b)
let unit_lid ~loc = lid ~loc "()"

let etuple ~loc list =
  match list with
  | [] -> expr ~loc (Expr.pexp_construct (unit_lid ~loc) None)
  | [x] -> x
  | _ :: _ :: _ -> expr ~loc (Expr.pexp_tuple list)

let ptuple ~loc list =
  match list with
  | [] -> pattern ~loc (Pat.ppat_construct (unit_lid ~loc) None)
  | [x] -> x
  | _ :: _ :: _ -> pattern ~loc (Pat.ppat_tuple list)

class expression_lifters loc = object
  inherit [Expression.t] Traverse_builtins.lift
  method record flds =
    expr ~loc
      (Expr.pexp_record
         (List.map flds ~f:(fun (lab, e) ->
            (Longident_loc.create { loc; txt = Longident.lident lab }, e)))
         None)
  method constr id args =
    expr ~loc
      (Expr.pexp_construct
         (Longident_loc.create { loc; txt = Longident.lident id })
         (match args with
          | [] -> None
          | l  -> Some (etuple ~loc l)))
  method tuple     l = etuple ~loc l
  method int       i = einteger ~loc (Int.to_string i) None
  method int32     i = einteger ~loc (Int32.to_string i) (Some 'l')
  method int64     i = einteger ~loc (Int64.to_string i) (Some 'L')
  method nativeint i = einteger ~loc (Nativeint.to_string i) (Some 'n')
  method float     f = econst ~loc (Constant.pconst_float (Float.to_string f) None)
  method string    s = econst ~loc (Constant.pconst_string s None)
  method char      c = econst ~loc (Constant.pconst_char c)
  method bool      b = expr ~loc (Expr.pexp_construct (bool_lid ~loc b) None)
  method array : 'a. ('a -> Expression.t) -> 'a array -> Expression.t =
    fun f a ->
      expr ~loc (Expr.pexp_array (List.map (Array.to_list a) ~f))
  method unit () = expr ~loc (Expr.pexp_construct (unit_lid ~loc) None)
  method other : 'a. 'a -> Expression.t = fun _ -> failwith "not supported"
end

class pattern_lifters loc = object
  inherit [Pattern.t] Traverse_builtins.lift
  method record flds =
    pattern ~loc
      (Pat.ppat_record
         (List.map flds ~f:(fun (lab, e) ->
            (Longident_loc.create { loc; txt = Longident.lident lab }, e)))
         Closed_flag.closed)
  method constr id args =
    pattern ~loc
      (Pat.ppat_construct
         (Longident_loc.create { loc; txt = Longident.lident id })
         (match args with
          | [] -> None
          | l  -> Some (ptuple ~loc l)))
  method tuple     l = ptuple ~loc l
  method int       i = pinteger ~loc (Int.to_string i) None
  method int32     i = pinteger ~loc (Int32.to_string i) (Some 'l')
  method int64     i = pinteger ~loc (Int64.to_string i) (Some 'L')
  method nativeint i = pinteger ~loc (Nativeint.to_string i) (Some 'n')
  method float     f = pconst ~loc (Constant.pconst_float (Float.to_string f) None)
  method string    s = pconst ~loc (Constant.pconst_string s None)
  method char      c = pconst ~loc (Constant.pconst_char c)
  method bool      b = pattern ~loc (Pat.ppat_construct (bool_lid ~loc b) None)
  method array : 'a. ('a -> Pattern.t) -> 'a array -> Pattern.t =
    fun f a ->
      pattern ~loc (Pat.ppat_array (List.map (Array.to_list a) ~f))
  method unit () = pattern ~loc (Pat.ppat_construct (unit_lid ~loc) None)
  method other : 'a. 'a -> Pattern.t = fun _ -> failwith "not supported"
end
