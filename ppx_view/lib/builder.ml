open Stdppx
open Ppx_ast.V4_07

module Module : sig
  val view : string
end = struct
  let runtime sub = "Viewlib__" ^ sub
  let view = runtime "View"
end

let expression ~loc pexp_desc =
  Expression.create
    ~pexp_desc
    ~pexp_attributes:(Attributes.create [])
    ~pexp_loc:loc

let pattern ~loc ppat_desc =
  Pattern.create
    ~ppat_desc
    ~ppat_loc:loc
    ~ppat_attributes:(Attributes.create [])

let longident_loc ~loc longident =
  Astlib.Loc.create ~loc ~txt:longident ()
  |> Longident_loc.create

let lident_loc ~loc str =
  let lident = Longident.lident str in
  longident_loc ~loc lident

let view_lib_ident ~loc name =
  longident_loc ~loc Longident.(ldot (lident Module.view) name)

let view_lib_sequence ~loc = view_lib_ident ~loc "sequence"

let view_lib_interval ~loc = view_lib_ident ~loc "interval"

let view_lib_choice ~loc = view_lib_ident ~loc "choice"

let view_lib_larray_cons ~loc = view_lib_ident ~loc "larray_cons"

module Exp = struct
  let apply ~loc fun_expr args =
    let no_label_args = List.map args ~f:(fun expr -> (Arg_label.nolabel, expr)) in
    expression ~loc (Expression_desc.pexp_apply fun_expr no_label_args)

  let apply_lident ~loc lident args =
    let f = expression ~loc (Expression_desc.pexp_ident lident) in
    apply ~loc f args

  let list_lit ~loc exprs =
    let open Expression_desc in
    let cons_ident = lident_loc ~loc "::" in
    let nil = expression ~loc (pexp_construct (lident_loc ~loc "[]") None) in
    let cons expr list =
      let cons_args = expression ~loc (pexp_tuple [expr; list]) in
      expression ~loc (pexp_construct cons_ident (Some cons_args))
    in
    List.fold_right exprs ~init:nil ~f:cons

  let unit ~loc =
    expression ~loc (Expression_desc.pexp_construct (lident_loc ~loc "()") None)

  let lident ~loc name =
    expression ~loc (Expression_desc.pexp_ident (lident_loc ~loc name))

  let view_lib_ident ~loc name =
    expression ~loc (Expression_desc.pexp_ident (view_lib_ident ~loc name))

  let view_lib_capture ~loc = view_lib_ident ~loc "__"

  let view_lib_drop ~loc = view_lib_ident ~loc "drop"

  let view_lib_larray_nil ~loc = view_lib_ident ~loc "larray_nil"

  let view_lib_sequence ~loc exprs =
    let rec aux = function
      | [] -> assert false
      | [hd] -> hd
      | hd :: tl ->
        apply_lident ~loc (view_lib_sequence ~loc) [hd; aux tl]
    in
    aux exprs
end

module Pat = struct
  let construct ~loc ident args =
    let args =
      match args with
      | [] -> None
      | [one] -> Some one
      | _ -> Some (pattern ~loc (Pattern_desc.ppat_tuple args))
    in
    pattern ~loc (Pattern_desc.ppat_construct ident args)

  let var ~loc name =
    let var = Astlib.Loc.create ~loc ~txt:name () in
    pattern ~loc (Pattern_desc.ppat_var var)

  let view_lib_var_nil ~loc =
    construct ~loc (view_lib_ident ~loc "Var_nil") []

  let view_lib_var_snoc ~loc acc var =
    construct ~loc (view_lib_ident ~loc "Var_snoc") [acc; var]
end
