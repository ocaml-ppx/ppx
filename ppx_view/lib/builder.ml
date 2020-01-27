open Stdppx
open Ppx_ast.V4_07
open Ppx_ast.Builder

module Module : sig
  val view : string
end = struct
  let runtime sub = "Viewlib__" ^ sub
  let view = runtime "View"
end

let view_lib_ident ~loc name =
  Located.longident ~loc Longident.(ldot (lident Module.view) name)

let view_lib_sequence ~loc = view_lib_ident ~loc "sequence"

let view_lib_interval ~loc = view_lib_ident ~loc "interval"

let view_lib_choice ~loc = view_lib_ident ~loc "choice"

let view_lib_larray_cons ~loc = view_lib_ident ~loc "larray_cons"

module Exp = struct
  let apply_lident ~loc lident args =
    let f = V4_07.pexp_ident ~loc lident in
    eapply ~loc f args

  let lident ~loc name = V4_07.pexp_ident (Located.lident ~loc name)

  let view_lib_ident ~loc name =
    V4_07.pexp_ident ~loc (view_lib_ident ~loc name)

  let view_lib_capture ~loc =
    view_lib_ident ~loc "__"

  let view_lib_drop ~loc =
    view_lib_ident ~loc "drop"

  let view_lib_larray_nil ~loc =
    view_lib_ident ~loc "larray_nil"

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
      | _ -> Some (V4_07.ppat_tuple ~loc args)
    in
    V4_07.ppat_construct ident args

  let view_lib_var_nil ~loc =
    construct ~loc (view_lib_ident ~loc "Var_nil") []

  let view_lib_var_snoc ~loc acc var =
    construct ~loc (view_lib_ident ~loc "Var_snoc") [acc; var]
end
