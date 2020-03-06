open Stdppx

module V4_07 = Builder_v4_07
module Unstable_for_testing = Builder_unstable_for_testing

open V4_07
open Versions.V4_07

module Common = struct
  module Located = struct
    let longident ~loc longident = Astlib.Loc.create ~loc ~txt:longident ()
    let lident ~loc x = longident ~loc (Longident.lident x)

    let dotted ~loc list =
      match list with
      | [] -> invalid_arg "Located.dotted: empty list"
      | head :: tail ->
        longident ~loc
          (List.fold_left ~init:(Longident.lident head) tail ~f:Longident.ldot)
  end

  let echar ~loc x = pexp_constant ~loc (Constant.pconst_char x)

  let estring ~loc x = pexp_constant ~loc (Constant.pconst_string x None)

  let eint ~loc x =
    pexp_constant ~loc (Constant.pconst_integer (Int.to_string x) None)

  let eint32 ~loc x =
    pexp_constant ~loc (Constant.pconst_integer (Int32.to_string x) (Some 'l'))

  let eint64 ~loc x =
    pexp_constant ~loc (Constant.pconst_integer (Int64.to_string x) (Some 'L'))

  let enativeint ~loc x =
    pexp_constant ~loc (Constant.pconst_integer (Nativeint.to_string x) (Some 'n'))

  let efloat ~loc x =
    pexp_constant ~loc (Constant.pconst_float (Float.to_string x) None)

  let evar ~loc x = pexp_ident ~loc (Located.lident ~loc x)

  let eunit ~loc =
    pexp_construct ~loc (Located.lident ~loc "()") None

  let ebool ~loc x = pexp_construct ~loc (Located.lident ~loc (Bool.to_string x)) None

  let enil ~loc = pexp_construct ~loc (Located.lident ~loc "[]") None

  let etuple ~loc list =
    match list with
    | [] -> eunit ~loc
    | [e] -> e
    | _ -> pexp_tuple ~loc list

  let elist ~loc exprs =
    let nil = enil ~loc in
    let cons expr list_expr =
      pexp_construct ~loc
        (Located.lident ~loc "::")
        (Some (pexp_tuple ~loc [expr; list_expr]))
    in
    List.fold_right exprs ~f:cons ~init:nil

  let eapply ~loc fun_expr args =
    pexp_apply ~loc
      fun_expr
      (List.map args ~f:(fun expr -> (Arg_label.nolabel, expr)))

  let eabstract ~loc pats body =
    List.fold_right pats ~init:body ~f:(fun pat body ->
      pexp_fun ~loc Arg_label.nolabel None pat body)

  let pchar ~loc x = ppat_constant ~loc (Constant.pconst_char x)

  let pstring ~loc x = ppat_constant ~loc (Constant.pconst_string x None)

  let pint ~loc x =
    ppat_constant ~loc (Constant.pconst_integer (Int.to_string x) None)

  let pint32 ~loc x =
    ppat_constant ~loc (Constant.pconst_integer (Int32.to_string x) (Some 'l'))

  let pint64 ~loc x =
    ppat_constant ~loc (Constant.pconst_integer (Int64.to_string x) (Some 'L'))

  let pnativeint ~loc x =
    ppat_constant ~loc (Constant.pconst_integer (Nativeint.to_string x) (Some 'n'))

  let pfloat ~loc x =
    ppat_constant ~loc (Constant.pconst_float (Float.to_string x) None)

  let punit ~loc = ppat_construct ~loc (Located.lident ~loc "()") None

  let pbool ~loc x = ppat_construct ~loc (Located.lident ~loc (Bool.to_string x)) None

  let pnil ~loc = ppat_construct ~loc (Located.lident ~loc "[]") None

  let pvar ~loc x = ppat_var ~loc (Astlib.Loc.create ~loc ~txt:x ())

  let ptuple ~loc list =
    match list with
    | [] -> punit ~loc
    | [e] -> e
    | _ -> ppat_tuple ~loc list

  let plist ~loc exprs =
    let nil = pnil ~loc in
    let cons pat list_pat =
      ppat_construct ~loc
        (Located.lident ~loc "::")
        (Some (ppat_tuple ~loc [pat; list_pat]))
    in
    List.fold_right exprs ~f:cons ~init:nil

  module Error_ext = struct
    open Versions

    let extension ~loc msg =
      let pp fmt = Format.pp_print_string fmt msg in
      let error = Astlib.Location.Error.make ~loc pp in
      Conversion.ast_of_extension (Astlib.Location.Error.to_extension error)

    let build_ext : 'node 'a .
      (loc: Astlib.Location.t -> extension -> 'node) ->
      loc: Astlib.Location.t ->
      ('a, unit, string, 'node) format4 ->
      'a
      =
      fun f ~loc fmt -> Format.ksprintf (fun msg -> f ~loc (extension ~loc msg)) fmt

    let no_attr = Attributes.create []

    let exprf ~loc fmt = build_ext pexp_extension ~loc fmt
    let patf ~loc fmt = build_ext ppat_extension ~loc fmt
    let typf ~loc fmt = build_ext ptyp_extension ~loc fmt
    let strif ~loc fmt =
      build_ext (fun ~loc ext -> pstr_extension ~loc ext no_attr) ~loc fmt
    let sigif ~loc fmt =
      build_ext (fun ~loc ext -> psig_extension ~loc ext no_attr) ~loc fmt
  end
end
