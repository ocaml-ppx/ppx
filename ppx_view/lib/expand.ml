open Import

(** Generates functions akin to `View.tuple{2,3,4}` for arbitrary order *)
let inline_tuple_function ~loc n =
  let view idx = Printf.sprintf "view%d" idx in
  let value idx = Printf.sprintf "value%d" idx in
  let component idx =
    let view_idx = Located.lident ~loc (view idx) in
    let value_idx = Builder.Exp.lident ~loc (value idx) in
    Builder.Exp.apply_lident ~loc (view_idx) [value_idx]
  in
  let rec body idx =
    if idx >= n then
      component idx
    else
      let chain = Builder.view_lib_ident ~loc ">>+" in
      Builder.Exp.apply_lident ~loc chain [component idx; body (idx + 1)]
  in
  let rec values acc idx =
    if idx <= 0 then
      acc
    else
      values (pvar ~loc (value idx)::acc) (idx - 1)
  in
  let values_tuple = ppat_tuple ~loc (values [] n) in
  let res =
    pexp_fun ~loc Arg_label.nolabel None values_tuple (body 1)
  in
  let rec fun_ idx =
    if idx > n then
      res
    else
      pexp_fun ~loc
        Arg_label.nolabel
        None
        (pvar ~loc (view idx))
        (fun_ (idx + 1))
  in
  fun_ 1

let predefined_ident ~loc = function
  | "Some" -> Some (Builder.view_lib_ident ~loc "some")
  | "None" -> Some (Builder.view_lib_ident ~loc "none")
  | "::" -> Some (Builder.view_lib_ident ~loc "cons")
  | "[]" -> Some (Builder.view_lib_ident ~loc "nil")
  | "()" -> Some (Builder.view_lib_ident ~loc "unit")
  | "true" -> Some (Builder.view_lib_ident ~loc "true_")
  | "false" -> Some (Builder.view_lib_ident ~loc "false_")
  | "Not" -> Some (Builder.view_lib_ident ~loc "not")
  | _ -> None

let ctor_viewer s =
  let lowercase = String.uncapitalize_ascii s in
  lowercase ^ "'const"

let translate_ctor_ident ~loc (ident : Longident.concrete) =
  match ident with
  | Lident s ->
    (match predefined_ident ~loc s with
     | Some ident -> ident
     | None -> Located.lident ~loc (ctor_viewer s))
  | Ldot (li, s) ->
    Located.longident ~loc (Longident.ldot li (ctor_viewer s))
  | (Lapply _) as li ->
    Located.longident ~loc (Longident.of_concrete li)

let translate_constant ~loc constant =
  let make name =
    let f = Builder.view_lib_ident ~loc name in
    Builder.Exp.apply_lident ~loc f [pexp_constant ~loc constant]
  in
  match Constant.to_concrete constant with
  | Pconst_integer (_, None) -> make "int"
  | Pconst_integer (_, Some 'l') -> make "int32"
  | Pconst_integer (_, Some 'L') -> make "nativeint"
  | Pconst_char _ -> make "char"
  | Pconst_string _ -> make "string"
  | Pconst_float (_, None) -> make "float"
  | Pconst_integer (_, Some suffix) ->
    Error.unsupported_num_const ~loc ~kind:"int" ~suffix
  | Pconst_float (_, Some suffix) ->
    Error.unsupported_num_const ~loc ~kind:"float" ~suffix

let same_variables vl vl' =
  List.for_all2 vl vl'
    ~f:(fun v v' ->
      let (_, desc) = Deconstructor.pattern v in
      let (_, desc') = Deconstructor.pattern v' in
      match desc, desc' with
      | Ppat_var loc_name, Ppat_var loc_name' ->
        loc_name.txt = loc_name'.txt
      | _ -> false)

let rec translate_pattern pattern =
  match Pattern.to_concrete pattern with
  | {ppat_desc; ppat_loc; ppat_attributes} ->
    let loc = ppat_loc in
    let fields = View_attr.extract_fields ppat_attributes in
    match fields with
    | None -> translate_pattern_desc ~loc ppat_desc
    | Some fields ->
      let expr, vars = translate_pattern_desc ~loc ppat_desc in
      let field_exprs_vars = List.map fields ~f:translate_record_field in
      let field_exprs, field_vars = List.split field_exprs_vars in
      ( Builder.Exp.view_lib_sequence ~loc (field_exprs @ [expr])
      , List.flatten field_vars @ vars )

and translate_pattern_desc ~loc desc =
  match Pattern_desc.to_concrete desc with
  | Ppat_any ->
    ( Builder.Exp.view_lib_drop ~loc
    , [] )
  | Ppat_var v ->
    ( Builder.Exp.view_lib_capture ~loc
    , [ppat_var ~loc v] )
  | Ppat_alias (patt, alias) ->
    let expr, vars = translate_pattern patt in
    let alias_var = ppat_var ~loc alias in
    ( Builder.Exp.view_lib_sequence ~loc
        [Builder.Exp.view_lib_capture ~loc; expr]
    , alias_var :: vars )
  | Ppat_constant c ->
    ( translate_constant ~loc c
    , [] )
  | Ppat_interval (lower, upper) ->
    let f = Builder.view_lib_interval ~loc in
    let args = [translate_constant ~loc lower; translate_constant ~loc upper] in
    ( Builder.Exp.apply_lident ~loc f args
    , [] )
  | Ppat_tuple patts ->
    translate_tuple ~loc patts
  | Ppat_construct (ctor_ident, None) ->
    let ctor_loc, ctor_ident = Deconstructor.longident_loc ctor_ident in
    let f = translate_ctor_ident ~loc:ctor_loc ctor_ident in
    ( pexp_ident ~loc f
    , [] )
  | Ppat_construct (ctor_ident, Some patt) ->
    let ctor_loc, ctor_ident = Deconstructor.longident_loc ctor_ident in
    let f = translate_ctor_ident ~loc:ctor_loc ctor_ident in
    let apply args = Builder.Exp.apply_lident ~loc f args in
    (match Deconstructor.pattern patt with
     | (_, Ppat_tuple [hd; tl]) when ctor_ident = Lident "::" ->
       let expr_hd, vars_hd = translate_pattern hd in
       let expr_tl, vars_tl = translate_pattern tl in
       ( apply [expr_hd; expr_tl]
       , vars_hd @ vars_tl )
     | (ploc, Ppat_tuple patts) ->
       (* why is this needed? *)
       let expr, vars = translate_tuple ~loc:ploc patts in
       ( apply [expr]
       , vars )
     | _ ->
       let expr, vars = translate_pattern patt in
       ( apply [expr]
       , vars ))
  | Ppat_or (first, second) ->
    let expr_first, vars_first = translate_pattern first in
    let expr_second, vars_second = translate_pattern second in
    if same_variables vars_first vars_second then
      let f = Builder.view_lib_choice ~loc in
      ( Builder.Exp.apply_lident ~loc f [expr_first; expr_second]
      , vars_first )
    else
      Error.or_pattern_variables_differ ~loc
  | Ppat_record (fields, _closed) ->
    let exprs_vars = List.map ~f:translate_record_field fields in
    let exprs, vars = List.split exprs_vars in
    ( Builder.Exp.view_lib_sequence ~loc exprs
    , List.flatten vars )
  | Ppat_array patts ->
    translate_array ~loc patts
  | Ppat_constraint (patt, ctyp) ->
    let expr, vars = translate_pattern patt in
    ( pexp_constraint ~loc expr ctyp
    , vars )
  | Ppat_open (lid, patt) ->
    let expr, vars = translate_pattern patt in
    ( pexp_open ~loc Override_flag.override lid expr
    , vars )
  | Ppat_variant _ ->
    Error.unsupported_pattern ~loc "polymorphic variants"
  | Ppat_type _ ->
    Error.unsupported_pattern ~loc "sub types"
  | Ppat_lazy _ ->
    Error.unsupported_pattern ~loc "lazy values"
  | Ppat_unpack _ ->
    Error.unsupported_pattern ~loc "first class modules"
  | Ppat_exception _ ->
    Error.unsupported_pattern ~loc "exceptions"
  | Ppat_extension _ ->
    Error.unsupported_pattern ~loc "extension points"

and translate_patterns patts =
  let exprs, vars =
    List.split (List.map ~f:translate_pattern patts)
  in
  (exprs, List.flatten vars)

and translate_tuple ~loc patts =
  let exprs, vars = translate_patterns patts in
  match List.length exprs with
  | (2 | 3 | 4) as len ->
    let f =  Builder.view_lib_ident ~loc (Printf.sprintf "tuple%d" len) in
    ( Builder.Exp.apply_lident ~loc f exprs
    , vars )
  | len ->
    assert (len > 4);
    let f = inline_tuple_function ~loc len in
    ( eapply ~loc f exprs
    , vars )

and translate_record_field (label, patt) =
  let (label_loc, label) = Deconstructor.longident_loc label in
  match label with
  | Lident label ->
    let expr, vars = translate_pattern patt in
    let f = Located.lident ~loc:label_loc (label ^ "'match") in
    ( Builder.Exp.apply_lident ~loc:label_loc f [expr]
    , vars )
  | _ ->
    Error.invalid_record_field ~loc:label_loc

and translate_array ~loc patts =
  match patts with
  | [] ->
    ( Builder.Exp.view_lib_larray_nil ~loc
    , [] )
  | hd :: tl ->
    let expr_hd, vars_hd = translate_pattern hd in
    let expr_tl, vars_tl = translate_array ~loc tl in
    let f = Builder.view_lib_larray_cons ~loc in
    ( Builder.Exp.apply_lident ~loc f [expr_hd; expr_tl]
    , vars_hd @ vars_tl )

let translate_case_body ~loc ~vars ~pc_guard ~pc_rhs =
  let vars_cons_pattern =
    List.fold_left vars
      ~init:(Builder.Pat.view_lib_var_nil ~loc)
      ~f:(Builder.Pat.view_lib_var_snoc ~loc)
  in
  match pc_guard with
  | Some _ ->
    let guarded_case =
      Case.create ~pc_lhs:vars_cons_pattern ~pc_guard ~pc_rhs
    in
    let guard_failed_case =
      let pc_lhs = ppat_any ~loc in
      let pc_rhs =
        let f = Builder.view_lib_ident ~loc "guard_failed" in
        Builder.Exp.(apply_lident ~loc f [eunit ~loc])
      in
      Case.create ~pc_lhs ~pc_guard:None ~pc_rhs
    in
    pexp_function ~loc [guarded_case; guard_failed_case]
  | None ->
    pexp_fun ~loc Arg_label.nolabel None vars_cons_pattern pc_rhs

let translate_case ~loc match_case =
  match Case.to_concrete match_case with
  | {pc_lhs; pc_guard; pc_rhs} ->
    let pattern, vars = translate_pattern pc_lhs in
    let body = translate_case_body ~loc ~vars ~pc_guard ~pc_rhs in
    let f = Builder.view_lib_ident ~loc "case" in
    Builder.Exp.apply_lident ~loc f [pattern; body]

let pos_argument (loc : Astlib.Location.t) =
  let pos = loc.loc_start in
  pexp_tuple ~loc
    [ estring ~loc pos.pos_fname
    ; eint ~loc pos.pos_lnum
    ; eint ~loc (pos.pos_cnum - pos.pos_bol) ]

let translate_match ~loc ?match_expr match_cases =
  let pos = pos_argument loc in
  let cases = List.map match_cases ~f:(translate_case ~loc) in
  let cases_arg = elist ~loc cases in
  let args =
    match match_expr with
    | Some expr -> [pos; cases_arg; expr]
    | None -> [pos; cases_arg]
  in
  let f_ident = Builder.view_lib_ident ~loc "match_" in
  Builder.Exp.apply_lident ~loc f_ident args

let payload ~loc payload_expr =
  let pexp_loc, pexp_desc = Deconstructor.expression payload_expr in
  match pexp_desc with
  | Pexp_match (match_expr, match_cases) ->
    translate_match ~loc ~match_expr match_cases
  | Pexp_function match_cases ->
    translate_match ~loc match_cases
  | _ ->
    Error.invalid_payload ~loc:pexp_loc
