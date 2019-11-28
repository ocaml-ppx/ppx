open Ppx_view_common.Ast_utils
open Ppx_view_common.Ast_utils.Fixed_ast


(* module paths *)

module Module : sig
  val view : string
end = struct
  let runtime sub = "Viewlib__" ^ sub
  let view = runtime "View"
end


(* utility values/functions *)

let no_loc = Location.none

let raise_error loc str =
  raise Location.(Error (error ~loc str))

let ppx_error loc msg =
  raise_error loc ("ppx: " ^ msg)

let ppx_error_unsupported_pattern loc kind =
  raise_error loc (Printf.sprintf "ppx: unsupported pattern (%S)" kind)

let rec extract_view_attribute_fields = function
  | ({ Location.txt = "view"; loc; }, payload) :: _ ->
    begin match payload with
    (* Shouldn't the paylaod be a pattern? so that one could write
     [[@view? {pexp_attributes = []; _}]] for instance? Even
     if we don't allow further deconstruction, I still feel like
     a pattern would be better suited. *)
    | Parsetree.PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_record (fields,
                                                                          None);
                                                 _ }, _); _ }] ->
      Some fields
    | _ ->
      ppx_error loc "invalid 'view' payload"
    end
  | _ :: tl ->
    extract_view_attribute_fields tl
  | [] ->
    None


(* generation of functions akin to `View.tuple{2,3,4}` for arbitrary order *)

let generate_tuple_function n =
  let loc = no_loc in
  (* generates `viewIDX valueIDX` *)
  let component idx =
    make_exp_apply
      no_loc
      (make_ident ~name:(Printf.sprintf "view%d" idx) ())
      [make_exp_ident no_loc ~name:(Printf.sprintf "value%d" idx) ()]
  in
  (* generates `viewIDX valueIDX >>= ... >>= viewN valueN` *)
  let rec body idx =
    if idx >= n then
      component idx
    else begin
      make_exp_apply
        loc
        (make_ident ~modname:Module.view ~name:">>+" ())
        [component idx;
         body (succ idx)]
    end
  in
  (* generates `[value1; ... valueIDX]` *)
  let rec values acc idx =
    if idx <= 0 then
      acc
    else
      let var = make_pat_var no_loc ~name:(Printf.sprintf "value%d" idx) in
      values (var :: acc) (pred idx)
  in
  (* `fun (value1, ..., valueN) -> BODY` *)
  let res =
    Ast_helper.Exp.fun_
       ~loc
       Nolabel
       None
       (Ast_helper.Pat.tuple ~loc (values [] n))
       (body 1)
  in
  (* generates `fun viewIDX -> ... -> RES` *)
  let rec fun_ idx =
    if idx > n then
      res
    else begin
      make_exp_fun
        ~labelled:false
        ~param_name:(Printf.sprintf "view%d" idx)
        (fun_ (succ idx))
    end
  in
  fun_ 1


(* Mapping of predefined constructors *)

let predefined_idents = [
  "Some",  make_ident ~modname:Module.view ~name:"some"   ();
  "None",  make_ident ~modname:Module.view ~name:"none"   ();
  "::",    make_ident ~modname:Module.view ~name:"cons"   ();
  "[]",    make_ident ~modname:Module.view ~name:"nil"    ();
  "()",    make_ident ~modname:Module.view ~name:"unit"   ();
  "true",  make_ident ~modname:Module.view ~name:"true_"  ();
  "false", make_ident ~modname:Module.view ~name:"false_" ();
  "Not",   make_ident ~modname:Module.view ~name:"not"    ();
]


(* translation functions *)

let transl_constructor : Longident.t -> Longident.t = function
  | Lident s ->
    begin try
      List.assoc s predefined_idents
    with Not_found ->
      Lident (String.uncapitalize_ascii s)
    end
  | Ldot (li, s) ->
    Ldot (li, String.uncapitalize_ascii s)
  | (Lapply _) as li ->
    li

let transl_ident_loc : Longident.t Asttypes.loc -> Longident.t Asttypes.loc =
  function
  | { txt; _ } as li ->
    { li with txt = transl_constructor txt; }

let transl_constant : Location.t -> Parsetree.constant -> Parsetree.expression =
  fun loc const ->
    let make_constant name value =
      make_exp_apply
        loc
        (make_ident ~modname:Module.view ~name ())
        [Ast_helper.Exp.constant ~loc value]
    in
    match const with
    | Pconst_integer (value, suffix) ->
      let name =
        match suffix with
        | None     -> "int"
        | Some 'l' -> "int32"
        | Some 'L' -> "int64"
        | Some 'n' -> "nativeint"
        (* Shouldn't this be interpreted by other ppxs somehow or reported by the compiler
          as is? *)
        | Some _   -> ppx_error loc "invalid integer suffix"
      in
      make_constant name (Ast_helper.Const.integer ?suffix value)
    | Parsetree.Pconst_char value ->
      make_constant "char" (Ast_helper.Const.char value)
    | Pconst_string (value, quotation_delimiter) ->
      make_constant "string" (Ast_helper.Const.string ?quotation_delimiter value)
    | Pconst_float (value, suffix) ->
      make_constant "float" (Ast_helper.Const.float ?suffix value)

let rec transl_pattern patt =
  match extract_view_attribute_fields patt.Parsetree.ppat_attributes with
  | Some fields ->
    let expr, vars = transl_pattern { patt with ppat_attributes = []; } in
    List.fold_right
      (fun field (acc_expr, acc_vars) ->
         match field with
         | { Location.txt = Longident.Lident label; loc = label_loc; },
           { Parsetree.pexp_desc = Pexp_ident { txt = Lident var;
                                                loc = var_loc; }; _ } ->
           make_exp_apply
             label_loc
             (make_ident ~name:(label ^ "'field") ())
             [make_exp_ident var_loc ~modname:Module.view ~name:"__" ();
              acc_expr],
           (make_pat_var var_loc ~name:var) :: acc_vars
         | { loc; _ }, _ ->
           ppx_error loc "invalid 'view' payload")
      fields
      (expr, vars)
  | None ->
    transl_pattern_desc patt.ppat_loc patt.ppat_desc

and transl_pattern_desc loc desc =
  match desc with
  | Ppat_any ->
    make_exp_ident loc ~modname:Module.view ~name:"drop" (),
    []
  | Ppat_var _ ->
    make_exp_ident loc ~modname:Module.view ~name:"__" (),
    [Ast_helper.Pat.mk ~loc desc]
  | Ppat_alias (patt, alias) ->
    let expr, vars = transl_pattern patt in
    make_exp_apply
      no_loc
      (make_ident ~modname:Module.view ~name:"sequence" ())
      [make_exp_ident alias.loc ~modname:Module.view ~name:"__" ();
       expr],
    (make_pat_var alias.loc ~name:alias.txt) :: vars
  | Ppat_constant c ->
    transl_constant loc c,
    []
  | Ppat_interval (lower, upper) ->
    make_exp_apply
      loc
      (make_ident ~modname:Module.view ~name:"interval" ())
      [transl_constant loc lower;
       transl_constant loc upper],
    []
  | Ppat_tuple patts ->
    transl_tuple patts
  | Ppat_construct (lid, patts) ->
    let mk_apply args =
      Ast_helper.Exp.apply
        ~loc:(lid : Longident.t Location.loc).loc
        (Ast_helper.Exp.ident (transl_ident_loc lid))
        (List.map (fun x -> Asttypes.Nolabel, x) args)
    in
    begin match patts with
    | None ->
      Ast_helper.Exp.ident
        ~loc:lid.loc
        { lid with txt = transl_constructor lid.txt; },
      []
    | Some ({ ppat_desc = Ppat_tuple [hd; tl]; _ }) when lid.txt = Lident "::" ->
      let expr_hd, vars_hd = transl_pattern hd in
      let expr_tl, vars_tl = transl_pattern tl in
      mk_apply [expr_hd; expr_tl],
      vars_hd @ vars_tl
    | Some ({ ppat_desc = Ppat_tuple l; _ }) ->
      (* why is this needed? *)
      let exprs, vars = transl_tuple l in
      mk_apply [exprs],
      vars
    | Some x ->
      let exprs, vars = transl_pattern x in
      mk_apply [exprs],
      vars
    end
  | Ppat_variant _ ->
    ppx_error_unsupported_pattern loc "variant"
  | Ppat_or (first, second) ->
    let expr_first,  vars_first  = transl_pattern first  in
    let expr_second, vars_second = transl_pattern second in
    let same_variables =
      (* Isn't there an ordering issue? *)
      List.for_all2
        (fun var_first var_second ->
           match  var_first.Parsetree.ppat_desc,
                 var_second.Parsetree.ppat_desc with
           | Ppat_var { txt = name_first;  _ },
             Ppat_var { txt = name_second; _ } ->
             name_first = name_second
           | _ -> false)
        vars_first
        vars_second
    in
    if same_variables then begin
      make_exp_apply
        no_loc
        (make_ident ~modname:Module.view ~name:"choice" ())
        [expr_first; expr_second],
      vars_first
    end else
      ppx_error loc "branches of an or-pattern should define the same variables"
  | Ppat_record (fields, _closed) ->
    let rec sequence = function
      | [] ->  assert false
      | [hd] -> hd
      | hd :: tl ->
        make_exp_apply
          no_loc
          (make_ident ~modname:Module.view ~name:"sequence" ())
          [hd; sequence tl]
    in
    let exprs_vars =
      List.map
        (fun (label, patt) ->
           match label with
           | { Location.txt = Longident.Lident id; loc; } ->
             let expr, vars = transl_pattern patt in
             make_exp_apply
               loc
               (make_ident ~name:(id ^ "'match") ())
               [expr],
             vars
           | { loc; _ } ->
             (* Couldn't that be seen as an open and the field
                translated to [Some_module.x'match]? *)
             ppx_error loc "invalid field (should be unqualified)")
        fields
    in
    let exprs, vars = List.split exprs_vars in
    sequence exprs,
    List.flatten vars
  | Ppat_array patts ->
    transl_array patts
  | Ppat_constraint (patt, ctyp) ->
    let expr, vars = transl_pattern patt in
    Ast_helper.Exp.constraint_ ~loc expr ctyp,
    vars
  | Ppat_type _ ->
    ppx_error_unsupported_pattern loc "type"
  | Ppat_lazy _ ->
    ppx_error_unsupported_pattern loc "lazy"
  | Ppat_unpack _ ->
    ppx_error_unsupported_pattern loc "unpack"
  | Ppat_exception _ ->
    ppx_error_unsupported_pattern loc "exception"
  | Ppat_extension _ ->
    ppx_error_unsupported_pattern loc "extension"
  | Ppat_open (id, patt) ->
    let expr, vars = transl_pattern patt in
    Ast_helper.Exp.open_ ~loc Override id expr,
    vars

and transl_patterns patts =
  let exprs, vars = List.split (List.map transl_pattern patts) in
  exprs, List.flatten vars

and transl_tuple patts =
  let exprs, vars = transl_patterns patts in
  begin match List.length exprs with
  | (2 | 3 | 4) as len ->
    make_exp_apply
      no_loc
      (make_ident ~modname:Module.view ~name:(Printf.sprintf "tuple%d" len) ())
      exprs,
    vars
  | len ->
    assert (len > 4);
    Ast_helper.Exp.apply
      ~loc:no_loc
      (generate_tuple_function len)
      (List.map (fun e -> Asttypes.Nolabel, e) exprs),
    vars
  end

and transl_array = function
  | hd :: tl ->
    let expr_hd, vars_hd = transl_pattern hd in
    let expr_tl, vars_tl = transl_array   tl in
    (* This should be larray_cons and larray_nil *)
    make_exp_apply
      no_loc
      (make_ident ~modname:Module.view ~name:"array_cons" ())
      [expr_hd; expr_tl],
    vars_hd @ vars_tl
  | [] ->
    (* larray_nil is a value not a constructor *)
    make_exp_construct
      no_loc
      (make_ident ~modname:Module.view ~name:"array_nil" ())
      [],
    []

let trans_case_body vars guard body =
  let var_list =
    List.fold_left
      (fun acc var ->
         make_pat_construct
           no_loc
           (make_ident ~modname:Module.view ~name:"Var_snoc" ())
           [acc; var])
      (make_pat_construct
        no_loc
        (make_ident ~modname:Module.view ~name:"Var_nil" ())
        [])
      vars
  in
  match guard with
  | Some guard ->
    Ast_helper.Exp.function_
      ~loc:no_loc
      [Ast_helper.Exp.case
         var_list
         ~guard
         body;
       Ast_helper.Exp.case
         (Ast_helper.Pat.any ~loc:no_loc ())
         (make_exp_apply
            no_loc
            (make_ident ~modname:Module.view ~name:"guard_failed" ())
            [make_exp_construct no_loc (make_ident ~name:"()" ()) []])]
  | None ->
    Ast_helper.Exp.fun_
      ~loc:no_loc
      Nolabel
      None
      var_list
      body

let transl_cases cases =
  List.map
    (function
      | { Parsetree.pc_lhs; pc_guard; pc_rhs; } ->
        let patt, vars = transl_pattern pc_lhs in
        let body = trans_case_body vars pc_guard pc_rhs in
        make_exp_apply
          no_loc
          (make_ident ~modname:Module.view ~name:"case" ())
          [patt; body])
    cases

let transl_match loc match_expr match_cases =
  let start = loc.Location.loc_start in
  let string x = Ast_helper.(Exp.constant (Const.string x)) in
  let int    x = Ast_helper.(Exp.constant (Const.int    x)) in
  let pos =
    Ast_helper.Exp.tuple
      [string start.pos_fname;
       int    start.pos_lnum;
       int   (start.pos_cnum - start.pos_bol)]
  in
  let cases = make_exp_list (transl_cases match_cases) in
  make_exp_apply
    no_loc
    (make_ident ~modname:Module.view ~name:"match_" ())
    (match match_expr with
     | Some expr -> [pos; cases; expr]
     | None      -> [pos; cases])


(* mapper for match%view / function%view *)

let mapper =
  let super = Ast_mapper.default_mapper in
  let expr self e =
    let e = super.expr self e in
    match e.Parsetree.pexp_desc with
    | Pexp_extension ({ txt = "view"; loc; }, payload) ->
      begin match payload with
      | PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_match (match_expr,
                                                                 match_cases);
                                         _ }, _); _; }] ->
        transl_match loc (Some match_expr) match_cases
      | PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_function function_cases;
                                         _ }, _); _; }] ->
        transl_match loc None function_cases
      | _ ->
        ppx_error loc "invalid 'view' payload"
      end
    | _ ->
      e
  in
  { super with expr; }


let () =
  Migrate_parsetree.Driver.register
    ~name:"view_pattern"
    (module Fixed_ocaml)
    (fun _ _ -> mapper)
