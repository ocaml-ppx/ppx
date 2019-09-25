open Ppx_view_common.Ast_utils
open Ppx_view_common.Ast_utils.Fixed_ast
open Ppx_view_common.String_utils


(* utility values/types/functions *)

let no_loc = Location.none

type shorcut = {
  type_name  : string;
  field_name : string;
}

module StringMap = Map.Make (String)

module StringSet = Set.Make (String)

let make_view a b c =
  Ast_helper.Typ.constr
    (make_ident_loc no_loc ~modname:"View" ~name:"t" ())
    [a; b; c]

let parseview_signature = Generator_list.make_signature ()

let parseview_structure = Generator_list.make_structure ()

let ast_viewer_signature = Generator_list.make_signature ()

let ast_viewer_structure = Generator_list.make_structure ()

let add_ast_viewer_element, copy_ast_viewer_elements =
  let elements = ref StringMap.empty in
  let add module_name element =
    elements :=
      try
        let previous_elements = StringMap.find module_name !elements in
        StringMap.add module_name (element :: previous_elements) !elements
      with Not_found ->
        StringMap.add module_name [element] !elements
  in
  let copy () =
    StringMap.iter
      (fun module_name module_elements ->
         let signature_structure =
           List.rev_map
             (fun (function_name, function_type, function_def) ->
                Ast_helper.Sig.value
                  (Ast_helper.Val.mk
                     (make_str function_name)
                     function_type),
                Ast_helper.Str.value
                  Nonrecursive
                  [Ast_helper.Vb.mk
                     (make_pat_var no_loc ~name:function_name)
                     function_def])
             module_elements
         in
         let signature, structure =
           List.split signature_structure
         in
         ast_viewer_signature.Generator_list.add
           (Ast_helper.Sig.module_
              (Ast_helper.Md.mk
                 (make_str module_name)
                 (Ast_helper.Mty.signature signature)));
         ast_viewer_structure.Generator_list.add
           (Ast_helper.Str.module_
              (Ast_helper.Mb.mk
                 (make_str module_name)
                 (Ast_helper.Mod.structure structure))))
      !elements
  in
  add, copy

let add_parseview_attribute name value =
  if !Generator_args.verbose then begin
    let open Parsetree in
    let const = Ast_helper.Exp.constant (Ast_helper.Const.string value) in
    let attr = make_str name, PStr [Ast_helper.Str.eval const] in
    parseview_signature.Generator_list.add (Ast_helper.Sig.attribute attr);
    parseview_structure.Generator_list.add (Ast_helper.Str.attribute attr)
  end


(* actual processing *)

let process_type_decl ~module_name ~types ~type_decl ~shortcuts ~prefixes =
  let open Parsetree in
  let open Longident in
  match type_decl.ptype_kind with
  | Ptype_abstract ->
    ()
  | Ptype_variant constructor_decls ->
    if !Generator_args.verbose then
      Printf.printf "... type %S (variant)\n%!" type_decl.ptype_name.txt;
    add_parseview_attribute "type" type_decl.ptype_name.txt;
    List.iter
      (fun { pcd_name; pcd_args; _ } ->
         let res_type_name =
           try
             (StringMap.find type_decl.ptype_name.txt shortcuts).type_name
           with Not_found ->
             type_decl.ptype_name.txt
         in
         let make_res_type a b =
           make_view
             (make_typ_constr
                ~module_name
                ~type_name:res_type_name
                ~type_params:[])
             (make_typ_var ~name:a)
             (make_typ_var ~name:b)
         in
         let make_def param_names match_case =
           let matched_value = make_exp_ident no_loc ~name:"value" () in
           let matched_value =
             try
               let fn =
                 StringMap.find
                   type_decl.ptype_name.txt
                   shortcuts
               in
               Ast_helper.Exp.field
                 matched_value
                 (make_ident_loc
                    no_loc
                    ~modname:module_name
                    ~name:fn.field_name
                    ())
             with Not_found ->
               matched_value
           in
           let body =
             Ast_helper.Exp.match_
               matched_value
               [match_case;
                Ast_helper.Exp.case
                  (Ast_helper.Pat.any ())
                  ( make_exp_ident no_loc ~modname:"View" ~name:"error" ())]
           in
           make_exp_funs ~labelled:false ~param_names body
         in
         let val_name = uncapitalize_str pcd_name in
         let val_type, val_def, val_arrow_type, val_arrow_def =
           match pcd_args with
           | Pcstr_tuple [] ->
             make_res_type "a" "a",
             make_def
               ["value"]
               (Ast_helper.Exp.case
                  (make_pat_construct
                     no_loc
                     (make_ident ~modname:module_name ~name:pcd_name.txt ())
                     [])
                  (make_exp_ident no_loc ~modname:"View" ~name:"ok" ())),
             make_typ_arrow
               (make_view
                  (make_typ_constr
                     ~module_name:""
                     ~type_name:"unit"
                     ~type_params:[])
                  (make_typ_var ~name:"a")
                  (make_typ_var ~name:"a"))
               (make_res_type "a" "a"),
             Ast_helper.Exp.fun_
               Nolabel
               None
               (Ast_helper.Pat.constraint_
                  (make_pat_var no_loc ~name:"view")
                  (make_view
                     (make_typ_constr
                        ~module_name:""
                        ~type_name:"unit"
                        ~type_params:[])
                     (make_typ_var ~name:"a")
                     (make_typ_var ~name:"a")))
               (make_def
                  ["value"]
                  (Ast_helper.Exp.case
                     (make_pat_construct
                        no_loc
                        (make_ident ~modname:module_name ~name:pcd_name.txt ())
                        [])
                     (make_exp_apply
                        no_loc
                        (make_ident ~name:"view" ())
                        [make_exp_construct
                           no_loc
                           (make_ident ~name:"()" ())
                           []])))
           | Pcstr_tuple args ->
             let patt, expr =
               match args with
               | [] ->
                 assert false
               | [_] ->
                 make_pat_var no_loc ~name:"arg",
                 make_exp_ident no_loc ~name:"arg" ()
               | _ ->
                 Ast_helper.Pat.tuple
                   (List.mapi
                      (fun idx _ ->
                         make_pat_var no_loc ~name:(Printf.sprintf "arg%d" idx))
                      args),
                 Ast_helper.Exp.tuple
                   (List.mapi
                      (fun idx _ ->
                         make_exp_ident no_loc ~name:(Printf.sprintf "arg%d" idx) ())
                      args)
             in
             let typ =
               make_typ_arrow
                 (make_view
                    (match args with
                     | [] -> assert false
                     | [arg] -> arg
                     | _ -> make_typ_tuple args)
                    (make_typ_var ~name:"a")
                    (make_typ_var ~name:"b"))
                 (make_res_type "a" "b")
             in
             let def =
               make_def
                 ["view"; "value"]
               (Ast_helper.Exp.case
                  (Ast_helper.Pat.construct
                     (make_ident_loc no_loc ~modname:module_name ~name:pcd_name.txt ())
                     (Some patt))
                  (make_exp_apply
                     no_loc
                     (make_ident ~name:"view" ())
                     [expr]))
             in
             typ, def, typ, def
           | Pcstr_record _ ->
             assert false
         in
         let val_arrow_type =
           qualify_core_type ~types val_arrow_type in
         begin try
           let name = val_name.txt in
           let prefix =
             StringSet.choose
             (StringSet.filter
                (fun prefix -> starts_with ~prefix val_name.txt)
                prefixes)
           in
           let module_name = String.sub name 1 (String.length prefix - 2) in
           let function_name =
             safe_ident
               (String.sub
                  name
                  (String.length prefix)
                  (String.length name - String.length prefix))
           in
           add_ast_viewer_element
             (String.capitalize_ascii module_name)
             (function_name, val_arrow_type, val_arrow_def)
         with Not_found ->
           ()
         end;
         parseview_signature.Generator_list.add
           (Ast_helper.Sig.value
              (Ast_helper.Val.mk
                 val_name
                 (qualify_core_type ~types val_type)));
         parseview_structure.Generator_list.add
           (Ast_helper.Str.value
              Nonrecursive
              [Ast_helper.Vb.mk
                 (make_pat_var no_loc ~name:val_name.txt)
                 val_def]))
      constructor_decls
  | Ptype_record label_decls ->
    if !Generator_args.verbose then
      Printf.printf "... type %S (record)\n%!" type_decl.ptype_name.txt;
    add_parseview_attribute "type" type_decl.ptype_name.txt;
    let orig_type_name = type_decl.ptype_name.txt in
    (* field matchers *)
    begin
      List.iter
        (function
          | { pld_name = { txt; _ };
              pld_type;
              _ } ->
            let record_type =
              make_typ_constr
                ~module_name
                ~type_name:orig_type_name
                ~type_params:(List.map fst type_decl.ptype_params)
            in
            let val_name = txt ^ "'match" in
            let val_type =
              make_typ_arrow
                (make_view
                   pld_type
                   (make_typ_var ~name:"i")
                   (make_typ_var ~name:"o"))
                (make_view
                   record_type
                   (make_typ_var ~name:"i")
                   (make_typ_var ~name:"o"))
            in
            let value =
              Ast_helper.Exp.constraint_
                (make_exp_ident no_loc ~name:"value" ())
                record_type
            in
            let field_access =
              Ast_helper.Exp.field
                value
                (make_ident_loc no_loc ~name:txt ())
            in
            let body =
              make_exp_apply
                no_loc
                (make_ident ~name:"view" ())
                [field_access]
            in
            let val_def =
                make_exp_funs
                  ~labelled:false
                  ~param_names:["view"; "value"]
                  body
            in
            parseview_signature.Generator_list.add
              (Ast_helper.Sig.value
                 (Ast_helper.Val.mk
                    (make_str val_name)
                    (qualify_core_type ~types val_type)));
            parseview_structure.Generator_list.add
              (Ast_helper.Str.value
                 Nonrecursive
                 [Ast_helper.Vb.mk
                    (make_pat_var no_loc ~name:val_name)
                    val_def]))
        label_decls
    end;
    (* field accessors *)
    let candidates =
      StringMap.filter (fun _ { type_name; _ } -> type_name = orig_type_name) shortcuts
    in
    if StringMap.cardinal candidates = 1 then begin
      let _, shortcut = StringMap.choose candidates in
      let add_field_accessor pseudo field_label field_type_name =
        if field_label <> shortcut.field_name then begin
          let field_module, field_name =
            match field_type_name with
            | Ldot (Lident field_module, field_name) ->
              field_module, field_name
            | Lident field_name ->
              module_name, field_name
            | _ ->
              assert false
          in
          let val_name = field_label ^ "'field" in
          let val_type =
            make_typ_arrow
              (make_view
                 (make_typ_constr
                    ~module_name:field_module
                    ~type_name:field_name
                    ~type_params:[])
                 (make_typ_var ~name:"x0")
                 (make_typ_var ~name:"x1"))
              (make_typ_arrow
                 (make_view
                    (make_typ_constr
                       ~module_name
                       ~type_name:shortcut.type_name
                       ~type_params:[])
                    (make_typ_var ~name:"x1")
                    (make_typ_var ~name:"x2"))
                 (make_view
                    (make_typ_constr
                       ~module_name
                       ~type_name:shortcut.type_name
                       ~type_params:[])
                    (make_typ_var ~name:"x0")
                    (make_typ_var ~name:"x2")))
          in
          let value =
            if pseudo then
              make_exp_ident no_loc ~name:"value" ()
            else
              Ast_helper.Exp.field
                (make_exp_ident no_loc ~name:"value" ())
                (make_ident_loc no_loc ~modname:module_name ~name:field_label ())
          in
          let matched_value =
            make_exp_apply
              no_loc
              (make_ident ~name:"field_view" ())
              [value]
          in
          let call =
            make_exp_apply
              no_loc
              (make_ident ~name:"view" ())
              [make_exp_ident no_loc ~name:"value" ()]
          in
          let body =
            make_exp_apply
              no_loc
              (make_ident ~modname:"View" ~name:">>+" ())
              [matched_value; call]
          in
          let val_def =
            make_exp_funs
              ~labelled:false
              ~param_names:["field_view"; "view"; "value"]
              body
          in
          parseview_signature.Generator_list.add
            (Ast_helper.Sig.value
               (Ast_helper.Val.mk
                  (make_str val_name)
                  (qualify_core_type ~types val_type)));
          parseview_structure.Generator_list.add
            (Ast_helper.Str.value
               Nonrecursive
               [Ast_helper.Vb.mk
                  (make_pat_var no_loc ~name:val_name)
                  val_def])
        end
      in
      add_field_accessor
        true
        type_decl.ptype_name.txt
        (Lident type_decl.ptype_name.txt);
      List.iter
        (function
          | { pld_name = { txt; _ };
              pld_type = { ptyp_desc = Ptyp_constr ({ txt = field_type_name;
                                                      _ },
                                                    []);
                           _ };
              _ } ->
            add_field_accessor false txt field_type_name
          | _ ->
            ())
        label_decls
    end
| Ptype_open ->
    ()

let process_mli ~path ~ignored_types ~desc_shortcuts ~seen =
  let open Parsetree in
  add_parseview_attribute "file" path;
  if !Generator_args.verbose then
    Printf.printf "file %S...\n%!" path;
  let full_path = Filename.concat !Generator_args.ocaml_where_path path in
  let signature_items = Generator_source.parse_signature full_path in
  let module_name =
    String.capitalize_ascii
      (Filename.(chop_extension (basename path)))
  in
  let fold_sig_types f z =
    List.fold_left
      (fun acc { psig_desc; _ } ->
         match psig_desc with
         | Psig_type (rec_flag, type_decls) ->
           f ~acc ~rec_flag ~type_decls
         | _ ->
           acc)
      z
      signature_items
  in
  (* compute shortcuts and prefixes *)
  let should_process_type type_name =
    not (List.mem type_name ignored_types)
  in
  let shortcuts, prefixes =
    if desc_shortcuts then begin
      fold_sig_types
        (fun ~acc ~rec_flag:_ ~type_decls ->
           List.fold_left
             (fun (acc_shortcuts, acc_prefixes) type_decl ->
                let type_name = type_decl.ptype_name.txt in
                if should_process_type type_name then begin
                  match type_decl.ptype_kind with
                  | Ptype_record label_decls ->
                    let rec find_shortcut = function
                      | { pld_name = { txt = field_name; _ };
                          pld_type = { ptyp_desc = Ptyp_constr ({ txt = Lident target_type_name;
                                                                  _ },
                                                                []);
                                       _ };
                          _ }
                        :: _ when ends_with ~suffix:"_desc" field_name ->
                        let prefix = Filename.chop_suffix field_name "desc" in
                        if !Generator_args.verbose then begin
                          Printf.printf "... shortcut %S -> %S (%S), prefix %S\n%!"
                            target_type_name
                            type_name
                            field_name
                            prefix;
                        end;
                        prefix, target_type_name, { type_name; field_name; }
                      | _ :: tl ->
                        find_shortcut tl
                      | [] ->
                        raise Not_found
                    in
                    begin try
                      let prefix, target_type_name, shorctut =
                        find_shortcut label_decls
                      in
                      StringMap.add target_type_name shorctut acc_shortcuts,
                      StringSet.add prefix acc_prefixes
                    with Not_found ->
                      acc_shortcuts, acc_prefixes
                    end
                  | _ ->
                    acc_shortcuts, acc_prefixes
                end else
                  acc_shortcuts, acc_prefixes)
             acc
             type_decls)
        (StringMap.empty, StringSet.empty)
    end else
      StringMap.empty, StringSet.empty
  in
  (* process types, keeping track of previously-seen ones *)
  let seen =
    fold_sig_types
      (fun ~acc ~rec_flag ~type_decls ->
         let all_type_names =
           List.map
             (fun { ptype_name = { txt = name; _ }; _ } ->
                name, module_name)
             type_decls
         in
         List.fold_left
           (fun acc_seen type_decl ->
              let type_name = type_decl.ptype_name.txt in
              let acc_seen =
                if rec_flag = Recursive then
                  acc_seen
                else
                  (type_name, module_name) :: acc_seen
              in
              if should_process_type type_name then begin
                process_type_decl
                  ~module_name
                  ~types:acc_seen
                  ~type_decl
                  ~shortcuts
                  ~prefixes
              end;
              acc_seen)
           (if rec_flag = Recursive then
              all_type_names @ acc
            else
              acc)
           type_decls)
      seen
  in
  seen


(* main *)

let () =
  (* analyse command line *)
  Generator_args.parse ();
  if !Generator_args.ocaml_where_path = "" then begin
    prerr_endline "*** error: OCaml path not set";
    exit 1
  end;
  (* open ocaml-migrate-parsetree and viewlib in every produced file *)
  let make_open txt =
    { Parsetree.
      popen_lid = { txt; loc = Location.none; };
      popen_override = Override;
      popen_loc = Location.none;
      popen_attributes = []; }
  in
  List.iter
    (fun open_ ->
       let open_sig = Ast_helper.Sig.open_ open_ in
       let open_str = Ast_helper.Str.open_ open_ in
       ast_viewer_signature.Generator_list.add open_sig;
       ast_viewer_structure.Generator_list.add open_str;
       parseview_signature.Generator_list.add open_sig;
       parseview_structure.Generator_list.add open_str)
    [make_open Longident.(Ldot (Lident "Migrate_parsetree", "Ast_" ^ fixed));
     make_open Longident.(Lident "Viewlib")];
  (* add manually-defined elememts to Ast_viewer *)
  ast_viewer_signature.Generator_list.add_from_file "ast_viewer_const_mli-src";
  ast_viewer_signature.Generator_list.add_from_file "ast_viewer_const_exp_mli-src";
  ast_viewer_structure.Generator_list.add_from_file "ast_viewer_const_ml-src";
  ast_viewer_structure.Generator_list.add_from_file "ast_viewer_const_exp_ml-src";
  (* process file from the OCaml distribution *)
  let _ =
    List.fold_left
      (fun acc_seen { Generator_config.path; ignored_types; desc_shortcuts; } ->
         process_mli ~path ~ignored_types ~desc_shortcuts ~seen:acc_seen)
      []
      Generator_config.files
  in
  (* copy shortcut/prefix elements to Ast_viewer *)
  copy_ast_viewer_elements ();
  (* emit files *)
  parseview_signature.Generator_list.write "parseview.mli";
  parseview_structure.Generator_list.write "parseview.ml";
  ast_viewer_signature.Generator_list.write "ast_viewer.mli";
  ast_viewer_structure.Generator_list.write "ast_viewer.ml";
  (* report *)
  if !Generator_args.verbose then
    print_endline "done."
