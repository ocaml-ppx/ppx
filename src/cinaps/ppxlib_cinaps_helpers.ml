open Stdppx
open Ppx_ast_cinaps
open Re

let str_to_sig =
  let re = Str.regexp {|\(_?[sS]tructure\|impl\(ementation\)?\|str_\|_str\|\b\(st\|Str\)\b\)|} in
  let map s =
    match Str.matched_string s with
    | "st"             -> "sg"
    | "Str"            -> "Sig"
    | "structure"      -> "signature"
    | "Structure"      -> "Signature"
    | "_structure"     -> "_signature"
    | "_Structure"     -> "_Signature"
    | "str_"           -> "sig_"
    | "_str"           -> "_sig"
    | "implementation" -> "interface"
    | "impl"           -> "intf"
    | _                -> assert false
  in
  fun s ->
    print_string (Str.global_substitute re map s)

let define_current_ast () =
  Print.newline ();
  Print.println "module Current_ast = Ppx_ast.%s"
    (Ml.module_name (Astlib.Version.to_string Astlib.current_version))

module Generate_ast_patterns = struct
  let current_grammar () =
    Astlib.History.find_grammar Astlib.history ~version:Astlib.current_version

  let rec simplify_with_acc ty ~acc ~grammar =
    match (ty : Astlib.Grammar.ty) with
    | Name string ->
      (match
         List.find_map grammar ~f:(fun (name, kind) ->
           if String.equal name string
           then Some kind
           else None)
       with
       | Some (Mono (Ty ty) : Astlib.Grammar.kind) ->
         simplify_with_acc ty ~grammar
           ~acc:(Printf.sprintf "%s.to_concrete" (Ml.module_name string) :: acc)
       | _ -> acc, ty)
    | Loc ty -> simplify_with_acc ty ~grammar ~acc:("Loc.txt" :: acc)
    | _ -> acc, ty

  let simplify ty ~grammar = simplify_with_acc ty ~grammar ~acc:[]

  let generate_variant clauses ~grammar ~self_type ~node_name ~field_name ~on_clause =
    List.iter clauses ~f:(fun (tag, clause) ->
      match (clause : Astlib.Grammar.clause) with
      | Empty -> ()
      | Tuple tuple ->
        on_clause ~self_type ~node_name ~field_name ~tag
          (List.map tuple ~f:(simplify ~grammar))
      | Record _ ->
        (* not yet supported or needed *)
        assert false)

  let generate_desc ty ~grammar ~self_type ~node_name ~field_name ~on_clause =
    match (ty : Astlib.Grammar.ty) with
    | Name name ->
      (match
         List.find_map grammar ~f:(fun (node_name, kind) ->
           if String.equal node_name name then Some kind else None)
       with
       | None -> assert false
       | Some kind ->
         let decl =
           match (kind : Astlib.Grammar.kind) with
           | Mono decl -> decl
           | Poly (_, decl) -> decl
         in
         match decl with
         | Ty _ -> ()
         | Record _ -> ()
         | Variant clauses ->
           generate_variant clauses
             ~grammar
             ~self_type
             ~node_name
             ~field_name:(Some field_name)
             ~on_clause)
    | _ -> ()

  let generate ~on_field ~on_clause =
    let grammar = current_grammar () in
    List.iter grammar ~f:(fun (node_name, kind) ->
      let tvars, decl =
        match (kind : Astlib.Grammar.kind) with
        | Mono decl -> [], decl
        | Poly (tvars, decl) -> tvars, decl
      in
      let self_type =
        Ml.poly_inst node_name
          ~args:(List.map tvars ~f:(fun tvar -> Ml.poly_type "node" ~tvars:[tvar]))
      in
      match decl with
      | Ty _ -> ()
      | Record record ->
        let prefix = String.common_prefix (List.map record ~f:fst) in
        List.iter record ~f:(fun (field_name, ty) ->
          match Option.value_exn (String.drop_prefix field_name ~prefix) with
          | ("loc" | "attributes") ->
            let conv, ty = simplify ty ~grammar in
            on_field ~self_type ~node_name ~field_name ~conv ~ty
          | "desc" ->
            generate_desc ty ~grammar ~self_type ~node_name ~field_name ~on_clause
          | _ -> ())
      | Variant clauses ->
        if not (String.is_suffix node_name ~suffix:"_desc") then
          generate_variant clauses
            ~grammar
            ~self_type
            ~node_name
            ~field_name:None
            ~on_clause)

  let string_of_ty ty = Grammar.string_of_ty ~nodify:false ty

  let print_arrow inputs output =
    Ml.print_arrow inputs ~f:Fn.id output

  let pattern_type a b c =
    Ml.poly_inst "Ast_pattern0.t" ~args:[a; b; c]

  let tvar n = Ml.tvar ("t" ^ Int.to_string n)

  let wrap ~conv expr =
    List.fold_right conv ~init:expr ~f:(fun fn expr ->
      Printf.sprintf "(%s %s)" fn expr)

  let field_intf ~self_type ~node_name:_ ~field_name ~conv:_ ~ty =
    Print.newline ();
    Ml.declare_val (String.lowercase field_name) (Block (fun () ->
      print_arrow
        [ pattern_type (string_of_ty ty) (tvar 1) (tvar 2) ]
        (pattern_type self_type (tvar 1) (tvar 2))))

  let clause_intf ~self_type ~node_name:_ ~field_name:_ ~tag tuple =
    Print.newline ();
    Ml.declare_val (String.lowercase tag) (Block (fun () ->
      print_arrow
        (List.mapi tuple ~f:(fun i (_, ty) ->
           pattern_type (string_of_ty ty) (tvar i) (tvar (i + 1))))
        (pattern_type self_type (tvar 0) (tvar (List.length tuple)))))

  let field_impl ~self_type:_ ~node_name ~field_name ~conv ~ty:_ =
    Print.newline ();
    Print.println "let %s (T f') =" (Ml.id (String.lowercase field_name));
    Print.indented (fun () ->
      Print.println "T (fun c' l' x' k' ->";
      Print.indented (fun () ->
        Print.println "match %s.to_concrete_opt x' with" (Ml.module_name node_name);
        Print.println "| Some { %s = x'; _ } -> f' c' l' %s k'"
          (Ml.id field_name)
          (wrap ~conv "x'");
        Print.println "| _ -> fail l' %S" (Ml.id field_name));
      Print.println ")")

  let clause_impl ~self_type:_ ~node_name ~field_name ~tag tuple =
    let vars =
      List.mapi tuple ~f:(fun i _ -> Ml.id (Printf.sprintf "x%d'" i))
    in
    let funs =
      List.mapi tuple ~f:(fun i _ -> Ml.id (Printf.sprintf "f%d'" i))
    in
    Print.newline ();
    Print.println "let %s %s ="
      (Ml.id (String.lowercase tag))
      (String.concat ~sep:" "
         (List.map funs ~f:(Printf.sprintf "(T %s)")));
    Print.indented (fun () ->
      Print.println "T (fun c' l' x' k' ->";
      Print.indented (fun () ->
        let print_variant_expr ~module_name =
          Print.println "match %s.to_concrete_opt x' with" (Ml.module_name module_name);
          Print.println "| Some (%s (%s)) ->" (Ml.tag tag) (String.concat ~sep:", " vars);
          Print.indented (fun () ->
            Print.println "begin";
            Print.indented (fun () ->
              Print.println "c'.matched <- c'.matched + 1;";
              Print.println "%s"
                (List.fold_left
                   (List.zip_exn tuple (List.zip_exn vars funs))
                   ~init:"k'"
                   ~f:(fun expr ((conv, _), (var, fn)) ->
                     Printf.sprintf "%s c' l' %s (%s)" fn (wrap ~conv var) expr)));
            Print.println "end");
          Print.println "| _ -> fail l' %S" (Ml.tag tag);
        in
        match field_name with
        | None -> print_variant_expr ~module_name:node_name
        | Some field_name ->
          Print.println "match %s.to_concrete_opt x' with" (Ml.module_name node_name);
          Print.println "| Some { %s = x'; _ } ->" (Ml.id field_name);
          Print.indented (fun () ->
            Print.println "c'.matched <- c'.matched + 1;";
            Print.println "begin";
            Print.indented (fun () ->
              print_variant_expr ~module_name:(node_name ^ "_desc"));
            Print.println "end");
          Print.println "| _ -> fail l' %S" (Ml.tag tag));
      Print.println ")")

  let impl () = generate ~on_field:field_impl ~on_clause:clause_impl
  let intf () = generate ~on_field:field_intf ~on_clause:clause_intf
end

let generate_ast_pattern_impl () = Generate_ast_patterns.impl ()
let generate_ast_pattern_intf () = Generate_ast_patterns.intf ()
