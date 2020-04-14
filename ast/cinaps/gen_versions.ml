open Stdppx

module Render (Config : sig val internal : bool end) = struct
  let string_of_ty ?nodify ty =
    Grammar.string_of_ty ?nodify ~internal:Config.internal ty

  let string_of_tuple_type ?(parens = true) tuple =
    Grammar.string_of_tuple_type ~internal:Config.internal ~parens tuple

  let print_record_type record =
    Ml.print_record_type record ~f:string_of_ty

  let clause_type_element clause : Ml.element =
    match (clause : Astlib.Grammar.clause) with
    | Empty -> Empty
    | Tuple tuple -> Line (string_of_tuple_type tuple ~parens:false)
    | Record record -> Block (fun () -> print_record_type record)

  let print_variant_type variant =
    Ml.print_variant_type variant ~f:clause_type_element

  let type_element decl : Ml.element =
    match (decl : Astlib.Grammar.decl) with
    | Wrapper ty -> Line (string_of_ty ty)
    | Record record -> Block (fun () -> print_record_type record)
    | Variant variant -> Block (fun () -> print_variant_type variant)
end

module Signature = struct
  module Render = Render (struct let internal = false end)

  let inst_node ty ~tvars =
    Ml.poly_inst ty ~args:(List.map tvars ~f:(fun tvar ->
      Render.string_of_ty (Instance ("node", [Tvar tvar]))))

  let inst ty ~tvars =
    Ml.poly_inst ty ~args:(List.map tvars ~f:(fun tvar ->
      Render.string_of_ty (Var tvar)))

  let declare_constructors decl ~tvars =
    let env = Poly_env.uninstantiated tvars in
    let string_of_ty ty = Render.string_of_ty ~nodify:true (Poly_env.subst_ty ty ~env) in
    match (decl : Astlib.Grammar.decl) with
    | Wrapper ty ->
      Ml.declare_val
        "create"
        (Line (Printf.sprintf "%s -> %s"
                 (string_of_ty ty)
                 (inst "t" ~tvars)))
    | Record record ->
      Ml.declare_val
        "create"
        (Block (fun () ->
           Ml.print_labelled_arrow record ~f:string_of_ty (inst_node "t" ~tvars)))
    | Variant variant ->
      List.iter variant ~f:(fun (tag, clause) ->
        Ml.declare_val
          (Name.make [tag] [])
          (match (clause : Astlib.Grammar.clause) with
           | Empty -> Line (inst_node "t" ~tvars)
           | Tuple tuple ->
             Block (fun () ->
               Ml.print_arrow tuple ~f:string_of_ty (inst_node "t" ~tvars))
           | Record record ->
             Block (fun () ->
               Ml.print_labelled_arrow record ~f:string_of_ty
                 (inst_node "t" ~tvars))))

  let print decl ~name ~tvars =
    Ml.declare_type "t" ~tvars (Line (Ml.poly_type name ~tvars));
    Print.newline ();
    Ml.declare_type "concrete" ~tvars (Render.type_element decl);
    Print.newline ();
    Ml.declare_val
      "of_concrete"
      (Line
         (Printf.sprintf "%s -> %s"
            (inst_node "concrete" ~tvars)
            (inst_node "t" ~tvars)));
    Ml.declare_val
      "to_concrete"
      (Line
         (Printf.sprintf "%s -> %s"
            (inst_node "t" ~tvars)
            (inst_node "concrete" ~tvars)));
    Ml.declare_val
      "to_concrete_opt"
      (Line
         (Printf.sprintf "%s -> %s option"
            (inst_node "t" ~tvars)
            (inst_node "concrete" ~tvars)));
    Print.newline ();
    declare_constructors decl ~tvars
end

module Structure = struct
  module Render = Render (struct let internal = true end)

  let rec ast_of_ty ~grammar ty =
    match (ty : Astlib.Grammar.ty) with
    | Var _
    | Name _
    | Instance _ -> "Data.of_node"
    | Bool -> "Data.of_bool"
    | Char -> "Data.of_char"
    | Int -> "Data.of_int"
    | String -> "Data.of_string"
    | Location -> "Data.of_location"
    | Loc ty -> Printf.sprintf "(Data.of_loc ~f:%s)" (ast_of_ty ~grammar ty)
    | List ty -> Printf.sprintf "(Data.of_list ~f:%s)" (ast_of_ty ~grammar ty)
    | Option ty -> Printf.sprintf "(Data.of_option ~f:%s)" (ast_of_ty ~grammar ty)
    | Tuple tuple ->
      Printf.sprintf "(Data.of_tuple%d %s)"
        (List.length tuple)
        (String.concat ~sep:" "
           (List.mapi tuple ~f:(fun i ty ->
              Printf.sprintf "~f%d:%s" (i + 1) (ast_of_ty ~grammar ty))))

  let rec ast_to_ty ~grammar ty =
    match (ty : Astlib.Grammar.ty) with
    | Var _
    | Name _
    | Instance _ -> "Data.to_node"
    | Bool -> "Data.to_bool"
    | Char -> "Data.to_char"
    | Int -> "Data.to_int"
    | String -> "Data.to_string"
    | Location -> "Data.to_location"
    | Loc ty -> Printf.sprintf "(Data.to_loc ~f:%s)" (ast_to_ty ~grammar ty)
    | List ty -> Printf.sprintf "(Data.to_list ~f:%s)" (ast_to_ty ~grammar ty)
    | Option ty -> Printf.sprintf "(Data.to_option ~f:%s)" (ast_to_ty ~grammar ty)
    | Tuple tuple ->
      Printf.sprintf "(Data.to_tuple%d %s)"
        (List.length tuple)
        (String.concat ~sep:" "
           (List.mapi tuple ~f:(fun i ty ->
              Printf.sprintf "~f%d:%s" (i + 1) (ast_to_ty ~grammar ty))))

  let tuple_var i = Ml.id (Printf.sprintf "x%d" (i + 1))

  let define_constructors decl ~node_name ~grammar =
    match (decl : Astlib.Grammar.decl) with
    | Wrapper ty ->
      Print.println "let create =";
      Print.indented (fun () ->
        Print.println "let data = %s in" (ast_of_ty ~grammar ty);
        Print.println "fun x -> node %S (data x)" node_name)
    | Record record ->
      Print.println "let create %s ="
        (String.concat ~sep:" "
           (List.map record ~f:(fun (field, _) ->
              Printf.sprintf "~%s" (Ml.id field))));
      Print.indented (fun () ->
        Print.println "let fields =";
        Print.indented (fun () ->
          Ml.print_array record ~f:(fun _ (field, ty) ->
            Printf.sprintf "%s %s" (ast_of_ty ~grammar ty) (Ml.id field)));
        Print.println "in";
        Print.println "node %S (Record fields)" node_name)
    | Variant variant ->
      List.iter variant ~f:(fun (tag, clause) ->
        match (clause : Astlib.Grammar.clause) with
        | Empty ->
          Print.println "let %s =" (Name.make [tag] []);
          Print.indented (fun () ->
            Print.println "node %S (Variant { tag = %S; args = [||] })" node_name tag)
        | Tuple tuple ->
          Print.println "let %s %s ="
            (Name.make [tag] [])
            (String.concat ~sep:" " (List.mapi tuple ~f:(fun i _ -> tuple_var i)));
          Print.indented (fun () ->
            Print.println "node %S" node_name;
            Print.indented (fun () ->
              Print.println "(Variant";
              Print.indented (fun () ->
                Print.println "{ tag = %S" tag;
                Print.println "; args =";
                Print.indented (fun () ->
                  Ml.print_array tuple ~f:(fun i ty ->
                    Printf.sprintf "%s %s" (ast_of_ty ~grammar ty) (tuple_var i)));
                Print.println "})")))
        | Record record ->
          Print.println "let %s %s ="
            (Name.make [tag] [])
            (String.concat ~sep:" "
               (List.map record ~f:(fun (field, _) ->
                  Printf.sprintf "~%s" (Ml.id field))));
          Print.indented (fun () ->
            Print.println "node %S" node_name;
            Print.indented (fun () ->
              Print.println "(Variant";
              Print.indented (fun () ->
                Print.println "{ tag = %S" tag;
                Print.println "; args =";
                Print.indented (fun () ->
                  Ml.print_array record ~f:(fun _ (field, ty) ->
                    Printf.sprintf "%s %s" (ast_of_ty ~grammar ty) (Ml.id field)));
                Print.println "})"))))

  let define_of_concrete decl =
    match (decl : Astlib.Grammar.decl) with
    | Wrapper _ -> Print.println "let of_concrete = create"
    | Record record ->
      Print.println "let of_concrete { %s } ="
        (String.concat ~sep:"; "
           (List.map record ~f:(fun (field, _) -> Ml.id field)));
      Print.indented (fun () ->
        Print.println "create %s"
          (String.concat ~sep:" "
             (List.map record ~f:(fun (field, _) -> "~" ^ Ml.id field))))
    | Variant variant ->
      Print.println "let of_concrete c =";
      Print.indented (fun () ->
        Print.println "match c with";
        List.iter variant ~f:(fun (tag, clause) ->
          match (clause : Astlib.Grammar.clause) with
          | Empty -> Print.println "| %s -> %s" (Ml.tag tag) (Name.make [tag] [])
          | Tuple tuple ->
            let vars = List.mapi tuple ~f:(fun i _ -> tuple_var i) in
            Print.println "| %s (%s) ->" (Ml.tag tag) (String.concat ~sep:", " vars);
            Print.indented (fun () ->
              Print.println "%s %s"
                (Name.make [tag] [])
                (String.concat ~sep:" " vars))
          | Record record ->
            Print.println "| %s { %s } ->"
              (Ml.tag tag)
              (String.concat ~sep:"; "
                 (List.map record ~f:(fun (field, _) -> Ml.id field)));
            Print.indented (fun () ->
              Print.println "%s %s"
                (Name.make [tag] [])
                (String.concat ~sep:" "
                   (List.map record ~f:(fun (field, _) -> "~" ^ Ml.id field))))))

  let with_ast_to_ty_bindings alist ~grammar ~f =
    match alist with
    | [] -> f ()
    | _ ->
      let rec loop alist =
        match alist with
        | [] -> f ()
        | (var, ty) :: rest ->
          Print.println "Option.bind (%s %s) ~f:(fun %s ->"
            (ast_to_ty ~grammar ty)
            (Ml.id var)
            (Ml.id var);
          Print.indented (fun () ->
            loop rest)
      in
      loop alist;
      Print.println "%s" (String.make (List.length alist) ')')

  let define_to_concrete_opt decl ~node_name ~grammar =
    match (decl : Astlib.Grammar.decl) with
    | Wrapper ty ->
      Print.println "let to_concrete_opt t =";
      Print.indented (fun () ->
        Print.println
          "match Node.to_node (Unversioned.Private.transparent t) ~version with";
        Print.println "| { name = %S; data } -> %s data"
          node_name
          (ast_to_ty ~grammar ty);
        Print.println "| _ -> None")
    | Record record ->
      Print.println "let to_concrete_opt t =";
      Print.indented (fun () ->
        Print.println
          "match Node.to_node (Unversioned.Private.transparent t) ~version with";
        Print.println "| { name = %S" node_name;
        Print.indented (fun () ->
          Print.println "; data = Record [| %s |]"
            (String.concat ~sep:"; "
               (List.map record ~f:(fun (field, _) -> Ml.id field)));
          Print.println "} ->";
          Print.indented (fun () ->
            with_ast_to_ty_bindings record ~grammar ~f:(fun () ->
              Print.println "Some { %s }"
                (String.concat ~sep:"; "
                   (List.map record ~f:(fun (field, _) -> Ml.id field))))));
        Print.println "| _ -> None")
    | Variant variant ->
      Print.println "let to_concrete_opt t =";
      Print.indented (fun () ->
        Print.println
          "match Node.to_node (Unversioned.Private.transparent t) ~version with";
        Print.println "| { name = %S; data } ->" node_name;
        Print.indented (fun () ->
          Print.println "begin";
          Print.indented (fun () ->
            Print.println "match data with";
            List.iter variant ~f:(fun (tag, clause) ->
              match (clause : Astlib.Grammar.clause) with
              | Empty ->
                Print.println "| Variant { tag = %S; args = [||] } -> Some %s"
                  tag
                  (Ml.tag tag)
              | Tuple tuple ->
                Print.println "| Variant { tag = %S; args = [| %s |] } ->"
                  tag
                  (String.concat ~sep:"; " (List.mapi tuple ~f:(fun i _ -> tuple_var i)));
                Print.indented (fun () ->
                  with_ast_to_ty_bindings ~grammar
                    (List.mapi tuple ~f:(fun i ty -> tuple_var i, ty))
                    ~f:(fun () ->
                      Print.println "Some (%s (%s))"
                        (Ml.tag tag)
                        (String.concat ~sep:", "
                           (List.mapi tuple ~f:(fun i _ -> tuple_var i)))))
              | Record record ->
                Print.println "| Variant { tag = %S; args = [| %s |] } ->"
                  tag
                  (String.concat ~sep:"; "
                     (List.map record ~f:(fun (field, _) -> Ml.id field)));
                Print.indented (fun () ->
                  with_ast_to_ty_bindings ~grammar
                    record
                    ~f:(fun () ->
                      Print.println "Some (%s { %s })"
                        (Ml.tag tag)
                        (String.concat ~sep:"; "
                           (List.map record ~f:(fun (field, _) -> Ml.id field)))))));
          Print.println "| _ -> None";
          Print.println "end");
        Print.println "| _ -> None")

  let define_to_concrete ~node_name =
    Print.println "let to_concrete node =";
    Print.indented (fun () ->
      Print.println "match to_concrete_opt node with";
      Print.println "| Some concrete -> concrete";
      Print.println "| None ->";
      Print.indented (fun () ->
        Print.println "raise";
        Print.indented (fun () ->
          Print.println "(Unversioned.Private.Cannot_interpret_ast {";
          Print.indented (fun () ->
            Print.println "version;";
            Print.println "node_name = %S;" node_name;
            Print.println "node = Unversioned.Private.transparent node;");
          Print.println "})")))

  let print decl ~node_name ~tvars ~grammar =
      Ml.declare_type "t" ~tvars (Line (Ml.poly_type node_name ~tvars));
      Print.newline ();
      Ml.declare_type "concrete" ~tvars (Render.type_element decl);
      Print.newline ();
      define_constructors decl ~node_name ~grammar;
      Print.newline ();
      define_of_concrete decl;
      Print.newline ();
      define_to_concrete_opt decl ~node_name ~grammar;
      Print.newline ();
      define_to_concrete ~node_name
end

module Unversioned = struct
  let types_of_grammar grammar =
    List.map grammar ~f:(fun (type_name, kind) ->
      let tvars  =
        match (kind : Astlib.Grammar.kind) with
        | Mono _ -> []
        | Poly (tvars, _) -> tvars
      in
      (type_name, tvars))

  let combine_types types =
    List.sort_uniq (List.concat types) ~compare

  let all_types grammars =
    let types =
      List.map grammars ~f:(fun (_, grammar) -> types_of_grammar grammar)
    in
    combine_types types
end

let print_ast_types grammars =
  let types = Unversioned.all_types grammars in
  List.iter types ~f:(fun (type_name, tvars) ->
    let type_name_ = type_name ^ "_" in
    Ml.declare_type type_name_ ~tvars Empty);
  Print.newline ();
  List.iter types ~f:(fun (type_name, tvars) ->
    let type_name_ = type_name ^ "_" in
    let node_args =
      [Ml.poly_inst type_name_ ~args:(List.map tvars ~f:Ml.tvar)]
    in
    Ml.declare_type type_name ~tvars
      (Line (Ml.poly_inst "node" ~args:node_args)))

let print_unversioned_types () =
  Print.newline ();
  let grammars = Astlib.History.versioned_grammars Astlib.history in
  Print.indented (fun () ->
    print_ast_types grammars)

let print_version_mli version =
  let grammar = Astlib.History.find_grammar Astlib.history ~version in
  Print.newline ();
  Ml.declare_modules grammar ~recursive:true ~f:(fun node_name kind ->
    match (kind : Astlib.Grammar.kind) with
    | Mono decl ->
      Signature.print decl ~name:node_name ~tvars:[]
    | Poly (tvars, decl) ->
      Signature.print decl ~name:node_name ~tvars)

let print_version_ml version =
  let grammar = Astlib.History.find_grammar Astlib.history ~version in
  Print.newline ();
  Print.println
    "let version = Astlib.Version.of_string %S"
    (Astlib.Version.to_string version);
  Print.println
    "let node name data = Unversioned.Private.opaque (Node.of_node ~version { name; data })";
  Print.newline ();
  Ml.define_modules grammar ~f:(fun node_name kind ->
    match (kind : Astlib.Grammar.kind) with
    | Mono decl -> Structure.print decl ~node_name ~tvars:[] ~grammar
    | Poly (tvars, decl) ->
      Structure.print decl ~node_name ~tvars ~grammar)
