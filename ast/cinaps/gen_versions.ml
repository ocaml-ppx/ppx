open Stdppx

module Render (Config : sig val internal : bool end) = struct
  let string_of_ty ty = Grammar.string_of_ty ~internal:Config.internal ty
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

  let versioned_type_element versioned : Ml.element =
    match (versioned : Astlib.Grammar.versioned) with
    | Wrapper ty -> Line (string_of_ty ty)
    | Record record -> Block (fun () -> print_record_type record)
    | Variant variant -> Block (fun () -> print_variant_type variant)
end

module Signature = struct
  module Render = Render (struct let internal = false end)

  let inst_node ty ~tvars =
    Ml.poly_inst ty ~args:(List.map tvars ~f:(fun tvar ->
      Render.string_of_ty (Instance ("node", [Var tvar]))))

  let inst ty ~tvars =
    Ml.poly_inst ty ~args:(List.map tvars ~f:(fun tvar ->
      Render.string_of_ty (Var tvar)))

  let declare_constructors versioned ~tvars =
    let env = Poly_env.nodify_targs tvars in
    let string_of_ty ty = Render.string_of_ty (Poly_env.subst_ty ty ~env) in
    match (versioned : Astlib.Grammar.versioned) with
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
    match (decl : Astlib.Grammar.decl) with
    | Unversioned ty ->
      Ml.declare_type "t" ~tvars (Line (Render.string_of_ty ty))
    | Versioned versioned ->
      Ml.declare_type "t" ~tvars (Line (Ml.poly_type name ~tvars));
      Print.newline ();
      Ml.declare_type "concrete" ~tvars (Render.versioned_type_element versioned);
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
           (Printf.sprintf "%s -> %s option"
              (inst_node "t" ~tvars)
              (inst_node "concrete" ~tvars)));
      Print.newline ();
      declare_constructors versioned ~tvars
end

module Structure = struct
  module Render = Render (struct let internal = true end)

  let find_kind (grammar : Astlib.Grammar.t) name =
    match List.assoc grammar name with
    | None -> assert false
    | Some kind -> kind

  let find_mono grammar name =
    match find_kind grammar name with
    | Mono decl -> decl
    | Poly _ -> assert false

  let find_poly grammar name args =
    match find_kind grammar name with
    | Mono _ -> assert false
    | Poly (vars, decl) ->
      Poly_env.subst_decl decl ~env:(Poly_env.create ~vars ~args)

  let rec ast_of_ty ~grammar ty =
    match (ty : Astlib.Grammar.ty) with
    | Var _ -> "Data.of_node"
    | Name name -> ast_of_decl ~grammar (find_mono grammar name)
    | Instance (name, args) -> ast_of_decl ~grammar (find_poly grammar name args)
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

  and ast_of_decl ~grammar decl =
    match (decl : Astlib.Grammar.decl) with
    | Unversioned ty -> ast_of_ty ~grammar ty
    | Versioned _ -> "Data.of_node"

  let rec ast_to_ty ~grammar ty =
    match (ty : Astlib.Grammar.ty) with
    | Var _ -> "Data.to_node"
    | Name name -> ast_to_decl ~grammar (find_mono grammar name)
    | Instance (name, args) -> ast_to_decl ~grammar (find_poly grammar name args)
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

  and ast_to_decl ~grammar decl =
    match (decl : Astlib.Grammar.decl) with
    | Unversioned ty -> ast_to_ty ~grammar ty
    | Versioned _ -> "Data.to_node"

  let tuple_var i = Ml.id (Printf.sprintf "x%d" (i + 1))

  let define_constructors versioned ~node_name ~grammar =
    match (versioned : Astlib.Grammar.versioned) with
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

  let define_of_concrete versioned =
    match (versioned : Astlib.Grammar.versioned) with
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
          Print.println "Helpers.Option.bind (%s %s) ~f:(fun %s ->"
            (ast_to_ty ~grammar ty)
            (Ml.id var)
            (Ml.id var);
          Print.indented (fun () ->
            loop rest)
      in
      loop alist;
      Print.println "%s" (String.make (List.length alist) ')')

  let define_to_concrete versioned ~node_name ~grammar =
    match (versioned : Astlib.Grammar.versioned) with
    | Wrapper ty ->
      Print.println "let to_concrete t =";
      Print.indented (fun () ->
        Print.println "match Node.to_node t ~version with";
        Print.println "| { name = %S; data } -> %s data"
          node_name
          (ast_to_ty ~grammar ty);
        Print.println "| _ -> None")
    | Record record ->
      Print.println "let to_concrete t =";
      Print.indented (fun () ->
        Print.println "match Node.to_node t ~version with";
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
      Print.println "let to_concrete t =";
      Print.indented (fun () ->
        Print.println "match Node.to_node t ~version with";
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

  let print decl ~node_name ~tvars ~grammar =
    match (decl : Astlib.Grammar.decl) with
    | Unversioned ty ->
      Ml.declare_type "t" ~tvars (Line (Render.string_of_ty ty))
    | Versioned versioned ->
      Ml.declare_type "t" ~tvars (Line (Ml.poly_type node_name ~tvars));
      Print.newline ();
      Ml.declare_type "concrete" ~tvars (Render.versioned_type_element versioned);
      Print.newline ();
      define_constructors versioned ~node_name ~grammar;
      Print.newline ();
      define_of_concrete versioned;
      Print.newline ();
      define_to_concrete versioned ~node_name ~grammar
end

module Unversioned = struct
  let types_of_grammar grammar =
    List.partition_map grammar ~f:(fun (type_name, kind) ->
      let tvars, decl =
        match (kind : Astlib.Grammar.kind) with
        | Mono decl -> [], decl
        | Poly (tvars, decl) -> tvars, decl
      in
      match decl with
      | Unversioned ty -> Left (type_name, tvars, ty)
      | Versioned _ -> Right (type_name, tvars))

  let combine_types types =
    List.sort_uniq (List.concat types) ~compare

  let all_types grammars =
    let structural, versioned =
      List.map grammars ~f:(fun (_, grammar) -> types_of_grammar grammar)
      |> List.unzip
    in
    combine_types structural, combine_types versioned
end

let print_ast_types grammars =
  let structural_types, versioned_types = Unversioned.all_types grammars in
  List.iter versioned_types ~f:(fun (type_name, tvars) ->
    let type_name_ = type_name ^ "_" in
    Ml.declare_type type_name_ ~tvars Empty);
  Print.newline ();
  List.iter versioned_types ~f:(fun (type_name, tvars) ->
    let type_name_ = type_name ^ "_" in
    Ml.declare_type type_name ~tvars
      (Line
         (Grammar.string_of_ty ~internal:true
            (Instance
               ("node",
                [Instance
                   (type_name_, List.map tvars ~f:(fun v -> Astlib.Grammar.Var v))])))));
  Print.newline ();
  List.iter structural_types ~f:(fun (type_name, tvars, ty) ->
    Ml.declare_type type_name ~tvars
      (Line (Grammar.string_of_ty ~internal:true ty)))

let print_versions_mli () =
  Print.newline ();
  let grammars = Astlib.History.versioned_grammars Astlib.history in
  print_ast_types grammars;
  Print.newline ();
  Ml.declare_modules grammars ~f:(fun _ grammar ->
    Ml.declare_modules grammar ~recursive:true ~f:(fun node_name kind ->
      match (kind : Astlib.Grammar.kind) with
      | Mono decl ->
        Signature.print decl ~name:node_name ~tvars:[]
      | Poly (tvars, decl) ->
        Signature.print decl ~name:node_name ~tvars))

let print_versions_ml () =
  Print.newline ();
  let grammars = Astlib.History.versioned_grammars Astlib.history in
  print_ast_types grammars;
  Print.newline ();
  Ml.define_modules grammars ~f:(fun version grammar ->
    Print.println "let version = %S" version;
    Print.println "let node name data = Node.of_node ~version { name; data }";
    Print.newline ();
    Ml.define_modules grammar ~f:(fun node_name kind ->
      match (kind : Astlib.Grammar.kind) with
      | Mono decl -> Structure.print decl ~node_name ~tvars:[] ~grammar
      | Poly (tvars, decl) ->
        Structure.print decl ~node_name ~tvars ~grammar))
