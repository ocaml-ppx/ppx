open StdLabels

module Render (Config : sig val internal : bool end) = struct
  let rec string_of_ty ty =
    match (ty : Astlib.Grammar.ty) with
    | Var var -> Ml.tvar var
    | Name name -> if Config.internal then "Node.t" else Ml.module_name name ^ ".t"
    | Bool -> "bool"
    | Char -> "char"
    | Int -> "int"
    | String -> "string"
    | Location -> "Astlib.Location.t"
    | Loc ty -> string_of_ty ty ^ " Astlib.Loc.t"
    | List ty -> string_of_ty ty ^ " list"
    | Option ty -> string_of_ty ty ^ " option"
    | Tuple tuple -> string_of_tuple_type tuple
    | Instance (poly, args) ->
      Ml.poly_inst (poly ^ ".t") ~args:(List.map args ~f:(string_of_ty))

  and string_of_tuple_type ?(parens = true) tuple =
    Printf.sprintf "%s%s%s"
      (if parens then "(" else "")
      (String.concat ~sep:" * " (List.map tuple ~f:string_of_ty))
      (if parens then ")" else "")

  let print_record_type record =
    Ml.print_record_type record ~f:string_of_ty

  let clause_type_element clause : Ml.element =
    match (clause : Astlib.Grammar.clause) with
    | Empty -> Empty
    | Tuple tuple -> Line (string_of_tuple_type tuple ~parens:false)
    | Record record -> Block (fun () -> print_record_type record)

  let print_variant_type variant =
    Ml.print_variant_type variant ~f:clause_type_element

  let decl_type_element decl : Ml.element =
    match (decl : Astlib.Grammar.decl) with
    | Alias ty -> Line (string_of_ty ty)
    | Record record -> Block (fun () -> print_record_type record)
    | Variant variant -> Block (fun () -> print_variant_type variant)
end

module Signature = struct
  module Render = Render (struct let internal = false end)

  let inst ty ~env =
    Ml.poly_inst ty ~args:(List.map (Poly_env.args env) ~f:Render.string_of_ty)

  let declare_constructors decl ~env =
    let string_of_ty ty = Render.string_of_ty (Poly_env.subst_ty ty ~env) in
    match (decl : Astlib.Grammar.decl) with
    | Alias ty ->
      Ml.declare_val
        (Name.make ["create"] (Poly_env.args env))
        (Line (Printf.sprintf "%s -> %s" (string_of_ty ty) (inst "t" ~env)))
    | Record record ->
      Ml.declare_val
        (Name.make ["create"] (Poly_env.args env))
        (Block (fun () ->
           Ml.print_labelled_arrow record ~f:string_of_ty (inst "t" ~env)))
    | Variant variant ->
      List.iter variant ~f:(fun (tag, clause) ->
        Ml.declare_val
          (Name.make [tag] (Poly_env.args env))
          (match (clause : Astlib.Grammar.clause) with
           | Empty -> Line (inst "t" ~env)
           | Tuple tuple ->
             Block (fun () ->
               Ml.print_arrow tuple ~f:string_of_ty (inst "t" ~env))
           | Record record ->
             Block (fun () ->
               Ml.print_labelled_arrow record ~f:string_of_ty (inst "t" ~env))))

  let print decl ~tvars ~envs =
    Ml.declare_type "t" ~tvars Empty;
    Print.newline ();
    Ml.declare_type "concrete" ~tvars (Render.decl_type_element decl);
    List.iter envs ~f:(fun env ->
      Print.newline ();
      Ml.declare_val
        (Name.make ["of_concrete"] (Poly_env.args env))
        (Line
           (Printf.sprintf "%s -> %s"
              (inst "concrete" ~env)
              (inst "t" ~env)));
      Ml.declare_val
        (Name.make ["to_concrete"] (Poly_env.args env))
        (Line
           (Printf.sprintf "%s -> %s option"
              (inst "t" ~env)
              (inst "concrete" ~env)));
      Print.newline ();
      declare_constructors decl ~env)
end

module Structure = struct
  module Render = Render (struct let internal = true end)

  let rec ast_of_ty ty =
    match (ty : Astlib.Grammar.ty) with
    | Var var -> Ml.id ("ast_of_" ^ var)
    | Name _ | Instance _ -> "Data.of_node"
    | Bool -> "Data.of_bool"
    | Char -> "Data.of_char"
    | Int -> "Data.of_int"
    | String -> "Data.of_string"
    | Location -> "Data.of_location"
    | Loc ty -> Printf.sprintf "(Data.of_loc ~f:%s)" (ast_of_ty ty)
    | List ty -> Printf.sprintf "(Data.of_list ~f:%s)" (ast_of_ty ty)
    | Option ty -> Printf.sprintf "(Data.of_option ~f:%s)" (ast_of_ty ty)
    | Tuple tuple ->
      Printf.sprintf "(Data.of_tuple%d %s)"
        (List.length tuple)
        (String.concat ~sep:" "
           (List.mapi tuple ~f:(fun i ty ->
              Printf.sprintf "~f%d:%s" (i + 1) (ast_of_ty ty))))

  let rec ast_to_ty ty =
    match (ty : Astlib.Grammar.ty) with
    | Var var -> Ml.id ("ast_to_" ^ var)
    | Name _ | Instance _ -> "Data.to_node"
    | Bool -> "Data.to_bool"
    | Char -> "Data.to_char"
    | Int -> "Data.to_int"
    | String -> "Data.to_string"
    | Location -> "Data.to_location"
    | Loc ty -> Printf.sprintf "(Data.to_loc ~f:%s)" (ast_to_ty ty)
    | List ty -> Printf.sprintf "(Data.to_list ~f:%s)" (ast_to_ty ty)
    | Option ty -> Printf.sprintf "(Data.to_option ~f:%s)" (ast_to_ty ty)
    | Tuple tuple ->
      Printf.sprintf "(Data.to_tuple%d %s)"
        (List.length tuple)
        (String.concat ~sep:" "
           (List.mapi tuple ~f:(fun i ty ->
              Printf.sprintf "~f%d:%s" (i + 1) (ast_to_ty ty))))

  let tuple_var i = Ml.id (Printf.sprintf "x%d" (i + 1))

  let define_constructors decl ~tvars ~node_name =
    let poly_args =
      String.concat ~sep:""
        (List.map tvars ~f:(fun tvar ->
           " " ^ Ml.id ("ast_of_" ^ tvar)))
    in
    match (decl : Astlib.Grammar.decl) with
    | Alias ty ->
      Print.println "let create%s =" poly_args;
      Print.indented (fun () ->
        Print.println "let data = %s in" (ast_of_ty ty);
        Print.println "fun x -> node %S (data x)" node_name)
    | Record record ->
      Print.println "let create%s %s ="
        poly_args
        (String.concat ~sep:" "
           (List.map record ~f:(fun (field, _) ->
              Printf.sprintf "~%s" (Ml.id field))));
      Print.indented (fun () ->
        Print.println "let fields =";
        Print.indented (fun () ->
          Ml.print_array record ~f:(fun _ (field, ty) ->
            Printf.sprintf "%s %s" (ast_of_ty ty) (Ml.id field)));
        Print.println "in";
        Print.println "node %S (Record fields)" node_name)
    | Variant variant ->
      List.iter variant ~f:(fun (tag, clause) ->
        match (clause : Astlib.Grammar.clause) with
        | Empty ->
          Print.println "let %s%s =" (Name.make [tag] []) poly_args;
          Print.indented (fun () ->
            Print.println "node %S (Variant { tag = %S; args = [||] })" node_name tag)
        | Tuple tuple ->
          Print.println "let %s%s %s ="
            (Name.make [tag] [])
            poly_args
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
                    Printf.sprintf "%s %s" (ast_of_ty ty) (tuple_var i)));
                Print.println "})")))
        | Record record ->
          Print.println "let %s%s %s ="
            (Name.make [tag] [])
            poly_args
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
                    Printf.sprintf "%s %s" (ast_of_ty ty) (Ml.id field)));
                Print.println "})"))))

  let underscore ~tvars =
    match tvars with
    | [] -> ""
    | _ :: _ -> "_ "

  let define_of_concrete decl ~tvars =
    let poly_args =
      String.concat ~sep:""
        (List.map tvars ~f:(fun tvar ->
           " " ^ Ml.id ("ast_of_" ^ tvar)))
    in
    match (decl : Astlib.Grammar.decl) with
    | Alias _ -> Print.println "let of_concrete = create"
    | Record record ->
      Print.println "let of_concrete%s ({ %s } : %sconcrete) ="
        poly_args
        (String.concat ~sep:"; "
           (List.map record ~f:(fun (field, _) -> Ml.id field)))
        (underscore ~tvars);
      Print.indented (fun () ->
        Print.println "create%s %s"
          poly_args
          (String.concat ~sep:" "
             (List.map record ~f:(fun (field, _) -> "~" ^ Ml.id field))))
    | Variant variant ->
      Print.println "let of_concrete%s (c : %sconcrete) ="
        poly_args
        (underscore ~tvars);
      Print.indented (fun () ->
        Print.println "match c with";
        List.iter variant ~f:(fun (tag, clause) ->
          match (clause : Astlib.Grammar.clause) with
          | Empty -> Print.println "| %s -> %s" (Ml.tag tag) (Name.make [tag] [])
          | Tuple tuple ->
            let vars = List.mapi tuple ~f:(fun i _ -> tuple_var i) in
            Print.println "| %s (%s) ->" (Ml.tag tag) (String.concat ~sep:", " vars);
            Print.indented (fun () ->
              Print.println "%s%s %s"
                (Name.make [tag] [])
                poly_args
                (String.concat ~sep:" " vars))
          | Record record ->
            Print.println "| %s { %s } ->"
              (Ml.tag tag)
              (String.concat ~sep:"; "
                 (List.map record ~f:(fun (field, _) -> Ml.id field)));
            Print.indented (fun () ->
              Print.println "%s%s %s"
                (Name.make [tag] [])
                poly_args
                (String.concat ~sep:" "
                   (List.map record ~f:(fun (field, _) -> "~" ^ Ml.id field))))))

  let with_ast_to_ty_bindings alist ~f =
    match alist with
    | [] -> f ()
    | _ ->
      let rec loop alist =
        match alist with
        | [] -> f ()
        | (var, ty) :: rest ->
          Print.println "Helpers.Option.bind (%s %s) ~f:(fun %s ->"
            (ast_to_ty ty)
            (Ml.id var)
            (Ml.id var);
          Print.indented (fun () ->
            loop rest)
      in
      loop alist;
      Print.println "%s" (String.make (List.length alist) ')')

  let define_to_concrete decl ~node_name ~tvars =
    let poly_args =
      String.concat ~sep:""
        (List.map tvars ~f:(fun tvar ->
           " " ^ Ml.id ("ast_to_" ^ tvar)))
    in
    match (decl : Astlib.Grammar.decl) with
    | Alias ty ->
      Print.println "let to_concrete%s (t : %st) =" poly_args (underscore ~tvars);
      Print.indented (fun () ->
        Print.println "match Node.to_node t ~version with";
        Print.println "| { name = %S; data } -> %s data" node_name (ast_to_ty ty);
        Print.println "| _ -> None")
    | Record record ->
      Print.println "let to_concrete%s (t : %st) : %sconcrete option ="
        poly_args
        (underscore ~tvars)
        (underscore ~tvars);
      Print.indented (fun () ->
        Print.println "match Node.to_node t ~version with";
        Print.println "| { name = %S" node_name;
        Print.indented (fun () ->
          Print.println "; data = Record [| %s |]"
            (String.concat ~sep:"; "
               (List.map record ~f:(fun (field, _) -> Ml.id field)));
          Print.println "} ->";
          Print.indented (fun () ->
            with_ast_to_ty_bindings record ~f:(fun () ->
              Print.println "Some { %s }"
                (String.concat ~sep:"; "
                   (List.map record ~f:(fun (field, _) -> Ml.id field))))));
        Print.println "| _ -> None")
    | Variant variant ->
      Print.println "let to_concrete%s (t : %st) : %sconcrete option ="
        poly_args
        (underscore ~tvars)
        (underscore ~tvars);
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
                  with_ast_to_ty_bindings
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
                  with_ast_to_ty_bindings
                    record
                    ~f:(fun () ->
                      Print.println "Some (%s { %s })"
                        (Ml.tag tag)
                        (String.concat ~sep:"; "
                           (List.map record ~f:(fun (field, _) -> Ml.id field)))))));
          Print.println "| _ -> None";
          Print.println "end");
        Print.println "| _ -> None")

  let define_instances decl ~env =
    Print.newline ();
    Print.println "let %s =" (Name.make ["of_concrete"] (Poly_env.args env));
    Print.indented (fun () ->
      Print.println "of_concrete";
      Print.indented (fun () ->
        List.iter (Poly_env.args env) ~f:(fun ty ->
          Print.println "%s" (ast_of_ty ty))));
    Print.newline ();
    Print.println "let %s =" (Name.make ["to_concrete"] (Poly_env.args env));
    Print.indented (fun () ->
      Print.println "to_concrete";
      Print.indented (fun () ->
        List.iter (Poly_env.args env) ~f:(fun ty ->
          Print.println "%s" (ast_to_ty ty))));
    match (decl : Astlib.Grammar.decl) with
    | Alias _ | Record _ ->
      Print.newline ();
      Print.println "let %s =" (Name.make ["create"] (Poly_env.args env));
      Print.indented (fun () ->
        Print.println "create";
        Print.indented (fun () ->
          List.iter (Poly_env.args env) ~f:(fun ty ->
            Print.println "%s" (ast_of_ty ty))))
    | Variant variant ->
      List.iter variant ~f:(fun (tag, _) ->
        Print.newline ();
        Print.println "let %s =" (Name.make [tag] (Poly_env.args env));
        Print.indented (fun () ->
          Print.println "%s" (Name.make [tag] []);
          Print.indented (fun () ->
            List.iter (Poly_env.args env) ~f:(fun ty ->
              Print.println "%s" (ast_of_ty ty)))))

  let print decl ~node_name ~tvars ~envs =
    Ml.declare_type "t" ~tvars (Line "Node.t");
    Print.newline ();
    Ml.declare_type "concrete" ~tvars (Render.decl_type_element decl);
    Print.newline ();
    define_constructors decl ~node_name ~tvars;
    Print.newline ();
    define_of_concrete decl ~tvars;
    Print.newline ();
    define_to_concrete decl ~node_name ~tvars;
    List.iter envs ~f:(fun env ->
      if not (Poly_env.env_is_empty env)
      then define_instances decl ~env)
end

let print_versions_mli () =
  Print.newline ();
  let grammars = Astlib.History.versioned_grammars Astlib.history in
  Ml.declare_modules grammars ~f:(fun _ grammar ->
    let env_table = Poly_env.env_table grammar in
    Ml.declare_modules grammar ~recursive:true ~f:(fun node_name kind ->
      match (kind : Astlib.Grammar.kind) with
      | Mono decl ->
        Signature.print decl ~tvars:[] ~envs:[Poly_env.empty_env]
      | Poly (tvars, decl) ->
        let envs = Poly_env.find env_table node_name in
        Signature.print ~tvars ~envs decl))

let print_versions_ml () =
  Print.newline ();
  let grammars = Astlib.History.versioned_grammars Astlib.history in
  Ml.define_modules grammars ~f:(fun version grammar ->
    Print.println "let version = %S" version;
    Print.println "let node name data = Node.of_node ~version { name; data }";
    Print.newline ();
    let env_table = Poly_env.env_table grammar in
    Ml.define_modules grammar ~f:(fun node_name kind ->
      match (kind : Astlib.Grammar.kind) with
      | Mono decl -> Structure.print decl ~node_name ~tvars:[] ~envs:[Poly_env.empty_env]
      | Poly (tvars, decl) ->
        let envs = Poly_env.find env_table node_name in
        Structure.print decl ~node_name ~tvars ~envs))
