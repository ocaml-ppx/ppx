open Stdppx

let version = Astlib.current_version

let type_name name ~view =
  match view with
  | `ast ->
    Printf.sprintf "Versions.%s.%s.t"
      (Ml.module_name (Astlib.Version.to_string version))
      (Ml.module_name name)
  | `concrete ->
    Printf.sprintf "Versions.%s.%s.concrete"
      (Ml.module_name (Astlib.Version.to_string version))
      (Ml.module_name name)
  | `parsetree ->
    Printf.sprintf "Compiler_types.%s" (Ml.id name)

let string_of_targ ~view targ =
  match (targ : Astlib.Grammar.targ) with
  | Tname name -> type_name name ~view
  | Tvar var -> Ml.tvar var

let rec string_of_ty ty ~view =
  match (ty : Astlib.Grammar.ty) with
  | Var var -> Ml.tvar var
  | Name name -> type_name name ~view
  | Bool -> "bool"
  | Char -> "char"
  | Int -> "int"
  | String -> "string"
  | Location ->
    (match view with
     | `ast | `concrete -> "Astlib.Location.t"
     | `parsetree -> "Ocaml_common.Location.t")
  | Loc ty ->
    let suffix =
      match view with
      | `ast | `concrete -> " Astlib.Loc.t"
      | `parsetree -> " Ocaml_common.Location.loc"
    in
    string_of_ty ty ~view ^ suffix
  | List ty -> string_of_ty ty ~view ^ " list"
  | Option ty -> string_of_ty ty ~view ^ " option"
  | Tuple tuple -> string_of_tuple tuple ~view
  | Instance (poly, args) ->
    let arg_view =
      match view with
      | `ast | `concrete -> `ast
      | `parsetree -> `parsetree
    in
    Ml.poly_inst
      (type_name poly ~view)
      ~args:(List.map args ~f:(string_of_targ ~view:arg_view))

and string_of_tuple tuple ~view =
  Printf.sprintf "(%s)"
    (String.concat ~sep:" * " (List.map tuple ~f:(string_of_ty ~view)))

let ast_ty ty = string_of_ty ty ~view:`ast
let concrete_ty ty = string_of_ty ty ~view:`concrete
let parsetree_ty ty = string_of_ty ty ~view:`parsetree

let node_ty ~node_name ~env =
  if Poly_env.env_is_empty env
  then Astlib.Grammar.Name node_name
  else Astlib.Grammar.Instance (node_name, Poly_env.args env)

let print_conversion_intf ~node_name ~env =
  let ty = node_ty ~node_name ~env in
  let ast_ty = ast_ty ty in
  let parsetree_ty = parsetree_ty ty in
  Ml.declare_val (Name.make ["ast_of"; node_name] (Poly_env.args env)) (Block (fun () ->
    Ml.print_arrow [parsetree_ty] ast_ty ~f:(fun x -> x)));
  Print.newline ();
  Ml.declare_val (Name.make ["ast_to"; node_name] (Poly_env.args env)) (Block (fun () ->
    Ml.print_arrow [ast_ty] parsetree_ty ~f:(fun x -> x)))

let fn_value option =
  match option with
  | Some fn -> fn
  | None -> "Fn.id"

let conversion_prefix ~conv =
  match conv with
  | `concrete_of -> "ast_of"
  | `concrete_to -> "ast_to"

let concrete_prefix ~conv =
  match conv with
  | `concrete_of -> "concrete_of"
  | `concrete_to -> "concrete_to"

let rec ty_conversion ty ~conv =
  match (ty : Astlib.Grammar.ty) with
  | Var var -> Some (Ml.id (conversion_prefix ~conv ^ "_" ^ var))
  | Name name -> Some (Ml.id (conversion_prefix ~conv ^ "_" ^ name))
  | Bool | Char | Int | String -> None
  | Location -> None
  | Loc ty ->
    Option.map (ty_conversion ty ~conv) ~f:(fun ty_conv ->
      Printf.sprintf "(Astlib.Loc.map ~f:%s)" ty_conv)
  | List ty ->
    Option.map (ty_conversion ty ~conv)
      ~f:(Printf.sprintf "(List.map ~f:%s)")
  | Option ty ->
    Option.map (ty_conversion ty ~conv)
      ~f:(Printf.sprintf "(Option.map ~f:%s)")
  | Tuple tuple -> tuple_conversion tuple ~conv
  | Instance (poly, args) ->
    Some (Name.make [conversion_prefix ~conv; poly] args)

and tuple_conversion tuple ~conv =
  let conversions = List.map tuple ~f:(ty_conversion ~conv) in
  if List.for_all conversions ~f:Option.is_none
  then None
  else
    Some
      (Printf.sprintf "(Tuple.map%d %s)"
         (List.length tuple)
         (String.concat ~sep:" "
            (List.mapi conversions ~f:(fun i option ->
               Printf.sprintf "~f%d:%s" (i + 1) (fn_value option)))))

let input_ty ty ~conv =
  match conv with
  | `concrete_of -> parsetree_ty ty
  | `concrete_to -> concrete_ty ty

let output_ty ty ~conv =
  match conv with
  | `concrete_of -> concrete_ty ty
  | `concrete_to -> parsetree_ty ty

let tuple_var i = Ml.id (Printf.sprintf "x%d" (i + 1))

let define_conversion decl ~node_name ~env ~conv =
  let name = Name.make [concrete_prefix ~conv; node_name] (Poly_env.args env) in
  let node_ty = node_ty ~node_name ~env in
  match (decl : Astlib.Grammar.decl) with
  | Wrapper ty ->
    Print.println "and %s x =" name;
    Print.indented (fun () ->
      Print.println "%s x" (fn_value (ty_conversion ~conv (Poly_env.subst_ty ty ~env))))
  | Record record ->
    let fields =
      String.concat ~sep:"; "
        (List.map record ~f:(fun (field, _) -> Ml.id field))
    in
    Print.println "and %s" name;
    Print.indented (fun () ->
      Print.println "({ %s } : %s)" fields (input_ty ~conv node_ty));
    Print.println "=";
    Print.indented (fun () ->
      List.iter record ~f:(fun (field, ty) ->
        match ty_conversion ~conv (Poly_env.subst_ty ty ~env) with
        | None -> ()
        | Some fn -> Print.println "let %s = %s %s in" (Ml.id field) fn (Ml.id field));
      Print.println "({ %s } : %s)" fields (output_ty ~conv node_ty))
  | Variant variant ->
    Print.println "and %s x : %s =" name (output_ty ~conv node_ty);
    Print.indented (fun () ->
      Print.println "match (x : %s) with" (input_ty ~conv node_ty);
      List.iter variant ~f:(fun (tag, clause) ->
        match (clause : Astlib.Grammar.clause) with
        | Empty ->
          Print.println "| %s -> %s" (Ml.tag tag) (Ml.tag tag)
        | Tuple tuple ->
          Print.println "| %s (%s) ->"
            (Ml.tag tag)
            (String.concat ~sep:", " (List.mapi tuple ~f:(fun i _ -> tuple_var i)));
          Print.indented (fun () ->
            List.iteri tuple ~f:(fun i ty ->
              match ty_conversion (Poly_env.subst_ty ty ~env) ~conv with
              | None -> ()
              | Some fn ->
                Print.println "let %s = %s %s in" (tuple_var i) fn (tuple_var i));
            Print.println "%s (%s)"
              (Ml.tag tag)
              (String.concat ~sep:", "
                 (List.mapi tuple ~f:(fun i _ -> tuple_var i))))
        | Record record ->
          Print.println "| %s { %s } ->"
            (Ml.tag tag)
            (String.concat ~sep:"; "
               (List.map record ~f:(fun (field, _) -> Ml.id field)));
          Print.indented (fun () ->
            List.iter record ~f:(fun (field, ty) ->
              match ty_conversion (Poly_env.subst_ty ty ~env) ~conv with
              | None -> ()
              | Some fn ->
                Print.println "let %s = %s %s in" (Ml.id field) fn (Ml.id field));
            Print.println "%s { %s }"
              (Ml.tag tag)
              (String.concat ~sep:"; "
                 (List.map record ~f:(fun (field, _) -> Ml.id field))))))

let print_conversion_impl decl ~node_name ~env ~is_initial =
    Print.println
      "%s %s x ="
      (if is_initial then "let rec" else "and")
      (Name.make ["ast_of"; node_name] (Poly_env.args env));
    Print.indented (fun () ->
      Print.println "Versions.%s.%s.%s (%s x)"
        (Ml.module_name (Astlib.Version.to_string version))
        (Ml.module_name node_name)
        "of_concrete"
        (Name.make ["concrete_of"; node_name] (Poly_env.args env)));
    Print.newline ();
    define_conversion decl ~node_name ~env ~conv:`concrete_of;
    Print.newline ();
    Print.println "and %s x =" (Name.make ["ast_to"; node_name] (Poly_env.args env));
    Print.indented (fun () ->
      Print.println "let concrete = Versions.%s.%s.to_concrete x in"
        (Ml.module_name (Astlib.Version.to_string version))
        (Ml.module_name node_name);
      Print.println "%s concrete"
        (Name.make ["concrete_to"; node_name] (Poly_env.args env)));
    Print.newline ();
    define_conversion decl ~node_name ~env ~conv:`concrete_to

let print_conversion_mli () =
  let grammar = Astlib.History.find_grammar Astlib.history ~version in
  let env_table = Poly_env.env_table grammar in
  List.iter grammar ~f:(fun (node_name, kind) ->
    match (kind : Astlib.Grammar.kind) with
    | Mono _ ->
      Print.newline ();
      print_conversion_intf ~node_name ~env:Poly_env.empty_env
    | Poly _ ->
      List.iter (Poly_env.find env_table node_name) ~f:(fun env ->
        Print.newline ();
        print_conversion_intf ~node_name ~env))

let print_conversion_ml () =
  let grammar = Astlib.History.find_grammar Astlib.history ~version in
  let env_table = Poly_env.env_table grammar in
  List.iteri grammar ~f:(fun node_index (node_name, kind) ->
    match (kind : Astlib.Grammar.kind) with
    | Mono decl ->
      Print.newline ();
      print_conversion_impl decl ~node_name
        ~env:Poly_env.empty_env
        ~is_initial:(node_index = 0)
    | Poly (_, decl) ->
      List.iteri (Poly_env.find env_table node_name) ~f:(fun env_index env ->
        Print.newline ();
        let is_initial = node_index = 0 && env_index = 0 in
        print_conversion_impl decl ~node_name ~env ~is_initial))
