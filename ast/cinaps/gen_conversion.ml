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

let tvar var ~poly =
  if poly
  then Ml.id var
  else Ml.tvar var

let string_of_tvar var ~view ~poly =
  match view with
  | `ast | `concrete -> Ml.poly_inst "Unversioned.Types.node" ~args:[tvar var ~poly]
  | `parsetree -> tvar (var ^ "_") ~poly

let string_of_targ ~poly ~view targ =
  match (targ : Astlib.Grammar.targ) with
  | Tname name -> type_name name ~view
  | Tvar var -> string_of_tvar var ~view ~poly

let rec string_of_ty ty ~view ~poly =
  match (ty : Astlib.Grammar.ty) with
  | Var var -> string_of_tvar var ~view ~poly
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
    string_of_ty ty ~view ~poly ^ suffix
  | List ty -> string_of_ty ty ~view ~poly ^ " list"
  | Option ty -> string_of_ty ty ~view ~poly ^ " option"
  | Tuple tuple -> string_of_tuple tuple ~view ~poly
  | Instance (name, args) ->
    let arg_view =
      match view with
      | `ast | `concrete -> `ast
      | `parsetree -> `parsetree
    in
    Ml.poly_inst
      (type_name name ~view)
      ~args:(List.map args ~f:(string_of_targ ~view:arg_view ~poly))

and string_of_tuple tuple ~view ~poly =
  Printf.sprintf "(%s)"
    (String.concat ~sep:" * " (List.map tuple ~f:(string_of_ty ~view ~poly)))

let ast_ty ty ~poly = string_of_ty ty ~view:`ast ~poly
let concrete_ty ty ~poly = string_of_ty ty ~view:`concrete ~poly
let parsetree_ty ty ~poly = string_of_ty ty ~view:`parsetree ~poly

let parsetree_to_ast_args ~poly ~tvars =
  List.map tvars ~f:(fun tvar ->
    "(" ^ Ml.arrow_type [parsetree_ty ~poly (Var tvar); ast_ty ~poly (Var tvar)] ^ ")")

let ast_to_parsetree_args ~poly ~tvars =
  List.map tvars ~f:(fun tvar ->
    "(" ^ Ml.arrow_type [ast_ty ~poly (Var tvar); parsetree_ty ~poly (Var tvar)] ^ ")")

let print_conversion_intf ~node_name ~self_type ~tvars =
  let poly = false in
  let ast_ty = ast_ty self_type ~poly in
  let parsetree_ty = parsetree_ty self_type ~poly in
  Ml.declare_val (Name.make ["ast_of"; node_name] []) (Block (fun () ->
    Ml.print_arrow
      (parsetree_to_ast_args ~poly ~tvars @ [parsetree_ty])
      ast_ty
      ~f:(fun x -> x)));
  Print.newline ();
  Ml.declare_val (Name.make ["ast_to"; node_name] []) (Block (fun () ->
    Ml.print_arrow
      (ast_to_parsetree_args ~poly ~tvars @ [ast_ty])
      parsetree_ty
      ~f:(fun x -> x)))

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

let conversion_var tvar ~conv =
  Ml.id (conversion_prefix ~conv ^ "_" ^ tvar)

let conversion_arg targ ~conv =
  match (targ : Astlib.Grammar.targ) with
  | Tvar tvar -> conversion_var tvar ~conv
  | Tname name -> Ml.id (conversion_prefix ~conv ^ "_" ^ name)

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
  | Instance (poly, targs) ->
    Some (Printf.sprintf "(%s %s)"
            (Name.make [conversion_prefix ~conv; poly] [])
            (String.concat ~sep:" "
               (List.map targs ~f:(conversion_arg ~conv))))

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

let input_ty ty ~conv ~poly =
  match conv with
  | `concrete_of | `ast_of -> parsetree_ty ty ~poly
  | `concrete_to -> concrete_ty ty ~poly
  | `ast_to -> ast_ty ty ~poly

let output_ty ty ~conv ~poly =
  match conv with
  | `concrete_of -> concrete_ty ty ~poly
  | `concrete_to | `ast_to -> parsetree_ty ty ~poly
  | `ast_of -> ast_ty ty ~poly

let tuple_var i = Ml.id (Printf.sprintf "x%d" (i + 1))

let conversion_vars ~tvars ~conv =
  String.concat ~sep:""
    (List.map tvars ~f:(fun tvar -> " " ^ conversion_var tvar ~conv))

let poly_conversion_ty ~self_type ~tvars ~conv =
  let for_all =
    if List.is_empty tvars
    then ""
    else
      Printf.sprintf "type %s ."
        (String.concat ~sep:" "
           (List.map tvars ~f:(fun tvar ->
              Ml.id tvar ^ " " ^ Ml.id (tvar ^ "_"))))
  in
  let arrow =
    let args =
      List.map tvars ~f:(fun tvar ->
        Printf.sprintf "(%s)"
          (Ml.arrow_type
             [ input_ty (Var tvar) ~poly:true ~conv
             ; output_ty (Var tvar) ~poly:true ~conv
             ]))
    in
    Ml.arrow_type
      (args @
       [ input_ty self_type ~poly:true ~conv
       ; output_ty self_type ~poly:true ~conv
       ])
  in
  for_all ^ arrow

let define_conversion decl ~node_name ~self_type ~tvars ~conv =
  let name = Name.make [concrete_prefix ~conv; node_name] [] in
  Print.println "and %s" name;
  Print.indented (fun () ->
    Print.println ": %s" (poly_conversion_ty ~self_type ~tvars ~conv);
    match (decl : Astlib.Grammar.decl) with
    | Ty ty ->
      Print.println "= fun%s x ->"
        (conversion_vars ~tvars ~conv);
      Print.indented (fun () ->
        Print.println "%s x" (fn_value (ty_conversion ~conv ty)))
    | Record record ->
      let fields =
        String.concat ~sep:"; "
          (List.map record ~f:(fun (field, _) -> Ml.id field))
      in
      Print.println "= fun%s { %s } ->" (conversion_vars ~tvars ~conv) fields;
      Print.indented (fun () ->
        Print.indented (fun () ->
          List.iter record ~f:(fun (field, ty) ->
            match ty_conversion ~conv ty with
            | None -> ()
            | Some fn -> Print.println "let %s = %s %s in" (Ml.id field) fn (Ml.id field));
          Print.println "{ %s }" fields))
    | Variant variant ->
      Print.println "= fun%s x ->"
        (conversion_vars ~tvars ~conv);
      Print.indented (fun () ->
        Print.println "match (x : %s) with" (input_ty ~conv ~poly:false self_type);
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
                match ty_conversion ty ~conv with
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
                match ty_conversion ty ~conv with
                | None -> ()
                | Some fn ->
                  Print.println "let %s = %s %s in" (Ml.id field) fn (Ml.id field));
              Print.println "%s { %s }"
                (Ml.tag tag)
                (String.concat ~sep:"; "
                   (List.map record ~f:(fun (field, _) -> Ml.id field)))))))

let print_conversion_impl decl ~node_name ~self_type ~tvars ~is_initial =
    Print.println
      "%s %s"
      (if is_initial then "let rec" else "and")
      (Name.make ["ast_of"; node_name] []);
    Print.indented (fun () ->
      Print.println ": %s" (poly_conversion_ty ~self_type ~tvars ~conv:`ast_of);
      Print.println "= fun%s x ->"
        (conversion_vars ~tvars ~conv:`concrete_of);
      Print.indented (fun () ->
        Print.println "Versions.%s.%s.%s (%s%s x)"
          (Ml.module_name (Astlib.Version.to_string version))
          (Ml.module_name node_name)
          "of_concrete"
          (Name.make ["concrete_of"; node_name] [])
          (conversion_vars ~tvars ~conv:`concrete_of)));
    Print.newline ();
    define_conversion decl ~node_name ~self_type ~tvars ~conv:`concrete_of;
    Print.newline ();
    Print.println "and %s" (Name.make ["ast_to"; node_name] []);
    Print.indented (fun () ->
      Print.println ": %s" (poly_conversion_ty ~self_type ~tvars ~conv:`ast_to);
      Print.println "= fun%s x ->" (conversion_vars ~tvars ~conv:`concrete_to);
      Print.indented (fun () ->
        Print.println "let concrete = Versions.%s.%s.to_concrete x in"
          (Ml.module_name (Astlib.Version.to_string version))
          (Ml.module_name node_name);
        Print.println "%s%s concrete"
          (Name.make ["concrete_to"; node_name] [])
          (conversion_vars ~tvars ~conv:`concrete_to)));
    Print.newline ();
    define_conversion decl ~node_name ~self_type ~tvars ~conv:`concrete_to

let make_self_type ~node_name ~tvars : Astlib.Grammar.ty =
  if List.is_empty tvars
  then Name node_name
  else Instance (node_name, List.map tvars ~f:(fun tvar -> Astlib.Grammar.Tvar tvar))

let print_conversion_mli () =
  let grammar = Astlib.History.find_grammar Astlib.history ~version in
  List.iter grammar ~f:(fun (node_name, kind) ->
    let tvars =
      match (kind : Astlib.Grammar.kind) with
      | Mono _ -> []
      | Poly (tvars, _) -> tvars
    in
    let self_type = make_self_type ~node_name ~tvars in
    Print.newline ();
    print_conversion_intf ~node_name ~self_type ~tvars)

let print_conversion_ml () =
  let grammar = Astlib.History.find_grammar Astlib.history ~version in
  List.iteri grammar ~f:(fun node_index (node_name, kind) ->
    let tvars, decl =
      match (kind : Astlib.Grammar.kind) with
      | Mono decl -> [], decl
      | Poly (tvars, decl) -> tvars, decl
    in
    let self_type = make_self_type ~node_name ~tvars in
    Print.newline ();
    print_conversion_impl decl ~node_name ~self_type ~tvars ~is_initial:(node_index = 0))
