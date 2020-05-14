open Stdppx
open Ppx_ast_cinaps

let string_of_targ targ =
  match (targ : Astlib.Grammar.targ) with
  | Tname name -> Ml.id name
  | Tvar var -> Ml.tvar var

let rec string_of_ty ty =
  match (ty : Astlib.Grammar.ty) with
  | Var var -> Ml.tvar var
  | Name name -> Ml.id name
  | Instance (poly, args) ->
    Ml.poly_inst poly ~args:(List.map args ~f:string_of_targ)
  | Bool -> "bool"
  | Char -> "char"
  | Int -> "int"
  | String -> "string"
  | Location -> "location"
  | Loc ty -> Ml.poly_inst "loc" ~args:[string_of_ty ty]
  | List ty -> Ml.poly_inst "list" ~args:[string_of_ty ty]
  | Option ty -> Ml.poly_inst "option" ~args:[string_of_ty ty]
  | Tuple tuple ->
    Printf.sprintf "(%s)"
      (String.concat ~sep:" * "
         (List.map tuple ~f:string_of_ty))

let print_decl_intf ~name ~tvars =
  Print.println "type %s = %s"
    (Ml.poly_type "t" ~tvars)
    (Ml.poly_type ("Compiler_types." ^ name) ~tvars);
  Print.println "[@@deriving equal, quickcheck, sexp_of]"

let print_deriving_mli () =
  let grammar =
    Astlib.History.find_grammar Astlib.history ~version:Astlib.current_version
  in
  List.iter grammar ~f:(fun (name, kind) ->
    Print.newline ();
    Ml.declare_module name (fun () ->
      match (kind : Astlib.Grammar.kind) with
      | Mono _ -> print_decl_intf ~name ~tvars:[]
      | Poly (tvars, _) -> print_decl_intf ~name ~tvars))

let print_decl_impl ~name ~tvars =
  Print.println "type %s = %s" (Ml.poly_type "t" ~tvars) (Ml.poly_type name ~tvars);
  Print.println "[@@deriving equal, quickcheck, sexp_of]"

let print_deriving_type decl ~index ~name ~tvars =
  Print.println "%s %s ="
    (if index = 0 then "type" else "and")
    (Ml.poly_type name ~tvars);
  Print.indented (fun () ->
    match (decl : Astlib.Grammar.decl) with
    | Ty ty -> Print.println "%s" (string_of_ty ty)
    | Record record ->
      Print.println "%s =" (Ml.poly_type ("Compiler_types." ^ name) ~tvars);
      Ml.print_record_type record ~f:string_of_ty
    | Variant variant ->
      Print.println "%s =" (Ml.poly_type ("Compiler_types." ^ name) ~tvars);
      Ml.print_variant_type variant ~f:(fun clause ->
        match (clause : Astlib.Grammar.clause) with
        | Empty -> Empty
        | Tuple tuple -> Line (String.concat ~sep:" * " (List.map tuple ~f:string_of_ty))
        | Record record ->
          Block (fun () -> Ml.print_record_type record ~f:string_of_ty)))

let poly_generator_args ~tvars =
  String.concat ~sep:""
    (List.map tvars ~f:(fun tvar ->
       " " ^ Ml.id ("quickcheck_generator_" ^ tvar)))

let underscores ~tvars =
  String.concat ~sep:""
    (List.map tvars ~f:(fun _ -> " _"))

let tvars_of kind =
  match (kind : Astlib.Grammar.kind) with
  | Mono _ -> []
  | Poly (tvars, _) -> tvars

let var_generator var = Ml.id ("quickcheck_generator_" ^ var)

let name_generator name =
    Printf.sprintf "(Generator.create generate_%s)" (Ml.id name)

let generator_targ targ =
  match (targ : Astlib.Grammar.targ) with
  | Tvar var -> var_generator var
  | Tname name -> name_generator name

let rec generator_string ty =
  match (ty : Astlib.Grammar.ty) with
  | Var var -> var_generator var
  | Name name -> name_generator name
  | Instance (poly, args) ->
    Printf.sprintf "(Generator.create (%s %s))"
      (Ml.id ("generate_" ^ poly))
      (String.concat ~sep:" "
         (List.map args ~f:generator_targ))
  | Bool -> "quickcheck_generator_bool"
  | Char -> "quickcheck_generator_char"
  | Int -> "quickcheck_generator_int"
  | String -> "quickcheck_generator_string"
  | Location -> "quickcheck_generator_location"
  | Loc ty -> Printf.sprintf "(quickcheck_generator_loc %s)" (generator_string ty)
  | List ty -> Printf.sprintf "(quickcheck_generator_list %s)" (generator_string ty)
  | Option ty -> Printf.sprintf "(quickcheck_generator_option %s)" (generator_string ty)
  | Tuple tuple ->
    Printf.sprintf "(quickcheck_generator_tuple%d %s)"
      (List.length tuple)
      (String.concat ~sep:" "
         (List.map tuple ~f:generator_string))

let rec ty_is_recursive ty =
  match (ty : Astlib.Grammar.ty) with
  | Name _ | Instance _ -> true
  | Var _ | Bool | Char | Int | String | Location -> false
  | Loc ty | List ty | Option ty -> ty_is_recursive ty
  | Tuple tuple -> List.exists tuple ~f:ty_is_recursive

let clause_is_recursive clause =
  match (clause : Astlib.Grammar.clause) with
  | Empty -> false
  | Tuple tuple -> List.exists tuple ~f:ty_is_recursive
  | Record record -> List.exists record ~f:(fun (_, ty) -> ty_is_recursive ty)

let gen_id name = Ml.id ("gen_" ^ name)

let print_fields_gen record =
  List.iteri record ~f:(fun index (field, ty) ->
    let id = gen_id field in
    let generator_string =
      let open Astlib.Grammar in
      match field, ty with
      | s, (List Location) when String.is_suffix ~suffix:"loc_stack" s ->
        "Generator.return []"
      | _ -> generator_string ty
    in
    Print.println "%s %s = %s"
      (if index = 0 then "let" else "and")
      id
      generator_string);
  Print.println "in"

let print_quickcheck_generator decl ~index ~name ~tvars =
  if List.length tvars = 0
  then
    Print.println "%s %s ~size ~random ="
      (if index = 0 then "let rec" else "and")
      (Ml.id ("generate_" ^ name))
  else (
    Print.println "%s %s"
      (if index = 0 then "let rec" else "and")
      (Ml.id ("generate_" ^ name));
    Print.indented (fun () ->
      Print.println ": type %s . %s -> size:int -> random:Splittable_random.State.t -> %s"
        (String.concat ~sep:" "
           (List.map tvars ~f:Ml.id))
        (String.concat ~sep:" -> "
           (List.map tvars ~f:(fun tvar ->
              Printf.sprintf "%s Generator.t" (Ml.id tvar))))
        (Ml.poly_inst name ~args:(List.map tvars ~f:Ml.id));
      Print.println "= fun %s ~size ~random ->"
        (String.concat ~sep:" "
           (List.map tvars ~f:(fun tvar ->
              Ml.id ("quickcheck_generator_" ^ tvar))))));
  Print.indented (fun () ->
    match (decl : Astlib.Grammar.decl) with
    | Ty ty ->
      Print.println "let gen = %s in" (generator_string ty);
      Print.println "Generator.generate gen ~size ~random"
    | Record record ->
      print_fields_gen record;
      List.iteri record ~f:(fun index (field, _) ->
        Print.println "%s %s = Generator.generate gen_%s ~size ~random"
          (if index = 0 then "{" else ";")
          (Ml.id field)
          (Ml.id field));
      Print.println "}"
    | Variant variant ->
      List.iteri variant ~f:(fun index (tag, clause) ->
        Print.println "%s %s =" (if index = 0 then "let" else "and") (gen_id tag);
        Print.indented (fun () ->
          match (clause : Astlib.Grammar.clause) with
          | Empty ->
            Print.println "Generator.return %s" (Ml.tag tag)
          | Tuple tuple ->
            Print.println "Generator.create (fun ~size ~random ->";
            Print.indented (fun () ->
              List.iteri tuple ~f:(fun index ty ->
                Print.println "%s gen%d = %s"
                  (if index = 0 then "let" else "and")
                  index
                  (generator_string ty));
              Print.println "in";
              Print.println "%s" (Ml.tag tag);
              Print.indented (fun () ->
                List.iteri tuple ~f:(fun index _ ->
                  Print.println
                    "%s Generator.generate gen%d ~size ~random"
                    (if index = 0 then "(" else ",")
                    index);
                Print.println "))"))
          | Record record ->
            Print.println "Generator.create (fun ~size ~random ->";
            Print.indented (fun () ->
              List.iteri record ~f:(fun index (field, ty) ->
                Print.println "%s %s = %s"
                  (if index = 0 then "let" else "and")
                  (gen_id field)
                  (generator_string ty));
              Print.println "in";
              Print.println "%s" (Ml.tag tag);
              Print.indented (fun () ->
                List.iteri record ~f:(fun index (field, _) ->
                  Print.println
                    "%s %s = Generator.generate %s ~size ~random"
                    (if index = 0 then "{" else ";")
                    (Ml.id field)
                    (gen_id field));
                Print.println "})"))));
      Print.println "in";
      match
        List.partition variant ~f:(fun (_, clause) ->
          clause_is_recursive clause)
      with
      | [], _ | _, [] ->
        Print.println "Generator.generate ~size ~random";
        Print.indented (fun () ->
          Print.println "(Base_quickcheck.Generator.union";
          Print.indented (fun () ->
            Print.println "[%s])"
              (String.concat ~sep:"; "
                 (List.map variant ~f:(fun (tag, _) ->
                    gen_id tag)))))
      | ((_ :: _) as recursive), ((_ :: _) as nonrecursive) ->
        Print.println "if size = 0";
        Print.println "then";
        Print.indented (fun () ->
          Print.println "Generator.generate ~size ~random";
          Print.indented (fun () ->
            Print.println "(Base_quickcheck.Generator.union";
            Print.indented (fun () ->
              Print.println "[%s])"
                (String.concat ~sep:"; "
                   (List.map nonrecursive ~f:(fun (tag, _) ->
                      gen_id tag))))));
        Print.println "else";
        Print.indented (fun () ->
          Print.println "Generator.generate ~size:(size-1) ~random";
          Print.indented (fun () ->
            Print.println "(Base_quickcheck.Generator.union";
            Print.indented (fun () ->
              Print.println "[%s])"
                (String.concat ~sep:"; "
                   (List.map (nonrecursive @ recursive) ~f:(fun (tag, _) ->
                      gen_id tag)))))))

let print_deriving_quickcheck grammar =
  List.iteri grammar ~f:(fun index (name, kind) ->
    match (kind : Astlib.Grammar.kind) with
    | Mono decl -> print_quickcheck_generator decl ~index ~name ~tvars:[]
    | Poly (tvars, decl) -> print_quickcheck_generator decl ~index ~name ~tvars);
  Print.newline ();
  List.iter grammar ~f:(fun (name, kind) ->
    match (kind : Astlib.Grammar.kind) with
    | Mono _ ->
      Print.println "let quickcheck_generator_%s =" (Ml.id name);
      Print.indented (fun () ->
        Print.println "Generator.create generate_%s" (Ml.id name))
    | Poly (tvars, _) ->
      let args = poly_generator_args ~tvars in
      Print.println "let quickcheck_generator_%s%s =" (Ml.id name) args;
      Print.indented (fun () ->
        Print.println "Generator.create (generate_%s%s)"
          (Ml.id name)
          args));
  Print.newline ();
  List.iter grammar ~f:(fun (name, kind) ->
    Print.println "let quickcheck_observer_%s%s = Observer.opaque"
      (Ml.id name)
      (underscores ~tvars:(tvars_of kind)));
  Print.newline ();
  List.iter grammar ~f:(fun (name, kind) ->
    Print.println "let quickcheck_shrinker_%s%s = Shrinker.atomic"
      (Ml.id name)
      (underscores ~tvars:(tvars_of kind)))

let print_deriving_ml () =
  let grammar =
    Astlib.History.find_grammar Astlib.history ~version:Astlib.current_version
  in
  List.iteri grammar ~f:(fun index (name, kind) ->
    Print.newline ();
    match (kind : Astlib.Grammar.kind) with
    | Mono decl -> print_deriving_type decl ~index ~name ~tvars:[]
    | Poly (tvars, decl) -> print_deriving_type decl ~index ~name ~tvars);
  Print.println "[@@deriving equal, sexp_of]";
  Print.newline ();
  print_deriving_quickcheck grammar;
  List.iter grammar ~f:(fun (name, kind) ->
    Print.newline ();
    Ml.define_module name (fun () ->
      match (kind : Astlib.Grammar.kind) with
      | Mono _ -> print_decl_impl ~name ~tvars:[]
      | Poly (tvars, _) -> print_decl_impl ~name ~tvars))

let entry_points = ["signature"; "structure"; "toplevel_phrase"]

let print_test name ~version =
  Print.println "let%%expect_test %S =" name;
  Print.indented (fun () ->
    Print.println "Test.run_exn ~config";
    Print.indented (fun () ->
      Print.println "(module Deriving.%s)" (Ml.module_name name);
      Print.println "~f:(fun x ->";
      Print.indented (fun () ->
        Print.println "try";
        Print.indented (fun () ->
          Print.println "require_equal [%%here] (module Deriving.%s) x"
            (Ml.module_name name);
          Print.indented (fun () ->
            Print.println
              "(Conversion.ast_to_%s"
              (Ml.id name);
            Print.indented (fun () ->
              Print.println
                "((new %s.map)#%s"
                (Ml.module_name (Astlib.Version.to_string version))
                (Ml.id name);
              Print.indented (fun () ->
                Print.println
                  "(Conversion.ast_of_%s x)))"
                  (Ml.id name)))));
        Print.println "with Unversioned.Private.Cannot_interpret_ast _ -> ());"));
    Print.println "[%%expect {| |}]")

let print_test_ml () =
  let alist = Astlib.History.versioned_grammars Astlib.history in
  List.iter alist ~f:(fun (version, grammar) ->
    Print.newline ();
    Ml.define_module (Astlib.Version.to_string version) (fun () ->
      let have_printed = ref false in
      List.iter grammar ~f:(fun (name, kind) ->
        match (kind : Astlib.Grammar.kind) with
        | Poly _ -> ()
        | Mono _ ->
          if List.mem name ~set:entry_points
          then (
            if !have_printed then Print.newline ();
            have_printed := true;
            print_test name ~version))))
