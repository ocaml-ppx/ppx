open Base

let rec type_of_data data ~opaque =
  match (data : Astlib_ast.Grammar.data) with
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | Kind name -> if opaque then name ^ ".t" else "Versioned_ast.t"
  | List data -> type_of_data data ~opaque ^ " list"
  | Option data -> type_of_data data ~opaque ^ " option"

let rec data_of_concrete : Astlib_ast.Grammar.data -> string = function
  | Bool -> "Versioned_value.of_bool"
  | Char -> "Versioned_value.of_char"
  | String -> "Versioned_value.of_string"
  | Kind _ -> "Versioned_value.of_ast"
  | List data -> Printf.sprintf "(Versioned_value.of_list ~f:%s)" (data_of_concrete data)
  | Option data ->
    Printf.sprintf "(Versioned_value.of_option ~f:%s)" (data_of_concrete data)

let rec data_to_concrete data =
  match (data : Astlib_ast.Grammar.data) with
  | Bool -> "Versioned_value.to_bool"
  | Char -> "Versioned_value.to_char"
  | String -> "Versioned_value.to_string"
  | Kind _ -> Printf.sprintf "Versioned_value.to_ast"
  | List data -> Printf.sprintf "(Versioned_value.to_list ~f:%s)" (data_to_concrete data)
  | Option data ->
    Printf.sprintf "(Versioned_value.to_option ~f:%s)" (data_to_concrete data)

let print_clause_type (clause : Astlib_ast.Grammar.clause) ~opaque =
  Print.format "| %s of { %s }"
    clause.clause_name
    (String.concat ~sep:"; "
       ("loc : Location.t" ::
        List.map clause.fields ~f:(fun (field : Astlib_ast.Grammar.field) ->
          field.field_name ^ " : " ^ type_of_data field.data ~opaque)))

let print_kind_type (kind : Astlib_ast.Grammar.kind) ~opaque =
  Print.format "type t =";
  Print.indented (fun () ->
    List.iter kind.clauses ~f:(print_clause_type ~opaque))

let print_clause_of_concrete ~kind_name (clause : Astlib_ast.Grammar.clause) =
  Print.format "| %s { %s } ->"
    clause.clause_name
    (String.concat ~sep:"; "
       ("loc" ::
        List.map clause.fields ~f:(fun (field : Astlib_ast.Grammar.field) ->
          field.field_name)));
  Print.indented (fun () ->
    Print.format "Versioned_ast.create ~version";
    Print.indented (fun () ->
      Print.format "{ kind = %S" kind_name;
      Print.format "; clause = %S" clause.clause_name;
      Print.format "; loc";
      (match clause.fields with
       | [] -> Print.format "; fields = []"
       | _ ->
         Print.format "; fields =";
         Print.indented ~levels:2 (fun () ->
           List.iteri clause.fields
             ~f:(fun field_index (field : Astlib_ast.Grammar.field) ->
               Print.format "%c { name = %S; value = %s %s }"
                 (if field_index = 0 then '[' else ';')
                 field.field_name
                 (data_of_concrete field.data)
                 field.field_name);
           Print.format "]"));
      Print.format "}"))

let print_clause_to_concrete ~kind_name (clause : Astlib_ast.Grammar.clause) =
  Print.format "| { kind = %S" kind_name;
  Print.indented (fun () ->
    Print.format "; clause = %S" clause.clause_name;
    Print.format "; loc";
    (match clause.fields with
     | [] -> Print.format "; fields = []"
     | _ ->
       Print.format "; fields =";
       Print.indented ~levels:2 (fun () ->
         List.iteri clause.fields
           ~f:(fun field_index (field : Astlib_ast.Grammar.field) ->
             Print.format "%c { name = %S; value = %s }"
               (if field_index = 0 then '[' else ';')
               field.field_name
               field.field_name);
         Print.format "]"));
    Print.format "} ->";
    let rec loop fields =
      match fields with
      | (field : Astlib_ast.Grammar.field) :: fields ->
        Print.format "Option.bind (%s %s) ~f:(fun %s ->"
          (data_to_concrete field.data)
          field.field_name
          field.field_name;
        Print.indented (fun () -> loop fields)
      | [] ->
        Print.format "Some (Concrete.%s { %s })%s"
          clause.clause_name
          (String.concat ~sep:"; "
             ("loc" ::
              List.map clause.fields ~f:(fun (field : Astlib_ast.Grammar.field) ->
                field.field_name)))
          (String.make (List.length clause.fields) ')')
    in
    loop clause.fields)

let print_kind_of_concrete (kind : Astlib_ast.Grammar.kind) =
  Print.format "let of_concrete : Concrete.t -> t = function";
  Print.indented (fun () ->
    List.iter kind.clauses ~f:(fun clause ->
      print_clause_of_concrete clause ~kind_name:kind.kind_name))

let print_kind_to_concrete (kind : Astlib_ast.Grammar.kind) =
  Print.format "let to_concrete t =";
  Print.indented (fun () ->
    Print.format "match Versioned_ast.convert t ~version with";
    List.iter kind.clauses ~f:(fun clause ->
      print_clause_to_concrete clause ~kind_name:kind.kind_name);
    Print.format "| _ -> None")

let print_kind_signature (kind : Astlib_ast.Grammar.kind) =
  Print.format "type t";
  Print.newline ();
  Print.format "val sexp_of_t : t -> Sexp.t";
  Print.format "val equal : t -> t -> bool";
  Print.format "val of_ast : Versioned_ast.t -> t";
  Print.format "val to_ast : t -> Versioned_ast.t";
  Print.newline ();
  Print.declare_module "Concrete" (fun () ->
    print_kind_type kind ~opaque:true);
  Print.newline ();
  Print.format "val of_concrete : Concrete.t -> t";
  Print.format "val to_concrete : t -> Concrete.t option"

let print_kind_structure (kind : Astlib_ast.Grammar.kind) =
  Print.format "type t = Versioned_ast.t";
  Print.newline ();
  Print.format "let sexp_of_t = Versioned_ast.sexp_of_t";
  Print.format "let equal = Versioned_ast.equal";
  Print.format "let of_ast t = t";
  Print.format "let to_ast t = t";
  Print.newline ();
  Print.define_module "Concrete" (fun () ->
    print_kind_type kind ~opaque:false);
  Print.newline ();
  print_kind_of_concrete kind;
  Print.newline ();
  print_kind_to_concrete kind

let print_grammar_mli grammar =
  Print.declare_recursive_modules
    (List.map grammar ~f:(fun (kind : Astlib_ast.Grammar.kind) ->
       (kind.kind_name,
        (fun () -> print_kind_signature kind))))

let print_grammar_ml grammar =
  List.iteri grammar ~f:(fun kind_index (kind : Astlib_ast.Grammar.kind) ->
    if kind_index > 0 then Print.newline ();
    Print.define_module kind.kind_name (fun () ->
      print_kind_structure kind))

let print_astlib_mli () =
  Print.newline ();
  Print.format "open Base";
  Print.format "open Ocaml_common";
  Astlib_ast.History.to_versioned_grammars Astlib_ast.For_testing.history
  |> List.iter ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.declare_module version (fun () ->
      print_grammar_mli grammar))
;;

let print_astlib_ml () =
  Print.newline ();
  Print.format "open Base";
  Print.format "open Ocaml_common";
  Astlib_ast.History.to_versioned_grammars Astlib_ast.For_testing.history
  |> List.iter ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.define_module version (fun () ->
      Print.format "let version = %S" version;
      Print.newline ();
      print_grammar_ml grammar))
;;

let print_astlib_test_mli () =
  Print.newline ();
  Print.newline ();
  Print.format "(** This signature is deliberately empty. *)";
  Print.newline ()
;;

let version_index_exn =
  let rec loop ~index ~version alist =
    match alist with
    | [] ->
      Base.raise_s
        (Sexp.message
           "Astlib_cinaps.version_index_exn: no such version"
           [("version", Atom version)])
    | (grammar_version, _) :: alist ->
      if String.equal grammar_version version
      then index
      else loop ~index:(index + 1) ~version alist
  in
  fun ~version alist -> loop ~index:0 ~version alist

let print_copy_test
      (kind : Astlib_ast.Grammar.kind)
      ~version
      ~index
      ~grammar_version
      ~grammar_index
  =
  Print.format "let%%expect_test \"%s copy\" =" grammar_version;
  Print.indented (fun () ->
    Print.format "Expect_test_helpers_base.require_does_not_raise [%%here] (fun () ->";
    Print.indented (fun () ->
      Print.format "Base_quickcheck.Test.run_exn";
      Print.indented (fun () ->
        Print.format "(module Generators.%s.%s)" version kind.kind_name;
        Print.format "~f:(fun x ->";
        Print.indented (fun () ->
          Print.format "let y =";
          Print.indented (fun () ->
            if grammar_index = index
            then (
              Print.format "Traversal.%s.%s.copy x" version kind.kind_name)
            else (
              Print.format "x";
              Print.format "|> Astlib.%s.%s.to_ast" version kind.kind_name;
              Print.format "|> Astlib.%s.%s.of_ast" grammar_version kind.kind_name;
              Print.format "|> Traversal.%s.%s.copy" grammar_version kind.kind_name;
              Print.format "|> Astlib.%s.%s.to_ast" grammar_version kind.kind_name;
              Print.format "|> Astlib.%s.%s.of_ast" version kind.kind_name;
              Print.format "|> Traversal.%s.%s.copy" version kind.kind_name));
          Print.format "in";
          Print.format "if not (Astlib.%s.%s.equal x y)" version kind.kind_name;
          Print.format "then (";
          Print.indented (fun () ->
            Print.format "let x = Astlib.%s.%s.sexp_of_t x in" version kind.kind_name;
            Print.format "let y = Astlib.%s.%s.sexp_of_t y in" version kind.kind_name;
            Print.format
              "raise_s (Sexp.message \"ASTs differ\" [(\"\", x); (\"\", y)]))));"))));
    Print.format "[%%expect {| |}]")

let print_completeness_test
      (kind : Astlib_ast.Grammar.kind)
      ~version
      ~index
      ~grammar_version
      ~grammar_index
  =
  Print.format "let%%expect_test \"%s completeness\" =" grammar_version;
  Print.indented (fun () ->
    Print.format "Expect_test_helpers_base.require_does_not_raise [%%here] (fun () ->";
    Print.indented (fun () ->
      Print.format "Base_quickcheck.Test.run_exn";
      Print.indented (fun () ->
        Print.format "(module Generators.%s.%s)" version kind.kind_name;
        if grammar_index = index
        then
          Print.format "~f:Completeness.%s.%s.check_exn);"
            grammar_version
            kind.kind_name
        else (
          Print.format "~f:(fun x ->";
          Print.indented (fun () ->
            Print.format "x";
            Print.format "|> Astlib.%s.%s.to_ast" version kind.kind_name;
            Print.format "|> Astlib.%s.%s.of_ast" grammar_version kind.kind_name;
            Print.format "|> Traversal.%s.%s.copy" grammar_version kind.kind_name;
            Print.format "|> Completeness.%s.%s.check_exn));"
              grammar_version
              kind.kind_name))));
    Print.format "[%%expect {| |}]")

let print_kind_test (kind : Astlib_ast.Grammar.kind) ~version ~versioned_grammars =
  let versioned_grammars =
    List.filter versioned_grammars ~f:(fun (_, grammar) ->
      List.exists grammar ~f:(fun (grammar_kind : Astlib_ast.Grammar.kind) ->
        String.equal kind.kind_name grammar_kind.kind_name))
  in
  let index = version_index_exn ~version versioned_grammars in
  List.iteri versioned_grammars ~f:(fun grammar_index (grammar_version, _) ->
    if grammar_index > 0 then Print.newline ();
    print_copy_test kind ~version ~index ~grammar_version ~grammar_index;
    if grammar_index >= index then (
      Print.newline ();
      print_completeness_test kind ~version ~index ~grammar_version ~grammar_index))

let print_grammar_test grammar ~version ~versioned_grammars =
  List.iteri grammar ~f:(fun kind_index (kind : Astlib_ast.Grammar.kind) ->
    if kind_index > 0 then Print.newline ();
    Print.define_module kind.kind_name (fun () ->
      print_kind_test kind ~version ~versioned_grammars))
;;

let print_astlib_test_ml () =
  let versioned_grammars =
    Astlib_ast.History.to_versioned_grammars Astlib_ast.For_testing.history
  in
  Print.newline ();
  Print.format "open Base";
  List.iter versioned_grammars ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.define_module version (fun () ->
      print_grammar_test grammar ~version ~versioned_grammars))
;;

let print_generators_mli () =
  Astlib_ast.History.to_versioned_grammars Astlib_ast.For_testing.history
  |> List.iter ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.declare_module version (fun () ->
      List.iteri grammar ~f:(fun kind_index (kind : Astlib_ast.Grammar.kind) ->
        if kind_index > 0 then Print.newline ();
        Print.declare_module kind.kind_name (fun () ->
          Print.format "type t = Astlib.%s.%s.t" version kind.kind_name;
          Print.format "[@@deriving sexp_of]";
          Print.newline ();
          Print.format "val quickcheck_generator : t Base_quickcheck.Generator.t";
          Print.format "val quickcheck_shrinker : t Base_quickcheck.Shrinker.t"))))

let rec data_generator (data : Astlib_ast.Grammar.data) =
  match data with
  | Bool -> "Base_quickcheck.Generator.bool"
  | Char -> "Base_quickcheck.Generator.char"
  | String -> "Base_quickcheck.Generator.string_non_empty"
  | Kind name ->
    Printf.sprintf "(lazy_generator %s_generator)" (String.lowercase name)
  | List data ->
    Printf.sprintf "(Base_quickcheck.Generator.list %s)" (data_generator data)
  | Option data ->
    Printf.sprintf "(Base_quickcheck.Generator.option %s)" (data_generator data)

let print_kind_generator (kind : Astlib_ast.Grammar.kind) ~version ~suffix =
  List.iter kind.clauses ~f:(fun (clause : Astlib_ast.Grammar.clause) ->
    Print.format "let %s_gen =" (String.lowercase clause.clause_name);
    Print.indented (fun () ->
      let number_of_fields = List.length clause.fields in
      Print.format "let%%bind sizes =";
      Print.indented (fun () ->
        Print.format "Base_quickcheck.Generator.sizes ~min_length:%d ~max_length:%d ()"
          number_of_fields
          number_of_fields);
      Print.format "in";
      List.iteri clause.fields ~f:(fun field_index (field : Astlib_ast.Grammar.field) ->
        Print.format "%s %s ="
          (if field_index = 0 then "let%bind" else "and")
          field.field_name;
        Print.indented (fun () ->
          Print.format "%s" (data_generator field.data);
          Print.format "|> Base_quickcheck.Generator.with_size";
          Print.indented (fun () ->
            Print.format "~size:(List.nth_exn sizes %d)" field_index)));
      if number_of_fields > 0 then Print.format "in";
      Print.format "return (Astlib.%s.%s.Concrete.%s { %s })"
        version
        kind.kind_name
        clause.clause_name
        (String.concat ~sep:"; "
           ("loc" ::
            List.map clause.fields ~f:(fun (field : Astlib_ast.Grammar.field) ->
              field.field_name))));
    Print.format "in");
  Print.format "Base_quickcheck.Generator.union";
  Print.indented (fun () ->
    List.iteri kind.clauses ~f:(fun clause_index (clause : Astlib_ast.Grammar.clause) ->
      Print.format "%c %s_gen"
        (if clause_index = 0 then '[' else ';')
        (String.lowercase clause.clause_name));
    Print.format "]";
    Print.format
      "|> Base_quickcheck.Generator.map ~f:Astlib.%s.%s.of_concrete%s"
      version
      kind.kind_name
      suffix)

let print_generators_ml () =
  Print.newline ();
  Print.format "open Base";
  Print.format "open Ocaml_common";
  Print.format "open Base_quickcheck.Generator.Let_syntax";
  Print.newline ();
  Print.format "let loc = Location.none";
  Astlib_ast.History.to_versioned_grammars Astlib_ast.For_testing.history
  |> List.iter ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.define_module version (fun () ->
      Print.define_recursive_values
        (List.map grammar ~f:(fun (kind : Astlib_ast.Grammar.kind) ->
           let header =
             Printf.sprintf "%s_generator = lazy ("
               (String.lowercase kind.kind_name)
           in
           let body () =
             print_kind_generator kind ~version ~suffix:")"
           in
           header, body));
      Print.newline ();
      List.iteri grammar ~f:(fun kind_index (kind : Astlib_ast.Grammar.kind) ->
        if kind_index > 0 then Print.newline ();
        Print.define_module kind.kind_name (fun () ->
          Print.format "type t = Astlib.%s.%s.t" version kind.kind_name;
          Print.format "[@@deriving sexp_of]";
          Print.newline ();
          Print.format
            "let quickcheck_generator = Lazy.force %s_generator"
            (String.lowercase kind.kind_name);
          Print.format "let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic"))))

let print_completeness_mli () =
  Astlib_ast.History.to_versioned_grammars Astlib_ast.For_testing.history
  |> List.iter ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.declare_module version (fun () ->
      List.iteri grammar ~f:(fun kind_index (kind : Astlib_ast.Grammar.kind) ->
        if kind_index > 0 then Print.newline ();
        Print.declare_module kind.kind_name (fun () ->
          Print.format "val check_exn : Astlib.%s.%s.t -> unit" version kind.kind_name))))

let rec data_completeness (data : Astlib_ast.Grammar.data) =
  match data with
  | Bool | Char | String -> "ignore"
  | Kind name -> Printf.sprintf "check_%s_exn" (String.lowercase name)
  | List data -> Printf.sprintf "(List.iter ~f:%s)" (data_completeness data)
  | Option data -> Printf.sprintf "(Option.iter ~f:%s)" (data_completeness data)

let print_kind_completeness (kind : Astlib_ast.Grammar.kind) ~version =
  Print.format "match Astlib.%s.%s.to_concrete x with" version kind.kind_name;
  List.iter kind.clauses ~f:(fun (clause : Astlib_ast.Grammar.clause) ->
    let max_field_index = List.length clause.fields - 1 in
    Print.format "| Some (%s { %s }) ->"
      clause.clause_name
      (String.concat ~sep:"; "
         ("loc = _" ::
          List.map clause.fields ~f:(fun (field : Astlib_ast.Grammar.field) ->
            field.field_name)));
    Print.indented (fun () ->
      List.iteri clause.fields ~f:(fun field_index (field : Astlib_ast.Grammar.field) ->
        Print.format "%s %s%s"
          (data_completeness field.data)
          field.field_name
          (if field_index = max_field_index then "" else ";"))));
  Print.format "| None ->";
  Print.indented (fun () ->
  Print.format
    "Base.raise_s (Sexp.message \"invalid %s %s\" [\"ast\", (Astlib.%s.%s.sexp_of_t x)])"
    version
    kind.kind_name
    version
    kind.kind_name)

let print_completeness_ml () =
  Print.newline ();
  Print.format "open Base";
  Astlib_ast.History.to_versioned_grammars Astlib_ast.For_testing.history
  |> List.iter ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.define_module version (fun () ->
      Print.define_recursive_values
        (List.map grammar ~f:(fun (kind : Astlib_ast.Grammar.kind) ->
           let header =
             Printf.sprintf "check_%s_exn x =" (String.lowercase kind.kind_name)
           in
           let body () = print_kind_completeness kind ~version in
           header, body));
      Print.newline ();
      List.iteri grammar ~f:(fun kind_index (kind : Astlib_ast.Grammar.kind) ->
        if kind_index > 0 then Print.newline ();
        Print.define_module kind.kind_name (fun () ->
          Print.format "let check_exn = check_%s_exn"
            (String.lowercase kind.kind_name)))))

let print_traversal_mli () =
  Astlib_ast.History.to_versioned_grammars Astlib_ast.For_testing.history
  |> List.iter ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.declare_module version (fun () ->
      List.iteri grammar ~f:(fun kind_index (kind : Astlib_ast.Grammar.kind) ->
        if kind_index > 0 then Print.newline ();
        Print.declare_module kind.kind_name (fun () ->
          Print.format "val copy : Astlib.%s.%s.t -> Astlib.%s.%s.t"
            version
            kind.kind_name
            version
            kind.kind_name))))

let rec copy_data (data : Astlib_ast.Grammar.data) =
  match data with
  | Bool | Char | String -> None
  | Kind name -> Some (Printf.sprintf "copy_%s" (String.lowercase name))
  | List data ->
    Option.map (copy_data data) ~f:(fun expr ->
      Printf.sprintf "(List.map ~f:%s)" expr)
  | Option data ->
    Option.map (copy_data data) ~f:(fun expr ->
      Printf.sprintf "(Option.map ~f:%s)" expr)

let print_traversal_ml () =
  Print.newline ();
  Print.format "open Base";
  Astlib_ast.History.to_versioned_grammars Astlib_ast.For_testing.history
  |> List.iter ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.define_module version (fun () ->
      Print.define_recursive_values
        (List.map grammar ~f:(fun (kind : Astlib_ast.Grammar.kind) ->
           let header =
             Printf.sprintf "copy_%s x =" (String.lowercase kind.kind_name)
           in
           let body () =
             Print.format "match Astlib.%s.%s.to_concrete x with" version kind.kind_name;
             List.iter kind.clauses ~f:(fun (clause : Astlib_ast.Grammar.clause) ->
               let field_names =
                 List.map clause.fields ~f:(fun (field : Astlib_ast.Grammar.field) ->
                   field.field_name)
               in
               Print.format "| Some (%s { %s }) ->"
                 clause.clause_name
                 (String.concat ~sep:"; " ("loc" :: field_names));
               Print.indented (fun () ->
                 List.iter clause.fields ~f:(fun (field : Astlib_ast.Grammar.field) ->
                   Print.format "let %s = %s%s in"
                     field.field_name
                     (match copy_data field.data with
                      | Some f -> f ^ " "
                      | None -> "")
                     field.field_name);
                 Print.format "Astlib.%s.%s.of_concrete (%s { %s })"
                   version
                   kind.kind_name
                   clause.clause_name
                   (String.concat ~sep:"; " ("loc" :: field_names))));
             Print.format "| None -> x"
           in
           header, body));
      Print.newline ();
      List.iteri grammar ~f:(fun kind_index (kind : Astlib_ast.Grammar.kind) ->
        if kind_index > 0 then Print.newline ();
        Print.define_module kind.kind_name (fun () ->
          Print.format "let copy = copy_%s" (String.lowercase kind.kind_name)))))
