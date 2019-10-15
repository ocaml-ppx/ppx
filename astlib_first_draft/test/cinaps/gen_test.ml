open Base
open Astlib_first_draft_cinaps

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
           "Astlib_first_draft_src_cinaps.version_index_exn: no such version"
           [("version", Atom version)])
    | (grammar_version, _) :: alist ->
      if String.equal grammar_version version
      then index
      else loop ~index:(index + 1) ~version alist
  in
  fun ~version alist -> loop ~index:0 ~version alist

let print_copy_test
      (kind : Astlib_first_draft_ast.Grammar.kind)
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
              Print.format "|> Astlib_first_draft.%s.%s.to_ast" version kind.kind_name;
              Print.format "|> Astlib_first_draft.%s.%s.of_ast" grammar_version kind.kind_name;
              Print.format "|> Traversal.%s.%s.copy" grammar_version kind.kind_name;
              Print.format "|> Astlib_first_draft.%s.%s.to_ast" grammar_version kind.kind_name;
              Print.format "|> Astlib_first_draft.%s.%s.of_ast" version kind.kind_name;
              Print.format "|> Traversal.%s.%s.copy" version kind.kind_name));
          Print.format "in";
          Print.format
            "if not (Ast.equal (Astlib_first_draft.%s.%s.to_ast x) (Astlib_first_draft.%s.%s.to_ast y))"
            version
            kind.kind_name
            version
            kind.kind_name;
          Print.format "then (";
          Print.indented (fun () ->
            Print.format
              "let x = Ast.sexp_of_t (Astlib_first_draft.%s.%s.to_ast x) in"
              version
              kind.kind_name;
            Print.format
              "let y = Ast.sexp_of_t (Astlib_first_draft.%s.%s.to_ast y) in"
              version
              kind.kind_name;
            Print.format
              "raise_s (Sexp.message \"ASTs differ\" [(\"\", x); (\"\", y)]))));"))));
    Print.format "[%%expect {| |}]")

let print_completeness_test
      (kind : Astlib_first_draft_ast.Grammar.kind)
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
            Print.format "|> Astlib_first_draft.%s.%s.to_ast" version kind.kind_name;
            Print.format "|> Astlib_first_draft.%s.%s.of_ast" grammar_version kind.kind_name;
            Print.format "|> Traversal.%s.%s.copy" grammar_version kind.kind_name;
            Print.format "|> Completeness.%s.%s.check_exn));"
              grammar_version
              kind.kind_name))));
    Print.format "[%%expect {| |}]")

let print_kind_test (kind : Astlib_first_draft_ast.Grammar.kind) ~version ~versioned_grammars =
  let versioned_grammars =
    List.filter versioned_grammars ~f:(fun (_, grammar) ->
      List.exists grammar ~f:(fun (grammar_kind : Astlib_first_draft_ast.Grammar.kind) ->
        String.equal kind.kind_name grammar_kind.kind_name))
  in
  let index = version_index_exn ~version versioned_grammars in
  List.iteri versioned_grammars ~f:(fun grammar_index (grammar_version, _) ->
    if grammar_index > 0 then Print.newline ();
    print_copy_test kind ~version ~index ~grammar_version ~grammar_index;
    if grammar_index >= index then (
      Print.newline ();
      print_completeness_test kind ~version ~index ~grammar_version ~grammar_index))

let print_grammar_test grammar ~entry_points ~version ~versioned_grammars =
  let grammar =
    List.filter grammar ~f:(fun (kind : Astlib_first_draft_ast.Grammar.kind) ->
      List.mem entry_points kind.kind_name ~equal:String.equal)
  in
  List.iteri grammar ~f:(fun kind_index (kind : Astlib_first_draft_ast.Grammar.kind) ->
    if kind_index > 0 then Print.newline ();
    Print.define_module kind.kind_name (fun () ->
      print_kind_test kind ~version ~versioned_grammars))
;;

let print_astlib_test_ml () =
  let versioned_grammars =
    Astlib_first_draft_ast.History.to_versioned_grammars Astlib_first_draft_ast.History.history
  in
  let entry_points =
    List.map Astlib_first_draft_parsetree_types.entry_points ~f:String.capitalize
  in
  Print.newline ();
  Print.format "open! Base";
  List.iter versioned_grammars ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.define_module version (fun () ->
      print_grammar_test grammar ~entry_points ~version ~versioned_grammars))
;;
