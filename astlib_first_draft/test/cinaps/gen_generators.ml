open Base
open Astlib_first_draft_cinaps

let print_generators_mli () =
  Astlib_first_draft_ast.History.to_versioned_grammars Astlib_first_draft_ast.History.history
  |> List.iter ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.declare_module version (fun () ->
      List.iteri grammar ~f:(fun kind_index (kind : Astlib_first_draft_ast.Grammar.kind) ->
        if kind_index > 0 then Print.newline ();
        Print.declare_module kind.kind_name (fun () ->
          Print.format "type t = Astlib_first_draft.%s.%s.t" version kind.kind_name;
          Print.format "[@@deriving sexp_of]";
          Print.newline ();
          Print.format "val quickcheck_generator : t Base_quickcheck.Generator.t";
          Print.format "val quickcheck_shrinker : t Base_quickcheck.Shrinker.t"))))

let rec data_contains_mutual_recursion (data : Astlib_first_draft_ast.Grammar.data) =
  match data with
  | Bool | Char | String | Location -> false
  | Kind _ -> true
  | List data | Option data -> data_contains_mutual_recursion data

let clause_mutual_recursion_count (clause : Astlib_first_draft_ast.Grammar.clause) =
  List.count clause.fields ~f:(fun field ->
    data_contains_mutual_recursion field.data)

let rec data_generator (data : Astlib_first_draft_ast.Grammar.data) =
  match data with
  | Bool -> "Base_quickcheck.Generator.bool"
  | Char -> "Base_quickcheck.Generator.char"
  | String -> "Base_quickcheck.Generator.string_non_empty"
  | Location -> "(Base_quickcheck.Generator.return Location.none)"
  | Kind name ->
    Printf.sprintf
      "(Base_quickcheck.Generator.of_lazy %s_generator)"
      (String.lowercase name)
  | List data ->
    Printf.sprintf "(Base_quickcheck.Generator.list %s)" (data_generator data)
  | Option data ->
    Printf.sprintf "(Base_quickcheck.Generator.option %s)" (data_generator data)

let print_clause_generators (kind : Astlib_first_draft_ast.Grammar.kind) ~version =
  List.iter kind.clauses ~f:(fun (clause : Astlib_first_draft_ast.Grammar.clause) ->
    Print.format "let %s_gen =" (String.lowercase clause.clause_name);
    Print.indented (fun () ->
      List.iteri clause.fields ~f:(fun field_index (field : Astlib_first_draft_ast.Grammar.field) ->
        Print.format "%s %s ="
          (if field_index = 0 then "let%bind" else "and")
          field.field_name;
        Print.indented (fun () ->
          Print.format "%s" (data_generator field.data)));
      if not (List.is_empty clause.fields) then Print.format "in";
      Print.format "return (Astlib_first_draft.%s.%s.Concrete.%s%s)"
        version
        kind.kind_name
        clause.clause_name
        (match clause.fields with
         | [] -> ""
         | _ :: _ ->
           Printf.sprintf " { %s }"
             (String.concat ~sep:"; "
                (List.map clause.fields ~f:(fun (field : Astlib_first_draft_ast.Grammar.field) ->
                   field.field_name)))));
    Print.format "in")

let print_weighted_generators clauses =
  List.iteri clauses ~f:(fun clause_index clause ->
    Print.format "%c 1. /. %d., %s_gen"
      (if clause_index = 0 then '[' else ';')
      (clause_mutual_recursion_count clause + 1)
      (String.lowercase clause.clause_name));
  Print.format "]"

let print_simple_kind_generator clauses =
  Print.format "Base_quickcheck.Generator.weighted_union";
  Print.indented (fun () -> print_weighted_generators clauses)

let print_complex_kind_generator ~non_recursive_clauses ~recursive_clauses =
  Print.format "let non_recursive_alist =";
  Print.indented (fun () ->
    print_weighted_generators non_recursive_clauses);
  Print.format "and recursive_alist =";
  Print.indented (fun () ->
    print_weighted_generators recursive_clauses;
    Print.format
      "|> List.map ~f:(fun (weight, gen) -> (weight, with_decremented_size gen))");
  Print.format "in";
  Print.format "let non_recursive_gen =";
  Print.indented (fun () ->
    Print.format "Base_quickcheck.Generator.weighted_union non_recursive_alist");
  Print.format "and recursive_gen =";
  Print.indented (fun () ->
    Print.format "Base_quickcheck.Generator.weighted_union";
    Print.indented (fun () ->
      Print.format "(non_recursive_alist @ recursive_alist)"));
  Print.format "in";
  Print.format "match%%bind Base_quickcheck.Generator.size with";
  Print.format "| 0 -> non_recursive_gen";
  Print.format "| _ -> recursive_gen"

let print_kind_generator (kind : Astlib_first_draft_ast.Grammar.kind) ~version =
  print_clause_generators kind ~version;
  match
    List.partition_tf kind.clauses ~f:(fun clause ->
      clause_mutual_recursion_count clause = 0)
  with
  | [], clauses | clauses, [] ->
    print_simple_kind_generator clauses
  | non_recursive_clauses, recursive_clauses ->
    print_complex_kind_generator ~non_recursive_clauses ~recursive_clauses

let print_generators_ml () =
  Print.newline ();
  Print.format "open! Base";
  Print.format "open! Ocaml_common";
  Print.format "open Base_quickcheck.Generator.Let_syntax";
  Print.newline ();
  Print.format "let with_decremented_size generator =";
  Print.indented (fun () ->
    Print.format "let%%bind size = Base_quickcheck.Generator.size in";
    Print.format "Base_quickcheck.Generator.with_size generator ~size:(size - 1)");
  Astlib_first_draft_ast.History.to_versioned_grammars Astlib_first_draft_ast.History.history
  |> List.iter ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.define_module version (fun () ->
      Print.define_recursive_values
        (List.map grammar ~f:(fun (kind : Astlib_first_draft_ast.Grammar.kind) ->
           let header =
             Printf.sprintf "%s_generator ="
               (String.lowercase kind.kind_name)
           in
           let body () =
             Print.format "lazy begin";
             Print.indented (fun () ->
               Print.format "let%%map concrete =";
               Print.indented (fun () ->
                 print_kind_generator kind ~version);
               Print.format "in";
               Print.format "Astlib_first_draft.%s.%s.of_concrete concrete" version kind.kind_name);
             Print.format "end"
           in
           header, body));
      Print.newline ();
      List.iteri grammar ~f:(fun kind_index (kind : Astlib_first_draft_ast.Grammar.kind) ->
        if kind_index > 0 then Print.newline ();
        Print.define_module kind.kind_name (fun () ->
          Print.format "type t = Astlib_first_draft.%s.%s.t" version kind.kind_name;
          Print.newline ();
          Print.format
            "let sexp_of_t t = Ast.sexp_of_t (Astlib_first_draft.%s.%s.to_ast t)"
            version
            kind.kind_name;
          Print.format
            "let quickcheck_generator = Lazy.force %s_generator"
            (String.lowercase kind.kind_name);
          Print.format "let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic"))))
