open Base
open Astlib_first_draft_cinaps

let print_completeness_mli () =
  Astlib_first_draft_ast.History.to_versioned_grammars Astlib_first_draft_ast.History.history
  |> List.iter ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.declare_module version (fun () ->
      List.iteri grammar ~f:(fun kind_index (kind : Astlib_first_draft_ast.Grammar.kind) ->
        if kind_index > 0 then Print.newline ();
        Print.declare_module kind.kind_name (fun () ->
          Print.format "val check_exn : Astlib_first_draft.%s.%s.t -> unit" version kind.kind_name))))

let rec data_completeness (data : Astlib_first_draft_ast.Grammar.data) =
  match data with
  | Bool | Char | String | Location -> "ignore"
  | Kind name -> Printf.sprintf "check_%s_exn" (String.lowercase name)
  | List data -> Printf.sprintf "(List.iter ~f:%s)" (data_completeness data)
  | Option data -> Printf.sprintf "(Option.iter ~f:%s)" (data_completeness data)

let print_kind_completeness (kind : Astlib_first_draft_ast.Grammar.kind) ~version =
  Print.format "match Astlib_first_draft.%s.%s.to_concrete x with" version kind.kind_name;
  List.iter kind.clauses ~f:(fun (clause : Astlib_first_draft_ast.Grammar.clause) ->
    let max_field_index = List.length clause.fields - 1 in
    Print.format "| Some (%s%s) ->%s"
      clause.clause_name
      (match clause.fields with
       | [] -> ""
       | _ :: _ ->
         Printf.sprintf " { %s }"
           (String.concat ~sep:"; "
              (List.map clause.fields ~f:(fun (field : Astlib_first_draft_ast.Grammar.field) ->
                 field.field_name))))
      (if List.is_empty clause.fields
       then " ()"
       else "");
    Print.indented (fun () ->
      List.iteri clause.fields ~f:(fun field_index (field : Astlib_first_draft_ast.Grammar.field) ->
        Print.format "%s %s%s"
          (data_completeness field.data)
          field.field_name
          (if field_index = max_field_index then "" else ";"))));
  Print.format "| None ->";
  Print.indented (fun () ->
    Print.format "Base.raise_s";
    Print.indented (fun () ->
      Print.format "(Sexp.message";
      Print.indented (fun () ->
        Print.format "\"invalid %s %s\"" version kind.kind_name;
        Print.format
          "[\"ast\", (Ast.sexp_of_t (Astlib_first_draft.%s.%s.to_ast x))])"
          version
          kind.kind_name)))

let print_completeness_ml () =
  Print.newline ();
  Print.format "open! Base";
  Astlib_first_draft_ast.History.to_versioned_grammars Astlib_first_draft_ast.History.history
  |> List.iter ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.define_module version (fun () ->
      Print.define_recursive_values
        (List.map grammar ~f:(fun (kind : Astlib_first_draft_ast.Grammar.kind) ->
           let header =
             Printf.sprintf "check_%s_exn x =" (String.lowercase kind.kind_name)
           in
           let body () = print_kind_completeness kind ~version in
           header, body));
      Print.newline ();
      List.iteri grammar ~f:(fun kind_index (kind : Astlib_first_draft_ast.Grammar.kind) ->
        if kind_index > 0 then Print.newline ();
        Print.define_module kind.kind_name (fun () ->
          Print.format "let check_exn = check_%s_exn"
            (String.lowercase kind.kind_name)))))
