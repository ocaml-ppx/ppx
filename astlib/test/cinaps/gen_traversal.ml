open Base
open Astlib_cinaps

let print_traversal_mli () =
  Astlib_ast.History.to_versioned_grammars Astlib_ast.History.history
  |> List.iter ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.declare_module version (fun () ->
      List.iteri grammar ~f:(fun kind_index (kind : Astlib_ast.Type.kind) ->
        if kind_index > 0 then Print.newline ();
        Print.declare_module kind.kind_name (fun () ->
          Print.format "val copy : Astlib.%s.%s.t -> Astlib.%s.%s.t"
            version
            kind.kind_name
            version
            kind.kind_name))))

let rec copy_data (data : Astlib_ast.Type.data) =
  match data with
  | Bool | Char | String | Location -> None
  | Kind name -> Some (Printf.sprintf "copy_%s" (String.lowercase name))
  | List data ->
    Option.map (copy_data data) ~f:(fun expr ->
      Printf.sprintf "(List.map ~f:%s)" expr)
  | Option data ->
    Option.map (copy_data data) ~f:(fun expr ->
      Printf.sprintf "(Option.map ~f:%s)" expr)

let print_traversal_ml () =
  Print.newline ();
  Print.format "open! Base";
  Astlib_ast.History.to_versioned_grammars Astlib_ast.History.history
  |> List.iter ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.define_module version (fun () ->
      Print.define_recursive_values
        (List.map grammar ~f:(fun (kind : Astlib_ast.Type.kind) ->
           let header =
             Printf.sprintf "copy_%s x =" (String.lowercase kind.kind_name)
           in
           let body () =
             Print.format "match Astlib.%s.%s.to_concrete x with" version kind.kind_name;
             List.iter kind.clauses ~f:(fun (clause : Astlib_ast.Type.clause) ->
               let field_names =
                 List.map clause.fields ~f:(fun (field : Astlib_ast.Type.field) ->
                   field.field_name)
               in
               Print.format "| Some (%s%s) ->"
                 clause.clause_name
                 (match clause.fields with
                  | [] -> ""
                  | _ :: _ ->
                    Printf.sprintf " { %s }"
                      (String.concat ~sep:"; " (field_names)));
               Print.indented (fun () ->
                 List.iter clause.fields ~f:(fun (field : Astlib_ast.Type.field) ->
                   Print.format "let %s = %s%s in"
                     field.field_name
                     (match copy_data field.data with
                      | Some f -> f ^ " "
                      | None -> "")
                     field.field_name);
                 Print.format "Astlib.%s.%s.of_concrete (%s%s)"
                   version
                   kind.kind_name
                   clause.clause_name
                   (match clause.fields with
                    | [] -> ""
                    | _ :: _ ->
                      Printf.sprintf " { %s }"
                        (String.concat ~sep:"; " field_names))));
             Print.format "| None -> x"
           in
           header, body));
      Print.newline ();
      List.iteri grammar ~f:(fun kind_index (kind : Astlib_ast.Type.kind) ->
        if kind_index > 0 then Print.newline ();
        Print.define_module kind.kind_name (fun () ->
          Print.format "let copy = copy_%s" (String.lowercase kind.kind_name)))))
