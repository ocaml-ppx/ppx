open StdLabels
open Astlib_cinaps

let rec type_of_data data ~opaque =
  match (data : Astlib_ast.Grammar.data) with
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | Location -> "Location.t"
  | Kind name -> if opaque then name ^ ".t" else "Versioned_ast.t"
  | List data -> type_of_data data ~opaque ^ " list"
  | Option data -> type_of_data data ~opaque ^ " option"

let rec data_of_concrete : Astlib_ast.Grammar.data -> string = function
  | Bool -> "Versioned_value.of_bool"
  | Char -> "Versioned_value.of_char"
  | String -> "Versioned_value.of_string"
  | Location -> "Versioned_value.of_location"
  | Kind _ -> "Versioned_value.of_ast"
  | List data -> Printf.sprintf "(Versioned_value.of_list ~f:%s)" (data_of_concrete data)
  | Option data ->
    Printf.sprintf "(Versioned_value.of_option ~f:%s)" (data_of_concrete data)

let rec data_to_concrete data =
  match (data : Astlib_ast.Grammar.data) with
  | Bool -> "Versioned_value.to_bool"
  | Char -> "Versioned_value.to_char"
  | String -> "Versioned_value.to_string"
  | Location -> "Versioned_value.to_location"
  | Kind _ -> Printf.sprintf "Versioned_value.to_ast"
  | List data -> Printf.sprintf "(Versioned_value.to_list ~f:%s)" (data_to_concrete data)
  | Option data ->
    Printf.sprintf "(Versioned_value.to_option ~f:%s)" (data_to_concrete data)

let print_clause_type (clause : Astlib_ast.Grammar.clause) ~opaque =
  Print.format "| %s%s"
    clause.clause_name
    (match clause.fields with
     | [] -> ""
     | _ :: _ ->
       Printf.sprintf " of { %s }"
         (String.concat ~sep:"; "
            (List.map clause.fields ~f:(fun (field : Astlib_ast.Grammar.field) ->
               field.field_name ^ " : " ^ type_of_data field.data ~opaque))))

let print_kind_type (kind : Astlib_ast.Grammar.kind) ~opaque =
  Print.format "type t =";
  Print.indented (fun () ->
    List.iter kind.clauses ~f:(print_clause_type ~opaque))

let print_clause_of_concrete ~kind_name (clause : Astlib_ast.Grammar.clause) =
  Print.format "| %s%s ->"
    clause.clause_name
    (match clause.fields with
     | [] -> ""
     | _ :: _ ->
       Printf.sprintf " { %s }"
         (String.concat ~sep:"; "
            (List.map clause.fields ~f:(fun (field : Astlib_ast.Grammar.field) ->
               field.field_name))));
  Print.indented (fun () ->
    Print.format "Versioned_ast.create ~version";
    Print.indented (fun () ->
      Print.format "{ kind = %S" kind_name;
      Print.format "; clause = %S" clause.clause_name;
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
        Print.format "Optional.bind (%s %s) ~f:(fun %s ->"
          (data_to_concrete field.data)
          field.field_name
          field.field_name;
        Print.indented (fun () -> loop fields)
      | [] ->
        Print.format "Some (Concrete.%s%s)%s"
          clause.clause_name
          (match clause.fields with
           | [] -> ""
           | _ :: _ ->
             Printf.sprintf " { %s }"
               (String.concat ~sep:"; "
                  (List.map clause.fields ~f:(fun (field : Astlib_ast.Grammar.field) ->
                     field.field_name))))
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
  Print.format "type t = Unversioned.%s" (String.lowercase_ascii kind.kind_name);
  Print.newline ();
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

let names_of_versioned_grammars versioned_grammars =
  versioned_grammars
  |> List.map ~f:snd
  |> List.concat
  |> List.map ~f:(fun (kind : Astlib_ast.Grammar.kind) -> kind.kind_name)
  |> List.sort_uniq ~cmp:String.compare

let print_unversioned_mli versioned_grammars =
  let names = names_of_versioned_grammars versioned_grammars in
  List.iter names ~f:(fun name ->
    Print.format "type %s" (String.lowercase_ascii name))

let print_unversioned_ml versioned_grammars =
  let names = names_of_versioned_grammars versioned_grammars in
  List.iter names ~f:(fun name ->
    Print.format "type %s = Versioned_ast.t" (String.lowercase_ascii name))

let print_astlib_mli () =
  Print.newline ();
  Print.format "open! StdLabels";
  Print.format "open! Ocaml_common";
  let versioned_grammars =
    Astlib_ast.History.to_versioned_grammars Astlib_ast.History.history
  in
  Print.newline ();
  Print.declare_module "Unversioned" (fun () ->
    print_unversioned_mli versioned_grammars);
  List.iter versioned_grammars ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.declare_module version (fun () ->
      print_grammar_mli grammar))
;;

let print_astlib_ml () =
  Print.newline ();
  Print.format "open! StdLabels";
  Print.format "open! Ocaml_common";
  let versioned_grammars =
    Astlib_ast.History.to_versioned_grammars Astlib_ast.History.history
  in
  Print.newline ();
  Print.define_module "Unversioned" (fun () ->
    print_unversioned_ml versioned_grammars);
  List.iter versioned_grammars ~f:(fun (version, grammar) ->
    Print.newline ();
    Print.define_module version (fun () ->
      Print.format "let version = %S" version;
      Print.newline ();
      print_grammar_ml grammar))
;;
