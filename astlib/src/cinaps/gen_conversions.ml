open! StdLabels
open Astlib_cinaps

let option_map option ~f =
  match option with
  | None -> None
  | Some x -> Some (f x)

let zip xs ys = List.map2 xs ys ~f:(fun x y -> x, y)

let module_name ~name =
  Printf.sprintf "Stable.%s.%s"
    (Astlib_ast.History.current_version Astlib_ast.History.history)
    (String.capitalize_ascii name)

let ast_type ~name =
  Printf.sprintf "Stable.%s.%s.t"
    (Astlib_ast.History.current_version Astlib_ast.History.history)
    (String.capitalize_ascii name)

let concrete_type ~name =
  Printf.sprintf "Stable.%s.%s.Concrete.t"
    (Astlib_ast.History.current_version Astlib_ast.History.history)
    (String.capitalize_ascii name)

let tree_type ~name = Printf.sprintf "Astlib_parsetree.%s" name

let current_grammar =
  Lazy.from_fun (fun () ->
    Astlib_ast.History.to_versioned_grammars Astlib_ast.History.history
    |> List.assoc (Astlib_ast.History.current_version Astlib_ast.History.history))

let poly_args vars n =
  match vars with
  | [] -> ""
  | [var] -> Printf.sprintf "'%s%d " var n
  | _ ->
    Printf.sprintf "(%s) "
      (String.concat ~sep:", "
         (List.map vars ~f:(fun var ->
            Printf.sprintf "'%s%d" var n)))

let poly_conversions vars =
  String.concat ~sep:""
    (List.map vars ~f:(fun var ->
       Printf.sprintf "('%s1 -> '%s2) -> " var var))

let print_conversions_mli () =
  List.iter (Lazy.force current_grammar) ~f:(fun (name, decl) ->
    let { vars; body = _ } : Astlib_ast.Grammar.decl = decl in
    Print.newline ();
    let ast_type = ast_type ~name in
    let tree_type = tree_type ~name in
    Print.format "val %s_to_ast : %s%s%s -> %s%s"
      name
      (poly_conversions vars)
      (poly_args vars 1)
      tree_type
      (poly_args vars 2)
      ast_type;
    Print.format "val %s_of_ast : %s%s%s -> %s%s option"
      name
      (poly_conversions vars)
      (poly_args vars 1)
      ast_type
      (poly_args vars 2)
      tree_type)

let variable_names =
  List.init ~len:256 ~f:Char.chr
  |> List.filter ~f:(function 'a' .. 'z' -> true | _ -> false)
  |> List.map ~f:(String.make 1)
  |> Array.of_list

let variable_name i = Array.get variable_names i

let tuple_vars list =
  List.mapi list ~f:(fun i _ ->
    variable_name i)

let tuple_string list = Printf.sprintf "(%s)" (String.concat ~sep:", " list)
let record_string list = Printf.sprintf "{ %s }" (String.concat ~sep:"; " list)

let rec to_ast structural =
  match (structural : Astlib_ast.Grammar.structural) with
  | Bool | Int | Char | String | Location -> None
  | Name string | Var string -> Some (Printf.sprintf "%s_to_ast" string)
  | Inst { poly; args } ->
    Some
      (Printf.sprintf "(%s_to_ast %s)"
         poly
         (String.concat ~sep:" " (List.map args ~f:to_ast_or_id)))
  | List structural ->
    option_map ~f:(Printf.sprintf "List.map ~f:(%s)") (to_ast structural)
  | Option structural ->
    option_map ~f:(Printf.sprintf "Optional.map ~f:(%s)") (to_ast structural)
  | Tuple tuple -> tuple_to_ast tuple

and tuple_to_ast tuple =
  let conversions = List.map tuple ~f:to_ast in
  if List.for_all conversions ~f:(function None -> true | Some _ -> false)
  then None
  else (
    let vars = List.mapi tuple ~f:(fun i _ -> Printf.sprintf "x%d" (i + 1)) in
    Some
      (Printf.sprintf "(fun (%s) -> (%s))"
         (String.concat ~sep:", " vars)
         (String.concat ~sep:", "
            (List.map2 vars conversions ~f:(fun var conversion ->
               match conversion with
               | None -> var
               | Some conversion -> Printf.sprintf "%s %s" conversion var)))))

and to_ast_or_id structural =
  match to_ast structural with
  | Some x -> x
  | None -> "(fun x -> x)"

let bind_to_ast bindings =
  List.iter bindings ~f:(fun (name, structural) ->
    match to_ast structural with
    | None -> ()
    | Some expr -> Print.format "let %s = %s %s in" name expr name)

let print_nominal_to_ast nominal ~name =
  let module_name = module_name ~name in
  match (nominal : Astlib_ast.Grammar.nominal) with
  | Alias structural ->
    Print.format "fun x ->";
    Print.indented (fun () ->
      bind_to_ast ["x", structural];
      Print.format "%s.of_concrete x" module_name)
  | Record fields ->
    let vars = List.map fields ~f:fst in
    Print.format "fun %s ->" (record_string vars);
    Print.indented (fun () ->
      bind_to_ast fields;
      Print.format "%s.of_concrete %s" module_name (record_string vars))
  | Variant variant ->
    Print.format "function";
    List.iter variant ~f:(fun (tag, clause) ->
      match (clause : Astlib_ast.Grammar.clause) with
      | Empty ->
        Print.format "| %s -> %s.of_concrete %s"
          (String.capitalize_ascii tag)
          module_name
          (String.capitalize_ascii tag)
      | Tuple tuple ->
        let vars = tuple_vars tuple in
        Print.format "| %s %s ->" (String.capitalize_ascii tag) (tuple_string vars);
        Print.indented (fun () ->
          bind_to_ast (List.map2 vars tuple ~f:(fun var structural -> (var, structural)));
          Print.format "%s.of_concrete (%s %s)"
            module_name
            (String.capitalize_ascii tag)
            (tuple_string vars))
      | Record record ->
        let vars = List.map record ~f:fst in
        Print.format "| %s %s ->" (String.capitalize_ascii tag) (record_string vars);
        Print.indented (fun () ->
          bind_to_ast record;
          Print.format "%s.of_concrete (%s %s)"
            module_name
            (String.capitalize_ascii tag)
            (record_string vars)))

let print_to_ast ~index ~name ({ vars; body } : Astlib_ast.Grammar.decl) =
  Print.newline ();
  Print.format "%s %s_to_ast%s : %s -> %s ="
    (if index = 0 then "let rec" else "and")
    name
    (String.concat ~sep:""
       (List.map vars ~f:(fun var -> Printf.sprintf " %s_to_ast" var)))
    (tree_type ~name)
    (ast_type ~name);
  Print.indented (fun () ->
    print_nominal_to_ast body ~name)

let rec of_ast structural =
  match (structural : Astlib_ast.Grammar.structural) with
  | Bool | Int | Char | String | Location -> None
  | Name string | Var string -> Some (Printf.sprintf "%s_of_ast" string)
  | Inst { poly; args } ->
    Some
      (Printf.sprintf "(%s_of_ast %s)"
         poly
         (String.concat ~sep:" " (List.map args ~f:of_ast_or_id)))
  | List structural ->
    option_map ~f:(Printf.sprintf "List.map ~f:(%s)") (of_ast structural)
  | Option structural ->
    option_map ~f:(Printf.sprintf "Optional.map ~f:(%s)") (of_ast structural)
  | Tuple tuple -> tuple_of_ast tuple

and tuple_of_ast tuple =
  let conversions = List.map tuple ~f:of_ast in
  if List.for_all conversions ~f:(function None -> true | Some _ -> false)
  then None
  else (
    let vars = List.mapi tuple ~f:(fun i _ -> Printf.sprintf "x%d" (i + 1)) in
    Some
      (Printf.sprintf "(fun (%s) -> (%s))"
         (String.concat ~sep:", " vars)
         (String.concat ~sep:", "
            (List.map2 vars conversions ~f:(fun var conversion ->
               match conversion with
               | None -> var
               | Some conversion -> Printf.sprintf "%s %s" conversion var)))))

and of_ast_or_id structural =
  match of_ast structural with
  | Some x -> x
  | None -> "(fun x -> x)"

let bind_of_ast =
  let rec loop ~bindings ~print =
    match bindings with
    | [] -> print ()
    | (name, structural) :: bindings ->
      match of_ast structural with
      | None -> loop ~bindings ~print
      | Some expr ->
        Print.format "Optional.bind (%s %s) ~f:(fun %s ->" expr name name;
        Print.indented (fun () -> loop ~bindings ~print)
  in
  fun bindings print ->
    loop ~bindings ~print;
    let count = List.length bindings in
    if count > 0 then Print.format "%s" (String.make count ')')

let print_nominal_of_ast nominal ~name =
  match (nominal : Astlib_ast.Grammar.nominal) with
  | Alias structural ->
    Print.format "fun (%s { a }) ->" (String.capitalize_ascii name);
    Print.indented (fun () ->
      bind_of_ast ["a", structural] (fun () ->
        Print.format "Some a"))
  | Record record ->
    let vars = List.map record ~f:fst in
    Print.format "fun (%s %s) ->" (String.capitalize_ascii name) (record_string vars);
    Print.indented (fun () ->
      bind_of_ast record (fun () ->
        Print.format "Some (%s : %s)" (record_string vars) (tree_type ~name)))
  | Variant variant ->
    Print.format "function";
    List.iter variant ~f:(fun (tag, clause) ->
      match (clause : Astlib_ast.Grammar.clause) with
      | Empty ->
        Print.format "| %s -> Some %s"
          (String.capitalize_ascii tag)
          (String.capitalize_ascii tag)
      | Tuple tuple ->
        let vars = tuple_vars tuple in
        Print.format "| %s %s ->"
          (String.capitalize_ascii tag)
          (tuple_string vars);
        Print.indented (fun () ->
          bind_of_ast (zip vars tuple) (fun () ->
            Print.format "Some (%s %s)"
              (String.capitalize_ascii tag)
              (tuple_string vars)))
      | Record record ->
        let vars = List.map record ~f:fst in
        Print.format "| %s %s ->"
          (String.capitalize_ascii tag)
          (record_string vars);
        Print.indented (fun () ->
          bind_of_ast record (fun () ->
            Print.format "Some (%s %s)"
              (String.capitalize_ascii tag)
              (record_string vars))))

let print_of_ast ~name ~index ({ vars; body } : Astlib_ast.Grammar.decl) =
  Print.newline ();
  Print.format "%s %s_of_ast%s ="
    (if index = 0 then "let rec" else "and")
    name
    (String.concat ~sep:""
       (List.map vars ~f:(fun var -> Printf.sprintf " %s_of_ast" var)));
  Print.indented (fun () ->
    Print.format "let of_concrete : %s -> %s option ="
      (concrete_type ~name)
      (tree_type ~name);
    Print.indented (fun () -> print_nominal_of_ast ~name body);
    Print.format "in";
    Print.format "fun x -> Optional.bind ~f:of_concrete (%s.to_concrete x)"
      (module_name ~name))

let print_conversions_ml () =
  Print.newline ();
  Print.format "open! StdLabels";
  let alist = Lazy.force current_grammar in
  List.iteri alist ~f:(fun index (name, nominal) -> print_of_ast ~index ~name nominal);
  List.iteri alist ~f:(fun index (name, nominal) -> print_to_ast ~index ~name nominal)
