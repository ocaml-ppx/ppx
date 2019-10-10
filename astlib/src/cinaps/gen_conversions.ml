open! StdLabels
open Astlib_cinaps

let option_map option ~f =
  match option with
  | None -> None
  | Some x -> Some (f x)

let filter_map list ~f =
  List.map list ~f:(fun x ->
    match f x with
    | None -> []
    | Some y -> [y])
  |> List.concat

let zip xs ys = List.map2 xs ys ~f:(fun x y -> x, y)

let rec substitute_structural structural ~subst : Astlib_ast.Grammar.structural =
  match (structural : Astlib_ast.Grammar.structural) with
  | Bool | Int | Char | String | Location | Name _ -> structural
  | Var var -> List.assoc var subst
  | Inst { poly; args } ->
    Inst { poly; args = List.map args ~f:(substitute_structural ~subst) }
  | List structural -> List (substitute_structural structural ~subst)
  | Option structural -> Option (substitute_structural structural ~subst)
  | Tuple tuple -> Tuple (substitute_tuple tuple ~subst)

and substitute_tuple tuple ~subst =
  List.map tuple ~f:(substitute_structural ~subst)

let substitute_record record ~subst =
  List.map record ~f:(fun (name, structural) ->
    (name, substitute_structural structural ~subst))

let substitute_clause clause ~subst : Astlib_ast.Grammar.clause =
  match (clause : Astlib_ast.Grammar.clause) with
  | Empty -> Empty
  | Tuple tuple -> Tuple (substitute_tuple tuple ~subst)
  | Record record -> Record (substitute_record record ~subst)

let substitute_variant variant ~subst =
  List.map variant ~f:(fun (name, clause) ->
    (name, substitute_clause clause ~subst))

let substitute_nominal nominal ~subst : Astlib_ast.Grammar.nominal =
  match (nominal : Astlib_ast.Grammar.nominal) with
  | Alias structural -> Alias (substitute_structural structural ~subst)
  | Record record -> Record (substitute_record record ~subst)
  | Variant variant -> Variant (substitute_variant variant ~subst)

let instantiate alist ~poly ~args =
  let { vars ; body } : Astlib_ast.Grammar.decl = List.assoc poly alist in
  let subst = List.map2 vars args ~f:(fun var arg -> (var, arg)) in
  substitute_nominal body ~subst

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

let print_conversions_mli () =
  Astlib_ast.History.to_versioned_grammars Astlib_ast.History.history
  |> List.assoc (Astlib_ast.History.current_version Astlib_ast.History.history)
  |> List.iter ~f:(fun (name, decl) ->
    let { vars; body } : Astlib_ast.Grammar.decl = decl in
    Print.newline ();
    let ast_type = ast_type ~name in
    let tree_type = tree_type ~name in
    Print.format "val %s_to_ast : %s -> %s" name tree_type ast_type;
    Print.format "val %s_of_ast : %s -> %s option" name ast_type tree_type)

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
  let rec loop ~bindings ~print ~suffix =
    match bindings with
    | [] -> print ~suffix
    | (name, structural) :: bindings ->
      match of_ast structural with
      | None -> loop ~bindings ~print ~suffix
      | Some expr ->
        Print.format "Optional.bind (%s %s) ~f:(fun %s ->" expr name name;
        Print.indented (fun () ->
          loop ~bindings ~print ~suffix:(suffix ^ ")"))
  in
  fun bindings print ->
    loop ~bindings ~print ~suffix:""

let print_of_ast nominal ~name ~index =
  let module_name = module_name ~name in
  Print.newline ();
  Print.format "%s %s_of_ast x =" (if index = 0 then "let rec" else "and") name;
  Print.indented (fun () ->
    Print.format "let of_concrete : %s -> %s option ="
      (concrete_type ~name)
      (tree_type ~name);
    Print.indented (fun () ->
      match (nominal : _ Astlib_ast.Grammar.nominal) with
      | Alias structural ->
        Print.format "fun (%s { a }) ->" (String.capitalize_ascii name);
        Print.indented (fun () ->
          bind_of_ast ["a", structural] (fun ~suffix ->
            Print.format "Some a%s" suffix))
      | Tuple tys ->
        let vars = tuple_vars tys in
        Print.format "fun (%s %s) ->" (String.capitalize_ascii name) (record_string vars);
        Print.indented (fun () ->
          bind_of_ast (zip vars tys) (fun ~suffix ->
            Print.format "Some %s%s" (tuple_string vars) suffix))
      | Record { source = _; fields } ->
        let vars = List.map fields ~f:fst in
        Print.format "fun (%s %s) ->" (String.capitalize_ascii name) (record_string vars);
        Print.indented (fun () ->
          bind_of_ast fields (fun ~suffix ->
            Print.format "Some (%s : %s)%s"
              (record_string vars)
              (tree_type ~name)
              suffix))
      | Variant { source = _; clauses } ->
        Print.format "function";
        List.iter clauses ~f:(fun (tag, tys) ->
          let vars = tuple_vars tys in
          Print.format "| %s%s ->"
            (String.capitalize_ascii tag)
            (match vars with
             | [] -> ""
             | _ :: _ -> " " ^ record_string vars);
          Print.indented (fun () ->
            bind_of_ast (zip vars tys) (fun ~suffix ->
              Print.format "Some (%s%s : %s)%s"
                (String.capitalize_ascii tag)
                (match vars with
                 | [] -> ""
                 | _ :: _ -> " " ^ tuple_string vars)
                (tree_type ~name)
                suffix))));
    Print.format "in";
    Print.format "Optional.bind ~f:of_concrete (%s.to_concrete x)" module_name)

let print_conversions_ml () =
  let alist = Astlib_ast.Grammar.t in
  let reps =
    filter_map alist ~f:(fun (name, decl) ->
      match (decl : Astlib_ast.Grammar.decl) with
      | Poly _ -> None
      | Mono nominal -> Some (name, nominal)
      | Inst { poly; arg } -> Some (name, instantiate alist ~poly ~arg))
  in
  Print.newline ();
  Print.format "open! StdLabels";
  List.iteri reps ~f:(fun index (name, nominal) -> print_of_ast ~index ~name nominal);
  List.iteri reps ~f:(fun index (name, nominal) -> print_to_ast ~index ~name nominal)
