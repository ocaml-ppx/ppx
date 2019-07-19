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

let rec substitute_ty ty ~arg : _ Astlib_parsetree_types.ty =
  match (ty : _ Astlib_parsetree_types.ty) with
  | (Bool | Char | String | Location | T _) as ty -> ty
  | A () -> arg
  | List ty -> List (substitute_ty ty ~arg)
  | Option ty -> Option (substitute_ty ty ~arg)

let substitute_rep rep ~arg : _ Astlib_parsetree_types.rep =
  match (rep : _ Astlib_parsetree_types.rep) with
  | Alias ty -> Alias (substitute_ty ty ~arg)
  | Tuple tys -> Tuple (List.map tys ~f:(substitute_ty ~arg))
  | Record { source; fields } ->
    let fields =
      List.map fields ~f:(fun (name, ty) ->
        (name, substitute_ty ty ~arg))
    in
    Record { source; fields }
  | Variant { source; clauses } ->
    let clauses =
      List.map clauses ~f:(fun (name, tys) ->
        (name, List.map tys ~f:(substitute_ty ~arg)))
    in
    Variant { source; clauses }

let instantiate alist ~poly ~arg =
  match (List.assoc_opt poly alist : Astlib_parsetree_types.decl option) with
  | Some (Poly rep) -> substitute_rep rep ~arg
  | _ -> assert false

let module_name ~name =
  Printf.sprintf "Stable.%s.%s"
    Astlib_parsetree_types.version
    (String.capitalize_ascii name)

let ast_type ~name =
  Printf.sprintf "Stable.%s.%s.t"
    Astlib_parsetree_types.version
    (String.capitalize_ascii name)

let concrete_type ~name =
  Printf.sprintf "Stable.%s.%s.Concrete.t"
    Astlib_parsetree_types.version
    (String.capitalize_ascii name)

let tree_type ~name = Printf.sprintf "Astlib_parsetree.%s" name

let print_conversions_mli () =
  List.iter Astlib_parsetree_types.t ~f:(fun (name, decl) ->
    match (decl : Astlib_parsetree_types.decl) with
    | Poly _ -> ()
    | Inst _ | Mono _ ->
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

let rec to_ast ty =
  match (ty : Astlib_parsetree_types.nothing Astlib_parsetree_types.ty) with
  | Bool | Char | String | Location -> None
  | A _ -> .
  | T name -> Some (Printf.sprintf "%s_to_ast" name)
  | List ty -> option_map ~f:(Printf.sprintf "List.map ~f:(%s)") (to_ast ty)
  | Option ty -> option_map ~f:(Printf.sprintf "Optional.map ~f:(%s)") (to_ast ty)

let bind_to_ast bindings =
  List.iter bindings ~f:(fun (name, ty) ->
    match to_ast ty with
    | None -> ()
    | Some expr -> Print.format "let %s = %s %s in" name expr name)

let print_to_ast rep ~name ~index =
  Print.newline ();
  Print.format "%s %s_to_ast : %s -> %s ="
    (if index = 0 then "let rec" else "and")
    name
    (tree_type ~name)
    (ast_type ~name);
  let module_name = module_name ~name in
  Print.indented (fun () ->
    match (rep : _ Astlib_parsetree_types.rep) with
    | Alias ty ->
      Print.format "fun a ->";
      Print.indented (fun () ->
        bind_to_ast ["a", ty];
        Print.format "%s.of_concrete (%s { a })"
          module_name
          (String.capitalize_ascii name))
    | Tuple tys ->
      let vars = tuple_vars tys in
      Print.format "fun %s ->" (tuple_string vars);
      Print.indented (fun () ->
        bind_to_ast (zip vars tys);
        Print.format "%s.of_concrete (%s %s)"
          module_name
          (String.capitalize_ascii name)
          (record_string vars))
    | Record { source = _; fields } ->
      let vars = List.map fields ~f:fst in
      Print.format "fun %s ->" (record_string vars);
      Print.indented (fun () ->
        bind_to_ast fields;
        Print.format "%s.of_concrete (%s %s)"
          module_name
          (String.capitalize_ascii name)
          (record_string vars))
    | Variant { source = _; clauses } ->
      Print.format "function";
      List.iter clauses ~f:(fun (tag, tys) ->
        let vars = tuple_vars tys in
        Print.format "| %s%s ->"
          (String.capitalize_ascii tag)
          (match vars with
           | [] -> ""
           | _ :: _ -> " " ^ tuple_string vars);
        Print.indented (fun () ->
          bind_to_ast (zip vars tys);
          Print.format "%s.of_concrete (%s%s)"
            module_name
            (String.capitalize_ascii tag)
            (match tys with
             | [] -> ""
             | _ :: _ -> " " ^ record_string vars))))

let rec of_ast ty =
  match (ty : Astlib_parsetree_types.nothing Astlib_parsetree_types.ty) with
  | Bool | Char | String | Location -> None
  | A _ -> .
  | T name -> Some (Printf.sprintf "%s_of_ast" name)
  | List ty -> option_map ~f:(Printf.sprintf "Optional.List.map ~f:(%s)") (of_ast ty)
  | Option ty -> option_map ~f:(Printf.sprintf "Optional.Option.map ~f:(%s)") (of_ast ty)

let bind_of_ast =
  let rec loop ~bindings ~print ~suffix =
    match bindings with
    | [] -> print ~suffix
    | (name, ty) :: bindings ->
      match of_ast ty with
      | None -> loop ~bindings ~print ~suffix
      | Some expr ->
        Print.format "Optional.bind (%s %s) ~f:(fun %s ->" expr name name;
        Print.indented (fun () ->
          loop ~bindings ~print ~suffix:(suffix ^ ")"))
  in
  fun bindings print ->
    loop ~bindings ~print ~suffix:""

let print_of_ast rep ~name ~index =
  let module_name = module_name ~name in
  Print.newline ();
  Print.format "%s %s_of_ast x =" (if index = 0 then "let rec" else "and") name;
  Print.indented (fun () ->
    Print.format "let of_concrete : %s -> %s option ="
      (concrete_type ~name)
      (tree_type ~name);
    Print.indented (fun () ->
      match (rep : _ Astlib_parsetree_types.rep) with
      | Alias ty ->
        Print.format "fun (%s { a }) ->" (String.capitalize_ascii name);
        Print.indented (fun () ->
          bind_of_ast ["a", ty] (fun ~suffix ->
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
  let alist = Astlib_parsetree_types.t in
  let reps =
    filter_map alist ~f:(fun (name, decl) ->
      match (decl : Astlib_parsetree_types.decl) with
      | Poly _ -> None
      | Mono rep -> Some (name, rep)
      | Inst { poly; arg } -> Some (name, instantiate alist ~poly ~arg))
  in
  Print.newline ();
  Print.format "open! StdLabels";
  List.iteri reps ~f:(fun index (name, rep) -> print_of_ast ~index ~name rep);
  List.iteri reps ~f:(fun index (name, rep) -> print_to_ast ~index ~name rep)
