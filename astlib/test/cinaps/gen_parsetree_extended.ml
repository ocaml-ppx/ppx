open! Base
open Astlib_cinaps

let is_poly decl =
  match (decl : Astlib_parsetree_types.decl) with
  | Poly _ -> true
  | Mono _ | Inst _ -> false

let poly_arg_string decl = if is_poly decl then "'a " else ""

let print_parsetree_extended_mli () =
  Print.newline ();
  Print.format "open! Base";
  Astlib_parsetree_types.t
  |> List.iter ~f:(fun (name, decl) ->
    Print.newline ();
    Print.format "module %s : sig" (String.capitalize name);
    Print.indented (fun () ->
      let arg = poly_arg_string decl in
      Print.format "type %st = %sAstlib_parsetree.%s" arg arg name;
      Print.format "[@@deriving compare, equal, quickcheck, sexp_of]");
    Print.format "end")

let rec string_of_ty ty =
  match (ty : _ Astlib_parsetree_types.ty) with
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | Location -> "Location_extended.t"
  | A _ -> "'a"
  | T name -> name
  | List ty -> string_of_ty ty ^ " list"
  | Option ty -> string_of_ty ty ^ " option"

let print_parsetree_rep rep =
  match (rep : _ Astlib_parsetree_types.rep) with
  | Alias ty -> Print.format "%s" (string_of_ty ty)
  | Tuple tys ->
    Print.format "(%s)" (List.map tys ~f:string_of_ty |> String.concat ~sep:" * ")
  | Record { source = _; fields } ->
    List.iteri fields ~f:(fun index (name, ty) ->
      Print.format "%c %s: %s" (if index = 0 then '{' else ';') name (string_of_ty ty));
    Print.format "}"
  | Variant { source = _; clauses } ->
    List.iter clauses ~f:(fun (name, fields) ->
      Print.format "| %s%s"
        (String.capitalize name)
        (if List.is_empty fields
         then ""
         else Printf.sprintf " of %s"
                (List.map fields ~f:string_of_ty |> String.concat ~sep:" * ")))

let rep_is_generative rep =
  match (rep : _ Astlib_parsetree_types.rep) with
  | Alias _ | Tuple _ -> false
  | Record _ | Variant _ -> true

let decl_is_generative decl =
  match (decl : Astlib_parsetree_types.decl) with
  | Poly rep -> rep_is_generative rep
  | Mono rep -> rep_is_generative rep
  | Inst _ -> false

let print_parsetree_decl decl =
  match (decl : Astlib_parsetree_types.decl) with
  | Poly rep -> print_parsetree_rep rep
  | Mono rep -> print_parsetree_rep rep
  | Inst { poly; arg } -> Print.format "%s %s" (string_of_ty arg) poly

let print_parsetree_types types =
  List.iteri types ~f:(fun index (name, decl) ->
    Print.newline ();
    let arg = poly_arg_string decl in
    Print.format "%s %s%s =%s"
      (if index = 0 then "type" else "and")
      arg
      name
      (if decl_is_generative decl
       then Printf.sprintf " %sAstlib_parsetree.%s =" arg name
       else "");
    Print.indented (fun () ->
      print_parsetree_decl decl));
  Print.format "[@@deriving compare, equal, hash, sexp_of]"

let rec ty_generator ty =
  match (ty : _ Astlib_parsetree_types.ty) with
  | Bool -> "quickcheck_generator_bool"
  | Char -> "quickcheck_generator_char"
  | String -> "quickcheck_generator_string"
  | Location -> "Location_extended.quickcheck_generator"
  | A _ -> "quickcheck_generator_a"
  | T name ->
    Printf.sprintf "(Base_quickcheck.Generator.of_lazy quickcheck_generator_%s)" name
  | List ty -> Printf.sprintf "(quickcheck_generator_list %s)" (ty_generator ty)
  | Option ty -> Printf.sprintf "(quickcheck_generator_option %s)" (ty_generator ty)

let let_bind fields =
  Print.format "let%%bind sizes =";
  Print.indented (fun () ->
    let n = List.length fields in
    Print.format "Base_quickcheck.Generator.sizes ~min_length:%d ~max_length:%d ()" n n;
    Print.format "|> Base_quickcheck.Generator.map ~f:Array.of_list");
  Print.format "in";
  List.iteri fields ~f:(fun index (name, ty) ->
    Print.format "%s %s =" (if index = 0 then "let%map" else "and") name;
    Print.indented (fun () ->
      Print.format "%s" (ty_generator ty);
      Print.format "|> Base_quickcheck.Generator.with_size ~size:sizes.(%d)" index));
  Print.format "in"

let print_record_generator fields =
  let_bind fields;
  Print.format "{ %s }"
    (List.map fields ~f:(fun (name, _) -> name)
     |> String.concat ~sep:"; ")

let tuple_fields tys = List.mapi tys ~f:(fun i ty -> Printf.sprintf "x%d" i, ty)

let print_tuple_generator tys =
  let fields = tuple_fields tys in
  let_bind fields;
  Print.format "(%s)" (String.concat ~sep:", " (List.map fields ~f:fst))

let rec ty_contains_recursion ty =
  match (ty : _ Astlib_parsetree_types.ty) with
  | Bool | Char | String | Location | A _ -> false
  | T _ -> true
  | List ty | Option ty -> ty_contains_recursion ty

let print_clause_generator_bindings clauses =
  List.iteri clauses ~f:(fun clause_index (name, tys) ->
    Print.format "%s quickcheck_generator_%s ="
      (if clause_index = 0 then "let" else "and")
      (String.lowercase name);
    Print.indented (fun () ->
      if List.is_empty tys
      then Print.format "return Astlib_parsetree.%s" name
      else (
        let fields = tuple_fields tys in
        let_bind fields;
        Print.format "Astlib_parsetree.%s (%s)"
          name
          (String.concat ~sep:", " (List.map fields ~f:fst)))));
  Print.format "in"

let print_list_of_named_generators alist =
  List.iteri alist ~f:(fun index (name, _) ->
    Print.format "%c quickcheck_generator_%s"
      (if index = 0 then '[' else ';')
      (String.lowercase name));
  Print.format "]"

let print_variant_generator clauses =
  print_clause_generator_bindings clauses;
  match
    List.partition_tf clauses ~f:(fun (_, tys) ->
      List.exists tys ~f:ty_contains_recursion)
  with
  | [], clauses | clauses, [] ->
    Print.format "Base_quickcheck.Generator.union";
    Print.indented (fun () ->
      List.iteri clauses ~f:(fun index (name, _) ->
        Print.format "%c quickcheck_generator_%s"
          (if index = 0 then '[' else ';')
          (String.lowercase name));
      Print.format "]")
  | recursive, non_recursive ->
    Print.format "let nonrec_list =";
    Print.indented (fun () ->
      print_list_of_named_generators non_recursive);
    Print.format "in";
    Print.format "let rec_list =";
    Print.indented (fun () ->
      print_list_of_named_generators recursive;
      Print.format "|> List.map ~f:with_decremented_size");
    Print.format "in";
    Print.format "let nonrec_gen = Base_quickcheck.Generator.union nonrec_list in";
    Print.format "let rec_gen = Base_quickcheck.Generator.union rec_list in";
    Print.format "match%%bind Base_quickcheck.Generator.size with";
    Print.format "| 0 -> nonrec_gen";
    Print.format "| _ -> rec_gen"

let print_rep_generator rep =
  match (rep : _ Astlib_parsetree_types.rep) with
  | Alias ty -> Print.format "%s" (ty_generator ty)
  | Tuple tys -> print_tuple_generator tys
  | Record { source = _; fields } -> print_record_generator fields
  | Variant { source = _; clauses } -> print_variant_generator clauses

let print_decl_generator decl ~name ~decl_index =
  let header = if decl_index = 0 then "let rec" else "and" in
  match (decl : Astlib_parsetree_types.decl) with
  | Poly rep ->
    Print.format "%s quickcheck_generator_%s" header name;
    Print.indented (fun () ->
      Print.format
        ": type a . a Base_quickcheck.Generator.t -> a %s Base_quickcheck.Generator.t"
        name;
      Print.format "= fun quickcheck_generator_a ->";
      Print.indented (fun () ->
        print_rep_generator rep))
  | Mono rep ->
    Print.format "%s quickcheck_generator_%s = lazy begin" header name;
    Print.indented (fun () ->
      print_rep_generator rep);
    Print.format "end"
  | Inst { poly; arg } ->
    Print.format "%s quickcheck_generator_%s =" header name;
    Print.indented (fun () ->
      Print.format "lazy (quickcheck_generator_%s %s)" poly (ty_generator arg))

let print_parsetree_generators types =
  Print.newline ();
  List.iteri types ~f:(fun decl_index (name, decl) ->
    Print.newline ();
    print_decl_generator decl ~name ~decl_index)

let print_parsetree_modules types =
  List.iter types ~f:(fun (name, decl) ->
    Print.newline ();
    Print.format "module %s = struct" (String.capitalize name);
    Print.indented (fun () ->
      let arg = poly_arg_string decl in
      let is_poly = is_poly decl in
      Print.format "type %st = %s%s" arg arg name;
      Print.format "[@@deriving compare, equal, hash, sexp_of]";
      Print.newline ();
      if is_poly
      then (
        Print.format "let quickcheck_generator = quickcheck_generator_%s" name;
        Print.format "let quickcheck_observer quickcheck_observer_a =";
        Print.indented (fun () ->
          Print.format "let hash_fold_a hash a =";
          Print.indented (fun () ->
            Print.format
              "Base_quickcheck.Observer.observe quickcheck_observer_a a ~size:0 ~hash");
          Print.format "in";
          Print.format "Base_quickcheck.Observer.of_hash_fold (hash_fold_%s hash_fold_a)"
            name;
          Print.format "let quickcheck_shrinker _ = Base_quickcheck.Shrinker.atomic"))
      else (
        Print.format "let quickcheck_generator =";
        Print.indented (fun () ->
          Print.format "Base_quickcheck.Generator.of_lazy quickcheck_generator_%s" name);
        Print.format
          "let quickcheck_observer = Base_quickcheck.Observer.of_hash_fold hash_fold_%s"
          name;
        Print.format "let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic"));
    Print.format "end")

let print_parsetree_extended_ml () =
  Print.newline ();
  Print.format "open! Base";
  Print.format "open Base_quickcheck.Export";
  Print.format "open Base_quickcheck.Generator.Let_syntax";
  print_parsetree_types Astlib_parsetree_types.t;
  print_parsetree_generators Astlib_parsetree_types.t;
  print_parsetree_modules Astlib_parsetree_types.t
