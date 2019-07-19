open! StdLabels
open Astlib_cinaps

let rec string_of_ty (ty : _ Astlib_parsetree_types.ty) =
  match ty with
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | Location -> "Location.t"
  | A _ -> "'a"
  | T name -> name
  | List ty -> string_of_ty ty ^ " list"
  | Option ty -> string_of_ty ty ^ " option"

let print_parsetree_clause clause ~name =
  match clause with
  | [] -> Print.format "| %s" name
  | fields ->
    Print.format "| %s of %s"
      name
      (List.map fields ~f:string_of_ty |> String.concat ~sep:" * ")

let print_parsetree_struct fields =
  List.iteri fields ~f:(fun index (name, ty) ->
    Print.format "%c %s : %s"
      (if index = 0 then '{' else ';')
      name
      (string_of_ty ty));
  Print.format "}"

let print_parsetree_rep (rep : _ Astlib_parsetree_types.rep) =
  match rep with
  | Alias ty -> Print.format "%s" (string_of_ty ty)
  | Variant { clauses; _ } ->
    List.iter clauses ~f:(fun (name, clause) ->
      print_parsetree_clause clause ~name)
  | Record { fields; _ } ->
    print_parsetree_struct fields
  | Tuple tys ->
    Print.format "(%s)"
      (List.map tys ~f:string_of_ty
       |> String.concat ~sep:" * ")

let print_parsetree_decl (decl : Astlib_parsetree_types.decl) =
  match decl with
  | Mono rep -> print_parsetree_rep rep
  | Poly rep -> print_parsetree_rep rep
  | Inst { poly; arg } ->
    Print.format "%s %s" (string_of_ty arg) poly

let arg_of_decl (decl : Astlib_parsetree_types.decl) =
  match decl with
  | Poly _ -> "'a "
  | Mono _ | Inst _ -> ""

let equation_of_rep (rep : _ Astlib_parsetree_types.rep) ~arg =
  match rep with
  | Alias _ | Tuple _ -> ""
  | Record { source; _ } | Variant { source; _ } ->
    Printf.sprintf " %s%s =" arg source

let equation_of_decl (decl : Astlib_parsetree_types.decl) =
  let arg = arg_of_decl decl in
  match decl with
  | Mono rep -> equation_of_rep rep ~arg
  | Poly rep -> equation_of_rep rep ~arg
  | Inst _ -> ""

let print_parsetree_decls () =
  Astlib_parsetree_types.t
  |> List.iteri ~f:(fun decl_index (name, decl) ->
    Print.newline ();
    Print.format "%s %s%s =%s"
      (if decl_index = 0 then "type" else "and")
      (arg_of_decl decl)
      name
      (equation_of_decl decl);
    Print.indented (fun () ->
      print_parsetree_decl decl))

let print_parsetree_ml () =
  Print.newline ();
  Print.format "open! StdLabels";
  Print.format "open Ocaml_common";
  print_parsetree_decls ()
