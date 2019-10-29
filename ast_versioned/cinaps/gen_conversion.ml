open StdLabels

module Helpers = struct
  module Option = struct
    let is_none = function
      | None -> true
      | Some _ -> false

    let map t ~f =
      match t with
      | None -> None
      | Some x -> Some (f x)
  end
end

let version = Astlib.current_version

let type_name name ~view =
  match view with
  | `ast ->
    Printf.sprintf "Versions.%s.%s.t"
      (Ml.module_name version)
      (Ml.module_name name)
  | `concrete ->
    Printf.sprintf "Versions.%s.%s.concrete"
      (Ml.module_name version)
      (Ml.module_name name)
  | `parsetree ->
    Printf.sprintf "Compiler_types.%s" (Ml.id name)

let rec string_of_ty ty ~view =
  match (ty : Astlib.Grammar.ty) with
  | Var var -> Ml.tvar var
  | Name name -> type_name name ~view
  | Bool -> "bool"
  | Char -> "char"
  | Int -> "int"
  | String -> "string"
  | Location -> "Location.t"
  | Loc ty -> string_of_ty ty ~view ^ " Location.loc"
  | List ty -> string_of_ty ty ~view ^ " list"
  | Option ty -> string_of_ty ty ~view ^ " option"
  | Tuple tuple -> string_of_tuple tuple ~view
  | Instance (poly, args) ->
    let arg_view =
      match view with
      | `ast | `concrete -> `ast
      | `parsetree -> `parsetree
    in
    Ml.poly_inst
      (type_name poly ~view)
      ~args:(List.map args ~f:(string_of_ty ~view:arg_view))

and string_of_tuple tuple ~view =
  Printf.sprintf "(%s)"
    (String.concat ~sep:" * " (List.map tuple ~f:(string_of_ty ~view)))

let ast_ty ty = string_of_ty ty ~view:`ast
let concrete_ty ty = string_of_ty ty ~view:`concrete
let parsetree_ty ty = string_of_ty ty ~view:`parsetree

let node_ty ~node_name ~env =
  if Poly.env_is_empty env
  then Astlib.Grammar.Name node_name
  else Astlib.Grammar.Instance (node_name, Poly.args env)

let print_conversion_intf ~node_name ~env =
  let ty = node_ty ~node_name ~env in
  let ast_ty = ast_ty ty in
  let parsetree_ty = parsetree_ty ty in
  Ml.declare_val (Name.make ["ast_of"; node_name] (Poly.args env)) (Block (fun () ->
    Ml.print_arrow [parsetree_ty] ast_ty ~f:(fun x -> x)));
  Print.newline ();
  Ml.declare_val (Name.make ["ast_to"; node_name] (Poly.args env)) (Block (fun () ->
    Ml.print_arrow [ast_ty] parsetree_ty ~f:(fun x -> x)))

let fn_value option =
  match option with
  | Some fn -> fn
  | None -> "Helpers.Fn.id"

let conversion_prefix ~conv =
  match conv with
  | `concrete_of -> "ast_of"
  | `concrete_to -> "ast_to"

let concrete_prefix ~conv =
  match conv with
  | `concrete_of -> "concrete_of"
  | `concrete_to -> "concrete_to"

let rec ty_conversion ty ~conv =
  match (ty : Astlib.Grammar.ty) with
  | Var var -> Some (Ml.id (conversion_prefix ~conv ^ "_" ^ var))
  | Name name -> Some (Ml.id (conversion_prefix ~conv ^ "_" ^ name))
  | Bool | Char | Int | String | Location -> None
  | Loc ty ->
    Helpers.Option.map (ty_conversion ty ~conv)
      ~f:(Printf.sprintf "(Helpers.Loc.map ~f:%s)")
  | List ty ->
    Helpers.Option.map (ty_conversion ty ~conv)
      ~f:(Printf.sprintf "(List.map ~f:%s)")
  | Option ty ->
    Helpers.Option.map (ty_conversion ty ~conv)
      ~f:(Printf.sprintf "(Helpers.Option.map ~f:%s)")
  | Tuple tuple -> tuple_conversion tuple ~conv
  | Instance (poly, args) ->
    Some (Name.make [conversion_prefix ~conv; poly] args)

and tuple_conversion tuple ~conv =
  let conversions = List.map tuple ~f:(ty_conversion ~conv) in
  if List.for_all conversions ~f:Helpers.Option.is_none
  then None
  else
    Some
      (Printf.sprintf "(Helpers.Tuple.map%d %s)"
         (List.length tuple)
         (String.concat ~sep:" "
            (List.mapi conversions ~f:(fun i option ->
               Printf.sprintf "~f%d:%s" (i + 1) (fn_value option)))))

let input_ty ty ~conv =
  match conv with
  | `concrete_of -> parsetree_ty ty
  | `concrete_to -> concrete_ty ty

let output_ty ty ~conv =
  match conv with
  | `concrete_of -> concrete_ty ty
  | `concrete_to -> parsetree_ty ty

let tuple_var i = Ml.id (Printf.sprintf "x%d" (i + 1))

let define_conversion decl ~node_name ~env ~conv =
  let name = Name.make [concrete_prefix ~conv; node_name] (Poly.args env) in
  let node_ty = node_ty ~node_name ~env in
  match (decl : Astlib.Grammar.decl) with
  | Alias ty ->
    Print.println "and %s x =" name;
    Print.indented (fun () ->
      Print.println "%s x" (fn_value (ty_conversion ~conv (Poly.subst_ty ty ~env))))
  | Record record ->
    let fields =
      String.concat ~sep:"; "
        (List.map record ~f:(fun (field, _) -> Ml.id field))
    in
    Print.println "and %s" name;
    Print.indented (fun () ->
      Print.println "({ %s } : %s)" fields (input_ty ~conv node_ty));
    Print.println "=";
    Print.indented (fun () ->
      List.iter record ~f:(fun (field, ty) ->
        match ty_conversion ~conv (Poly.subst_ty ty ~env) with
        | None -> ()
        | Some fn -> Print.println "let %s = %s %s in" (Ml.id field) fn (Ml.id field));
      Print.println "({ %s } : %s)" fields (output_ty ~conv node_ty))
  | Variant variant ->
    Print.println "and %s x : %s =" name (output_ty ~conv node_ty);
    Print.indented (fun () ->
      Print.println "match (x : %s) with" (input_ty ~conv node_ty);
      List.iter variant ~f:(fun (tag, clause) ->
        match (clause : Astlib.Grammar.clause) with
        | Empty ->
          Print.println "| %s -> %s" (Ml.tag tag) (Ml.tag tag)
        | Tuple tuple ->
          Print.println "| %s (%s) ->"
            (Ml.tag tag)
            (String.concat ~sep:", " (List.mapi tuple ~f:(fun i _ -> tuple_var i)));
          Print.indented (fun () ->
            List.iteri tuple ~f:(fun i ty ->
              match ty_conversion (Poly.subst_ty ty ~env) ~conv with
              | None -> ()
              | Some fn ->
                Print.println "let %s = %s %s in" (tuple_var i) fn (tuple_var i));
            Print.println "%s (%s)"
              (Ml.tag tag)
              (String.concat ~sep:", "
                 (List.mapi tuple ~f:(fun i _ -> tuple_var i))))
        | Record record ->
          Print.println "| %s { %s } ->"
            (Ml.tag tag)
            (String.concat ~sep:"; "
               (List.map record ~f:(fun (field, _) -> Ml.id field)));
          Print.indented (fun () ->
            List.iter record ~f:(fun (field, ty) ->
              match ty_conversion (Poly.subst_ty ty ~env) ~conv with
              | None -> ()
              | Some fn ->
                Print.println "let %s = %s %s in" (Ml.id field) fn (Ml.id field));
            Print.println "%s { %s }"
              (Ml.tag tag)
              (String.concat ~sep:"; "
                 (List.map record ~f:(fun (field, _) -> Ml.id field))))))

let print_conversion_impl decl ~node_name ~env ~is_initial =
  Print.println
    "%s %s x ="
    (if is_initial then "let rec" else "and")
    (Name.make ["ast_of"; node_name] (Poly.args env));
  Print.indented (fun () ->
    Print.println "Versions.%s.%s.%s (%s x)"
      (Ml.module_name version)
      (Ml.module_name node_name)
      (Name.make ["of_concrete"] (Poly.args env))
      (Name.make ["concrete_of"; node_name] (Poly.args env)));
  Print.newline ();
  define_conversion decl ~node_name ~env ~conv:`concrete_of;
  Print.newline ();
  Print.println "and %s x =" (Name.make ["ast_to"; node_name] (Poly.args env));
  Print.indented (fun () ->
    Print.println "let option = Versions.%s.%s.%s x in"
      (Ml.module_name version)
      (Ml.module_name node_name)
      (Name.make ["to_concrete"] (Poly.args env));
    Print.println "let concrete =";
    Print.indented (fun () ->
      Print.println "Helpers.Option.value_exn option";
      Print.indented (fun () ->
        Print.println "~message:%S"
          (Printf.sprintf "%s: conversion failed"
             (Name.make ["concrete_to"; node_name] (Poly.args env)))));
    Print.println "in";
    Print.println "%s concrete"
      (Name.make ["concrete_to"; node_name] (Poly.args env)));
  Print.newline ();
  define_conversion decl ~node_name ~env ~conv:`concrete_to

let print_conversion_mli () =
  let grammar = Astlib.History.find_grammar Astlib.history ~version in
  let env_table = Poly.env_table grammar in
  List.iter grammar ~f:(fun (node_name, kind) ->
    match (kind : Astlib.Grammar.kind) with
    | Mono _ ->
      Print.newline ();
      print_conversion_intf ~node_name ~env:Poly.empty_env
    | Poly _ ->
      List.iter (Poly.find env_table node_name) ~f:(fun env ->
        Print.newline ();
        print_conversion_intf ~node_name ~env))

let print_conversion_ml () =
  let grammar = Astlib.History.find_grammar Astlib.history ~version in
  let env_table = Poly.env_table grammar in
  List.iteri grammar ~f:(fun node_index (node_name, kind) ->
    match (kind : Astlib.Grammar.kind) with
    | Mono decl ->
      Print.newline ();
      print_conversion_impl decl ~node_name
        ~env:Poly.empty_env
        ~is_initial:(node_index = 0)
    | Poly (_, decl) ->
      List.iteri (Poly.find env_table node_name) ~f:(fun env_index env ->
        Print.newline ();
        let is_initial = node_index = 0 && env_index = 0 in
        print_conversion_impl decl ~node_name ~env ~is_initial))
