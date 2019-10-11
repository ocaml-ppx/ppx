open StdLabels
open Astlib_cinaps

let rec type_of_structural structural ~opaque =
  match (structural : Astlib_ast.Grammar.structural) with
  | Bool -> "bool"
  | Int -> "int"
  | Char -> "char"
  | String -> "string"
  | Location -> "Location.t"
  | Var var -> "'" ^ var
  | Inst { poly; args } ->
    Printf.sprintf "(%s) %s"
      (String.concat ~sep:", " (List.map args ~f:(type_of_structural ~opaque)))
      poly
  | Name name -> if opaque then String.capitalize_ascii name ^ ".t" else "Versioned_ast.t"
  | List structural -> type_of_structural structural ~opaque ^ " list"
  | Option structural -> type_of_structural structural ~opaque ^ " option"
  | Tuple tuple -> type_of_tuple tuple ~opaque

and type_of_tuple tuple ~opaque =
  Printf.sprintf "(%s)"
    (String.concat ~sep:" * " (List.map tuple ~f:(type_of_structural ~opaque)))

let type_of_record record ~opaque =
  Printf.sprintf "{ %s }"
    (String.concat ~sep:"; "
       (List.map record ~f:(fun (name, structural) ->
          name ^ " : " ^ type_of_structural structural ~opaque)))

let rec structural_of_concrete : Astlib_ast.Grammar.structural -> string = function
  | Bool -> "Versioned_value.of_bool"
  | Int -> "Versioned_value.of_int"
  | Char -> "Versioned_value.of_char"
  | String -> "Versioned_value.of_string"
  | Location -> "Versioned_value.of_location"
  | Var var -> var ^ "_of_concrete"
  | Name _ -> "Versioned_value.of_ast"
  | Inst { poly; args } ->
    Printf.sprintf "(%s_of_concrete %s)"
      poly
      (String.concat ~sep:" " (List.map args ~f:structural_of_concrete))
  | List structural ->
    Printf.sprintf "(Versioned_value.of_list ~f:%s)" (structural_of_concrete structural)
  | Option structural ->
    Printf.sprintf "(Versioned_value.of_option ~f:%s)" (structural_of_concrete structural)
  | Tuple tuple -> tuple_of_concrete tuple

and tuple_of_concrete tuple =
  Printf.sprintf "(Versioned_value.of_tuple%d %s)"
    (List.length tuple)
    (String.concat ~sep:" "
       (List.mapi tuple ~f:(fun i structural ->
          Printf.sprintf "~f%d:%s" (i + 1) (structural_of_concrete structural))))

let rec structural_to_concrete structural =
  match (structural : Astlib_ast.Grammar.structural) with
  | Bool -> "Versioned_value.to_bool"
  | Int -> "Versioned_value.to_int"
  | Char -> "Versioned_value.to_char"
  | String -> "Versioned_value.to_string"
  | Location -> "Versioned_value.to_location"
  | Var var -> var ^ "_to_concrete"
  | Name _ -> Printf.sprintf "Versioned_value.to_ast"
  | Inst { poly; args } ->
    Printf.sprintf "(%s_to_concrete %s)"
      poly
      (String.concat ~sep:" " (List.map args ~f:structural_to_concrete))
  | List structural ->
    Printf.sprintf "(Versioned_value.to_list ~f:%s)" (structural_to_concrete structural)
  | Option structural ->
    Printf.sprintf "(Versioned_value.to_option ~f:%s)" (structural_to_concrete structural)
  | Tuple tuple -> tuple_to_concrete tuple

and tuple_to_concrete tuple =
  Printf.sprintf "(Versioned_value.to_tuple%d %s)"
    (List.length tuple)
    (String.concat ~sep:" "
       (List.mapi tuple ~f:(fun i structural ->
          Printf.sprintf "~f%d:%s" (i + 1) (structural_to_concrete structural))))

let print_clause_type name (clause : Astlib_ast.Grammar.clause) ~opaque =
  Print.format "| %s%s"
    name
    (match clause with
     | Empty -> ""
     | Tuple tuple ->
       " of " ^ String.concat ~sep:" * " (List.map tuple ~f:(type_of_structural ~opaque))
     | Record record -> " of " ^ type_of_record record ~opaque)

let print_variant_type variant ~opaque =
  Print.format "type t =";
  Print.indented (fun () ->
    List.iter variant ~f:(fun (name, clause) ->
      print_clause_type name clause ~opaque))

let type_vars vars =
  match vars with
  | [] -> ""
  | [var] -> "'" ^ var ^ " "
  | _ ->
    Printf.sprintf "(%s) "
      (String.concat ~sep:", " (List.map vars ~f:(fun var -> "'" ^ var)))

let print_nominal_type (nominal : Astlib_ast.Grammar.nominal) ~opaque =
  match nominal with
  | Alias structural -> Print.format "type t = %s" (type_of_structural structural ~opaque)
  | Record record -> Print.format "type t = %s" (type_of_record record ~opaque)
  | Variant variant -> print_variant_type variant ~opaque

let tuple_argument_types tuple =
  List.map tuple ~f:(type_of_structural ~opaque:true)

let record_argument_types record =
  List.map record ~f:(fun (name, structural) ->
    name ^ ":" ^ type_of_structural structural ~opaque:true)

let clause_argument_types (clause : Astlib_ast.Grammar.clause) =
  match clause with
  | Empty -> []
  | Tuple tuple -> tuple_argument_types tuple
  | Record record -> record_argument_types record

let constructor_name ~suffix =
  match suffix with
  | None -> "create"
  | Some suffix -> "create_" ^ String.lowercase_ascii suffix

let declare_constructor ~suffix ~vars ~arguments =
  Print.format "val %s : %s"
    (constructor_name ~suffix)
    (String.concat ~sep:" -> " (arguments @ [type_vars vars ^ "t"]))

let declare_clause_constructor name ~vars (clause : Astlib_ast.Grammar.clause) =
  declare_constructor
    ~suffix:(Some name)
    ~vars
    ~arguments:(clause_argument_types clause)

let declare_nominal_constructors ~vars (nominal : Astlib_ast.Grammar.nominal) =
  match nominal with
  | Alias structural ->
    declare_constructor
      ~suffix:None
      ~vars
      ~arguments:[type_of_structural structural ~opaque:true]
  | Record record ->
    declare_constructor ~suffix:None ~vars ~arguments:(record_argument_types record)
  | Variant variant ->
    List.iter variant ~f:(fun (name, clause) ->
      declare_clause_constructor name ~vars clause)

let define_constructor ~decl_name ~suffix ~arguments data =
  Print.format "let %s ="
    (String.concat ~sep:" " (constructor_name ~suffix :: arguments));
  Print.indented (fun () ->
    Print.format "Versioned_ast.create ~version { name = %S; data = %s }" decl_name data)

let tuple_argument i = Printf.sprintf "x%d" (i + 1)
let tuple_arguments tuple = List.init ~len:(List.length tuple) ~f:tuple_argument

let define_tuple_constructor ~decl_name ~suffix tuple f =
  define_constructor ~decl_name ~suffix ~arguments:(tuple_arguments tuple)
    (f (Printf.sprintf "[%s]"
          (String.concat ~sep:"; "
             (List.mapi tuple ~f:(fun i structural ->
                Printf.sprintf "%s %s"
                  (structural_of_concrete structural)
                  (tuple_argument i))))))

let define_record_constructor ~decl_name ~suffix record f =
  define_constructor ~decl_name ~suffix
    ~arguments:(List.map record ~f:(fun (field, _) -> "~" ^ field))
    (f (Printf.sprintf "[%s]"
          (String.concat ~sep:"; "
             (List.map record ~f:(fun (field, structural) ->
                Printf.sprintf "(%S, %s %s)"
                  field
                  (structural_of_concrete structural)
                  field)))))

let define_clause_constructor name ~decl_name (clause : Astlib_ast.Grammar.clause) =
  let suffix = Some name in
  match clause with
  | Empty ->
    define_constructor ~decl_name ~suffix ~arguments:[]
      (Printf.sprintf "Variant (%S, Empty)" name)
  | Tuple tuple ->
    define_tuple_constructor ~decl_name ~suffix tuple (fun list ->
      Printf.sprintf "Variant (%S, Tuple %s)" name list)
  | Record record ->
    define_record_constructor ~decl_name ~suffix record (fun list ->
      Printf.sprintf "Variant (%S, Record %s)" name list)

let define_nominal_constructors ~decl_name (nominal : Astlib_ast.Grammar.nominal) =
  match nominal with
  | Alias structural ->
    define_constructor
      ~decl_name
      ~suffix:None
      ~arguments:["x"]
      (Printf.sprintf "%s x" (structural_of_concrete structural))
  | Record record ->
    define_record_constructor ~decl_name ~suffix:None record (fun list ->
      Printf.sprintf "Record %s" list)
  | Variant variant ->
    List.iter variant ~f:(fun (name, clause) ->
      define_clause_constructor name ~decl_name clause)

let print_clause_of_concrete name (clause : Astlib_ast.Grammar.clause) =
  let suffix = Some name in
  match clause with
  | Empty -> Print.format "| %s -> %s" name (constructor_name ~suffix)
  | Tuple tuple ->
    Print.format "| %s (%s) ->" name (String.concat ~sep:", " (tuple_arguments tuple));
    Print.indented (fun () ->
      Print.format "%s %s"
        (constructor_name ~suffix)
        (String.concat ~sep:" " (tuple_arguments tuple)))
  | Record record ->
    Print.format "| %s { %s } ->" name (String.concat ~sep:"; " (List.map record ~f:fst));
    Print.indented (fun () ->
      Print.format "%s %s"
        (constructor_name ~suffix)
        (String.concat ~sep:" " (List.map record ~f:(fun (field, _) -> "~" ^ field))))

let with_optional_bindings bindings f =
  let rec loop bindings =
    match bindings with
    | [] -> f ()
    | (arg, expr) :: bindings ->
      Print.format "Optional.bind (%s) ~f:(fun %s ->" expr arg;
      Print.indented (fun () -> loop bindings)
  in
  loop bindings;
  let count = List.length bindings in
  if count > 0 then Print.format "%s" (String.make count ')')

let with_tuple_to_concrete tuple f =
  let bindings =
    List.mapi tuple ~f:(fun i structural ->
      let arg = tuple_argument i in
      let conv = structural_to_concrete structural in
      arg, Printf.sprintf "%s %s" conv arg)
  in
  with_optional_bindings bindings f

let with_record_to_concrete record f =
  let bindings =
    List.map record ~f:(fun (field, structural) ->
      let conv = structural_to_concrete structural in
      field, Printf.sprintf "%s %s" conv field)
  in
  with_optional_bindings bindings f

let print_clause_to_concrete name (clause : Astlib_ast.Grammar.clause) =
  match clause with
  | Empty ->
    Print.format "| Variant (%S, Empty) -> Some %s" name name
  | Tuple tuple ->
    Print.format "| Variant (%S, Tuple [%s]) ->"
      name
      (String.concat ~sep:"; " (tuple_arguments tuple));
    Print.indented (fun () ->
      with_tuple_to_concrete tuple (fun () ->
        Print.format "Some (%s (%s))"
          name
          (String.concat ~sep:", " (tuple_arguments tuple))))
  | Record record ->
    Print.format "| Variant (%S, Record [%s]) ->"
      name
      (String.concat ~sep:"; " (List.map record ~f:fst));
    Print.indented (fun () ->
      with_record_to_concrete record (fun () ->
        Print.format "Some (%s { %s })"
          name
          (String.concat ~sep:"; " (List.map record ~f:fst))))

let print_of_concrete ~vars ~pattern print_body =
  let tyvars = type_vars vars in
  Print.format "let of_concrete (%s : %sConcrete.t) : %st =" pattern tyvars tyvars;
  Print.indented print_body

let print_nominal_of_concrete ~decl_name ~vars (nominal : Astlib_ast.Grammar.nominal) =
  match nominal with
  | Alias structural ->
    let arg = "concrete" in
    print_of_concrete ~vars ~pattern:arg (fun () ->
      Print.format "{ name = %S; data = %s %s }"
        decl_name
        (structural_of_concrete structural)
        arg)
  | Record record ->
    print_of_concrete
      ~vars
      ~pattern:(Printf.sprintf "{ %s }"
                  (String.concat ~sep:", " (List.map record ~f:fst)))
      (fun () ->
         Print.format "{ name = %S; data = Record [%s] }"
           decl_name
           (String.concat ~sep:"; "
              (List.map record ~f:(fun (field, structural) ->
                 Printf.sprintf "(%S, %s %s)"
                   field
                   (structural_of_concrete structural)
                   field))))
  | Variant variant ->
    let arg = "concrete" in
    print_of_concrete ~vars ~pattern:arg (fun () ->
      Print.format "match %s with" arg;
      List.iter variant ~f:(fun (name, clause) ->
        print_clause_of_concrete name clause))

let print_to_concrete ~decl_name ~vars ~pattern print_body =
  let tyvars = type_vars vars in
  Print.format "let to_concrete t : %sConcrete.t =" tyvars;
  Print.indented (fun () ->
    Print.format "match Versioned_ast.convert t ~version with";
    Print.format "| { name = %S; data = %s } ->" decl_name pattern;
    Print.indented print_body;
    Print.format "| _ -> None")

let print_nominal_to_concrete ~decl_name ~vars (nominal : Astlib_ast.Grammar.nominal) =
  match nominal with
  | Alias structural ->
    let var = "data" in
    print_to_concrete ~decl_name ~vars ~pattern:var (fun () ->
      Print.format "%s %s" (structural_to_concrete structural) var)
  | Record record ->
    print_to_concrete ~decl_name ~vars
      ~pattern:(Printf.sprintf "Record [%s]"
                  (String.concat ~sep:"; "
                     (List.map record ~f:(fun (field, _) ->
                        Printf.sprintf "%S, %s" field field))))
      (fun () ->
         with_record_to_concrete record (fun () ->
           Print.format "Some { %s }" (String.concat ~sep:"; " (List.map record ~f:fst))))
  | Variant variant ->
    let var = "data" in
    print_to_concrete ~decl_name ~vars ~pattern:var (fun () ->
      Print.format "( match %s with" var;
      List.iter variant ~f:(fun (name, clause) ->
        print_clause_to_concrete name clause);
      Print.format ")")

let print_decl_signature ~decl_name ({ vars; body = nominal } : Astlib_ast.Grammar.decl) =
  Print.format "type t = Unversioned.%s" (String.lowercase_ascii decl_name);
  Print.newline ();
  Print.format "val of_ast : Versioned_ast.t -> t";
  Print.format "val to_ast : t -> Versioned_ast.t";
  Print.newline ();
  Print.declare_module "Concrete" (fun () ->
    print_nominal_type nominal ~opaque:true);
  Print.newline ();
  Print.format "val of_concrete : Concrete.t -> t";
  Print.format "val to_concrete : t -> Concrete.t option";
  declare_nominal_constructors ~vars nominal

let print_decl_structure ~decl_name ({ vars; body = nominal } : Astlib_ast.Grammar.decl) =
  Print.format "type t = Versioned_ast.t";
  Print.newline ();
  Print.format "let of_ast t = t";
  Print.format "let to_ast t = t";
  Print.newline ();
  Print.define_module "Concrete" (fun () ->
    print_nominal_type nominal ~opaque:false);
  Print.newline ();
  define_nominal_constructors ~decl_name nominal;
  Print.newline ();
  print_nominal_of_concrete ~decl_name ~vars nominal;
  Print.newline ();
  print_nominal_to_concrete ~decl_name ~vars nominal

let print_grammar_mli grammar =
  Print.declare_recursive_modules
    (List.map grammar ~f:(fun (decl_name, decl) ->
       (String.capitalize_ascii decl_name,
        (fun () -> print_decl_signature ~decl_name decl))))

let print_grammar_ml grammar =
  List.iteri grammar ~f:(fun decl_index (decl_name, decl) ->
    if decl_index > 0 then Print.newline ();
    Print.define_module (String.capitalize_ascii decl_name) (fun () ->
      print_decl_structure ~decl_name decl))

let names_of_versioned_grammars versioned_grammars =
  versioned_grammars
  |> List.map ~f:snd
  |> List.concat
  |> List.map ~f:fst
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
