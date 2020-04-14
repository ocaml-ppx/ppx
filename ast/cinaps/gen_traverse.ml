open Stdppx

let string_of_targ targ = Grammar.string_of_targ ~internal:false targ

let parens x = Printf.sprintf "(%s)" x

type base_method =
  { method_name : string
  ; params : string list
  ; type_name : string
  }

let base_methods =
  [ {method_name = "bool"; params = []; type_name = "bool"}
  ; {method_name = "char"; params = []; type_name = "char"}
  ; {method_name = "int"; params = []; type_name = "int"}
  ; {method_name = "list"; params = ["a"]; type_name = "list"}
  ; {method_name = "option"; params = ["a"]; type_name = "option"}
  ; {method_name = "string"; params = []; type_name = "string"}
  ; {method_name = "location"; params = []; type_name = "Astlib.Location.t"}
  ; {method_name = "loc"; params = ["a"]; type_name = "Astlib.Loc.t"}
  ]

let poly_signature ~signature ~params ~type_name =
  let poly_type = Ml.poly_type ~tvars:params type_name in
  let poly_params = List.map ~f:Ml.tvar params in
  let fun_pre_args = List.map ~f:(fun t -> parens (signature t)) poly_params in
  let fun_sig = Ml.arrow_type (fun_pre_args @ [signature poly_type]) in
  let universal_quantifiers = String.concat ~sep:" " poly_params in
  Printf.sprintf "%s . %s" universal_quantifiers fun_sig

let base_method_signature ~signature ~params ~type_name =
  match params with
  | [] -> signature type_name
  | params -> poly_signature ~signature ~params ~type_name

let declare_base_method ~signature {method_name; params; type_name} =
  let signature = base_method_signature ~signature ~params ~type_name in
  Ml.declare_method ~virtual_:true ~signature ~name:method_name ()

(* This type describes a variables bound when deconstructing a value.
   - [var] is the variable name
   - [recursive_call] is the function to apply to the variable to recursively
   traverse it. It is based on the type of the variable. For example if the
   variable is an expression it will be ["self#expression"]. If it's a list
    of integers it will be ["self#list self#int"]. *)
type var =
  { var : string
  ; recursive_call : string
  }

type kind =
  | Kalias
  | Krecord
  | Ktuple
  | Kconstr of string

(** This is a generic type used to describe how to deconstruct a value of any
    given type.
    - [pattern] is the OCaml pattern used to deconstruct the value, as a string.
    For example, for a record type with two fields a and b, it would be
    ["{a; b}"], for a pair it would be ["(x0, x1)"].
    - [vars] is the list of variables that are bound in the above mentioned
    pattern.
    - [kind] describes the kind of value of the deconstructed value. *)
type deconstructed =
  { pattern : string
  ; vars : var list
  ; kind : kind
  }

(** This type describes the kind of value we're trying to traverse.
    - [Ast_type {node_name; targs}] means we are traversing a named type of
    the AST. [node_name] is the name of the AST node we
    want to traverse and [targs] are its type arguments if it is an
    instance of a polymorphic AST type.
    - [Abstract] means we're inside an anonymous function and want
    to traverse something that isn't a named type of the AST.

    This distinction is useful for map-like traversal classes that need to
    return abstract type and therefore must wrap the "reconstructed" value
    in an [of_concrete] call in the [Ast_type _] case.
    Other traversal classes can ignore the context. *)
type value_kind =
  | Ast_type of {node_name : string; targs : Astlib.Grammar.targ list}
  | Abstract

(** The type used to describe the various traversal classes and how to generate
    them.
    - [class_name] is the name of the class
    - [extra_methods] prints any methods which isn't generated from the types
    description in the grammar that the class needs
    - [complete] determines whether all methods are defined in [Virtual_traverse]
    and [Traverse_builtins] or if it needs to be declared as [virtual] in
    [Traverse].
    - [params] is the list of type parameters for the class, as strings.
    - [signature] returns the signature of a method given the name of the input
    type. For examples for map, [signature "expression"] is
    ["expression -> expression"].
    - [args] returns the list of arguments of one the class' methods, given the
    name of the node it must be applied to. For example, for fold, [args "x"] is
    [["x"; "acc"]].
    - [recurse] is the core of the traversal. It generates the code for traversing
    a the given deconstructed value. *)
type traversal =
  { class_name : string
  ; extra_methods : (unit -> unit) option
  ; complete : bool
  ; params : string list option
  ; signature : string -> string
  ; args : string -> string list
  ; recurse : value_kind: value_kind -> deconstructed: deconstructed -> string list
  }

type type_ = Concrete | T

let node_type ~type_ ~args node_name =
  let type_name = match type_ with T -> "t" | Concrete -> "concrete" in
  let node_type = Printf.sprintf "%s.%s" (Ml.module_name node_name) type_name in
  let args = List.map args ~f:string_of_targ in
  Ml.poly_inst node_type ~args

let fun_arg type_name = Ml.id (Printf.sprintf "f%s" type_name)

let tuple_var i = Printf.sprintf "x%d" i

let rec deconstruct_alias ~traversal ~var ty =
  let vars = [{var; recursive_call = recursive_call ~traversal ty}] in
  let pattern = var in
  {pattern; vars; kind = Kalias}

and deconstruct_record ~traversal fields =
  let vars =
    List.map fields ~f:(fun (var, ty) -> {var; recursive_call = recursive_call ~traversal ty})
  in
  let var_names = List.map vars ~f:(fun {var; _} -> var) in
  let pattern = Printf.sprintf "{ %s }" (String.concat ~sep:"; " var_names) in
  {pattern; vars; kind = Krecord}

and deconstruct_tuple ~traversal tyl =
  let vars =
    List.mapi tyl ~f:(fun i ty -> {var = (tuple_var i); recursive_call = recursive_call ~traversal ty})
  in
  let var_names = List.map vars ~f:(fun {var; _} -> var) in
  let pattern =
    match var_names with
    | [n] -> n
    | nl -> Ml.tuple nl
  in
  {pattern; vars; kind = Ktuple}

and deconstruct_variant ~traversal (name, clause) =
  let constr_pattern = Printf.sprintf "%s %s" name in
  match (clause : Astlib.Grammar.clause) with
  | Empty -> {pattern = name; vars = []; kind = Kconstr name}
  | Tuple tyl ->
    let d = deconstruct_tuple ~traversal tyl in
    { d with pattern = constr_pattern d.pattern; kind = Kconstr name }
  | Record fields ->
    let d = deconstruct_record ~traversal fields in
    { d with pattern = constr_pattern d.pattern; kind = Kconstr name }

and recursive_call ?(nested=false) ~traversal (ty : Astlib.Grammar.ty) =
  let recursive_call = recursive_call ~traversal in
  let parens s = if nested then parens s else s in
  match ty with
  | Var s -> (fun_arg s)
  | Name n -> Printf.sprintf "self#%s" (Ml.id n)
  | Bool -> "self#bool"
  | Char -> "self#char"
  | Int -> "self#int"
  | String -> "self#string"
  | List ty -> parens (Printf.sprintf "self#list %s" (recursive_call ~nested:true ty))
  | Option ty -> parens (Printf.sprintf "self#option %s" (recursive_call ~nested:true ty))
  | Tuple tyl ->
    let deconstructed = deconstruct_tuple ~traversal tyl in
    let exprs = traversal.recurse ~value_kind:Abstract ~deconstructed in
    let args = String.concat ~sep:" " (traversal.args deconstructed.pattern) in
    Printf.sprintf "(fun %s -> %s)" args (String.concat ~sep:" " exprs)
  | Instance (n, tyl) ->
    Printf.sprintf "self#%s" (Name.make [n] tyl)
  | Loc ty -> parens (Printf.sprintf "self#loc %s" (recursive_call ~nested:true ty))
  | Location -> "self#location"

let print_method_for_alias ~traversal ~value_kind ~var ty =
  match (ty : Astlib.Grammar.ty) with
  | Tuple tyl ->
    let deconstructed = deconstruct_tuple ~traversal tyl in
    let exprs =
      traversal.recurse ~value_kind ~deconstructed
    in
    Print.println "let %s = %s in" deconstructed.pattern var;
    List.iter exprs ~f:(Print.println "%s")
  | _ ->
    let deconstructed = deconstruct_alias ~traversal ~var ty in
    let exprs =
      traversal.recurse ~value_kind ~deconstructed
    in
    List.iter exprs ~f:(Print.println "%s")

let print_method_body
      ~traversal
      ~targs
      ~node_name
      ~var
      (decl : Astlib.Grammar.decl)
  =
  let value_kind = Ast_type {node_name; targs} in
  match decl with
  | Wrapper ty -> print_method_for_alias ~traversal ~value_kind ~var ty
  | Record fields ->
    let deconstructed = deconstruct_record ~traversal fields in
    let concrete_type = (node_type ~type_:Concrete ~args:targs node_name) in
    let exprs = traversal.recurse ~value_kind ~deconstructed in
    Print.println "let %s : %s = %s in" deconstructed.pattern concrete_type var;
    List.iter exprs ~f:(Print.println "%s")
  | Variant variants ->
    let concrete_type = (node_type ~type_:Concrete ~args:targs node_name) in
    Print.println "match (%s : %s) with" var concrete_type;
    List.iter variants
      ~f:(fun variant ->
        let deconstructed = deconstruct_variant ~traversal variant in
        let exprs = traversal.recurse ~value_kind ~deconstructed in
        Print.println "| %s ->" deconstructed.pattern;
        Print.indented (fun () -> List.iter exprs ~f:(Print.println "%s")))

(** Return the deconstructed pattern, wrapped in parens if it needs to be *)
let parenthesized_pattern = function
  | {kind = Kconstr _; vars = _::_; pattern} -> parens pattern
  | {kind = Kalias | Krecord | Ktuple; vars = _; pattern}
  | {kind = _; vars = []; pattern} -> pattern

module Map = struct
  let signature node_type = Ml.arrow_type [node_type; node_type]

  let args x = [x]

  let recurse ~value_kind ~deconstructed =
    let {pattern; vars; _} = deconstructed in
    let recurse =
      List.map vars
        ~f:(fun {var; recursive_call} ->
          Printf.sprintf "let %s = %s %s in" var recursive_call var)
    in
    let return =
      match value_kind with
      | Abstract -> pattern
      | Ast_type {node_name; targs=_} ->
        Printf.sprintf "%s.%s %s"
          (Ml.module_name node_name)
          "of_concrete"
          (parenthesized_pattern deconstructed)
    in
    recurse @ [return]

  let traversal =
    { class_name = "map"
    ; extra_methods = None
    ; complete = true
    ; params = None
    ; signature
    ; args
    ; recurse
    }
end

module Iter = struct
  let signature node_type = Ml.arrow_type [node_type; "unit"]

  let args x = [x]

  let recurse ~value_kind:_ ~deconstructed:{vars; _} =
    match vars with
    | [] -> ["()"]
    | _ ->
      let length = List.length vars in
      List.mapi vars
        ~f:(fun i {var; recursive_call} ->
          let apply = Printf.sprintf "%s %s" recursive_call var in
          if i = length - 1 then apply else apply ^ ";")

  let traversal =
    { class_name = "iter"
    ; extra_methods = None
    ; complete = true
    ; params = None
    ; signature
    ; args
    ; recurse
    }
end

module Fold = struct
  let acc_var = "acc"
  let acc_type = Ml.tvar acc_var

  let signature node_type = Ml.arrow_type [node_type; acc_type; acc_type]

  let args x = [x; acc_var]

  let recurse ~value_kind:_ ~deconstructed:{vars; _} =
    let recurse =
      List.map vars
        ~f:(fun {var; recursive_call} ->
          Printf.sprintf "let %s = %s %s %s in" acc_var recursive_call var acc_var)
    in
    let return = acc_var in
    recurse @ [return]

  let traversal =
    { class_name = "fold"
    ; extra_methods = None
    ; complete = true
    ; params = Some [acc_type]
    ; signature
    ; args
    ; recurse
    }
end

module Fold_map = struct
  let acc_var = "acc"
  let acc_type = Ml.tvar acc_var

  let signature node_type =
    Ml.arrow_type [node_type; acc_type; parens (Ml.tuple_type [node_type; acc_type])]

  let args x = [x; acc_var]

  let recurse ~value_kind ~deconstructed =
    let {pattern; vars; _} = deconstructed in
    let recurse =
      List.map vars
        ~f:(fun {var; recursive_call} ->
          Printf.sprintf "let %s = %s %s %s in" (Ml.tuple [var; acc_var]) recursive_call var acc_var)
    in
    let return =
      match value_kind with
      | Abstract -> Ml.tuple [pattern; acc_var]
      | Ast_type {node_name; targs=_} ->
        let mapped =
          Printf.sprintf "%s.%s %s"
            (Ml.module_name node_name)
            "of_concrete"
            (parenthesized_pattern deconstructed)
        in
        Ml.tuple [mapped; acc_var]
    in
    recurse @ [return]

  let traversal =
    { class_name = "fold_map"
    ; extra_methods = None
    ; complete = true
    ; params = Some [acc_type]
    ; signature
    ; args
    ; recurse
    }
end

module Map_with_context = struct
  let ctx_var = "_ctx"
  let ctx_type = Ml.tvar "ctx"

  let signature node_type =
    Ml.arrow_type [ctx_type; node_type; node_type]

  let args x = [ctx_var; x]

  let recurse ~value_kind ~deconstructed =
    let {pattern; vars; _} = deconstructed in
    let recurse =
      List.map vars
        ~f:(fun {var; recursive_call} ->
          Printf.sprintf "let %s = %s %s %s in" var recursive_call ctx_var var)
    in
    let return =
      match value_kind with
      | Abstract -> pattern
      | Ast_type {node_name; targs=_} ->
        Printf.sprintf "%s.%s %s"
          (Ml.module_name node_name)
          "of_concrete"
          (parenthesized_pattern deconstructed)
    in
    recurse @ [return]

  let traversal =
    { class_name = "map_with_context"
    ; extra_methods = None
    ; complete = true
    ; params = Some [ctx_type]
    ; signature
    ; args
    ; recurse
    }
end

module Lift = struct
  let res_type = Ml.tvar "res"

  let extra_methods () =
    Ml.declare_method
      ~virtual_:true
      ~name:"node"
      ~signature:"(string * int) option -> 'res -> 'res"
      ();
    Ml.declare_method
      ~virtual_:true
      ~name:"record"
      ~signature:"(string * int) option -> (string * 'res) list -> 'res"
      ();
    Ml.declare_method
      ~virtual_:true
      ~name:"constr"
      ~signature:"(string * int) option -> string -> 'res list -> 'res"
      ();
    Ml.declare_method
      ~virtual_:true
      ~name:"tuple"
      ~signature:"'res list -> 'res"
      ()

  let signature node_type = Ml.arrow_type [node_type; res_type]

  let args x = [x]

  let var_names vars = List.map vars ~f:(fun v -> v.var)

  let tuple_arg vars =
    let var_names = var_names vars in
    Ml.list_lit var_names

  let record_arg vars =
    let var_names = var_names vars in
    let name_and_val var_name = Printf.sprintf "(%S, %s)" var_name var_name in
    Ml.list_lit (List.map ~f:name_and_val var_names)

  let make_node_arg ~value_kind =
    match value_kind with
    | Abstract -> "None"
    | Ast_type { node_name; targs } ->
      sprintf "(Some (%S, %d))" node_name (List.length targs)

  let recurse ~value_kind ~deconstructed =
    let {kind; vars; pattern} = deconstructed in
    let recurse =
      List.map vars
        ~f:(fun {var; recursive_call} ->
          Printf.sprintf "let %s = %s %s in" var recursive_call var)
    in
    let node_arg = make_node_arg ~value_kind in
    let result =
      match kind with
      | Kalias ->
        Printf.sprintf "self#node %s %s" node_arg pattern
      | Ktuple ->
        Printf.sprintf "self#node %s (self#tuple %s)" node_arg (tuple_arg vars)
      | Krecord ->
        Printf.sprintf "self#record %s %s" node_arg (record_arg vars)
      | Kconstr name ->
        Printf.sprintf "self#constr %s %S %s" node_arg name (tuple_arg vars)
    in
    recurse @ [result]

  let traversal =
    { class_name = "lift"
    ; extra_methods = Some extra_methods
    ; complete = false
    ; params = Some [res_type]
    ; signature
    ; args
    ; recurse
    }
end

let traversal_classes =
  [ Map.traversal
  ; Iter.traversal
  ; Fold.traversal
  ; Fold_map.traversal
  ; Map_with_context.traversal
  ; Lift.traversal
  ]

let print_to_concrete node_name =
  Print.println "let concrete = %s.to_concrete %s in"
    (Ml.module_name node_name)
    (Ml.id node_name)

let print_method_value ~traversal ~targs ~node_name decl =
  let args = traversal.args (Ml.id node_name) in
  Ml.define_anon_fun ~args (fun () ->
    print_to_concrete node_name;
    print_method_body ~traversal ~targs ~node_name ~var:"concrete" decl)

let declare_node_methods ~env_table ~signature (node_name, kind) =
  match (kind : Astlib.Grammar.kind) with
  | Mono _ ->
    let name = Name.make [node_name] [] in
    let signature = signature (node_type ~type_:T ~args:[] node_name) in
    Ml.declare_method ~signature ~name ()
  | Poly (_, _) ->
    let envs = Poly_env.find env_table node_name in
    List.iter envs ~f:(fun env ->
      let args = Poly_env.args env in
      let name = Name.make [node_name] args in
      let signature = signature (node_type ~type_:T ~args node_name) in
      Ml.declare_method ~signature ~name ())

let define_node_methods ~env_table ~traversal (node_name, kind) =
  match (kind : Astlib.Grammar.kind) with
  | Mono decl ->
    let name = Name.make [node_name] [] in
    let signature = traversal.signature (node_type ~type_:T ~args:[] node_name) in
    Ml.define_method ~signature name (fun () ->
      print_method_value ~traversal ~targs:[] ~node_name decl)
  | Poly (_, decl) ->
    let envs = Poly_env.find env_table node_name in
    List.iter envs ~f:(fun env ->
      let targs = Poly_env.args env in
      let name = Name.make [node_name] targs in
      let signature = traversal.signature (node_type ~type_:T ~args:targs node_name) in
      let subst_decl = Poly_env.subst_decl ~env decl in
      Ml.define_method ~signature name (fun () ->
        print_method_value ~traversal ~targs ~node_name subst_decl))

let declare_virtual_traversal_class ~traversal grammar =
  let env_table = Poly_env.env_table grammar in
  let {class_name; params; signature; extra_methods; _} = traversal in
  Ml.declare_class ~virtual_:true ?params class_name (fun () ->
    Ml.declare_object (fun () ->
      Option.iter extra_methods ~f:(fun f -> f ());
      List.iter base_methods ~f:(declare_base_method ~signature);
      List.iter grammar ~f:(declare_node_methods ~env_table ~signature)))

let define_virtual_traversal_class ~traversal grammar =
  let env_table = Poly_env.env_table grammar in
  let {class_name; params; signature; extra_methods; _} = traversal in
  Ml.define_class ~virtual_:true ?params class_name (fun () ->
    Ml.define_object ~bind_self:true (fun () ->
      Option.iter extra_methods ~f:(fun f -> f ());
      List.iter base_methods ~f:(declare_base_method ~signature);
      List.iter grammar ~f:(define_node_methods ~env_table ~traversal)))

let declare_virtual_traversal_classes grammar =
  List.iter traversal_classes
    ~f:(fun traversal ->
      Print.newline ();
      declare_virtual_traversal_class ~traversal grammar)

let define_virtual_traversal_classes grammar =
  List.iter traversal_classes
    ~f:(fun traversal ->
      Print.newline ();
      define_virtual_traversal_class ~traversal grammar)

let print_virtual_traverse_ml version =
  Print.newline ();
  let grammar = Astlib.History.find_grammar Astlib.history ~version in
  let version = Ml.module_name (Astlib.Version.to_string version) in
  Print.println "open Versions.%s" version;
  define_virtual_traversal_classes grammar

let print_virtual_traverse_mli version =
  Print.newline ();
  let grammar = Astlib.History.find_grammar Astlib.history ~version in
  Print.println "open Versions.%s" (Ml.module_name (Astlib.Version.to_string version));
  declare_virtual_traversal_classes grammar

let inherits ~params ~class_name ~version =
  let params = Ml.class_params params in
  Ml.declare_object (fun () ->
    Print.println "inherit %sTraverse_builtins.%s" params class_name;
    Print.println "inherit %sVirtual_traverse.%s.%s"
      params (Ml.module_name version) class_name)

let traversal_class ~impl ~traversal:{params; class_name; complete; _} ~version =
  let virtual_ = not complete in
  let params = Option.value ~default:[] params in
  let object_ () = inherits ~params ~class_name ~version in
  if impl then
    Ml.define_class ~virtual_ ~params class_name object_
  else
    Ml.declare_class ~virtual_ ~params class_name object_

let print_traverse_ml version =
  Print.newline ();
  let version = Ml.module_name (Astlib.Version.to_string version) in
  List.iteri traversal_classes ~f:(fun i traversal ->
    if i <> 0 then Print.newline ();
    traversal_class ~impl:true ~traversal ~version)

let print_traverse_mli version =
  Print.newline ();
  let version = Ml.module_name (Astlib.Version.to_string version) in
  List.iteri traversal_classes ~f:(fun i traversal ->
    if i <> 0 then Print.newline ();
    traversal_class ~impl:false ~traversal ~version)
