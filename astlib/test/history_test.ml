open Base
open Stdio

let () =
  Expect_test_helpers_base.sexp_style
  := Expect_test_helpers_base.Sexp_style.simple_pretty

let%expect_test "versions" =
  Astlib_ast.For_testing.history
  |> Astlib_ast.History.to_versioned_grammars
  |> List.iter ~f:(fun (version, _) ->
    print_endline version);
  [%expect {|
    V1
    V2
    V3 |}]

let rec sexp_of_data : Astlib_ast.Grammar.data -> Sexp.t = function
  | Kind name -> Atom (name ^ ".t")
  | Bool -> Atom "bool"
  | Char -> Atom "char"
  | String -> Atom "string"
  | List data -> List [sexp_of_data data; Atom "list"]
  | Option data -> List [sexp_of_data data; Atom "option"]

let sexp_of_field ({ field_name; data } : Astlib_ast.Grammar.field) =
  [%sexp (field_name : string), (data : data)]

let sexp_of_clause ({ clause_name; fields } : Astlib_ast.Grammar.clause) =
  [%sexp (clause_name : string), (fields : field list)]

let sexp_of_kind ({ kind_name; clauses } : Astlib_ast.Grammar.kind) =
  [%sexp (kind_name : string), (clauses : clause list)]

let sexp_of_grammar = [%sexp_of: kind list]

let%expect_test "grammars" =
  Astlib_ast.For_testing.history
  |> Astlib_ast.History.to_versioned_grammars
  |> List.iter ~f:(fun (version, grammar) ->
    print_endline "";
    print_endline version;
    Expect_test_helpers_base.print_s [%sexp (grammar : grammar)]);
  [%expect {|
    V1
    ((Expression
      ((Ident ((name string))) (Match ((arg Expression.t) (cases (Case.t list))))))
     (Case
      ((Case ((lhs Pattern.t) (guard (Expression.t option)) (rhs Expression.t)))))
     (Pattern ((Ident ((name string))))))

    V2
    ((Expression
      ((Ident ((name string))) (Match ((arg Expression.t) (cases (Case.t list))))))
     (Case ((Case ((lhs Pattern.t) (rhs Expression.t)))))
     (Pattern
      ((Ident ((name string))) (Guard ((body Pattern.t) (guard Expression.t))))))

    V3
    ((Identifier
      ((Var ((name string))) (Dot ((base Identifier.t) (label string)))))
     (Expression
      ((Ident ((id Identifier.t)))
       (Match ((arg Expression.t) (cases (Case.t list))))))
     (Case ((Case ((lhs Pattern.t) (rhs Expression.t)))))
     (Pattern
      ((Ident ((name string))) (Guard ((body Pattern.t) (guard Expression.t)))))) |}]

type conversion = Astlib_ast.History.conversion

let sexp_of_conversion (conversion : conversion) : Sexp.t = Atom conversion.name

type conversion_step = Astlib_ast.History.conversion_step =
  { src_version : string
  ; dst_version : string
  ; conversion : conversion
  }
[@@deriving sexp_of]

let%expect_test "conversions" =
  let versions =
    Astlib_ast.For_testing.history
    |> Astlib_ast.History.to_versioned_grammars
    |> List.map ~f:fst
  in
  List.iter versions ~f:(fun from_version ->
    List.iter versions ~f:(fun to_version ->
      match
        Astlib_ast.History.conversion_steps
          Astlib_ast.For_testing.history
          ~from_version
          ~to_version
      with
      | None ->
        Expect_test_helpers_base.print_cr [%here]
          [%message "no conversion steps found" ~from_version ~to_version]
      | Some steps ->
        Expect_test_helpers_base.print_s
          [%message "" ~from_version ~to_version (steps : conversion_step list)]));
  [%expect {|
    ((from_version V1) (to_version V1) (steps ()))
    ((from_version V1)
     (to_version V2)
     (steps (((src_version V1) (dst_version V2) (conversion v2_of_v1)))))
    ((from_version V1)
     (to_version V3)
     (steps
      (((src_version V1) (dst_version V2) (conversion v2_of_v1))
       ((src_version V2) (dst_version V3) (conversion v3_of_v2)))))
    ((from_version V2)
     (to_version V1)
     (steps (((src_version V2) (dst_version V1) (conversion v1_of_v2)))))
    ((from_version V2) (to_version V2) (steps ()))
    ((from_version V2)
     (to_version V3)
     (steps (((src_version V2) (dst_version V3) (conversion v3_of_v2)))))
    ((from_version V3)
     (to_version V1)
     (steps
      (((src_version V3) (dst_version V2) (conversion v2_of_v3))
       ((src_version V2) (dst_version V1) (conversion v1_of_v2)))))
    ((from_version V3)
     (to_version V2)
     (steps (((src_version V3) (dst_version V2) (conversion v2_of_v3)))))
    ((from_version V3) (to_version V3) (steps ())) |}]
