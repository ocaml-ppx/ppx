open! Base
open Astlib_base

module Node = struct
  type t = { ast : t Ast.t } [@@deriving equal, sexp_of]

  let of_ast ast = { ast }
  let to_ast { ast } = ast

  let matches t ~grammar = Ast.matches t ~grammar ~to_ast
  let generator grammar = Ast.generator grammar ~of_ast
  let shrinker = Base_quickcheck.Shrinker.atomic
end

let v1 = Astlib.Version.of_string "v1"
let v2 = Astlib.Version.of_string "v2"
let v3 = Astlib.Version.of_string "v3"

(* simple int exprs: constants and binary addition *)
let (v1_grammar : Astlib.Grammar.t) = [
  "expr", Mono (Variant [
    "int", Tuple [Int];
    "add", Tuple [Name "expr"; Name "expr"];
  ]);
]

(* add variables: new variant that has nothing to down-convert to *)
let (v2_grammar : Astlib.Grammar.t) = [
  "expr", Mono (Variant [
    "var", Tuple [String];
    "int", Tuple [Int];
    "add", Tuple [Name "expr"; Name "expr"];
  ]);
]

(* change addition to n-ary; down-conversion picks arbitrary tree structure *)
let (v3_grammar : Astlib.Grammar.t) = [
  "expr", Mono (Variant [
    "var", Tuple [String];
    "int", Tuple [Int];
    "add", Tuple [List (Name "expr")];
  ]);
]

let v2_of_v1 x ~to_ast:_ ~of_ast:_ = x
let v1_of_v2 x ~to_ast:_ ~of_ast:_ = x

let rec v3_addends_of_v2 ast ~to_ast ~of_ast : _ Ast.data list =
  match (ast : _ Ast.t) with
  | { name = "expr"; data = Variant { tag = "add"; args = [| Node x; Node y |] } } ->
    v3_addends_of_v2 (to_ast x ~version:v2) ~to_ast ~of_ast @
    v3_addends_of_v2 (to_ast y ~version:v2) ~to_ast ~of_ast
  | _ -> [ Node (of_ast ast ~version:v3) ]

let v3_of_v2 ast ~to_ast ~of_ast =
  match v3_addends_of_v2 ast ~to_ast ~of_ast with
  | [ _ ] -> ast
  | list -> { name = "expr" ; data = Variant { tag = "add" ; args = [| List list |] } }

let v2_add x y ~of_ast : _ Ast.t =
  { name = "expr"
  ; data =
      Variant
        { tag = "add"
        ; args = [| Node (of_ast x ~version:v2); Node (of_ast y ~version:v2) |]
        }
  }

let v2_of_v3 ast ~to_ast ~of_ast =
  match (ast : _ Ast.t) with
  | { name = "expr"; data = Variant { tag = "add"; args = [| List data |] } } ->
    (match
       Option.all (List.map data ~f:(function
         | Node node -> Some (to_ast node ~version:v3)
         | _ -> None))
     with
     | None -> ast
     | Some asts ->
       (match List.reduce asts ~f:(v2_add ~of_ast) with
        | Some ast -> ast
        | None -> { name = "expr"; data = Variant { tag = "int"; args = [| Int 0 |] } }))
  | _ -> ast

let versioned_grammars = [
  v1, v1_grammar;
  v2, v2_grammar;
  v3, v3_grammar;
]

let conversions : Astlib.History.conversion list = [
  { src_version = v1; dst_version = v2; f = v2_of_v1 };
  { src_version = v2; dst_version = v1; f = v1_of_v2 };
  { src_version = v2; dst_version = v3; f = v3_of_v2 };
  { src_version = v3; dst_version = v2; f = v2_of_v3 };
]

let history = Astlib.History.create ~versioned_grammars ~conversions

let%expect_test "generator consistency" =
  List.iter versioned_grammars ~f:(fun (version, grammar) ->
    Base_quickcheck.Test.run_exn
      (module struct
        type t = Node.t
        let sexp_of_t = Node.sexp_of_t
        let quickcheck_generator = Node.generator grammar
        let quickcheck_shrinker = Node.shrinker
      end)
      ~f:(fun node ->
        if not (Node.matches node ~grammar)
        then raise_s [%sexp "invalid AST", { version : Version.t }]));
  [%expect {| |}]

(* Down-conversions can fail. We don't currently have a good way to detect when they do,
   so we don't have a good invariant to express about them on their own. *)
let%expect_test "history up-conversions always work" =
  List.iteri versioned_grammars ~f:(fun src_index (src_version, src_grammar) ->
    List.iteri versioned_grammars ~f:(fun dst_index (dst_version, dst_grammar) ->
      if dst_index >= src_index then
        Base_quickcheck.Test.run_exn
          (module struct
            type t = Node.t
            let sexp_of_t = Node.sexp_of_t
            let quickcheck_generator = Node.generator src_grammar
            let quickcheck_shrinker = Node.shrinker
          end)
          ~f:(fun src ->
            let dst =
              src
              |> Node.to_ast
              |> Astlib.History.convert history
                   ~src_version
                   ~dst_version
                   ~to_ast:(fun x ~version:_ -> Node.to_ast x)
                   ~of_ast:(fun x ~version:_ -> Node.of_ast x)
              |> Node.of_ast
            in
            if not (Node.matches dst ~grammar:dst_grammar) then
              raise_s
                [%sexp
                  "invalid conversion"
                , { src_version : Version.t; dst_version : Version.t; dst : Node.t }])));
  [%expect{||}]

(* Round-trips aren't guaranteed to end up at the same AST, case in point, here a tree of
   addends round-tripping through the list version might come back to a
   differently-balanced tree. But even with down-conversions that can fail, one should
   always be able to get back to the original grammar. *)
let%expect_test "history round-trips always work" =
  List.iter versioned_grammars ~f:(fun (src_version, src_grammar) ->
    List.iter versioned_grammars ~f:(fun (dst_version, _) ->
      Base_quickcheck.Test.run_exn
        (module struct
          type t = Node.t
          let sexp_of_t = Node.sexp_of_t
          let quickcheck_generator = Node.generator src_grammar
          let quickcheck_shrinker = Node.shrinker
        end)
        ~f:(fun original ->
          let round_trip =
            original
            |> Node.to_ast
            |> Astlib.History.convert history
                 ~src_version
                 ~dst_version
                 ~to_ast:(fun x ~version:_ -> Node.to_ast x)
                 ~of_ast:(fun x ~version:_ -> Node.of_ast x)
            |> Astlib.History.convert history
                 ~src_version:dst_version
                 ~dst_version:src_version
                 ~to_ast:(fun x ~version:_ -> Node.to_ast x)
                 ~of_ast:(fun x ~version:_ -> Node.of_ast x)
            |> Node.of_ast
          in
          if not (Node.matches round_trip ~grammar:src_grammar) then
            raise_s
              [%sexp
                "round-trip failed"
              , { src_version : Version.t
                ; dst_version : Version.t
                ; round_trip : Node.t }])));
  [%expect {| |}]
