open! Base
open Astlib_base

module Versioned_node = struct
  type t = { version : Version.t; ast : t Ast.t }

  let of_ast ~version ast = { version; ast }

  let rec to_ast ~version:dst_version { version = src_version; ast } ~history =
    let to_ast = to_ast ~history in
    Astlib.History.convert history ast ~src_version ~dst_version ~to_ast ~of_ast
end

module Unversioned_node = struct
  type t = { ast : t Ast.t } [@@deriving equal, sexp_of]

  let of_versioned_node node ~version ~history =
    let rec f node =
      { ast = Ast.map (Versioned_node.to_ast node ~version ~history) ~f }
    in
    f node

  let to_versioned_node t ~version =
    let rec f { ast } = Versioned_node.of_ast ~version (Ast.map ast ~f) in
    f t

  let matches t ~grammar = Ast.matches t.ast ~grammar ~to_ast:(fun { ast } -> ast)

  let generator grammar =
    Base_quickcheck.Generator.map ~f:(fun ast -> { ast })
      (Ast.generator grammar ~of_ast:(fun ast -> { ast }))

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

let v2_of_v1 x ~to_src_ast:_ ~of_dst_ast:_ = x
let v1_of_v2 x ~to_src_ast:_ ~of_dst_ast:_ = x

let rec v3_addends_of_v2 ast ~to_src_ast ~of_dst_ast : _ Ast.data list =
  match (ast : _ Ast.t) with
  | { name = "expr"; data = Variant { tag; args } } ->
    (match tag with
     | "var" | "int" -> [ Node (of_dst_ast ast) ]
     | "add" ->
       (match args with
        | [| Node x; Node y |] ->
          v3_addends_of_v2 (to_src_ast x) ~to_src_ast ~of_dst_ast @
          v3_addends_of_v2 (to_src_ast y) ~to_src_ast ~of_dst_ast
        | [| List _ |] -> assert false
        | _ -> assert false)
     | _ -> assert false)
  | _ -> assert false

let v3_of_v2 ast ~to_src_ast ~of_dst_ast =
  match v3_addends_of_v2 ast ~to_src_ast ~of_dst_ast with
  | [ _ ] -> ast
  | list -> { name = "expr" ; data = Variant { tag = "add" ; args = [| List list |] } }

let v2_zero () : _ Ast.t =
  { name = "expr"
  ; data = Variant { tag = "int"; args = [| Int 0 |] }
  }

let v2_add x y : _ Ast.t =
  { name = "expr"
  ; data =
      Variant
        { tag = "add"
        ; args = [| Node x; Node y |]
        }
  }

let v2_of_v3 ast ~to_src_ast:_ ~of_dst_ast =
  match (ast : _ Ast.t) with
  | { name = "expr"; data = Variant { tag; args } } ->
    (match tag with
     | "var" | "int" -> ast
     | "add" ->
       (match args with
        | [| List data |] ->
          let nodes =
            List.map data ~f:(function
              | Node node -> node
              | _ -> assert false)
          in
          List.fold_right nodes ~init:(v2_zero ()) ~f:(fun node acc ->
            v2_add node (of_dst_ast acc))
        | [| Node _; Node _ |] -> assert false
        | _ -> assert false)
     | _ -> assert false)
  | _ -> assert false

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
        type t = Unversioned_node.t
        let sexp_of_t = Unversioned_node.sexp_of_t
        let quickcheck_generator = Unversioned_node.generator grammar
        let quickcheck_shrinker = Unversioned_node.shrinker
      end)
      ~f:(fun node ->
        if not (Unversioned_node.matches node ~grammar)
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
            type t = Unversioned_node.t
            let sexp_of_t = Unversioned_node.sexp_of_t
            let quickcheck_shrinker = Unversioned_node.shrinker
            let quickcheck_generator = Unversioned_node.generator src_grammar
          end)
          ~f:(fun src ->
            let dst =
              src
              |> Unversioned_node.to_versioned_node ~version:src_version
              |> Unversioned_node.of_versioned_node ~version:dst_version ~history
            in
            if not (Unversioned_node.matches dst ~grammar:dst_grammar)
            then
              raise_s
                [%sexp
                  "invalid conversion"
                , { src_version : Version.t
                  ; dst_version : Version.t
                  ; dst : Unversioned_node.t
                  }])));
  [%expect {| |}]

(* Round-trips aren't guaranteed to end up at the same AST, case in point, here a tree of
   addends round-tripping through the list version might come back to a
   differently-balanced tree. But even with down-conversions that can fail, one should
   always be able to get back to the original grammar. *)
let%expect_test "history round-trips always work" =
  List.iter versioned_grammars ~f:(fun (src_version, src_grammar) ->
    List.iter versioned_grammars ~f:(fun (dst_version, _) ->
      Base_quickcheck.Test.run_exn
        (module struct
          type t = Unversioned_node.t
          let sexp_of_t = Unversioned_node.sexp_of_t
          let quickcheck_generator = Unversioned_node.generator src_grammar
          let quickcheck_shrinker = Unversioned_node.shrinker
        end)
        ~f:(fun original ->
          let converted =
            try
              original
              |> Unversioned_node.to_versioned_node ~version:src_version
              |> Unversioned_node.of_versioned_node ~version:dst_version ~history
            with exn ->
              raise_s
                [%sexp
                  "first conversion failed"
                , { src_version : Version.t
                  ; dst_version : Version.t
                  ; exn : exn
                  }]
          in
          let round_trip =
            try
              converted
              |> Unversioned_node.to_versioned_node ~version:dst_version
              |> Unversioned_node.of_versioned_node ~version:src_version ~history
            with exn ->
              raise_s
                [%sexp
                  "second conversion failed"
                , { src_version : Version.t
                  ; dst_version : Version.t
                  ; converted : Unversioned_node.t
                  ; exn : exn
                  }]
          in
          if
            not
              (Unversioned_node.matches round_trip ~grammar:src_grammar)
          then
            raise_s
              [%sexp
                "round-trip failed"
              , { src_version : Version.t
                ; dst_version : Version.t
                ; converted : Unversioned_node.t
                ; round_trip : Unversioned_node.t
                }])));
  [%expect {| |}]
