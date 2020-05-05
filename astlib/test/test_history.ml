open! Base
open Astlib_base

module Versioned_node = struct
  type t = { version : Version.t; ast : t Ast.t } [@@deriving sexp_of]

  let wrap ~version ast = { version; ast }

  let rec convert { version = src_version; ast } ~version:dst_version ~history =
    let version, ast =
      let unwrap = unwrap ~history in
      Astlib.History.convert history ast ~src_version ~dst_version ~unwrap ~wrap
    in
    { version; ast }

  and unwrap ~history ~version t =
    let t = convert t ~version ~history in
    if Version.equal t.version version
    then Some t.ast
    else None

  let rec deep_convert t ~version ~history =
    let t = convert t ~version ~history in
    if Version.equal t.version version
    then { t with ast = Ast.map t.ast ~f:(deep_convert ~version ~history) }
    else t
end

module Unversioned_node = struct
  type t = { ast : t Ast.t } [@@deriving equal, sexp_of]

  let of_versioned_node node ~version ~history =
    let rec f node =
      node
      |> Versioned_node.unwrap ~version ~history
      |> Option.bind ~f:(Ast.Optional.map ~f)
      |> Option.map ~f:(fun ast -> { ast })
    in
    f node

  let rec of_versioned_node_without_converting (node : Versioned_node.t) =
    { ast = Ast.map node.ast ~f:of_versioned_node_without_converting }

  let to_versioned_node t ~version =
    let rec f { ast } = Versioned_node.wrap ~version (Ast.map ast ~f) in
    f t

  let matches t ~grammar = Ast.matches t.ast ~grammar ~unwrap:(fun { ast } -> ast)

  let generator grammar =
    Base_quickcheck.Generator.map ~f:(fun ast -> { ast })
      (Ast.generator grammar ~wrap:(fun ast -> { ast }))

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

let v2_of_v1 x ~unwrap:_ ~wrap:_ = Some x

let v1_of_v2 ast ~unwrap:_ ~wrap:_ =
  match (ast : _ Ast.t) with
  | { name = "expr"; data = Variant { tag; args = _ } } ->
    (match tag with
     | "int" | "add" -> Some ast
     | "var" -> None
     | _ -> assert false)
  | _ -> assert false

let rec v3_addends_of_v2 ast ~unwrap ~wrap : _ Ast.data list option =
  match (ast : _ Ast.t) with
  | { name = "expr"; data = Variant { tag; args } } ->
    (match tag with
     | "var" | "int" -> Some [ Node (wrap ast) ]
     | "add" ->
       (match args with
        | [| Node x; Node y |] ->
          Option.bind (Option.both (unwrap x) (unwrap y)) ~f:(fun (x, y) ->
            Option.map
              (Option.both
                 (v3_addends_of_v2 x ~unwrap ~wrap)
                 (v3_addends_of_v2 y ~unwrap ~wrap))
              ~f:(fun (x, y) -> x @ y))
        | [| List _ |] -> assert false
        | _ -> assert false)
     | _ -> assert false)
  | _ -> assert false

let v3_of_v2 ast ~unwrap ~wrap =
  Option.map (v3_addends_of_v2 ast ~unwrap ~wrap) ~f:(function
    | [ _ ] -> ast
    | list -> { name = "expr" ; data = Variant { tag = "add" ; args = [| List list |] } })

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

let v2_of_v3 ast ~unwrap:_ ~wrap =
  match (ast : _ Ast.t) with
  | { name = "expr"; data = Variant { tag; args } } ->
    (match tag with
     | "var" | "int" -> Some ast
     | "add" ->
       (match args with
        | [| List data |] ->
          let nodes =
            List.map data ~f:(function
              | Node node -> node
              | _ -> assert false)
          in
          List.fold_right nodes ~init:(v2_zero ()) ~f:(fun node acc ->
            v2_add node (wrap acc))
          |> Option.return
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

let%expect_test "deep-convert consistency" =
  List.iter versioned_grammars ~f:(fun (src_version, src_grammar) ->
    List.iter versioned_grammars ~f:(fun (dst_version, _) ->
      Base_quickcheck.Test.run_exn
        (module struct
          type t = Unversioned_node.t
          let sexp_of_t = Unversioned_node.sexp_of_t
          let quickcheck_generator = Unversioned_node.generator src_grammar
          let quickcheck_shrinker = Unversioned_node.shrinker
        end)
        ~f:(fun src ->
          let src = Unversioned_node.to_versioned_node src ~version:src_version in
          match
            Unversioned_node.of_versioned_node src ~version:dst_version ~history
          with
          | None -> ()
          | Some dst ->
            let dst_via_deep_convert =
              src
              |> Versioned_node.deep_convert ~version:dst_version ~history
              |> Unversioned_node.of_versioned_node_without_converting
            in
            if not (Unversioned_node.equal dst dst_via_deep_convert)
            then
              raise_s
                [%sexp
                  "deep convert failed"
                , { src_version : Version.t
                  ; dst_version : Version.t
                  ; dst : Unversioned_node.t
                  ; dst_via_deep_convert : Unversioned_node.t
                  }])));
  [%expect {| |}]

let%expect_test "history down-conversions either work or else fail gracefully" =
  List.iteri versioned_grammars ~f:(fun src_index (src_version, src_grammar) ->
    List.iteri versioned_grammars ~f:(fun dst_index (dst_version, dst_grammar) ->
      if dst_index < src_index then
        Base_quickcheck.Test.run_exn
          (module struct
            type t = Unversioned_node.t
            let sexp_of_t = Unversioned_node.sexp_of_t
            let quickcheck_shrinker = Unversioned_node.shrinker
            let quickcheck_generator = Unversioned_node.generator src_grammar
          end)
          ~f:(fun src ->
            match
              src
              |> Unversioned_node.to_versioned_node ~version:src_version
              |> Unversioned_node.of_versioned_node ~version:dst_version ~history
            with
            | exception exn ->
              raise_s
                [%sexp
                  "conversion raised"
                , { src_version : Version.t; dst_version : Version.t; exn : exn }]
            | None -> ()
            | Some dst ->
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
              match
                src
                |> Unversioned_node.to_versioned_node ~version:src_version
                |> Unversioned_node.of_versioned_node ~version:dst_version ~history
              with
              | Some dst -> dst
              | None ->
                raise_s
                  [%sexp
                    "conversion failed"
                  , { src_version : Version.t; dst_version : Version.t }]
              | exception exn ->
                raise_s
                  [%sexp
                    "conversion raised"
                  , { src_version : Version.t; dst_version : Version.t; exn : exn }]
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
            match
              original
              |> Unversioned_node.to_versioned_node ~version:src_version
              |> Versioned_node.deep_convert ~version:dst_version ~history
            with
            | converted -> converted
            | exception exn ->
              raise_s
                [%sexp
                  "first conversion raised"
                , { src_version : Version.t
                  ; dst_version : Version.t
                  ; exn : exn
                  }]
          in
          let round_trip =
            match
              converted
              |> Unversioned_node.of_versioned_node ~version:src_version ~history
            with
            | Some round_trip -> round_trip
            | None ->
              raise_s
                [%sexp
                  "second conversion failed"
                , { src_version : Version.t
                  ; dst_version : Version.t
                  ; converted : Versioned_node.t
                  }]
            | exception exn ->
              raise_s
                [%sexp
                  "second conversion raised"
                , { src_version : Version.t
                  ; dst_version : Version.t
                  ; converted : Versioned_node.t
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
                ; converted : Versioned_node.t
                ; round_trip : Unversioned_node.t
                }])));
  [%expect {| |}]
