(*$ Astlib_cinaps.print_astlib_test_ml () *)
open Base

module V1 = struct
  module Expression = struct
    let%expect_test "V1 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Expression)
          ~f:(fun x ->
            let y =
              Traversal.V1.Expression.copy x
            in
            if not (Astlib.V1.Expression.equal x y)
            then (
              let x = Astlib.V1.Expression.sexp_of_t x in
              let y = Astlib.V1.Expression.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V1 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Expression)
          ~f:Completeness.V1.Expression.check_exn);
      [%expect {| |}]

    let%expect_test "V2 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Expression)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V1.Expression.to_ast
              |> Astlib.V2.Expression.of_ast
              |> Traversal.V2.Expression.copy
              |> Astlib.V2.Expression.to_ast
              |> Astlib.V1.Expression.of_ast
              |> Traversal.V1.Expression.copy
            in
            if not (Astlib.V1.Expression.equal x y)
            then (
              let x = Astlib.V1.Expression.sexp_of_t x in
              let y = Astlib.V1.Expression.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V2 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Expression)
          ~f:(fun x ->
            x
            |> Astlib.V1.Expression.to_ast
            |> Astlib.V2.Expression.of_ast
            |> Traversal.V2.Expression.copy
            |> Completeness.V2.Expression.check_exn));
      [%expect {| |}]

    let%expect_test "V3 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Expression)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V1.Expression.to_ast
              |> Astlib.V3.Expression.of_ast
              |> Traversal.V3.Expression.copy
              |> Astlib.V3.Expression.to_ast
              |> Astlib.V1.Expression.of_ast
              |> Traversal.V1.Expression.copy
            in
            if not (Astlib.V1.Expression.equal x y)
            then (
              let x = Astlib.V1.Expression.sexp_of_t x in
              let y = Astlib.V1.Expression.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V3 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Expression)
          ~f:(fun x ->
            x
            |> Astlib.V1.Expression.to_ast
            |> Astlib.V3.Expression.of_ast
            |> Traversal.V3.Expression.copy
            |> Completeness.V3.Expression.check_exn));
      [%expect {| |}]
  end

  module Case = struct
    let%expect_test "V1 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Case)
          ~f:(fun x ->
            let y =
              Traversal.V1.Case.copy x
            in
            if not (Astlib.V1.Case.equal x y)
            then (
              let x = Astlib.V1.Case.sexp_of_t x in
              let y = Astlib.V1.Case.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V1 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Case)
          ~f:Completeness.V1.Case.check_exn);
      [%expect {| |}]

    let%expect_test "V2 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Case)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V1.Case.to_ast
              |> Astlib.V2.Case.of_ast
              |> Traversal.V2.Case.copy
              |> Astlib.V2.Case.to_ast
              |> Astlib.V1.Case.of_ast
              |> Traversal.V1.Case.copy
            in
            if not (Astlib.V1.Case.equal x y)
            then (
              let x = Astlib.V1.Case.sexp_of_t x in
              let y = Astlib.V1.Case.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V2 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Case)
          ~f:(fun x ->
            x
            |> Astlib.V1.Case.to_ast
            |> Astlib.V2.Case.of_ast
            |> Traversal.V2.Case.copy
            |> Completeness.V2.Case.check_exn));
      [%expect {| |}]

    let%expect_test "V3 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Case)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V1.Case.to_ast
              |> Astlib.V3.Case.of_ast
              |> Traversal.V3.Case.copy
              |> Astlib.V3.Case.to_ast
              |> Astlib.V1.Case.of_ast
              |> Traversal.V1.Case.copy
            in
            if not (Astlib.V1.Case.equal x y)
            then (
              let x = Astlib.V1.Case.sexp_of_t x in
              let y = Astlib.V1.Case.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V3 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Case)
          ~f:(fun x ->
            x
            |> Astlib.V1.Case.to_ast
            |> Astlib.V3.Case.of_ast
            |> Traversal.V3.Case.copy
            |> Completeness.V3.Case.check_exn));
      [%expect {| |}]
  end

  module Pattern = struct
    let%expect_test "V1 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Pattern)
          ~f:(fun x ->
            let y =
              Traversal.V1.Pattern.copy x
            in
            if not (Astlib.V1.Pattern.equal x y)
            then (
              let x = Astlib.V1.Pattern.sexp_of_t x in
              let y = Astlib.V1.Pattern.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V1 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Pattern)
          ~f:Completeness.V1.Pattern.check_exn);
      [%expect {| |}]

    let%expect_test "V2 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Pattern)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V1.Pattern.to_ast
              |> Astlib.V2.Pattern.of_ast
              |> Traversal.V2.Pattern.copy
              |> Astlib.V2.Pattern.to_ast
              |> Astlib.V1.Pattern.of_ast
              |> Traversal.V1.Pattern.copy
            in
            if not (Astlib.V1.Pattern.equal x y)
            then (
              let x = Astlib.V1.Pattern.sexp_of_t x in
              let y = Astlib.V1.Pattern.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V2 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Pattern)
          ~f:(fun x ->
            x
            |> Astlib.V1.Pattern.to_ast
            |> Astlib.V2.Pattern.of_ast
            |> Traversal.V2.Pattern.copy
            |> Completeness.V2.Pattern.check_exn));
      [%expect {| |}]

    let%expect_test "V3 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Pattern)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V1.Pattern.to_ast
              |> Astlib.V3.Pattern.of_ast
              |> Traversal.V3.Pattern.copy
              |> Astlib.V3.Pattern.to_ast
              |> Astlib.V1.Pattern.of_ast
              |> Traversal.V1.Pattern.copy
            in
            if not (Astlib.V1.Pattern.equal x y)
            then (
              let x = Astlib.V1.Pattern.sexp_of_t x in
              let y = Astlib.V1.Pattern.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V3 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V1.Pattern)
          ~f:(fun x ->
            x
            |> Astlib.V1.Pattern.to_ast
            |> Astlib.V3.Pattern.of_ast
            |> Traversal.V3.Pattern.copy
            |> Completeness.V3.Pattern.check_exn));
      [%expect {| |}]
  end
end

module V2 = struct
  module Expression = struct
    let%expect_test "V1 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V2.Expression)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V2.Expression.to_ast
              |> Astlib.V1.Expression.of_ast
              |> Traversal.V1.Expression.copy
              |> Astlib.V1.Expression.to_ast
              |> Astlib.V2.Expression.of_ast
              |> Traversal.V2.Expression.copy
            in
            if not (Astlib.V2.Expression.equal x y)
            then (
              let x = Astlib.V2.Expression.sexp_of_t x in
              let y = Astlib.V2.Expression.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V2 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V2.Expression)
          ~f:(fun x ->
            let y =
              Traversal.V2.Expression.copy x
            in
            if not (Astlib.V2.Expression.equal x y)
            then (
              let x = Astlib.V2.Expression.sexp_of_t x in
              let y = Astlib.V2.Expression.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V2 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V2.Expression)
          ~f:Completeness.V2.Expression.check_exn);
      [%expect {| |}]

    let%expect_test "V3 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V2.Expression)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V2.Expression.to_ast
              |> Astlib.V3.Expression.of_ast
              |> Traversal.V3.Expression.copy
              |> Astlib.V3.Expression.to_ast
              |> Astlib.V2.Expression.of_ast
              |> Traversal.V2.Expression.copy
            in
            if not (Astlib.V2.Expression.equal x y)
            then (
              let x = Astlib.V2.Expression.sexp_of_t x in
              let y = Astlib.V2.Expression.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V3 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V2.Expression)
          ~f:(fun x ->
            x
            |> Astlib.V2.Expression.to_ast
            |> Astlib.V3.Expression.of_ast
            |> Traversal.V3.Expression.copy
            |> Completeness.V3.Expression.check_exn));
      [%expect {| |}]
  end

  module Case = struct
    let%expect_test "V1 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V2.Case)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V2.Case.to_ast
              |> Astlib.V1.Case.of_ast
              |> Traversal.V1.Case.copy
              |> Astlib.V1.Case.to_ast
              |> Astlib.V2.Case.of_ast
              |> Traversal.V2.Case.copy
            in
            if not (Astlib.V2.Case.equal x y)
            then (
              let x = Astlib.V2.Case.sexp_of_t x in
              let y = Astlib.V2.Case.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V2 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V2.Case)
          ~f:(fun x ->
            let y =
              Traversal.V2.Case.copy x
            in
            if not (Astlib.V2.Case.equal x y)
            then (
              let x = Astlib.V2.Case.sexp_of_t x in
              let y = Astlib.V2.Case.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V2 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V2.Case)
          ~f:Completeness.V2.Case.check_exn);
      [%expect {| |}]

    let%expect_test "V3 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V2.Case)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V2.Case.to_ast
              |> Astlib.V3.Case.of_ast
              |> Traversal.V3.Case.copy
              |> Astlib.V3.Case.to_ast
              |> Astlib.V2.Case.of_ast
              |> Traversal.V2.Case.copy
            in
            if not (Astlib.V2.Case.equal x y)
            then (
              let x = Astlib.V2.Case.sexp_of_t x in
              let y = Astlib.V2.Case.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V3 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V2.Case)
          ~f:(fun x ->
            x
            |> Astlib.V2.Case.to_ast
            |> Astlib.V3.Case.of_ast
            |> Traversal.V3.Case.copy
            |> Completeness.V3.Case.check_exn));
      [%expect {| |}]
  end

  module Pattern = struct
    let%expect_test "V1 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V2.Pattern)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V2.Pattern.to_ast
              |> Astlib.V1.Pattern.of_ast
              |> Traversal.V1.Pattern.copy
              |> Astlib.V1.Pattern.to_ast
              |> Astlib.V2.Pattern.of_ast
              |> Traversal.V2.Pattern.copy
            in
            if not (Astlib.V2.Pattern.equal x y)
            then (
              let x = Astlib.V2.Pattern.sexp_of_t x in
              let y = Astlib.V2.Pattern.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V2 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V2.Pattern)
          ~f:(fun x ->
            let y =
              Traversal.V2.Pattern.copy x
            in
            if not (Astlib.V2.Pattern.equal x y)
            then (
              let x = Astlib.V2.Pattern.sexp_of_t x in
              let y = Astlib.V2.Pattern.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V2 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V2.Pattern)
          ~f:Completeness.V2.Pattern.check_exn);
      [%expect {| |}]

    let%expect_test "V3 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V2.Pattern)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V2.Pattern.to_ast
              |> Astlib.V3.Pattern.of_ast
              |> Traversal.V3.Pattern.copy
              |> Astlib.V3.Pattern.to_ast
              |> Astlib.V2.Pattern.of_ast
              |> Traversal.V2.Pattern.copy
            in
            if not (Astlib.V2.Pattern.equal x y)
            then (
              let x = Astlib.V2.Pattern.sexp_of_t x in
              let y = Astlib.V2.Pattern.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V3 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V2.Pattern)
          ~f:(fun x ->
            x
            |> Astlib.V2.Pattern.to_ast
            |> Astlib.V3.Pattern.of_ast
            |> Traversal.V3.Pattern.copy
            |> Completeness.V3.Pattern.check_exn));
      [%expect {| |}]
  end
end

module V3 = struct
  module Identifier = struct
    let%expect_test "V3 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V3.Identifier)
          ~f:(fun x ->
            let y =
              Traversal.V3.Identifier.copy x
            in
            if not (Astlib.V3.Identifier.equal x y)
            then (
              let x = Astlib.V3.Identifier.sexp_of_t x in
              let y = Astlib.V3.Identifier.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V3 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V3.Identifier)
          ~f:Completeness.V3.Identifier.check_exn);
      [%expect {| |}]
  end

  module Expression = struct
    let%expect_test "V1 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V3.Expression)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V3.Expression.to_ast
              |> Astlib.V1.Expression.of_ast
              |> Traversal.V1.Expression.copy
              |> Astlib.V1.Expression.to_ast
              |> Astlib.V3.Expression.of_ast
              |> Traversal.V3.Expression.copy
            in
            if not (Astlib.V3.Expression.equal x y)
            then (
              let x = Astlib.V3.Expression.sexp_of_t x in
              let y = Astlib.V3.Expression.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V2 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V3.Expression)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V3.Expression.to_ast
              |> Astlib.V2.Expression.of_ast
              |> Traversal.V2.Expression.copy
              |> Astlib.V2.Expression.to_ast
              |> Astlib.V3.Expression.of_ast
              |> Traversal.V3.Expression.copy
            in
            if not (Astlib.V3.Expression.equal x y)
            then (
              let x = Astlib.V3.Expression.sexp_of_t x in
              let y = Astlib.V3.Expression.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V3 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V3.Expression)
          ~f:(fun x ->
            let y =
              Traversal.V3.Expression.copy x
            in
            if not (Astlib.V3.Expression.equal x y)
            then (
              let x = Astlib.V3.Expression.sexp_of_t x in
              let y = Astlib.V3.Expression.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V3 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V3.Expression)
          ~f:Completeness.V3.Expression.check_exn);
      [%expect {| |}]
  end

  module Case = struct
    let%expect_test "V1 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V3.Case)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V3.Case.to_ast
              |> Astlib.V1.Case.of_ast
              |> Traversal.V1.Case.copy
              |> Astlib.V1.Case.to_ast
              |> Astlib.V3.Case.of_ast
              |> Traversal.V3.Case.copy
            in
            if not (Astlib.V3.Case.equal x y)
            then (
              let x = Astlib.V3.Case.sexp_of_t x in
              let y = Astlib.V3.Case.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V2 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V3.Case)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V3.Case.to_ast
              |> Astlib.V2.Case.of_ast
              |> Traversal.V2.Case.copy
              |> Astlib.V2.Case.to_ast
              |> Astlib.V3.Case.of_ast
              |> Traversal.V3.Case.copy
            in
            if not (Astlib.V3.Case.equal x y)
            then (
              let x = Astlib.V3.Case.sexp_of_t x in
              let y = Astlib.V3.Case.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V3 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V3.Case)
          ~f:(fun x ->
            let y =
              Traversal.V3.Case.copy x
            in
            if not (Astlib.V3.Case.equal x y)
            then (
              let x = Astlib.V3.Case.sexp_of_t x in
              let y = Astlib.V3.Case.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V3 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V3.Case)
          ~f:Completeness.V3.Case.check_exn);
      [%expect {| |}]
  end

  module Pattern = struct
    let%expect_test "V1 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V3.Pattern)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V3.Pattern.to_ast
              |> Astlib.V1.Pattern.of_ast
              |> Traversal.V1.Pattern.copy
              |> Astlib.V1.Pattern.to_ast
              |> Astlib.V3.Pattern.of_ast
              |> Traversal.V3.Pattern.copy
            in
            if not (Astlib.V3.Pattern.equal x y)
            then (
              let x = Astlib.V3.Pattern.sexp_of_t x in
              let y = Astlib.V3.Pattern.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V2 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V3.Pattern)
          ~f:(fun x ->
            let y =
              x
              |> Astlib.V3.Pattern.to_ast
              |> Astlib.V2.Pattern.of_ast
              |> Traversal.V2.Pattern.copy
              |> Astlib.V2.Pattern.to_ast
              |> Astlib.V3.Pattern.of_ast
              |> Traversal.V3.Pattern.copy
            in
            if not (Astlib.V3.Pattern.equal x y)
            then (
              let x = Astlib.V3.Pattern.sexp_of_t x in
              let y = Astlib.V3.Pattern.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V3 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V3.Pattern)
          ~f:(fun x ->
            let y =
              Traversal.V3.Pattern.copy x
            in
            if not (Astlib.V3.Pattern.equal x y)
            then (
              let x = Astlib.V3.Pattern.sexp_of_t x in
              let y = Astlib.V3.Pattern.sexp_of_t y in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V3 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V3.Pattern)
          ~f:Completeness.V3.Pattern.check_exn);
      [%expect {| |}]
  end
end
(*$*)
