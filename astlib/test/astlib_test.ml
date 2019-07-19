(*$ Astlib_test_cinaps.print_astlib_test_ml () *)
open! Base

module V4_07 = struct
  module Signature = struct
    let%expect_test "V4_07 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V4_07.Signature)
          ~f:(fun x ->
            let y =
              Traversal.V4_07.Signature.copy x
            in
            if not (Ast.equal (Astlib.V4_07.Signature.to_ast x) (Astlib.V4_07.Signature.to_ast y))
            then (
              let x = Ast.sexp_of_t (Astlib.V4_07.Signature.to_ast x) in
              let y = Ast.sexp_of_t (Astlib.V4_07.Signature.to_ast y) in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V4_07 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V4_07.Signature)
          ~f:Completeness.V4_07.Signature.check_exn);
      [%expect {| |}]
  end

  module Structure = struct
    let%expect_test "V4_07 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V4_07.Structure)
          ~f:(fun x ->
            let y =
              Traversal.V4_07.Structure.copy x
            in
            if not (Ast.equal (Astlib.V4_07.Structure.to_ast x) (Astlib.V4_07.Structure.to_ast y))
            then (
              let x = Ast.sexp_of_t (Astlib.V4_07.Structure.to_ast x) in
              let y = Ast.sexp_of_t (Astlib.V4_07.Structure.to_ast y) in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V4_07 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V4_07.Structure)
          ~f:Completeness.V4_07.Structure.check_exn);
      [%expect {| |}]
  end

  module Toplevel_phrase = struct
    let%expect_test "V4_07 copy" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V4_07.Toplevel_phrase)
          ~f:(fun x ->
            let y =
              Traversal.V4_07.Toplevel_phrase.copy x
            in
            if not (Ast.equal (Astlib.V4_07.Toplevel_phrase.to_ast x) (Astlib.V4_07.Toplevel_phrase.to_ast y))
            then (
              let x = Ast.sexp_of_t (Astlib.V4_07.Toplevel_phrase.to_ast x) in
              let y = Ast.sexp_of_t (Astlib.V4_07.Toplevel_phrase.to_ast y) in
              raise_s (Sexp.message "ASTs differ" [("", x); ("", y)]))));
      [%expect {| |}]

    let%expect_test "V4_07 completeness" =
      Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
        Base_quickcheck.Test.run_exn
          (module Generators.V4_07.Toplevel_phrase)
          ~f:Completeness.V4_07.Toplevel_phrase.check_exn);
      [%expect {| |}]
  end
end
(*$*)
