open Base_quickcheck
open Ppx_ast_versioned
open Expect_test_helpers_kernel

(*$ Ppx_ast_versioned_tests_cinaps.print_test_ml () *)
let%expect_test "signature" =
  Test.run_exn
    (module Deriving.Signature)
    ~f:(fun x ->
      require_equal [%here] (module Deriving.Signature) x
        (Conversion.ast_to_signature (Conversion.ast_of_signature x)));
  [%expect {| |}]

let%expect_test "structure" =
  Test.run_exn
    (module Deriving.Structure)
    ~f:(fun x ->
      require_equal [%here] (module Deriving.Structure) x
        (Conversion.ast_to_structure (Conversion.ast_of_structure x)));
  [%expect {| |}]

let%expect_test "toplevel_phrase" =
  Test.run_exn
    (module Deriving.Toplevel_phrase)
    ~f:(fun x ->
      require_equal [%here] (module Deriving.Toplevel_phrase) x
        (Conversion.ast_to_toplevel_phrase (Conversion.ast_of_toplevel_phrase x)));
  [%expect {| |}]
(*$*)
