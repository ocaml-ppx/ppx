open Base_quickcheck
open Ppx_ast
open Expect_test_helpers_kernel

(*$ Ppx_ast_tests_cinaps.print_test_ml () *)
let config = { Test.default_config with test_count = 1_000 }

module Unstable_for_testing = struct
  let%expect_test "toplevel_phrase" =
    Test.run_exn ~config
      (module Deriving.Toplevel_phrase)
      ~f:(fun x ->
        require_equal [%here] (module Deriving.Toplevel_phrase) x
          (Conversion.ast_to_toplevel_phrase
            ((new Unstable_for_testing.map)#toplevel_phrase
              (Conversion.ast_of_toplevel_phrase x))));
    [%expect {| |}]

  let%expect_test "structure" =
    Test.run_exn ~config
      (module Deriving.Structure)
      ~f:(fun x ->
        require_equal [%here] (module Deriving.Structure) x
          (Conversion.ast_to_structure
            ((new Unstable_for_testing.map)#structure
              (Conversion.ast_of_structure x))));
    [%expect {| |}]

  let%expect_test "signature" =
    Test.run_exn ~config
      (module Deriving.Signature)
      ~f:(fun x ->
        require_equal [%here] (module Deriving.Signature) x
          (Conversion.ast_to_signature
            ((new Unstable_for_testing.map)#signature
              (Conversion.ast_of_signature x))));
    [%expect {| |}]
end

module V4_07 = struct
  let%expect_test "signature" =
    Test.run_exn ~config
      (module Deriving.Signature)
      ~f:(fun x ->
        require_equal [%here] (module Deriving.Signature) x
          (Conversion.ast_to_signature
            ((new V4_07.map)#signature
              (Conversion.ast_of_signature x))));
    [%expect {| |}]

  let%expect_test "structure" =
    Test.run_exn ~config
      (module Deriving.Structure)
      ~f:(fun x ->
        require_equal [%here] (module Deriving.Structure) x
          (Conversion.ast_to_structure
            ((new V4_07.map)#structure
              (Conversion.ast_of_structure x))));
    [%expect {| |}]

  let%expect_test "toplevel_phrase" =
    Test.run_exn ~config
      (module Deriving.Toplevel_phrase)
      ~f:(fun x ->
        require_equal [%here] (module Deriving.Toplevel_phrase) x
          (Conversion.ast_to_toplevel_phrase
            ((new V4_07.map)#toplevel_phrase
              (Conversion.ast_of_toplevel_phrase x))));
    [%expect {| |}]
end
(*$*)
