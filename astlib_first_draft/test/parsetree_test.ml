(*$ Astlib_first_draft_test_cinaps.print_parsetree_test_ml () *)
open! Base
open Expect_test_helpers_base

let%expect_test "signature" =
  require_does_not_raise [%here] (fun () ->
    Base_quickcheck.Test.run_exn
      (module Parsetree_extended.Signature)
      ~f:(fun x ->
        [%test_result: Parsetree_extended.Signature.t option]
          ~expect:(Some x)
          (Astlib_first_draft.Conversions.signature_of_ast
            (Astlib_first_draft.Conversions.signature_to_ast x))));
  [%expect {| |}]

let%expect_test "structure" =
  require_does_not_raise [%here] (fun () ->
    Base_quickcheck.Test.run_exn
      (module Parsetree_extended.Structure)
      ~f:(fun x ->
        [%test_result: Parsetree_extended.Structure.t option]
          ~expect:(Some x)
          (Astlib_first_draft.Conversions.structure_of_ast
            (Astlib_first_draft.Conversions.structure_to_ast x))));
  [%expect {| |}]

let%expect_test "toplevel_phrase" =
  require_does_not_raise [%here] (fun () ->
    Base_quickcheck.Test.run_exn
      (module Parsetree_extended.Toplevel_phrase)
      ~f:(fun x ->
        [%test_result: Parsetree_extended.Toplevel_phrase.t option]
          ~expect:(Some x)
          (Astlib_first_draft.Conversions.toplevel_phrase_of_ast
            (Astlib_first_draft.Conversions.toplevel_phrase_to_ast x))));
  [%expect {| |}]
(*$*)
