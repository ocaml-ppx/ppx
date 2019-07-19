open Base
open Astlib_cinaps

let print_parsetree_test_mli () =
  Print.newline ();
  Print.newline ();
  Print.format "(** This signature is deliberately empty. *)";
  Print.newline ()
;;

let print_parsetree_test_ml () =
  Print.newline ();
  Print.format "open! Base";
  Print.format "open Expect_test_helpers_base";
  Astlib_parsetree_types.t
  |> List.filter_map ~f:(fun (name, _) ->
    if List.mem Astlib_parsetree_types.entry_points name ~equal:String.equal
    then Some name
    else None)
  |> List.iter ~f:(fun name ->
    let module_name = "Parsetree_extended." ^ String.capitalize name in
    Print.newline ();
    Print.format "let%%expect_test %S =" name;
    Print.indented (fun () ->
      Print.format "require_does_not_raise [%%here] (fun () ->";
      Print.indented (fun () ->
      Print.format "Base_quickcheck.Test.run_exn";
      Print.indented (fun () ->
        Print.format "(module %s)" module_name;
        Print.format "~f:(fun x ->";
        Print.indented (fun () ->
          Print.format "[%%test_result: %s.t option]" module_name;
          Print.indented (fun () ->
            Print.format "~expect:(Some x)";
            Print.format "(Astlib.Conversions.%s_of_ast" name;
            Print.indented (fun () ->
              (Print.format "(Astlib.Conversions.%s_to_ast x))));" name))))));
      Print.format "[%%expect {| |}]"))
