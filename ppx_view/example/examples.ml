let example_functions = [
  Ex01_basic.run;
  Ex02_combinator.run;
  Ex03_ast_viewer.run;
  Ex04_ident.run;
  Ex05_not.run;
  Ex06_combinator_again.run;
]

let () =
  List.iteri
    (fun i f ->
       Printf.printf "--- example #%d\n%!" (succ i);
       f ())
    example_functions
