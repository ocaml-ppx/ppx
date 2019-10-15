let indentation = ref 0

let indented f =
  indentation := !indentation + 2;
  f ();
  indentation := !indentation - 2

let println fmt = Printf.printf ("%*s" ^^ fmt ^^ "\n") !indentation ""

let newline () = print_endline ""
