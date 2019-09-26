let data_path = ref "."

let dest_path = ref "."

let ocaml_where_path = ref ""

let verbose = ref false

let switches = [
  "-data-path",        Arg.Set_string data_path,        "Set data path";
  "-dest-path",        Arg.Set_string dest_path,        "Set destination path";
  "-ocaml-where-path", Arg.Set_string ocaml_where_path, "Set OCaml path";
  "-verbose",          Arg.Set verbose,                 "Enable verbose output";
]

let anon_fun s =
  raise (Arg.Bad s)

let usage =
  Printf.sprintf "Usage: %s <options>" Sys.argv.(0)

let parse () =
  Arg.parse switches anon_fun usage
