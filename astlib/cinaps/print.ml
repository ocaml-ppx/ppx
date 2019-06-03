open StdLabels

let indentation_depth = ref 0

let indented ?(levels=1) f =
  indentation_depth := !indentation_depth + levels;
  let result = f () in
  indentation_depth := !indentation_depth - levels;
  result

let print string =
  let indentation = 2 * !indentation_depth in
  String.split_on_char string ~sep:'\n'
  |> List.iter ~f:(fun line ->
    Printf.printf "%*s%s\n" indentation "" line)

let format fmt = Printf.ksprintf print fmt

let newline () = print_endline ""

let declare_module name f =
  format "module %s : sig" name;
  indented f;
  format "end"

let define_module name f =
  format "module %s = struct" name;
  indented f;
  format "end"

let declare_recursive_modules alist =
  List.iteri alist ~f:(fun index (name, f) ->
    if index = 0
    then (
      format "module rec %s : sig" name;
      indented f;
      format "end")
    else (
      newline ();
      format "and %s : sig" name;
      indented f;
      format "end"))

let define_recursive_values alist =
  List.iteri alist ~f:(fun index (header, f) ->
    if index = 0
    then (
      format "let rec %s" header;
      indented f)
    else (
      newline ();
      format "and %s" header;
      indented f))
