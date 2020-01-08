let output_stanzas filename =
  let base = Filename.remove_extension filename in
  Printf.printf
    {|
(library
 (name %s)
 (modules %s)
 (preprocess (pps ppx.ppx_view)))

(rule
 (target %s.actual)
 (deps (:pp bin/pp.exe) (:input %s.ml))
 (action
  (setenv "OCAML_ERROR_STYLE" "short"
   (setenv "OCAML_COLOR" "never"
    (with-stderr-to %%{target}
     (bash "./%%{pp} -no-color --impl %%{input} || true"))))))

(alias
 (name runtest)
 (action (diff %s.expected %s.actual)))
|}
    base
    base
    base
    base
    base
    base

let is_error_test filename =
  Filename.check_suffix filename ".ml"

let () =
  Sys.readdir "."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter is_error_test
  |> List.iter output_stanzas
