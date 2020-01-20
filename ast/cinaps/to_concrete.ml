let define_conversion_failed ~version =
  Print.newline ();
  Print.println "let conversion_failed name =";
  Print.indented (fun () ->
    Print.println
      {|let msg = Printf.sprintf "Ppx_ast: Could not convert %%s to %s" name in|}
      version;
    Print.println "failwith msg")

let print_to_concrete_exn ~targs ~node_name expr =
  Print.println "let concrete =";
  Print.indented (fun () ->
    let to_concrete = Name.make ["to_concrete"] targs in
    Print.println "match %s.%s %s with"
      (Ml.module_name node_name)
      to_concrete
      expr;
    Print.println "| None -> conversion_failed %S" node_name;
    Print.println "| Some n -> n");
  Print.println "in"
