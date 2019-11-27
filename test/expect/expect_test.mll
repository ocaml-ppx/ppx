{
open StdLabels

let read_file file =
  let ic = open_in_bin file in
  let len = in_channel_length ic in
  let file_contents = really_input_string ic len in
  close_in ic;
  file_contents

let run_expect_test file ~f =
  let file_contents = read_file file in
  let lexbuf = Lexing.from_string file_contents in
  lexbuf.lex_curr_p <-
    { pos_fname = file
    ; pos_cnum  = 0
    ; pos_lnum  = 1
    ; pos_bol   = 0
    };

  let expected = f file_contents lexbuf in

  let corrected_file = file ^ ".corrected" in
  if file_contents <> expected then begin
    let oc = open_out_bin corrected_file in
    output_string oc expected;
    close_out oc;
  end else begin
    if Sys.file_exists corrected_file then Sys.remove corrected_file;
    exit 0
  end

}

rule code txt start = parse
  | "[%%expect{|\n" {
    let pos = start.Lexing.pos_cnum in
    let len = Lexing.lexeme_start lexbuf - pos in
    let s = String.sub txt ~pos ~len in
    Lexing.new_line lexbuf;
    (start, s) :: expectation txt lexbuf
  }
  | [^'\n']*'\n' {
    Lexing.new_line lexbuf;
    code txt start lexbuf
  }
  | eof {
    let pos = start.Lexing.pos_cnum in
    let len = String.length txt - pos in
    if pos > 0 then begin
      let s = String.sub txt ~pos ~len in
      if String.trim s = "" then
        []
      else
        [(start, s)]
    end else
      []
  }

and expectation txt = parse
  | "|}]\n" {
      Lexing.new_line lexbuf;
      code txt lexbuf.lex_curr_p lexbuf
    }
  | [^'\n']*'\n' {
    Lexing.new_line lexbuf;
    expectation txt lexbuf
  }

{

let print_loc ppf (loc : Location.t) =
  let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  Format.fprintf ppf "Line _";
  if startchar >= 0 then
    Format.fprintf ppf ", characters %d-%d" startchar endchar;
  Format.fprintf ppf ":@.";
;;
let warning_printer loc ppf w =
  match Warnings.report w with
  | `Inactive -> ()
  | `Active { Warnings. number; message; is_error; sub_locs = _ } ->
    print_loc ppf loc;
    if is_error
    then
      Format.fprintf ppf "Error (Warning %d): %s@." number message
    else Format.fprintf ppf "Warning %d: %s@." number message
;;
let rec error_reporter ppf {Location.loc; msg; sub; if_highlight=_} =
  print_loc ppf loc;
  Format.fprintf ppf "Error: %s" msg;
  List.iter sub ~f:(fun err ->
    Format.fprintf ppf "@\n@[<2>%a@]" error_reporter err)
;;
let apply_rewriters : (Parsetree.toplevel_phrase -> Parsetree.toplevel_phrase) = function
  | Ptop_dir _ as x -> x
  | Ptop_def s ->
    let s = Ppx.Selected_ast.of_ocaml Structure s in
    Ptop_def (Ppx.Driver.map_structure s
              |> Migrate_parsetree.Driver.migrate_some_structure
                   (module Migrate_parsetree.OCaml_current))
;;
let main () =
  run_expect_test Sys.argv.(1) ~f:(fun file_contents lexbuf ->
    let chunks = code file_contents lexbuf.lex_curr_p lexbuf in

    Warnings.parse_options false "@a-4-29-40-41-42-44-45-48-58";
    Clflags.real_paths := false;
    Toploop.initialize_toplevel_env ();
    List.iter
      [ "ast_deprecated/.ppx_ast_deprecated.objs"
      ; "src/.ppx.objs"
      ; "metaquot_lifters/.ppx_metaquot_lifters.objs"
      ; "traverse/.ppx_traverse.objs"
      ; "metaquot/.ppx_metaquot.objs"
      ]
      ~f:(fun d -> Topdirs.dir_directory (d ^ "/byte"));

    let buf = Buffer.create (String.length file_contents + 1024) in
    let ppf = Format.formatter_of_buffer buf in
    Location.formatter_for_warnings := ppf;
    Location.warning_printer := warning_printer;
    Location.error_reporter := error_reporter;
    List.iter chunks ~f:(fun (pos, s) ->
      Format.fprintf ppf "%s[%%%%expect{|@." s;
      let lexbuf = Lexing.from_string s in
      lexbuf.lex_curr_p <- { pos with pos_lnum = 1; };
      let phrases = !Toploop.parse_use_file lexbuf in
      List.iter phrases ~f:(fun phr ->
        try
          ignore (Toploop.execute_phrase true ppf (apply_rewriters phr) : bool)
        with exn ->
          Location.report_exception ppf exn
      );
      Format.fprintf ppf "@?|}]@.");
    Buffer.contents buf)

let () =
  try
    main ()
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1
}
