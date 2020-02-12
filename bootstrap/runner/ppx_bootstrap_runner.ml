open! Stdppx
open Asttypes
open Parsetree

module Kind = struct
  type t = Intf | Impl
end

module Label_map = Map.Make (String)

type t =
  { patt : pattern    Ppx_bootstrap.Extension.entry Label_map.t
  ; expr : expression Ppx_bootstrap.Extension.entry Label_map.t
  }

module Driver = struct
  let mark_attribute_as_handled_manually = ignore
  let assert_no_attributes = ignore
end

module Metaquot = Ppx_metaquot_expander.Extensions (Driver)

(* eventually add ppx_view extensions here *)
let extensions = Metaquot.extensions

let entry_map list =
  Label_map.of_list_map_exn list ~f:(fun (entry : _ Ppx_bootstrap.Extension.entry) ->
    entry.name, entry)

let t =
  let patt, expr =
    List.partition_map extensions ~f:(function
      | Patt x -> Left  x
      | Expr x -> Right x)
  in
  { patt = entry_map patt
  ; expr = entry_map expr
  }

let bootstrap_unsupported_extension
      (_ : Ast_mapper.mapper)
      (({ txt = name; loc }, _) : Parsetree.extension)
  =
  let const = Pconst_string ("unsupported bootstrap extension " ^ name, None) in
  let expr = { pexp_loc = loc; pexp_attributes = []; pexp_desc = Pexp_constant const } in
  let item = { pstr_loc = loc; pstr_desc = Pstr_eval (expr, []) } in
  ({ txt = "error"; loc }, PStr [item])
;;

let bootstrap_expression (mapper : Ast_mapper.mapper) (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_extension (({ txt = name; loc }, payload) as ext) ->
    (match Label_map.find t.expr name with
     | Some entry -> mapper.expr mapper (entry.callback ~loc payload)
     | None ->
       { pexp_desc = Pexp_extension (bootstrap_unsupported_extension mapper ext)
       ; pexp_loc = loc
       ; pexp_attributes = []
       })
  | _ -> Ast_mapper.default_mapper.expr mapper expr

let bootstrap_pattern (mapper : Ast_mapper.mapper) (pat : Parsetree.pattern) =
  match pat.ppat_desc with
  | Ppat_extension (({ txt = name; loc }, payload) as ext) ->
    (match Label_map.find t.patt name with
     | Some entry -> mapper.pat mapper (entry.callback ~loc payload)
     | None ->
       { ppat_desc = Ppat_extension (bootstrap_unsupported_extension mapper ext)
       ; ppat_loc = loc
       ; ppat_attributes = []
       })
  | _ -> Ast_mapper.default_mapper.pat mapper pat

let bootstrap_mapper =
  { Ast_mapper.default_mapper with
    extension = bootstrap_unsupported_extension
  ; expr      = bootstrap_expression
  ; pat       = bootstrap_pattern
  }

let rewrite_ast = function
  | `Intf signature -> `Intf (bootstrap_mapper.signature bootstrap_mapper signature)
  | `Impl structure -> `Impl (bootstrap_mapper.structure bootstrap_mapper structure)

let read_source_file filename kind =
  let lexbuf = Lexing.from_channel (open_in filename) in
  lexbuf.lex_curr_p <-
    { pos_fname = filename
    ; pos_lnum  = 1
    ; pos_bol   = 0
    ; pos_cnum  = 0
    };
  match (kind : Kind.t) with
  | Intf -> `Intf (Parse.interface lexbuf)
  | Impl -> `Impl (Parse.implementation lexbuf)

let write_target_file filename ast =
  match ast with
  | `Intf signature -> Pparse.write_ast Signature filename signature
  | `Impl structure -> Pparse.write_ast Structure filename structure

let rewrite_file ~source_filename ~target_filename ~kind =
  let source_ast = read_source_file source_filename kind in
  let target_ast = rewrite_ast source_ast in
  write_target_file target_filename target_ast

let run () =
  let target_filename = ref "" in
  let kind = ref Kind.Impl in
  let args =
    Arg.align
      [ "--cookie", String ignore,
        "STRING Ignored during bootstrap"
      ; "-o", Set_string target_filename, "FILENAME Output file name."
      ; "--impl", Arg.Unit (fun () -> kind := Impl),
        " Treat the input as an implementation."
      ; "--intf", Arg.Unit (fun () -> kind := Intf),
        " Treat the input as in interface."
      ]
  in
  let anon source_filename =
    rewrite_file ~source_filename ~target_filename:!target_filename ~kind:!kind
  in
  let usage = Printf.sprintf "Usage: %s [OPTIONS]"
                (Filename.basename Sys.executable_name) in
  Arg.parse args anon usage