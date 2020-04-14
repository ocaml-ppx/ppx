open! Stdppx
open Ppx_ast.V4_07

module Kind = struct
  type t = Intf | Impl
end

module Label_map = Map.Make (String)

type t =
  { patt : Ppx_ast.pattern    Ppx_bootstrap.Extension.entry Label_map.t
  ; expr : Ppx_ast.expression Ppx_bootstrap.Extension.entry Label_map.t
  }

module Driver = struct
  let mark_attribute_as_handled_manually = ignore
  let assert_no_attributes = ignore
end

module Metaquot = Ppx_metaquot_expander.Extensions (Driver)

module Ppx_view = struct
  let expr : Ppx_bootstrap.Extension.t =
    Expr
      { name = "view"
      ; callback =
          fun ~loc payload ->
            match Ppx_bootstrap.single_expression_payload payload with
            | Some (expr, attrs) ->
              Driver.assert_no_attributes attrs;
              Ppx_view_lib.Expand.payload ~loc expr
            | _ ->
              Error_ext.exprf ~loc
                "Invalid view payload: expected a single expression"
      }

  let extensions = [expr]
end

let extensions =
  Metaquot.extensions
  @ Ppx_view.extensions

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

let unsupported_extension extension =
  match Extension.to_concrete extension with
  | { loc; txt = name }, _ ->
    let expr = estring ~loc ("unsupported bootstrap extension " ^ name) in
    let item = pstr_eval ~loc expr (Attributes.create []) in
    let payload = Payload.pstr (Structure.create [item]) in
    Extension.create ({ txt = "error"; loc }, payload)
;;

let expression_extension expr =
  match Expression.to_concrete_opt expr with
  | None -> None
  | Some expr ->
    match Expression_desc.to_concrete_opt expr.pexp_desc with
    | Some (Pexp_extension ext) ->
      (match Extension.to_concrete_opt ext with
       | None -> None
       | Some (name_loc, payload) -> Some (name_loc, payload, ext))
    | None | Some _ -> None

let pattern_extension pat =
  match Pattern.to_concrete_opt pat with
  | None -> None
  | Some pat ->
    match Pattern_desc.to_concrete_opt pat.ppat_desc with
    | Some (Ppat_extension ext) ->
      (match Extension.to_concrete_opt ext with
       | None -> None
       | Some (name_loc, payload) -> Some (name_loc, payload, ext))
    | None | Some _ -> None

let rewriter =
  object (self)
    inherit map as super

    method! extension = unsupported_extension

    method! expression expr =
      match expression_extension expr with
      | None -> super#expression expr
      | Some ({ loc; txt }, payload, ext) ->
        (match Label_map.find t.expr txt with
         | Some entry -> self#expression (entry.callback ~loc payload)
         | None -> pexp_extension ~loc (unsupported_extension ext))

    method! pattern pat =
      match pattern_extension pat with
      | None -> super#pattern pat
      | Some ({ loc; txt }, payload, ext) ->
        (match Label_map.find t.patt txt with
         | Some entry -> self#pattern (entry.callback ~loc payload)
         | None -> ppat_extension ~loc (unsupported_extension ext))
  end

let rewrite_ast = function
  | `Intf signature -> `Intf (rewriter#signature signature)
  | `Impl structure -> `Impl (rewriter#structure structure)

let read_source_file filename kind =
  let lexbuf = Lexing.from_channel (open_in filename) in
  lexbuf.lex_curr_p <-
    { pos_fname = filename
    ; pos_lnum  = 1
    ; pos_bol   = 0
    ; pos_cnum  = 0
    };
  match (kind : Kind.t) with
  | Intf -> `Intf (Parse.interface lexbuf |> Ppx_ast.Conversion.ast_of_signature)
  | Impl -> `Impl (Parse.implementation lexbuf |> Ppx_ast.Conversion.ast_of_structure)

let write_target_file filename ast =
  match ast with
  | `Intf signature ->
    Pparse.write_ast Signature filename (Ppx_ast.Conversion.ast_to_signature signature)
  | `Impl structure ->
    Pparse.write_ast Structure filename (Ppx_ast.Conversion.ast_to_structure structure)

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
