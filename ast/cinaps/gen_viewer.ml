open Stdppx

let string_of_ty ty = Grammar.string_of_ty ~internal:false ty

module type VIEWER_PRINTER = sig
  val print_field_viewer :
    targs: string list ->
    name: string ->
    (string * Astlib.Grammar.ty) ->
    unit

  val print_variant_viewer :
    name: string ->
    shortcut: Shortcut.t option ->
    (string * Astlib.Grammar.clause) ->
    unit
end

module Structure : VIEWER_PRINTER = struct
  let print_to_concrete ~shortcut node_name expr =
    match (shortcut : Shortcut.t option) with
    | None ->
      To_concrete.print_to_concrete_exn ~node_name expr
    | Some {outer_record; desc_field; _} ->
      To_concrete.print_to_concrete_exn
        ~var_name:"parent_concrete" ~node_name:outer_record expr;
      Print.println "let desc = parent_concrete.%s.%s in"
        (Ml.module_name outer_record) desc_field;
      To_concrete.print_to_concrete_exn ~node_name "desc"

  let tuple tyl =
    match tyl with
    | [_] -> "arg"
    | _ ->
      let vars = List.mapi tyl ~f:(fun i _ -> Printf.sprintf "arg%d" i) in
      Ml.tuple vars

  let print_field_viewer ~targs:_ ~name (fname, _ty) =
    Print.newline ();
    Print.println "let %s'match view value =" (Ml.id fname);
    Print.indented (fun () ->
      print_to_concrete ~shortcut:None name "value";
      Print.println "view concrete.%s.%s" (Ml.module_name name) (Ml.id fname))

  let print_variant_viewer ~name ~shortcut (cname, clause) =
    match (clause : Astlib.Grammar.clause) with
    | Empty ->
      Print.newline ();
      Print.println "let %s value =" (Ml.id cname);
      Print.indented (fun () ->
        print_to_concrete ~shortcut name "value";
        Print.println "match concrete with";
        Print.println "| %s.%s -> View.ok" (Ml.module_name name) (Ml.tag cname);
        Print.println "| _ -> View.error")
    | Tuple tyl ->
      Print.newline ();
      Print.println "let %s view value =" (Ml.id cname);
      Print.indented (fun () ->
        let args = tuple tyl in
        print_to_concrete ~shortcut name "value";
        Print.println "match concrete with";
        Print.println "| %s.%s %s -> view %s"
          (Ml.module_name name) (Ml.tag cname) args args;
        Print.println "| _ -> View.error")
    | Record _fields ->
      (* There are no inline records atm in the AST so it's okay to skip this.
         If some were added in the future, we'd need either:
         1. no sharing of field names accross different variant types and within
         a variant type, fields with the same name also have the same type.
         2. some deeper changes to ppx_view *)
      assert false
end

module Signature : VIEWER_PRINTER = struct
  let view_t ty ~in_ ~out =
    Printf.sprintf "(%s, %s, %s) View.t" ty in_ out

  let t_type node_name = string_of_ty (Name node_name)

  let value_type ~targs ~shortcut name =
    match targs, (shortcut : Shortcut.t option) with
    | [], None -> t_type name
    | _, None ->
      let args = List.map ~f:Ml.tvar targs in
      let node_type = Ml.poly_inst ~args "node" in
      Printf.sprintf "%s %s" node_type (t_type name)
    | [], Some {outer_record; _} -> (t_type outer_record)
    | _, Some _ ->
      (* If we ever have shortcuts to polymorphic types we'll need to explicitly
         deal with it.
         In particular we'll need to know if [outer_record] is polymorphic
         over the the same type argument or if it expects a specific instance
         of [inner_variant]. *)
      assert false

  let view_type ~targs ty =
    match (ty : Astlib.Grammar.ty) with
    (* Polymorphic types in the AST can only be instanciated with other nodes *)
    | Var v when List.mem ~set:targs v -> Ml.(poly_inst ~args:[tvar v] "node")
    | _ -> string_of_ty ty

  let print_field_viewer ~targs ~name (fname, ty) =
    let value_type = value_type ~targs ~shortcut:None name in
    let view_type = view_type ~targs ty in
    Print.newline ();
    let in_, out = "'i", "'o" in
    Print.println "val %s'match : %s -> %s"
      (Ml.id fname)
      (view_t view_type ~in_ ~out)
      (view_t value_type ~in_ ~out)

  let print_variant_viewer ~name ~shortcut (cname, clause) =
    let value_type = value_type ~targs:[] ~shortcut name in
    match (clause : Astlib.Grammar.clause) with
    | Empty ->
      Print.newline ();
      let in_, out = "'a", "'a" in
      Print.println "val %s : %s" (Ml.id cname) (view_t value_type ~in_ ~out)
    | Tuple tyl ->
      let in_, out = "'i", "'o" in
      let arg_type : Astlib.Grammar.ty =
        match tyl with
        | [ty] -> ty
        | _ -> Tuple tyl
      in
      Print.println "val %s : %s -> %s"
        (Ml.id cname)
        (view_t (string_of_ty arg_type) ~in_ ~out)
        (view_t value_type ~in_ ~out)
    | Record _fields -> ()
end

let print_viewer ~what ~shortcuts (name, kind) =
  let (module M : VIEWER_PRINTER) =
    match what with
    | `Intf -> (module Signature)
    | `Impl -> (module Structure)
  in
  let targs, decl =
    match (kind : Astlib.Grammar.kind) with
    | Mono decl -> ([], decl)
    | Poly (targs, decl) -> (targs, decl)
  in
  match targs, decl with
  | [], Versioned (Variant variants) ->
    let shortcut = Shortcut.Map.find shortcuts name in
    List.iter variants ~f:(M.print_variant_viewer ~name ~shortcut)
  | _, Versioned (Variant _) ->
    (* There are no polymorphic variant types in the AST atm. If some are
       added we'll need to handle a few things, including properly generating
       fresh type variables for the input and output type varibales in the
       [View.t] types for empty variants. *)
    assert false
  | _, Versioned (Record fields) ->
    List.iter fields ~f:(M.print_field_viewer ~targs ~name)
  | _, (Unversioned _ | Versioned (Wrapper _)) ->
    ()

let print_viewer_ml () =
  Print.newline ();
  let grammars = Astlib.History.versioned_grammars Astlib.history in
  Ml.define_modules grammars ~f:(fun version grammar ->
    let shortcuts = Shortcut.Map.from_grammar grammar in
    let version = Ml.module_name version in
    Print.println "open Versions.%s" version;
    Print.println "include Loc_types";
    To_concrete.define_conversion_failed ~version;
    List.iter grammar ~f:(print_viewer ~what:`Impl ~shortcuts))

let print_viewer_mli () =
  Print.newline ();
  let grammars = Astlib.History.versioned_grammars Astlib.history in
  Ml.declare_modules grammars ~f:(fun version grammar ->
    let shortcuts = Shortcut.Map.from_grammar grammar in
    Print.println "open Versions";
    Print.println "open %s" (Ml.module_name version);
    Print.println "include LOC_TYPES";
    List.iter grammar ~f:(print_viewer ~what:`Intf ~shortcuts))
