open Stdppx

let string_of_ty ty = Grammar.string_of_ty ~internal:false ty

module type VIEWER_PRINTER = sig
  val print_field_viewer :
    name: string ->
    (string * Astlib.Grammar.ty) ->
    unit

  val print_variant_viewer :
    name: string ->
    (string * Astlib.Grammar.clause) ->
    unit
end

module Structure : VIEWER_PRINTER = struct
  let print_to_concrete node_name expr =
    To_concrete.print_to_concrete_exn ~node_name expr

  let tuple tyl =
    match tyl with
    | [_] -> "arg"
    | _ ->
      let vars = List.mapi tyl ~f:(fun i _ -> Printf.sprintf "arg%d" i) in
      Ml.tuple vars

  let print_field_viewer ~name (fname, _ty) =
    Print.newline ();
    Print.println "let %s'match view value =" (Ml.id fname);
    Print.indented (fun () ->
      print_to_concrete name "value";
      Print.println "view concrete.%s.%s" (Ml.module_name name) (Ml.id fname))

  let print_variant_viewer ~name (cname, clause) =
    match (clause : Astlib.Grammar.clause) with
    | Empty ->
      Print.newline ();
      Print.println "let %s value =" (Ml.id cname);
      Print.indented (fun () ->
        print_to_concrete name "value";
        Print.println "match concrete with";
        Print.println "| %s.%s -> View.ok" (Ml.module_name name) (Ml.tag cname);
        Print.println "| _ -> View.error")
    | Tuple tyl ->
      Print.newline ();
      Print.println "let %s view value =" (Ml.id cname);
      Print.indented (fun () ->
        let args = tuple tyl in
        print_to_concrete name "value";
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
      ()
end

module Signature : VIEWER_PRINTER = struct
  let view_t ty ~in_ ~out =
    Printf.sprintf "(%s, %s, %s) View.t" (string_of_ty ty) in_ out

  let print_field_viewer ~name (fname, ty) =
    Print.newline ();
    let in_, out = "'i", "'o" in
    Print.println "val %s'match : %s -> %s"
      (Ml.id fname)
      (view_t ty ~in_ ~out)
      (view_t (Name name) ~in_ ~out)

  let print_variant_viewer ~name (cname, clause) =
    match (clause : Astlib.Grammar.clause) with
    | Empty ->
      Print.newline ();
      let in_, out = "'a", "'a" in
      Print.println "val %s : %s" (Ml.id cname) (view_t (Name name) ~in_ ~out)
    | Tuple tyl ->
      let in_, out = "'i", "'o" in
      let arg_type : Astlib.Grammar.ty =
        match tyl with
        | [ty] -> ty
        | _ -> Tuple tyl
      in
      Print.println "val %s : %s -> %s"
        (Ml.id cname)
        (view_t arg_type ~in_ ~out)
        (view_t (Name name) ~in_ ~out)
    | Record _fields -> ()
end

let print_viewer ~what ~shortcuts:_ (name, kind) =
  let (module M : VIEWER_PRINTER) =
    match what with
    | `Intf -> (module Signature)
    | `Impl -> (module Structure)
  in
  match (kind : Astlib.Grammar.kind) with
  | Poly (_, _decl) ->
    (* We skip polymorphic types because [Versions] only provide specialized
       [to_concrete_xxx] functions which mean we would have to generate
       [xxx'match] function for each instance of the type. *)
    ()
  | Mono decl ->
    (match decl with
     | Variant variants ->
       List.iter variants ~f:(M.print_variant_viewer ~name)
     | Record fields ->
       List.iter fields ~f:(M.print_field_viewer ~name)
     | Alias _ ->
       ())

let print_viewer_ml () =
  Print.newline ();
  let grammars = Astlib.History.versioned_grammars Astlib.history in
  Ml.define_modules grammars ~f:(fun version grammar ->
    let shortcuts = Shortcut.Map.from_grammar grammar in
    let version = Ml.module_name version in
    Print.println "open Versions.%s" version;
    To_concrete.define_conversion_failed ~version;
    List.iter grammar ~f:(print_viewer ~what:`Impl ~shortcuts))

let print_viewer_mli () =
  Print.newline ();
  let grammars = Astlib.History.versioned_grammars Astlib.history in
  Ml.declare_modules grammars ~f:(fun version grammar ->
    let shortcuts = Shortcut.Map.from_grammar grammar in
    Print.println "open Versions.%s" (Ml.module_name version);
    List.iter grammar ~f:(print_viewer ~what:`Intf ~shortcuts))
