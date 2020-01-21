open Stdppx

let string_of_ty ty = Grammar.string_of_ty ~internal:false ty

module Structure = struct
  let print_to_concrete node_name expr =
    To_concrete.print_to_concrete_exn ~node_name expr

  let print_field_viewer ~name (fname, _ty) =
    Print.newline ();
    Print.println "let %s'match view value =" (Ml.id fname);
    Print.indented (fun () ->
      print_to_concrete name "value";
      Print.println "view concrete.%s.%s" (Ml.module_name name) (Ml.id fname))

  let print_viewer ~shortcuts:_ (name, kind) =
    match (kind : Astlib.Grammar.kind) with
    | Poly (_, _decl) ->
      (* We skip polymorphic types because [Versions] only provide specialized
         [to_concrete_xxx] functions which mean we would have to generate
         [xxx'match] function for each instance of the type. *)
      ()
    | Mono decl ->
      (match decl with
       | Variant _variants -> ()
       | Record fields ->
         List.iter fields ~f:(print_field_viewer ~name)
       | Alias _ ->
         ())
end

module Signature = struct
  let view_t ty ~in_ ~out =
    Printf.sprintf "(%s, %s, %s) View.t" (string_of_ty ty) in_ out

  let print_field_viewer ~name (fname, ty) =
    Print.newline ();
    let in_, out = "'i", "'o" in
    Print.println "val %s'match : %s -> %s"
      (Ml.id fname)
      (view_t ty ~in_ ~out)
      (view_t (Name name) ~in_ ~out)

  let print_viewer ~shortcuts:_ (name, kind) =
    match (kind : Astlib.Grammar.kind) with
    | Poly (_, _decl) ->
      (* We skip polymorphic types because [Versions] only provide specialized
         [to_concrete_xxx] functions which mean we would have to generate
         [xxx'match] function for each instance of the type. *)
      ()
    | Mono decl ->
      (match decl with
       | Variant _variants -> ()
       | Record fields ->
         List.iter fields ~f:(print_field_viewer ~name)
       | Alias _ ->
         ())
end

let print_viewer_ml () =
  Print.newline ();
  let grammars = Astlib.History.versioned_grammars Astlib.history in
  Ml.define_modules grammars ~f:(fun version grammar ->
    let shortcuts = Shortcut.Map.from_grammar grammar in
    let version = Ml.module_name version in
    Print.println "open Versions.%s" version;
    To_concrete.define_conversion_failed ~version;
    List.iter grammar ~f:(Structure.print_viewer ~shortcuts))

let print_viewer_mli () =
  Print.newline ();
  let grammars = Astlib.History.versioned_grammars Astlib.history in
  Ml.declare_modules grammars ~f:(fun version grammar ->
    let shortcuts = Shortcut.Map.from_grammar grammar in
    Print.println "open Versions.%s" (Ml.module_name version);
    List.iter grammar ~f:(Signature.print_viewer ~shortcuts))
