open Stdppx

let string_of_ty ty = Grammar.string_of_ty ~internal:false ty

let variant_viewer_name cname =
  Ml.id (cname ^ "'const")

let wrapper_types grammar =
  let init = String.Map.empty in
  List.fold_left ~init grammar
    ~f:(fun acc (name, kind) ->
      match (kind : Astlib.Grammar.kind) with
      | Mono (Wrapper ty) -> String.Map.add acc name ([], ty)
      | Poly (targs, Wrapper ty) -> String.Map.add acc name (targs, ty)
      | _ -> acc)

module type VIEWER_PRINTER = sig
  val print_wrapper_viewer :
    wrapper_types: (string list * Astlib.Grammar.ty) String.Map.t ->
    targs: string list ->
    name: string ->
    Astlib.Grammar.ty ->
    unit

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

  (** Returns the list of types until a non-wrapper type is reached.
      It's used to determine which `to_concrete` functions should be applied
      until the result can be matched over.
      E.g. if you consider a grammar describing:
      [{
      type ('a, 'b) t = {a : 'a; b = 'b}
      type 'a u = ('a, int) t
      type v = int u
      }]
      [unwrap_chain ~wrapper_types ~name:"v" ~ty:(Instance ("u", [Int])]
      returns ["v"; "u"] because values of type [v] can only be matched on by
      unrolling aliases [v] and [u]. It stops without including [t] because [t]
      is a record type. It can be matched based on the fields [a] and [b]. *)
  let unwrap_chain ~wrapper_types ~name ~ty =
    let rec aux acc ty =
      match (ty : Astlib.Grammar.ty) with
      | Name n | Instance (n, _) ->
        (match String.Map.find wrapper_types n with
         | Some (_targs, ty) -> aux (n::acc) ty
         | None -> List.rev acc)
      | Var _ | Bool | Char | Int | String | Location | Loc _ | List _
      | Option _ | Tuple _ ->
        List.rev acc
    in
    aux [name] ty

  let print_wrapper_viewer ~wrapper_types ~targs:_ ~name ty =
    let unwrap_chain = unwrap_chain ~wrapper_types ~name ~ty in
    Print.newline ();
    Print.println "let %s view value =" (variant_viewer_name name);
    Print.indented (fun () ->
      let concrete i = Printf.sprintf "concrete%d" i in
      List.iteri unwrap_chain ~f:(fun i node_name ->
        let input = if i = 0 then "value" else concrete (i - 1) in
        To_concrete.print_to_concrete_exn ~node_name ~var_name:(concrete i) input);
      Print.println "view %s" (concrete (List.length unwrap_chain - 1)))

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
      Print.println "let %s value =" (variant_viewer_name cname);
      Print.indented (fun () ->
        print_to_concrete ~shortcut name "value";
        Print.println "match concrete with";
        Print.println "| %s.%s -> View.ok" (Ml.module_name name) (Ml.tag cname);
        Print.println "| _ -> View.error")
    | Tuple tyl ->
      Print.newline ();
      Print.println "let %s view value =" (variant_viewer_name cname);
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

  let rec subst_vars ~substs ty =
    let open Astlib.Grammar in
    match ty with
    | Var s ->
      (match List.assoc_opt s substs with
       | Some subst_ty -> subst_ty
       | None -> assert false)
    | Loc ty -> Loc (subst_vars ~substs ty)
    | List ty -> List (subst_vars ~substs ty)
    | Option ty -> Option (subst_vars ~substs ty)
    | Tuple tyl -> Tuple (List.map ~f:(subst_vars ~substs) tyl)
    | Instance (s, tyl) -> Instance (s, List.map ~f:(subst_vars ~substs) tyl)
    | Bool | Char | Int | String | Location | Name _ -> ty

  (** Recursively follows the type aliases, replacing type variables in
      polymorphic types instances when needed.
      This is used to determined the type of the view argument of views for
      wrapper types.
      E.g. if you consider the grammar representing the following types:
      [{
      type ('a, 'b) t = {a : 'a; b = 'b}
      type 'a u = ('a, int) t
      type v = int u
      }]
      The view for type v should be a:
      [((int, int) t, 'i, 'o) View.t -> (v, 'i, 'o) View.t] and therefore
      [unwrapped_view_type ~wrapper_types ~targs:[] ~name:"t" (Instance ...)]
      will return [Instance ("t", [Int; Int])]. *)
  let unwrapped_view_type ~wrapper_types ~targs ty =
    let rec aux ~substs ty =
      let substituted_ty = subst_vars ~substs ty in
      let name_and_tyl = 
        match substituted_ty with
        | Name s -> Some (s, [])
        | Instance (s, l) -> Some (s, l)
        | Var _ | Bool | Char | Int | String | Location | Loc _ | List _
        | Option _ | Tuple _ -> None
      in
      let next =
        let open Option.O in
        name_and_tyl >>= fun (name, tyl) ->
        String.Map.find wrapper_types name >>| fun (targs, ty) ->
        let substs = List.combine targs tyl in
        substs, ty
      in
      match next with
      | None -> substituted_ty
      | Some (substs, ty) -> aux ~substs ty
    in
    let substs = List.map targs ~f:(fun s -> (s, Astlib.Grammar.Var s)) in
    aux ~substs ty

  let print_wrapper_viewer ~wrapper_types ~targs ~name ty =
    let in_, out = "'i", "'o" in
    let value_type = value_type ~targs ~shortcut:None name in
    let view_type =
      let ty = unwrapped_view_type ~wrapper_types ~targs ty in
      view_type ~targs ty
    in
    Print.println "val %s: %s -> %s"
      (variant_viewer_name name)
      (view_t view_type ~in_ ~out)
      (view_t value_type ~in_ ~out)

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
      Print.println "val %s : %s"
        (variant_viewer_name cname)
        (view_t value_type ~in_ ~out)
    | Tuple tyl ->
      let in_, out = "'i", "'o" in
      let arg_type : Astlib.Grammar.ty =
        match tyl with
        | [ty] -> ty
        | _ -> Tuple tyl
      in
      Print.println "val %s : %s -> %s"
        (variant_viewer_name cname)
        (view_t (string_of_ty arg_type) ~in_ ~out)
        (view_t value_type ~in_ ~out)
    | Record _fields -> ()
end

let print_viewer ~what ~shortcuts ~wrapper_types (name, kind) =
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
  | [], Variant variants ->
    let shortcut = Shortcut.Map.find shortcuts name in
    List.iter variants ~f:(M.print_variant_viewer ~name ~shortcut)
  | _, Variant _ ->
    (* There are no polymorphic variant types in the AST atm. If some are
       added we'll need to handle a few things, including properly generating
       fresh type variables for the input and output type varibales in the
       [View.t] types for empty variants. *)
    assert false
  | _, Record fields ->
    List.iter fields ~f:(M.print_field_viewer ~targs ~name)
  | _, Wrapper ty ->
    M.print_wrapper_viewer ~wrapper_types ~targs ~name ty

let print_viewer_ml version =
  Print.newline ();
  let grammar = Astlib.History.find_grammar Astlib.history ~version in
  let wrapper_types = wrapper_types grammar in
  let shortcuts = Shortcut.Map.from_grammar grammar in
  let version = Ml.module_name (Astlib.Version.to_string version) in
  Print.println "open Versions.%s" version;
  Print.println "include Viewer_common";
  To_concrete.define_conversion_failed ~version;
  List.iter grammar ~f:(print_viewer ~what:`Impl ~shortcuts ~wrapper_types)

let print_viewer_mli version =
  Print.newline ();
  let grammar = Astlib.History.find_grammar Astlib.history ~version in
  let wrapper_types = wrapper_types grammar in
  let shortcuts = Shortcut.Map.from_grammar grammar in
  Print.println "open Versions";
  Print.println "open %s" (Ml.module_name (Astlib.Version.to_string version));
  Print.println "include module type of Viewer_common";
  List.iter grammar ~f:(print_viewer ~what:`Intf ~shortcuts ~wrapper_types)
