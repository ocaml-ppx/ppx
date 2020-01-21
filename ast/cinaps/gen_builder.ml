open Stdppx

module Arrow = struct
  module Arg = struct
    type t =
      { label : string option
      ; typ : Astlib.Grammar.ty
      }

    let anon typ = { label = None; typ }

    let named name typ = { label = Some (Ml.id name); typ }

    let loc = { label = Some "loc"; typ = Astlib.Grammar.Location }
  end

  type t =
    { ret : Astlib.Grammar.ty
    ; args : Arg.t list
    }

  let to_pattern t =
    List.mapi t.args ~f:(fun i (a : Arg.t) ->
      match a.label with
      | Some label -> "~" ^ label
      | None -> sprintf "a%d" i)
    |> String.concat ~sep:" "

  let to_pattern_expr t =
    List.mapi t.args ~f:(fun i (a : Arg.t) ->
      match a.label with
      | Some label -> (Some label, label)
      | None -> (None, sprintf "a%d" i))

  let add_loc t = { t with args = Arg.loc :: t.args }

  let const ret = { ret ; args = [] }

  let of_tuple tup ret =
    { ret
    ; args = List.map tup ~f:Arg.anon
    }

  let of_record fields ret =
    { ret
    ; args = List.map fields ~f:(fun (name, ty) -> Arg.named name ty)
    }

  let print_sig { ret; args } =
    Grammar.string_of_ty ~internal:false ret
    |> Ml.print_arrow args ~f:(fun a ->
      let typ = Grammar.string_of_ty ~internal:false a.typ in
      match a.label with
      | None -> typ
      | Some label -> Ml.id label ^ ":" ^ typ)
end

module M = struct
  module rec Constr : sig
    type t =
      | Tuple of Expr.t list
      | Record of Record.t
  end = Constr
  and Expr : sig
    type t =
      | Constr of string * Constr.t
      | Record of Record.t
      | App of string * (string option * t) list
      | Const of Const.t
      | Ident of string
  end = Expr
  and Record : sig
    type t = (string * Expr.t option) list
  end = Record
  and Const : sig
    type t = Nil
  end = Const
end

module Expr = struct
  module Constr = M.Constr
  module Record = M.Record
  module Const = M.Const
  include M.Expr

  let print_const = function
    | Const.Nil -> print_string "[]"

  let rec print_record fields =
    Print.println "{";
    List.iter fields ~f:(fun (name, expr) ->
      match expr with
      | None -> Print.println "%s;" name
      | Some e ->
        Printf.printf "%s = " name;
        print e;
        Print.println ";"
    );
    Print.println "}"
  and print_constr name = function
    | Constr.Tuple [] -> print_string name
    | Tuple a ->
      print_string "(";
      List.iter a ~f:print;
      print_string ")"
    | Record r -> print_record r
  and print = function
    | Ident s -> print_string s
    | Const c -> print_const c
    | Record r -> print_record r
    | Constr (name, arg) -> print_constr name arg
    | App (f, args) ->
      Printf.printf "%s" f;
      List.iter args ~f:(fun (label, e) ->
        match label, e with
        | Some label, Ident id ->
          if label = id then
            Printf.printf " ~%s" label
          else
            Printf.printf " ~%s:%s" label id
        | Some label, Const Nil ->
          Printf.printf " ~%s:" label;
          print (Const Nil)
        | Some label, e ->
          Printf.printf " ~%s:(" label;
          print e;
          Printf.printf ")"
        | None, Ident id ->
          Printf.printf " %s" id
        | None, Const Nil ->
          print_string " ";
          print (Const Nil)
        | None, e ->
          print_string " (";
          print e;
          print_string ")")
end

let empty_attributes = Expr.App ("Attributes.of_concrete", [None, Const Nil])

module Builder = struct
  type t =
    { name : string
    ; arr : Arrow.t
    ; impl : Expr.t Lazy.t
    }

  let make ~name ~arr ~impl =
    { name = Ml.id name
    ; arr
    ; impl
    }

  let print_sig { name; arr ; impl = _ } =
    Ml.declare_val name (Block (fun () ->
      Arrow.print_sig arr))

  let print_impl { name; arr ; impl } =
    Print.println "let %s %s =" name (Arrow.to_pattern arr);
    Expr.print (Lazy.force impl);
    Print.newline ()

  let of_variant type_name (v : Astlib.Grammar.variant) desc_for =
    match desc_for type_name with
    | None -> []
    | Some (described_type_name, described_type)  ->
      let type_ = Astlib.Grammar.Name described_type_name in
      List.map v ~f:(fun (cname, (constr : Astlib.Grammar.clause)) ->
        let arr =
          match constr with
          | Empty -> Arrow.const type_
          | Tuple tuple -> Arrow.of_tuple tuple type_
          | Record record -> Arrow.of_record record type_
        in
        let label_prefix =
          described_type
          |> List.map ~f:fst
          |> String.common_prefix
        in
        let arr =
          if List.exists described_type ~f:(fun (f, _) ->
            String.drop_prefix f ~prefix:label_prefix = Some "loc"
          ) then
            Arrow.add_loc arr
          else
            arr
        in
        let impl = lazy (
          let desc =
            let fun_name =
              sprintf "%s.%s" (Ml.module_name type_name) (Ml.id cname)
            in
            let args =
              Arrow.to_pattern_expr arr
              |> List.filter_map ~f:(fun (l, a) ->
                if l = Some "loc" then
                  None
                else
                  Some (l, Expr.Ident a))
            in
            Expr.App (fun_name, args)
          in
          let constructor_function =
            Printf.sprintf "%s.create" (Ml.module_name described_type_name)
          in
          let args =
            List.map described_type ~f:(fun (field, _) ->
              let expr =
                match String.drop_prefix field ~prefix:label_prefix with
                | Some "loc" -> Expr.Ident "loc"
                | Some "attributes" -> empty_attributes
                | Some "desc" -> desc
                | _ -> assert false
              in
              (Some field, expr))
          in
          Expr.App (constructor_function, args)
        ) in
        make ~name:cname ~arr ~impl)

  let of_record name (fields : Astlib.Grammar.record) =
    if Shortcuts.has_desc_field fields then
      (* This case will be covered when we generate the _desc type *)
      None
    else
      let ret = Astlib.Grammar.Name name in
      let common_prefix =
        String.common_prefix (List.map ~f:fst fields) in
      let args =
        List.filter_map fields ~f:(fun (name, ty) ->
          let name =
            String.drop_prefix name ~prefix:common_prefix
            |> Option.value_exn
          in
          if name = "attributes" then
            None
          else
            Some (name, ty))
        |> List.sort ~compare:(fun (n1, _) (n2, _) ->
          match n1, n2 with
          | "loc", "loc" -> Ordering.Eq
          | "loc", _ -> Ordering.Lt
          | _, "loc" -> Ordering.Gt
          | _, _ -> String.compare n1 n2)
      in
      let arr = Arrow.of_record args ret in
      let impl = lazy (
        let fields = List.map fields ~f:(fun (name, _ty) ->
          let pat =
            String.drop_prefix name ~prefix:common_prefix
            |> Option.value_exn
          in
          if pat = "attributes" then
            (Some name, empty_attributes)
          else
            (Some name, Expr.Ident (Ml.id pat))
        ) in
        let constructor = Printf.sprintf "%s.create"
                            (String.capitalize_ascii name) in
        Expr.App (constructor, fields)
      ) in
      Some (make ~name ~arr ~impl)
end

let builders name (grammar : Astlib.Grammar.kind) desc_for =
  match grammar with
  | Poly (_, _) -> []
  | Mono decl ->
    match decl with
    | Alias _ -> []
    | Record r ->
      begin match Builder.of_record name r with
      | None -> []
      | Some r -> [r]
      end
    | Variant v -> Builder.of_variant name v desc_for

let print_builder_ml () =
  Print.newline ();
  let grammars = Astlib.History.versioned_grammars Astlib.history in
  Ml.define_modules grammars ~f:(fun version grammar ->
    let version = Ml.module_name version in
    Print.println "open Versions.%s" version;
    let desc_for =
      let m = lazy (Shortcuts.from_grammar grammar) in
      fun name -> Shortcuts.shortcut (Lazy.force m) name
    in
    List.iter grammar ~f:(fun (node_name, (kind : Astlib.Grammar.kind)) ->
      let builders = builders node_name kind desc_for in
      List.iter ~f:Builder.print_impl builders))

let print_builder_mli () =
  Print.newline ();
  let grammars = Astlib.History.versioned_grammars Astlib.history in
  Ml.declare_modules grammars ~f:(fun version grammar ->
    let version = Ml.module_name version in
    let desc_for =
      let m = lazy (Shortcuts.from_grammar grammar) in
      fun name -> Shortcuts.shortcut (Lazy.force m) name
    in
    Print.println "open Versions.%s" version;
    List.iter grammar ~f:(fun (node_name, (kind : Astlib.Grammar.kind)) ->
      let builders = builders node_name kind desc_for in
      List.iter ~f:Builder.print_sig builders))
