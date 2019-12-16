open Stdppx

let map_keyword = function
  | "open"
  | "private"
  | "downto"
  | "to"
  | "mutable"
  | "rec"
  | "nonrec"
  | "virtual"
  | "type"
  | "mod"
  | "begin"
  | "end" as s -> s ^ "_"
  | s -> s

module Arrow = struct
  module Arg = struct
    type t =
      { label : string option
      ; typ : Astlib.Grammar.ty
      }

    let anon typ = { label = None; typ }

    let named name typ = { label = Some (map_keyword name); typ }

    let loc = { label = Some "loc"; typ = Astlib.Grammar.Location }
  end

  type t =
    { ret : Astlib.Grammar.ty
    ; args : Arg.t list
    }

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

module Builder = struct
  type t =
    { name : string
    ; arr : Arrow.t
    }

  let make ~name ~arr =
    { name = Ml.id name
    ; arr
    }

  let print_sig { name; arr } =
    Ml.declare_val name (Block (fun () ->
      Arrow.print_sig arr))
end

let of_variant name (v : Astlib.Grammar.variant) =
  match String.drop_suffix name ~suffix:"_desc" with
  | None -> []
  | Some type_ ->
    let type_ = Astlib.Grammar.Name type_ in
    List.map v ~f:(fun (name, (constr : Astlib.Grammar.clause)) ->
      let arr =
        match constr with
        | Empty -> Arrow.const type_
        | Tuple tuple -> Arrow.of_tuple tuple type_
        | Record record -> Arrow.of_record record type_
      in
      let arr = Arrow.add_loc arr in
      Builder.make ~name ~arr)

let of_record name (fields : Astlib.Grammar.record) =
  if List.exists fields ~f:(fun (name, _) ->
    String.is_suffix name ~suffix:"_desc") then
    (* This case will be covered when we generate the _desc type *)
    None
  else
    let ret = Astlib.Grammar.Name name in
    let fields =
      let common_prefix =
        String.common_prefix (List.map ~f:fst fields) in
      List.filter_map fields ~f:(fun (name, ty) ->
        let name =
          String.drop_prefix name ~prefix:common_prefix
          |> Option.value_exn
        in
        if name = "attributes" || name = "loc" then
          None
        else
          Some (name, ty))
    in
    let arr = Arrow.of_record fields ret in
    let arr = Arrow.add_loc arr in
    Some (Builder.make ~name ~arr)

let builders name (grammar : Astlib.Grammar.kind) =
  match grammar with
  | Poly (_, _) -> []
  | Mono decl ->
    match decl with
    | Alias _ -> []
    | Record r ->
      begin match of_record name r with
      | None -> []
      | Some r -> [r]
      end
    | Variant v -> of_variant name v

let print_builder_ml () = ()

let print_builder_mli () =
  Print.newline ();
  let grammars = Astlib.History.versioned_grammars Astlib.history in
  Ml.declare_modules grammars ~f:(fun version grammar ->
    let version = Ml.module_name version in
    Print.println "open Versions.%s" version;
    List.iter grammar ~f:(fun (node_name, (kind : Astlib.Grammar.kind)) ->
      let builders = builders node_name kind in
      List.iter ~f:Builder.print_sig builders))
