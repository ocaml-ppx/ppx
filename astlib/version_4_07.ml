let version = Version.of_string "v4_07"

let concat_map ~f l =
  List.concat (List.map f l)

module Grammar = struct
  let list_remove ~f l =
    let rec aux acc = function
      | [] -> List.rev acc
      | hd::tl when f hd -> aux acc tl
      | hd::tl -> aux (hd::acc) tl
    in
    aux [] l

  let remove_field ~name record =
    let open Grammar in
    match record with
    | Mono (Record fields) ->
      let new_fields = list_remove ~f:(fun (n, _) -> n = name) fields in
      Mono (Record new_fields)
    | _ -> assert false

  let update_variant ~f variant =
    let open Grammar in
    match variant with
    | Mono (Variant ctors) ->
      Mono (Variant (concat_map ~f ctors))
    | _ -> assert false

  let append_ctor ctor variant =
    let open Grammar in
    match variant with
    | Mono (Variant ctors) ->
      Mono (Variant (ctor::ctors))
    | _ -> assert false

  let attribute : Grammar.kind =
    Mono (Ty (Tuple [Loc String; Name "payload"]))

  let row_field : Grammar.kind =
    Mono
      (Variant
         [ ( "Rtag"
           , Tuple
               [ Loc String
               ; Name "attributes"
               ; Bool
               ; List (Name "core_type") ] )
         ; ("Rinherit", Tuple [Name "core_type"]) ])

  let object_field : Grammar.kind =
    Mono
      (Variant
         [ ( "Otag"
           , Tuple
               [Loc String; Name "attributes"; Name "core_type"]
           )
         ; ("Oinherit", Tuple [Name "core_type"]) ])

  let pexp_open : Grammar.clause =
    Tuple
      [ Name "override_flag"
      ; Name "longident_loc"
      ; Name "expression" ]

  let pcty_open : Grammar.clause =
    Tuple
      [ Name "override_flag"
      ; Name "longident_loc"
      ; Name "class_type" ]

  let pcl_open : Grammar.clause =
    Tuple
      [ Name "override_flag"
      ; Name "longident_loc"
      ; Name "class_expr" ]

  let psig_exception : Grammar.clause =
    Tuple [Name "extension_constructor"]

  let open_description : Grammar.kind =
    Mono
      (Record
         [ ("popen_lid", Name "longident_loc")
         ; ("popen_override", Name "override_flag")
         ; ("popen_loc", Location)
         ; ("popen_attributes", Name "attributes") ])

  let pstr_exception : Grammar.clause =
    Tuple [Name "extension_constructor"]

  let pstr_open : Grammar.clause =
    Tuple [Name "open_description"]

  let ptop_dir : Grammar.clause =
    Tuple [String; Name "directive_argument"]

  let update_type_decl type_decl =
    let drop = [] in
    match type_decl with
    | ("attribute" as n, _) -> [(n, attribute)]
    | ("core_type" as n, record) ->
      [(n, remove_field ~name:"ptyp_loc_stack" record)]
    | ("row_field" as n, _) -> [(n, row_field)]
    | ("row_field_desc", _) -> drop
    | ("object_field" as n, _) -> [(n, object_field)]
    | ("object_field_desc", _) -> drop
    | ("pattern" as n, record) ->
      [(n, remove_field ~name:"ppat_loc_stack" record)]
    | ("expression" as n, record) ->
      [(n, remove_field ~name:"pexp_loc_stack" record)]
    | ("expression_desc" as n, variant) ->
      [ ( n
        , update_variant variant ~f:(function
            | ("Pexp_open" as n, _) -> [(n, pexp_open)]
            | ("Pexp_letop", _) -> drop
            | c -> [c])
        )
      ]
    | ("letop", _) -> drop
    | ("binding_op", _) -> drop
    | ("type_extension" as n, record) ->
      [(n, remove_field ~name:"ptyext_loc" record)]
    | ("type_exception", _) -> drop
    | ("class_type_desc" as n, variant) ->
      [ ( n
        , update_variant variant ~f:(function
            | ("Pcty_open" as n, _) -> [(n, pcty_open)]
            | c -> [c])
        )
      ]
    | ("class_expr_desc" as n, variant) ->
      [ ( n
        , update_variant variant ~f:(function
            | ("Pcl_open" as n, _) -> [(n, pcl_open)]
            | c -> [c]))
      ]
    | ("signature_item_desc" as n, variant) ->
      [ ( n
        , update_variant variant ~f:(function
            | ("Psig_typesubst", _) -> drop
            | ("Psig_exception" as n, _) -> [(n, psig_exception)]
            | ("Psig_modsubst", _) -> drop
            | c -> [c])
        )
      ]
    | ("module_substitution", _) -> drop
    | ("open_infos", _) -> drop
    | ("open_description" as n, _) -> [(n, open_description)]
    | ("open_declaration", _) -> drop
    | ("structure_item_desc" as n, variant) ->
      [ ( n
        , update_variant variant ~f:(function
            | ("Pstr_exception" as n, _) -> [(n, pstr_exception)]
            | ("Pstr_open" as n, _) -> [(n, pstr_open)]
            | c -> [c])
        )
      ]
    | ("toplevel_phrase" as n, variant) ->
      [ ( n
        , update_variant variant ~f:(function
            | ("Ptop_dir" as n, _) -> [(n, ptop_dir)]
            | c -> [c])
        )
      ]
    | ("toplevel_directive", _) -> drop
    | ("directive_argument", _) -> drop
    | ("directive_argument_desc", variant) ->
      [("directive_argument", append_ctor ("Pdir_none", Empty) variant)]
    | _ -> [type_decl]
end

module Node = struct
  open Ast

  let empty_attr ~wrap =
    let data = List [] in
    Node (wrap {name = "attributes"; data})

  module Down = struct
    let downgrade_attribute node =
      match node.data with
      | Record [| name; payload; _loc |] ->
        Some { node with data = Tuple [| name; payload |] }
      | _ -> None

    let drop_loc_stack node =
      match node.data with
      | Record [| desc; loc; _loc_stack; attributes |] ->
        Some { node with data = Record [| desc; loc; attributes |] }
      | _ -> None

    let downgrade_row_field ~unwrap node =
      match node.data with
      | Record [| Node prf_desc; _prf_loc; prf_attributes |] ->
        ( match unwrap prf_desc with
          | Some
              { name = "row_field_desc"
              ; data =
                  Variant
                    { tag = "Rtag" as tag
                    ; args = [| label; empty; args |] } } ->
            let data =
              Variant { tag; args = [| label; prf_attributes; empty; args |] }
            in
            Some { node with data }
          | Some
              { name = "row_field_desc"
              ; data = Variant { tag = "Rinherit"; _} as variant } ->
            Some { node with data = variant }
          | _ -> None )
      | _ -> None

    let downgrade_object_field ~unwrap node =
      match node.data with
      | Record [| Node pof_desc; _pof_loc; pof_attributes |] ->
        ( match unwrap pof_desc with
          | Some
              { name = "object_field_desc"
              ; data =
                  Variant { tag = "Otag" as tag; args = [| label; args |] } } ->
            let data =
              Variant { tag; args = [| label; pof_attributes; args |] }
            in
            Some { node with data }
          | Some
              { name = "object_field_desc"
              ; data = Variant {tag = "Oinherit"; _} as variant } ->
            Some { node with data = variant }
          | _ -> None )
      | _ -> None

    let deconstruct_odecl ~unwrap open_decl =
      match unwrap open_decl with
      | Some { name = "open_declaration" ; data = Node open_infos } ->
        ( match unwrap open_infos with
          | Some
              { name = "open_infos"
              ; data = Record [| Node mod_expr; override; loc; attr |] } ->
            ( match unwrap mod_expr with
              | Some
                  { name = "module_expr"
                  ; data = Record [| Node desc; _loc; _attr |] } ->
                ( match unwrap desc with
                  | Some
                      { name = "module_expr_desc"
                      ; data =
                          Variant
                            { tag = "Pmod_ident"; args = [| lident |] } } ->
                    Some (lident, override, loc, attr)
                  | _ -> None )
              | _ -> None )
          | _ -> None )
      | _ -> None

    let downgrade_expression_desc ~unwrap node =
      match node.data with
      | Variant { tag = "Pexp_open" as tag; args = [| Node open_decl; expr |] } ->
        ( match deconstruct_odecl ~unwrap open_decl with
          | Some (lident, override, _loc, _attr) ->
            let data = Variant {tag; args = [|override; lident; expr|]} in
            Some { node with data }
          | None -> None )
      | Variant { tag = "Pexp_open"; _ }
      | Variant { tag = "Pexp_letop"; _ } -> None
      | _ -> Some node

    let downgrade_type_extenstion node =
      match node.data with
      | Record [| path; params; ctors; private_; _loc; attr |] ->
        let data = Record [| path; params; ctors; private_; attr |] in
        Some { node with data }
      | _ -> None

    let lident_and_override_from_odesc ~unwrap open_desc =
      match unwrap open_desc with
      | Some { name = "open_description" ; data = Node open_infos } ->
        ( match unwrap open_infos with
          | Some
              { name = "open_infos"
              ; data = Record [| lident; override; _loc; _attr |] } ->
            Some (lident, override)
          | _ -> None )
      | _ -> None

    let downgrade_class_type_desc ~unwrap node =
      match node.data with
      | Variant { tag = "Pcty_open" as tag; args = [| Node open_desc; cty |] } ->
        ( match lident_and_override_from_odesc ~unwrap open_desc with
          | Some (lident, override) ->
            let data = Variant { tag; args = [| override; lident; cty |] } in
            Some { node with data }
          | None -> None )
      | Variant { tag = "Pcty_open"; _ } -> None
      | _ -> Some node

    let downgrade_class_expr_desc ~unwrap node =
      match node.data with
      | Variant { tag = "Pcl_open" as tag; args = [| Node open_desc; cl |] } ->
        ( match lident_and_override_from_odesc ~unwrap open_desc with
          | Some (lident, override) ->
            let data = Variant { tag; args = [| override; lident; cl |] } in
            Some { node with data }
          | None -> None )
      | Variant { tag = "Pcl_open"; _ } -> None
      | _ -> Some node

    let ext_ctor_from_type_exc ~unwrap type_exc =
      match unwrap type_exc with
      | Some
          { name = "type_exception"
          ; data = Record [| ext_ctor; _loc; _attr |] } ->
        Some ext_ctor
      | _ -> None

    let downgrade_signature_item_desc ~unwrap node =
      match node.data with
      | Variant { tag = "Psig_exception" as tag; args = [| Node type_exc |] } ->
        ( match ext_ctor_from_type_exc ~unwrap type_exc with
          | Some ext_ctor ->
            Some { node with data = Variant { tag; args = [| ext_ctor |] } }
          | None -> None )
      | Variant { tag = "Psig_exception"; _ }
      | Variant { tag = ("Psig_typesubst" | "Psig_modsubst"); _ } -> None
      | _ -> Some node

    let downgrade_structure_item_desc ~wrap ~unwrap node =
      match node.data with
      | Variant { tag = "Pstr_exception" as tag; args = [| Node type_exc |] } ->
        ( match ext_ctor_from_type_exc ~unwrap type_exc with
          | Some ext_ctor ->
            Some { node with data = Variant { tag; args = [| ext_ctor |] } }
          | None -> None )
      | Variant { tag = "Pstr_open" as tag; args = [| Node open_decl |] } ->
        ( match deconstruct_odecl ~unwrap open_decl with
          | Some (lident, override, loc, attr) ->
            let open_desc =
              let data = Record [| lident; override; loc; attr |] in
              wrap {name = "open_description"; data}
            in
            let data = Variant { tag; args = [| Node open_desc |] } in
            Some { node with data }
          | None -> None )
      | Variant { tag = ("Pstr_exception" | "Pstr_open"); _} -> None
      | _ -> Some node

    let downgrade_open_description ~unwrap node =
      match node.data with
      | Node open_infos ->
        ( match unwrap open_infos with
          | Some { name = "open_infos"; data } -> Some { node with data }
          | _ -> None )
      | _ -> None

    let dir_arg ~wrap ~unwrap pdir_arg =
      let wrap data =
        Node (wrap { name = "directive_argument"; data })
      in
      match pdir_arg with
      | Option None ->
        Some (wrap (Variant { tag = "Pdir_none"; args = [||] }))
      | Option (Some (Node dir_arg)) ->
        ( match unwrap dir_arg with
          | Some
              { name = "directive_argument"
              ; data = Record [| Node desc; _loc |] } ->
            ( match unwrap desc with
              | Some { name = "directive_argument_desc"; data } ->
                Some (wrap data)
              | _ -> None )
          | _ -> None )
      | _ -> None

    let downgrade_toplevel_phrase ~wrap ~unwrap node =
      match node.data with
      | Variant { tag = "Ptop_dir" as tag; args = [| Node tdir |] } ->
        ( match unwrap tdir with
          | Some
              { name = "toplevel_directive"
              ; data = Record [| Loc name; pdir_arg; _loc |] } ->
            ( match dir_arg ~wrap ~unwrap pdir_arg with
              | Some dir_arg ->
                let data = Variant { tag; args = [| name.txt; dir_arg |] } in
                Some { node with data }
              | _ -> None )
          | _ -> None )
      | Variant { tag = "Ptop_dir"; _} -> None
      | _ -> Some node

    let downgrade : _ History.conversion_function =
      fun node ~unwrap ~wrap ->
      match node.name with
      | "attribute" -> downgrade_attribute node
      | "core_type"
      | "pattern"
      | "expression" -> drop_loc_stack node
      | "row_field" -> downgrade_row_field ~unwrap node
      | "object_field" -> downgrade_object_field ~unwrap node
      | "expression_desc" -> downgrade_expression_desc ~unwrap node
      | "type_extension" -> downgrade_type_extenstion node
      | "class_type_desc" -> downgrade_class_type_desc ~unwrap node
      | "class_expr_desc" -> downgrade_class_expr_desc ~unwrap node
      | "signature_item_desc" ->
        downgrade_signature_item_desc ~unwrap node
      | "open_description" -> downgrade_open_description ~unwrap node
      | "structure_item_desc" ->
        downgrade_structure_item_desc ~wrap ~unwrap node
      | "toplevel_phrase" -> downgrade_toplevel_phrase ~wrap ~unwrap node
      | _ -> Some node
  end

  module Up = struct
    let upgrade_attribute node =
      match node.data with
      | Tuple [| (Loc {loc; _})  as name ; payload |] ->
        Some { node with data = Record [| name; payload; Location loc |] }
      | _ -> None

    let add_loc_stack node =
      match node.data with
      | Record [| desc; loc; attributes |] ->
        Some { node with data = Record [| desc; loc; List []; attributes |] }
      | _ -> None

    let upgrade_row_field ~wrap ~unwrap node =
      let desc data =
        Node (wrap {name = "row_field_desc"; data})
      in
      match node.data with
      | Variant
          { tag = "Rtag" as tag
          ; args = [|Loc x as label; attributes; empty; args|] } ->
        let variant = Variant {tag; args = [|label; empty; args|]} in
        let data =  Record [| desc variant; Location x.loc; attributes |] in
        Some { node with data }
      | Variant {tag = "Rinherit"; args = [| Node core_type |]} as variant ->
        ( match unwrap core_type with
          | Some
              { name = "core_type" ; data = Record [| _desc; loc; _attr |] } ->
            let data =
              Record [| desc variant; loc; empty_attr ~wrap |]
            in
            Some { node with data }
          | _ -> None )
      | _ -> None

    let upgrade_object_field ~wrap ~unwrap node =
      let desc data =
        Node (wrap {name = "object_field_desc"; data})
      in
      match node.data with
      | Variant
          { tag = "Otag" as tag; args = [| Loc x as label; attr; args |] } ->
        let variant = Variant {tag; args = [| label; args |]} in
        let data = Record [| desc variant; Location x.loc; attr |] in
        Some { node with data }
      | Variant { tag = "Oinherit"; args = [| Node core_type |] } as variant ->
        ( match unwrap core_type with
          | Some
              { name = "core_type" ; data = Record [| _desc; loc; _attr |] } ->
            let attributes =
              Node (wrap {name = "attributes"; data = List []})
            in
            let data = Record [| desc variant; loc; attributes |] in
            Some { node with data }
          | _ -> None )
      | _ -> None

    let od_pmod_ident
          ~wrap ~unwrap ~override ~lident_loc ?popen_loc ?popen_attr () =
      match unwrap lident_loc with
      | Some { name = "longident_loc"; data = (Loc {loc; _}) } ->
        let attr = empty_attr ~wrap in
        let data = Variant {tag = "Pmod_ident"; args = [|Node lident_loc|]} in
        let desc = wrap {name = "module_expr_desc"; data} in
        let data = Record [| Node desc; Location loc; attr |] in
        let mod_exp = wrap {name = "module_expr"; data} in
        let popen_loc = Option.value ~default:(Location loc) popen_loc in
        let popen_attr = Option.value ~default:attr popen_attr in
        let data = Record [| Node mod_exp; override; popen_loc; popen_attr |] in
        let open_infos = wrap {name = "open_infos"; data} in
        let data = Node open_infos in
        Some (Node (wrap {name = "open_declaration"; data}))
      | _ -> None

    let upgrade_expression_desc ~wrap ~unwrap node =
      match node.data with
      | Variant
          { tag = "Pexp_open" as tag
          ; args = [|override; Node lident_loc; expr|] } ->
        ( match od_pmod_ident ~wrap ~unwrap ~override ~lident_loc () with
          | Some open_declaration ->
            let data = Variant {tag; args = [|open_declaration; expr|]} in
            Some { node with data }
          | _ -> None )
      | Variant { tag = "Pexp_open"; _ } -> None
      | _ -> Some node

    let upgrade_type_extension ~unwrap node =
      match node.data with
      | Record [| Node li as path; params; ctors; private_; attr |] ->
        ( match unwrap li with
          | Some { name = "longident_loc"; data = Loc x } ->
            let data =
              Record [| path; params; ctors; private_; Location x.loc; attr |]
            in
            Some { node with data }
          | _ -> None )
      | _ -> None

    let open_desc ~wrap ~unwrap ~override ~lident_loc =
      match unwrap lident_loc with
      | Some { name = "longident_loc"; data = Loc {loc; _} } ->
        let attr = empty_attr ~wrap in
        let data = Record [| Node lident_loc; override; Location loc; attr |] in
        let open_infos = wrap {name = "open_infos"; data} in
        let data = Node open_infos in
        Some (Node (wrap {name = "open_description"; data}))
      | _ -> None

    let upgrade_class_type_desc ~wrap ~unwrap node =
      match node.data with
      | Variant
          { tag = "Pcty_open" as tag
          ; args = [| override; Node lident_loc; cty |] } ->
        let open_desc = open_desc ~wrap ~unwrap ~override ~lident_loc in
        Option.map
          (fun odesc ->
             { node with data = Variant { tag; args = [| odesc; cty |] } })
          open_desc
      | Variant { tag = "Pcty_open"; _ } -> None
      | _ -> Some node

    let upgrade_class_expr_desc ~wrap ~unwrap node =
      match node.data with
      | Variant
          { tag = "Pcl_open" as tag
          ; args = [| override; Node lident_loc; cl |] } ->
        let open_desc = open_desc ~wrap ~unwrap ~override ~lident_loc in
        Option.map
          (fun odesc ->
             { node with data = Variant { tag; args = [| odesc; cl |] } })
          open_desc
      | _ -> Some node

    let type_exc ~wrap ~unwrap ext_ctor =
      match unwrap ext_ctor with
      | Some
          { name = "extension_constructor";
            data = Record [| _name; _kind; loc; _attr |] } ->
        let data = Record [| Node ext_ctor; loc; empty_attr ~wrap |] in
        Some (Node (wrap {name = "type_exception"; data}))
      | _ -> None

    let upgrade_signature_item_desc ~wrap ~unwrap node =
      match node.data with
      | Variant { tag = "Psig_exception" as tag; args = [| Node ext_ctor |] } ->
        ( match type_exc ~wrap ~unwrap ext_ctor with
          | Some type_exc ->
            Some { node with data = Variant {tag; args = [| type_exc |]} }
          | None -> None )
      | Variant { tag = "Psig_exception"; _ } -> None
      | _ -> Some node

    let upgrade_open_description ~wrap node =
      let data = Node (wrap {name = "open_infos"; data = node.data}) in
      Some { node with data }

    let upgrade_structure_item_desc ~wrap ~unwrap node =
      match node.data with
      | Variant { tag = "Pstr_exception" as tag; args = [| Node ext_ctor |] } ->
        ( match type_exc ~wrap ~unwrap ext_ctor with
          | Some type_exc ->
            Some { node with data = Variant { tag; args = [| type_exc |] } }
          | None -> None )
      | Variant { tag = "Pstr_open" as tag; args = [| Node open_desc |] } ->
        ( match unwrap open_desc with
          | Some
              { name = "open_description"
              ; data =
                  Record
                    [| Node lident_loc; override; popen_loc; popen_attr |] } ->
            let open_decl =
              od_pmod_ident
                ~wrap ~unwrap ~lident_loc ~override ~popen_loc ~popen_attr ()
            in
            Option.map
              (fun open_decl ->
                 { node with data = Variant { tag; args = [| open_decl |] }})
              open_decl
          | _ -> None )
      | Variant { tag = ("Pstr_exception" | "Pstr_open"); _ } -> None
      | _ -> Some node

    let pdir_arg ~wrap ~unwrap dir_arg =
      match unwrap dir_arg with
      | Some
          { name = "directive_argument"
          ; data = Variant { tag = "Pdir_none"; args = [||] } } ->
        Some (Option None)
      | Some dir_arg ->
        let desc = wrap {dir_arg with name = "directive_argument_desc"} in
        let data = Record [| Node desc; Location Location.none |] in
        Some (Option (Some (Node (wrap {name = "directive_argument"; data}))))
      | None -> None

    let upgrade_toplevel_phrase ~wrap ~unwrap node =
      match node.data with
      | Variant { tag = "Ptop_dir" as tag; args = [| name; Node dir_arg |] } ->
        ( match pdir_arg ~wrap ~unwrap dir_arg with
          | Some pdir_arg ->
            let pdir_loc = Location Location.none in
            let pdir_name = Loc {txt = name; loc = Location.none } in
            let data = Record [| pdir_name; pdir_arg; pdir_loc |] in
            let tdir = wrap {name = "toplevel_directive"; data} in
            Some { node with data = Variant { tag; args = [| Node tdir |] } }
          | None -> None )
      | _ -> Some node

    let upgrade : _ History.conversion_function =
      fun node ~unwrap ~wrap ->
      match node.name with
      | "attribute" -> upgrade_attribute node
      | "core_type"
      | "pattern"
      | "expression" -> add_loc_stack node
      | "row_field" -> upgrade_row_field ~wrap ~unwrap node
      | "object_field" -> upgrade_object_field ~wrap ~unwrap node
      | "expression_desc" -> upgrade_expression_desc ~wrap ~unwrap node
      | "type_extension" -> upgrade_type_extension ~unwrap node
      | "class_type_desc" -> upgrade_class_type_desc ~wrap ~unwrap node
      | "class_expr_desc" -> upgrade_class_expr_desc ~wrap ~unwrap node
      | "signature_item_desc" -> upgrade_signature_item_desc ~wrap ~unwrap node
      | "open_description" -> upgrade_open_description ~wrap node
      | "structure_item_desc" -> upgrade_structure_item_desc ~wrap ~unwrap node
      | "toplevel_phrase" -> upgrade_toplevel_phrase ~wrap ~unwrap node
      | _ -> Some node
  end
end

let grammar = concat_map ~f:Grammar.update_type_decl Version_4_08.grammar

let to_4_08 : History.conversion =
  { src_version = version
  ; dst_version = Version_4_08.version
  ; f = Node.Up.upgrade
  }

let of_4_08 : History.conversion =
  { src_version = Version_4_08.version
  ; dst_version = version
  ; f = Node.Down.downgrade
  }

let conversions = [to_4_08; of_4_08]
