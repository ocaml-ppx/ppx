open Ppx
open Stdppx
open Astlib
open Ppx_ast
open V4_07

let alphabet =
  Array.init (Char.code 'z' - Char.code 'a' + 1)
    ~f:(fun i -> String.make 1 (Char.chr (i + Char.code 'a')))
;;

let vars_of_list ~get_loc l =
  List.mapi l ~f:(fun i x -> ({ txt = alphabet.(i); loc = get_loc x } : _ Loc.t))

let evar_of_var = function%view { txt; loc } -> evar ~loc txt
let pvar_of_var = function%view { txt; loc } -> pvar ~loc txt
let tvar_of_var = function%view { txt; loc } -> ptyp_var ~loc txt

let evars_of_vars = List.map ~f:evar_of_var
let pvars_of_vars = List.map ~f:pvar_of_var
let tvars_of_vars = List.map ~f:tvar_of_var

let longident_loc string_loc =
  Longident_loc.create (Loc.map string_loc ~f:Longident.lident)

module Backends = struct
  class reconstructors = object
    method record ~loc flds = pexp_record ~loc flds None
    method construct ~loc id args =
      pexp_construct ~loc id
        (match args with
         | [] -> None
         | _  -> Some (etuple ~loc args))
    method tuple ~loc es = etuple ~loc es
  end

  class type what = object
    method name : string

    inherit reconstructors

    method class_params : loc:Location.t -> (core_type * variance) list

    method apply
      :  loc:Location.t
      -> expression
      -> expression list
      -> expression

    method abstract
      :  loc:Location.t
      -> pattern
      -> expression
      -> expression

    (* Basic combinator type *)
    method typ : loc:Location.t -> core_type -> core_type

    method any : loc:Location.t -> expression

    method combine
      :  loc:Location.t
      -> (string Loc.t * expression) list
      -> reconstruct:expression
      -> expression
  end

  let mapper : what = object
    method name = "map"

    inherit reconstructors

    method class_params ~loc:_ = []

    method apply ~loc expr args = eapply ~loc expr args
    method abstract ~loc patt expr = pexp_fun ~loc Arg_label.nolabel None patt expr

    method typ ~loc ty = ptyp_arrow ~loc Arg_label.nolabel ty ty

    method any ~loc = [%expr fun x -> x]

    method combine ~loc combinators ~reconstruct =
      List.fold_right combinators ~init:reconstruct ~f:(fun (v, expr) acc ->
        pexp_let ~loc Rec_flag.nonrecursive
          [value_binding ~loc ~pat:(pvar_of_var v) ~expr] acc)
  end

  let iterator : what = object
    method name = "iter"

    inherit reconstructors

    method class_params ~loc:_ = []

    method apply ~loc expr args = eapply ~loc expr args
    method abstract ~loc patt expr = pexp_fun ~loc Arg_label.nolabel None patt expr

    method typ ~loc ty = [%type: [%t ty] -> unit]

    method any ~loc = [%expr fun _ -> ()]

    method combine ~loc combinators ~reconstruct:_ =
      match List.rev combinators with
      | [] -> [%expr ()]
      | (_, expr) :: rest ->
        List.fold_left rest ~init:expr ~f:(fun acc (_v, expr) ->
          pexp_sequence ~loc expr acc)
  end

  let folder : what = object
    method name = "fold"

    inherit reconstructors

    method class_params ~loc = [(ptyp_var ~loc "acc", Variance.invariant)]

    method apply ~loc expr args = eapply ~loc expr (args @ [evar ~loc "acc"])
    method abstract ~loc patt expr =
      eabstract ~loc [patt; pvar ~loc "acc"] expr

    method typ ~loc ty = [%type: [%t ty] -> 'acc -> 'acc]

    method any ~loc = [%expr fun _ acc -> acc]

    method combine ~loc combinators ~reconstruct:_ =
      match combinators with
      | [(_, expr)] -> expr
      | _ ->
        List.fold_right combinators ~init:[%expr acc] ~f:(fun (_v, expr) acc ->
          [%expr
            let acc = [%e expr] in
            [%e acc]
          ])
  end

  let fold_mapper : what = object
    method name = "fold_map"

    inherit reconstructors

    method class_params ~loc = [(ptyp_var ~loc "acc", Variance.invariant)]

    method apply ~loc expr args = eapply ~loc expr (args @ [evar ~loc "acc"])
    method abstract ~loc patt expr = eabstract ~loc [patt; pvar ~loc "acc"] expr

    method typ ~loc ty = [%type: [%t ty] -> 'acc -> [%t ty] * 'acc]

    method any ~loc = [%expr fun x acc -> (x, acc)]

    method combine ~loc combinators ~reconstruct =
      List.fold_right combinators ~init:[%expr ([%e reconstruct], acc)]
        ~f:(fun (v, expr) acc ->
          [%expr
            let ([%p pvar_of_var v], acc) = [%e expr] in
            [%e acc]
          ])
  end

  exception Found
  let uses_var var =
    let iter = object
      inherit iter as super
      method! expression = function%view
        | Eident (Longident_loc { txt = Lident id; _ })
          when String.equal id var ->
          Exn.raise_notrace Found
        | e -> super#expression e
    end in
    fun e ->
      try
        iter#expression e;
        false
      with Found ->
        true
  ;;

  let mapper_with_context : what =
    let uses_ctx = uses_var "ctx" in
    object
      method name = "map_with_context"

      inherit reconstructors

      method class_params ~loc = [(ptyp_var ~loc "ctx", Variance.invariant)]

      method apply ~loc expr args = eapply ~loc expr (evar ~loc "ctx" :: args)
      method abstract ~loc patt expr =
        if uses_ctx expr then
          eabstract ~loc [pvar ~loc "ctx"; patt] expr
        else
          eabstract ~loc [pvar ~loc "_ctx"; patt] expr

      method typ ~loc ty = [%type: 'ctx -> [%t ty] -> [%t ty]]

      method any ~loc = [%expr fun _ctx x -> x]

      method combine ~loc combinators ~reconstruct =
        List.fold_right combinators ~init:reconstruct
          ~f:(fun (v, expr) acc ->
            [%expr
              let [%p pvar_of_var v] = [%e expr] in
              [%e acc]
            ])
    end

  let rec string_of_lid id =
    match%view id with
    | Lident name -> name
    | Ldot (id, name) -> string_of_lid id ^ ("." ^ name)
    | Lapply (id_f, id_x) -> string_of_lid id_f ^ ("(" ^ string_of_lid id_x ^ ")")

  let lifter : what = object
    method name = "lift"

    method class_params ~loc = [(ptyp_var ~loc "res", Variance.invariant)]

    method apply ~loc expr args = eapply ~loc expr args
    method abstract ~loc patt expr = pexp_fun ~loc Arg_label.nolabel None patt expr

    method typ ~loc ty = [%type: [%t ty] -> 'res]

    method any ~loc = [%expr self#other]

    method combine ~loc combinators ~reconstruct =
      List.fold_right combinators ~init:reconstruct ~f:(fun (v, expr) acc ->
        pexp_let ~loc Rec_flag.nonrecursive
          [value_binding ~loc ~pat:(pvar_of_var v) ~expr] acc)

    method record ~loc flds =
      let flds =
        elist ~loc
          (List.map flds ~f:(function%view
             | Longident_loc {loc; txt}, ({ pexp_loc; _ } as e) ->
               etuple
                 ~loc:{ loc with loc_end = pexp_loc.loc_end }
                 [ estring ~loc (string_of_lid txt)
                 ; e
                 ]))
      in
      [%expr self#record [%e flds]]
    method construct ~loc id args =
      let args = elist ~loc args in
      match%view id with
      | Longident_loc {loc; txt} ->
      [%expr
        self#constr
          [%e estring ~loc:loc (string_of_lid txt)]
          [%e args]]
    method tuple ~loc es =
      [%expr self#tuple [%e elist ~loc es]]
  end

  let all = [mapper; iterator; folder; fold_mapper; mapper_with_context; lifter]
end
type what = Backends.what

let mapper_type ~(what:what) ~loc type_name params =
  let vars = vars_of_list params ~get_loc:(function%view { ptyp_loc; _ } -> ptyp_loc) in
  let params = tvars_of_vars vars in
  let ty = ptyp_constr ~loc (longident_loc type_name) params in
  let ty =
    List.fold_right params ~init:(what#typ ~loc ty)
      ~f:(fun param ty ->
        match%view param with
        | { ptyp_loc = loc; _ } ->
          ptyp_arrow ~loc Arg_label.nolabel (what#typ ~loc param) ty)
  in
  ptyp_poly ~loc vars ty
;;

let constrained_mapper ~(what:what) ?(is_gadt=false) mapper td =
  match%view td, mapper with
  | { ptype_name; ptype_params; ptype_loc; _ }, { pexp_loc; _ } ->
    let vars =
      vars_of_list ptype_params ~get_loc:(function%view ({ptyp_loc; _}, _) -> ptyp_loc)
    in
    let make_type params =
      let loc = ptype_loc in
      let ty = ptyp_constr ~loc (longident_loc ptype_name) params in
      List.fold_right params ~init:(what#typ ~loc:ptype_loc ty)
        ~f:(fun param ty ->
          match%view param with
          | { ptyp_loc = loc; _ } ->
            ptyp_arrow ~loc Arg_label.nolabel (what#typ ~loc param) ty)
    in
    let typ =
      let loc = ptype_loc in
      ptyp_poly ~loc vars (make_type (tvars_of_vars vars))
    in
    let mapper =
      if false || is_gadt then
        let typs =
          List.map vars ~f:(fun (v : _ Loc.t) ->
            ptyp_constr ~loc:v.loc (longident_loc v) [])
        in
        List.fold_right vars
          ~init:(pexp_constraint ~loc:pexp_loc mapper (make_type typs))
          ~f:(fun (v : _ Loc.t) e -> pexp_newtype ~loc:v.loc v e)
      else
        mapper
    in
    pexp_poly ~loc:pexp_loc mapper (Some typ)
;;

let mapper_type_of_td ~what td =
  match%view td with
  | { ptype_name; ptype_params; ptype_loc; _ } ->
    mapper_type ~what ~loc:ptype_loc ptype_name (List.map ptype_params ~f:fst)
;;

let method_name = function%view
  | Lident s -> String.lowercase s
  | Ldot (_, b) -> b
  | Lapply _ -> assert false
;;

let rec type_expr_mapper ~(what:what) te =
  match%view te with
  | { ptyp_loc=loc; ptyp_desc; _ } ->
    match%view ptyp_desc with
    | Ptyp_var s -> evar ~loc ("_" ^ s)
    | Ptyp_tuple tes ->
      let vars = vars_of_list tes ~get_loc:(function%view { ptyp_loc; _ } -> ptyp_loc) in
      let deconstruct = ptuple ~loc (pvars_of_vars vars) in
      let reconstruct = what#tuple ~loc (evars_of_vars vars) in
      let mappers = map_variables ~what vars tes in
      what#abstract ~loc deconstruct (what#combine ~loc mappers ~reconstruct)
    | Ptyp_constr (Longident_loc path, params) ->
      let f lident = method_name lident in
      let map = pexp_send ~loc (evar ~loc "self") (Loc.map path ~f) in
      (match params with
       | [] -> map
       | _  ->
         eapply ~loc map
           (List.map params
              ~f:(fun te ->
                type_expr_mapper ~what te)))
    | _ -> what#any ~loc

and map_variables ~(what:what) vars tes =
  List.map2 tes vars ~f:(fun te var ->
    match%view te with
    | { ptyp_loc; _ } ->
      (var,
       what#apply ~loc:ptyp_loc (type_expr_mapper ~what te)
         [evar_of_var var]))
;;

let gen_record' ~(what:what) ~loc lds =
  let vars = List.map lds ~f:(function%view { pld_name; _ } -> pld_name) in
  let deconstruct =
    ppat_record ~loc
      (List.map vars ~f:(fun v -> (longident_loc v, pvar_of_var v)))
      Closed_flag.closed
  in
  let reconstruct =
    what#record ~loc
      (List.map vars ~f:(fun v -> (longident_loc v, evar_of_var v)))
  in
  let mappers =
    map_variables ~what
      vars
      (List.map lds ~f:(function%view { pld_type; _ } -> pld_type))
  in
  deconstruct, reconstruct, mappers
;;

let gen_record ~(what:what) ~loc lds =
  let deconstruct, reconstruct, mappers =
    gen_record' ~what lds ~loc
  in
  what#abstract ~loc deconstruct (what#combine ~loc mappers ~reconstruct)
;;

let is_constant_constructor = function%view
  | { pcd_args = Pcstr_tuple []; _ } -> true
  | _ -> false

let erase_type_variables = object
  inherit map as super

  method! core_type core_type =
    match%view core_type with
    | { ptyp_loc = loc; ptyp_desc = Ptyp_var _; _ } -> ptyp_any ~loc
    | x -> super#core_type x
end

let gen_variant ~(what:what) ~loc cds =
  if not (String.equal what#name "lift") &&
     List.for_all cds ~f:is_constant_constructor
  then
    what#any ~loc
  else
    let cases =
      List.map cds ~f:(function%view { pcd_name; pcd_loc; pcd_res; pcd_args; _ } ->
        let cstr = longident_loc pcd_name in
        let loc = pcd_loc in
        let args =
          match pcd_res with
          | None -> pcd_args
          | Some _ ->
            (* This is a big sur-approximation but it's enough for our only use of GADTs
               in ppx_custom_format *)
            erase_type_variables#constructor_arguments pcd_args
        in
        match%view args with
        | Pcstr_tuple args ->
          let vars =
            vars_of_list args ~get_loc:(function%view { ptyp_loc; _ } -> ptyp_loc)
          in
          let deconstruct =
            ppat_construct cstr ~loc
              (match vars with
               | [] -> None
               | _ -> Some (ptuple ~loc (pvars_of_vars vars)))
          in
          let reconstruct =
            what#construct cstr ~loc (evars_of_vars vars)
          in
          let mappers =
            map_variables ~what vars args
          in
          case ~lhs:deconstruct ~rhs:(what#combine ~loc mappers ~reconstruct) ~guard:None
        | Pcstr_record labels ->
          let deconstruct, reconstruct, mappers =
            gen_record' ~loc ~what labels
          in
          let deconstruct = ppat_construct ~loc cstr (Some deconstruct) in
          let reconstruct = what#construct ~loc cstr [reconstruct] in
          case ~lhs:deconstruct ~rhs:(what#combine ~loc mappers ~reconstruct) ~guard:None)
    in
    what#abstract ~loc (pvar ~loc "x") (pexp_match ~loc (evar ~loc "x") cases)

let gen_mapper ~(what:what) td =
  match%view td with
  | { ptype_loc = loc; ptype_kind; ptype_manifest; ptype_params; _ } ->
    let body =
      match%view ptype_kind with
      | Ptype_open -> what#any ~loc
      | Ptype_record  lds -> gen_record  ~what lds ~loc
      | Ptype_variant cds -> gen_variant ~what cds ~loc
      | Ptype_abstract ->
        match ptype_manifest with
        | None -> what#any ~loc
        | Some te -> type_expr_mapper ~what te
    in
    List.fold_right ptype_params ~init:body ~f:(fun (ty, _) acc ->
      match%view ty with
      | { ptyp_loc = loc; ptyp_desc = Ptyp_var s; _ } ->
        pexp_fun ~loc Arg_label.nolabel None (pvar ~loc ("_" ^ s)) acc
      | { ptyp_loc = loc; _ } ->
        pexp_fun ~loc Arg_label.nolabel None (ppat_any ~loc) acc)
;;

module Longident_map = Map.Make(struct
    include Longident

    let compare = compare
  end)

let type_deps =
  let collect = object
    inherit [int Longident_map.t] fold as super
    method! core_type t acc =
      let acc =
        match%view t with
        | Tconstr (Longident_loc id, vars) ->
          Longident_map.add acc id.txt (List.length vars)
        | _ -> acc
      in
      super#core_type t acc
  end in
  fun tds ->
    let empty = Longident_map.empty in
    let map =
      List.fold_left tds ~init:empty ~f:(fun map td ->
        match%view td with
        | { ptype_kind; ptype_manifest; _ } ->
          let map = collect#type_kind ptype_kind map in
          match%view ptype_kind, ptype_manifest with
          | Ptype_abstract, Some ty -> collect#core_type ty map
          | _ -> map)
    in
    let map =
      List.fold_left tds ~init:map ~f:(fun map td ->
        match%view td with
        | { ptype_name; _ } ->
          Longident_map.remove map (Longident.lident ptype_name.txt))
    in
    Longident_map.to_list map

let lift_virtual_methods ~loc methods =
  let collect = object
    inherit [String.Set.t] fold as super

    method! expression x acc =
      match%view x with
      | Esend (_, ({ txt = "tuple"|"record"|"constr"|"other" as s; loc = _; })) ->
        String.Set.add acc s
      | _ -> super#expression x acc
  end in
  let used = collect#list collect#class_field methods String.Set.empty in
  let all_virtual_methods =
    match%view
      [%stri
        class virtual blah = object
          method virtual record : (string * 'res) list -> 'res
          method virtual constr : string -> 'res list -> 'rest
          method virtual tuple : 'res list -> 'res
          method virtual other : 'a. 'a -> 'res
        end
      ]
    with
    | Strclass
        [ Class_declaration { pci_expr = Cestructure { pcstr_fields = l; _ } ; _ } ] ->
      l
    | _ -> assert false
  in
  List.filter all_virtual_methods ~f:(fun m ->
    match%view m with
    | Cfmethod (s, _, _) -> String.Set.mem used s.txt
    | _ -> false)

let map_lident id ~f =
  match%view id with
  | Lident s -> Longident.lident (f s)
  | Ldot (id, s) -> Longident.ldot id (f s)
  | Lapply _ -> assert false

let lident_last_exn id =
  match%view id with
  | Lident s -> s
  | Ldot (_, s) -> s
  | Lapply _ -> assert false

let class_constr ~what ~class_params id =
  let longident_loc =
    Longident_loc.create
      (Loc.map id ~f:(map_lident ~f:(fun s -> what#name ^ "_" ^ s)))
  in
  pcl_constr ~loc:id.loc
    longident_loc
    (List.map class_params ~f:fst)

let gen_class ~(what:what) ~loc tds =
  let class_params = what#class_params ~loc in
  let virtual_methods =
    List.map (type_deps tds) ~f:(fun (id, arity) ->
      let (id : _ Loc.t) = { txt = lident_last_exn id; loc } in
      pcf_method ~loc
        (id,
         Private_flag.public,
         Class_field_kind.cfk_virtual
           (mapper_type ~what ~loc id
              (List.init arity ~f:(fun _ -> ptyp_any ~loc)))))
  in
  let methods =
    List.map tds ~f:(fun td ->
      match%view td with
      | { ptype_loc = loc; ptype_kind; ptype_name; _ } ->
        let mapper = gen_mapper ~what td in
        let is_gadt =
          match%view ptype_kind with
          | Ptype_variant cds ->
            List.exists cds ~f:(function%view { pcd_res } -> Option.is_some pcd_res)
          | _ -> false
        in
        let mapper = constrained_mapper ~what ~is_gadt mapper td in
        pcf_method ~loc
          (ptype_name,
           Private_flag.public,
           Class_field_kind.cfk_concrete Override_flag.fresh mapper))
  in
  let virtual_methods =
    if String.equal what#name "lift"
    then
      lift_virtual_methods ~loc methods @ virtual_methods
    else
      virtual_methods
  in
  let virt =
    if List.is_empty virtual_methods then Virtual_flag.concrete else Virtual_flag.virtual_
  in
  Class_infos.create
    ~pci_attributes:(Attributes.create [])
    ~pci_loc:loc
    ~pci_virt:virt
    ~pci_params:class_params
    ~pci_name:{ loc; txt = what#name }
    ~pci_expr:(pcl_structure ~loc
             (class_structure
                ~self:(ppat_var ~loc { txt = "self"; loc })
                ~fields:(virtual_methods @ methods)))

let gen_str ~what ~loc ~path:_ (rf, tds) =
  (match%view rf with
   | Nonrecursive ->
     (* The method name would clash... *)
     Location.Error.report
       Format.err_formatter
       (Location.Error.createf ~loc "ppx_traverse doesn't support nonrec");
     assert false
   | Recursive -> ());
  match%view gen_class ~loc ~what tds with
  | { pci_loc = loc; _ } as cl ->
    let class_decl = Class_declaration.create cl in
    Structure.create [ pstr_class ~loc [class_decl] ]

let () =
  let derivers =
    List.map Backends.all ~f:(fun what ->
      Deriving.add ("traverse_" ^ what#name)
        ~str_type_decl:(Deriving.Generator.make_noarg (gen_str ~what)))
  in
  Deriving.add_alias "traverse" (List.rev derivers)
  |> Deriving.ignore
