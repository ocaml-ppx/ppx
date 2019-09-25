module Fixed_ocaml = Migrate_parsetree.OCaml_407
module Fixed_ast = Migrate_parsetree.Ast_407
open Fixed_ast

let fixed = "407"

let make_str s =
  { Location.txt = s; loc = Location.none; }

let uncapitalize_str str =
  { str with Location.txt = String_utils.safe_uncapitalize str.Location.txt; }


let make_ident ?(modname = "") ~name () =
  if modname = "" then
    Longident.Lident name
  else
    Longident.Ldot (Lident modname, name)

let make_ident_loc loc ?(modname = "") ~name () =
  let txt = make_ident ~modname ~name () in
  { Location.txt; loc; }


let make_exp_apply loc ident args =
  Ast_helper.Exp.apply
    ~loc
    (Ast_helper.Exp.ident ~loc { txt = ident; loc; })
    (List.map (fun a -> Asttypes.Nolabel, a) args)

let make_exp_construct loc ident args =
  Ast_helper.Exp.construct
    ~loc
    { txt = ident; loc; }
    (match args with
     | []    -> None
     | [one] -> Some one
     | _     -> Some (Ast_helper.Exp.tuple args))

let make_exp_ident loc ?(modname = "") ~name () =
  Ast_helper.Exp.ident
    ~loc
    (make_ident_loc loc ~modname ~name ())

let make_exp_fun ~labelled ~param_name body =
  Ast_helper.Exp.fun_
    (if labelled then Labelled param_name else Nolabel)
    None
    (Ast_helper.Pat.var { txt = param_name; loc = Location.none; })
    body

let make_exp_funs ~labelled ~param_names body =
  List.fold_right
    (fun param_name acc ->
       make_exp_fun ~labelled ~param_name acc)
    param_names
    body

let make_exp_list l =
  let no_loc = Location.none in
  List.fold_right
    (fun expr acc ->
       make_exp_construct no_loc (make_ident ~name:"::" ()) [expr; acc])
    l
    (make_exp_construct no_loc (make_ident ~name:"[]" ()) [])


let make_pat_construct loc ident args =
  Ast_helper.Pat.construct
    ~loc
    { txt = ident; loc; }
    (match args with
     | []    -> None
     | [one] -> Some one
     | _     -> Some (Ast_helper.Pat.tuple args))

let make_pat_var loc ~name =
  Ast_helper.Pat.var
    ~loc
    { txt = name; loc; }


let make_typ_arrow typ1 typ2 =
  Ast_helper.Typ.arrow Nolabel typ1 typ2

let rec make_typ_arrows = function
  | [] ->
    assert false
  | [one] ->
    one
  | hd :: tl ->
    make_typ_arrow hd (make_typ_arrows tl)

let make_typ_constr ~module_name ~type_name ~type_params =
  let txt =
    if module_name = "" then
      Longident.Lident type_name
    else
      Longident.(Ldot (Lident module_name, type_name))
  in
  Ast_helper.Typ.constr { txt; loc = Location.none; } type_params

let make_typ_tuple l =
  Ast_helper.Typ.tuple l

let make_typ_var ~name =
  Ast_helper.Typ.var name


(* [qualify_core_type_desc ~types desc] qualifies types appearing in [desc]
   according to [types]. [types] is an association list from type names to
   their modules. If a type has no binding in [types], it remains unqualified. *)
let rec qualify_core_type_desc ~types desc =
  let open Parsetree in
  match desc with
  | Ptyp_any | Ptyp_var _ | Ptyp_extension _ ->
    desc
  | Ptyp_arrow (label, ctyp1, ctyp2) ->
    Ptyp_arrow (label,
                qualify_core_type ~types ctyp1,
                qualify_core_type ~types ctyp2)
  | Ptyp_tuple l ->
    Ptyp_tuple (qualify_core_type_list ~types l)
  | Ptyp_constr ({ txt = Lident id; _ } as lid, l) when List.mem_assoc id types ->
    Ptyp_constr ({ lid with txt = Ldot (Lident (List.assoc id types), id); },
                 qualify_core_type_list ~types l)
  | Ptyp_constr (lid, l) ->
    Ptyp_constr (lid, qualify_core_type_list ~types l)
  | Ptyp_object (l, closed) ->
    Ptyp_object (List.map
                   (function
                     | Otag (label, attrs, core_type) ->
                       Otag (label, attrs
                            , qualify_core_type ~types core_type)
                     | Oinherit core_type ->
                       Oinherit (qualify_core_type ~types core_type))
                   l,
                 closed)
  | Ptyp_class (lid, l) ->
    Ptyp_class (lid, qualify_core_type_list ~types l)
  | Ptyp_alias (ctyp, s) ->
    Ptyp_alias (qualify_core_type ~types ctyp, s)
  | Ptyp_variant (l, closed, labels) ->
    Ptyp_variant (List.map
                    (function
                      | Rtag (label, attrs, empty, l) ->
                        Rtag (label,
                              attrs,
                              empty,
                              List.map (qualify_core_type ~types) l)
                      | Rinherit ctyp ->
                        Rinherit (qualify_core_type ~types ctyp))
                    l,
                  closed,
                  labels)
  | Ptyp_poly (l, ctyp) ->
    Ptyp_poly (l, qualify_core_type ~types ctyp)
  | Ptyp_package (lid, l) ->
    Ptyp_package (lid,
                  List.map
                    (fun (lid, ctyp) ->
                       lid, qualify_core_type ~types ctyp)
                    l)

and qualify_core_type ~types ctyp =
  { ctyp with ptyp_desc = qualify_core_type_desc ~types ctyp.ptyp_desc; }

and qualify_core_type_list ~types l =
  List.map (qualify_core_type ~types) l
