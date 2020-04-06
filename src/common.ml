open! Import
open Ast_builder

let lident x = Longident.lident x

let core_type_of_type_declaration td =
  match%view td with
  | { ptype_name; ptype_params; _ } ->
    let loc = ptype_name.loc in
    ptyp_constr ~loc
      (Located.map_lident ptype_name)
      (List.map ptype_params ~f:fst)
;;

let gen_symbol =
  let cnt = ref 0 in
  fun ?(prefix = "_x") () ->
    cnt := !cnt + 1;
    Printf.sprintf "%s__%03i_" prefix !cnt
;;

let name_type_params_in_td (td : type_declaration) : type_declaration =
  let name_param (tp, variance) =
    let ptyp_desc =
      match%view tp with
      | { ptyp_desc = Ptyp_any; _ } -> Core_type_desc.ptyp_var ("v" ^ gen_symbol ())
      | { ptyp_desc = Ptyp_var _ as v; _ } -> v
      | { ptyp_loc; _ } -> Location.raise_errorf ~loc:ptyp_loc "not a type parameter"
    in
    (Core_type.update tp ~ptyp_desc, variance)
  in
  match%view td with
  | { ptype_params; _ } ->
    Type_declaration.update td ~ptype_params:(List.map ptype_params ~f:name_param)
;;

let combinator_type_of_type_declaration td ~f =
  let td = name_type_params_in_td td in
  let result_type = f ~loc:(Type_declaration.ptype_name td).loc (core_type_of_type_declaration td) in
  List.fold_right (Type_declaration.ptype_params td) ~init:result_type ~f:(fun (tp, _variance) acc ->
    let loc = (Core_type.ptyp_loc tp) in
    ptyp_arrow ~loc Arg_label.nolabel (f ~loc tp) acc)
;;

let string_of_core_type ct =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Ocaml_common.Pprintast.core_type ppf (Conversion.ast_to_core_type ct);
  Format.pp_print_flush ppf ();
  Buffer.contents buf
;;

let get_type_param_name (ty, _) =
  let loc = (Core_type.ptyp_loc ty) in
  match%view (Core_type.ptyp_desc ty) with
  | Ptyp_var name -> Located.mk ~loc name
  | _ -> Location.raise_errorf ~loc "not a type parameter"


exception Type_is_recursive

class type_is_recursive rec_flag tds = object(self)
  inherit Ast_traverse.iter as super

  val type_names : string list = List.map tds ~f:(fun td -> (Type_declaration.ptype_name td).txt)

  method return_true () = Exn.raise_notrace Type_is_recursive

  method! core_type ctype =
    match%view (Core_type.ptyp_desc ctype) with
    | Ptyp_arrow _ -> ()
    | Ptyp_constr (Longident_loc { txt = Lident id; _ }, _)
      when List.mem ~set:type_names id ->
      self#return_true ()
    | _ -> super#core_type ctype

  method! constructor_declaration cd =
    (* Don't recurse through (Constructor_declaration.pcd_res cd) *)
    match%view (Constructor_declaration.pcd_args cd) with
    | Pcstr_tuple args -> List.iter args ~f:self#core_type
    | Pcstr_record fields -> List.iter fields ~f:self#label_declaration

  method go () =
    match%view rec_flag with
    | Nonrecursive -> Rec_flag.nonrecursive
    | Recursive    ->
      match List.iter tds ~f:self#type_declaration with
      | exception Type_is_recursive -> Rec_flag.recursive
      | () -> Rec_flag.nonrecursive

end

let really_recursive rec_flag tds = (new type_is_recursive rec_flag tds)#go ()

let rec last x l =
  match l with
  | [] -> x
  | x :: l -> last x l
;;

let loc_of_payload ((name : _ Loc.t), payload) =
  match%view payload with
  | PStr (Structure [])          -> name.loc
  | PStr (Structure (x :: l))    ->
    { (Structure_item.pstr_loc x) with loc_end = (Structure_item.pstr_loc (last x l)).loc_end }
  | PSig (Signature [])          -> name.loc
  | PSig (Signature (x :: l))    ->
    { (Signature_item.psig_loc x) with loc_end = (Signature_item.psig_loc (last x l)).loc_end }
  | PTyp t           -> (Core_type.ptyp_loc t)
  | PPat (x, None)   -> (Pattern.ppat_loc x)
  | PPat (x, Some e) -> { (Pattern.ppat_loc x) with loc_end = (Expression.pexp_loc e).loc_end }
;;

let loc_of_attribute = function%view (Attribute (name, payload)) ->
  (* TODO: fix this in the compiler *)
  (* "ocaml.doc" attributes are generated with [Location.none], which is not helpful for
     error messages. *)
  if name.loc = Location.none then
    loc_of_payload (name, payload)
  else
    { name.loc with loc_end = (loc_of_payload (name, payload)).loc_end }
;;

let curry_applications expr =
  match%view (Expression.pexp_desc expr) with
  | Pexp_apply (f,orig_forward_args) ->
    let loc = (Expression.pexp_loc expr) in
    let rec loop = function
      | [] -> f
      | last_arg::rev_front_args -> pexp_apply ~loc (loop rev_front_args) [last_arg]
    in
    loop (List.rev orig_forward_args)
  | _ -> expr
;;

let rec assert_no_attributes_in_list list =
  match%view list with
  | [] -> ()
  | Attribute (name, _) :: rest when Name.ignore_checks name.Loc.txt ->
    assert_no_attributes_in_list rest
  | attr :: _ ->
    let loc = loc_of_attribute attr in
    Location.raise_errorf ~loc "Attributes not allowed here"

let assert_no_attributes = function%view (Attributes list) ->
  assert_no_attributes_in_list list

let assert_no_attributes_in = object
  inherit Ast_traverse.iter

  method! attribute a = assert_no_attributes_in_list [a]
end

let attribute_of_warning loc s =
  Ast.Attribute.create
    ({ loc; txt = "ocaml.ppwarning" },
     Payload.pStr
       (Structure.create [pstr_eval ~loc (estring ~loc s) (Attributes.create [])]))

let is_polymorphic_variant =
  let rec check core_type =
    match%view core_type with
    | { ptyp_desc = Ptyp_variant _; _ } -> `Definitely
    | { ptyp_desc = Ptyp_alias (typ,_); _ } -> check typ
    | { ptyp_desc = Ptyp_constr _; _ } -> `Maybe
    | _ -> `Surely_not (* Type vars go here even though they could be polymorphic
                          variants, however we don't handle it if they get substituted
                          by a polymorphic variant that is then included. *)
  in
  fun td ~sig_ ->
    match%view (Type_declaration.ptype_kind td) with
    | Ptype_variant _ | Ptype_record _ | Ptype_open -> `Surely_not
    | Ptype_abstract ->
      match (Type_declaration.ptype_manifest td) with
      | None -> if sig_ then `Maybe else `Surely_not
      | Some typ -> check typ

let mk_named_sig ~loc ~sg_name ~handle_polymorphic_variant = function
  | [ td ] when String.equal (Type_declaration.ptype_name td).txt "t" && List.is_empty (Type_declaration.ptype_cstrs td) ->
    if not handle_polymorphic_variant &&
       (is_polymorphic_variant td ~sig_:true) = `Definitely
    then
      None
    else
      let arity = List.length (Type_declaration.ptype_params td) in
      if arity >= 4 then
        None
      else
        let mty =
          if arity = 0
          then sg_name
          else Printf.sprintf "%s%d" sg_name arity
        in
        let td = name_type_params_in_td td in
        let for_subst =
          type_declaration
            ~loc
            ~name:(Type_declaration.ptype_name td)
            ~params:(Type_declaration.ptype_params td)
            ~manifest:
              (Some
                 (ptyp_constr ~loc (Located.map_lident (Type_declaration.ptype_name td))
                    (List.map ~f:fst (Type_declaration.ptype_params td))))
            ~cstrs:[]
            ~kind:Type_kind.ptype_abstract
            ~private_:Private_flag.public
        in
        Some (
          include_infos ~loc
            (pmty_with ~loc (pmty_ident ~loc (Located.lident mty ~loc))
               [With_constraint.pwith_typesubst (Located.lident ~loc "t") for_subst])
          |> Include_description.create
        )
  | _ -> None
