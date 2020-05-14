open! Import
include Ppx_ast
include Current_ast

let esequence ~loc el =
  match el with
  | [] -> eunit ~loc
  | hd :: tl -> List.fold_left tl ~init:hd ~f:(fun acc e -> pexp_sequence ~loc acc e)
;;

let pconstruct cd arg =
  match%view cd with
  | { pcd_loc; pcd_name = { loc; txt }; _ } ->
    ppat_construct ~loc:pcd_loc (Located.lident ~loc txt) arg

let econstruct cd arg =
  match%view cd with
  | { pcd_loc; pcd_name = { loc; txt }; _ } ->
    pexp_construct ~loc:pcd_loc (Located.lident ~loc txt) arg

let pexp_tuple_opt ~loc l =
  match l with
  | [] -> None
  | _ :: _ -> Some (pexp_tuple ~loc l)

let ppat_tuple_opt ~loc l =
  match l with
  | [] -> None
  | _ :: _ -> Some (ppat_tuple ~loc l)

let pstr_value_list ~loc rec_flag = function
  | [] -> []
  | vbs -> [pstr_value ~loc rec_flag vbs]

let nonrec_type_declaration ~loc:_ ~name:_ ~params:_ ~cstrs:_ ~kind:_ ~private_:_
      ~manifest:_ =
  failwith "Ppx.Ast_builder.nonrec_type_declaration: don't use this function"
;;

let unapplied_type_constr_conv_without_apply ~loc (ident : Longid.t) ~f =
  match%view ident with
  | Lident n -> pexp_ident ~loc (Located.lident ~loc (f n))
  | Ldot (path, n) -> pexp_ident ~loc (Located.longident ~loc (Longid.ldot path (f n)))
  | Lapply _ -> Location.raise_errorf ~loc "unexpected applicative functor type"

let type_constr_conv ~loc:apply_loc longident_loc ~f args =
  match%view longident_loc with
  | { txt = longident; loc } ->
    match%view longident with
    | Lident _
    | Ldot ((Lident _ | Ldot _), _)
    | Lapply _ ->
      let ident = unapplied_type_constr_conv_without_apply longident ~loc ~f in
      begin match args with
      | [] -> ident
      | _ :: _ -> eapply ~loc:apply_loc ident args
      end
    | Ldot (Lapply _ as module_path, n) ->
      let suffix_n functor_ = String.uncapitalize functor_ ^ "__" ^ n in
      let rec gather_lapply functor_args : Longid.t -> Longid.t * _ = function%view
        | Lapply (rest, arg) ->
          gather_lapply (arg :: functor_args) rest
        | Lident functor_ ->
          Longid.lident (suffix_n functor_), functor_args
        | Ldot (functor_path, functor_) ->
          Longid.ldot functor_path (suffix_n functor_), functor_args
      in
      let ident, functor_args = gather_lapply [] module_path in
      eapply ~loc:apply_loc (unapplied_type_constr_conv_without_apply ident ~loc ~f)
        (List.map functor_args ~f:(fun path ->
           pexp_pack ~loc (pmod_ident ~loc (Located.longident ~loc path)))
         @ args)

let unapplied_type_constr_conv ~loc longident ~f =
  type_constr_conv longident ~loc ~f []

let include_infos ~loc:pincl_loc pincl_mod =
  Include_infos.create ~pincl_loc ~pincl_mod ~pincl_attributes:(Attributes.create [])

let eta_reduce =
  let rec gather_params acc expr =
    match%view expr with
    | { pexp_desc =
          Pexp_fun (label, None (* no default expression *), subpat, body)
      ; pexp_attributes = Attributes []
      ; pexp_loc = _
      } ->
      begin match%view subpat with
      | { ppat_desc = Ppat_var name; ppat_attributes = Attributes []; ppat_loc = _ } ->
        gather_params ((label, name, None) :: acc) body
      | { ppat_desc = Ppat_constraint ({ ppat_desc = Ppat_var name
                                       ; ppat_attributes = Attributes []
                                       ; ppat_loc = _ }, ty)
        ; ppat_attributes = Attributes []; ppat_loc = _ } ->
        (* We reduce [fun (x : ty) -> f x] by rewriting it [(f : ty -> _)]. *)
        gather_params ((label, name, Some ty) :: acc) body
      | _ -> List.rev acc, expr
      end
    | _ -> List.rev acc, expr
  in
  let annotate ~loc expr params =
    if List.exists params ~f:(fun (_, _, ty) -> Option.is_some ty)
    then
      let ty =
        List.fold_right params ~init:(ptyp_any ~loc)
          ~f:(fun (param_label, param, ty_opt) acc ->
            let loc = Loc.loc param in
            let ty =
              match ty_opt with
              | None -> ptyp_any ~loc
              | Some ty -> ty
            in
            ptyp_arrow ~loc param_label ty acc)
      in
      pexp_constraint ~loc expr ty
    else expr
  in
  let rec gather_args n x =
    if n = 0 then Some (x, [])
    else match%view x with
      | { pexp_desc = Pexp_apply (body, args)
        ; pexp_attributes = Attributes []; pexp_loc = _ } ->
        if List.length args <= n then
          match gather_args (n - List.length args) body with
          | None -> None
          | Some (body, args') ->
            Some (body, args' @ args)
        else
          None
      | _ -> None
  in
  fun expr ->
    match%view expr with
    | { pexp_loc; _ } ->
      let params, body = gather_params [] expr in
      match%view gather_args (List.length params) body with
      | None -> None
      | Some (({ pexp_desc = Pexp_ident _; _ } as f_ident), args) ->
        begin
          match
            List.for_all2 args params ~f:(fun (arg_label, arg) (param_label, param, _) ->
              (arg_label : arg_label) = param_label
              && match%view arg with
              | { pexp_desc = Pexp_ident (Longident_loc { txt = Lident name'; _ })
                ; pexp_attributes = Attributes []
                ; pexp_loc = _ }
                -> String.equal name' (Loc.txt param)
              | _ -> false)
          with
          | false -> None
          | true -> Some (annotate ~loc:pexp_loc f_ident params)
          | exception Invalid_argument _ -> assert false
        end
      | _ -> None
;;

let eta_reduce_if_possible expr = Option.value (eta_reduce expr) ~default:expr
let eta_reduce_if_possible_and_nonrec expr ~rec_flag =
  match%view rec_flag with
  | Recursive -> expr
  | Nonrecursive -> eta_reduce_if_possible expr
