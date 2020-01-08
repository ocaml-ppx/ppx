open! Stdppx
open Ppx_ast.V4_07
module Conversion = Ppx_ast.Conversion
module Traverse = Ppx_ast.Traverse.V4_07
module Traverse_builtins = Ppx_ast.Traverse_builtins

module Make(M : sig
    type result
    val cast : Extension.t -> result
    val location : Location.t -> result
    val attributes : (Location.t -> result) option
    class std_lifters : Location.t -> [result] Traverse_builtins.std_lifters
  end) = struct
  let lift loc = object
    inherit [M.result] Traverse.lift as super
    inherit! M.std_lifters loc

    method! attribute x =
      Ppx.Attribute.mark_as_handled_manually (Conversion.ast_to_attribute x);
      super#attribute x

    method! location _ = M.location loc
    method! attributes x =
      match M.attributes with
      | None -> super#attributes x
      | Some f ->
        Ppx.assert_no_attributes (Conversion.ast_to_attributes x); f loc

    method! expression e =
      let expression_escape e =
        match Expression.to_concrete e with
        | Some e ->
          (match Expression_desc.to_concrete e.pexp_desc with
           | Some (Pexp_extension ext) ->
             (match Extension.to_concrete ext with
              | Some (loc, _) when String.equal "e" (Astlib.Loc.txt loc) -> Some ext
              | Some _ | None -> None)
           | Some _ | None -> None)
        | None -> None
      in
      match expression_escape e with
      | Some ext -> M.cast ext
      | _ -> super#expression e

    method! pattern p =
      let pattern_escape p =
        match Pattern.to_concrete p with
        | Some p ->
          (match Pattern_desc.to_concrete p.ppat_desc with
           | Some (Ppat_extension ext) ->
             (match Extension.to_concrete ext with
              | Some (loc, _) when String.equal "p" (Astlib.Loc.txt loc) -> Some ext
              | Some _ | None -> None)
           | Some _ | None -> None)
        | None -> None
      in
      match pattern_escape p with
      | Some ext -> M.cast ext
      | _ -> super#pattern p

    method! core_type t =
      let core_type_escape t =
        match Core_type.to_concrete t with
        | Some t ->
          (match Core_type_desc.to_concrete t.ptyp_desc with
           | Some (Ptyp_extension ext) ->
             (match Extension.to_concrete ext with
              | Some (loc, _) when String.equal "t" (Astlib.Loc.txt loc) -> Some ext
              | Some _ | None -> None)
           | Some _ | None -> None)
        | None -> None
      in
      match core_type_escape t with
      | Some ext -> M.cast ext
      | _ -> super#core_type t

    method! module_expr m =
      let module_expr_escape m =
        match Module_expr.to_concrete m with
        | Some m ->
          (match Module_expr_desc.to_concrete m.pmod_desc with
           | Some (Pmod_extension ext) ->
             (match Extension.to_concrete ext with
              | Some (loc, _) when String.equal "m" (Astlib.Loc.txt loc) -> Some ext
              | Some _ | None -> None)
           | Some _ | None -> None)
        | None -> None
      in
      match module_expr_escape m with
      | Some ext -> M.cast ext
      | _ -> super#module_expr m

    method! module_type m =
      let module_type_escape m =
        match Module_type.to_concrete m with
        | Some m ->
          (match Module_type_desc.to_concrete m.pmty_desc with
           | Some (Pmty_extension ext) ->
             (match Extension.to_concrete ext with
              | Some (loc, _) when String.equal "m" (Astlib.Loc.txt loc) -> Some ext
              | Some _ | None -> None)
           | Some _ | None -> None)
        | None -> None
      in
      match module_type_escape m with
      | Some ext -> M.cast ext
      | _ -> super#module_type m

    method! structure_item i =
      let structure_item_escape i =
        match Structure_item.to_concrete i with
        | Some i ->
          (match Structure_item_desc.to_concrete i.pstr_desc with
           | Some (Pstr_extension (ext, attrs)) ->
             (match Extension.to_concrete ext with
              | Some (loc, _) when String.equal "i" (Astlib.Loc.txt loc) ->
                Some (ext, attrs)
              | Some _ | None -> None)
           | Some _ | None -> None)
        | None -> None
      in
      match structure_item_escape i with
      | Some (ext, attrs) ->
        Ppx.assert_no_attributes (Conversion.ast_to_attributes attrs);
        M.cast ext
      | _ -> super#structure_item i

    method! signature_item i =
      let signature_item_escape i =
        match Signature_item.to_concrete i with
        | Some i ->
          (match Signature_item_desc.to_concrete i.psig_desc with
           | Some (Psig_extension (ext, attrs)) ->
             (match Extension.to_concrete ext with
              | Some (loc, _) when String.equal "i" (Astlib.Loc.txt loc) ->
                Some (ext, attrs)
              | Some _ | None -> None)
           | Some _ | None -> None)
        | None -> None
      in
      match signature_item_escape i with
      | Some (ext, attrs) ->
        Ppx.assert_no_attributes (Conversion.ast_to_attributes attrs);
        M.cast ext
      | _ -> super#signature_item i
  end
end

let payload_of ext =
  match Extension.to_concrete ext with
  | Some (_, payload) -> Payload.to_concrete payload
  | None -> None

let expr_payload_of ext =
  match payload_of ext with
  | Some (PStr structure) ->
    (match Structure.to_concrete structure with
     | Some [item] ->
       (match Structure_item.to_concrete item with
        | Some item ->
          (match Structure_item_desc.to_concrete item.pstr_desc with
           | Some (Pstr_eval (e, attrs)) -> Some (e, attrs)
           | Some _ | None -> None)
        | None -> None)
     | Some _ | None -> None)
  | Some _ | None -> None

let pattern_payload_of ext =
  match payload_of ext with
  | Some (PPat (pat, option)) -> Some (pat, option)
  | Some _ | None -> None

let loc_of_extension attr =
  match Extension.to_concrete attr with
  | Some (x, _) -> Some (Astlib.Loc.loc x)
  | None -> None

let loc_of_expression attr =
  match Expression.to_concrete attr with
  | Some e -> Some e.pexp_loc
  | None -> None

let ppat_any ~loc =
  Pattern.create
    ~ppat_loc:loc
    ~ppat_attributes:(Attributes.create [])
    ~ppat_desc:Pattern_desc.ppat_any

module Expr = Make(struct
    type result = Expression.t
    let location loc =
      Expression.create
        ~pexp_loc:loc
        ~pexp_attributes:(Attributes.create [])
        ~pexp_desc:(Expression_desc.pexp_ident
                      (Longident_loc.create
                         (Astlib.Loc.create ~loc ~txt:(Longident.lident "loc") ())))
    let attributes = None
    class std_lifters = Ppx_metaquot_lifters.expression_lifters
    let cast ext =
      match expr_payload_of ext with
      | Some (e, attrs) ->
        Ppx.assert_no_attributes (Conversion.ast_to_attributes attrs);
        e
      | _ ->
        Location.raise_errorf ?loc:(loc_of_extension ext)
          "expression expected"
  end)

module Patt = Make(struct
    type result = Pattern.t
    let location loc = ppat_any ~loc
    let attributes = Some (fun loc -> ppat_any ~loc)
    class std_lifters = Ppx_metaquot_lifters.pattern_lifters
    let cast ext =
      match pattern_payload_of ext with
      | Some (p, None) -> p
      | Some (_, Some e) ->
        Location.raise_errorf ?loc:(loc_of_expression e)
          "guard not expected here"
      | _ ->
        Location.raise_errorf ?loc:(loc_of_extension ext)
          "pattern expected"
  end)

let () =
  let module A = Ppx.Ast_pattern in
  let module E = Ppx.Extension in
  let extensions ctx lifter convert =
    [ E.declare "expr" ctx A.(single_expr_payload __)
        (fun ~loc ~path:_ e ->
           e
           |> Conversion.ast_of_expression
           |> (lifter loc)#expression
           |> convert)
    ; E.declare "pat"  ctx A.(ppat __ none)
        (fun ~loc ~path:_ p ->
           p
           |> Conversion.ast_of_pattern
           |> (lifter loc)#pattern
           |> convert)
    ; E.declare "str"  ctx A.(pstr __)
        (fun ~loc ~path:_ s ->
           s
           |> Conversion.ast_of_structure
           |> (lifter loc)#structure
           |> convert)
    ; E.declare "stri"  ctx A.(pstr (__ ^:: nil))
        (fun ~loc ~path:_ s ->
           s
           |> Conversion.ast_of_structure_item
           |> (lifter loc)#structure_item
           |> convert)
    ; E.declare "sig"  ctx A.(psig __)
        (fun ~loc ~path:_ s ->
           s
           |> Conversion.ast_of_signature
           |> (lifter loc)#signature
           |> convert)
    ; E.declare "sigi"  ctx A.(psig (__ ^:: nil))
        (fun ~loc ~path:_ s ->
           s
           |> Conversion.ast_of_signature_item
           |> (lifter loc)#signature_item
           |> convert)
    ; E.declare "type"  ctx A.(ptyp __)
        (fun ~loc ~path:_ t ->
           t
           |> Conversion.ast_of_core_type
           |> (lifter loc)#core_type
           |> convert)
    ]
  in
  let extensions =
    extensions Expression Expr.lift Conversion.ast_to_expression @
    extensions Pattern    Patt.lift Conversion.ast_to_pattern
  in
  Ppx.Driver.register_transformation
    "metaquot"
    ~extensions
