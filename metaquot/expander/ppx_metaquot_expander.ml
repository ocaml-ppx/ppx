open! Stdppx
open Ppx_ast.V4_07
module Conversion = Ppx_ast.Conversion
module Traverse = Ppx_ast.Traverse.V4_07
module Traverse_builtins = Ppx_ast.Traverse_builtins

module type Driver = sig
  val assert_no_attributes : Ppx_ast.attributes -> unit
  val mark_attribute_as_handled_manually : Ppx_ast.attribute -> unit
end

module type Non_terminal = sig
  type t
  val cast : Extension.t -> t
  val location : Astlib.Location.t -> t
  val attributes : (Astlib.Location.t -> t) option
  class std_lifters : Astlib.Location.t -> [t] Traverse_builtins.std_lifters
  val extension : t Ppx_bootstrap.Extension.entry -> Ppx_bootstrap.Extension.t
end

module Make (Driver : Driver) (Non_terminal : Non_terminal) = struct
  let lift loc = object
    inherit [Non_terminal.t] Traverse.lift as super
    inherit! Non_terminal.std_lifters loc

    method! attribute x =
      Driver.mark_attribute_as_handled_manually x;
      super#attribute x

    method! location _ = Non_terminal.location loc
    method! attributes x =
      match Non_terminal.attributes with
      | None -> super#attributes x
      | Some f ->
        Driver.assert_no_attributes x; f loc

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
      | Some ext -> Non_terminal.cast ext
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
      | Some ext -> Non_terminal.cast ext
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
      | Some ext -> Non_terminal.cast ext
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
      | Some ext -> Non_terminal.cast ext
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
      | Some ext -> Non_terminal.cast ext
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
        Driver.assert_no_attributes attrs;
        Non_terminal.cast ext
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
        Driver.assert_no_attributes attrs;
        Non_terminal.cast ext
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
  | Some (x, _) -> Some (Astlib.Location.to_location (Astlib.Loc.loc x))
  | None -> None

let loc_of_expression attr =
  match Expression.to_concrete attr with
  | Some e -> Some (Astlib.Location.to_location e.pexp_loc)
  | None -> None

let ppat_any ~loc =
  Pattern.create
    ~ppat_loc:loc
    ~ppat_attributes:(Attributes.create [])
    ~ppat_desc:Pattern_desc.ppat_any

module Expr (Driver : Driver) = struct
  type t = Expression.t
  let location loc =
    Expression.create
      ~pexp_loc:loc
      ~pexp_attributes:(Attributes.create [])
      ~pexp_desc:(Expression_desc.pexp_ident
                    (Astlib.Loc.create ~loc ~txt:(Longident.lident "loc") ()))
  let attributes = None
  class std_lifters = Ppx_metaquot_lifters.expression_lifters
  let cast ext =
    match expr_payload_of ext with
    | Some (e, attrs) ->
      Driver.assert_no_attributes attrs;
      e
    | _ ->
      Location.raise_errorf ?loc:(loc_of_extension ext)
        "expression expected"
  let extension entry = Ppx_bootstrap.Extension.Expr entry
end

module Patt = struct
  type t = Pattern.t
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
  let extension entry = Ppx_bootstrap.Extension.Patt entry
end

module Extensions_for_nt (Driver : Driver) (Non_terminal : Non_terminal) = struct
  module Lift = Make (Driver) (Non_terminal)

  let extension name callback = Non_terminal.extension { name; callback }

  let extensions =
    [ extension "expr" (fun ~loc payload ->
        match Ppx_bootstrap.single_expression_payload payload with
        | Some (e, attrs) ->
          Driver.assert_no_attributes attrs;
          (Lift.lift loc)#expression e
        | None -> Ppx_bootstrap.Expected.raise_ ~loc "single-expression payload")
    ; extension "pat" (fun ~loc payload ->
        match Payload.to_concrete payload with
        | Some (PPat (p, None)) -> (Lift.lift loc)#pattern p
        | _ -> Ppx_bootstrap.Expected.raise_ ~loc "pattern without guard")
    ; extension "str" (fun ~loc payload ->
        match Payload.to_concrete payload with
        | Some (PStr s) -> (Lift.lift loc)#structure s
        | _ -> Ppx_bootstrap.Expected.raise_ ~loc "structure")
    ; extension "stri" (fun ~loc payload ->
        match Ppx_bootstrap.single_structure_item_payload payload with
        | Some s -> (Lift.lift loc)#structure_item s
        | _ -> Ppx_bootstrap.Expected.raise_ ~loc "structure item")
    ; extension "sig" (fun ~loc payload ->
        match Payload.to_concrete payload with
        | Some (PSig s) -> (Lift.lift loc)#signature s
        | _ -> Ppx_bootstrap.Expected.raise_ ~loc "signature")
    ; extension "sigi" (fun ~loc payload ->
        match Ppx_bootstrap.single_signature_item_payload payload with
        | Some s -> (Lift.lift loc)#signature_item s
        | _ -> Ppx_bootstrap.Expected.raise_ ~loc "signature item")
    ; extension "type" (fun ~loc payload ->
        match Payload.to_concrete payload with
        | Some (PTyp t) -> (Lift.lift loc)#core_type t
        | _ -> Ppx_bootstrap.Expected.raise_ ~loc "type")
    ]
end

module Extensions (Driver : Driver) = struct
  module Expr_extensions = Extensions_for_nt (Driver) (Expr (Driver))
  module Patt_extensions = Extensions_for_nt (Driver) (Patt)

  let extensions = Expr_extensions.extensions @ Patt_extensions.extensions
end
