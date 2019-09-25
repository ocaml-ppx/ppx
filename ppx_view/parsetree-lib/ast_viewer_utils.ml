open Migrate_parsetree.Ast_407
open Viewlib

module Attribute = struct

  module StringLocationSet = Set.Make (struct
      [@@@ocaml.warning "-3"]
      type t = string Location.loc
      let compare = Pervasives.compare
    end)

  let used_attributes = ref StringLocationSet.empty

  let add_used_attribute (attr_name, _) =
    used_attributes := StringLocationSet.add attr_name !used_attributes

  let has_been_used (attr_name, _) =
    StringLocationSet.mem attr_name !used_attributes

  let rec choice_over_attributes l view value =
    match l with
    | ({ Location.txt; loc }, payload) as attr :: tl ->
      (View.choice
         ((fun value vars ->
            View.(>>|)
              (view (value, txt, loc, payload, l) vars)
              (fun x -> add_used_attribute attr; x)))
         (choice_over_attributes tl view))
        value
    | [] ->
      View.error

  type ('a, 'i, 'o) view =
       ('a * string * Location.t * Parsetree.payload * Parsetree.attributes, 'i, 'o) View.t
    -> ('a, 'i, 'o) View.t

  let core_type view value =
    choice_over_attributes value.Parsetree.ptyp_attributes view value

  let row_field view value =
    match value with
    | Parsetree.Rtag (_label, attrs, _empty, _types) ->
      choice_over_attributes attrs view value
    | Parsetree.Rinherit _typ ->
      View.error

  let object_field view value =
    match value with
    | Parsetree.Otag (_label, attrs, _typ) ->
      choice_over_attributes attrs view value
    | Parsetree.Oinherit _typ->
      View.error

  let pattern view value =
    choice_over_attributes value.Parsetree.ppat_attributes view value

  let expression view value =
    choice_over_attributes value.Parsetree.pexp_attributes view value

  let value_description view value =
    choice_over_attributes value.Parsetree.pval_attributes view value

  let type_declaration view value =
    choice_over_attributes value.Parsetree.ptype_attributes view value

  let label_declaration view value =
    choice_over_attributes value.Parsetree.pld_attributes view value

  let constructor_declaration view value =
    choice_over_attributes value.Parsetree.pcd_attributes view value

  let type_extension view value =
    choice_over_attributes value.Parsetree.ptyext_attributes view value

  let extension_constructor view value =
    choice_over_attributes value.Parsetree.pext_attributes view value

  let class_type view value =
    choice_over_attributes value.Parsetree.pcty_attributes view value

  let class_type_field view value =
    choice_over_attributes value.Parsetree.pctf_attributes view value

  let class_infos view value =
    choice_over_attributes value.Parsetree.pci_attributes view value

  let class_expr view value =
    choice_over_attributes value.Parsetree.pcl_attributes view value

  let class_field view value =
    choice_over_attributes value.Parsetree.pcf_attributes view value

  let module_type view value =
    choice_over_attributes value.Parsetree.pmty_attributes view value

  let module_declaration view value =
    choice_over_attributes value.Parsetree.pmd_attributes view value

  let module_type_declaration view value =
    choice_over_attributes value.Parsetree.pmtd_attributes view value

  let open_description view value =
    choice_over_attributes value.Parsetree.popen_attributes view value

  let include_infos view value =
    choice_over_attributes value.Parsetree.pincl_attributes view value

  let module_expr view value =
    choice_over_attributes value.Parsetree.pmod_attributes view value

  let pstr_eval view value =
    match value.Parsetree.pstr_desc with
    | Parsetree.Pstr_eval (_expr, attrs) ->
      choice_over_attributes attrs view value
    | _ ->
      View.error

  let value_binding view value =
    choice_over_attributes value.Parsetree.pvb_attributes view value

  let module_binding view value =
    choice_over_attributes value.Parsetree.pmb_attributes view value

end

module Extension = struct

  type ('a, 'i, 'o) view =
       (string * Location.t * Parsetree.payload, 'i, 'o) View.t
    -> ('a, 'i, 'o) View.t

  type ('a, 'i, 'o) view_attr =
       (string * Location.t * Parsetree.payload * Parsetree.attributes, 'i, 'o) View.t
    -> ('a, 'i, 'o) View.t

  let core_type view value =
    match value.Parsetree.ptyp_desc with
    | Ptyp_extension ({ txt; loc }, payload) -> view (txt, loc, payload)
    | _                                      -> View.error

  let pattern view value =
    match value.Parsetree.ppat_desc with
    | Ppat_extension ({ txt; loc }, payload) -> view (txt, loc, payload)
    | _                                      -> View.error

  let expression view value =
    match value.Parsetree.pexp_desc with
    | Pexp_extension ({ txt; loc }, payload) -> view (txt, loc, payload)
    | _                                      -> View.error

  let class_type view value =
    match value.Parsetree.pcty_desc with
    | Pcty_extension ({ txt; loc }, payload) -> view (txt, loc, payload)
    | _                                      -> View.error

  let class_type_field view value =
    match value.Parsetree.pctf_desc with
    | Pctf_extension ({ txt; loc }, payload) -> view (txt, loc, payload)
    | _                                      -> View.error

  let class_expr view value =
    match value.Parsetree.pcl_desc with
    | Pcl_extension ({ txt; loc }, payload) -> view (txt, loc, payload)
    | _                                     -> View.error

  let class_field view value =
    match value.Parsetree.pcf_desc with
    | Pcf_extension ({ txt; loc }, payload) -> view (txt, loc, payload)
    | _                                     -> View.error

  let module_type view value =
    match value.Parsetree.pmty_desc with
    | Pmty_extension ({ txt; loc }, payload) -> view (txt, loc, payload)
    | _                                      -> View.error

  let signature_item view value =
    match value.Parsetree.psig_desc with
    | Psig_extension (({ txt; loc }, payload), attrs) -> view (txt, loc, payload, attrs)
    | _                                               -> View.error

  let module_expr view value =
    match value.Parsetree.pmod_desc with
    | Pmod_extension ({ txt; loc }, payload) -> view (txt, loc, payload)
    | _                                      -> View.error

  let structure_item view value =
    match value.Parsetree.pstr_desc with
    | Pstr_extension (({ txt; loc }, payload), attrs) -> view (txt, loc, payload, attrs)
    | _                                               -> View.error

end

module type Payload = sig

  type ('a, 'i, 'o) view =
       ('a, 'i, 'o) View.t
    -> (Parsetree.payload, 'i, 'o) View.t

  val structure
     : (Parsetree.structure, 'i, 'o) View.t
    -> (Parsetree.payload, 'i, 'o) View.t

  val signature
     : (Parsetree.signature, 'i, 'o) View.t
    -> (Parsetree.payload, 'i, 'o) View.t

  val core_type
     : (Parsetree.core_type, 'i, 'o) View.t
    -> (Parsetree.payload, 'i, 'o) View.t

  val pattern
     : (Parsetree.pattern * Parsetree.expression option, 'i, 'o) View.t
    -> (Parsetree.payload, 'i, 'o) View.t

  val none : (Parsetree.payload, 'i, 'i) View.t

  val simple
     : (Parsetree.expression * Parsetree.attributes, 'i, 'o) View.t
    -> (Parsetree.payload, 'i, 'o) View.t

end

module type Payload_error = sig
  val error : Location.t -> 'a -> 'b View.match_result
end

module Make_payload (PE : Payload_error) : Payload = struct

  type ('a, 'i, 'o) view =
       ('a, 'i, 'o) View.t
    -> (Parsetree.payload, 'i, 'o) View.t

  let location_of_payload = function
    | Parsetree.PStr ({ pstr_loc; _ } :: _) -> pstr_loc
    | Parsetree.PStr []                     -> Location.none
    | Parsetree.PSig ({ psig_loc; _ } :: _) -> psig_loc
    | Parsetree.PSig []                     -> Location.none
    | Parsetree.PTyp { ptyp_loc; _ }        -> ptyp_loc
    | Parsetree.PPat ({ ppat_loc; _ }, _)   -> ppat_loc

  let error value _view =
    PE.error (location_of_payload value)

  let structure view value =
    match value with
    | Parsetree.PStr s -> (View.choice view (error value)) s
    | _                -> PE.error (location_of_payload value)

  let signature view value =
    match value with
    | Parsetree.PSig s -> (View.choice view (error value)) s
    | _                -> PE.error (location_of_payload value)

  let core_type view value =
    match value with
    | Parsetree.PTyp t -> (View.choice view (error value)) t
    | _                -> PE.error (location_of_payload value)

  let pattern view value =
    match value with
    | Parsetree.PPat (p, e) -> (View.choice view (error value)) (p, e)
    | _                     -> PE.error (location_of_payload value)

  let simple view value =
    match value with
    | Parsetree.PStr [{ pstr_desc = Pstr_eval (expr, attrs); _ }] ->
      (View.choice view (error value)) (expr, attrs)
    | _ ->
      PE.error (location_of_payload value)

  let none value =
    match value with
    | Parsetree.PStr [] -> View.ok
    | Parsetree.PSig [] -> View.ok
    | _                 -> PE.error (location_of_payload value)

end

let invalid_payload loc _ =
  let msg =
    if loc.Location.loc_ghost then
      "missing payload"
    else
      "invalid payload"
  in
  raise (Location.Error (Location.error ~loc msg))

module Payload = struct
  module Match = Make_payload (struct let error _loc = View.error end)
  module Fail  = Make_payload (struct let error = invalid_payload end)
end

let report_ppx_error ?loc fmt =
  Location.raise_errorf ?loc fmt

let register_ppx_driver ~name ?reset_args ?args rewriter =
  Migrate_parsetree.Driver.register
    ~name
    ?reset_args
    ?args
    (module Migrate_parsetree.OCaml_407)
    rewriter

type cases = Parsetree.case list

type 'a mapper = 'a -> 'a

class virtual default_mapper =
  let super = Ast_mapper.default_mapper in
  let unwrap = function
    | Some x -> x
    | None   -> report_ppx_error "mapper has not been initialized"
  in
  object (self)

    val virtual name        : string
    val mutable config_opt  = None
    val mutable cookies_opt = None
    val mutable mapper      = super

    method config  = unwrap config_opt
    method cookies = unwrap cookies_opt

    initializer
      let attribute               _self x = self#attribute               x in
      let attributes              _self x = self#attributes              x in
      let case                    _self x = self#case                    x in
      let cases                   _self x = self#cases                   x in
      let class_declaration       _self x = self#class_declaration       x in
      let class_description       _self x = self#class_description       x in
      let class_expr              _self x = self#class_expr              x in
      let class_field             _self x = self#class_field             x in
      let class_signature         _self x = self#class_signature         x in
      let class_structure         _self x = self#class_structure         x in
      let class_type              _self x = self#class_type              x in
      let class_type_declaration  _self x = self#class_type_declaration  x in
      let class_type_field        _self x = self#class_type_field        x in
      let constructor_declaration _self x = self#constructor_declaration x in
      let expr                    _self x = self#expr                    x in
      let extension               _self x = self#extension               x in
      let extension_constructor   _self x = self#extension_constructor   x in
      let include_declaration     _self x = self#include_declaration     x in
      let include_description     _self x = self#include_description     x in
      let label_declaration       _self x = self#label_declaration       x in
      let location                _self x = self#location                x in
      let module_binding          _self x = self#module_binding          x in
      let module_declaration      _self x = self#module_declaration      x in
      let module_expr             _self x = self#module_expr             x in
      let module_type             _self x = self#module_type             x in
      let module_type_declaration _self x = self#module_type_declaration x in
      let open_description        _self x = self#open_description        x in
      let pat                     _self x = self#pat                     x in
      let payload                 _self x = self#payload                 x in
      let signature               _self x = self#signature               x in
      let signature_item          _self x = self#signature_item          x in
      let structure               _self x = self#structure               x in
      let structure_item          _self x = self#structure_item          x in
      let typ                     _self x = self#typ                     x in
      let type_declaration        _self x = self#type_declaration        x in
      let type_extension          _self x = self#type_extension          x in
      let type_kind               _self x = self#type_kind               x in
      let value_binding           _self x = self#value_binding           x in
      let value_description       _self x = self#value_description       x in
      let with_constraint         _self x = self#with_constraint         x in
      mapper <- {
        attribute;
        attributes;
        case;
        cases;
        class_declaration;
        class_description;
        class_expr;
        class_field;
        class_signature;
        class_structure;
        class_type;
        class_type_declaration;
        class_type_field;
        constructor_declaration;
        expr;
        extension;
        extension_constructor;
        include_declaration;
        include_description;
        label_declaration;
        location;
        module_binding;
        module_declaration;
        module_expr;
        module_type;
        module_type_declaration;
        open_description;
        pat;
        payload;
        signature;
        signature_item;
        structure;
        structure_item;
        typ;
        type_declaration;
        type_extension;
        type_kind;
        value_binding;
        value_description;
        with_constraint;
      }

    method attribute               x = super.attribute               mapper x
    method attributes              x = super.attributes              mapper x
    method case                    x = super.case                    mapper x
    method cases                   x = super.cases                   mapper x
    method class_declaration       x = super.class_declaration       mapper x
    method class_description       x = super.class_description       mapper x
    method class_expr              x = super.class_expr              mapper x
    method class_field             x = super.class_field             mapper x
    method class_signature         x = super.class_signature         mapper x
    method class_structure         x = super.class_structure         mapper x
    method class_type              x = super.class_type              mapper x
    method class_type_declaration  x = super.class_type_declaration  mapper x
    method class_type_field        x = super.class_type_field        mapper x
    method constructor_declaration x = super.constructor_declaration mapper x
    method expr                    x = super.expr                    mapper x
    method extension               x = super.extension               mapper x
    method extension_constructor   x = super.extension_constructor   mapper x
    method include_declaration     x = super.include_declaration     mapper x
    method include_description     x = super.include_description     mapper x
    method label_declaration       x = super.label_declaration       mapper x
    method location                x = super.location                mapper x
    method module_binding          x = super.module_binding          mapper x
    method module_declaration      x = super.module_declaration      mapper x
    method module_expr             x = super.module_expr             mapper x
    method module_type             x = super.module_type             mapper x
    method module_type_declaration x = super.module_type_declaration mapper x
    method open_description        x = super.open_description        mapper x
    method pat                     x = super.pat                     mapper x
    method payload                 x = super.payload                 mapper x
    method signature               x = super.signature               mapper x
    method signature_item          x = super.signature_item          mapper x
    method structure               x = super.structure               mapper x
    method structure_item          x = super.structure_item          mapper x
    method typ                     x = super.typ                     mapper x
    method type_declaration        x = super.type_declaration        mapper x
    method type_extension          x = super.type_extension          mapper x
    method type_kind               x = super.type_kind               mapper x
    method value_binding           x = super.value_binding           mapper x
    method value_description       x = super.value_description       mapper x
    method with_constraint         x = super.with_constraint         mapper x

    method expression x = self#expr x
    method pattern    x = self#pat  x
    method core_type  x = self#typ  x

    method signature_item_to_list f =
      fun sign ->
        match sign with
        | hd :: tl ->
          (f (self#signature_item hd)) @ (self#signature tl)
        | [] ->
          []
    method structure_item_to_list f =
      fun struc ->
        match struc with
        | hd :: tl ->
          (f (self#structure_item hd)) @ (self#structure tl)
        | [] ->
          []

    method register =
      register_ppx_driver
        ~name
        (fun config cookies ->
           config_opt  <- Some config;
           cookies_opt <- Some cookies;
           mapper)

  end
