open! Import

let poly_equal a b =
  let module Poly = struct
    type t = T : _ -> t
  end in
  (Poly.T a) = (Poly.T b)
;;

module Context = struct
  type 'a t =
    | Label_declaration       : label_declaration       t
    | Constructor_declaration : constructor_declaration t
    | Type_declaration        : type_declaration        t
    | Type_extension          : type_extension          t
    | Extension_constructor   : extension_constructor   t
    | Pattern                 : pattern                 t
    | Core_type               : core_type               t
    | Expression              : expression              t
    | Value_description       : value_description       t
    | Class_type              : class_type              t
    | Class_type_field        : class_type_field        t
    | Class_infos             : _ node class_infos      t
    | Class_expr              : class_expr              t
    | Class_field             : class_field             t
    | Module_type             : module_type             t
    | Module_declaration      : module_declaration      t
    | Module_type_declaration : module_type_declaration t
    | Open_description        : open_description        t
    | Include_infos           : _ node include_infos    t
    | Module_expr             : module_expr             t
    | Value_binding           : value_binding           t
    | Module_binding          : module_binding          t
    | Pstr_eval               : structure_item          t
    | Pstr_extension          : structure_item          t
    | Psig_extension          : signature_item          t
    | Rtag                    : row_field               t
    | Object_type_field       : object_field            t

  let label_declaration       = Label_declaration
  let constructor_declaration = Constructor_declaration
  let type_declaration        = Type_declaration
  let type_extension          = Type_extension
  let extension_constructor   = Extension_constructor
  let pattern                 = Pattern
  let core_type               = Core_type
  let expression              = Expression
  let value_description       = Value_description
  let class_type              = Class_type
  let class_type_field        = Class_type_field
  let class_infos             = Class_infos
  let class_expr              = Class_expr
  let class_field             = Class_field
  let module_type             = Module_type
  let module_declaration      = Module_declaration
  let module_type_declaration = Module_type_declaration
  let open_description        = Open_description
  let include_infos           = Include_infos
  let module_expr             = Module_expr
  let value_binding           = Value_binding
  let module_binding          = Module_binding
  let pstr_eval               = Pstr_eval
  let pstr_extension          = Pstr_extension
  let psig_extension          = Psig_extension
  let rtag                    = Rtag
  let object_type_field       = Object_type_field

  let get_pstr_eval st =
    match%view (Structure_item.pstr_desc st) with
    | Pstr_eval (e, l) -> (e, l)
    | _ -> failwith "Attribute.Context.get_pstr_eval"

  let get_pstr_extension st =
    match%view (Structure_item.pstr_desc st) with
    | Pstr_extension (e, l) -> (e, l)
    | _ -> failwith "Attribute.Context.get_pstr_extension"

  let get_psig_extension st =
    match%view (Signature_item.psig_desc st) with
    | Psig_extension (e, l) -> (e, l)
    | _ -> failwith "Attribute.Context.get_psig_extension"

  let get_attributes : type a. a t -> a -> attribute list = fun t x ->
    let attributes =
      match t with
      | Label_declaration       -> Label_declaration.pld_attributes x
      | Constructor_declaration -> Constructor_declaration.pcd_attributes x
      | Type_declaration        -> Type_declaration.ptype_attributes x
      | Type_extension          -> Type_extension.ptyext_attributes x
      | Extension_constructor   -> Extension_constructor.pext_attributes x
      | Pattern                 -> Pattern.ppat_attributes x
      | Core_type               -> Core_type.ptyp_attributes x
      | Expression              -> Expression.pexp_attributes x
      | Value_description       -> Value_description.pval_attributes x
      | Class_type              -> Class_type.pcty_attributes x
      | Class_type_field        -> Class_type_field.pctf_attributes x
      | Class_infos             -> Class_infos.pci_attributes x
      | Class_expr              -> Class_expr.pcl_attributes x
      | Class_field             -> Class_field.pcf_attributes x
      | Module_type             -> Module_type.pmty_attributes x
      | Module_declaration      -> Module_declaration.pmd_attributes x
      | Module_type_declaration -> Module_type_declaration.pmtd_attributes x
      | Open_description        -> Open_description.popen_attributes x
      | Include_infos           -> Include_infos.pincl_attributes x
      | Module_expr             -> Module_expr.pmod_attributes x
      | Value_binding           -> Value_binding.pvb_attributes x
      | Module_binding          -> Module_binding.pmb_attributes x
      | Pstr_eval               -> snd (get_pstr_eval      x)
      | Pstr_extension          -> snd (get_pstr_extension x)
      | Psig_extension          -> snd (get_psig_extension x)
      | Rtag                    ->
        begin match%view x with
        | Rtag (_, attrs, _, _) -> attrs
        | Rinherit _ -> Attributes.create []
        end
      | Object_type_field       ->
        begin match%view x with
        | Otag (_, attrs, _) -> attrs
        | Oinherit _ -> Attributes.create []
        end
    in
    Attributes.to_concrete attributes

  let set_attributes : type a. a t -> a -> attribute list -> a = fun t x attrs ->
    let attrs = Attributes.create attrs in
    match t with
    | Label_declaration ->
      Label_declaration.update x ~pld_attributes:(Label_declaration.pld_attributes x)
    | Constructor_declaration ->
      Constructor_declaration.update x
        ~pcd_attributes:(Constructor_declaration.pcd_attributes x)
    | Type_declaration ->
      Type_declaration.update x ~ptype_attributes:(Type_declaration.ptype_attributes x)
    | Type_extension ->
      Type_extension.update x ~ptyext_attributes:(Type_extension.ptyext_attributes x)
    | Extension_constructor ->
      Extension_constructor.update x
        ~pext_attributes:(Extension_constructor.pext_attributes x)
    | Pattern -> Pattern.update x ~ppat_attributes:(Pattern.ppat_attributes x)
    | Core_type -> Core_type.update x ~ptyp_attributes:(Core_type.ptyp_attributes x)
    | Expression -> Expression.update x ~pexp_attributes:(Expression.pexp_attributes x)
    | Value_description ->
      Value_description.update x ~pval_attributes:(Value_description.pval_attributes x)
    | Class_type -> Class_type.update x ~pcty_attributes:(Class_type.pcty_attributes x)
    | Class_type_field ->
      Class_type_field.update x ~pctf_attributes:(Class_type_field.pctf_attributes x)
    | Class_infos -> Class_infos.update x ~pci_attributes:(Class_infos.pci_attributes x)
    | Class_expr -> Class_expr.update x ~pcl_attributes:(Class_expr.pcl_attributes x)
    | Class_field -> Class_field.update x ~pcf_attributes:(Class_field.pcf_attributes x)
    | Module_type -> Module_type.update x ~pmty_attributes:(Module_type.pmty_attributes x)
    | Module_declaration ->
      Module_declaration.update x ~pmd_attributes:(Module_declaration.pmd_attributes x)
    | Module_type_declaration ->
      Module_type_declaration.update x
        ~pmtd_attributes:(Module_type_declaration.pmtd_attributes x)
    | Open_description ->
      Open_description.update x ~popen_attributes:(Open_description.popen_attributes x)
    | Include_infos ->
      Include_infos.update x ~pincl_attributes:(Include_infos.pincl_attributes x)
    | Module_expr -> Module_expr.update x ~pmod_attributes:(Module_expr.pmod_attributes x)
    | Value_binding ->
      Value_binding.update x ~pvb_attributes:(Value_binding.pvb_attributes x)
    | Module_binding ->
      Module_binding.update x ~pmb_attributes:(Module_binding.pmb_attributes x)
    | Pstr_eval ->
      Structure_item.update x
        ~pstr_desc:(Structure_item_desc.pstr_eval (fst (get_pstr_eval x)) attrs)
    | Pstr_extension ->
      Structure_item.update x
        ~pstr_desc:(Structure_item_desc.pstr_extension (fst (get_pstr_extension x)) attrs)
    | Psig_extension ->
      Signature_item.update x
        ~psig_desc:(Signature_item_desc.psig_extension (fst (get_psig_extension x)) attrs)
    | Rtag                   ->
      begin match%view x with
      | Rtag (lbl, _, can_be_constant, params_opts) ->
        Row_field.rtag lbl attrs can_be_constant params_opts
      | Rinherit _ ->
        assert (List.is_empty (Attributes.to_concrete attrs));
        x
      end
    | Object_type_field ->
      begin match%view x with
      | Otag (lbl, _, typ) ->
        Object_field.otag lbl attrs typ
      | Oinherit _ ->
        assert (List.is_empty (Attributes.to_concrete attrs));
        x
      end

  let desc : type a. a t -> string = function
    | Label_declaration       -> "label declaration"
    | Constructor_declaration -> "constructor declaration"
    | Type_declaration        -> "type declaration"
    | Type_extension          -> "type extension"
    | Extension_constructor   -> "extension constructor"
    | Pattern                 -> "pattern"
    | Core_type               -> "core type"
    | Expression              -> "expression"
    | Value_description       -> "value"
    | Class_type              -> "class type"
    | Class_type_field        -> "class type field"
    | Class_infos             -> "class declaration"
    | Class_expr              -> "class expression"
    | Class_field             -> "class field"
    | Module_type             -> "module type"
    | Module_declaration      -> "module declaration"
    | Module_type_declaration -> "module type declaration"
    | Open_description        -> "open"
    | Include_infos           -> "include"
    | Module_expr             -> "module expression"
    | Value_binding           -> "value binding"
    | Module_binding          -> "module binding"
    | Pstr_eval               -> "toplevel expression"
    | Pstr_extension          -> "toplevel extension"
    | Psig_extension          -> "toplevel signature extension"
    | Rtag                    -> "polymorphic variant tag"
    | Object_type_field       -> "object type field"

(*
  let pattern : type a b c d. a t
    -> (attributes, b, c) Ast_pattern.t
    -> (a, c, d) Ast_pattern.t
    -> (a, b, d) Ast_pattern.t = function
    | Label_declaration       -> (Label_declaration.pld_attributes Ast_pattern)
    | Constructor_declaration -> (Constructor_declaration.pcd_attributes Ast_pattern)
    | Type_declaration        -> (Type_declaration.ptype_attributes Ast_pattern)
    | Type_extension          -> (Type_extension.ptyext_attributes Ast_pattern)
    | Extension_constructor   -> (Extension_constructor.pext_attributes Ast_pattern)
*)

  let equal : _ t -> _ t -> bool = poly_equal
end

module Floating_context = struct
  type 'a t =
    | Structure_item   : structure_item   t
    | Signature_item   : signature_item   t
    | Class_field      : class_field      t
    | Class_type_field : class_type_field t

  let structure_item   = Structure_item
  let signature_item   = Signature_item
  let class_field      = Class_field
  let class_type_field = Class_type_field

  let get_attribute_if_is_floating_node : type a. a t -> a -> attribute option
    = fun t x ->
      match t with
      | Structure_item   -> (match%view x with | { pstr_desc = Pstr_attribute a; _ } -> Some a | _ -> None)
      | Signature_item   -> (match%view x with | { psig_desc = Psig_attribute a; _ } -> Some a | _ -> None)
      | Class_field      -> (match%view x with | { pcf_desc  = Pcf_attribute  a; _ } -> Some a | _ -> None)
      | Class_type_field -> (match%view x with | { pctf_desc = Pctf_attribute a; _ } -> Some a | _ -> None)

  let get_attribute t x =
    match get_attribute_if_is_floating_node t x with
    | Some a -> a
    | None   -> failwith "Attribute.Floating.Context.get_attribute"

  let dummy_ext =
    Extension.create
      ({ txt = ""; loc = Location.none }, Payload.pStr (Structure.create []))

  let replace_by_dummy : type a. a t -> a -> a =
    fun t x ->
      match t with
      | Structure_item   ->
        Structure_item.update x
          ~pstr_desc:(Structure_item_desc.pstr_extension dummy_ext (Attributes.create []))
      | Signature_item   ->
        Signature_item.update x
          ~psig_desc:(Signature_item_desc.psig_extension dummy_ext (Attributes.create []))
      | Class_field      ->
        Class_field.update x ~pcf_desc:(Class_field_desc.pcf_extension dummy_ext)
      | Class_type_field ->
        Class_type_field.update x
          ~pctf_desc:(Class_type_field_desc.pctf_extension dummy_ext)

  let desc : type a. a t -> string = function
    | Structure_item   -> "structure item"
    | Signature_item   -> "signature item"
    | Class_field      -> "class field"
    | Class_type_field -> "class type field"

  let equal : _ t -> _ t -> bool = poly_equal
end

type packed_context =
  | On_item  : _ Context.t          -> packed_context
  | Floating : _ Floating_context.t -> packed_context

type _ payload_parser =
    Payload_parser
    : (payload, 'a, 'b) Ast_pattern.t * (name_loc:Location.t -> 'a)
    -> 'b payload_parser

type ('a, 'b) t =
  { name    : Name.Pattern.t
  ; context : 'a Context.t
  ; payload : 'b payload_parser
  }

type packed = T : (_, _) t -> packed

let name t = Name.Pattern.name t.name
let context t = t.context

let registrar =
  Name.Registrar.create
    ~kind:"attribute"
    ~current_file:__FILE__
    ~string_of_context:(function
      | On_item  t -> Some (Context         .desc t)
      | Floating t -> Some (Floating_context.desc t ^ " (floating)"))
;;

let declare_with_name_loc name context pattern k =
  Name.Registrar.register ~kind:`Attribute registrar (On_item context) name;
  { name = Name.Pattern.make name
  ; context
  ; payload = Payload_parser (pattern, k)
  }
;;

let declare name context pattern k =
  declare_with_name_loc name context pattern (fun ~name_loc:_ -> k)
;;

module Attribute_table = Hashtbl.Make(struct
    type t = string Loc.t
    let hash : t -> int = Hashtbl.hash
    let equal : t -> t -> bool = (=)
  end)

let not_seen = Attribute_table.create 128

let mark_as_seen attr =
  let name = fst (Attribute.to_concrete attr) in
  Attribute_table.remove not_seen name
;;

let mark_as_handled_manually = mark_as_seen

let explicitly_drop = object
  inherit Ast_traverse.iter
  method! attribute = mark_as_seen
end

let get_internal =
  let rec find_best_match t attributes longest_match =
    match%view attributes with
    | [] -> longest_match
    | Attribute (name, _) as attr :: rest ->
      if Name.Pattern.matches t.name name.txt then begin
        match%view longest_match with
        | None -> find_best_match t rest (Some attr)
        | Some (Attribute (name', _)) ->
          let len = String.length name.txt in
          let len' = String.length name'.txt in
          if len > len' then
            find_best_match t rest (Some attr)
          else if len < len' then
            find_best_match t rest longest_match
          else
            Location.raise_errorf ~loc:name.loc "Duplicated attribute"
      end else
        find_best_match t rest longest_match
  in
  fun t attributes ->
    find_best_match t attributes None
;;

let convert ?(do_mark_as_seen = true) pattern attr =
  if do_mark_as_seen then mark_as_seen attr;
  let (Payload_parser (pattern, k)) = pattern in
  let attr = Attribute.to_concrete attr in
  Ast_pattern.parse pattern
    (Common.loc_of_payload attr)
    (snd attr)
    (k ~name_loc:(fst attr).loc)
;;

let get t ?mark_as_seen:do_mark_as_seen x =
  let attrs = Context.get_attributes t.context x in
  match get_internal t attrs with
  | None -> None
  | Some attr -> Some (convert t.payload attr ?do_mark_as_seen)
;;

let consume t x =
  let attrs = Context.get_attributes t.context x in
  match get_internal t attrs with
  | None -> None
  | Some attr ->
    let attrs =
      List.filter attrs ~f:(fun attr' -> not (attr == attr'))
    in
    let x = Context.set_attributes t.context x attrs in
    Some (x, convert t.payload attr)
;;

let remove_seen (type a) (context : a Context.t) packeds (x : a) =
  let attrs = Context.get_attributes context x in
  let matched =
    let rec loop acc = function
      | [] -> acc
      | T t :: rest ->
        if Context.equal t.context context then
          match get_internal t attrs with
          | None      -> loop acc rest
          | Some attr ->
            let name = fst (Attribute.to_concrete attr) in
            if Attribute_table.mem not_seen name then
              loop acc rest
            else
              loop (attr :: acc) rest
        else
          loop acc rest
    in
    loop [] packeds
  in
  let attrs = List.filter attrs ~f:(fun attr' -> not (List.memq ~set:matched attr')) in
  Context.set_attributes context x attrs
;;

let pattern t p =
  let f = Ast_pattern.to_func p in
  Ast_pattern.of_func (fun ctx loc x k ->
    match consume t x with
    | None        -> f ctx loc x (k None)
    | Some (x, v) -> f ctx loc x (k (Some v))
  )
;;

module Floating = struct
  module Context = Floating_context

  type ('a, 'b) t =
    { name    : Name.Pattern.t
    ; context : 'a Context.t
    ; payload : 'b payload_parser
    }

  let name t = Name.Pattern.name t.name

  let declare name context pattern k =
    Name.Registrar.register ~kind:`Attribute registrar (Floating context) name;
    { name = Name.Pattern.make name
    ; context
    ; payload = Payload_parser (pattern, fun ~name_loc:_ -> k)
    }
  ;;

  let convert ts x =
    match ts with
    | [] -> None
    | { context; _ } :: _ ->
      assert (List.for_all ts ~f:(fun t -> Context.equal t.context context));
      let attr = Context.get_attribute context x in
      let name = fst (Attribute.to_concrete attr) in
      match List.filter ts ~f:(fun t -> Name.Pattern.matches t.name name.txt) with
      | [] -> None
      | [t] -> Some (convert t.payload attr)
      | l ->
        Location.raise_errorf ~loc:name.loc
          "Multiple match for floating attributes: %s"
          (String.concat ~sep:", " (List.map l ~f:(fun t -> Name.Pattern.name t.name)))
  ;;
end

let check_attribute registrar context (name : _ Loc.t) =
  if not (Name.Whitelisted.is_whitelisted ~kind:`Attribute name.txt
          || Name.ignore_checks name.txt)
  && Attribute_table.mem not_seen name then
    let white_list = Name.Whitelisted.get_attribute_list () in
    Name.Registrar.raise_errorf registrar context ~white_list
      "Attribute `%s' was not used" name
;;


let check_unused = object(self)
  inherit Ast_traverse.iter as super

  method! attribute attr =
    match%view attr with
    | Attribute (name, _) ->
      Location.raise_errorf ~loc:name.loc
        "attribute not expected here, Ppx.Attribute needs updating!"

  method private check_node : type a. a Context.t -> a -> a = fun context node ->
    let attrs = Context.get_attributes context node in
    match attrs with
    | [] -> node
    | _  ->
      List.iter attrs ~f:(function%view (Attribute (name, payload) as attr) ->
        self#payload payload;
        check_attribute registrar (On_item context) name;
        (* If we allow the attribute to pass through, mark it as seen *)
        mark_as_seen attr);
      Context.set_attributes context node []

  method private check_floating : type a. a Floating.Context.t -> a -> a
    = fun context node ->
      match%view Floating.Context.get_attribute_if_is_floating_node context node with
      | None -> node
      | Some (Attribute (name, payload) as attr) ->
        self#payload payload;
        check_attribute registrar (Floating context) name;
        mark_as_seen attr;
        Floating.Context.replace_by_dummy context node

  method! label_declaration       x = super#label_declaration       (self#check_node Label_declaration       x)
  method! constructor_declaration x = super#constructor_declaration (self#check_node Constructor_declaration x)
  method! type_declaration        x = super#type_declaration        (self#check_node Type_declaration        x)
  method! type_extension          x = super#type_extension          (self#check_node Type_extension          x)
  method! extension_constructor   x = super#extension_constructor   (self#check_node Extension_constructor   x)
  method! pattern                 x = super#pattern                 (self#check_node Pattern                 x)
  method! core_type               x = super#core_type               (self#check_node Core_type               x)
  method! expression              x = super#expression              (self#check_node Expression              x)
  method! value_description       x = super#value_description       (self#check_node Value_description       x)
  method! class_type              x = super#class_type              (self#check_node Class_type              x)
  method! class_infos f           x = super#class_infos f           (self#check_node Class_infos             x)
  method! class_expr              x = super#class_expr              (self#check_node Class_expr              x)
  method! module_type             x = super#module_type             (self#check_node Module_type             x)
  method! module_declaration      x = super#module_declaration      (self#check_node Module_declaration      x)
  method! module_type_declaration x = super#module_type_declaration (self#check_node Module_type_declaration x)
  method! open_description        x = super#open_description        (self#check_node Open_description        x)
  method! include_infos f         x = super#include_infos f         (self#check_node Include_infos           x)
  method! module_expr             x = super#module_expr             (self#check_node Module_expr             x)
  method! value_binding           x = super#value_binding           (self#check_node Value_binding           x)
  method! module_binding          x = super#module_binding          (self#check_node Module_binding          x)

  method! class_field x =
    let x = self#check_node     Class_field x in
    let x = self#check_floating Class_field x in
    super#class_field x

  method! class_type_field x =
    let x = self#check_node     Class_type_field x in
    let x = self#check_floating Class_type_field x in
    super#class_type_field x

  method! row_field x =
    let x =
      match%view x with
      | Rtag _ -> self#check_node Rtag x
      | _      -> x
    in
    super#row_field x

  method! core_type_desc x =
    let x =
      match%view x with
      | Ptyp_object (fields, closed_flag) ->
        let fields = List.map fields ~f:(self#check_node Object_type_field) in
        Core_type_desc.ptyp_object fields closed_flag
      | _ -> x
    in
    super#core_type_desc x

  method! structure_item item =
    let item = self#check_floating Structure_item item in
    let item =
      match%view (Structure_item.pstr_desc item) with
      | Pstr_eval      _ -> self#check_node Pstr_eval      item
      | Pstr_extension _ -> self#check_node Pstr_extension item
      | _                -> item
    in
    super#structure_item item

  method! signature_item item =
    let item = self#check_floating Signature_item item in
    let item =
      match%view (Signature_item.psig_desc item) with
      | Psig_extension _ -> self#check_node Psig_extension item
      | _                -> item
    in
    super#signature_item item
end

let reset_checks () = Attribute_table.clear not_seen

let collect = object
  inherit Ast_traverse.iter as super

  method! attribute attr =
    match%view attr with
    | Attribute (name, payload) ->
      let loc = Common.loc_of_attribute attr in
      super#payload payload;
      Attribute_table.add not_seen name loc
end

let check_all_seen () =
  let fail ~key:name ~data:loc =
    let txt = name.Loc.txt in
    if not (Name.ignore_checks txt) then
      Location.raise_errorf ~loc "Attribute `%s' was silently dropped" txt
  in
  Attribute_table.iter ~f:fail not_seen
;;

let remove_attributes_present_in table = object
  inherit Ast_traverse.iter as super

  method! attribute = function%view (Attribute (name, payload)) ->
    super#payload payload;
    Attribute_table.remove table name
end

let copy_of_not_seen () =
  let copy = Attribute_table.create (Attribute_table.length not_seen) in
  Attribute_table.iter ~f:(fun ~key ~data -> Attribute_table.add copy key data) not_seen;
  copy
;;

let dropped_so_far_structure st =
  let table = copy_of_not_seen () in
  (remove_attributes_present_in table)#structure st;
  Attribute_table.foldi
    ~f:(fun name loc acc -> ({ txt = name.txt; loc } : _ Loc.t) :: acc)
    table
    ~init:[]
;;

let dropped_so_far_signature sg =
  let table = copy_of_not_seen () in
  (remove_attributes_present_in table)#signature sg;
  Attribute_table.foldi
    ~f:(fun name loc acc -> ({ txt = name.txt; loc } : _ Loc.t) :: acc)
    table
    ~init:[]
;;
