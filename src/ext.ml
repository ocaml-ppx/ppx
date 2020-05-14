open! Import
open Current_ast
open Common

type (_, _) equality = Eq : ('a, 'a) equality | Ne : (_, _) equality

module Context = struct
  type 'a t =
    | Class_expr       : class_expr       t
    | Class_field      : class_field      t
    | Class_type       : class_type       t
    | Class_type_field : class_type_field t
    | Core_type        : core_type        t
    | Expression       : expression       t
    | Module_expr      : module_expr      t
    | Module_type      : module_type      t
    | Pattern          : pattern          t
    | Signature_item   : signature_item   t
    | Structure_item   : structure_item   t

  type packed = T : _ t -> packed

  let class_expr       = Class_expr
  let class_field      = Class_field
  let class_type       = Class_type
  let class_type_field = Class_type_field
  let core_type        = Core_type
  let expression       = Expression
  let module_expr      = Module_expr
  let module_type      = Module_type
  let pattern          = Pattern
  let signature_item   = Signature_item
  let structure_item   = Structure_item

  let desc : type a. a t -> string = function
    | Class_expr       -> "class expression"
    | Class_field      -> "class field"
    | Class_type       -> "class type"
    | Class_type_field -> "class type field"
    | Core_type        -> "core type"
    | Expression       -> "expression"
    | Module_expr      -> "module expression"
    | Module_type      -> "module type"
    | Pattern          -> "pattern"
    | Signature_item   -> "signature item"
    | Structure_item   -> "structure item"

  let eq : type a b. a t -> b t -> (a, b) equality = fun a b ->
    match a, b with
    | Class_expr       , Class_expr       -> Eq
    | Class_field      , Class_field      -> Eq
    | Class_type       , Class_type       -> Eq
    | Class_type_field , Class_type_field -> Eq
    | Core_type        , Core_type        -> Eq
    | Expression       , Expression       -> Eq
    | Module_expr      , Module_expr      -> Eq
    | Module_type      , Module_type      -> Eq
    | Pattern          , Pattern          -> Eq
    | Signature_item   , Signature_item   -> Eq
    | Structure_item   , Structure_item   -> Eq
    | _ -> assert ((T a) <> (T b)); Ne

  let get_extension : type a. a t -> a -> (extension * attributes) option = fun t x ->
    match t with
    | Class_expr       ->
      (match%view x with
       | {pcl_desc =Pcl_extension  e; pcl_attributes =a;_} -> Some (e, a)
       | _ -> None)
    | Class_field      ->
      (match%view x with
       | {pcf_desc =Pcf_extension  e; pcf_attributes =a;_} -> Some (e, a)
       | _ -> None)
    | Class_type       ->
      (match%view x with
       | {pcty_desc=Pcty_extension e; pcty_attributes=a;_} -> Some (e, a)
       | _ -> None)
    | Class_type_field ->
      (match%view x with
       | {pctf_desc=Pctf_extension e; pctf_attributes=a;_} -> Some (e, a)
       | _ -> None)
    | Core_type        ->
      (match%view x with
       | {ptyp_desc=Ptyp_extension e; ptyp_attributes=a;_} -> Some (e, a)
       | _ -> None)
    | Expression       ->
      (match%view x with
       | {pexp_desc=Pexp_extension e; pexp_attributes=a;_} -> Some (e, a)
       | _ -> None)
    | Module_expr      ->
      (match%view x with
       | {pmod_desc=Pmod_extension e; pmod_attributes=a;_} -> Some (e, a)
       | _ -> None)
    | Module_type      ->
      (match%view x with
       | {pmty_desc=Pmty_extension e; pmty_attributes=a;_} -> Some (e, a)
       | _ -> None)
    | Pattern          ->
      (match%view x with
       | {ppat_desc=Ppat_extension e; ppat_attributes=a;_} -> Some (e, a)
       | _ -> None)
    | Signature_item   ->
      (match%view x with
       | {psig_desc=Psig_extension(e, a)               ;_} -> Some (e, a)
       | _ -> None)
    | Structure_item   ->
      (match%view x with
       | {pstr_desc=Pstr_extension(e, a)               ;_} -> Some (e, a)
       | _ -> None)

  let append x y =
    Attributes.create (Attributes.to_concrete x @ Attributes.to_concrete y)

  let merge_attributes : type a. a t -> a -> attributes -> a = fun t x attrs ->
    match t with
    | Class_expr       ->
      Class_expr.update x ~pcl_attributes:(append (Class_expr.pcl_attributes x) attrs)
    | Class_field      ->
      Class_field.update x ~pcf_attributes:(append (Class_field.pcf_attributes x) attrs)
    | Class_type       ->
      Class_type.update x ~pcty_attributes:(append (Class_type.pcty_attributes x) attrs)
    | Class_type_field ->
      Class_type_field.update x
        ~pctf_attributes:(append (Class_type_field.pctf_attributes x) attrs)
    | Core_type        ->
      Core_type.update x ~ptyp_attributes:(append (Core_type.ptyp_attributes x) attrs)
    | Expression       ->
      Expression.update x ~pexp_attributes:(append (Expression.pexp_attributes x) attrs)
    | Module_expr      ->
      Module_expr.update x ~pmod_attributes:(append (Module_expr.pmod_attributes x) attrs)
    | Module_type      ->
      Module_type.update x ~pmty_attributes:(append (Module_type.pmty_attributes x) attrs)
    | Pattern          ->
      Pattern.update x ~ppat_attributes:(append (Pattern.ppat_attributes x) attrs)
    | Signature_item   -> assert_no_attributes attrs; x
    | Structure_item   -> assert_no_attributes attrs; x
end

let registrar =
  Name.Registrar.create
    ~kind:"extension"
    ~current_file:__FILE__
    ~string_of_context:(fun (Context.T ctx) -> Some (Context.desc ctx))
;;

module Make(Callback : sig type 'a t end) = struct

  type ('a, 'b) payload_parser =
      Payload_parser : ('a, 'b, 'c) Ast_pattern.t * 'b Callback.t
      -> ('a, 'c) payload_parser

  type ('context, 'payload) t =
    { name     : Name.Pattern.t
    ; context  : 'context Context.t
    ; payload  : (payload, 'payload) payload_parser
    ; with_arg : bool
    }

  let declare ~with_arg name context pattern k =
    Name.Registrar.register ~kind:`Extension registrar (Context.T context) name;
    { name = Name.Pattern.make name
    ; context
    ; payload = Payload_parser (pattern, k)
    ; with_arg
    }
  ;;

  let find ts (ext : extension) =
    match%view ext with
    | Extension ({ txt = name; loc }, _) ->
      let name, arg = Name.split_path name in
      match List.filter ts ~f:(fun t -> Name.Pattern.matches t.name name) with
      | [] -> None
      | _ :: _ :: _ as l ->
        Location.raise_errorf ~loc
          "Multiple match for extensions: %s"
          (String.concat ~sep:", " (List.map l ~f:(fun t -> Name.Pattern.name t.name)))
      | [t] ->
        if not t.with_arg && Option.is_some arg then
          Location.raise_errorf ~loc
            "Extension %s doesn't expect a path argument"
            name;
        let arg =
          Option.map arg ~f:(fun s ->
            let shift = String.length name + 1 in
            let start = loc.loc_start in
            ({ txt = Longid.parse s
             ; loc = { loc with loc_start =
                                  { start with pos_cnum = start.pos_cnum + shift }
                     }
             } : _ Loc.t))
        in
        Some (t, arg)
  ;;
end

module Expert = struct
  include Make(struct type 'a t = arg:Longident.t Loc.t option -> 'a end)

  let declare_with_path_arg name ctx patt f =
    declare ~with_arg:true name ctx patt f

  let declare name ctx patt f =
    declare ~with_arg:false name ctx patt (fun ~arg:_ -> f)

  let convert ts ~loc ext =
    match find ts ext with
    | None -> None
    | Some ({ payload = Payload_parser (pattern, f); _ }, arg) ->
      Some (Ast_pattern.parse pattern loc (snd (Extension.to_concrete ext)) (f ~arg))
end

module M = Make(struct
    type 'a t = ctxt:Expansion_context.Extension.t -> arg:Longident.t Loc.t option -> 'a
  end)

type 'a expander_result =
  | Simple of 'a
  | Inline of 'a list

module For_context = struct
  type 'a t = ('a, 'a expander_result) M.t

  let convert ts ~ctxt ext =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    match M.find ts ext with
    | None -> None
    | Some ({ payload = M.Payload_parser (pattern, f); _  }, arg) ->
      match
        Ast_pattern.parse pattern loc (snd (Extension.to_concrete ext)) (f ~ctxt ~arg)
      with
      | Simple x -> Some x
      | Inline _ -> failwith "Extension.convert"
  ;;

  let convert_inline ts ~ctxt ext =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    match M.find ts ext with
    | None -> None
    | Some ({ payload = M.Payload_parser (pattern, f); _  }, arg) ->
      match
        Ast_pattern.parse pattern loc (snd (Extension.to_concrete ext)) (f ~ctxt ~arg)
      with
      | Simple x -> Some [x]
      | Inline l -> Some l
  ;;
end

type t = T : _ For_context.t -> t

let check_context_for_inline : type a. func:string -> a Context.t -> unit =
  fun ~func ctx ->
    match ctx with
    | Context.Class_field      -> ()
    | Context.Class_type_field -> ()
    | Context.Signature_item   -> ()
    | Context.Structure_item   -> ()
    | context ->
      Printf.ksprintf invalid_arg "%s: %s can't be inlined"
        func
        (Context.desc context)
;;

let rec filter_by_context
    : type a. a Context.t -> t list -> a For_context.t list =
    fun context expanders ->
      match expanders with
      | [] -> []
      | T t :: rest ->
        match Context.eq context t.context with
        | Eq -> t :: filter_by_context context rest
        | Ne ->      filter_by_context context rest
;;

let fail ctx ext =
  match%view ext with
  | Extension (name, _) ->
    if not (Name.Whitelisted.is_whitelisted ~kind:`Extension name.txt
            || Name.ignore_checks name.txt) then
      Name.Registrar.raise_errorf registrar (Context.T ctx)
        "Extension `%s' was not translated" name
;;

let check_unused = object
  inherit Ast_traverse.iter as super

  method! extension ext =
    match%view ext with
    | Extension (name, _) ->
      Location.raise_errorf ~loc:name.loc
        "extension not expected here, Ppx.Extension needs updating!"

  method! core_type_desc = function%view
    | Ptyp_extension ext -> fail Core_type ext
    | x -> super#core_type_desc x

  method! pattern_desc = function%view
    | Ppat_extension ext -> fail Pattern ext
    | x -> super#pattern_desc x

  method! expression_desc = function%view
    | Pexp_extension ext -> fail Expression ext
    | x -> super#expression_desc x

  method! class_type_desc = function%view
    | Pcty_extension ext -> fail Class_type ext
    | x -> super#class_type_desc x

  method! class_type_field_desc = function%view
    | Pctf_extension ext -> fail Class_type_field ext
    | x -> super#class_type_field_desc x

  method! class_expr_desc = function%view
    | Pcl_extension ext -> fail Class_expr ext
    | x -> super#class_expr_desc x

  method! class_field_desc = function%view
    | Pcf_extension ext -> fail Class_field ext
    | x -> super#class_field_desc x

  method! module_type_desc = function%view
    | Pmty_extension ext -> fail Module_type ext
    | x -> super#module_type_desc x

  method! signature_item_desc = function%view
    | Psig_extension (ext, _) -> fail Signature_item ext
    | x -> super#signature_item_desc x

  method! module_expr_desc = function%view
    | Pmod_extension ext -> fail Module_expr ext
    | x -> super#module_expr_desc x

  method! structure_item_desc = function%view
    | Pstr_extension (ext, _) -> fail Structure_item ext
    | x -> super#structure_item_desc x
end

module V3 = struct
  type nonrec t = t

  let declare name context pattern k =
    let pattern = Ast_pattern.map_result pattern ~f:(fun x -> Simple x) in
    T (M.declare ~with_arg:false name context pattern
         (fun ~ctxt ~arg:_ -> k ~ctxt))

  let declare_inline name context pattern k =
    check_context_for_inline context ~func:"Extension.declare_inline";
    let pattern = Ast_pattern.map_result pattern ~f:(fun x -> Inline x) in
    T (M.declare ~with_arg:false name context pattern
         (fun ~ctxt ~arg:_ -> k ~ctxt))
end

let declare name context pattern f =
  V3.declare name context pattern (Expansion_context.Extension.with_loc_and_path f)

let declare_inline name context pattern f =
  V3.declare_inline name context pattern (Expansion_context.Extension.with_loc_and_path f)

let declare_with_path_arg name context pattern k =
  let k' = Expansion_context.Extension.with_loc_and_path k in
  let pattern = Ast_pattern.map_result pattern ~f:(fun x -> Simple x) in
  T (M.declare ~with_arg:true  name context pattern k')
;;

let declare_inline_with_path_arg name context pattern k =
  let k' = Expansion_context.Extension.with_loc_and_path k in
  check_context_for_inline context ~func:"Extension.declare_inline_with_path_arg";
  let pattern = Ast_pattern.map_result pattern ~f:(fun x -> Inline x) in
  T (M.declare ~with_arg:true name context pattern k')
;;

let of_bootstrap_extension ext =
  let wrap f ~loc ~path:_ x = f ~loc x in
  match (ext : Ppx_bootstrap.Extension.t) with
  | Patt { name; callback } ->
    declare name Pattern Ast_pattern.__ (wrap callback)
  | Expr { name; callback } ->
    declare name Expression Ast_pattern.__ (wrap callback)
;;

module V2 = struct
  type nonrec t = t

  let declare = declare
  let declare_inline = declare_inline
end
