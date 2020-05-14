open! Import
open Current_ast

class map = object
  inherit Traverse_builtins.map
  inherit Virtual.map
end

class iter = object
  inherit Traverse_builtins.iter
  inherit Virtual.iter
end

class ['acc] fold = object
  inherit ['acc] Traverse_builtins.fold
  inherit ['acc] Virtual.fold
end

class ['acc] fold_map = object
  inherit ['acc] Traverse_builtins.fold_map
  inherit ['acc] Virtual.fold_map
end

class ['ctx] map_with_context = object
  inherit ['ctx] Traverse_builtins.map_with_context
  inherit ['ctx] Virtual.map_with_context
end

class virtual ['res] lift = object
  inherit ['res] Traverse_builtins.lift
  inherit ['res] Virtual.lift
end

let enter name path = if String.is_empty path then name else path ^ "." ^ name

class map_with_path = object
  inherit [string] map_with_context as super

  (* WAS:
     method! structure_item_desc path x =
     match x with
     | Pstr_module mb -> super#structure_item_desc (enter mb.pmb_name.txt path) x
     | _ -> super#structure_item_desc path x

     Overriding [module_binding] seems to be OK because it does not catch
     local module bindings because at the moment the parsetree doesn't make
     use of [module_binding] for local modules, but that might change in the
     future, so this might be something to keep in mind.

     The following:

         module A = struct .. end
         module A = struct .. end

     is disallowed, but

         let _ = .. let module A = struct .. end in ..
         module A = struct .. end
         let _ = .. let module A = struct .. end in ..

     isn't, and the "path" constructed here would be able to differentiate
     between them. *)
  method! module_binding path mb =
    match%view mb with
    | { pmb_name = { txt; _ }; _ } ->
      super#module_binding (enter txt path) mb

  method! module_declaration path md =
    match%view md with
    | { pmd_name = { txt; _ }; _ } ->
      super#module_declaration (enter txt path) md

  method! module_type_declaration path mtd =
    match%view mtd with
    | { pmtd_name = { txt; _ }; _ } ->
      super#module_type_declaration (enter txt path) mtd
end

let var_names_of = object
  inherit [string list] fold as super

  method! pattern p acc =
    let acc = super#pattern p acc in
    match%view p with
    | { ppat_desc = Ppat_var {txt; _}; _ } -> txt :: acc
    | _ -> acc
end

class map_with_expansion_context = object (self)
  inherit [Expansion_context.Base.t] map_with_context as super

  method! expression ctxt expr =
    super#expression (Expansion_context.Base.enter_expr ctxt) expr

  method! module_binding ctxt mb =
    match%view mb with
    | { pmb_loc; pmb_name = { txt; _ }; _ } ->
      super#module_binding
        (Expansion_context.Base.enter_module ~loc:pmb_loc txt ctxt)
        mb

  method! module_declaration ctxt md =
    match%view md with
    | { pmd_loc; pmd_name = { txt; _ }; _ } ->
      super#module_declaration
        (Expansion_context.Base.enter_module ~loc:pmd_loc txt ctxt)
        md

  method! module_type_declaration ctxt mtd =
    match%view mtd with
    | { pmtd_loc; pmtd_name = { txt; _ }; _ } ->
    super#module_type_declaration
      (Expansion_context.Base.enter_module ~loc:pmtd_loc txt ctxt)
      mtd

  method! value_description ctxt vd =
    match%view vd with
    | { pval_loc; pval_name = { txt; _ }; _ } ->
    super#value_description
      (Expansion_context.Base.enter_value ~loc:pval_loc txt ctxt)
      vd

  method! value_binding ctxt pvb =
    match%view pvb with
    | {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} ->
      let all_var_names = var_names_of#pattern pvb_pat [] in
      let var_name = List.last all_var_names in
      let in_binding_ctxt =
        Option.fold var_name
          ~init:ctxt
          ~f:(fun ctxt var_name ->
            Expansion_context.Base.enter_value ~loc:pvb_loc var_name ctxt)
      in
      let pvb_pat = self#pattern ctxt pvb_pat in
      let pvb_expr = self#expression in_binding_ctxt pvb_expr in
      let pvb_attributes = self#attributes in_binding_ctxt pvb_attributes in
      let pvb_loc = self#location ctxt pvb_loc in
      Value_binding.of_concrete { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }
end

class sexp_of = object
  inherit [Sexp.t] Virtual.lift

  method int       = Sexp.Encoder.int
  method string    = Sexp.Encoder.string
  method bool      = Sexp.Encoder.bool
  method char      = Sexp.Encoder.char
  method float     = Sexp.Encoder.float
  method int32     = Sexp.Encoder.int32
  method int64     = Sexp.Encoder.int64
  method nativeint = Sexp.Encoder.nativeint
  method unit      = Sexp.Encoder.unit
  method option    = Sexp.Encoder.option
  method list      = Sexp.Encoder.list
  method array : 'a. ('a -> Sexp.t) -> 'a array -> Sexp.t = Sexp.Encoder.array
  method position (_ : Astlib.Position.t) = Sexp.Atom "_"
  method location (_ : Astlib.Location.t) = Sexp.Atom "_"
  method loc f loc = f loc.txt

  method other : 'a. 'a -> Sexp.t = fun _ -> Sexp.Atom "_"

  method node _ sexp = sexp

  method record _ fields =
    List (List.map fields ~f:(fun (label, sexp) ->
      Sexp.List [Atom label; sexp]))

  method constr _ tag args =
    match args with
    | [] -> Atom tag
    | _  -> List (Atom tag :: args)

  method tuple l = List l
end

let sexp_of = new sexp_of
