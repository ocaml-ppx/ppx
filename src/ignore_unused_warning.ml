open Import
open Current_ast
open Ast_builder

let underscore_binding exp =
  let loc = (Expression.pexp_loc exp) in
  value_binding ~loc ~pat:(ppat_any ~loc) ~expr:exp

let vars_of = object
  inherit [Longident_loc.t list] Ast_traverse.fold as super
  method! pattern patt acc =
    match%view (Pattern.ppat_desc patt) with
    | Ppat_var v -> Located.map_lident v :: acc
    | _ -> super#pattern patt acc
end

(* For every [let x = ...] structure item, add a [let _ = x] *)
let add_dummy_user_for_values = object
  inherit Ast_traverse.map as super
  method! structure st =
    let rec loop st acc =
      match%view st with
      | [] -> List.rev acc
      | { pstr_desc = Pstr_value (_, vbs); pstr_loc = loc } as item :: rest ->
        let vars =
          List.fold_left vbs ~init:[] ~f:(fun acc vb ->
            vars_of#pattern (Value_binding.pvb_pat vb) acc)
        in
        let ign =
          pstr_value_list ~loc Rec_flag.nonrecursive
            (List.rev_map vars ~f:(function%view (Longident_loc {loc;_} as v) ->
               underscore_binding (pexp_ident ~loc v)))
        in
        loop rest (ign @ (item :: acc))
      | item :: rest ->
        loop rest (item :: acc)
    in
    Structure.create (loop (Structure.to_concrete (super#structure st)) [])
end
