open! Import
open Current_ast
open Ast_pattern0

(*$ Ppxlib_cinaps_helpers.generate_ast_pattern_impl () *)
let lident (T f0') =
  T (fun c' l' x' k' ->
    match Longident.to_concrete_opt x' with
    | Some (Lident (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' x0' (k')
      end
    | _ -> fail l' "Lident"
  )

let ldot (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Longident.to_concrete_opt x' with
    | Some (Ldot (x0', x1')) ->
      begin
        c'.matched <- c'.matched + 1;
        f1' c' l' x1' (f0' c' l' x0' (k'))
      end
    | _ -> fail l' "Ldot"
  )

let lapply (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Longident.to_concrete_opt x' with
    | Some (Lapply (x0', x1')) ->
      begin
        c'.matched <- c'.matched + 1;
        f1' c' l' x1' (f0' c' l' x0' (k'))
      end
    | _ -> fail l' "Lapply"
  )

let labelled (T f0') =
  T (fun c' l' x' k' ->
    match Arg_label.to_concrete_opt x' with
    | Some (Labelled (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' x0' (k')
      end
    | _ -> fail l' "Labelled"
  )

let optional (T f0') =
  T (fun c' l' x' k' ->
    match Arg_label.to_concrete_opt x' with
    | Some (Optional (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' x0' (k')
      end
    | _ -> fail l' "Optional"
  )

let pconst_integer (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Constant.to_concrete_opt x' with
    | Some (Pconst_integer (x0', x1')) ->
      begin
        c'.matched <- c'.matched + 1;
        f1' c' l' x1' (f0' c' l' x0' (k'))
      end
    | _ -> fail l' "Pconst_integer"
  )

let pconst_char (T f0') =
  T (fun c' l' x' k' ->
    match Constant.to_concrete_opt x' with
    | Some (Pconst_char (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' x0' (k')
      end
    | _ -> fail l' "Pconst_char"
  )

let pconst_string (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Constant.to_concrete_opt x' with
    | Some (Pconst_string (x0', x1')) ->
      begin
        c'.matched <- c'.matched + 1;
        f1' c' l' x1' (f0' c' l' x0' (k'))
      end
    | _ -> fail l' "Pconst_string"
  )

let pconst_float (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Constant.to_concrete_opt x' with
    | Some (Pconst_float (x0', x1')) ->
      begin
        c'.matched <- c'.matched + 1;
        f1' c' l' x1' (f0' c' l' x0' (k'))
      end
    | _ -> fail l' "Pconst_float"
  )

let pstr (T f0') =
  T (fun c' l' x' k' ->
    match Payload.to_concrete_opt x' with
    | Some (PStr (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' (Structure.to_concrete x0') (k')
      end
    | _ -> fail l' "PStr"
  )

let psig (T f0') =
  T (fun c' l' x' k' ->
    match Payload.to_concrete_opt x' with
    | Some (PSig (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' (Signature.to_concrete x0') (k')
      end
    | _ -> fail l' "PSig"
  )

let ptyp (T f0') =
  T (fun c' l' x' k' ->
    match Payload.to_concrete_opt x' with
    | Some (PTyp (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' x0' (k')
      end
    | _ -> fail l' "PTyp"
  )

let ppat (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Payload.to_concrete_opt x' with
    | Some (PPat (x0', x1')) ->
      begin
        c'.matched <- c'.matched + 1;
        f1' c' l' x1' (f0' c' l' x0' (k'))
      end
    | _ -> fail l' "PPat"
  )

let ptyp_var (T f0') =
  T (fun c' l' x' k' ->
    match Core_type.to_concrete_opt x' with
    | Some { ptyp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Core_type_desc.to_concrete_opt x' with
        | Some (Ptyp_var (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Ptyp_var"
      end
    | _ -> fail l' "Ptyp_var"
  )

let ptyp_arrow (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Core_type.to_concrete_opt x' with
    | Some { ptyp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Core_type_desc.to_concrete_opt x' with
        | Some (Ptyp_arrow (x0', x1', x2')) ->
          begin
            c'.matched <- c'.matched + 1;
            f2' c' l' x2' (f1' c' l' x1' (f0' c' l' x0' (k')))
          end
        | _ -> fail l' "Ptyp_arrow"
      end
    | _ -> fail l' "Ptyp_arrow"
  )

let ptyp_tuple (T f0') =
  T (fun c' l' x' k' ->
    match Core_type.to_concrete_opt x' with
    | Some { ptyp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Core_type_desc.to_concrete_opt x' with
        | Some (Ptyp_tuple (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Ptyp_tuple"
      end
    | _ -> fail l' "Ptyp_tuple"
  )

let ptyp_constr (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Core_type.to_concrete_opt x' with
    | Some { ptyp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Core_type_desc.to_concrete_opt x' with
        | Some (Ptyp_constr (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k'))
          end
        | _ -> fail l' "Ptyp_constr"
      end
    | _ -> fail l' "Ptyp_constr"
  )

let ptyp_object (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Core_type.to_concrete_opt x' with
    | Some { ptyp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Core_type_desc.to_concrete_opt x' with
        | Some (Ptyp_object (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Ptyp_object"
      end
    | _ -> fail l' "Ptyp_object"
  )

let ptyp_class (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Core_type.to_concrete_opt x' with
    | Some { ptyp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Core_type_desc.to_concrete_opt x' with
        | Some (Ptyp_class (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k'))
          end
        | _ -> fail l' "Ptyp_class"
      end
    | _ -> fail l' "Ptyp_class"
  )

let ptyp_alias (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Core_type.to_concrete_opt x' with
    | Some { ptyp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Core_type_desc.to_concrete_opt x' with
        | Some (Ptyp_alias (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Ptyp_alias"
      end
    | _ -> fail l' "Ptyp_alias"
  )

let ptyp_variant (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Core_type.to_concrete_opt x' with
    | Some { ptyp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Core_type_desc.to_concrete_opt x' with
        | Some (Ptyp_variant (x0', x1', x2')) ->
          begin
            c'.matched <- c'.matched + 1;
            f2' c' l' x2' (f1' c' l' x1' (f0' c' l' x0' (k')))
          end
        | _ -> fail l' "Ptyp_variant"
      end
    | _ -> fail l' "Ptyp_variant"
  )

let ptyp_poly (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Core_type.to_concrete_opt x' with
    | Some { ptyp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Core_type_desc.to_concrete_opt x' with
        | Some (Ptyp_poly (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Ptyp_poly"
      end
    | _ -> fail l' "Ptyp_poly"
  )

let ptyp_package (T f0') =
  T (fun c' l' x' k' ->
    match Core_type.to_concrete_opt x' with
    | Some { ptyp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Core_type_desc.to_concrete_opt x' with
        | Some (Ptyp_package (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Package_type.to_concrete x0') (k')
          end
        | _ -> fail l' "Ptyp_package"
      end
    | _ -> fail l' "Ptyp_package"
  )

let ptyp_extension (T f0') =
  T (fun c' l' x' k' ->
    match Core_type.to_concrete_opt x' with
    | Some { ptyp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Core_type_desc.to_concrete_opt x' with
        | Some (Ptyp_extension (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Extension.to_concrete x0') (k')
          end
        | _ -> fail l' "Ptyp_extension"
      end
    | _ -> fail l' "Ptyp_extension"
  )

let ptyp_loc (T f') =
  T (fun c' l' x' k' ->
    match Core_type.to_concrete_opt x' with
    | Some { ptyp_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "ptyp_loc"
  )

let ptyp_attributes (T f') =
  T (fun c' l' x' k' ->
    match Core_type.to_concrete_opt x' with
    | Some { ptyp_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "ptyp_attributes"
  )

let rtag (T f0') (T f1') (T f2') (T f3') =
  T (fun c' l' x' k' ->
    match Row_field.to_concrete_opt x' with
    | Some (Rtag (x0', x1', x2', x3')) ->
      begin
        c'.matched <- c'.matched + 1;
        f3' c' l' x3' (f2' c' l' x2' (f1' c' l' (Attributes.to_concrete x1') (f0' c' l' (Loc.txt x0') (k'))))
      end
    | _ -> fail l' "Rtag"
  )

let rinherit (T f0') =
  T (fun c' l' x' k' ->
    match Row_field.to_concrete_opt x' with
    | Some (Rinherit (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' x0' (k')
      end
    | _ -> fail l' "Rinherit"
  )

let otag (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Object_field.to_concrete_opt x' with
    | Some (Otag (x0', x1', x2')) ->
      begin
        c'.matched <- c'.matched + 1;
        f2' c' l' x2' (f1' c' l' (Attributes.to_concrete x1') (f0' c' l' (Loc.txt x0') (k')))
      end
    | _ -> fail l' "Otag"
  )

let oinherit (T f0') =
  T (fun c' l' x' k' ->
    match Object_field.to_concrete_opt x' with
    | Some (Oinherit (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' x0' (k')
      end
    | _ -> fail l' "Oinherit"
  )

let ppat_var (T f0') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_var (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Loc.txt x0') (k')
          end
        | _ -> fail l' "Ppat_var"
      end
    | _ -> fail l' "Ppat_var"
  )

let ppat_alias (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_alias (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' (Loc.txt x1') (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Ppat_alias"
      end
    | _ -> fail l' "Ppat_alias"
  )

let ppat_constant (T f0') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_constant (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Ppat_constant"
      end
    | _ -> fail l' "Ppat_constant"
  )

let ppat_interval (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_interval (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Ppat_interval"
      end
    | _ -> fail l' "Ppat_interval"
  )

let ppat_tuple (T f0') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_tuple (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Ppat_tuple"
      end
    | _ -> fail l' "Ppat_tuple"
  )

let ppat_construct (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_construct (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k'))
          end
        | _ -> fail l' "Ppat_construct"
      end
    | _ -> fail l' "Ppat_construct"
  )

let ppat_variant (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_variant (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Ppat_variant"
      end
    | _ -> fail l' "Ppat_variant"
  )

let ppat_record (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_record (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Ppat_record"
      end
    | _ -> fail l' "Ppat_record"
  )

let ppat_array (T f0') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_array (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Ppat_array"
      end
    | _ -> fail l' "Ppat_array"
  )

let ppat_or (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_or (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Ppat_or"
      end
    | _ -> fail l' "Ppat_or"
  )

let ppat_constraint (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_constraint (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Ppat_constraint"
      end
    | _ -> fail l' "Ppat_constraint"
  )

let ppat_type (T f0') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_type (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k')
          end
        | _ -> fail l' "Ppat_type"
      end
    | _ -> fail l' "Ppat_type"
  )

let ppat_lazy (T f0') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_lazy (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Ppat_lazy"
      end
    | _ -> fail l' "Ppat_lazy"
  )

let ppat_unpack (T f0') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_unpack (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Loc.txt x0') (k')
          end
        | _ -> fail l' "Ppat_unpack"
      end
    | _ -> fail l' "Ppat_unpack"
  )

let ppat_exception (T f0') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_exception (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Ppat_exception"
      end
    | _ -> fail l' "Ppat_exception"
  )

let ppat_extension (T f0') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_extension (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Extension.to_concrete x0') (k')
          end
        | _ -> fail l' "Ppat_extension"
      end
    | _ -> fail l' "Ppat_extension"
  )

let ppat_open (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Pattern_desc.to_concrete_opt x' with
        | Some (Ppat_open (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k'))
          end
        | _ -> fail l' "Ppat_open"
      end
    | _ -> fail l' "Ppat_open"
  )

let ppat_loc (T f') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "ppat_loc"
  )

let ppat_attributes (T f') =
  T (fun c' l' x' k' ->
    match Pattern.to_concrete_opt x' with
    | Some { ppat_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "ppat_attributes"
  )

let pexp_ident (T f0') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_ident (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k')
          end
        | _ -> fail l' "Pexp_ident"
      end
    | _ -> fail l' "Pexp_ident"
  )

let pexp_constant (T f0') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_constant (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pexp_constant"
      end
    | _ -> fail l' "Pexp_constant"
  )

let pexp_let (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_let (x0', x1', x2')) ->
          begin
            c'.matched <- c'.matched + 1;
            f2' c' l' x2' (f1' c' l' x1' (f0' c' l' x0' (k')))
          end
        | _ -> fail l' "Pexp_let"
      end
    | _ -> fail l' "Pexp_let"
  )

let pexp_function (T f0') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_function (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pexp_function"
      end
    | _ -> fail l' "Pexp_function"
  )

let pexp_fun (T f0') (T f1') (T f2') (T f3') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_fun (x0', x1', x2', x3')) ->
          begin
            c'.matched <- c'.matched + 1;
            f3' c' l' x3' (f2' c' l' x2' (f1' c' l' x1' (f0' c' l' x0' (k'))))
          end
        | _ -> fail l' "Pexp_fun"
      end
    | _ -> fail l' "Pexp_fun"
  )

let pexp_apply (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_apply (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pexp_apply"
      end
    | _ -> fail l' "Pexp_apply"
  )

let pexp_match (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_match (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pexp_match"
      end
    | _ -> fail l' "Pexp_match"
  )

let pexp_try (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_try (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pexp_try"
      end
    | _ -> fail l' "Pexp_try"
  )

let pexp_tuple (T f0') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_tuple (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pexp_tuple"
      end
    | _ -> fail l' "Pexp_tuple"
  )

let pexp_construct (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_construct (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k'))
          end
        | _ -> fail l' "Pexp_construct"
      end
    | _ -> fail l' "Pexp_construct"
  )

let pexp_variant (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_variant (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pexp_variant"
      end
    | _ -> fail l' "Pexp_variant"
  )

let pexp_record (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_record (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pexp_record"
      end
    | _ -> fail l' "Pexp_record"
  )

let pexp_field (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_field (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' (Loc.txt (Longident_loc.to_concrete x1')) (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pexp_field"
      end
    | _ -> fail l' "Pexp_field"
  )

let pexp_setfield (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_setfield (x0', x1', x2')) ->
          begin
            c'.matched <- c'.matched + 1;
            f2' c' l' x2' (f1' c' l' (Loc.txt (Longident_loc.to_concrete x1')) (f0' c' l' x0' (k')))
          end
        | _ -> fail l' "Pexp_setfield"
      end
    | _ -> fail l' "Pexp_setfield"
  )

let pexp_array (T f0') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_array (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pexp_array"
      end
    | _ -> fail l' "Pexp_array"
  )

let pexp_ifthenelse (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_ifthenelse (x0', x1', x2')) ->
          begin
            c'.matched <- c'.matched + 1;
            f2' c' l' x2' (f1' c' l' x1' (f0' c' l' x0' (k')))
          end
        | _ -> fail l' "Pexp_ifthenelse"
      end
    | _ -> fail l' "Pexp_ifthenelse"
  )

let pexp_sequence (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_sequence (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pexp_sequence"
      end
    | _ -> fail l' "Pexp_sequence"
  )

let pexp_while (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_while (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pexp_while"
      end
    | _ -> fail l' "Pexp_while"
  )

let pexp_for (T f0') (T f1') (T f2') (T f3') (T f4') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_for (x0', x1', x2', x3', x4')) ->
          begin
            c'.matched <- c'.matched + 1;
            f4' c' l' x4' (f3' c' l' x3' (f2' c' l' x2' (f1' c' l' x1' (f0' c' l' x0' (k')))))
          end
        | _ -> fail l' "Pexp_for"
      end
    | _ -> fail l' "Pexp_for"
  )

let pexp_constraint (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_constraint (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pexp_constraint"
      end
    | _ -> fail l' "Pexp_constraint"
  )

let pexp_coerce (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_coerce (x0', x1', x2')) ->
          begin
            c'.matched <- c'.matched + 1;
            f2' c' l' x2' (f1' c' l' x1' (f0' c' l' x0' (k')))
          end
        | _ -> fail l' "Pexp_coerce"
      end
    | _ -> fail l' "Pexp_coerce"
  )

let pexp_send (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_send (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' (Loc.txt x1') (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pexp_send"
      end
    | _ -> fail l' "Pexp_send"
  )

let pexp_new (T f0') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_new (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k')
          end
        | _ -> fail l' "Pexp_new"
      end
    | _ -> fail l' "Pexp_new"
  )

let pexp_setinstvar (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_setinstvar (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' (Loc.txt x0') (k'))
          end
        | _ -> fail l' "Pexp_setinstvar"
      end
    | _ -> fail l' "Pexp_setinstvar"
  )

let pexp_override (T f0') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_override (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pexp_override"
      end
    | _ -> fail l' "Pexp_override"
  )

let pexp_letmodule (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_letmodule (x0', x1', x2')) ->
          begin
            c'.matched <- c'.matched + 1;
            f2' c' l' x2' (f1' c' l' x1' (f0' c' l' (Loc.txt x0') (k')))
          end
        | _ -> fail l' "Pexp_letmodule"
      end
    | _ -> fail l' "Pexp_letmodule"
  )

let pexp_letexception (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_letexception (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pexp_letexception"
      end
    | _ -> fail l' "Pexp_letexception"
  )

let pexp_assert (T f0') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_assert (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pexp_assert"
      end
    | _ -> fail l' "Pexp_assert"
  )

let pexp_lazy (T f0') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_lazy (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pexp_lazy"
      end
    | _ -> fail l' "Pexp_lazy"
  )

let pexp_poly (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_poly (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pexp_poly"
      end
    | _ -> fail l' "Pexp_poly"
  )

let pexp_object (T f0') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_object (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pexp_object"
      end
    | _ -> fail l' "Pexp_object"
  )

let pexp_newtype (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_newtype (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' (Loc.txt x0') (k'))
          end
        | _ -> fail l' "Pexp_newtype"
      end
    | _ -> fail l' "Pexp_newtype"
  )

let pexp_pack (T f0') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_pack (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pexp_pack"
      end
    | _ -> fail l' "Pexp_pack"
  )

let pexp_open (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_open (x0', x1', x2')) ->
          begin
            c'.matched <- c'.matched + 1;
            f2' c' l' x2' (f1' c' l' (Loc.txt (Longident_loc.to_concrete x1')) (f0' c' l' x0' (k')))
          end
        | _ -> fail l' "Pexp_open"
      end
    | _ -> fail l' "Pexp_open"
  )

let pexp_extension (T f0') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Expression_desc.to_concrete_opt x' with
        | Some (Pexp_extension (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Extension.to_concrete x0') (k')
          end
        | _ -> fail l' "Pexp_extension"
      end
    | _ -> fail l' "Pexp_extension"
  )

let pexp_loc (T f') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pexp_loc"
  )

let pexp_attributes (T f') =
  T (fun c' l' x' k' ->
    match Expression.to_concrete_opt x' with
    | Some { pexp_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pexp_attributes"
  )

let pval_attributes (T f') =
  T (fun c' l' x' k' ->
    match Value_description.to_concrete_opt x' with
    | Some { pval_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pval_attributes"
  )

let pval_loc (T f') =
  T (fun c' l' x' k' ->
    match Value_description.to_concrete_opt x' with
    | Some { pval_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pval_loc"
  )

let ptype_attributes (T f') =
  T (fun c' l' x' k' ->
    match Type_declaration.to_concrete_opt x' with
    | Some { ptype_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "ptype_attributes"
  )

let ptype_loc (T f') =
  T (fun c' l' x' k' ->
    match Type_declaration.to_concrete_opt x' with
    | Some { ptype_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "ptype_loc"
  )

let ptype_variant (T f0') =
  T (fun c' l' x' k' ->
    match Type_kind.to_concrete_opt x' with
    | Some (Ptype_variant (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' x0' (k')
      end
    | _ -> fail l' "Ptype_variant"
  )

let ptype_record (T f0') =
  T (fun c' l' x' k' ->
    match Type_kind.to_concrete_opt x' with
    | Some (Ptype_record (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' x0' (k')
      end
    | _ -> fail l' "Ptype_record"
  )

let pld_loc (T f') =
  T (fun c' l' x' k' ->
    match Label_declaration.to_concrete_opt x' with
    | Some { pld_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pld_loc"
  )

let pld_attributes (T f') =
  T (fun c' l' x' k' ->
    match Label_declaration.to_concrete_opt x' with
    | Some { pld_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pld_attributes"
  )

let pcd_loc (T f') =
  T (fun c' l' x' k' ->
    match Constructor_declaration.to_concrete_opt x' with
    | Some { pcd_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pcd_loc"
  )

let pcd_attributes (T f') =
  T (fun c' l' x' k' ->
    match Constructor_declaration.to_concrete_opt x' with
    | Some { pcd_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pcd_attributes"
  )

let pcstr_tuple (T f0') =
  T (fun c' l' x' k' ->
    match Constructor_arguments.to_concrete_opt x' with
    | Some (Pcstr_tuple (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' x0' (k')
      end
    | _ -> fail l' "Pcstr_tuple"
  )

let pcstr_record (T f0') =
  T (fun c' l' x' k' ->
    match Constructor_arguments.to_concrete_opt x' with
    | Some (Pcstr_record (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' x0' (k')
      end
    | _ -> fail l' "Pcstr_record"
  )

let ptyext_attributes (T f') =
  T (fun c' l' x' k' ->
    match Type_extension.to_concrete_opt x' with
    | Some { ptyext_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "ptyext_attributes"
  )

let pext_loc (T f') =
  T (fun c' l' x' k' ->
    match Extension_constructor.to_concrete_opt x' with
    | Some { pext_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pext_loc"
  )

let pext_attributes (T f') =
  T (fun c' l' x' k' ->
    match Extension_constructor.to_concrete_opt x' with
    | Some { pext_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pext_attributes"
  )

let pext_decl (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Extension_constructor_kind.to_concrete_opt x' with
    | Some (Pext_decl (x0', x1')) ->
      begin
        c'.matched <- c'.matched + 1;
        f1' c' l' x1' (f0' c' l' x0' (k'))
      end
    | _ -> fail l' "Pext_decl"
  )

let pext_rebind (T f0') =
  T (fun c' l' x' k' ->
    match Extension_constructor_kind.to_concrete_opt x' with
    | Some (Pext_rebind (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k')
      end
    | _ -> fail l' "Pext_rebind"
  )

let pcty_constr (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Class_type.to_concrete_opt x' with
    | Some { pcty_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_type_desc.to_concrete_opt x' with
        | Some (Pcty_constr (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k'))
          end
        | _ -> fail l' "Pcty_constr"
      end
    | _ -> fail l' "Pcty_constr"
  )

let pcty_signature (T f0') =
  T (fun c' l' x' k' ->
    match Class_type.to_concrete_opt x' with
    | Some { pcty_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_type_desc.to_concrete_opt x' with
        | Some (Pcty_signature (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pcty_signature"
      end
    | _ -> fail l' "Pcty_signature"
  )

let pcty_arrow (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Class_type.to_concrete_opt x' with
    | Some { pcty_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_type_desc.to_concrete_opt x' with
        | Some (Pcty_arrow (x0', x1', x2')) ->
          begin
            c'.matched <- c'.matched + 1;
            f2' c' l' x2' (f1' c' l' x1' (f0' c' l' x0' (k')))
          end
        | _ -> fail l' "Pcty_arrow"
      end
    | _ -> fail l' "Pcty_arrow"
  )

let pcty_extension (T f0') =
  T (fun c' l' x' k' ->
    match Class_type.to_concrete_opt x' with
    | Some { pcty_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_type_desc.to_concrete_opt x' with
        | Some (Pcty_extension (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Extension.to_concrete x0') (k')
          end
        | _ -> fail l' "Pcty_extension"
      end
    | _ -> fail l' "Pcty_extension"
  )

let pcty_open (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Class_type.to_concrete_opt x' with
    | Some { pcty_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_type_desc.to_concrete_opt x' with
        | Some (Pcty_open (x0', x1', x2')) ->
          begin
            c'.matched <- c'.matched + 1;
            f2' c' l' x2' (f1' c' l' (Loc.txt (Longident_loc.to_concrete x1')) (f0' c' l' x0' (k')))
          end
        | _ -> fail l' "Pcty_open"
      end
    | _ -> fail l' "Pcty_open"
  )

let pcty_loc (T f') =
  T (fun c' l' x' k' ->
    match Class_type.to_concrete_opt x' with
    | Some { pcty_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pcty_loc"
  )

let pcty_attributes (T f') =
  T (fun c' l' x' k' ->
    match Class_type.to_concrete_opt x' with
    | Some { pcty_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pcty_attributes"
  )

let pctf_inherit (T f0') =
  T (fun c' l' x' k' ->
    match Class_type_field.to_concrete_opt x' with
    | Some { pctf_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_type_field_desc.to_concrete_opt x' with
        | Some (Pctf_inherit (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pctf_inherit"
      end
    | _ -> fail l' "Pctf_inherit"
  )

let pctf_val (T f0') =
  T (fun c' l' x' k' ->
    match Class_type_field.to_concrete_opt x' with
    | Some { pctf_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_type_field_desc.to_concrete_opt x' with
        | Some (Pctf_val (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pctf_val"
      end
    | _ -> fail l' "Pctf_val"
  )

let pctf_method (T f0') =
  T (fun c' l' x' k' ->
    match Class_type_field.to_concrete_opt x' with
    | Some { pctf_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_type_field_desc.to_concrete_opt x' with
        | Some (Pctf_method (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pctf_method"
      end
    | _ -> fail l' "Pctf_method"
  )

let pctf_constraint (T f0') =
  T (fun c' l' x' k' ->
    match Class_type_field.to_concrete_opt x' with
    | Some { pctf_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_type_field_desc.to_concrete_opt x' with
        | Some (Pctf_constraint (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pctf_constraint"
      end
    | _ -> fail l' "Pctf_constraint"
  )

let pctf_attribute (T f0') =
  T (fun c' l' x' k' ->
    match Class_type_field.to_concrete_opt x' with
    | Some { pctf_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_type_field_desc.to_concrete_opt x' with
        | Some (Pctf_attribute (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Attribute.to_concrete x0') (k')
          end
        | _ -> fail l' "Pctf_attribute"
      end
    | _ -> fail l' "Pctf_attribute"
  )

let pctf_extension (T f0') =
  T (fun c' l' x' k' ->
    match Class_type_field.to_concrete_opt x' with
    | Some { pctf_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_type_field_desc.to_concrete_opt x' with
        | Some (Pctf_extension (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Extension.to_concrete x0') (k')
          end
        | _ -> fail l' "Pctf_extension"
      end
    | _ -> fail l' "Pctf_extension"
  )

let pctf_loc (T f') =
  T (fun c' l' x' k' ->
    match Class_type_field.to_concrete_opt x' with
    | Some { pctf_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pctf_loc"
  )

let pctf_attributes (T f') =
  T (fun c' l' x' k' ->
    match Class_type_field.to_concrete_opt x' with
    | Some { pctf_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pctf_attributes"
  )

let pci_loc (T f') =
  T (fun c' l' x' k' ->
    match Class_infos.to_concrete_opt x' with
    | Some { pci_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pci_loc"
  )

let pci_attributes (T f') =
  T (fun c' l' x' k' ->
    match Class_infos.to_concrete_opt x' with
    | Some { pci_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pci_attributes"
  )

let pcl_constr (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Class_expr.to_concrete_opt x' with
    | Some { pcl_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_expr_desc.to_concrete_opt x' with
        | Some (Pcl_constr (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k'))
          end
        | _ -> fail l' "Pcl_constr"
      end
    | _ -> fail l' "Pcl_constr"
  )

let pcl_structure (T f0') =
  T (fun c' l' x' k' ->
    match Class_expr.to_concrete_opt x' with
    | Some { pcl_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_expr_desc.to_concrete_opt x' with
        | Some (Pcl_structure (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pcl_structure"
      end
    | _ -> fail l' "Pcl_structure"
  )

let pcl_fun (T f0') (T f1') (T f2') (T f3') =
  T (fun c' l' x' k' ->
    match Class_expr.to_concrete_opt x' with
    | Some { pcl_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_expr_desc.to_concrete_opt x' with
        | Some (Pcl_fun (x0', x1', x2', x3')) ->
          begin
            c'.matched <- c'.matched + 1;
            f3' c' l' x3' (f2' c' l' x2' (f1' c' l' x1' (f0' c' l' x0' (k'))))
          end
        | _ -> fail l' "Pcl_fun"
      end
    | _ -> fail l' "Pcl_fun"
  )

let pcl_apply (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Class_expr.to_concrete_opt x' with
    | Some { pcl_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_expr_desc.to_concrete_opt x' with
        | Some (Pcl_apply (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pcl_apply"
      end
    | _ -> fail l' "Pcl_apply"
  )

let pcl_let (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Class_expr.to_concrete_opt x' with
    | Some { pcl_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_expr_desc.to_concrete_opt x' with
        | Some (Pcl_let (x0', x1', x2')) ->
          begin
            c'.matched <- c'.matched + 1;
            f2' c' l' x2' (f1' c' l' x1' (f0' c' l' x0' (k')))
          end
        | _ -> fail l' "Pcl_let"
      end
    | _ -> fail l' "Pcl_let"
  )

let pcl_constraint (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Class_expr.to_concrete_opt x' with
    | Some { pcl_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_expr_desc.to_concrete_opt x' with
        | Some (Pcl_constraint (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pcl_constraint"
      end
    | _ -> fail l' "Pcl_constraint"
  )

let pcl_extension (T f0') =
  T (fun c' l' x' k' ->
    match Class_expr.to_concrete_opt x' with
    | Some { pcl_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_expr_desc.to_concrete_opt x' with
        | Some (Pcl_extension (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Extension.to_concrete x0') (k')
          end
        | _ -> fail l' "Pcl_extension"
      end
    | _ -> fail l' "Pcl_extension"
  )

let pcl_open (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Class_expr.to_concrete_opt x' with
    | Some { pcl_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_expr_desc.to_concrete_opt x' with
        | Some (Pcl_open (x0', x1', x2')) ->
          begin
            c'.matched <- c'.matched + 1;
            f2' c' l' x2' (f1' c' l' (Loc.txt (Longident_loc.to_concrete x1')) (f0' c' l' x0' (k')))
          end
        | _ -> fail l' "Pcl_open"
      end
    | _ -> fail l' "Pcl_open"
  )

let pcl_loc (T f') =
  T (fun c' l' x' k' ->
    match Class_expr.to_concrete_opt x' with
    | Some { pcl_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pcl_loc"
  )

let pcl_attributes (T f') =
  T (fun c' l' x' k' ->
    match Class_expr.to_concrete_opt x' with
    | Some { pcl_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pcl_attributes"
  )

let pcf_inherit (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Class_field.to_concrete_opt x' with
    | Some { pcf_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_field_desc.to_concrete_opt x' with
        | Some (Pcf_inherit (x0', x1', x2')) ->
          begin
            c'.matched <- c'.matched + 1;
            f2' c' l' x2' (f1' c' l' x1' (f0' c' l' x0' (k')))
          end
        | _ -> fail l' "Pcf_inherit"
      end
    | _ -> fail l' "Pcf_inherit"
  )

let pcf_val (T f0') =
  T (fun c' l' x' k' ->
    match Class_field.to_concrete_opt x' with
    | Some { pcf_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_field_desc.to_concrete_opt x' with
        | Some (Pcf_val (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pcf_val"
      end
    | _ -> fail l' "Pcf_val"
  )

let pcf_method (T f0') =
  T (fun c' l' x' k' ->
    match Class_field.to_concrete_opt x' with
    | Some { pcf_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_field_desc.to_concrete_opt x' with
        | Some (Pcf_method (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pcf_method"
      end
    | _ -> fail l' "Pcf_method"
  )

let pcf_constraint (T f0') =
  T (fun c' l' x' k' ->
    match Class_field.to_concrete_opt x' with
    | Some { pcf_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_field_desc.to_concrete_opt x' with
        | Some (Pcf_constraint (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pcf_constraint"
      end
    | _ -> fail l' "Pcf_constraint"
  )

let pcf_initializer (T f0') =
  T (fun c' l' x' k' ->
    match Class_field.to_concrete_opt x' with
    | Some { pcf_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_field_desc.to_concrete_opt x' with
        | Some (Pcf_initializer (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pcf_initializer"
      end
    | _ -> fail l' "Pcf_initializer"
  )

let pcf_attribute (T f0') =
  T (fun c' l' x' k' ->
    match Class_field.to_concrete_opt x' with
    | Some { pcf_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_field_desc.to_concrete_opt x' with
        | Some (Pcf_attribute (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Attribute.to_concrete x0') (k')
          end
        | _ -> fail l' "Pcf_attribute"
      end
    | _ -> fail l' "Pcf_attribute"
  )

let pcf_extension (T f0') =
  T (fun c' l' x' k' ->
    match Class_field.to_concrete_opt x' with
    | Some { pcf_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Class_field_desc.to_concrete_opt x' with
        | Some (Pcf_extension (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Extension.to_concrete x0') (k')
          end
        | _ -> fail l' "Pcf_extension"
      end
    | _ -> fail l' "Pcf_extension"
  )

let pcf_loc (T f') =
  T (fun c' l' x' k' ->
    match Class_field.to_concrete_opt x' with
    | Some { pcf_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pcf_loc"
  )

let pcf_attributes (T f') =
  T (fun c' l' x' k' ->
    match Class_field.to_concrete_opt x' with
    | Some { pcf_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pcf_attributes"
  )

let cfk_virtual (T f0') =
  T (fun c' l' x' k' ->
    match Class_field_kind.to_concrete_opt x' with
    | Some (Cfk_virtual (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' x0' (k')
      end
    | _ -> fail l' "Cfk_virtual"
  )

let cfk_concrete (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Class_field_kind.to_concrete_opt x' with
    | Some (Cfk_concrete (x0', x1')) ->
      begin
        c'.matched <- c'.matched + 1;
        f1' c' l' x1' (f0' c' l' x0' (k'))
      end
    | _ -> fail l' "Cfk_concrete"
  )

let pmty_ident (T f0') =
  T (fun c' l' x' k' ->
    match Module_type.to_concrete_opt x' with
    | Some { pmty_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Module_type_desc.to_concrete_opt x' with
        | Some (Pmty_ident (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k')
          end
        | _ -> fail l' "Pmty_ident"
      end
    | _ -> fail l' "Pmty_ident"
  )

let pmty_signature (T f0') =
  T (fun c' l' x' k' ->
    match Module_type.to_concrete_opt x' with
    | Some { pmty_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Module_type_desc.to_concrete_opt x' with
        | Some (Pmty_signature (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Signature.to_concrete x0') (k')
          end
        | _ -> fail l' "Pmty_signature"
      end
    | _ -> fail l' "Pmty_signature"
  )

let pmty_functor (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Module_type.to_concrete_opt x' with
    | Some { pmty_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Module_type_desc.to_concrete_opt x' with
        | Some (Pmty_functor (x0', x1', x2')) ->
          begin
            c'.matched <- c'.matched + 1;
            f2' c' l' x2' (f1' c' l' x1' (f0' c' l' (Loc.txt x0') (k')))
          end
        | _ -> fail l' "Pmty_functor"
      end
    | _ -> fail l' "Pmty_functor"
  )

let pmty_with (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Module_type.to_concrete_opt x' with
    | Some { pmty_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Module_type_desc.to_concrete_opt x' with
        | Some (Pmty_with (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pmty_with"
      end
    | _ -> fail l' "Pmty_with"
  )

let pmty_typeof (T f0') =
  T (fun c' l' x' k' ->
    match Module_type.to_concrete_opt x' with
    | Some { pmty_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Module_type_desc.to_concrete_opt x' with
        | Some (Pmty_typeof (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pmty_typeof"
      end
    | _ -> fail l' "Pmty_typeof"
  )

let pmty_extension (T f0') =
  T (fun c' l' x' k' ->
    match Module_type.to_concrete_opt x' with
    | Some { pmty_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Module_type_desc.to_concrete_opt x' with
        | Some (Pmty_extension (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Extension.to_concrete x0') (k')
          end
        | _ -> fail l' "Pmty_extension"
      end
    | _ -> fail l' "Pmty_extension"
  )

let pmty_alias (T f0') =
  T (fun c' l' x' k' ->
    match Module_type.to_concrete_opt x' with
    | Some { pmty_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Module_type_desc.to_concrete_opt x' with
        | Some (Pmty_alias (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k')
          end
        | _ -> fail l' "Pmty_alias"
      end
    | _ -> fail l' "Pmty_alias"
  )

let pmty_loc (T f') =
  T (fun c' l' x' k' ->
    match Module_type.to_concrete_opt x' with
    | Some { pmty_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pmty_loc"
  )

let pmty_attributes (T f') =
  T (fun c' l' x' k' ->
    match Module_type.to_concrete_opt x' with
    | Some { pmty_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pmty_attributes"
  )

let psig_value (T f0') =
  T (fun c' l' x' k' ->
    match Signature_item.to_concrete_opt x' with
    | Some { psig_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Signature_item_desc.to_concrete_opt x' with
        | Some (Psig_value (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Psig_value"
      end
    | _ -> fail l' "Psig_value"
  )

let psig_type (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Signature_item.to_concrete_opt x' with
    | Some { psig_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Signature_item_desc.to_concrete_opt x' with
        | Some (Psig_type (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Psig_type"
      end
    | _ -> fail l' "Psig_type"
  )

let psig_typext (T f0') =
  T (fun c' l' x' k' ->
    match Signature_item.to_concrete_opt x' with
    | Some { psig_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Signature_item_desc.to_concrete_opt x' with
        | Some (Psig_typext (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Psig_typext"
      end
    | _ -> fail l' "Psig_typext"
  )

let psig_exception (T f0') =
  T (fun c' l' x' k' ->
    match Signature_item.to_concrete_opt x' with
    | Some { psig_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Signature_item_desc.to_concrete_opt x' with
        | Some (Psig_exception (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Psig_exception"
      end
    | _ -> fail l' "Psig_exception"
  )

let psig_module (T f0') =
  T (fun c' l' x' k' ->
    match Signature_item.to_concrete_opt x' with
    | Some { psig_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Signature_item_desc.to_concrete_opt x' with
        | Some (Psig_module (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Psig_module"
      end
    | _ -> fail l' "Psig_module"
  )

let psig_recmodule (T f0') =
  T (fun c' l' x' k' ->
    match Signature_item.to_concrete_opt x' with
    | Some { psig_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Signature_item_desc.to_concrete_opt x' with
        | Some (Psig_recmodule (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Psig_recmodule"
      end
    | _ -> fail l' "Psig_recmodule"
  )

let psig_modtype (T f0') =
  T (fun c' l' x' k' ->
    match Signature_item.to_concrete_opt x' with
    | Some { psig_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Signature_item_desc.to_concrete_opt x' with
        | Some (Psig_modtype (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Psig_modtype"
      end
    | _ -> fail l' "Psig_modtype"
  )

let psig_open (T f0') =
  T (fun c' l' x' k' ->
    match Signature_item.to_concrete_opt x' with
    | Some { psig_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Signature_item_desc.to_concrete_opt x' with
        | Some (Psig_open (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Psig_open"
      end
    | _ -> fail l' "Psig_open"
  )

let psig_include (T f0') =
  T (fun c' l' x' k' ->
    match Signature_item.to_concrete_opt x' with
    | Some { psig_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Signature_item_desc.to_concrete_opt x' with
        | Some (Psig_include (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Include_description.to_concrete x0') (k')
          end
        | _ -> fail l' "Psig_include"
      end
    | _ -> fail l' "Psig_include"
  )

let psig_class (T f0') =
  T (fun c' l' x' k' ->
    match Signature_item.to_concrete_opt x' with
    | Some { psig_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Signature_item_desc.to_concrete_opt x' with
        | Some (Psig_class (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Psig_class"
      end
    | _ -> fail l' "Psig_class"
  )

let psig_class_type (T f0') =
  T (fun c' l' x' k' ->
    match Signature_item.to_concrete_opt x' with
    | Some { psig_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Signature_item_desc.to_concrete_opt x' with
        | Some (Psig_class_type (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Psig_class_type"
      end
    | _ -> fail l' "Psig_class_type"
  )

let psig_attribute (T f0') =
  T (fun c' l' x' k' ->
    match Signature_item.to_concrete_opt x' with
    | Some { psig_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Signature_item_desc.to_concrete_opt x' with
        | Some (Psig_attribute (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Attribute.to_concrete x0') (k')
          end
        | _ -> fail l' "Psig_attribute"
      end
    | _ -> fail l' "Psig_attribute"
  )

let psig_extension (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Signature_item.to_concrete_opt x' with
    | Some { psig_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Signature_item_desc.to_concrete_opt x' with
        | Some (Psig_extension (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' (Attributes.to_concrete x1') (f0' c' l' (Extension.to_concrete x0') (k'))
          end
        | _ -> fail l' "Psig_extension"
      end
    | _ -> fail l' "Psig_extension"
  )

let psig_loc (T f') =
  T (fun c' l' x' k' ->
    match Signature_item.to_concrete_opt x' with
    | Some { psig_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "psig_loc"
  )

let pmd_attributes (T f') =
  T (fun c' l' x' k' ->
    match Module_declaration.to_concrete_opt x' with
    | Some { pmd_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pmd_attributes"
  )

let pmd_loc (T f') =
  T (fun c' l' x' k' ->
    match Module_declaration.to_concrete_opt x' with
    | Some { pmd_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pmd_loc"
  )

let pmtd_attributes (T f') =
  T (fun c' l' x' k' ->
    match Module_type_declaration.to_concrete_opt x' with
    | Some { pmtd_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pmtd_attributes"
  )

let pmtd_loc (T f') =
  T (fun c' l' x' k' ->
    match Module_type_declaration.to_concrete_opt x' with
    | Some { pmtd_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pmtd_loc"
  )

let popen_loc (T f') =
  T (fun c' l' x' k' ->
    match Open_description.to_concrete_opt x' with
    | Some { popen_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "popen_loc"
  )

let popen_attributes (T f') =
  T (fun c' l' x' k' ->
    match Open_description.to_concrete_opt x' with
    | Some { popen_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "popen_attributes"
  )

let pincl_loc (T f') =
  T (fun c' l' x' k' ->
    match Include_infos.to_concrete_opt x' with
    | Some { pincl_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pincl_loc"
  )

let pincl_attributes (T f') =
  T (fun c' l' x' k' ->
    match Include_infos.to_concrete_opt x' with
    | Some { pincl_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pincl_attributes"
  )

let pwith_type (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match With_constraint.to_concrete_opt x' with
    | Some (Pwith_type (x0', x1')) ->
      begin
        c'.matched <- c'.matched + 1;
        f1' c' l' x1' (f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k'))
      end
    | _ -> fail l' "Pwith_type"
  )

let pwith_module (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match With_constraint.to_concrete_opt x' with
    | Some (Pwith_module (x0', x1')) ->
      begin
        c'.matched <- c'.matched + 1;
        f1' c' l' (Loc.txt (Longident_loc.to_concrete x1')) (f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k'))
      end
    | _ -> fail l' "Pwith_module"
  )

let pwith_typesubst (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match With_constraint.to_concrete_opt x' with
    | Some (Pwith_typesubst (x0', x1')) ->
      begin
        c'.matched <- c'.matched + 1;
        f1' c' l' x1' (f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k'))
      end
    | _ -> fail l' "Pwith_typesubst"
  )

let pwith_modsubst (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match With_constraint.to_concrete_opt x' with
    | Some (Pwith_modsubst (x0', x1')) ->
      begin
        c'.matched <- c'.matched + 1;
        f1' c' l' (Loc.txt (Longident_loc.to_concrete x1')) (f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k'))
      end
    | _ -> fail l' "Pwith_modsubst"
  )

let pmod_ident (T f0') =
  T (fun c' l' x' k' ->
    match Module_expr.to_concrete_opt x' with
    | Some { pmod_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Module_expr_desc.to_concrete_opt x' with
        | Some (Pmod_ident (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Loc.txt (Longident_loc.to_concrete x0')) (k')
          end
        | _ -> fail l' "Pmod_ident"
      end
    | _ -> fail l' "Pmod_ident"
  )

let pmod_structure (T f0') =
  T (fun c' l' x' k' ->
    match Module_expr.to_concrete_opt x' with
    | Some { pmod_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Module_expr_desc.to_concrete_opt x' with
        | Some (Pmod_structure (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Structure.to_concrete x0') (k')
          end
        | _ -> fail l' "Pmod_structure"
      end
    | _ -> fail l' "Pmod_structure"
  )

let pmod_functor (T f0') (T f1') (T f2') =
  T (fun c' l' x' k' ->
    match Module_expr.to_concrete_opt x' with
    | Some { pmod_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Module_expr_desc.to_concrete_opt x' with
        | Some (Pmod_functor (x0', x1', x2')) ->
          begin
            c'.matched <- c'.matched + 1;
            f2' c' l' x2' (f1' c' l' x1' (f0' c' l' (Loc.txt x0') (k')))
          end
        | _ -> fail l' "Pmod_functor"
      end
    | _ -> fail l' "Pmod_functor"
  )

let pmod_apply (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Module_expr.to_concrete_opt x' with
    | Some { pmod_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Module_expr_desc.to_concrete_opt x' with
        | Some (Pmod_apply (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pmod_apply"
      end
    | _ -> fail l' "Pmod_apply"
  )

let pmod_constraint (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Module_expr.to_concrete_opt x' with
    | Some { pmod_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Module_expr_desc.to_concrete_opt x' with
        | Some (Pmod_constraint (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pmod_constraint"
      end
    | _ -> fail l' "Pmod_constraint"
  )

let pmod_unpack (T f0') =
  T (fun c' l' x' k' ->
    match Module_expr.to_concrete_opt x' with
    | Some { pmod_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Module_expr_desc.to_concrete_opt x' with
        | Some (Pmod_unpack (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pmod_unpack"
      end
    | _ -> fail l' "Pmod_unpack"
  )

let pmod_extension (T f0') =
  T (fun c' l' x' k' ->
    match Module_expr.to_concrete_opt x' with
    | Some { pmod_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Module_expr_desc.to_concrete_opt x' with
        | Some (Pmod_extension (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Extension.to_concrete x0') (k')
          end
        | _ -> fail l' "Pmod_extension"
      end
    | _ -> fail l' "Pmod_extension"
  )

let pmod_loc (T f') =
  T (fun c' l' x' k' ->
    match Module_expr.to_concrete_opt x' with
    | Some { pmod_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pmod_loc"
  )

let pmod_attributes (T f') =
  T (fun c' l' x' k' ->
    match Module_expr.to_concrete_opt x' with
    | Some { pmod_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pmod_attributes"
  )

let pstr_eval (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Structure_item_desc.to_concrete_opt x' with
        | Some (Pstr_eval (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' (Attributes.to_concrete x1') (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pstr_eval"
      end
    | _ -> fail l' "Pstr_eval"
  )

let pstr_value (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Structure_item_desc.to_concrete_opt x' with
        | Some (Pstr_value (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pstr_value"
      end
    | _ -> fail l' "Pstr_value"
  )

let pstr_primitive (T f0') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Structure_item_desc.to_concrete_opt x' with
        | Some (Pstr_primitive (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pstr_primitive"
      end
    | _ -> fail l' "Pstr_primitive"
  )

let pstr_type (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Structure_item_desc.to_concrete_opt x' with
        | Some (Pstr_type (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' x1' (f0' c' l' x0' (k'))
          end
        | _ -> fail l' "Pstr_type"
      end
    | _ -> fail l' "Pstr_type"
  )

let pstr_typext (T f0') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Structure_item_desc.to_concrete_opt x' with
        | Some (Pstr_typext (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pstr_typext"
      end
    | _ -> fail l' "Pstr_typext"
  )

let pstr_exception (T f0') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Structure_item_desc.to_concrete_opt x' with
        | Some (Pstr_exception (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pstr_exception"
      end
    | _ -> fail l' "Pstr_exception"
  )

let pstr_module (T f0') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Structure_item_desc.to_concrete_opt x' with
        | Some (Pstr_module (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pstr_module"
      end
    | _ -> fail l' "Pstr_module"
  )

let pstr_recmodule (T f0') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Structure_item_desc.to_concrete_opt x' with
        | Some (Pstr_recmodule (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pstr_recmodule"
      end
    | _ -> fail l' "Pstr_recmodule"
  )

let pstr_modtype (T f0') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Structure_item_desc.to_concrete_opt x' with
        | Some (Pstr_modtype (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pstr_modtype"
      end
    | _ -> fail l' "Pstr_modtype"
  )

let pstr_open (T f0') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Structure_item_desc.to_concrete_opt x' with
        | Some (Pstr_open (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pstr_open"
      end
    | _ -> fail l' "Pstr_open"
  )

let pstr_class (T f0') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Structure_item_desc.to_concrete_opt x' with
        | Some (Pstr_class (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pstr_class"
      end
    | _ -> fail l' "Pstr_class"
  )

let pstr_class_type (T f0') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Structure_item_desc.to_concrete_opt x' with
        | Some (Pstr_class_type (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' x0' (k')
          end
        | _ -> fail l' "Pstr_class_type"
      end
    | _ -> fail l' "Pstr_class_type"
  )

let pstr_include (T f0') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Structure_item_desc.to_concrete_opt x' with
        | Some (Pstr_include (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Include_declaration.to_concrete x0') (k')
          end
        | _ -> fail l' "Pstr_include"
      end
    | _ -> fail l' "Pstr_include"
  )

let pstr_attribute (T f0') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Structure_item_desc.to_concrete_opt x' with
        | Some (Pstr_attribute (x0')) ->
          begin
            c'.matched <- c'.matched + 1;
            f0' c' l' (Attribute.to_concrete x0') (k')
          end
        | _ -> fail l' "Pstr_attribute"
      end
    | _ -> fail l' "Pstr_attribute"
  )

let pstr_extension (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_desc = x'; _ } ->
      c'.matched <- c'.matched + 1;
      begin
        match Structure_item_desc.to_concrete_opt x' with
        | Some (Pstr_extension (x0', x1')) ->
          begin
            c'.matched <- c'.matched + 1;
            f1' c' l' (Attributes.to_concrete x1') (f0' c' l' (Extension.to_concrete x0') (k'))
          end
        | _ -> fail l' "Pstr_extension"
      end
    | _ -> fail l' "Pstr_extension"
  )

let pstr_loc (T f') =
  T (fun c' l' x' k' ->
    match Structure_item.to_concrete_opt x' with
    | Some { pstr_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pstr_loc"
  )

let pvb_attributes (T f') =
  T (fun c' l' x' k' ->
    match Value_binding.to_concrete_opt x' with
    | Some { pvb_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pvb_attributes"
  )

let pvb_loc (T f') =
  T (fun c' l' x' k' ->
    match Value_binding.to_concrete_opt x' with
    | Some { pvb_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pvb_loc"
  )

let pmb_attributes (T f') =
  T (fun c' l' x' k' ->
    match Module_binding.to_concrete_opt x' with
    | Some { pmb_attributes = x'; _ } -> f' c' l' (Attributes.to_concrete x') k'
    | _ -> fail l' "pmb_attributes"
  )

let pmb_loc (T f') =
  T (fun c' l' x' k' ->
    match Module_binding.to_concrete_opt x' with
    | Some { pmb_loc = x'; _ } -> f' c' l' x' k'
    | _ -> fail l' "pmb_loc"
  )

let ptop_def (T f0') =
  T (fun c' l' x' k' ->
    match Toplevel_phrase.to_concrete_opt x' with
    | Some (Ptop_def (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' (Structure.to_concrete x0') (k')
      end
    | _ -> fail l' "Ptop_def"
  )

let ptop_dir (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Toplevel_phrase.to_concrete_opt x' with
    | Some (Ptop_dir (x0', x1')) ->
      begin
        c'.matched <- c'.matched + 1;
        f1' c' l' x1' (f0' c' l' x0' (k'))
      end
    | _ -> fail l' "Ptop_dir"
  )

let pdir_string (T f0') =
  T (fun c' l' x' k' ->
    match Directive_argument.to_concrete_opt x' with
    | Some (Pdir_string (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' x0' (k')
      end
    | _ -> fail l' "Pdir_string"
  )

let pdir_int (T f0') (T f1') =
  T (fun c' l' x' k' ->
    match Directive_argument.to_concrete_opt x' with
    | Some (Pdir_int (x0', x1')) ->
      begin
        c'.matched <- c'.matched + 1;
        f1' c' l' x1' (f0' c' l' x0' (k'))
      end
    | _ -> fail l' "Pdir_int"
  )

let pdir_ident (T f0') =
  T (fun c' l' x' k' ->
    match Directive_argument.to_concrete_opt x' with
    | Some (Pdir_ident (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' x0' (k')
      end
    | _ -> fail l' "Pdir_ident"
  )

let pdir_bool (T f0') =
  T (fun c' l' x' k' ->
    match Directive_argument.to_concrete_opt x' with
    | Some (Pdir_bool (x0')) ->
      begin
        c'.matched <- c'.matched + 1;
        f0' c' l' x0' (k')
      end
    | _ -> fail l' "Pdir_bool"
  )
(*$*)
