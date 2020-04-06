open! Import

let get_default_path (loc : Location.t) =
  let fname = loc.loc_start.pos_fname in
  match String.drop_prefix ~prefix:"./" fname with
  | Some fname -> fname
  | None       -> fname
;;

let get_default_path_str : structure -> string = function%view
  | Structure [] -> ""
  | Structure ({ pstr_loc = loc; _ } :: _) -> get_default_path loc
;;

let get_default_path_sig : signature -> string = function%view
  | Signature [] -> ""
  | Signature ({ psig_loc = loc; _ } :: _) -> get_default_path loc
;;
