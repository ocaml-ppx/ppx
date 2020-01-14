(** Helpers to interpret [[@view ...]] attributes *)

open Ppx_ast.V4_07

(** The type for information about an extra record field. *)
type field =
  { label : string
  ; label_loc : Astlib.Location.t
  ; var : string
  ; var_loc : Astlib.Location.t
  }

(** Extracts the extra field information from the [[@view ...]] attributes
    amongst the given pattern attributes. Returns [None] if there is no
    such attribute. The given location is only used to report errors. *)
val extract_fields : err_loc: Astlib.Location.t -> Attributes.t -> field list option
