(** Helpers to interpret [[@view? ...]] attributes *)

open Ppx_ast.V4_07

(** The type for information about an extra record field. *)
type field = Longident_loc.t * Pattern.t

(** Extracts the extra field information from the [[@view? ...]] attributes
    amongst the given pattern attributes. Returns [None] if there is no
    such attribute. *)
val extract_fields :
  Attributes.t ->
  (field list) option
