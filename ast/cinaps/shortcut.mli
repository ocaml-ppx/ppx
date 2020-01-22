(** Type for shortcuts from description types to their parent record types:
    - [record] is the parent type name
    - [variant] is the description type name
    - [desc_field] is the name of the field in the parent pointing to the
    description
    - [loc_field] is the name of the field in the parent pointing to the
    location, if any
    - [attr_field] is the name of the field in the parent pointing to the
    attributes, if any
    - [other_fields] is the list of the remaining fields of the parent *)
type t =
  { outter_record : string
  ; inner_variant : string
  ; desc_field : string
  ; attr_field : string option
  ; loc_field : string option
  ; other_fields : (string * Astlib.Grammar.ty) list
  }

type shortcut = t

module Map : sig
  (** Maps from type names to shortcuts *)

  type t

  (** Returns the shortcuts for the given grammar. *)
  val from_grammar : Astlib.Grammar.t -> t

  (** Returns the shortcut for the given type, if any *)
  val find : t -> string -> shortcut option
end
