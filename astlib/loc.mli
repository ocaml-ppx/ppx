(** Stable interface for arbitrary values annotated with Astlib locations. *)

(** {1 Type} *)

type 'a t = 'a Ocaml_common.Location.loc =
  { txt : 'a
  ; loc : Location.t
  }

val map : 'a t -> f:('a -> 'b) -> 'b t
