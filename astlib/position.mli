(** Stable interface for Astlib positions, which are used as the start and end of AST
    locations. *)

(** {1 Type} *)

type t = Lexing.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
