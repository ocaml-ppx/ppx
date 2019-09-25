val safe_ident
   : string
  -> string
(** [safe_ident str] returns [str] if it is not equal to an OCaml keyword,
    [str ^ "_"] otherwise. *)

val safe_uncapitalize
   : string
  -> string
(** [safe_uncapitalize x] is a shorthand for
    [safe_ident (String.uncapitalize_ascii x)]. *)

val starts_with
   : prefix:string
  -> string
  -> bool
(** [starts_with ~prefix str] returns [true] iff [str] starts with [prefix]. *)

val ends_with
   : suffix:string
  -> string
  -> bool
(** [ends_with ~suffix str] returns [true] iif [str] ends with [suffix]. *)
