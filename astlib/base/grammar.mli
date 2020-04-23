open! Base

type tuple = Astlib.Grammar.tuple
[@@deriving compare, equal, hash, sexp_of]

type record = Astlib.Grammar.record
[@@deriving compare, equal, hash, sexp_of]

type variant = Astlib.Grammar.variant
[@@deriving compare, equal, hash, sexp_of]

type targ = Astlib.Grammar.targ =
  | Tname of string
  | Tvar of string
[@@deriving compare, equal, hash, sexp_of]

type ty = Astlib.Grammar.ty =
  | Var of string
  | Name of string
  | Bool
  | Char
  | Int
  | String
  | Location
  | Loc of ty
  | List of ty
  | Option of ty
  | Tuple of tuple
  | Instance of string * targ list
[@@deriving compare, equal, hash, sexp_of]

type clause = Astlib.Grammar.clause =
  | Empty
  | Tuple of tuple
  | Record of record
[@@deriving compare, equal, hash, sexp_of]

type decl = Astlib.Grammar.decl =
  | Ty of ty
  | Record of record
  | Variant of variant
[@@deriving compare, equal, hash, sexp_of]

type kind = Astlib.Grammar.kind =
  | Mono of decl
  | Poly of string list * decl
[@@deriving compare, equal, hash, sexp_of]

type t = Astlib.Grammar.t
[@@deriving compare, equal, hash, sexp_of]

val count_recursions_in_clause : clause -> int

val lookup_mono : t -> name:string -> decl option

val lookup_instance : t -> name:string -> args:targ list -> decl option
