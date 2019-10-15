type ty =
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
  | Instance of string * ty list

and tuple = ty list

type record = (string * ty) list

type clause =
  | Empty
  | Tuple of tuple
  | Record of record

type variant = (string * clause) list

type decl =
  | Alias of ty
  | Record of record
  | Variant of variant

type kind =
  | Mono of decl
  | Poly of string list * decl

type t = (string * kind) list
