open! Base

type 'node data = 'node Astlib.Ast.data =
  | Node of 'node
  | Bool of bool
  | Char of char
  | Int of int
  | String of string
  | Location of Location.t
  | Loc of 'node data Loc.t
  | List of 'node data list
  | Option of 'node data option
  | Tuple of 'node data array
  | Record of 'node data array
  | Variant of { tag : string; args : 'node data array }
[@@deriving compare, equal, sexp_of]

type 'node t = 'node Astlib.Ast.t = { name : string; data : 'node data }
[@@deriving compare, equal, sexp_of]

val map
  :  'a t
  -> f:('a -> 'b)
  -> 'b t

val matches
  :  'node t
  -> grammar:Grammar.t
  -> unwrap:('node -> 'node t)
  -> bool

val generator
  :  Grammar.t
  -> wrap:('node t -> 'node)
  -> 'node t Base_quickcheck.Generator.t

module Optional : sig
  val map
    :  'a t
    -> f:('a -> 'b option)
    -> 'b t option
end
