(** Runtime representation of ASTs. *)

(** Represents data stored in each AST node.

    Fields of tuples, records, and variants are stored in arrays rather than lists to
    reduce runtime cost of accessing them.

    Variant arguments are stored the same whether the arguments are part of an inline
    record or an inline tuple.

    Record and variant arguments are stored without names, so that the runtime cost of
    named fields is no higher than the cost of unnamed fields (e.g., tuples). *)
type 'a data =
  | Node of 'a
  | Bool of bool
  | Char of char
  | Int of int
  | String of string
  | Location of Location.t
  | Loc of 'a data Loc.t
  | List of 'a data list
  | Option of 'a data option
  | Tuple of 'a data array
  | Record of 'a data array
  | Variant of { tag : string; args : 'a data array }

(** Represents an AST node. The [name] field specifies the name of the type in the AST's
    grammar, such as "expression" or "pattern". *)
type 'a node = { name : string; data : 'a data }
