(** Runtime representation of ASTs. *)

(** Represents data stored in each AST node.

    Fields of tuples, records, and variants are stored in arrays rather than lists to
    reduce runtime cost of accessing them.

    Variant arguments are stored the same whether the arguments are part of an inline
    record or an inline tuple.

    Record and variant arguments are stored without names, so that the runtime cost of
    named fields is no higher than the cost of unnamed fields (e.g., tuples). *)
type 'node data =
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

(** Represents an AST node. The [name] field specifies the name of the type in the AST's
    grammar, such as "expression" or "pattern". *)
type 'node t = { name : string; data : 'node data }
