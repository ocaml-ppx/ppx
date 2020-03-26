(** Compile-time representation of ASTs; i.e., their types.

    Lists in these types are always non-empty Tuples and records always have at least one
    field; AST variants always have at least one clause; polymorphic types always have at
    least one argument. *)

(** Represents a structural type in the AST's grammar.

    [Var "a"] represnts the type variable ['a].

    [Name "expression"] represents the type [expression].

    [Instance ("class_infos", [Name "class_expr"])] represents the type
    [class_expr class_infos]. *)
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

(** Represents named fields of a record. *)
type record = (string * ty) list

(** A clause may have no arguments, or an inline tuple (potentially a singleton) as
    arguments, or an inline record as arguments.

    For example, this variant:

    {[
      [ ("A", Empty)
      ; ("B", Tuple [ Char; Bool ])
      ; ("C", Record [ ("x", Int); ("y", Int) ])
      ]
    ]}

    represents the following type:

    {[
      type t =
        | A
        | B of char * bool
        | C of { x : int; y : int }
    ]} *)
type clause =
  | Empty
  | Tuple of tuple
  | Record of record

type variant = (string * clause) list

(** A nominal type may contain a structural type, a record type, or a variant type. *)
type decl =
  | Wrapper of ty
  | Record of record
  | Variant of variant

(** The "kind" of a type determines its type arguments. [Mono]morphic types have no type
    arguments; [Poly]morphic types have one or more type arguments. *)
type kind =
  | Mono of decl
  | Poly of string list * decl

(** A grammar is a list of named types. *)
type t = (string * kind) list
