(** Helpers for build OCaml AST fragments *)

(* TODO: remove in favor of Ppx_ast's versioned builders *)

open! Import

(** This module is similar to the [Ast_helper] module distrubuted with OCaml but uses
    different conventions.

    {3 Locations}

    [Ast_helper] uses a global variable for the default locations, we found that to it
    makes it quite easy to mess up locations. Instead this modules forces you to provide a
    location argument.

    For building fragment using the same location everywhere, a functor is provided.

    {3 Naming}

    The names match the [Parsetree] names closely, which makes it easy to build AST
    fragments by just knowing the [Parsetree].

    For types of the form a wrapper record with a [_desc] field, helpers are generated for
    each constructor constructing the record directly. For instance for the type
    [Parsetree.expression]:

    {[
      type expression =
        { pexp_desc       : expression_desc
        ; pexp_loc        : Location.t
        ; pexp_attributes : attributes
        }

      and expression_desc =
        | Pexp_ident    of Longident.t loc
        | Pexp_constant of constant
        | Pexp_let      of rec_flag * value_binding list * expression
        ...
    ]}

    The following helpers are created:

    {[
      val pexp_ident    : loc:Location.t -> Longident.t Located.t          -> expression
      val pexp_constant : loc:Location.t -> constant                       -> expression
      val pexp_let      : loc:Location.t -> rec_flag -> value_binding list -> expression
      ...
    ]}

    For other record types, such as type_declaration, we have the following helper:

    {[
      type type_declaration =
        { ptype_name       : string Located.t
        ; ptype_params     : (core_type * variance) list
        ; ptype_cstrs      : (core_type * core_type * Location.t) list
        ; ptype_kind       : type_kind
        ; ptype_private    : private_flag
        ; ptype_manifest   : core_type option
        ; ptype_attributes : attributes
        ; ptype_loc        : Location.t
        }


      val type_declaration
        :  loc      : Location.t
        -> name     : string Located.t
        -> params   : (core_type * variance) list
        -> cstrs    : (core_type * core_type * Location.t) list
        -> kind     : type_kind
        -> private  : private_flag
        -> manifest : core_type option
        -> type_declaration
    ]}

    Attributes are always set to the empty list. If you want to set them you have to
    override the field with the [{ e with pexp_attributes = ... }] notation.
 *)

val eint       : loc:Location.t -> int       -> expression
val echar      : loc:Location.t -> char      -> expression
val estring    : loc:Location.t -> string    -> expression
val efloat     : loc:Location.t -> string    -> expression
val eint32     : loc:Location.t -> int32     -> expression
val eint64     : loc:Location.t -> int64     -> expression
val enativeint : loc:Location.t -> nativeint -> expression
val ebool      : loc:Location.t -> bool      -> expression

val pint       : loc:Location.t -> int       -> pattern
val pchar      : loc:Location.t -> char      -> pattern
val pstring    : loc:Location.t -> string    -> pattern
val pfloat     : loc:Location.t -> string    -> pattern
val pint32     : loc:Location.t -> int32     -> pattern
val pint64     : loc:Location.t -> int64     -> pattern
val pnativeint : loc:Location.t -> nativeint -> pattern
val pbool      : loc:Location.t -> bool      -> pattern

val eunit : loc:Location.t -> expression
val punit : loc:Location.t -> pattern

(** [evar id] produces a [Pexp_ident _] expression, it parses its input so you can pass
      any dot-separated identifier, for instance: [evar ~loc "Foo.bar"]. *)
val evar : loc:Location.t -> string -> expression
val pvar : loc:Location.t -> string -> pattern

(** Same as pexp_apply but without labels *)
val eapply : loc:Location.t -> expression -> expression list -> expression

val eabstract : loc:Location.t -> pattern list -> expression -> expression

val esequence : loc:Location.t -> expression list -> expression

val ppat_tuple_opt : loc:Location.t -> pattern list -> pattern option
val pexp_tuple_opt : loc:Location.t -> expression list -> expression option

val pconstruct : constructor_declaration -> pattern    option -> pattern
val econstruct : constructor_declaration -> expression option -> expression

val elist : loc:Location.t -> expression list -> expression
val plist : loc:Location.t -> pattern    list -> pattern

val pstr_value_list :
  loc:Location.t -> rec_flag -> value_binding list -> structure_item list
(** [pstr_value_list ~loc rf vbs] = [pstr_value ~loc rf vbs] if [vbs <> []], [[]]
      otherwise. *)

val nonrec_type_declaration :
  loc:Location.t
  -> name:string Loc.t
  -> params:(core_type * variance) list
  -> cstrs:(core_type * core_type * Location.t) list
  -> kind:type_kind
  -> private_:private_flag
  -> manifest:core_type option
  -> type_declaration
       [@@deprecated
            "[since 2016-10] use Nonrecursive on the P(str|sig)_type instead"]

(** [unapplied_type_constr_conv] is the standard way to map identifiers to conversion
      fonctions, for preprocessor that creates values that follow the structure of types.
      More precisely, [path_conv path (sprintf "sexp_of_%s")] is:
      - sexp_of_t if path is "t"
      - A.B.sexp_of_foo if path is "A.B.foo"
      - A.B.sexp_of_f__foo (module A1) (module A2) if path is "A.B.F(A1)(A2).foo"
      [type_constr_conv] also applies it to a list of expression, which both prevents
      the compiler from allocating useless closures, and almost always what is needed,
      since type constructors are always applied. *)
val unapplied_type_constr_conv :
  loc:Location.t
  -> longident Loc.t -> f:(string -> string) -> expression
val type_constr_conv :
  loc:Location.t
  -> longident Loc.t -> f:(string -> string) -> expression list -> expression

val include_infos : loc:Location.t -> 'a node -> 'a node include_infos

(** Tries to simplify [fun v1 v2 .. -> f v1 v2 ..] into [f]. Only works when [f] is a
      path, not an arbitrary expression as that would change the meaning of
      the code.
      This can be used either for cleaning up the generated code, or to reduce allocation
      if [f] is a local variable (the compiler won't optimize the allocation of the
      closure).

      Eta-reduction can change the types/behavior in some corner cases that are unlikely
      to show up in generated code:
      - if [f] has optional arguments, eta-expanding [f] can drop them
      - because labels commute, it can change the type of an expression:
        $ let f ~x y = x + y
          let f2 = fun x -> add x;;
        val f  : x:int -> int -> int = <fun>
        val f2 : int -> x:int -> int = <fun>
        In fact, if [f] does side effects before receiving all its arguments, and if
        the eta-expansion is partially applied, eta-reducing could change behavior.

      [eta_reduce_if_possible_and_nonrec] is meant for the case where the resulting
      expression is going to be bound in a potentially recursive let-binding, where
      we have to keep the eta-expansion when [rec_flag] is [Recursive] to avoid
      a compile error. *)
val eta_reduce : expression -> expression option
val eta_reduce_if_possible : expression -> expression
val eta_reduce_if_possible_and_nonrec : expression -> rec_flag:rec_flag -> expression

module Located : sig
  val loc : _ Loc.t -> Location.t

  val mk : loc:Location.t -> 'a -> 'a Loc.t

  val map        : 'a Loc.t -> f:('a -> 'b) -> 'b Loc.t
  val map_lident : string Loc.t -> longident_loc

  val lident : loc:Location.t -> string -> longident_loc
end
