open! Import

module type Additional_helpers = sig
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
    loc:Location.t -> Asttypes.rec_flag -> value_binding list -> structure_item list
  (** [pstr_value_list ~loc rf vbs] = [pstr_value ~loc rf vbs] if [vbs <> []], [[]]
      otherwise. *)

  val nonrec_type_declaration :
    loc:Location.t
    -> name:string Loc.t
     -> params:(core_type * Asttypes.variance) list
     -> cstrs:(core_type * core_type * Location.t) list
     -> kind:type_kind
     -> private_:Asttypes.private_flag
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
    -> Longident.t Loc.t -> f:(string -> string) -> expression
  val type_constr_conv :
    loc:Location.t
    -> Longident.t Loc.t -> f:(string -> string) -> expression list -> expression

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
end

module type Located = sig
  type 'a t = 'a Loc.t

  val loc : _ t -> Location.t

  val mk : loc:Location.t -> 'a -> 'a t

  val map        : ('a -> 'b) -> 'a t -> 'b t
  val map_lident : string t -> Longident.t t

  val lident : loc:Location.t -> string -> Longident.t t
end
