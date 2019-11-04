(** Stable interface for Astlib positions, which are used as the start and end of AST
    locations.

    Astlib positions use the same representation as positions in the compiler; however, we
    allow for the possibility that new fields will be added to the type over time. We
    expose the type as [private] to discourage explicit construction, and provide a
    [create] function that can be extended with optional arguments in a
    backward-compatible way. *)

(** {1 Type} *)

(** The type equivalence with [Lexing.position] is expected to stay stable. *)
type t = private Lexing.position

(** {1 Conversions} It should always be possible to convert to/from [Lexing.position]. *)

val of_position : Lexing.position -> t
val to_position : t -> Lexing.position

(** {1 Accessors} These should be stable. *)

val fname : t -> string
val lnum : t -> int
val bol : t -> int
val cnum : t -> int

(** {1 Constructors} These may have additional optional arguments added over time. Where
    necessary, we add an unnamed [unit] argument so that optional arguments can be
    dropped, even if there currently are none. *)

val create
  :  fname : string
  -> lnum : int
  -> bol : int
  -> cnum : int
  -> unit
  -> t

val update
  :  ?fname : string
  -> ?lnum : int
  -> ?bol : int
  -> ?cnum : int
  -> t
  -> t
