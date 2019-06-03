(*$ Astlib_cinaps.print_astlib_mli () *)
open Base
open Ocaml_common

module V1 : sig
  module rec Expression : sig
    type t

    val sexp_of_t : t -> Sexp.t
    val equal : t -> t -> bool
    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Ident of { loc : Location.t; name : string }
        | Match of { loc : Location.t; arg : Expression.t; cases : Case.t list }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
  end

  and Case : sig
    type t

    val sexp_of_t : t -> Sexp.t
    val equal : t -> t -> bool
    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Case of { loc : Location.t; lhs : Pattern.t; guard : Expression.t option; rhs : Expression.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
  end

  and Pattern : sig
    type t

    val sexp_of_t : t -> Sexp.t
    val equal : t -> t -> bool
    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Ident of { loc : Location.t; name : string }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
  end
end

module V2 : sig
  module rec Expression : sig
    type t

    val sexp_of_t : t -> Sexp.t
    val equal : t -> t -> bool
    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Ident of { loc : Location.t; name : string }
        | Match of { loc : Location.t; arg : Expression.t; cases : Case.t list }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
  end

  and Case : sig
    type t

    val sexp_of_t : t -> Sexp.t
    val equal : t -> t -> bool
    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Case of { loc : Location.t; lhs : Pattern.t; rhs : Expression.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
  end

  and Pattern : sig
    type t

    val sexp_of_t : t -> Sexp.t
    val equal : t -> t -> bool
    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Ident of { loc : Location.t; name : string }
        | Guard of { loc : Location.t; body : Pattern.t; guard : Expression.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
  end
end

module V3 : sig
  module rec Identifier : sig
    type t

    val sexp_of_t : t -> Sexp.t
    val equal : t -> t -> bool
    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Var of { loc : Location.t; name : string }
        | Dot of { loc : Location.t; base : Identifier.t; label : string }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
  end

  and Expression : sig
    type t

    val sexp_of_t : t -> Sexp.t
    val equal : t -> t -> bool
    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Ident of { loc : Location.t; id : Identifier.t }
        | Match of { loc : Location.t; arg : Expression.t; cases : Case.t list }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
  end

  and Case : sig
    type t

    val sexp_of_t : t -> Sexp.t
    val equal : t -> t -> bool
    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Case of { loc : Location.t; lhs : Pattern.t; rhs : Expression.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
  end

  and Pattern : sig
    type t

    val sexp_of_t : t -> Sexp.t
    val equal : t -> t -> bool
    val of_ast : Versioned_ast.t -> t
    val to_ast : t -> Versioned_ast.t

    module Concrete : sig
      type t =
        | Ident of { loc : Location.t; name : string }
        | Guard of { loc : Location.t; body : Pattern.t; guard : Expression.t }
    end

    val of_concrete : Concrete.t -> t
    val to_concrete : t -> Concrete.t option
  end
end
(*$*)
