(*$ Astlib_cinaps.print_generators_mli () *)
module V1 : sig
  module Expression : sig
    type t = Astlib.V1.Expression.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Case : sig
    type t = Astlib.V1.Case.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Pattern : sig
    type t = Astlib.V1.Pattern.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end
end

module V2 : sig
  module Expression : sig
    type t = Astlib.V2.Expression.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Case : sig
    type t = Astlib.V2.Case.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Pattern : sig
    type t = Astlib.V2.Pattern.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end
end

module V3 : sig
  module Identifier : sig
    type t = Astlib.V3.Identifier.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Expression : sig
    type t = Astlib.V3.Expression.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Case : sig
    type t = Astlib.V3.Case.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end

  module Pattern : sig
    type t = Astlib.V3.Pattern.t
    [@@deriving sexp_of]

    val quickcheck_generator : t Base_quickcheck.Generator.t
    val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
  end
end
(*$*)
