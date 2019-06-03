let lazy_generator lazy_t =
  Base_quickcheck.Generator.create (fun ~size ~random ->
    Base_quickcheck.Generator.generate (Lazy.force lazy_t) ~size ~random)

(*$ Astlib_cinaps.print_generators_ml () *)
open Base
open Ocaml_common
open Base_quickcheck.Generator.Let_syntax

let loc = Location.none

module V1 = struct
  let rec expression_generator = lazy (
    let ident_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      in
      let%bind name =
        Base_quickcheck.Generator.string_non_empty
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      in
      return (Astlib.V1.Expression.Concrete.Ident { loc; name })
    in
    let match_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      in
      let%bind arg =
        (lazy_generator expression_generator)
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      and cases =
        (Base_quickcheck.Generator.list (lazy_generator case_generator))
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 1)
      in
      return (Astlib.V1.Expression.Concrete.Match { loc; arg; cases })
    in
    Base_quickcheck.Generator.union
      [ ident_gen
      ; match_gen
      ]
      |> Base_quickcheck.Generator.map ~f:Astlib.V1.Expression.of_concrete)

  and case_generator = lazy (
    let case_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:3 ~max_length:3 ()
      in
      let%bind lhs =
        (lazy_generator pattern_generator)
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      and guard =
        (Base_quickcheck.Generator.option (lazy_generator expression_generator))
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 1)
      and rhs =
        (lazy_generator expression_generator)
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 2)
      in
      return (Astlib.V1.Case.Concrete.Case { loc; lhs; guard; rhs })
    in
    Base_quickcheck.Generator.union
      [ case_gen
      ]
      |> Base_quickcheck.Generator.map ~f:Astlib.V1.Case.of_concrete)

  and pattern_generator = lazy (
    let ident_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      in
      let%bind name =
        Base_quickcheck.Generator.string_non_empty
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      in
      return (Astlib.V1.Pattern.Concrete.Ident { loc; name })
    in
    Base_quickcheck.Generator.union
      [ ident_gen
      ]
      |> Base_quickcheck.Generator.map ~f:Astlib.V1.Pattern.of_concrete)

  module Expression = struct
    type t = Astlib.V1.Expression.t
    [@@deriving sexp_of]

    let quickcheck_generator = Lazy.force expression_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Case = struct
    type t = Astlib.V1.Case.t
    [@@deriving sexp_of]

    let quickcheck_generator = Lazy.force case_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Pattern = struct
    type t = Astlib.V1.Pattern.t
    [@@deriving sexp_of]

    let quickcheck_generator = Lazy.force pattern_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end
end

module V2 = struct
  let rec expression_generator = lazy (
    let ident_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      in
      let%bind name =
        Base_quickcheck.Generator.string_non_empty
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      in
      return (Astlib.V2.Expression.Concrete.Ident { loc; name })
    in
    let match_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      in
      let%bind arg =
        (lazy_generator expression_generator)
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      and cases =
        (Base_quickcheck.Generator.list (lazy_generator case_generator))
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 1)
      in
      return (Astlib.V2.Expression.Concrete.Match { loc; arg; cases })
    in
    Base_quickcheck.Generator.union
      [ ident_gen
      ; match_gen
      ]
      |> Base_quickcheck.Generator.map ~f:Astlib.V2.Expression.of_concrete)

  and case_generator = lazy (
    let case_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      in
      let%bind lhs =
        (lazy_generator pattern_generator)
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      and rhs =
        (lazy_generator expression_generator)
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 1)
      in
      return (Astlib.V2.Case.Concrete.Case { loc; lhs; rhs })
    in
    Base_quickcheck.Generator.union
      [ case_gen
      ]
      |> Base_quickcheck.Generator.map ~f:Astlib.V2.Case.of_concrete)

  and pattern_generator = lazy (
    let ident_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      in
      let%bind name =
        Base_quickcheck.Generator.string_non_empty
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      in
      return (Astlib.V2.Pattern.Concrete.Ident { loc; name })
    in
    let guard_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      in
      let%bind body =
        (lazy_generator pattern_generator)
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      and guard =
        (lazy_generator expression_generator)
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 1)
      in
      return (Astlib.V2.Pattern.Concrete.Guard { loc; body; guard })
    in
    Base_quickcheck.Generator.union
      [ ident_gen
      ; guard_gen
      ]
      |> Base_quickcheck.Generator.map ~f:Astlib.V2.Pattern.of_concrete)

  module Expression = struct
    type t = Astlib.V2.Expression.t
    [@@deriving sexp_of]

    let quickcheck_generator = Lazy.force expression_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Case = struct
    type t = Astlib.V2.Case.t
    [@@deriving sexp_of]

    let quickcheck_generator = Lazy.force case_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Pattern = struct
    type t = Astlib.V2.Pattern.t
    [@@deriving sexp_of]

    let quickcheck_generator = Lazy.force pattern_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end
end

module V3 = struct
  let rec identifier_generator = lazy (
    let var_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      in
      let%bind name =
        Base_quickcheck.Generator.string_non_empty
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      in
      return (Astlib.V3.Identifier.Concrete.Var { loc; name })
    in
    let dot_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      in
      let%bind base =
        (lazy_generator identifier_generator)
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      and label =
        Base_quickcheck.Generator.string_non_empty
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 1)
      in
      return (Astlib.V3.Identifier.Concrete.Dot { loc; base; label })
    in
    Base_quickcheck.Generator.union
      [ var_gen
      ; dot_gen
      ]
      |> Base_quickcheck.Generator.map ~f:Astlib.V3.Identifier.of_concrete)

  and expression_generator = lazy (
    let ident_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      in
      let%bind id =
        (lazy_generator identifier_generator)
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      in
      return (Astlib.V3.Expression.Concrete.Ident { loc; id })
    in
    let match_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      in
      let%bind arg =
        (lazy_generator expression_generator)
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      and cases =
        (Base_quickcheck.Generator.list (lazy_generator case_generator))
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 1)
      in
      return (Astlib.V3.Expression.Concrete.Match { loc; arg; cases })
    in
    Base_quickcheck.Generator.union
      [ ident_gen
      ; match_gen
      ]
      |> Base_quickcheck.Generator.map ~f:Astlib.V3.Expression.of_concrete)

  and case_generator = lazy (
    let case_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      in
      let%bind lhs =
        (lazy_generator pattern_generator)
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      and rhs =
        (lazy_generator expression_generator)
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 1)
      in
      return (Astlib.V3.Case.Concrete.Case { loc; lhs; rhs })
    in
    Base_quickcheck.Generator.union
      [ case_gen
      ]
      |> Base_quickcheck.Generator.map ~f:Astlib.V3.Case.of_concrete)

  and pattern_generator = lazy (
    let ident_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:1 ~max_length:1 ()
      in
      let%bind name =
        Base_quickcheck.Generator.string_non_empty
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      in
      return (Astlib.V3.Pattern.Concrete.Ident { loc; name })
    in
    let guard_gen =
      let%bind sizes =
        Base_quickcheck.Generator.sizes ~min_length:2 ~max_length:2 ()
      in
      let%bind body =
        (lazy_generator pattern_generator)
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 0)
      and guard =
        (lazy_generator expression_generator)
        |> Base_quickcheck.Generator.with_size
          ~size:(List.nth_exn sizes 1)
      in
      return (Astlib.V3.Pattern.Concrete.Guard { loc; body; guard })
    in
    Base_quickcheck.Generator.union
      [ ident_gen
      ; guard_gen
      ]
      |> Base_quickcheck.Generator.map ~f:Astlib.V3.Pattern.of_concrete)

  module Identifier = struct
    type t = Astlib.V3.Identifier.t
    [@@deriving sexp_of]

    let quickcheck_generator = Lazy.force identifier_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Expression = struct
    type t = Astlib.V3.Expression.t
    [@@deriving sexp_of]

    let quickcheck_generator = Lazy.force expression_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Case = struct
    type t = Astlib.V3.Case.t
    [@@deriving sexp_of]

    let quickcheck_generator = Lazy.force case_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end

  module Pattern = struct
    type t = Astlib.V3.Pattern.t
    [@@deriving sexp_of]

    let quickcheck_generator = Lazy.force pattern_generator
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
  end
end
(*$*)
