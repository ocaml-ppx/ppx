(*$ Astlib_cinaps.print_completeness_ml () *)
open Base

module V1 = struct
  let rec check_expression_exn x =
    match Astlib.V1.Expression.to_concrete x with
    | Some (Ident { loc = _; name }) ->
      ignore name
    | Some (Match { loc = _; arg; cases }) ->
      check_expression_exn arg;
      (List.iter ~f:check_case_exn) cases
    | None ->
      Base.raise_s (Sexp.message "invalid V1 Expression" ["ast", (Astlib.V1.Expression.sexp_of_t x)])

  and check_case_exn x =
    match Astlib.V1.Case.to_concrete x with
    | Some (Case { loc = _; lhs; guard; rhs }) ->
      check_pattern_exn lhs;
      (Option.iter ~f:check_expression_exn) guard;
      check_expression_exn rhs
    | None ->
      Base.raise_s (Sexp.message "invalid V1 Case" ["ast", (Astlib.V1.Case.sexp_of_t x)])

  and check_pattern_exn x =
    match Astlib.V1.Pattern.to_concrete x with
    | Some (Ident { loc = _; name }) ->
      ignore name
    | None ->
      Base.raise_s (Sexp.message "invalid V1 Pattern" ["ast", (Astlib.V1.Pattern.sexp_of_t x)])

  module Expression = struct
    let check_exn = check_expression_exn
  end

  module Case = struct
    let check_exn = check_case_exn
  end

  module Pattern = struct
    let check_exn = check_pattern_exn
  end
end

module V2 = struct
  let rec check_expression_exn x =
    match Astlib.V2.Expression.to_concrete x with
    | Some (Ident { loc = _; name }) ->
      ignore name
    | Some (Match { loc = _; arg; cases }) ->
      check_expression_exn arg;
      (List.iter ~f:check_case_exn) cases
    | None ->
      Base.raise_s (Sexp.message "invalid V2 Expression" ["ast", (Astlib.V2.Expression.sexp_of_t x)])

  and check_case_exn x =
    match Astlib.V2.Case.to_concrete x with
    | Some (Case { loc = _; lhs; rhs }) ->
      check_pattern_exn lhs;
      check_expression_exn rhs
    | None ->
      Base.raise_s (Sexp.message "invalid V2 Case" ["ast", (Astlib.V2.Case.sexp_of_t x)])

  and check_pattern_exn x =
    match Astlib.V2.Pattern.to_concrete x with
    | Some (Ident { loc = _; name }) ->
      ignore name
    | Some (Guard { loc = _; body; guard }) ->
      check_pattern_exn body;
      check_expression_exn guard
    | None ->
      Base.raise_s (Sexp.message "invalid V2 Pattern" ["ast", (Astlib.V2.Pattern.sexp_of_t x)])

  module Expression = struct
    let check_exn = check_expression_exn
  end

  module Case = struct
    let check_exn = check_case_exn
  end

  module Pattern = struct
    let check_exn = check_pattern_exn
  end
end

module V3 = struct
  let rec check_identifier_exn x =
    match Astlib.V3.Identifier.to_concrete x with
    | Some (Var { loc = _; name }) ->
      ignore name
    | Some (Dot { loc = _; base; label }) ->
      check_identifier_exn base;
      ignore label
    | None ->
      Base.raise_s (Sexp.message "invalid V3 Identifier" ["ast", (Astlib.V3.Identifier.sexp_of_t x)])

  and check_expression_exn x =
    match Astlib.V3.Expression.to_concrete x with
    | Some (Ident { loc = _; id }) ->
      check_identifier_exn id
    | Some (Match { loc = _; arg; cases }) ->
      check_expression_exn arg;
      (List.iter ~f:check_case_exn) cases
    | None ->
      Base.raise_s (Sexp.message "invalid V3 Expression" ["ast", (Astlib.V3.Expression.sexp_of_t x)])

  and check_case_exn x =
    match Astlib.V3.Case.to_concrete x with
    | Some (Case { loc = _; lhs; rhs }) ->
      check_pattern_exn lhs;
      check_expression_exn rhs
    | None ->
      Base.raise_s (Sexp.message "invalid V3 Case" ["ast", (Astlib.V3.Case.sexp_of_t x)])

  and check_pattern_exn x =
    match Astlib.V3.Pattern.to_concrete x with
    | Some (Ident { loc = _; name }) ->
      ignore name
    | Some (Guard { loc = _; body; guard }) ->
      check_pattern_exn body;
      check_expression_exn guard
    | None ->
      Base.raise_s (Sexp.message "invalid V3 Pattern" ["ast", (Astlib.V3.Pattern.sexp_of_t x)])

  module Identifier = struct
    let check_exn = check_identifier_exn
  end

  module Expression = struct
    let check_exn = check_expression_exn
  end

  module Case = struct
    let check_exn = check_case_exn
  end

  module Pattern = struct
    let check_exn = check_pattern_exn
  end
end
(*$*)
