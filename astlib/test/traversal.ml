(*$ Astlib_cinaps.print_traversal_ml () *)
open Base

module V1 = struct
  let rec copy_expression x =
    match Astlib.V1.Expression.to_concrete x with
    | Some (Ident { loc; name }) ->
      let name = name in
      Astlib.V1.Expression.of_concrete (Ident { loc; name })
    | Some (Match { loc; arg; cases }) ->
      let arg = copy_expression arg in
      let cases = (List.map ~f:copy_case) cases in
      Astlib.V1.Expression.of_concrete (Match { loc; arg; cases })
    | None -> x

  and copy_case x =
    match Astlib.V1.Case.to_concrete x with
    | Some (Case { loc; lhs; guard; rhs }) ->
      let lhs = copy_pattern lhs in
      let guard = (Option.map ~f:copy_expression) guard in
      let rhs = copy_expression rhs in
      Astlib.V1.Case.of_concrete (Case { loc; lhs; guard; rhs })
    | None -> x

  and copy_pattern x =
    match Astlib.V1.Pattern.to_concrete x with
    | Some (Ident { loc; name }) ->
      let name = name in
      Astlib.V1.Pattern.of_concrete (Ident { loc; name })
    | None -> x

  module Expression = struct
    let copy = copy_expression
  end

  module Case = struct
    let copy = copy_case
  end

  module Pattern = struct
    let copy = copy_pattern
  end
end

module V2 = struct
  let rec copy_expression x =
    match Astlib.V2.Expression.to_concrete x with
    | Some (Ident { loc; name }) ->
      let name = name in
      Astlib.V2.Expression.of_concrete (Ident { loc; name })
    | Some (Match { loc; arg; cases }) ->
      let arg = copy_expression arg in
      let cases = (List.map ~f:copy_case) cases in
      Astlib.V2.Expression.of_concrete (Match { loc; arg; cases })
    | None -> x

  and copy_case x =
    match Astlib.V2.Case.to_concrete x with
    | Some (Case { loc; lhs; rhs }) ->
      let lhs = copy_pattern lhs in
      let rhs = copy_expression rhs in
      Astlib.V2.Case.of_concrete (Case { loc; lhs; rhs })
    | None -> x

  and copy_pattern x =
    match Astlib.V2.Pattern.to_concrete x with
    | Some (Ident { loc; name }) ->
      let name = name in
      Astlib.V2.Pattern.of_concrete (Ident { loc; name })
    | Some (Guard { loc; body; guard }) ->
      let body = copy_pattern body in
      let guard = copy_expression guard in
      Astlib.V2.Pattern.of_concrete (Guard { loc; body; guard })
    | None -> x

  module Expression = struct
    let copy = copy_expression
  end

  module Case = struct
    let copy = copy_case
  end

  module Pattern = struct
    let copy = copy_pattern
  end
end

module V3 = struct
  let rec copy_identifier x =
    match Astlib.V3.Identifier.to_concrete x with
    | Some (Var { loc; name }) ->
      let name = name in
      Astlib.V3.Identifier.of_concrete (Var { loc; name })
    | Some (Dot { loc; base; label }) ->
      let base = copy_identifier base in
      let label = label in
      Astlib.V3.Identifier.of_concrete (Dot { loc; base; label })
    | None -> x

  and copy_expression x =
    match Astlib.V3.Expression.to_concrete x with
    | Some (Ident { loc; id }) ->
      let id = copy_identifier id in
      Astlib.V3.Expression.of_concrete (Ident { loc; id })
    | Some (Match { loc; arg; cases }) ->
      let arg = copy_expression arg in
      let cases = (List.map ~f:copy_case) cases in
      Astlib.V3.Expression.of_concrete (Match { loc; arg; cases })
    | None -> x

  and copy_case x =
    match Astlib.V3.Case.to_concrete x with
    | Some (Case { loc; lhs; rhs }) ->
      let lhs = copy_pattern lhs in
      let rhs = copy_expression rhs in
      Astlib.V3.Case.of_concrete (Case { loc; lhs; rhs })
    | None -> x

  and copy_pattern x =
    match Astlib.V3.Pattern.to_concrete x with
    | Some (Ident { loc; name }) ->
      let name = name in
      Astlib.V3.Pattern.of_concrete (Ident { loc; name })
    | Some (Guard { loc; body; guard }) ->
      let body = copy_pattern body in
      let guard = copy_expression guard in
      Astlib.V3.Pattern.of_concrete (Guard { loc; body; guard })
    | None -> x

  module Identifier = struct
    let copy = copy_identifier
  end

  module Expression = struct
    let copy = copy_expression
  end

  module Case = struct
    let copy = copy_case
  end

  module Pattern = struct
    let copy = copy_pattern
  end
end
(*$*)
