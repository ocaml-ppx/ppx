type t = Ocaml_common.Location.t =
  { loc_start : Lexing.position
  ; loc_end : Lexing.position
  ; loc_ghost : bool
  }

let of_location (t : t) = t
let to_location (t : t) = t

let start t = Position.of_position t.loc_start
let end_ t = Position.of_position t.loc_end
let ghost t = t.loc_ghost

let create ~start ~end_ ?(ghost = false) () =
  { loc_start = Position.to_position start
  ; loc_end = Position.to_position end_
  ; loc_ghost = ghost
  }

let none = of_location Ocaml_common.Location.none

let update_internal t ?(start = start t) ?(end_ = end_ t) ?(ghost = ghost t) () =
  create ~start ~end_ ~ghost ()

let update ?start ?end_ ?ghost t = update_internal t ?start ?end_ ?ghost ()

type location = t

module Error = struct
  type t = Ocaml_common.Location.error

  let of_error (t : t) = t
  let to_error (t : t) = t
  let make ~loc f = Ocaml_common.Location.error_of_printer loc (fun fmt () -> f fmt) ()
  let location (t : t) = t.loc
  let report fmt t = Ocaml_common.Location.report_error fmt t
  let register_of_exn f = Ocaml_common.Location.register_error_of_exn f

  let of_exn exn =
    match Ocaml_common.Location.error_of_exn exn with
    | None -> None
    | Some `Already_displayed -> None
    | Some (`Ok t) -> Some t
end
