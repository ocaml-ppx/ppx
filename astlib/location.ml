type t = Ocaml_common.Location.t =
  { loc_start : Position.t
  ; loc_end : Position.t
  ; loc_ghost : bool
  }

let none = Ocaml_common.Location.none

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

  exception Error = Ocaml_common.Location.Error
end
