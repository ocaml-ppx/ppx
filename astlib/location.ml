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
  type nonrec t =
    { loc : t
    ; fmt_msg : (Format.formatter -> unit)
    }

  let make ~loc fmt_msg = {loc; fmt_msg}

  let report fmt {loc; fmt_msg} =
    let msg = Format.asprintf "%t" fmt_msg in
    let err = Ocaml_common.Location.error ~loc msg in
    Ocaml_common.Location.(report_exception fmt (Error err))

  let to_extension {loc; fmt_msg} =
    let msg = Format.asprintf "%t" fmt_msg in
    let open Parsetree in
    let ext_name = {Ocaml_common.Location.txt = "ocaml.error"; loc} in
    let msg_item =
      let expr =
        { pexp_loc = loc
        ; pexp_attributes = []
        ; pexp_desc = Pexp_constant (Pconst_string (msg, None))
        }
      in
      {pstr_loc = loc; pstr_desc = Pstr_eval (expr, [])}
    in
    let payload = PStr [msg_item] in
    ext_name, payload
end
