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

let update_internal t ?(start = start t) ?(end_ = end_ t) ?(ghost = ghost t) () =
  create ~start ~end_ ~ghost ()

let update ?start ?end_ ?ghost t = update_internal t ?start ?end_ ?ghost ()
