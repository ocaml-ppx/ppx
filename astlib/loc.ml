type 'a t = 'a Ocaml_common.Location.loc =
  { txt : 'a
  ; loc : Ocaml_common.Location.t
  }

let of_loc (t : _ t) = t
let to_loc (t : _ t) = t

let txt t = t.txt
let loc t = Location.of_location t.loc

let create ~txt ~loc () = { txt; loc = Location.to_location loc }

let update_internal t ?(txt = txt t) ?(loc = loc t) () = create ~txt ~loc ()

let update ?txt ?loc t = update_internal t ?txt ?loc ()

let update_txt_internal t ~txt ?(loc = loc t) () = create ~txt ~loc ()

let update_txt ~txt ?loc t = update_txt_internal t ~txt ?loc ()

let map t ~f = update_txt t ~txt:(f t.txt)
