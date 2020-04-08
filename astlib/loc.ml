type 'a t = 'a Ocaml_common.Location.loc =
  { txt : 'a
  ; loc : Ocaml_common.Location.t
  }

let map t ~f = { t with txt = f t.txt }
