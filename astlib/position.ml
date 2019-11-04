type t = Lexing.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }

let of_position (t : t) = t
let to_position (t : t) = t

let fname t = t.pos_fname
let lnum t = t.pos_lnum
let bol t = t.pos_bol
let cnum t = t.pos_cnum

let create ~fname ~lnum ~bol ~cnum () =
  { pos_fname = fname
  ; pos_lnum = lnum
  ; pos_bol = bol
  ; pos_cnum = cnum
  }

let update_internal
      t
      ?(fname = fname t)
      ?(lnum = lnum t)
      ?(bol = bol t)
      ?(cnum = cnum t)
      ()
  =
  create ~fname ~lnum ~bol ~cnum ()

let update ?fname ?lnum ?bol ?cnum t = update_internal t ?fname ?lnum ?bol ?cnum ()
