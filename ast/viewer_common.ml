let txt'match view value = view (Astlib.Loc.txt value)
let loc'match view value = view (Astlib.Loc.loc value)

let loc_start'match view value = view (Astlib.Location.start value)
let loc_end'match view value = view (Astlib.Location.end_ value)
let loc_ghost'match view value = view (Astlib.Location.ghost value)

let pos_fname'match view value = view (Astlib.Position.fname value)
let pos_lnum'match view value = view (Astlib.Position.lnum value)
let pos_bol'match view value = view (Astlib.Position.bol value)
let pos_cnum'match view value = view (Astlib.Position.cnum value)
