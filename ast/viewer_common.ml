include struct
  open Astlib.Loc
  let txt'match view value = view value.txt
  let loc'match view value = view value.loc
end

include struct
  open Astlib.Location
  let loc_start'match view value = view value.loc_start
  let loc_end'match view value = view value.loc_end
  let loc_ghost'match view value = view value.loc_ghost
end

include struct
  open Astlib.Position
  let pos_fname'match view value = view value.pos_fname
  let pos_lnum'match view value = view value.pos_lnum
  let pos_bol'match view value = view value.pos_bol
  let pos_cnum'match view value = view value.pos_cnum
end
